#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Chapter:: *)
(*Transaction simplification*)


(* ::Section:: *)
(*Etape 0 : graphics*)


(* ::Input::Initialization:: *)
construireGrapheUndirected[graph_,titre_]:=Graph[Apply[UndirectedEdge,Take[#,2]]&/@graph,
VertexStyle->Orange,
EdgeStyle->Purple,
VertexLabels->"Name",
PlotLabel->titre]


(* ::Input::Initialization:: *)
construireGraphe[graph_,titre_]:=Graph[
Apply[{x,y,z}\[Function]Labeled[x->y,If[z<0,Style[z,Red,Bold],z]],#]&/@graph,
VertexStyle->Orange,
EdgeStyle->Blue,
VertexLabels->Placed["Name",After],
PlotLabel->titre]


(* ::Input::Initialization:: *)
plots[graphe_]:=GraphicsRow[{construireGrapheUndirected[graphe,"Graphe Initial"],construireGraphe[etape1[graphe],"Step 1"],construireGraphe[tricount[graphe],"Step finale"]},ImageSize->Full]


(* ::Section:: *)
(*Step 1 : Simplification of multiple transactions*)


(* ::Input::Initialization:: *)
etape1[graphe_]:=Module[{reductionFunction,reduction1,couplesOrdonnesTransaction,mutuellement,Lmutuellement,uniqueSense,perfectKeys,f,intermediateReduction2,reduction2},
reductionFunction[graff_]:=GroupBy[graff,Take[#,2]&->Last,Total];
reduction1=reductionFunction[graphe];
couplesOrdonnesTransaction=Keys[reduction1];
mutuellement=Intersection[couplesOrdonnesTransaction,Reverse/@couplesOrdonnesTransaction];
Lmutuellement=Length[mutuellement];
uniqueSense=Take[mutuellement,Lmutuellement/2];
perfectKeys=Union[Complement[couplesOrdonnesTransaction,mutuellement],uniqueSense];
f=If[
MemberQ[perfectKeys,First[#]]==False,
#/.{Rule[a_,b_]:>Rule[Reverse[a],-b]},
#
]&;
intermediateReduction2=(f/@Normal[reduction1])/.{(a_->b_):>Flatten[{a,b}]};
reduction2=reductionFunction[intermediateReduction2];
(Apply[List,#]&/@Normal[reduction2])/.{{{a_,b_},c_}:>{b,a,c}}
]


(* ::Section:: *)
(*Step 2 : Finding triangles*)


(* ::Input::Initialization:: *)
trianglesDansGraphe[graphe_]:=Select[Subsets[graphe,{3}],Length[Union[Flatten[Transpose[#][[1;;2]]]]]==3&]


(* ::Section:: *)
(*Step 3 : Triangle simplification*)


(* ::Input::Initialization:: *)
signeDominant[{w1_,w2_,w3_}]:=MaximalBy[Tally[Sign/@{w1,w2,w3}],Last][[1,1]]


(* ::Input::Initialization:: *)
signeIsole[liste_]:=-1signeDominant[liste]


(* ::Input::Initialization:: *)
cagnottesPerso[cas_]:=Module[{gens,action},
gens=Union[Flatten[Take[#,2]&/@cas]];
action[{p1_,p2_,argent_}]:={{p1,-argent},{p2,argent}};
GroupBy[Flatten[action/@cas,1],First->Last,Total]
]


(* ::Input::Initialization:: *)
ResultatTriangleMoins[cagnotte_]:=Module[{Le,LesAutres,resultat},
Le=Select[Normal[cagnotte]/.{Rule->List},Sign[#[[2]]]==-1&][[1,1]];
LesAutres=Complement[Keys[cagnotte],{Le}];
Table[{Le,LesAutres[[i]],cagnotte[LesAutres[[i]]]},{i,Length[LesAutres]}]
]


(* ::Input::Initialization:: *)
ResultatTrianglePlus[cagnotte_]:=Module[{Le,LesAutres,resultat},
Le=Select[Normal[cagnotte]/.{Rule->List},Sign[#[[2]]]==1&][[1,1]];
LesAutres=Complement[Keys[cagnotte],{Le}];
Table[{LesAutres[[i]],Le,-cagnotte[LesAutres[[i]]]},{i,Length[LesAutres]}]
]


(* ::Input::Initialization:: *)
supprimerCagnotte[cagnotte_]:=Module[{cles=Keys[cagnotte]},
Append[#,0]&/@Thread[List[cles,RotateLeft[cles]]]
]


(* ::Input::Initialization:: *)
simplificationTriangle[cagnotte_]:=Which[
MemberQ[Values[cagnotte],0],Flatten[Append[Keys[Sort[Select[cagnotte,#!=0&]]],Values[Select[cagnotte,#>0&]]]],
Equal@@Values[cagnotte],supprimerCagnotte[cagnotte],
signeIsole[Values[cagnotte]]==-1,
ResultatTriangleMoins[cagnotte],
signeIsole[Values[cagnotte]]==1,
ResultatTrianglePlus[cagnotte]
]


(* ::Section:: *)
(*Step 4 : Simplifying all the triangles*)


(* ::Input::Initialization:: *)
tricount[graphe_]:=Module[{simplifiedGraphNow,trianglesNow,triangle},
simplifiedGraphNow=etape1[graphe];
trianglesNow=trianglesDansGraphe[simplifiedGraphNow];
While[trianglesNow!={},
triangle=First[trianglesNow];
simplifiedGraphNow=DeleteCases[Partition[Flatten[Append[DeleteCases[simplifiedGraphNow,Alternatives@@triangle],simplificationTriangle[cagnottesPerso[triangle]]]],3],{_,_,0}];
trianglesNow=trianglesDansGraphe[simplifiedGraphNow];
];
simplifiedGraphNow]


(* ::Input::Initialization:: *)
randomGraph[nbTransactions_,gens_]:=Module[{lettres=If[gens<27,CharacterRange["A","Z"],Range[gens]],personnes},
personnes=RandomSample[lettres,gens];
Table[Append[RandomSample[personnes,2],RandomInteger[{1,10}]],{i,nbTransactions}]
]
