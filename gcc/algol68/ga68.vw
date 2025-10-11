{ ga68.vw - The GNU Algol 68 strict language -*- vw -*-

  Copyright (C) 2025 Jose E. Marchesi <jemarch@gnu.org>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.  }

{ This file contains an annotated description of the syntax of the GNU
  Algol 68 strict language.  GNU Algol 68 aims to be a super-language
  of Algol 68.

  Extension to the strict Revised Report incorporated into GNU Algol
  68 are:

  [US] This is the GNU68-2025-001-unsafe GNU extension.  It adds an
       unsafe clause that marks a controlled clause as containing
       unsafe constructs which are known by the programmer, and makes
       the compiler to avoid certain diagnostics.  See the GNU Algol
       68 Compiler manual for more information.

  [SC] This is the GNU68-2025-003-andth-orel GNU extension.  It adds
       two units that act as pseudo-operators providing logical AND
       and OR functions with short-circuited elaboration.  See the GNU
       Algol 68 Compiler manual for more information.

  [MR] A modules and separated compilation system based on the modules
       system recomended by IFIP Working Group 2.1 Standing
       Subcommittee on Algol 68 Support, described in:

         A Modules and Separate Compilation Facility for Algol 68 by
         Lindsey and Boom.

  [NC] This is the GNU68-2025-005-nestable-comments GNU extension.  It
       adds support for nestable block comments.

  The metaproduction rules, hyper-rules and hyper-alternatives
  introduced by each extension are clearly marked in the sections
  below.  You can easily search for them using the extensions tags in
  the list above.  For example, to look for the extensions introduced
  by the modules and separated compilation system, search for [MR].

  The few deviations to the RR Algol 68 are clearly marked as well in
  this specification.

  A complete description of the semantics of the Algol 68 subset of
  the described language and the extensions is not included in this
  file.  The reader is referred to the Revised Report and other
  documentation corresponding to the extensions, like the GNU Algol 68
  compiler manual.

  The sectioning and the enumeration of metaproduction rules and of
  hyper-rules, including cross-references, are the same than in the
  Report.  The annotations and pragmatics added between curly brackets
  explain the meaning of the rules and how they describe the syntax of
  the language.  Note that thanks to the expressive power of VW
  grammars the syntax expressed by this description covers much more
  than what is usually expressed by context-free grammars in the
  descriptions of other languages that typically use some variant of
  the BNF notation.

  Sample code in examples and pragmatics is expressed using the UPPER
  stropping regime.

  This file is better browsed using the Emacs vw-mode, which provides
  automatic indentation, font-lock, and other facilities like the
  hiding of annotations an the following of cross-references.  See the
  vw-mode manual for more information, including a primer on VW
  grammars and the formal description used in both the original Report
  and this file.  }

1 Language and metalanguage

1.2 General metaproduction rules

1.2.1 Metaproduction rules of modes

A) MODE :: PLAIN ; STOWED ; REF to MODE ; PROCEDURE ;
           UNITED ; MU definition of MODE ; MU application.
B) PLAIN :: INTREAL ; boolean ; character.
C) INTREAL :: SIZETY integral ; SIZETY real.
D) SIZETY :: long LONGSETY ; short SHORTSETY ; EMPTY.
E) LONGSETY :: long LONGSETY ; EMPTY.
F) SHORTSETY :: short SHORTSETY ; EMPTY.
G) EMPTY :: .
H) STOWED :: structured with FIELDS mode ;
             FLEXETY ROWS of MODE.
I) FIELDS :: FIELD ; FIELDS FIELD.
J) FIELD :: MODE field TAG{942A}.
K) FLEXETY :: flexible ; EMPTY.
L) ROWS :: row ; ROWS row.
M) REF :: reference ; transient reference.
N) PROCEDURE :: procedure PARAMETY yielding MOID.
O) PARAMETY :: with PARAMETERS ; EMPTY.
P) PARAMETERS :: PARAMETER ; PARAMETERS PARAMETER.
Q) PARAMETER :: MODE parameter.
R) MOID :: MODE ; void.
S) UNITED :: union of MOODS mode.
T) MOODS :: MOOD ; MOODS MOOD.
U) MOOD :: PLAIN ; STOWED ; reference to MODE ; PROCEDURE ; void.
V) MU :: muTALLY.
W) TALLY :: i ; TALLY i.

1.2.2 Metaproduction rules associated with phrases and coercion

{ Extensions:
  [MR] access
  [US] unsafe }

A) ENCLOSED :: closed ; collateral ; parallel ; CHOICE{34A} ;
               loop ; access ; unsafe.
B) SOME :: SORT MOID NEST.
C) SORT :: strong ; firm ; meek ; weak ; soft.

{ Modules are activated by means of access-clauses.  }

1.2.3 Metaproduction rules associated with nests

{ Extensions:
  [MR] MODSETY, MOD, MODS, REVSETY, REVS, REV,
       TAU, INKSETY, INKS, INK }

A) NEST :: LAYER ; NEST LAYER.
B) LAYER :: new DECSETY LABSETY INKSETY.
C) DECSETY :: DECS ; EMPTY.
D) DECS :: DEC ; DECS DEC.
E) DEC :: MODE TAG{942A} ; priority PRIO TAD{942F} ;
          MOID TALLY TAB{942D} ; DUO TAD{942F}  MONO TAM{492K} ;
          MOD.
F) PRIO :: i ; ii ; iii ;
           iii i ; iii ii ; iii iii ;
           iii iii i ; iii iii ii ; iii iii iii.
G) MONO :: procedure with PARAMETER yielding MOID.
H) DUO :: procedure with PARAMETER1 PARAMETER2 yielding MOID.
I) LABSETY :: LABS ; EMPTY.
J) LABS :: LAB ; LABS LAB.
K) LAB :: label TAG{942A}.
L) MODSETY :: MODS ; EMPTY.
M) MODS :: MOD ; MODS MOD.
N) MOD :: module REVS TAB.
O) REVSETY :: REVS ; EMPTY.
P) REVS :: REV ; REVS REV.
Q) REV :: TAU reveals DECSETY INKS.
R) TAU :: MU.
S) INKSETY :: INKS ; EMPTY.
T) INKS :: INK ; INKS INK.
U) INK :: invoked TAU.

{ The primal environment is just 'new'. }

1.3 General hyper-rules

1.3.1 Syntax of general predicates

A) NOTION :: ALPHA ; NOTION ALPHA.
B) ALPHA :: a ; b ; c ; d ; e ; f ; g ; h ; i ; j;
            k ; l ; m ; n ; o ; p ; q ; r ; s ; t;
            u ; v ; w ; x ; y ; z.
C) NOTETY :: NOTION ; EMPTY.
D) THING :: NOTION ;
            (NOTETY1) NOTETY2 ;
            THING (NOTETY1) NOTETY2.
E) WHETHER :: where ; unless.

a) where true : EMPTY.
b) unless false : EMPTY.
c) where THING1 and THING2 : where THING1, where THING2.
d) where THING1 or THING2 : where THING1 ; where THING2.
e) unless THING1 and THING2 : unless THING1; unless THING2.
f) unless THING1 or THING2 : unless THING1, unless THING2.
g) WHETHER (NOTETY1) is (NOTETY2) :
     WHETHER (NOTETY1) begins with (NOTETY2){h,i,j}
             and (NOTETY2) begins with (NOTETY1){h,i,j}.
h) WHETHER (EMPTY) begins with (NOTION){g,j} :
     WHETHER false{b,-}.
i) WHETHER (NOTETY1) begins with (EMPTY){g,j} :
     WHETHER true{a,-}.
j) WHETHER (ALPHA1 NOTETY1) begins with
           (ALPHA2 NOTETY2){g,j,m} :
     WHETHER (ALPHA1) coincides with (ALPHA2) in
             (abcdefghijklmnopqrstuvwxyz){k,l,-}
             and (NOTETY1) begins with (NOTETY2){h,i,j}.
k) where (ALPHA) coincides with (ALPHA) in (NOTION){j} :
     where true{a}.
l) unless (ALPHA1) coincides with (ALPHA2) in (NOTION){j} :
     where (NOTION) contains (ALPHA1 NOTETY ALPHA2){m}
           or (NOTION) contains (ALPHA2 NOTETY ALPHA1){m}.
m) WHETHER (ALPHA NOTETY) contains (NOTION){l,m} :
     WHETHER (ALPHA NOTETY) begins with (NOTION){j}
             or (NOTETY) contains (NOTION){m,n}.
n) WHETHER (EMPTY) contains (NOTION){m} : WHETHER false{b,-}.

1.3.3 Syntax of general constructions

A) STYLE :: brief ; bold ; style TALLY.

a) NOTION option : NOTION ; EMPTY.
b) NOTION sequence{b} : NOTION ; NOTION, NOTION sequence{b}.
c) NOTION list{c} :
     NOTION ; NOTION, and also{94f} token, NOTION list{c}.
d) NOTETY STYLE pack :
     STYLE begin{94f,-} token, NOTETY, STYLE end{94f,-} token.
e) NOTION STYLE bracket :
     STYLE sub{94f,-} token, NOTION, STYLE bus{94f,-} token.
f) THING1 or alternatively THING2 : THING1 ; THING2.

2 The computer and the program

2.2 The program

2.2.1 Syntax

a) program : program token, strong integral new closed clause{31a}.

{ The value yielded by the elaboration of the program is the exit
  status returned by the process to the operating system upon
  termination.  This is a slight deviation from the Report, which
  instead specifies:

  a) program : strong void new closed clause.

  and mandates that the production tree of a particular program should
  be akin to the production of the program in the strict language. }

3 Clauses

3.0.1 Syntax

{ Extensions:
  [MR] "NEST module text publishing REVS defining LAYER",
       "NEST LAYER1 LAYER2 module series with DECSETY
          without DECSETY",
       "SOID NEST access clause"  }

a) *phrase : SOME unit{32d} ; NEST declaration of DECS{41a}.
b) *SORT MODE expression : SORT MODE NEST UNIT{5A}.
c) *statement : strong void NEST UNIT{5A}.
d) *MOID constant : MOID NEST DEFIED identifier with TAG{48a,b} ;
                    MOID NEST denoter{80a}.
e) *MODE variable :
     reference to MODE NEST DEFIED identifier with TAG{48a,b}.
f) *NEST range :
     SOID NEST serial clause defining LAYER{32a} ;
     SOID NEST chooser CHOICE STYLE clause{34b} ;
     SOID NEST case part of choice using UNITED{34i} ;
     NEST STYLE repeating part with DEC{35e} ;
     NEST STYLE while do part{35f} ;
     PROCEDURE NEST routine text{541a,b} ;
     NEST module text publishing REVS defining LAYER{49c,-} ;
     NEST LAYER1 LAYER2 module series
          with DECSETY without DECSETY1{49d} ;
     SOID NEST access clause{36a}.

{ The rules b and c establish a precise definition of "expressions"
  and "statements".  The former are units yielding values of a mode
  other than VOID in any context.  The later are units in a strong
  context with a-posteriori mode of VOID, that get voided. }

{ The rules d and e establish a precise definition of "constants" and
  "variables".  The former are either a denotation or an identifier of
  some mode.  The second are identifiers of a "reference to" mode. }

{ The rule f introduces a paranotion for all the constructs that
  introduce new ranges.  }

3.1 Closed clauses

3.1.1 Syntax

A) SOID :: SORT MOID.
B) PACK :: STYLE pack.

a) SOID NEST closed clause{22a,5D,551a,A341h,A349a} :
     SOID NEST serial clause defining LAYER{32a} PACK.

{ Examples:
  a) BEGIN x := 1; y := 2 END }

3.2 Serial clauses

3.2.1 Syntax

{ Extensions:
  [MR] "establishing clause"  }

a) SOID NEST serial clause defining new PROPSETY{31a,34f,1,35h} :
     SOID NEST new PROPSETY series with PROPSETY{b}.
b) SOID NEST series with PROPSETY{a,b,35c} :
     strong void NEST unit{d}, go on{94f} token,
       SOID NEST series with PROPSETY{b} ;
     where (PROPSETY) is (DECS DECSETY LABSETY),
       NEST declaration of DECS{41a}, go on{94f} token,
       SOID NEST series with DECSETY LABSETY{b} ;
     where (PROPSETY) is (LAB LABSETY),
       NEST label definition of LAB{c},
       SOID NEST series with LABSETY{b} ;
     where (PROPSETY) is (LAB LABSETY)
           and SOID balances SOID1 and SOID2{3}, SOID1 NEST unit{d},
       completion{94f} token, NEST label definition of LAB{c},
       SOID2 NEST series with LABSETY{b} ;
     where (PROPSETY) is (EMPTY),
       SOID NEST unit{d}.
c) NEST label definition of label TAG{b} :
     label NEST defining identifier with TAG{48a}, label{94f} token.
d) SOME unit{b,33b,g,34i,35d,46m,n,521c,532e,541a,b,543c,A34Ab,c,d} :
     SOME UNIT{5A,-}.
e) WHETHER SORT MOID balances
           SORT1 MOID1 an SORT2 MOID2{b,33b,34d,h} :
     WHETHER SORT balances SORT1 and SORT2{f}
             and MOID balances MOID1 and MOID2{g}.
f) WHETHER SORT balances SORT1 and SORT2{e,522a} :
     where (SORT1) is (strong), WHETHER (SORT2) is (SORT) ;
     where (SORT2) is (strong), WHETHER (SORT1) is (SORT).
g) WHETHER MOID balances MOID1 and MOID2{3} :
     where (MOID1) is (MOID2), WHETHER (MOID) is (MOID1) ;
     where (MOID1) is (transient MOID2),
       WHETHER (MOID) is (MOID1) ;
     where (MOID2) is (transient MOID1),
       WHETHER (MOID) is (MOID2).

h) *SOID unitary clause : SOID NEST unit{d}.
i) *establishing clause :
     SOID NEST serial clause defining LAYER{32a} ;
     MODE NEST enquiry clause defining LAYER{34c}.

{ The paranotion establishing-clause encompasses both module-texts and
  revelations.  }

{ Examples:
  b) read (x1); REAL s:= 0;
     sum: FOR i TO n DO (x1[i] > 0 | s :+= x1[i] | nonpos) OD EXIT
     nonpos: print (s)

     REAL s := 0;
     sum: FOR i TO n DO (x1[i] > 0 | s :+= x1[i] | nonpos) OD EXIT
     nonpos: print (s)

     sum: FOR i TO n DO (x1[i] > 0 | s :+= x1[i] | nonpos) OD EXIT
     nonpos: print (s)

     FOR i TO n DO (x1[i] > 0 | s :+= x1[i] | nonpos) OD EXIT
     nonpos: print (s)
  c) sum:
  d) print (s) }

3.3 Collateral and parallel clauses

3.3.1 Syntax

a) strong void NEST collateral clause{5D,551a} :
     strong void NEST joined portrait{b} PACK.
b) SOID NEST joined portrait{a,b,c,d,34g} :
     were SOID balances SOID1 and SOID2{32e},
       SOID1 NEST unit{32d}, and also{94f} token,
       SOID2 NEST unit{32d}
             or alternatively SOID2 NEST joined portrait{b}.
c) strong void NEST parallel clause{5D,551a} :
     parallel{94f} token, strong void NEST joined portrait{b} PACK.
d) strong ROWS of MODE NEST collateral clause{5D,551a} :
     where (ROWS) is (row),
       strong MODE NEST joined portrait{b} PACK ;
     where (ROWS) is (row ROWS1),
       strong ROWS1 of MODE NEST joined portrait{b} PACK ;
     EMPTY PACK.
e) strong structured with
          FIELDS FIELD mode NEST collateral clause{5D,551a} :
     NEST FIELDS FIELD portrait{f} PACK.
f) NEST FIELDS FIELD portrait{3,f} :
     NEST FIELDS portrait{f,g}, an also{94f} token,
       NEST FIELD portrait{g}.
g) NEST MODE field TAG portrait{f} : strong MODE NEST unit{32d}.

h) *structure display :
     strong structured with FIELDS FIELD mode NEST collateral clause{e}.
i) *row display :
     strong ROWS of MODE NEST collateral clause{d}.
j) *display : strong STOWED NEST collateral clause{d,e}.
k) *vacuum : EMPTY PACK.

3.4 Choice clauses

3.4.1 Syntax

A) CHOICE :: choice using boolean ; CASE.
B) CASE :: choice using intgral ; choice using UNITED.

a) SOID NEST1 CHOICE clause{5D,551a,A341h,A349a} :
     CHOICE STYLE start{91a,-},
       SOID NEST1 chooser CHOICE STYLE clause{b},
       CHOICE STYLE finish{91e,-}.
b) SOID NEST1 chooser choice using MODE STYLE clause{a,l} :
     MODE NEST1 enquiry clause defining LAYER2{c,-},
       SOID NEST1 LAYER2 alternate choice using MODE STYLE clause{d}.
c) MODE NEST1 enquiry clause defining new DECSETY2{b,35g} :
     meek MODE NEST1 new DESETY2 series with DECSETY2{32b}.
d) SOID NEST2 alternate CHOICE STYLE clause{b} :
     SOID NEST2 in CHOICE STYLE clause{e} ;
     where SOID balances SOID1 and SOID2{32e},
       SOID1 NEST2 in CHOICE STYLE clause{3},
       SOID2 NEST2 out CHOICE STYLE clause{l}.
e) SOID NEST2 in CHOICE STYLE clause{d} :
     CHOICE STYLE in{91b,-}, SOID NEST2 in part of CHOICE{f,g,h}.
f) SOID NEST2 in part of choice using boolean{e} :
     SOID NEST2 serial clause defining LAYER3{32a}.
g) SOID NEST2 in part of choice using integral{e} :
     SOID NEST2 joined portrait{33b}.
h) SOID NEST2 in part of choice using UNITED{e,h} :
     SOID NEST2 case part of choice using UNITED{i} ;
     where SOID balances SOID1 and SOID2{32e},
       SOID1 NEST2 case part of choice using UNITED{i},
       and also{94f} token,
       SOID2 NEST2 in part of choice using UNITED{h}.
i) SOID NEST2 case part of choice using UNITED{h} :
     MOID NEST2 LAYER3 specification defining LAYER3{jk,-},
       where MOID unites to UNITED{64b},
       SOID NEST2 LAYER3 unit{32d}.
j) MODE NEST3 specification defining new MODE TAG3{i} :
     NEST3 declarative defining new MODE TAG3{541e} brief pack,
       colon{94f} token.
k) MOID NEST3 specification defining new EMPTY{i} :
     formal MOID NEST3 declarer{46b} brief pack, colon{94f} token.
l) SOID NEST2 out CHOICE STYLE clause{d} :
     CHOICE STYLE out{91d,-},
       SOID NEST2 serial lause defining LAYER3{32a} ;
     CHOICE STYLE again{91c,-},
       SOID NEST2 chooser CHOICE2 STYLE clause{b},
       where CHOICE2 may follow CHOICE{m}.
m) WHETHER choice using MODE2 may follow choice using MODE1{l} :
     where (MODE1) is (MOOD), WHETHER (MODE2) is (MODE1) ;
     where (MODE1) begins with (union of),
       WHETHER (MODE2) begins with (union of).

n) *SOME choice clause : SOME CHOICE clause{a}.
o) *SOME conditional clause : SOME choice using boolean clause{a}.
p) *SOME case clause : SOME choice using integral clause{a}.
q) *SOME conformity clause : SOME choice using UNITED clause{a}.

3.5 Loop clauses

3.5.1 Syntax

A) FROBYT :: from ; by ; to.

a) strong void NEST1 loop clause{5D,551a} :
     NEST1 STYLE for part defining new integral TAG2{b},
       NEST1 STYLE intervals{c},
       NEST1 STYLE repeating part with integral TAG2{e}.
b) NEST1 STYLE for part defining new integral TAG2{a} :
     STYLE for{94g,-} token,
       integral NEST1 new integral TAG2 defining identifier
                with TAG2{48a} ;
     where (TAG2) is (letter aleph), EMPTY.
c) NEST1 STYLE intervals{a} :
     NEST1 STYLE from part{d} option,
       NEST1 STYLE by part{d} option,
       NEST1 STYLE to part{d} option.
d) NEST1 STYLE FROBYT part{c} :
     STYLE FROBYT{94g,-} token, meek integral NEST1 unit{32d}.
e) NEST1 STYLE repeating part with DEC2{a} :
     NEST1 new DEC2 STYLE while do part{f} ;
     NEST1 new DEC2 STYLE do part{h}.
f) NEST2 STYLE while do part{e} :
     NEST2 STYLE while part defining LAYER3{g},
       NEST2 LAYER3 STYLE do part{h}.
g) NEST2 STYLE while part defining LAYER3{f} :
     STYLE while{94g,-} token,
       boolean NEST2 enquiry clause defining LAYER3{34c,-}.
h) NEST3 STYLE do part{3,f} :
     STYLE do{94g,-} token,
       strong void NEST3 serial clause defining LAYER4{32a},
       STYLE od{94g,-} token.

3.6 Access clauses

{ Extensions:
  [MR] GMR }

{ Access clauses contain a controlled-clause, which is an
  enclosed-clause. }

3.6.1 Syntax

a) SOID NEST access clause{5D,551a,A341h,A349a} :
     NEST revelation publishing EMPTY defining LAYER{b},
       SOID NEST LAYER ENCLOSED clause{a,31a,33a,c,d,e,34a,35a,-}.
b) NEST revelation publishing REVSETY
        defining new DECSETY INKSETY{a,49c} :
     access{94d} token,
       NEST joined module call publishing REVSETY revealing REVS{c},
       where DECSETY INKS revealed by REVS{e,f}
             and NEST filters INKSETY out of INKS{h}.
c) NEST joined module call publishing REVSETY revealing RES{b,c} :
     NEST moule call publishing REVSETY revealing REVS{d,-} ;
     where (REVSETY) is (REVSETY1 REVSETY2)
           an (REVS) is (REVS1 REVS2),
       NEST module call publishing REVSETY1 revealing REVS1{d,-},
       and also{94f} token,
       NEST joined module call publishing REVSETY2 revealing REVS2{c}.
d) NEST module call publishing REVSETY revealing REVS{c} :
     where (REVSETY) is (EMPTY),
       module REVS NEST applied module indication with TAB{48b} ;
     where (REVSETY) is (REVS),
       public{94d} token,
       module REVS NEST applied module indication with TAB{48b}.
e) WHETHER DECSETY1 DECSETY2 INKS1 INKSETY2 revealed by
           TAU reveals DECSETY1 INKS1 REVSETY3
           TAU reveals DECSETY1 INKS1 REVSETY4{b,e,f} :
     WHETHER DECSETY DECSETY2 INKS1 INKSETY2 revealed by
             TAU reveals DECSETY1 INKS1 REVSETY3 REVSETY4{e,f}.
f) WHETHER DECSETY1 DECSETY2 INKS1 INKSETY2 revealed by
           TAU reveals DECSETY1 INKS1 REVSETY2{b,e,f} :
     WHETHER DECSETY2 INKSETY2 revealed by REVSETY2
             and DECSETY1 independent DECSETY2{71a,b,c}.
g) WHETHER EMPTY revealed by EmPTY{e,f} : WHETHER true.
h) WHETHER NEST filters INKSETY1 out of INKSETY INK{b} :
     unless INK ientified in NEST{72a},
       WHETHER (INKSETY) is (INKSETY2 INK)
               and NEST INK filers INKSETY2 out of INKSETY{h,i} ;
     where INK identified in NEST{72a},
       WHETHER NEST filters INKSETY1 out of INKSETY{h,i}.
i) WHETHER NEST filters EMPTY out of EMPTY{h} : WHETHER true.

{ Examples:
    a) ACCESS A, B (gets (f, a); puts (a))
    b) ACCESS A, B
    c) A, B
    d) A
       PUB B  }

{ In rule b, the 'invoke TAU's enveloped by 'INKS' represent those
  modules which might need to be invoked at any module-call whose
  applied-module-indication identified a particular
  defining-module-indication, whereas those enveloped by 'INKSETY'
  represent only those which need invocation in the particular
  context, the remainder having already being elaborated, as can be
  determined statically from the 'NEST'.  The presence of 'INKSETY' in
  the nest of all descendent constructs of the access-clause ensures
  that all modules now invoked will never be invoked again within
  those descendents.  }

{ Rule f ensures the independence of declarations revealed by one
  revelation; thus

    MODULE A = DEF PUB REAL x FED, B = DEF PUB REAL x FED;
    ACCESS A, B (x)

  is not produced.  However, rule e allows a given declaration to be
  revealed by two public accesses of the same module, as in

    MODULE A = DEF PUB REAL x FED;
    MODULE B = ACCESS PUB A DEF REAL y FED,
           C = ACCESS PUB A DEF REAL z FED;
    ACCESS B C (x + y + z)

  in which the module-definitions for both B and C reveal x, by virtue
  of the PUB A in their constituent revelations.  }

{ Note that a particular-program may now consist of a
  joined-label-definition followed by an access-clause.  The
  defining-module- indications identified thereby would be in the
  library-prelude or the user-prelude.  }

3.7 Unsafe clauses

{ Extensions: [US] }

{ Unsafe clauses contain a controlled-clause, which is an enclosed-clause. }

3.7.1 Syntax

a) SOID NEST unsafe clause :
     unsafe{94f} token, SOID NEST ENCLOSED clause{a,31a,33a,c,d,e,34a,35a,-}.

{ Examples:
    a) UNSAFE (ptr := dst)  }

4 Declarations, declarers and indicators

4.1 Declarations

4.1.1 Syntax

{ Extensions:
  [MR] module
       "declaration with DECSETY without DECSETY1" }

A) COMMON :: mode ; priority ; MODINE identity ;
             reference to MODINE variable ; MODINE operation ;
             PARAMETER ; MODE FIELDS ; module.
             { MODINE :: MODE ; routine. }

a) NEST declaration of DECS{a,32b} :
     NEST COMMON declaration of DECS{42a,43a,44a,e,45a,-} ;
     where (DECS) is (DECS1 DECS2),
       NEST COMMON declaration of DECS1{42a,43a,44a,e,45a,-},
       and also{94f} token, NEST declaration of DECS2{a}.
b) NEST COMMON joined definition of PROPS PROP{b,42a,43a,44a,e,45a,46e,541e} :
     NEST COMON joined definition of PROPS{b,c},
       and also{94f} token,
       NEST COMMON joined definition of PROP{c}.
c) NEST COMMON joined definition of PROP{b,42a,43a,44a,e,45a,46e,541e} :
     NEST COMMON definition of PROP{42b,43b,44c,f,45c,46f,541f,-}.

d) *definition of PROP :
     NEST COMMON definition of PROP{42b,43b,44c,f,45c,46f,541f} ;
     NEST label definition of PROP{32}.
e) NEST declaration with DECSETY without DECSETY1{49e} :
     where (DECSETY without DECSETY1) is (EMPTY without DECS1),
       NEST COMMON declaration of DECS1{42a,43a,44a,e,45a,49a,-} ;
     where (DECSETY without DECSETY1) is (DECS without EMPTY),
       public{94d} token,
       NEST COMMON declaration of DECS{42a,43a,44a,e,45a,49a,-} ;
     where (DECSETY without DECSETY1) is
           (DECSETY without DECS1 DECSETY2),
       NEST COMMON declaration of DECS1{42a,43a,44a,e,45a,49a,-},
       and also{94f} token,
       NEST declaration with DECSETY without DECSETY2{e} ;
     where (DECSETY without DECSETY1) is
           (DECS DECSETY3 without DECSETY1),
       public{94d} token,
       NEST COMMON declaration of DECS{42a,43a,44a,e,45a,49a,-},
       and also{94f} token,
       NEST declaration with DECSETY3 without DECSETY1{e}.

{ Rule e determines how a "NEST declaration with DECSETY without
  DECSETY1" results into two groups of declarations.  The declarations
  in 'DECSETY' are public and syntactically preceded by PUB.  The
  declarations in 'DECSETY1 are non-public and are not marked by
  PUB.  }

4.2 Mode declarations

4.2.1 Syntax

a) NEST mode declaration of DECS{41a} :
     mode{94d} token, NEST mode joined definition of DECS{41b,c}.
b) NEST mode definition of MOID TALLY TAB{41c} :
     where (TAB) is (bold TAG) or (NEST) is (new LAYER),
       MOID TALLY NEST defining mode indication with TAB{48a},
       is defined as{94d} token,
       actual MOI TALLY NEST declarer{c}.
c) actual MOID TALLY1 NEST declarer{b} :
     where (TALLY1) is (i),
       actual MOID NEST declarator{46c,d,g,h,o,s,-} ;
     where (TALLY1) is (TALLY2 i),
       MOID TALLY2 NEST applied mode indication with TAB2{48b}.

{ The use of TALLY excludes circular chains of mode-definitions such
  as `mode a = b, b = a'. }

4.3 Priority declarations

4.3.1 Syntax

a) NEST priority declaration of DECS{41a} :
     priority{94d} token, NEST priority joined definition of DECS{41b,c}.
b) NEST priority definition of priority PRIO TAD{41c} :
     priority PRIO NEST definining operator with TAD{48a},
       is defined as{94d} token, DIGIT{94b} token,
       where DIGIT counts DIGIT{94b} token,
       where DIGIT counts PRIO{c,d}.
   {DIGIT :: digit zero ; digit one ; digit two ; digit three ;
             digit four ; digit five ; digit six ; digit seven ;
             digit eight ; digit nine.}
c) WHETHER DIGIT1 counts PRIO i{b,c} :
     WHETHER DIGIT2 counts PRIO{c,d},
       where (digit one igit two digit three digit four
              digit five digit six digit seven digit eight digit nine)
              contains (DIGIT2 DIGIT1).
d) WHETHER digit one counts i{b,c} : WHETHER true.

4.4 Identifier declarations

4.4.1 Syntax

A) MODINE :: MODE ; routine.
B) LEAP :: local ; heap ; primal.

a) NEST MODINE identity declaration of DECS{41a} :
     formal MODINE NEST declarer{b,46b},
       NEST MODINE identity joined definition of DECS{41b,c}.
b) VICTAL routine NEST declarer{a,523b} : procedure{94d} token.
c) NEST MODINE identity definition of MODE TAG{41c} :
     MODE NEST defining identifier with TAG{48a},
       is defined as{94d} token, MODE NEST source for MODINE{d}.
d) MODE NEST source for MODINE{c,f,45c} :
     where (MODINE) is (MODE), MODE NEST source{521c} ;
     where (MODINE) is (routine), MODE NEST routine text{541a,b,-}.
e) NEST reference to MODINE variable declaration of DECS{41a} :
     reference to MODINE NEST LEAP sample generator{523b},
       NEST reference to MODINE variable joined definition of DECS{41b,c}.
f) NEST reference to MODINE variable definition
        of reference to MODE TAG{41c} :
     reference to MODE NEST defining identifier with TAG{48a},
       becomes{94c} token, MODE NEST soure for MODINE{d} ;
     where (MODINE) is (MODE),
       reference to MODE NEST defining identifier with TAG{48a}.

g) *identifier declaration :
     NEST MODINE identity declaration of DECS{a} ;
     NEST reference to MODINE variable declaration of DECS{e}.

4.5 Operation declarations

4.5.1 Syntax

A) PRAM :: DUO ; MONO.
B) TAO :: TAD ; TAM.

a) NEST MODINE operation delarations of DECS{41a} :
     operator{94d} token, formal MODINE NEST plan{b,46p,-},
       NEST MODINE operation joined definition of DECS{41b,c}.
b) formal routine NEST plan{a} : EMPTY.
c) NEST MODINE operation definition of PRAM TAO{41c} :
     PRAM NEST defining operator with TAO{48a},
       is defined as{94d} token, PRAM NEST source for MODINE{44d}.

4.6 Declarers

4.6.1 Syntax

A) VICTAL :: VIRACT ; formal.
B) VIRACT :: virtual ; actual.
C) MOIDS :: MOID ; MOIDS MOID.

a) VIRACT MOID NEST declarer{c,e,g,h,523a,b} :
     VIRACT MOID NEST declarator{c,d,g,h,o,s,-} ;
     MOID TALLY NEST applied mode indication with TAB{48b,-}.
b) formal MOID NEST declarer{e,h,p,r,u,34k,44a,541a,b,e,551a} :
     where MOID deflexes to MOID{47a,b,c,-},
       formal MOID NEST declarator{c,d,h,o,s,-} ;
     MOID1 TALLY NEST applied mode indication with TAB{48b,-},
       where MOID1 deflexes to MOID{47a,b,c,-}.
c) VICTAL reference to MODE NEST declarator{a,b,42c} :
     reference to{94d} token, virtual MODE NEST declarer{a}.
d) VICTAL structured with FIELDS mode NEST declarator{a,b,42c} :
     structure{94d} token,
       VICTAL FIELDS NEST portrayer of FIELDS{e} brief pack.
e) VICTAL FIELDS NEST portrayer of FIELDS1{d,e} :
     VICTAL MODE NEST declarer{a,b},
       NEST MODE FIELDS joined definition of FIELDS1{41b,c} ;
     where (FIELDS1) is (FIELDS2 FIELDS3),
       VICTAL MODE NEST declarer{a,b},
       NEST MODE FIELDS joined definition of FIELDS2{41b,c},
       and also{94f} token,
       VICTAL FIELDS NEST portrayer of FIELDS3{e}.
f) NEST MODE FIELDS definition of MODE field TAG{41c} :
     MODE field FIELDS defining field selector with TAG{48c}.
g) VIRACT flexible ROWS of MODE NEST declarator{a,42c} :
     flexible{94d} token, VIRACT ROWS of MODE NEST declarer{a}.
h) VICTAL ROWS of MODE NEST declarator{a,b,42c} :
     VICTAL ROWS NEST rower{i,j,k,l} STYLE bracket,
       VICTAL MODE NEST declarer{a,b,}.
i) VICTAL row ROWS NEST rower{h,i} :
     VICTAL row NEST rower{j,k,l}, and also{94f} token,
       VICTAL ROWS NEST rower{i,j,k,l}.
j) actual row NEST rower{h,i} :
     NEST lower bound{m}, up to{94f} token, NEST upper bound{n} ;
     NEST upper bound{n}.
k) virtual row NEST rower{h,i} : up to{94f} token option.
l) formal row NEST rower{h,i} : up to{94f} token option.
m) NEST lower bound{j,532f,g} : meek integral NEST unit{32d}.
n) NEST upper bound{j,532f} : meek integral NEST unit{32d}.
o) VICTAL PROCEDURE NEST declarator{a,b,42c} :
     procedure{94d} token, formal PROCEDURE NEST plan{p}.
p) formal procedure PARAMETY yieling MOID NEST plan{o,45a} :
     where (PARAMETY) is (EMPTY), formal MOID NEST declarer{b} ;
     PARAMETERS NEST joined declarer{q,r} brief pack,
       formal MOID NEST declarer{b}.
q) PARAMETERS PARAMETER NEST joined declarer{p,q} :
     PARAMETERS NEST joined declarer{q,r}, and also{94f} token,
       PARAMETER NEST joined declarer{r}.
r) MODE parameter NEST joined declarer{p,q} :
     formal MODE NEST declarer{b}.
s) VICTAL union of MOODS1 MOOD1 mode NEST declarator{a,b,42c} :
     unless EMPTY with MOODS1 MOOD1 incestuous{47f},
       union of{94d} token,
       MOIDS NEST joined declarer{t,u} brief pack,
       where MOIDS ravels to MOODS2{47g}
             and safe MOODS1 MOOD1 subset of safe MOODS2{73l}
             and safe MOODS2 subset of safe MOODS1 MOOD1{731,m}.
t) MOIDS MOID NEST joined declarer{s,t} :
     MOIDS NEST joined declarer{t,u}, an also{94f} token,
       MOID NEST joined declarer{u}.
u) MOID NEST joined declarer{s,t} : formal MOID NEST declarer{b}.

4.7 Relationships between modes

4.7.1 Syntax

A) NONSTOWED :: PLAIN ; REF to MODE ; PROCEDURE ; UNITED ; void.
B) MOODSETY :: MOODS ; EMPTY.
C) MOIDSETY :: MOIDS ; EMPTY.

a) WHETHER NONSTOWED deflexes to NONSTOWED{b,e,46b,521c,62a,71n} :
     WHETHER true.
b) WHETHER FLEXETY ROWS of MODE1 deflexes to ROWS of MODE2{b,e,46b,521c,62a,71n} :
     WHETHER MODE1 deflexes to MODE2{a,b,c,-}.
c) WHETHER structured with FIELDS1 mode deflexes to
           structured with FIELDS2 mode{b,e,46b,521c,62a,71n} :
     WHETHER FIELDS1 deflexes to FIELDS2{d,e,-}.
d) WHETHER FIELDS1 FIELD1 deflexes to FIELDS2 FIELD2{c,d} :
     WHETHER FIELDS1 deflexes to FIELDS2{d,e,-}
             and FIELD1 deflexes to FIELD2{e,-}.
e) WHETHER MODE1 field TAG deflexes to MODE2 field TAG{c,d} :
     WHETHER MODE1 deflexes to MODE2{a,b,c,-}.

f) WHETHER MOODSETY1 with MOODSETY2 inestuous{f,46s} :
     where (MOODSETY2) is (MOOD MOODSETY3),
       WHETHER MOODSETY1 MOOD with MOODSETY3 incestuous{f}
               or MOOD is firm union of MOODSETY1 MOODSETY3 mode{71m} ;
     where (MOODSETY2) is (EMPTY), WHETHER false.

g) WHETHER MOIDS ravels to MOODS{g,46s} :
     where (MOIDS) is (MOODS), WHETHER true ;
     where (MOIDS) is
           (MOODSETY union of MOODS1 mode MOIDSETY),
       WHETHER MOODSETY MOODS1 MOIDSETY ravels to MOODS{g}.

{ The hyperrules from a) to e) implement a predicate deflexes-to that
  determines whether a given mode deflexes to another mode.  Any
  non-stowed mode deflexes to any other non-stowed mode.  A row mode
  deflexes to another row mode if the ranks of the modes are the same
  and the mode of the former's elements deflexes to the mode of the
  later's elements.  A structured mode deflexes to another structured
  mode if they have the same number of fields with the same tags and
  their modes deflex.  }

{ The hyperrule f) implements a predicate that determines whether two
  provided sets of moods are incestuous, i.e. whether they contain
  modes which are firmly related.  }

{ The hyperrule g) determines whether a set of moods and
  united modes may be ravelled.  }

4.8 Indicators and field selectors

4.8.1 Syntax

{ Extensions:
  [MR] INK, "module indication", "module REVS", "invoked", TAU }

A) INDICATOR :: identifier ; mode indication ; operator ;
                module indication.
B) DEFIED :: defining ; applied.
C) PROPSETY :: PROPS ; EMPTY.
D) PROPS :: PROP ; PROPS PROP.
E) PROP :: DEC ; LAB ; FIELD ; INK.
F) QUALITY :: MODE ; MOID TALLY ; DYADIC ; label ; MODE field ;
              module REVS ; invoked.
G) TAX :: TAG ; TAB ; TAD ; TAM ; TAU.

a) QUALITY NEST new PROPSETY1 QUALITY TAX PROPSETY2
           defining INDICATOR with TAX{32c,35b,42b,43b,44c,f,45c,541f} :
     where QUALITY TAX independent PROPSETY1 PROPSETY2{71a,b,c},
       TAX{942A,D,F,K} token.
b) QUALITY NEST applied INDICATOR with TAX{42c,46a,b,5D,542a,b,544a} :
     where QUALITY TAX identified in NEST{72a},
       TAX{942A,D,F,K} token.
c) MODE field PROPSETY1 MODE field TAG PROPSETY2 defining
        field selector with TAG{46f} :
     where MODE field TAG independent PROPSETY1 PrOPSETY2{71a,b,c},
       TAX{942A} token.
d) MODE field FIELDS applied field selector with TAG{531a} :
     where MODE field TAG resides in FIELDS{72b,c,-},
       TAG{942A} token.

e) *QUALITY NEST DEFIED indicator with TAX :
     QUALITY NEST DEFIED INDICATOR with TAX{a,b}.
f) *MODE DEFIED field seletor with TAG :
     MODE field FIELDS DEFIED field selector with TAG{c,d}.

{ MODs are introduced into a nest by module-declarations.
  INKs are introduced into a nest by module-calls. }

{ Modules are ascribed to module-indications by means of
  module-declarations.  }

4.9 Module declarations

4.9.1 Syntax

a) NEST1 module declaration of MODS{41a,e} :
     module{94d} token,
       NEST1 module joined definition of MODS{41b,c}.
b) NEST1 module definition of module RESETY REV TAB{41c} :
     where (REV) is (TAU reveals DECSETY invoked TAU)
           and (TAB) is (bold TAG),
       where (NEST1) is (NOTION1 invoked TAU NOTETY2),
       unless (NOTION1 NOTETY2) contains (invoked TAU),
       module REVSETY REV NEST1 defining module indication with TAB{48a},
       is defined as{94d} token,
       NEST1 module text publishing REVSETY REV defining LAYER{c,-}.
c) NEST1 module text
         publishing REVSETY TAU reveals DECSETY INKSETY INK
         defining new DECSETY1 DECSETY INK{b} :
     where (INKSETY) is (EMPTY) and (REVSETY) is (EMPTY),
       def{94d} token,
       NEST1 new new DECSETY1 DECSETY INK module series
             with DECSETY without DECSETY1{d},
       fed{94d} token ;
     NEST1 revelation publishing REVSETY defining LAYER{36b},
       def{94d} token,
       NEST1 LAYER new DECSETY1 DECSETY INK module series
             with DECSETY without DECSETY1{d},
       fed{94d} token,
       where (LAYER) is (new DECSETY2 INKSETY).
d) NEST3 module series with DECSETY without DECSETY1{c} :
     NEST3 module prelude with DECSETY without DECSETY1{e},
       NEST3 module postlude{f} option.
e) NEST3 module prelude with DECSETY1 without DECSETY2{d,e} :
     strong void NEST3 unit{32d}, go on{94f} token,
       NEST3 module prelude with DECSETY1 without DECSETY2{e} ;
     where (DECSETY1 without DECSETY2) is
           (DECSETY3 DECSETY4 without DECSETY5 DECSETY6>,
       NEST3 declaration with DECSETY3 without DECSETY5{41e},
       go on{94f} token,
       NEST3 module prelude with DECSETY4 without DECSETY6{e} ;
     where (DECSETY1 without DECSETY2) is (EMPTY without EMPTY),
       strong void NEST3 unit{32d} ;
     NEST3 declaration with DECSETY1 without DECSETY2{41e}.
f) NEST3 module postlude{d} :
     postlude{94d} token, strong void NEST3 series with EMPTY{32b}.

g) *module text :
     NEST module text publishing REVS defining LAYER{c}.

{ Examples:
    a) MODULE A = DEF STRING s; gets (s);
                      PUB STRING t = "file"+s, PUB REAL a FED,
              B = ACCESS A DEF PUB INT fd;
                               fopen (fd, file o rdonly)
                           POSTLUDE close (f) FED

    b) A = DEF STRING s; gets (s);
               PUB STRING t = "file"+s, PUB REAL a FED

       B = ACCESS A DEF PUB FILE f;
                        fopen (fd, file o rdonly)
                        POSTLUDE close (f) FED

    c) DEF STRING s; gets (s);
           PUB STRING t = "file"+s, PUB REAL a FED

       ACCESS A DEF PUB FILE f;
                        fopen (fd, file o rdonly)
                        POSTLUDE close (f) FED

    d) STRING s; gets (s); PUB STRING t = "file"+s, PUB real a

       PUB FILE f; fopen (fd, file o rdonly) POSTLUDE close (f)

    e) STRING s; gets (s); PUB STRING t = "file"+s, PUB real a

       PUB FILE f; fopen (fd, file o rdonly)

    f) POSTLUDE close (f) }

{ Note that the EMPTY (for PROPSETY) in rule f enforces that a module
  postlude cannot contain declarations, labels or module accesses.
  Only units are allowed.  }

{ Rule b ensures that a unique 'TAU' is associated with each
  module-text accessible from any given point in the program.  This is
  used to ensure that an invoke ATU' can be identified in the nest of
  all descendent constructs of any access-clause or module-text which
  invokes that module-text.

  In general, a module-text-publishing-REVS-defining-LAYER T makes
  'LAYER' visible within itself, and makes the properties revealed by
  'REVS' visible wherever T is accessed.  'LAYER' includes both a
  'DECSETY' corresponding to its public declarations and an INK' which
  links T to its unique associated 'TAU' and signifies in the nest
  that T is now known to be invoked.  REVS' always reveals 'DECSETY
  INKSETY INK' (but not 'DECSETY1'), where INKSETY' signifies the
  invocation of any other modules accessed by T.  'REVS' may also
  reveal the publications of the other modules accessed by T if their
  module-calls within T contained a public-token. }

5 Units

5.1 Syntax

{ Extensions:
  [MR] formal hole, virtual hole }

A) UNIT{32d} ::
     assignation{521a} coercee ; identity relation{522a} coercee ;
     routine text{541a,b} coercee ; jump{544a} ; skip{552a} ;
     and function{57a} ; or function{57b} ;
     formal hole{561b} ; virtual hole{561a} ;
     TERTIARY{B}.
B) TERTIARY{A,521b,522a} ::
     ADIC formula{542a,b} coercee ; nihil ;
     SECONDARY{C}.
C) SECONDARY{B,531a,542c} ::
     LEAP generator{523a} coercee ; selection{531a} coercee ;
     PRIMARY{D}.
D) PRIMARY{C,532a,543a} ::
     slice{532a} coercee ; call{543a} coercee ;
     format text{A341a} coercee ;
     applied identifier with TAG{48b} coercee ;
     ENCLOSED clause{31a,33a,c,d,e,34a,35a}.

a) *SOME hip :
     SOME jump{544a} : SOME skip{552a} ; SOME nihil{524a}.

5.2 Units associated with names

5.2.1 Assignations

5.2.1.1 Syntax

a) REF to MODE NEST assignation{5A} :
     REF to MODE NEST destination{b}, becomes{94c} token,
       MODE NEST source{c}.
b) REF to MODE NEST destination{a} :
     soft REF to MODE NEST TERTIARY{5B}.
c) MODE1 NEST source{a,44d} :
     strong MODE2 NEST unit{32d},
       where MODE1 deflexes to MODE2{47a,b,c,-}.

5.2.2 Identity relations

5.2.2.1 Syntax

a) boolean NEST identity relation{5A} :
     where soft balances SORT1 and SORT2{32f},
       SORT1 reference to MODE NEST TERTIARY1{5B},
       identity relator{b},
       SORT2 reference to MODE NEST TERTIARY2{5B}.
b) identity relator{a} : is{94f} token ; is not{94f} token.

5.2.3 Generators

5.2.3.1 Syntax

a) reference to MODE NEST LEAP generator{5C} :
     LEAP{94d,-} token, actual MODE NEST declarer{46a}.
b) reference to MODINE NEST LEAP sample generator{44e} :
     LEAP{94d,-} token, actual MODINE NEST declarer{44b,46a} ;
     where (LEAP) is (local), actual MODINE NEST declarer{44b,46a}.

5.2.4 Nihils

5.2.4.1 Syntax

a) strong reference to MODE NEST nihil{5B} :
     nil{94f} token.

5.3 Units associated with stowed values

5.3.1 Selections

5.3.1.1 Syntax

A) REFETY :: REF to ; EMPTY.
B) REFLEXETY :: REF to ; REF to flexible ; EMPTY.

a) REFETY MODE1 NEST selection{5C} :
     MODE1 field FIELDS applied field selector with TAG{48d},
       of{94f} token, weak REFLEXETY ROWS of structured with
                           FIELDS mode NEST SECONDARY{5C},
       where (REFETY) is derived from (REFLEXETY){b,c,-}.
b) WHETHER (transient reference to) is derived from
           (REF to flexible){a,532,66a} :
     WHETHER true.
c) WHETHER (REFETY) is derived from (REFETY){a,532a,66a} :
     WHETHER true.

5.3.2 Slices

5.3.2.1 Syntax

A) ROWSETY :: ROWS ; EMPTY.

a) REFETY MODE1 NEST slice{5D} :
     weak REFLEXETY ROWS1 of MODE1 NEST PRIMARY{5D},
       ROWS1 leaving EMPTY NEST indexer{b,c,-} STYLE bracket,
       where (REFETY) is derived from (REFLEXETY){531b,c,-} ;
     where (MODE1) is (ROWS2 of MODE2),
       weak REFLEXETY ROWS1 of MODE2 NEST PRIMARY{5D},
       ROWS1 leaving ROWS2 NEST indexer{b,d,-} STYLE bracket,
       where (REFETY) is derived from (REFLEXETY){531b,c,-}.
b) row ROWS leaving ROWSETY1 ROWSETY2 NEST indexer{a,b} :
     row leaing ROWSETY1 NEST indexer{c,d,-}, and also{94f} token,
       ROWS leaving ROWSETY2 NEST indexer{b,c,d,-}.
c) row leaving EMPTY NEST indexer{a,b} : NEST subscript{3}.
d) row leaving row NEST indexer{a,b} :
     NEST trimmer{f} ; NEST revised lower bound{g} option.
e) NEST subscript{c} : meek integral NEST unit{32d}.
f) NEST trimmer{d} :
     NEST lower bound{46m} option, up to{94f} token,
       NEST upper bound{46n} option,
       NEST revised lower bound{g} option.
g) NEST revised lower bound{d,f} :
     at{94f} token, NEST lower bound{46m}.

h) *trimscript :
     NEST subscript{e} ; NEST trimmer{f};
     NEST revised lower bound{g} option.
i) *indexer :
     ROWS leaving ROWSETY NEST indexer{b,c,d}.
j) *boundscript :
     NEST subscript{e} ; NEST lower bound{46m} ;
     NEST upper bound{46n} ; NEST revised lowe bound{g}.

5.4 Units associated with routines

5.4.1 Routine texts

5.4.1.1 Syntax

a) procedure yielding MOID NEST1 routine text{44d,5A} :
     formal MOID NEST1 declarer{46b}, routine{94f} token,
       strong MOID NEST1 unit{32d}.
b) procedure with PARAMETERS yielding
             MOID NEST1 routine text{44d,5A} :
     NEST1 new DECS2 declarative defining
           new DECS2{e} brief pack,
       where DECS2 like PARAMETERS{c,d,-},
       formal MOID NEST1 declarer{46b}, routine{94f} token,
       strong MOID NEST1 new DECS2 unit{32d}.
c) WHETHER DECS DEC like PARAMETERS PARAMETER{b,c} :
     WHETHER DECS like PARAMETERS{c,d-}
             and DEC like PARAMETER{d,-}.
d) WHETHER MODE TAG like MODE parameters{b,c} :
     WHETHER true.
e) NEST2 declarative defining new DECS2{b,e,34j} :
     formal MODE NEST2 declarer{46b},
       NEST2 MODE parameter joined definition of DECS2{41b,c} ;
     where (DECS2) is (DECS3 DECS4),
       formal MODE NEST2 declarer{46b},
       NEST2 MODE parameter joind definition of DECS3{41b,c},
       and also{94f} token, NEST2 declarative defining new DECS4{3}.
f) NEST2 MODE parameter efinition of MODE TAG2{41c} :
     MDOE NEST2 defining identifier with TAG2{48a}.

g) *formal MODE parameter :
     NEST MODE parameter definition of MODE TAG{f}.

5.4.2 Formulas

5.4.2.1 Syntax

A) DYADIC :: priority PRIO.
B) MONADIC :: priority iii iii iii i.
C) ADIC :: DYADIC ; MONADIC.
D) TALLETY :: TALLY ; EMPTY.

a) MOID NEST DYADIC formula{c,5B} :
     MODE1 NEST DYADIC TALLETY operand{c,-},
       procedure with MODE1 parameter MODE2 parameter
                 yielding MOID NEST applied operator with TAD{48b},
       where DYADIC TAD identified in NEST{72a},
       MODE2 NEST DYADIC TALLY operand{c,-}.
b) MOID NEST MONADIC formula{c,5B} :
     procedure with MODE parameter yielding MOID
               NEST applied operator ith TAM{48b},
       MODE NEST MONADIC operand{c}.
c) MODE NEST ADIC operand{a,b} :
     firm MODE NEST ADIC formula{a,b} coercee{61b} ;
     where (ADIC) is (MONADIC), firm MODE NEST SECONDARY{5C}.

d) *MOID formula : MOID NEST ADIC formula{a,b}.
e) *DUO dyadic operator with TAD :
     DUO NEST DEFIED operator with TAD{48a,b}.
f) *MONO monadic operator with TAM :
     MONO NEST DEFIED operator with TAM{48a,b}.
g) *MODE operand : MODE NEST ADIC operand{c}.

5.4.3 Calls

5.4.3.1 Syntax

a) MOID NEST call{5D} :
     meek procedure with PARAMETERS yielding MOID NEST PRIMARY{5D},
       actual NEST PARAMETERS{b,c} brief pack.
b) actual NEST PARAMETERS PARAMETER{a,b} :
     actual NEST PARAMETERS{b,c}, and also{94f} token,
       actual NEST PARAMETER{c}.
c) actual NEST MODE parameter{a,b} : strong MODE NEST unit{32d}.

5.4.4 Jumps

5.4.4.1 Syntax

a) strong MOID NEST jump{5A} :
     go to{b} option,
       label NEST applied identifier with TAG{48b}.
b) go to{a} : STYLE go to{94f,-} token ;
              STYLE go{94f,-} token, STYLE to symbol{94g,-}.

5.5 Units associated with values of any mode

5.5.1 Casts

5.5.1.1 Syntax

a) MOID NEST cast{5D} :
     formal MOID NEST declarer{46b},
       strong MOID NEST ENCLOSED clause{31a,33c,d,e,34a,35a,-}.

5.5.2 Skips

5.5.2.1 Syntax

a) strong MOID NEST skip{5A} : skip{94f} token.

5.6 Holes

5.6.1 Syntax

A) LANGUAGE :: algol sixty eight ; fortran ; c language ; cpp language.
B) ALGOL68 :: algol sixty eight.
C) FORTRAM :: fortran.
D) CLANG :: c language.
E) CPPLANG :: cpp language.
F) DLANG :: d language.

a) strong MOID NEST virtual hole{5A} :
     virtual nest symbol, strong MOID NEST closed clause{31a}.
b) strong MOID NEST formal hole{5A} :
     formal nest{94d} token, MOID LANGUAGE indication{e,f,-},
       hole indication{d}.
c) MOID NEST actual hole{A6a} :
     strong MOID NEST ENCLOSED clause{31a,33a,c,34a,35a,36a,-}.
d) hole indication{b} :
     character denotation{814a} ; row of character denotation{83a}.
e) MOID ALGOL68 indication{b} : EMPTY.
f) MOID FORTRAN indication{b} : bold letter f letter o letter r letter t
                                     letter r letter a letter n token.
g) MOID CLANG indication{b} : bold letter c letter l letter a letter n
                                   letter g.
e) MOID CPPLANG indication{b} : bold letter c letter p letter p letter l
                                     letter a letter n letter g.
f) MOID DLANG indication{b} : bold letter d letter l letter a letter n
                                   letter g.

{ Since no representation is provided for the virtual-nest-symbol, the
  user is unable to construct virtual-holes for himself, but a
  mechanism is provided (10.6.2.a) for constructing them out of
  formal- and actual-holes. }

5.7 Short-circuit logical functions

{ Extensions: [SC] }

{ The short-circuit logical functions are pseudo-operators providing
  logical AND and OR functions with short-circuited elaboration.  }

5.7.1 Syntax

a) boolean NEST and function{5A} :
     meek boolean NEST TERTIARY1, andth{94c} token, meek boolean NEST TERTIARY2.

b) boolean NEST or function{5A} :
     meek boolean NEST TERTIARY1, orel{94c} token, meek boolean NEST TERTIARY2.

c) *boolean NEST short circuit function :
     boolean NEST and function{a} ; boolean NEST or function{b}.

{ Examples:
    a) UPB str > 2 ANDTH str[3] /= "x"
    b) error = 0 OREL (print ("error"); stop; SKIP) }

6 Coercion

6.1 Coercees

6.1.1 Syntax

A) STRONG{a,66a} ::
     FIRM{B} ; widened to{65a,b,c,d} ; rowed to{66a} ;
     voided for{67a,b}.
B) FIRM{A,b} :: MEEK{C} ; united to{64a}.
C) MEEK{B,c,d,64a,63a,64a,65a,b,c,d} ::
     unchanged from{f} ; dereferenced to{62a} ; deprocedured to{63a}.
D) SOFT{e,63b} ::
     unchanged from{f} ; softly deprocedured to{63b}.
E) FORM :: MORF ; COMORF.
F) MORF ::
     NEST selection ; NEST slice ; NEST routine text ;
     NEST ADIC formula ; NEST call ;
     NEST applied identifier with TAG.
G) COMORF ::
     NEST assignation ; NEST identity relation ;
     NEST LEAP generator ; NEST cast ; NEST denoter ;
     NEST format text.

a) strong MOID FORM coercee{5A,B,C,D,A341i} :
     where (FORM) is (MORF), STRONG{A} MOID MORF ;
     where (FORM) is (COMORF), STRONG{A} MOID COMORF,
       unless (STRONG MOID) is (deprocedured to void).
b) firm MODE FORM coercee{5A,B,C,D,542c} : FIRM{B} MODE FORM.
c) meek MODE FORM coercee{5A,B,C,D} : MEEK{C} MOID FORM.
d) weak REFETY STOWED FORM coercee{5A,B,C,D} :
     MEEK{C} REFETY STOWED FORM,
       unless (MEEK) is (dereferenced to)
              and (REFETY) is (EMPTY).
e) soft MODE FORM coercee{5A,B,C,D} : SOFT{D} MODE FORM.
f) unchanged from MOID FORM{C,D,67a,b} : MOID FORM.

g) *SORT MOID coercee : SORT MOID FORM coercee{a,b,c,d,e}.
h) *MOID coercend : MOID FORM.

{ Examples:
    a) 3.14 (in x := 3.14)
    b) 3.14 (in x + 3.14)
    c) sin (in sin (x))
    d) x1 (in x1[2] := 3.14)
    e) x (in x := 3.14)  }

6.2 Dereferencing

6.2.1 Syntax

a) dereferenced to{61C} MODE1 FORM :
     MEEK{61C} REF to MODE2 FORM,
       where MODE2 deflexes to MODE1{47a,b,c,-}.

{ Examples:
    a) x in (real (x)) }

6.3 Deproceduring

6.3.1 Syntax

a) deprocedured to{61C,67a} MOID FORM :
     MEEK{61C} procedure yielding MOID FORM.
b) softly deprocedured to{61D} MODE FORM :
     SOFT{61D} procedure yielding MODE FORM.

{ Examples:
    a) random (in real (random))
    b) x or y (in x or y := 3.14, given
               PROC x or y = REF REAL: (random < .5 | x | y)) }

6.4 Uniting

6.4.1 Syntax

a) united to{64B} UNITED FORM :
     MEEK{61C} MOID FORM,
       where MOID unites to UNITED{b}.
b) WHETHER MOID1 unites to MOID2{a,34i,71m} :
     where MOID1 equivalent MOID2{73a}, WHETHER false ;
     unless MOID1 equivalent MOID2{73a},
       WHETHER safe MOODS1 subset of safe MOODS2{73l,m,n},
       where (MOODS1) is (MOID1)
             or (union of MOODS1 mode) is (MOID1),
       where (MOODS2) is (MOIDS2)
             or (union of MOODS2 mode) is (MOIDS2).

{ Examples:
    a) x (in uir := x)
       u (in UNION(CHAR,INT,VOID)(u), in a reach containing
          UNION(INt,VOID) u := EMPTY) }

6.5 Widening

6.5.1 Syntax

A) BITS :: structured with
                      row of boolean field SITHETY letter aleph mode.
B) BYTES :: structured with
                       row of character field SITHETY letter aleph mode.
C) SITHETY :: LENGTH LENGTHETY ; SHORT SHORTHETY ; EMPTY.
D) LENGTH :: letter l letter o letter n letter g.
E) SHORT :: letter s letter h letter o letter r letter t.
F) LENGTHETY :: LENGTH LENGTHETY ; EMPTY.
G) SHORTHETY :: SHORT SHORTHETY ; EMPTY.

a) widened to{b,61A} SIZETY real FORM :
     MEEK{61C} SIZETY integral FORM.
b) widened to{61A} structured with SIZETY real field letter r letter e
           SIZETY real field letter i letter m mode FORM :
     MEEK{61C} SIZETY real FORM ;
     widened to{a} SIZETY real FORM.
c) widened to{61A} row of boolean FORM : MEEK{61C} BIT FORM.
d) widened to{61A} row of character FORM : MEEK{61C} BYTES FORM.

{ Examples:
    a) 1 (in x := 1)
    b) 1.0 (in z := 1.0)
       1 (in z := 1)
    c) 2r101 (in []BOOL(2r101))
    d) r (in []CHAR(r)) }

6.6 Rowing

6.6.1 Syntax

a) rowed to{61A} REFETY ROWS1 of MODE FORM :
     where (ROWS1) is (row),
       STRONG{61A} REFLEXETY MODE FORM,
       where (REFETY) is derived from (REFLEXETY){531b,c,-} ;
     where (ROWS1) is (row ROWS2),
       STRONG{61A} REFLEXETY ROWS2 of MODE FORM,
       where (REFETY) is derived from (REFLEXETY){531b,c,-}.

{ Examples:
    a) 4.13 (in [1:1]REAL b1 := 4.13)
       x1 (in [1:1,1:n]REAL b2 := x1) }

6.7 Voiding

6.7.1 Syntax

A) NONPROC :: PLAIN ; STOWED ; REF to NONPROC ;
              procedure with PARAMETERS yielding MOID ; UNITED.

a) voided to{61A} void MORF :
     deprocedured to{63a} NONPROC MORF ;
     unchanged from{61f} NONPROC MORF.
b) voided to{61A} void COMORF :
     unchanged from{61f} MODE COMORF.

{ Examples:
    a) random (in SKIP; random;)
       next random (last random)
         (in SKIP; next random (lat random);)
    b) PROC VOID (pp) (in PROC PROC VOID pp = PROC VOID : (print (1);
            VOID : print (2)); PROC VOID (pp);) }

8 Denotations

8.1 Plain denotations

8.1.0.1 Syntax

A) SIZE:: long ; short.
B) *NUMERAL :: fixed point numeral ; variable point numeral ;
               floating point numeral.

a) SIZE INTREAL denotation{a,80a} :
     SIZE symbol{94d}, INTREAL, denotation{a,811a,812a}.

b) *plain denotation :
     PLAIN denotation{a,811a,812a,813a,814a} ; void denotation{815a}.

{ Example:
    a) LONG 0 }

8.1.1 Integral denotations

8.1.1.1 Syntax

a) integral denotation{80a,810a} : fixed point numeral{b}.
b) fixed point numeral{a,812c,d,f,i,A341h} : digit cypher{c} sequence.
c) digit cypher{b} : DIGIT symbol{94b}.

{ Examples:
    a) 4096
    b) 4096
    c) 4 }

8.1.2 Real denotations

8.1.2.1 Syntax

a) real denotation{80a,810a} :
     variable point numeral{b} ; floating point numeral{e}.
b) variable point numeral{a,f} :
     integral part{c} option, fractional part{d}.
c) integral part{b} : fixed point numeral{811b}.
d) fractional part{b} : point symbol{94b}, fixed point numeral{811b}.
e) floating point numeral{a} : stagnant part{f}, exponent part{g}.
f) stagnant part{e} :
     fixed point numeral{811b} : variable point numeral{b}.
g) exponent part{e} :
     times ten to the power choice{h}, power of then{i}.
h) times ten to the power choice{g} :
     times ten to the power symbol{94b} ; letter e symbol{94a}.
i) power of ten{g}: plusminus{j} option, fixed point numeral{811b}.
j) plusminus{i} : plus symbol{94c} ; mius symbol{94c}.

{ Examples:
    a) 0.00123
       1.23e-3
    b) 0.00123
    c) 0
    d) .00123
    e) 1.23e-3
    f) 123
       1.23
    g) E-3
    h) E
    i) -3
    j) +
       - }

8.1.3 Boolean denotations

8.1.3.1 Syntax

a) boolean denotation{80a} : true{94b} symbol ; false{94b} smbol.

{ Examples:
    a) TRUE
       FALSE }

8.1.4 Character denotations

8.1.4.1 Syntax

a) character denotation{80a} :
     quote{94b} symbol, string item{b}, quote sybol{94b}.
b) string item{a,83b} :
     character glyph{c} ; quote image symbol{94f} ; other string item{d}.
c) character glyph{b,92c} :
     LETTER symbol{94a} ; DIGIT symbol{94b} ;
     point sybol{94b} ; open symbol{94f} ; close symbol{94f} ;
     comma symbol{94b} ; space symbol{94b} ;
     plus symbol{94c} ; minus symbol{94c}.

{ A production rule may be added for the notion 'other string item'
  each of whose alternatives is a symbol 1.1.3.1.f which is different
  from any terminal production of 'character glyph' and which is not
  'quote symbol' }

{ Examples:
    a) "a"
    b) a
       ""
       ?
    c) a 1 . ( ) , . space + - }

8.1.5 Void denotation

5.1.5.1 Syntax

a) void denotation{80a} : empty{94b} symbol.

{ Example:
    a) EMPTY }

8.2 Bits denotations

8.2.1 Syntax

A) RADIX :: radix two ; radix four ; radix eight ; radix sixteen.

a) structured with row of boolean field
              LENGTH LENGTHETY letter aleph mode denotation{a,80a} :
     long{94d} symbol, structured with row of boolean field
                                  LENGTHETY letter aleph mode denotation{a,c}.
b) structured with row of boolean field
              SHORT SHORTHTETY letter aleph mode denotation{b,80a} :
     short{94d} symbol,
       structured with row of boolean field SHORTHETY letter aleph mode denotation{b,c}.
c) structured wih row of boolean field
              letter aleph mode denotation{a,b,80a} :
     RADIX{d,e,f,g}, letter r symbol{94a}, RADIX digit{h,i,j,k} sequence.
d) radix two{c,A347b} : digit two{94b} symbol.
e) radix four{c,A347b} : digit four{94b} symbol.
f) radix eight{c,A347b} : digit eight{94b} symbol.
g) radix sixteen{c,A347b} : digit one symbol{94b}, digit six symbol{94b}.
h) radix two digit{c,i} : digit zero symbol{94b} ; digit one symbol{94b}.
i) radix four digit{c,j} :
     radix two digit{h} ; digit two symbol{94b} ;
     digit three symbol{94b}.
j) raidx eight digit{c,k} :
     radix four digit{i} ; digit four symbol{94b} ;
     digit five symbol{94b} ; digit six symbol{94b} ;
     digit seven symbol{94b}.
k) radix sixteen digit{c} :
     radix eight digit{j} ; digit eight symbol{94b} ;
     digit nine symbol{94b} ; letter a symbol{94a} ;
     letter b symbol{94a} ; letter e symbol{94a} ; letter d symbol{94a} ;
     letter e symbol{94a} ; letter f symbol{94a}.

l) *bits denotation : BITS denotation{a,b,c}.
m) *radix digit : RADIX digit{h,i,j,k}.

{ Examples:
    a) LONG 2r101
    b) SHORT 16rffff
    c) 8r231 }

8.3 String denotations

8.3.1 Syntax

a) row of character denotation{80a} :
     quote{94b} symbol, string{b} option, quote symbol{94b}.
b) string{a} : string item{814b}, string item{814b} sequence.

c) *string denotation : row of charater denotation{a}.

{ Examples:
    a) "abc"
    b) abc }

9 Tokens and symbols

9.1 Tokens

{ Tokens are symbols possibly preceded by pragments.  }

9.1.1 Syntax

a) CHOICE STYLE start{34a} :
     where (CHOICE) is (choice using boolean),
       STYLE if{94f,-} token ;
     where (CHOICE) is (CASE), STYLE case{94f,-} token.
b) CHOICE STYLE in{34e} :
     where (CHOICE) is (choice using boolean),
       STYLE then{94f,-} token ;
     where (CHOICE) is (CASE), STYLE in{94f,-} token.
c) CHOICE STYLE again{34l} :
     where (CHOICE) is (choice using boolean),
       STYLE else if{94f,-} token ;
     where (CHOICE) is (CASE), STYLE ouse{94f,-} token.
d) CHOICE STYLE out{34l} :
     where (CHOICE) is (choice using boolean),
       STYLE else{94f,-} token ;
     where (CHOICE) is (CASE), STYLE out{94f,-} token.
e) CHOICE STYLE finish{34a} :
     whre (CHOICE) is (choice using boolean),
       STYLE fi{94f,-} token ;
     where (CHOICE) is (CASE), STYLE esac{94f,-} token.
f) NOTION token :
     pragment{92a} sequence option,
       NOTION symbol{94a,b,c,d,e,f,g,h}.

g) *token : NOTION token{f}.
h) *symbol : NOTION symbol{94a,b,c,d,e,f,g,h}.

9.2 Comments and pragmats

9.2.1 Syntax

{ Extensions:
  [NC] nestable comments.  }

A) PRAGMENT :: pragmat ; comment.

a) pragment{80a,91f,A341b,h,A348a,b,c,A349a,A34Ab} : PRAGMENT{b}.
b) PRAGMENT{a} :
     STYLE PRAGMENT symbol{94h,-},
       STYLE PRAGMENT item{c} sequence option,
       STYLE PRAGMENT symbol{94h,-} ;
     STYLE nestable comment{d}.
c) STYLE PRAGMENT item{b} :
     character glyph{814c} ; STYLE other PRAGMENT item{d}.
d) STYLE nestable comment{b} :
     STYLE comment begin symbol{94h,-},
       STYLE nestable comment contents{e} sequence,
       STYLE comment end symbol{94h,-}.
e) STYLE nestable comment contents{d} :
     STYLE nestable comment item{c} sequence option,
       STYLE nestable comment{d} option.
f) STYLE nestable comment item{e} :
     character glyph{814c} ; STYLE other nestable comment item{d}.

{ A production rule may be added for each notion designated by 'STYLE
  other PRAGMENT item' each of whose alternatives is a symbol
  different from any terminal production of 'character glyph', and
  such that no terminal production of any 'STYLE other PRAGMENT item'
  is the corresponding 'STYLE PRAGMENT symbol'.  This allows to nest
  different comment or pragmat for example.  }

9.4 The reference language

9.4.1 Representations of symbols

{ Extensions:
  [CS] andth symbol, orel symbol
  [MR] access symbol, module symbol, def symbol, public symbol,
       postlude symbol, formal nest symbol, egg symbol
  [US] unsafe symbol }

{ This section of the Report doesn't describe syntax, but lists all
  the different symbols along with their representation in the
  reference language.  We only include here symbols corresponding to
  the GNU extensions implemented by this compiler.  }

        symbol                         representation

c) andth symbol{57a}                    ANDTH
   orel symbol{57b}                     OREL
d) module symbol{49a}                   MODULE
   access symbol{36b}                   ACCESS
   def symbol{49c}                      DEF
   fed symbol{49c}                      FED
   public symbol{36d,41e}               PUB
   postlude symbol{49f}                 POSTLUDE
   formal nest symbol{56b}              NEST
   egg symbol{A6a,c}                    EGG
f) unsafe symbol{37a}                   UNSAFE
h) bold comment begin symbol{92a}       NOTE
   bold comment end symbol{92a}         ETON
   brief comment begin symbol{92a}      {
   brief comment end symbol{92a}        }

10.1.1 Syntax

{ Extensions:
  [MR] user, user task }

A) EXTERNAL :: user.

f) NEST1 user task{d} :
     NEST2 particular prelude with DECS{c},
       NEST2 user prelude with MODSETY{c},
       NEST2 particular program{g} PACK, go on{94f} token,
       NEST2 particualr poslude{i},
       where (NEST2) is (NEST1 new DECS MODSETY STOP).

10.6 Packets

10.6.1 Syntax

a) MOID NEST new MODSETY ALGOL68 stuffing packet{A7a} :
     egg{94d} token, hole indication{56d}, is defined as{94d} token,
       MOID NEST new MODSETY actual hole{56c}.

{ b) Note that the rules for "MOID NEST new MODSETY LANGUAGE stuffing
  packets" for other languages are not explicitly included in the
  syntax.  These rules conceptually transform all such
  LANGUAGE-stuffing-packets into ALGOL68-stuffing-packets with the
  same meaning. }

c) NEST new MODSETY1 MODS definition module packet of MODS{A7a} :
     egg{94d} token, hole indication{56d}, is defined as{94d} token,
       NEST new MODSETY1 MODS module declaration of MODS{49a},
       where MODS absent from NEST{e}.
d) new LAYER1 new DECS MODSETY1 MODS STOP
       prelude packet of MODS{A7a} :
     new LAYER1 new DECS MODSETY1 MODS STOP
         module declaration of MODS{4a},
       where MODS absent from new LAYER1{e}.
e) WHETHER MODSETY MOD absent from NEST{c,d} :
     WHETEHR MODSETY absent from NEST{e,f}
             and MOD independent PROPSETY{71a,b,c},
       where PROPSETY collected properties from NEST{g,h}.
f) WHETHER EMPTY absent from NEST{e} :
     WHETHER true.
g) WHETHER PROPSETY1 PROPSETY2 collected properties from
           NEST new PROPSETY2{e,g} :
     WHETHER PROPSETY1 collected properties from NEST {g,h}.
h) WHETHER EMPTY collected properties from new EMPTY{e,g} :
     WHETHER true.

i) *NEST new PROPSETY packet :
     MOID NEST new PROPSETY LANGUAGE stuffing packet{a,b} ;
     NEST new PROPSETY definition module packet of MODS{c} ;
     NEST new PROPSETY particular program{A1g} ;
     NEST new PROPSETY prelude packet of MODS{d}.
j) *letter symbol : LETTER symbol{94a}.
k) *digit symbol : DIGIT symbol{94b}.

{ Examples:

    a) EGG "abc" = ACCESS A,B (x := 1; y := 2; print (x+y))
    c) EGG "abc" = MODULE A = DEF PUB REAL x FED
    d) MODULE B = DEF PUB REAL y FED

  The thre examples above would form a compatible collection of
  packets when taken in conjunction with the particular-program BEGIN
  NEST "abc" END }

{ In rule a above, 'MODSETY' envelops the 'MOD's defined by al the
  definition-module-packets that are being stuffed along with the
  stuffing-packet.

  In rules c and d, 'MODSETY1' need only envelop the 'MOD's for those
  modules actually accessed from within that packet.

  The semantics related to packets are only defined if, for a
  collection of packets being stuffed together, all the 'MOD's
  enveloped by the various 'MODSETY1's are enveloped by 'MODSETY'.  }

{ A stuffing packet contains the definition of an actual-hole.  For
  Algol 68 this consists on an enclosed-clause.  For other values of
  the metanotion 'LANGUAGE' it is different, and it is expected to be
  translated somehow to an equivalent Algol 68 definition,
  conceptually naturally.

  A definition module packet contains the definition of an actual-hole
  which consists in one or more joined module declarations, with the
  restriction that none of the declared modules shall exist in the
  static environment at the formal-hole.

  A prelude packet contains one or more joined module declarations.  }

10.7 Compilation systems

{ An implementtion of Algol 68 in which packets of a collection are
  compiled into a collection of object-modules should conform to the
  provisions of this section.  }

10.7.1 Syntax

{ Note that we use the notion "compilation unit" rather than the
  original "compilation input" used in the IFIP modules definition.  }

A) *LAYERS :: LAYER ; LAYERS LAYER.

a) compilation unit :
     MOID NEST new MODSETY LANGUAGE stuffing packet{A6a,b},
       MOID NEST hole interface{d},
       joined module interface with MODSETY{b,c} ;
     NEST new MODSETY1 MODS definition module packet of MODS{A6c},
       MOID NEST hole interface{d},
       joined module interface with MODSETY1{b,c},
       module interface with MODS{d} option ;
     new LAYER1 new DECS MODSETY STOP particular program{A1g},
       { void new LAYER1 new DECS STOP hole interface,}
       unless (DECS) contains (MODULE),
       joined module interface with MODSETY{b,c} ;
     new LAYER1 new DECS MODSETY1 MODS STOP
         prelude packet of MODS{A6d},
       { void new LAYER1 new DECS STOP hole interface,}
       unless (DECS) contains (module),
       joined module interface with MODSETY1{b,c},
       module interface with MODS{d} option.
b) joined module interface with MODS MODSETY{a,b} :
     module interface with MODS{d},
       joined module interface with MODSETY{b,c}.
c) joined module inteface with EMPTY{a,b} : EMPTY.

{ A compilation-unit is either a stuffing packet, a definition module
  packet, a particular program, or a prelude packet.  The packets
  shall be accompanied by the required hole and module interface
  information.  }

{ d) Hyper-rules for "MOID NEST hole interface", "module interface
     with MODS" and "MOID NEST object module".  The terminal
     productions will most likely be in some cryptic notation
     understood only by the compiler, i.e. the interface data. }

{ The inclusion of the hypernotions "void new LAYER1 new DECS STOP
  hole interface" within pragmatic remarks in rule a is intended to
  signify that this information (which describes the standard
  environment) must clearly be available to the compiler, but that it
  may well not be provided in the form of an explicit
  hole-interface. }
