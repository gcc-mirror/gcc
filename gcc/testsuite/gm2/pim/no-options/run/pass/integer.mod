(* Copyright (C) 2011 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

MODULE integer ;

(*
    Title      : integer
    Author     : Gaius Mulley
    System     : GNU Modula-2
    Date       : Fri May 18 17:05:36 2012
    Revision   : $Version$
    Description: simple test module to test the principles of catching signed and unsigned
                 integer arithmetic overflow.
*)

FROM SYSTEM IMPORT ADDRESS ;
FROM libc IMPORT printf ;
FROM DynamicStrings IMPORT String, InitString, string, KillString, InitString ;

CONST
   Verbose = TRUE ;
   SizeOfIntAndLongSame = TRUE ;


PROCEDURE ssub (i, j: INTEGER) ;
BEGIN
   IF ((j>0) AND (i < MIN(INTEGER)+j)) OR
      ((j<0) AND (i > MAX(INTEGER)+j))
   THEN
      expecting(overflow, 'signed subtraction')
   ELSE
      expecting(none, 'signed subtraction')
   END
END ssub ;


PROCEDURE sadd (i, j: INTEGER) ;
BEGIN
   printf ("i = %d,  j = %d   MIN(INTEGER) = %d\n",
           i, j, MIN(INTEGER));
   printf ("MIN(INTEGER) = %d,  -j = %d\n", MIN(INTEGER), -j);
   IF ((j = MIN(INTEGER)) AND (i < 0)) OR
      ((i = MIN(INTEGER)) AND (j < 0)) OR

      ((j>0) AND (i > MAX(INTEGER)-j)) OR
      ((j<0) AND (i < MIN(INTEGER)-j))
   THEN
      expecting(overflow, 'signed addition')
   ELSE
      expecting(none, 'signed addition')
   END
END sadd ;


(*
   smallMult -
*)

PROCEDURE smallMult (i, j: INTEGER) ;
BEGIN
   IF i>0
   THEN
      IF j>0
      THEN
         IF i>maxInt DIV j
         THEN
            expecting(overflow, 'signed mult')
         ELSE
            expecting(none, 'signed mult')
         END
      ELSE
         IF j<minInt DIV i
         THEN
            expecting(overflow, 'signed mult')
         ELSE
            expecting(none, 'signed mult')
         END
      END
   ELSE
      IF j>0
      THEN
         IF i<minInt DIV j
         THEN
            expecting(overflow, 'signed mult')
         ELSE
            expecting(none, 'signed mult')
         END
      ELSE
         IF (i#0) AND (j<maxInt DIV i)
         THEN
            expecting(overflow, 'signed mult')
         ELSE
            expecting(none, 'signed mult')
         END
      END
   END
END smallMult ;


(*
   smult -
*)

PROCEDURE smult (i, j: INTEGER) ;
VAR
   li, lj, lt: LONGINT ;
BEGIN
   IF SizeOfIntAndLongSame OR (SIZE(LONGINT)=SIZE(INTEGER))
   THEN
      smallMult(i, j)
   ELSE
      li := i ;
      lj := j ;
      lt := li * lj ;
      IF (lt<VAL(LONGINT, minInt)) OR (lt>VAL(LONGINT, maxInt))
      THEN
         expecting(overflow, 'signed multiply')
      ELSE
         expecting(none, 'signed multiply')
      END
   END
END smult ;


(*
   sneg -
*)

PROCEDURE sneg (i: INTEGER) ;
BEGIN
   IF i=minInt
   THEN
      expecting(overflow, 'signed negate')
   ELSE
      expecting(none, 'signed negate')
   END
END sneg ;


(*
   passed -
*)

PROCEDURE expecting (e: error; a: ARRAY OF CHAR) ;
VAR
   s: String ;
   t: ADDRESS ;
BEGIN
   WITH test[testNo] DO
      IF expected#e
      THEN
         s := InitString(a) ;
         t := string(s) ;
         printf("test %s (%d) has failed\n", t, testNo) ;
         s := KillString(s)
      ELSIF Verbose
      THEN
         s := InitString(a) ;
         t := string(s) ;
         printf("test %s (%d) has passed\n", t, testNo) ;
         s := KillString(s)
      END
   END
END expecting ;


(*
   doTest -
*)

PROCEDURE doTest ;
BEGIN
   WITH test[testNo] DO
      CASE op OF

      iadd :  sadd(l, r) |
      isub :  ssub(l, r) |
      ineg :  sneg(l) |
      imult:  smult(l, r) |
      idiv :  |
      imod :  |

      END
   END
END doTest ;


(*
   doTests -
*)

PROCEDURE doTests ;
BEGIN
   testNo := 0 ;
   WHILE testNo<=maxTest DO
      doTest ;
      INC(testNo)
   END
END doTests ;


CONST
   maxTest = 25 ;
   maxInt = MAX(INTEGER) ;
   minInt = MIN(INTEGER) ;

TYPE
   opcode = (iadd, isub, ineg, imult, idiv, imod) ;
   error  = (overflow, underflow, none) ;

   case = RECORD
             l, r    : INTEGER ;
             op      : opcode ;
             expected: error ;
          END ;
   cases = ARRAY [0..maxTest] OF case ;

VAR
   test  : cases ;
   testNo: CARDINAL ;

BEGIN
   test := cases{{minInt, 0, ineg, overflow},
                 (* 1 *)
                 {maxInt, 0, ineg, none},
                 {minInt DIV 2, minInt DIV 2, iadd, none},
                 {minInt DIV 2, minInt DIV 2-1, iadd, overflow},
                 {maxInt DIV 2, maxInt DIV 2, iadd, none},
                 (* 4 *)
                 {maxInt DIV 2, maxInt DIV 2+1, iadd, none},
                 {maxInt DIV 2+1, maxInt DIV 2+1, iadd, overflow},
                 {maxInt, 1, iadd, overflow},
                 {maxInt, 0, iadd, none},
                 (* 8 *)
                 {minInt, -1, iadd, overflow},
                 {minInt, 0, iadd, none},
                 {-1, maxInt, isub, none},
                 {-2, maxInt, isub, overflow},
                 (* 12 *)
                 {minInt, 1, isub, overflow},
                 {minInt, 0, isub, none},
                 {maxInt, -2, isub, overflow},
                 {maxInt, minInt, isub, overflow},
                 (* 16 *)
                 {0, maxInt, isub, none},
                 {0, minInt, isub, overflow},
                 {-1, maxInt, isub, none},
                 {-2, maxInt, isub, overflow},
                 (* 20 *)
                 {maxInt, 2, imult, overflow},
                 {maxInt DIV 2, 2, imult, none},
                 {minInt DIV 2, 2, imult, none},
                 {minInt DIV 2-1, 2, imult, overflow},
		 (* 24 *)
                 {maxInt DIV 3, 3, imult, none},
                 {minInt DIV 3, 3, imult, none}
           } ;
   doTests
END integer.
