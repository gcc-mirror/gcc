(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
IMPLEMENTATION MODULE Chance ;


FROM Args IMPORT GetArg ;
FROM NumberIO IMPORT StrToCard ;

FROM StrIO IMPORT WriteString, WriteLn ;

(* FROM Random IMPORT RandomCard ; *)


CONST
   MaxCard   = 65535 ;
   MaxRandom =  8000 ;
   MaxIndex  =   500 ;

TYPE
   Index  = RECORD
               Start,               (* Start of the Random list *)
               End  : CARDINAL ;    (* End of the Random list   *)
            END ;

VAR
   RandomIndex: ARRAY [0..MaxIndex] OF Index ;
   Random     : ARRAY [1..MaxRandom] OF CARDINAL ;
   NoOfRandom : CARDINAL ;   (* Number of random numbers in array Coords *)
   NoOfIndices: CARDINAL ;   (* Number of indices in RandomIndex         *)


(*
   InitRandom - Initializes a potential list of random numbers.
                An index to this potential random number list is returned.
*)

PROCEDURE InitRandom () : CARDINAL ;
BEGIN
   IF NoOfIndices=MaxIndex
   THEN
      WriteString('Too many random list indices in Module Chance') ;
      WriteLn ;
      HALT
   ELSE
      INC(NoOfIndices) ;
      WITH RandomIndex[NoOfIndices] DO
         Start := NoOfRandom+1 ;
         End := 0
      END ;
      Add(NoOfIndices, 0) ;     (* Dummy random no. that we keep *)
      RETURN(NoOfIndices)       (* for the life of this list.    *)
   END
END InitRandom ;


(*
   KillRandom - Kills a complete list of random numbers.
*)

PROCEDURE KillRandom (RandomListIndex: CARDINAL) ;
BEGIN
   IF NoOfIndices>0
   THEN
      (* Destroy index to Random list *)
      WITH RandomIndex[RandomListIndex] DO
         Start := 0 ;
         End := 0
      END ;
      (*
         If killed last Random index list see if we can garbage collect
         previously killed middle indices.
      *)
      IF NoOfIndices=RandomListIndex
      THEN
         REPEAT
            DEC(NoOfIndices)
         UNTIL (NoOfIndices=0) OR (RandomIndex[NoOfIndices].Start#0)
      END ;
      NoOfRandom := RandomIndex[NoOfIndices].End
   ELSE
      WriteString('All Random lists have been killed - Module Chance') ;
      WriteLn ;
      HALT
   END
END KillRandom ;


(*
   AddRandom - places a list of numbers 1..n into the specified list.
*)

PROCEDURE AddRandom (RandomListIndex: CARDINAL; n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      Add(RandomListIndex, n) ;
      DEC(n)
   END
END AddRandom ;


PROCEDURE Add (RandomListIndex: CARDINAL; i: CARDINAL) ;
BEGIN
   IF NoOfRandom=MaxRandom
   THEN
      WriteString('Too many random numbers in a list in Module Chance') ;
      WriteLn ;
      HALT
   ELSE
      INC(NoOfRandom) ;
      Random[NoOfRandom] := i ;
      WITH RandomIndex[RandomListIndex] DO
         End := NoOfRandom
      END
   END
END Add ;


(*
   GetAndDeleteRandom - Returns a random number from the
                        list and then it is deleted.
*)

PROCEDURE GetAndDeleteRandom (RandomListIndex: CARDINAL) : CARDINAL ;
VAR
   i, j: CARDINAL ;
BEGIN
   WITH RandomIndex[RandomListIndex] DO
      i := Start+GetRand(End-Start+1) ;  (* +1 for GetRand *)
      j := i ;
      REPEAT
         IF Random[j]=0
         THEN
            INC(j) ;
            IF j>End
            THEN
               j := Start
            END
         END
      UNTIL (j=i) OR (Random[j]#0) ;
      i := Random[j] ;
      Random[j] := 0        (* Now delete this box *)
   END ;
   RETURN( i )
END GetAndDeleteRandom ;


(*
   GetRand - returns a number between 0..n-1.
             This routine is independant of the above routines.
*)

VAR
   RandomSeed: CARDINAL ;
   Num       : ARRAY [0..9] OF CHAR ;

PROCEDURE GetRand (n: CARDINAL) : CARDINAL ;
BEGIN
   (* $R- *)
   RandomSeed := (RandomSeed*257 + 0ABCDH) MOD MaxCard ;
   (* $R= *)
   RETURN( RandomSeed MOD n )
(*
   IF n<2
   THEN
      RETURN( 0 )  (* return 0  if n=0  or n=1 *)
   ELSE
      RETURN( RandomCard(n) )
   END
*)
END GetRand ;


PROCEDURE Init ;
BEGIN
   NoOfRandom := 0 ;
   NoOfIndices := 0 ;
   WITH RandomIndex[NoOfIndices] DO
      End := 0
   END
END Init ;


BEGIN
   Init
   ; IF GetArg(Num, 1)
   THEN
      StrToCard(Num, RandomSeed)
   ELSE
      RandomSeed := 3
   END
END Chance.
