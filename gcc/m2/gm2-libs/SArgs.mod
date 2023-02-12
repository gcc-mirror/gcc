(* SArgs.mod provides a String interface to the command line arguments.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE SArgs ;

FROM SYSTEM IMPORT TSIZE, ADDRESS ;
FROM UnixArgs IMPORT GetArgC, GetArgV ;

FROM DynamicStrings IMPORT InitStringCharStar,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

TYPE
   PtrToChar      = POINTER TO CHAR ;
   PtrToPtrToChar = POINTER TO PtrToChar ;

(*
#undef GM2_DEBUG_SARGS
if defined(GM2_DEBUG_SARGS)
#  define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#  define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#  define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#  define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#  define Dup(X) DupDB(X, __FILE__, __LINE__)
#  define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
#endif
*)


(*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
            If TRUE is returned then the string, s, contains a
            new string, otherwise s is set to NIL.
*)

PROCEDURE GetArg (VAR s: String; n: CARDINAL) : BOOLEAN ;
VAR
   i  : INTEGER ;
   ppc: PtrToPtrToChar ;
BEGIN
   i := VAL (INTEGER, n) ;
   IF i < GetArgC ()
   THEN
      (* ppc := ADDRESS (VAL (PtrToPtrToChar, ArgV) + (i * CARDINAL (TSIZE(PtrToChar)))) ; *)
      ppc := ADDRESS (PtrToChar (GetArgV ()) + (n * TSIZE (PtrToChar))) ;
      s   := InitStringCharStar (ppc^) ;

      RETURN TRUE
   ELSE
      s := NIL ;
      RETURN FALSE
   END ;
END GetArg ;


(*
   Narg - returns the number of arguments available from
          command line.
*)

PROCEDURE Narg () : CARDINAL ;
BEGIN
   RETURN GetArgC ()
END Narg ;


END SArgs.
