(* Strings.mod provides a Logitech-3.0 compatible library.

Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Strings ;

FROM ASCII IMPORT nul ;
IMPORT StrLib ;
IMPORT DynamicStrings ;


(*
   Assign - source := dest.
*)

PROCEDURE Assign (VAR dest: ARRAY OF CHAR; source: ARRAY OF CHAR) ;
BEGIN
   StrLib.StrCopy(source, dest)
END Assign ;


(*
   Insert - insert the string, substr, into str at position, index.
            substr, is added to the end of, str, if, index >= length(str)
*)

PROCEDURE Insert (substr: ARRAY OF CHAR; VAR str: ARRAY OF CHAR;
                  index: CARDINAL) ;
VAR
   s1, s2: DynamicStrings.String ;
BEGIN
   IF index>Length(str)
   THEN
      ConCat(str, substr, str)
   ELSE
      s1 := DynamicStrings.InitString(str) ;
      s2 := DynamicStrings.ConCat(DynamicStrings.Slice(s1, 0, index),
                                  DynamicStrings.Mark(DynamicStrings.InitString(str))) ;
      s2 := DynamicStrings.ConCat(s2, DynamicStrings.Slice(s1, index, 0)) ;
      DynamicStrings.CopyOut(str, s2) ;
      s1 := DynamicStrings.KillString(s1) ;
      s2 := DynamicStrings.KillString(s2)
   END
END Insert ;


(*
   Delete - delete len characters from, str, starting at, index.
*)

PROCEDURE Delete (VAR str: ARRAY OF CHAR; index: CARDINAL; length: CARDINAL) ;
VAR
   s: DynamicStrings.String ;
BEGIN
   s := DynamicStrings.InitString(str) ;
   s := DynamicStrings.ConCat(DynamicStrings.Mark(DynamicStrings.Slice(s, 0, index)),
                              DynamicStrings.Mark(DynamicStrings.Slice(s, index+length, 0))) ;
   DynamicStrings.CopyOut(str, s) ;
   s := DynamicStrings.KillString(s)
END Delete ;


(*
   PosLower - return the first position of substr in str.
*)

PROCEDURE PosLower (substr, str: ARRAY OF CHAR) : CARDINAL ;
VAR
   i, strLen, substrLen   : INTEGER ;
   strS, substrS, scratchS: DynamicStrings.String ;
BEGIN
   strS := DynamicStrings.InitString (str) ;
   substrS := DynamicStrings.InitString (substr) ;
   strLen := DynamicStrings.Length (strS) ;
   substrLen := DynamicStrings.Length (substrS) ;
   i := 0 ;
   REPEAT
      i := DynamicStrings.Index (strS, DynamicStrings.char (substrS, 0), i) ;
      IF i < 0
      THEN
         (* No match on first character therefore return now.  *)
         strS := DynamicStrings.KillString (strS) ;
         substrS := DynamicStrings.KillString (substrS) ;
         scratchS := DynamicStrings.KillString (scratchS) ;
         RETURN( HIGH (str) + 1 )
      ELSIF i + substrLen <= strLen
      THEN
         scratchS := DynamicStrings.Slice (strS, i, i + substrLen) ;
         IF DynamicStrings.Equal (scratchS, substrS)
         THEN
            strS := DynamicStrings.KillString (strS) ;
            substrS := DynamicStrings.KillString (substrS) ;
            scratchS := DynamicStrings.KillString (scratchS) ;
            RETURN( i )
         END ;
         scratchS := DynamicStrings.KillString (scratchS)
      END ;
      INC (i)
   UNTIL i >= strLen ;
   strS := DynamicStrings.KillString (strS) ;
   substrS := DynamicStrings.KillString (substrS) ;
   scratchS := DynamicStrings.KillString (scratchS) ;
   RETURN( HIGH (str) + 1 )
END PosLower ;


(*
   Pos - return the first position of substr in str.
         If substr is not found in str then it returns
         HIGH (str) + 1.
*)

PROCEDURE Pos (substr, str: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   IF Length (substr) <= Length (str)
   THEN
      RETURN PosLower (substr, str)
   END ;
   RETURN( HIGH (str) + 1 )
END Pos ;


(*
   Copy - copy at most, length, characters in, substr, to, str,
          starting at position, index.
*)

PROCEDURE Copy (str: ARRAY OF CHAR;
                index, length: CARDINAL; VAR result: ARRAY OF CHAR) ;
VAR
   s1, s2: DynamicStrings.String ;
BEGIN
   s1 := DynamicStrings.InitString (str) ;
   s2 := DynamicStrings.Slice (s1, index, index+length) ;
   DynamicStrings.CopyOut (result, s2) ;
   s1 := DynamicStrings.KillString (s1) ;
   s2 := DynamicStrings.KillString (s2)
END Copy ;


(*
   ConCat - concatenates two strings, s1, and, s2
            and places the result into, dest.
*)

PROCEDURE ConCat (s1, s2: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR) ;
BEGIN
   StrLib.StrConCat(s1, s2, dest)
END ConCat ;


(*
   Length - return the length of string, s.
*)

PROCEDURE Length (s: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   RETURN( StrLib.StrLen(s) )
END Length ;


(*
   CompareStr - compare two strings, left, and, right.
*)

PROCEDURE CompareStr (left, right: ARRAY OF CHAR) : INTEGER ;
BEGIN
   IF StrLib.StrLess(left, right)
   THEN
      RETURN( -1 )
   ELSIF StrLib.StrEqual(left, right)
   THEN
      RETURN( 0 )
   ELSE
      RETURN( 1 )
   END
END CompareStr ;


END Strings.
