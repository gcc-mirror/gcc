(* M2Printf.mod provides a simple printf capability.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Printf ;

FROM SFIO IMPORT WriteS ;
FROM FIO IMPORT StdOut ;
FROM DynamicStrings IMPORT String, string, InitString, KillString, InitStringCharStar, Mark ;
FROM StrLib IMPORT StrLen ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3, Sprintf4 ;
FROM NameKey IMPORT Name, KeyToCharStar ;


(*
   IsDigit - returns TRUE if, ch, is a character 0..9
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch>='0') AND (ch<='9')
END IsDigit ;


(*
   Cast - casts a := b
*)

PROCEDURE Cast (VAR a: ARRAY OF BYTE; b: ARRAY OF BYTE) ;
VAR
   i: CARDINAL ;
BEGIN
   IF HIGH(a) = HIGH(b)
   THEN
      FOR i := 0 TO HIGH(a) DO
         a[i] := b[i]
      END
   ELSE
      HALT
   END
END Cast ;


(*
   TranslateNameToCharStar - takes a format specification string, a, and
                             if it contains %a then this is translated
                             into a String and %a is replaced by %s.
*)

PROCEDURE TranslateNameToCharStar (VAR a: ARRAY OF CHAR;
                                   n: CARDINAL) : BOOLEAN ;
VAR
   argno,
   i, h : CARDINAL ;
BEGIN
   argno := 1 ;
   i := 0 ;
   h := StrLen(a) ;
   WHILE i<h DO
      IF (a[i]='%') AND (i+1<h)
      THEN
         IF (a[i+1] = 'a') AND (argno = n)
         THEN
            a[i+1] := 's' ;
            RETURN TRUE
         END ;
         INC (argno) ;
         IF argno>n
         THEN
            (* all done *)
            RETURN FALSE
         END
      END ;
      INC (i)
   END ;
   RETURN FALSE
END TranslateNameToCharStar ;


(*
   fprintf0 - writes out an array to, file, after the escape sequences
              have been translated.
*)

PROCEDURE fprintf0 (file: File; a: ARRAY OF CHAR) ;
BEGIN
   IF KillString (WriteS (file, Sprintf0 (InitString(a)))) = NIL
   THEN
   END
END fprintf0 ;


PROCEDURE fprintf1 (file: File; a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   s, t: String ;
   n   : Name ;
BEGIN
   IF TranslateNameToCharStar (a, 1)
   THEN
      Cast (n, w) ;
      s := Mark (InitStringCharStar (KeyToCharStar (n))) ;
      t := Mark (InitString (a)) ;
      s := Sprintf1 (t, s)
   ELSE
      t := Mark (InitString (a)) ;
      s := Sprintf1 (t, w)
   END ;
   IF KillString (WriteS (file, s)) = NIL
   THEN
   END
END fprintf1 ;


PROCEDURE fprintf2 (file: File; a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) ;
VAR
   n     : Name ;
   s,
   s1, s2: String ;
   b     : BITSET ;
BEGIN
   b := {} ;
   IF TranslateNameToCharStar (a, 1)
   THEN
      Cast (n, w1) ;
      s1 := Mark(InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 1)
   END ;
   IF TranslateNameToCharStar (a, 2)
   THEN
      Cast(n, w2) ;
      s2 := Mark(InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 2)
   END ;
   CASE b OF

   {}   :  s := Sprintf2 (Mark (InitString(a)), w1, w2) |
   {1}  :  s := Sprintf2 (Mark (InitString(a)), s1, w2) |
   {2}  :  s := Sprintf2 (Mark (InitString(a)), w1, s2) |
   {1,2}:  s := Sprintf2 (Mark (InitString(a)), s1, s2)

   ELSE
      HALT
   END ;
   IF KillString (WriteS (file, s)) = NIL
   THEN
   END
END fprintf2 ;


PROCEDURE fprintf3 (file: File; a: ARRAY OF CHAR;
                    w1, w2, w3: ARRAY OF BYTE) ;
VAR
   n            : Name ;
   s, s1, s2, s3: String ;
   b            : BITSET ;
BEGIN
   b := {} ;
   IF TranslateNameToCharStar (a, 1)
   THEN
      Cast (n, w1) ;
      s1 := Mark (InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 1)
   END ;
   IF TranslateNameToCharStar(a, 2)
   THEN
      Cast (n, w2) ;
      s2 := Mark (InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 2)
   END ;
   IF TranslateNameToCharStar(a, 3)
   THEN
      Cast (n, w3) ;
      s3 := Mark (InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 3)
   END ;
   CASE b OF

   {}     :  s := Sprintf3 (Mark (InitString (a)), w1, w2, w3) |
   {1}    :  s := Sprintf3 (Mark (InitString (a)), s1, w2, w3) |
   {2}    :  s := Sprintf3 (Mark (InitString (a)), w1, s2, w3) |
   {1,2}  :  s := Sprintf3 (Mark (InitString (a)), s1, s2, w3) |
   {3}    :  s := Sprintf3 (Mark (InitString (a)), w1, w2, s3) |
   {1,3}  :  s := Sprintf3 (Mark (InitString (a)), s1, w2, s3) |
   {2,3}  :  s := Sprintf3 (Mark (InitString (a)), w1, s2, s3) |
   {1,2,3}:  s := Sprintf3 (Mark (InitString (a)), s1, s2, s3)

   ELSE
      HALT
   END ;
   IF KillString (WriteS (file, s)) = NIL
   THEN
   END
END fprintf3 ;


PROCEDURE fprintf4 (file: File; a: ARRAY OF CHAR;
                    w1, w2, w3, w4: ARRAY OF BYTE) ;
VAR
   n                : Name ;
   s, s1, s2, s3, s4: String ;
   b                : BITSET ;
BEGIN
   b := {} ;
   IF TranslateNameToCharStar (a, 1)
   THEN
      Cast (n, w1) ;
      s1 := Mark (InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 1)
   END ;
   IF TranslateNameToCharStar (a, 2)
   THEN
      Cast (n, w2) ;
      s2 := Mark (InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 2)
   END ;
   IF TranslateNameToCharStar (a, 3)
   THEN
      Cast (n, w3) ;
      s3 := Mark (InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 3)
   END ;
   IF TranslateNameToCharStar (a, 4)
   THEN
      Cast (n, w4) ;
      s4 := Mark (InitStringCharStar (KeyToCharStar (n))) ;
      INCL (b, 4)
   END ;
   CASE b OF

   {}       :  s := Sprintf4 (Mark (InitString (a)), w1, w2, w3, w4) |
   {1}      :  s := Sprintf4 (Mark (InitString (a)), s1, w2, w3, w4) |
   {2}      :  s := Sprintf4 (Mark (InitString (a)), w1, s2, w3, w4) |
   {1,2}    :  s := Sprintf4 (Mark (InitString (a)), s1, s2, w3, w4) |
   {3}      :  s := Sprintf4 (Mark (InitString (a)), w1, w2, s3, w4) |
   {1,3}    :  s := Sprintf4 (Mark (InitString (a)), s1, w2, s3, w4) |
   {2,3}    :  s := Sprintf4 (Mark (InitString (a)), w1, s2, s3, w4) |
   {1,2,3}  :  s := Sprintf4 (Mark (InitString (a)), s1, s2, s3, w4) |
   {4}      :  s := Sprintf4 (Mark (InitString (a)), w1, w2, w3, s4) |
   {1,4}    :  s := Sprintf4 (Mark (InitString (a)), s1, w2, w3, s4) |
   {2,4}    :  s := Sprintf4 (Mark (InitString (a)), w1, s2, w3, s4) |
   {1,2,4}  :  s := Sprintf4 (Mark (InitString (a)), s1, s2, w3, s4) |
   {3,4}    :  s := Sprintf4 (Mark (InitString (a)), w1, w2, s3, s4) |
   {1,3,4}  :  s := Sprintf4 (Mark (InitString (a)), s1, w2, s3, s4) |
   {2,3,4}  :  s := Sprintf4 (Mark (InitString (a)), w1, s2, s3, s4) |
   {1,2,3,4}:  s := Sprintf4 (Mark (InitString (a)), s1, s2, s3, s4)

   ELSE
      HALT
   END ;
   IF KillString (WriteS (file, s)) = NIL
   THEN
   END
END fprintf4 ;


(*
   printf0 - writes out an array to, StdOut, after the escape
             sequences have been translated.
*)

PROCEDURE printf0 (a: ARRAY OF CHAR) ;
BEGIN
   fprintf0 (StdOut, a)
END printf0 ;


PROCEDURE printf1 (a: ARRAY OF CHAR;
                   w: ARRAY OF BYTE) ;
BEGIN
   fprintf1 (StdOut, a, w)
END printf1 ;


PROCEDURE printf2 (a: ARRAY OF CHAR;
                   w1, w2: ARRAY OF BYTE) ;
BEGIN
   fprintf2 (StdOut, a, w1, w2)
END printf2 ;


PROCEDURE printf3 (a: ARRAY OF CHAR;
                   w1, w2, w3: ARRAY OF BYTE) ;
BEGIN
   fprintf3 (StdOut, a, w1, w2, w3)
END printf3 ;


PROCEDURE printf4 (a: ARRAY OF CHAR;
                   w1, w2, w3, w4: ARRAY OF BYTE) ;
BEGIN
   fprintf4 (StdOut, a, w1, w2, w3, w4)
END printf4 ;


END M2Printf.
