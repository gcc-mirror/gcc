(* Copyright (C) 2019 Free Software Foundation, Inc.  *)
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
Boston, MA 02110-1301, USA.  *)

MODULE format ;  (*!m2pim+gm2*)

FROM DynamicStrings IMPORT String, ConCat, InitString, EqualArray, KillString, string ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2 ;
FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADDRESS, ADR ;


(*
   checkDelete -
*)

PROCEDURE checkDelete (s: String; a: ARRAY OF CHAR) ;
VAR
   s1    : String ;
   c1, c2: ADDRESS ;
BEGIN
   c2 := string (s) ;
   s1 := InitString (a) ;
   c1 := string (s1) ;
   IF EqualArray (s, a)
   THEN
      printf ("string test passed, '%s' correcly seen '%s'\n", c1, c2)
   ELSE
      printf ("string test failed, expecting '%s' and seen '%s'\n", c1, c2) ;
      r := 1 ;
   END ;
   s1 := KillString (s1) ;
   s := KillString (s) ;
END checkDelete ;


(*
   test -
*)

PROCEDURE test ;
VAR
   s   : String ;
   i, j: INTEGER ;
   ch  : CHAR ;
BEGIN
   s := Sprintf0 (InitString ("abc")) ;
   checkDelete (s, "abc") ;
   s := Sprintf0 (InitString ("%%")) ;
   checkDelete (s, "%") ;
   s := Sprintf0 (InitString ("%%%%")) ;
   checkDelete (s, "%%") ;
   s := Sprintf0 (InitString ("%z")) ;
   checkDelete (s, "z") ;
   i := 1 ;
   s := Sprintf1 (InitString ("%d"), i) ;
   checkDelete (s, "1") ;
   i := 12 ;
   s := Sprintf1 (InitString ("%d"), i) ;
   checkDelete (s, "12") ;
   i := 123 ;
   s := Sprintf1 (InitString ("%d"), i) ;
   checkDelete (s, "123") ;
   i := 1 ;
   j := 2 ;
   s := Sprintf2 (InitString ("%d %d"), i, j) ;
   checkDelete (s, "1 2") ;
   s := Sprintf2 (InitString ("%% %d %d"), i, j) ;
   checkDelete (s, "% 1 2") ;
   ch := 'a' ;
   s := Sprintf1 (InitString ("%c"), ch) ;
   checkDelete (s, "a") ;
   s := Sprintf2 (InitString ("%c%c"), ch, ch) ;
   checkDelete (s, "aa")
END test ;


VAR
   r: INTEGER ;
BEGIN
   r := 0 ;
   test ;
   exit (r)
END format.
