(* Copyright (C) 2009 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE arraycons2 ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM StrLib IMPORT StrEqual ;
FROM FIO IMPORT StdOut, FlushBuffer ;
FROM libc IMPORT exit ;

(*
CONST
   myconst = sigfigArray{tests{ "12"  , "34"},
                         tests{ "56"  , "78"}} ;
*)
TYPE
   tests = RECORD
              i, o: ARRAY [0..maxString] OF CHAR ;
           END ;
   sigfigArray = ARRAY [0..1] OF tests ;

CONST
   maxString = 80 ;

VAR
   a: sigfigArray ;
   e: INTEGER ;
   t: tests ;
BEGIN
   t := tests{ "01", "02" };
(*
   a := myconst ;
   e := 0 ;
   WITH a[0] DO
      WriteString(i) ;
      WriteString('  ') ;
      WriteString(o) ;
      WriteLn ;
      IF NOT StrEqual(i, "12")
      THEN
         e := 1
      END ;
      IF NOT StrEqual(o, "34")
      THEN
         e := 1
      END
   END ;
   WriteLn ;
   WITH a[1] DO
      WriteString(i) ;
      WriteString('  ') ;
      WriteString(o) ;
      WriteLn ;
      IF NOT StrEqual(i, "56")
      THEN
         e := 1
      END ;
      IF NOT StrEqual(o, "78")
      THEN
         e := 1
      END
   END ;
   WriteLn ;
   FlushBuffer(StdOut) ;
   exit(e)
*)
END arraycons2.
