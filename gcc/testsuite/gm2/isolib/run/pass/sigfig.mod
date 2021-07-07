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

MODULE sigfig ;

FROM DynamicStrings IMPORT String, EqualArray, KillString, InitString,
                           PushAllocation, PopAllocation ;

FROM StringConvert IMPORT ToSigFig ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM FIO IMPORT StdOut, FlushBuffer ;
FROM SFIO IMPORT WriteS ;
FROM libc IMPORT exit ;

TYPE
   tests = RECORD
              n   : CARDINAL ;
              i, o: ARRAY [0..maxString] OF CHAR ;
           END ;
   sigfigArray = ARRAY [0..6] OF tests ;

CONST
   maxString = 80 ;

VAR
   j: CARDINAL ;
   t,
   s: String ;
   a: sigfigArray ;
   e: INTEGER ;
BEGIN
   a := sigfigArray{tests{ 3, "12.3456789"  , "12.3"},
                    tests{ 4, "12.3456789"  , "12.35"},
                    tests{ 5, "12.3456789"  , "12.346"},
                    tests{ 6, "12.3456789"  , "12.3457"},

                    tests{ 3, "19.99"       , "20.0"},
                    tests{ 3, "99.999"      , "100"},
                    tests{ 3, "99.999"      , "100"}} ;
   e := 0 ;
   FOR j := 0 TO HIGH(a) DO
      PushAllocation ;
      WITH a[j] DO
         t := InitString(i) ;
         s := ToSigFig(t, n) ;
         IF EqualArray(s, o)
         THEN
            WriteString('  passed  ')
         ELSE
            WriteString('**failed**')
         END ;
         WriteString(' ToSigFig(') ;
         WriteString(i) ; WriteString(', ') ; WriteCard(n, 0) ; WriteString(') -> ') ;
         IF EqualArray(s, o)
         THEN
            WriteString(o)
         ELSE
            e := 1 ;  (* failure code *)
            s := WriteS(StdOut, s) ; WriteString(' (it should be: ') ;
            WriteString(o) ; WriteString(')')
         END ;
         WriteLn ;
         s := KillString(s)
      END ;
      PopAllocation(TRUE)
   END ;
   WriteLn ;
   WriteString('The sigfig tests: ') ;
   IF e=0
   THEN
      WriteString('passed')
   ELSE
      WriteString('failed')
   END ;
   WriteLn ;
   FlushBuffer(StdOut) ;
   exit(e)
END sigfig.
