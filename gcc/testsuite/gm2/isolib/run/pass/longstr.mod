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

MODULE longstr ;

FROM DynamicStrings IMPORT String, EqualArray, KillString, InitString ;
FROM ConvStringLong IMPORT RealToFloatString, RealToEngString, RealToFixedString ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteInt ;
FROM FIO IMPORT StdOut, FlushBuffer ;
FROM SFIO IMPORT WriteS ;
FROM libc IMPORT exit ;

TYPE
   floatTests = RECORD
                   f: INTEGER ;
                   r: LONGREAL ;
                   i,
                   o: ARRAY [0..maxString] OF CHAR ;
                   k: kind ;
                END ;
   realArray = ARRAY [0..49] OF floatTests ;
   kind = (fixed, float, eng) ;
   kindArray = ARRAY kind OF BOOLEAN ;

CONST
   maxString = 80 ;

VAR
   j: CARDINAL ;
   s: String ;
   a: realArray ;
   t: kindArray ;
   m: kind ;
   e: INTEGER ;
BEGIN
   e := 0 ;
   a := realArray{floatTests{ 3, 12.3456789  , "12.3456789"  , "12.346"      , fixed},
                  floatTests{ 3, 123.456789  , "123.456789"  , "123.457"     , fixed},
                  floatTests{ 3, 1234.56789  , "1234.56789"  , "1234.568"    , fixed},
                  floatTests{-3, 1234.56789  , "1234.56789"  , "1200"        , fixed},
                  floatTests{-2, 1234.56789  , "1234.56789"  , "1230"        , fixed},
                  floatTests{-1, 1234.56789  , "1234.56789"  , "1235"        , fixed},
                  floatTests{ 0, 1234.56789  , "1234.56789"  , "1235."       , fixed},
                  floatTests{ 1, 1234.56789  , "1234.56789"  , "1234.6"      , fixed},
                  floatTests{ 2, 1234.56789  , "1234.56789"  , "1234.57"     , fixed},

                  floatTests{ 3, 12.3456789  , "12.3456789"  , "12.3"        , eng},
                  floatTests{ 3, 123.456789  , "123.456789"  , "123"         , eng},
                  floatTests{ 3, 1234.56789  , "1234.56789"  , "1.23E+3"     , eng},
                  floatTests{ 3, 12345.6789  , "12345.6789"  , "12.3E+3"     , eng},

                  floatTests{ 3, 1234.56789  , "1234.56789"  , "1.23E+3"     , float},
                  (*
                   *  the following examples are from P445 of the
                   *  ISO standard.
                   *)
                  floatTests{ 1, 3923009.0   , "3923009.0"   , "4E+6"        , float},
                  floatTests{ 2, 3923009.0   , "3923009.0"   , "3.9E+6"      , float},
                  floatTests{ 5, 3923009.0   , "3923009.0"   , "3.9230E+6"   , float},
                  floatTests{ 1, 39.23009    , "39.23009"    , "4E+1"        , float},
                  floatTests{ 2, 39.23009    , "39.23009"    , "3.9E+1"      , float},
                  floatTests{ 5, 39.23009    , "39.23009"    , "3.9230E+1"   , float},
                  floatTests{ 1, 0.0003923009, "0.0003923009", "4E-4"        , float},
                  floatTests{ 2, 0.0003923009, "0.0003923009", "3.9E-4"      , float},
                  floatTests{ 5, 0.0003923009, "0.0003923009", "3.9230E-4"   , float},
                  (*
                   *  the following examples are from P446 of the
                   *  ISO standard.
                   *)
                  floatTests{ 1, 3923009.0   , "3923009.0"   , "4E+6"        , eng},
                  floatTests{ 2, 3923009.0   , "3923009.0"   , "3.9E+6"      , eng},
                  floatTests{ 5, 3923009.0   , "3923009.0"   , "3.9230E+6"   , eng},
                  floatTests{ 1, 39.23009    , "39.23009"    , "40"          , eng},
                  floatTests{ 2, 39.23009    , "39.23009"    , "39"          , eng},
                  floatTests{ 5, 39.23009    , "39.23009"    , "39.230"      , eng},
                  floatTests{ 1, 0.0003923009, "0.0003923009", "400E-6"      , eng},
                  floatTests{ 2, 0.0003923009, "0.0003923009", "390E-6"      , eng},
                  floatTests{ 5, 0.0003923009, "0.0003923009", "392.30E-6"   , eng},
                  (*
                   *  the following examples are from P446 of the
                   *  ISO standard.
                   *)
                  floatTests{-5, 3923009.0   , "3923009.0"   , "3920000"     , fixed},
                  floatTests{-2, 3923009.0   , "3923009.0"   , "3923010"     , fixed},
                  floatTests{-1, 3923009.0   , "3923009.0"   , "3923009"     , fixed},
                  floatTests{ 0, 3923009.0   , "3923009.0"   , "3923009."    , fixed},
                  floatTests{ 1, 3923009.0   , "3923009.0"   , "3923009.0"   , fixed},
                  floatTests{ 4, 3923009.0   , "3923009.0"   , "3923009.0000", fixed},
                  floatTests{-5, 39.23009    , "39.23009"    , "0"           , fixed},
                  floatTests{-2, 39.23009    , "39.23009"    , "40"          , fixed},
                  floatTests{-1, 39.23009    , "39.23009"    , "39"          , fixed},
                  floatTests{ 0, 39.23009    , "39.23009"    , "39."         , fixed},
                  floatTests{ 1, 39.23009    , "39.23009"    , "39.2"        , fixed},
                  floatTests{ 4, 39.23009    , "39.23009"    , "39.2301"     , fixed},
                  floatTests{-5, 0.0003923009, "0.0003923009", "0"           , fixed},
                  floatTests{-2, 0.0003923009, "0.0003923009", "0"           , fixed},
                  floatTests{-1, 0.0003923009, "0.0003923009", "0"           , fixed},
                  floatTests{ 0, 0.0003923009, "0.0003923009", "0."          , fixed},
                  floatTests{ 1, 0.0003923009, "0.0003923009", "0.0"         , fixed},
                  floatTests{ 4, 0.0003923009, "0.0003923009", "0.0004"      , fixed}} ;
   t := kindArray{TRUE, TRUE, TRUE} ;
   FOR j := 0 TO HIGH(a) DO
      WITH a[j] DO
         CASE k OF

         fixed:  s := RealToFixedString(r, f) |
         eng  :  s := RealToEngString(r, f) |
         float:  s := RealToFloatString(r, f)

         END ;
         IF EqualArray(s, o)
         THEN
            WriteString('  passed  ')
         ELSE
            WriteString('**failed**') ;
            t[k] := FALSE
         END ;
         WriteString(' performing a ') ;
         CASE k OF

         fixed:  WriteString('RealToFixedString') |
         eng  :  WriteString('RealToEngString') |
         float:  WriteString('RealToFloatString')

         END ;
         WriteString('(') ;
         WriteString(i) ; WriteString(', ') ; WriteInt(f, 2) ; WriteString(') -> ') ;
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
      END
   END ;
   WriteLn ;
   WriteString('Summary') ; WriteLn ;
   WriteString('=======') ; WriteLn ;
   FOR m := MIN(kind) TO MAX(kind) DO
      WriteString('The ') ;
      CASE m OF

      fixed:  WriteString('fixed') |
      float:  WriteString('float') |
      eng  :  WriteString('engineering')

      END ;
      WriteString(' tests ') ;
      IF t[m]
      THEN
         WriteString('passed')
      ELSE
         WriteString('failed')
      END ;
      WriteLn
   END ;
   FlushBuffer(StdOut) ;
   exit(e)
END longstr.
