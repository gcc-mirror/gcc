(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE FpuIOBug;

(*
 *  This module tests the implementation of InOut by stress testing
 *  StringConvert (and checks to see that the -Wpim4 flag does cause
 *  any unexpected DIV and MOD results).
 *)

FROM StringConvert IMPORT LongIntegerToString ;
FROM DynamicStrings IMPORT String, InitString, EqualArray, KillString, string ;
FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR, TSIZE ;


PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF v
   THEN
      r := printf("successfully evaluated %s\n", ADR(e))
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;


PROCEDURE WriteLongInt (x: LONGINT; n: CARDINAL) : String ;
BEGIN
   RETURN LongIntegerToString(x, n, ' ', FALSE, 10, TRUE)
END WriteLongInt ;


VAR
   s     : String ;
   r, res: INTEGER ;
BEGIN
   res := 0 ;

   s := WriteLongInt(MAX(LONGINT), 0) ;
   r := printf('result of MAX(LONGINT) = %s\n', string(s)) ;
   IF TSIZE(LONGINT)=4
   THEN
      Assert(EqualArray(s, '2147483647'), __FILE__, __LINE__,
             'MAX(LONGINT) in LongIntegerToString')
   ELSE
      Assert(EqualArray(s, '9223372036854775807'), __FILE__, __LINE__,
             'MAX(LONGINT) in LongIntegerToString')
   END ;

   s := KillString(s) ;

   s := WriteLongInt(MIN(LONGINT), 0) ;
   r := printf('result of MIN(LONGINT) = %s\n', string(s)) ;
   IF TSIZE(LONGINT)=4
   THEN
      Assert(EqualArray(s, '-2147483648'), __FILE__, __LINE__,
             'MIN(LONGINT) in LongIntegerToString')
   ELSE
      Assert(EqualArray(s, '-9223372036854775808'), __FILE__, __LINE__,
             'MIN(LONGINT) in LongIntegerToString')
   END ;

   s := KillString(s) ;

   s := WriteLongInt(MIN(LONGINT)+1, 0) ;
   r := printf('result of MIN(LONGINT)+1 = %s\n', string(s)) ;
   IF TSIZE(LONGINT)=4
   THEN
      Assert(EqualArray(s, '-2147483647'), __FILE__, __LINE__,
             'MIN(LONGINT)+1 in itos')
   ELSE
      Assert(EqualArray(s, '-9223372036854775807'), __FILE__, __LINE__,
             'MIN(LONGINT)+1 in itos')
   END ;

   s := KillString(s) ;
   exit(res)
END FpuIOBug.
