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

MODULE InOutBug;

(*
 *  This module tests the implementation of InOut by stress testing
 *  StringConvert (and checks to see that the -Wpim4 flag does cause
 *  any unexpected DIV and MOD results).
 *)

FROM StringConvert IMPORT itos ;
FROM DynamicStrings IMPORT String, InitString, EqualArray, KillString, string ;
FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;


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


PROCEDURE WriteInt (x: INTEGER; n: CARDINAL) : String ;
BEGIN
   RETURN itos(x, n, ' ', FALSE)
END WriteInt ;


VAR
   s     : String ;
   r, res: INTEGER ;
BEGIN
   res := 0 ;

   s := WriteInt(MAX(INTEGER), 0) ;
   r := printf('result of MAX(INTEGER) = %s\n', string(s)) ;
   Assert(EqualArray(s, '2147483647'), __FILE__, __LINE__, 'MAX(INTEGER) in itos') ;
   s := KillString(s) ;

   s := WriteInt(MIN(INTEGER), 0) ;
   r := printf('result of MIN(INTEGER) = %s\n', string(s)) ;
   Assert(EqualArray(s, '-2147483648'), __FILE__, __LINE__, 'MIN(INTEGER) in itos') ;
   s := KillString(s) ;

   s := WriteInt(MIN(INTEGER)+1, 0) ;
   r := printf('result of MIN(INTEGER)+1 = %s\n', string(s)) ;
   Assert(EqualArray(s, '-2147483647'), __FILE__, __LINE__, 'MIN(INTEGER)+1 in itos') ;
   s := KillString(s) ;
   exit(res)
END InOutBug.
