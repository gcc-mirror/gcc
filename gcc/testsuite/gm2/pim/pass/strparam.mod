(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE strparam ;

(*
FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrLen, StrRemoveWhitePrefix ;
*)


PROCEDURE toint (a: ARRAY OF CHAR; VAR x: INTEGER) ;
BEGIN
   x := 0
END toint ;

(*
VAR
   i        : CARDINAL ;
   ok,
   Negative : BOOLEAN ;
   higha    : CARDINAL ;
BEGIN
   StrRemoveWhitePrefix(a, a) ;
   higha := StrLen(a) ;
   i := 0 ;
   Negative := FALSE ;
   ok := TRUE ;
   WHILE ok DO
      IF i<higha
      THEN
         IF a[i]='-'
         THEN
            INC(i) ;
            Negative := NOT Negative
         ELSIF (a[i]<'0') OR (a[i]>'9')
         THEN
            INC(i)
         ELSE
            ok := FALSE
         END
      ELSE
         ok := FALSE
      END
   END ;
   x := 0 ;
   IF i<higha
   THEN
      ok := TRUE ;
      REPEAT
         IF Negative
         THEN
            x := 10*x - INTEGER(ORD(a[i])-ORD('0'))
         ELSE
            x := 10*x + INTEGER(ORD(a[i])-ORD('0'))
         END ;
         IF i<higha
         THEN
            INC(i) ;
            IF (a[i]<'0') OR (a[i]>'9')
            THEN
               ok := FALSE
            END
         ELSE
            ok := FALSE
         END
      UNTIL NOT ok ;
   END
END toint ;
*)

END strparam.
