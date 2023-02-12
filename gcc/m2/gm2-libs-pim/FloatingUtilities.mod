(* FloatingUtilities.mod provides a Logitech compatible library.

Copyright (C) 2005-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE FloatingUtilities ;


(*
   Frac - returns the fractional component of, r.
*)

PROCEDURE Frac (r: REAL) : REAL ;
BEGIN
   RETURN r-VAL(REAL, Int(r))
END Frac ;


(*
   Int - returns the integer part of r. It rounds the value towards zero.
*)

PROCEDURE Int (r: REAL) : INTEGER ;
BEGIN
   IF r>=0.0
   THEN
      RETURN VAL(INTEGER, r)
   ELSE
      RETURN -VAL(INTEGER, -r)
   END
END Int ;


(*
   Round - returns the number rounded to the nearest integer.
           It rounds away from zero.
*)

PROCEDURE Round (r: REAL) : INTEGER ;
BEGIN
   IF r>=0.0
   THEN
      RETURN Int(r+0.5)
   ELSE
      RETURN Int(r-0.5)
   END
END Round ;


(*
   Float - returns a REAL value corresponding to, i.
*)

PROCEDURE Float (i: INTEGER) : REAL ;
BEGIN
   RETURN VAL(REAL, i)
END Float ;


(*
   Trunc - round to the nearest integer not larger in absolute
           value.
*)

PROCEDURE Trunc (r: REAL) : INTEGER ;
BEGIN
   RETURN TRUNC(r)
END Trunc ;


(*
   Fracl - returns the fractional component of, r.
*)

PROCEDURE Fracl (r: LONGREAL) : LONGREAL ;
BEGIN
   RETURN r-VAL(LONGREAL, Intl(r))
END Fracl ;


(*
   Intl - returns the integer part of r. It rounds the value towards zero.
*)

PROCEDURE Intl (r: LONGREAL) : LONGINT ;
BEGIN
   IF r>=0.0
   THEN
      RETURN VAL(LONGINT, r)
   ELSE
      RETURN -VAL(LONGINT, -r)
   END
END Intl ;


(*
   Roundl - returns the number rounded to the nearest integer.
*)

PROCEDURE Roundl (r: LONGREAL) : LONGINT ;
BEGIN
   IF r>=0.0
   THEN
      RETURN Intl(r+0.5)
   ELSE
      RETURN Intl(r-0.5)
   END
END Roundl ;


(*
   Floatl - returns a REAL value corresponding to, i.
*)

PROCEDURE Floatl (i: INTEGER) : LONGREAL ;
BEGIN
   RETURN VAL(LONGREAL, i)
END Floatl ;


(*
   Truncl - round to the nearest integer not larger in absolute
            value.
*)

PROCEDURE Truncl (r: LONGREAL) : LONGINT ;
BEGIN
   RETURN VAL(LONGINT, r)
END Truncl ;


END FloatingUtilities.
