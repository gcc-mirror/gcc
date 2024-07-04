(* LowShort.mod provides access to limits of the gm2 SHORTREAL.

Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE LowShort ;

FROM SYSTEM IMPORT ADDRESS ;
FROM Builtins IMPORT ilogbf, modff, signbitf, scalbnf, huge_valf, nextafterf ;
FROM dtoa IMPORT Mode, strtod, dtoa ;
FROM libc IMPORT free ;
FROM RealMath IMPORT power ;
FROM ConvStringReal IMPORT RealToFloatString ;
FROM StringConvert IMPORT ToSigFig ;

FROM EXCEPTIONS IMPORT ExceptionSource, AllocateSource, RAISE, CurrentNumber,
                       IsCurrentSource, IsExceptionalExecution ;

FROM DynamicStrings IMPORT String, InitString, KillString, Slice, Mark,
                           Mult, InitStringCharStar, Length, ConCat,
                           ConCatChar, InitStringChar, string ;

TYPE
   FloatingPointExceptions = (badparam) ;

VAR
   currentmode: Modes ;


(*
   exponent - returns the exponent value of x
*)

PROCEDURE exponent (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN ilogbf(x)
END exponent ;


(*
   fraction - returns the significand (or significant part) of x
*)

PROCEDURE fraction (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN scalbnf (x, -ilogbf (x))
END fraction ;


(*
   sign - returns the signum of x.  sign(x) = 1.0  for all x>0.0
          sign(x) = -1.0  for all x<0.0.
          may be either -1.0 or 1.0 if x = 0.0
*)

PROCEDURE sign (x: SHORTREAL) : SHORTREAL ;
BEGIN
   IF signbitf(x)=0
   THEN
      RETURN 1.0
   ELSE
      RETURN -1.0
   END
END sign ;


(*
   succ - returns the next value of the type REAL greater than x
*)

PROCEDURE succ (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN nextafterf(x, huge_valf())
END succ ;


(*
   ulp - returns the value of a unit in the last place of x.
         So either:

         ulp(x) = succ(x)-x     or
         ulp(x) = x-pred(x)     or both are true.

         if the value does not exist then an exception is raised.
*)

PROCEDURE ulp (x: SHORTREAL) : SHORTREAL ;
BEGIN
   IF x<huge_valf()
   THEN
      RETURN succ(x)-x
   ELSE
      RETURN x-pred(x)
   END
END ulp ;


(*
   pred - returns the previous value of the type REAL less than x.
*)

PROCEDURE pred (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN nextafterf(x, -huge_valf())
END pred ;


(*
   intpart - returns the integer part of x
*)

PROCEDURE intpart (x: SHORTREAL) : SHORTREAL ;
VAR
   y, z: SHORTREAL ;
BEGIN
   z := modff(x, y) ;
   RETURN y
END intpart ;


(*
   fractpart - returns the fractional part of x
*)

PROCEDURE fractpart (x: SHORTREAL) : SHORTREAL ;
VAR
   y: SHORTREAL ;
BEGIN
   RETURN modff(x, y)
END fractpart ;


(*
   scale - returns the value of x * radix ** n

           The following holds true:

           x = synthesize(exponent(x),fraction(x))
           x = scale(fraction(x), exponent(x))
*)

PROCEDURE scale (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
BEGIN
   RETURN scalbnf(x, n)
END scale ;


(*
   trunc - returns the value of the first n places of x.
*)

PROCEDURE trunc (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
VAR
   y         : SHORTREAL ;
   sign,
   error     : BOOLEAN ;
   s         : String ;
   r         : ADDRESS ;
   point, l,
   powerOfTen: INTEGER ;
BEGIN
   IF n<0
   THEN
      (* exception raised *)
      RAISE(except, ORD(badparam),
            'LowLong.trunc: cannot truncate to a negative number of digits') ;
      RETURN x
   ELSE
      r := dtoa(x, maxsignificant, 100, point, sign) ;
      s := InitStringCharStar(r) ;
      free(r) ;
      l := Length(s) ;
      IF VAL(INTEGER, n)<l
      THEN
         s := Slice(ToSigFig(s, n), 0, n)
      ELSE
         (* add '0's to make up significant figures *)
         s := ConCat(s, Mark(Mult(InitStringChar('0'), l-VAL(INTEGER, n))))
      END ;
      powerOfTen := point-1 ;
      point := 1 ;

      IF (point<l) AND (point<VAL(INTEGER, n))
      THEN
         s := ConCat(ConCatChar(Slice(s, 0, point), '.'),
                     Slice(s, point, 0))
      END ;
      y := strtod(string(s), error) ;
      IF powerOfTen#0
      THEN
         y := power(y, FLOATS(powerOfTen))
      END ;
      s := KillString(s) ;
      RETURN y
   END
END trunc ;


(*
   round - returns the value of x rounded to the first n places.
           n significant figures.
*)

PROCEDURE round (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
VAR
   y    : SHORTREAL ;
   error: BOOLEAN ;
   s    : String ;
BEGIN
   IF n<0
   THEN
      (* exception raised *)
      RAISE(except, ORD(badparam),
            'LowLong.round: cannot round to a negative number of digits') ;
      RETURN x
   ELSE
      s := RealToFloatString(x, n) ;
      y := strtod(string(s), error) ;
      s := KillString(s) ;
      RETURN y
   END
END round ;


(*
   synthesize - returns a value of the type SHORTREAL constructed from
                the given expart and frapart.

                The following holds true:

                x = synthesize(exponent(x),fraction(x))
                x = scale(fraction(x), exponent(x))
*)

PROCEDURE synthesize (expart: INTEGER; frapart: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN scalbnf(frapart, expart)
END synthesize ;


(*
   setMode - sets status flags appropriate to the underlying implementation
             of the type REAL.
*)

PROCEDURE setMode (m: Modes) ;
BEGIN
   currentmode := m
END setMode ;


(*
   currentMode - returns the current status flags in the form set by setMode
*)

PROCEDURE currentMode () : Modes ;
BEGIN
   RETURN currentmode
END currentMode ;


(*
   IsLowException - returns TRUE if the current coroutine is in the exceptional
                    execution state because of the raising of an exception in a
                    routine from this module; otherwise returns FALSE.
*)

PROCEDURE IsLowException () : BOOLEAN ;
BEGIN
   RETURN( IsExceptionalExecution() AND IsCurrentSource(except) )
END IsLowException ;


VAR
   except: ExceptionSource ;
BEGIN
   AllocateSource(except)
END LowShort.
