(* LowLong.mod implement ISO LowLong specification.

Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE LowLong ;

FROM SYSTEM IMPORT ADDRESS ;
FROM Builtins IMPORT ilogbl, modfl, signbitl, scalbnl, huge_vall, nextafterl ;
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

PROCEDURE exponent (x: LONGREAL) : INTEGER ;
BEGIN
   RETURN ilogbl(x)
END exponent ;


(*
   fraction - returns the significand (or significant part) of x
*)

PROCEDURE fraction (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN scalbnl(x, -ilogbl (x))
END fraction ;

(*
   sign - returns the signum of x.  sign(x) = 1.0  for all x>0.0
          sign(x) = -1.0  for all x<0.0.
          may be either -1.0 or 1.0 if x = 0.0
*)

PROCEDURE sign (x: LONGREAL) : LONGREAL ;
BEGIN
   IF signbitl(x)=0
   THEN
      RETURN 1.0
   ELSE
      RETURN -1.0
   END
END sign ;


(*
   succ - returns the next value of the type REAL greater than x
*)

PROCEDURE succ (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN nextafterl(x, huge_vall())
END succ ;


(*
   ulp - returns the value of a unit in the last place of x.
         So either:

         ulp(x) = succ(x)-x     or
         ulp(x) = x-pred(x)     or both are true.

         if the value does not exist then an exception is raised.
*)

PROCEDURE ulp (x: LONGREAL) : LONGREAL ;
BEGIN
   IF x<huge_vall()
   THEN
      RETURN succ(x)-x
   ELSE
      RETURN x-pred(x)
   END
END ulp ;


(*
   pred - returns the previous value of the type REAL less than x.
*)

PROCEDURE pred (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN nextafterl(x, -huge_vall())
END pred ;


(*
   intpart - returns the integer part of x
*)

PROCEDURE intpart (x: LONGREAL) : LONGREAL ;
VAR
   y, z: LONGREAL ;
BEGIN
   z := modfl(x, y) ;
   RETURN y
END intpart ;


(*
   fractpart - returns the fractional part of x
*)

PROCEDURE fractpart (x: LONGREAL) : LONGREAL ;
VAR
   y: LONGREAL ;
BEGIN
   RETURN modfl(x, y)
END fractpart ;


(*
   scale - returns the value of x * radix ** n

           The following holds true:

           x = synthesize(exponent(x),fraction(x))
           x = scale(fraction(x), exponent(x))
*)

PROCEDURE scale (x: LONGREAL; n: INTEGER) : LONGREAL ;
BEGIN
   RETURN scalbnl(x, n)
END scale ;


(*
   trunc - returns the value of the first n places of x.
*)

PROCEDURE trunc (x: LONGREAL; n: INTEGER) : LONGREAL ;
VAR
   y         : LONGREAL ;
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
      RAISE(exceptSrc, ORD(badparam),
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
         y := power(y, FLOATL(powerOfTen))
      END ;
      s := KillString(s) ;
      RETURN y
   END
END trunc ;


(*
   round - returns the value of x rounded to the first n places.
           n significant figures.
*)

PROCEDURE round (x: LONGREAL; n: INTEGER) : LONGREAL ;
VAR
   y    : LONGREAL ;
   error: BOOLEAN ;
   s    : String ;
BEGIN
   IF n<0
   THEN
      (* exception raised *)
      RAISE(exceptSrc, ORD(badparam),
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
   synthesize - returns a value of the type REAL constructed from
                the given expart and frapart.

                The following holds true:

                x = synthesize(exponent(x),fraction(x))
                x = scale(fraction(x), exponent(x))
*)

PROCEDURE synthesize (expart: INTEGER; frapart: LONGREAL) : LONGREAL ;
BEGIN
   RETURN scalbnl(frapart, expart)
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
   RETURN( IsExceptionalExecution () AND IsCurrentSource (exceptSrc) )
END IsLowException ;


VAR
   exceptSrc: ExceptionSource ;
BEGIN
   AllocateSource (exceptSrc)
END LowLong.
