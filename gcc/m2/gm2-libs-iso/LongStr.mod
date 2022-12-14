(* LongStr.mod implement the ISO LongStr specification.

Copyright (C) 2009-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE LongStr;

(* REAL/string conversions *)

IMPORT LongConv ;

FROM DynamicStrings IMPORT String, InitString, KillString, Length, CopyOut ;

FROM ConvStringLong IMPORT RealToFixedString, RealToFloatString,
                           RealToEngString ;


(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".",
     {decimal digit}]
*)

(* the string form of a signed floating-point real number is
     signed fixed-point real number, "E", ["+" | "-"],
     decimal digit, {decimal digit}
*)

PROCEDURE StrToReal (str: ARRAY OF CHAR; VAR real: LONGREAL;
                     VAR res: ConvResults) ;
  (* Ignores any leading spaces in str. If the subsequent characters
     in str are in the format of a signed real number, assigns a
     corresponding value to real.  Assigns a value indicating the
     format of str to res.
  *)
BEGIN
   res := LongConv.FormatReal(str) ;
   IF res=strAllRight
   THEN
      real := LongConv.ValueReal(str)
   END
END StrToReal ;


PROCEDURE RealToFloat (real: LONGREAL; sigFigs: CARDINAL;
                       VAR str: ARRAY OF CHAR) ;
  (* Converts the value of real to floating-point string form, with
     sigFigs significant figures, and copies the possibly truncated
     result to str.
  *)
VAR
   s: String ;
BEGIN
   s := RealToFloatString(real, sigFigs) ;
   CopyOut(str, s) ;
   s := KillString(s)
END RealToFloat ;


PROCEDURE RealToEng (real: LONGREAL; sigFigs: CARDINAL;
                     VAR str: ARRAY OF CHAR) ;
  (* Converts the value of real to floating-point string form, with
     sigFigs significant figures, and copies the possibly truncated
     result to str.  The number is scaled with one to three digits
     in the whole number part and with an exponent that is a multiple
     of three.
  *)
VAR
   s: String ;
BEGIN
   s := RealToEngString(real, sigFigs) ;
   CopyOut(str, s) ;
   s := KillString(s)
END RealToEng ;


PROCEDURE RealToFixed (real: LONGREAL; place: INTEGER;
                       VAR str: ARRAY OF CHAR) ;
  (* Converts the value of real to fixed-point string form, rounded
     to the given place relative to the decimal point, and copies
     the possibly truncated result to str.
  *)
VAR
   s: String ;
BEGIN
   s := RealToFixedString(real, place) ;
   CopyOut(str, s) ;
   s := KillString(s)
END RealToFixed ;


PROCEDURE RealToStr (real: LONGREAL; VAR str: ARRAY OF CHAR) ;
  (* Converts the value of real as RealToFixed if the sign and
     magnitude can be shown within the capacity of str, or
     otherwise as RealToFloat, and copies the possibly truncated
     result to str.  The number of places or significant digits
     are implementation-defined.
  *)
VAR
   s      : String ;
   sigFigs: CARDINAL ;
BEGIN
   sigFigs := HIGH(str) ;
   WHILE sigFigs>1 DO
      s := RealToFixedString(real, sigFigs) ;
      IF Length(s)<=HIGH(str)
      THEN
         CopyOut(str, s) ;
         s := KillString(s) ;
         RETURN
      END ;
      s := KillString(s) ;
      DEC(sigFigs)
   END ;
   sigFigs := HIGH(str) ;
   WHILE sigFigs#0 DO
      s := RealToFloatString(real, sigFigs) ;
      IF Length(s)<=HIGH(str)
      THEN
         CopyOut(str, s) ;
         s := KillString(s) ;
         RETURN
      END ;
      s := KillString(s) ;
      DEC(sigFigs)
   END
END RealToStr ;


END LongStr.
