(* SRealIO.mod implement the ISO SRealIO specification.

Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE SRealIO ;

IMPORT StdChans, RealIO ;

  (* Input and output of real numbers in decimal text form over
     default channels.  The read result is of the type
     IOConsts.ReadResults.
  *)

  (* The text form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit},
       [".", {decimal digit}]

     The text form of a signed floating-point real number is
       signed fixed-point real number,
       "E", ["+" | "-"], decimal digit, {decimal digit}
  *)

PROCEDURE ReadReal (VAR real: REAL);
  (* Skips leading spaces, and removes any remaining characters
     from the default input channel that form part of a signed
     fixed or floating point number. The value of this number
     is assigned to real.  The read result is set to the value
     allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
  *)
BEGIN
   RealIO.ReadReal(StdChans.StdInChan(), real)
END ReadReal ;

PROCEDURE WriteFloat (real: REAL; sigFigs: CARDINAL; width: CARDINAL);
  (* Writes the value of real to the default output channel in
     floating-point text form, with sigFigs significant figures,
     in a field of the given minimum width.
  *)
BEGIN
   RealIO.WriteFloat(StdChans.StdOutChan(), real, sigFigs, width)
END WriteFloat ;

PROCEDURE WriteEng (real: REAL; sigFigs: CARDINAL; width: CARDINAL);
  (* As for WriteFloat, except that the number is scaled with one to
     three digits in the whole number part, and with an exponent that
     is a multiple of three.
  *)
BEGIN
   RealIO.WriteFloat(StdChans.StdOutChan(), real, sigFigs, width)
END WriteEng ;

PROCEDURE WriteFixed (real: REAL; place: INTEGER; width: CARDINAL);
  (* Writes the value of real to the default output channel in
     fixed-point text form, rounded to the given place relative
     to the decimal point, in a field of the given minimum width.
  *)
BEGIN
   RealIO.WriteFixed(StdChans.StdOutChan(), real, place, width)
END WriteFixed ;

PROCEDURE WriteReal (real: REAL; width: CARDINAL);
  (* Writes the value of real to the default output channel, as
     WriteFixed if the sign and magnitude can be shown in the
     given width, or otherwise as WriteFloat. The number of
     places or significant digits depends on the given width.
  *)
BEGIN
   RealIO.WriteReal(StdChans.StdOutChan(), real, width)
END WriteReal ;

END SRealIO.
