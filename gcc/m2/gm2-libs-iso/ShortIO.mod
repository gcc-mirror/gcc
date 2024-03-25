(* ShortIO.mod implements input/output of SHORTREAL over channels.

Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE ShortIO;

IMPORT RealIO ;

  (* The text form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit},
       [".", {decimal digit}]

     The text form of a signed floating-point real number is
       signed fixed-point real number,
       "E", ["+" | "-"], decimal digit, {decimal digit}
  *)

(* Skips leading spaces, and removes any remaining characters
   from cid that form part of a signed fixed or floating
   point number.  The value of this number is assigned to real.
   The read result is set to the value allRight, outOfRange,
   wrongFormat, endOfLine, or endOfInput.
*)

PROCEDURE ReadReal (cid: IOChan.ChanId; VAR real: SHORTREAL);
VAR
   r: REAL ;
BEGIN
   RealIO.ReadReal(cid, r) ;
   real := r
END ReadReal ;


(* Writes the value of real to cid in floating-point text form,
   with sigFigs significant figures, in a field of the given
   minimum width.
*)

PROCEDURE WriteFloat (cid: IOChan.ChanId; real: SHORTREAL;
                      sigFigs: CARDINAL; width: CARDINAL);
BEGIN
   RealIO.WriteFloat(cid, real, sigFigs, width)
END WriteFloat ;


(* As for WriteFloat, except that the number is scaled with
   one to three digits in the whole number part, and with an
   exponent that is a multiple of three.
*)

PROCEDURE WriteEng (cid: IOChan.ChanId; real: SHORTREAL;
                    sigFigs: CARDINAL; width: CARDINAL);
BEGIN
   RealIO.WriteEng(cid, real, sigFigs, width)
END WriteEng ;


(* Writes the value of real to cid in fixed-point text form,
   rounded to the given place relative to the decimal point,
   in a field of the given minimum width.
*)

PROCEDURE WriteFixed (cid: IOChan.ChanId; real: SHORTREAL;
                      place: INTEGER; width: CARDINAL);
BEGIN
   RealIO.WriteFixed(cid, real, place, width)
END WriteFixed ;


(* Writes the value of real to cid, as WriteFixed if the sign
   and magnitude can be shown in the given width, or otherwise
   as WriteFloat.  The number of places or significant digits
   depends on the given width.
*)

PROCEDURE WriteReal (cid: IOChan.ChanId;
                     real: SHORTREAL; width: CARDINAL);
BEGIN
   RealIO.WriteReal(cid, real, width)
END WriteReal ;


END ShortIO.
