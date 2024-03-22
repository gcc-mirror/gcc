(* LongConv.mod implement the ISO LongConv specification.

Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE LongConv ;

FROM SYSTEM IMPORT ADDRESS ;
FROM ConvTypes IMPORT ScanClass ;
FROM CharClass IMPORT IsNumeric, IsWhiteSpace ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, KillString, Length, Slice, Mark, Index, string ;
FROM ldtoa IMPORT strtold ;
FROM ConvStringLong IMPORT RealToFloatString, RealToEngString, RealToFixedString ;
FROM M2RTS IMPORT Halt ;
FROM libc IMPORT free ;
IMPORT EXCEPTIONS ;


TYPE
   RealConvException = (noException, invalid, outofrange) ;

VAR
   realConv:  EXCEPTIONS.ExceptionSource ;


(* Low-level LONGREAL/string conversions *)

(* Represents the start state of a finite state scanner for real
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)

PROCEDURE ScanReal (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                    VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanSecondDigit ;
      chClass := valid
   ELSIF (inputCh='+') OR (inputCh='-')
   THEN
      nextState := scanFirstDigit ;
      chClass := valid
   ELSIF IsWhiteSpace(inputCh)
   THEN
      nextState := ScanReal ;
      chClass := padding
   ELSE
      nextState := ScanReal ;
      chClass := invalid
   END
END ScanReal ;


(*
   scanFirstDigit -
*)

PROCEDURE scanFirstDigit (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                          VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanSecondDigit ;
      chClass := valid
   ELSE
      nextState := scanFirstDigit ;
      chClass := invalid
   END
END scanFirstDigit ;


(*
   scanSecondDigit -
*)

PROCEDURE scanSecondDigit (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                           VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanSecondDigit ;
      chClass := valid
   ELSIF inputCh='.'
   THEN
      nextState := scanFixed ;
      chClass := valid
   ELSIF inputCh='E'
   THEN
      nextState := scanScientific ;
      chClass := valid
   ELSE
      nextState := noOpFinished ;
      chClass := terminator
   END
END scanSecondDigit ;


(*
   scanFixed -
*)

PROCEDURE scanFixed (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                     VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanFixed ;
      chClass := valid
   ELSIF inputCh='E'
   THEN
      nextState := scanScientific ;
      chClass := valid
   ELSE
      nextState := noOpFinished ;
      chClass := terminator
   END
END scanFixed ;


(*
   scanScientific -
*)

PROCEDURE scanScientific (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                          VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanScientificSecond ;
      chClass := valid
   ELSIF (inputCh='-') OR (inputCh='+')
   THEN
      nextState := scanScientificSign ;
      chClass := valid
   ELSE
      nextState := scanScientific ;
      chClass := invalid
   END
END scanScientific ;


(*
   scanScientificSign -
*)

PROCEDURE scanScientificSign (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                              VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanScientificSecond ;
      chClass := valid
   ELSE
      nextState := scanScientificSign ;
      chClass := invalid
   END
END scanScientificSign ;


(*
   scanScientificSecond -
*)

PROCEDURE scanScientificSecond (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                                VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanScientificSecond ;
      chClass := valid
   ELSE
      nextState := noOpFinished ;
      chClass := terminator
   END
END scanScientificSecond ;


(*
   noOpFinished -
*)

PROCEDURE noOpFinished (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                        VAR nextState: ConvTypes.ScanState) ;
BEGIN
   nextState := noOpFinished ;
   chClass := terminator ;
   (* should we raise an exception here? *)
END noOpFinished ;


(* Returns the format of the string value for conversion to LONGREAL. *)

PROCEDURE FormatReal (str: ARRAY OF CHAR) : ConvResults ;
VAR
   proc   : ConvTypes.ScanState ;
   chClass: ConvTypes.ScanClass ;
   i, h   : CARDINAL ;
BEGIN
   i := 1 ;
   h := LENGTH(str) ;
   ScanReal(str[0], chClass, proc) ;
   WHILE (i<h) AND (chClass=padding) DO
      proc(str[i], chClass, proc) ;
      INC(i)
   END ;

   IF chClass=terminator
   THEN
      RETURN( strEmpty )
   END ;
   WHILE (i<h) AND (chClass=valid) DO
      proc(str[i], chClass, proc) ;
      INC(i)
   END ;
   CASE chClass OF

   padding   :  RETURN( strWrongFormat ) |
   terminator,
   valid     :  RETURN( strAllRight ) |
   invalid   :  RETURN( strWrongFormat )

   END
END FormatReal ;


(* Returns the value corresponding to the real number string value
   str if str is well-formed; otherwise raises the RealConv
   exception.
*)

PROCEDURE ValueReal (str: ARRAY OF CHAR) : LONGREAL ;
BEGIN
   IF FormatReal(str)=strAllRight
   THEN
      RETURN( doValueReal(str) )
   ELSE
      EXCEPTIONS.RAISE(realConv, ORD(invalid),
                       'LongConv.' + __FUNCTION__ + ': real number is invalid')
   END
END ValueReal ;


(*
   doValueReal - str, is a well-formed real number and its
                 value is returned.
*)

PROCEDURE doValueReal (str: ARRAY OF CHAR) : LONGREAL ;
VAR
   r    : LONGREAL ;
   error: BOOLEAN ;
   s    : String ;
BEGIN
   s := InitString(str) ;
   r := strtold(string(s), error) ;
   s := KillString(s) ;
   IF error
   THEN
      EXCEPTIONS.RAISE(realConv, ORD(outofrange),
                       'LongConv.' + __FUNCTION__ + ': real number is out of range')
   END ;
   RETURN( r )
END doValueReal ;


(* Returns the number of characters in the floating-point string
   representation of real with sigFigs significant figures.
*)

PROCEDURE LengthFloatReal (real: LONGREAL; sigFigs: CARDINAL) : CARDINAL ;
VAR
   s: String ;
   l: CARDINAL ;
BEGIN
   s := RealToFloatString(real, sigFigs) ;
   l := Length(s) ;
   s := KillString(s) ;
   RETURN( l )
END LengthFloatReal ;


(* Returns the number of characters in the floating-point engineering
   string representation of real with sigFigs significant figures.
*)

PROCEDURE LengthEngReal (real: LONGREAL; sigFigs: CARDINAL) : CARDINAL ;
VAR
   s: String ;
   l: CARDINAL ;
BEGIN
   s := RealToEngString(real, sigFigs) ;
   l := Length(s) ;
   s := KillString(s) ;
   RETURN( l )
END LengthEngReal ;


(* Returns the number of characters in the fixed-point string
   representation of real rounded to the given place relative to the
   decimal point.
*)

PROCEDURE LengthFixedReal (real: LONGREAL; place: INTEGER) : CARDINAL ;
VAR
   s: String ;
   l: CARDINAL ;
BEGIN
   s := RealToFixedString(real, place) ;
   l := Length(s) ;
   s := KillString(s) ;
   RETURN( l )
END LengthFixedReal ;


(* Returns TRUE if the current coroutine is in the exceptional
   execution state because of the raising of an exception in a
   routine from this module; otherwise returns FALSE.
*)

PROCEDURE IsRConvException () : BOOLEAN ;
BEGIN
   RETURN( EXCEPTIONS.IsCurrentSource(realConv) )
END IsRConvException ;


BEGIN
   EXCEPTIONS.AllocateSource(realConv)
END LongConv.
