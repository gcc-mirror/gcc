(* WholeConv.mod implement the ISO WholeConv specification.

Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE WholeConv ;

FROM CharClass IMPORT IsNumeric, IsWhiteSpace ;
IMPORT EXCEPTIONS ;

FROM ConvTypes IMPORT ScanClass ;


TYPE
   WholeConvException = (noException, invalidSigned, invalidUnsigned) ;

VAR
   wholeConv:  EXCEPTIONS.ExceptionSource ;


(*
   ScanInt - represents the start state of a finite state scanner
             for signed whole numbers - assigns class of inputCh
             to chClass and a procedure representing the next state
             to nextState.
*)

PROCEDURE ScanInt (inputCh: CHAR;
                   VAR chClass: ConvTypes.ScanClass;
                   VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanRemainingDigits ;
      chClass := valid
   ELSIF (inputCh='+') OR (inputCh='-')
   THEN
      nextState := scanFirstDigit ;
      chClass := valid
   ELSIF IsWhiteSpace(inputCh)
   THEN
      nextState := scanSpace ;
      chClass := padding
   ELSE
      nextState := ScanInt ;
      chClass := invalid
   END
END ScanInt ;


PROCEDURE scanFirstDigit (ch: CHAR;
                          VAR chClass: ConvTypes.ScanClass;
                          VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(ch)
   THEN
      chClass := valid ;
      nextState := scanRemainingDigits
   ELSE
      chClass := invalid
   END
END scanFirstDigit ;


PROCEDURE scanRemainingDigits (ch: CHAR;
                               VAR chClass: ConvTypes.ScanClass;
                               VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(ch)
   THEN
      chClass := valid
   ELSE
      chClass := terminator
   END
END scanRemainingDigits ;


PROCEDURE scanSpace (ch: CHAR;
                     VAR chClass: ConvTypes.ScanClass;
                     VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsWhiteSpace(ch)
   THEN
      chClass := padding
   ELSIF (ch='+') OR (ch='-')
   THEN
      chClass := valid ;
      nextState := scanFirstDigit
   ELSE
      chClass := invalid
   END
END scanSpace ;


(*
   FormatInt - returns the format of the string value for
               conversion to INTEGER.
*)

PROCEDURE FormatInt (str: ARRAY OF CHAR) : ConvResults ;
VAR
   proc   : ConvTypes.ScanState ;
   chClass: ConvTypes.ScanClass ;
   i, h   : CARDINAL ;
BEGIN
   i := 1 ;
   h := LENGTH(str) ;
   ScanInt(str[0], chClass, proc) ;
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
END FormatInt ;


(*
   ValueInt - returns the value corresponding to the signed whole
              number string value str if str is well-formed;
              otherwise raises the WholeConv exception.
*)

PROCEDURE ValueInt (str: ARRAY OF CHAR) : INTEGER;
VAR
   proc   : ConvTypes.ScanState ;
   chClass: ConvTypes.ScanClass ;
   i, h   : CARDINAL ;
   v      : INTEGER ;
   value  : CARDINAL ;
   neg    : BOOLEAN ;
BEGIN
   IF FormatInt(str)=strAllRight
   THEN
      value := 0 ;
      neg := FALSE ;
      i := 0 ;
      h := LENGTH(str) ;
      proc := ScanInt ;
      chClass := valid ;
      WHILE (i<h) AND ((chClass=valid) OR (chClass=padding)) DO
         IF str[i]='-'
         THEN
            neg := NOT neg
         ELSIF str[i]='+'
         THEN
            (* ignore *)
         ELSIF IsNumeric(str[i])
         THEN
            value := value*10+(ORD(str[i])-ORD('0'))
         END ;
         proc(str[i], chClass, proc) ;
         INC(i)
      END ;
      IF neg
      THEN
         v := -value
      ELSE
         v := value
      END ;
      RETURN( v )
   ELSE
      EXCEPTIONS.RAISE(wholeConv, ORD(invalidSigned),
                       'WholeConv.' + __FUNCTION__ + ': signed number is invalid') ;
      RETURN 0
   END
END ValueInt ;


(*
   LengthInt - returns the number of characters in the string
               representation of int.
*)

PROCEDURE LengthInt (int: INTEGER) : CARDINAL ;
VAR
   c, l: CARDINAL ;
BEGIN
   IF int<0
   THEN
      l := 2 ;
      IF int=MIN(INTEGER)
      THEN
         c := VAL(CARDINAL, MAX(INTEGER))+1
      ELSE
         c := -int
      END
   ELSE
      l := 1 ;
      c := int
   END ;
   WHILE c>9 DO
      c := c DIV 10 ;
      INC(l)
   END ;
   RETURN( l )
END LengthInt ;


(*
   ScanCard - represents the start state of a finite state scanner for
              unsigned whole numbers - assigns class of inputCh to
              chClass and a procedure representing the next state to
              nextState.
*)

PROCEDURE ScanCard (inputCh: CHAR;
                    VAR chClass: ConvTypes.ScanClass;
                    VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanRemainingDigits ;
      chClass := valid
   ELSIF inputCh='+'
   THEN
      nextState := scanFirstDigit ;
      chClass := valid
   ELSIF IsWhiteSpace(inputCh)
   THEN
      nextState := scanSpace ;
      chClass := padding
   ELSE
      nextState := ScanCard ;
      chClass := invalid
   END
END ScanCard ;


(*
   FormatCard - returns the format of the string value for
                conversion to CARDINAL.
*)

PROCEDURE FormatCard (str: ARRAY OF CHAR) : ConvResults ;
VAR
   proc   : ConvTypes.ScanState ;
   chClass: ConvTypes.ScanClass ;
   i, h   : CARDINAL ;
BEGIN
   i := 1 ;
   h := LENGTH(str) ;
   ScanCard(str[0], chClass, proc) ;
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
END FormatCard ;


(*
   ValueCard - returns the value corresponding to the unsigned
               whole number string value str if str is well-formed;
               otherwise raises the WholeConv exception.
*)

PROCEDURE ValueCard (str: ARRAY OF CHAR) : CARDINAL ;
VAR
   proc   : ConvTypes.ScanState ;
   chClass: ConvTypes.ScanClass ;
   i, h   : CARDINAL ;
   value  : CARDINAL ;
BEGIN
   IF FormatCard(str)=strAllRight
   THEN
      value := 0 ;
      i := 0 ;
      h := LENGTH(str) ;
      ScanCard(str[0], chClass, proc) ;
      proc := ScanCard ;
      chClass := valid ;
      WHILE (i<h) AND ((chClass=valid) OR (chClass=padding)) DO
         IF str[i]='+'
         THEN
            (* ignore *)
         ELSIF IsNumeric(str[i])
         THEN
            value := value*10+(ORD(str[i])-ORD('0'))
         END ;
         proc(str[i], chClass, proc) ;
         INC(i)
      END ;
      RETURN( value )
   ELSE
      EXCEPTIONS.RAISE(wholeConv, ORD(invalidUnsigned),
                       'WholeConv:' + __FUNCTION__ + ': unsigned number is invalid') ;
      RETURN 0
   END
END ValueCard ;


(*
   LengthCard - returns the number of characters in the string
                representation of, card.
*)

PROCEDURE LengthCard (card: CARDINAL) : CARDINAL ;
VAR
   l: CARDINAL ;
BEGIN
   l := 1 ;
   WHILE card>9 DO
      card := card DIV 10 ;
      INC(l)
   END ;
   RETURN( l )
END LengthCard ;


(*
   IsWholeConvException - returns TRUE if the current coroutine is
                          in the exceptional execution state because
                          of the raising of an exception in a routine
                          from this module; otherwise returns FALSE.
*)

PROCEDURE IsWholeConvException () : BOOLEAN ;
BEGIN
   RETURN( EXCEPTIONS.IsCurrentSource(wholeConv) )
END IsWholeConvException ;


BEGIN
   EXCEPTIONS.AllocateSource(wholeConv)
END WholeConv.
