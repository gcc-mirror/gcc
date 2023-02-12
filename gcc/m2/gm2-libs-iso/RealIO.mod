(* Library module defined by the International Standard
   Information technology - programming languages
   BS ISO/IEC 10514-1:1996E Part 1: Modula-2, Base Language.

   Copyright ISO/IEC (International Organization for Standardization
   and International Electrotechnical Commission) 1996-2021.

   It may be freely copied for the purpose of implementation (see page
   707 of the Information technology - Programming languages Part 1:
   Modula-2, Base Language.  BS ISO/IEC 10514-1:1996).  *)

IMPLEMENTATION MODULE RealIO;

  (* Input and output of real numbers in decimal text form
     over specified channels.  The read result is of the
     type IOConsts.ReadResults.
  *)

FROM TextIO IMPORT WriteChar, ReadChar ;
FROM StringChan IMPORT writeString ;
FROM IOChan IMPORT SetReadResult ;
FROM IOConsts IMPORT ReadResults ;

FROM ConvStringReal IMPORT RealToFixedString, RealToFloatString,
                           RealToEngString ;

FROM ConvTypes IMPORT ScanClass, ScanState ;
FROM TextIO IMPORT WriteChar, ReadChar ;
FROM DynamicStrings IMPORT String, char, KillString, Length, InitString, ConCatChar, string ;
FROM RealConv IMPORT ScanReal ;
FROM StringChan IMPORT writeString, writeFieldWidth ;
FROM dtoa IMPORT strtod ;


  (* The text form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit},
       [".", {decimal digit}]

     The text form of a signed floating-point real number is
       signed fixed-point real number,
       "E", ["+" | "-"], decimal digit, {decimal digit}
  *)

PROCEDURE ReadReal (cid: IOChan.ChanId; VAR real: REAL);
  (* Skips leading spaces, and removes any remaining characters
     from cid that form part of a signed fixed or floating
     point number.  The value of this number is assigned to real.
     The read result is set to the value allRight, outOfRange,
     wrongFormat, endOfLine, or endOfInput.
  *)
VAR
   chClass  : ScanClass ;
   nextState: ScanState ;
   ch       : CHAR ;
   s        : String ;
   error    : BOOLEAN ;
BEGIN
   ReadChar(cid, ch) ;
   nextState := ScanReal ;
   REPEAT
      nextState(ch, chClass, nextState) ;
      IF chClass=padding
      THEN
         ReadChar(cid, ch)
      END
   UNTIL chClass#padding ;
   IF chClass=valid
   THEN
      s := InitString('') ;
      WHILE chClass=valid DO
         s := ConCatChar(s, ch) ;
         ReadChar(cid, ch) ;
         nextState(ch, chClass, nextState)
      END ;
      real := strtod(string(s), error) ;
      s := KillString(s) ;
      IF error
      THEN
         SetReadResult(cid, outOfRange)
      ELSE
         SetReadResult(cid, allRight)
      END
   ELSE
      SetReadResult(cid, wrongFormat)
   END
END ReadReal ;


PROCEDURE WriteFloat (cid: IOChan.ChanId; real: REAL;
                      sigFigs: CARDINAL; width: CARDINAL);
  (* Writes the value of real to cid in floating-point text form,
     with sigFigs significant figures, in a field of the given
     minimum width.
  *)
VAR
   s: String ;
BEGIN
   s := RealToFloatString(real, sigFigs) ;
   writeFieldWidth(cid, s, width) ;
   s := KillString(s)
END WriteFloat ;


PROCEDURE WriteEng (cid: IOChan.ChanId; real: REAL;
                    sigFigs: CARDINAL; width: CARDINAL);
  (* As for WriteFloat, except that the number is scaled with
     one to three digits in the whole number part, and with an
     exponent that is a multiple of three.
  *)
VAR
   s: String ;
BEGIN
   s := RealToEngString(real, sigFigs) ;
   writeFieldWidth(cid, s, width) ;
   s := KillString(s)
END WriteEng ;


PROCEDURE WriteFixed (cid: IOChan.ChanId; real: REAL;
                      place: INTEGER; width: CARDINAL);
  (* Writes the value of real to cid in fixed-point text form,
     rounded to the given place relative to the decimal point,
     in a field of the given minimum width.
  *)
VAR
   s: String ;
BEGIN
   s := RealToFixedString(real, place) ;
   writeFieldWidth(cid, s, width) ;
   s := KillString(s)
END WriteFixed ;


PROCEDURE WriteReal (cid: IOChan.ChanId;
                     real: REAL; width: CARDINAL);
  (* Writes the value of real to cid, as WriteFixed if the sign
     and magnitude can be shown in the given width, or otherwise
     as WriteFloat.  The number of places or significant digits
     depends on the given width.
  *)
VAR
   sigFigs: CARDINAL ;
   s      : String ;
BEGIN
   sigFigs := width ;
   WHILE sigFigs>1 DO
      s := RealToFixedString(real, sigFigs) ;
      IF Length(s)<=width
      THEN
         writeFieldWidth(cid, s, width) ;
         s := KillString(s) ;
         RETURN
      END ;
      s := KillString(s) ;
      DEC(sigFigs)
   END ;
   sigFigs := width ;
   WHILE sigFigs#0 DO
      s := RealToFloatString(real, sigFigs) ;
      IF Length(s)<=width
      THEN
         writeFieldWidth(cid, s, width) ;
         s := KillString(s) ;
         RETURN
      END ;
      s := KillString(s) ;
      DEC(sigFigs)
   END
END WriteReal ;


END RealIO.
