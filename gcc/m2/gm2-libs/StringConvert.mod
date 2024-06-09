(* StringConvert.mod provides functions to convert numbers to and from strings.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE StringConvert ;

FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM libc IMPORT free, printf ;
FROM libm IMPORT powl ;
FROM M2RTS IMPORT ErrorMessage ;

IMPORT DynamicStrings ;

FROM DynamicStrings IMPORT InitString,
                           InitStringChar, InitStringCharStar,
                           Mark, ConCat, Dup, string,
                           Slice, Index, char, Assign, Length, Mult,
                           RemoveWhitePrefix, ConCatChar, KillString,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

FROM ldtoa IMPORT Mode, strtold, ldtoa ;
IMPORT dtoa ;   (* this fixes linking - as the C ldtoa uses dtoa *)


(*
#undef GM2_DEBUG_STRINGCONVERT
#if defined(GM2_DEBUG_STRINGCONVERT)
#  define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#  define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#  define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#  define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#  define Dup(X) DupDB(X, __FILE__, __LINE__)
#  define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
#endif
*)

(*
   Assert - implement a simple assert.
*)

PROCEDURE Assert (b: BOOLEAN; file: ARRAY OF CHAR; line: CARDINAL; func: ARRAY OF CHAR) ;
BEGIN
   IF NOT b
   THEN
      ErrorMessage('assert failed', file, line, func)
   END
END Assert ;


(*
   Max -
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Min -
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   LongMin - returns the smallest LONGCARD
*)

PROCEDURE LongMin (a, b: LONGCARD) : LONGCARD ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END LongMin ;


(*
   IsDigit - returns TRUE if, ch, lies between '0'..'9'.
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch>='0') AND (ch<='9')
END IsDigit ;


(*
   IsDecimalDigitValid - returns the TRUE if, ch, is a base legal decimal digit.
                         If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsDecimalDigitValid (ch: CHAR; base: CARDINAL; VAR c: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsDigit(ch) AND (ORD(ch)-ORD('0')<base)
   THEN
      c := c*base + (ORD(ch)-ORD('0')) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsDecimalDigitValid ;


(*
   IsHexidecimalDigitValid - returns the TRUE if, ch, is a base legal hexidecimal digit.
                             If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsHexidecimalDigitValid (ch: CHAR; base: CARDINAL; VAR c: CARDINAL) : BOOLEAN ;
BEGIN
   IF (ch>='a') AND (ch<='f') AND (ORD(ch)-ORD('a')+10<base)
   THEN
      c := c*base + (ORD(ch)-ORD('a')+10) ;
      RETURN( TRUE )
   ELSIF (ch>='A') AND (ch<='F') AND (ORD(ch)-ORD('F')+10<base)
   THEN
      c := c*base + (ORD(ch)-ORD('A')+10) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsHexidecimalDigitValid ;


(*
   IsDecimalDigitValidLong - returns the TRUE if, ch, is a base legal decimal digit.
                             If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsDecimalDigitValidLong (ch: CHAR; base: CARDINAL;
                                   VAR c: LONGCARD) : BOOLEAN ;
BEGIN
   IF IsDigit(ch) AND (ORD(ch)-ORD('0')<base)
   THEN
      c := c * VAL(LONGCARD, base + (ORD(ch)-ORD('0'))) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsDecimalDigitValidLong ;


(*
   IsHexidecimalDigitValidLong - returns the TRUE if, ch, is a base legal hexidecimal digit.
                                 If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsHexidecimalDigitValidLong (ch: CHAR; base:  CARDINAL; VAR c: LONGCARD) : BOOLEAN ;
BEGIN
   IF (ch>='a') AND (ch<='f') AND (ORD(ch)-ORD('a')+10<base)
   THEN
      c := c * VAL(LONGCARD, base + (ORD(ch)-ORD('a')+10)) ;
      RETURN( TRUE )
   ELSIF (ch>='A') AND (ch<='F') AND (ORD(ch)-ORD('F')+10<base)
   THEN
      c := c * VAL(LONGCARD, base + (ORD(ch)-ORD('A')+10)) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsHexidecimalDigitValidLong ;


(*
   IsDecimalDigitValidShort - returns the TRUE if, ch, is a base legal decimal digit.
                              If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsDecimalDigitValidShort (ch: CHAR; base: CARDINAL; VAR c: SHORTCARD) : BOOLEAN ;
BEGIN
   IF IsDigit(ch) AND (ORD(ch)-ORD('0')<base)
   THEN
      c := c * VAL(SHORTCARD, base + (ORD(ch)-ORD('0'))) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsDecimalDigitValidShort ;


(*
   IsHexidecimalDigitValidShort - returns the TRUE if, ch, is a base legal hexidecimal digit.
                                  If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsHexidecimalDigitValidShort (ch: CHAR; base: CARDINAL; VAR c: SHORTCARD) : BOOLEAN ;
BEGIN
   IF (ch>='a') AND (ch<='f') AND (ORD(ch)-ORD('a')+10<base)
   THEN
      c := c * VAL(SHORTCARD, base + (ORD(ch)-ORD('a')+10)) ;
      RETURN( TRUE )
   ELSIF (ch>='A') AND (ch<='F') AND (ORD(ch)-ORD('F')+10<base)
   THEN
      c := c * VAL(SHORTCARD, base + (ORD(ch)-ORD('A')+10)) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsHexidecimalDigitValidShort ;


(*
   IntegerToString - converts INTEGER, i, into a String. The field with can be specified
                     if non zero. Leading characters are defined by padding and this
                     function will prepend a + if sign is set to TRUE.
                     The base allows the caller to generate binary, octal, decimal, hexidecimal
                     numbers. The value of lower is only used when hexidecimal numbers are
                     generated and if TRUE then digits abcdef are used, and if FALSE then ABCDEF
                     are used.
*)

PROCEDURE IntegerToString (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN;
                           base: CARDINAL; lower: BOOLEAN) : String ;
VAR
   s: String ;
   c: CARDINAL ;
BEGIN
   IF i<0
   THEN
      IF i=MIN(INTEGER)
      THEN
         (* remember that -15 MOD 4 = 1 in Modula-2 *)
         c := VAL(CARDINAL, ABS(i+1))+1 ;
         IF width>0
         THEN
            RETURN( ConCat(IntegerToString(-VAL(INTEGER, c DIV base),
                                           width-1, padding, sign, base, lower),
                           Mark(IntegerToString(c MOD base, 0, ' ', FALSE, base, lower))) )
         ELSE
            RETURN( ConCat(IntegerToString(-VAL(INTEGER, c DIV base),
                                           0, padding, sign, base, lower),
                           Mark(IntegerToString(c MOD base, 0, ' ', FALSE, base, lower))) )
         END
      ELSE
         s := InitString('-')
      END ;
      i := -i
   ELSE
      IF sign
      THEN
         s := InitString('+')
      ELSE
         s := InitString('')
      END
   END ;
   IF i>VAL(INTEGER, base)-1
   THEN
      s := ConCat(ConCat(s, Mark(IntegerToString(VAL(CARDINAL, i) DIV base, 0, ' ', FALSE, base, lower))),
                  Mark(IntegerToString(VAL(CARDINAL, i) MOD base, 0, ' ', FALSE, base, lower)))
   ELSE
      IF i<=9
      THEN
         s := ConCat(s, Mark(InitStringChar(CHR(VAL(CARDINAL, i)+ORD('0')))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, Mark(InitStringChar(CHR(VAL(CARDINAL, i)+ORD('a')-10))))
         ELSE
            s := ConCat(s, Mark(InitStringChar(CHR(VAL(CARDINAL, i)+ORD('A')-10))))
         END
      END
   END ;
   IF width>DynamicStrings.Length(s)
   THEN
      RETURN( ConCat(Mult(Mark(InitStringChar(padding)), width-DynamicStrings.Length(s)), Mark(s)) )
   END ;
   RETURN( s )
END IntegerToString ;


(*
   CardinalToString - converts CARDINAL, c, into a String. The field with can be specified
                      if non zero. Leading characters are defined by padding.
                      The base allows the caller to generate binary, octal, decimal, hexidecimal
                      numbers. The value of lower is only used when hexidecimal numbers are
                      generated and if TRUE then digits abcdef are used, and if FALSE then ABCDEF
                      are used.
*)

PROCEDURE CardinalToString (c: CARDINAL; width: CARDINAL; padding: CHAR;
                            base: CARDINAL; lower: BOOLEAN) : String ;
VAR
   s: String ;
BEGIN
   s := InitString('') ;
   IF c>base-1
   THEN
      s := ConCat(ConCat(s, Mark(CardinalToString(c DIV base, 0, ' ', base, lower))),
                  Mark(CardinalToString(c MOD base, 0, ' ', base, lower)))
   ELSE
      IF c<=9
      THEN
         s := ConCat(s, Mark(InitStringChar(CHR(c+ORD('0')))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, Mark(InitStringChar(CHR(c+ORD('a')-10))))
         ELSE
            s := ConCat(s, Mark(InitStringChar(CHR(c+ORD('A')-10))))
         END
      END
   END ;
   IF width>DynamicStrings.Length(s)
   THEN
      RETURN( ConCat(Mult(Mark(InitStringChar(padding)), width-DynamicStrings.Length(s)), s) )
   END ;
   RETURN( s )
END CardinalToString ;


(*
   LongIntegerToString - converts LONGINT, i, into a String. The field with
                         can be specified if non zero. Leading characters
                         are defined by padding and this function will
                         prepend a + if sign is set to TRUE.
                         The base allows the caller to generate binary,
                         octal, decimal, hexidecimal numbers.
                         The value of lower is only used when hexidecimal
                         numbers are generated and if TRUE then digits
                         abcdef are used, and if FALSE then ABCDEF are used.
*)

PROCEDURE LongIntegerToString (i: LONGINT; width: CARDINAL; padding: CHAR;
                               sign: BOOLEAN; base: CARDINAL; lower: BOOLEAN) : String ;

VAR
   s: String ;
   c: LONGCARD ;
BEGIN
   IF i<0
   THEN
      IF i=MIN(LONGINT)
      THEN
         (* remember that -15 MOD 4 is 1 in Modula-2, and although ABS(MIN(LONGINT)+1)
            is very likely MAX(LONGINT), it is safer not to assume this is the case *)
         c := VAL(LONGCARD, ABS(i+1))+1 ;
         IF width>0
         THEN
            RETURN( ConCat(LongIntegerToString(-VAL(LONGINT, c DIV VAL(LONGCARD, base)),
                                               width-1, padding, sign, base, lower),
                           Mark(LongIntegerToString(c MOD VAL(LONGCARD, base), 0, ' ', FALSE, base, lower))) )
         ELSE
            RETURN( ConCat(LongIntegerToString(-VAL(LONGINT, c DIV VAL(LONGCARD, base)),
                                               0, padding, sign, base, lower),
                           Mark(LongIntegerToString(c MOD VAL(LONGCARD, base), 0, ' ', FALSE, base, lower))) )
         END
      ELSE
         s := InitString('-')
      END ;
      i := -i
   ELSE
      IF sign
      THEN
         s := InitString('+')
      ELSE
         s := InitString('')
      END
   END ;
   IF i>VAL(LONGINT, base-1)
   THEN
      s := ConCat(ConCat(s, Mark(LongIntegerToString(i DIV VAL(LONGINT, base), 0, ' ', FALSE, base, lower))),
                  Mark(LongIntegerToString(i MOD VAL(LONGINT, base), 0, ' ', FALSE, base, lower)))
   ELSE
      IF i<=9
      THEN
         s := ConCat(s, Mark(InitStringChar(CHR(VAL(CARDINAL, i)+ORD('0')))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, Mark(InitStringChar(CHR(VAL(CARDINAL, i)+ORD('a')-10))))
         ELSE
            s := ConCat(s, Mark(InitStringChar(CHR(VAL(CARDINAL, i)+ORD('A')-10))))
         END
      END
   END ;
   IF width>DynamicStrings.Length(s)
   THEN
      RETURN( ConCat(Mult(Mark(InitStringChar(padding)), width-DynamicStrings.Length(s)), s) )
   END ;
   RETURN( s )
END LongIntegerToString ;


(*
   StringToLongInteger - converts a string, s, of, base, into an LONGINT.
                         Leading white space is ignored. It stops converting
                         when either the string is exhausted or if an illegal
                         numeral is found.
                         The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToLongInteger (s: String; base: CARDINAL; VAR found: BOOLEAN) : LONGINT ;
VAR
   n, l    : CARDINAL ;
   c       : LONGCARD ;
   negative: BOOLEAN ;
BEGIN
   s := RemoveWhitePrefix(s) ;    (* returns a new string, s *)
   l := DynamicStrings.Length(s) ;
   c := 0 ;
   n := 0 ;
   negative := FALSE ;
   IF n<l
   THEN
      (* parse leading + and - *)
      WHILE (char(s, n)='-') OR (char(s, n)='+') DO
         IF char(s, n)='-'
         THEN
            negative := NOT negative
         END ;
         INC(n)
      END ;
      WHILE (n<l) AND (IsDecimalDigitValidLong(char(s, n), base, c) OR
                       IsHexidecimalDigitValidLong(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   IF negative
   THEN
      RETURN( -VAL(LONGINT, LongMin(VAL(LONGCARD, MAX(LONGINT))+1, c)) )
   ELSE
      RETURN( VAL(LONGINT, LongMin(MAX(LONGINT), c)) )
   END
END StringToLongInteger ;


(*
   StringToInteger - converts a string, s, of, base, into an INTEGER.
                     Leading white space is ignored. It stops converting
                     when either the string is exhausted or if an illegal
                     numeral is found.
                     The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToInteger (s: String; base: CARDINAL;
                           VAR found: BOOLEAN) : INTEGER ;
VAR
   n, l    : CARDINAL ;
   c       : CARDINAL ;
   negative: BOOLEAN ;
BEGIN
   s := RemoveWhitePrefix(s) ;    (* returns a new string, s *)
   l := DynamicStrings.Length(s) ;
   c := 0 ;
   n := 0 ;
   negative := FALSE ;
   IF n<l
   THEN
      (* parse leading + and - *)
      WHILE (char(s, n)='-') OR (char(s, n)='+') DO
         IF char(s, n)='-'
         THEN
            negative := NOT negative
         END ;
         INC(n)
      END ;
      WHILE (n<l) AND (IsDecimalDigitValid(char(s, n), base, c) OR
                       IsHexidecimalDigitValid(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   IF negative
   THEN
      RETURN( -VAL(INTEGER, Min(VAL(CARDINAL, MAX(INTEGER))+1, c)) )
   ELSE
      RETURN( VAL(INTEGER, Min(MAX(INTEGER), c)) )
   END
END StringToInteger ;


(*
   StringToCardinal - converts a string, s, of, base, into a CARDINAL.
                      Leading white space is ignored. It stops converting
                      when either the string is exhausted or if an illegal
                      numeral is found.
                      The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToCardinal (s: String; base: CARDINAL;
                            VAR found: BOOLEAN) : CARDINAL ;
VAR
   n, l: CARDINAL ;
   c   : CARDINAL ;
BEGIN
   s := RemoveWhitePrefix(s) ;    (* returns a new string, s *)
   l := DynamicStrings.Length(s) ;
   c := 0 ;
   n := 0 ;
   IF n<l
   THEN
      (* parse leading + *)
      WHILE (char(s, n)='+') DO
         INC(n)
      END ;
      WHILE (n<l) AND (IsDecimalDigitValid(char(s, n), base, c) OR
                       IsHexidecimalDigitValid(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   RETURN( c )
END StringToCardinal ;


(*
   stoi - decimal string to INTEGER
*)

PROCEDURE stoi (s: String) : INTEGER ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToInteger(s, 10, found) )
END stoi ;


(*
   itos - integer to decimal string.
*)

PROCEDURE itos (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN) : String ;
BEGIN
   RETURN( IntegerToString(i, width, padding, sign, 10, FALSE) )
END itos ;


(*
   ctos - cardinal to decimal string.
*)

PROCEDURE ctos (c: CARDINAL; width: CARDINAL; padding: CHAR) : String ;
BEGIN
   RETURN( CardinalToString(c, width, padding, 10, FALSE) )
END ctos ;


(*
   stoc - decimal string to CARDINAL
*)

PROCEDURE stoc (s: String) : CARDINAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToCardinal(s, 10, found) )
END stoc ;


(*
   hstoi - hexidecimal string to INTEGER
*)

PROCEDURE hstoi (s: String) : INTEGER ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToInteger(s, 16, found) )
END hstoi ;


(*
   ostoi - octal string to INTEGER
*)

PROCEDURE ostoi (s: String) : INTEGER ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToInteger(s, 8, found) )
END ostoi ;


(*
   bstoi - binary string to INTEGER
*)

PROCEDURE bstoi (s: String) : INTEGER ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToInteger(s, 2, found) )
END bstoi ;


(*
   hstoc - hexidecimal string to CARDINAL
*)

PROCEDURE hstoc (s: String) : CARDINAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToCardinal(s, 16, found) )
END hstoc ;


(*
   ostoc - octal string to CARDINAL
*)

PROCEDURE ostoc (s: String) : CARDINAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToCardinal(s, 8, found) )
END ostoc ;


(*
   bstoc - binary string to CARDINAL
*)

PROCEDURE bstoc (s: String) : CARDINAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToCardinal(s, 2, found) )
END bstoc ;


(* **********************************************************************
   R e a l    a n d    L o n g R e a l    c o n v e r s i o n
   ********************************************************************** *)


(*
   ToThePower10 - returns a LONGREAL containing the value of v * 10^power.
*)

PROCEDURE ToThePower10 (v: LONGREAL; power: INTEGER) : LONGREAL;
VAR
   i: INTEGER ;
BEGIN
   i := 0 ;
   IF power>0
   THEN
      WHILE i<power DO
         v := v * 10.0 ;
         INC(i)
      END
   ELSE
      WHILE i>power DO
         v := v / 10.0 ;
         DEC(i)
      END
   END ;
   RETURN( v )
END ToThePower10 ;


(*
   DetermineSafeTruncation - we wish to use TRUNC when converting REAL/LONGREAL
                             into a string for the non fractional component.
                             However we need a simple method to
                             determine the maximum safe truncation value.
*)

PROCEDURE DetermineSafeTruncation () : CARDINAL ;
VAR
   MaxPowerOfTen: REAL ;
   LogPower     : CARDINAL ;
BEGIN
   MaxPowerOfTen := 1.0 ;
   LogPower      := 0 ;
   WHILE MaxPowerOfTen*10.0<FLOAT(MAX(INTEGER) DIV 10) DO
      MaxPowerOfTen := MaxPowerOfTen * 10.0 ;
      INC(LogPower)
   END ;
   RETURN( LogPower )
END DetermineSafeTruncation ;


(*
   LongrealToString - converts a LONGREAL number, Real, which has,
                      TotalWidth, and FractionWidth into a string.
                      It uses decimal notation.

                      So for example:

                      LongrealToString(1.0, 4, 2)  -> '1.00'
                      LongrealToString(12.3, 5, 2) -> '12.30'
                      LongrealToString(12.3, 6, 2) -> ' 12.30'
                      LongrealToString(12.3, 6, 3) -> '12.300'

                      if total width is too small then the fraction
                      becomes truncated.

                      LongrealToString(12.3, 5, 3) -> '12.30'

                      Positive numbers do not have a '+' prepended.
                      Negative numbers will have a '-' prepended and
                      the TotalWidth will need to be large enough
                      to contain the sign, whole number, '.' and
                      fractional components.
*)

PROCEDURE LongrealToString (x: LONGREAL;
                            TotalWidth, FractionWidth: CARDINAL) : String ;
VAR
   maxprecision: BOOLEAN ;
   s           : String ;
   r           : ADDRESS ;
   point       : INTEGER ;
   sign        : BOOLEAN ;
   l           : INTEGER ;
BEGIN
   IF TotalWidth=0
   THEN
      maxprecision := TRUE ;
      r := ldtoa(x, decimaldigits, 100, point, sign)
   ELSE
      r := ldtoa(x, decimaldigits, 100, point, sign)
   END ;
   s := InitStringCharStar(r) ;
   free(r) ;
   l := DynamicStrings.Length(s) ;
   IF point>l
   THEN
      s := ConCat(s, Mark(Mult(Mark(InitStringChar('0')), point-l))) ;
      s := ConCat(s, Mark(InitString('.0'))) ;
      IF (NOT maxprecision) AND (FractionWidth>0)
      THEN
         DEC(FractionWidth) ;
         IF VAL(INTEGER, FractionWidth)>point-l
         THEN
            s := ConCat(s, Mark(Mult(Mark(InitString('0')), FractionWidth)))
         END
      END
   ELSIF point<0
   THEN
      s := ConCat(Mult(Mark(InitStringChar('0')), -point), Mark(s)) ;
      l := DynamicStrings.Length(s) ;
      s := ConCat(InitString('0.'), Mark(s)) ;
      IF (NOT maxprecision) AND (l<VAL(INTEGER, FractionWidth))
      THEN
         s := ConCat(s, Mark(Mult(Mark(InitString('0')), VAL(INTEGER, FractionWidth)-l)))
      END
   ELSE
      IF point=0
      THEN
         s := ConCat(InitString('0.'), Mark(Slice(Mark(s), point, 0)))
      ELSE
         s := ConCat(ConCatChar(Slice(Mark(s), 0, point), '.'),
                     Mark(Slice(Mark(s), point, 0)))
      END ;
      IF (NOT maxprecision) AND (l-point<VAL(INTEGER, FractionWidth))
      THEN
         s := ConCat(s, Mark(Mult(Mark(InitString('0')), VAL(INTEGER, FractionWidth)-(l-point))))
      END
   END ;
   IF DynamicStrings.Length(s)>TotalWidth
   THEN
      IF TotalWidth>0
      THEN
         IF sign
         THEN
            s := Slice(Mark(ToDecimalPlaces(s, FractionWidth)), 0, TotalWidth-1) ;
            s := ConCat(InitStringChar('-'), Mark(s)) ;
            sign := FALSE
         ELSE
            (* minus 1 because all results will include a '.' *)
            s := Slice(Mark(ToDecimalPlaces(s, FractionWidth)), 0, TotalWidth) ;
         END
      ELSE
         IF sign
         THEN
            s := ToDecimalPlaces(s, FractionWidth) ;
            s := ConCat(InitStringChar('-'), Mark(s)) ;
            sign := FALSE
         ELSE
            (* minus 1 because all results will include a '.' *)
            s := ToDecimalPlaces(s, FractionWidth)
         END
      END
   END ;
   IF DynamicStrings.Length(s)<TotalWidth
   THEN
      s := ConCat(Mult(Mark(InitStringChar(' ')), TotalWidth-DynamicStrings.Length(s)), Mark(s))
   END ;
   RETURN( s )
END LongrealToString ;


(*
   StringToLongreal - returns a LONGREAL and sets found to TRUE if a legal number is seen.
*)

PROCEDURE StringToLongreal (s: String; VAR found: BOOLEAN) : LONGREAL ;
VAR
   error: BOOLEAN ;
   value: LONGREAL ;
BEGIN
   s := RemoveWhitePrefix(s) ;   (* new string is created *)
   value := strtold(string(s), error) ;
   s := KillString(s) ;
   found := NOT error ;
   RETURN value
END StringToLongreal ;


(*
   rtos -
*)

PROCEDURE rtos (r: REAL; TotalWidth, FractionWidth: CARDINAL) : String ;
BEGIN
   HALT ;
   RETURN ( NIL )
END rtos ;


(*
   stor - returns a REAL given a string.
*)

PROCEDURE stor (s: String) : REAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( VAL(REAL, StringToLongreal(s, found)) )
END stor ;


(*
   lrtos -
*)

PROCEDURE lrtos (r: LONGREAL; TotalWidth, FractionWidth: CARDINAL) : String ;
BEGIN
   HALT ;
   RETURN ( NIL )
END lrtos ;


(*
   stolr - returns a LONGREAL given a string.
*)

PROCEDURE stolr (s: String) : LONGREAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToLongreal(s, found) )
END stolr ;


(*
   LongCardinalToString - converts LONGCARD, c, into a String. The field
                          width can be specified if non zero. Leading
                          characters are defined by padding.
                          The base allows the caller to generate binary,
                          octal, decimal, hexidecimal numbers.
                          The value of lower is only used when hexidecimal
                          numbers are generated and if TRUE then digits
                          abcdef are used, and if FALSE then ABCDEF are used.
*)

PROCEDURE LongCardinalToString (c: LONGCARD; width: CARDINAL; padding: CHAR;
                                base: CARDINAL; lower: BOOLEAN) : String ;
VAR
   s: String ;
BEGIN
   s := InitString('') ;
   IF c>VAL(LONGCARD, base-1)
   THEN
      s := ConCat(ConCat(s, LongCardinalToString(c DIV VAL(LONGCARD, base), 0, ' ', base, lower)),
                  LongCardinalToString(c MOD VAL(LONGCARD, base), 0, ' ', base, lower))
   ELSE
      IF c<=9
      THEN
         s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('0'))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('a')-10)))
         ELSE
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('A')-10)))
         END
      END
   END ;
   IF width>DynamicStrings.Length(s)
   THEN
      RETURN( ConCat(Mult(Mark(InitStringChar(padding)), width-DynamicStrings.Length(s)), s) )
   END ;
   RETURN( s )
END LongCardinalToString ;


(*
   StringToLongCardinal - converts a string, s, of, base, into a LONGCARD.
                          Leading white space is ignored. It stops converting
                          when either the string is exhausted or if an illegal
                          numeral is found.
                          The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToLongCardinal (s: String; base: CARDINAL; VAR found: BOOLEAN) : LONGCARD ;
VAR
   n, l: CARDINAL ;
   c   : LONGCARD ;
BEGIN
   s := RemoveWhitePrefix(s) ;    (* returns a new string, s *)
   l := DynamicStrings.Length(s) ;
   c := 0 ;
   n := 0 ;
   IF n<l
   THEN
      (* parse leading + *)
      WHILE (char(s, n)='+') DO
         INC(n)
      END ;
      WHILE (n<l) AND (IsDecimalDigitValidLong(char(s, n), base, c) OR
                       IsHexidecimalDigitValidLong(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   RETURN( c )
END StringToLongCardinal ;


(*
   ShortCardinalToString - converts SHORTCARD, c, into a String. The field
                          width can be specified if non zero. Leading
                          characters are defined by padding.
                          The base allows the caller to generate binary,
                          octal, decimal, hexidecimal numbers.
                          The value of lower is only used when hexidecimal
                          numbers are generated and if TRUE then digits
                          abcdef are used, and if FALSE then ABCDEF are used.
*)

PROCEDURE ShortCardinalToString (c: SHORTCARD; width: CARDINAL; padding: CHAR;
                                base: CARDINAL; lower: BOOLEAN) : String ;
VAR
   s: String ;
BEGIN
   s := InitString('') ;
   IF VAL(CARDINAL, c)>base-1
   THEN
      s := ConCat(ConCat(s, ShortCardinalToString(c DIV VAL(SHORTCARD, base), 0, ' ', base, lower)),
                  ShortCardinalToString(c MOD VAL(SHORTCARD, base), 0, ' ', base, lower))
   ELSE
      IF c<=9
      THEN
         s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('0'))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('a')-10)))
         ELSE
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('A')-10)))
         END
      END
   END ;
   IF width>DynamicStrings.Length(s)
   THEN
      RETURN( ConCat(Mult(Mark(InitStringChar(padding)), width-DynamicStrings.Length(s)), s) )
   END ;
   RETURN( s )
END ShortCardinalToString ;


(*
   StringToShortCardinal - converts a string, s, of, base, into a SHORTCARD.
                           Leading white space is ignored. It stops converting
                           when either the string is exhausted or if an illegal
                           numeral is found.
                           The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToShortCardinal (s: String; base: CARDINAL;
                                 VAR found: BOOLEAN) : SHORTCARD ;
VAR
   n, l: CARDINAL ;
   c   : SHORTCARD ;
BEGIN
   s := RemoveWhitePrefix(s) ;    (* returns a new string, s *)
   l := DynamicStrings.Length(s) ;
   c := 0 ;
   n := 0 ;
   IF n<l
   THEN
      (* parse leading + *)
      WHILE (char(s, n)='+') DO
         INC(n)
      END ;
      WHILE (n<l) AND (IsDecimalDigitValidShort(char(s, n), base, c) OR
                       IsHexidecimalDigitValidShort(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   RETURN( c )
END StringToShortCardinal ;


(*
   ToDecimalPlaces - returns a floating point or base 10 integer
                     string which is accurate to, n, decimal
                     places.  It will return a new String
                     and, s, will be destroyed.
                     Decimal places yields, n, digits after
                     the .

                     So:  12.345

                     rounded to the following decimal places yields

                     5      12.34500
                     4      12.3450
                     3      12.345
                     2      12.34
                     1      12.3
*)

PROCEDURE ToDecimalPlaces (s: String; n: CARDINAL) : String ;
VAR
   point: INTEGER ;
BEGIN
   Assert(IsDigit(char(s, 0)) OR (char(s, 0)='.'), __FILE__, __LINE__, __FUNCTION__) ;
   point := Index(s, '.', 0) ;
   IF point<0
   THEN
      IF n>0
      THEN
         RETURN( ConCat(ConCat(s, Mark(InitStringChar('.'))), Mult(Mark(InitStringChar('0')), n)) )
      ELSE
         RETURN( s )
      END
   END ;
   s := doDecimalPlaces(s, n) ;
   (* if the last character is '.' remove it *)
   IF (DynamicStrings.Length(s)>0) AND (char(s, -1)='.')
   THEN
      RETURN( Slice(Mark(s), 0, -1) )
   ELSE
      RETURN( s )
   END
END ToDecimalPlaces ;


(*
   doDecimalPlaces - returns a string which is accurate to
                     n decimal places.  It returns a new String
                     and, s, will be destroyed.
*)

PROCEDURE doDecimalPlaces (s: String; n: CARDINAL) : String ;
VAR
   i, l,
   point    : INTEGER ;
   t,
   tenths,
   hundreths: String ;
BEGIN
   l := DynamicStrings.Length(s) ;
   i := 0 ;
   (* remove '.' *)
   point := Index(s, '.', 0) ;
   IF point=0
   THEN
      s := Slice(Mark(s), 1, 0)
   ELSIF point<l
   THEN
      s := ConCat(Slice(Mark(s), 0, point),
                  Mark(Slice(Mark(s), point+1, 0)))
   ELSE
      s := Slice(Mark(s), 0, point)
   END ;
   l := DynamicStrings.Length(s) ;
   i := 0 ;
   IF l>0
   THEN
      (* skip over leading zeros *)
      WHILE (i<l) AND (char(s, i)='0') DO
         INC(i)
      END ;
      (* was the string full of zeros? *)
      IF (i=l) AND (char(s, i-1)='0')
      THEN
         s := KillString(s) ;
         s := ConCat(InitString('0.'), Mark(Mult(Mark(InitStringChar('0')), n))) ;
         RETURN( s )
      END
   END ;
   (* add a leading zero in case we need to overflow the carry *)
   (* insert leading zero *)
   s := ConCat(InitStringChar('0'), Mark(s)) ;
   INC(point) ;  (* and move point position to correct place *)
   l := DynamicStrings.Length(s) ;   (* update new length *)
   i := point ;
   WHILE (n>1) AND (i<l) DO
      DEC(n) ;
      INC(i)
   END ;
   IF i+3<=l
   THEN
      t := Dup(s) ;
      hundreths := Slice(Mark(s), i+1, i+3) ;
      s := t ;
      IF stoc(hundreths)>=50
      THEN
         s := carryOne(Mark(s), i)
      END ;
      hundreths := KillString(hundreths)
   ELSIF i+2<=l
   THEN
      t := Dup(s) ;
      tenths := Slice(Mark(s), i+1, i+2) ;
      s := t ;
      IF stoc(tenths)>=5
      THEN
         s := carryOne(Mark(s), i)
      END ;
      tenths := KillString(tenths)
   END ;
   (* check whether we need to remove the leading zero *)
   IF char(s, 0)='0'
   THEN
      s := Slice(Mark(s), 1, 0) ;
      DEC(l) ;
      DEC(point)
   END ;
   IF i<l
   THEN
      s := Slice(Mark(s), 0, i) ;
      l := DynamicStrings.Length(s) ;
      IF l<point
      THEN
         s := ConCat(s, Mult(Mark(InitStringChar('0')), point-l))
      END
   END ;
   (* re-insert the point *)
   IF point>=0
   THEN
      IF point=0
      THEN
         s := ConCat(InitStringChar('.'), Mark(s))
      ELSE
         s := ConCat(ConCatChar(Slice(Mark(s), 0, point), '.'),
                     Mark(Slice(Mark(s), point, 0)))
      END
   END ;
   RETURN( s )
END doDecimalPlaces ;


(*
   ToSigFig - returns a floating point or base 10 integer
              string which is accurate to, n, significant
              figures.  It will return a new String
              and, s, will be destroyed.


              So:  12.345

              rounded to the following significant figures yields

              5      12.345
              4      12.34
              3      12.3
              2      12
              1      10
*)

PROCEDURE ToSigFig (s: String; n: CARDINAL) : String ;
VAR
   point: INTEGER ;
   poTen: CARDINAL ;
BEGIN
   Assert(IsDigit(char(s, 0)) OR (char(s, 0)='.'), __FILE__, __LINE__, __FUNCTION__) ;
   point := Index(s, '.', 0) ;
   IF point<0
   THEN
      poTen := DynamicStrings.Length(s)
   ELSE
      poTen := point
   END ;
   s := doSigFig(s, n) ;
   (* if the last character is '.' remove it *)
   IF (DynamicStrings.Length(s)>0) AND (char(s, -1)='.')
   THEN
      RETURN( Slice(Mark(s), 0, -1) )
   ELSE
      IF poTen>DynamicStrings.Length(s)
      THEN
         s := ConCat(s, Mark(Mult(Mark(InitStringChar('0')), poTen-DynamicStrings.Length(s))))
      END ;
      RETURN( s )
   END
END ToSigFig ;


(*
   doSigFig - returns a string which is accurate to
              n decimal places.  It returns a new String
              and, s, will be destroyed.
*)

PROCEDURE doSigFig (s: String; n: CARDINAL) : String ;
VAR
   i, l, z,
   point    : INTEGER ;
   t,
   tenths,
   hundreths: String ;
BEGIN
   l := DynamicStrings.Length(s) ;
   i := 0 ;
   (* remove '.' *)
   point := Index(s, '.', 0) ;
   IF point>=0
   THEN
      IF point=0
      THEN
         s := Slice(Mark(s), 1, 0)
      ELSIF point<l
      THEN
         s := ConCat(Slice(Mark(s), 0, point),
                     Mark(Slice(Mark(s), point+1, 0)))
      ELSE
         s := Slice(Mark(s), 0, point)
      END
   ELSE
      s := Dup(Mark(s))
   END ;
   l := DynamicStrings.Length(s) ;
   i := 0 ;
   IF l>0
   THEN
      (* skip over leading zeros *)
      WHILE (i<l) AND (char(s, i)='0') DO
         INC(i)
      END ;
      (* was the string full of zeros? *)
      IF (i=l) AND (char(s, i-1)='0')
      THEN
         (* truncate string *)
         s := Slice(Mark(s), 0, n) ;
         i := n
      END
   END ;
   (* add a leading zero in case we need to overflow the carry *)
   z := i ;  (* remember where we inserted zero *)
   IF z=0
   THEN
      s := ConCat(InitStringChar('0'), Mark(s))
   ELSE
      s := ConCat(ConCatChar(Slice(Mark(s), 0, i), '0'),
                  Mark(Slice(Mark(s), i, 0)))
   END ;
   INC(n) ;  (* and increase the number of sig figs needed *)
   l := DynamicStrings.Length(s) ;
   WHILE (n>1) AND (i<l) DO
      DEC(n) ;
      INC(i)
   END ;
   IF i+3<=l
   THEN
      t := Dup(s) ;
      hundreths := Slice(Mark(s), i+1, i+3) ;
      s := t ;
      IF stoc(hundreths)>=50
      THEN
         s := carryOne(Mark(s), i)
      END ;
      hundreths := KillString(hundreths)
   ELSIF i+2<=l
   THEN
      t := Dup(s) ;
      tenths := Slice(Mark(s), i+1, i+2) ;
      s := t ;
      IF stoc(tenths)>=5
      THEN
         s := carryOne(Mark(s), i)
      END ;
      tenths := KillString(tenths)
   END ;
   (* check whether we need to remove the leading zero *)
   IF char(s, z)='0'
   THEN
      IF z=0
      THEN
         s := Slice(Mark(s), z+1, 0)
      ELSE
         s := ConCat(Slice(Mark(s), 0, z),
                     Mark(Slice(Mark(s), z+1, 0)))
      END ;
      l := DynamicStrings.Length(s)
   ELSE
      INC(point)
   END ;
   IF i<l
   THEN
      s := Slice(Mark(s), 0, i) ;
      l := DynamicStrings.Length(s) ;
      IF l<point
      THEN
         s := ConCat(s, Mult(Mark(InitStringChar('0')), point-l))
      END
   END ;
   (* re-insert the point *)
   IF point>=0
   THEN
      IF point=0
      THEN
         s := ConCat(InitStringChar('.'), Mark(s))
      ELSE
         s := ConCat(ConCatChar(Slice(Mark(s), 0, point), '.'),
                     Mark(Slice(Mark(s), point, 0)))
      END
   END ;
   RETURN( s )
END doSigFig ;


(*
   carryOne - add a carry at position, i.
*)

PROCEDURE carryOne (s: String; i: CARDINAL) : String ;
BEGIN
   IF i>=0
   THEN
      IF IsDigit(char(s, i))
      THEN
         IF char(s, i)='9'
         THEN
            IF i=0
            THEN
               s := ConCat(InitStringChar('1'), Mark(s)) ;
               RETURN s
            ELSE
               s := ConCat(ConCatChar(Slice(Mark(s), 0, i), '0'),
                           Mark(Slice(Mark(s), i+1, 0))) ;
               RETURN carryOne(s, i-1)
            END
         ELSE
            IF i=0
            THEN
               s := ConCat(InitStringChar(CHR(ORD(char(s, i))+1)),
                           Mark(Slice(Mark(s), i+1, 0)))
            ELSE
               s := ConCat(ConCatChar(Slice(Mark(s), 0, i),
                                      CHR(ORD(char(s, i))+1)),
                           Mark(Slice(Mark(s), i+1, 0)))
            END
         END
      END
   END ;
   RETURN s
END carryOne ;


END StringConvert.
