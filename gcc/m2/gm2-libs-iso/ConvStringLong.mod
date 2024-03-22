(* ConvStringLong.mod converts floating point numbers to Strings.

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

IMPLEMENTATION MODULE ConvStringLong ;

FROM DynamicStrings IMPORT InitString, KillString, ConCat, ConCatChar,
                           Slice, Length, Mult, Mark, InitStringCharStar,
                           InitStringChar, Index, char ;
FROM StringConvert IMPORT IntegerToString, ToSigFig ;
FROM ldtoa IMPORT ldtoa, Mode ;
FROM libc IMPORT free ;
FROM SYSTEM IMPORT ADDRESS ;


(*
   IsDigit - returns TRUE if, ch, lies between '0'..'9'.
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch>='0') AND (ch<='9')
END IsDigit ;


(*
   RealToFloatString - converts a real with, sigFigs, into a string
                       and returns the result as a string.
*)

PROCEDURE RealToFloatString (real: LONGREAL; sigFigs: CARDINAL) : String ;
VAR
   point, l,
   powerOfTen: INTEGER ;
   s         : String ;
   r         : ADDRESS ;
   sign      : BOOLEAN ;
BEGIN
   r := ldtoa(real, maxsignificant, 100, point, sign) ;
   s := InitStringCharStar(r) ;
   free(r) ;
   IF sigFigs>0
   THEN
      l := Length(s) ;
      IF (l>0) AND IsDigit(char(s, 0))
      THEN
         IF VAL(INTEGER, sigFigs)<l
         THEN
            s := Slice(ToSigFig(s, sigFigs), 0, sigFigs)
         ELSE
            (* add '0's to make up significant figures *)
            s := ConCat(s, Mark(Mult(InitStringChar('0'), l-VAL(INTEGER, sigFigs))))
         END ;
         l := Length(s) ;
         (*
          *  we reassign point to 1 and adjust the exponent
          *  accordingly, so we can achieve the format X.XXXE+X
          *)
         powerOfTen := point-1 ;
         point := 1 ;

         IF (point<l) AND (point<VAL(INTEGER, sigFigs))
         THEN
            s := ConCat(ConCatChar(Slice(s, 0, point), '.'),
                        Slice(s, point, 0))
         END ;

         IF powerOfTen#0
         THEN
            s := ConCat(ConCatChar(s, 'E'),
                        IntegerToString(powerOfTen, 0, ' ', TRUE, 10, FALSE))
         END
      END ;
      IF sign
      THEN
         s := ConCat(InitStringChar('-'), Mark(s))
      END
   END ;
   RETURN( s )
END RealToFloatString ;


(*
   RealToEngString - converts the value of real to floating-point
                     string form, with sigFigs significant figures.
                     The number is scaled with one to three digits
                     in the whole number part and with an exponent
                     that is a multiple of three.
*)

PROCEDURE RealToEngString (real: LONGREAL; sigFigs: CARDINAL) : String ;
VAR
   offset,
   point,
   powerOfTen: INTEGER ;
   s         : String ;
   l         : CARDINAL ;
   r         : ADDRESS ;
   sign      : BOOLEAN ;
BEGIN
   r := ldtoa(real, maxsignificant, 100, point, sign) ;
   s := InitStringCharStar(r) ;
   free(r) ;
   IF sigFigs>0
   THEN
      l := Length(s) ;
      IF (l>0) AND IsDigit(char(s, 0))
      THEN
         IF sigFigs<l
         THEN
            s := Slice(ToSigFig(s, sigFigs), 0, sigFigs)
         ELSE
            (* add '0's to make up significant figures *)
            s := ConCat(s, Mark(Mult(InitStringChar('0'), l-sigFigs)))
         END ;
         l := Length(s) ;
         IF (point>0) AND (point<=2)
         THEN
            (* current range is fine, no need for a exponent *)
            powerOfTen := 0 ;
            IF point>VAL(INTEGER, sigFigs)
            THEN
               (* add '0's to make up required mantissa length *)
               s := ConCat(s, Mark(Mult(InitStringChar('0'), point-VAL(INTEGER, sigFigs)))) ;
               l := Length(s)
            END
         ELSE
            (*
             *  desire a value of point which lies between 1..3
             *  this allows the mantissa to have the format
             *  X.XXX  or  XX.XX  or XXX.X
             *)
            powerOfTen := point-VAL(INTEGER, l) ;
            point := point-powerOfTen ;
            offset := 0 ;
            IF point>3
            THEN
               offset := (point DIV 3) * 3 ;
               point := point-offset ;
               powerOfTen := powerOfTen+offset
            ELSIF point<0
            THEN
               offset := (ABS(point) DIV 3) * 3 ;
               point := point+offset ;
               powerOfTen := powerOfTen-offset
            END ;
            IF powerOfTen<0
            THEN
               IF ABS(powerOfTen) MOD 3#0
               THEN
                  offset := 3-(ABS(powerOfTen) MOD 3)
               END
            ELSE
               (* at this stage, point >= sigFigs *)
               IF powerOfTen MOD 3#0
               THEN
                  offset := -(3-(powerOfTen MOD 3))
               END
            END ;
            IF offset+point>VAL(INTEGER, sigFigs)
            THEN
               (* add '0's to make up required mantissa length *)
               s := ConCat(s, Mark(Mult(InitStringChar('0'), offset+point-VAL(INTEGER, sigFigs)))) ;
               l := Length(s)
            END ;
            (* now adjust point and powerOfTen by offset *)
            point := point + offset ;
            powerOfTen := powerOfTen - offset
         END ;

         IF point<0
         THEN
            s := ConCat(ConCat(InitString('0.'), Mult(InitStringChar('0'), -point)), s)
         ELSIF (point>0) AND (point<VAL(INTEGER, l)) AND (point<VAL(INTEGER, sigFigs))
         THEN
            s := ConCat(ConCatChar(Slice(s, 0, point), '.'),
                        Slice(s, point, 0))
         END ;

         IF powerOfTen#0
         THEN
            s := ConCat(ConCatChar(s, 'E'),
                        IntegerToString(powerOfTen, 0, ' ', TRUE, 10, FALSE))
         END
      END ;
      IF sign
      THEN
         s := ConCat(InitStringChar('-'), Mark(s))
      END
   END ;
   RETURN( s )
END RealToEngString ;


(*
   RealToFixedString - returns the number of characters in the fixed-point
                       string representation of real rounded to the given
                       place relative to the decimal point.
*)

PROCEDURE RealToFixedString (real: LONGREAL; place: INTEGER) : String ;
VAR
   l,
   point: INTEGER ;
   sign : BOOLEAN ;
   r    : ADDRESS ;
   s    : String ;
BEGIN
   r := ldtoa(real, maxsignificant, 100, point, sign) ;
   s := InitStringCharStar(r) ;
   free(r) ;
   l := Length(s) ;
   IF (l>0) AND IsDigit(char(s, 0))
   THEN
      IF point+place>=0
      THEN
         (* add decimal point at correct position *)
         IF point<0
         THEN
            s := ConCat(ConCat(InitString('0.'), Mult(InitStringChar('0'), -point)), s)
         ELSIF point=0
         THEN
            s := ConCat(InitString('0.'), Mark(s))
         ELSIF point<l
         THEN
            s := ConCat(ConCatChar(Slice(s, 0, point), '.'),
                        Slice(s, point, 0))
         END ;
         IF place<0
         THEN
            s := ToSigFig(s, point+place+1)
         ELSE
            s := ToSigFig(s, point+place)
         END ;
         l := Length(s) ;
         IF place>=0
         THEN
            IF Index(s, '.', 0)<0
            THEN
               s := ConCatChar(s, '.') ;
               s := ConCat(s, Mark(Mult(InitStringChar('0'), place)))
            ELSE
               point := Index(s, '.', 0) ;
               IF l-point<place
               THEN
                  s := ConCat(s, Mark(Mult(InitStringChar('0'), l-point-place)))
               END
            END
         END
      ELSE
         IF place<0
         THEN
            s := InitString('0')
         ELSIF place=0
         THEN
            s := InitString('0.')
         ELSE
            s := InitString('0.0')
         END
      END
   END ;
   IF sign
   THEN
      s := ConCat(InitStringChar('-'), Mark(s))
   END ;
   RETURN( s )
END RealToFixedString ;


END ConvStringLong.
