(* RealConversions.mod provides a Logitech-3.0 compatible module.

Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE RealConversions ;

FROM DynamicStrings IMPORT String, InitString, KillString, CopyOut, Length,
                           ConCat, ConCatChar, Mark, RemoveWhitePrefix,
                           InitStringChar, Mult, Slice, Index, char, string,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

FROM StringConvert IMPORT LongrealToString, StringToLongreal,
                          StringToInteger, itos ;

FROM ASCII IMPORT nul ;
FROM Builtins IMPORT logl, log10l ;
FROM libm IMPORT powl ;
FROM libc IMPORT printf ;


CONST
   Debugging = FALSE ;
   DefaultExponentDigits = 0 ;

VAR
   ExponentDigits: CARDINAL ;

(*
#define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#define Dup(X) DupDB(X, __FILE__, __LINE__)
#define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
*)


(*
   IsNan - return TRUE if x is a nan (which never are equal to themselves).
*)

PROCEDURE IsNan (x: LONGREAL) : BOOLEAN ;
BEGIN
   RETURN x # x
END IsNan ;


(*
   logl10 - this is a local implementation of log10l, currently the ppe64le
            builtin log10l is broken.
*)

PROCEDURE logl10 (r: LONGREAL) : LONGREAL ;
BEGIN
   IF Debugging
   THEN
      printf ("logl10 (%lf) = %lf,  logl/logl(10.0) = %lf\n",
              r, log10l (r), logl(r)/logl(10.0))
   END ;
   RETURN logl(r)/logl(10.0)
END logl10 ;


(*
   logi10 -
*)

PROCEDURE logi10 (i: INTEGER) : INTEGER ;
VAR
   j: INTEGER ;
BEGIN
   j := 0 ;
   IF i<0
   THEN
      WHILE i<-9 DO
         DEC(j) ;
         i := i DIV 10
      END
   ELSE
      WHILE i>9 DO
         INC(j) ;
         i := i DIV 10
      END
   END ;
   RETURN j
END logi10 ;


(*
   powl10 -
*)

PROCEDURE powl10 (i: INTEGER) : LONGREAL ;
VAR
   r: LONGREAL ;
BEGIN
   r := 1.0 ;
   IF i<0
   THEN
      WHILE i<0 DO
         r := r/10.0 ;
         INC(i)
      END
   ELSE
      WHILE i>0 DO
         r := r*10.0 ;
         DEC(i)
      END
   END ;
   RETURN r
END powl10 ;


(*
   doPowerOfTen - safely returns the exponent of a LONGREAL as an INTEGER.
*)

PROCEDURE doPowerOfTen (r: LONGREAL) : INTEGER ;
VAR
   i   : INTEGER ;
   c, d: LONGREAL ;
BEGIN
   IF r=0.0
   THEN
      RETURN( 0 )
   ELSE
      IF r<0.0
      THEN
         c := -r
      ELSE
         c := r
      END ;
      IF c>=1.0
      THEN
         RETURN VAL (INTEGER, log10l (c))
      ELSE
         RETURN VAL (INTEGER, log10l (c)) -1
      END
   END
END doPowerOfTen ;


(*
   SetNoOfExponentDigits - sets the number of exponent digits to be
                           used during future calls of LongRealToString
                           and RealToString providing that the width
                           is sufficient.
                           If this value is set to 0 (the default) then
                           the number digits used is the minimum necessary.
*)

PROCEDURE SetNoOfExponentDigits (places: CARDINAL) ;
BEGIN
   ExponentDigits := places
END SetNoOfExponentDigits ;


(*
   Pad - prefixes spaces in front of, s, so that width characters are used.
*)

PROCEDURE Pad (s: String; width: CARDINAL) : String ;
BEGIN
   IF Length(s)<width
   THEN
      RETURN( ConCat(Mult(InitStringChar(' '), width-Length(s)), Mark(s)) )
   ELSE
      RETURN( s )
   END
END Pad ;


(*
   MakeNanString -
*)

PROCEDURE MakeNanString (VAR str: ARRAY OF CHAR; width: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := Pad(InitString('nan'), width) ;
   IF Length(s)<=width
   THEN
      CopyOut(str, s)
   ELSE
      str[0] := nul
   END ;
   s := KillString(s)
END MakeNanString ;


(*
   RealToString - converts a real, r, into a right justified string, str.
                  The number of digits to the right of the decimal point
                  is given in, digits.  The value, width, represents the
                  maximum number of characters to be used in the string,
                  str.

                  If digits is negative then exponental notation is used
                  whereas if digits is positive then fixed point notation
                  is used.

                  If, r, is less than 0.0 then a '-' preceeds the value,
                  str.  However, if, r, is >= 0.0 a '+' is not added.

                  If the conversion of, r, to a string requires more
                  than, width, characters then the string, str, is set
                  to a nul string and, ok is assigned FALSE.

                  For fixed point notation the minimum width required is
                  ABS(width)+8

                  For exponent notation the minimum width required is
                  ABS(digits)+2+log10(magnitude).

                  if r is a NaN then the string 'nan' is returned formatted
                  and ok will be FALSE.
*)

PROCEDURE RealToString (r: REAL; digits, width: INTEGER;
                        VAR str: ARRAY OF CHAR; VAR ok: BOOLEAN) ;
VAR
   l: LONGREAL ;
BEGIN
   (* --fixme-- *)
   (* IF IsNan(r)
      THEN
         ok := FALSE ;
         MakeNanString(str, width) ;
      ELSE
         ...
      END
    *)
   l := VAL(LONGREAL, r) ;
   LongRealToString(l, digits, width, str, ok)
END RealToString ;


(*
   doLongRealToString -
*)

PROCEDURE doLongRealToString (r: LONGREAL; digits, width, powerOfTen: INTEGER; VAR ok: BOOLEAN) : String ;
VAR
   sign, s , e  : String ;
   point, len: INTEGER ;
BEGIN
   IF digits>0
   THEN
      ok := TRUE ;
      RETURN( Slice(Mark(LongrealToString(r, width, digits)), 0, width) )
   ELSE
      digits := ABS(digits) ;
      IF r>=0.0
      THEN
         sign := InitString('')
      ELSE
         sign := InitString('-') ;
         r := -r
      END ;
      s := InitString('') ;
      r := r*powl10(-powerOfTen) ;
      IF width>=VAL(INTEGER, Length(s))+2
      THEN
         s := ConCat(s, Mark(RemoveWhitePrefix(Mark(LongrealToString(r, width+1, width))))) ;
         IF Debugging
         THEN
            printf('value returned was %s\n', string(s))
         END ;
         point := Index(s, '.', 0) ;
         IF point>=0
         THEN
            (* remove the '.' *)
            s := ConCat(Slice(Mark(s), 0, point), Mark(Slice(Mark(s), point+1, 0))) ;
            s := Slice(Mark(s), 0, width) ;
            IF Debugging
            THEN
               printf('value returned was %s\n', string(s))
            END ;
            point := powerOfTen ;
            (* now strip off trailing '0's *)
            WHILE (Length(s)>2) AND (char(s, -1)='0') DO
               s := Slice(Mark(s), 0, -1)
            END ;
            len := Length(s) ;
            IF Debugging
            THEN
               printf('point = %d, powerOfTen = %d, len = %d,  len-point = %d\n',
                      point, powerOfTen, len, len-point) ;
               printf('value returned was %s\n', string(s))
            END ;
            WHILE len<2 DO
               s := ConCat(s, Mark(InitString('0'))) ;
               len := Length(s)
            END ;
            point := 1 ;
            IF digits>width-point-2
            THEN
               (* need to round the result *)
               digits := width-point-2
            END ;
            s := ConCat(Slice(s, 0, point),
                        Mark(ConCat(InitStringChar('.'),
                                    Mark(Slice(Mark(s), point, point+digits))))) ;
            IF Debugging
            THEN
               printf("value returned was '%s'\n", string(s))
            END ;
            (* and add trailing '0's if needed *)
            IF VAL(INTEGER, Length(s))-point<digits+1
            THEN
               s := ConCat(s, Mark(Mult(Mark(InitString('0')), digits+1-(VAL(INTEGER, Length(s))-point)))) ;
               IF Debugging
               THEN
                  printf("value returned was '%s'\n", string(s))
               END
            END ;
            IF powerOfTen-point+1>=0
            THEN
               e := ConCat(InitString('E+'),
                           Mark(itos(powerOfTen-point+1, ExponentDigits, '0', FALSE)))
            ELSE
               e := ConCat(InitString('E-'),
                           Mark(itos(ABS(powerOfTen-point+1), ExponentDigits, '0', FALSE)))
            END ;
            IF Debugging
            THEN
               printf("value returned was '%s' and '%s'\n", string(s), string(e))
            END
         END ;
         s := ConCat(sign, Mark(ConCat(s, Mark(e)))) ;
         ok := TRUE
      ELSE
         s := InitString('') ;
         ok := FALSE
      END
   END ;
   RETURN( s )
END doLongRealToString ;


(*
   LongRealToString - converts a real, r, into a right justified string, str.
                      The number of digits to the right of the decimal point
                      is given in, digits.  The value, width, represents the
                      maximum number of characters to be used in the string,
                      str.

                      If digits is negative then exponent notation is used
                      whereas if digits is positive then fixed point notation
                      is used.

                      If, r, is less than 0.0 then a '-' preceeds the value,
                      str.  However, if, r, is >= 0.0 a '+' is not added.

                      If the conversion of, r, to a string requires more
                      than, width, characters then the string, str, is set
                      to a nul string and, ok is assigned FALSE.

                      For fixed point notation the minimum width required is
                      ABS(width)+8

                      For exponent notation the minimum width required is
                      ABS(digits)+2+log10(magnitude).

                      Examples:
                      RealToString(100.0, 10, 10, a, ok)       ->  '100.000000'
                      RealToString(100.0, -5, 12, a, ok)       ->  '  1.00000E+2'

                      RealToString(123.456789, 10, 10, a, ok)  ->  '123.456789'
                      RealToString(123.456789, -5, 13, a, ok)  ->  '    1.23456E+2'

                      RealToString(123.456789, -2, 15, a, ok)  ->  '          1.23E+2'

                      if r is a NaN then the string 'nan' is returned formatted and
                      ok will be FALSE.
*)

PROCEDURE LongRealToString (r: LONGREAL; digits, width: INTEGER;
                            VAR str: ARRAY OF CHAR; VAR ok: BOOLEAN) ;
VAR
   s         : String ;
   powerOfTen: INTEGER ;
BEGIN
   IF IsNan (r)
   THEN
      ok := FALSE ;
      MakeNanString (str, width) ;
      RETURN
   END ;
   powerOfTen := doPowerOfTen(r) ;
   IF (powerOfTen=MAX(INTEGER)) OR (powerOfTen=MIN(INTEGER))
   THEN
      ok := FALSE ;
      MakeNanString(str, width) ;
      RETURN
   END ;
   s := doLongRealToString(r, digits, width, powerOfTen, ok) ;
   ok := TRUE ;
   IF VAL(INTEGER, Length(s))<=width
   THEN
      s := ConCat(Mult(Mark(InitStringChar(' ')), width-VAL(INTEGER, Length(s))), Mark(s)) ;
      IF Debugging
      THEN
         printf('value returned was %s\n', string(s))
      END ;
      CopyOut(str, s)
   ELSE
      str[0] := nul ;
      ok := FALSE
   END ;
   s := KillString(s)
END LongRealToString ;


(*
   StringToLongReal - converts, str, into a LONGREAL, r. The parameter, ok, is
                      set to TRUE if the conversion was successful.
*)

PROCEDURE StringToLongReal (str: ARRAY OF CHAR; VAR r: LONGREAL; VAR ok: BOOLEAN) ;
VAR
   s: String ;
BEGIN
   s := InitString(str) ;
   r := StringToLongreal(s, ok) ;
   s := KillString(s)
END StringToLongReal ;


(*
   StringToReal - converts, str, into a REAL, r. The parameter, ok, is
                  set to TRUE if the conversion was successful.
*)

PROCEDURE StringToReal (str: ARRAY OF CHAR; VAR r: REAL; VAR ok: BOOLEAN) ;
VAR
   l: LONGREAL ;
BEGIN
   StringToLongReal(str, l, ok) ;
   IF ok
   THEN
      r := VAL(REAL, l)
   END
END StringToReal ;


BEGIN
   ExponentDigits := DefaultExponentDigits
END RealConversions.
