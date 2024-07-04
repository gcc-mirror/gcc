(* NumberIO.mod provides conversion of ordinal numbers.

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

IMPLEMENTATION MODULE NumberIO ;


FROM ASCII IMPORT nul ;
FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM StrLib IMPORT StrLen, StrRemoveWhitePrefix ;


CONST
   MaxLineLength = 79 ;
   MaxDigits     = 20 ;
   MaxHexDigits  = 20 ;
   MaxOctDigits  = 40 ;
   MaxBits       = 64 ;


PROCEDURE CardToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
VAR
   i, j,
   Higha : CARDINAL ;
   buf   : ARRAY [1..MaxDigits] OF CARDINAL ;
BEGIN
   i := 0 ;
   REPEAT
      INC(i) ;
      IF i>MaxDigits
      THEN
         WriteString('NumberIO - increase MaxDigits') ; WriteLn ;
         HALT
      END ;
      buf[i] := x MOD 10 ;
      x := x DIV 10 ;
   UNTIL x=0 ;
   j := 0 ;
   Higha := HIGH(a) ;
   WHILE (n>i) AND (j<=Higha) DO
      a[j] := ' ' ;
      INC(j) ;
      DEC(n)
   END ;
   WHILE (i>0) AND (j<=Higha) DO
      a[j] := CHR( buf[i] + ORD('0') ) ;
      INC(j) ;
      DEC(i)
   END ;
   IF j<=Higha
   THEN
      a[j] := nul
   END
END CardToStr ;


PROCEDURE StrToCard (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;
VAR
   i     : CARDINAL ;
   ok    : BOOLEAN ;
   higha : CARDINAL ;
BEGIN
   StrRemoveWhitePrefix(a, a) ;
   higha := StrLen(a) ;
   i := 0 ;
   ok := TRUE ;
   WHILE ok DO
      IF i<higha
      THEN
         IF (a[i]<'0') OR (a[i]>'9')
         THEN
            INC(i)
         ELSE
            ok := FALSE
         END
      ELSE
         ok := FALSE
      END
   END ;
   x := 0 ;
   IF i<higha
   THEN
      ok := TRUE ;
      REPEAT
         x := 10*x + (ORD(a[i])-ORD('0')) ;
         IF i<higha
         THEN
            INC(i) ;
            IF (a[i]<'0') OR (a[i]>'9')
            THEN
               ok := FALSE
            END
         ELSE
            ok := FALSE
         END
      UNTIL NOT ok ;
   END
END StrToCard ;


PROCEDURE IntToStr (x: INTEGER; n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
VAR
   i, j, c,
   Higha   : CARDINAL ;
   buf     : ARRAY [1..MaxDigits] OF CARDINAL ;
   Negative: BOOLEAN ;
BEGIN
   IF x<0
   THEN
      Negative := TRUE ;
      c := VAL(CARDINAL, ABS(x+1))+1 ;
      IF n>0
      THEN
         DEC(n)
      END
   ELSE
      c := x ;
      Negative := FALSE
   END ;
   i := 0 ;
   REPEAT
      INC(i) ;
      IF i>MaxDigits
      THEN
         WriteString('NumberIO - increase MaxDigits') ; WriteLn ;
         HALT
      END ;
      buf[i] := c MOD 10 ;
      c := c DIV 10 ;
   UNTIL c=0 ;
   j := 0 ;
   Higha := HIGH(a) ;
   WHILE (n>i) AND (j<=Higha) DO
      a[j] := ' ' ;
      INC(j) ;
      DEC(n)
   END ;
   IF Negative
   THEN
      a[j] := '-' ;
      INC(j)
   END ;
   WHILE (i#0) AND (j<=Higha) DO
      a[j] := CHR( buf[i] + ORD('0') ) ;
      INC(j) ;
      DEC(i)
   END ;
   IF j<=Higha
   THEN
      a[j] := nul
   END
END IntToStr ;


PROCEDURE StrToInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;
VAR
   i        : CARDINAL ;
   ok,
   Negative : BOOLEAN ;
   higha    : CARDINAL ;
BEGIN
   StrRemoveWhitePrefix(a, a) ;
   higha := StrLen(a) ;
   i := 0 ;
   Negative := FALSE ;
   ok := TRUE ;
   WHILE ok DO
      IF i<higha
      THEN
         IF a[i]='-'
         THEN
            INC(i) ;
            Negative := NOT Negative
         ELSIF (a[i]<'0') OR (a[i]>'9')
         THEN
            INC(i)
         ELSE
            ok := FALSE
         END
      ELSE
         ok := FALSE
      END
   END ;
   x := 0 ;
   IF i<higha
   THEN
      ok := TRUE ;
      REPEAT
         IF Negative
         THEN
            x := 10*x - INTEGER(ORD(a[i])-ORD('0'))
         ELSE
            x := 10*x + INTEGER(ORD(a[i])-ORD('0'))
         END ;
         IF i<higha
         THEN
            INC(i) ;
            IF (a[i]<'0') OR (a[i]>'9')
            THEN
               ok := FALSE
            END
         ELSE
            ok := FALSE
         END
      UNTIL NOT ok ;
   END
END StrToInt ;


PROCEDURE HexToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
VAR
   i, j,
   Higha : CARDINAL ;
   buf   : ARRAY [1..MaxHexDigits] OF CARDINAL ;
BEGIN
   i := 0 ;
   REPEAT
      INC(i) ;
      IF i>MaxHexDigits
      THEN
         WriteString('NumberIO - increase MaxDigits') ; WriteLn ;
         HALT
      END ;
      buf[i] := x MOD 010H ;
      x := x DIV 010H ;
   UNTIL x=0 ;
   j := 0 ;
   Higha := HIGH(a) ;
   WHILE (n>i) AND (j<=Higha) DO
      a[j] := '0' ;
      INC(j) ;
      DEC(n)
   END ;
   WHILE (i#0) AND (j<=Higha) DO
      IF buf[i]<10
      THEN
         a[j] := CHR( buf[i] + ORD('0') )
      ELSE
         a[j] := CHR( buf[i] + ORD('A')-10 )
      END ;
      INC(j) ;
      DEC(i)
   END ;
   IF j<=Higha
   THEN
      a[j] := nul
   END
END HexToStr ;


PROCEDURE StrToHex (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;
VAR
   i: INTEGER ;
BEGIN
   StrToHexInt(a, i) ;
   x := VAL(CARDINAL, i)
END StrToHex ;


PROCEDURE StrToHexInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;
VAR
   i     : CARDINAL ;
   ok    : BOOLEAN ;
   higha : CARDINAL ;
BEGIN
   StrRemoveWhitePrefix(a, a) ;
   higha := StrLen(a) ;
   i := 0 ;
   ok := TRUE ;
   WHILE ok DO
      IF i<higha
      THEN
         IF ((a[i]>='0') AND (a[i]<='9')) OR ((a[i]>='A') AND (a[i]<='F'))
         THEN
            ok := FALSE
         ELSE
            INC(i)
         END
      ELSE
         ok := FALSE
      END
   END ;
   x := 0 ;
   IF i<higha
   THEN
      ok := TRUE ;
      REPEAT
         IF (a[i]>='0') AND (a[i]<='9')
         THEN
            x := 010H*x + VAL(INTEGER, (ORD(a[i])-ORD('0')))
         ELSIF (a[i]>='A') AND (a[i]<='F')
         THEN
            x := 010H*x + VAL(INTEGER, (ORD(a[i])-ORD('A')+10))
         END ;
         IF i<higha
         THEN
            INC(i) ;
            IF ((a[i]<'0') OR (a[i]>'9')) AND ((a[i]<'A') OR (a[i]>'F'))
            THEN
               ok := FALSE
            END
         ELSE
            ok := FALSE
         END
      UNTIL NOT ok ;
   END
END StrToHexInt ;


PROCEDURE OctToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
VAR
   i, j,
   Higha : CARDINAL ;
   buf   : ARRAY [1..MaxOctDigits] OF CARDINAL ;
BEGIN
   i := 0 ;
   REPEAT
      INC(i) ;
      IF i>MaxOctDigits
      THEN
         WriteString('NumberIO - increase MaxDigits') ; WriteLn ;
         HALT
      END ;
      buf[i] := x MOD 8 ;
      x := x DIV 8 ;
   UNTIL x=0 ;
   j := 0 ;
   Higha := HIGH(a) ;
   WHILE (n>i) AND (j<=Higha) DO
      a[j] := ' ' ;
      INC(j) ;
      DEC(n)
   END ;
   WHILE (i>0) AND (j<=Higha) DO
      a[j] := CHR( buf[i] + ORD('0') ) ;
      INC(j) ;
      DEC(i)
   END ;
   IF j<=Higha
   THEN
      a[j] := nul
   END
END OctToStr ;


PROCEDURE StrToOct (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;
VAR
   i: INTEGER ;
BEGIN
   StrToOctInt(a, i) ;
   x := VAL(CARDINAL, i)
END StrToOct ;


PROCEDURE StrToOctInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;
VAR
   i     : CARDINAL ;
   ok    : BOOLEAN ;
   higha : CARDINAL ;
BEGIN
   StrRemoveWhitePrefix(a, a) ;
   higha := StrLen(a) ;
   i := 0 ;
   ok := TRUE ;
   WHILE ok DO
      IF i<higha
      THEN
         IF (a[i]<'0') OR (a[i]>'7')
         THEN
            INC(i)
         ELSE
            ok := FALSE
         END
      ELSE
         ok := FALSE
      END
   END ;
   x := 0 ;
   IF i<higha
   THEN
      ok := TRUE ;
      REPEAT
         x := 8*x + VAL(INTEGER, (ORD(a[i])-ORD('0'))) ;
         IF i<higha
         THEN
            INC(i) ;
            IF (a[i]<'0') OR (a[i]>'7')
            THEN
               ok := FALSE
            END
         ELSE
            ok := FALSE
         END
      UNTIL NOT ok ;
   END
END StrToOctInt ;


PROCEDURE BinToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
VAR
   i, j,
   Higha : CARDINAL ;
   buf   : ARRAY [1..MaxBits] OF CARDINAL ;
BEGIN
   i := 0 ;
   REPEAT
      INC(i) ;
      IF i>MaxBits
      THEN
         WriteString('NumberIO - increase MaxBits') ; WriteLn ;
         HALT
      END ;
      buf[i] := x MOD 2 ;
      x := x DIV 2 ;
   UNTIL x=0 ;
   j := 0 ;
   Higha := HIGH(a) ;
   WHILE (n>i) AND (j<=Higha) DO
      a[j] := ' ' ;
      INC(j) ;
      DEC(n)
   END ;
   WHILE (i>0) AND (j<=Higha) DO
      a[j] := CHR( buf[i] + ORD('0') ) ;
      INC(j) ;
      DEC(i)
   END ;
   IF j<=Higha
   THEN
      a[j] := nul
   END
END BinToStr ;


PROCEDURE StrToBin (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;
VAR
   i: INTEGER ;
BEGIN
   StrToBinInt(a, i) ;
   x := VAL(CARDINAL, i)
END StrToBin ;


PROCEDURE StrToBinInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;
VAR
   i     : CARDINAL ;
   ok    : BOOLEAN ;
   higha : CARDINAL ;
BEGIN
   StrRemoveWhitePrefix(a, a) ;
   higha := StrLen(a) ;
   i := 0 ;
   ok := TRUE ;
   WHILE ok DO
      IF i<higha
      THEN
         IF (a[i]<'0') OR (a[i]>'1')
         THEN
            INC(i)
         ELSE
            ok := FALSE
         END
      ELSE
         ok := FALSE
      END
   END ;
   x := 0 ;
   IF i<higha
   THEN
      ok := TRUE ;
      REPEAT
         x := 2*x + VAL(INTEGER, (ORD(a[i])-ORD('0'))) ;
         IF i<higha
         THEN
            INC(i) ;
            IF (a[i]<'0') OR (a[i]>'1')
            THEN
               ok := FALSE
            END
         ELSE
            ok := FALSE
         END
      UNTIL NOT ok ;
   END
END StrToBinInt ;


PROCEDURE ReadOct (VAR x: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString( a ) ;
   StrToOct( a, x )
END ReadOct ;


PROCEDURE WriteOct (x, n: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   OctToStr( x, n, a ) ;
   WriteString( a )
END WriteOct ;


PROCEDURE ReadBin (VAR x: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString(a) ;
   StrToBin(a, x)
END ReadBin ;


PROCEDURE WriteBin (x, n: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   BinToStr( x, n, a ) ;
   WriteString( a )
END WriteBin ;


PROCEDURE ReadCard (VAR x: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString( a ) ;
   StrToCard( a, x )
END ReadCard ;


PROCEDURE WriteCard (x, n: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   CardToStr( x, n, a ) ;
   WriteString( a )
END WriteCard ;


PROCEDURE ReadInt (VAR x: INTEGER) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString( a ) ;
   StrToInt( a, x )
END ReadInt ;


PROCEDURE WriteInt (x: INTEGER; n: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   IntToStr( x, n, a ) ;
   WriteString( a )
END WriteInt ;


PROCEDURE ReadHex (VAR x: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString( a ) ;
   StrToHex( a, x )
END ReadHex ;


PROCEDURE WriteHex (x, n: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   HexToStr( x, n, a ) ;
   WriteString( a )
END WriteHex ;


END NumberIO.
