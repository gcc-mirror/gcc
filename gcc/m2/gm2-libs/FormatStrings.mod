(* FormatStrings.mod provides a pseudo printf capability.

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

IMPLEMENTATION MODULE FormatStrings ;

FROM DynamicStrings IMPORT InitString, InitStringChar, Mark,
                           ConCat, Slice, Index, char, string,
                           Assign, Length, Mult, Dup, ConCatChar,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB,
                           KillString ;

FROM StringConvert IMPORT IntegerToString, CardinalToString, hstoc ;
FROM SYSTEM IMPORT ADDRESS ;

IMPORT ASCII ;


(*
#undef GM2_DEBUG_FORMATSTRINGS
#if defined(GM2_DEBUG_FORMATSTRINGS)
#  define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#  define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#  define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#  define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#  define Dup(X) DupDB(X, __FILE__, __LINE__)
#  define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
#endif
*)


(*
   doDSdbEnter -
*)

PROCEDURE doDSdbEnter ;
BEGIN
   PushAllocation
END doDSdbEnter ;


(*
   doDSdbExit -
*)

PROCEDURE doDSdbExit (s: String) ;
BEGIN
   s := PopAllocationExemption (TRUE, s)
END doDSdbExit ;


(*
   DSdbEnter -
*)

PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;


(*
   DSdbExit -
*)

PROCEDURE DSdbExit (s: String) ;
BEGIN
END DSdbExit ;


(*
#if defined(GM2_DEBUG_FORMATSTRINGS)
#  define DBsbEnter doDBsbEnter
#  define DBsbExit  doDBsbExit
#endif
*)


(*
   IsDigit - returns TRUE if ch lies in the range: 0..9
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch>='0') AND (ch<='9') )
END IsDigit ;


(*
   Cast - casts a := b
*)

PROCEDURE Cast (VAR a: ARRAY OF BYTE; b: ARRAY OF BYTE) ;
VAR
   i: CARDINAL ;
BEGIN
   IF HIGH(a)=HIGH(b)
   THEN
      FOR i := 0 TO HIGH(a) DO
         a[i] := b[i]
      END
   ELSE
      HALT
   END
END Cast ;


(*
   isHex -
*)

PROCEDURE isHex (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN ( ((ch >= '0') AND (ch <= '9')) OR
            ((ch >= 'A') AND (ch <= 'F')) OR
            ((ch >= 'a') AND (ch <= 'f')) )
END isHex ;


(*
   toHex -
*)

PROCEDURE toHex (ch: CHAR) : CARDINAL ;
BEGIN
   IF ((ch >= '0') AND (ch <= '9'))
   THEN
      RETURN ORD (ch) - ORD ('0')
   ELSIF (ch >= 'A') AND (ch <= 'F')
   THEN
      RETURN ORD (ch) - ORD ('A') + 10
   ELSE
      RETURN ORD (ch) - ORD ('a') + 10
   END
END toHex ;


(*
   toOct -
*)

PROCEDURE toOct (ch: CHAR) : CARDINAL ;
BEGIN
   RETURN ORD (ch) - ORD ('0')
END toOct ;


(*
   isOct -
*)

PROCEDURE isOct (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch >= '0') AND (ch <= '8')
END isOct ;


(*
   HandleEscape - translates \a, \b, \e, \f, \n, \r, \x[hex] \[octal] into
                  their respective ascii codes.  It also converts \[any] into
                  a single [any] character.
*)

PROCEDURE HandleEscape (s: String) : String ;
VAR
   d   : String ;
   i, j: INTEGER ;
   ch  : CHAR ;
   b   : BYTE ;
BEGIN
   DSdbEnter ;
   d := InitString ('') ;
   i := Index (s, '\', 0) ;
   j := 0 ;
   WHILE i>=0 DO
      IF i>0
      THEN
         (* initially i might be zero which means the end of the string, which is not what we want.  *)
         d := ConCat (d, Slice (s, j, i))
      END ;
      ch := char (s, i+1) ;
      IF ch='a'
      THEN
         (* requires a bell.  *)
         d := ConCat (d, Mark (InitStringChar (ASCII.bel)))
      ELSIF ch='b'
      THEN
         (* requires a backspace.  *)
         d := ConCat (d, Mark (InitStringChar (ASCII.bs)))
      ELSIF ch='e'
      THEN
         (* requires a escape.  *)
         d := ConCat (d, Mark (InitStringChar (ASCII.esc)))
      ELSIF ch='f'
      THEN
         (* requires a formfeed.  *)
         d := ConCat (d, Mark (InitStringChar (ASCII.ff)))
      ELSIF ch='n'
      THEN
         (* requires a newline.  *)
         d := ConCat (d, Mark (InitStringChar (ASCII.nl)))
      ELSIF ch='r'
      THEN
         (* requires a carriage return.  *)
         d := ConCat (d, Mark (InitStringChar (ASCII.cr)))
      ELSIF ch='t'
      THEN
         (* requires a tab.  *)
         d := ConCat (d, Mark (InitStringChar (ASCII.tab)))
      ELSIF ch='x'
      THEN
         INC (i) ;
         IF isHex (char (s, i+1))
         THEN
            b := VAL (BYTE, toHex (char (s, i+1))) ;
            INC (i) ;
            IF isHex (char (s, i+1))
            THEN
               b := VAL (BYTE, VAL (CARDINAL, b) * 010H + toHex (char (s, i+1))) ;
               d := ConCat (d, Mark (InitStringChar (VAL (CHAR, b))))
            END
         END
      ELSIF isOct (ch)
      THEN
         b := VAL (BYTE, toOct (ch)) ;
         INC (i) ;
         IF isOct (char (s, i+1))
         THEN
            b := VAL (BYTE, VAL (CARDINAL, b) * 8 + toOct (char (s, i+1))) ;
            INC (i) ;
            IF isOct (char (s, i+1))
            THEN
               b := VAL (BYTE, VAL (CARDINAL, b) * 8 + toOct (char (s, i+1)))
            END
         END ;
         d := ConCat (d, Mark (InitStringChar (VAL (CHAR, b))))
      ELSE
         (* copy escaped character.  *)
         d := ConCat (d, Mark (InitStringChar (ch)))
      END ;
      INC (i, 2) ;
      j := i ;
      i := Index (s, '\', CARDINAL (i))
   END ;
   (*   s := Assign(s, Mark(ConCat(d, Mark(Slice(s, j, 0))))) ;   (* dont Mark(s) in the Slice as we Assign contents *) *)
   s := ConCat (d, Mark (Slice (Mark (s), j, 0))) ;
   DSdbExit (s) ;
   RETURN s
END HandleEscape ;


(*
   FormatString - returns a String containing, s, together with encapsulated
                  entity, w. It only formats the first %s or %d or %u with n.
                  A new string is returned.
*)

PROCEDURE FormatString (fmt: String; VAR startpos: INTEGER; in: String; w: ARRAY OF BYTE) : String ;
VAR
   s: String ;
BEGIN
   DSdbEnter ;
   IF startpos >= 0
   THEN
      s := PerformFormatString (fmt, startpos, in, w)
   ELSE
      s := Dup (in)
   END ;
   DSdbExit (s) ;
   RETURN s
END FormatString ;


PROCEDURE PerformFormatString (fmt: String; VAR startpos: INTEGER; in: String; w: ARRAY OF BYTE) : String ;
VAR
   left     : BOOLEAN ;
   u        : CARDINAL ;
   c,
   width,
   nextperc,
   afterperc: INTEGER ;
   leader,
   ch, ch2  : CHAR ;
   p        : String ;
BEGIN
   WHILE startpos >= 0 DO
      nextperc := Index (fmt, '%', startpos) ;
      afterperc := nextperc ;
      IF nextperc >= 0
      THEN
         INC (afterperc) ;
         IF char (fmt, afterperc)='-'
         THEN
            left := TRUE ;
            INC (afterperc)
         ELSE
            left := FALSE
         END ;
         ch := char (fmt, afterperc) ;
         IF ch = '0'
         THEN
            leader := '0'
         ELSE
            leader := ' '
         END ;
         width := 0 ;
         WHILE IsDigit (ch) DO
            width := (width*10)+VAL (INTEGER, ORD (ch) - ORD ('0')) ;
            INC (afterperc) ;
            ch := char (fmt, afterperc)
         END ;
         IF (ch='c') OR (ch='s')
         THEN
            INC (afterperc) ;
            IF (ch='c')
            THEN
               ch2 := w[0] ;
               p := ConCatChar (InitString (''), ch2)
            ELSE
               Cast (p, w) ;
               p := Dup (p)
            END ;
            IF (width>0) AND (VAL (INTEGER, Length (p)) < width)
            THEN
               IF left
               THEN
                  (* place trailing spaces after, p.  *)
                  p := ConCat(p,
                              Mark(Mult(Mark(InitString(' ')), width-VAL(INTEGER, Length(p)))))
               ELSE
                  (* padd string, p, with leading spaces.  *)
                  p := ConCat(Mult(Mark(InitString(' ')), width-VAL(INTEGER, Length(p))),
                              Mark(p))
               END
            END ;
            (* include string, p, into, in.  *)
            IF nextperc > 0
            THEN
               in := ConCat (in, Slice (fmt, startpos, nextperc))
            END ;
            in := ConCat (in, p) ;
            startpos := afterperc ;
            DSdbExit (NIL) ;
            RETURN in
         ELSIF ch='d'
         THEN
            INC (afterperc) ;
            Cast (c, w) ;
            in := Copy (fmt, in, startpos, nextperc) ;
            in := ConCat (in, IntegerToString (c, width, leader, FALSE, 10, FALSE)) ;
            startpos := afterperc ;
            DSdbExit (NIL) ;
            RETURN in
         ELSIF ch='x'
         THEN
            INC (afterperc) ;
            Cast (u, w) ;
            in := Copy (fmt, in, startpos, nextperc) ;
            in := ConCat (in, CardinalToString (u, width, leader, 16, TRUE)) ;
            startpos := afterperc ;
            DSdbExit (NIL) ;
            RETURN in
         ELSIF ch='u'
         THEN
            INC (afterperc) ;
            Cast (u, w) ;
            in := Copy (fmt, in, startpos, nextperc) ;
            in := ConCat (in, CardinalToString (u, width, leader, 10, FALSE)) ;
            startpos := afterperc ;
            DSdbExit (NIL) ;
            RETURN in
         ELSE
            INC (afterperc) ;
            (* copy format string.  *)
            IF nextperc > 0
            THEN
               in := ConCat (in, Slice (fmt, startpos, nextperc))
            END ;
            (* and the character after the %.  *)
            in := ConCat (in, Mark (InitStringChar (ch)))
         END ;
         startpos := afterperc
      ELSE
         (* nothing to do.  *)
         DSdbExit (NIL) ;
         RETURN in
      END
   END ;
   DSdbExit (NIL) ;
   RETURN in
END PerformFormatString ;


(*
   Copy - copies, fmt[start:end] -> in and returns in.  Providing that start >= 0.
*)

PROCEDURE Copy (fmt, in: String; start, end: INTEGER) : String ;
BEGIN
   IF start >= 0
   THEN
      IF end > 0
      THEN
         in := ConCat (in, Mark (Slice (fmt, start, end)))
      ELSIF end < 0
      THEN
         in := ConCat (in, Mark (Slice (fmt, start, 0)))
      END
   END ;
   RETURN in
END Copy ;


(*
   HandlePercent - pre-condition:  s, is a string.
                   Post-condition:  a new string is returned which is a copy of,
                   s, except %% is transformed into %.
*)

PROCEDURE HandlePercent (fmt, s: String; startpos: INTEGER) : String ;
VAR
   prevpos: INTEGER ;
BEGIN
   IF (startpos = VAL (INTEGER, Length (fmt))) OR (startpos < 0)
   THEN
      RETURN s
   ELSE
      prevpos := startpos ;
      WHILE (startpos >= 0) AND (prevpos < INTEGER (Length (fmt))) DO
         startpos := Index (fmt, '%', startpos) ;
         IF startpos >= prevpos
         THEN
            IF startpos > 0
            THEN
               s := ConCat (s, Mark (Slice (fmt, prevpos, startpos)))
            END ;
            INC (startpos) ;
            IF char (fmt, startpos) = '%'
            THEN
               s := ConCatChar (s, '%') ;
               INC (startpos)
            END ;
            prevpos := startpos
         END
      END ;
      IF (prevpos < INTEGER (Length (fmt)))
      THEN
         s := ConCat (s, Mark (Slice (fmt, prevpos, 0)))
      END ;
      RETURN s
   END
END HandlePercent ;


(*
   Sprintf0 - returns a String containing, s, after it has had its
              escape sequences translated.
*)

PROCEDURE Sprintf0 (fmt: String) : String ;
VAR
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   s := HandlePercent (fmt, InitString (''), 0) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf0 ;


(*
   Sprintf1 - returns a String containing, s, together with encapsulated
              entity, w. It only formats the first %s or %d with n.
*)

PROCEDURE Sprintf1 (fmt: String; w: ARRAY OF BYTE) : String ;
VAR
   i: INTEGER ;
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   i := 0 ;
   s := FormatString (fmt, i, InitString (''), w) ;
   s := HandlePercent (fmt, s, i) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf1 ;


(*
   Sprintf2 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf2 (fmt: String; w1, w2: ARRAY OF BYTE) : String ;
VAR
   i: INTEGER ;
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   i := 0 ;
   s := FormatString (fmt, i, InitString (''), w1) ;
   s := FormatString (fmt, i, s, w2) ;
   s := HandlePercent (fmt, s, i) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf2 ;


(*
   Sprintf3 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf3 (fmt: String; w1, w2, w3: ARRAY OF BYTE) : String ;
VAR
   i: INTEGER ;
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   i := 0 ;
   s := FormatString (fmt, i, InitString (''), w1) ;
   s := FormatString (fmt, i, s, w2) ;
   s := FormatString (fmt, i, s, w3) ;
   s := HandlePercent (fmt, s, i) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf3 ;


(*
   Sprintf4 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf4 (fmt: String; w1, w2, w3, w4: ARRAY OF BYTE) : String ;
VAR
   i: INTEGER ;
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   i := 0 ;
   s := FormatString (fmt, i, InitString (''), w1) ;
   s := FormatString (fmt, i, s, w2) ;
   s := FormatString (fmt, i, s, w3) ;
   s := FormatString (fmt, i, s, w4) ;
   s := HandlePercent (fmt, s, i) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf4 ;


END FormatStrings.
