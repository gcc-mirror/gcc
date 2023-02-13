(* StrLib.mod provides string manipulation procedures.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE StrLib ;

FROM ASCII IMPORT nul, tab ;


(*
   StrConCat - combines a and b into c.
*)

PROCEDURE StrConCat (a: ARRAY OF CHAR; b: ARRAY OF CHAR; VAR c: ARRAY OF CHAR) ;
VAR
   Highb,
   Highc,
   i, j : CARDINAL ;
BEGIN
   Highb := StrLen(b) ;
   Highc := HIGH(c) ;
   StrCopy(a, c) ;
   i := StrLen(c) ;
   j := 0 ;
   WHILE (j<Highb) AND (i<=Highc) DO
      c[i] := b[j] ;
      INC(i) ;
      INC(j)
   END ;
   IF i<=Highc
   THEN
      c[i] := nul
   END
END StrConCat ;


(*
   StrLess - returns TRUE if string, a, alphabetically occurs before
             string, b.
*)

PROCEDURE StrLess (a, b: ARRAY OF CHAR) : BOOLEAN ;
VAR
   Higha,
   Highb,
   i    : CARDINAL ;
BEGIN
   Higha := StrLen(a) ;
   Highb := StrLen(b) ;
   i := 0 ;
   WHILE (i<Higha) AND (i<Highb) DO
      IF a[i]<b[i]
      THEN
         RETURN( TRUE )
      ELSIF a[i]>b[i]
      THEN
         RETURN( FALSE )
      END ;
      (* must be equal, move on to next character *)
      INC(i)
   END ;
   RETURN( Higha<Highb )    (* substrings are equal so we go on length *)
END StrLess ;


PROCEDURE StrEqual (a, b: ARRAY OF CHAR) : BOOLEAN ;
VAR
   i,
   higha,
   highb: CARDINAL ;
BEGIN
   higha := HIGH(a) ;
   highb := HIGH(b) ;
   i := 0 ;
   WHILE (i<=higha) AND (i<=highb) AND (a[i]#nul) AND (b[i]#nul) DO
      IF a[i]#b[i]
      THEN
         RETURN( FALSE )
      END ;
      INC(i)
   END ;
   RETURN NOT (((i<=higha) AND (a[i]#nul)) OR
               ((i<=highb) AND (b[i]#nul)))
END StrEqual ;


PROCEDURE StrLen (a: ARRAY OF CHAR) : CARDINAL ;
VAR
   High,
   Len : CARDINAL ;
BEGIN
   Len := 0 ;
   High := HIGH(a) ;
   WHILE (Len<=High) AND (a[Len]#nul) DO
      INC(Len)
   END ;
   RETURN( Len )
END StrLen ;


(*
   StrCopy - copy string src into string dest providing dest is large enough.
             If dest is smaller than a then src then the string is truncated when
             dest is full.  Add a nul character if there is room in dest.
*)

PROCEDURE StrCopy (src: ARRAY OF CHAR ; VAR dest: ARRAY OF CHAR) ;
VAR
   HighSrc,
   HighDest,
   n       : CARDINAL ;
BEGIN
   n := 0 ;
   HighSrc := StrLen (src) ;
   HighDest := HIGH (dest) ;
   WHILE (n < HighSrc) AND (n <= HighDest) DO
      dest[n] := src[n] ;
      INC (n)
   END ;
   IF n <= HighDest
   THEN
      dest[n] := nul
   END
END StrCopy ;


(*
   IsSubString - returns true if b is a subcomponent of a.
*)

PROCEDURE IsSubString (a, b: ARRAY OF CHAR) : BOOLEAN ;
VAR
   i, j,
   LengthA,
   LengthB: CARDINAL ;
BEGIN
   LengthA := StrLen(a) ;
   LengthB := StrLen(b) ;
   i := 0 ;
   IF LengthA>LengthB
   THEN
      WHILE i<=LengthA-LengthB DO
      	 j := 0 ;
      	 WHILE (j<LengthB) AND (a[i+j]=b[j]) DO
      	    INC(j)
      	 END ;
      	 IF j=LengthB
      	 THEN
      	    RETURN( TRUE )
      	 ELSE
      	    INC(i)
      	 END
      END
   END ;
   RETURN( FALSE )
END IsSubString ;


(*
   IsWhite - returns TRUE if, ch, is a space or a tab.
*)

PROCEDURE IsWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch=' ') OR (ch=tab) )
END IsWhite ;


(*
   StrRemoveWhitePrefix - copies string, into string, b, excluding any white
                          space infront of a.
*)

PROCEDURE StrRemoveWhitePrefix (a: ARRAY OF CHAR; VAR b: ARRAY OF CHAR) ;
VAR
   i, j,
   higha, highb: CARDINAL ;
BEGIN
   i := 0 ;
   j := 0 ;
   higha := StrLen(a) ;
   highb := HIGH(b) ;
   WHILE (i<higha) AND IsWhite(a[i]) DO
      INC(i)
   END ;
   WHILE (i<higha) AND (j<=highb) DO
      b[j] := a[i] ;
      INC(i) ;
      INC(j)
   END ;
   IF j<=highb
   THEN
      b[j] := nul
   END
END StrRemoveWhitePrefix ;


END StrLib.
