(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE StrLib ;

FROM ASCII IMPORT nul, tab ;

(* %%%FORWARD%%%
PROCEDURE StrEqual (a, b: ARRAY OF CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE StrLen (a: ARRAY OF CHAR) : CARDINAL ; FORWARD ;
PROCEDURE StrCopy (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE StrConCat (a: ARRAY OF CHAR ; b: ARRAY OF CHAR ; VAR c: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE IsSubString (a, b: ARRAY OF CHAR) : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


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
   Higha,
   Highb: CARDINAL ;
   Equal: BOOLEAN ;
BEGIN
   Higha := StrLen(a) ;
   Highb := StrLen(b) ;
   IF Higha=Highb
   THEN
      Equal := TRUE ;
      i := 0 ;
      WHILE Equal AND (i<Higha) DO
         Equal := (a[i]=b[i]) ;
         INC(i)
      END ;
      RETURN( Equal )
   ELSE
      RETURN( FALSE )
   END
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


PROCEDURE StrCopy (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ;
VAR
   Higha,
   Highb,
   n    : CARDINAL ;
BEGIN
   n := 0 ;
   Higha := StrLen(a) ;
   Highb := HIGH(b) ;
   WHILE (n<Higha) AND (n<=Highb) DO
      b[n] := a[n] ;
      INC(n)
   END ;
   IF n<=Highb
   THEN
      b[n] := nul
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
