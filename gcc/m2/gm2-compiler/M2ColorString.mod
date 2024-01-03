(* M2ColorString.mod provides procedures for obtaining GCC color strings.

Copyright (C) 2019-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2ColorString ;

FROM m2color IMPORT colorize_start, colorize_stop, open_quote, close_quote ;
FROM DynamicStrings IMPORT InitString, InitStringCharStar,
                           ConCat, ConCatChar, Mark, string, KillString,
                           Dup, char, Length, Mult ;
FROM StrLib IMPORT StrLen ;
FROM libc IMPORT printf ;
FROM SYSTEM IMPORT ADR ;


VAR
   EnableColor: BOOLEAN ;


(*
   SetEnableColor - sets the global variable to, b, and returns
                    the previous value.
*)

PROCEDURE SetEnableColor (b: BOOLEAN) : BOOLEAN ;
VAR
   previous: BOOLEAN ;
BEGIN
   previous := EnableColor ;
   EnableColor := b ;
   RETURN previous
END SetEnableColor ;


(*
   append - appends color string, name, to the end of string, s,
            and returns, s.
*)

PROCEDURE append (s: String; name: ARRAY OF CHAR) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, ADR (name), StrLen (name))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END append ;


(*
   quoteOpen - adds an open quote to string, s.
*)

PROCEDURE quoteOpen (s: String) : String ;
BEGIN
   RETURN ConCat (append (s, "quote"), Mark (InitStringCharStar (open_quote ())))
END quoteOpen ;


(*
   quoteClose - adds a close quote to string, s.
*)

PROCEDURE quoteClose (s: String) : String ;
BEGIN
   s := endColor (s) ;
   s := append (s, "quote") ;
   s := ConCat (s, Mark (InitStringCharStar (close_quote ()))) ;
   s := endColor (s) ;
   RETURN s
END quoteClose ;


(*
   endColor - stops using color.
*)

PROCEDURE endColor (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_stop (EnableColor)) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END endColor ;


(*
   quoteColor - adds quote color to string, s.
*)

PROCEDURE quoteColor (s: String) : String ;
BEGIN
   RETURN append (s, "quote")
END quoteColor ;


(*
   errorColor - adds error color to string, s.
*)

PROCEDURE errorColor (s: String) : String ;
BEGIN
   RETURN append (s, "error")
END errorColor ;


(*
   warningColor - adds warning color to string, s.
*)

PROCEDURE warningColor (s: String) : String ;
BEGIN
   RETURN append (s, "warning")
END warningColor ;


(*
   noteColor - adds note color to string, s.
*)

PROCEDURE noteColor (s: String) : String ;
BEGIN
   RETURN append (s, "note")
END noteColor ;


(*
   locusColor - adds locus color to string, s.
*)

PROCEDURE locusColor (s: String) : String ;
BEGIN
   RETURN append (s, "locus")
END locusColor ;


(*
   insertColor - adds fixit-insert color to string, s.
*)

PROCEDURE insertColor (s: String) : String ;
BEGIN
   RETURN append (s, "fixit-insert")
END insertColor ;


(*
   deleteColor - adds fixit-insert color to string, s.
*)

PROCEDURE deleteColor (s: String) : String ;
BEGIN
   RETURN append (s, "fixit-delete")
END deleteColor ;


(*
   filenameColor - adds filename color to string, s.
*)

PROCEDURE filenameColor (s: String) : String ;
BEGIN
   RETURN append (s, "diff-filename")
END filenameColor ;


(*
   typeColor - adds type color to string, s.
*)

PROCEDURE typeColor (s: String) : String ;
BEGIN
   RETURN append (s, "type")
END typeColor ;


(*
   range1Color - adds type color to string, s.
*)

PROCEDURE range1Color (s: String) : String ;
BEGIN
   RETURN append (s, "range1")
END range1Color ;


(*
   range2Color - adds type color to string, s.
*)

PROCEDURE range2Color (s: String) : String ;
BEGIN
   RETURN append (s, "range2")
END range2Color ;


BEGIN
   EnableColor := TRUE
END M2ColorString.
