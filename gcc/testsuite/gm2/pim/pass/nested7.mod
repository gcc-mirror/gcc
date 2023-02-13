(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

MODULE nested7 ;   (*!m2pim*)

TYPE
   soc = SET OF CHAR ;

   PROCEDURE outer ;

      MODULE inner ;
      IMPORT soc ;
      EXPORT ident, foo ;
      VAR
         ident: ARRAY [0..7] OF CHAR ;

         PROCEDURE foo (VAR a: soc) ;
         BEGIN
         END foo ;
      BEGIN
         ident[0] := 'b'
      END inner ;
   BEGIN
      foo (s) ;
      ident[0] := 'a'
   END outer ;

VAR
   s: soc ;
BEGIN
   outer
END nested7.
