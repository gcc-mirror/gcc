(* Copyright (C) 2019 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE danglingelse ;  (*!m2pim*)

VAR
   result: CARDINAL ;


PROCEDURE Open (first, second, third: BOOLEAN) ;
BEGIN
   IF third
   THEN
      result := 1
   END ;
   IF second
   THEN
      IF first
      THEN
         IF third
         THEN
            result := 2
         END
      ELSE
         result := 3
      END
   ELSE
      IF first
      THEN
         IF third
         THEN
            result := 4
         END
      ELSE
         result := 5
      END
   END ;
   IF (NOT second) AND first
   THEN
      result := 6 ;
      IF third
      THEN
         result := 7
      ELSE
         result := 8 ;
         result := 9 ;
      END
   ELSE
      result := 10 ;
      IF second
      THEN
         IF first
         THEN
            IF third
            THEN
               result := 11
            END ;
            IF first
            THEN
               (* nothing *)
            ELSE
               result := 12 ;
               result := 13
            END
         ELSE
            IF third
            THEN
               result := 14
            END
         END
      ELSE
         IF first
         THEN
            IF third
            THEN
               result := 15
            END ;
            IF second
            THEN
               (* nothing *)
            ELSE
               result := 16 ;
               result := 17
            END
         ELSE
            IF third
            THEN
               result := 18
            END
         END
      END ;
      result := 19 ;
      result := 20
   END
END Open ;


BEGIN
   Open (TRUE, TRUE, TRUE)
END danglingelse.
