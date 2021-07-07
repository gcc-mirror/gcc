(* Copyright (C) 2014 Free Software Foundation, Inc. *)
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
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE EndFor ;

FROM libc IMPORT exit, printf ;


PROCEDURE CheckLongintInteger (des: LONGINT; inc: INTEGER) ;
VAR
   lg        : LONGINT ;
   room, desn: LONGINT ;
BEGIN
   lg := VAL(LONGINT, inc) ;
   IF inc>=0
   THEN
      IF des>=0
      THEN
         room := MAX(LONGINT)-des ;
         IF lg>room
         THEN
            printf("increment exceeds range at end of FOR loop\n") ;
            exit (2)
         END
      ELSE
         (* inc can never cause an overflow given its type *)
      END
   ELSE
      (* inc < 0 *)
      IF des>=0
      THEN
         (* inc can never cause an underflow given its type *)
      ELSE
         (* des < 0 *)
         IF des=MIN(LONGINT)
         THEN
            printf("increment exceeds range at end of FOR loop\n") ;
            exit (4)
         ELSE
            desn := -des;
            room := MIN(LONGINT)+desn ;
            lg := -lg ;
            IF lg>room
            THEN
               printf("increment exceeds range at end of FOR loop\n") ;
               exit (5)
            END
         END
      END
   END
END CheckLongintInteger ;


PROCEDURE CheckLongintLongint (des: LONGINT; inc: LONGINT) ;
VAR
   lg        : LONGINT ;
   room, desn: LONGINT ;
BEGIN
   IF inc>=0
   THEN
      IF des>=0
      THEN
         lg := inc ;
         room := MAX(LONGINT)-des ;
         IF lg>room
         THEN
            printf("increment exceeds range at end of FOR loop\n") ;
            exit (2)
         END
      ELSE
         (* inc can never cause an overflow given its type *)
      END
   ELSE
      (* inc < 0 *)
      IF des>=0
      THEN
         (* inc can never cause an underflow given its type *)
      ELSE
         IF des=MIN(LONGINT)
         THEN
            printf("increment exceeds range at end of FOR loop\n") ;
            exit (4)
         ELSE
            IF inc=MIN(LONGINT)
            THEN
               IF des=0
               THEN
                  printf("increment exceeds range at end of FOR loop\n") ;
                  exit (5)
               END
            ELSE
               (* des < 0 *)
               desn := -des;
               room := MIN(LONGINT)+desn ;
               lg := -inc ;
               IF lg>room
               THEN
                  printf("increment exceeds range at end of FOR loop\n") ;
                  exit (6)
               END
            END
         END
      END
   END
END CheckLongintLongint ;


PROCEDURE CheckCardinalInteger (des: CARDINAL; inc: INTEGER) ;
VAR
   room: CARDINAL ;
   lg  : CARDINAL ;
BEGIN
   IF inc>=0
   THEN
      IF des>=0
      THEN
         lg := VAL(CARDINAL, inc) ;
         room := MAX(CARDINAL)-des ;
         IF lg>room
         THEN
            printf("increment exceeds range at end of FOR loop\n") ;
            exit (2)
         END
      ELSE
         (* inc can never cause an overflow given its type *)
      END
   ELSE
      (* inc < 0 *)
      IF des>VAL(CARDINAL, MAX(INTEGER))
      THEN
         (* inc can never cause an underflow given its range *)
      ELSE
         (* des <= MAX(INTEGER) *)
         IF des=MIN(INTEGER)
         THEN
            printf("increment exceeds range at end of FOR loop\n") ;
            exit (4)
         ELSE
            IF inc=MIN(INTEGER)
            THEN
               IF des=0
               THEN
                  printf("increment exceeds range at end of FOR loop\n") ;
                  exit (5)
               END
            ELSE
               lg := VAL(CARDINAL, -inc) ;
               IF lg>des
               THEN
                  printf("increment exceeds range at end of FOR loop\n") ;
                  exit (5)
               END
            END
         END
      END
   END
END CheckCardinalInteger ;


PROCEDURE CheckCardinalCardinal (des: CARDINAL; inc: CARDINAL) ;
BEGIN
   IF MAX(CARDINAL)-des<inc
   THEN
      printf("increment exceeds range at end of FOR loop\n") ;
      exit (2)
   END
END CheckCardinalCardinal ;


END EndFor.
