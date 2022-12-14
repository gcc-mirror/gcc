(* Copyright (C) 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)

MODULE testiotransfer ;


FROM SYSTEM IMPORT ADDRESS, PROCESS, TRANSFER, NEWPROCESS,
                   BYTE, LISTEN, IOTRANSFER, ListenLoop ;

FROM COROUTINES IMPORT PROTECTION ;
FROM RTint IMPORT InitTimeVector, ReArmTimeVector ;
FROM Storage IMPORT ALLOCATE ;
FROM libc IMPORT printf, exit ;

CONST
   Debugging = FALSE ;


PROCEDURE Timer ;
CONST
   MaxCount = 1000 ;
VAR
   v: CARDINAL ;
   c: CARDINAL ;
BEGIN
   printf ('clock starting\n') ;
   v := InitTimeVector (500, 0, MAX (PROTECTION)) ;
   c := 0 ;
   LOOP
      INC (c) ;
      IF Debugging
      THEN
         printf ('about to call IOTRANSFER: %d\n', c)
      END ;
      IOTRANSFER (p2, p1, v) ;
      IF Debugging
      THEN
         printf ('back from IOTRANSFER: %d\n', c)
      END ;
      ReArmTimeVector (v, 500, 0) ;
      IF Debugging
      THEN
         printf ('ReArmed timer: %d\n', c)
      END ;
      IF c = MaxCount
      THEN
         printf ("%d IOTRANSFERs successfully completed\nexit 0\n", c) ;
         exit (0)
      END
   END
END Timer ;


CONST
   MaxStack = 16 * 1024 * 1024 ;

VAR
   s1, s2 : ADDRESS ;
   p1, p2 : PROCESS ;
BEGIN
   (* exit (1) ; *)   (* disable test for now.  *)
   ALLOCATE (s1, MaxStack) ;
   ALLOCATE (s2, MaxStack) ;
   NEWPROCESS (Timer, s2, MaxStack, p2) ;
   printf ('now to TRANSFER...\n') ;
   TRANSFER (p1, p2) ;
   printf ('now to LISTEN\n') ;
   ListenLoop
(*
   LOOP
      LISTEN
   END
*)
END testiotransfer.