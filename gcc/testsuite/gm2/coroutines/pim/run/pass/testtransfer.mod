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

MODULE testtransfer ;

FROM SYSTEM IMPORT ADDRESS, PROCESS, NEWPROCESS, TRANSFER ;
FROM Storage IMPORT ALLOCATE ;
FROM libc IMPORT printf, exit ;


CONST
   MaxStack  = 16 * 1024 * 1024 ;
   Debugging = FALSE ;
   MaxCount  = 1000 ;


PROCEDURE p1 ;
BEGIN
   LOOP
      IF Debugging
      THEN
         printf('hello world process 1\n')
      END ;
      TRANSFER(P1, P2) ;
      IF Debugging
      THEN
         printf('after TRANSFER in process 2\n')
      END
   END
END p1 ;


PROCEDURE p2 ;
BEGIN
   LOOP
      IF Debugging
      THEN
         printf('hello world process 2  (%d)\n', count)
      END ;
      TRANSFER(P2, P1) ;
      IF Debugging
      THEN
         printf('after TRANSFER in process 2\n')
      END ;
      INC(count) ;
      IF count=MaxCount
      THEN
         printf('completed %d TRANSFERs successfully\n', count) ;
         exit(0)
      END
   END
END p2 ;


VAR
   MainP, P1, P2: PROCESS ;
   S1, S2, S3   : ADDRESS ;
   count        : CARDINAL ;
BEGIN
   printf ("inside testtransfer\n");
   count := 0 ;
   ALLOCATE(S1, MaxStack) ;
   ALLOCATE(S2, MaxStack) ;
   ALLOCATE(S3, MaxStack) ;
   printf ("before newprocess 1\n");
   NEWPROCESS(p1, S1, MaxStack, P1) ;
   printf ("before newprocess 2\n");
   NEWPROCESS(p2, S2, MaxStack, P2) ;
   printf ("before TRANSFER\n");
   TRANSFER(MainP, P1) ;
   printf ("program finishing - which should not occur here!\n");
   exit (1)
END testtransfer.
