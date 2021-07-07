(* Storage.mod implement the ISO Storage specification.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Storage ;

FROM libc IMPORT malloc, free, memcpy ;
FROM M2RTS IMPORT Halt ;
FROM SYSTEM IMPORT TSIZE ;
FROM M2EXCEPTION IMPORT M2Exceptions ;
FROM RTentity IMPORT Group, InitGroup, GetKey, PutKey, DelKey, IsIn ;

FROM EXCEPTIONS IMPORT ExceptionNumber, RAISE,
                       AllocateSource, ExceptionSource, IsCurrentSource,
                       IsExceptionalExecution ;



PROCEDURE ALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL) ;
BEGIN
   addr := malloc (amount) ;
   IF addr#NIL
   THEN
      PutKey (storageTree, addr, amount)
   END
END ALLOCATE ;


PROCEDURE DEALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL) ;
BEGIN
   IF VerifyDeallocate (addr, amount)
   THEN
      free (addr) ;
      addr := NIL
   END
END DEALLOCATE ;


PROCEDURE REALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL);
  (* Attempts to reallocate, amount of storage.  Effectively it
     calls ALLOCATE, copies the amount of data pointed to by
     addr into the new space and DEALLOCATES the addr.
     This procedure is a GNU extension.
  *)
VAR
   newa: SYSTEM.ADDRESS ;
   n   : CARDINAL ;
BEGIN
   IF NOT IsIn(storageTree, addr)
   THEN
      RAISE (storageException, ORD(pointerToUnallocatedStorage),
             'trying to reallocate memory which has never been allocated') ;
   END ;
   n := GetKey (storageTree, addr) ;
   ALLOCATE(newa, amount) ;
   IF n<amount
   THEN
      newa := memcpy(newa, addr, n)
   ELSE
      newa := memcpy(newa, addr, amount)
   END ;
   DEALLOCATE(addr, n) ;
   addr := newa
END REALLOCATE ;


PROCEDURE IsStorageException () : BOOLEAN;
BEGIN
   RETURN( IsCurrentSource (storageException) )
END IsStorageException ;


PROCEDURE StorageException () : StorageExceptions ;
BEGIN
   IF NOT IsExceptionalExecution ()
   THEN
      RAISE (storageException, ORD (functionException), 'no storage exception raised')
   END ;
   RETURN (currentException)
END StorageException ;


(*
   VerifyDeallocate -
*)

PROCEDURE VerifyDeallocate (addr: SYSTEM.ADDRESS; amount: CARDINAL) : BOOLEAN ;
VAR
   a: CARDINAL ;
BEGIN
   IF addr=NIL
   THEN
      RAISE (storageException, ORD(nilDeallocation), 'deallocating pointer whose value is NIL') ;
      RETURN (FALSE)
   ELSE
      IF NOT IsIn(storageTree, addr)
      THEN
         RAISE (storageException, ORD(pointerToUnallocatedStorage), 'trying to deallocate memory which has never been allocated') ;
         RETURN (FALSE)
      END ;
      a := GetKey (storageTree, addr) ;
      IF a#amount
      THEN
         RAISE (storageException, ORD(wrongStorageToUnallocate), 'wrong amount of storage being deallocated') ;
         RETURN (FALSE)
      END
   END ;
   DelKey (storageTree, addr) ;
   RETURN (TRUE)
END VerifyDeallocate ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   storageTree := InitGroup () ;
   AllocateSource (storageException)
END Init ;


VAR
   storageException: ExceptionSource ;
   currentException: StorageExceptions ;
   storageTree     : Group ;
BEGIN
   Init
END Storage.
