(* SYSTEM.mod provides access to COROUTINE primitives and underlying system.

Copyright (C) 2002-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE SYSTEM ;

FROM RTco IMPORT init, initThread, transfer, currentThread, turnInterrupts ;
FROM RTint IMPORT Listen, AttachVector, IncludeVector, ExcludeVector ;

IMPORT RTint ;

FROM Storage IMPORT ALLOCATE ;
FROM M2RTS IMPORT Halt ;
FROM libc IMPORT printf, memcpy, memset ;


CONST
   BitsPerBitset = MAX (BITSET) +1 ;

TYPE
   PtrToIOTransferState = POINTER TO IOTransferState ;
   IOTransferState      = RECORD
                             ptrToFirst,
                             ptrToSecond: POINTER TO PROCESS ;
                             next       : PtrToIOTransferState ;
                          END ;

VAR
   initMain,
   initGTh : BOOLEAN ;


(*
   TRANSFER - save the current volatile environment into, p1.
              Restore the volatile environment from, p2.
*)

PROCEDURE TRANSFER (VAR p1: PROCESS; p2: PROCESS) ;
BEGIN
   localMain (p1) ;
   IF p1.context=p2.context
   THEN
      Halt('error when attempting to context switch to the same process',
           __FILE__, __FUNCTION__, __LINE__)
   END ;
   transfer (p1.context, p2.context)
END TRANSFER ;


(*
   NEWPROCESS - p is a parameterless procedure, a, is the origin of
                the workspace used for the process stack and containing
                the volatile environment of the process.  StackSize, is
                the maximum size of the stack in bytes which can be used
                by this process.  new, is the new process.
*)

PROCEDURE NEWPROCESS (p: PROC; a: ADDRESS; StackSize: CARDINAL; VAR new: PROCESS) ;
BEGIN
   localInit ;
   WITH new DO
      context := initThread (p, StackSize, MAX(PROTECTION))
   END
END NEWPROCESS ;


(*
   IOTRANSFER - saves the current volatile environment into, First,
                and restores volatile environment, Second.
                When an interrupt, InterruptNo, is encountered then
                the reverse takes place. (The then current volatile
                environment is shelved onto Second and First is resumed).

                NOTE: that upon interrupt the Second might not be the
                      same process as that before the original call to
                      IOTRANSFER.
*)

PROCEDURE IOTRANSFER (VAR First, Second: PROCESS; InterruptNo: CARDINAL) ;
VAR
   iots: IOTransferState ;
BEGIN
   localMain (First) ;
   WITH iots DO
      ptrToFirst  := ADR (First) ;
      ptrToSecond := ADR (Second) ;
      next        := AttachVector (InterruptNo, ADR (iots))
   END ;
   IncludeVector (InterruptNo) ;
   TRANSFER (First, Second)
END IOTRANSFER ;


(*
   IOTransferHandler - handles interrupts related to a pending IOTRANSFER.
*)

PROCEDURE IOTransferHandler (InterruptNo: CARDINAL;
                             Priority: CARDINAL ;
                             piots: PtrToIOTransferState) ;
VAR
   old: PtrToIOTransferState ;
BEGIN
   IF piots = NIL
   THEN
      Halt ('no processes attached to this interrupt vector which is associated with IOTRANSFER',
            __FILE__, __FUNCTION__, __LINE__)
   ELSE
      WITH piots^ DO
         old := AttachVector (InterruptNo, next) ;
         IF old # piots
         THEN
            Halt ('inconsistancy of return result',
                  __FILE__, __FUNCTION__, __LINE__)
         END ;
         IF next=NIL
         THEN
            ExcludeVector (InterruptNo)
         ELSE
            printf ('odd vector has been chained\n')
         END ;
         TRANSFER (ptrToSecond^, ptrToFirst^)
      END
   END
END IOTransferHandler ;


(*
   LISTEN - briefly listen for any interrupts.
*)

PROCEDURE LISTEN ;
BEGIN
   localInit ;
   Listen (FALSE, IOTransferHandler, MIN (PROTECTION))
END LISTEN ;


(*
   ListenLoop - should be called instead of users writing:

                LOOP
                   LISTEN
                END

                It performs the same function but yields
                control back to the underlying operating system.
                It also checks for deadlock.
                This function returns when an interrupt occurs.
                (File descriptor becomes ready or time event expires).
*)

PROCEDURE ListenLoop ;
BEGIN
   localInit ;
   LOOP
      Listen (TRUE, IOTransferHandler, MIN (PROTECTION))
   END
END ListenLoop ;


(*
   TurnInterrupts - switches processor interrupts to the
                    protection level, to.  It returns the old value.
*)

PROCEDURE TurnInterrupts (to: PROTECTION) : PROTECTION ;
VAR
   old: PROTECTION ;
BEGIN
   localInit ;
   old := VAL (PROTECTION, turnInterrupts (VAL (CARDINAL, to))) ;
   Listen (FALSE, IOTransferHandler, to) ;
   (* printf ("interrupt level is %d\n", currentIntValue); *)
   RETURN old
END TurnInterrupts ;


(*
   Finished - generates an error message. Modula-2 processes should never
              terminate.
*)

PROCEDURE Finished (p: ADDRESS) ;
BEGIN
   Halt ('process terminated illegally',
         __FILE__, __FUNCTION__, __LINE__)
END Finished ;


(*
   localInit - checks to see whether we need to initialize pthread
*)

PROCEDURE localInit ;
BEGIN
   IF NOT initGTh
   THEN
      initGTh := TRUE ;
      IF init () # 0
      THEN
         Halt ("gthr did not initialize",
               __FILE__, __FUNCTION__, __LINE__)
      END ;
      RTint.Init
   END
END localInit ;


(*
   localMain - creates the holder for the main process.
*)

PROCEDURE localMain (VAR mainProcess: PROCESS) ;
BEGIN
   IF NOT initMain
   THEN
      initMain := TRUE ;
      WITH mainProcess DO
         context := currentThread ()
      END
   END
END localMain ;


BEGIN
   initGTh := FALSE ;
   initMain := FALSE
END SYSTEM.
