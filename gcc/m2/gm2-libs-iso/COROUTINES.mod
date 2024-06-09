(* COROUTINES.mod implement the ISO COROUTINES specification.

Copyright (C) 2002-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE COROUTINES ;

FROM RTco IMPORT init, initThread, transfer, initSemaphore,
                 wait, signal, currentThread, turnInterrupts,
                 currentInterruptLevel ;

FROM RTExceptions IMPORT EHBlock, InitExceptionBlock,
                         SetExceptionBlock, GetExceptionBlock,
                         SetExceptionState, IsInExceptionState,
                         SetExceptionSource, GetExceptionSource ;

FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM EXCEPTIONS IMPORT ExceptionSource ;
FROM RTint IMPORT Listen, AttachVector, IncludeVector, ExcludeVector ;
FROM Storage IMPORT ALLOCATE ;
FROM Assertion IMPORT Assert ;
FROM M2RTS IMPORT Halt ;
FROM libc IMPORT printf ;
FROM Processes IMPORT displayProcesses ;

IMPORT RTint ;


CONST
   MinStack  = 16 * 1024 * 1024 ;
   Debugging = FALSE ;

TYPE
   Status = (suspended, ready, new, running) ;

   COROUTINE = POINTER TO RECORD
                             context   : INTEGER ;
                             ehblock   : EHBlock ;
                             inexcept  : BOOLEAN ;
                             source    : ExceptionSource ;
                             wspace    : SYSTEM.ADDRESS ;
                             nLocs     : CARDINAL ;
                             status    : Status ;
                             attached  : SourceList ;
                             next      : COROUTINE ;
                          END ;

   SourceList = POINTER TO RECORD
                              next     : SourceList ;    (* next in the list of vectors which are      *)
                                                         (* attached to this coroutine.                *)
                              vec      : INTERRUPTSOURCE ;  (* the interrupt vector (source)           *)
                              curco    : COROUTINE ;     (* the coroutine which is waiting on this vec *)
                              chain    : SourceList ;    (* the next coroutine waiting on this vec     *)
                              ptrToTo,
                              ptrToFrom: POINTER TO COROUTINE ;
                           END ;


VAR
   freeList         : SourceList ;
   head             : COROUTINE ;
   previous,
   currentCoRoutine : COROUTINE ;
   illegalFinish    : ADDRESS ;
   initMain,
   initCo           : BOOLEAN ;
   lock             : INTEGER ;    (* semaphore protecting module data structures.  *)


PROCEDURE NEWCOROUTINE (procBody: PROC;
                        workspace: SYSTEM.ADDRESS;
                        size: CARDINAL;
                        VAR cr: COROUTINE;
                        [initProtection: PROTECTION]);

  (* Creates a new coroutine whose body is given by procBody, and
     returns the identity of the coroutine in cr. workspace is a
     pointer to the work space allocated to the coroutine; size
     specifies the size of this workspace in terms of SYSTEM.LOC.

     The optarg, initProtection, may contain a single parameter
     which specifies the initial protection level of the coroutine.
  *)
VAR
   tp : INTEGER ;
   old: PROTECTION ;
BEGIN
   localInit ;
   old := TurnInterrupts (MAX (PROTECTION)) ;
   IF initProtection = UnassignedPriority
   THEN
      initProtection := PROT ()
   END ;
   tp := initThread (procBody, size, initProtection) ;
   IF tp = -1
   THEN
      Halt ('unable to create a new thread', __FILE__, __FUNCTION__, __LINE__)
   END ;
   NEW (cr) ;
   WITH cr^ DO
      context    := tp ;
      ehblock    := InitExceptionBlock () ;
      inexcept   := FALSE ;
      source     := NIL ;
      wspace     := workspace ;
      nLocs      := size ;
      status     := new ;
      attached   := NIL ;
      next       := head
   END ;
   head := cr ;
   old := TurnInterrupts (old)
END NEWCOROUTINE ;


PROCEDURE TRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from, and
     transfers control to the coroutine specified by to.
  *)
VAR
   old: PROTECTION ;
BEGIN
   localInit ;
   old := TurnInterrupts (MAX (PROTECTION)) ;
   (* wait (lock) ; *)
   Transfer (from, to) ;
   (* signal (lock) ; *)
   old := TurnInterrupts (old)
END TRANSFER ;


(*
   Transfer -
*)

PROCEDURE Transfer (VAR from: COROUTINE; to: COROUTINE) ;
BEGIN
   IF Debugging
   THEN
      printf ("TRANSFER\n");
      printf ("current coroutine is: %d\n", currentCoRoutine^.context);
      IF previous # NIL
      THEN
         printf ("previous coroutine is: %d\n", previous^.context)
      END ;
      printf ("wishes to context switch to: %d\n", to^.context);
   END ;
   previous := currentCoRoutine ;
   from := currentCoRoutine ;
   IF to^.context = from^.context
   THEN
      Halt ('error when attempting to context switch to the same process',
            __FILE__, __FUNCTION__, __LINE__)
   END ;
   from^.inexcept := SetExceptionState (to^.inexcept) ;
   from^.source := GetExceptionSource () ;
   currentCoRoutine := to ;
   SetExceptionBlock (currentCoRoutine^.ehblock) ;
   SetExceptionSource (currentCoRoutine^.source) ;
   transfer (from^.context, to^.context)
END Transfer ;


(*
   localMain - creates the holder for the main process.
*)

PROCEDURE localMain ;
VAR
   old: PROTECTION ;
BEGIN
   IF NOT initMain
   THEN
      initMain := TRUE ;
      lock := initSemaphore (1) ;
      wait (lock) ;
      NEW (currentCoRoutine) ;
      WITH currentCoRoutine^ DO
         context    := currentThread () ;
         ehblock    := GetExceptionBlock () ;
         inexcept   := IsInExceptionState () ;
         source     := GetExceptionSource () ;
         wspace     := NIL ;
         nLocs      := 0 ;
         status     := running ;
         attached   := NIL ;
         next       := head
      END ;
      head := currentCoRoutine ;
      old := turnInterrupts (MAX (PROTECTION)) ;    (* was UnassignedPriority *)
      signal (lock)
   END
END localMain ;


(*
   localInit - checks to see whether we need to initialize our interface to pthreads.
*)

PROCEDURE localInit ;
BEGIN
   IF NOT initCo
   THEN
      Init ;
      IF init () # 0
      THEN
         Halt ('failed to initialize RTco',
               __FILE__, __FUNCTION__, __LINE__)
      END ;
      RTint.Init ;
      initCo := TRUE
   END ;
   localMain
END localInit ;


PROCEDURE IOTRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from and
     transfers control to the coroutine specified by to.  On
     occurrence of an interrupt, associated with the caller, control
     is transferred back to the caller, and the identity of the
     interrupted coroutine is returned in from.  The calling coroutine
     must be associated with a source of interrupts.
  *)
VAR
   prev,
   l   : SourceList ;
   old : PROTECTION ;
BEGIN
   localInit ;
   old := TurnInterrupts (MAX (PROTECTION)) ;
   IF from = to
   THEN
      Halt ("error IOTRANSFER cannot transfer control to the running COROUTINE",
            __FILE__, __FUNCTION__, __LINE__)
   END ;
   wait (lock) ;
   l := currentCoRoutine^.attached ;
   IF l=NIL
   THEN
      printf ("no source of interrupts associated with coroutine\n")
   END ;
   WHILE l # NIL DO
      WITH l^ DO
         ptrToFrom := ADR (from) ;
         ptrToTo   := ADR (to) ;
         curco := currentCoRoutine ;
         Assert (currentCoRoutine # NIL) ;
         prev := AttachVector (vec, l) ;
         Assert (from # to) ;
         IF (prev # NIL) AND (prev # l)
         THEN
            printf ("not expecting multiple COROUTINES to be waiting on a single interrupt source\n")
         END ;
         IncludeVector (vec)
      END ;
      l := l^.next
   END ;
   signal (lock) ;
   Transfer (from, to) ;
   from := previous ;
   old := TurnInterrupts (old)
END IOTRANSFER ;


(*
   New - assigns, l, to a new SourceList.
*)

PROCEDURE New (VAR l: SourceList) ;
BEGIN
   IF freeList=NIL
   THEN
      NEW (l)
   ELSE
      l := freeList ;
      freeList := freeList^.next
   END
END New ;


(*
   Dispose - returns, l, to the freeList.
*)

PROCEDURE Dispose (l: SourceList) ;
BEGIN
   l^.next := freeList ;
   freeList := l
END Dispose ;


PROCEDURE ATTACH (source: INTERRUPTSOURCE);
  (* Associates the specified source of interrupts with the calling
     coroutine. *)
VAR
   l: SourceList ;
BEGIN
   localInit ;
   wait (lock) ;
   l := currentCoRoutine^.attached ;
   WHILE l#NIL DO
      IF l^.vec = source
      THEN
         l^.curco := currentCoRoutine ;
         signal (lock) ;
         RETURN
      ELSE
         l := l^.next
      END
   END ;
   New (l) ;
   WITH l^ DO
      next := currentCoRoutine^.attached ;
      vec := source ;
      curco := currentCoRoutine ;
      chain := NIL ;
   END ;
   currentCoRoutine^.attached := l ;
   IF AttachVector (source, l) # NIL
   THEN
      printf ("ATTACH implementation restriction only one coroutine may be attached to a specific interrupt source\n")
   END ;
   signal (lock)
END ATTACH ;


PROCEDURE DETACH (source: INTERRUPTSOURCE);
  (* Dissociates the specified source of interrupts from the calling
     coroutine. *)
VAR
   l, prev: SourceList ;
BEGIN
   localInit ;
   wait (lock) ;
   l := currentCoRoutine^.attached ;
   prev := NIL ;
   WHILE l # NIL DO
      IF l^.vec = source
      THEN
         IF prev = NIL
         THEN
            Assert (l = currentCoRoutine^.attached) ;
            currentCoRoutine^.attached := currentCoRoutine^.attached^.next ;
         ELSE
            prev^.next := l^.next
         END ;
         Dispose (l) ;
         signal (lock) ;
         RETURN
      ELSE
         prev := l ;
         l := l^.next
      END
   END ;
   signal (lock)
END DETACH ;


(*
   getAttached - returns the first COROUTINE associated with, source.
                 It returns NIL is no COROUTINE is associated with, source.
*)

PROCEDURE getAttached (source: INTERRUPTSOURCE) : COROUTINE ;
VAR
   l: SourceList ;
   c: COROUTINE ;
BEGIN
   localInit ;
   c := head ;
   WHILE c # NIL DO
      l := c^.attached ;
      WHILE l#NIL DO
         IF l^.vec = source
         THEN
            RETURN c
         ELSE
            l := l^.next
         END
      END ;
      c := c^.next
   END ;
   RETURN NIL
END getAttached ;


PROCEDURE IsATTACHED (source: INTERRUPTSOURCE): BOOLEAN;
  (* Returns TRUE if and only if the specified source of interrupts is
     currently associated with a coroutine; otherwise returns FALSE.
  *)
VAR
   result: BOOLEAN ;
BEGIN
   localInit ;
   wait (lock) ;
   result := getAttached (source) # NIL ;
   signal (lock) ;
   RETURN result
END IsATTACHED ;


PROCEDURE HANDLER (source: INTERRUPTSOURCE) : COROUTINE;
  (* Returns the coroutine, if any, that is associated with the source
     of interrupts. The result is undefined if IsATTACHED(source) =
     FALSE.
  *)
VAR
   co: COROUTINE ;
BEGIN
   localInit ;
   wait (lock) ;
   co := getAttached (source) ;
   signal (lock) ;
   RETURN co
END HANDLER ;


PROCEDURE CURRENT () : COROUTINE ;
  (* Returns the identity of the calling coroutine. *)
BEGIN
   localInit ;
   RETURN currentCoRoutine
END CURRENT ;


PROCEDURE LISTEN (p: PROTECTION) ;
  (* Momentarily changes the protection of the calling coroutine to p. *)
BEGIN
   localInit ;
   Listen (FALSE, IOTransferHandler, p)
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
   Listen (TRUE, IOTransferHandler, MIN (PROTECTION))
END ListenLoop ;


(*
   removeAttached - removes all sources of interrupt from COROUTINE, c.
*)

PROCEDURE removeAttached (c: COROUTINE) ;
VAR
   l: SourceList ;
BEGIN
   localInit ;
   l := c^.attached ;
   WHILE l#NIL DO
      ExcludeVector (l^.vec) ;
      l := l^.next
   END
END removeAttached ;


(*
   IOTransferHandler - handles interrupts related to a pending IOTRANSFER.
*)

PROCEDURE IOTransferHandler (InterruptNo: CARDINAL;
                             Priority: CARDINAL ;
                             l: SourceList) ;
VAR
   ourself: SourceList ;
BEGIN
   IF Debugging
   THEN
      printf ("IOTransferHandler called\n") ;
      displayProcesses ("IOTransferHandler") ;
      printf ("IOTransferHandler vec %d coroutine: %d\n", l^.vec, l^.curco^.context);
      printf ("localInit\n");
   END ;
   localInit ;
   IF l = NIL
   THEN
      Halt ('no coroutine attached to this interrupt vector which was initiated by IOTRANSFER',
            __FILE__, __FUNCTION__, __LINE__)
   ELSE
      IF Debugging
      THEN
         printf ("IOTransferHandler called\n");
         printf ("before wait (lock)\n");
      END ;
      wait (lock) ;
      IF Debugging
      THEN
         printf ("IOTransferHandler vec %d coroutine 0x%x\n", l^.vec, l^.curco);
         printf ("current coroutine is: %d\n", currentCoRoutine^.context);
         IF previous # NIL
         THEN
            printf ("previous coroutine is: %d\n", previous^.context)
         END ;
         printf ("handler wants to context switch to:  %d\n", l^.curco^.context);
         displayProcesses ("IOTransferHandler")
      END ;
      WITH l^ DO
         (*
         ourself := AttachVector (InterruptNo, chain) ;
         IF ourself # l
         THEN
            Halt ('inconsistancy of return result',
                  __FILE__, __FUNCTION__, __LINE__)
         END ;
         IF chain = NIL
         THEN
            removeAttached (curco)
         ELSE
            printf ('odd vector has been chained\n')
         END ;
         *)
         removeAttached (curco) ;   (* remove all sources of interrupt for l^.curco.  *)
         ptrToFrom^ := currentCoRoutine ;
         previous := currentCoRoutine ;
         previous^.inexcept := SetExceptionState (curco^.inexcept) ;
         previous^.source := GetExceptionSource () ;
         currentCoRoutine := curco ;
         SetExceptionBlock (currentCoRoutine^.ehblock) ;
         SetExceptionSource (currentCoRoutine^.source) ;
         signal (lock) ;
         transfer (previous^.context, currentCoRoutine^.context)
      END
   END
END IOTransferHandler ;


PROCEDURE PROT () : PROTECTION;
  (* Returns the protection of the calling coroutine. *)
BEGIN
   localInit ;
   RETURN currentInterruptLevel ()
END PROT ;


(*
   TurnInterrupts - switches processor interrupts to the protection
                    level, to.  It returns the old value.
*)

PROCEDURE TurnInterrupts (to: PROTECTION) : PROTECTION ;
VAR
   old: PROTECTION ;
BEGIN
   localInit ;
   old := turnInterrupts (to) ;
   Listen (FALSE, IOTransferHandler, to) ;
   RETURN old
END TurnInterrupts ;


(*
   Init - initialize the global data structures.
*)

PROCEDURE Init ;
BEGIN
   freeList := NIL ;
   initMain := FALSE ;
   currentCoRoutine := NIL
END Init ;


END COROUTINES.
