(* Executive.mod provides a simple multitasking executive.

Copyright (C) 2002-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Executive[MAX(PROTECTION)] ;

FROM SYSTEM IMPORT ADDRESS, PROCESS, LISTEN, ADR,
                   NEWPROCESS, TRANSFER, IOTRANSFER, ListenLoop,
                   TurnInterrupts ;

FROM COROUTINES IMPORT PROTECTION ;
FROM SysStorage IMPORT ALLOCATE, DEALLOCATE ;
FROM StrLib IMPORT StrCopy ;
FROM StrLib IMPORT StrLen ;
FROM NumberIO IMPORT CardToStr ;
FROM Debug IMPORT DebugString, Halt ;


(* IMPORT gdb ; *)


CONST
   MaxCharsInName = 15 ;
   IdleStackSize  = 16 * 1024 * 1024 ;

TYPE
   SEMAPHORE = POINTER TO Semaphore ;         (* defines dijkstra's semaphores *)
   Semaphore = RECORD
                  Value  : CARDINAL ;         (* semaphore value               *)
                  SemName: EntityName ;       (* semaphore name for debugging  *)
                  Who    : DESCRIPTOR ;       (* queue of waiting processes    *)
                  ExistsQ: SemQueue ;         (* list of existing semaphores   *)
               END ;

   DESCRIPTOR= POINTER TO Descriptor ;        (* handle onto a process         *)
   Descriptor= RECORD
                  Volatiles  : PROCESS ;      (* process volatile environment  *)
                  ReadyQ     : DesQueue ;     (* queue of ready processes      *)
                  ExistsQ    : DesQueue ;     (* queue of existing processes   *)
                  SemaphoreQ : DesQueue ;     (* queue of waiting processes    *)
                  Which      : SEMAPHORE ;    (* which semaphore are we waiting*)
                  RunName    : EntityName ;   (* process name for debugging    *)
                  Status     : State ;        (* state of process              *)
                  RunPriority: Priority ;     (* runtime priority of process   *)
                  Size       : CARDINAL ;     (* Maximum stack size            *)
                  Start      : ADDRESS ;      (* Stack start                   *)
                  Debugged   : BOOLEAN ;      (* Does user want to debug a     *)
                                              (* deadlocked process?           *)
               END ;

   DesQueue  = RECORD
                  Right,
                  Left : DESCRIPTOR ;
               END ;

   SemQueue  = RECORD
                  Right,
                  Left : SEMAPHORE ;
               END ;

   EntityName= ARRAY [0..MaxCharsInName] OF CHAR ;

   Priority  = (idle, lo, hi) ;               (* process run priority          *)

   State     = (Runnable, Suspended, WaitOnSem, WaitOnInt) ;

VAR
   ExistsQueue   : DESCRIPTOR ;               (* List of existing processes    *)
   RunQueue      : ARRAY Priority OF DESCRIPTOR ;
                                              (* List of runnable processes    *)
   CurrentProcess: DESCRIPTOR ;
   AllSemaphores : SEMAPHORE ;                (* List of all semaphores        *)
   GarbageItem   : DESCRIPTOR ;               (* Descriptor destined to free   *)


(*
   Assert -
*)

PROCEDURE Assert (c: BOOLEAN; file: ARRAY OF CHAR; line: CARDINAL;
                  function: ARRAY OF CHAR) ;
BEGIN
   IF NOT c
   THEN
      Ps ;
      Halt(file, line, function, 'assert failed')
   END
END Assert ;


(*
   InitProcess - initializes a process which is held in the suspended
                 state. When the process is resumed it will start executing
                 procedure, p. The process has a maximum stack size of,
                 StackSize, bytes and its textual name is, Name.
                 The StackSize should be at least 5000 bytes.
*)

PROCEDURE InitProcess (p: PROC;
                       StackSize: CARDINAL;
                       Name: ARRAY OF CHAR) : DESCRIPTOR ;
VAR
   d         : DESCRIPTOR ;
   ToOldState: PROTECTION ;
   db        : ARRAY [0..80] OF CHAR ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   NEW(d) ;
   WITH d^ DO
      Size        := StackSize ;
                                 (* allocate space for this processes stack *)
      ALLOCATE(Start, StackSize) ;
      NEWPROCESS(p, Start, StackSize, Volatiles) ;  (* create volatiles     *)
      InitQueue(ReadyQ) ;        (* not on the ready queue as suspended     *)
      AddToExists(d) ;           (* add process to the exists queue         *)
      InitQueue(SemaphoreQ) ;    (* not on a semaphore queue yet            *)
      Which       := NIL ;       (* not on a semaphore queue yet            *)
      StrCopy(Name, RunName) ;   (* copy name into descriptor for debugging *)
      Status      := Suspended ; (* this process will be suspended          *)
      RunPriority := lo ;        (* all processes start off at lo priority  *)
      Debugged    := FALSE ;     (* no need to debug deadlock yet!          *)
   END ;
(* ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN( d )                   (* and return a descriptor to the caller   *)
END InitProcess ;


(*
   KillProcess - kills the current process. Notice that if InitProcess
                 is called again, it might reuse the DESCRIPTOR of the
                 killed process. It is the responsibility of the caller
                 to ensure all other processes understand this process
                 is different.
*)

PROCEDURE KillProcess ;
VAR
   ToOldState: PROTECTION ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;    (* disable interrupts *) *)
   SubFromReady(CurrentProcess) ;
   SubFromExists(ExistsQueue, CurrentProcess) ;
   GarbageItem := CurrentProcess ;
   Reschedule ;
(* ToOldState := TurnInterrupts(ToOldState)         (* restore interrupts *) *)
END KillProcess ;


(*
   Resume - resumes a suspended process. If all is successful then the process, p,
            is returned. If it fails then NIL is returned.
*)

PROCEDURE Resume (d: DESCRIPTOR) : DESCRIPTOR ;
VAR
   ToOldState: PROTECTION ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)

   (* your code needs to go here *)
   WITH d^ DO                                                                  (* remove for student *)
      IF Status=Suspended                                                      (* remove for student *)
      THEN                                                                     (* remove for student *)
         (* legal state transition *)                                          (* remove for student *)
         Status := Runnable ;                         (* change status      *) (* remove for student *)
         AddToReady(d) ;                              (* add to run queue   *) (* remove for student *)
         RunQueue[RunPriority] := d ;                 (* make d at top of q *) (* remove for student *)
         Reschedule (* check whether this process has a higher run priority *) (* remove for student *)
      ELSE                                                                     (* remove for student *)
         (* we are trying to Resume a process which is *)                      (* remove for student *)
         Halt(__FILE__, __LINE__, __FUNCTION__,                                (* remove for student *)
              'trying to resume a process which is not suspended') ;           (* remove for student *)
         RETURN( NIL )        (* not held in a Suspended state - error      *) (* remove for student *)
      END                                                                      (* remove for student *)
   END ;                                                                       (* remove for student *)
(* ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN( d )
END Resume ;


(*
   Suspend - suspend the calling process.
             The process can only continue running if another process
             Resumes it.
*)

PROCEDURE Suspend ;
VAR
   ToOldState: PROTECTION ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   WITH CurrentProcess^ DO
      Status := Suspended
   END ;
   SubFromReady(CurrentProcess) ;
   Reschedule ;
(* ToOldState := TurnInterrupts(ToOldState)           (* restore interrupts *) *)
END Suspend ;


(*
   InitSemaphore - creates a semaphore whose initial value is, v, and
                   whose name is, Name.
*)

PROCEDURE InitSemaphore (v: CARDINAL; Name: ARRAY OF CHAR) : SEMAPHORE ;
VAR
   s         : SEMAPHORE ;
   ToOldState: PROTECTION ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   NEW(s) ;
   WITH s^ DO
      Value := v ;                  (* initial value of semaphore           *)
      StrCopy(Name, SemName) ;      (* save the name for future debugging   *)
      Who := NIL ;                  (* no one waiting on this semaphore yet *)
      AddToSemaphoreExists(s) ;     (* add semaphore to exists list         *)
   END ;
(* ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN( s )
END InitSemaphore ;


(*
   Wait - performs dijkstra's P operation on a semaphore.
          A process which calls this procedure will
          wait until the value of the semaphore is > 0
          and then it will decrement this value.
*)

PROCEDURE Wait (s: SEMAPHORE) ;
VAR
   ToOldState: PROTECTION ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)

   (* your code needs to go here *)
   WITH s^ DO                                                                  (* remove for student *)
      IF Value>0                                                               (* remove for student *)
      THEN                                                                     (* remove for student *)
         DEC( Value )                                                          (* remove for student *)
      ELSE                                                                     (* remove for student *)
         SubFromReady(CurrentProcess) ;               (* remove from run q  *) (* remove for student *)
         IF Who=CurrentProcess
         THEN
            Ps ;
            Halt(__FILE__, __LINE__, __FUNCTION__, 'we are already on sem')
         END ;
         AddToSemaphore(Who, CurrentProcess) ;        (* add to semaphore q *) (* remove for student *)
         CurrentProcess^.Status := WaitOnSem ;        (* set new status     *) (* remove for student *)
         CurrentProcess^.Which := s ;                 (* debugging aid      *) (* remove for student *)
         Reschedule                                   (* find next process  *) (* remove for student *)
      END                                                                      (* remove for student *)
   END ;                                                                       (* remove for student *)
(* ToOldState := TurnInterrupts(ToOldState)           (* restore interrupts *) *)
END Wait ;


(*
   Signal - performs dijkstra's V operation on a semaphore.
            A process which calls the procedure will increment
            the semaphores value.
*)

PROCEDURE Signal (s: SEMAPHORE) ;
VAR
   ToOldState: PROTECTION ;
   d         : DESCRIPTOR ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   WITH s^ DO
      IF Who=NIL
      THEN
         INC( Value )                                 (* no process waiting *)
      ELSE
         d := SubFromSemaphoreTop(Who) ; (* remove process from semaphore q *)
         d^.Which := NIL ;              (* no longer waiting on semaphore   *)
         d^.Status := Runnable ;        (* set new status                   *)
         AddToReady(d) ;                (* add process to the run queue     *)
         Reschedule                     (* find out whether there is a      *)
                                        (* higher priority to run.          *)
      END
   END ;
(* ToOldState := TurnInterrupts(ToOldState)           (* restore interrupts *) *)
END Signal ;


(*
   WaitForIO - waits for an interrupt to occur on vector, VectorNo.
*)

PROCEDURE WaitForIO (VectorNo: CARDINAL) ;
VAR
   Calling   : DESCRIPTOR ;
   Next      : PROCESS ;
   ToOldState: PROTECTION ;
   r         : INTEGER ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ; *)
(*
   DebugString('inside WaitForIO ') ;
   DebugString(CurrentProcess^.RunName) ;
   DebugString('\n') ;
*)
   Assert(CurrentProcess^.Status=Runnable,
          __FILE__, __LINE__, __FUNCTION__) ;
   SubFromReady(CurrentProcess) ;   (* remove process from run queue *)
   (*
      alter run priority to hi as all processes waiting for an interrupt
      are scheduled to run at the highest priority.
   *)
   WITH CurrentProcess^ DO
      Status := WaitOnInt ;   (* it will be blocked waiting for an interrupt.  *)
      RunPriority := hi ;     (* this (hopefully) allows it to run as soon as  *)
                              (* the interrupt occurs.                         *)
   END ;
   Calling := CurrentProcess ;      (* process which called WaitForIO          *)
   CurrentProcess := NextReady() ;  (* find next process to run while we wait  *)
   Next := CurrentProcess^.Volatiles ;
   (*
      This is quite complicated. We transfer control to the next process saving
      our volatile environment into the Calling process descriptor volatiles.
      When an interrupt occurs the calling process will be resumed and the
      interrupted process volatiles will be placed into Next.
   *)
   IOTRANSFER(Calling^.Volatiles, Next, VectorNo) ;

   (*
      At this point the interrupt has just occurred and the volatiles of
      the interrupted process are in Next. Next is the current process
      and so we must save them before picking up the Calling descriptor.
   *)

   CurrentProcess^.Volatiles := Next ;             (* carefully stored away *)
   CurrentProcess := Calling ;                     (* update CurrentProcess *)
(*
   DebugString(CurrentProcess^.RunName) ;
*)
   CurrentProcess^.Status := Runnable ;            (* add to run queue      *)
   AddToReady(CurrentProcess) ;
(*
   DebugString(' finishing WaitForIO\n') ;
*)

(* ToOldState := TurnInterrupts(ToOldState)           (* restore interrupts *) *)
END WaitForIO ;


(*
   Ps - displays a process list together with relevant their status.
*)

PROCEDURE Ps ;
VAR
   ToOldState: PROTECTION ;
   p         : DESCRIPTOR ;
   s         : SEMAPHORE ;
   a         : ARRAY [0..5] OF CHAR ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   p := ExistsQueue ;
   IF p#NIL
   THEN
      REPEAT
         DisplayProcess(p) ;
         p := p^.ExistsQ.Right
      UNTIL p=ExistsQueue
   END ;
   s := AllSemaphores ;
   IF s#NIL
   THEN
      REPEAT
         WITH s^ DO
            DebugString(SemName) ;
            WriteNSpaces(MaxCharsInName-StrLen(SemName)) ;
            CardToStr(Value, 0, a) ;
            DebugString(a) ;
            DebugString('\n')
         END ;
         s := s^.ExistsQ.Right
      UNTIL s=AllSemaphores
   END ;
(* ToOldState := TurnInterrupts(ToOldState)           (* restore interrupts *) *)
END Ps ;


(*
   DisplayProcess - displays the process, p, together with its status.
*)

PROCEDURE DisplayProcess (p: DESCRIPTOR) ;
VAR
   a: ARRAY [0..4] OF CHAR ;
BEGIN
   WITH p^ DO
      DebugString(RunName) ; WriteNSpaces(MaxCharsInName-StrLen(RunName)) ;
      CASE RunPriority OF

      idle: DebugString(' idle ') |
      lo  : DebugString(' lo   ') |
      hi  : DebugString(' hi   ')

      END ;
      CASE Status OF

      Runnable : DebugString('runnable ') |
      Suspended: DebugString('suspended') |
      WaitOnSem: DebugString('waitonsem   (') ;
                 DebugString(Which^.SemName) ;
                 DebugString(')') |
      WaitOnInt: DebugString('waitonint')

      END ;
      DebugString('\n')
   END
END DisplayProcess ;


(*
   WriteNSpaces - writes, n, spaces.
*)

PROCEDURE WriteNSpaces (n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      DebugString(' ') ;
      DEC(n)
   END
END WriteNSpaces ;


(*
   GetCurrentProcess - returns the descriptor of the current running
                       process.
*)

PROCEDURE GetCurrentProcess () : DESCRIPTOR ;
VAR
   ToOldState: PROTECTION ;
   p         : DESCRIPTOR ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;      (* disable interrupts *) *)
   p := CurrentProcess ;
(* ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN( p )
END GetCurrentProcess ;


(*
   RotateRunQueue - rotates the process run queue.
*)

PROCEDURE RotateRunQueue ;
VAR
   ToOldState: PROTECTION ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   (* we only need to rotate the lo priority processes as:
      idle - should only have one process (the idle process)
      hi   - are the device drivers which most of the time are performing
             WaitForIO
   *)
   IF RunQueue[lo]#NIL
   THEN
      RunQueue[lo] := RunQueue[lo]^.ReadyQ.Right
   END ;
(* ToOldState := TurnInterrupts(ToOldState)           (* restore interrupts *) *)
END RotateRunQueue ;


(*
   ProcessName - displays the name of process, d, through
                 DebugString.
*)

PROCEDURE ProcessName (d: DESCRIPTOR) ;
BEGIN
   DebugString(d^.RunName)
END ProcessName ;


(*
   DebugProcess -
*)

PROCEDURE DebugProcess (d: DESCRIPTOR) ;
VAR
   ToOldState: PROTECTION ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ; *)
   WITH d^ DO
      IF Status=WaitOnSem
      THEN
         DebugString('debugging process (') ;
         DebugString(RunName) ;
         DebugString(') was waiting on semaphore (') ;
         DebugString(Which^.SemName) ;
         DebugString(')\n') ;
         SubFromSemaphore(Which^.Who, d) ;
         AddToReady(d) ;
         Status := Runnable ;
         Debugged := TRUE ;
         Reschedule
      ELSE
         DebugString('can only debug deadlocked processes (') ;
         DebugString(RunName) ;
         DebugString(') which are waiting on a semaphore\n')
      END
   END ;
(* ToOldState := TurnInterrupts(ToOldState) *)
END DebugProcess ;


(*
   CheckDebugged - checks to see whether the debugged flag has
                   been set by the debugger.
                   TRUE  is returned if the process was debugged.
                   FALSE is returned if the process was not debugged.
*)

PROCEDURE CheckDebugged () : BOOLEAN ;
BEGIN
   WITH CurrentProcess^ DO
      IF Debugged
      THEN
         (*
            You will see this comment after you have enabled a
            deadlocked process to continue via the gdb command:

            print Executive_DebugProcess(d)

            debugger caused deadlocked process to continue
         *)
         (* gdb.breakpoint ; *)
         Debugged := FALSE ;
         SubFromReady(CurrentProcess) ;
         AddToSemaphore(Which^.Who, CurrentProcess) ;
                                           (* add it back to the queue sem *)
         Status := WaitOnSem ;

         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END CheckDebugged ;


(*
   Reschedule - reschedules to the highest runnable process.
*)

PROCEDURE Reschedule ;
BEGIN
   (*
      the repeat loop allows us to debug a process even when it is
      technically waiting on a semaphore. We run the process into
      a breakpoint and then back into this schedule routine.
      This is really useful when trying to find out why processes have
      deadlocked.
   *)
   REPEAT
      ScheduleProcess
   UNTIL NOT CheckDebugged()
END Reschedule ;


(*
   ScheduleProcess - finds the highest priority Runnable process and
                     then transfers control to it.
*)

PROCEDURE ScheduleProcess ;
VAR
   From,
   Highest: DESCRIPTOR ;
BEGIN
   Highest := NextReady() ;

   (* rotate ready Q to ensure fairness *)
   RunQueue[Highest^.RunPriority] := Highest^.ReadyQ.Right ;

   (* no need to transfer if Highest=CurrentProcess *)
   IF Highest#CurrentProcess
   THEN
      From := CurrentProcess ;
(*
      DebugString('context switching from ') ; DebugString(From^.RunName) ;
*)
      (* alter CurrentProcess before we TRANSFER *)
      CurrentProcess := Highest ;
(*
      DebugString(' to ') ; DebugString(CurrentProcess^.RunName) ;
*)

      TRANSFER(From^.Volatiles, Highest^.Volatiles) ;
(*
      ; DebugString(' (') ; DebugString(CurrentProcess^.RunName) ;
      DebugString(')\n') ;
*)
      CheckGarbageCollect
   END
END ScheduleProcess ;


(*
   NextReady - returns the highest priority Runnable process.
*)

PROCEDURE NextReady () : DESCRIPTOR ;
VAR
   Highest: DESCRIPTOR ;
   Pri    : Priority ;
BEGIN
   Highest := NIL ;
   FOR Pri := idle TO hi DO
      IF RunQueue[Pri]#NIL
      THEN
         Highest := RunQueue[Pri]
      END
   END ;
   Assert(Highest#NIL, __FILE__, __LINE__, __FUNCTION__) ;
   RETURN( Highest )
END NextReady ;


(*
   CheckGarbageCollect - checks to see whether GarbageItem is set
                         and if so it deallocates storage associated
                         with this descriptor.
*)

PROCEDURE CheckGarbageCollect ;
BEGIN
   IF GarbageItem#NIL
   THEN
      WITH GarbageItem^ DO
         DEALLOCATE(Start, Size)
      END ;
      DISPOSE(GarbageItem) ;
      GarbageItem := NIL
   END
END CheckGarbageCollect ;


(*
   AddToExists - adds item, Item, to the exists queue.
*)

PROCEDURE AddToExists (Item: DESCRIPTOR) ;
BEGIN
   IF ExistsQueue=NIL
   THEN
      ExistsQueue := Item ;                  (* Head is empty therefore make *)
      Item^.ExistsQ.Left := Item ;           (* Item the only entry on this  *)
      Item^.ExistsQ.Right := Item            (* queue.                       *)
   ELSE
      Item^.ExistsQ.Right := ExistsQueue ;   (* Add Item to the end of queue *)
      Item^.ExistsQ.Left  := ExistsQueue^.ExistsQ.Left ;
      ExistsQueue^.ExistsQ.Left^.ExistsQ.Right := Item ;
      ExistsQueue^.ExistsQ.Left := Item
   END
END AddToExists ;


(*
   SubFromExists - removes a process, Item, from the exists queue, Head.
*)

PROCEDURE SubFromExists (VAR Head: DESCRIPTOR; Item: DESCRIPTOR) ;
BEGIN
   IF (Item^.ExistsQ.Right=Head) AND (Item=Head)
   THEN
      Head := NIL
   ELSE
      IF Head=Item
      THEN
         Head := Head^.ExistsQ.Right
      END ;
      Item^.ExistsQ.Left^.ExistsQ.Right := Item^.ExistsQ.Right ;
      Item^.ExistsQ.Right^.ExistsQ.Left := Item^.ExistsQ.Left
   END
END SubFromExists ;


(*
   AddToSemaphore - adds item, Item, to the semaphore queue defined by Head.
*)

PROCEDURE AddToSemaphore (VAR Head: DESCRIPTOR; Item: DESCRIPTOR) ;
BEGIN
   IF Head=NIL
   THEN
      Head := Item ;                         (* Head is empty therefore make *)
      Item^.SemaphoreQ.Left := Item ;        (* Item the only entry on this  *)
      Item^.SemaphoreQ.Right := Item         (* queue.                       *)
   ELSE
      Item^.SemaphoreQ.Right := Head ;       (* Add Item to the end of queue *)
      Item^.SemaphoreQ.Left  := Head^.SemaphoreQ.Left ;
      Head^.SemaphoreQ.Left^.SemaphoreQ.Right := Item ;
      Head^.SemaphoreQ.Left := Item
   END
END AddToSemaphore ;


(*
   AddToSemaphoreExists - adds item, Item, to the semaphore exists queue.
*)

PROCEDURE AddToSemaphoreExists (Item: SEMAPHORE) ;
BEGIN
   IF AllSemaphores=NIL
   THEN
      AllSemaphores := Item ;                (* Head is empty therefore make *)
      Item^.ExistsQ.Left := Item ;           (* Item the only entry on this  *)
      Item^.ExistsQ.Right := Item            (* queue.                       *)
   ELSE
      Item^.ExistsQ.Right := AllSemaphores ;
                                             (* Add Item to the end of queue *)
      Item^.ExistsQ.Left  := AllSemaphores^.ExistsQ.Left ;
      AllSemaphores^.ExistsQ.Left^.ExistsQ.Right := Item ;
      AllSemaphores^.ExistsQ.Left := Item
   END
END AddToSemaphoreExists ;


(*
   AddToReady - adds item, Item, to the ready queue.
*)

PROCEDURE AddToReady (Item: DESCRIPTOR) ;
BEGIN
   AddToReadyQ(RunQueue[Item^.RunPriority], Item)
END AddToReady ;


(*
   AddToReadyQ - adds item, Item, to the ready queue defined by Head.
*)

PROCEDURE AddToReadyQ (VAR Head: DESCRIPTOR; Item: DESCRIPTOR) ;
BEGIN
   IF Head=NIL
   THEN
      Head := Item ;                         (* Head is empty therefore make *)
      Item^.ReadyQ.Left := Item ;            (* Item the only entry on this  *)
      Item^.ReadyQ.Right := Item             (* queue.                       *)
   ELSE
      Item^.ReadyQ.Right := Head ;           (* Add Item to the end of queue *)
      Item^.ReadyQ.Left  := Head^.ReadyQ.Left ;
      Head^.ReadyQ.Left^.ReadyQ.Right := Item ;
      Head^.ReadyQ.Left := Item
   END
END AddToReadyQ ;


(*
   SubFromReady - subtract process descriptor, Item, from the Ready queue.
*)

PROCEDURE SubFromReady (Item: DESCRIPTOR) ;
BEGIN
   SubFromReadyQ(RunQueue[Item^.RunPriority], Item)
END SubFromReady ;


(*
   SubFromReadyQ - removes a process, Item, from a queue, Head.
*)

PROCEDURE SubFromReadyQ (VAR Head: DESCRIPTOR; Item: DESCRIPTOR) ;
BEGIN
   IF (Item^.ReadyQ.Right=Head) AND (Item=Head)
   THEN
      Head := NIL
   ELSE
      IF Head=Item
      THEN
         Head := Head^.ReadyQ.Right
      END ;
      Item^.ReadyQ.Left^.ReadyQ.Right := Item^.ReadyQ.Right ;
      Item^.ReadyQ.Right^.ReadyQ.Left := Item^.ReadyQ.Left
   END
END SubFromReadyQ ;


(*
   SubFromSemaphoreTop - returns the first descriptor in the
                         semaphore queue.
*)

PROCEDURE SubFromSemaphoreTop (VAR Head: DESCRIPTOR) : DESCRIPTOR ;
VAR
   Top: DESCRIPTOR ;
BEGIN
   Top := Head ;
   SubFromSemaphore(Head, Top) ;
   RETURN( Top )
END SubFromSemaphoreTop ;


(*
   SubFromSemaphore - removes a process, Item, from a queue, Head.
*)

PROCEDURE SubFromSemaphore (VAR Head: DESCRIPTOR; Item: DESCRIPTOR) ;
BEGIN
   IF (Item^.SemaphoreQ.Right=Head) AND (Item=Head)
   THEN
      Head := NIL
   ELSE
      IF Head=Item
      THEN
         Head := Head^.SemaphoreQ.Right
      END ;
      Item^.SemaphoreQ.Left^.SemaphoreQ.Right := Item^.SemaphoreQ.Right ;
      Item^.SemaphoreQ.Right^.SemaphoreQ.Left := Item^.SemaphoreQ.Left
   END
END SubFromSemaphore ;


(*
   Idle - this process is only run whenever there is no other Runnable
          process. It should never be removed from the run queue.
*)

PROCEDURE Idle ;
VAR
   ToOldState: PROTECTION ;
BEGIN
   ToOldState := TurnInterrupts(MIN(PROTECTION)) ;    (* enable interrupts *)
   LOOP
      (*
         Listen for interrupts.
         We could solve chess endgames here or calculate PI etc.
         We forever wait for an interrupt since there is nothing else
         to do...
      *)
      ListenLoop
   END
   (* we must NEVER exit from the above loop *)
END Idle ;


(*
   InitIdleProcess - creates an idle process descriptor which
                     is run whenever no other process is Runnable.
                     The Idle process should be the only process which
                     has the priority idle.
*)

VAR
   IdleProcess: DESCRIPTOR ;              (* Idle process always runnable  *)

PROCEDURE InitIdleProcess ;
VAR
   db         : ARRAY [0..80] OF CHAR ;
BEGIN
   NEW(IdleProcess) ;
   WITH IdleProcess^ DO
      ALLOCATE(Start, IdleStackSize) ;
      Size := IdleStackSize ;
      NEWPROCESS(Idle, Start, IdleStackSize, Volatiles) ;
      InitQueue(SemaphoreQ) ;             (* not on a semaphore queue     *)
      Which      := NIL ;                 (* at all.                      *)
      StrCopy('Idle', RunName) ;          (* idle process's name          *)
      Status := Runnable ;                (* should always be idle        *)
      RunPriority := idle ;               (* lowest priority possible     *)
      Debugged    := FALSE ;              (* should never be debugging    *)
   END ;
   AddToReady(IdleProcess) ;              (* should be the only           *)
                                          (* process at this run priority *)
   AddToExists(IdleProcess)               (* process now exists..         *)
END InitIdleProcess ;


(*
   InitInitProcess - creates a descriptor for this running environment
                     so it too can be manipulated by Reschedule.

                     This concept is important to understand.
                     InitInitProcess is called by the startup code to this
                     module. It ensures that the current stack and processor
                     volatiles can be "housed" in a process descriptor and
                     therefore it can be manipulated just like any other
                     process.
*)

PROCEDURE InitInitProcess ;
BEGIN
   NEW(CurrentProcess) ;
   WITH CurrentProcess^ DO
      Size := 0 ;              (* we dont know the size of main stack  *)
      Start := NIL ;           (* we don't need to know where it is.   *)
      InitQueue(ReadyQ) ;      (* assign queues to NIL                 *)
      InitQueue(ExistsQ) ;
      InitQueue(SemaphoreQ) ;  (* not waiting on a semaphore queue yet *)
      Which       := NIL ;     (* at all.                              *)
      StrCopy('Init', RunName) ;  (* name for debugging purposes       *)
      Status      := Runnable ;   (* currently running                 *)
      RunPriority := lo ;         (* default status                    *)
      Debugged    := FALSE ;      (* not deadlock debugging yet        *)
   END ;
   AddToExists(CurrentProcess) ;
   AddToReady(CurrentProcess)
END InitInitProcess ;


(*
   InitQueue - initializes a queue, q, to empty.
*)

PROCEDURE InitQueue (VAR q: DesQueue) ;
BEGIN
   WITH q DO
      Right := NIL ;
      Left  := NIL
   END
END InitQueue ;


(*
   Init - initializes all the global variables.
*)

PROCEDURE Init ;
BEGIN
   ExistsQueue := NIL ;
   RunQueue[lo] := NIL ;
   RunQueue[hi] := NIL ;
   RunQueue[idle] := NIL ;
   AllSemaphores := NIL ;
   GarbageItem := NIL ;
   InitInitProcess ;
   InitIdleProcess
END Init ;


BEGIN
   Init
END Executive.
