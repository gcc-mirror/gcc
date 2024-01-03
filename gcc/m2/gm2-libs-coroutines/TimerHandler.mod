(* TimerHandler.mod provides a simple timer handler for the Executive.

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

IMPLEMENTATION MODULE TimerHandler[MAX(PROTECTION)] ;


FROM COROUTINES IMPORT PROTECTION ;
FROM SysStorage IMPORT ALLOCATE ;
FROM NumberIO IMPORT CardToStr ;
FROM Debug IMPORT Halt, DebugString ;
FROM KeyBoardLEDs IMPORT SwitchScroll ;
FROM RTint IMPORT ReArmTimeVector, GetTimeVector, InitTimeVector ;
FROM Executive IMPORT DESCRIPTOR, Suspend, Resume, GetCurrentProcess,
                      WaitForIO, InitProcess, RotateRunQueue,
                      ProcessName, Ps ;

CONST
   MaxQuantum     =     4 ;   (* Maximum ticks a process may consume    *)
                              (* before being rescheduled.              *)
   BaseTicks      = 1000000 ; (* Max resolution of clock ticks per sec  *)
   TimerStackSize = 100000H ; (* Reasonable sized stack for a process   *)
   Debugging      =  FALSE ;  (* Do you want lots of debugging info?    *)
   EnableLED      =  FALSE ;  (* Should the scroll LED be pulsed?       *)

TYPE
   EVENT = POINTER TO RECORD
                         EventQ      : Queue ;
                         WhichQ      : QueueType ;
                         Process     : DESCRIPTOR ;
                         NoOfTicks   : CARDINAL ;
                         WasCancelled: BOOLEAN ;
                      END ;

   (* the queue types are either:

      active queue which has a list of outstanding events
      dead queue which is essentially the free list
      solo which is no queue and the event is in limbo
   *)

   QueueType = (active, dead, solo) ;

   Queue  = RECORD
               Right,
               Left : EVENT ;
            END ;

VAR
   TotalTicks    : CARDINAL ;   (* System up time tick count            *)
   CurrentQuanta : CARDINAL ;   (* Currentprocess time quanta allowance *)
   ActiveQueue,                 (* Queue of outstanding timer requests  *)
   DeadQueue     : EVENT ;      (* Free list of events.                 *)


(*
   GetTicks - returns the number of ticks since boottime.
*)

PROCEDURE GetTicks () : CARDINAL ;
VAR
   ToOldState : PROTECTION ;
   CopyOfTicks: CARDINAL ;
BEGIN
(*   ToOldState := TurnInterrupts(MAX(PROTECTION)) ;      (* disable interrupts *) *)
   CopyOfTicks := TotalTicks ;
(*   ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN( CopyOfTicks )
END GetTicks ;


(*
   Sleep - suspends the current process for a time, t.
           The time is measured in ticks.
*)

PROCEDURE Sleep (t: CARDINAL) ;
VAR
   ToOldState: PROTECTION ;
   e         : EVENT ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;               (* disable interrupts *) *)
   e := ArmEvent (t) ;
   IF WaitOn (e)
   THEN
   END ;
(* ToOldState := TurnInterrupts(ToOldState)          (* restore interrupts *) *)
END Sleep ;


(*
   More lower system calls to the timer procedures follow,
   they are necessary to allow handling multiple events.
*)


(*
   ArmEvent - initializes an event, e, to occur at time, t.
              The time, t, is measured in ticks.
              The event is NOT placed onto the event queue.
*)

PROCEDURE ArmEvent (t: CARDINAL) : EVENT ;
VAR
   e         : EVENT ;
   ToOldState: PROTECTION ;
   Ticks     : CARDINAL ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   e := CreateSolo() ;

   (* your code needs to go here *)
   WITH e^ DO                                                                       (* remove for student *)
      InitQueue(EventQ) ;           (* not on a queue yet                   *)      (* remove for student *)
      WhichQ       := solo ;        (* and set the queue state accordingly  *)      (* remove for student *)
      Process      := NIL ;         (* no process waiting event yet         *)      (* remove for student *)
      NoOfTicks    := t ;           (* absolute number of ticks             *)      (* remove for student *)
      WasCancelled := FALSE ;       (* has not been cancelled               *)      (* remove for student *)
   END ;                                                                            (* remove for student *)

(* ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN( e )
END ArmEvent ;


(*
   WaitOn - places event, e, onto the event queue and then the calling
            process suspends. It is resumed up by either the event
            expiring or the event, e, being cancelled.
            TRUE is returned if the event was cancelled
            FALSE is returned if the event expires.
            The event, e, is always assigned to NIL when the function
            finishes.
*)

PROCEDURE WaitOn (VAR e: EVENT) : BOOLEAN ;
VAR
   ToOldState: PROTECTION ;
   Cancelled : BOOLEAN ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   IF e=NIL
   THEN
      Halt ('event should never be NIL',
            __FILE__, __FUNCTION__, __LINE__)
   ELSE
      WITH e^ DO
         (* we will just check to see whether someone has cancelled this    *)
         (* event before it ever got to the queue...                        *)
         IF NOT WasCancelled
         THEN
            (* right so it wasn't cancelled. Lets place it on the queue and *)
            (* go to sleep.                                                 *)
            Process := GetCurrentProcess() ;  (* so we know who is waiting  *)
            OnActiveQueue(e) ;                (* add to the queue and then  *)

            IF Debugging
            THEN
               DisplayActive ;  (* debugging *)
            END ;

            Suspend                           (* wait for Resume (we sleep) *)
         END ;
         (* At this point we have either been cancelled or not. We must     *)
         (* check the event again as we might have been sleeping (Suspend)  *)
         Cancelled := WasCancelled
      END
   END ;
   OnDeadQueue(e) ;              (* now it is safe to throw this event away *)
   e := NIL ;
(* ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN Cancelled
END WaitOn ;


(*
   Cancel - cancels the event, e, on the event queue and makes
            the appropriate process runnable again.
            TRUE is returned if the event was cancelled and
            FALSE is returned is the event was not found or
                  no process was waiting on this event.
*)

PROCEDURE Cancel (e: EVENT) : BOOLEAN ;
VAR
   ToOldState: PROTECTION ;
   Cancelled : BOOLEAN ;
   Private   : DESCRIPTOR ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   IF IsOnActiveQueue(e)
   THEN
      WITH e^ DO
         Cancelled := NOT WasCancelled ;
         IF WasCancelled
         THEN
            Halt ('inconsistancy event has been cancelled and it is on queue',
                  __FILE__, __FUNCTION__, __LINE__)
         END ;
         OnSoloQueue(e) ;
         WasCancelled := TRUE ;
         IF Process#NIL                (* double check that it has not     *)
                                       (* already been cancelled           *)
         THEN
            Private := Process ;       (* we use our own Private variable  *)
            Process := NIL ;           (* as we need to set Process to NIL *)
            Process := Resume(Private) (* before we Resume. Otherwise      *)
                                       (* there is the possibility that it *)
                                       (* might be reused before we := NIL *)
                                       (* (because when we touch Resume    *)
                                       (* another process could run and..) *)
         END
      END
   ELSE
      Cancelled := FALSE
   END ;
(* ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN( Cancelled )
END Cancel ;


(*
   ReArmEvent - removes an event, e, from the event queue. A new time
                is given to this event and it is then re-inserted onto the
                event queue in the correct place.
                TRUE is returned if this occurred
                FALSE is returned if the event was not found.
*)

PROCEDURE ReArmEvent (e: EVENT; t: CARDINAL) : BOOLEAN ;
VAR
   ToOldState: PROTECTION ;
   ReArmed   : BOOLEAN ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ;                (* disable interrupts *) *)
   WITH e^ DO
      IF WasCancelled
      THEN
         ReArmed := FALSE
      ELSIF IsOnActiveQueue(e) OR IsOnSoloQueue(e)
      THEN
         ReArmed := TRUE ;
         OnSoloQueue(e) ;                             (* remove from queue  *)
         NoOfTicks := t ;                             (* give it a new time *)
         OnActiveQueue(e)                             (* back on queue      *)
      ELSE
         Halt ('ReArm should not be asked to ReArm a dead event',
               __FILE__, __FUNCTION__, __LINE__)
      END
   END ;
(* ToOldState := TurnInterrupts(ToOldState) ;         (* restore interrupts *) *)
   RETURN( ReArmed )
END ReArmEvent ;


(*
   StartClock - ticks is milli seconds.
*)

PROCEDURE StartClock (vec: CARDINAL; ticks: CARDINAL) ;
BEGIN
   ReArmTimeVector (vec, ticks MOD BaseTicks, ticks DIV BaseTicks)
END StartClock ;


(*
   LoadClock - returns the number of milli seconds.
*)

PROCEDURE LoadClock (vec: CARDINAL) : CARDINAL ;
VAR
   micro, secs: CARDINAL ;
BEGIN
   GetTimeVector (vec, micro, secs) ;
   RETURN secs * BaseTicks + micro
END LoadClock ;


(*
   Timer  - is a process which serves the clock interrupt.
            Its function is fourfold:

            (i)   to maintain the timer event queue
            (ii)  to give some fairness to processes via round robin scheduling
            (iii) to keep a count of the total ticks so far  (time of day)
            (iv)  provide a heartbeat sign of life via the scroll lock LED
*)

PROCEDURE Timer ;
VAR
   CurrentCount: CARDINAL ;
   ToOldState  : PROTECTION ;
   ScrollLED   : BOOLEAN ;
   TimerIntNo  : CARDINAL ;
   r           : INTEGER ;
BEGIN
(* ToOldState := TurnInterrupts(MAX(PROTECTION)) ; *)
   ScrollLED := FALSE ;
   TimerIntNo := InitTimeVector ((BaseTicks DIV TicksPerSecond) MOD BaseTicks,
                                 (BaseTicks DIV TicksPerSecond) DIV BaseTicks,
                                 MAX (PROTECTION)) ;
   LOOP
      WaitForIO (TimerIntNo) ;

      (* Get current clock count *)
      CurrentCount := (* LoadClock(TimerIntNo) ; *) 0 ;
      (* Now compenstate for lost ticks *)
      StartClock (TimerIntNo, CurrentCount + (BaseTicks DIV TicksPerSecond)) ;

      INC (TotalTicks) ;                                     (* (iii) *)
      IF EnableLED
      THEN
         (* now pulse scroll LED *)
         IF (TotalTicks MOD TicksPerSecond) = 0
         THEN
            ScrollLED := NOT ScrollLED ;
            (* r := printf("<scroll %d>", TotalTicks); *)
            SwitchScroll(ScrollLED)                          (* (iv)  *)
         END
      END ;
      IF (TotalTicks MOD MaxQuantum) = 0
      THEN
         RotateRunQueue                                      (* (ii)  *)
      END ;

      CheckActiveQueue                                       (* (i)   *)
   END
END Timer ;


(*
   CheckActiveQueue - purpose is:

                      (i)   to remove all events which have expired
                      (ii)  resume all processes waiting on these events
                      (iii) decrement the first event with a non zero NoOfTicks
*)

PROCEDURE CheckActiveQueue ;
VAR
   e      : EVENT ;
   Private: DESCRIPTOR ;
BEGIN
   IF Debugging
   THEN
      DebugString('inside CheckActiveQueue\n') ;
      DisplayActive
   END ;
   WHILE (ActiveQueue#NIL) AND (ActiveQueue^.NoOfTicks=0) DO    (* (i)  *)
      e := ActiveQueue ;
      OnSoloQueue(e) ;
      (* note we do not put it onto the dead queue. The process
         waiting for the event will place, e, onto the dead queue *)
      WITH e^ DO
         IF (NOT WasCancelled) AND (Process#NIL)
         THEN
            Private := Process ;    (* we use our own Private variable  *)
            Process := NIL ;        (* as we might context switch in    *)
            Process := Resume(Private) ;  (* resume.               (ii) *)
            IF Debugging
            THEN
               Ps
            END
         END
      END
   END ;
   IF ActiveQueue#NIL
   THEN
      DEC(ActiveQueue^.NoOfTicks)                              (* (iii) *)
   END ;
   IF Debugging
   THEN
      DebugString('after CheckActiveQueue\n') ;
      DisplayActive
   END ;
END CheckActiveQueue ;


(*
   CreateSolo - create a new event. It does this by either getting an event from
                the dead queue or (if the dead queue is empty) an event is created
                by using NEW.
*)

PROCEDURE CreateSolo () : EVENT ;
VAR
   e: EVENT ;
BEGIN
   IF DeadQueue=NIL
   THEN
      NEW(e)
   ELSE
      e := DeadQueue ;
      SubFrom(DeadQueue, e)
   END ;
   e^.WhichQ := solo ;
   RETURN( e )
END CreateSolo ;


(*
   RemoveFromDead - removes event, e, from the dead queue.
*)

PROCEDURE RemoveFromDead (e: EVENT) ;
BEGIN
   SubFrom(DeadQueue, e)
END RemoveFromDead ;


(*
   OnDeadQueue - places an event onto the dead queue.
*)

PROCEDURE OnDeadQueue (e: EVENT) ;
BEGIN
   IF e#NIL
   THEN
      OnSoloQueue(e) ;                   (* put on solo queue first       *)
      AddTo(DeadQueue, e) ;              (* now safe to put on dead queue *)
      e^.WhichQ := dead
   END
END OnDeadQueue ;


(*
   OnSoloQueue - places an event onto the solo queue.
*)

PROCEDURE OnSoloQueue (e: EVENT) ;
BEGIN
   IF e#NIL
   THEN
      IF IsOnActiveQueue(e)
      THEN
         RemoveFromActive(e)
      ELSIF IsOnDeadQueue(e)
      THEN
         RemoveFromDead(e)
      END ;
      e^.WhichQ := solo
   END
END OnSoloQueue ;


(*
   OnActiveQueue - places an event onto the active queue.
*)

PROCEDURE OnActiveQueue (e: EVENT) ;
BEGIN
   IF e#NIL
   THEN
      IF IsOnDeadQueue(e)
      THEN
         Halt ('illegal state change',
               __FILE__, __FUNCTION__, __LINE__)
      ELSIF IsOnSoloQueue(e)
      THEN
         RelativeAddToActive(e) ;
         e^.WhichQ := active
      END
   END
END OnActiveQueue ;


(*
   IsOnSoloQueue - returns TRUE if event, e, is on the solo queue.
*)

PROCEDURE IsOnSoloQueue (e: EVENT) : BOOLEAN ;
BEGIN
   RETURN( (e#NIL) AND (e^.WhichQ=solo) )
END IsOnSoloQueue ;


(*
   IsOnDeadQueue - returns TRUE if event, e, is on the dead queue.
*)

PROCEDURE IsOnDeadQueue (e: EVENT) : BOOLEAN ;
BEGIN
   RETURN( (e#NIL) AND (e^.WhichQ=dead) )
END IsOnDeadQueue ;


(*
   IsOnActiveQueue - returns TRUE if event, e, is on the active queue.
*)

PROCEDURE IsOnActiveQueue (e: EVENT) : BOOLEAN ;
BEGIN
   RETURN( (e#NIL) AND (e^.WhichQ=active) )
END IsOnActiveQueue ;


(*
   RemoveFromActive - removes an event, e, from the active queue.
*)

PROCEDURE RemoveFromActive (e: EVENT) ;
BEGIN
   IF ActiveQueue=e
   THEN
      SubFrom(ActiveQueue, e) ;
      (* providing that the ActiveQueue is non empty we need to
         modify first event ticks as we have removed the first event, e. *)
      IF ActiveQueue#NIL
      THEN
         INC(ActiveQueue^.NoOfTicks, e^.NoOfTicks)
      END
   ELSE
      (* providing that event, e, is not the last event on the list then
         update the next event by the time of, e. *)
      IF e^.EventQ.Right#ActiveQueue
      THEN
         INC(e^.EventQ.Right^.NoOfTicks, e^.NoOfTicks)
      END ;
      SubFrom(ActiveQueue, e)
   END
END RemoveFromActive ;


(*
   InsertBefore - insert an event, new, on a circular event queue BEFORE
                  event, pos.
*)

PROCEDURE InsertBefore (VAR Head: EVENT; pos, new: EVENT) ;
BEGIN
   IF Head=NIL
   THEN
      (* empty queue *)
      Head := new ;
      new^.EventQ.Right := new ;
      new^.EventQ.Left := new
   ELSIF Head=pos
   THEN
      (* insert before the first element on the queue *)
      new^.EventQ.Right := pos ;
      new^.EventQ.Left := pos^.EventQ.Left ;
      pos^.EventQ.Left^.EventQ.Right := new ;
      pos^.EventQ.Left := new ;
      Head := new
   ELSE
      (* insert before any other element *)
      new^.EventQ.Right := pos ;
      new^.EventQ.Left := pos^.EventQ.Left ;
      pos^.EventQ.Left^.EventQ.Right := new ;
      pos^.EventQ.Left := new
   END
END InsertBefore ;


(*
   InsertAfter - place an event, new, AFTER the event pos on any circular event queue.
*)

PROCEDURE InsertAfter (pos, new: EVENT) ;
BEGIN
   new^.EventQ.Right := pos^.EventQ.Right ;
   new^.EventQ.Left := pos ;
   pos^.EventQ.Right^.EventQ.Left := new ;
   pos^.EventQ.Right := new
END InsertAfter ;


(*
   RelativeAddToActive - the active event queue is an ordered queue of
                         relative time events.
                         The event, e, is inserted at the appropriate
                         position in the queue. The event, e, enters
                         this routine with an absolute NoOfTicks field which
                         is then used to work out the relative position
                         of the event. After the position is found then
                         the absolute NoOfTicks field is altered to a
                         relative value and inserted on the queue.
*)

PROCEDURE RelativeAddToActive (e: EVENT) ;
VAR
   t  : EVENT ;
   sum: CARDINAL ;
BEGIN
   IF ActiveQueue = NIL
   THEN
      (* simple as the queue is empty (relative=absolute) *)
      InsertBefore (ActiveQueue, ActiveQueue, e)
   ELSE
      (* at the end of the while loop sum will contain the total of all
         events up to but not including, t.
         If the value of sum is <  e^.NoOfTicks then e must be placed at the end
                                >= e^.NoOfTicks then e needs to be placed in the middle
      *)

      sum := ActiveQueue^.NoOfTicks ;
      t := ActiveQueue^.EventQ.Right ;      (* second event *)
      WHILE (sum < e^.NoOfTicks) AND (t # ActiveQueue) DO
         INC (sum, t^.NoOfTicks) ;
         t := t^.EventQ.Right
      END ;
      IF sum < e^.NoOfTicks
      THEN
         (* e will occur after all the current ActiveQueue has expired therefore
            we must add it to the end of the ActiveQueue. *)
         DEC (e^.NoOfTicks, sum) ;
         InsertAfter (ActiveQueue^.EventQ.Left, e)
      ELSE
         (* as sum >= e^.NoOfTicks we know that e is scheduled to occur
            in the middle of the queue but before t^.Left
         *)
         DEC (e^.NoOfTicks, sum-t^.EventQ.Left^.NoOfTicks) ;
         InsertBefore (ActiveQueue, t^.EventQ.Left, e)
      END ;
      (* the first event after e must have its relative NoOfTicks altered *)
      IF e^.EventQ.Right # ActiveQueue
      THEN
         DEC (e^.EventQ.Right^.NoOfTicks, e^.NoOfTicks)
      END
   END
END RelativeAddToActive ;


(*
   AddTo - adds an event to a specified queue.
*)

PROCEDURE AddTo (VAR Head: EVENT; e: EVENT) ;
BEGIN
   IF Head=NIL
   THEN
      Head := e ;
      e^.EventQ.Left := e ;
      e^.EventQ.Right := e
   ELSE
      e^.EventQ.Right := Head ;
      e^.EventQ.Left := Head^.EventQ.Left ;
      Head^.EventQ.Left^.EventQ.Right := e ;
      Head^.EventQ.Left := e
   END
END AddTo ;


(*
   SubFrom - removes an event from a queue.
*)

PROCEDURE SubFrom (VAR Head: EVENT; e: EVENT) ;
BEGIN
   IF (e^.EventQ.Left = Head) AND (e = Head)
   THEN
      Head := NIL
   ELSE
      IF Head = e
      THEN
         Head := Head^.EventQ.Right
      END ;
      e^.EventQ.Left^.EventQ.Right := e^.EventQ.Right ;
      e^.EventQ.Right^.EventQ.Left := e^.EventQ.Left
   END
END SubFrom ;


(*
   DisplayActive - display the active queue.
*)

PROCEDURE DisplayActive ;
VAR
   e: EVENT ;
BEGIN
   e := ActiveQueue ;
   IF e#NIL
   THEN
      REPEAT
         DisplayEvent(e) ;
         e := e^.EventQ.Right
      UNTIL e=ActiveQueue
   END
END DisplayActive ;


(*
   DisplayEvent - display a single event, e.
*)

PROCEDURE DisplayEvent (e: EVENT) ;
VAR
   a: ARRAY [0..20] OF CHAR ;
BEGIN
   WITH e^ DO
      CardToStr(NoOfTicks, 6, a) ;
      DebugString(a) ;
      DebugString('  process (') ;
      IF Process=NIL
      THEN
         DebugString('is NIL') ;
      ELSE
         ProcessName(Process)
      END ;
      DebugString(')') ;
      IF WasCancelled
      THEN
         DebugString('  has been cancelled')
      END
   END ;
   DebugString('\n')
END DisplayEvent ;


(*
   InitQueue -
*)

PROCEDURE InitQueue (VAR q: Queue) ;
BEGIN
   q.Right := NIL ;
   q.Left  := NIL
END InitQueue ;


(*
   Init - starts the timer process and initializes some queues.
*)

PROCEDURE Init ;
VAR
   d: DESCRIPTOR ;
BEGIN
   TotalTicks := 0 ;
   CurrentQuanta := 0 ;
   ActiveQueue := NIL ;
   DeadQueue := NIL ;
   d := Resume(InitProcess(Timer, TimerStackSize, 'Timer'))
END Init ;


BEGIN
   Init
END TimerHandler.
