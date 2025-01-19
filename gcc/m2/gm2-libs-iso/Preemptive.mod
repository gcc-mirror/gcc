(* Premptive.mod provides the Processes module with a premptive scheduler.

Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Preemptive ;

FROM COROUTINES IMPORT TurnInterrupts, PROTECTION ;
FROM Processes IMPORT Wait, Attach, Detach, Create, ProcessId, Urgency, Activate, SuspendMe ;
FROM RTint IMPORT InitTimeVector, ReArmTimeVector, IncludeVector, ExcludeVector ;
FROM libc IMPORT printf ;

CONST
   debugging = FALSE ;
   (* The space we request becomes part of a stack request, which generally
      has constraints on size and alignment.  *)
   extraWorkSpace = 10 * 1024 * 1024 ;

(*
   timer - the timer process which runs at maximum scheduling priority with
           interrupts off.  It sleeps for a time quantum, performs a Wait
           which will rotate the ready queue and then Waits again.
*)

PROCEDURE timer ;
VAR
   vec,
   currentUsec,
   currentSec : CARDINAL ;
   old        : PROTECTION ;
BEGIN
   IF debugging
   THEN
      printf ("timer\n");
   END ;
   old := TurnInterrupts (MAX (PROTECTION)) ;
   vec := InitTimeVector (timeSliceUsec, timeSliceSec, MAX (PROTECTION)) ;
   IF debugging
   THEN
      printf ("attach\n");
   END ;
   Attach (vec) ;  (* attach vector to this process.  *)
   IF debugging
   THEN
      printf ("include vec\n");
   END ;
   IncludeVector (vec) ;
   LOOP
      currentSec := timeSliceSec ;
      currentUsec := timeSliceUsec ;
      IF debugging
      THEN
         printf ("timer process about to Wait\n");
      END ;
      Wait ;
      (*
      printf ("yes 2 seconds elapsed, suspending\n");
      SuspendMe ;
      *)
      IF debugging
      THEN
         printf ("timer process wakes up, now calling ReArmTimeVector\n");
      END ;
      ReArmTimeVector (vec, timeSliceUsec, timeSliceSec) ;
      IF debugging
      THEN
         printf ("ReArmTimeVector complete\n");
         printf ("attach\n");
      END ;
      Attach (vec) ;  (* attach vector to this process.  *)
      IF debugging
      THEN
         printf ("finished attach, now include vec\n");
      END ;
      IncludeVector (vec) ;
   END
END timer ;


(*
   initPreemptive - if millisecs > 0 then turn on preemptive scheduling.
                    if millisecs = 0 then preemptive scheduling is turned off.
*)

PROCEDURE initPreemptive (seconds, microsecs: CARDINAL) ;
BEGIN
   timeSliceUsec := microsecs ;
   timeSliceSec := seconds ;
   IF NOT init
   THEN
      init := TRUE ;
      Create (timer, extraWorkSpace, MAX (Urgency), NIL, timerId) ;
      Activate (timerId)
   END
END initPreemptive ;


VAR
   init        : BOOLEAN ;
   timerId     : ProcessId ;
   timeSliceSec,
   timeSliceUsec: CARDINAL ;
BEGIN
   init := FALSE ;
   timeSliceSec := 0 ;
   timeSliceUsec := 0
END Preemptive.
