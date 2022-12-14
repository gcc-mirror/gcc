(* concurrentstore.mod a concurrent test for ALLOCATE/DEALLOCATE.

Copyright (C) 2020 Free Software Foundation, Inc.
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

MODULE concurrentstore ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Processes IMPORT Create, Start, StopMe, SuspendMe, Activate, SuspendMeAndActivate, ProcessId, Me, Reschedule ;
FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT ADDRESS ;
FROM Preemptive IMPORT initPreemptive ;
FROM Processes IMPORT Wait, Attach, Create, ProcessId, Urgency, Activate ;
FROM RTint IMPORT InitTimeVector, ReArmTimeVector, IncludeVector, ExcludeVector ;
FROM COROUTINES IMPORT TurnInterrupts, PROTECTION ;


CONST
   maxProcesses    = 5 ;
   maxStorageItems = 10 ;
   stackSpace      = 1 * 1024 * 1024 ;
   maxTests        = 10 ;

VAR
   processes: ARRAY [1..maxProcesses] OF ProcessId ;
   heap     : ARRAY [1..maxProcesses] OF ARRAY [1..maxStorageItems] OF ADDRESS ;


(*
   createThreads -
*)

PROCEDURE createThreads ;
VAR
   t: INTEGER ;
BEGIN
   FOR t := 1 TO maxProcesses DO
      Create (stressAllocation, stackSpace, -1, NIL, processes[t])
   END
END createThreads ;


(*
   stressAllocation -
*)

PROCEDURE stressAllocation ;
VAR
   looping,
   count,
   a, tid : CARDINAL ;
BEGIN
   tid := Find (Me ()) ;
   count := 0 ;
   looping := 0 ;
   LOOP
      FOR a := 1 TO maxStorageItems DO
         ALLOCATE (heap[tid][a], a)
      END ;
      FOR a := 1 TO maxStorageItems DO
         DEALLOCATE (heap[tid][a], a)
      END ;
      INC (count) ;
      (* Reschedule ; *)
      IF count = maxTests
      THEN
         printf ("process %d completed %d allocate/deallocates\n",
                 tid, count * maxTests) ;
         count := 0 ;
         INC (looping) ;
         IF looping = 10
         THEN
            printf ("this thread is complete\n") ;
            SuspendMe
         END
      END
   END
END stressAllocation ;


(*
   Find -
*)

PROCEDURE Find (id: ProcessId) : CARDINAL ;
VAR
   c: CARDINAL ;
BEGIN
   FOR c := 1 TO maxProcesses DO
      IF processes[c] = id
      THEN
         RETURN c
      END
   END ;
   HALT
END Find ;


(*
   timedWait -
*)

PROCEDURE timedWait ;
VAR
   old: PROTECTION ;
   vec: CARDINAL ;
BEGIN
   printf ("timedWait for 5 seconds\n");
   old := TurnInterrupts (MAX (PROTECTION)) ;
   vec := InitTimeVector (5, 5, MAX (PROTECTION)) ;
   Attach (vec) ;  (* attach vector to this process.  *)
   IncludeVector (vec) ;
   (* ReArmTimeVector (vec, 3, 3) ;  (* 10 seconds.  *) *)
   printf ("main process is now going Wait for 5 seconds\n");
   Wait ;
   printf ("yes, 5 seconds has elapsed\n");
END timedWait ;


PROCEDURE parallelRun ;
VAR
   c: CARDINAL ;
BEGIN
   printf ("initPreemptive\n") ;
   initPreemptive (2, 0) ;
   printf ("after initPreemptive\n") ;

   FOR c := 1 TO maxProcesses DO
      Activate (processes[c])
   END ;
   timedWait ;
   printf ("all complete\n") ;
END parallelRun ;


BEGIN
   printf ("starting concurrentstore test\n") ;
   createThreads ;
   printf ("running all threads\n") ;
   parallelRun
END concurrentstore.
