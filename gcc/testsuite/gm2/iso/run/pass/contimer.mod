(* contimer.mod a basic thread test for round robin scheduling.

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

MODULE contimer ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Processes IMPORT Create, Start, StopMe, SuspendMe, Activate, SuspendMeAndActivate, ProcessId, Me, Reschedule ;
FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT ADDRESS ;
FROM Preemptive IMPORT initPreemptive ;
FROM COROUTINES IMPORT LISTEN, PROTECTION ;

CONST
   maxProcesses    = 10 ;
   stackSpace      = 1 * 1024 * 1024 ;

VAR
   processes: ARRAY [1..maxProcesses] OF ProcessId ;


(*
   createThreads -
*)

PROCEDURE createThreads ;
VAR
   t: INTEGER ;
BEGIN
   FOR t := 1 TO maxProcesses DO
      Create (simpleProcess, stackSpace, -1, NIL, processes[t])
   END
END createThreads ;


(*
   simpleProcess -
*)

PROCEDURE simpleProcess ;
VAR
   count: CARDINAL ;
BEGIN
   count := 0 ;
   LOOP
      printf ("%x:  hello world\n", Me ()) ;
      (* LISTEN (MIN (PROTECTION)) ; *)
      Reschedule ;
      INC (count) ;
      IF count = 1000
      THEN
         printf ("all done - finishing\n");
         exit (0)
      END
   END
END simpleProcess ;


PROCEDURE parallelRun ;
VAR
   c: CARDINAL ;
BEGIN
   printf ("initPreemptive\n") ;
   initPreemptive (1, 0) ;
   printf ("after initPreemptive\n") ;
   FOR c := 1 TO maxProcesses DO
      Activate (processes[c])
   END ;
   printf ("all complete\n") ;
   SuspendMe
END parallelRun ;


BEGIN
   printf ("starting contimer test\n") ;
   createThreads ;
   printf ("running all threads\n") ;
   parallelRun
END contimer.
