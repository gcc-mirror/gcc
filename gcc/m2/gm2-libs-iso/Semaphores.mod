(* Semaphores.mod implement the ISO Semaphores specification.

Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Semaphores ;

(* Provides mutual exclusion facilities for use by processes. *)

FROM Storage IMPORT ALLOCATE ;
FROM Processes IMPORT ProcessId, Me, SuspendMe, Activate, UrgencyOf ;


TYPE
   SEMAPHORE = POINTER TO RECORD
                             value: CARDINAL ;
                             next : SEMAPHORE ;
                             head : ProcessList ;
                          END ;

   ProcessList = POINTER TO RECORD
                               waiting: ProcessId ;
                               right,
                               left   : ProcessList ;
                            END ;

VAR
   freeSem        :  SEMAPHORE ;
   freeProcessList:  ProcessList ;


(*
   Create - creates and returns s as the identity of a new
            semaphore that has its associated count initialized
            to initialCount, and has no processes yet waiting on it.
*)

PROCEDURE Create (VAR s: SEMAPHORE; initialCount: CARDINAL) ;
BEGIN
   s := newSemaphore () ;
   WITH s^ DO
      value := initialCount ;
      next  := NIL ;
      head  := NIL
   END
END Create ;


(*
   Destroy - recovers the resources used to implement the semaphore s,
             provided that no process is waiting for s to become free.
*)

PROCEDURE Destroy (VAR s: SEMAPHORE) ;
BEGIN
   WITH s^ DO
      IF head=NIL
      THEN
         next := freeSem ;
         freeSem := s
      ELSE
         (* raise exception? *)
      END
   END
END Destroy ;


(*
   newSemaphore -
*)

PROCEDURE newSemaphore () : SEMAPHORE ;
VAR
   s: SEMAPHORE ;
BEGIN
   IF freeSem=NIL
   THEN
      NEW (s)
   ELSE
      s := freeSem ;
      freeSem := freeSem^.next
   END ;
   RETURN s
END newSemaphore ;


(*
   newProcessList - returns a new ProcessList.
*)

PROCEDURE newProcessList () : ProcessList ;
VAR
   l: ProcessList ;
BEGIN
   IF freeProcessList=NIL
   THEN
      NEW (l)
   ELSE
      l := freeProcessList ;
      freeProcessList := freeProcessList^.right
   END ;
   RETURN l
END newProcessList ;


(*
   add - adds process, p, to queue, head.
*)

PROCEDURE add (VAR head: ProcessList; p: ProcessList) ;
BEGIN
   IF head=NIL
   THEN
      head := p ;
      p^.left := p ;
      p^.right := p
   ELSE
      p^.right := head ;
      p^.left := head^.left ;
      head^.left^.right := p ;
      head^.left := p
   END
END add ;


(*
   sub - subtracts process, p, from queue, head.
*)

PROCEDURE sub (VAR head: ProcessList; p: ProcessList) ;
BEGIN
   IF (p^.left=head) AND (p=head)
   THEN
      head := NIL
   ELSE
      IF head=p
      THEN
         head := head^.right
      END ;
      p^.left^.right := p^.right ;
      p^.right^.left := p^.left
   END
END sub ;


(*
   addProcess - adds the current process to the semaphore list.
                Remove the current process from the ready queue.
*)

PROCEDURE addProcess (VAR head: ProcessList) ;
VAR
   l: ProcessList ;
BEGIN
   l := newProcessList() ;
   WITH l^ DO
      waiting := Me () ;
      right := NIL ;
      left := NIL
   END ;
   add (head, l) ;
   SuspendMe
END addProcess ;


(*
   chooseProcess -
*)

PROCEDURE chooseProcess (head: ProcessList) : ProcessList ;
VAR
   best, l: ProcessList ;
BEGIN
   best := head ;
   l := head^.right ;
   WHILE l#head DO
      IF UrgencyOf (l^.waiting) > UrgencyOf (best^.waiting)
      THEN
         best := l
      END ;
      l := l^.right
   END ;
   RETURN best
END chooseProcess ;


(*
   removeProcess - removes process, l, from the list and adds it to the
                   ready queue.
*)

PROCEDURE removeProcess (VAR head: ProcessList; l: ProcessList) ;
BEGIN
   sub (head, l) ;
   WITH l^ DO
      right := freeProcessList ;
      freeProcessList := l ;
      Activate (waiting)
   END
END removeProcess ;


(*
   Claim - if the count associated with the semaphore s is non-zero,
           decrements this count and allows the calling process to
           continue; otherwise suspends the calling process until
           s is released.
*)

PROCEDURE Claim (s: SEMAPHORE) ;
BEGIN
   WITH s^ DO
      IF value>0
      THEN
         DEC (value)
      ELSE
         addProcess (head)
      END
   END
END Claim ;


(*
   Release - if there are any processes waiting on the semaphore s,
             allows one of them to enter the ready state; otherwise
             increments the count associated with s.
*)

PROCEDURE Release (s: SEMAPHORE) ;
BEGIN
   WITH s^ DO
      IF head=NIL
      THEN
         INC (value)
      ELSE
         removeProcess (head, chooseProcess (head))
      END
   END
END Release ;


(*
   CondClaim - returns FALSE if the call Claim(s) would cause the calling
               process to be suspended; in this case the count associated
               with s is not changed. Otherwise returns TRUE and the
               associated count is decremented.
*)

PROCEDURE CondClaim (s: SEMAPHORE) : BOOLEAN ;
BEGIN
   WITH s^ DO
      IF value>0
      THEN
         DEC (value) ;
         RETURN TRUE
      ELSE
         RETURN FALSE
      END
   END
END CondClaim ;


BEGIN
   freeSem := NIL ;
   freeProcessList := NIL
END Semaphores.
