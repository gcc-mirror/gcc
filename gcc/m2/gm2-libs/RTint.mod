(* RTint.mod provides users of the COROUTINES library with the.

Copyright (C) 2009-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE RTint ;


FROM M2RTS IMPORT Halt ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM RTco IMPORT select, initSemaphore, wait, signal ;
FROM COROUTINES IMPORT PROTECTION ;
FROM libc IMPORT printf, perror ;
FROM Assertion IMPORT Assert ;

FROM Selective IMPORT InitSet, FdSet, Timeval, InitTime, KillTime, KillSet,
                      SetOfFd, FdIsSet, GetTime, FdZero, GetTimeOfDay, SetTime,
                      FdClr;

CONST
   Microseconds = 1000000 ;
   DebugTime    = 0 ;
   Debugging    = FALSE ;

TYPE
   VectorType = (input, output, time) ;
   Vector     = POINTER TO RECORD
                              type    : VectorType ;
                              priority: CARDINAL ;
                              arg     : ADDRESS ;
                              pending,
                              exists  : Vector ;
                              no      : CARDINAL ;
                              File    : INTEGER ;
                              rel,
                              abs     : Timeval ;
                              queued  : BOOLEAN ;
                           END ;

VAR
   VecNo      : CARDINAL ;
   Exists     : Vector ;
   Pending    : ARRAY [MIN(PROTECTION)..MAX(PROTECTION)] OF Vector ;
   lock       : INTEGER ;
   initialized: BOOLEAN ;


(*
   Max - returns the maximum: i or j.
*)

PROCEDURE Max (i, j: INTEGER) : INTEGER ;
BEGIN
   IF i>j
   THEN
      RETURN i
   ELSE
      RETURN j
   END
END Max ;


(*
   Max - returns the minimum: i or j.
*)

PROCEDURE Min (i, j: INTEGER) : INTEGER ;
BEGIN
   IF i<j
   THEN
      RETURN i
   ELSE
      RETURN j
   END
END Min ;


(*
   FindVector - searches the exists list for a vector of type, t,
                which is associated with file descriptor, fd.
*)

PROCEDURE FindVector (fd: INTEGER; t: VectorType) : Vector ;
VAR
   v: Vector ;
BEGIN
   v := Exists ;
   WHILE v#NIL DO
      IF (v^.type=t) AND (v^.File=fd)
      THEN
         RETURN v
      END ;
      v := v^.exists
   END ;
   RETURN NIL
END FindVector ;


(*
   InitInputVector - returns an interrupt vector which is associated
                     with the file descriptor, fd.
*)

PROCEDURE InitInputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;
VAR
   v: Vector ;
BEGIN
   IF Debugging
   THEN
      printf("InitInputVector fd = %d priority = %d\n", fd, pri)
   END ;
   wait (lock) ;
   v := FindVector(fd, input) ;
   IF v=NIL
   THEN
      NEW(v) ;
      INC(VecNo) ;
      WITH v^ DO
         type     := input ;
         priority := pri ;
         arg      := NIL ;
         pending  := NIL ;
         exists   := Exists ;
         no       := VecNo ;
         File     := fd
      END ;
      Exists := v ;
      signal (lock) ;
      RETURN VecNo
   ELSE
      signal (lock) ;
      RETURN v^.no
   END
END InitInputVector ;


(*
   InitOutputVector - returns an interrupt vector which is associated
                      with the file descriptor, fd.
*)

PROCEDURE InitOutputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;
VAR
   v: Vector ;
BEGIN
   wait (lock) ;
   v := FindVector (fd, output) ;
   IF v=NIL
   THEN
      NEW (v) ;
      IF v = NIL
      THEN
         HALT
      ELSE
         INC (VecNo) ;
         WITH v^ DO
            type     := output ;
            priority := pri ;
            arg      := NIL ;
            pending  := NIL ;
            exists   := Exists ;
            no       := VecNo ;
            File     := fd
         END ;
         Exists := v ;
         signal (lock) ;
         RETURN VecNo
      END
   ELSE
      signal (lock) ;
      RETURN v^.no
   END
END InitOutputVector ;


(*
   InitTimeVector - returns an interrupt vector associated with
                    the relative time.
*)

PROCEDURE InitTimeVector (micro, secs: CARDINAL; pri: CARDINAL) : CARDINAL ;
VAR
   v: Vector ;
BEGIN
   wait (lock) ;
   NEW (v) ;
   IF v = NIL
   THEN
      HALT
   ELSE
      INC (VecNo) ;
      Assert (micro<Microseconds) ;
      WITH v^ DO
         type     := time ;
         priority := pri ;
         arg      := NIL ;
         pending  := NIL ;
         exists   := Exists ;
         no       := VecNo ;
         rel      := InitTime(secs+DebugTime, micro) ;
         abs      := InitTime(0, 0) ;
         queued   := FALSE
      END ;
      Exists := v
   END ;
   signal (lock) ;
   RETURN VecNo
END InitTimeVector ;


(*
   FindVectorNo - searches the Exists list for vector, vec.
*)

PROCEDURE FindVectorNo (vec: CARDINAL) : Vector ;
VAR
   v: Vector ;
BEGIN
   v := Exists ;
   WHILE (v#NIL) AND (v^.no#vec) DO
      v := v^.exists
   END ;
   RETURN v
END FindVectorNo ;


(*
   FindPendingVector - searches the pending list for vector, vec.
*)

PROCEDURE FindPendingVector (vec: CARDINAL) : Vector ;
VAR
   i: CARDINAL ;
   v: Vector ;
BEGIN
   FOR i := MIN(PROTECTION) TO MAX(PROTECTION) DO
      v := Pending[i] ;
      WHILE (v#NIL) AND (v^.no#vec) DO
         v := v^.pending
      END ;
      IF (v#NIL) AND (v^.no=vec)
      THEN
         RETURN v
      END
   END ;
   RETURN NIL
END FindPendingVector ;


(*
   ReArmTimeVector - reprimes the vector, vec, to deliver an interrupt
                     at the new relative time.
*)

PROCEDURE ReArmTimeVector (vec: CARDINAL;
                           micro, secs: CARDINAL) ;
VAR
   v: Vector ;
BEGIN
   Assert(micro<Microseconds) ;
   wait (lock) ;
   v := FindVectorNo(vec) ;
   IF v=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'cannot find vector supplied')
   ELSE
      WITH v^ DO
         SetTime (rel, secs + DebugTime, micro)
      END
   END ;
   signal (lock)
END ReArmTimeVector ;


(*
   GetTimeVector - assigns, micro, and, secs, with the remaining
                   time before this interrupt will expire.
                   This value is only updated when a Listen
                   occurs.
*)

PROCEDURE GetTimeVector (vec: CARDINAL; VAR micro, secs: CARDINAL) ;
VAR
   v: Vector ;
BEGIN
   wait (lock) ;
   v := FindVectorNo (vec) ;
   IF v=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'cannot find vector supplied')
   ELSE
      WITH v^ DO
         GetTime (rel, secs, micro) ;
         Assert (micro < Microseconds)
      END
   END ;
   signal (lock)
END GetTimeVector ;


(*
   AttachVector - adds the pointer, p, to be associated with the interrupt
                  vector. It returns the previous value attached to this
                  vector.
*)

PROCEDURE AttachVector (vec: CARDINAL; p: ADDRESS) : ADDRESS ;
VAR
   v: Vector ;
   l: ADDRESS ;
BEGIN
   wait (lock) ;
   v := FindVectorNo (vec) ;
   IF v=NIL
   THEN
      Halt (__FILE__, __LINE__, __FUNCTION__, 'cannot find vector supplied')
   ELSE
      l := v^.arg ;
      v^.arg := p ;
      IF Debugging
      THEN
         printf ("AttachVector %d with 0x%x\n", vec, p);
         DumpPendingQueue ;
      END ;
      signal (lock) ;
      RETURN l
   END
END AttachVector ;


(*
   IncludeVector - includes, vec, into the dispatcher list of
                   possible interrupt causes.
*)

PROCEDURE IncludeVector (vec: CARDINAL) ;
VAR
   v   : Vector ;
   m, s: CARDINAL ;
   r   : INTEGER ;
BEGIN
   wait (lock) ;
   v := FindPendingVector (vec) ;
   IF v=NIL
   THEN
      v := FindVectorNo (vec) ;
      IF v = NIL
      THEN
         Halt (__FILE__, __LINE__, __FUNCTION__,
               'cannot find vector supplied') ;
      ELSE
         (* printf('including vector %d  (fd = %d)\n', vec, v^.File) ; *)
         v^.pending := Pending[v^.priority] ;
         Pending[v^.priority] := v ;
         IF (v^.type = time) AND (NOT v^.queued)
         THEN
            v^.queued := TRUE ;
            r := GetTimeOfDay (v^.abs) ;
            Assert (r=0) ;
            GetTime (v^.abs, s, m) ;
            Assert (m<Microseconds) ;
            AddTime (v^.abs, v^.rel) ;
            GetTime (v^.abs, s, m) ;
            Assert (m<Microseconds)
         END
      END
   ELSE
      IF Debugging
      THEN
         printf ('odd vector (%d) type (%d) arg (0x%x) is already attached to the pending queue\n',
                 vec, v^.type, v^.arg)
      END ;
      stop
   END ;
   signal (lock)
END IncludeVector ;


(*
   ExcludeVector - excludes, vec, from the dispatcher list of
                   possible interrupt causes.
*)

PROCEDURE ExcludeVector (vec: CARDINAL) ;
VAR
   v, u: Vector ;
BEGIN
   wait (lock) ;
   v := FindPendingVector(vec) ;
   IF v=NIL
   THEN
      Halt (__FILE__, __LINE__, __FUNCTION__,
            'cannot find pending vector supplied')
   ELSE
      (* printf('excluding vector %d\n', vec) ; *)
      IF Pending[v^.priority]=v
      THEN
         Pending[v^.priority] := Pending[v^.priority]^.pending
      ELSE
         u := Pending[v^.priority] ;
         WHILE u^.pending#v DO
            u := u^.pending
         END ;
         u^.pending := v^.pending
      END ;
      IF v^.type=time
      THEN
         v^.queued := FALSE
      END
   END ;
   signal (lock)
END ExcludeVector ;


(*
   AddFd - adds the file descriptor, fd, to set, s, updating, max.
*)

PROCEDURE AddFd (VAR s: SetOfFd; VAR max: INTEGER; fd: INTEGER) ;
BEGIN
   max := Max (fd, max) ;
   IF s = NIL
   THEN
      s := InitSet () ;
      FdZero (s)
   END ;
   FdSet (fd, s)
   (* printf('%d, ', fd) *)
END AddFd ;


(*
   DumpPendingQueue - displays the pending queue.
*)

PROCEDURE DumpPendingQueue ;
VAR
   p   : PROTECTION ;
   v   : Vector ;
   s, m: CARDINAL ;
BEGIN
   printf ("Pending queue\n");
   FOR p := MIN (PROTECTION) TO MAX (PROTECTION) DO
      printf ("[%d]  ", p);
      v := Pending[p] ;
      WHILE v#NIL DO
         IF (v^.type=input) OR (v^.type=output)
         THEN
            printf ("(fd=%d) (vec=%d)", v^.File, v^.no)
         ELSIF v^.type=time
         THEN
            GetTime(v^.rel, s, m) ;
            Assert (m<Microseconds) ;
            printf ("time (%u.%06u secs) (arg = 0x%x)\n", s, m, v^.arg)
         END ;
         v := v^.pending
      END ;
      printf (" \n")
   END
END DumpPendingQueue ;


PROCEDURE stop ;
BEGIN
END stop ;


(*
   AddTime - t1 := t1 + t2
*)

PROCEDURE AddTime (t1, t2: Timeval) ;
VAR
   a, b, s, m: CARDINAL ;
BEGIN
   GetTime (t1, s, m) ;
   Assert (m < Microseconds) ;
   GetTime (t2, a, b) ;
   Assert (b < Microseconds) ;
   INC (a, s) ;
   INC (b, m) ;
   IF b >= Microseconds
   THEN
      DEC (b, Microseconds) ;
      INC (a)
   END ;
   SetTime (t1, a, b)
END AddTime ;


(*
   IsGreaterEqual - returns TRUE if, a>=b
*)

PROCEDURE IsGreaterEqual (a, b: Timeval) : BOOLEAN ;
VAR
   as, am, bs, bm: CARDINAL ;
BEGIN
   GetTime (a, as, am) ;
   Assert (am < Microseconds) ;
   GetTime (b, bs, bm) ;
   Assert (bm < Microseconds) ;
   RETURN (as > bs) OR ((as = bs) AND (am >= bm))
END IsGreaterEqual ;


(*
   SubTime - assigns, s and m, to a - b.
*)

PROCEDURE SubTime (VAR s, m: CARDINAL; a, b: Timeval) ;
VAR
   as, am,
   bs, bm: CARDINAL ;
BEGIN
   GetTime (a, as, am) ;
   Assert (am < Microseconds) ;
   GetTime (b, bs, bm) ;
   Assert (bm < Microseconds) ;
   IF IsGreaterEqual (a, b)
   THEN
      s := as - bs ;
      IF am >= bm
      THEN
         m := am - bm ;
         Assert (m < Microseconds) ;
      ELSE
         Assert (s > 0) ;
         DEC (s) ;
         m := (Microseconds + am) - bm ;
         Assert (m < Microseconds)
      END
   ELSE
      s := 0 ;
      m := 0
   END
END SubTime ;


(*
   activatePending - activates the first interrupt pending and clears it.
*)

PROCEDURE activatePending (untilInterrupt: BOOLEAN; call: DispatchVector; pri: CARDINAL;
                           maxFd: INTEGER; VAR i, o: SetOfFd; VAR t: Timeval; b4, after: Timeval) : BOOLEAN ;
VAR
   r   : INTEGER ;
   p   : CARDINAL ;
   v   : Vector ;
   b4s,
   b4m,
   afs,
   afm,
   s, m: CARDINAL ;
BEGIN
   wait (lock) ;
   p := MAX (PROTECTION) ;
   WHILE p > pri DO
      v := Pending[p] ;
      WHILE v # NIL DO
         WITH v^ DO
            CASE type OF

            input :  IF (File < maxFd) AND (i # NIL) AND FdIsSet (File, i)
                     THEN
                        IF Debugging
                        THEN
                           printf ('read (fd=%d) is ready (vec=%d)\n', File, no) ;
                           DumpPendingQueue
                        END ;
                        FdClr (File, i) ;  (* so we dont activate this again from our select.  *)
                        signal (lock) ;
                        call (no, priority, arg) ;
                        RETURN TRUE
                     END |
            output:  IF (File < maxFd) AND (o#NIL) AND FdIsSet (File, o)
                     THEN
                        IF Debugging
                        THEN
                           printf ('write (fd=%d) is ready (vec=%d)\n', File, no) ;
                           DumpPendingQueue
                        END ;
                        FdClr (File, o) ;  (* so we dont activate this again from our select.  *)
                        signal (lock) ;
                        call (no, priority, arg) ;
                        RETURN TRUE
                     END |
            time  :  IF untilInterrupt AND (t # NIL)
                     THEN
                        r := GetTimeOfDay (after) ;
                        Assert (r=0) ;
                        IF Debugging
                        THEN
                           GetTime (t, s, m) ;
                           Assert (m < Microseconds) ;
                           GetTime (after, afs, afm) ;
                           Assert (afm < Microseconds) ;
                           GetTime (b4, b4s, b4m) ;
                           Assert (b4m < Microseconds) ;
                           printf ("waited %u.%06u + %u.%06u now is %u.%06u\n",
                                   s, m, b4s, b4m, afs, afm) ;
                        END ;
                        IF IsGreaterEqual (after, abs)
                        THEN
                           IF Debugging
                           THEN
                              DumpPendingQueue ;
                              printf ("time has expired calling dispatcher\n")
                           END ;
                           t := KillTime (t) ;  (* so we dont activate this again from our select.  *)
                           signal (lock) ;
                           IF Debugging
                           THEN
                              printf ("call (%d, %d, 0x%x)\n", no, priority, arg)
                           END ;
                           call (no, priority, arg) ;
                           RETURN TRUE
                        ELSIF Debugging
                        THEN
                           printf ("must wait longer as time has not expired\n")
                        END
                     END
            END
         END ;
         v := v^.pending
      END ;
      DEC (p)
   END ;
   signal (lock) ;
   RETURN FALSE
END activatePending ;


(*
   Listen - will either block indefinitely (until an interrupt)
            or alteratively will test to see whether any interrupts
            are pending.
            If a pending interrupt was found then, call, is called
            and then this procedure returns.
            It only listens for interrupts > pri.
*)

PROCEDURE Listen (untilInterrupt: BOOLEAN;
                  call: DispatchVector;
                  pri: CARDINAL) ;
VAR
   found: BOOLEAN ;
   r    : INTEGER ;
   after,
   b4,
   t    : Timeval ;
   v    : Vector ;
   i, o : SetOfFd ;
   b4s,
   b4m,
   afs,
   afm,
   s, m : CARDINAL ;
   maxFd: INTEGER ;
   p    : CARDINAL ;
BEGIN
   wait (lock) ;
   IF pri < MAX (PROTECTION)
   THEN
      IF Debugging
      THEN
         DumpPendingQueue
      END ;
      maxFd := -1 ;
      t := NIL ;
      i := NIL ;
      o := NIL ;
      t := InitTime (MAX (INTEGER), 0) ;
      p := MAX (PROTECTION) ;
      found := FALSE ;
      WHILE p>pri DO
         v := Pending[p] ;
         WHILE v#NIL DO
            WITH v^ DO
               CASE type OF

               input :  AddFd (i, maxFd, File) |
               output:  AddFd (o, maxFd, File) |
               time  :  IF IsGreaterEqual (t, abs)
                        THEN
                           GetTime (abs, s, m) ;
                           Assert (m<Microseconds) ;
                           IF Debugging
                           THEN
                              printf ("shortest delay is %u.%06u\n", s, m)
                           END ;
                           SetTime (t, s, m) ;
                           found := TRUE
                        END

               END
            END ;
            v := v^.pending
         END ;
         DEC (p)
      END ;
      IF NOT untilInterrupt
      THEN
         SetTime (t, 0, 0)
      END ;
      IF untilInterrupt AND (i=NIL) AND (o=NIL) AND (NOT found)
      THEN
         Halt (__FILE__, __LINE__, __FUNCTION__,
               'deadlock found, no more processes to run and no interrupts active')
      END ;
      (* printf('timeval = 0x%x\n', t) ; *)
      (* printf('}\n') ; *)
      IF (NOT found) AND (maxFd=-1) AND (i=NIL) AND (o=NIL)
      THEN
         (* no file descriptors to be selected upon.  *)
         t := KillTime (t) ;
         signal (lock) ;
         RETURN
      ELSE
         GetTime (t, s, m) ;
         Assert (m<Microseconds) ;
         b4 := InitTime (0, 0) ;
         after := InitTime (0, 0) ;
         r := GetTimeOfDay (b4) ;
         Assert (r=0) ;
         SubTime (s, m, t, b4) ;
         SetTime (t, s, m) ;
         IF Debugging
         THEN
            printf ("select waiting for %u.%06u seconds\n", s, m)
         END ;
         signal (lock) ;
         REPEAT
            IF Debugging
            THEN
               printf ("select (.., .., .., %u.%06u)\n", s, m)
            END ;
            r := select (maxFd+1, i, o, NIL, t) ;
            IF r=-1
            THEN
               perror ("select") ;
               r := select (maxFd+1, i, o, NIL, NIL) ;
               IF r=-1
               THEN
                  perror ("select timeout argument is faulty")
               END ;
               r := select (maxFd+1, i, NIL, NIL, t) ;
               IF r=-1
               THEN
                  perror ("select output fd argument is faulty")
               END ;
               r := select (maxFd+1, NIL, o, NIL, t) ;
               IF r=-1
               THEN
                  perror ("select input fd argument is faulty")
               ELSE
                  perror ("select maxFD+1 argument is faulty")
               END
            END
         UNTIL r#-1
      END ;
      WHILE activatePending (untilInterrupt, call, pri,
                             maxFd+1, i, o, t, b4, after) DO
      END ;
      IF t#NIL
      THEN
         t := KillTime (t)
      END ;
      IF after#NIL
      THEN
         t := KillTime (after)
      END ;
      IF b4#NIL
      THEN
         t := KillTime (b4)
      END ;
      IF i#NIL
      THEN
         i := KillSet (i)
      END ;
      IF o#NIL
      THEN
         o := KillSet (o)
      END
   END ;
   signal (lock)
END Listen ;


(*
   init -
*)

PROCEDURE init ;
VAR
   p: PROTECTION ;
BEGIN
   lock := initSemaphore (1) ;
   wait (lock) ;
   Exists := NIL ;
   FOR p := MIN(PROTECTION) TO MAX(PROTECTION) DO
      Pending[p] := NIL
   END ;
   initialized := TRUE ;
   signal (lock)
END init ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   IF NOT initialized
   THEN
      init
   END
END Init ;


BEGIN
   Init
END RTint.
