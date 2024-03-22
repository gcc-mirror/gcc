(* RTint.mod provides users of the COROUTINES library with the.

Copyright (C) 2009-2024 Free Software Foundation, Inc.
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
   FindVector - searches the exists list for a vector of type
                which is associated with file descriptor, fd.
*)

PROCEDURE FindVector (fd: INTEGER; type: VectorType) : Vector ;
VAR
   vec: Vector ;
BEGIN
   vec := Exists ;
   WHILE vec#NIL DO
      IF (vec^.type=type) AND (vec^.File=fd)
      THEN
         RETURN vec
      END ;
      vec := vec^.exists
   END ;
   RETURN NIL
END FindVector ;


(*
   InitInputVector - returns an interrupt vector which is associated
                     with the file descriptor, fd.
*)

PROCEDURE InitInputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;
VAR
   vptr: Vector ;
BEGIN
   IF Debugging
   THEN
      printf("InitInputVector fd = %d priority = %d\n", fd, pri)
   END ;
   wait (lock) ;
   vptr := FindVector(fd, input) ;
   IF vptr = NIL
   THEN
      NEW (vptr) ;
      INC (VecNo) ;
      WITH vptr^ DO
         type     := input ;
         priority := pri ;
         arg      := NIL ;
         pending  := NIL ;
         exists   := Exists ;
         no       := VecNo ;
         File     := fd
      END ;
      Exists := vptr ;
      signal (lock) ;
      RETURN VecNo
   ELSE
      signal (lock) ;
      RETURN vptr^.no
   END
END InitInputVector ;


(*
   InitOutputVector - returns an interrupt vector which is associated
                      with the file descriptor, fd.
*)

PROCEDURE InitOutputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;
VAR
   vptr: Vector ;
BEGIN
   wait (lock) ;
   vptr := FindVector (fd, output) ;
   IF vptr = NIL
   THEN
      NEW (vptr) ;
      IF vptr = NIL
      THEN
         HALT
      ELSE
         INC (VecNo) ;
         WITH vptr^ DO
            type     := output ;
            priority := pri ;
            arg      := NIL ;
            pending  := NIL ;
            exists   := Exists ;
            no       := VecNo ;
            File     := fd
         END ;
         Exists := vptr ;
         signal (lock) ;
         RETURN VecNo
      END
   ELSE
      signal (lock) ;
      RETURN vptr^.no
   END
END InitOutputVector ;


(*
   InitTimeVector - returns an interrupt vector associated with
                    the relative time.
*)

PROCEDURE InitTimeVector (micro, secs: CARDINAL; pri: CARDINAL) : CARDINAL ;
VAR
   vptr: Vector ;
BEGIN
   wait (lock) ;
   NEW (vptr) ;
   IF vptr = NIL
   THEN
      HALT
   ELSE
      INC (VecNo) ;
      Assert (micro<Microseconds) ;
      WITH vptr^ DO
         type     := time ;
         priority := pri ;
         arg      := NIL ;
         pending  := NIL ;
         exists   := Exists ;
         no       := VecNo ;
         rel      := InitTime (secs+DebugTime, micro) ;
         abs      := InitTime (0, 0) ;
         queued   := FALSE
      END ;
      Exists := vptr
   END ;
   signal (lock) ;
   RETURN VecNo
END InitTimeVector ;


(*
   FindVectorNo - searches the Exists list for vector vec.
*)

PROCEDURE FindVectorNo (vec: CARDINAL) : Vector ;
VAR
   vptr: Vector ;
BEGIN
   vptr := Exists ;
   WHILE (vptr#NIL) AND (vptr^.no#vec) DO
      vptr := vptr^.exists
   END ;
   RETURN vptr
END FindVectorNo ;


(*
   FindPendingVector - searches the pending list for vector, vec.
*)

PROCEDURE FindPendingVector (vec: CARDINAL) : Vector ;
VAR
   pri : CARDINAL ;
   vptr: Vector ;
BEGIN
   FOR pri := MIN(PROTECTION) TO MAX(PROTECTION) DO
      vptr := Pending[pri] ;
      WHILE (vptr#NIL) AND (vptr^.no#vec) DO
         vptr := vptr^.pending
      END ;
      IF (vptr#NIL) AND (vptr^.no=vec)
      THEN
         RETURN vptr
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
   vptr: Vector ;
BEGIN
   Assert (micro<Microseconds) ;
   wait (lock) ;
   vptr := FindVectorNo (vec) ;
   IF vptr = NIL
   THEN
      Halt ('cannot find vector supplied',
            __FILE__, __FUNCTION__, __LINE__)
   ELSE
      WITH vptr^ DO
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
   vptr: Vector ;
BEGIN
   wait (lock) ;
   vptr := FindVectorNo (vec) ;
   IF vptr=NIL
   THEN
      Halt ('cannot find vector supplied',
            __FILE__, __FUNCTION__, __LINE__)
   ELSE
      WITH vptr^ DO
         GetTime (rel, secs, micro) ;
         Assert (micro < Microseconds)
      END
   END ;
   signal (lock)
END GetTimeVector ;


(*
   AttachVector - adds the pointer ptr to be associated with the interrupt
                  vector. It returns the previous value attached to this
                  vector.
*)

PROCEDURE AttachVector (vec: CARDINAL; ptr: ADDRESS) : ADDRESS ;
VAR
   vptr   : Vector ;
   prevArg: ADDRESS ;
BEGIN
   wait (lock) ;
   vptr := FindVectorNo (vec) ;
   IF vptr = NIL
   THEN
      Halt ( 'cannot find vector supplied',
            __FILE__, __FUNCTION__, __LINE__)
   ELSE
      prevArg := vptr^.arg ;
      vptr^.arg := ptr ;
      IF Debugging
      THEN
         printf ("AttachVector %d with %p\n", vec, ptr);
         DumpPendingQueue ;
      END ;
      signal (lock) ;
      RETURN prevArg
   END
END AttachVector ;


(*
   IncludeVector - includes, vec, into the dispatcher list of
                   possible interrupt causes.
*)

PROCEDURE IncludeVector (vec: CARDINAL) ;
VAR
   vptr      : Vector ;
   micro, sec: CARDINAL ;
   result    : INTEGER ;
BEGIN
   wait (lock) ;
   vptr := FindPendingVector (vec) ;
   IF vptr = NIL
   THEN
      vptr := FindVectorNo (vec) ;
      IF vptr = NIL
      THEN
         Halt ('cannot find vector supplied',
               __FILE__, __FUNCTION__, __LINE__)
      ELSE
         (* printf('including vector %d  (fd = %d)\n', vec, v^.File) ; *)
         vptr^.pending := Pending[vptr^.priority] ;
         Pending[vptr^.priority] := vptr ;
         IF (vptr^.type = time) AND (NOT vptr^.queued)
         THEN
            vptr^.queued := TRUE ;
            result := GetTimeOfDay (vptr^.abs) ;
            Assert (result=0) ;
            GetTime (vptr^.abs, sec, micro) ;
            Assert (micro<Microseconds) ;
            AddTime (vptr^.abs, vptr^.rel) ;
            GetTime (vptr^.abs, sec, micro) ;
            Assert (micro<Microseconds)
         END
      END
   ELSE
      IF Debugging
      THEN
         printf ('odd vector (%d) type (%d) arg (%p) is already attached to the pending queue\n',
                 vec, vptr^.type, vptr^.arg)
      END
   END ;
   signal (lock)
END IncludeVector ;


(*
   ExcludeVector - excludes, vec, from the dispatcher list of
                   possible interrupt causes.
*)

PROCEDURE ExcludeVector (vec: CARDINAL) ;
VAR
   vptr, uptr: Vector ;
BEGIN
   wait (lock) ;
   vptr := FindPendingVector (vec) ;
   IF vptr = NIL
   THEN
      Halt ('cannot find pending vector supplied',
            __FILE__, __FUNCTION__, __LINE__)
   ELSE
      (* printf('excluding vector %d\n', vec) ; *)
      IF Pending[vptr^.priority] = vptr
      THEN
         Pending[vptr^.priority] := Pending[vptr^.priority]^.pending
      ELSE
         uptr := Pending[vptr^.priority] ;
         WHILE uptr^.pending#vptr DO
            uptr := uptr^.pending
         END ;
         uptr^.pending := vptr^.pending
      END ;
      IF vptr^.type=time
      THEN
         vptr^.queued := FALSE
      END
   END ;
   signal (lock)
END ExcludeVector ;


(*
   AddFd - adds the file descriptor fd to set updating max.
*)

PROCEDURE AddFd (VAR set: SetOfFd; VAR max: INTEGER; fd: INTEGER) ;
BEGIN
   IF (fd<0)
   THEN
      RETURN
   END ;
   max := Max (fd, max) ;
   IF set = NIL
   THEN
      set := InitSet () ;
      FdZero (set)
   END ;
   FdSet (fd, set)
   (* printf('%d, ', fd) *)
END AddFd ;


(*
   DumpPendingQueue - displays the pending queue.
*)

PROCEDURE DumpPendingQueue ;
VAR
   pri  : PROTECTION ;
   vptr : Vector ;
   sec,
   micro: CARDINAL ;
BEGIN
   printf ("Pending queue\n");
   FOR pri := MIN (PROTECTION) TO MAX (PROTECTION) DO
      printf ("[%d]  ", pri);
      vptr := Pending[pri] ;
      WHILE vptr # NIL DO
         IF (vptr^.type=input) OR (vptr^.type=output)
         THEN
            printf ("(fd=%d) (vec=%d)", vptr^.File, vptr^.no)
         ELSIF vptr^.type=time
         THEN
            GetTime (vptr^.rel, sec, micro) ;
            Assert (micro < Microseconds) ;
            printf ("time (%u.%06u secs) (arg = %p)\n",
                    sec, micro, vptr^.arg)
         END ;
         vptr := vptr^.pending
      END ;
      printf (" \n")
   END
END DumpPendingQueue ;


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
                           maxFd: INTEGER; VAR inSet, outSet: SetOfFd; VAR timeval: Timeval; b4, after: Timeval) : BOOLEAN ;
VAR
   result: INTEGER ;
   p     : CARDINAL ;
   vec   : Vector ;
   b4s,
   b4m,
   afs,
   afm,
   sec,
   micro : CARDINAL ;
BEGIN
   wait (lock) ;
   p := MAX (PROTECTION) ;
   WHILE p > pri DO
      vec := Pending[p] ;
      WHILE vec # NIL DO
         WITH vec^ DO
            CASE type OF

            input :  IF (File < maxFd) AND (inSet # NIL) AND FdIsSet (File, inSet)
                     THEN
                        IF Debugging
                        THEN
                           printf ('read (fd=%d) is ready (vec=%d)\n', File, no) ;
                           DumpPendingQueue
                        END ;
                        FdClr (File, inSet) ;  (* so we dont activate this again from our select.  *)
                        signal (lock) ;
                        call (no, priority, arg) ;
                        RETURN TRUE
                     END |
            output:  IF (File < maxFd) AND (outSet#NIL) AND FdIsSet (File, outSet)
                     THEN
                        IF Debugging
                        THEN
                           printf ('write (fd=%d) is ready (vec=%d)\n', File, no) ;
                           DumpPendingQueue
                        END ;
                        FdClr (File, outSet) ;  (* so we dont activate this again from our select.  *)
                        signal (lock) ;
                        call (no, priority, arg) ;
                        RETURN TRUE
                     END |
            time  :  IF untilInterrupt AND (timeval # NIL)
                     THEN
                        result := GetTimeOfDay (after) ;
                        Assert (result=0) ;
                        IF Debugging
                        THEN
                           GetTime (timeval, sec, micro) ;
                           Assert (micro < Microseconds) ;
                           GetTime (after, afs, afm) ;
                           Assert (afm < Microseconds) ;
                           GetTime (b4, b4s, b4m) ;
                           Assert (b4m < Microseconds) ;
                           printf ("waited %u.%06u + %u.%06u now is %u.%06u\n",
                                   sec, micro, b4s, b4m, afs, afm) ;
                        END ;
                        IF IsGreaterEqual (after, abs)
                        THEN
                           IF Debugging
                           THEN
                              DumpPendingQueue ;
                              printf ("time has expired calling dispatcher\n")
                           END ;
                           timeval := KillTime (timeval) ;  (* so we dont activate this again from our select.  *)
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
         vec := vec^.pending
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
   found  : BOOLEAN ;
   result : INTEGER ;
   zero,
   after,
   b4,
   timeval: Timeval ;
   vec    : Vector ;
   inSet,
   outSet : SetOfFd ;
   sec,
   micro  : CARDINAL ;
   maxFd  : INTEGER ;
   p      : CARDINAL ;
BEGIN
   wait (lock) ;
   IF pri < MAX (PROTECTION)
   THEN
      IF Debugging
      THEN
         DumpPendingQueue
      END ;
      maxFd := -1 ;
      timeval := NIL ;
      inSet := NIL ;
      outSet := NIL ;
      timeval := InitTime (MAX (INTEGER), 0) ;
      p := MAX (PROTECTION) ;
      found := FALSE ;
      WHILE p>pri DO
         vec := Pending[p] ;
         WHILE vec#NIL DO
            WITH vec^ DO
               CASE type OF

               input :  AddFd (inSet, maxFd, File) |
               output:  AddFd (outSet, maxFd, File) |
               time  :  IF IsGreaterEqual (timeval, abs)
                        THEN
                           GetTime (abs, sec, micro) ;
                           Assert (micro < Microseconds) ;
                           IF Debugging
                           THEN
                              printf ("shortest delay is %u.%06u\n", sec, micro)
                           END ;
                           SetTime (timeval, sec, micro) ;
                           found := TRUE
                        END

               END
            END ;
            vec := vec^.pending
         END ;
         DEC (p)
      END ;
      IF NOT untilInterrupt
      THEN
         SetTime (timeval, 0, 0)
      END ;
      IF untilInterrupt AND (((inSet=NIL) AND (outSet=NIL)) OR (maxFd=-1)) AND (NOT found)
      THEN
         Halt ('deadlock found, no more processes to run and no interrupts active',
               __FILE__, __FUNCTION__, __LINE__)
      END ;
      (* printf('timeval = 0x%x\n', timeval) ; *)
      (* printf('}\n') ; *)
      IF (NOT found) AND (maxFd=-1)
      THEN
         (* no file descriptors to be selected upon.  *)
         timeval := KillTime (timeval) ;
         signal (lock) ;
         RETURN
      ELSE
         GetTime (timeval, sec, micro) ;
         Assert (micro < Microseconds) ;
         zero := InitTime (0, 0) ;
         b4 := InitTime (0, 0) ;
         after := InitTime (0, 0) ;
         result := GetTimeOfDay (b4) ;
         Assert (result=0) ;
         SubTime (sec, micro, timeval, b4) ;
         SetTime (timeval, sec, micro) ;
         IF Debugging
         THEN
            printf ("select waiting for %u.%06u seconds\n", sec, micro)
         END ;
         signal (lock) ;
         REPEAT
            IF Debugging
            THEN
               printf ("select (.., .., .., %u.%06u)\n", sec, micro)
            END ;
            IF maxFd<0
            THEN
               result := select (0, NIL, NIL, NIL, timeval)
            ELSE
               result := select (maxFd+1, inSet, outSet, NIL, timeval)
            END ;
            IF result=-1
            THEN
               IF Debugging
               THEN
                  perror ("select failed : ") ;
               END ;
               result := select (maxFd+1, inSet, outSet, NIL, zero) ;
               IF result#-1
               THEN
                   GetTime (timeval, sec, micro) ;
                   IF Debugging
                   THEN
                     printf ("(nfds : %d timeval: %u.%06u) : \n", maxFd, sec, micro) ;
                   END ;
                   perror ("select timeout argument was faulty : ")
               ELSE
                  result := select (maxFd+1, inSet, NIL, NIL, timeval) ;
                  IF result#-1
                  THEN
                     perror ("select output fd argument was faulty : ")
                  ELSE
                     result := select (maxFd+1, NIL, outSet, NIL, timeval) ;
                     IF result#-1
                     THEN
                        perror ("select input fd argument was faulty : ")
                     ELSE
                        IF maxFd=-1
                        THEN
                           result := select (0, NIL, NIL, NIL, timeval) ;
                           IF result=-1
                           THEN
                              IF Debugging
                              THEN
                                 perror ("select does not accept nfds == 0 ") ;
                              END ;
                              result := 0
                           END
                        ELSE
                           perror ("select maxFD+1 argument was faulty : ") ;
                        END
                     END
                  END
               END
            END
         UNTIL result#-1
      END ;
      WHILE activatePending (untilInterrupt, call, pri,
                             maxFd+1, inSet, outSet, timeval, b4, after) DO
      END ;
      IF timeval#NIL
      THEN
         timeval := KillTime (timeval)
      END ;
      IF zero#NIL
      THEN
         zero := KillTime (zero)
      END ;
      IF after#NIL
      THEN
         after := KillTime (after)
      END ;
      IF b4#NIL
      THEN
         b4 := KillTime (b4)
      END ;
      IF inSet#NIL
      THEN
         inSet := KillSet (inSet)
      END ;
      IF outSet#NIL
      THEN
         outSet := KillSet (outSet)
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
