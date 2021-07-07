(* Copyright (C) 2005-2020
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)

MODULE testtime ;


FROM Debug IMPORT Halt ;
FROM StdIO IMPORT PushOutput ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM TimerHandler IMPORT EVENT, TicksPerSecond, Sleep, ArmEvent,
                         Cancel, WaitOn, ReArmEvent ;
FROM SYSTEM IMPORT TurnInterrupts ;
FROM COROUTINES IMPORT PROTECTION ;
FROM Executive IMPORT DESCRIPTOR, InitProcess, Resume, Ps ;
FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT write, printf ;


(*
   OneSecond -
*)

PROCEDURE OneSecond ;
VAR
   n: CARDINAL ;
BEGIN
   OldInts := TurnInterrupts (MIN (PROTECTION)) ;
   printf ("1 second process has come to life\n");
   n := 0 ;
   LOOP
      Sleep (1*TicksPerSecond) ;
      INC (n) ;
      printf ("%d seconds\n", n);
   END
END OneSecond ;


(*
   FourSeconds -
*)

PROCEDURE FourSeconds ;
VAR
   n: CARDINAL ;
BEGIN
   OldInts := TurnInterrupts (MIN (PROTECTION)) ;
   printf ("4 seconds process has come to life\n");
   n := 0 ;
   LOOP
      Sleep (4*TicksPerSecond) ;
      INC (n) ;
      printf ("4 second alarm (%d occurance)\n", n);
   END
END FourSeconds ;


(*
   SixSeconds -
*)

PROCEDURE SixSeconds ;
VAR
   n: CARDINAL ;
BEGIN
   OldInts := TurnInterrupts (MAX (PROTECTION)) ;
   printf ("6 seconds process has come to life\n");
   n := 0 ;
   LOOP
      Timeout := ArmEvent (6*TicksPerSecond) ;
      IF WaitOn (Timeout)
      THEN
         WriteString ('...someone cancelled it...')
      ELSE
         INC (n) ;
         printf ("6 second alarm (%d occurance)\n", n)
      END
   END
END SixSeconds ;


CONST
   StackSize = 0100000H ;

VAR
   p1, p4,
   p6      : DESCRIPTOR ;
   OldInts : PROTECTION ;
   Timeout : EVENT ;
BEGIN
   OldInts := TurnInterrupts (MIN (PROTECTION)) ;
   printf ("got to OS\n") ;

   printf ("now to create three processes...\n") ;

   p1 := Resume (InitProcess (OneSecond  , StackSize, '1')) ;
   p4 := Resume (InitProcess (FourSeconds, StackSize, '4')) ;
   p6 := Resume (InitProcess (SixSeconds , StackSize, '6')) ;

   Sleep (20*TicksPerSecond) ;
   printf ("successfully completed, finishing now.\n")
END testtime.
