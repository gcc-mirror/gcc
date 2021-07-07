(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
MODULE testdiv ;

FROM SYSTEM IMPORT ADDRESS ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StrLib IMPORT StrLen ;


CONST
   ClockFreq = 20 ;

TYPE
   ProcType   = (User, System) ;
 
   ProcStatus = (Runnable, WaitOnSem, WaitOnIO, Suspended, Killed, Sleeping) ;
 
   PtrToProcDes      = POINTER TO ProcessDescriptor ;
   ProcessDescriptor = RECORD
                          RightPtr      : PtrToProcDes ;
                          LeftPtr       : PtrToProcDes ;
                          RightRunPtr   : PtrToProcDes ;
                          LeftRunPtr    : PtrToProcDes ;
                          RightOwner    : PtrToProcDes ;
                          LeftOwner     : PtrToProcDes ;
                          Father        : PtrToProcDes ;
                          Sons          : PtrToProcDes ;
                          PPriority     : CARDINAL ;  (* 0..MaxPriority *)
                          PQuanta       : CARDINAL ;
                          PMemorySize   : CARDINAL ;
                          PMemoryStart  : ADDRESS ;
                          PHeapSize     : CARDINAL ;
                          PHeapStart    : ADDRESS ;
                          PFreeMem      : ADDRESS ;
                          TimeSecs      : CARDINAL ;
                          PName         : ARRAY [0..15] OF CHAR ;
                          Process       : ADDRESS ;
                          PType         : ProcType ;
                          PCurrentStatus: ProcStatus ;
                          PNextStatus   : ProcStatus ;
                       END ;


PROCEDURE WriteProcess (p: PtrToProcDes) ;
VAR
   i : CARDINAL ;
BEGIN
(*
   WriteString( p^.PName ) ;
   i := HIGH( p^.PName )-StrLen( p^.PName ) ;
   WHILE i>0 DO
       WriteString(' ') ;
       DEC( i )
   END ;
*)
   WITH p^ DO
(*
      WriteCard( PMemorySize DIV 1024, 6 ) ;
      WriteString('k   (') ;
      IF PMemorySize#0
      THEN
         WriteCard( (PMemorySize -
                     (CARDINAL(Process) - CARDINAL(PMemoryStart))) * 100
                    DIV PMemorySize, 3)
      ELSE
         WriteString('  0')
      END ;
      WriteString('%)  Pri') ;
      WriteCard( PPriority, 2 ) ;
      WriteCard( PQuanta, 3 ) ;
      WriteString('  ') ;
      WriteString('  ') ;
      WriteString('  ') ;
*)
      WriteCard( (TimeSecs DIV ClockFreq) DIV (60*60), 2 ) ;      (* Hours   *)
      WriteString(':') ;
      WriteCard( ((TimeSecs DIV ClockFreq) DIV 60) MOD 60, 2) ;   (* Minutes *)
      WriteString(':') ;
      WriteCard( (TimeSecs DIV ClockFreq) MOD 60, 2)              (* Seconds *)
   END
END WriteProcess ;


VAR
   TimeSecs: CARDINAL ;
   p       : PtrToProcDes ;
BEGIN
   WriteProcess(p)
END testdiv.
