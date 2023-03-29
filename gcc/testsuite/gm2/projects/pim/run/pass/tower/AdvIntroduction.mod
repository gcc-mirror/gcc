(* Copyright (C) 2003
                 Free Software Foundation, Inc. *)
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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

IMPLEMENTATION MODULE AdvIntroduction ;

FROM SYSTEM IMPORT ADR, SIZE ;
FROM ASCII IMPORT lf, cr, nul ;
FROM StrLib IMPORT StrLen ;
FROM SocketControl IMPORT nonBlocking, ignoreSignals ;

FROM Executive IMPORT WaitForIO, InitProcess, InitSemaphore, Wait, Signal, Resume,
                      Suspend, DESCRIPTOR, SEMAPHORE, KillProcess ;

FROM RTint IMPORT InitInputVector ;
FROM COROUTINES IMPORT PROTECTION ;
FROM sckt IMPORT tcpServerState, tcpServerEstablish, tcpServerAccept, tcpServerSocketFd ;
FROM libc IMPORT printf, read, write ;
FROM AdvUtil IMPORT Positioning, TestIfLastLivePlayer ;

FROM AdvSystem IMPORT Player, TypeOfDeath, StartPlayer, PlayerNo,
                      ClientRead, DefaultWrite, UnAssign,
                      ReadString, GetReadAccessToPlayer,
                      ReleaseReadAccessToPlayer,
                      GetAccessToScreen, ReleaseAccessToScreen ;

FROM AdvTreasure IMPORT DisplayEnemy, Grenade ;
FROM AdvUtil IMPORT InitialDisplay ;
FROM AdvCmd IMPORT ExecuteCommand ;
FROM Screen IMPORT WriteCommand, ClearScreen, WriteString, PromptString, Pause, Quit ;
FROM AdvSound IMPORT EnterGame ;
FROM StdIO IMPORT PushOutput ;


CONST
   Meg       = 1024*1024 ;
   StackSize = 30 * Meg ;

VAR
   ToBeTaken: SEMAPHORE ;
   NextFd   : INTEGER ;


PROCEDURE theServer ;
VAR
   fd: INTEGER ;
   v : CARDINAL ;
   ch: CHAR ;
   r : INTEGER ;
BEGIN
   fd := NextFd ;
   Signal(ToBeTaken) ;
   v := InitInputVector(fd, MAX(PROTECTION)) ;
   r := printf("inside `theServer' using fd=%d\n", fd);
   StartPlayer(fd) ;
   Copyleft ;
   Title ;
   Knight ;
   WITH Player[PlayerNo()] DO
      fd := -1 ;
      PlayerProcess := NIL ;
   END ;
   UnAssign ;
   KillProcess
END theServer ;


PROCEDURE StartGame ;
VAR
   r   : INTEGER ;
   v   : CARDINAL ;
   fd  : INTEGER ;
   s   : tcpServerState ;
   g, p: DESCRIPTOR ;
BEGIN
   ignoreSignals ;
   PushOutput(DefaultWrite) ;
   g := Resume(InitProcess(Grenade, StackSize, 'grenade')) ;
   s := tcpServerEstablish() ;
   ToBeTaken := InitSemaphore(1, 'ToBeTaken') ;
   v := InitInputVector(tcpServerSocketFd(s), MAX(PROTECTION)) ;
   LOOP
      r := printf("before WaitForIO\n");
      WaitForIO(v) ;
      fd := tcpServerAccept(s) ;
      r := nonBlocking(fd) ;
      r := printf("before InitProcess\n");
      p := InitProcess(theServer, StackSize, 'theServer') ;
      NextFd := fd ;
      r := printf("before Resume\n");
      p := Resume(p) ;
      Wait(ToBeTaken)
   END
END StartGame ;


PROCEDURE Knight ;
VAR
   Dead: BOOLEAN ;
   ch  : CHAR ;
   p   : CARDINAL ;
BEGIN
   EquipKnight ;
   SetUpKnight ;
   InitialDisplay ;
   p := PlayerNo() ;
   EnterGame(p) ;
   Dead := FALSE ;
   REPEAT
      IF ClientRead(ch)
      THEN
         WriteCommand(p, ch) ;
         ExecuteCommand(ch, Dead)
      ELSE
         Dead := TRUE
      END
   UNTIL Dead ;
   GiveResults
END Knight ;


PROCEDURE Copyleft ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   ClearScreen(p) ;
   WriteString(p, 'Written whilst on holiday during the rainy months of\n') ;
   WriteString(p, 'August 85, August 86 and ported to GNU/linux during July/August 2005\n\n') ;
   WriteString(p, '\n') ;
   WriteString(p, 'A multiplayer game inspired by two single player Commodore PET\n') ;
   WriteString(p, 'game of circa 1979 (Morloc Tower and Temple of Apshai)\n') ;
   WriteString(p, '\n') ;
   WriteString(p, 'This game is rather different (similar key commands) and\n') ;
   WriteString(p, 'in retrospect a very very poor persons multiplayer doom!\n') ;
   Pause(p)
END Copyleft ;


PROCEDURE Title ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   ClearScreen(p) ;
   WriteString(p, '...set in a time of long ago, when life hast no value and\n') ;
   WriteString(p, '   death sometimes, hadst a price. Thou needst to be quick\n') ;
   WriteString(p, '   with thy sword and fast with thy bow, for only the best\n') ;
   WriteString(p, '   survived...\n\n\n')
END Title ;


PROCEDURE EquipKnight ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   PromptString(p, 'What is thy name? ') ;
   WITH Player[PlayerNo()] DO
      ReadString(ManName)
   END ;
   WriteString(p, '\n')
END EquipKnight ;


PROCEDURE SetUpKnight ;
BEGIN
   WITH Player[PlayerNo()] DO
      NoOfMagic := 1 ;
      NoOfNormal := 7
   END ;
   Positioning
END SetUpKnight ;


PROCEDURE GiveResults ;
VAR
   yes: BOOLEAN ;
   p  : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   Pause(p) ;
   GetReadAccessToPlayer ;
   GetAccessToScreen ;
   ClearScreen(p) ;
   WITH Player[p] DO
      IF Wounds=0
      THEN
         WriteString(p, 'Thou art slain...') ;
         GiveDescriptionOfDeath(p, DeathType)
      ELSE
         TestIfLastLivePlayer(yes) ;
         IF yes
         THEN
            WriteString(p, 'Thou art the conqueror of the dungeon')
         ELSE
            WriteString(p, 'Thou art the coward of the dungeon')
         END ;
         Wounds := 0
      END ;
      WriteString(p, '\n\n\n')
   END ;
   ReleaseAccessToScreen ;
   ReleaseReadAccessToPlayer ;
   Pause(p) ;
   Quit(p)
END GiveResults ;


PROCEDURE GiveDescriptionOfDeath (p: CARDINAL; Dt: TypeOfDeath) ;
BEGIN
   WriteString(p, '\n\n\n\nThou expired from life after :\n\n') ;
   CASE Dt OF

   sword       : WriteString(p, 'being slain with a sword') |
   magicarrow  : WriteString(p, 'being pierced by a magic arrow') |
   fireball    : WriteString(p, 'thou wast struck by a fireball burning thy body fatally') |
   normalarrow : WriteString(p, 'thou wast struck a deadly blow caused by an arrow') |
   explosion   : WriteString(p, 'having thy guts blown all over the dungeon') |
   exitdungeon : WriteString(p, 'thou crawlest out of the dungeon and expired')

   END
END GiveDescriptionOfDeath ;


(* Monitor allows a dead player to look around the dungeon unaffecting *)
(* the current game.                                                   *)

PROCEDURE Monitor ;
VAR
   p : CARDINAL ;
   ch: CHAR ;
BEGIN
   p := PlayerNo() ;
   REPEAT
      ClearScreen(p) ;
      WriteString(p, 'Monitor --- Look at other players\n\n\n\n') ;
      WriteString(p, 'Commands:\n') ;
      WriteString(p, '1)  Look at other players\n') ;
      WriteString(p, '2)  Exit\n\n') ;
      WriteString(p, 'Option:') ;
      IF ClientRead(ch)
      THEN
         DefaultWrite(ch) ;
         IF ch='1'
         THEN
            DisplayEnemy
         END
      ELSE
         RETURN
      END
   UNTIL ch='2'
END Monitor ;


END AdvIntroduction.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
