IMPLEMENTATION MODULE AdvSystem ;

FROM ASCII IMPORT nul, cr, lf, bs ;
FROM StdIO IMPORT PushOutput, PopOutput ;
FROM libc IMPORT printf, write, read ;
FROM StrLib IMPORT StrLen ;
FROM SYSTEM IMPORT ADR ;
FROM Debug IMPORT Halt ;
FROM TimerHandler IMPORT GetTicks, TicksPerSecond ;
FROM RTint IMPORT InitInputVector, InitOutputVector ;
FROM COROUTINES IMPORT PROTECTION ;

FROM Executive IMPORT GetCurrentProcess, DESCRIPTOR, SEMAPHORE,
                      Resume, InitProcess, InitSemaphore, Wait, Signal,
                      WaitForIO ;

FROM Lock IMPORT InitLock, GetReadAccess, GetWriteAccess, LOCK,
                 ReleaseReadAccess, ReleaseWriteAccess ;
FROM ProcArgs IMPORT ProcessArgs, InitArgs, SetArgs, CollectArgs ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM AdvUtil IMPORT NormalArrow, MagicArrow ;


CONST
   MaxNoOfProcesses = MaxNoOfPlayers*3 + 1 ;
   ArrowProcessSize = 30 * 1024 * 1024 ;

TYPE
   ProcPlayer = RECORD
                   process : DESCRIPTOR ;
                   playerId: CARDINAL ;
                END ;

VAR
   GlobalFd      : INTEGER ;
   ScreenQ       : SEMAPHORE ;
   PlayerLock    : LOCK ;
   DoorLock      : LOCK ;
   TreasureLock  : LOCK ;
   AccessToRandom: SEMAPHORE ;
   RandomCount   : CARDINAL ;
   ProcToPlay    : ARRAY [0..MaxNoOfPlayers] OF ProcPlayer ;
   PArgs         : ProcessArgs ;

(*
   AssignOutputTo - assigns the current process to be associated with
                    player, p.
*)

PROCEDURE AssignOutputTo (p: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i<MaxNoOfProcesses DO
      WITH ProcToPlay[i] DO
         IF process=NIL
         THEN
            process := GetCurrentProcess() ;
            playerId := p ;
            RETURN
         ELSIF process=GetCurrentProcess()
         THEN
            playerId := p ;
            RETURN
         END
      END ;
      INC(i)
   END ;
   Halt ('increase MaxNoOfProcesses', __FILE__, __FUNCTION__, __LINE__)
END AssignOutputTo ;


(*
   UnAssign - unassign the current process from any player.
*)

PROCEDURE UnAssign ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i<MaxNoOfProcesses DO
      WITH ProcToPlay[i] DO
         IF process=GetCurrentProcess()
         THEN
            process := NIL ;
            RETURN
         END
      END ;
      INC(i)
   END
END UnAssign ;


(*
   ProcessToPlayer - returns the player associated with the current process.
*)

PROCEDURE ProcessToPlayer () : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i<MaxNoOfProcesses DO
      WITH ProcToPlay[i] DO
         IF process=GetCurrentProcess()
         THEN
            RETURN( playerId )
         END
      END ;
      INC(i)
   END ;
   Halt ('process has never has a player assigned to its output',
         __FILE__, __FUNCTION__, __LINE__)
END ProcessToPlayer ;


(*
   checkStatus -
*)

PROCEDURE checkStatus (r: INTEGER) ;
VAR
   p: CARDINAL ;
BEGIN
   IF r<1
   THEN
      r := printf("WriteS client has gone away - tidying up\n") ;
      p := ProcessToPlayer() ;
      WITH Player[p] DO
         fd := -1 ;
         IF DeathType=living
         THEN
            DeathType := exitdungeon
         END
      END
   END
END checkStatus ;


PROCEDURE localWrite (fd: INTEGER; ch: CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF fd>=0
   THEN
      WaitForIO(InitOutputVector(fd, MAX(PROTECTION))) ;
      checkStatus(write(fd, ADR(ch), SIZE(ch)))
   END
END localWrite ;


PROCEDURE localWriteS (fd: INTEGER; s: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF fd>=0
   THEN
      checkStatus(write(fd, ADR(s), StrLen(s)))
   END
END localWriteS ;


PROCEDURE ReadString (VAR s: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
   h: CARDINAL ;
BEGIN
   h := 0 ;
   WHILE h<HIGH(s) DO
      IF ClientRead(s[h])
      THEN
         IF (s[h]=lf) OR (s[h]=cr) OR (s[h]=nul)
         THEN
            s[h] := nul ;
            RETURN
         ELSIF s[h]=bs
         THEN
            IF h>0
            THEN
               WriteChar(bs) ;
               DEC(h)
            END
         ELSE
            WriteChar(s[h]) ;
            INC(h)
         END
      ELSE
         s[h] := nul ;
         RETURN
      END
   END ;
   IF (s[h]=lf) OR (s[h]=cr)
   THEN
      s[h] := nul
   END
END ReadString ;


(*
   DefaultWrite - writes to the default (local) file descriptor.
*)

PROCEDURE DefaultWrite (ch: CHAR) ;
VAR
   p: CARDINAL ;
BEGIN
   p := ProcessToPlayer() ;
   localWrite(Player[p].fd, ch)
END DefaultWrite ;


PROCEDURE ClientRead (VAR ch: CHAR) : BOOLEAN ;
VAR
   r: INTEGER ;
BEGIN
   WITH Player[PlayerNo()] DO
      IF fd>=0
      THEN
         WaitForIO(InitInputVector(fd, MAX(PROTECTION))) ;
         r := read(fd, ADR(ch), SIZE(ch)) ;
         checkStatus(r) ;
         RETURN( r=1 )
      ELSE
         RETURN( FALSE )
      END
   END ;
END ClientRead ;


PROCEDURE WriteChar (ch: CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   WITH Player[PlayerNo()] DO
      IF fd>=0
      THEN
         IF ch=bs
         THEN
            localWriteS(fd, 'eC')
         ELSE
            localWriteS(fd, 'dC ') ;
            localWrite(fd, ch)
         END ;
         localWrite(fd, lf)
      END
   END ;
END WriteChar ;


PROCEDURE PlayerNo () : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 0 TO NextFreePlayer-1 DO
      IF GetCurrentProcess()=Player[i].PlayerProcess
      THEN
         RETURN( i )
      END
   END ;
   Halt ('process calling is not a player process',
         __FILE__, __FUNCTION__, __LINE__)
END PlayerNo ;


(*
   IsaPlayer -
*)

PROCEDURE IsaPlayer (d: DESCRIPTOR) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 0 TO NextFreePlayer-1 DO
      IF Player[i].PlayerProcess=d
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END IsaPlayer ;


(*
   FindFreePlayer -
*)

PROCEDURE FindFreePlayer () : INTEGER ;
VAR
   i : INTEGER ;
   pc: POINTER TO CARDINAL ;
BEGIN
   IF NextFreePlayer<=MaxNoOfPlayers
   THEN
      IF NextFreePlayer>0
      THEN
         (* reuse an old player who has left the game *)
         FOR i := 0 TO NextFreePlayer-1 DO
            WITH Player[i] DO
               IF fd=-1
               THEN
                  Weight := ManWeight ;
                  TreasureOwn := {} ;
                  RETURN( i )
               END
            END
         END
      END ;
      i := NextFreePlayer ;
      INC(NextFreePlayer) ;
      WITH Player[i] DO
         Weight := ManWeight ;
         TreasureOwn := {} ;
         NormalProcArgs := InitArgs() ;
         MagicProcArgs := InitArgs() ;
         NEW(pc) ;
         pc^ := i ;
         MagicP := Resume(InitProcess(MagicArrowP, ArrowProcessSize, 'Magic Arrow')) ;
         pc := SetArgs(PArgs, pc) ;
         NormalP := Resume(InitProcess(NormalArrowP, ArrowProcessSize, 'Normal Arrow')) ;
         NEW(pc) ;
         pc^ := i ;
         pc := SetArgs(PArgs, pc)
      END ;
      RETURN( i )
   ELSE
      RETURN( -1 )
   END
END FindFreePlayer ;


(*
   IsPlayerActive - returns TRUE if player, p, is still playing
*)

PROCEDURE IsPlayerActive (p: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (p<NextFreePlayer) AND (Player[p].fd#-1) )
END IsPlayerActive ;


PROCEDURE NormalArrowP ;
VAR
   pc: POINTER TO CARDINAL ;
BEGIN
   pc := CollectArgs(PArgs) ;
   LOOP
      NormalArrow(pc^)
   END
END NormalArrowP ;


PROCEDURE MagicArrowP ;
VAR
   pc: POINTER TO CARDINAL ;
BEGIN
   pc := CollectArgs(PArgs) ;
   LOOP
      MagicArrow(pc^)
   END
END MagicArrowP ;


PROCEDURE StartPlayer (f: INTEGER) ;
VAR
   i : INTEGER ;
BEGIN
   i := FindFreePlayer() ;
   IF i=-1
   THEN
      (* write an error message to fd *)
   ELSE
      WITH Player[i] DO
         fd := f ;
         PlayerProcess := GetCurrentProcess() ;
         DeathType := living ;
         Wounds := 100 ;
         Fatigue := 100 ;
         Direction := 0 ;
         TimeMinSec(LastSecWounds) ;
         LastSecFatigue := GetTicks() DIV (TicksPerSecond DIV 2)
      END
   END
END StartPlayer ;


PROCEDURE Init ;
BEGIN
   PlayerLock := InitLock('player lock') ;
   ScreenQ := InitSemaphore(1, 'ScreenQ') ;
   DoorLock := InitLock('DoorLock') ;
   TreasureLock := InitLock('TreasureLock') ;
   AccessToRandom := InitSemaphore(1, 'AccessToRandom') ;
   PArgs := InitArgs() ;
   TimeMinSec (RandomCount) ;
   NextFreePlayer := 0 ;
   PushOutput (DefaultWrite)
END Init ;


PROCEDURE TimeMinSec (VAR MinSec: CARDINAL) ;
BEGIN
   MinSec := GetTicks() DIV TicksPerSecond
END TimeMinSec ;


PROCEDURE RandomNumber (VAR r: CARDINAL; n: CARDINAL) ;
VAR
   ms: CARDINAL ;
BEGIN
   IF n=1
   THEN
      r := 0
   ELSE
      Wait( AccessToRandom ) ;
      r := RandomCount MOD n ;

      ms := RandomCount MOD 256 ;
      RandomCount := ms*256+ms ;   (* multiply by 257 *)

      IF (MAX(CARDINAL)-RandomCount) >= 0ABCDH  (* Add 0ABCDH *)
      THEN
         INC( RandomCount, 0ABCDH )
      ELSE
         DEC( RandomCount, (MAX(CARDINAL)-0ABCDH) )
      END ;

      Signal( AccessToRandom ) ;

      (* Returns a number 1..n  *)
   END
END RandomNumber ;



(* The rules which govern the allocation of these resourses are *)

(* 1)  One may claim multiple resourses in the following order: *)
(*        AccessPlayer                                          *)
(*        AccessDoor                                            *)
(*        AccessTreasure                                        *)
(*        AccessScreen                                          *)
(*        GetTime - Procedure NOT lock!                         *)
(*                                                              *)
(*                          All r/w  -  doesn't matter!         *)
(*                                                              *)
(* 2)  Must never reverse this claiming or DEADLOCK may occur.  *)
(*                                                              *)
(* 3)  Must access players in order ie 0 1 2                    *)



(*
 * Access To Players
 *)

PROCEDURE GetReadAccessToPlayer ;
BEGIN
   GetReadAccess(PlayerLock)
END GetReadAccessToPlayer ;


PROCEDURE GetWriteAccessToPlayer ;
BEGIN
   GetWriteAccess(PlayerLock)
END GetWriteAccessToPlayer ;


PROCEDURE ReleaseReadAccessToPlayer ;
BEGIN
   ReleaseReadAccess(PlayerLock)
END ReleaseReadAccessToPlayer ;


PROCEDURE ReleaseWriteAccessToPlayer ;
BEGIN
   ReleaseWriteAccess(PlayerLock)
END ReleaseWriteAccessToPlayer ;


(*
 * Access To Doors
 *)

PROCEDURE GetReadAccessToDoor ;
BEGIN
   GetReadAccess(DoorLock)
END GetReadAccessToDoor ;


PROCEDURE GetWriteAccessToDoor ;
BEGIN
   GetWriteAccess(DoorLock)
END GetWriteAccessToDoor ;


PROCEDURE ReleaseReadAccessToDoor ;
BEGIN
   ReleaseReadAccess(DoorLock)
END ReleaseReadAccessToDoor ;


PROCEDURE ReleaseWriteAccessToDoor ;
BEGIN
   ReleaseWriteAccess(DoorLock)
END ReleaseWriteAccessToDoor ;


(*
 * Access To Treasures
 *)

PROCEDURE GetReadAccessToTreasure ;
BEGIN
   GetReadAccess(TreasureLock)
END GetReadAccessToTreasure ;


PROCEDURE GetWriteAccessToTreasure ;
BEGIN
   GetWriteAccess(TreasureLock)
END GetWriteAccessToTreasure ;


PROCEDURE ReleaseReadAccessToTreasure ;
BEGIN
   ReleaseReadAccess(TreasureLock)
END ReleaseReadAccessToTreasure ;


PROCEDURE ReleaseWriteAccessToTreasure ;
BEGIN
   ReleaseWriteAccess(TreasureLock)
END ReleaseWriteAccessToTreasure ;


(*
 *  Access To Screen
 *)

PROCEDURE GetAccessToScreen ;
BEGIN
   GetAccessToScreenNo(PlayerNo())
END GetAccessToScreen ;


PROCEDURE ReleaseAccessToScreen ;
BEGIN
   ReleaseAccessToScreenNo(PlayerNo())
END ReleaseAccessToScreen ;


PROCEDURE GetAccessToScreenNo (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   Wait(ScreenQ)
END GetAccessToScreenNo ;


PROCEDURE ReleaseAccessToScreenNo (p: CARDINAL) ;
BEGIN
   Signal(ScreenQ)
END ReleaseAccessToScreenNo ;


BEGIN
   Init
END AdvSystem.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
