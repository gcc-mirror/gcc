IMPLEMENTATION MODULE AdvTreasure ;


FROM libc IMPORT printf ;
FROM Executive IMPORT GetCurrentProcess, InitSemaphore, SEMAPHORE,
                      Wait, Signal, InitProcess, Resume, DESCRIPTOR ;
FROM TimerHandler IMPORT Sleep, TicksPerSecond, EVENT, ReArmEvent, ArmEvent, WaitOn, Cancel ;

FROM ASCII IMPORT cr ;
FROM StrLib IMPORT StrCopy, StrConCat, StrLen ;
FROM NumberIO IMPORT CardToStr, WriteCard ;
FROM Storage IMPORT ALLOCATE ;
FROM Assertion IMPORT Assert ;

FROM AdvSystem IMPORT MaxNoOfPlayers,
                      ManWeight,
                      Man,
                      TypeOfDeath,
                      Player,
                      PlayerSet,
                      PlayerNo,
                      ArrowArgs,
                      StartPlayer,
                      TimeMinSec,
                      RandomNumber,
                      ClientRead,
                      DefaultWrite,
                      ReadString,
                      NextFreePlayer,
                      IsPlayerActive,
                      AssignOutputTo,

                      GetReadAccessToPlayer,
                      GetWriteAccessToPlayer,
                      ReleaseReadAccessToPlayer,
                      ReleaseWriteAccessToPlayer,

                      GetReadAccessToDoor,
                      GetWriteAccessToDoor,
                      ReleaseReadAccessToDoor,
                      ReleaseWriteAccessToDoor,

                      GetReadAccessToTreasure,
                      GetWriteAccessToTreasure,
                      ReleaseReadAccessToTreasure,
                      ReleaseWriteAccessToTreasure,

                      GetAccessToScreen,
                      ReleaseAccessToScreen,
                      GetAccessToScreenNo,
                      ReleaseAccessToScreenNo ;

FROM AdvMap IMPORT Treasure, Rooms, DoorStatus, IncPosition,
                   NoOfRoomsToSpring,
                   NoOfRoomsToHideCoal, NoOfRoomsToHideGrenade,
                   TreasureKind, Treasure,
                   ActualNoOfRooms ;

FROM AdvMath IMPORT MaxNoOfTreasures, LowFreePool, HighFreePool ;

FROM Screen IMPORT Width, Height,
                   ClearScreen,
                   InitScreen,
                   WriteWounds,
                   WriteWeight,
                   WriteString,
                   WriteCommentLine1,
                   WriteCommentLine2,
                   WriteCommentLine3,
                   DelCommentLine1,
                   DelCommentLine2,
                   DelCommentLine3,
                   WriteArrows, WriteMagicArrows ;

FROM AdvMath IMPORT MagicKey,
                    CrystalBall,
                    MagicSpring,
                    SackOfCoal1,
                    SackOfCoal2,
                    HotIron,
                    HandGrenade,
                    MagicSword,
                    MagicShoes,
                    SleepPotion,
                    LumpOfIron,
                    TreasTrove,
                    SpeedPotion,
                    MagicShield,
                    VisionChest,
                    QuiverNormal,
                    QuiverMagic,
                    HealingPotion,

                    UpDateWoundsAndFatigue,
                    DammageByHandGrenade,
                    DammageByHotIron ;


FROM DrawG IMPORT DrawTreasure, EraseTreasure, EraseMan, DrawMan ;
FROM DrawL IMPORT DrawRoom, DrawAllPlayers ;
FROM AdvSound IMPORT Explode ;

FROM AdvUtil IMPORT PointOnWall, GetDoorOnPoint, PointOnTreasure,
                    HideDoor, RandomRoom, PositionInRoom, InitialDisplay,
                    FreeOfPlayersAndTreasure, Dead ;



(* Treasure routines.                                                *)
(*                                                                   *)
(* The treasures are as follows:                                     *)
(*                                                                   *)
(* 1:  Magic Key           - This treasures allows one to make a     *)
(*                           closed door into a secret door          *)
(*                                                                   *)
(* 2:  Crystal Ball        - This treasure allows one to get the     *)
(*                           direction and Room No of the other      *)
(*                           players                                 *)
(*                                                                   *)
(* 3:  Magic Spring        - When Grabbed, it springs one to another *)
(*                           random picked room                      *)
(*                                                                   *)
(* 4:  Sack Of Coal        - When Grabbed, it insists that it must   *)
(*                           be taken to a randomly picked room      *)
(*                           before it can be dropped                *)
(*                                                                   *)
(* 5:  Sack Of Coal        - Ditto                                   *)
(*                                                                   *)
(* 6:  Hot Iron            - Scolds one if picked up                 *)
(*                                                                   *)
(* 7:  Hand Grenade        - If used will blow up whole room in      *)
(*                           25 seconds                              *)
(*                                                                   *)
(* 8:  Magic Sword         - Enables one to fight with ease          *)
(*                                                                   *)
(* 9:  Magic Shoes         - Enable one to run with minimal effort   *)
(*                                                                   *)
(* 10: Sleep Potion        - Makes one fall to sleep for 24 seconds  *)
(*                                                                   *)
(* 11: Lump Of Iron        - When picked up scatters all treasure in *)
(*                           current room.                           *)
(*                                                                   *)
(* 12: Treasure Trove      - Tells one where or who has the          *)
(*                           treasures.                              *)
(*                                                                   *)
(* 13: Speed Potion        - Increases ones responce time.           *)
(*                                                                   *)
(* 14: Magic Shield        - Repels Normal Arrows.                   *)
(*                                                                   *)
(* 15: Vision Chest        - Allows one to see enemies screen.       *)


CONST
   respawnStack          = 10 * 1024 * 1024 ;
   RespawnArrowTime      = 13 ;   (* seconds delay between respawning more arrow treasures 16/17.    *)
   RespawnArrowInventory = 20 ;   (* seconds delay between respawning inventory arrows after death.  *)
   RespawnMagicInventory = 30 ;   (* seconds delay between respawning inventory magic arrows after death.  *)

TYPE
   QDesc = POINTER TO RECORD
                         right: QDesc ;
                         Rm,
                         tno   : CARDINAL ;
                         kind  : TreasureKind ;
                         amount,
                         ticks : CARDINAL ;
                      END ;




VAR
   Tmessage         : ARRAY [0..13] OF CHAR ;
   SackOfCoal       : ARRAY [0..1] OF CARDINAL ;
   PinPulled        : SEMAPHORE ;
   PinHasBeenPulled : BOOLEAN ;
   PlayerPulled     : CARDINAL ;
   qHead,
   freeDesc         : QDesc ;
   armedTimer       : EVENT ;
   qAvailable,
   qMutex           : SEMAPHORE ;
   qThread          : DESCRIPTOR ;


PROCEDURE GetTreasure ;
VAR
   p, r : CARDINAL ;
   died : BOOLEAN ;
   Tno  : CARDINAL ;
BEGIN
   p := PlayerNo() ;
   GetWriteAccessToPlayer ;
   Tno := GetTreasure1(p) ;
   (* Only way so far to die directly from getting treasures.         *)
   died := (Player[p].DeathType=fireball) ;
   r := Player[p].RoomOfMan ;
   ReleaseWriteAccessToPlayer ;
   IF Tno=SleepPotion
   THEN
      (* must not sleep holding the lock.  *)
      Sleep(24*TicksPerSecond)
   END ;
   IF died
   THEN
      Dead(p, r)
   END
END GetTreasure ;


PROCEDURE GetTreasure1 (p: CARDINAL) : CARDINAL ;
VAR
   x, y, d,
   r,
   TreasNo: CARDINAL ;
   ReDraw,
   ok     : BOOLEAN ;
BEGIN
   ReDraw := FALSE ;
   WITH Player[p] DO
      d := Direction ;
      r := RoomOfMan ;
      x := Xman ;
      y := Yman ;
      IncPosition(x, y, d) ;
      GetWriteAccessToTreasure ;
      PointOnTreasure(r, x, y, TreasNo, ok) ;
      IF ok
      THEN
         IF TreasNo>9
         THEN
            Tmessage[12] := '1' ;
            Tmessage[13] := CHR((TreasNo MOD 10)+ORD('0'))
         ELSE
            Tmessage[12] := ' ' ;
            Tmessage[13] := CHR(TreasNo+ORD('0'))
         END ;
         GetAccessToScreenNo(p) ;
         WriteCommentLine1(p, Tmessage) ;
         WriteCommentLine2(p, Treasure[TreasNo].TreasureName) ;
         DelCommentLine3(p) ;
         ReleaseAccessToScreenNo(p) ;
         PickUpTreasure(p, r, TreasNo, x, y, ReDraw)
      ELSE
         GetAccessToScreenNo(p) ;
         WriteCommentLine1(p, 'thou canst') ;
         DelCommentLine2(p) ;
         DelCommentLine3(p) ;
         ReleaseAccessToScreenNo(p)
      END ;
      ReleaseWriteAccessToTreasure ;
      IF ReDraw
      THEN
         InitScreen(p) ;
         DrawRoom
      END
   END ;
   RETURN( TreasNo )
END GetTreasure1 ;


(*
   RandomDrop -
*)

PROCEDURE RandomDrop (VAR r, x, y: CARDINAL) : BOOLEAN ;
VAR
   roomCount: CARDINAL ;
   ok       : BOOLEAN ;
BEGIN
   REPEAT
      RandomNumber (roomCount, ActualNoOfRooms) ;
      INC (roomCount) ;
      RandomRoom (roomCount, NoOfRoomsToSpring, r) ;
      PositionInRoom (r, x, y, ok)
   UNTIL ok ;
   RETURN TRUE
END RandomDrop ;


(*
   IsTreasureArrow - returns TRUE if treasure, i, is an arrow (normal or magic)
                     it can be a respawnable treasure or an item dropped after
                     death.
*)

PROCEDURE IsTreasureArrow (i: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (i = QuiverNormal) OR (i = QuiverMagic) OR (i >= LowFreePool)
END IsTreasureArrow ;


PROCEDURE PickUpTreasure (p, r, TreasNo, tx, ty: CARDINAL ;
                          VAR ReDraw: BOOLEAN) ;
VAR
   tr, i: CARDINAL ;
   ok   : BOOLEAN ;
   a    : ARRAY [0..14] OF CHAR ;
   b    : ARRAY [0..4] OF CHAR ;
BEGIN
   WITH Player[p] DO
      (* Magic Spring CANNOT be Grabbed AND neither can Lump Of Iron *)
      IF (TreasNo#MagicSpring) AND (TreasNo#LumpOfIron) AND
         (NOT IsTreasureArrow (TreasNo))
      THEN
         pickUp (p, r, TreasNo) ;
         GetAccessToScreenNo (p) ;
         WriteWeight (p, Weight) ;
         ReleaseAccessToScreenNo (p) ;
      END ;
      EXCL (Rooms[r].Treasures, TreasNo) ; (* Room no longer has treasure *)
      EraseTreasure(r, tx, ty) ;

      IF TreasNo=MagicSpring
      THEN
         (* Magic Spring - Springs treasure and player into different      *)
         (*                rooms.                                          *)

         EraseMan(p) ;
         REPEAT
            RandomNumber(r, ActualNoOfRooms) ;  (* r>=0 & r<=ActualNoOfRooms-1 *)
            INC(r) ;
            RandomRoom(r, NoOfRoomsToSpring, tr) ;
            PositionInRoom(tr, tx, ty, ok)
         UNTIL ok ;
         RoomOfMan := tr ;
         Xman := tx ;
         Yman := ty ;
         ScreenX := tx-(tx MOD Width) ;
         ScreenY := ty-(ty MOD Height) ;
         DrawMan(p) ;
         IF RandomDrop (tr, tx, ty)
         THEN
            WITH Treasure[TreasNo] DO
               Rm := tr ;
               Xpos := tx ;
               Ypos := ty
            END
         END ;
         INCL(Rooms[tr].Treasures, TreasNo) ; (* Room has treasure *)
         DrawTreasure(tr, tx, ty) ;
         ReDraw := TRUE
      ELSIF (TreasNo=SackOfCoal1) OR (TreasNo=SackOfCoal2)  (* Sacks Of Coal *)
      THEN
         RandomNumber(r, ActualNoOfRooms) ;
         INC(r) ;
         RandomRoom(r, NoOfRoomsToHideCoal, tr) ;
         SackOfCoal[TreasNo-SackOfCoal1] := tr ;
         StrCopy('to room ', a ) ;
         CardToStr(tr, 4, b) ;
         StrConCat(a, b, a) ;
         GetAccessToScreenNo(p) ;
         WriteCommentLine3(p, a) ;
         ReleaseAccessToScreenNo(p)
      ELSIF TreasNo=HotIron             (* Hot iron      *)
      THEN
         GetAccessToScreenNo(p) ;
         WriteCommentLine1(p, 'ouch') ;
         WriteCommentLine2(p, 'fire ball') ;
         WriteCommentLine3(p, 'hit thee') ;
         IF DammageByHotIron>Wounds
         THEN
            Wounds := 0 ;
            DeathType := fireball
         ELSE
            DEC(Wounds, DammageByHotIron)
         END ;
         WriteWounds(p, Wounds) ;
         ReleaseAccessToScreenNo(p)
      ELSIF TreasNo=LumpOfIron
      THEN
         ScatterAllTreasures(p, RoomOfMan)
      ELSIF TreasNo=SpeedPotion
      THEN
         (* PutPriority(CurrentProcess, User, 4) *)
      ELSIF (TreasNo=QuiverNormal) OR (TreasNo=QuiverMagic)
      THEN
         IF TreasNo=QuiverNormal
         THEN
            INC (NoOfNormal, 6) ;
            GetAccessToScreenNo (p) ;
            WriteArrows (p, NoOfNormal) ;
            WriteCommentLine1 (p, '6 normal') ;
            WriteCommentLine2 (p, 'arrows') ;
            ReleaseAccessToScreenNo (p) ;
            Treasure[TreasNo].kind := respawnnormal
         ELSE
            INC (NoOfMagic, 1) ;
            GetAccessToScreenNo (p) ;
            WriteMagicArrows (p, NoOfMagic) ;
            WriteCommentLine1 (p, 'a magic') ;
            WriteCommentLine2 (p, 'arrow') ;
            ReleaseAccessToScreenNo (p) ;
            Treasure[TreasNo].kind := respawnmagic
         END ;
         RespawnTreasure (tr, TreasNo, RespawnArrowTime * TicksPerSecond) ;
      ELSIF IsTreasureArrow (TreasNo)
      THEN
         GetAccessToScreenNo (p) ;
         IF Treasure[TreasNo].kind = normal
         THEN
            INC (NoOfNormal, Treasure[TreasNo].amount) ;
            WriteArrows (p, NoOfNormal) ;
         ELSIF Treasure[TreasNo].kind = magic
         THEN
            INC (NoOfMagic, Treasure[TreasNo].amount) ;
            WriteMagicArrows (p, NoOfMagic) ;
         END ;
         Treasure[TreasNo].amount := 0 ;
         Treasure[TreasNo].Rm := 0 ;
         Treasure[TreasNo].kind := unused ;
         ReleaseAccessToScreenNo (p)
      END
   END
END PickUpTreasure ;


PROCEDURE ScatterAllTreasures (p, r: CARDINAL) ;
VAR
   tp, tr,
   x, y, i : CARDINAL ;
   ok      : BOOLEAN ;
BEGIN
   FOR i := 1 TO MaxNoOfTreasures DO
      FOR tp := 0 TO NextFreePlayer-1 DO
         IF IsPlayerActive(tp)
         THEN
            WITH Player[tp] DO
               IF (i IN TreasureOwn) AND (r=RoomOfMan)
               THEN
                  REPEAT
                     RandomRoom(r, NoOfRoomsToSpring, tr) ;
                     PositionInRoom(tr, x, y, ok)
                  UNTIL ok ;
                  putDown (tp, tr, i, x, y) ;
                  printf ("treasure %d is in room %d at %d,%d\n", i, tr, x, y)
               END
            END
         END
      END ;
      IF Treasure[i].Rm=r
      THEN
         REPEAT
            RandomRoom(r, NoOfRoomsToSpring, tr) ;
            PositionInRoom(tr, x, y, ok)
         UNTIL ok ;
         printf ("treasure %d is in room %d at %d,%d\n", i, tr, x, y) ;
         WITH Treasure[i] DO
            EraseTreasure (Rm, Xpos, Ypos) ;
            EXCL (Rooms[Rm].Treasures, i) ;
            Xpos := x ;
            Ypos := y ;
            Rm := tr
         END ;
         INCL (Rooms[tr].Treasures, i)
      END ;
      WITH Treasure[i] DO
         DrawTreasure (Rm, Xpos, Ypos)
      END
   END ;
   FOR tp := 0 TO NextFreePlayer-1 DO
      IF IsPlayerActive(tp)
      THEN
         WITH Player[tp] DO
            IF (TreasureOwn#{}) AND (RoomOfMan=r)
            THEN
               (* Now undo treasures which have an automatic effect *)
               IF SpeedPotion IN TreasureOwn
               THEN
                  (* PutPriority(PlayerProcess(p), User, 3) *)
               END ;

               TreasureOwn := {} ;
               GetAccessToScreenNo(tp) ;

               WriteWeight(p, Weight) ;
               WriteCommentLine1(p, 'thy burdens') ;
               WriteCommentLine2(p, 'hast been') ;
               WriteCommentLine3(p, 'lifted') ;

               ReleaseAccessToScreenNo(tp)
            END
         END
      END
   END
END ScatterAllTreasures ;


PROCEDURE ScatterTreasures (p, r: CARDINAL) ;
VAR
   c       : INTEGER ;
   x, y, i : CARDINAL ;
   ok      : BOOLEAN ;
BEGIN
   WITH Player[p] DO
      FOR i := 1 TO MaxNoOfTreasures DO
         IF i IN TreasureOwn
         THEN
            (* Now undo treasures which have an automatic effect *)
            IF SpeedPotion IN TreasureOwn
            THEN
               (* PutPriority(PlayerProcess(p), User, 3) *)
            END ;

            REPEAT
               PositionInRoom(r, x, y, ok) ;
               IF ok
               THEN
                  WITH Treasure[i] DO
                     DEC(Weight, Tweight) ;
                     Xpos := x ;
                     Ypos := y ;
                     Rm := r
                  END ;
                  DrawTreasure(r, x, y) ;
                  INCL(Rooms[r].Treasures, i) ;
               ELSE
                  c := printf('trying another room\n') ;
                  RandomRoom(r, 1, x) ;
                  r := x
               END
            UNTIL ok
         END
      END ;
      TreasureOwn := {} ;
      (* and respawn arrows.  *)
      RespawnArrow (r, 0, respawnmagic, magic, NoOfMagic, TicksPerSecond * RespawnMagicInventory) ;
      RespawnArrow (r, 0, respawnnormal, normal, NoOfNormal, TicksPerSecond * RespawnArrowInventory) ;
      GetAccessToScreenNo(p) ;
      WriteWeight(p, Weight) ;
      ReleaseAccessToScreenNo(p)
   END
END ScatterTreasures ;


PROCEDURE DropTreasure ;
VAR
   ok        : BOOLEAN ;
   p, TreasNo: CARDINAL ;
   ch,
   units,
   tens      : CHAR ;
BEGIN
   p := PlayerNo() ;
   GetAccessToScreenNo(p) ;
   WriteCommentLine2(p, 'which one?') ;
   ReleaseAccessToScreenNo(p) ;
   ch := ' ' ;
   units := ' ' ;
   tens := ' ' ;
   REPEAT
      tens := units ;
      units := ch ;
      ok := ClientRead(ch)
   UNTIL (NOT ok) OR (ch=cr) ;
   IF ok
   THEN
      IF (units>='0') AND (units<='9')
      THEN
         TreasNo := ORD(units)-ORD('0') ;
         IF (tens>='0') AND (tens<='9')
         THEN
            TreasNo := TreasNo+10*(ORD(tens)-ORD('0'))
         END
      END ;
      IF (TreasNo<1) OR (TreasNo>MaxNoOfTreasures)
      THEN
         GetAccessToScreenNo(p) ;
         WriteCommentLine1(p, 'thou canst') ;
         DelCommentLine2(p) ;
         DelCommentLine3(p);
         ReleaseAccessToScreenNo(p)
      ELSE
         GetWriteAccessToPlayer ;
         DropTreasure1(p, TreasNo) ;
         ReleaseWriteAccessToPlayer
      END
   END
END DropTreasure ;


PROCEDURE DropTreasure1(p, TreasNo: CARDINAL) ;
VAR
   x, y, d,
   r, z   : CARDINAL ;
   ok     : BOOLEAN ;
BEGIN
   WITH Player[p] DO
      IF TreasNo IN TreasureOwn
      THEN
         d := Direction ;
         r := RoomOfMan ;
         x := Xman ;
         y := Yman ;
         IncPosition(x, y, d) ;
         PointOnWall(r, x, y, ok) ;
         IF NOT ok
         THEN
            GetDoorOnPoint(r, x, y, z, ok) ;
            IF NOT ok
            THEN
               GetWriteAccessToTreasure ;
               FreeOfPlayersAndTreasure(r, x, y, ok) ;
               IF ok
               THEN
                  IF TreasNo>9
                  THEN
                     Tmessage[12] := '1' ;
                     Tmessage[13] := CHR((TreasNo MOD 10)+ORD('0'))
                  ELSE
                     Tmessage[12] := ' ' ;
                     Tmessage[13] := CHR(TreasNo+ORD('0'))
                  END ;
                  GetAccessToScreenNo(p) ;
                  WriteCommentLine1(p, Tmessage) ;
                  WriteCommentLine2(p, Treasure[TreasNo].TreasureName) ;
                  ReleaseAccessToScreenNo( p ) ;
                  PutDownTreasure(p, r, TreasNo, x, y)
               ELSE
                  GetAccessToScreenNo(p) ;
                  WriteCommentLine1(p, 'thou canst') ;
                  DelCommentLine2(p) ;
                  DelCommentLine3(p) ;
                  ReleaseAccessToScreenNo(p)
               END ;
               ReleaseWriteAccessToTreasure
            ELSE
               GetAccessToScreenNo(p) ;
               WriteCommentLine1(p, 'thou canst') ;
               DelCommentLine2(p) ;
               DelCommentLine3(p) ;
               ReleaseAccessToScreenNo(p)
            END
         ELSE
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'thou canst') ;
            DelCommentLine2(p) ;
            DelCommentLine3(p) ;
            ReleaseAccessToScreenNo(p)
         END
      ELSE
         GetAccessToScreenNo(p) ;
         WriteCommentLine1(p, 'thou canst') ;
         DelCommentLine2(p) ;
         DelCommentLine3(p) ;
         ReleaseAccessToScreenNo(p)
      END
   END
END DropTreasure1 ;


(*
   putDown -
*)

PROCEDURE putDown (p, r, TreasNo, tx, ty: CARDINAL) ;
BEGIN
   WITH Player[p] DO
      DEC (Weight, Treasure[TreasNo].Tweight) ;
      Treasure[TreasNo].Rm := r ;    (* Put in this Room *)
      Treasure[TreasNo].Xpos := tx ;
      Treasure[TreasNo].Ypos := ty ;
      Treasure[TreasNo].kind := onfloor ;
      INCL (Rooms[r].Treasures, TreasNo) ; (* Room has treasure             *)
      EXCL (TreasureOwn, TreasNo) ;  (* Player no longer has treasure *)
   END
END putDown ;


(*
   pickUp -
*)

PROCEDURE pickUp (p, r, TreasNo: CARDINAL) ;
BEGIN
   WITH Player[p] DO
      INC (Weight, Treasure[TreasNo].Tweight) ;
      Treasure[TreasNo].Rm := 0 ; (* No longer in a Room *)
      Treasure[TreasNo].kind := onperson ; (* No longer in a Room *)
      INCL (TreasureOwn, TreasNo)
   END
END pickUp ;


PROCEDURE PutDownTreasure (p, r, TreasNo, tx, ty: CARDINAL) ;
VAR
   tr, i: CARDINAL ;
   ok   : BOOLEAN ;
   a    : ARRAY [0..14] OF CHAR ;
   b    : ARRAY [0..4] OF CHAR ;
BEGIN
   WITH Player[p] DO
      IF (TreasNo=SackOfCoal1) OR (TreasNo=SackOfCoal2)  (* Sacks Of Coal *)
      THEN
         IF r=SackOfCoal[TreasNo-SackOfCoal1]
         THEN
            GetAccessToScreenNo(p) ;
            WriteWeight(p, Weight) ;
            WriteCommentLine3(p, 'dropped') ;
            ReleaseAccessToScreenNo(p) ;
            putDown (p, r, TreasNo, tx, ty) ;
            DrawTreasure(r, tx, ty)
         ELSE
            StrCopy('to room ', a) ;
            CardToStr(SackOfCoal[TreasNo-SackOfCoal1], 4, b) ;
            StrConCat(a, b, a) ;
            GetAccessToScreenNo(p) ;
            WriteCommentLine3(p, a) ;
            ReleaseAccessToScreenNo(p)
         END
      ELSE
         IF TreasNo=SpeedPotion
         THEN
            (* PutPriority(CurrentProcess, User, 3) *)
         END ;
         putDown (p, r, TreasNo, tx, ty) ;
         GetAccessToScreenNo(p) ;
         WriteCommentLine3(p, 'dropped') ;
         WriteWeight(p, Weight) ;
         ReleaseAccessToScreenNo(p) ;
         DrawTreasure(r, tx, ty)
      END
   END
END PutDownTreasure ;


PROCEDURE UseTreasure ;
VAR
   x, y, d,
   r, p,
   TreasNo: CARDINAL ;
   ok     : BOOLEAN ;
   ch,
   units,
   tens   : CHAR ;
BEGIN
   p := PlayerNo() ;
   WITH Player[p] DO
      GetAccessToScreenNo(p) ;
      WriteCommentLine2(p, 'which one?') ;
      ReleaseAccessToScreenNo(p) ;
      ch := ' ' ;
      units := ' ' ;
      tens := ' ' ;
      REPEAT
         tens := units ;
         units := ch ;
         ok := ClientRead(ch)
      UNTIL (NOT ok) OR (ch=cr) ;
      IF ok
      THEN
         IF (units>='0') AND (units<='9')
         THEN
            TreasNo := ORD(units)-ORD('0') ;
            IF (tens>='0') AND (tens<='9')
            THEN
               TreasNo := TreasNo+10*(ORD(tens)-ORD('0'))
            END
         END ;
         GetReadAccessToPlayer ;
         IF (TreasNo<1) OR (TreasNo>MaxNoOfTreasures)
         THEN
            ReleaseReadAccessToPlayer ;
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'thou canst') ;
            DelCommentLine2(p) ;
            DelCommentLine3(p) ;
            ReleaseAccessToScreenNo(p)
         ELSIF TreasNo IN TreasureOwn
         THEN
            ReleaseReadAccessToPlayer ;
            IF TreasNo>9
            THEN
               Tmessage[12] := tens ;
               Tmessage[13] := units
            ELSE
               Tmessage[12] := ' ' ;
               Tmessage[13] := units
            END ;
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'using') ;
            WriteCommentLine2(p, Tmessage) ;
            WriteCommentLine3(p, Treasure[TreasNo].TreasureName) ;
            ReleaseAccessToScreenNo(p) ;
            IF TreasNo=MagicKey        (* Magic Key *)
            THEN
               HideDoor
            ELSIF TreasNo=CrystalBall  (* Crystal Ball *)
            THEN
               UseCrystalBall
            ELSIF TreasNo=HandGrenade  (* Hand Grenade *)
            THEN
               PullPin
            ELSIF TreasNo=TreasTrove
            THEN
               DisplayTreasures
            ELSIF TreasNo=VisionChest
            THEN
               DisplayEnemy
            END
         ELSE
            ReleaseReadAccessToPlayer ;
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'thou canst') ;
            DelCommentLine2(p) ;
            DelCommentLine3(p) ;
            ReleaseAccessToScreenNo(p)
         END
      END
   END
END UseTreasure ;


PROCEDURE UseCrystalBall ;
VAR
   p, x, y,
   px, py,
   r, i : CARDINAL ;
   a    : ARRAY [0..14] OF CHAR ;
   b    : ARRAY [0..4]  OF CHAR ;
   who  : ARRAY [0..1] OF CARDINAL ;
   first: BOOLEAN ;
   ch   : CHAR ;
BEGIN
(*
   p := PlayerNo() ;
   first := TRUE ;
   FOR i := 0 TO MaxNoOfPlayers DO
      IF i#p
      THEN
         IF first
         THEN
            who[0] := i ;
            first := FALSE
         ELSE
            who[1] := i
         END
      END
   END ;
   GetReadAccessToPlayerNo( p ) ;
   WITH Player[p] DO
      px := Xman ;
      py := Yman
   END ;
   ReleaseReadAccessToPlayerNo( p ) ;
   GetReadAccessToPlayerNo( who[0] ) ;
   StrConCat('1: ', Player[who[0]].ManName, a ) ;
   ReleaseReadAccessToPlayerNo( who[0] ) ;
   GetAccessToScreenNo( p ) ;
   WriteCommentLine1(p, a) ;
   ReleaseAccessToScreenNo( p ) ;
   GetReadAccessToPlayerNo( who[1] ) ;
   StrConCat('2: ', Player[who[1]].ManName, a ) ;
   ReleaseReadAccessToPlayerNo( who[1] ) ;
   GetAccessToScreenNo( p ) ;
   WriteCommentLine2(p, a) ;
   WriteCommentLine3(p, 'peer at ?') ;
   ReleaseAccessToScreenNo( p ) ;
   REPEAT
      Read( ch ) ;
      IF (ch='1') OR (ch='2')
      THEN
         i := ORD(ch)-ORD('1') ;
         GetReadAccessToPlayerNo( who[i] ) ;
         WITH Player[who[i]] DO
            x := Xman ;
            y := Yman ;
            r := RoomOfMan
         END ;
         ReleaseReadAccessToPlayerNo( who[i] ) ;
         IF r=0
         THEN
            StrCopy('is slain: ', a )
         ELSE
            StrCopy('room', a) ;
            CardToStr( r, 4, b ) ;
            StrConCat( a, b, a ) ;
            StrConCat( a, ' ', a )
         END ;
         IF y>py
         THEN
            StrConCat( a, 'S', a )
         END ;
         IF y<py
         THEN
            StrConCat( a, 'N', a )
         END ;
         IF x>px
         THEN
            StrConCat( a, 'E', a )
         END ;
         IF x<px
         THEN
            StrConCat( a, 'W', a )
         END ;
         GetAccessToScreenNo( p ) ;
         IF ch='1'
         THEN
            WriteCommentLine1(p, a)
         ELSE
            WriteCommentLine2(p, a)
         END ;
         ReleaseAccessToScreenNo( p )
      END
   UNTIL (ch#'1') AND (ch#'2') ;
   GetAccessToScreenNo( p ) ;
   DelCommentLine1(p) ;
   DelCommentLine2(p) ;
   DelCommentLine3(p) ;
   ReleaseAccessToScreenNo( p )
*)
END UseCrystalBall ;

(*
PROCEDURE DisplayWounds ;
VAR
   p, w,
   r, i : CARDINAL ;
   b    : ARRAY [0..4]  OF CHAR ;
   a    : ARRAY [0..14] OF CHAR ;
   who  : ARRAY [0..1] OF CARDINAL ;
   first: BOOLEAN ;
   ch   : CHAR ;
BEGIN
   p := PlayerNo() ;
   first := TRUE ;
   FOR i := 0 TO MaxNoOfPlayers DO
      IF i#p
      THEN
         IF first
         THEN
            who[0] := i ;
            first := FALSE
         ELSE
            who[1] := i
         END
      END
   END ;
   GetReadAccessToPlayerNo( who[0] ) ;
   StrConCat('1: ', Player[who[0]].ManName, a ) ;
   ReleaseReadAccessToPlayerNo( who[0] ) ;
   GetAccessToScreenNo( p ) ;
   WriteCommentLine1(p, a) ;
   ReleaseAccessToScreenNo( p ) ;
   GetReadAccessToPlayerNo( who[1] ) ;
   StrConCat('2: ', Player[who[1]].ManName, a ) ;
   ReleaseReadAccessToPlayerNo( who[1] ) ;
   GetAccessToScreenNo( p ) ;
   WriteCommentLine2(p, a) ;
   WriteCommentLine3(p, 'peer at ?') ;
   ReleaseAccessToScreenNo( p ) ;
   REPEAT
      Read( ch ) ;
      IF (ch='1') OR (ch='2')
      THEN
         i := ORD(ch)-ORD('1') ;
         GetReadAccessToPlayerNo( who[i] ) ;
         WITH Player[who[i]] DO
            w := Wounds ;
            r := RoomOfMan
         END ;
         ReleaseReadAccessToPlayerNo( who[i] ) ;
         IF r=0
         THEN
            StrCopy('is slain: ', a )
         ELSE
            StrCopy('Wounds ', a) ;
            CardToStr( w, 4, b ) ;
            StrConCat( a, b, a ) ;
            StrConCat( a, ' ', a )
         END ;
         GetAccessToScreenNo( p ) ;
         IF ch='1'
         THEN
            WriteCommentLine1(p, a)
         ELSE
            WriteCommentLine2(p, a)
         END ;
         ReleaseAccessToScreenNo( p )
      END
   UNTIL (ch#'1') AND (ch#'2') ;
   GetAccessToScreenNo( p ) ;
   DelCommentLine1(p) ;
   DelCommentLine2(p) ;
   DelCommentLine3(p) ;
   ReleaseAccessToScreenNo( p )
END DisplayWounds ;
*)

PROCEDURE DisplayEnemy ;
VAR
   p,
   r, i : CARDINAL ;
   who  : ARRAY [0..1] OF CARDINAL ;
   a    : ARRAY [0..14] OF CHAR ;
   first: BOOLEAN ;
   ch   : CHAR ;
BEGIN
(*
   p := PlayerNo() ;
   first := TRUE ;
   FOR i := 0 TO MaxNoOfPlayers DO
      IF i#p
      THEN
         IF first
         THEN
            who[0] := i ;
            first := FALSE
         ELSE
            who[1] := i
         END
      END
   END ;
   REPEAT
      GetReadAccessToPlayerNo( who[0] ) ;
      StrConCat('1: ', Player[who[0]].ManName, a ) ;
      ReleaseReadAccessToPlayerNo( who[0] ) ;
      GetAccessToScreenNo( p ) ;
      WriteCommentLine1(p, a) ;
      ReleaseAccessToScreenNo( p ) ;
      GetReadAccessToPlayerNo( who[1] ) ;
      StrConCat('2: ', Player[who[1]].ManName, a ) ;
      ReleaseReadAccessToPlayerNo( who[1] ) ;
      GetAccessToScreenNo( p ) ;
      WriteCommentLine2(p, a) ;
      WriteCommentLine3(p, 'peer at ?') ;
      ReleaseAccessToScreenNo( p ) ;
      Read( ch ) ;
      IF (ch='1') OR (ch='2')
      THEN
         i := ORD(ch)-ORD('1') ;
         GetReadAccessToPlayerNo( who[i] ) ;
         WITH Player[who[i]] DO
            r := RoomOfMan
         END ;
         ReleaseReadAccessToPlayerNo( who[i] ) ;
         IF r=0
         THEN
            GetAccessToScreenNo( p ) ;
            IF ch='1'
            THEN
               WriteCommentLine1(p, 'is slain:')
            ELSE
               WriteCommentLine2(p, 'is slain:')
            END ;
            ReleaseAccessToScreenNo( p )
         ELSE
            DisplayEn( p, who[i] )
         END
      END
   UNTIL (ch#'1') AND (ch#'2') ;
   GetAccessToScreenNo( p ) ;
   DelCommentLine1(p) ;
   DelCommentLine2(p) ;
   DelCommentLine3(p) ;
   ReleaseAccessToScreenNo( p )
*)
END DisplayEnemy ;


PROCEDURE DisplayEn (p, e: CARDINAL) ;
VAR
   OldMan: Man ;
   ch    : CHAR ;
BEGIN
(* ******************
   (* Save player p man first *)
   GetWriteAccessToAllPlayers ;
   OldMan := Player[p] ;
   Player[p] := Player[e] ;
   (* Now draw Screen etc *)
   InitScreen ;
   DrawRoom ;
   DrawAllPlayers ;
   Player[p] := OldMan ;
   ReleaseWriteAccessToAllPlayers ;
   Read( ch ) ;
   IF Player[p].RoomOfMan#0   (* So alive - or just killed hopefully... *)
   THEN
      InitialDisplay
   END
********************** *)
END DisplayEn ;


PROCEDURE DisplayTreasures ;
VAR
   p, tp,
   i, j : CARDINAL ;
   ok   : BOOLEAN ;
   ch   : CHAR ;
   no   : ARRAY [0..3] OF CHAR ;
   line : ARRAY [0..80] OF CHAR ;
BEGIN
   p := PlayerNo() ;
   GetReadAccessToPlayer ;
   GetReadAccessToTreasure ;
   GetAccessToScreenNo(p) ;
   ClearScreen(p) ;
   FOR i := 1 TO MaxNoOfTreasures DO
      ok := FALSE ;
      FOR tp := 0 TO MaxNoOfPlayers DO
         IF IsPlayerActive(tp)
         THEN
            WITH Player[tp] DO
               IF i IN TreasureOwn
               THEN
                  StrCopy(ManName, line) ;
                  StrConCat(line, ' ', line) ;
                  ok := TRUE
               END
            END
         END
      END ;
      IF (Treasure[i].Rm # 0) AND (Treasure[i].kind = onfloor)
      THEN
         IF (NOT ok)
         THEN
            CardToStr(Treasure[i].Rm, 6, line) ;
            StrConCat(' ', line, line) ;
            StrConCat('Room Number', line, line) ;
         END ;
         StrConCat(line, ' has Treasure No ', line) ;
         CardToStr(i, 0, no) ;
         StrConCat(line, no, line) ;
         StrConCat(line, ' The ', line) ;
         StrConCat(line, Treasure[i].TreasureName, line) ;
         WriteString(p, line)
      END
   END ;
   ReleaseReadAccessToPlayer ;
   ReleaseReadAccessToTreasure ;
   ReleaseAccessToScreenNo(p) ;
   IF ClientRead(ch)
   THEN
   END ;
   InitialDisplay
END DisplayTreasures ;


PROCEDURE PullPin ;
VAR
   p: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   GetWriteAccessToTreasure ;
   IF PinHasBeenPulled
   THEN
      ReleaseWriteAccessToTreasure ;
      GetAccessToScreen ;
      WriteCommentLine1(p, 'pin has been') ;
      WriteCommentLine2(p, 'pulled') ;
      DelCommentLine3(p) ;
      ReleaseAccessToScreen
   ELSE
      PinHasBeenPulled := TRUE ;
      PlayerPulled := p ;
      ReleaseWriteAccessToTreasure ;
      Signal(PinPulled)
   END
END PullPin ;


PROCEDURE Grenade ;
VAR
   pulled,
   RoomOfExplosion,
   sec, i,
   start : CARDINAL ;
   hit,
   ok    : BOOLEAN ;
   SlainP: PlayerSet ;
BEGIN
   LOOP
      Wait(PinPulled) ;
      pulled := PlayerPulled ;
      Sleep(25*TicksPerSecond) ;

      (* Ok now explode! *)

      hit := FALSE ;
      GetWriteAccessToPlayer ;
      GetWriteAccessToTreasure ;

      (* Find out where grenade is! *)

      WITH Treasure[HandGrenade] DO
         IF Rm=0
         THEN
            i := 0 ;
            RoomOfExplosion := 0 ;
            REPEAT
               IF IsPlayerActive(i)
               THEN
                  WITH Player[i] DO
                     IF HandGrenade IN TreasureOwn
                     THEN
                        RoomOfExplosion := RoomOfMan ;
                        DEC(Weight, Tweight) ;
                        EXCL(TreasureOwn, HandGrenade) ;
                        GetAccessToScreenNo(i) ;
                        WriteWeight(i, Weight) ;
                        ReleaseAccessToScreenNo(i)
                     END
                  END
               END ;
               INC(i)
            UNTIL (RoomOfExplosion#0) OR (i=NextFreePlayer)
         ELSE
            RoomOfExplosion := Rm ;
            EXCL(Rooms[Rm].Treasures, HandGrenade) ;
            EraseTreasure(Rm, Xpos, Ypos)
         END
      END ;

      IF RoomOfExplosion # 0
      THEN
         SlainP := PlayerSet{} ;
         FOR i := 0 TO NextFreePlayer-1 DO
            IF IsPlayerActive(i)
            THEN
               WITH Player[i] DO
                  IF RoomOfExplosion=RoomOfMan
                  THEN
                     hit := TRUE ;
                     GetAccessToScreenNo(i) ;
                     UpDateWoundsAndFatigue(i) ;
                     WriteCommentLine1(i, 'boooommm') ;
                     DelCommentLine2(i) ;
                     DelCommentLine3(i) ;
                     IF Wounds>DammageByHandGrenade
                     THEN
                        DEC(Wounds, DammageByHandGrenade) ;
                     ELSE
                        INCL(SlainP, i) ;
                        Wounds := 0 ;
                        DeathType := explosion
                     END ;
                     WriteWounds(i, Wounds) ;
                     ReleaseAccessToScreenNo(i)
                  END
               END
            END
         END
      END ;
      ReleaseWriteAccessToTreasure ;
      ReleaseWriteAccessToPlayer ;
      IF RoomOfExplosion # 0
      THEN
         Explode(RoomOfExplosion, pulled, hit) ;
         FOR i := 0 TO MaxNoOfPlayers DO
            IF i IN SlainP
            THEN
               Dead(i, RoomOfExplosion)
            END
         END
      END ;

      (* and hide the grenade again.  *)
      GetWriteAccessToPlayer ;
      GetWriteAccessToTreasure ;
      WITH Treasure[HandGrenade] DO
         REPEAT
            RandomRoom(RoomOfExplosion, NoOfRoomsToHideGrenade, Rm) ;
            PositionInRoom(Rm, Xpos, Ypos, ok)
         UNTIL ok ;
         INCL(Rooms[Rm].Treasures, HandGrenade) ;
         DrawTreasure(Rm, Xpos, Ypos)
      END ;
      PinHasBeenPulled := FALSE ;
      ReleaseWriteAccessToTreasure ;
      ReleaseWriteAccessToPlayer ;
   END
END Grenade ;


(*
   newQDesc -
*)

PROCEDURE newQDesc (Rm, tno: CARDINAL; kind: TreasureKind; amount, ticks: CARDINAL) : QDesc ;
VAR
   d: QDesc ;
BEGIN
   IF freeDesc = NIL
   THEN
      NEW (d)
   ELSE
      d := freeDesc ;
      freeDesc := freeDesc^.right
   END ;
   d^.Rm := Rm ;
   d^.tno := tno ;
   d^.kind := kind ;
   d^.amount := amount ;
   d^.ticks := ticks ;
   d^.right := NIL ;
   RETURN d
END newQDesc ;


(*
   freeQ - return the head desc to the freeQ.
*)

PROCEDURE freeQ ;
VAR
   desc: QDesc ;
BEGIN
   desc := qHead ;
   qHead := qHead^.right ;
   desc^.right := freeDesc ;
   freeDesc := desc
END freeQ ;


(*
   respawnThread -
*)

PROCEDURE respawnThread ;
VAR
   desc: QDesc ;
BEGIN
   LOOP
      printf ("respawnThread\n");
      Wait (qAvailable) ;
      REPEAT
         Wait (qMutex) ;
         armedTimer := ArmEvent (qHead^.ticks) ;
         Signal (qMutex) ;
      UNTIL NOT WaitOn (armedTimer) ;
      printf ("respawnThread has waited\n");
      Wait (qMutex) ;
      IF qHead # NIL
      THEN
         WITH qHead^ DO
            IF tno < LowFreePool
            THEN
               RespawnTreasure (Rm, tno, 0)
            ELSE
               (* as the time is zero there is no need for a respawn kind.  *)
               RespawnArrow (Rm, tno, kind, kind, amount, 0)
            END
         END
      END ;
      freeQ ;
      Signal (qMutex)
   END
END respawnThread ;


(*
   relativeAdd -
*)

PROCEDURE relativeAdd (desc: QDesc) ;
VAR
   s, t: QDesc ;
   sum : CARDINAL ;
BEGIN
(* works - ish...
   qHead := desc ;
   desc^.right := NIL ;
   RETURN ;
*)
   IF qHead = NIL
   THEN
      (* simple as the queue is empty (relative=absolute).  *)
      qHead := desc ;
      desc^.right := NIL
   ELSE
      (* at the end of the while loop sum will contain the total of all
         events up to but not including, t.
         If the value of sum is <  e^.NoOfTicks then e must be placed at the end
                                >= e^.NoOfTicks then e needs to be placed in the middle
      *)

      sum := qHead^.ticks ;
      s := qHead ;
      t := qHead^.right ;      (* second event *)
      WHILE (sum < desc^.ticks) AND (t # NIL) DO
         INC (sum, t^.ticks) ;
         s := t ;
         t := t^.right
      END ;
      IF sum < desc^.ticks
      THEN
         (* desc will occur after all the current qHead has expired therefore
            we must add it to the end of the qHead. *)
         DEC (desc^.ticks, sum) ;
         s^.right:= desc ;
         desc^.right := NIL
      ELSE
         (* as sum >= desc^.ticks we know that desc is scheduled to occur
            in the middle of the queue but after, s.
         *)
         Assert (sum >= s^.ticks) ;
         DEC (desc^.ticks, sum - s^.ticks) ;
         desc^.right := t ;
         s^.right := desc
      END ;
      (* the first event after desc must have its relative ticks altered.  *)
      IF desc^.right # NIL
      THEN
         DEC (desc^.right^.ticks, desc^.ticks)
      END
   END
END relativeAdd ;


(*
   addToQueue -
*)

PROCEDURE addToQueue (seedRoom: CARDINAL; tno: CARDINAL; kind: TreasureKind;
                      amount: CARDINAL; ticks: CARDINAL) ;
VAR
   desc: QDesc ;
BEGIN
   Wait (qMutex) ;
   desc := newQDesc (seedRoom, tno, kind, amount, ticks) ;
   relativeAdd (desc) ;
   IF armedTimer = NIL
   THEN
      armedTimer := ArmEvent (desc^.ticks)
   ELSE
      IF Cancel (armedTimer)
      THEN
      END ;
      armedTimer := ArmEvent (desc^.ticks)
   END ;
   Signal (qAvailable) ;
   Signal (qMutex) ;
   IF qThread = NIL
   THEN
      qThread := Resume (InitProcess (respawnThread, respawnStack, "respawnThread"))
   END
END addToQueue ;


PROCEDURE RespawnTreasure (seedRoom: CARDINAL; tno: CARDINAL; ticks: CARDINAL) ;
VAR
   x, y: CARDINAL ;
BEGIN
   IF ticks = 0
   THEN
      randomPlace (seedRoom, tno, onfloor)
   ELSE
      addToQueue (seedRoom, tno, onfloor, 0, ticks)
   END
END RespawnTreasure ;


(*
   findSpareTreasure -
*)

PROCEDURE findSpareTreasure (newkind: TreasureKind) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   i := LowFreePool ;
   WHILE i <= HighFreePool DO
      IF Treasure[i].kind = unused
      THEN
         Treasure[i].kind := newkind ;
         RETURN i
      END ;
      INC (i)
   END ;
   RETURN 0
END findSpareTreasure ;


(*
   randomPlace -
*)

PROCEDURE randomPlace (seedRoom: CARDINAL; tno: CARDINAL; k: TreasureKind) ;
VAR
   x, y: CARDINAL ;
BEGIN
   IF RandomDrop (seedRoom, x, y)
   THEN
      WITH Treasure[tno] DO
         Rm := seedRoom ;
         Xpos := x ;
         Ypos := y ;
         kind := k ;
      END ;
      IF (tno >= LowFreePool) AND (tno <= HighFreePool)
      THEN
         IF k = magic
         THEN
            StrCopy ('Magic Arrows', Treasure[tno].TreasureName)
         ELSIF k = normal
         THEN
            StrCopy ('Arrow Quiver', Treasure[tno].TreasureName)
         END
      END ;
      INCL (Rooms[seedRoom].Treasures, tno) ;
      WITH Treasure[tno] DO
         IF (kind = onfloor) AND (Rm # 0)
         THEN
            DrawTreasure (Rm, Xpos, Ypos)
         END
      END
   END
END randomPlace ;


PROCEDURE RespawnArrow (seedRoom: CARDINAL; tno: CARDINAL;
                        spawnKind, arrowKind: TreasureKind;
                        amount: CARDINAL; ticks: CARDINAL) ;
BEGIN
   IF amount > 0
   THEN
      IF tno = 0
      THEN
         tno := findSpareTreasure (spawnKind) ;
      END ;
      IF tno # 0
      THEN
         (* this should nearly always be true, but if we did run out of slots then
            we forget the dynamic arrow treasure.  *)
         IF ticks = 0
         THEN
            randomPlace (seedRoom, tno, arrowKind)
         ELSE
            addToQueue (seedRoom, tno, arrowKind, amount, ticks)
         END
      END
   END
END RespawnArrow ;


(*
   initPool - initialise a treasure so that it might be dynamically allocated
              as items on the floor.
*)

PROCEDURE initPool (i: CARDINAL) ;
BEGIN
   WITH Treasure[i] DO
      Xpos := 0 ;
      Ypos := 0 ;
      Rm := 0 ;
      Tweight := 0 ;
      StrCopy ('', TreasureName) ;
      kind := unused ;
      amount := 0
   END
END initPool ;


(*
   initTreasure - initialize weight and kind.
*)

PROCEDURE initTreasure (i: CARDINAL; weight: CARDINAL; kind: TreasureKind) ;
BEGIN
   Treasure[i].Tweight := weight ;
   Treasure[i].kind := kind ;
END initTreasure ;


PROCEDURE Init ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 1 TO MaxNoOfTreasures DO
      initPool (i)
   END ;

   PinPulled := InitSemaphore(0, 'PinPulled') ;
   PinHasBeenPulled := FALSE ;

   StrCopy('Magic Key'   , Treasure[MagicKey   ].TreasureName ) ;
   StrCopy('Crystal Ball', Treasure[CrystalBall].TreasureName ) ;
   StrCopy('Magic Spring', Treasure[MagicSpring].TreasureName ) ;
   StrCopy('Sack Of Coal', Treasure[SackOfCoal1].TreasureName ) ;
   StrCopy('Sack Of Coal', Treasure[SackOfCoal2].TreasureName ) ;
   StrCopy('Hot Iron'    , Treasure[HotIron    ].TreasureName ) ;
   StrCopy('Hand Grenade', Treasure[HandGrenade].TreasureName ) ;
   StrCopy('Magic Sword' , Treasure[MagicSword ].TreasureName ) ;
   StrCopy('Magic Shoes' , Treasure[MagicShoes ].TreasureName ) ;
   StrCopy('Sleep Potion', Treasure[SleepPotion].TreasureName ) ;
   StrCopy('Lump Of Iron', Treasure[LumpOfIron ].TreasureName ) ;
   StrCopy('Treas. Trove', Treasure[TreasTrove ].TreasureName ) ;
   StrCopy('Speed Potion', Treasure[SpeedPotion].TreasureName ) ;
   StrCopy('Magic Shield', Treasure[MagicShield].TreasureName ) ;
   StrCopy('Vision Chest', Treasure[VisionChest].TreasureName ) ;
   StrCopy('Arrow Quiver', Treasure[QuiverNormal].TreasureName ) ;
   StrCopy('Magic Arrows', Treasure[QuiverMagic].TreasureName ) ;
   StrCopy('Salve       ', Treasure[HealingPotion].TreasureName ) ;

   initTreasure (MagicKey   ,   0, unused) ;
   initTreasure (CrystalBall,  33, unused) ;
   initTreasure (MagicSpring,   0, unused) ;
   initTreasure (SackOfCoal1, 100, unused) ;
   initTreasure (SackOfCoal2, 100, unused) ;
   initTreasure (HotIron    ,   4, unused) ;   (* was   4 *)
   initTreasure (HandGrenade,   3, unused) ;   (* was   3 *)
   initTreasure (MagicSword ,   1, unused) ;   (* was   1 *)
   initTreasure (MagicShoes ,   0, unused) ;   (* was   0 *)
   initTreasure (SleepPotion,   5, unused) ;   (* was   5 *)
   initTreasure (LumpOfIron ,   0, unused) ;   (* was   0 *)
   initTreasure (TreasTrove ,  53, unused) ;   (* was  43 *)
   initTreasure (SpeedPotion,   0, unused) ;   (* was   0 *)
   initTreasure (MagicShield,   2, unused) ;   (* was   2 *)
   initTreasure (VisionChest, 120, unused) ;   (* was 150 *)
   initTreasure (HealingPotion, 0, unused) ;

   StrCopy('Treasure No xx', Tmessage ) ;
   armedTimer := NIL ;
   qMutex := InitSemaphore (1, "qMutex") ;
   qAvailable := InitSemaphore (0, "qAvailable") ;
   qHead := NIL ;
   freeDesc := NIL ;
   qThread := NIL
END Init ;


BEGIN
   Init
END AdvTreasure.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
