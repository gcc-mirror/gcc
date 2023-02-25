IMPLEMENTATION MODULE AdvUtil ;


FROM libc IMPORT printf ;
FROM AdvSystem IMPORT PlayerSet ;
FROM ASCII IMPORT cr, lf, nul, bs, del, nak ;
FROM Screen IMPORT WriteString ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Assertion IMPORT Assert ;
FROM AdvSound IMPORT Miss, Swish, Hit ;

FROM AdvSystem IMPORT Player, PlayerNo, IsPlayerActive,
                      MaxNoOfPlayers,
                      ArrowArgs,
                      NextFreePlayer,
                      TypeOfDeath,
                      RandomNumber,
                      DefaultWrite,

                      GetReadAccessToPlayer,
                      ReleaseReadAccessToPlayer,
                      GetWriteAccessToPlayer,
                      ReleaseWriteAccessToPlayer,

                      GetReadAccessToTreasure,
                      ReleaseReadAccessToTreasure,
                      GetWriteAccessToTreasure,
                      ReleaseWriteAccessToTreasure,

                      GetReadAccessToDoor,
                      ReleaseReadAccessToDoor,
                      GetWriteAccessToDoor,
                      ReleaseWriteAccessToDoor,

                      GetAccessToScreen,
                      ReleaseAccessToScreen,
                      GetAccessToScreenNo,
                      ReleaseAccessToScreenNo,
                      ClientRead ;

FROM AdvMath IMPORT MaxNoOfTreasures ;

FROM AdvMap IMPORT Treasure, Rooms, DoorStatus, TreasureKind,
                   NoOfRoomsToHidePlayers,
                   ActualNoOfRooms,
                   IncPosition ;

FROM Executive IMPORT GetCurrentProcess, Wait, Signal ;
FROM TimerHandler IMPORT Sleep, TicksPerSecond, GetTicks ;
FROM ProcArgs IMPORT ProcessArgs, CollectArgs ;

FROM Screen IMPORT InitScreen,
                   WriteWounds,
                   WriteRoom, WriteWeight,
                   WriteCommentLine1, DelCommentLine1,
                   WriteCommentLine2, DelCommentLine2,
                   WriteCommentLine3, DelCommentLine3,
                   InnerX, OuterX, InnerY, OuterY, OffX, OffY,
                   Height, Width ;

FROM AdvMath IMPORT DammageByParry,
                    DammageByAttack,
                    DammageByThrust,
                    DammageByFireArrow,
                    DammageByFireMagic,
                    DammageByMagicParry,
                    DammageByMagicAttack,
                    DammageByMagicThrust,

                    MagicSword,
                    MagicShield,

                    StrengthToParry,
                    StrengthToAttack,
                    StrengthToThrust,
                    StrengthToFireArrow,
                    StrengthToFireMagic,
                    StrengthToMove,
                    UpDateWoundsAndFatigue ;

FROM DrawL IMPORT DrawAllPlayers, DrawRoom, ClearRoom ;

FROM DrawG IMPORT DrawMan, EraseMan, DrawDoor, DrawArrow, EraseArrow,
                  DisplayMessage, DrawTreasure ;

FROM AdvTreasure IMPORT ScatterTreasures, RespawnTreasure ;


CONST
   SquaresPerSecond =    25 ;    (* speed of arrows *)
   DelayPerSquare   = TicksPerSecond DIV SquaresPerSecond ;


PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


PROCEDURE PointOnLine (x, y, x1, y1, x2, y2: CARDINAL ; VAR ok: BOOLEAN) ;
BEGIN
   IF (x=x1) AND (y>=y1) AND (y<=y2)
   THEN
      ok := TRUE
   ELSIF (y=y1) AND (x>=x1) AND (x<=x2)
   THEN
      ok := TRUE
   ELSE
      ok := FALSE
   END
END PointOnLine ;


(* All the following routines assume that the calling process has gained *)
(* access to map, if needed!                                             *)

PROCEDURE ChangeStatusOfDoor (RoomNo, DoorNo: CARDINAL ; ds: DoorStatus) ;
VAR
   r,i,x1,x2,y1,y2: CARDINAL ;
BEGIN
   WITH Rooms[RoomNo].Doors[DoorNo] DO
      r := LeadsTo ;
      StateOfDoor := ds ;
      x1 := Position.X1 ;
      y1 := Position.Y1 ;
      x2 := Position.X2 ;
      y2 := Position.Y2
   END ;
   DrawDoor( RoomNo, DoorNo ) ;
   IF r#0
   THEN
      GetDoorIndex( r, x1, y1, x2, y2, i ) ;
      Rooms[r].Doors[i].StateOfDoor := ds ;
      DrawDoor( r, i )
   END
END ChangeStatusOfDoor ;


PROCEDURE GetDoorIndex (RoomNo,
                        x1, y1, x2, y2: CARDINAL ;
                        VAR i: CARDINAL) ;
VAR
   Max : CARDINAL ;
   ok  : BOOLEAN ;
BEGIN
   i := 1 ;
   ok := TRUE ;
   Max := Rooms[RoomNo].NoOfDoors ;
   WITH Rooms[RoomNo] DO
      WHILE (i<=NoOfDoors) AND ok DO
         WITH Doors[i].Position DO
            IF (x1=X1) AND (y1=Y1) AND
               (x2=X2) AND (y2=Y2)
            THEN
               ok := FALSE
            ELSE
               INC( i )
            END
         END
      END
   END
END GetDoorIndex ;


PROCEDURE GetDoorOnPoint (RoomNo, x, y: CARDINAL ;
                          VAR DoorNo: CARDINAL ; VAR Success: BOOLEAN) ;
VAR
   Max : CARDINAL ;
BEGIN
   Max := Rooms[RoomNo].NoOfDoors ;
   DoorNo := 1 ;
   Success := FALSE ;
   WITH Rooms[RoomNo] DO
      WHILE (NOT Success) AND (DoorNo<=Max) DO
         WITH Doors[DoorNo].Position DO
            PointOnLine( x, y, X1, Y1, X2, Y2, Success ) ;
            IF NOT Success
            THEN
               INC( DoorNo )
            END
         END
      END
   END
END GetDoorOnPoint ;


PROCEDURE OpenToClosedDoor (VAR Success: BOOLEAN) ;
VAR
   x, y, DoorNo: CARDINAL ;
BEGIN
   WITH Player[PlayerNo()] DO
      x := Xman ;
      y := Yman ;
      IncPosition( x, y, Direction ) ;
      GetDoorOnPoint( RoomOfMan, x, y, DoorNo, Success ) ;
      IF Success
      THEN
         IF Rooms[RoomOfMan].Doors[DoorNo].StateOfDoor=Open
         THEN
            ChangeStatusOfDoor( RoomOfMan, DoorNo, Closed )
         ELSE
            Success := FALSE
         END
      END
   END
END OpenToClosedDoor ;


PROCEDURE ClosedToOpenDoor (VAR Success: BOOLEAN) ;
VAR
   x, y, DoorNo: CARDINAL ;
BEGIN
   WITH Player[PlayerNo()] DO
      x := Xman ;
      y := Yman ;
      IncPosition(x, y, Direction) ;
      GetDoorOnPoint( RoomOfMan, x, y, DoorNo, Success ) ;
      IF Success
      THEN
         IF Rooms[RoomOfMan].Doors[DoorNo].StateOfDoor=Closed
         THEN
            ChangeStatusOfDoor( RoomOfMan, DoorNo, Open )
         ELSE
            Success := FALSE
         END
      END
   END
END ClosedToOpenDoor ;


PROCEDURE ClosedToSecretDoor (VAR Success: BOOLEAN) ;
VAR
   x, y, DoorNo: CARDINAL ;
BEGIN
   WITH Player[PlayerNo()] DO
      x := Xman ;
      y := Yman ;
      IncPosition( x, y, Direction ) ;
      GetDoorOnPoint( RoomOfMan, x, y, DoorNo, Success ) ;
      IF Success
      THEN
         IF Rooms[RoomOfMan].Doors[DoorNo].StateOfDoor=Closed
         THEN
            ChangeStatusOfDoor( RoomOfMan, DoorNo, Secret )
         ELSE
            Success := FALSE
         END
      END
   END
END ClosedToSecretDoor ;


PROCEDURE SecretToClosedDoor (VAR Success: BOOLEAN) ;
VAR
   x, y, DoorNo: CARDINAL ;
BEGIN
   WITH Player[PlayerNo()] DO
      x := Xman ;
      y := Yman ;
      IncPosition( x, y, Direction ) ;
      GetDoorOnPoint( RoomOfMan, x, y, DoorNo, Success ) ;
      IF Success
      THEN
         IF Rooms[RoomOfMan].Doors[DoorNo].StateOfDoor=Secret
         THEN
            ChangeStatusOfDoor( RoomOfMan, DoorNo, Closed )
         ELSE
            Success := FALSE
         END
      END
   END
END SecretToClosedDoor ;


PROCEDURE PointOnWall (RoomNo, x, y: CARDINAL ;
                       VAR Success: BOOLEAN) ;
VAR
   Max,
   WallNo : CARDINAL ;
BEGIN
   Max := Rooms[RoomNo].NoOfWalls ;
   WallNo := 1 ;
   Success := FALSE ;
   WHILE (NOT Success) AND (WallNo<=Max) DO
      WITH Rooms[RoomNo].Walls[WallNo] DO
         PointOnLine( x, y, X1, Y1, X2, Y2, Success ) ;
         IF NOT Success
         THEN
            INC( WallNo )
         END
      END
   END
END PointOnWall ;


PROCEDURE PointOnTreasure (RoomNo, x, y: CARDINAL ;
                           VAR TreasNo: CARDINAL ; VAR Success: BOOLEAN) ;
BEGIN
   TreasNo := 1 ;
   Success := FALSE ;
   WHILE (NOT Success) AND (TreasNo<=MaxNoOfTreasures) DO
      WITH Treasure[TreasNo] DO
         IF (Rm = RoomNo) AND (kind = onfloor)
         THEN
            IF (Xpos=x) AND (Ypos=y)
            THEN
               Success := TRUE
            ELSE
               INC( TreasNo )
            END
         ELSE
            INC( TreasNo )
         END
      END
   END
END PointOnTreasure ;


(* This routine finds out if a point is upon a player. It does use *)
(* GetReadAccessToPlayer & ReleaseReadAccessToPlayer. This routine *)
(* returns Success if the point was on a player and also sets      *)
(* PlayNo to tell which player is upon this square.                *)

PROCEDURE PointOnPlayer (RoomNo, x, y: CARDINAL ;
                         VAR PlayNo: CARDINAL ; VAR Success: BOOLEAN) ;
BEGIN
   PlayNo := 0 ;
   Success := FALSE ;
   GetReadAccessToPlayer ;
   WHILE (NOT Success) AND (PlayNo<NextFreePlayer) DO
      WITH Player[PlayNo] DO
         IF IsPlayerActive(PlayNo) AND (Xman=x) AND (Yman=y)
         THEN
            Success := TRUE
         ELSE
            INC(PlayNo)
         END
      END
   END ;
   ReleaseReadAccessToPlayer
END PointOnPlayer ;


PROCEDURE TestIfLastLivePlayer (VAR yes: BOOLEAN) ;
VAR
   p, i: CARDINAL ;
BEGIN
   p := PlayerNo() ;
   i := 0 ;
   yes := TRUE ;
   WHILE (i<NextFreePlayer) AND yes DO
      IF p#i
      THEN
         IF (Player[i].DeathType=living) AND IsPlayerActive(i)
         THEN
            yes := FALSE
         END
      END ;
      INC( i )
   END
END TestIfLastLivePlayer ;

PROCEDURE ReceiveFireFromProcess (p: CARDINAL;
                                  VAR r, x, y, d: CARDINAL; magic: BOOLEAN) ;
VAR
   aa: ArrowArgs ;
BEGIN
   IF magic
   THEN
      aa := CollectArgs(Player[p].MagicProcArgs)
   ELSE
      aa := CollectArgs(Player[p].NormalProcArgs)
   END ;
   WITH aa^ DO
      Assert(p=ArrowPlayer) ;
      r := ArrowRoom ;
      x := ArrowX ;
      y := ArrowY ;
      d := ArrowDir
   END ;
   DISPOSE(aa)
END ReceiveFireFromProcess ;


PROCEDURE NormalArrow (p: CARDINAL) ;
VAR
   x, y, r,
   d,
   player : CARDINAL ;
   hit,
   done,
   SlainP : BOOLEAN ;
BEGIN
   WITH Player[p] DO
      LOOP
         SlainP := FALSE ;
         ReceiveFireFromProcess(p, r, x, y, d, FALSE) ;
         REPEAT
            FireArrow(p, r, x, y, d, hit, player) ;
            IF hit
            THEN
               GetReadAccessToPlayer ;
               WITH Player[player] DO
                  IF MagicShield IN TreasureOwn
                  THEN
                     done := FALSE ;
                     d := (d+2) MOD 4 ;
                     x := Xman ;
                     y := Yman ;
                     r := RoomOfMan ;
                     IncPosition(x, y, d)
                  ELSE
                     done := TRUE
                  END
               END ;
               ReleaseReadAccessToPlayer
            ELSE
               done := TRUE
            END
         UNTIL done ;
         IF hit
         THEN
            WITH Player[player] DO
               GetWriteAccessToPlayer ;

               GetAccessToScreenNo(player) ;
               UpDateWoundsAndFatigue(player) ;
               IF Wounds<=DammageByFireArrow
               THEN
                  r := RoomOfMan ;
                  SlainP := TRUE ;
                  Wounds := 0 ;
                  DeathType := normalarrow ;
               ELSE
                  DEC( Wounds, DammageByFireArrow )
               END ;
               WriteWounds(player, Wounds) ;
               WriteCommentLine1(player, 'struck thee') ;
               DelCommentLine2(player) ;
               DelCommentLine3(player) ;
               ReleaseAccessToScreenNo(player) ;

               GetAccessToScreenNo(p) ;
               IF Wounds=0
               THEN
                  DelCommentLine1(p) ;
                  WriteCommentLine2(p, 'slain') ;
                  WriteCommentLine3(p,  ManName )
               ELSE
                  WriteCommentLine1(p, 'thwunk') ;
                  DelCommentLine2(p) ;
                  DelCommentLine3(p)
               END ;
               ReleaseAccessToScreenNo(p) ;
               ReleaseWriteAccessToPlayer
            END
         ELSE
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'swish') ;
            DelCommentLine2(p) ;
            DelCommentLine3(p) ;
            ReleaseAccessToScreenNo(p)
         END ;
         IF SlainP
         THEN
            Dead(player, r)
         END
      END
   END
END NormalArrow ;


PROCEDURE MagicArrow (p: CARDINAL) ;
VAR
   x, y, r,
   d,
   player : CARDINAL ;
   hit,
   SlainP : BOOLEAN ;
BEGIN
   WITH Player[p] DO
      LOOP
         SlainP := FALSE ;
         ReceiveFireFromProcess(p, r, x, y, d, TRUE) ;
         FireArrow(p, r, x, y, d, hit, player) ;
         IF hit
         THEN
            WITH Player[player] DO
               GetWriteAccessToPlayer ;

               GetAccessToScreenNo(player) ;
               UpDateWoundsAndFatigue(player) ;
               IF Wounds<=DammageByFireMagic
               THEN
                  r := RoomOfMan ;
                  SlainP := TRUE ;
                  DeathType := magicarrow ;
                  Wounds := 0
               ELSE
                  DEC(Wounds, DammageByFireMagic)
               END ;
               WriteWounds(player, Wounds) ;
               WriteCommentLine1(player, 'struck thee') ;
               DelCommentLine2(player) ;
               DelCommentLine3(player) ;
               ReleaseAccessToScreenNo(player) ;

               GetAccessToScreenNo(p) ;

               IF Wounds=0
               THEN
                  DelCommentLine1(p) ;
                  WriteCommentLine2(p, 'slain') ;
                  WriteCommentLine3(p,  ManName)
               ELSE
                  WriteCommentLine1(p, 'thwunk') ;
                  DelCommentLine2(p) ;
                  DelCommentLine3(p)
               END ;
               ReleaseAccessToScreenNo(p) ;
               ReleaseWriteAccessToPlayer
            END
         ELSE
            GetAccessToScreenNo(p) ;

            WriteCommentLine1(p, 'swish') ;
            DelCommentLine2(p) ;
            DelCommentLine3(p) ;
            ReleaseAccessToScreenNo(p)
         END ;
         IF SlainP
         THEN
            Dead(player, r)
         END
      END
   END
END MagicArrow ;


PROCEDURE FireArrow (p, r, x, y, d: CARDINAL ;
                     VAR hit: BOOLEAN ; VAR player: CARDINAL) ;
VAR
   t           : INTEGER ;
   NotCont     : BOOLEAN ;
   door,
   X, Y,
   i, j        : CARDINAL ;
   playerscreen: PlayerSet ;
   LastTime,
   DelayTime   : CARDINAL ;
BEGIN
   i := x ;   (* old x & y         *)
   j := y ;
   playerscreen := PlayerSet{} ;
   Swish(r) ;
   REPEAT
      LastTime := GetTicks() ;
      PointOnPlayer(r, x, y, player, NotCont) ;
      hit := NotCont ;
      IF NOT NotCont
      THEN
         GetReadAccessToDoor ;
         GetDoorOnPoint(r, x, y, door, NotCont) ;
         IF NotCont
         THEN
            WITH Rooms[r].Doors[door] DO
               IF (StateOfDoor=Open) AND (LeadsTo#0)
               THEN
                  NotCont := FALSE ;
                  r := LeadsTo
               END
            END ;
            ReleaseReadAccessToDoor ;
         ELSE
            ReleaseReadAccessToDoor ;
            PointOnWall(r, x, y, NotCont) ;
            IF NOT NotCont
            THEN
               GetReadAccessToTreasure ;
               PointOnTreasure(r, x, y, door, NotCont) ;
               ReleaseReadAccessToTreasure ;
               IF NOT NotCont
               THEN
                  GetReadAccessToPlayer ;
                  EraseArrow(i, j, playerscreen, FALSE) ;
                  DrawArrow(r, x, y, d, playerscreen) ;
                  ReleaseReadAccessToPlayer ;
                  i := x ;
                  j := y
               END
            END
         END
      END ;
      IncPosition(x, y, d) ;
      (* now we regulate a constant velocity arrow! *)
      DelayTime := GetTicks() - LastTime ;
      IF DelayTime<DelayPerSquare
      THEN
         (* t := printf("before Sleep for %d ticks\n", DelayPerSquare-DelayTime) ; *)
         Sleep(DelayPerSquare-DelayTime)
         (* ; t := printf("after Sleep\n") ; *)
      END
   UNTIL NotCont ;
   IF hit
   THEN
      Hit(p)
   ELSE
      Miss(r)
   END ;
   IF (X#i) OR (Y#j)
   THEN
      GetReadAccessToPlayer ;
      EraseArrow(i, j, playerscreen, TRUE) ;
      ReleaseReadAccessToPlayer
   END
END FireArrow ;


PROCEDURE Exit ;
VAR
   p  : CARDINAL ;
   yes: BOOLEAN ;
BEGIN
   p := PlayerNo() ;
   GetReadAccessToPlayer ;
   TestIfLastLivePlayer(yes) ;
   ReleaseReadAccessToPlayer ;
   IF NOT yes
   THEN
      Dead(p, Player[p].RoomOfMan)
   END
END Exit ;


(* MoveMan moves the man forward n squares, providing these squares   *)
(* are free from a WALL, DOOR (closed, secret), TREASURE and MAN.     *)

PROCEDURE MoveMan (n: CARDINAL) ;
VAR
   p   : CARDINAL ;
   yes : BOOLEAN ;
BEGIN
   IF n>0
   THEN
      p := PlayerNo() ;
      GetWriteAccessToPlayer ;
      MoveMan1(n, p) ;
      ReleaseWriteAccessToPlayer ;
      IF Player[p].DeathType=exitdungeon
      THEN
         TestIfLastLivePlayer(yes) ;
         IF NOT yes
         THEN
            Dead(p, Player[0].RoomOfMan)
         END
      END
   END
END MoveMan ;


PROCEDURE MoveMan1 (n, p: CARDINAL) ;
VAR
   x, y,
   i, j, s,
   r,  dir,
   tr, z,
   Sx, Sy,
   DoorNo : CARDINAL ;
   hit    : BOOLEAN ;
BEGIN
   StrengthToMove(n, hit) ;
   IF hit
   THEN
      WITH Player[p] DO
         EraseMan(p) ;
         dir := Direction ;
         tr := RoomOfMan ;
         x := Xman ;
         y := Yman ;
         Sx := ScreenX ;
         Sy := ScreenY ;
         hit := FALSE ;
         s := 1 ;
         i := x ;
         j := y ;
         r := tr ;
         WHILE (s<=n) AND (NOT hit) DO
            IncPosition(i, j, dir) ;
            GetReadAccessToDoor ;
            GetDoorOnPoint(r, i, j, DoorNo, hit) ;
            IF hit
            THEN
               IF Rooms[r].Doors[DoorNo].StateOfDoor=Open
               THEN
                  z := Rooms[r].Doors[DoorNo].LeadsTo ;
                  ReleaseReadAccessToDoor ;
                  IF z=0
                  THEN
                     DeathType := exitdungeon
                  ELSE
                     IncPosition(i, j, dir) ;
                     TakenPointInRoom(z, i, j, hit) ;
                     IF NOT hit  (* Empty Point In Room *)
                     THEN
                        INC(s) ;
                        x := i ;
                        y := j ;
                        r := z  (* Ok so changed room *)
                     END
                  END
               ELSE
                  ReleaseReadAccessToDoor
               END ;
            ELSE
               ReleaseReadAccessToDoor ;
               PointOnWall(r, i, j, hit) ;
               IF NOT hit
               THEN
                  GetReadAccessToTreasure ;
                  PointOnTreasure(r, i, j, z, hit) ;
                  ReleaseReadAccessToTreasure ;
                  IF NOT hit
                  THEN
                     PointOnOtherPlayer(i, j, z, hit) ;
                     IF NOT hit
                     THEN
                        x := i ;
                        y := j ;
                        INC(s)
                     END
                  END
               END
            END
         END ;
         IF (x#Xman) OR (y#Yman)
         THEN
            Xman := x ;
            Yman := y ;
            ScaleSights(x, y, Sx, Sy, hit) ;
            ScreenX := Sx ;
            ScreenY := Sy ;
            RoomOfMan := r ;
            IF r#0
            THEN
               IF (tr#r) OR hit
               THEN
                  IF hit
                  THEN
                     InitScreen(p)
                  ELSE
                     GetAccessToScreenNo(p) ;
                     WriteRoom(p, r) ;
                     ReleaseAccessToScreenNo(p)
                  END ;
                  ClearRoom(tr) ;
                  DrawRoom
               END ;
               DrawMan(p)
            END
         ELSE
            DrawMan(p)
         END
      END
   END
END MoveMan1 ;


PROCEDURE TakenPointInRoom (room, x, y: CARDINAL ; VAR ok: BOOLEAN) ;
VAR
   z: CARDINAL ;
BEGIN
   PointOnWall( room, x, y, ok ) ;
   IF NOT ok
   THEN
      GetReadAccessToDoor ;
      GetDoorOnPoint(room, x, y, z, ok) ;
      ReleaseReadAccessToDoor ;
      IF NOT ok
      THEN

         (* No need to get Read Access To Player 's since taken care of *)
         (* in the called routine.                                      *)

         PointOnOtherPlayer(x, y, z, ok) ;
         IF NOT ok
         THEN
            GetReadAccessToTreasure ;
            PointOnTreasure(room, x, y, z, ok) ;
            ReleaseReadAccessToTreasure
         END
      END
   END
END TakenPointInRoom ;


(* ScaleSights scales the sights of the ScreenX and ScreenY      *)
(* coordinates according to whether the player is off the        *)
(* screen or off the boundary. It returns Done if the routine    *)
(* has altered Sx, Sy.                                           *)

PROCEDURE ScaleSights (x, y: CARDINAL ; VAR Sx, Sy: CARDINAL ;
                       VAR Done: BOOLEAN) ;
VAR
   sx, sy: CARDINAL ;
BEGIN
   sx := Sx ;
   sy := Sy ;
   IF Sx+InnerX>x
   THEN
      Dec(Sx, OffX)
   ELSIF Sx+OuterX < x
   THEN
      Inc(Sx, OffX)
   END ;
   IF Sy+InnerY>y
   THEN
      Dec(Sy, OffY)
   ELSIF Sy+OuterY<y
   THEN
      Inc(Sy, OffY)
   END ;
   Done := (sx#Sx) OR (sy#Sy)
END ScaleSights ;


PROCEDURE Inc (VAR s: CARDINAL ; c: CARDINAL) ;
BEGIN
   IF (c DIV 2) + (s DIV 2) < 32768
   THEN
      INC(s, c)
   END
END Inc ;


PROCEDURE Dec (VAR s: CARDINAL ; c: CARDINAL) ;
BEGIN
   IF c<=s
   THEN
      DEC(s, c)
   ELSIF s>0
   THEN
      s := 0
   END
END Dec ;


PROCEDURE Parry ;
VAR
   p, r, x, y, Pn : CARDINAL ;
   hit, SlainP    : BOOLEAN ;
BEGIN
   SlainP := FALSE ;
   p := PlayerNo() ;
   WITH Player[p] DO
      GetWriteAccessToPlayer ;
      StrengthToParry(hit) ;
      IF hit
      THEN
         ReleaseWriteAccessToPlayer ;
         GetReadAccessToPlayer ;
         x := Xman ;
         y := Yman ;
         IncPosition(x, y, Direction) ;
         PointOnOtherPlayer(x, y, Pn, hit) ;
         ReleaseReadAccessToPlayer ;
         IF hit
         THEN
            WITH Player[Pn] DO
               GetWriteAccessToPlayer ;

               GetAccessToScreenNo(Pn) ;
               UpDateWoundsAndFatigue(Pn) ;
               ReleaseAccessToScreenNo(Pn) ;

               IF MagicSword IN Player[p].TreasureOwn
               THEN
                  GetAccessToScreenNo(p) ;
                  IF Wounds>DammageByMagicParry
                  THEN
                     DEC(Wounds, DammageByMagicParry) ;
                     WriteCommentLine1(p, 'hit') ;
                     DelCommentLine2(p) ;
                     DelCommentLine3(p)
                  ELSE
                     r := RoomOfMan ;
                     SlainP := TRUE ;
                     Wounds := 0 ;
                     DeathType := sword ;
                     DelCommentLine1(p) ;
                     WriteCommentLine2(p, 'Slain') ;
                     WriteCommentLine3(p, ManName )
                  END ;
                  ReleaseAccessToScreenNo(p)
               ELSE
                  GetAccessToScreenNo(p) ;
                  IF Wounds>DammageByParry
                  THEN
                     DEC( Wounds, DammageByParry ) ;
                     WriteCommentLine1(p, 'hit') ;
                     DelCommentLine2(p) ;
                     DelCommentLine3(p)
                  ELSE
                     r := RoomOfMan ;
                     SlainP := TRUE ;
                     Wounds := 0 ;
                     DeathType := sword ;
                     DelCommentLine1(p) ;
                     WriteCommentLine2(p, 'Slain') ;
                     WriteCommentLine3(p, ManName ) ;
                  END ;
                  ReleaseAccessToScreenNo(p)
               END ;

               GetAccessToScreenNo(Pn) ;
               WriteCommentLine1(Pn, 'hit thee') ;
               DelCommentLine2(Pn) ;
               DelCommentLine3(Pn) ;
               WriteWounds( Pn, Wounds ) ;
               ReleaseAccessToScreenNo(Pn) ;

               ReleaseWriteAccessToPlayer
            END
         ELSE
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'missed') ;
            DelCommentLine2(p) ;
            DelCommentLine3(p) ;
            ReleaseAccessToScreenNo(p)
         END
      ELSE
         ReleaseWriteAccessToPlayer
      END
   END ;
   IF SlainP
   THEN
      Dead( Pn, r )
   END
END Parry ;


PROCEDURE Attack ;
VAR
   p, r, x, y, Pn : CARDINAL ;
   hit, SlainP    : BOOLEAN ;
BEGIN
   SlainP := FALSE ;
   p := PlayerNo() ;
   WITH Player[p] DO
      GetWriteAccessToPlayer ;
      StrengthToAttack( hit ) ;
      IF hit
      THEN
         ReleaseWriteAccessToPlayer ;
         GetReadAccessToPlayer ;
         x := Xman ;
         y := Yman ;
         IncPosition(x, y, Direction) ;
         PointOnOtherPlayer(x, y, Pn, hit) ;
         ReleaseReadAccessToPlayer ;
         IF hit
         THEN
            WITH Player[Pn] DO
               GetWriteAccessToPlayer ;

               GetAccessToScreenNo(Pn) ;
               UpDateWoundsAndFatigue(Pn) ;
               ReleaseAccessToScreenNo(Pn) ;

               IF MagicSword IN Player[p].TreasureOwn
               THEN
                  GetAccessToScreenNo(p) ;
                  IF Wounds>DammageByMagicAttack
                  THEN
                     DEC(Wounds, DammageByMagicAttack) ;
                     WriteCommentLine1(p, 'hit') ;
                     DelCommentLine2(p) ;
                     DelCommentLine3(p)
                  ELSE
                     r := RoomOfMan ;
                     SlainP := TRUE ;
                     Wounds := 0 ;
                     DeathType := sword ;
                     DelCommentLine1(p) ;
                     WriteCommentLine2(p, 'Slain') ;
                     WriteCommentLine3(p, ManName)
                  END ;
                  ReleaseAccessToScreenNo( p )
               ELSE
                  GetAccessToScreenNo( p ) ;
                  IF Wounds>DammageByAttack
                  THEN
                     DEC( Wounds, DammageByAttack ) ;
                     WriteCommentLine1(p, 'hit') ;
                     DelCommentLine2(p) ;
                     DelCommentLine3(p)
                  ELSE
                     r := RoomOfMan ;
                     SlainP := TRUE ;
                     Wounds := 0 ;
                     DeathType := sword ;
                     DelCommentLine1(p) ;
                     WriteCommentLine2(p, 'Slain') ;
                     WriteCommentLine3(p, ManName)
                  END ;
                  ReleaseAccessToScreenNo(p)
               END ;

               GetAccessToScreenNo(Pn) ;

               WriteCommentLine1(Pn, 'hit thee') ;
               DelCommentLine2(Pn) ;
               DelCommentLine3(Pn) ;
               WriteWounds(Pn, Wounds) ;
               ReleaseAccessToScreenNo(Pn) ;

               ReleaseWriteAccessToPlayer
            END
         ELSE
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'missed') ;
            DelCommentLine2(p) ;
            DelCommentLine3(p) ;
            ReleaseAccessToScreenNo(p)
         END
      ELSE
         ReleaseWriteAccessToPlayer
      END
   END ;
   IF SlainP
   THEN
      Dead( Pn, r )
   END
END Attack ;


PROCEDURE Thrust ;
VAR
   p, r, x, y, Pn : CARDINAL ;
   hit, SlainP    : BOOLEAN ;
BEGIN
   SlainP := FALSE ;
   p := PlayerNo() ;
   WITH Player[p] DO
      GetWriteAccessToPlayer ;
      StrengthToThrust(hit) ;
      IF hit
      THEN
         ReleaseWriteAccessToPlayer ;
         GetReadAccessToPlayer ;
         x := Xman ;
         y := Yman ;
         IncPosition(x, y, Direction) ;
         PointOnOtherPlayer(x, y, Pn, hit) ;
         ReleaseReadAccessToPlayer ;
         IF hit
         THEN
            WITH Player[Pn] DO
               GetWriteAccessToPlayer ;

               GetAccessToScreenNo(Pn) ;
               UpDateWoundsAndFatigue(Pn) ;
               ReleaseAccessToScreenNo(Pn) ;

               IF MagicSword IN Player[p].TreasureOwn
               THEN
                  GetAccessToScreenNo(p) ;
                  IF Wounds>DammageByMagicThrust
                  THEN
                     DEC(Wounds, DammageByMagicThrust) ;
                     WriteCommentLine1(p, 'hit') ;
                     DelCommentLine2(p) ;
                     DelCommentLine3(p)
                  ELSE
                     r := RoomOfMan ;
                     SlainP := TRUE ;
                     Wounds := 0 ;
                     DeathType := sword ;
                     DelCommentLine1(p) ;
                     WriteCommentLine2(p, 'Slain') ;
                     WriteCommentLine3(p, ManName)
                  END ;
                  ReleaseAccessToScreenNo(p)
               ELSE
                  GetAccessToScreenNo(p) ;
                  IF Wounds>DammageByThrust
                  THEN
                     DEC(Wounds, DammageByThrust) ;
                     WriteCommentLine1(p, 'hit') ;
                     DelCommentLine2(p) ;
                     DelCommentLine3(p)
                  ELSE
                     r := RoomOfMan ;
                     SlainP := TRUE ;
                     Wounds := 0 ;
                     DeathType := sword ;
                     DelCommentLine1(p) ;
                     WriteCommentLine2(p, 'Slain') ;
                     WriteCommentLine3(p, ManName)
                  END ;
                  ReleaseAccessToScreenNo( p )
               END ;

               GetAccessToScreenNo(Pn) ;

               WriteCommentLine1(Pn, 'hit thee') ;
               DelCommentLine2(Pn) ;
               DelCommentLine3(Pn) ;
               WriteWounds( Pn, Wounds ) ;
               ReleaseAccessToScreenNo(Pn) ;

               ReleaseWriteAccessToPlayer
            END
         ELSE
            GetAccessToScreenNo(p) ;
            WriteCommentLine1(p, 'missed') ;
            DelCommentLine2(p) ;
            DelCommentLine3(p) ;
            ReleaseAccessToScreenNo(p)
         END
      ELSE
         ReleaseWriteAccessToPlayer
      END
   END ;
   IF SlainP
   THEN
      Dead( Pn, r )
   END
END Thrust ;


(* Tests to see whether the point is occupied by another player. *)
(* This procedure does NOT use any lock.                         *)

PROCEDURE PointOnOtherPlayer (x, y: CARDINAL ;
                              VAR Pn: CARDINAL ; VAR Success: BOOLEAN) ;
VAR
   p: CARDINAL ;
BEGIN
   Success := FALSE ;
   p := PlayerNo() ;
   Pn := 0 ;
   WHILE (Pn<NextFreePlayer) AND (NOT Success) DO
      IF (Pn#p) AND IsPlayerActive(Pn)
      THEN
         WITH Player[Pn] DO
            IF (Xman=x) AND (Yman=y)
            THEN
               Success := TRUE
            ELSE
               INC( Pn )
            END
         END
      ELSE
         INC( Pn )
      END
   END
END PointOnOtherPlayer ;


PROCEDURE OpenDoor ;
VAR
   Success: BOOLEAN ;
BEGIN
   GetReadAccessToPlayer ;
   GetWriteAccessToDoor ;
   ClosedToOpenDoor(Success) ;
   GetAccessToScreen ;
   IF Success
   THEN
      DelCommentLine1(PlayerNo())
   ELSE
      WriteCommentLine1(PlayerNo(), 'thou canst')
   END ;
   ReleaseAccessToScreen ;
   ReleaseWriteAccessToDoor ;
   ReleaseReadAccessToPlayer
END OpenDoor ;


PROCEDURE ExamineDoor ;
VAR
   Success: BOOLEAN ;
BEGIN
   GetReadAccessToPlayer ;
   GetWriteAccessToDoor ;
   SecretToClosedDoor(Success) ;
   GetAccessToScreen ;
   IF Success
   THEN
      DelCommentLine1(PlayerNo())
   ELSE
      WriteCommentLine1(PlayerNo(), 'nothing')
   END ;
   ReleaseAccessToScreen ;
   ReleaseWriteAccessToDoor ;
   ReleaseReadAccessToPlayer
END ExamineDoor ;


PROCEDURE CloseDoor ;
VAR
   Success: BOOLEAN ;
BEGIN
   GetReadAccessToPlayer ;
   GetWriteAccessToDoor ;
   OpenToClosedDoor(Success) ;
   GetAccessToScreen ;
   IF Success
   THEN
      DelCommentLine1(PlayerNo())
   ELSE
      WriteCommentLine1(PlayerNo(), 'thou canst')
   END ;
   ReleaseAccessToScreen ;
   ReleaseWriteAccessToDoor ;
   ReleaseReadAccessToPlayer
END CloseDoor ;


PROCEDURE HideDoor ;
VAR
   Success: BOOLEAN ;
BEGIN
   GetReadAccessToPlayer ;
   GetWriteAccessToDoor ;
   ClosedToSecretDoor(Success) ;
   GetAccessToScreen ;
   IF Success
   THEN
      DelCommentLine1(PlayerNo())
   ELSE
      WriteCommentLine1(PlayerNo(), 'thou canst')
   END ;
   ReleaseAccessToScreen ;
   ReleaseWriteAccessToDoor ;
   ReleaseReadAccessToPlayer
END HideDoor ;


(* Speak Function                                                     *)

PROCEDURE Speak ;
VAR
   a1, a2, a3: ARRAY [0..14] OF CHAR ;
   i, r, p   : CARDINAL ;
   ch        : CHAR ;
BEGIN
   p := PlayerNo() ;
   r := Player[p].RoomOfMan ;
   i := 0 ;
   a1[0] := nul ;
   a2[0] := nul ;
   a3[0] := nul ;
   REPEAT
      IF ClientRead(ch)
      THEN
         IF ch=nak
         THEN
            i := 0
         ELSIF (ch=del) OR (ch=bs)
         THEN
            IF i>0
            THEN
               DEC( i )
            END
         ELSIF (ch>=' ') OR (ch=cr)
         THEN
            IF ch=cr
            THEN
               ch := nul
            END ;
            IF i<15
            THEN
               a1[i] := ch
            ELSIF i<30
            THEN
               a2[i-15] := ch
            ELSE
               a3[i-30] := ch
            END ;
            INC( i )
         END
      ELSE
         RETURN
      END
   UNTIL (ch=nul) OR (i>44) ;
   DisplayMessage( a1, a2, a3 ) ;
END Speak ;


(* Assumes we are configured to write to player, p, *)

PROCEDURE Dead (p, room : CARDINAL) ;
BEGIN
   WITH Player[p] DO
      GetWriteAccessToPlayer ;
      IF room#0
      THEN
         GetReadAccessToDoor ;
         GetWriteAccessToTreasure ;
         ScatterTreasures(p, room) ;
         EraseMan(p) ;
         ReleaseWriteAccessToTreasure ;
         ReleaseReadAccessToDoor
      END ;
      ReleaseWriteAccessToPlayer
   END
END Dead ;


(* RandomRoom takes the current room and works out a random room.    *)

PROCEDURE RandomRoom (CurrentRoom, NoOfRoomsApart: CARDINAL ;
                      VAR room: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   (* Warning the method used here may cause problems to a map which *)
   (* does not have consecutive rooms.                               *)

   RandomNumber( i, ActualNoOfRooms) ;
   room := i+1

(*
   Cutting out this routine for the moment.

   room := CurrentRoom ;
   WHILE NoOfRoomsApart>0 DO
      IF Rooms[CurrentRoom].NoOfDoors=1
      THEN
         i := 1
      ELSE
         RandomNumber( i, Rooms[CurrentRoom].NoOfDoors ) ;
         INC( i )
      END ;
      room := Rooms[CurrentRoom].Doors[i].LeadsTo ;
      IF room=0
      THEN
         room := CurrentRoom
      ELSE
         CurrentRoom := room
      END ;
      DEC( NoOfRoomsApart )
   END
*)
END RandomRoom ;


(* GetRandomPosition finds a free random position within a room *)

PROCEDURE PositionInRoom (room: CARDINAL ;
                          VAR x, y: CARDINAL ; VAR Success: BOOLEAN) ;
VAR
   r          : INTEGER ;
   maxx, maxy,
   x1, y1, doorno,
   j, i, d, z,
   OldRoom    : CARDINAL ;
   OkOld, ok,
   OkCurrent  : BOOLEAN ;
BEGIN
   doorno := 1 ;
   Success := FALSE ;
   WHILE (doorno<=Rooms[room].NoOfDoors) AND (NOT Success) DO
      OldRoom := Rooms[room].Doors[doorno].LeadsTo ;
      IF OldRoom#0
      THEN
         WITH Rooms[room].Doors[doorno].Position DO
            x1 := X1 ;
            y1 := Y1 ;
            i := X2
         END ;
         IF x1=i
         THEN
            d := 1
         ELSE
            d := 0
         END ;
         i := 1 ;
         maxx := 0 ;
         maxy := 0 ;
         WHILE i<=Rooms[room].NoOfWalls DO
            maxx := Max(Rooms[room].Walls[i].X2, maxx) ;
            maxy := Max(Rooms[room].Walls[i].Y2, maxy) ;
            INC(i)
         END ;
         OkCurrent := FALSE ;
         OkOld := FALSE ;
         WHILE NOT OkCurrent DO
            d := (d+2) MOD 4 ;
            i := x1 ;
            j := y1 ;
            REPEAT
               IncPosition(i, j, d) ;
               PointOnWall(room, i, j, OkCurrent) ;
               IF NOT OkCurrent
               THEN
                  GetDoorOnPoint(room, i, j, z, OkCurrent)
               END ;
               PointOnWall(OldRoom, i, j, OkOld) ;
               IF NOT OkOld
               THEN
                  GetDoorOnPoint(OldRoom, i, j, z, OkOld)
               END ;
               IF NOT OkCurrent
               THEN
                  FreeOfPlayersAndTreasure(room, i, j, ok) ;
                  IF ok
                  THEN
                     Success := TRUE ;
                     x := i ;
                     y := j
                  END
               END
            UNTIL OkCurrent OR OkOld OR
                  (ODD(d) AND ((i=0) OR (i>=maxx))) OR
                  ((NOT ODD(d)) AND ((j=0) OR (j>=maxy))) ;
            IF OkOld
            THEN
               Success := FALSE
            END
         END
      END ;
      INC(doorno)
   END ;
   IF Success
   THEN
      r := printf("room %d position %d %d\n", room, x, y)
   END
END PositionInRoom ;


PROCEDURE FreeOfPlayersAndTreasure (room, x, y: CARDINAL ; VAR Success: BOOLEAN) ;
VAR
   i : CARDINAL ;
BEGIN
   Success := TRUE ;
   i := 0 ;
   WHILE (i<NextFreePlayer) AND Success DO
      IF IsPlayerActive(i)
      THEN
         WITH Player[i] DO
            IF room=RoomOfMan
            THEN
               IF (Xman=x) AND (Yman=y)
               THEN
                  Success := FALSE
               END
            END
         END
      END ;
      INC (i)
   END ;
   IF Success
   THEN
      i := 1 ;
      WHILE (i<=MaxNoOfTreasures) AND Success DO
         WITH Treasure[i] DO
            IF (Rm=room) AND (kind=onfloor)
            THEN
               IF (Xpos=x) AND (Ypos=y)
               THEN
                  Success := FALSE
               END
            END
         END ;
         INC (i)
      END
   END
END FreeOfPlayersAndTreasure ;


(*
   HideTreasure - hides treasure, t, which is assummed to be absent from the
                  data structures when this procedure is called.
*)

PROCEDURE HideTreasure (t: CARDINAL) ;
VAR
   ok: BOOLEAN ;
BEGIN
   GetReadAccessToTreasure ;
   RespawnTreasure (GetRandomRoom(NoOfRoomsToHidePlayers, ActualNoOfRooms), t, 0) ;
   ReleaseReadAccessToTreasure
END HideTreasure ;


(*
   GetRandomRoom - returns a random room.
*)

PROCEDURE GetRandomRoom (NoOfRoomsToTraverse, TotalRooms: CARDINAL) : CARDINAL ;
VAR
   x, y, r: CARDINAL ;
BEGIN
   RandomNumber(x, NoOfRoomsToTraverse) ;
   INC(x) ;
   RandomNumber(y, TotalRooms) ;
   INC(y) ;
   RandomRoom(y, x, r) ;
   RETURN( r )
END GetRandomRoom ;


PROCEDURE Positioning ;
VAR
   Attempt,
   i, r, x, y, p: CARDINAL ;
   ok           : BOOLEAN ;
BEGIN
   Attempt := MaxNoOfPlayers ;
   p := PlayerNo() ;
   WITH Player[p] DO
      GetWriteAccessToPlayer ;
      GetReadAccessToDoor ;
      GetReadAccessToTreasure ;
      REPEAT
         r := GetRandomRoom(NoOfRoomsToHidePlayers, ActualNoOfRooms) ;
         ok := TRUE ;
         IF Attempt>0
         THEN
            FOR i := 0 TO NextFreePlayer-1 DO
               IF IsPlayerActive(i) AND (i#p)
               THEN
                  IF r=Player[i].RoomOfMan
                  THEN
                     ok := FALSE
                  END
               END
            END ;
            DEC(Attempt)
         END ;
         IF ok
         THEN
            PositionInRoom(r, x, y, ok)
         END
      UNTIL ok ;
      ScreenX := x-(x MOD Width) ;
      ScreenY := y-(y MOD Height) ;
      RoomOfMan := r ;
      Xman := x ;
      Yman := y ;
      ScaleSights(Xman, Yman, ScreenX, ScreenY, ok) ;
      ReleaseReadAccessToTreasure ;
      ReleaseReadAccessToDoor ;
      ReleaseWriteAccessToPlayer
   END
END Positioning ;


(* Miscellaneous routines that connect the screen to the main program *)


PROCEDURE InitialDisplay ;
BEGIN
   InitScreen(PlayerNo()) ;
   GetReadAccessToPlayer ;
   DrawRoom ;
   ReleaseReadAccessToPlayer
END InitialDisplay ;


END AdvUtil.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
