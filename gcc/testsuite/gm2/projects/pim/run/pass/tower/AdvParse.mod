(* it is advisable not to edit this file as it was automatically generated from the grammer file AdvParse.bnf *)
# 2 "AdvParse.bnf"

IMPLEMENTATION MODULE AdvParse ;

(*
   Author     : Gaius Mulley
   Title      : AdvParse
   Date       : 16/7/2005
   SYSTEM     : GNU Modula-2
   Description: parses maps.
*)

FROM libc IMPORT printf ;
FROM SYSTEM IMPORT ADDRESS ;
FROM DynamicStrings IMPORT String, string, InitStringCharStar, KillString,
                           InitString, ConCat, ConCatChar, Mark ;
FROM StringConvert IMPORT stoi ;
FROM advflex IMPORT toktype, OpenSource, CloseSource, error, GetToken,
                    currenttoken, currentinteger ;
FROM AdvMap IMPORT Rooms, Line, DoorStatus, Door, Room, Treasure,
                   ActualNoOfRooms, MaxNoOfRooms,
                   WallsPerRoom, DoorsPerRoom, TreasureKind ;
FROM AdvUtil IMPORT HideTreasure ;

FROM AdvMath IMPORT MaxNoOfTreasures ;


CONST
   Debugging = TRUE ;

TYPE
   SetOfTok = SET OF toktype ;

VAR
   LastInt,
   ExitValue     : INTEGER ;

   CurDoor,
   CurWall,
   CurRoom       : CARDINAL ;


(*
   Min -
*)

PROCEDURE Min (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   Max -
*)

PROCEDURE Max (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   OpenFile - attempts to open a file, mapfile.
*)

PROCEDURE OpenFile (mapfile: ADDRESS) : INTEGER ;
VAR
   r: INTEGER ;
BEGIN
   ExitValue := 0 ;
   IF OpenSource (mapfile)
   THEN
      RETURN 0
   ELSE
      r := printf ("cannot open file: %s\n", mapfile) ;
      RETURN 1
   END ;
END OpenFile ;


(*
   CloseFile -
*)

PROCEDURE CloseFile ;
BEGIN
   CloseSource
END CloseFile ;

(*
   expecting token set defined as an enumerated type
   (eoftok, roomtok, doortok, walltok, treasuretok, attok, leadstok, totok, statustok, closedtok, opentok, secrettok, istok, endtok, enddottok, integertok, randomizetok) ;
*)

(* %%%FORWARD%%%
PROCEDURE Integer (stopset: SetOfTok) ; FORWARD ;
PROCEDURE FileUnit (stopset: SetOfTok) ; FORWARD ;
PROCEDURE RoomDesc (stopset: SetOfTok) ; FORWARD ;
PROCEDURE WallDesc (stopset: SetOfTok) ; FORWARD ;
PROCEDURE WallCoords (stopset: SetOfTok) ; FORWARD ;
PROCEDURE DoorDesc (stopset: SetOfTok) ; FORWARD ;
PROCEDURE DoorCoords (stopset: SetOfTok) ; FORWARD ;
PROCEDURE Status (stopset: SetOfTok) ; FORWARD ;
PROCEDURE TreasureDesc (stopset: SetOfTok) ; FORWARD ;
PROCEDURE RandomTreasure (stopset: SetOfTok) ; FORWARD ;
   %%%FORWARD%%% *)

(*
   DescribeStop - issues a message explaining what tokens were expected
*)

PROCEDURE DescribeStop (stopset: SetOfTok) : String ;
VAR
   n      : CARDINAL ;
   str,
   message: String ;
BEGIN
   n := 0 ;
   message := InitString('') ;
   IF randomizetok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`RANDOMIZE'"))) ; INC(n)
   END ;
   IF integertok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`integer number'"))) ; INC(n)
   END ;
   IF enddottok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`END.'"))) ; INC(n)
   END ;
   IF endtok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`END'"))) ; INC(n)
   END ;
   IF istok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`IS'"))) ; INC(n)
   END ;
   IF secrettok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`SECRET'"))) ; INC(n)
   END ;
   IF opentok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`OPEN'"))) ; INC(n)
   END ;
   IF closedtok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`CLOSED'"))) ; INC(n)
   END ;
   IF statustok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`STATUS'"))) ; INC(n)
   END ;
   IF totok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`TO'"))) ; INC(n)
   END ;
   IF leadstok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`LEADS'"))) ; INC(n)
   END ;
   IF attok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`AT'"))) ; INC(n)
   END ;
   IF treasuretok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`TREASURE'"))) ; INC(n)
   END ;
   IF walltok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`WALL'"))) ; INC(n)
   END ;
   IF doortok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`DOOR'"))) ; INC(n)
   END ;
   IF roomtok IN stopset
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`ROOM'"))) ; INC(n)
   END ;
   IF eoftok IN stopset
   THEN
      (* eoftok has no token name (needed to generate error messages) *)
   END ;

   IF n=0
   THEN
      str := InitString(' syntax error') ;
      message := KillString(message) ;
   ELSIF n=1
   THEN
      str := ConCat(message, Mark(InitString(' missing '))) ;
   ELSE
      str := ConCat(InitString(' expecting one of'), message) ;
      message := KillString(message) ;
   END ;
   RETURN( str )
END DescribeStop ;


(*
   DescribeError - issues a message explaining what tokens were expected
*)

PROCEDURE DescribeError (stopset: SetOfTok) ;
VAR
   str: String ;
BEGIN
   str := InitString('') ;
   CASE currenttoken OF

   randomizetok: str := ConCat(InitString("syntax error, found `RANDOMIZE'"), Mark(str)) |
   integertok: str := ConCat(InitString("syntax error, found `integer number'"), Mark(str)) |
   enddottok: str := ConCat(InitString("syntax error, found `END.'"), Mark(str)) |
   endtok: str := ConCat(InitString("syntax error, found `END'"), Mark(str)) |
   istok: str := ConCat(InitString("syntax error, found `IS'"), Mark(str)) |
   secrettok: str := ConCat(InitString("syntax error, found `SECRET'"), Mark(str)) |
   opentok: str := ConCat(InitString("syntax error, found `OPEN'"), Mark(str)) |
   closedtok: str := ConCat(InitString("syntax error, found `CLOSED'"), Mark(str)) |
   statustok: str := ConCat(InitString("syntax error, found `STATUS'"), Mark(str)) |
   totok: str := ConCat(InitString("syntax error, found `TO'"), Mark(str)) |
   leadstok: str := ConCat(InitString("syntax error, found `LEADS'"), Mark(str)) |
   attok: str := ConCat(InitString("syntax error, found `AT'"), Mark(str)) |
   treasuretok: str := ConCat(InitString("syntax error, found `TREASURE'"), Mark(str)) |
   walltok: str := ConCat(InitString("syntax error, found `WALL'"), Mark(str)) |
   doortok: str := ConCat(InitString("syntax error, found `DOOR'"), Mark(str)) |
   roomtok: str := ConCat(InitString("syntax error, found `ROOM'"), Mark(str)) |
   eoftok: str := ConCat(InitString("syntax error, found `'"), Mark(str))
   ELSE
   END ;
   ErrorString(str) ;
END DescribeError ;
# 99 "AdvParse.bnf"



(*
   ErrorArray -
*)

PROCEDURE ErrorArray (a: ARRAY OF CHAR) ;
BEGIN
   ErrorString(InitString(a))
END ErrorArray ;


(*
   ErrorString -
*)

PROCEDURE ErrorString (s: String) ;
BEGIN
   error(string(s)) ;
   ExitValue := 1
END ErrorString ;


(*
   SyntaxError - after a syntax error we skip all tokens up until we reach
                 a stop symbol.
*)

PROCEDURE SyntaxError (stopset: SetOfTok) ;
VAR
   r: INTEGER ;
BEGIN
   DescribeError(stopset) ;
   IF Debugging
   THEN
      r := printf('\nskipping token *** ')
   END ;
   WHILE NOT (currenttoken IN stopset)
   DO
      GetToken
   END ;
   IF Debugging
   THEN
      r := printf(' ***\n')
   END ;
   ExitValue := 1
END SyntaxError ;


(*
   SyntaxCheck -
*)

PROCEDURE SyntaxCheck (stopset: SetOfTok) ;
BEGIN
   IF NOT (currenttoken IN stopset)
   THEN
      SyntaxError(stopset)
   END
END SyntaxCheck ;


(*
   WarnMissingToken - generates a warning message about a missing token, t.
*)

PROCEDURE WarnMissingToken (t: toktype) ;
VAR
   s  : SetOfTok ;
   str: String ;
BEGIN
   s := SetOfTok{t} ;
   str := DescribeStop(s) ;

   str := ConCat(InitString('syntax error,'), Mark(str)) ;
   ErrorString(str)
END WarnMissingToken ;


(*
   MissingToken - generates a warning message about a missing token, t.
*)

PROCEDURE MissingToken (t: toktype) ;
VAR
   r: INTEGER ;
BEGIN
   WarnMissingToken(t)
END MissingToken ;


(*
   InStopSet
*)

PROCEDURE InStopSet (t: toktype; stopset: SetOfTok) : BOOLEAN ;
BEGIN
   RETURN t IN stopset
END InStopSet ;


(*
   Expect -
*)

PROCEDURE Expect (t: toktype; stopset: SetOfTok) ;
BEGIN
   IF currenttoken=t
   THEN
      GetToken
   ELSE
      MissingToken(t)
   END ;
   SyntaxCheck(stopset)
END Expect ;


PROCEDURE ParseMap (a: ADDRESS) : INTEGER ;
VAR
   r: INTEGER ;
BEGIN
   r := OpenFile(a) ;
   IF r=0
   THEN
      GetToken ;
      FileUnit(SetOfTok{eoftok}) ;
      CloseFile ;
      RETURN( ExitValue )
   ELSE
      RETURN( r )
   END
END ParseMap ;


(*
   Integer -
*)

PROCEDURE Integer (stopset: SetOfTok) ;
BEGIN
   LastInt := currentinteger ;
   Expect(integertok, stopset)
END Integer ;


(*
   Integer :=

   first  symbols:integertok

   cannot reachend
*)
(*
   FileUnit := RoomDesc { RoomDesc  } [ RandomTreasure  ] 'END.'

   first  symbols:roomtok

   cannot reachend
*)

# 274 "AdvParse.bnf"
PROCEDURE FileUnit (stopset: SetOfTok) ;
# 274 "AdvParse.bnf"
BEGIN
# 274 "AdvParse.bnf"
   RoomDesc(stopset + SetOfTok{enddottok, roomtok, randomizetok}) ;
# 274 "AdvParse.bnf"
   WHILE currenttoken=roomtok DO
      RoomDesc(stopset + SetOfTok{enddottok, randomizetok, roomtok}) ;
   END (* while *) ;
# 274 "AdvParse.bnf"
   IF currenttoken=randomizetok
   THEN
      RandomTreasure(stopset + SetOfTok{enddottok}) ;
   END ;
# 274 "AdvParse.bnf"
   Expect(enddottok, stopset) ;
END FileUnit ;


(*
   RoomDesc := 'ROOM' Integer
               % VAR r: INTEGER ;  %

               % CurRoom := LastInt ;
                 ActualNoOfRooms := Max(CurRoom,
                                        ActualNoOfRooms) ;
                 WITH Rooms[CurRoom] DO
                    NoOfWalls := 0 ;
                    NoOfDoors := 0 ;
                    Treasures := {}
                 END ;
                 IF Debugging
                 THEN
                    r := printf('reading room %d\n', CurRoom)
                 END  %
               { WallDesc  | DoorDesc  | TreasureDesc  } 'END'

   first  symbols:roomtok

   cannot reachend
*)

# 276 "AdvParse.bnf"
PROCEDURE RoomDesc (stopset: SetOfTok) ;
VAR
 r: INTEGER ;
# 276 "AdvParse.bnf"
BEGIN
# 276 "AdvParse.bnf"
   Expect(roomtok, stopset + SetOfTok{integertok}) ;
# 276 "AdvParse.bnf"
   Integer(stopset + SetOfTok{endtok, walltok, doortok, treasuretok}) ;
# 276 "AdvParse.bnf"
# 277 "AdvParse.bnf"
# 288 "AdvParse.bnf"
   CurRoom := LastInt ;
   ActualNoOfRooms := Max(CurRoom,
                          ActualNoOfRooms) ;
   WITH Rooms[CurRoom] DO
      NoOfWalls := 0 ;
      NoOfDoors := 0 ;
      Treasures := {}
   END ;
   IF Debugging
   THEN
      r := printf('reading room %d\n', CurRoom)
   END  ;
# 289 "AdvParse.bnf"
   IF (currenttoken IN SetOfTok{treasuretok, doortok, walltok})
   THEN
      (* seen optional { | } expression *)
      WHILE (currenttoken IN SetOfTok{treasuretok, doortok, walltok}) DO
# 289 "AdvParse.bnf"
         IF currenttoken=walltok
         THEN
            WallDesc(stopset + SetOfTok{endtok, treasuretok, doortok, walltok}) ;
# 289 "AdvParse.bnf"
         ELSIF currenttoken=doortok
         THEN
            DoorDesc(stopset + SetOfTok{endtok, treasuretok, doortok, walltok}) ;
# 289 "AdvParse.bnf"
         ELSIF currenttoken=treasuretok
         THEN
            TreasureDesc(stopset + SetOfTok{endtok, treasuretok, doortok, walltok}) ;
         END ;
         (* end of optional { | } expression *)
      END ;
   END ;
# 289 "AdvParse.bnf"
Expect(endtok, stopset) ;
END RoomDesc ;


(*
   WallDesc := 'WALL' WallCoords { WallCoords  }

   first  symbols:walltok

   cannot reachend
*)

# 291 "AdvParse.bnf"
PROCEDURE WallDesc (stopset: SetOfTok) ;
# 291 "AdvParse.bnf"
BEGIN
# 291 "AdvParse.bnf"
   Expect(walltok, stopset + SetOfTok{integertok}) ;
# 291 "AdvParse.bnf"
   WallCoords(stopset + SetOfTok{integertok}) ;
# 291 "AdvParse.bnf"
   WHILE currenttoken=integertok DO
      WallCoords(stopset + SetOfTok{integertok}) ;
   END (* while *) ;
END WallDesc ;


(*
   WallCoords :=
                 % WITH Rooms[CurRoom] DO
                      INC(NoOfWalls) ;
                      IF NoOfWalls>WallsPerRoom
                      THEN
                         ErrorArray('too many walls') ;
                         NoOfWalls := WallsPerRoom
                      END ;
                      CurWall := NoOfWalls
                   END  %
                 Integer
                 % VAR x1, y1, x2, y2: INTEGER ;  %

                 % x1 := LastInt  %
                 Integer
                 % y1 := LastInt  %
                 Integer
                 % x2 := LastInt  %
                 Integer
                 % y2 := LastInt ;
                   WITH Rooms[CurRoom].Walls[CurWall] DO
                      X1 := Min(x1, x2) ;
                      Y1 := Min(y1, y2) ;
                      X2 := Max(x1, x2) ;
                      Y2 := Max(y1, y2) ;
                      IF (X1#X2) AND (Y1#Y2)
                      THEN
                         error(string(InitString("not allowed diagonal wall")))
                      END
                   END  %


   first  symbols:integertok

   cannot reachend
*)

# 293 "AdvParse.bnf"
PROCEDURE WallCoords (stopset: SetOfTok) ;
VAR
 x1, y1, x2, y2: INTEGER ;
# 293 "AdvParse.bnf"
BEGIN
# 293 "AdvParse.bnf"
# 301 "AdvParse.bnf"
   WITH Rooms[CurRoom] DO
      INC(NoOfWalls) ;
      IF NoOfWalls>WallsPerRoom
      THEN
         ErrorArray('too many walls') ;
         NoOfWalls := WallsPerRoom
      END ;
      CurWall := NoOfWalls
   END  ;
# 302 "AdvParse.bnf"
   Integer(stopset + SetOfTok{integertok}) ;
# 302 "AdvParse.bnf"
# 303 "AdvParse.bnf"
   x1 := LastInt  ;
# 304 "AdvParse.bnf"
   Integer(stopset + SetOfTok{integertok}) ;
# 304 "AdvParse.bnf"
   y1 := LastInt  ;
# 306 "AdvParse.bnf"
   Integer(stopset + SetOfTok{integertok}) ;
# 306 "AdvParse.bnf"
   x2 := LastInt  ;
# 308 "AdvParse.bnf"
   Integer(stopset) ;
# 308 "AdvParse.bnf"
# 318 "AdvParse.bnf"
   y2 := LastInt ;
   WITH Rooms[CurRoom].Walls[CurWall] DO
      X1 := Min(x1, x2) ;
      Y1 := Min(y1, y2) ;
      X2 := Max(x1, x2) ;
      Y2 := Max(y1, y2) ;
      IF (X1#X2) AND (Y1#Y2)
      THEN
         error(string(InitString("not allowed diagonal wall")))
      END
   END  ;
END WallCoords ;


(*
   DoorDesc := 'DOOR' DoorCoords { DoorCoords  }

   first  symbols:doortok

   cannot reachend
*)

# 321 "AdvParse.bnf"
PROCEDURE DoorDesc (stopset: SetOfTok) ;
# 321 "AdvParse.bnf"
BEGIN
# 321 "AdvParse.bnf"
   Expect(doortok, stopset + SetOfTok{integertok}) ;
# 321 "AdvParse.bnf"
   DoorCoords(stopset + SetOfTok{integertok}) ;
# 321 "AdvParse.bnf"
   WHILE currenttoken=integertok DO
      DoorCoords(stopset + SetOfTok{integertok}) ;
   END (* while *) ;
END DoorDesc ;


(*
   DoorCoords :=
                 % WITH Rooms[CurRoom] DO
                      INC(NoOfDoors) ;
                      IF NoOfDoors>DoorsPerRoom
                      THEN
                         ErrorArray('too many doors') ;
                         NoOfDoors := DoorsPerRoom
                      END ;
                      CurDoor := NoOfDoors
                   END  %
                 Integer
                 % VAR x1, y1, x2, y2: INTEGER ;  %

                 % x1 := LastInt  %
                 Integer
                 % y1 := LastInt  %
                 Integer
                 % x2 := LastInt  %
                 Integer
                 % y2 := LastInt ;
                   WITH Rooms[CurRoom].Doors[CurDoor].Position DO
                      X1 := Min(x1, x2) ;
                      Y1 := Min(y1, y2) ;
                      X2 := Max(x1, x2) ;
                      Y2 := Max(y1, y2) ;
                      IF (X1#X2) AND (Y1#Y2)
                      THEN
                         error(string(InitString("not allowed diagonal door")))
                      END

                   END  %
                 Status 'LEADS' 'TO' Integer
                 % Rooms[CurRoom].Doors[CurDoor].LeadsTo := LastInt  %


   first  symbols:integertok

   cannot reachend
*)

# 323 "AdvParse.bnf"
PROCEDURE DoorCoords (stopset: SetOfTok) ;
VAR
 x1, y1, x2, y2: INTEGER ;
# 323 "AdvParse.bnf"
BEGIN
# 323 "AdvParse.bnf"
# 331 "AdvParse.bnf"
   WITH Rooms[CurRoom] DO
      INC(NoOfDoors) ;
      IF NoOfDoors>DoorsPerRoom
      THEN
         ErrorArray('too many doors') ;
         NoOfDoors := DoorsPerRoom
      END ;
      CurDoor := NoOfDoors
   END  ;
# 332 "AdvParse.bnf"
   Integer(stopset + SetOfTok{integertok}) ;
# 332 "AdvParse.bnf"
# 333 "AdvParse.bnf"
   x1 := LastInt  ;
# 334 "AdvParse.bnf"
   Integer(stopset + SetOfTok{integertok}) ;
# 334 "AdvParse.bnf"
   y1 := LastInt  ;
# 336 "AdvParse.bnf"
   Integer(stopset + SetOfTok{integertok}) ;
# 336 "AdvParse.bnf"
   x2 := LastInt  ;
# 338 "AdvParse.bnf"
   Integer(stopset + SetOfTok{statustok}) ;
# 338 "AdvParse.bnf"
# 349 "AdvParse.bnf"
   y2 := LastInt ;
   WITH Rooms[CurRoom].Doors[CurDoor].Position DO
      X1 := Min(x1, x2) ;
      Y1 := Min(y1, y2) ;
      X2 := Max(x1, x2) ;
      Y2 := Max(y1, y2) ;
      IF (X1#X2) AND (Y1#Y2)
      THEN
         error(string(InitString("not allowed diagonal door")))
      END

   END  ;
# 352 "AdvParse.bnf"
   Status(stopset + SetOfTok{leadstok}) ;
# 352 "AdvParse.bnf"
   Expect(leadstok, stopset + SetOfTok{totok}) ;
# 352 "AdvParse.bnf"
   Expect(totok, stopset + SetOfTok{integertok}) ;
# 352 "AdvParse.bnf"
   Integer(stopset) ;
# 352 "AdvParse.bnf"
   Rooms[CurRoom].Doors[CurDoor].LeadsTo := LastInt  ;
END DoorCoords ;


(*
   Status := 'STATUS' ( 'OPEN'
                        % Rooms[CurRoom].Doors[CurDoor].StateOfDoor := Open  %
                         | 'CLOSED'
                        % Rooms[CurRoom].Doors[CurDoor].StateOfDoor := Closed  %
                         | 'SECRET'
                        % Rooms[CurRoom].Doors[CurDoor].StateOfDoor := Secret  %
                         )

   first  symbols:statustok

   cannot reachend
*)

# 355 "AdvParse.bnf"
PROCEDURE Status (stopset: SetOfTok) ;
# 355 "AdvParse.bnf"
BEGIN
# 355 "AdvParse.bnf"
   Expect(statustok, stopset + SetOfTok{opentok, closedtok, secrettok}) ;
# 355 "AdvParse.bnf"
   IF currenttoken=opentok
   THEN
      Expect(opentok, stopset) ;
# 355 "AdvParse.bnf"
      Rooms[CurRoom].Doors[CurDoor].StateOfDoor := Open  ;
# 356 "AdvParse.bnf"
   ELSIF currenttoken=closedtok
   THEN
      Expect(closedtok, stopset) ;
# 356 "AdvParse.bnf"
      Rooms[CurRoom].Doors[CurDoor].StateOfDoor := Closed  ;
# 357 "AdvParse.bnf"
   ELSIF currenttoken=secrettok
   THEN
      Expect(secrettok, stopset) ;
# 357 "AdvParse.bnf"
      Rooms[CurRoom].Doors[CurDoor].StateOfDoor := Secret  ;
   ELSE
      ErrorArray('expecting one of: SECRET CLOSED OPEN')
   END ;
END Status ;


(*
   TreasureDesc := 'TREASURE' 'AT' Integer
                   % VAR x, y: INTEGER ;  %

                   % x := LastInt  %
                   Integer
                   % y := LastInt  %
                   'IS' Integer
                   % WITH Treasure[LastInt] DO
                        Xpos := x ;
                        Ypos := y ;
                        Rm := CurRoom
                     END ;
                     INCL(Rooms[CurRoom].Treasures, LastInt)  %


   first  symbols:treasuretok

   cannot reachend
*)

# 361 "AdvParse.bnf"
PROCEDURE TreasureDesc (stopset: SetOfTok) ;
VAR
 x, y: INTEGER ;
# 361 "AdvParse.bnf"
BEGIN
# 361 "AdvParse.bnf"
   Expect(treasuretok, stopset + SetOfTok{attok}) ;
# 361 "AdvParse.bnf"
   Expect(attok, stopset + SetOfTok{integertok}) ;
# 362 "AdvParse.bnf"
   Integer(stopset + SetOfTok{integertok}) ;
# 362 "AdvParse.bnf"
# 363 "AdvParse.bnf"
   x := LastInt  ;
# 364 "AdvParse.bnf"
   Integer(stopset + SetOfTok{istok}) ;
# 364 "AdvParse.bnf"
   y := LastInt  ;
# 365 "AdvParse.bnf"
   Expect(istok, stopset + SetOfTok{integertok}) ;
# 365 "AdvParse.bnf"
   Integer(stopset) ;
# 365 "AdvParse.bnf"
# 370 "AdvParse.bnf"
   WITH Treasure[LastInt] DO
      Xpos := x ;
      Ypos := y ;
      Rm := CurRoom ;
      kind := onfloor
   END ;
   INCL(Rooms[CurRoom].Treasures, VAL(CARDINAL, LastInt))
END TreasureDesc ;


(*
   RandomTreasure := 'RANDOMIZE' 'TREASURE' Integer
                     % HideTreasure(LastInt)  %
                     { Integer
                       % HideTreasure(LastInt)  %
                        }

   first  symbols:randomizetok

   cannot reachend
*)

# 373 "AdvParse.bnf"
PROCEDURE RandomTreasure (stopset: SetOfTok) ;
# 373 "AdvParse.bnf"
BEGIN
# 373 "AdvParse.bnf"
   Expect(randomizetok, stopset + SetOfTok{treasuretok}) ;
# 373 "AdvParse.bnf"
   Expect(treasuretok, stopset + SetOfTok{integertok}) ;
# 373 "AdvParse.bnf"
   Integer(stopset + SetOfTok{integertok}) ;
# 373 "AdvParse.bnf"
   HideTreasure(LastInt)  ;
# 374 "AdvParse.bnf"
   WHILE currenttoken=integertok DO
      Integer(stopset + SetOfTok{integertok}) ;
# 374 "AdvParse.bnf"
      HideTreasure(LastInt)  ;
   END (* while *) ;
END RandomTreasure ;


# 245 "AdvParse.bnf"



END AdvParse.
