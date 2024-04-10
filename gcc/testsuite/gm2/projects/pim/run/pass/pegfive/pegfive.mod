(* Copyright (C) 2007 Free Software Foundation, Inc. *)
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

MODULE pegfive ;


FROM SYSTEM IMPORT BYTE ;
FROM NumberIO IMPORT WriteCard, StrToCard, WriteInt ;
FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM StdIO IMPORT Write ;
FROM StrLib IMPORT StrLen, StrRemoveWhitePrefix, StrEqual ;
FROM Selective IMPORT Timeval, GetTimeOfDay, GetTime, InitTime, KillTime ;
FROM FIO IMPORT FlushBuffer, StdOut ;


CONST
   EnableGame     =           FALSE ;  (* Set to TRUE if you want to play
                                          the game.  *)
   BoardX         =              16 ;
   BoardY         =              16 ;
   BoardSize      = BoardX * BoardY ;
   AreaRadius     =               1 ;
   MaxScore       =          100000 ;
   MinScore       =         -100000 ;
   OpenFour       =           10000 ;
   OpenThree      =            1000 ;
   OpenTwo        =             100 ;
   WinScore       =        MaxScore ;
   LooseScore     =       -WinScore ;
   MaxTimePerMove =               8 ;
   MinTimePerMove =               3 ;

TYPE
   Squares = [0..BoardSize-1] ;

   Moves   = RECORD
                NoOfPegs: CARDINAL ;
                Tiles   : ARRAY [0..BoardSize] OF BYTE ;
             END ;

   Board   = RECORD
                Pegs    : Moves ;
                Colours : SetOfSquare ;
             END ;

   SetOfSquare = SET OF Squares ;

   Colour = (black, white) ;


VAR
   count,
   NoOfPlies: CARDINAL ;


(*
   InitBoard - initialize the board.
*)

PROCEDURE InitBoard (VAR b: Board) ;
BEGIN
   b.Pegs.NoOfPegs := 0 ;
   b.Colours := SetOfSquare{}
END InitBoard ;


(*
   CreateListOfMachineMoves - given board, b, generate a list of possible
                              moves to consider in, m, for colour, c.
*)

PROCEDURE CreateListOfMoves (VAR b: Board; VAR m: Moves; c: Colour) ;
VAR
   i      : CARDINAL ;
   t, j, n: INTEGER ;
BEGIN
   m.NoOfPegs := 0 ;
   n := 0 ;
   i := 0 ;
   WHILE i<b.Pegs.NoOfPegs DO
      t := VAL(INTEGER, b.Pegs.Tiles[i]) ;
      FOR j := 1 TO AreaRadius DO
         IfFreeAdd(b, m, (t-1)*j, c) ;
         IfFreeAdd(b, m, (t+1)*j, c) ;
         IfFreeAdd(b, m, (t-BoardX)*j, c) ;
         IfFreeAdd(b, m, (t+BoardX)*j, c) ;
         IfFreeAdd(b, m, (t-(BoardX+1))*j, c) ;
         IfFreeAdd(b, m, (t+(BoardX+1))*j, c) ;
         IfFreeAdd(b, m, (t-(BoardX-1))*j, c) ;
         IfFreeAdd(b, m, (t+(BoardX-1))*j, c)
      END ;
      INC(i)
   END
END CreateListOfMoves ;


(*
   IfFreeAdd - tests to see whether position, i, is legal
               and adds it to the list of moves, m.
*)

PROCEDURE IfFreeAdd (VAR b: Board; VAR m: Moves; i: INTEGER; c: Colour) ;
VAR
   n: CARDINAL ;
BEGIN
   IF (i>=0) AND (i<BoardSize)
   THEN
      n := 0 ;
      WHILE n<b.Pegs.NoOfPegs DO
         IF VAL(INTEGER, b.Pegs.Tiles[n])=i
         THEN
            RETURN
         END ;
         INC(n)
      END ;
      (* it is free, so add it to, m *)
      m.Tiles[m.NoOfPegs] := VAL(BYTE, i) ;
      INC(m.NoOfPegs) ;
   END
END IfFreeAdd ;


(*
   MaximumScore - returns TRUE if the maximim score was found.
*)

PROCEDURE MaximumScore (score: INTEGER) : BOOLEAN ;
BEGIN
   IF (score<=MinScore) OR (score>=MaxScore)
   THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END MaximumScore ;


(*
   IsPositionCol - returns TRUE if position, pos, on board, b, contains a peg
                   of colour, c.
*)

PROCEDURE IsPositionCol (VAR b: Board; pos: CARDINAL; c: Colour) : BOOLEAN ;
VAR
   p: CARDINAL ;
BEGIN
   p := b.Pegs.NoOfPegs ;
   WHILE p>0 DO
      DEC(p) ;
      IF VAL(CARDINAL, b.Pegs.Tiles[p])=pos
      THEN
         IF c=white
         THEN
            RETURN pos IN b.Colours
         ELSE
            RETURN NOT (pos IN b.Colours)
         END
      END
   END ;
   RETURN FALSE
END IsPositionCol ;


(*
   IsPositionEmpty - returns TRUE if position, pos, on board, b, is empty.
*)

PROCEDURE IsPositionEmpty (VAR b: Board; pos: CARDINAL) : BOOLEAN ;
VAR
   p: CARDINAL ;
BEGIN
   p := b.Pegs.NoOfPegs ;
   WHILE p>0 DO
      DEC(p) ;
      IF VAL(CARDINAL, b.Pegs.Tiles[p])=pos
      THEN
         RETURN FALSE
      END
   END ;
   RETURN TRUE
END IsPositionEmpty ;


(*
   CheckDir - search back to at most square, begin, and then search forward to
              at most position, end, for a run of pegs.
*)

PROCEDURE CheckDir (VAR b: Board; pos: INTEGER;
                    col: Colour; dec, inc: INTEGER;
                    begin, end: INTEGER; score: INTEGER) : INTEGER ;
VAR
   newscore: INTEGER ;
   open,
   count   : CARDINAL ;
BEGIN
   WITH b DO
      IF (pos>=begin) AND IsPositionCol(b, pos, col)
      THEN
         WHILE (pos>=begin) AND IsPositionCol(b, pos, col) DO
            pos := pos + dec
         END ;
         open := 0 ;
         IF (pos>=begin) AND IsPositionEmpty(b, pos)
         THEN
            (* open this end *)
            open := 1
         END ;
         pos := pos + inc * 2 ;
         count := 1 ;
         WHILE (pos<=end) AND IsPositionCol(b, pos, col) DO
            pos := pos + inc ;
            INC(count)
         END ;
         IF (pos<end) AND IsPositionEmpty(b, pos)
         THEN
            (* open this end *)
            INC(open)
         END ;
         IF open>1
         THEN
            DEC(open)
         END ;
         CASE count OF

         2: newscore := open*OpenTwo |
         3: newscore := open*OpenThree |
         4: newscore := open*OpenFour |
         5: IF col=white
            THEN
               RETURN MaxScore
            ELSE
               RETURN MinScore
            END
         ELSE
            newscore := 0
         END ;
         IF col=white
         THEN
            RETURN score+newscore
         ELSE
            RETURN score-newscore
         END
      ELSE
         RETURN score
      END
   END
END CheckDir ;


(*
   RemoveScore - removes the value of a line of pegs starting at position, pos,
                 from the, score, and returns the new score.
*)

PROCEDURE RemoveScore (VAR b: Board; pos: INTEGER;
                       dec, inc: INTEGER;
                       begin, end: INTEGER; score: INTEGER) : INTEGER ;
VAR
   col: Colour ;
BEGIN
   IF (pos >= 0) AND (pos < BoardSize)
   THEN
      IF IsPositionEmpty(b, pos)
      THEN
         RETURN score
      ELSE
         IF IsPositionCol(b, pos, white)
         THEN
            col := white
         ELSE
            col := black
         END ;
         score := score - CheckDir(b, pos, col, dec, inc, begin, end, 0)
      END
   END ;
   RETURN score
END RemoveScore ;


(*
   CalcScore - returns the new score if move, pos, is played
               by colour, col.
*)

PROCEDURE CalcScore (VAR b: Board; score: INTEGER;
                     pos: INTEGER; col: Colour) : INTEGER ;
VAR
   s          : INTEGER ;
   x, y,
   lup, rdown,
   ldown, rup,
   up, down,
   left, right: CARDINAL ;
BEGIN
   x := pos MOD BoardX ;
   y := pos DIV BoardX ;
   left := y * BoardX ;
   right := left+BoardX-1 ;
   down := x ;
   up := down + (BoardX*(BoardY-1)) ;

   (* diag left down *)
   IF x>y
   THEN
      ldown := x-y
   ELSE
      ldown := (y-x) * BoardY
   END ;
   (* diag right up *)
   IF x>y
   THEN
      rup := (BoardX-x+y)*BoardY-1
   ELSE
      rup := ((BoardY-1)-y+x) + (BoardY-1)*BoardY
   END ;
   (* diag left up *)
   IF x>=BoardY-y
   THEN
      lup := (BoardY-1)*BoardX+(x-(BoardY-1-y))
   ELSE
      lup := (y+x)*BoardY
   END ;
   (* diag right down *)
   IF y >= BoardX-x
   THEN
      rdown := (y-(BoardX-x-1))*BoardY+BoardX-1
   ELSE
      rdown := x+y
   END ;

   (* firstly remove previous score for both colours from adjacent pegs *)
   score := RemoveScore(b, pos-1, -1, +1, left, right, score) ;
   score := RemoveScore(b, pos+1, +1, -1, left, right, score) ;
   score := RemoveScore(b, pos-BoardX, -BoardX, +BoardX, down, up, score) ;
   score := RemoveScore(b, pos+BoardX, +BoardX, -BoardX, down, up, score) ;

   score := RemoveScore(b, pos+(BoardX-1), +(BoardX-1), -(BoardX-1), rdown, lup, score) ;
   score := RemoveScore(b, pos-(BoardX-1), -(BoardX-1), +(BoardX-1), rdown, lup, score) ;
   score := RemoveScore(b, pos+(BoardX+1), +(BoardX+1), -(BoardX+1), ldown, rup, score) ;
   score := RemoveScore(b, pos-(BoardX+1), -(BoardX+1), +(BoardX+1), ldown, rup, score) ;

   (* now add our new peg *)
   ApplyMove(b, col, pos) ;

   (* and calculate the new score *)

   s := CheckDir(b, pos, col, -1, +1, left, right, 0) ;
   IF MaximumScore(s)
   THEN
      RETURN s
   END ;
   score := score + s ;

   s := CheckDir(b, pos, col, -BoardX, +BoardX, down, up, 0) ;
   IF MaximumScore(s)
   THEN
      RETURN s
   END ;
   score := score + s ;

   s := CheckDir(b, pos, col, -(BoardX+1), +(BoardX+1), ldown, rup, 0) ;
   IF MaximumScore(s)
   THEN
      RETURN s
   END ;
   score := score + s ;

   s := CheckDir(b, pos, col, -(BoardX-1), +(BoardX-1), rdown, lup, 0) ;
   IF MaximumScore(s)
   THEN
      RETURN s
   END ;
   score := score + s ;

   RETURN score
END CalcScore ;


(*
   DisplayBoard - displays the pegfive board.
*)

PROCEDURE DisplayBoard (VAR b: Board) ;
VAR
   pos,
   i, x, y: CARDINAL ;
   written: BOOLEAN ;
BEGIN
   WriteString('    A B C D E F G H I J K L M N O P') ; WriteLn ;
   WriteString('  +---------------------------------+') ; WriteLn ;
   FOR y := BoardY TO 1 BY -1 DO
      WriteCard(y, 2) ; Write('|') ;
      FOR x := 0 TO BoardX-1 DO
         i := b.Pegs.NoOfPegs ;
         written := FALSE ;
         WHILE i>0 DO
            pos := VAL(CARDINAL, b.Pegs.Tiles[i-1]) ;
            IF pos=((y-1)*BoardX)+x
            THEN
               written := TRUE ;
               IF pos IN b.Colours
               THEN
                  WriteString(' O')
               ELSE
                  WriteString(' X')
               END
            END ;
            DEC(i)
         END ;
         IF NOT written
         THEN
            WriteString(' .')
         END
      END ;
      WriteString(' |') ; WriteCard(y, 0) ; WriteLn
   END ;
   WriteString('  +---------------------------------+') ; WriteLn ;
   WriteString('    A B C D E F G H I J K L M N O P') ; WriteLn
END DisplayBoard ;


(*
   WriteColour - displays the colour, c.
*)

PROCEDURE WriteColour (c: Colour) ;
BEGIN
   CASE c OF

   white:  WriteString('naughts') |
   black:  WriteString('crosses')

   END
END WriteColour ;


(*
   ApplyMove - adds move, pos, to board.
*)

PROCEDURE ApplyMove (VAR b: Board; c: Colour; pos: CARDINAL) ;
BEGIN
   IF b.Pegs.NoOfPegs<BoardSize
   THEN
      b.Pegs.Tiles[b.Pegs.NoOfPegs] := VAL(BYTE, pos) ;
      INC(b.Pegs.NoOfPegs) ;
      IF c=white
      THEN
         INCL(b.Colours, pos)
      END
   END
END ApplyMove ;


(*
   RetractMove - removes the last move from the board.
*)

PROCEDURE RetractMove (VAR b: Board) ;
VAR
   pos: CARDINAL ;
BEGIN
   IF b.Pegs.NoOfPegs>0
   THEN
      DEC(b.Pegs.NoOfPegs) ;
      pos := VAL(CARDINAL, b.Pegs.Tiles[b.Pegs.NoOfPegs]) ;
      EXCL(b.Colours, pos)
   END
END RetractMove ;


(*
   AskMove - returns a move entered.
*)

PROCEDURE AskMove (VAR b: Board; c: Colour) : CARDINAL ;
VAR
   s   : ARRAY [0..80] OF CHAR ;
   x   : CHAR ;
   y, m: CARDINAL ;
BEGIN
   LOOP
      WriteString('Please enter your move, ') ;
      WriteColour(c) ;
      WriteString(': ') ;
      LOOP
         ReadString(s) ;
         StrRemoveWhitePrefix(s, s) ;
         IF StrEqual (s, 'exit') OR StrEqual (s, 'quit')
         THEN
            WriteString ('goodbye') ; WriteLn ;
            HALT (0)
         ELSIF StrLen(s)>0
         THEN
            x := CAP(s[0]) ;
            IF (x>='A') AND (x<='P')
            THEN
               m := ORD (x) - ORD ('A') ;
               s[0] := ' ' ;
               StrRemoveWhitePrefix(s, s) ;
               IF StrLen(s)>0
               THEN
                  StrToCard(s, y) ;
                  IF (y=0) OR (y>BoardY)
                  THEN
                     WriteString('Please enter a number between [1-16]') ; WriteLn
                  ELSE
                     m := m+(y-1)*BoardY ;
                     EXIT
                  END
               END
            ELSE
               WriteString('please enter a letter [A-P] followed by a number [1-16]') ; WriteLn
            END
         END
      END ;
      IF IsPositionEmpty(b, m)
      THEN
         RETURN m
      END ;
      IF IsPositionCol(b, m, c)
      THEN
         WriteString('You have already moved into that position') ; WriteLn
      ELSE
         WriteString('That position is occupied by your opponent') ; WriteLn
      END
   END
END AskMove ;


(*
   Opponent - returns the opposite colour.
*)

PROCEDURE Opponent (col: Colour) : Colour ;
BEGIN
   IF col=white
   THEN
      RETURN black
   ELSE
      RETURN white
   END
END Opponent ;


(*
   AlphaBeta - returns the score estimated should move, pos, be chosen.
               The board, b, and score is in the state _before_ move pos
               is made.
*)

PROCEDURE AlphaBeta (pos: CARDINAL; VAR b: Board; col: Colour;
                     depth: CARDINAL;
                     alpha, beta, score: INTEGER) : INTEGER ;
VAR
   try     : INTEGER ;
   i, n    : CARDINAL ;
   m       : Moves ;
   newBoard: Board ;
BEGIN
   score := CalcScore(b, score, pos, col) ;   (* make move and update score *)
   IF (depth=0) OR MaximumScore(score)
   THEN
      RetractMove(b) ;
      INC(count) ;
      IF col=white
      THEN
         RETURN score+VAL(INTEGER, depth)
      ELSE
         RETURN score-VAL(INTEGER, depth)
      END
   ELSE
      col := Opponent(col) ;
      CreateListOfMoves(b, m, col) ;
      i := 0 ;
      IF col=white
      THEN
         WHILE i<m.NoOfPegs DO
            try := AlphaBeta(VAL(CARDINAL, m.Tiles[i]),
                             b, white, depth-1, alpha, beta, score) ;
            IF try > alpha
            THEN
               (* found a better move *)
               alpha := try
            END ;
            IF alpha >= beta
            THEN
               RetractMove(b) ;
               RETURN alpha
            END ;
            INC(i)
         END ;
         RetractMove(b) ;
         RETURN alpha
      ELSE
         (* black to move, move is possible, continue searching *)
         WHILE i<m.NoOfPegs DO
            try := AlphaBeta(VAL(CARDINAL, m.Tiles[i]),
                             b, black, depth-1, alpha, beta, score) ;
            IF try < beta
            THEN
               (* found a better move *)
               beta := try
            END ;
            IF alpha >= beta
            THEN
               (* no point searching further as WHITE would choose
                  a different previous move *)
               RetractMove(b) ;
               RETURN beta
            END ;
            INC(i)
         END ;
         RetractMove(b) ;
         RETURN beta   (* the best score for a move BLACK has found *)
      END
   END
END AlphaBeta ;


(*
   MakeMove - computer makes a move for colour, col.
*)

PROCEDURE MakeMove (VAR b: Board; col: Colour; score: INTEGER) : INTEGER ;
VAR
   start, end: Timeval ;
   try,
   r, best   : INTEGER ;
   secS, usec,
   secE, i,
   move      : CARDINAL ;
   m         : Moves ;
BEGIN
   start := InitTime(0, 0) ;
   end := InitTime(0, 0) ;

   WriteString("I'm going to look ") ;
   WriteCard(NoOfPlies, 0) ; WriteString(' moves ahead') ; WriteLn ;

   r := GetTimeOfDay(start) ;
   best := MinScore-1 ;

   count := 0 ;
   i := 0 ;
   CreateListOfMoves(b, m, col) ;
   WHILE i<m.NoOfPegs DO
      try := AlphaBeta(VAL(CARDINAL, m.Tiles[i]), b, col, NoOfPlies,
                       MinScore, MaxScore, score) ;
      IF try>best
      THEN
         best := try ;
         move := VAL(CARDINAL, m.Tiles[i])
      END ;
      INC(i)
   END ;

   r := GetTimeOfDay(end) ;
   GetTime(start, secS, usec) ;
   GetTime(end, secE, usec) ;

   IF best >= WinScore
   THEN
      WriteString('I think I can force a win') ; WriteLn
   END ;
   IF best <= LooseScore
   THEN
      WriteString('You should be able to force a win') ; WriteLn
   END ;

   WriteString('I took ') ; WriteCard(secE-secS, 0) ;
   WriteString(' seconds and evaluated ') ;
   WriteCard(count, 0) ; WriteString(' positions,') ; WriteLn ;

   IF secE-secS > MaxTimePerMove
   THEN
      WriteString('sorry about the wait, I took too long so') ; WriteLn ;
      WriteString('I will reduce my search next go..') ; WriteLn ;
      IF NoOfPlies >= 3
      THEN
         DEC(NoOfPlies)
      END
   ELSE
      IF secE-secS < MinTimePerMove
      THEN
         INC(NoOfPlies)
      END
   END ;

   start := KillTime(start) ;
   end := KillTime(end) ;
   RETURN move
END MakeMove ;


(*
   Play -
*)

PROCEDURE Play ;
VAR
   b: Board ;
   c: Colour ;
   s: INTEGER ;
   m: CARDINAL ;
BEGIN
   InitBoard(b) ;
   NoOfPlies := 3 ;
   c := black ;
   s := 0 ;
   DisplayBoard(b) ;
   REPEAT
      m := AskMove(b, c) ;
      s := CalcScore(b, s, m, c) ;
      DisplayBoard(b) ;
      WriteString('Current score = ') ; WriteInt(s, 0) ; WriteLn ;
      FlushBuffer(StdOut) ;
      IF s<=MinScore
      THEN
         WriteString('Well done you win') ; WriteLn ;
         RETURN
      END ;
      c := Opponent(c) ;
      m := MakeMove(b, c, s) ;
      s := CalcScore(b, s, m, c) ;

      WriteString('I am going to move to position: ') ;
      Write(CHR(ORD('a')+m MOD BoardY)) ;
      WriteCard(m DIV BoardX+1, 0) ;
      WriteLn ;
      DisplayBoard(b) ;
      WriteString('Current score = ') ; WriteInt(s, 0) ; WriteLn ;
      IF s>=MaxScore
      THEN
         WriteString('Good try, but I win') ; WriteLn ;
         RETURN
      END ;
      c := Opponent(c)
   UNTIL b.Pegs.NoOfPegs=BoardSize ;
   WriteString('The game has ended in a draw as the board is full') ; WriteLn
END Play ;


BEGIN
   IF EnableGame
   THEN
      Play
   ELSE
      WriteString ('to enable the game - edit line 31 in pegfive.mod and recompile') ;
      WriteLn
   END
END pegfive.
