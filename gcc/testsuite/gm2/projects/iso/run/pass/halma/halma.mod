(* Copyright (C) 2010 Free Software Foundation, Inc. *)
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

MODULE halma ;

FROM STextIO IMPORT WriteString, WriteLn, WriteChar, ReadToken, SkipLine ;
FROM SWholeIO IMPORT WriteCard, WriteInt ;
FROM Strings IMPORT Length ;
FROM Selective IMPORT Timeval, GetTimeOfDay, GetTime, InitTime, KillTime ;
FROM WholeStr IMPORT StrToCard, ConvResults ;
FROM SYSTEM IMPORT CARDINAL8 ;

CONST
   TwoPlayer      =            TRUE ;
   FourPlayer     =           FALSE ;
   BoardX         =              16 ;
   BoardY         =              16 ;
   BoardSize      = BoardX * BoardY ;
   Pieces         =              19 ;   (* total pieces per player on the board  *)
   PieceHeap      =            4000 ;   (* maximum moves we will examine per ply *)
   MaxScore       =          100000 ;
   MinScore       =         -100000 ;
   WinScore       =        MaxScore ;
   LooseScore     =       -WinScore ;
   Debugging      =           FALSE ;
   Thinking       =              10 ;   (* how many seconds can the program think? *)
   slowEvaluation =           FALSE ;
   HomeWeight     =          BoardX ;

TYPE
   Squares = [0..BoardSize-1] ;
   SoS     = SET OF Squares ;
   Colour  = (Blue, Red, Green, White) ;

   Board = RECORD
              used  : SoS ;   (* is the square used at all? *)
              colour: ARRAY [0..1] OF SoS ;   (* if so which colour occupies the square? *)
              pieces: ARRAY [MIN(Colour)..MAX(Colour)] OF ARRAY [1..Pieces] OF CARDINAL8 ;
              home  : ARRAY [MIN(Colour)..MAX(Colour)] OF CARDINAL ;
           END ;

   Moves = RECORD
              pieceHead: ARRAY [0..Pieces] OF CARDINAL ;      (* pieceHead[0] is start of peg 1 moves in the heap *)
              pieceList: ARRAY [0..PieceHeap] OF CARDINAL8 ;  (* pieceHead[1] is start of peg 2 moves in the heap *)
           END ;

   Reachable = RECORD
                  no  : CARDINAL ;
                  prev: CARDINAL ;
                  dist: CARDINAL ;
                  list: ARRAY Squares OF CARDINAL ;
               END ;

   Graph = RECORD
              graph: ARRAY Squares OF Reachable ;
           END ;

VAR
   count   : CARDINAL ;
   homeBase: ARRAY [MIN(Colour)..MAX(Colour)] OF SoS ;


(*
   +-----------------------------------------------------------------+
   | 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 |
   |                                                                 |
   | 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 |
   |                                                                 |
   | 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 |
   |                                                                 |
   | 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 |
   |                                                                 |
   | 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 |
   |                                                                 |
   | 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 |
   |                                                                 |
   | 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 |
   |                                                                 |
   | 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 |
   |                                                                 |
   | 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 |
   |                                                                 |
   |  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 |
   |                                                                 |
   |  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 |
   |                                                                 |
   |  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79 |
   |---------                                                        |
   |  48  49 \50  51  52  53  54  55  56  57  58  59  60  61  62  63 |
   |           \                                                     |
   |  32  33  34 \35  36  37  38  39  40  41  42  43  44  45  46  47 |
   |               \                                                 |
   |  16  17  18  19| 20  21  22  23  24  25  26  27  28  29  30  31 |
   |                |                                                |
   |   0   1   2   3|  4   5   6   7   8   9  10  11  12  13  14  15 |
   +-----------------------------------------------------------------+
*)


(*
   stop -
*)

PROCEDURE stop ;
BEGIN
END stop ;


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
   assert -
*)

PROCEDURE assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      WriteString('assert failed') ; WriteLn ;
      HALT
   END
END assert ;


(*
   initGraph - initialise, g, to empty.
*)

PROCEDURE initGraph (VAR g: Graph) ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := MIN(Squares) TO MAX(Squares) DO
      g.graph[i].no   := 0 ;
      g.graph[i].prev := MAX(Squares)+1 ;
      g.graph[i].dist := MAX(Squares)+1
   END
END initGraph ;


(*
   isUsed - return whether a square, p, is in use on board, b.
*)

PROCEDURE isUsed (VAR b: Board; p: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN p IN b.used
END isUsed ;


(*
   isColour - return TRUE if a square, p, is used and contains a
              piece of colour, c.
*)

PROCEDURE isColour (VAR b: Board; p: CARDINAL; c: Colour) : BOOLEAN ;
BEGIN
   WITH b DO
      IF p IN used
      THEN
         CASE c OF

         Blue:  RETURN (NOT (p IN colour[0])) AND (NOT (p IN colour[1])) |
         Red :  RETURN (p IN colour[0]) AND (NOT (p IN colour[1])) |
         Green: RETURN (NOT (p IN colour[0])) AND (p IN colour[1]) |
         White: RETURN (p IN colour[0]) AND (p IN colour[1])

         END
      END
   END ;
   RETURN( FALSE )
END isColour ;


(*
   dumpBase -
*)

PROCEDURE dumpBase (c: Colour) ;
VAR
   n, i: CARDINAL ;
BEGIN
   WriteString('dumpBase(c) where ORD(c)=') ; WriteCard(ORD(c), 3) ; WriteLn ;
   n := 0 ;
   FOR i := 0 TO MAX(Squares) DO
      IF (n>0) AND ((n MOD 16) = 0)
      THEN
         WriteLn
      END ;
      IF i IN homeBase[c]
      THEN
         WriteChar('1')
      ELSE
         WriteChar('0')
      END ;
      INC(n)
   END ;
   WriteLn
END dumpBase ;



(*
   addPiece - adds a piece, pos, of colour, c, to the board, b.
*)

PROCEDURE addPiece (VAR b: Board; pos: CARDINAL; c: Colour; piece: CARDINAL) ;
BEGIN
(*
VAR
   i: CARDINAL ;

   IF pos IN homeBase[c]
   THEN
      WriteString('found ') ; WriteCard(pos, 3) ; WriteString(' in homeBase[c]') ;
      WriteLn ;
      dumpBase(c)
   END ;
*)

   WITH b DO
      INCL(used, pos) ;
      CASE c OF

      Blue:  EXCL(colour[0], pos) ;
             EXCL(colour[1], pos) |
      Red :  INCL(colour[0], pos) ;
             EXCL(colour[1], pos) |
      Green: EXCL(colour[0], pos) ;
             INCL(colour[1], pos) |
      White: INCL(colour[0], pos) ;
             INCL(colour[1], pos)

      END ;
      pieces[c][piece] := pos ;
      IF pos IN homeBase[c]
      THEN
         INC(home[c])
      END
   END
END addPiece ;


(*
   subPiece - removes a piece at, pos, from the board, b.
*)

PROCEDURE subPiece (VAR b: Board; pos: CARDINAL; c: Colour) ;
BEGIN
   WITH b DO
      EXCL(used, pos) ;
      IF pos IN homeBase[c]
      THEN
         DEC(home[c])
      END
   END
END subPiece ;


(*
   ifFreeAdd -
*)

PROCEDURE ifFreeAdd (condition: BOOLEAN; VAR b: Board; t: INTEGER; p: CARDINAL; c: Colour; VAR m: Moves) ;
BEGIN
   IF condition AND (NOT isUsed(b, t)) AND (NOT isRecorded(m, t, p))
   THEN
      recordMove(m, t, p)
   END
END ifFreeAdd ;


(*
   recordMove - adds tile, t, to piece, p, list of moves.
*)

PROCEDURE recordMove (VAR m: Moves; t: INTEGER; p: CARDINAL) ;
BEGIN
   WITH m DO
      pieceList[pieceHead[p]] := t ;
      INC(pieceHead[p])
   END
END recordMove ;


(*
   isRecorded - returns TRUE if tile, t, has been already entered as a
                possible move for piece, p, on move list, m.
*)

PROCEDURE isRecorded (VAR m: Moves; t: INTEGER; p: CARDINAL) : BOOLEAN ;
VAR
   i, j: CARDINAL ;
BEGIN
   WITH m DO
      j := pieceHead[p] ;
      i := pieceHead[p-1]+1 ;
      WHILE i<j DO
         IF pieceList[i] = VAL (CARDINAL8, t)
         THEN
            RETURN( TRUE )
         END ;
         INC(i)
      END
   END ;
   RETURN( FALSE )
END isRecorded ;


(*
   addSingle - adds a single move from a piece, testing all eight one square
               moves.
*)

PROCEDURE addSingle (VAR b: Board; VAR m: Moves; c: Colour; p: CARDINAL) ;
VAR
   t   : INTEGER ;
   x, y: INTEGER ;
BEGIN
   WITH b DO
      t := VAL(INTEGER, pieces[c][p]) ;
      x := t MOD BoardX ;
      y := t DIV BoardX ;
      (* vertical and horizontal *)

      ifFreeAdd(x>0, b, t-1, p, c, m) ;                                  (* -1,  0 *)
      ifFreeAdd(x<BoardX-1, b, t+1, p, c, m) ;                           (*  1,  0 *)
      ifFreeAdd(y>0, b, t-BoardX, p, c, m) ;                             (*  0, -1 *)
      ifFreeAdd(y<BoardY-1, b, t+BoardX, p, c, m) ;                      (*  0,  1 *)

      (* diagonals *)
      ifFreeAdd((x>0) AND (y>0), b, t-(BoardX+1), p, c, m) ;             (* -1, -1 *)
      ifFreeAdd((x<BoardX-1) AND (y<BoardY-1), b, t+BoardX+1, p, c, m) ; (*  1,  1 *)

      ifFreeAdd((x<BoardX-1) AND (y>0), b, t-(BoardX-1), p, c, m) ;      (*  1, -1 *)
      ifFreeAdd((x>0) AND (y<BoardY-1), b, t+(BoardX-1), p, c, m)        (* -1,  1 *)
   END
END addSingle ;


(*
   addMultipleV -
*)

PROCEDURE addMultipleV (VAR b: Board; VAR m: Moves; c: Colour; p: CARDINAL; x, y, i, j: INTEGER) ;
VAR
   t   : CARDINAL ;
   d, e: INTEGER ;
BEGIN
   d := 1 ;
   IF i=0
   THEN
      IF j>0
      THEN
         e := (BoardY-y) DIV 2
      ELSIF j<0
      THEN
         e := y DIV 2
      END
   ELSIF j=0
   THEN
      IF i>0
      THEN
         e := (BoardX-x) DIV 2
      ELSIF i<0
      THEN
         e := x DIV 2
      END
   ELSE
      IF (i=1) AND (j=1)
      THEN
         e := Min((BoardX-x) DIV 2, (BoardY-y) DIV 2)
      ELSIF (i=-1) AND (j=1)
      THEN
         e := Min(x DIV 2, (BoardY-y) DIV 2)
      ELSIF (i=-1) AND (j=-1)
      THEN
         e := Min(x DIV 2, y DIV 2)
      ELSE
         (* 1, -1 *)
         e := Min((BoardX-x) DIV 2, y DIV 2)
      END
   END ;
   LOOP
      IF d>e
      THEN
         (* no point searching further as there is no room for the reflective jump *)
         RETURN
      END ;
      x := x + i ;
      y := y + j ;
      IF (x<0) OR (y<0) OR (x>=BoardX) OR (y>=BoardY)
      THEN
         RETURN
      END ;
      t := VAL(CARDINAL, y)*BoardX+VAL(CARDINAL, x) ;
      IF isUsed(b, t)
      THEN
         (* found pivot, keep looking for the destination *)
         WHILE d>0 DO
            x := x + i ;
            y := y + j ;
         (*
            IF i>=0
            THEN
               INC(x, i)
            ELSE
               DEC(x, -i)
            END ;
            IF j>=0
            THEN
               INC(y, j)
            ELSE
               DEC(y, -j)
            END ;
         *)
            IF (x<0) OR (y<0) OR (x>=BoardX) OR (y>=BoardY)
            THEN
               RETURN
            END ;
            t := VAL(CARDINAL, y)*BoardX+VAL(CARDINAL, x) ;
            IF isUsed(b, t)
            THEN
               RETURN
            END ;
            DEC(d)
         END ;
         IF NOT isRecorded(m, t, p)
         THEN
            IF Debugging
            THEN
               WriteString('adding move ') ; WriteCard(t, 0) ; WriteLn
            END ;
            recordMove(m, t, p)
         END ;
         RETURN
      END ;
      INC(d)
   END
END addMultipleV ;


(*
   addMultiple - adds moves which involve jumping.  Current peg, p, is at at position
                 indicated by, m.pieceList[low].
*)

PROCEDURE addMultiple (VAR b: Board; VAR m: Moves; c: Colour; p: CARDINAL; low: CARDINAL) ;
VAR
   t   : INTEGER ;
   x, y: INTEGER ;
BEGIN
   WITH b DO
      WHILE low<m.pieceHead[p] DO
         t := VAL(INTEGER, m.pieceList[low]) ;
         x := t MOD BoardX ;
         y := t DIV BoardX ;
         addMultipleV(b, m, c, p, x, y, -1,  0) ;
         addMultipleV(b, m, c, p, x, y, -1,  1) ;
         addMultipleV(b, m, c, p, x, y, -1, -1) ;

         addMultipleV(b, m, c, p, x, y,  1,  0) ;
         addMultipleV(b, m, c, p, x, y,  1,  1) ;
         addMultipleV(b, m, c, p, x, y,  1, -1) ;

         addMultipleV(b, m, c, p, x, y,  0,  1) ;
         addMultipleV(b, m, c, p, x, y,  0, -1) ;
         INC(low)
      END
   END
END addMultiple ;


(*
   genMove - generate the moves for peg, p.
*)

PROCEDURE genMove (VAR b: Board; VAR m: Moves; c: Colour; p: CARDINAL) ;
BEGIN
   m.pieceHead[p] := m.pieceHead[p-1] ;
   recordMove(m, b.pieces[c][p], p) ;
   (* record the current position so we can ignore moving back to it *)
   addMultiple(b, m, c, p, m.pieceHead[p]-1) ;
   addSingle(b, m, c, p)
END genMove ;


(*
   genMoves - generate the list of moves for colour, c, on board, b.
              The board, b, is unaltered despite being passed by reference.
*)

PROCEDURE genMoves (VAR b: Board; VAR m: Moves; c: Colour) ;
VAR
   pos,
   peg: CARDINAL ;
BEGIN
   m.pieceHead[0] := 0 ;
   FOR peg := 1 TO Pieces DO
      pos := b.pieces[c][peg] ;
      subPiece(b, pos, c) ;     (* remove this peg while jumping (so we dont jump over ourself) *)
      genMove(b, m, c, peg) ;
      addPiece(b, pos, c, peg)  (* restore the peg *)
   END
END genMoves ;


(*
   addToGraph -
*)

PROCEDURE addToGraph (VAR g: Graph; from, to: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH g.graph[from] DO
      i := 0 ;
      WHILE i<no DO
         IF list[i]=to
         THEN
            RETURN
         ELSE
            INC(i)
         END
      END ;
      list[no] := to ;
      INC(no)
   END
END addToGraph ;


(*
   ifFreeRecord -
*)

PROCEDURE ifFreeRecord (condition: BOOLEAN; VAR b: Board; t: INTEGER; p: CARDINAL;
                        c: Colour; VAR m: Moves; from: CARDINAL; VAR g: Graph) ;
BEGIN
   IF condition AND (NOT isUsed(b, t)) AND (NOT isRecorded(m, t, p))
   THEN
      recordMove(m, t, p) ;
      addToGraph(g, from, t)
   END
END ifFreeRecord ;


(*
   recordSingle - adds a single move from a piece, testing all eight one square
                  moves.
*)

PROCEDURE recordSingle (VAR b: Board; VAR m: Moves; c: Colour;
                        p: CARDINAL; from: CARDINAL; VAR g: Graph) ;
VAR
   t   : INTEGER ;
   x, y: INTEGER ;
BEGIN
   WITH b DO
      t := VAL(INTEGER, pieces[c][p]) ;
      x := t MOD BoardX ;
      y := t DIV BoardX ;
      (* vertical and horizontal *)

      ifFreeRecord(x>0, b, t-1, p, c, m, from, g) ;                                  (* -1,  0 *)
      ifFreeRecord(x<BoardX-1, b, t+1, p, c, m, from, g) ;                           (*  1,  0 *)
      ifFreeRecord(y>0, b, t-BoardX, p, c, m, from, g) ;                             (*  0, -1 *)
      ifFreeRecord(y<BoardY-1, b, t+BoardX, p, c, m, from, g) ;                      (*  0,  1 *)

      (* diagonals *)
      ifFreeRecord((x>0) AND (y>0), b, t-(BoardX+1), p, c, m, from, g) ;             (* -1, -1 *)
      ifFreeRecord((x<BoardX-1) AND (y<BoardY-1), b, t+BoardX+1, p, c, m, from, g) ; (*  1,  1 *)

      ifFreeRecord((x<BoardX-1) AND (y>0), b, t-(BoardX-1), p, c, m, from, g) ;      (*  1, -1 *)
      ifFreeRecord((x>0) AND (y<BoardY-1), b, t+(BoardX-1), p, c, m, from, g)        (* -1,  1 *)
   END
END recordSingle ;


(*
   recordMultipleV -
*)

PROCEDURE recordMultipleV (VAR b: Board; VAR m: Moves; c: Colour;
                           p: CARDINAL; x, y, i, j: INTEGER;
                           from: CARDINAL; VAR g: Graph) ;
VAR
   t   : CARDINAL ;
   d, e: INTEGER ;
BEGIN
   d := 1 ;
   IF i=0
   THEN
      IF j>0
      THEN
         e := (BoardY-y) DIV 2
      ELSIF j<0
      THEN
         e := y DIV 2
      END
   ELSIF j=0
   THEN
      IF i>0
      THEN
         e := (BoardX-x) DIV 2
      ELSIF i<0
      THEN
         e := x DIV 2
      END
   ELSE
      IF (i=1) AND (j=1)
      THEN
         e := Min((BoardX-x) DIV 2, (BoardY-y) DIV 2)
      ELSIF (i=-1) AND (j=1)
      THEN
         e := Min(x DIV 2, (BoardY-y) DIV 2)
      ELSIF (i=-1) AND (j=-1)
      THEN
         e := Min(x DIV 2, y DIV 2)
      ELSE
         (* 1, -1 *)
         e := Min((BoardX-x) DIV 2, y DIV 2)
      END
   END ;
   LOOP
      IF d>e
      THEN
         (* no point searching further as there is no room for the reflective jump *)
         RETURN
      END ;
      x := x + i ;
      y := y + j ;
      IF (x<0) OR (y<0) OR (x>=BoardX) OR (y>=BoardY)
      THEN
         RETURN
      END ;
      t := VAL(CARDINAL, y)*BoardX+VAL(CARDINAL, x) ;
      IF isUsed(b, t)
      THEN
         (* found pivot, keep looking for the destination *)
         WHILE d>0 DO
            x := x + i ;
            y := y + j ;
            IF (x<0) OR (y<0) OR (x>=BoardX) OR (y>=BoardY)
            THEN
               RETURN
            END ;
            t := VAL(CARDINAL, y)*BoardX+VAL(CARDINAL, x) ;
            IF isUsed(b, t)
            THEN
               RETURN
            END ;
            DEC(d)
         END ;
         IF NOT isRecorded(m, t, p)
         THEN
            IF Debugging
            THEN
               WriteString('adding move ') ; WriteCard(t, 0) ; WriteLn
            END ;
            recordMove(m, t, p) ;
            addToGraph(g, from, t) ;
            addToGraph(g, t, from)
         END ;
         RETURN
      END ;
      INC(d)
   END
END recordMultipleV ;


(*
   recordMultiple - adds moves which involve jumping.  Current peg, p, is at at position
                    indicated by, m.pieceList[low].
*)

PROCEDURE recordMultiple (VAR b: Board; VAR m: Moves; c: Colour;
                          p: CARDINAL; low: CARDINAL; VAR g: Graph) ;
VAR
   from: INTEGER ;
   x, y: INTEGER ;
BEGIN
   WITH b DO
      WHILE low<m.pieceHead[p] DO
         from := VAL(INTEGER, m.pieceList[low]) ;
         x := from MOD BoardX ;
         y := from DIV BoardX ;
         recordMultipleV(b, m, c, p, x, y, -1,  0, from, g) ;
         recordMultipleV(b, m, c, p, x, y, -1,  1, from, g) ;
         recordMultipleV(b, m, c, p, x, y, -1, -1, from, g) ;

         recordMultipleV(b, m, c, p, x, y,  1,  0, from, g) ;
         recordMultipleV(b, m, c, p, x, y,  1,  1, from, g) ;
         recordMultipleV(b, m, c, p, x, y,  1, -1, from, g) ;

         recordMultipleV(b, m, c, p, x, y,  0,  1, from, g) ;
         recordMultipleV(b, m, c, p, x, y,  0, -1, from, g) ;
         INC(low)
      END
   END
END recordMultiple ;


(*
   recMove - generate the moves for peg, p.
*)

PROCEDURE recMove (VAR b: Board; VAR m: Moves; c: Colour;
                   p: CARDINAL; from: CARDINAL; VAR g: Graph) ;
BEGIN
   m.pieceHead[p-1] := 0 ;
   m.pieceHead[p] := 0 ;
   recordMove(m, from, p) ;
   (* record the current position so we can ignore moving back to it *)
   recordMultiple(b, m, c, p, m.pieceHead[p]-1, g) ;
   recordSingle(b, m, c, p, from, g)
END recMove ;


(*
   recMoves - generate the list of moves for colour, c, on board, b, and record each
              move in r.
              The board, b, is unaltered despite being passed by reference.
*)

PROCEDURE recMoves (VAR b: Board; VAR m: Moves; c: Colour;
                    peg: CARDINAL; from: CARDINAL; VAR g: Graph) ;
VAR
   pos: CARDINAL ;
BEGIN
   pos := b.pieces[c][peg] ;
   subPiece(b, pos, c) ;        (* remove this peg while jumping (so we dont jump over ourself) *)
   initGraph(g) ;
   recMove(b, m, c, peg, from, g) ;
   addPiece(b, pos, c, peg)  (* restore the peg *)
END recMoves ;


(*
   WriteColour - displays the colour, c.
*)

PROCEDURE WriteColour (c: Colour) ;
BEGIN
   CASE c OF

   White:  WriteString('white') |
   Blue :  WriteString('blue') |
   Green:  WriteString('green') |
   Red  :  WriteString('red')

   END
END WriteColour ;


(*
   getFirstPos -
*)

PROCEDURE getFirstPos (s: ARRAY OF CHAR; VAR b: Board; c: Colour) : CARDINAL ;
VAR
   from: CARDINAL ;
   x   : CHAR ;
   y   : CARDINAL ;
   res : ConvResults ;
BEGIN
   IF Length(s)>0
   THEN
      x := CAP(s[0]) ;
      IF x='?'
      THEN
         displayAllMoves(b, c)
      ELSIF (x>='A') AND (x<='P')
      THEN
         from := ORD (x) - ORD ('A') ;
         s[0] := '0' ;
         IF Length(s)>0
         THEN
            StrToCard(s, y, res) ;
            IF (res=strAllRight) AND ((y=0) OR (y>BoardY))
            THEN
               WriteString('Please enter a number between [1-16]') ; WriteLn
            ELSE
               from := from+(y-1)*BoardY ;
               IF isUsed(b, from) AND isColour(b, from, c)
               THEN
                  RETURN from
               ELSE
                  WriteString('That position is occupied by your opponent') ; WriteLn
               END
            END
         END
      ELSE
         WriteString('please enter a letter [A-P] followed by a number [1-16]') ; WriteLn
      END
   END ;
   RETURN BoardSize
END getFirstPos ;


(*
   getSecondPos -
*)

PROCEDURE getSecondPos (s: ARRAY OF CHAR; VAR b: Board; c: Colour; peg: CARDINAL) : CARDINAL ;
VAR
   from: CARDINAL ;
   x   : CHAR ;
   y   : CARDINAL ;
   res : ConvResults ;
BEGIN
   IF Length(s)>0
   THEN
      x := CAP(s[0]) ;
      IF x='?'
      THEN
         displayMovesPeg(b, c, peg) ;
         displayBoardPeg(b, c, peg)
      ELSIF (x>='A') AND (x<='P')
      THEN
         from := ORD (x) - ORD ('A') ;
         s[0] := '0' ;
         IF Length(s)>0
         THEN
            StrToCard(s, y, res) ;
            IF (res=strAllRight) AND ((y=0) OR (y>BoardY))
            THEN
               WriteString('Please enter a number between [1-16]') ; WriteLn
            ELSE
               from := from+(y-1)*BoardY ;
               IF NOT isUsed(b, from)
               THEN
                  RETURN from
               ELSIF isColour(b, from, c)
               THEN
                  WriteString('That position is already occupied by another of your pegs') ; WriteLn
               ELSE
                  WriteString('That position is occupied by your opponent') ; WriteLn
               END
            END
         END
      ELSE
         WriteString('please enter a letter [A-P] followed by a number [1-16]') ; WriteLn
      END
   END ;
   RETURN BoardSize
END getSecondPos ;


(*
   getPeg -
*)

PROCEDURE getPeg (VAR b: Board; c: Colour; from: CARDINAL) : CARDINAL ;
VAR
   p: CARDINAL ;
BEGIN
   FOR p := 1 TO Pieces DO
      IF b.pieces[c][p] = VAL (CARDINAL8, from)
      THEN
         RETURN p
      END
   END ;
   HALT ;
   RETURN Pieces+1
END getPeg ;


(*
   checkLegal -
*)

PROCEDURE checkLegal (VAR b: Board; col: Colour; from, to: CARDINAL; peg: CARDINAL) : BOOLEAN ;
VAR
   m   : Moves ;
   i, j: CARDINAL ;
BEGIN
   IF (to=BoardSize) OR (from=BoardSize)
   THEN
      RETURN FALSE
   END ;
   genMoves(b, m, col) ;
   IF VAL (CARDINAL8, from) # b.pieces[col][peg]
   THEN
      RETURN FALSE
   END ;
   i := m.pieceHead[peg-1]+1 ;  (* skip the initial move *)
   j := m.pieceHead[peg] ;
   WHILE i<j DO
      IF VAL (CARDINAL8, to) = m.pieceList[i]
      THEN
         RETURN TRUE
      END ;
      INC(i)
   END ;
   RETURN FALSE
END checkLegal ;


(*
   noOfMoves - returns the number of moves held in, m.
*)

PROCEDURE noOfMoves (VAR m: Moves) : CARDINAL ;
VAR
   n, p, i, j: CARDINAL ;
BEGIN
   n := 0 ;
   FOR p := 1 TO Pieces DO
      i := m.pieceHead[p-1]+1 ;  (* skip the initial move *)
      j := m.pieceHead[p] ;
      WHILE i<j DO
         INC(n) ;
         INC(i)
      END
   END ;
   RETURN n
END noOfMoves ;


(*
   askMove - returns a move entered.
*)

PROCEDURE askMove (VAR b: Board; c: Colour; VAR peg: CARDINAL) : CARDINAL ;
VAR
   s       : ARRAY [0..80] OF CHAR ;
   y,
   from, to: CARDINAL ;
   res     : ConvResults ;
BEGIN
   LOOP
      WriteString('Please enter your move, from, ') ;
      WriteColour(c) ;
      WriteString(' ') ;
      ReadToken(s) ;
      SkipLine ;
      from := getFirstPos(s, b, c) ;
      IF from=BoardSize
      THEN
         WriteString('please try again...') ; WriteLn
      ELSE
         WriteString('now please enter your move, to, ') ;
         WriteColour(c) ;
         WriteString(' ') ;
         ReadToken(s) ;
         SkipLine ;
         peg := getPeg(b, c, from) ;
         to := getSecondPos(s, b, c, peg) ;
         IF checkLegal(b, c, from, to, peg)
         THEN
            WriteString('you are ') ;
            showMove(b, c, peg, from, to) ;
            RETURN to
         END
      END
   END
END askMove ;


(*
   opponent - returns the opponents colour.
*)

PROCEDURE opponent (col: Colour) : Colour ;
BEGIN
   IF col=Red
   THEN
      RETURN Blue
   ELSE
      RETURN Red
   END
END opponent ;


(*
   maximumScore - returns TRUE if the maximim score was found.
*)

PROCEDURE maximumScore (score: INTEGER) : BOOLEAN ;
BEGIN
   RETURN (score<=MinScore) OR (score>=MaxScore)
END maximumScore ;


(*
   calcScoreForPos - returns the score for Colour, c, pos, on Board, b.
*)

PROCEDURE calcScoreForPos (VAR b: Board; c: Colour; pos: CARDINAL) : INTEGER ;
VAR
   home,
   x, y: CARDINAL ;
BEGIN
   IF c=Red
   THEN
      pos := (BoardSize-1) - pos
   ELSIF c=Blue
   THEN
      (* nothing to do *)
   ELSE
      HALT  (* not implemented yet *)
   END ;
   IF pos IN homeBase[c]
   THEN
      home := HomeWeight
   ELSE
      home := 0
   END ;

   (* our score is dependant upon how far this piece is away from the opposite corner *)
   x := pos MOD BoardX ;
   y := pos DIV BoardY ;
   IF x>y
   THEN
      (* max squares from 0,0 *)
      RETURN BoardX-x+home
   ELSE
      RETURN BoardY-y+home
   END
END calcScoreForPos ;


(*
   calcScoreFor - returns the score for Colour, c.
*)

PROCEDURE calcScoreFor (VAR b: Board; c: Colour) : INTEGER ;
VAR
   score: INTEGER ;
   p    : CARDINAL ;
BEGIN
   score := 0 ;
   FOR p := 1 TO Pieces DO
      INC(score, calcScoreForPos(b, c, b.pieces[c][p]))
   END ;
   RETURN score
END calcScoreFor ;


(*
   updateMove -
*)

PROCEDURE updateMove (VAR b: Board; col: Colour; peg: CARDINAL; topos: CARDINAL) ;
VAR
   frompos: CARDINAL ;
BEGIN
   frompos := b.pieces[col][peg] ;
   subPiece(b, frompos, col) ;
   addPiece(b, topos, col, peg)
END updateMove ;


(*
   retractMove -
*)

PROCEDURE retractMove (VAR b: Board; col: Colour; peg: CARDINAL; topos: CARDINAL) ;
BEGIN
   updateMove(b, col, peg, topos)
END retractMove ;


(*
   calcScore - make the move and update the score.
*)

PROCEDURE calcScore (VAR b: Board; score: INTEGER; peg: CARDINAL;
                     topos: CARDINAL; col: Colour) : INTEGER ;
VAR
   i, j, k: INTEGER ;
BEGIN
   IF slowEvaluation
   THEN
      (* compute the score by examine each peg in turn *)
      updateMove(b, col, peg, topos) ;

      (* check whether one side has won *)
      IF b.home[Blue]=Pieces
      THEN
         RETURN MaxScore
      ELSIF b.home[Red]=Pieces
      THEN
         RETURN MinScore
      END ;

      RETURN calcScoreFor(b, Blue) - calcScoreFor(b, Red)
   ELSE
      i := calcScoreForPos(b, col, b.pieces[col][peg]) ;
      updateMove(b, col, peg, topos) ;  (* move the peg *)

      (* check whether one side has won *)
      IF b.home[Blue]=Pieces
      THEN
         RETURN MaxScore
      ELSIF b.home[Red]=Pieces
      THEN
         RETURN MinScore
      END ;

      j := calcScoreForPos(b, col, topos) ;
      IF col=Red
      THEN
         score := score + i - j
      ELSE
         score := score - i + j
      END ;
      IF Debugging
      THEN
         k := calcScoreFor(b, Blue) - calcScoreFor(b, Red) ;
         IF score#k
         THEN
            HALT
         END
      END ;
      RETURN score
   END
END calcScore ;


(*
   alphaBeta - returns the score estimated should move, pos, be chosen.
               The board, b, and score is in the state _before_ move pos
               is made.
*)

PROCEDURE alphaBeta (peg: CARDINAL; frompos, topos: CARDINAL;
                     VAR b: Board; col: Colour;
                     depth: CARDINAL;
                     alpha, beta, score: INTEGER) : INTEGER ;
VAR
   try     : INTEGER ;
   i, j,
   n, p    : CARDINAL ;
   m       : Moves ;
   from, to: CARDINAL ;
   op      : Colour ;
BEGIN
   score := calcScore(b, score, peg, topos, col) ;   (* make move and update score *)
   IF (depth=0) OR maximumScore(score)
   THEN
      retractMove(b, col, peg, frompos) ;
      INC(count) ;
      IF col=Red
      THEN
         RETURN score+VAL(INTEGER, depth)
      ELSE
         RETURN score-VAL(INTEGER, depth)
      END
   ELSE
      op := opponent(col) ;
      genMoves(b, m, op) ;
      IF op=Blue
      THEN
         (* blue to move, move is possible, continue searching *)
         FOR p := 1 TO Pieces DO
            from := b.pieces[op][p] ;
            i := m.pieceHead[p-1]+1 ;  (* skip the initial move *)
            j := m.pieceHead[p] ;
            WHILE i<j DO
               to := m.pieceList[i] ;
               try := alphaBeta(p, from, to,
                                b, op, depth-1, alpha, beta, score) ;
               IF try > alpha
               THEN
                  (* found a better move *)
                  alpha := try
               END ;
               IF alpha >= beta
               THEN
                  retractMove(b, col, peg, frompos) ;
                  RETURN alpha
               END ;
               INC(i)
            END
         END ;
         retractMove(b, col, peg, frompos) ;
         RETURN alpha
      ELSE
         (* red to move, move is possible, continue searching *)
         FOR p := 1 TO Pieces DO
            from := b.pieces[op][p] ;
            i := m.pieceHead[p-1]+1 ;  (* skip the initial move *)
            j := m.pieceHead[p] ;
            WHILE i<j DO
               to := m.pieceList[i] ;
               try := alphaBeta(p, from, to,
                                b, op, depth-1, alpha, beta, score) ;
               IF try < beta
               THEN
                  (* found a better move *)
                  beta := try
               END ;
               IF alpha >= beta
               THEN
                  (* no point searching further as Red would choose
                   a different previous move *)
                  retractMove(b, col, peg, frompos) ;
                  RETURN beta
               END ;
               INC(i)
            END
         END ;
         retractMove(b, col, peg, frompos) ;
         RETURN beta   (* the best score for a move Blue has found *)
      END
   END
END alphaBeta ;


(*
   makeMove - computer makes a move for colour, col.
*)

PROCEDURE makeMove (VAR b: Board; col: Colour; score: INTEGER; VAR peg: CARDINAL) : INTEGER ;
VAR
   no        : CARDINAL ;
   p, from,
   frompos,
   topos, to : CARDINAL ;
   start, end: Timeval ;
   try,
   r, best   : INTEGER ;
   secS, usec,
   secE, i, j: CARDINAL ;
   m         : Moves ;
   plies     : CARDINAL ;
   outOfTime : BOOLEAN ;
BEGIN
   start := InitTime(0, 0) ;
   end := InitTime(0, 0) ;

   r := GetTimeOfDay(start) ;
   best := MinScore-1 ;   (* worst than minimum score so we will choose a loosing move if forced *)

   count := 0 ;
   i := 0 ;
   genMoves(b, m, col) ;
   no := noOfMoves(m) ;
   peg := Pieces+1 ;
   outOfTime := FALSE ;
   plies := 0 ;
   frompos := BoardSize ;
   topos := BoardSize ;
   REPEAT
      WriteString("I'm going to look ") ;
      WriteCard(plies, 0) ; WriteString(' moves ahead') ; WriteLn ;

      FOR p := 1 TO Pieces DO
         from := b.pieces[col][p] ;
         i := m.pieceHead[p-1]+1 ;  (* skip the initial move *)
         j := m.pieceHead[p] ;
         IF (no=1) AND (i<j)
         THEN
            (* only one move and this peg can move, therefore dont bother evaluating the move, just play it *)
            to := m.pieceList[i] ;
            frompos := from ;
            best := 0 ;
            topos := to ;
            peg := p
         ELSE
            WHILE (i<j) AND (NOT outOfTime) DO
               r := GetTimeOfDay(end) ;
               GetTime(start, secS, usec) ;
               GetTime(end, secE, usec) ;
               outOfTime := (secE-secS > Thinking) ;

               IF outOfTime
               THEN
                  WriteString('out of time...') ; WriteLn
               ELSE
                  to := m.pieceList[i] ;
                  try := alphaBeta(p, from, to,
                                   b, col, plies,
                                   MinScore, MaxScore, score) ;
                  IF try>best
                  THEN
                     best := try ;
                     topos := to ;
                     frompos := from ;
                     peg := p
                  END
               END ;
               INC(i)
            END
         END
      END ;
      IF (NOT outOfTime) AND (frompos<BoardSize) AND (topos<BoardSize)
      THEN
         WriteString('so far I think the best move is from') ;
         writePosition(frompos) ;
         WriteString(' to') ;
         writePosition(topos) ;
         WriteLn
      END ;
      INC(plies)
   UNTIL (no<2) OR outOfTime ;

   IF best >= WinScore
   THEN
      WriteString('I think I can force a win') ; WriteLn
   END ;
   IF best <= LooseScore
   THEN
      WriteString('You should be able to force a win') ; WriteLn
   END ;

   IF no=1
   THEN
      WriteString('I can only play one move, so there is little point wasting time') ; WriteLn
   ELSIF no=0
   THEN
      WriteString('I cannot move, so there is little point wasting time') ; WriteLn
   ELSE
      WriteString('I took ') ; WriteCard(secE-secS, 0) ;
      WriteString(' seconds and evaluated ') ;
      WriteCard(count, 0) ; WriteString(' positions,') ; WriteLn ;
   END ;

   start := KillTime(start) ;
   end := KillTime(end) ;
   RETURN topos
END makeMove ;


(*
   test -
*)

PROCEDURE test ;
VAR
   b  : Board ;
   c  : Colour ;
   s  : INTEGER ;
   peg,
   to : CARDINAL ;
BEGIN
   initBoard(b) ;
   c := Red ;
   s := 0 ;
   displayBoard(b) ;
   peg := getPeg(b, c, 4) ;
   displayBoardPeg(b, c, peg) ;
   to := 36 ;
   s := calcScore(b, s, peg, to, c) ;



   peg := 5 ;
   c := opponent(c) ;
   peg := getPeg(b, c, 12*BoardX+15) ;
   to := 12*BoardX+13 ;
   s := calcScore(b, s, peg, to, c) ;
   displayBoardPeg(b, c, peg) ;



   c := Red ;
   displayBoard(b) ;
   peg := getPeg(b, c, 36) ;
   stop ;
   displayBoardPeg(b, c, peg) ;
   to := 4 ;
   s := calcScore(b, s, peg, to, c) ;
   displayBoardPeg(b, c, peg) ;

END test ;


(*
   displayHow -
*)

PROCEDURE displayHow (from, to: CARDINAL; VAR rec: ARRAY OF CARDINAL; r: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   writePosition(from) ; WriteString(' can move to ') ; writePosition(to) ; WriteString(' by: ') ;
   i := 0 ;
   WHILE (i<r) AND (i<=HIGH(rec)) DO
      writePosition(rec[i])
   END ;
   WriteLn
END displayHow ;


(*
   addToList -
*)

PROCEDURE addToList (VAR choices: ARRAY OF CARDINAL; VAR n: CARDINAL; from: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i<n DO
      IF choices[i]=from
      THEN
         RETURN
      END ;
      INC(i)
   END ;
   choices[n] := from ;
   INC(n)
END addToList ;


(*
   subBest -
*)

PROCEDURE subBest (VAR choices: ARRAY OF CARDINAL; VAR n: CARDINAL; VAR g: Graph) : CARDINAL ;
VAR
   b: CARDINAL ;
   i, j, k: CARDINAL ;
BEGIN
   k := choices[0] ;
   b := g.graph[k].dist ;
   i := 1 ;
   WHILE i<n DO
      IF g.graph[choices[i]].dist<b
      THEN
         k := choices[i] ;
         b := g.graph[k].dist
      END ;
      INC(i)
   END ;
   (* remove, k, from, choices *)
   i := 0 ;
   j := 0 ;
   WHILE i<n DO
      IF i#j
      THEN
         choices[i] := choices[j] ;
         INC(i) ;
      ELSIF choices[i]#k
      THEN
         INC(i)
      END ;
      INC(j)
   END ;
   DEC(n) ;
   RETURN k
END subBest ;


(*
   dijkstra -
*)

PROCEDURE dijkstra (from, to: CARDINAL; VAR g: Graph) ;
VAR
   visited: SoS ;
   choices: ARRAY Squares OF CARDINAL ;
   alt,
   n, i   : CARDINAL ;
   u, v   : CARDINAL ;
BEGIN
   g.graph[from].dist := 0 ;
   g.graph[from].prev := from ;
   visited := SoS{from} ;
   n := 0 ;
   addToList(choices, n, from) ;
   WHILE n#0 DO
      u := subBest(choices, n, g) ;
      IF u=to
      THEN
         RETURN
      ELSE
         WITH g.graph[u] DO
            i := 0 ;
            WHILE i<no DO
               v := list[i] ;
               IF NOT (v IN visited)
               THEN
                  INCL(visited, v) ;
                  addToList(choices, n, v) ;
                  alt := dist + 1 ;
                  IF alt<g.graph[v].dist
                  THEN
                     g.graph[v].dist := alt ;
                     g.graph[v].prev := u
                  END
               END ;
               INC(i)
            END
         END
      END
   END
END dijkstra ;


(*
   showRoute -
*)

PROCEDURE showRoute (from, to: CARDINAL; VAR g: Graph) ;
BEGIN
   IF from#to
   THEN
      showRoute(from, g.graph[to].prev, g)
   END ;
   IF from=to
   THEN
      WriteString(' from')
   ELSE
      WriteString(' to')
   END ;
   writePosition(to)
END showRoute ;


(*
   showMove - show how, peg, can move, from, to, on board, b.
*)

PROCEDURE showMove (VAR b: Board;
                    c: Colour; peg: CARDINAL; from, to: CARDINAL) ;
VAR
   m: Moves ;
   g: Graph ;
BEGIN
   recMoves(b, m, c, peg, from, g) ;
   dijkstra(from, to, g) ;
   WriteString('moving peg') ;
   showRoute(from, to, g) ;
   WriteLn
END showMove ;


(*
   play -
*)

PROCEDURE play ;
VAR
   b       : Board ;
   c       : Colour ;
   s       : INTEGER ;
   peg,
   to, from: CARDINAL ;
BEGIN
   initBoard(b) ;
   c := Red ;
   s := 0 ;
   displayBoard(b) ;
   RETURN ;   (* remove this line of code if you really want to play the game.  *)
   LOOP
      to := askMove(b, c, peg) ;
      s := calcScore(b, s, peg, to, c) ;
      displayBoard(b) ;
      WriteString('Current score = ') ; WriteInt(s, 0) ; WriteLn ;
      IF s<=MinScore
      THEN
         WriteString('Well done you win') ; WriteLn ;
         RETURN
      END ;
      c := opponent(c) ;
      to := makeMove(b, c, s, peg) ;
      IF peg>Pieces
      THEN
         WriteString('I cannot move') ; WriteLn
      ELSE
         from := b.pieces[c][peg] ;
         WriteString('I am ') ;
         showMove(b, c, peg, from, to) ;
         s := calcScore(b, s, peg, to, c) ;
         displayBoard(b) ;
         WriteString('Current score = ') ; WriteInt(s, 0) ; WriteLn ;
         IF s>=MaxScore
         THEN
            WriteString('Good try, but I win') ; WriteLn ;
            RETURN
         END
      END ;
      c := opponent(c)
   END
END play ;


(*
   writePosition -
*)

PROCEDURE writePosition (x: CARDINAL) ;
BEGIN
   WriteChar(' ') ;
   WriteChar(CHR(ORD('a')+x MOD BoardX)) ;
   WriteCard(x DIV BoardX+1, 0)
END writePosition ;


(*
   displayMovesForPeg -
*)

PROCEDURE displayMovesForPeg (VAR b: Board; m: Moves; c: Colour; peg: CARDINAL) ;
VAR
   p, i, j: CARDINAL ;
BEGIN
   WriteString('peg at') ;
   writePosition(b.pieces[c][peg]) ;
   IF m.pieceHead[peg-1]+1<m.pieceHead[peg]
   THEN
      WriteString(' can move to ') ;
      i := m.pieceHead[peg-1]+1 ;  (* skip the initial move *)
      j := m.pieceHead[peg] ;
      WHILE i<j DO
         writePosition(m.pieceList[i]) ;
         WriteString(' ') ;
         INC(i)
      END ;
      WriteLn
   ELSE
      WriteString(' cannot move') ; WriteLn
   END
END displayMovesForPeg ;


(*
   displayMoves -
*)

PROCEDURE displayMoves (VAR b: Board; m: Moves; c: Colour) ;
VAR
   p, i, j: CARDINAL ;
BEGIN
   WriteString('possible moves are ') ; WriteLn ;
   FOR p := 1 TO Pieces DO
      IF m.pieceHead[p-1]+1<m.pieceHead[p]
      THEN
         WriteString('piece at position ') ;
         writePosition(b.pieces[c][p]) ;
         WriteString(' can move to ') ;
         i := m.pieceHead[p-1]+1 ;  (* skip the initial move *)
         j := m.pieceHead[p] ;
         WHILE i<j DO
            writePosition(m.pieceList[i]) ;
            WriteString(' ') ;
            INC(i)
         END ;
         WriteLn
      END
   END
END displayMoves ;


(*
   displayAllMoves -
*)

PROCEDURE displayAllMoves (VAR b: Board; c: Colour) ;
VAR
   m: Moves ;
BEGIN
   genMoves(b, m, c) ;
   displayMoves(b, m, c)
END displayAllMoves ;


(*
   displayMovesPeg -
*)

PROCEDURE displayMovesPeg (VAR b: Board; c: Colour; peg: CARDINAL) ;
VAR
   m: Moves ;
BEGIN
   genMoves(b, m, c) ;
   displayMovesForPeg(b, m, c, peg)
END displayMovesPeg ;


(*
   initBoard -
*)

PROCEDURE initBoard (VAR b: Board) ;
BEGIN
   b.used := SoS {} ;
   b.colour[0] := SoS {} ;
   b.colour[1] := SoS {} ;
   b.home[Blue] := 0 ;
   b.home[Red] := 0 ;
   b.home[Green] := 0 ;
   b.home[White] := 0 ;
   IF TwoPlayer OR FourPlayer
   THEN
      homeBase[Blue] := SoS{0, 1, 2, 3,
                            16, 17, 18, 19,
                            32, 33, 34,
                            48, 49} ;
      IF Debugging
      THEN
         dumpBase(Blue) ;
         dumpBase(Red)
      END ;

      homeBase[Red] := SoS{255-0, 255-1, 255-2, 255-3,
                           255-16, 255-17, 255-18, 255-19,
                           255-32, 255-33, 255-34,
                           255-48, 255-49} ;
      IF Debugging
      THEN
         dumpBase(Red) ;
         dumpBase(Blue)
      END ;

      (* red *)
      addPiece(b,  0, Red, 1) ;
      addPiece(b,  1, Red, 2) ;
      addPiece(b,  2, Red, 3) ;
      addPiece(b,  3, Red, 4) ;
      addPiece(b, 16, Red, 5) ;
      addPiece(b, 17, Red, 6) ;
      addPiece(b, 18, Red, 7) ;
      addPiece(b, 19, Red, 8) ;
      addPiece(b, 32, Red, 9) ;
      addPiece(b, 33, Red, 10) ;
      addPiece(b, 34, Red, 11) ;
      addPiece(b, 48, Red, 12) ;
      addPiece(b, 49, Red, 13) ;

      (* blue *)
      addPiece(b, 255-0, Blue, 1) ;
      addPiece(b, 255-1, Blue, 2) ;
      addPiece(b, 255-2, Blue, 3) ;
      addPiece(b, 255-3, Blue, 4) ;
      addPiece(b, 255-16, Blue, 5) ;
      addPiece(b, 255-17, Blue, 6) ;
      addPiece(b, 255-18, Blue, 7) ;
      addPiece(b, 255-19, Blue, 8) ;
      addPiece(b, 255-32, Blue, 9) ;
      addPiece(b, 255-33, Blue, 10) ;
      addPiece(b, 255-34, Blue, 11) ;
      addPiece(b, 255-48, Blue, 12) ;
      addPiece(b, 255-49, Blue, 13) ;

   END ;
   IF TwoPlayer
   THEN
      homeBase[Blue] := homeBase[Blue] + SoS{4, 20, 35, 50, 65, 64} ;
      IF Debugging
      THEN
         dumpBase(Blue)
      END ;
      homeBase[Red] := homeBase[Red] + SoS{255-4, 255-20, 255-35, 255-50, 255-65, 255-64} ;
      IF Debugging
      THEN
         dumpBase(Red)
      END ;
(*
      INCL(homeBase[Blue], 4) ;
      INCL(homeBase[Blue], 20) ;
      INCL(homeBase[Blue], 35) ;
      INCL(homeBase[Blue], 50) ;
      INCL(homeBase[Blue], 65) ;
      INCL(homeBase[Blue], 64) ;
*)
      IF Debugging
      THEN
         dumpBase(Blue)
      END ;

(*
      INCL(homeBase[Red], 255-4) ;
      INCL(homeBase[Red], 255-20) ;
      INCL(homeBase[Red], 255-35) ;
      INCL(homeBase[Red], 255-50) ;
      INCL(homeBase[Red], 255-65) ;
      INCL(homeBase[Red], 255-64) ;
*)

      IF Debugging
      THEN
         dumpBase(Red)
      END ;

      (* red *)
      addPiece(b,  4, Red, 14) ;
      addPiece(b, 20, Red, 15) ;
      addPiece(b, 35, Red, 16) ;
      addPiece(b, 50, Red, 17) ;
      addPiece(b, 65, Red, 18) ;
      addPiece(b, 64, Red, 19) ;

      (* blue *)
      addPiece(b, 255-4, Blue, 14) ;
      addPiece(b, 255-20, Blue, 15) ;
      addPiece(b, 255-35, Blue, 16) ;
      addPiece(b, 255-50, Blue, 17) ;
      addPiece(b, 255-65, Blue, 18) ;
      addPiece(b, 255-64, Blue, 19) ;

   END ;
   assert(b.home[Blue] = 0) ;
   assert(b.home[Red] = 0) ;
   assert(b.home[Green] = 0) ;
   assert(b.home[White] = 0)
END initBoard ;


(*
   displayBoard - displays the board.
*)

PROCEDURE displayBoard (b: Board) ;
VAR
   i, j: CARDINAL ;
BEGIN
   WriteString('     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p') ; WriteLn ;
   WriteString('   +------------------------------------------------+') ; WriteLn ;
   FOR j := BoardY TO 1 BY -1 DO
      WriteCard(j, 2) ;
      WriteString(' |') ;
      FOR i := 1 TO BoardX DO
         WriteChar(' ') ;
         IF isColour(b, (j-1)*BoardX+(i-1), Blue)
         THEN
            WriteChar('b')
         ELSIF isColour(b, (j-1)*BoardX+(i-1), Red)
         THEN
            WriteChar('r')
         ELSIF isColour(b, (j-1)*BoardX+(i-1), Green)
         THEN
            WriteChar('g')
         ELSIF isColour(b, (j-1)*BoardX+(i-1), White)
         THEN
            WriteChar('w')
         ELSE
            WriteChar(' ')
         END ;
         WriteChar(' ')
      END ;
      WriteString('| ') ;
      WriteCard(j, 2) ;
      WriteLn
   END ;
   WriteString('   +------------------------------------------------+') ; WriteLn ;
   WriteString('     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p') ; WriteLn
END displayBoard ;


(*
   emitSpecialIf -
*)

PROCEDURE emitSpecialIf (normal, special: CHAR; i, j, x, y: CARDINAL) ;
BEGIN
   IF (x=i) AND (y=j)
   THEN
      WriteChar(special)
   ELSE
      WriteChar(normal)
   END
END emitSpecialIf ;


(*
   displayBoardPeg - displays the board with all moves by peg illustrated.
*)

PROCEDURE displayBoardPeg (b: Board; c: Colour; peg: CARDINAL) ;
VAR
   x, y,
   i, j: CARDINAL ;
   m   : Moves ;
BEGIN
   genMoves(b, m, c) ;
   x := b.pieces[c][peg] MOD BoardX+1 ;
   y := b.pieces[c][peg] DIV BoardX+1 ;
   WriteString('     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p') ; WriteLn ;
   WriteString('   +------------------------------------------------+') ; WriteLn ;
   FOR j := BoardY TO 1 BY -1 DO
      WriteCard(j, 2) ;
      WriteString(' |') ;
      FOR i := 1 TO BoardX DO
         WriteChar(' ') ;
         IF isColour(b, (j-1)*BoardX+(i-1), Blue)
         THEN
            emitSpecialIf('b', 'x', i, j, x, y)
         ELSIF isColour(b, (j-1)*BoardX+(i-1), Red)
         THEN
            emitSpecialIf('r', 'x', i, j, x, y)
         ELSIF isColour(b, (j-1)*BoardX+(i-1), Green)
         THEN
            emitSpecialIf('g', 'x', i, j, x, y)
         ELSIF isColour(b, (j-1)*BoardX+(i-1), White)
         THEN
            emitSpecialIf('w', 'x', i, j, x, y)
         ELSE
            IF isRecorded(m, ((j-1)*BoardX)+(i-1), peg)
            THEN
               CASE c OF

               Blue :  WriteChar('B') |
               Red  :  WriteChar('R') |
               Green:  WriteChar('G') |
               White:  WriteChar('W')

               END
            ELSE
               WriteChar(' ')
            END
         END ;
         WriteChar(' ')
      END ;
      WriteString('| ') ;
      WriteCard(j, 2) ;
      WriteLn
   END ;
   WriteString('   +------------------------------------------------+') ; WriteLn ;
   WriteString('     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p') ; WriteLn
END displayBoardPeg ;


BEGIN
   (* test *)
   play
END halma.
(*
 * Local variables:
 *  compile-command: "gm2 -g -fiso halma.mod"
 * End:
 *)
