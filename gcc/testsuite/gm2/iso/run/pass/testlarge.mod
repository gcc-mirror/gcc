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

MODULE testlarge ;

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


VAR
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
   addPiece - adds a piece, pos, of colour, c, to the board, b.
*)

PROCEDURE addPiece (VAR b: Board; pos: CARDINAL; c: Colour; piece: CARDINAL) ;
BEGIN
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
         stop ;
         INC(home[c])
      END
   END
END addPiece ;


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

      homeBase[Blue] := SoS{0, 1, 2, 3,
                            16, 17, 18, 19,
                            32, 33, 34,
                            48, 49} ;

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

      homeBase[Red] := SoS{255-0, 255-1, 255-2, 255-3,
                           255-16, 255-17, 255-18, 255-19,
                           255-32, 255-33, 255-34,
                           255-48, 255-49}
   END ;
   IF TwoPlayer
   THEN
      (* red *)
      addPiece(b,  4, Red, 14) ;
      addPiece(b, 20, Red, 15) ;
      addPiece(b, 35, Red, 16) ;
      addPiece(b, 50, Red, 17) ;
      addPiece(b, 65, Red, 18) ;
      addPiece(b, 64, Red, 19) ;
      homeBase[Blue] := homeBase[Blue] + SoS{4, 20, 35, 50, 65, 64} ;

      (* blue *)
      addPiece(b, 255-4, Blue, 14) ;
      addPiece(b, 255-20, Blue, 15) ;
      addPiece(b, 255-35, Blue, 16) ;
      addPiece(b, 255-50, Blue, 17) ;
      addPiece(b, 255-65, Blue, 18) ;
      addPiece(b, 255-64, Blue, 19) ;

      homeBase[Red] := homeBase[Red] + SoS{255-4, 255-20, 255-35, 255-50, 255-65, 255-64}
   END ;
   assert(b.home[Blue] = 0) ;
   assert(b.home[Red] = 0) ;
   assert(b.home[Green] = 0) ;
   assert(b.home[White] = 0)
END initBoard ;


VAR
   b: Board ;
BEGIN
   initBoard(b)
END testlarge.
(*
 * Local variables:
 *  compile-command: "gm2 -g -fiso testlarge.mod"
 * End:
 *)
