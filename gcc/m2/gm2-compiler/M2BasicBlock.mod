(* M2BasicBlock.mod converts a scope block into a list of basic blocks.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2BasicBlock ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM M2Debug IMPORT Assert ;
FROM M2Options IMPORT OptimizeBasicBlock ;

FROM M2Quads IMPORT IsReferenced, IsConditional, IsUnConditional, IsCall,
                    IsReturn, IsNewLocalVar, IsKillLocalVar,
                    IsCatchBegin, IsCatchEnd,
                    IsInitStart, IsInitEnd, IsFinallyStart, IsFinallyEnd,
                    IsInitialisingConst,
                    IsPseudoQuad, IsDefOrModFile,
                    GetNextQuad, GetQuad, QuadOperator,
                    SubQuad ;

FROM M2Scope IMPORT ScopeBlock, ForeachScopeBlockDo ;
FROM M2GenGCC IMPORT ConvertQuadsToTree ;


TYPE
   BasicBlock = POINTER TO RECORD
                   StartQuad  : CARDINAL ;  (* First Quad in Basic Block *)
                   EndQuad    : CARDINAL ;  (* End Quad in Basic Block   *)
                   Right      : BasicBlock ;
                                            (* Last Basic Block in list  *)
                   Left       : BasicBlock ;
                END ;

VAR
   FreeList        : BasicBlock ;   (* Free list of Basic Blocks         *)
   HeadOfBasicBlock: BasicBlock ;


(*
   InitBasicBlocks - converts a list of quadruples as defined by
                     scope blocks into a set of basic blocks.
                     All quadruples within this list which are not
                     reachable are removed.
*)

PROCEDURE InitBasicBlocks (sb: ScopeBlock) : BasicBlock ;
BEGIN
   HeadOfBasicBlock := NIL ;
   ForeachScopeBlockDo (sb, ConvertQuads2BasicBlock) ;
   RETURN HeadOfBasicBlock
END InitBasicBlocks ;


(*
   InitBasicBlocksFromRange - converts a list of quadruples as defined by
                              start..end.
                              All quadruples within this list which are not
                              reachable are removed.
*)

PROCEDURE InitBasicBlocksFromRange (start, end: CARDINAL) : BasicBlock ;
BEGIN
   HeadOfBasicBlock := NIL ;
   ConvertQuads2BasicBlock(start, end) ;
   RETURN( HeadOfBasicBlock )
END InitBasicBlocksFromRange ;


(*
   KillBasicBlocks - destroys the list of Basic Blocks.
*)

PROCEDURE KillBasicBlocks (VAR bb: BasicBlock) ;
BEGIN
   FreeBasicBlocks (bb) ;
   bb := NIL
END KillBasicBlocks ;


(*
   FreeBasicBlocks - destroys the list of Basic Blocks.
*)

PROCEDURE FreeBasicBlocks (bb: BasicBlock) ;
VAR
   b, c: BasicBlock ;
BEGIN
   IF bb#NIL
   THEN
      b := bb ;
      REPEAT
         c := bb^.Right ;
         bb^.Right := FreeList ;
         FreeList := bb ;
         bb := c
      UNTIL bb=b
   END
END FreeBasicBlocks ;


(*
   New - returns a basic block.
*)

PROCEDURE New () : BasicBlock ;
VAR
   b: BasicBlock ;
BEGIN
   IF FreeList=NIL
   THEN
      NEW(b)
   ELSE
      b := FreeList ;
      FreeList := FreeList^.Right
   END ;
   Assert(b#NIL) ;
   RETURN( b )
END New ;


(*
   ConvertQuads2BasicBlock - converts a list of quadruples to a list of
                             Basic Blocks.
                             A Basic Block is defined as a list of quadruples
                             which has only has one entry and exit point.
*)

PROCEDURE ConvertQuads2BasicBlock (Start, End: CARDINAL) ;
VAR
   LastQuadDefMod,
   LastQuadConditional,
   LastQuadCall,
   LastQuadReturn     : BOOLEAN ;
   Quad               : CARDINAL ;
   CurrentBB          : BasicBlock ;
   LastBB             : BasicBlock ;
BEGIN
   (*
      Algorithm to perform Basic Block:

      For every quadruple establish a set of leaders.
      A Leader is defined as a quadruple which is
      either:

      (i)   The first quadruple.
      (ii)  Any quadruple which is the target of a jump or unconditional jump.
      (iii) Any statement which follows a conditional jump

      For each leader construct a basic block.
      A Basic Block starts with a leader quadruple and ends with either:

      (i)  Another Leader
      (ii) An unconditional Jump.

      Any quadruples that do not fall into a Basic Block can be thrown away
      since they will never be executed.
   *)
   LastBB := NIL ;
   Quad := Start ;
   LastQuadConditional := TRUE ;  (* Force Rule (i) *)
   LastQuadCall := FALSE ;
   LastQuadReturn := FALSE ;
   LastQuadDefMod := FALSE ;
   (* Scan all quadruples *)
   WHILE (Quad<=End) AND (Quad#0) DO
      IF LastQuadConditional OR LastQuadCall OR LastQuadReturn OR
         LastQuadDefMod OR IsReferenced(Quad)
      THEN
         (* Rule (ii) *)
         CurrentBB := New() ;                      (* Get a new Basic Block *)
                                  (* At least one quad in this Basic Block  *)
         StartBB(CurrentBB, Quad) ;
         EndBB(CurrentBB, Quad)
      ELSIF CurrentBB#NIL
      THEN
         (* We have a Basic Block - therefore add quad to this Block  *)
         EndBB(CurrentBB, Quad)
      ELSIF IsPseudoQuad(Quad)
      THEN
         (* Add Quad to the Last BB since Pseudo Quads - compiler directives *)
         (* must not be thrown away.                                         *)
         EndBB(LastBB, Quad)
      ELSIF IsReturn(Quad) OR IsKillLocalVar(Quad) OR
            IsCatchEnd(Quad) OR IsCatchBegin(Quad) OR
            IsInitStart(Quad) OR IsInitEnd(Quad) OR
            IsFinallyStart(Quad) OR IsFinallyEnd(Quad)
      THEN
         (* we must leave these quads alone *)
         EndBB(LastBB, Quad)
      ELSIF IsInitialisingConst(Quad)
      THEN
         (* we must leave these quads alone *)
         EndBB(LastBB, Quad)
      ELSE
         (* remove this Quad since it will never be reached *)
         SubQuad(Quad)
      END ;
      LastQuadConditional := IsConditional(Quad) ;
      LastQuadCall := IsCall(Quad) ;
      LastQuadReturn := IsReturn(Quad) ;
      LastQuadDefMod := IsDefOrModFile(Quad) ;
      IF IsUnConditional(Quad)
      THEN
         LastBB := CurrentBB ;
         CurrentBB := NIL
      END ;
      Quad := GetNextQuad(Quad)
   END
END ConvertQuads2BasicBlock ;


(*
   ForeachBasicBlockDo - for each basic block call procedure, p.
*)

PROCEDURE ForeachBasicBlockDo (bb: BasicBlock; p: BasicBlockProc) ;
VAR
   b: BasicBlock ;
BEGIN
   IF bb#NIL
   THEN
      b := bb ;
      REPEAT
         WITH b^ DO
            p(StartQuad, EndQuad)
         END ;
         b := b^.Right
      UNTIL b=bb
   END
END ForeachBasicBlockDo ;


(*
   StartBB - Initially fills a Basic Block, b, with a start quad Quad.
             The Basic Block is then added to the end of Basic Block list.
*)

PROCEDURE StartBB (b: BasicBlock; Quad: CARDINAL) ;
BEGIN
   WITH b^ DO
      StartQuad := Quad ;
      EndQuad := Quad
   END ;
   Add(HeadOfBasicBlock, b)   (* Add b to the end of the Basic Block list *)
END StartBB ;


(*
   EndBB - Fills a Basic Block, b, with an end quad Quad.
*)

PROCEDURE EndBB (b: BasicBlock; Quad: CARDINAL) ;
BEGIN
   b^.EndQuad := Quad
END EndBB ;


(*
   Add adds a specified element to the end of a queue.
*)

PROCEDURE Add (VAR Head: BasicBlock;
               b : BasicBlock) ;
BEGIN
   IF Head=NIL
   THEN
      Head := b ;
      b^.Left := b ;
      b^.Right := b
   ELSE
      b^.Right := Head ;
      b^.Left := Head^.Left ;
      Head^.Left^.Right := b ;
      Head^.Left := b
   END
END Add ;


(*
   Sub deletes an element from the specified queue.
*)

PROCEDURE Sub (VAR Head: BasicBlock;
               b: BasicBlock) ;
BEGIN
   IF (b^.Right=Head) AND (b=Head)
   THEN
      Head := NIL
   ELSE
      IF Head=b
      THEN
         Head := Head^.Right
      END ;
      b^.Left^.Right := b^.Right ;
      b^.Right^.Left := b^.Left
   END
END Sub ;


(*
   DisplayBasicBlocks - displays the basic block data structure.
*)

PROCEDURE DisplayBasicBlocks (bb: BasicBlock) ;
VAR
   b: BasicBlock ;
BEGIN
   b := bb ;
   WriteString('quadruples') ; WriteLn ;
   IF b#NIL
   THEN
      REPEAT
         DisplayBlock(b) ;
         b := b^.Right
      UNTIL b=bb
   END
END DisplayBasicBlocks ;


PROCEDURE DisplayBlock (b: BasicBlock) ;
BEGIN
   WITH b^ DO
      WriteString(' start ') ; WriteCard(StartQuad, 6) ;
      WriteString(' end   ') ; WriteCard(EndQuad, 6) ;
   END
END DisplayBlock ;


BEGIN
   FreeList := NIL
END M2BasicBlock.
