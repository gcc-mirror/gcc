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

MODULE record3 ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM SYSTEM IMPORT TSIZE, SIZE ;

TYPE
   QuadOperator = (BecomesOp, IndrXOp, XIndrOp, BaseOp, ElementSizeOp,
                   AddrOp,
                   SizeOp,
                   OffsetOp,
                   IfEquOp, IfLessEquOp, IfGreEquOp, IfGreOp, IfLessOp,
                   IfNotEquOp, IfInOp, IfNotInOp,
                   CallOp, ParamOp, OptParamOp, ReturnOp, ReturnValueOp, FunctValueOp,
                   NewLocalVarOp, KillLocalVarOp, ProcedureScopeOp,
                   DummyOp,
                   GotoOp, EndOp, StartOp,
                   NegateOp, AddOp, SubOp, DivOp, MultOp, ModOp,
      	       	   LogicalOrOp, LogicalAndOp, LogicalXorOp, LogicalDiffOp,
                   InclOp, ExclOp,
                   UnboundedOp, HighOp,
                   CoerceOp, ConvertOp,
                   StartDefFileOp, StartModFileOp, EndFileOp,
                   CodeOnOp, CodeOffOp,
                   ProfileOnOp, ProfileOffOp,
                   OptimizeOnOp, OptimizeOffOp,
                   InlineOp, LineNumberOp,
                   SubrangeLowOp, SubrangeHighOp,
                   BuiltinConstOp, StandardFunctionOp) ;

   QuadFrame = RECORD
                  Operator           : QuadOperator ;
                  Operand1           : CARDINAL ;
                  Operand2           : CARDINAL ;
                  Operand3           : CARDINAL ;
                  Next               : CARDINAL ;
                  LineNo             : CARDINAL ;
                  TokenNo            : CARDINAL ;
                  NoOfTimesReferenced: CARDINAL ;
               END ;

CONST
   MaxQuad = 50000 ;

VAR
   Quads: ARRAY [1..MaxQuad] OF QuadFrame ;
BEGIN
   WriteString('SIZE(QuadFrame) = ') ; WriteCard(TSIZE(QuadFrame), 6) ;
   WriteLn ;
   WriteString('SIZE(QuadFrame) = ') ; WriteCard(SIZE(Quads), 6) ;
   WriteLn
END record3.
