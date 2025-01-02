(* M2Size.mod exports the standard function SIZE.

Copyright (C) 2003-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Size ;

FROM NameKey IMPORT MakeKey ;
FROM M2Base IMPORT ZType ;
FROM M2LexBuf IMPORT BuiltinTokenNo ;

FROM SymbolTable IMPORT NulSym, MakeProcedure, PutFunction,
                        AddSymToModuleScope, GetCurrentScope,
                        ProcedureKind ;


(*
   MakeSize - creates and declares the standard function SIZE.
*)

PROCEDURE MakeSize ;
BEGIN
   IF Size=NulSym
   THEN
                                                     (* Function        *)
      Size := MakeProcedure (BuiltinTokenNo, MakeKey ('SIZE')) ;
      PutFunction (BuiltinTokenNo, Size, DefProcedure, ZType)
                                                     (* Return Type     *)
                                                     (* ZType           *)
   ELSE
      AddSymToModuleScope (GetCurrentScope (), Size)
   END
END MakeSize ;


BEGIN
   Size := NulSym
END M2Size.
