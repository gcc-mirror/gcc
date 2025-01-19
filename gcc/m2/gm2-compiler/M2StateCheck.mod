(* M2StateCheck.mod provide state check tracking for declarations.

Copyright (C) 2024-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

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

IMPLEMENTATION MODULE M2StateCheck ;

FROM Storage IMPORT ALLOCATE ;
FROM M2MetaError IMPORT MetaErrorStringT1 ;
FROM DynamicStrings IMPORT String, InitString, ConCat, Mark ;
FROM SymbolTable IMPORT NulSym, IsType, IsVar, IsConst ;


TYPE
   StateCheck = POINTER TO RECORD
                              state: StateSet ;
                              stack,
                              next : StateCheck ;
                           END ;

   State = (const, var, type, constfunc, varparam, constructor) ;

   StateSet = SET OF State ;

VAR
   FreeList: StateCheck ;


(*
   InitState - returns a new initialized StateCheck.
*)

PROCEDURE InitState () : StateCheck ;
VAR
   s: StateCheck ;
BEGIN
   s := New () ;
   WITH s^ DO
      state := StateSet {} ;
      stack := NIL ;
      next := NIL
   END ;
   RETURN s
END InitState ;


(*
   New - returns an uninitialized StateCheck.
*)

PROCEDURE New () : StateCheck ;
VAR
   s: StateCheck ;
BEGIN
   IF FreeList = NIL
   THEN
      NEW (s)
   ELSE
      s := FreeList ;
      FreeList := FreeList^.next
   END ;
   RETURN s
END New ;


(*
   PushState - duplicates the StateCheck s and chains the new copy to s.
               Return the copy.
*)

PROCEDURE PushState (VAR s: StateCheck) ;
VAR
   copy: StateCheck ;
BEGIN
   copy := InitState () ;
   copy^.state := s^.state ;
   copy^.stack := s ;
   s := copy
END PushState ;


(*
   KillState - destructor for StateCheck.
*)

PROCEDURE KillState (VAR s: StateCheck) ;
VAR
   t: StateCheck ;
BEGIN
   WHILE s^.stack # NIL DO
      t := s^.stack ;
      s^.stack := t^.stack ;
      Dispose (t)
   END ;
   Dispose (s)
END KillState ;


(*
   Dispose - place s onto the FreeList and set s to NIL.
*)

PROCEDURE Dispose (VAR s: StateCheck) ;
BEGIN
   s^.next := FreeList ;
   FreeList := s
END Dispose ;


(*
   InclVar - s := s + {var}.
*)

PROCEDURE InclVar (s: StateCheck) ;
BEGIN
   INCL (s^.state, var)
END InclVar ;


(*
   InclConst - s := s + {const}.
*)

PROCEDURE InclConst (s: StateCheck) ;
BEGIN
   INCL (s^.state, const)
END InclConst ;


(*
   InclType - s := s + {type}.
*)

PROCEDURE InclType (s: StateCheck) ;
BEGIN
   INCL (s^.state, type)
END InclType ;


(*
   InclConstFunc - s := s + {constfunc}.
*)

PROCEDURE InclConstFunc (s: StateCheck) ;
BEGIN
   INCL (s^.state, constfunc)
END InclConstFunc ;


(*
   InclVarParam - s := s + {varparam}.
*)

PROCEDURE InclVarParam (s: StateCheck) ;
BEGIN
   INCL (s^.state, varparam)
END InclVarParam ;


(*
   InclConstructor - s := s + {constructor}.
*)

PROCEDURE InclConstructor (s: StateCheck) ;
BEGIN
   INCL (s^.state, constructor)
END InclConstructor ;


(*
   ExclVar - s := s - {var}.
*)

PROCEDURE ExclVar (s: StateCheck) ;
BEGIN
   EXCL (s^.state, var)
END ExclVar ;


(*
   ExclConst - s := s - {const}.
*)

PROCEDURE ExclConst (s: StateCheck) ;
BEGIN
   EXCL (s^.state, const)
END ExclConst ;


(*
   ExclType - s := s - {type}.
*)

PROCEDURE ExclType (s: StateCheck) ;
BEGIN
   EXCL (s^.state, type)
END ExclType ;


(*
   ExclConstFunc - s := s - {constfunc}.
*)

PROCEDURE ExclConstFunc (s: StateCheck) ;
BEGIN
   EXCL (s^.state, constfunc)
END ExclConstFunc ;


(*
   ExclVarParam - s := s - {varparam}.
*)

PROCEDURE ExclVarParam (s: StateCheck) ;
BEGIN
   EXCL (s^.state, varparam)
END ExclVarParam ;


(*
   ExclConstructor - s := s - {varparam}.
*)

PROCEDURE ExclConstructor (s: StateCheck) ;
BEGIN
   EXCL (s^.state, constructor)
END ExclConstructor ;


(*
   PopState - pops the current state.
*)

PROCEDURE PopState (VAR s: StateCheck) ;
VAR
   t: StateCheck ;
BEGIN
   t := s ;
   s := s^.stack ;
   t^.stack := NIL ;
   Dispose (t)
END PopState ;


(*
   CheckQualident - checks to see that qualident sym is allowed in the state s.
*)

PROCEDURE CheckQualident (tok: CARDINAL; s: StateCheck; sym: CARDINAL) ;
BEGIN
   IF sym = NulSym
   THEN
      (* Ignore.  *)
   ELSIF IsType (sym)
   THEN
      IF (constfunc IN s^.state) OR (constructor IN s^.state)
      THEN
         (* Ok.  *)
      ELSIF const IN s^.state
      THEN
         GenerateError (tok, s, sym)
      END
   ELSIF IsConst (sym)
   THEN
      IF (constfunc IN s^.state) OR (constructor IN s^.state)
      THEN
         (* Ok.  *)
      ELSIF (var IN s^.state) OR (type IN s^.state)
      THEN
         GenerateError (tok, s, sym)
      END
   ELSIF IsVar (sym)
   THEN
      IF constfunc IN s^.state
      THEN
         (* Ok.  *)
      ELSIF (const IN s^.state) OR (type IN s^.state) OR (var IN s^.state)
      THEN
         GenerateError (tok, s, sym)
      END
   END
END CheckQualident ;


(*
   GenerateError - generates an unrecoverable error string based on the state and sym.
*)

PROCEDURE GenerateError (tok: CARDINAL; s: StateCheck; sym: CARDINAL) ;
VAR
   str: String ;
BEGIN
   str := InitString ('not expecting the {%1Ad} {%1a} in a ') ;
   IF const IN s^.state
   THEN
      str := ConCat (str, Mark (InitString ('{%kCONST} block')))
   ELSIF type IN s^.state
   THEN
      str := ConCat (str, Mark (InitString ('{%kTYPE} block')))
   ELSIF var IN s^.state
   THEN
      str := ConCat (str, Mark (InitString ('{%kVAR} block')))
   END ;
   IF constfunc IN s^.state
   THEN
      str := ConCat (str, Mark (InitString (' and within a constant procedure function actual parameter')))
   END ;
   IF constructor IN s^.state
   THEN
      str := ConCat (str, Mark (InitString (' and within a constructor')))
   END ;
   MetaErrorStringT1 (tok, str, sym)
END GenerateError ;


(*
   init - initialize the global variables in the module.
*)

PROCEDURE init ;
BEGIN
   FreeList := NIL
END init ;


BEGIN
   init
END M2StateCheck.
