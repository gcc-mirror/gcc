(* M2Pragma.mod build and interpret pragma symbols.

Copyright (C) 2026 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Pragma ;  (*!m2iso+gm2*)


FROM M2MetaError IMPORT MetaErrorT0, MetaErrorT1, MetaErrorStringT0,
                        MetaError1 ;

FROM M2Reserved IMPORT toktype,
                       equaltok, lessgreatertok, hashtok, bartok, ambersandtok ;

FROM DynamicStrings IMPORT InitString, String, string, InitStringCharStar, ConCat,
                           ConCatChar, KillString, Mark ;

FROM SymbolTable IMPORT PragmaAppend, GetNthPragma,
                        MakePragmaOption, MakePragmaLoop, GetPragmaOptions,
                        IsPragmaLoop, IsPragmaOption, IsPragmaFunc,
                        IsPragmaError, IsPragmaWarn, IsPragmaInfo,
                        IsPragmaFatal, GetPragmaMessage, PutPragmaMessage,
                        IsConstLit, GetType, GetSymName, GetScopePragma,
                        GetString, IsConstString, NulSym ;

FROM M2Quads IMPORT PushTtok, PopTtok, PopT, PopNothing ;
FROM NameKey IMPORT Name, makekey, MakeKey, KeyToCharStar ;
FROM M2Base IMPORT True, False, Integer, ZType ;
FROM StringConvert IMPORT stoi, itos ;
FROM M2LexBuf IMPORT MakeVirtual2Tok ;
FROM Storage IMPORT ALLOCATE ;
FROM M2Error IMPORT FlushWarnings, InternalError ;

FROM M2Options IMPORT Options, PushOptions, PopOptions,
                      OptionExistsBoolean, OptionExistsInteger,
                      OptionExistsName,
                      GetOptionBoolean, GetOptionInteger,
                      SetNamedOptionBoolean, SetNamedOptionInteger ;

CONST
   DebugPragma = FALSE ;

TYPE
   PragmaInfo = POINTER TO RECORD
                              scope: CARDINAL ;
                              count: CARDINAL ;
                              next : PragmaInfo ;
                           END ;

VAR
   FreeList,
   PragmaStack: PragmaInfo ;


(*
   PushPragmaScope -
*)

PROCEDURE PushPragmaScope (sym: CARDINAL) ;
VAR
   pi: PragmaInfo ;
BEGIN
   IF FreeList = NIL
   THEN
      NEW (pi)
   ELSE
      pi := FreeList ;
      FreeList := FreeList^.next
   END ;
   WITH pi^ DO
      scope := sym ;
      count := 0 ;
      next := PragmaStack
   END ;
   PragmaStack := pi
END PushPragmaScope ;


(*
   PopPragmaScope -
*)

PROCEDURE PopPragmaScope (sym: CARDINAL) ;
VAR
   pi: PragmaInfo ;
BEGIN
   IF PragmaStack^.scope # sym
   THEN
      MetaError1 ("{%1Aa} unexpected scope seen when ending pragma", sym)
   END ;
   pi := PragmaStack ;
   PragmaStack := PragmaStack^.next ;
   pi^.next := FreeList ;
   FreeList := pi
END PopPragmaScope ;


(*
   BuildPragmaOption - create or retrieve a pragma symbol and optionally
                       push it to the pragma stack.
*)

PROCEDURE BuildPragmaOption (tokno: CARDINAL;
                             create, push: BOOLEAN) ;
VAR
   scope,
   pragma: CARDINAL ;
BEGIN
   scope := PragmaStack^.scope ;
   IF create
   THEN
      pragma := MakePragmaOption (tokno) ;
      PragmaAppend (scope, pragma)
   ELSE
      INC (PragmaStack^.count) ;
      pragma := GetNthPragma (scope, PragmaStack^.count)
   END ;
   IF push
   THEN
      PushTtok (pragma, tokno)
   END
END BuildPragmaOption ;


(*
   BuildPragmaLoop - create or retrieve a pragma loop symbol and
                     push it to the quad stack.
*)

PROCEDURE BuildPragmaLoop (tokno: CARDINAL) ;
VAR
   pragma: CARDINAL ;
BEGIN
   pragma := MakePragmaLoop (tokno) ;
   PushTtok (tokno, pragma)
END BuildPragmaLoop ;


(*
   PopPragma - pops the top of stack from the pragma stack.
*)

PROCEDURE PopPragma ;
VAR
   pragma,
   tokno : CARDINAL ;
BEGIN
   PopTtok (pragma, tokno) ;
   IF NOT (IsPragmaOption (pragma) OR IsPragmaLoop (pragma) OR
          (pragma = True) OR (pragma = False) OR IsPragmaFunc (pragma))
   THEN
      InternalError ('PopPragma is expecting a pragma option, pragma loop or pragma func symbol')
   END
END PopPragma ;


(*
   BuildPragmaFalse - pushes false to the quad stack.
*)

PROCEDURE BuildPragmaFalse (tokno: CARDINAL) ;
BEGIN
   PushTtok (False, tokno)
END BuildPragmaFalse ;


(*
   BuildPragmaTrue - pushes true to the quad stack.
*)

PROCEDURE BuildPragmaTrue (tokno: CARDINAL) ;
BEGIN
   PushTtok (True, tokno)
END BuildPragmaTrue ;


(*
   PragmaEval - execute the func on the top of the stack providing the value
                under func is the same as condition.

                +-------------+  top of stack
                | func        |
                |-------------|
                | value       |
                |-------------|
*)

PROCEDURE PragmaEval (condition: BOOLEAN) ;
VAR
   functok, valuetok: CARDINAL ;
   func, value, cond: CARDINAL ;
BEGIN
   PopTtok (func, functok) ;
   PopTtok (value, valuetok) ;
   PushBooleanValue (valuetok, condition) ;
   PopT (cond) ;
   IF cond = value
   THEN
      EvalFunc (func, functok)
   END ;
   (* Restore value.  *)
   PushTtok (value, valuetok)
END PragmaEval ;


(*
   BuildPragmaName - merge the top two idents on the stack and
                     push the combined ident to the stack with
                     a combined virtual token.
*)

PROCEDURE BuildPragmaName ;
VAR
   prevname, curname,
   prevtok, curtok  : CARDINAL ;
   str              : String ;
BEGIN
   PopTtok (curname, curtok) ;
   PopTtok (prevname, prevtok) ;
   str := InitStringCharStar (KeyToCharStar (prevname)) ;
   str := ConCatChar (str, '.') ;
   str := ConCat (str, Mark (InitStringCharStar (KeyToCharStar (curname)))) ;
   curname := makekey (string (str)) ;
   PushTtok (curname, MakeVirtual2Tok (prevtok, curtok)) ;
   str := KillString (str)
END BuildPragmaName ;


(*
   BuildPragmaDomain - merge the top two idents on the stack and
                      push the combined ident to the stack with
                      a combined virtual token.  It ensures that
                      the domain is valid with either "gcc." or
                      "gm2.".
*)

PROCEDURE BuildPragmaDomain ;
VAR
   prevname, curname,
   prevtok, curtok  : CARDINAL ;
BEGIN
   PopTtok (curname, curtok) ;
   PopTtok (prevname, prevtok) ;
   IF prevname = MakeKey ('gcc')
   THEN
      IF (curname # MakeKey ('option')) AND
         (curname # MakeKey ('default'))
      THEN
         MetaErrorT0 (curtok, "expecting option or default scope to be specified for a gcc domain qualident")
      END
   ELSIF prevname = MakeKey ('gm2')
   THEN
      IF (curname # MakeKey ('option')) AND
         (curname # MakeKey ('local')) AND
         (curname # MakeKey ('default'))
      THEN
         MetaErrorT0 (curtok, "expecting option, local or default scope to be specified for a gm2 domain qualident")
      END
   ELSE
      MetaErrorT0 (prevtok, "{%A}expecting a gcc or gm2 domain name")
   END ;
   PushTtok (prevname, prevtok) ;
   PushTtok (curname, curtok) ;
   BuildPragmaName
END BuildPragmaDomain ;


(*
   PushBooleanValue - push the boolean value to the quad stack.
*)

PROCEDURE PushBooleanValue (tokno: CARDINAL; boolval: BOOLEAN) ;
BEGIN
   IF boolval
   THEN
      PushTtok (True, tokno)
   ELSE
      PushTtok (False, tokno)
   END
END PushBooleanValue ;


(*
   PushIntegerValue - push the value to the quad stack.
*)

PROCEDURE PushIntegerValue (tokno: CARDINAL; value: CARDINAL) ;
BEGIN
   PushTtok (value, tokno)
END PushIntegerValue ;


(*
   BuildPragmaValue - look up pragma value and push the value to stack.
                      If the value cannot be found push False.
*)

PROCEDURE BuildPragmaValue ;
VAR
   name : Name ;
   tokno: CARDINAL ;
   str  : String ;
BEGIN
   PopTtok (name, tokno) ;
   str := InitStringCharStar (KeyToCharStar (name)) ;
   IF OptionExistsBoolean (str)
   THEN
      PushBooleanValue (tokno, GetOptionBoolean (tokno, str))
   ELSIF OptionExistsInteger (str)
   THEN
      PushIntegerValue (tokno, GetOptionInteger (tokno, str))
(*   ELSIF OptionExistsName (str)
   THEN
      InternalError ('string value not yet implemented in M2Pragma')
      *)
      (* --FIXME--.  *)
   ELSE
      PushTtok (False, tokno)
   END ;
   str := KillString (str)
END BuildPragmaValue ;


(*
   BuildPragmaExists - If the pragma name is known push True
                       else push False.
*)

PROCEDURE BuildPragmaExists ;
VAR
   name : Name ;
   tokno: CARDINAL ;
   str  : String ;
BEGIN
   PopTtok (name, tokno) ;
   str := InitStringCharStar (KeyToCharStar (name)) ;
   IF OptionExistsBoolean (str)
   THEN
      PushBooleanValue (tokno, TRUE)
   ELSIF OptionExistsInteger (str)
   THEN
      PushBooleanValue (tokno, TRUE)
(*   ELSIF OptionExistsName (str)
   THEN
      InternalError ('string value not yet implemented in M2Pragma')
      *)
      (* --FIXME--.  *)
   ELSE
      PushTtok (False, tokno)
   END ;
   str := KillString (str)
END BuildPragmaExists ;


(*
   ToInteger - converts const lit sym to an INTEGER.
*)

PROCEDURE ToInteger (tok: CARDINAL; sym: CARDINAL) : INTEGER ;
VAR
   i  : INTEGER ;
   str: String ;
BEGIN
   IF IsConstLit (sym)
   THEN
      str := InitStringCharStar (KeyToCharStar (GetSymName (sym))) ;
      i := stoi (str) ;
      str := KillString (str) ;
      RETURN i
   ELSE
      MetaErrorT1 (tok, 'expecting an integer constant literal and not a {%1Ead}',
                   sym) ;
      RETURN 0
   END
END ToInteger ;


(*
   BuildPragmaAssignment - assigns top of stack to the pragma ident underneath.
                           Both the ident and value are removed from the stack.
*)

PROCEDURE BuildPragmaAssignment ;
VAR
   combined,
   nametok,
   valuetok: CARDINAL ;
   name    : Name ;
   value   : CARDINAL ;
   str     : String ;
BEGIN
   PopTtok (value, valuetok) ;
   PopTtok (name, nametok) ;
   str := InitStringCharStar (KeyToCharStar (name)) ;
   combined := MakeVirtual2Tok (nametok, valuetok) ;
   IF OptionExistsBoolean (str)
   THEN
      IF value = True
      THEN
         SetNamedOptionBoolean (combined, str, TRUE)
      ELSIF value = False
      THEN
         SetNamedOptionBoolean (combined, str, FALSE) ;
      ELSE
         MetaErrorT1 (combined, 'the value {%1Ea} is a {%1td} and is being assigned to a boolean option', value)
      END
   ELSIF OptionExistsInteger (str)
   THEN
      IF IsConstLit (value) AND (GetType (value) = ZType)
      THEN
         SetNamedOptionInteger (combined, str, ToInteger (valuetok, value)) ;
      END
      (*
   ELSIF OptionExistsName (str)
   THEN
      InternalError ('string value not yet implemented in M2Pragma') ;
      (* --FIXME--.  *)
       *)
   ELSE
      MetaErrorT1 (combined, 'the value {%1Ea} is being assigned to an unknown option', value)
   END ;
   str := KillString (str)
END BuildPragmaAssignment ;


(*
   PragmaInvert - invert the boolean value on the top of the stack.
*)

PROCEDURE PragmaInvert (nottok: CARDINAL) ;
VAR
   value, tokno: CARDINAL ;
BEGIN
   PopTtok (value, tokno) ;
   IF value=True
   THEN
      PushTtok (False, MakeVirtual2Tok (nottok, tokno))
   ELSIF value=False
   THEN
      PushTtok (True, MakeVirtual2Tok (nottok, tokno))
   ELSE
      MetaErrorT0 (tokno, 'cannot invert a non boolean expression') ;
      MetaErrorT0 (nottok, 'expression following the {%kNOT} operator is not a boolean expression') ;
      PushTtok (True, MakeVirtual2Tok (nottok, tokno))
   END
END PragmaInvert ;


(*
   PragmaBinary - pops the top two elements from the stack and
                  pushes the result after the binary operator
                  has been interpreted.
*)

PROCEDURE PragmaBinary ;
VAR
   op             : toktype ;
   opcard,
   combined,
   left, lefttok,
   optok,
   right, righttok: CARDINAL ;
BEGIN
   PopTtok (right, righttok) ;
   PopTtok (opcard, optok) ;
   PopTtok (left, lefttok) ;
   combined := MakeVirtual2Tok (righttok, lefttok) ;
   op := VAL (toktype, opcard) ;
   IF op = equaltok
   THEN
      IF left = right
      THEN
         PushTtok (True, combined)
      ELSE
         PushTtok (False, combined)
      END
   ELSIF (op = hashtok) OR (op = lessgreatertok)
   THEN
      IF left = right
      THEN
         PushTtok (False, combined)
      ELSE
         PushTtok (True, combined)
      END
   ELSIF op = bartok
   THEN
      (* Or.  *)
      IF (left = True) OR (right = True)
      THEN
         PushTtok (True, combined)
      ELSE
         PushTtok (False, combined)
      END
   ELSIF op = ambersandtok
   THEN
      (* And.  *)
      IF (left = True) AND (right = True)
      THEN
         PushTtok (True, combined)
      ELSE
         PushTtok (False, combined)
      END
   END
END PragmaBinary ;


(*
   EvalFunc - evaluate a possible pragma func value.
              If value is a pragma func then the appropriate
              message is generated.
*)

PROCEDURE EvalFunc (func: CARDINAL; tok: CARDINAL) ;
VAR
   message,
   msg    : String ;
BEGIN
   IF IsPragmaFunc (func)
   THEN
      msg := GetPragmaMessage (func) ;
      IF IsPragmaFatal (func)
      THEN
         message := InitString ('{%A}')
      ELSIF IsPragmaWarn (func)
      THEN
         message := InitString ('{%W}')
      ELSIF IsPragmaInfo (func)
      THEN
         message := InitString ('{%O}')
      ELSIF IsPragmaError (func)
      THEN
         message := InitString ('{%E}')
      END ;
      message := ConCat (message, msg) ;
      MetaErrorStringT0 (tok, message) ;
   END
END EvalFunc ;


(*
   Append - append exp to des.  If des is NIL then set des to exp.
            exp is consumed and should not be used afterwards.
*)

PROCEDURE Append (VAR des: String; exp: String) ;
BEGIN
   IF des = NIL
   THEN
      des := exp
   ELSE
      des := ConCat (des, Mark (exp))
   END
END Append ;


(*
   BuildPragmaMessage - assigns the top of stack string to the pragma func.
*)

PROCEDURE BuildPragmaMessage ;
VAR
   str            : String ;
   combined,
   msgtok, functok: CARDINAL ;
   Msg, PragmaFunc: CARDINAL ;
BEGIN
   PopTtok (Msg, msgtok) ;
   PopTtok (PragmaFunc, functok) ;
   combined := MakeVirtual2Tok (functok, msgtok) ;
   IF IsConstString (Msg)
   THEN
      str := GetPragmaMessage (PragmaFunc) ;
      Append (str, InitStringCharStar (KeyToCharStar (GetString (Msg)))) ;
      PutPragmaMessage (combined, PragmaFunc, str)
   END ;
   PushTtok (PragmaFunc, combined)
END BuildPragmaMessage ;


(*
   BuildPragmaMessageValue - appends the value of the top of stack string
                             to the pragma func.
*)

PROCEDURE BuildPragmaMessageValue ;
VAR
   option, str      : String ;
   combined,
   msgtok, functok  : CARDINAL ;
   optqual, PragmaFunc: CARDINAL ;
BEGIN
   PopTtok (optqual, msgtok) ;
   PopTtok (PragmaFunc, functok) ;
   combined := MakeVirtual2Tok (functok, msgtok) ;
   IF IsConstString (optqual)
   THEN
      option := InitStringCharStar (KeyToCharStar (GetString (optqual))) ;
      str := GetPragmaMessage (PragmaFunc) ;
      IF OptionExistsBoolean (option)
      THEN
         IF GetOptionBoolean (msgtok, option)
         THEN
            Append (str, Mark (InitString ('TRUE')))
         ELSE
            Append (str, Mark (InitString ('FALSE')))
         END
      ELSIF OptionExistsInteger (option)
      THEN
         Append (str, Mark (itos (GetOptionInteger (msgtok, option), 0, ' ', TRUE)))
      (*   ELSIF OptionExistsName (str)
           THEN
          InternalError ('string value not yet implemented in M2Pragma')
      *)
         (* --FIXME--.  *)
      END ;
      PutPragmaMessage (combined, PragmaFunc, str) ;
      option := KillString (option)
   END ;
   PushTtok (PragmaFunc, combined)
END BuildPragmaMessageValue ;


(*
   EnterPragmaOptions - push the options associated with procedure,
                        module or defimp sym.
*)

PROCEDURE EnterPragmaOptions (sym: CARDINAL) ;
VAR
   po: CARDINAL ;
BEGIN
   PushPragmaScope (sym) ;
   po := GetScopePragma (sym) ;
   IF po # NulSym
   THEN
      IF DebugPragma
      THEN
         MetaError1 ("{%1Oa} push pragma", sym) ;
         FlushWarnings    (* --fixme-- Warning this will change the currentScope in M2Error.  *)
      END ;
      PushOptions (GetPragmaOptions (po))
   END
END EnterPragmaOptions ;


(*
   EndPragmaOptions - pop the options associated with procedure,
                      module or defimp.
*)

PROCEDURE EndPragmaOptions (sym: CARDINAL) ;
VAR
   po: CARDINAL ;
BEGIN
   po := GetScopePragma (sym) ;
   IF po # NulSym
   THEN
      IF DebugPragma
      THEN
         MetaError1 ("{%1Oa} pop pragma", sym) ;
         FlushWarnings    (* --fixme-- Warning this will change the currentScope in M2Error.  *)
      END ;
      PopOptions (GetPragmaOptions (po))
   END ;
   PopPragmaScope (sym)
END EndPragmaOptions ;


(*
   Init - initialize module global data.
*)

PROCEDURE Init ;
BEGIN
   PragmaStack := NIL
END Init ;


BEGIN
   Init
END M2Pragma.
