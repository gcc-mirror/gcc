(* M2SymInit.mod records initialization state for variables.

Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2SymInit ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Debug IMPORT Assert ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;
FROM libc IMPORT printf ;
FROM NameKey IMPORT Name, NulName, KeyToCharStar, MakeKey ;
FROM M2Base IMPORT Nil ;

FROM M2Options IMPORT UninitVariableChecking, UninitVariableConditionalChecking,
                      CompilerDebugging ;

FROM M2MetaError IMPORT MetaErrorT1, MetaErrorStringT1, MetaErrorStringT2 ;
FROM M2LexBuf IMPORT UnknownTokenNo ;
FROM DynamicStrings IMPORT String, InitString, Mark, ConCat, InitString ;
FROM M2Error IMPORT InternalError ;

FROM M2BasicBlock IMPORT BasicBlock,
                         InitBasicBlocks, InitBasicBlocksFromRange,
			 KillBasicBlocks, FreeBasicBlocks,
                         ForeachBasicBlockDo,
                         GetBasicBlockStart, GetBasicBlockEnd ;

IMPORT Indexing ;
FROM Indexing IMPORT Index ;

FROM Lists IMPORT List, InitList, GetItemFromList, PutItemIntoList,
                  IsItemInList, IncludeItemIntoList, NoOfItemsInList,
                  RemoveItemFromList, ForeachItemInListDo, KillList, DuplicateList ;

FROM SymbolTable IMPORT NulSym, ModeOfAddr, IsVar, IsRecord, GetSType,
                        ProcedureKind, GetNthParam, NoOfParam,
                        GetNth, IsRecordField, IsSet, IsArray, IsProcedure,
                        GetVarScope, IsVarAParam, IsComponent, GetMode,
                        VarCheckReadInit, VarInitState, PutVarInitialized,
                        PutVarFieldInitialized, GetVarFieldInitialized,
                        IsConst, IsConstString, NoOfParamAny, IsVarParamAny,
                        ForeachLocalSymDo, ForeachParamSymDo,
                        IsTemporary, ModeOfAddr,
                        IsReallyPointer, IsUnbounded,
                        IsVarient, IsFieldVarient, GetVarient,
                        IsVarArrayRef, GetSymName,
                        IsType, IsPointer, IsTuple,
                        GetParameterShadowVar, IsParameter, GetLType,
                        GetParameterHeapVar, GetVarDeclTok ;

FROM M2Quads IMPORT QuadOperator, GetQuadOtok, GetQuad, GetNextQuad,
                    IsNewLocalVar, IsReturn, IsKillLocalVar, IsConditional,
                    IsUnConditional, IsBackReference, IsCall, IsGoto,
                    GetM2OperatorDesc, Opposite, DisplayQuadRange,
                    GetQuadTrash ;

FROM M2Printf IMPORT printf0, printf1, printf2 ;
FROM M2GCCDeclare IMPORT PrintSym ;


CONST
   Debugging = FALSE ;

TYPE
   descType = (scalar, record) ;

   InitDesc = POINTER TO RECORD
                            sym, type  : CARDINAL ;
                            initialized: BOOLEAN ;
                            CASE kind: descType OF

                            scalar:  |
                            record:  rec:  recordDesc |

                            END
                         END ;

   recordDesc = RECORD
                   fieldDesc: Indexing.Index ;
                END ;

   symAlias = POINTER TO RECORD
                            keySym,
                            alias : CARDINAL ;
                            next  : symAlias ;
                         END ;

   bbEntry = POINTER TO RECORD
                           start, end: CARDINAL ;
                           (* Is this the first bb?  *)
                           first,
                           (* Does it end with a call?  *)
                           endCall,
                           (* Does it end with a goto?  *)
                           endGoto,
                           (* Does it end with a conditional?  *)
                           endCond,
                           (* Does it form part of a loop?  *)
                           topOfLoop : BOOLEAN ;
                           trashQuad,
                           indexBB,
                           nextQuad,
                           condQuad,
                           nextBB,
                           condBB    : CARDINAL ;
                           next      : bbEntry ;
                        END ;

VAR
   IndirectArray,
   LArray       : Indexing.Index ;
   freeList     : symAlias ;
   bbArray      : Indexing.Index ;
   bbFreeList   : bbEntry ;
   ignoreList,
   errorList    : List ;   (* Ensure that we only generate one set of warnings per token.  *)


(*
   PrintSymInit -
*)

PROCEDURE PrintSymInit (desc: InitDesc) ;
VAR
   i, n: CARDINAL ;
BEGIN
   printf ("sym %d: type %d ", desc^.sym, desc^.type) ;
   IF desc^.kind = scalar
   THEN
      printf ("scalar")
   ELSE
      printf ("record")
   END ;
   IF NOT desc^.initialized
   THEN
      printf (" not")
   END ;
   printf (" initialized\n") ;
   IF (desc^.type # NulSym) AND IsRecord (desc^.type)
   THEN
      i := 1 ;
      n := Indexing.HighIndice (desc^.rec.fieldDesc) ;
      WHILE i <= n DO
         PrintSymInit (Indexing.GetIndice (desc^.rec.fieldDesc, i)) ;
         INC (i)
      END
   END
END PrintSymInit ;


PROCEDURE InitSymInit () : InitDesc ;
VAR
   id: InitDesc ;
BEGIN
   NEW (id) ;
   WITH id^ DO
      sym := NulSym ;
      type := NulSym ;
      initialized := TRUE ;
      kind := scalar
   END ;
   RETURN id
END InitSymInit ;


PROCEDURE KillSymInit (VAR desc: InitDesc) ;
BEGIN
   WITH desc^ DO
      CASE kind OF

      record:  KillFieldDesc (rec.fieldDesc)

      ELSE
      END
   END ;
   DISPOSE (desc) ;
   desc := NIL
END KillSymInit ;


PROCEDURE ConfigSymInit (desc: InitDesc; sym: CARDINAL) ;
BEGIN
   IF IsVar (sym) OR IsRecordField (sym)
   THEN
      desc^.sym := sym ;
      desc^.type := GetSType (sym) ;
      desc^.initialized := FALSE ;
      IF IsRecord (desc^.type)
      THEN
         desc^.kind := record ;
         desc^.rec.fieldDesc := Indexing.InitIndex (1) ;
         PopulateFields (desc, desc^.type)
      ELSE
         desc^.kind := scalar ;
         IF IsArray (desc^.type)
         THEN
            desc^.initialized := TRUE   (* For now we don't attempt to handle array types.  *)
         END
      END
   END
END ConfigSymInit ;


(*
   KillFieldDesc -
*)

PROCEDURE KillFieldDesc (VAR fielddesc: Indexing.Index) ;
VAR
   i, h: CARDINAL ;
   id  : InitDesc ;
BEGIN
   i := 1 ;
   h := Indexing.HighIndice (fielddesc) ;
   WHILE i <= h DO
      id := Indexing.GetIndice (fielddesc, i) ;
      KillSymInit (id) ;
      INC (i)
   END ;
   fielddesc := Indexing.KillIndex (fielddesc)
END KillFieldDesc ;


(*
   PopulateFields -
*)

PROCEDURE PopulateFields (desc: InitDesc; recsym: CARDINAL) ;
VAR
   field,
   i    : CARDINAL ;
   fdesc: InitDesc ;
BEGIN
   Assert (IsRecord (recsym)) ;
   i := 1 ;
   REPEAT
      field := GetNth (recsym, i) ;
      IF field # NulSym
      THEN
         fdesc := InitSymInit () ;
         ConfigSymInit (fdesc, field) ;
         Indexing.IncludeIndiceIntoIndex (desc^.rec.fieldDesc, fdesc) ;
         INC (i)
      END
   UNTIL field = NulSym
END PopulateFields ;


PROCEDURE SetInitialized (desc: InitDesc) ;
BEGIN
   desc^.initialized := TRUE
END SetInitialized ;


PROCEDURE GetInitialized (desc: InitDesc) : BOOLEAN ;
BEGIN
   IF NOT desc^.initialized
   THEN
      IF IsRecord (desc^.type)
      THEN
         TrySetInitialized (desc)
      END
   END ;
   IF Debugging
   THEN
      PrintSymInit (desc)
   END ;
   RETURN desc^.initialized
END GetInitialized ;


PROCEDURE GetFieldDesc (desc: InitDesc; field: CARDINAL) : InitDesc ;
VAR
   fsym,
   i    : CARDINAL ;
BEGIN
   IF IsRecord (desc^.type)
   THEN
      i := 1 ;
      REPEAT
         fsym := GetNth (desc^.type, i) ;
         IF field = fsym
         THEN
            RETURN Indexing.GetIndice (desc^.rec.fieldDesc, i)
         END ;
         INC (i)
      UNTIL fsym = NulSym
   END ;
   RETURN NIL
END GetFieldDesc ;


PROCEDURE SetFieldInitialized (desc: InitDesc; fieldlist: List) : BOOLEAN ;
BEGIN
   RETURN SetFieldInitializedNo (desc, fieldlist, 1)
END SetFieldInitialized ;


(*
   TrySetInitialized -
*)

PROCEDURE TrySetInitialized (desc: InitDesc) ;
VAR
   i, h : CARDINAL ;
   fdesc: InitDesc ;
BEGIN
   h := Indexing.HighIndice (desc^.rec.fieldDesc) ;
   i := 1 ;
   WHILE i <= h DO
      fdesc := Indexing.GetIndice (desc^.rec.fieldDesc, i) ;
      IF NOT fdesc^.initialized
      THEN
         RETURN
      END ;
      INC (i)
   END ;
   desc^.initialized := TRUE
END TrySetInitialized ;


(*
   SetFieldInitializedNo -
*)

PROCEDURE SetFieldInitializedNo (desc: InitDesc;
                                 fieldlist: List; level: CARDINAL) : BOOLEAN ;
VAR
   nsym : CARDINAL ;
   fdesc: InitDesc ;
BEGIN
   IF level > NoOfItemsInList (fieldlist)
   THEN
      RETURN FALSE
   ELSE
      nsym := GetItemFromList (fieldlist, level) ;
      fdesc := GetFieldDesc (desc, nsym) ;
      IF fdesc = NIL
      THEN
         RETURN FALSE
      ELSIF level = NoOfItemsInList (fieldlist)
      THEN
         SetInitialized (fdesc) ;
         TrySetInitialized (desc) ;
         RETURN desc^.initialized
      ELSE
         IF SetFieldInitializedNo (fdesc, fieldlist, level + 1)
         THEN
         END ;
         TrySetInitialized (desc) ;
         RETURN desc^.initialized
      END
   END
END SetFieldInitializedNo ;


PROCEDURE GetFieldInitialized (desc: InitDesc; fieldlist: List) : BOOLEAN ;
BEGIN
   RETURN GetFieldInitializedNo (desc, fieldlist, 1)
END GetFieldInitialized ;


PROCEDURE GetFieldInitializedNo (desc: InitDesc;
                                 fieldlist: List; level: CARDINAL) : BOOLEAN ;
VAR
   nsym : CARDINAL ;
   fdesc: InitDesc ;
BEGIN
   IF desc^.initialized
   THEN
      RETURN TRUE
   ELSIF level > NoOfItemsInList (fieldlist)
   THEN
      RETURN FALSE
   ELSE
      nsym := GetItemFromList (fieldlist, level) ;
      fdesc := GetFieldDesc (desc, nsym) ;
      IF fdesc = NIL
      THEN
         (* The pointer variable maybe uninitialized and hence we cannot
            find the record variable.  *)
         RETURN FALSE
      ELSIF fdesc^.initialized
      THEN
         RETURN TRUE
      ELSE
         RETURN GetFieldInitializedNo (fdesc, fieldlist, level + 1)
      END
   END
END GetFieldInitializedNo ;


(*
   IsGlobalVar -
*)

PROCEDURE IsGlobalVar (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsVar (sym) AND (NOT IsProcedure (GetVarScope (sym)))
END IsGlobalVar ;


(*
   IsLocalVar -

PROCEDURE IsLocalVar (procsym, varsym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsVar (varsym) AND (GetVarScope (varsym) = procsym)
END IsLocalVar ;
*)


(*
   RecordFieldContainsVarient -
*)

PROCEDURE RecordFieldContainsVarient (sym: CARDINAL; visited: List) : BOOLEAN ;
BEGIN
   Assert (IsRecordField (sym)) ;
   IF doContainsVariant (GetSType (sym), visited)
   THEN
      RETURN TRUE
   END ;
   RETURN GetVarient (sym) # NulSym
END RecordFieldContainsVarient ;


(*
   RecordContainsVarient -
*)

PROCEDURE RecordContainsVarient (sym: CARDINAL; visited: List) : BOOLEAN ;
VAR
   i,
   fieldsym: CARDINAL ;
BEGIN
   Assert (IsRecord (sym)) ;
   i := 1 ;
   REPEAT
      fieldsym := GetNth (sym, i) ;
      IF fieldsym # NulSym
      THEN
         IF IsRecordField (fieldsym)
         THEN
            IF RecordFieldContainsVarient (fieldsym, visited)
            THEN
               RETURN TRUE
            END
         ELSIF IsVarient (fieldsym)
         THEN
            RETURN TRUE
         END ;
         INC (i)
      END
   UNTIL fieldsym = NulSym ;
   RETURN FALSE
END RecordContainsVarient ;


(*
   VarContainsVarient -
*)

PROCEDURE VarContainsVarient (sym: CARDINAL; visited: List) : BOOLEAN ;
BEGIN
   Assert (IsVar (sym)) ;
   RETURN doContainsVariant (GetSType (sym), visited)
END VarContainsVarient ;


(*
   TypeContainsVarient -
*)

PROCEDURE TypeContainsVarient (sym: CARDINAL; visited: List) : BOOLEAN ;
BEGIN
   Assert (IsType (sym)) ;
   RETURN doContainsVariant (GetSType (sym), visited)
END TypeContainsVarient ;


(*
   ArrayContainsVarient -
*)

PROCEDURE ArrayContainsVarient (sym: CARDINAL; visited: List) : BOOLEAN ;
BEGIN
   Assert (IsArray (sym)) ;
   RETURN doContainsVariant (GetSType (sym), visited)
END ArrayContainsVarient ;


(*
   PointerContainsVarient -
*)

PROCEDURE PointerContainsVarient (sym: CARDINAL; visited: List) : BOOLEAN ;
BEGIN
   Assert (IsPointer (sym)) ;
   RETURN doContainsVariant (GetSType (sym), visited)
END PointerContainsVarient ;


(*
   doContainsVariant -
*)

PROCEDURE doContainsVariant (sym: CARDINAL; visited: List) : BOOLEAN ;
BEGIN
   IF (sym # NulSym) AND (NOT IsItemInList (visited, sym))
   THEN
      IncludeItemIntoList (visited, sym) ;
      IF IsVar (sym)
      THEN
         RETURN VarContainsVarient (sym, visited)
      ELSIF IsRecord (sym)
      THEN
         RETURN RecordContainsVarient (sym, visited)
      ELSIF IsPointer (sym)
      THEN
         RETURN PointerContainsVarient (sym, visited)
      ELSIF IsArray (sym)
      THEN
         RETURN ArrayContainsVarient (sym, visited)
      ELSIF IsType (sym)
      THEN
         RETURN TypeContainsVarient (sym, visited)
      END
   END ;
   RETURN FALSE
END doContainsVariant ;


(*
   ContainsVariant - returns TRUE if type sym contains a variant record.
*)

PROCEDURE ContainsVariant (sym: CARDINAL) : BOOLEAN ;
VAR
   visited: List ;
   result : BOOLEAN ;
BEGIN
   InitList (visited) ;
   result := doContainsVariant (sym, visited) ;
   KillList (visited) ;
   RETURN result
END ContainsVariant ;


(*
   IssueConditional -
*)

PROCEDURE IssueConditional (quad: CARDINAL; conditional: BOOLEAN) ;
VAR
   op                          : QuadOperator ;
   op1, op2, op3               : CARDINAL ;
   op1tok, op2tok, op3tok, qtok: CARDINAL ;
   constExpr, overflowChecking : BOOLEAN ;
   s                           : String ;
BEGIN
   GetQuadOtok (quad, qtok, op, op1, op2, op3,
                overflowChecking, constExpr,
                op1tok, op2tok, op3tok) ;
   IF IsUniqueWarning (qtok)
   THEN
      op1tok := DefaultTokPos (op1tok, qtok) ;
      op2tok := DefaultTokPos (op2tok, qtok) ;
      op3tok := DefaultTokPos (op3tok, qtok) ;
      IF NOT conditional
      THEN
         op := Opposite (op)
      END ;
      s := InitString ('depending upon the result of {%1Oad} ') ;
      s := ConCat (s, Mark (GetM2OperatorDesc (op))) ;
      s := ConCat (s, InitString (' {%2ad}')) ;
      MetaErrorStringT2 (qtok, s, op1, op2)
   END
END IssueConditional ;


(*
   GenerateNoteFlow -
*)

PROCEDURE GenerateNoteFlow (n: CARDINAL; warning: BOOLEAN) ;
VAR
   i     : CARDINAL ;
   ip1Ptr,
   iPtr  : bbEntry ;
BEGIN
   IF NOT warning
   THEN
      (* Only issue flow messages for non warnings.  *)
      i := 1 ;
      WHILE i <= n DO
         iPtr := Indexing.GetIndice (bbArray, i) ;
         IF iPtr^.endCond
         THEN
            IF i < n
            THEN
               ip1Ptr := Indexing.GetIndice (bbArray, i+1) ;
               IssueConditional (iPtr^.end, iPtr^.condBB = ip1Ptr^.indexBB)
            END
         END ;
         INC (i)
      END
   END
END GenerateNoteFlow ;


(*
   IssueWarning - issue a warning or note at tok location.
*)

PROCEDURE IssueWarning (tok: CARDINAL;
                        before, after: ARRAY OF CHAR;
                        sym: CARDINAL; warning: BOOLEAN) ;
VAR
   s: String ;
BEGIN
   s := InitString (before) ;
   IF warning
   THEN
      s := ConCat (s, Mark (InitString ('{%1Wad}')))
   ELSE
      s := ConCat (s, Mark (InitString ('{%1Oad}')))
   END ;
   s := ConCat (s, Mark (InitString (after))) ;
   MetaErrorStringT1 (tok, s, sym)
END IssueWarning ;


(*
   IsUniqueWarning - return TRUE if a warning has not been issued at tok.
                     It remembers tok and subsequent calls will always return FALSE.
*)

PROCEDURE IsUniqueWarning (tok: CARDINAL) : BOOLEAN ;
BEGIN
   IF NOT IsItemInList (errorList, tok)
   THEN
      IncludeItemIntoList (errorList, tok) ;
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END IsUniqueWarning ;


(*
   CheckDeferredRecordAccess -
*)

PROCEDURE CheckDeferredRecordAccess (tok: CARDINAL;
                                     sym: CARDINAL;
                                     canDereference, warning: BOOLEAN;
                                     i: CARDINAL) ;
VAR
   unique: BOOLEAN ;
BEGIN
   IF IsVar (sym)
   THEN
      IF Debugging
      THEN
         Trace ("CheckDeferredRecordAccess %d\n", sym) ;
         PrintSym (sym) ;
         IF canDereference
         THEN
            printf1 ("checkReadInit (%d, true)\n", sym)
         ELSE
            printf1 ("checkReadInit (%d, false)\n", sym)
         END
      END ;
      IF IsExempt (sym)
      THEN
         Trace ("checkReadInit sym is a parameter or not a local variable (%d)", sym) ;
         (* We assume parameters have been initialized.  *)
         PutVarInitialized (sym, LeftValue) ;
         PutVarInitialized (sym, RightValue)
         (* SetVarInitialized (sym, TRUE) *)
      ELSIF IsUnbounded (GetSType (sym))
      THEN
         SetVarInitialized (sym, TRUE, tok)
      ELSIF IsComponent (sym)
      THEN
         Trace ("checkReadInit IsComponent (%d) is true)", sym) ;
         IF (NOT GetVarComponentInitialized (sym, tok)) AND IsUniqueWarning (tok)
         THEN
            GenerateNoteFlow (i, warning) ;
            IssueWarning (tok,
                          'attempting to access ',
                          ' before it has been initialized',
                          sym, warning)
         END
      ELSIF (GetMode (sym) = LeftValue) AND canDereference
      THEN
         Trace ("checkReadInit GetMode (%d) = LeftValue and canDereference (LeftValue and RightValue VarCheckReadInit)", sym) ;
         unique := TRUE ;
         IF NOT VarCheckReadInit (sym, LeftValue)
         THEN
            unique := IsUniqueWarning (tok) ;
            IF unique
            THEN
               GenerateNoteFlow (i, warning) ;
               IssueWarning (tok,
                             'attempting to access the address of ',
                             ' before it has been initialized',
                             sym, warning)
            END
         END ;
         IF NOT VarCheckReadInit (sym, RightValue)
         THEN
            IF unique
            THEN
               GenerateNoteFlow (i, warning) ;
               IssueWarning (tok,
                             'attempting to access ', ' before it has been initialized',
                             sym, warning)
            END
         END
      ELSE
         Trace ("checkReadInit call VarCheckReadInit using GetMode (%d)", sym) ;
         IF (NOT VarCheckReadInit (sym, GetMode (sym))) AND IsUniqueWarning (tok)
         THEN
            GenerateNoteFlow (i, warning) ;
            IssueWarning (tok,
                          'attempting to access ',
                          ' before it has been initialized',
                          sym, warning)
         END
      END
   END
END CheckDeferredRecordAccess ;


(*
   SetVarUninitialized - resets variable init state.
*)

PROCEDURE SetVarUninitialized (sym: CARDINAL) ;
BEGIN
   IF IsVar (sym)
   THEN
      IF NOT IsUnbounded (GetSType (sym))
      THEN
         VarInitState (sym)
      END
   END
END SetVarUninitialized ;


(*
   ComponentFindVar -
*)

PROCEDURE ComponentFindVar (sym: CARDINAL;
                            VAR lvalue: BOOLEAN;
                            tok: CARDINAL) : CARDINAL ;
VAR
   nsym,
   i   : CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      nsym := GetNth (sym, i) ;
      lvalue := GetMode (nsym) = LeftValue ;
      nsym := getLAlias (nsym) ;
      IF nsym = Nil
      THEN
         MetaErrorT1 (tok,
                      "attempting to dereference {%1Wad} which will be a {%kNIL} pointer",
                      sym) ;
         RETURN NulSym
      ELSIF (nsym # NulSym) AND IsVar (nsym)
      THEN
         IF (nsym # sym) AND IsComponent (nsym)
         THEN
            RETURN ComponentFindVar (nsym, lvalue, tok)
         ELSE
            RETURN nsym
         END
      END ;
      INC (i)
   UNTIL nsym = NulSym ;
   RETURN NulSym
END ComponentFindVar ;


(*
   ComponentCreateFieldList - builds a list of fields accessed by the component var.
                              Each item in the list will be a field of incremental levels
                              though a nested record.  It is not a list of fields
                              at the same level.

                              foo = RECORD
                                       v: RECORD
                                             x, y: CARDINAL ;
                                          END ;
                                       w: CARDINAL ;
                                    END ;

                              { v, x } for example and not { v, w }
*)

PROCEDURE ComponentCreateFieldList (sym: CARDINAL) : List ;
VAR
   lst: List ;
BEGIN
   InitList (lst) ;
   IF IsVar (sym) AND IsComponent (sym)
   THEN
      ComponentBuildFieldList (lst, sym)
   END ;
   RETURN lst
END ComponentCreateFieldList ;


PROCEDURE ComponentBuildFieldList (lst: List; sym: CARDINAL) ;
VAR
   i, nsym: CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      nsym := GetNth (sym, i) ;
      IF nsym # NulSym
      THEN
         IF IsComponent (nsym)
         THEN
            ComponentBuildFieldList (lst, nsym)
         ELSIF IsRecordField (nsym)
         THEN
            IncludeItemIntoList (lst, nsym)
         END ;
         INC (i)
      END
   UNTIL nsym = NulSym
END ComponentBuildFieldList ;


(*
   deRefComponent -
*)

PROCEDURE deRefComponent (component: CARDINAL; lvalue: BOOLEAN;
                          sym: CARDINAL; tok: CARDINAL) : CARDINAL ;
BEGIN
   IF lvalue
   THEN
      RETURN getContent (component, sym, tok)
   ELSE
      RETURN component
   END
END deRefComponent ;


(*
   SetVarComponentInitialized -
*)

PROCEDURE SetVarComponentInitialized (sym: CARDINAL; tok: CARDINAL) ;
VAR
   lvalue: BOOLEAN ;
   i, n,
   fsym,
   vsym  : CARDINAL ;
   lst   : List ;
BEGIN
   vsym := ComponentFindVar (sym, lvalue, tok) ;
   vsym := deRefComponent (vsym, lvalue, sym, tok) ;
   IF vsym # NulSym
   THEN
      IF Debugging
      THEN
         printf0 ("*************** vsym is: ") ;
         PrintSym (vsym)
      END ;
      (* Build list accessing the field.  *)
      lst := ComponentCreateFieldList (sym) ;
      IF Debugging
      THEN
         printf2 ("sym = %d, vsym = %d, fields:", sym, vsym)
      END ;
      (* Now mark this field in the record variable as initialized.  *)
      IF PutVarFieldInitialized (vsym, RightValue, lst)
      THEN
         IF Debugging
         THEN
            i := 1 ;
            n := NoOfItemsInList (lst) ;
            WHILE i <= n DO
               fsym := GetItemFromList (lst, i) ;
               printf1 (" %d", fsym) ;
               INC (i)
            END ;
            printf0 (" is initialized\n")
         END
      ELSIF Debugging
      THEN
         printf0 (" vsym is not a var\n")
      END ;
      KillList (lst)
   END
END SetVarComponentInitialized ;


(*
   GetVarComponentInitialized -
*)

PROCEDURE GetVarComponentInitialized (sym: CARDINAL; tok: CARDINAL) : BOOLEAN ;
VAR
   lvalue,
   init     : BOOLEAN ;
   component,
   vsym     : CARDINAL ;
   lst      : List ;
BEGIN
   component := ComponentFindVar (sym, lvalue, tok) ;
   IF IsItemInList (ignoreList, component) OR IsExempt (component)
   THEN
      RETURN TRUE
   ELSE
      init := FALSE ;
      vsym := deRefComponent (component, lvalue, sym, tok) ;
      IF vsym # NulSym
      THEN
         IF IsExempt (vsym)
         THEN
            init := TRUE
         ELSE
            (* Create list representing how the field is accessed.  *)
            lst := ComponentCreateFieldList (sym) ;
            (* Now obtain the mark indicating whether this field was initialized.  *)
            init := GetVarFieldInitialized (vsym, RightValue, lst) ;
            KillList (lst)
         END
      END ;
      RETURN init
   END
END GetVarComponentInitialized ;


(*
   Trace -
*)

PROCEDURE Trace (message: ARRAY OF CHAR; sym: CARDINAL) ;
BEGIN
   IF Debugging
   THEN
      printf1 (message, sym) ;
      printf0 ("\n")
   END
END Trace ;


(*
   SetVarInitialized - if the variable has a left mode and can be dereferenced
                       then set the left and right initialization state.
*)

PROCEDURE SetVarInitialized (sym: CARDINAL; canDereference: BOOLEAN;
                             tok: CARDINAL) ;
BEGIN
   IF IsVar (sym)
   THEN
      RemoveItemFromList (ignoreList, sym) ;
      IF IsComponent (sym)
      THEN
         Trace ("SetVarInitialized sym %d is a component and calling SetVarComponentInitialized", sym);
         SetVarComponentInitialized (sym, tok)
      ELSIF (GetMode (sym) = LeftValue) AND canDereference
      THEN
         Trace ("SetVarInitialized sym %d is LeftValue and canDeference and calling PutVarInitialized LeftValue and RightValue", sym);
         PutVarInitialized (sym, LeftValue) ;
         PutVarInitialized (sym, RightValue)
      ELSE
         Trace ("SetVarInitialized sym %d calling PutVarInitialized with its mode", sym);
         PutVarInitialized (sym, GetMode (sym))
      END ;
      IF Debugging
      THEN
         PrintSym (sym)
      END
   END
END SetVarInitialized ;


(*
   doGetVarInitialized -
*)

PROCEDURE doGetVarInitialized (sym: CARDINAL; tok: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsVar (sym)
   THEN
      IF IsUnbounded (GetSType (sym))
      THEN
         RETURN TRUE
      ELSIF IsComponent (sym)
      THEN
         RETURN GetVarComponentInitialized (sym, tok)
      END ;
      RETURN VarCheckReadInit (sym, GetMode (sym))
   END ;
   RETURN IsConst (sym) AND IsConstString (sym)
END doGetVarInitialized ;


(*
   GetVarInitialized -
*)

PROCEDURE GetVarInitialized (sym: CARDINAL; tok: CARDINAL) : BOOLEAN ;
VAR
   init: BOOLEAN ;
BEGIN
   init := doGetVarInitialized (sym, tok) ;
   IF Debugging
   THEN
      IF init
      THEN
         Trace ("GetVarInitialized (sym = %d) returning TRUE", sym)
      ELSE
         Trace ("GetVarInitialized (sym = %d) returning FALSE", sym)
      END
   END ;
   RETURN init
END GetVarInitialized ;


(*
   IsExempt - returns TRUE if sym is a global variable or a parameter or
              a variable with a variant record type.
*)

PROCEDURE IsExempt (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (sym # NulSym) AND IsVar (sym) AND
          (IsGlobalVar (sym) OR
           (* (IsVarAParam (sym) AND (GetMode (sym) = LeftValue)) OR *)
           ContainsVariant (sym) OR
           IsArray (GetSType (sym)) OR IsSet (GetSType (sym)) OR
           IsUnbounded (GetSType (sym)) OR IsVarArrayRef (sym) OR
           IsItemInList (ignoreList, sym))
END IsExempt ;


(*
   CheckBinary -
*)

PROCEDURE CheckBinary (op1tok, op1,
                       op2tok, op2,
                       op3tok, op3: CARDINAL; warning: BOOLEAN;
                       i: CARDINAL) ;
BEGIN
   CheckDeferredRecordAccess (op2tok, op2, FALSE, warning, i) ;
   CheckDeferredRecordAccess (op3tok, op3, FALSE, warning, i) ;
   SetVarInitialized (op1, FALSE, op1tok)
END CheckBinary ;


(*
   CheckUnary -
*)

PROCEDURE CheckUnary (lhstok, lhs,
                      rhstok, rhs: CARDINAL; warning: BOOLEAN;
                      i: CARDINAL) ;
BEGIN
   CheckDeferredRecordAccess (rhstok, rhs, FALSE, warning, i) ;
   SetVarInitialized (lhs, FALSE, lhstok)
END CheckUnary ;


(*
   CheckXIndr -
*)

PROCEDURE CheckXIndr (lhstok, lhs, type,
                      rhstok, rhs: CARDINAL; warning: BOOLEAN;
                      i: CARDINAL) ;
VAR
   lst    : List ;
   content: CARDINAL ;
BEGIN
   CheckDeferredRecordAccess (rhstok, rhs, FALSE, warning, i) ;
   CheckDeferredRecordAccess (lhstok, lhs, FALSE, warning, i) ;
   (* Now see if we know what lhs is pointing to and set fields if necessary.  *)
   content := getContent (getLAlias (lhs), lhs, lhstok) ;
   IF (content # NulSym) AND (content # lhs) AND (GetSType (content) = type)
   THEN
      IF IsReallyPointer (rhs)
      THEN
         SetupLAlias (content, rhs)
      END ;
      IF IsRecord (type)
      THEN
         (* Set all fields of content as initialized.  *)
         SetVarInitialized (content, FALSE, lhstok)
      ELSE
         (* Set only the field assigned in vsym as initialized.  *)
         lst := ComponentCreateFieldList (rhs) ;
         IF PutVarFieldInitialized (content, RightValue, lst)
         THEN
         END ;
         KillList (lst)
      END
   END
END CheckXIndr ;


(*
   CheckIndrX -
*)

PROCEDURE CheckIndrX (lhstok, lhs, rhstok, rhs: CARDINAL;
                      warning: BOOLEAN;
                      i: CARDINAL) ;
VAR
   content: CARDINAL ;
BEGIN
   CheckDeferredRecordAccess (rhstok, rhs, FALSE, warning, i) ;
   content := getContent (getLAlias (rhs), rhs, rhstok) ;
   IF content = NulSym
   THEN
      IncludeItemIntoList (ignoreList, lhs)
   ELSE
      CheckDeferredRecordAccess (rhstok, content, TRUE, warning, i) ;
      SetVarInitialized (lhs, VarCheckReadInit (content, RightValue), lhstok) ;
      IF IsReallyPointer (content)
      THEN
         SetupLAlias (lhs, content)
      END
   END
END CheckIndrX ;


(*
   CheckRecordField -
*)

PROCEDURE CheckRecordField (op1: CARDINAL) ;
BEGIN
   PutVarInitialized (op1, LeftValue)
END CheckRecordField ;


(*
   CheckLastForIterator -
*)

PROCEDURE CheckLastForIterator (op1tok: CARDINAL; op1: CARDINAL;
                                op2tok: CARDINAL; op2: CARDINAL;
                                warning: BOOLEAN; i: CARDINAL) ;
BEGIN
   SetVarInitialized (op1, FALSE, op1tok) ;
   Assert (IsTuple (op2)) ;
   CheckDeferredRecordAccess (op2tok, GetNth (op2, 1), FALSE, warning, i) ;
   CheckDeferredRecordAccess (op2tok, GetNth (op2, 2), FALSE, warning, i) ;
END CheckLastForIterator ;


(*
   CheckBecomes -
*)

PROCEDURE CheckBecomes (destok, des, exprtok, expr: CARDINAL;
                        warning: BOOLEAN; i: CARDINAL) ;
VAR
   lvalue: BOOLEAN ;
   lst   : List ;
   vsym  : CARDINAL ;
BEGIN
   CheckDeferredRecordAccess (exprtok, expr, FALSE, warning, i) ;
   SetupLAlias (des, expr) ;
   SetVarInitialized (des, FALSE, destok) ;
   (* Now see if we know what lhs is pointing to and set fields if necessary.  *)
   IF IsComponent (des)
   THEN
      vsym := ComponentFindVar (des, lvalue, destok) ;
      vsym := deRefComponent (vsym, lvalue, des, destok) ;
      IF vsym # NulSym
      THEN
         (* Set only the field assigned in vsym as initialized.  *)
         lst := ComponentCreateFieldList (des) ;
         IF PutVarFieldInitialized (vsym, RightValue, lst)
         THEN
         END ;
         KillList (lst)
      END
   END
END CheckBecomes ;


(*
   CheckComparison -
*)

PROCEDURE CheckComparison (op1tok, op1, op2tok, op2: CARDINAL;
                           warning: BOOLEAN; i: CARDINAL) ;
BEGIN
   CheckDeferredRecordAccess (op1tok, op1, FALSE, warning, i) ;
   CheckDeferredRecordAccess (op2tok, op2, FALSE, warning, i)
END CheckComparison ;


(*
   CheckAddr -
*)

PROCEDURE CheckAddr (ptrtok, ptr, contenttok, content: CARDINAL) ;
BEGIN
   SetVarInitialized (ptr, GetVarInitialized (content, contenttok), ptrtok) ;
   SetupIndr (ptr, content)
END CheckAddr ;


(*
   DefaultTokPos -
*)

PROCEDURE DefaultTokPos (preferredPos, defaultPos: CARDINAL) : CARDINAL ;
BEGIN
   IF preferredPos = UnknownTokenNo
   THEN
      RETURN defaultPos
   END ;
   RETURN preferredPos
END DefaultTokPos ;


(*
   stop -
*)

PROCEDURE stop ;
END stop ;


(*
   CheckReadBeforeInitQuad -
*)

PROCEDURE CheckReadBeforeInitQuad (procSym: CARDINAL; quad: CARDINAL;
                                   warning: BOOLEAN; i: CARDINAL) : BOOLEAN ;
VAR
   op                          : QuadOperator ;
   op1, op2, op3               : CARDINAL ;
   op1tok, op2tok, op3tok, qtok: CARDINAL ;
   constExpr, overflowChecking : BOOLEAN ;
BEGIN
   IF quad = 3140
   THEN
      stop
   END ;
   IF Debugging
   THEN
      printf1 ("CheckReadBeforeInitQuad (quad %d)\n", quad) ;
      DumpAliases ;
      ForeachLocalSymDo (procSym, PrintSym) ;
      printf0 ("***********************************\n")
   END ;
   GetQuadOtok (quad, qtok, op, op1, op2, op3,
                overflowChecking, constExpr,
                op1tok, op2tok, op3tok) ;
   op1tok := DefaultTokPos (op1tok, qtok) ;
   op2tok := DefaultTokPos (op2tok, qtok) ;
   op3tok := DefaultTokPos (op3tok, qtok) ;
   CASE op OF

   (* Jumps, calls and branches.  *)
   IfInOp,
   IfNotInOp,
   IfEquOp,
   IfNotEquOp,
   IfLessOp,
   IfLessEquOp,
   IfGreOp,
   IfGreEquOp        : CheckComparison (op1tok, op1, op2tok, op2, warning, i) |
   LastForIteratorOp : CheckLastForIterator (op1tok, op1, op2tok, op2,
                                             warning, i) ;
                       Assert (IsConst (op3)) |
   TryOp,
   ReturnOp,
   CallOp,
   KillLocalVarOp,
   RetryOp,
   GotoOp            : RETURN TRUE |   (* End of basic block.  *)

   (* Variable references.  *)

   InclOp,
   ExclOp            : CheckDeferredRecordAccess (op1tok, op1, FALSE, warning, i) ;
                       CheckDeferredRecordAccess (op1tok, op1, TRUE, warning, i) ;
                       CheckDeferredRecordAccess (op3tok, op3, FALSE, warning, i) |
   NegateOp          : CheckUnary (op1tok, op1, op3tok, op3, warning, i) |
   BecomesOp         : CheckBecomes (op1tok, op1, op3tok, op3, warning, i) |
   UnboundedOp,
   FunctValueOp,
   StandardFunctionOp,
   HighOp,
   SizeOp            : SetVarInitialized (op1, FALSE, op1tok) |
   AddrOp            : CheckAddr (op1tok, op1, op3tok, op3) |
   ReturnValueOp     : SetVarInitialized (op1, FALSE, op1tok) |
   NewLocalVarOp     : SetParameterVariablesInitialized (op3) |
   ParamOp           : CheckDeferredRecordAccess (op2tok, op2, FALSE, warning, i) ;
                       CheckDeferredRecordAccess (op3tok, op3, FALSE, warning, i) ;
                       IF (op1 > 0) AND (op1 <= NoOfParamAny (op2)) AND
                          IsVarParamAny (op2, op1)
                       THEN
                          SetVarInitialized (op3, TRUE, op3tok)
                       END |
   ArrayOp           : CheckDeferredRecordAccess (op3tok, op3, FALSE, warning, i) ;
                       SetVarInitialized (op1, TRUE, op1tok) |
   RecordFieldOp     : CheckRecordField (op1) |
   LogicalShiftOp,
   LogicalRotateOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   LogicalDiffOp,
   CoerceOp,
   ConvertOp,
   CastOp,
   AddOp,
   ArithAddOp,
   SubOp,
   MultOp,
   DivM2Op,
   ModM2Op,
   ModFloorOp,
   DivCeilOp,
   ModCeilOp,
   DivFloorOp,
   ModTruncOp,
   DivTruncOp        : CheckBinary (op1tok, op1, op2tok, op2, op3tok, op3, warning, i) |
   XIndrOp           : CheckXIndr (op1tok, op1, op2, op3tok, op3, warning, i) |
   IndrXOp           : CheckIndrX (op1tok, op1, op3tok, op3, warning, i) |
   SaveExceptionOp   : SetVarInitialized (op1, FALSE, op1tok) |
   RestoreExceptionOp: CheckDeferredRecordAccess (op1tok, op1, FALSE, warning, i) |

   SubrangeLowOp,
   SubrangeHighOp    : InternalError ('quadruples should have been resolved') |
   ElementSizeOp,
   BuiltinConstOp,  (* Nothing to do, it is assigning a constant to op1 (also a const).  *)
   BuiltinTypeInfoOp,  (* Likewise assigning op1 (const) with a type.  *)
   StringConvertCnulOp,
   StringConvertM2nulOp,
   StringLengthOp,
   ProcedureScopeOp,
   InitEndOp,
   InitStartOp,
   FinallyStartOp,
   FinallyEndOp,
   CatchBeginOp,
   CatchEndOp,
   ThrowOp,
   StartDefFileOp,
   StartModFileOp,
   EndFileOp,
   CodeOnOp,
   CodeOffOp,
   ProfileOnOp,
   ProfileOffOp,
   OptimizeOnOp,
   OptimizeOffOp,
   InlineOp,
   LineNumberOp,
   StatementNoteOp,
   SavePriorityOp,
   RestorePriorityOp,
   RangeCheckOp,
   ModuleScopeOp,
   ErrorOp,
   DummyOp,
   OptParamOp,
   InitAddressOp   : |

   END ;
   RETURN FALSE
END CheckReadBeforeInitQuad ;


(*
   SetParameterVariablesInitialized - sets all shadow variables for parameters as
                                      initialized.
*)

PROCEDURE SetParameterVariablesInitialized (procSym: CARDINAL) ;
BEGIN
   ForeachLocalSymDo (procSym, SetVarUninitialized) ;
   ForeachParamSymDo (procSym, SetVarLRInitialized) ;
END SetParameterVariablesInitialized ;


(*
   FilterCheckReadBeforeInitQuad -
*)

PROCEDURE FilterCheckReadBeforeInitQuad (procSym: CARDINAL; start: CARDINAL;
                                         warning: BOOLEAN;
                                         i: CARDINAL) : BOOLEAN ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3: CARDINAL ;
BEGIN
   GetQuad (start, Op, Op1, Op2, Op3) ;
   IF (Op # RangeCheckOp) AND (Op # StatementNoteOp)
   THEN
      RETURN CheckReadBeforeInitQuad (procSym, start, warning, i)
   END ;
   RETURN FALSE
END FilterCheckReadBeforeInitQuad ;


(*
   CheckReadBeforeInitFirstBasicBlock -
*)

PROCEDURE CheckReadBeforeInitFirstBasicBlock (procSym: CARDINAL;
                                              start, end: CARDINAL;
                                              warning: BOOLEAN;
                                              i: CARDINAL) ;
BEGIN
   LOOP
      IF FilterCheckReadBeforeInitQuad (procSym, start, warning, i)
      THEN
      END ;
      IF start = end
      THEN
         RETURN
      ELSE
         start := GetNextQuad (start)
      END
   END
END CheckReadBeforeInitFirstBasicBlock ;


(*
   bbArrayKill -
*)

PROCEDURE bbArrayKill ;
VAR
   i, h : CARDINAL ;
   bbPtr: bbEntry ;
BEGIN
   h := Indexing.HighIndice (bbArray) ;
   i := 1 ;
   WHILE i <= h DO
      bbPtr := Indexing.GetIndice (bbArray, i) ;
      bbPtr^.next := bbFreeList ;
      bbFreeList := bbPtr ;
      INC (i)
   END ;
   bbArray := Indexing.KillIndex (bbArray)
END bbArrayKill ;


(*
   DumpBBEntry -
*)

PROCEDURE DumpBBEntry (bbPtr: bbEntry; procSym: CARDINAL) ;
BEGIN
   printf4 ("bb %d: scope %d:  quads: %d .. %d",
            bbPtr^.indexBB, procSym, bbPtr^.start, bbPtr^.end) ;
   IF bbPtr^.first
   THEN
      printf0 (" first")
   END ;
   IF bbPtr^.endCall
   THEN
      printf0 (" endcall")
   END ;
   IF bbPtr^.endGoto
   THEN
      printf0 (" endgoto")
   END ;
   IF bbPtr^.endCond
   THEN
      printf0 (" endcond")
   END ;
   IF bbPtr^.topOfLoop
   THEN
      printf0 (" topofloop")
   END ;
   IF bbPtr^.condBB # 0
   THEN
      printf1 (" cond %d", bbPtr^.condBB)
   END ;
   IF bbPtr^.nextBB # 0
   THEN
      printf1 (" next %d", bbPtr^.nextBB)
   END ;
   printf0 ("\n")
END DumpBBEntry ;


(*
   DumpBBArray -
*)

PROCEDURE DumpBBArray (procSym: CARDINAL) ;
VAR
   bbPtr: bbEntry ;
   i, n : CARDINAL ;
BEGIN
   i := 1 ;
   n := Indexing.HighIndice (bbArray) ;
   WHILE i <= n DO
      bbPtr := Indexing.GetIndice (bbArray, i) ;
      DumpBBEntry (bbPtr, procSym) ;
      INC (i)
   END ;
   i := 1 ;
   WHILE i <= n DO
      bbPtr := Indexing.GetIndice (bbArray, i) ;
      printf4 ("bb %d: scope %d:  quads: %d .. %d\n",
               bbPtr^.indexBB, procSym, bbPtr^.start, bbPtr^.end) ;
      DisplayQuadRange (procSym, bbPtr^.start, bbPtr^.end) ;
      INC (i)
   END
END DumpBBArray ;


(*
   DumpBBSequence -
*)

PROCEDURE DumpBBSequence (lst: List) ;
VAR
   arrayindex,
   listindex, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList (lst) ;
   listindex := 1 ;
   printf0 ("=============\n");
   printf0 (" checking sequence:");
   WHILE listindex <= n DO
      arrayindex := GetItemFromList (lst, listindex) ;
      printf2 (" lst[%d] -> %d", listindex, arrayindex) ;
      INC (listindex)
   END ;
   printf0 ("\n")
END DumpBBSequence ;


(*
   trashParam -
*)

PROCEDURE trashParam (trashQuad: CARDINAL) ;
VAR
   op                            : QuadOperator ;
   op1, proc, param, paramValue  : CARDINAL ;
   op1tok, op2tok, paramtok, qtok: CARDINAL ;
   constExpr, overflowChecking   : BOOLEAN ;
   heapValue, ptrToHeap          : CARDINAL ;
BEGIN
   IF trashQuad # 0
   THEN
      GetQuadOtok (trashQuad, qtok, op, op1, proc, param,
                   overflowChecking, constExpr,
                   op1tok, op2tok, paramtok) ;
      heapValue := GetQuadTrash (trashQuad) ;
      IF Debugging
      THEN
         printf1 ("heapValue = %d\n", heapValue)
      END ;
      IF heapValue # NulSym
      THEN
         SetVarInitialized (param, FALSE, paramtok) ;
         paramValue := getLAlias (param) ;
         ptrToHeap := getContent (paramValue, param, paramtok) ;
         IF ptrToHeap # NulSym
         THEN
            IF IsDeallocate (proc)
            THEN
               SetupLAlias (ptrToHeap, Nil) ;
               SetVarInitialized (ptrToHeap, FALSE, paramtok)
            ELSE
               SetupIndr (ptrToHeap, heapValue) ;
               SetVarInitialized (ptrToHeap, TRUE, paramtok)
            END
         END
      END
   END ;
   DumpAliases
END trashParam ;


(*
   SetVarLRInitialized - this sets up an alias between the parameter
                         value and the pointer for the case:

                         procedure foo (var shadow: PtrToType) ;

                         which allows shadow to be statically analyzed
                         once it is re-assigned.
*)

PROCEDURE SetVarLRInitialized (param: CARDINAL) ;
VAR
   heap,
   shadow: CARDINAL ;
BEGIN
   Assert (IsParameter (param)) ;
   shadow := GetParameterShadowVar (param) ;
   IF shadow # NulSym
   THEN
      IncludeItemIntoList (ignoreList, shadow)
   END ;
   heap := GetParameterHeapVar (param) ;
   IF (shadow # NulSym) AND (heap # NulSym)
   THEN
      PutVarInitialized (shadow, GetMode (shadow)) ;
      PutVarInitialized (heap, GetMode (heap)) ;
      SetupIndr (shadow, heap) ;
      IncludeItemIntoList (ignoreList, heap)
   END
END SetVarLRInitialized ;


(*
   TestBBSequence -
*)

PROCEDURE TestBBSequence (procSym: CARDINAL; lst: List) ;
VAR
   bbPtr  : bbEntry ;
   bbi,
   i, n   : CARDINAL ;
   warning: BOOLEAN ;  (* Should we issue a warning rather than a note?  *)
BEGIN
   IF Debugging
   THEN
      DumpBBSequence (lst)
   END ;
   initBlock ;
   ForeachLocalSymDo (procSym, SetVarUninitialized) ;
   ForeachParamSymDo (procSym, SetVarLRInitialized) ;
   n := NoOfItemsInList (lst) ;
   i := 1 ;
   warning := TRUE ;
   WHILE i <= n DO
      bbi := GetItemFromList (lst, i) ;
      bbPtr := Indexing.GetIndice (bbArray, bbi) ;
      CheckReadBeforeInitFirstBasicBlock (procSym,
                                          bbPtr^.start, bbPtr^.end,
                                          warning, i) ;
      IF bbPtr^.endCond
      THEN
         (* Check to see if we are moving into an conditional block in which case
            we will issue a note.  *)
         warning := FALSE
      ELSIF bbPtr^.endCall AND (bbPtr^.trashQuad # 0)
      THEN
         trashParam (bbPtr^.trashQuad)
      END ;
      INC (i)
   END ;
   killBlock
END TestBBSequence ;


(*
   CreateBBPermultations -
*)

PROCEDURE CreateBBPermultations (procSym: CARDINAL; i: CARDINAL; lst: List) ;
VAR
   duplst: List ;
   iPtr  : bbEntry ;
BEGIN
   IF i = 0
   THEN
      TestBBSequence (procSym, lst)
   ELSE
      iPtr := Indexing.GetIndice (bbArray, i) ;
      IF iPtr^.topOfLoop
      THEN
         TestBBSequence (procSym, lst)
      ELSE
         duplst := DuplicateList (lst) ;
         IncludeItemIntoList (duplst, i) ;
         IF iPtr^.endCall AND (iPtr^.trashQuad = 0)
         THEN
            TestBBSequence (procSym, duplst)
         ELSIF iPtr^.endGoto
         THEN
            CreateBBPermultations (procSym, iPtr^.nextBB, duplst)
         ELSIF UninitVariableConditionalChecking AND iPtr^.endCond
         THEN
            CreateBBPermultations (procSym, iPtr^.nextBB, duplst) ;
            CreateBBPermultations (procSym, iPtr^.condBB, duplst)
         ELSIF iPtr^.endCond
         THEN
            TestBBSequence (procSym, duplst)
         ELSE
            (* Fall through.  *)
            CreateBBPermultations (procSym, iPtr^.nextBB, duplst)
         END ;
         KillList (duplst)
      END
   END
END CreateBBPermultations ;


(*
   ScopeBlockVariableAnalysis - checks to see whether a variable is
                                read before it has been initialized.
*)

PROCEDURE ScopeBlockVariableAnalysis (Scope: CARDINAL;
                                      Start, End: CARDINAL) ;
VAR
   bb : BasicBlock ;
   lst: List ;
BEGIN
   IF UninitVariableChecking
   THEN
      bbArray := Indexing.InitIndex (1) ;
      bb := InitBasicBlocksFromRange (Scope, Start, End) ;
      ForeachBasicBlockDo (bb, AppendEntry) ;
      KillBasicBlocks (bb) ;
      GenerateCFG ;
      IF Scope # NulSym
      THEN
         InitList (lst) ;
         IF Debugging
         THEN
            DumpBBArray (Scope) ;
            IF UninitVariableConditionalChecking
            THEN
               printf0 ("UninitVariableConditionalChecking is TRUE\n")
            END
         END ;
         CreateBBPermultations (Scope, 1, lst) ;
         KillList (lst)
      END ;
      bbArrayKill
   END
END ScopeBlockVariableAnalysis ;


(*
   GetOp3 -
*)

PROCEDURE GetOp3 (quad: CARDINAL) : CARDINAL ;
VAR
   op: QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad (quad, op, op1, op2, op3) ;
   RETURN op3
END GetOp3 ;


(*
   getBBindex - return the basic block index which starts with quad.
*)

PROCEDURE getBBindex (quad: CARDINAL) : CARDINAL ;
VAR
   iPtr   : bbEntry ;
   i, high: CARDINAL ;
BEGIN
   i := 1 ;
   high := Indexing.HighIndice (bbArray) ;
   WHILE i <= high DO
      iPtr := Indexing.GetIndice (bbArray, i) ;
      IF iPtr^.start = quad
      THEN
         RETURN iPtr^.indexBB
      END ;
      INC (i)
   END ;
   RETURN 0
END getBBindex ;


(*
   GenerateCFG -
*)

PROCEDURE GenerateCFG ;
VAR
   iPtr   : bbEntry ;
   next,
   i, high: CARDINAL ;
BEGIN
   i := 1 ;
   high := Indexing.HighIndice (bbArray) ;
   WHILE i <= high DO
      iPtr := Indexing.GetIndice (bbArray, i) ;
      IF IsKillLocalVar (iPtr^.end) OR IsReturn (iPtr^.end)
      THEN
         (* Nothing to do as we have reached the end of this scope.  *)
      ELSE
         next := GetNextQuad (iPtr^.end) ;
         iPtr^.nextQuad := next ;
         iPtr^.nextBB := getBBindex (next) ;
         IF iPtr^.endCond
         THEN
            iPtr^.condQuad := GetOp3 (iPtr^.end) ;
            iPtr^.condBB := getBBindex (iPtr^.condQuad)
         END
      END ;
      INC (i)
   END
END GenerateCFG ;


(*
   NewEntry -
*)

PROCEDURE NewEntry () : bbEntry ;
VAR
   bbPtr: bbEntry ;
BEGIN
   IF bbFreeList = NIL
   THEN
      NEW (bbPtr)
   ELSE
      bbPtr := bbFreeList ;
      bbFreeList := bbFreeList^.next
   END ;
   RETURN bbPtr
END NewEntry ;


(*
   IsAllocate - return TRUE is sym is ALLOCATE.
*)

PROCEDURE IsAllocate (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsProcedure (sym) AND (GetSymName (sym) = MakeKey('ALLOCATE'))
END IsAllocate ;


(*
   IsDeallocate - return TRUE is sym is DEALLOCATE.
*)

PROCEDURE IsDeallocate (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsProcedure (sym) AND (GetSymName (sym) = MakeKey('DEALLOCATE'))
END IsDeallocate ;


(*
   DetectTrash -
*)

PROCEDURE DetectTrash (bbPtr: bbEntry) ;
VAR
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   IF bbPtr^.endCall
   THEN
      i := bbPtr^.start ;
      LOOP
         GetQuad (i, op, op1, op2, op3) ;
         IF (op = ParamOp) AND (op1 = 1) AND (IsAllocate (op2) OR IsDeallocate (op2))
         THEN
            bbPtr^.trashQuad := i
         END ;
         IF i = bbPtr^.end
         THEN
            RETURN
         END ;
         i := GetNextQuad (i)
      END
   END
END DetectTrash ;


(*
   AppendEntry -
*)

PROCEDURE AppendEntry (bb: BasicBlock) ;
VAR
   bbPtr: bbEntry ;
   high : CARDINAL ;
BEGIN
   high := Indexing.HighIndice (bbArray) ;
   bbPtr := NewEntry () ;
   WITH bbPtr^ DO
      start := GetBasicBlockStart (bb) ;
      end := GetBasicBlockEnd (bb) ;
      first := high = 0 ;
      endCall := IsCall (end) ;
      endGoto := IsGoto (end) ;
      endCond := IsConditional (end) ;
      topOfLoop := IsBackReference (start) ;
      trashQuad := 0 ;
      indexBB := high + 1 ;
      nextQuad := 0 ;
      condQuad := 0 ;
      nextBB := 0 ;
      condBB := 0 ;
      next := NIL
   END ;
   DetectTrash (bbPtr) ;
   Indexing.PutIndice (bbArray, high + 1, bbPtr)
END AppendEntry ;


(*
   DumpAlias -
*)

PROCEDURE DumpAlias (array: Index; aliasIndex: CARDINAL) ;
VAR
   sa: symAlias ;
BEGIN
   sa := Indexing.GetIndice (array, aliasIndex) ;
   printf2 ("keySym = %d: alias = %d\n", sa^.keySym, sa^.alias)
END DumpAlias ;


(*
   doDumpAliases -
*)

PROCEDURE doDumpAliases (array: Index) ;
VAR
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := Indexing.HighIndice (array) ;
   WHILE i <= n DO
      DumpAlias (array, i) ;
      INC (i)
   END
END doDumpAliases ;


(*
   DumpAliases -
*)

PROCEDURE DumpAliases ;
BEGIN
   IF Debugging
   THEN
      printf0 ("LArray\n") ;
      doDumpAliases (LArray) ;
      printf0 ("IndirectArray\n") ;
      doDumpAliases (IndirectArray)
   END
END DumpAliases ;


(*
   newAlias -
*)

PROCEDURE newAlias () : symAlias ;
VAR
   sa: symAlias ;
BEGIN
   IF freeList = NIL
   THEN
      NEW (sa)
   ELSE
      sa := freeList ;
      freeList := freeList^.next
   END ;
   RETURN sa
END newAlias ;


(*
   initAlias -
*)

PROCEDURE initAlias (sym: CARDINAL) : symAlias ;
VAR
   sa: symAlias ;
BEGIN
   sa := newAlias () ;
   WITH sa^ DO
      keySym := sym ;
      alias := NulSym ;
      next := NIL
   END ;
   RETURN sa
END initAlias ;


(*
   killAlias -
*)

PROCEDURE killAlias (sa: symAlias) ;
BEGIN
   sa^.next := freeList ;
   freeList := sa
END killAlias ;


(*
   initBlock -
*)

PROCEDURE initBlock ;
BEGIN
   LArray := Indexing.InitIndex (1) ;
   IndirectArray := Indexing.InitIndex (1) ;
   InitList (ignoreList)
END initBlock ;


(*
   killBlock -
*)

PROCEDURE killBlock ;
BEGIN
   doKillBlock (LArray) ;
   doKillBlock (IndirectArray) ;
   KillList (ignoreList)
END killBlock ;


PROCEDURE doKillBlock (VAR array: Index) ;
VAR
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := Indexing.HighIndice (array) ;
   WHILE i <= n DO
      killAlias (Indexing.GetIndice (array, i)) ;
      INC (i)
   END ;
   array := Indexing.KillIndex (array)
END doKillBlock ;


(*
   addAlias -
*)

PROCEDURE addAlias (array: Index; sym: CARDINAL; aliased: CARDINAL) ;
VAR
   i, n: CARDINAL ;
   sa  : symAlias ;
BEGIN
   i := 1 ;
   n := Indexing.HighIndice (array) ;
   WHILE i <= n DO
      sa := Indexing.GetIndice (array, i) ;
      IF sa^.keySym = sym
      THEN
         sa^.alias := aliased ;
         RETURN
      END ;
      INC (i)
   END ;
   sa := initAlias (sym) ;
   Indexing.IncludeIndiceIntoIndex (array, sa) ;
   sa^.alias := aliased
END addAlias ;


(*
   lookupAlias -
*)

PROCEDURE lookupAlias (array: Index; sym: CARDINAL) : symAlias ;
VAR
   i, n: CARDINAL ;
   sa  : symAlias ;
BEGIN
   i := 1 ;
   n := Indexing.HighIndice (array) ;
   WHILE i <= n DO
      sa := Indexing.GetIndice (array, i) ;
      IF sa^.keySym = sym
      THEN
         RETURN sa
      END ;
      INC (i)
   END ;
   RETURN NIL
END lookupAlias ;


(*
   doGetAlias -
*)

PROCEDURE doGetAlias (array: Index; sym: CARDINAL) : CARDINAL ;
VAR
   sa: symAlias ;
BEGIN
   sa := lookupAlias (array, sym) ;
   IF (sa # NIL) AND (sa^.alias # NulSym)
   THEN
      RETURN sa^.alias
   END ;
   RETURN NulSym
END doGetAlias ;


(*
   getLAlias - attempts to looks up an alias which is not a temporary variable.
*)

PROCEDURE getLAlias (sym: CARDINAL) : CARDINAL ;
VAR
   type,
   nsym: CARDINAL ;
BEGIN
   nsym := sym ;
   REPEAT
      sym := nsym ;
      type := GetSType (sym) ;
      IF (IsTemporary (sym) AND (GetMode (sym) = LeftValue)) OR
         ((type # NulSym) AND IsReallyPointer (type))
      THEN
         nsym := doGetAlias (LArray, sym)
      ELSE
         RETURN sym
      END
   UNTIL nsym = NulSym ;
   RETURN sym
END getLAlias ;


(*
   SetupLAlias -
*)

PROCEDURE SetupLAlias (des, exp: CARDINAL) ;
BEGIN
   IF (exp = Nil) OR
      (IsVar (exp) AND
       ((GetMode (des) = LeftValue) OR IsReallyPointer (GetSType (des))))
   THEN
      addAlias (LArray, des, exp) ;
      DumpAliases
   END
END SetupLAlias ;


(*
   SetupIndr -
*)

PROCEDURE SetupIndr (ptr, content: CARDINAL) ;
BEGIN
   addAlias (IndirectArray, ptr, content) ;
END SetupIndr ;


(*
   getContent - attempts to return the content pointed to by ptr.
                sym is the original symbol and ptr will be the equivalent lvalue.
*)

PROCEDURE getContent (ptr: CARDINAL; sym: CARDINAL; tok: CARDINAL) : CARDINAL ;
BEGIN
   IF ptr = Nil
   THEN
      MetaErrorT1 (tok,
                   "attempting to dereference {%1Wad} which will be a {%kNIL} pointer",
                   sym) ;
      RETURN NulSym
   ELSE
      RETURN doGetAlias (IndirectArray, ptr)
   END
END getContent ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   freeList := NIL ;
   bbFreeList := NIL ;
   InitList (errorList)
END init ;


BEGIN
   init
END M2SymInit.
