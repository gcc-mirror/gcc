(* M2SymInit.mod records initialization state for variables.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
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
FROM libc IMPORT printf ;
FROM NameKey IMPORT Name, NulName, KeyToCharStar ;
FROM M2Options IMPORT UninitVariableChecking ;
FROM M2MetaError IMPORT MetaErrorT1 ;
FROM M2LexBuf IMPORT UnknownTokenNo ;

IMPORT Indexing ;

FROM Lists IMPORT List, InitList, GetItemFromList, PutItemIntoList,
                  IsItemInList, IncludeItemIntoList, NoOfItemsInList,
                  RemoveItemFromList, ForeachItemInListDo, KillList ;

FROM SymbolTable IMPORT NulSym, ModeOfAddr, IsVar, IsRecord, GetSType,
                        GetNth, IsRecordField, IsSet, IsArray, IsProcedure,
                        GetVarScope, IsVarAParam, IsComponent, GetMode,
                        VarCheckReadInit, VarInitState, PutVarInitialized,
                        PutVarFieldInitialized, GetVarFieldInitialized,
                        IsConst, IsConstString, NoOfParam, IsVarParam,
                        ForeachLocalSymDo, IsTemporary, ModeOfAddr,
                        IsReallyPointer, IsUnbounded,
                        IsVarient, IsFieldVarient, GetVarient ;

FROM M2Quads IMPORT QuadOperator, GetQuadOtok, GetQuad, GetNextQuad ;
FROM M2Options IMPORT CompilerDebugging ;
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

VAR
   aliasArray: Indexing.Index ;
   freeList  : symAlias ;


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
   init : BOOLEAN ;
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
         init := SetFieldInitializedNo (fdesc, fieldlist, level + 1) ;
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
*)

PROCEDURE IsLocalVar (procsym, varsym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsVar (varsym) AND (GetVarScope (varsym) = procsym)
END IsLocalVar ;


(*
   RecordFieldContainsVarient -
*)

PROCEDURE RecordFieldContainsVarient (sym: CARDINAL) : BOOLEAN ;
BEGIN
   Assert (IsRecordField (sym)) ;
   IF ContainsVariant (GetSType (sym))
   THEN
      RETURN TRUE
   END ;
   RETURN GetVarient (sym) # NulSym
END RecordFieldContainsVarient ;


(*
   ContainsVariant - returns TRUE if type sym contains a variant record.
*)

PROCEDURE ContainsVariant (sym: CARDINAL) : BOOLEAN ;
VAR
   i,
   fieldsym,
   fieldtype: CARDINAL ;
BEGIN
   IF IsRecord (sym)
   THEN
      i := 1 ;
      REPEAT
         fieldsym := GetNth (sym, i) ;
         IF fieldsym # NulSym
         THEN
            IF IsRecordField (fieldsym)
            THEN
               IF RecordFieldContainsVarient (fieldsym)
               THEN
                  RETURN TRUE
               END
            ELSIF IsVarient (fieldsym)
            THEN
               RETURN TRUE
            END ;
            INC (i)
         END
      UNTIL fieldsym = NulSym
   END ;
   RETURN FALSE
END ContainsVariant ;


(*
   CheckDeferredRecordAccess -
*)

PROCEDURE CheckDeferredRecordAccess (procsym: CARDINAL; tok: CARDINAL;
                                     sym: CARDINAL; canDereference: BOOLEAN) ;
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
         SetVarInitialized (sym, TRUE)
      ELSIF IsComponent (sym)
      THEN
         Trace ("checkReadInit IsComponent (%d) is true)", sym) ;
         IF NOT GetVarComponentInitialized (sym)
         THEN
            MetaErrorT1 (tok,
                         'attempting to access {%1Wad} before it has been initialized',
                         sym)
         END
      ELSIF (GetMode (sym) = LeftValue) AND canDereference
      THEN
         Trace ("checkReadInit GetMode (%d) = LeftValue and canDereference (LeftValue and RightValue VarCheckReadInit)", sym) ;
         IF NOT VarCheckReadInit (sym, LeftValue)
         THEN
            MetaErrorT1 (tok,
                         'attempting to access the address of {%1Wad} before it has been initialized',
                         sym)
         END ;
         IF NOT VarCheckReadInit (sym, RightValue)
         THEN
            MetaErrorT1 (tok,
                         'attempting to access {%1Wad} before it has been initialized', sym)
         END
      ELSE
         Trace ("checkReadInit call VarCheckReadInit using GetMode (%d)", sym) ;
         IF NOT VarCheckReadInit (sym, GetMode (sym))
         THEN
            MetaErrorT1 (tok,
                         'attempting to access {%1Wad} before it has been initialized', sym)
         END
      END
   END
END CheckDeferredRecordAccess ;


(*
   SetVarUninitialized - resets variable init state.
*)

PROCEDURE SetVarUninitialized (sym: CARDINAL) ;
BEGIN
   IF IsVar (sym) AND (NOT IsUnbounded (GetSType (sym))) AND (NOT IsVarAParam (sym))
   THEN
      VarInitState (sym)
   END
END SetVarUninitialized ;


(*
   ComponentFindVar -
*)

PROCEDURE ComponentFindVar (sym: CARDINAL) : CARDINAL ;
VAR
   nsym,
   i   : CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      nsym := getAlias (GetNth (sym, i)) ;
      IF (nsym # NulSym) AND IsVar (nsym)
      THEN
         IF (nsym # sym) AND IsComponent (nsym)
         THEN
            RETURN ComponentFindVar (nsym)
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
   SetVarComponentInitialized -
*)

PROCEDURE SetVarComponentInitialized (sym: CARDINAL) ;
VAR
   i, n,
   fsym,
   vsym: CARDINAL ;
   lst : List ;
BEGIN
   vsym := ComponentFindVar (sym) ;
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

PROCEDURE GetVarComponentInitialized (sym: CARDINAL) : BOOLEAN ;
VAR
   init: BOOLEAN ;
   vsym: CARDINAL ;
   lst : List ;
BEGIN
   init := FALSE ;
   vsym := ComponentFindVar (sym) ;
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

PROCEDURE SetVarInitialized (sym: CARDINAL; canDereference: BOOLEAN) ;
BEGIN
   IF IsVar (sym)
   THEN
      IF IsComponent (sym)
      THEN
         Trace ("SetVarInitialized sym %d is a component and calling SetVarComponentInitialized", sym);
         SetVarComponentInitialized (sym)
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

PROCEDURE doGetVarInitialized (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsVar (sym)
   THEN
      IF IsUnbounded (GetSType (sym))
      THEN
         RETURN TRUE
      ELSIF IsComponent (sym)
      THEN
         RETURN GetVarComponentInitialized (sym)
      END ;
      RETURN VarCheckReadInit (sym, GetMode (sym))
   END ;
   RETURN IsConst (sym) AND IsConstString (sym)
END doGetVarInitialized ;


(*
   GetVarInitialized -
*)

PROCEDURE GetVarInitialized (sym: CARDINAL) : BOOLEAN ;
VAR
   init: BOOLEAN ;
BEGIN
   init := doGetVarInitialized (sym) ;
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
          (IsGlobalVar (sym) OR IsVarAParam (sym) OR
           ContainsVariant (GetSType (sym)) OR
           IsArray (GetSType (sym)) OR IsSet (GetSType (sym)) OR
           IsUnbounded (GetSType (sym)))
END IsExempt ;


(*
   CheckBinary -
*)

PROCEDURE CheckBinary (procSym,
                       op1tok, op1,
                       op2tok, op2,
                       op3tok, op3: CARDINAL) ;
BEGIN
   CheckDeferredRecordAccess (procSym, op2tok, op2, FALSE) ;
   CheckDeferredRecordAccess (procSym, op3tok, op3, FALSE) ;
   SetVarInitialized (op1, FALSE)
END CheckBinary ;


(*
   CheckUnary -
*)

PROCEDURE CheckUnary (procSym,
                      lhstok, lhs,
                      rhstok, rhs: CARDINAL) ;
BEGIN
   CheckDeferredRecordAccess (procSym, rhstok, rhs, FALSE) ;
   SetVarInitialized (lhs, FALSE)
END CheckUnary ;


(*
   CheckXIndr -
*)

PROCEDURE CheckXIndr (procSym, lhstok, lhs, type, rhstok, rhs: CARDINAL) ;
VAR
   lst : List ;
   vsym: CARDINAL ;
BEGIN
   CheckDeferredRecordAccess (procSym, rhstok, rhs, FALSE) ;
   CheckDeferredRecordAccess (procSym, lhstok, lhs, FALSE) ;
   (* Now see if we know what lhs is pointing to and set fields if necessary.  *)
   vsym := getAlias (lhs) ;
   IF (vsym # lhs) AND (GetSType (vsym) = type)
   THEN
      IF IsRecord (type)
      THEN
         (* Set all fields of vsym as initialized.  *)
         SetVarInitialized (vsym, FALSE)
      ELSE
         (* Set only the field assigned in vsym as initialized.  *)
         lst := ComponentCreateFieldList (rhs) ;
         IF PutVarFieldInitialized (vsym, RightValue, lst)
         THEN
         END ;
         KillList (lst)
      END
   END
END CheckXIndr ;


(*
   CheckIndrX -
*)

PROCEDURE CheckIndrX (procSym, lhstok, lhs, type, rhstok, rhs: CARDINAL) ;
BEGIN
   CheckDeferredRecordAccess (procSym, rhstok, rhs, FALSE) ;
   CheckDeferredRecordAccess (procSym, rhstok, rhs, TRUE) ;
   SetVarInitialized (lhs, FALSE)
END CheckIndrX ;


(*
   CheckRecordField -
*)

PROCEDURE CheckRecordField (procSym, op1tok, op1, op2tok, op2: CARDINAL) ;
BEGIN
   PutVarInitialized (op1, LeftValue)
END CheckRecordField ;


(*
   CheckBecomes -
*)

PROCEDURE CheckBecomes (procSym, destok, des, exprtok, expr: CARDINAL) ;
VAR
   lst : List ;
   vsym: CARDINAL ;
BEGIN
   CheckDeferredRecordAccess (procSym, exprtok, expr, FALSE) ;
   SetupAlias (des, expr) ;
   SetVarInitialized (des, FALSE) ;
   (* Now see if we know what lhs is pointing to and set fields if necessary.  *)
   IF IsComponent (des)
   THEN
      vsym := ComponentFindVar (des) ;
      (* Set only the field assigned in vsym as initialized.  *)
      lst := ComponentCreateFieldList (des) ;
      IF PutVarFieldInitialized (vsym, RightValue, lst)
      THEN
      END ;
      KillList (lst)
   END
END CheckBecomes ;


(*
   CheckComparison -
*)

PROCEDURE CheckComparison (procSym, op1tok, op1, op2tok, op2: CARDINAL) ;
BEGIN
   CheckDeferredRecordAccess (procSym, op1tok, op1, FALSE) ;
   CheckDeferredRecordAccess (procSym, op2tok, op2, FALSE)
END CheckComparison ;


(*
   CheckAddr -
*)

PROCEDURE CheckAddr (procSym, op1tok, op1, op3tok, op3: CARDINAL) ;
BEGIN
   SetVarInitialized (op1, GetVarInitialized (op3)) ;
   SetupAlias (op1, op3)
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

PROCEDURE CheckReadBeforeInitQuad (procSym: CARDINAL; quad: CARDINAL) : BOOLEAN ;
VAR
   op                          : QuadOperator ;
   op1, op2, op3               : CARDINAL ;
   op1tok, op2tok, op3tok, qtok: CARDINAL ;
   overflowChecking            : BOOLEAN ;
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
   GetQuadOtok (quad, qtok, op, op1, op2, op3, overflowChecking,
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
   IfGreEquOp        : CheckComparison (procSym, op1tok, op1, op2tok, op2) |
   TryOp,
   ReturnOp,
   CallOp,
   KillLocalVarOp,
   RetryOp,
   GotoOp            : RETURN TRUE |   (* End of basic block.  *)

   (* Variable references.  *)

   InclOp,
   ExclOp            : CheckDeferredRecordAccess (procSym, op1tok, op1, FALSE) ;
                       CheckDeferredRecordAccess (procSym, op1tok, op1, TRUE) ;
                       CheckDeferredRecordAccess (procSym, op3tok, op3, FALSE) |
   NegateOp          : CheckUnary (procSym, op1tok, op1, op3tok, op3) |
   BecomesOp         : CheckBecomes (procSym, op1tok, op1, op3tok, op3) |
   UnboundedOp,
   FunctValueOp,
   HighOp,
   SizeOp            : SetVarInitialized (op1, FALSE) |
   AddrOp            : CheckAddr (procSym, op1tok, op1, op3tok, op3) |
   ReturnValueOp     : SetVarInitialized (op1, FALSE) |
   NewLocalVarOp     : |
   ParamOp           : CheckDeferredRecordAccess (procSym, op2tok, op2, FALSE) ;
                       CheckDeferredRecordAccess (procSym, op3tok, op3, FALSE) ;
                       IF (op1 > 0) AND (op1 <= NoOfParam (op2)) AND
                          IsVarParam (op2, op1)
                       THEN
                          SetVarInitialized (op3, TRUE)
                       END |
   ArrayOp           : CheckDeferredRecordAccess (procSym, op3tok, op3, FALSE) ;
                       SetVarInitialized (op1, TRUE) |
   RecordFieldOp     : CheckRecordField (procSym, op1tok, op1, op2tok, op2) |
   LogicalShiftOp,
   LogicalRotateOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
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
   DivTruncOp        : CheckBinary (procSym,
                                    op1tok, op1, op2tok, op2, op3tok, op3) |
   XIndrOp           : CheckXIndr (procSym, op1tok, op1, op2, op3tok, op3) |
   IndrXOp           : CheckIndrX (procSym, op1tok, op1, op2, op3tok, op3) |
   RangeCheckOp      : |
   SaveExceptionOp   : SetVarInitialized (op1, FALSE) |
   RestoreExceptionOp: CheckDeferredRecordAccess (procSym, op1tok, op1, FALSE)

   ELSE
   END ;
   RETURN FALSE
END CheckReadBeforeInitQuad ;


(*
   FilterCheckReadBeforeInitQuad -
*)

PROCEDURE FilterCheckReadBeforeInitQuad (procSym: CARDINAL; start: CARDINAL) : BOOLEAN ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3: CARDINAL ;
BEGIN
   GetQuad (start, Op, Op1, Op2, Op3) ;
   IF (Op # RangeCheckOp) AND (Op # StatementNoteOp)
   THEN
      RETURN CheckReadBeforeInitQuad (procSym, start)
   END ;
   RETURN FALSE
END FilterCheckReadBeforeInitQuad ;


(*
   CheckReadBeforeInitFirstBasicBlock -
*)

PROCEDURE CheckReadBeforeInitFirstBasicBlock (procSym: CARDINAL;
                                              start, end: CARDINAL) ;
BEGIN
   ForeachLocalSymDo (procSym, SetVarUninitialized) ;
   LOOP
      IF FilterCheckReadBeforeInitQuad (procSym, start) OR (start = end)
      THEN
         RETURN
      END ;
      start := GetNextQuad (start)
   END
END CheckReadBeforeInitFirstBasicBlock ;


(*
   VariableAnalysis - checks to see whether a variable is:

                      read before it has been initialized
*)

PROCEDURE VariableAnalysis (Start, End: CARDINAL) ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3: CARDINAL ;
BEGIN
   IF UninitVariableChecking
   THEN
      GetQuad (Start, Op, Op1, Op2, Op3) ;
      CASE Op OF

      NewLocalVarOp:  initBlock ;
                      CheckReadBeforeInitFirstBasicBlock (Op3, Start, End) ;
                      killBlock

      ELSE
      END
   END
END VariableAnalysis ;


(*
   DumpAlias -
*)

PROCEDURE DumpAlias (aliasIndex: CARDINAL) ;
VAR
   sa: symAlias ;
BEGIN
   sa := Indexing.GetIndice (aliasArray, aliasIndex) ;
   printf2 ("keySym = %d: alias = %d\n", sa^.keySym, sa^.alias) ;
END DumpAlias ;


(*
   DumpAliases -
*)

PROCEDURE DumpAliases ;
VAR
   i, n: CARDINAL ;
BEGIN
   IF Debugging
   THEN
      i := 1 ;
      n := Indexing.HighIndice (aliasArray) ;
      WHILE i <= n DO
         DumpAlias (i) ;
         INC (i)
      END
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
   aliasArray := Indexing.InitIndex (1) ;
END initBlock ;


(*
   killBlock -
*)

PROCEDURE killBlock ;
VAR
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := Indexing.HighIndice (aliasArray) ;
   WHILE i <= n DO
      killAlias (Indexing.GetIndice (aliasArray, i)) ;
      INC (i)
   END ;
   aliasArray := Indexing.KillIndex (aliasArray)
END killBlock ;


(*
   addAlias -
*)

PROCEDURE addAlias (sym: CARDINAL; aliased: CARDINAL) ;
VAR
   i, n: CARDINAL ;
   sa  : symAlias ;
BEGIN
   i := 1 ;
   n := Indexing.HighIndice (aliasArray) ;
   WHILE i <= n DO
      sa := Indexing.GetIndice (aliasArray, i) ;
      IF sa^.keySym = sym
      THEN
         sa^.alias := aliased ;
         RETURN
      END ;
      INC (i)
   END ;
   sa := initAlias (sym) ;
   Indexing.IncludeIndiceIntoIndex (aliasArray, sa) ;
   sa^.alias := aliased
END addAlias ;


(*
   lookupAlias -
*)

PROCEDURE lookupAlias (sym: CARDINAL) : symAlias ;
VAR
   i, n: CARDINAL ;
   sa  : symAlias ;
BEGIN
   i := 1 ;
   n := Indexing.HighIndice (aliasArray) ;
   WHILE i <= n DO
      sa := Indexing.GetIndice (aliasArray, i) ;
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

PROCEDURE doGetAlias (sym: CARDINAL) : CARDINAL ;
VAR
   sa: symAlias ;
BEGIN
   sa := lookupAlias (sym) ;
   IF (sa # NIL) AND (sa^.alias # NulSym)
   THEN
      RETURN sa^.alias
   END ;
   RETURN NulSym
END doGetAlias ;


(*
   getAlias - attempts to looks up an alias which is not a temporary variable.
*)

PROCEDURE getAlias (sym: CARDINAL) : CARDINAL ;
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
         nsym := doGetAlias (sym)
      ELSE
         RETURN sym
      END
   UNTIL nsym = NulSym ;
   RETURN sym
END getAlias ;


(*
   SetupAlias -
*)

PROCEDURE SetupAlias (des, exp: CARDINAL) ;
BEGIN
   IF IsVar (exp) AND
      ((GetMode (des) = LeftValue) OR IsReallyPointer (GetSType (des)))
   THEN
      addAlias (des, exp) ;
      DumpAliases
   END
END SetupAlias ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   freeList := NIL
END init ;


BEGIN
   init
END M2SymInit.
