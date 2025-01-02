(* M2Check.mod perform rigerous type checking for fully declared symbols.

Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Check ;

(*
    Title      : M2Check
    Author     : Gaius Mulley
    System     : GNU Modula-2
    Date       : Fri Mar  6 15:32:10 2020
    Revision   : $Version$
    Description: provides a module to check the symbol type compatibility.
                 It assumes that the declaration of all dependants
                 is complete.
*)

FROM M2System IMPORT IsSystemType, IsGenericSystemType, IsSameSize, IsComplexN ;
FROM M2Base IMPORT IsParameterCompatible, IsAssignmentCompatible, IsExpressionCompatible, IsComparisonCompatible, IsBaseType, IsMathType, ZType, CType, RType, IsComplexType, Char ;
FROM Indexing IMPORT Index, InitIndex, GetIndice, PutIndice, KillIndex, HighIndice, LowIndice, IncludeIndiceIntoIndex, ForeachIndiceInIndexDo ;
FROM M2Error IMPORT Error, InternalError, NewError, ErrorString, ChainError ;
FROM M2MetaError IMPORT MetaErrorStringT2, MetaErrorStringT3, MetaErrorStringT4, MetaString2, MetaString3, MetaString4 ;
FROM StrLib IMPORT StrEqual ;
FROM M2Debug IMPORT Assert ;

FROM SymbolTable IMPORT NulSym, IsRecord, IsSet, GetDType, GetSType, IsType,
                        SkipType, IsProcedure, NoOfParamAny, IsVarParamAny, GetNth,
                        GetNthParamAny, IsProcType, IsVar, IsEnumeration, IsArray,
                        IsSubrange, GetArraySubscript, IsConst,
                        IsReallyPointer, IsPointer, IsParameter, ModeOfAddr,
                        GetMode, GetType, IsUnbounded, IsComposite, IsConstructor,
                        IsParameter, IsConstString, IsConstLitInternal, IsConstLit,
                        GetStringLength, GetProcedureProcType, IsHiddenType,
                        IsHiddenReallyPointer, GetDimension ;

FROM M2GCCDeclare IMPORT GetTypeMin, GetTypeMax ;
FROM M2System IMPORT Address ;
FROM M2ALU IMPORT Equ, PushIntegerTree ;
FROM m2expr IMPORT AreConstantsEqual ;
FROM SymbolConversion IMPORT Mod2Gcc ;
FROM DynamicStrings IMPORT String, InitString, KillString ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM Storage IMPORT ALLOCATE ;
FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT printf ;


CONST
   debugging = FALSE ;

TYPE
   errorSig = POINTER TO RECORD
                            token:  CARDINAL ;
                            left,
                            right:  CARDINAL ;
                         END ;

   pair = POINTER TO RECORD
                        left, right: CARDINAL ;
                        pairStatus : status ;
                        next       : pair ;
                     END ;

   typeCheckFunction = PROCEDURE (status, tInfo, CARDINAL, CARDINAL) : status ;

   checkType = (parameter, assignment, expression) ;

   tInfo = POINTER TO RECORD
                         format    : String ;
                         kind      : checkType ;
                         token,
                         actual,
                         formal,
                         left,
                         right,
                         procedure,
                         nth       : CARDINAL ;
                         isvar     : BOOLEAN ;
                         strict    : BOOLEAN ;  (* Comparison expression.  *)
                         isin      : BOOLEAN ;  (* Expression created by IN?  *)
                         error     : Error ;
                         checkFunc : typeCheckFunction ;
                         visited,
                         resolved,
                         unresolved: Index ;
                         next      : tInfo ;
                      END ;

   status = (true, false, unknown, visited, unused) ;


VAR
   pairFreeList : pair ;
   tinfoFreeList: tInfo ;
   errors       : Index ;


(*
   dumpIndice -
*)

PROCEDURE dumpIndice (ptr: pair) ;
BEGIN
   printf (" left (%d), right (%d), status ",
           ptr^.left, ptr^.right);
   CASE ptr^.pairStatus OF

   true   :  printf ("true") |
   false  :  printf ("false") |
   unknown:  printf ("unknown") |
   visited:  printf ("visited") |
   unused :  printf ("unused")

   END ;
   printf ("\n")
END dumpIndice ;


(*
   dumpIndex -
*)

PROCEDURE dumpIndex (name: ARRAY OF CHAR; index: Index) ;
BEGIN
   printf ("status: %s\n", ADR (name)) ;
   ForeachIndiceInIndexDo (index, dumpIndice)
END dumpIndex ;


(*
   dumptInfo -
*)

PROCEDURE dumptInfo (t: tInfo) ;
BEGIN
   printf ("actual (%d), formal (%d), left (%d), right (%d), procedure (%d)\n",
           t^.actual, t^.formal, t^.left, t^.right, t^.procedure) ;
   dumpIndex ('visited', t^.visited) ;
   dumpIndex ('resolved', t^.resolved) ;
   dumpIndex ('unresolved', t^.unresolved)
END dumptInfo ;


(*
   isKnown - returns BOOLEAN:TRUE if result is status:true or status:false.
*)

PROCEDURE isKnown (result: status) : BOOLEAN ;
BEGIN
   RETURN (result = true) OR (result = false) OR (result = visited)
END isKnown ;


(*
   isTrue - returns BOOLEAN:TRUE if result is status:true

PROCEDURE isTrue (result: status) : BOOLEAN ;
BEGIN
   RETURN result = true
END isTrue ;
*)


(*
   isFalse - returns BOOLEAN:TRUE if result is status:false
*)

PROCEDURE isFalse (result: status) : BOOLEAN ;
BEGIN
   RETURN result = false
END isFalse ;


(*
   checkTypeEquivalence - returns TRUE if left and right can be skipped and found to be equal.
*)

PROCEDURE checkTypeEquivalence (result: status; left, right: CARDINAL) : status ;
VAR
   leftT, rightT: CARDINAL ;
BEGIN
   (* firstly check to see if we already have resolved this as false.  *)
   IF isFalse (result)
   THEN
      RETURN result
   ELSE
      (* check to see if we dont care about left or right.  *)
      IF (left = NulSym) OR (right = NulSym)
      THEN
         RETURN true
      ELSE
         leftT := SkipType (left) ;
         rightT := SkipType (right) ;
         IF leftT = rightT
         THEN
            RETURN true
         ELSIF IsType (leftT) AND IsType (rightT)
         THEN
            (* the fundamental types are definitely different.  *)
            RETURN false
         END
      END
   END ;
   RETURN result
END checkTypeEquivalence ;


(*
   checkSubrange - check to see if subrange types left and right have the same limits.
*)

PROCEDURE checkSubrange (result: status; tinfo: tInfo; left, right: CARDINAL) : status ;
VAR
   lLow,  rLow,
   lHigh, rHigh: CARDINAL ;
BEGIN
   (* firstly check to see if we already have resolved this as false.  *)
   IF isFalse (result)
   THEN
      RETURN result
   ELSE
      Assert (IsSubrange (left)) ;
      Assert (IsSubrange (right)) ;
      lLow := GetTypeMin (left) ;
      lHigh := GetTypeMax (left) ;
      rLow := GetTypeMin (right) ;
      rHigh := GetTypeMax (right) ;
      PushIntegerTree (Mod2Gcc (lLow)) ;
      PushIntegerTree (Mod2Gcc (rLow)) ;
      IF NOT Equ (tinfo^.token)
      THEN
         RETURN false
      END ;
      PushIntegerTree (Mod2Gcc (lHigh)) ;
      PushIntegerTree (Mod2Gcc (rHigh)) ;
      IF NOT Equ (tinfo^.token)
      THEN
         RETURN false
      END
   END ;
   RETURN true
END checkSubrange ;


(*
   checkUnboundedArray - returns status if unbounded is parameter compatible with array.
                         It checks all type equivalences of the static array for a
                         match with the dynamic (unbounded) array.
*)

PROCEDURE checkUnboundedArray (result: status;
                               unbounded, array: CARDINAL) : status ;
VAR
   dim   : CARDINAL ;
   ubtype,
   type  : CARDINAL ;
BEGIN
   (* Firstly check to see if we have resolved this as false.  *)
   IF isFalse (result)
   THEN
      RETURN result
   ELSE
      Assert (IsUnbounded (unbounded)) ;
      Assert (IsArray (array)) ;
      dim := GetDimension (unbounded) ;
      ubtype := GetType (unbounded) ;
      type := array ;
      REPEAT
         type := GetType (type) ;
         DEC (dim) ;
         (* Check type equivalences.  *)
         IF checkTypeEquivalence (result, type, ubtype) = true
         THEN
            RETURN true
         END ;
         type := SkipType (type) ;
         (* If we have run out of dimensions we conclude false.  *)
         IF dim = 0
         THEN
            RETURN false
         END ;
      UNTIL NOT IsArray (type)
   END ;
   RETURN false
END checkUnboundedArray ;


(*
   checkUnboundedUnbounded - check to see if formal and actual are compatible.
                             Both are unbounded parameters.
*)

PROCEDURE checkUnboundedUnbounded (result: status;
                                   tinfo: tInfo;
                                   formal, actual: CARDINAL) : status ;
BEGIN
   (* Firstly check to see if we have resolved this as false.  *)
   IF isFalse (result)
   THEN
      RETURN result
   ELSE
      Assert (IsUnbounded (formal)) ;
      Assert (IsUnbounded (actual)) ;
      (* The actual parameter above might be a different symbol to the actual parameter
         symbol in the tinfo.  So we must compare the original actual parameter against
         the formal.
         The actual above maybe a temporary which is created after derefencing an array.
         For example 'bar[10]' where bar is defined as ARRAY OF ARRAY OF CARDINAL.
         The GetDimension for 'bar[10]' is 1 indicating that one dimension has been
         referenced.  We use GetDimension for 'bar' which is 2.  *)
      IF GetDimension (formal) # GetDimension (tinfo^.actual)
      THEN
         RETURN false
      END ;
      IF checkTypeEquivalence (result, GetType (formal), GetType (actual)) = true
      THEN
         RETURN true
      END
   END ;
   RETURN false
END checkUnboundedUnbounded ;


(*
   checkUnbounded - check to see if the unbounded is type compatible with right.
                    This is only allowed during parameter passing.
*)

PROCEDURE checkUnbounded (result: status;
                          tinfo: tInfo;
                          unbounded, right: CARDINAL) : status ;
BEGIN
   (* Firstly check to see if we have resolved this as false.  *)
   IF isFalse (result)
   THEN
      RETURN result
   ELSE
      Assert (IsUnbounded (unbounded)) ;
      IF tinfo^.kind = parameter
      THEN
         (* Check the unbounded data type against the type of right, SYSTEM types
            are compared by the caller, so no need to test for them again.  *)
         IF isSkipEquivalence (GetType (unbounded), right)
         THEN
            RETURN true
         ELSIF IsType (right)
         THEN
            IF GetType (right) = NulSym
            THEN
               (* Base type check.  *)
               RETURN checkPair (result, tinfo, GetType (unbounded), right)
            ELSE
               (* It is safe to GetType (right) and we check the pair
                  [unbounded, GetType (right)].  *)
               RETURN checkPair (result, tinfo, unbounded, GetType (right))
            END
         ELSIF IsArray (right)
         THEN
            RETURN checkUnboundedArray (result, unbounded, right)
         ELSIF IsUnbounded (right)
         THEN
            RETURN checkUnboundedUnbounded (result, tinfo, unbounded, right)
         END
      END
   END ;
   RETURN false
END checkUnbounded ;


(*
   checkArrayTypeEquivalence - check array and unbounded array type equivalence.
*)

PROCEDURE checkArrayTypeEquivalence (result: status; tinfo: tInfo;
                                     left, right: CARDINAL) : status ;
VAR
   lSub , rSub: CARDINAL ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF IsArray (left) AND IsArray (right)
   THEN
      lSub := GetArraySubscript (left) ;
      rSub := GetArraySubscript (right) ;
      result := checkPair (result, tinfo, GetSType (left), GetSType (right)) ;
      IF (lSub # NulSym) AND (rSub # NulSym)
      THEN
         result := checkSubrange (result, tinfo, getSType (lSub), getSType (rSub))
      END
   ELSIF IsUnbounded (left) AND (IsArray (right) OR IsUnbounded (right))
   THEN
      IF IsGenericSystemType (getSType (left)) OR IsGenericSystemType (getSType (right))
      THEN
         RETURN true
      ELSE
         result := checkUnbounded (result, tinfo, left, right)
      END
   ELSIF IsUnbounded (right) AND (IsArray (left) OR IsUnbounded (left))
   THEN
      IF IsGenericSystemType (getSType (right)) OR IsGenericSystemType (getSType (left))
      THEN
         RETURN true
      ELSE
         result := checkUnbounded (result, tinfo, right, left)
      END
   ELSIF IsArray (left) AND IsConst (right)
   THEN
      result := checkPair (result, tinfo, GetType (left), GetType (right))
   ELSIF IsArray (right) AND IsConst (left)
   THEN
      result := checkPair (result, tinfo, GetType (left), GetType (right))
   END ;
   RETURN result
END checkArrayTypeEquivalence ;


(*
   checkGenericTypeEquivalence - check left and right for generic equivalence.
*)

PROCEDURE checkGenericTypeEquivalence (result: status; left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF left = right
   THEN
      RETURN true
   ELSE
      RETURN result
   END
END checkGenericTypeEquivalence ;


(*
   firstTime - returns TRUE if the triple (token, left, right) has not been seen before.
*)

PROCEDURE firstTime (token: CARDINAL; left, right: CARDINAL) : BOOLEAN ;
VAR
   p   : errorSig ;
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice (errors) ;
   WHILE i <= n DO
      p := GetIndice (errors, i) ;
      IF (p^.token = token) AND (p^.left = left) AND (p^.right = right)
      THEN
         RETURN FALSE
      END ;
      INC (i)
   END ;
   NEW (p) ;
   p^.token := token ;
   p^.left := left ;
   p^.right := right ;
   IncludeIndiceIntoIndex (errors, p) ;
   RETURN TRUE
END firstTime ;


(*
   buildError4 - generate a MetaString4 error.  This is only used when checking
                 parameter compatibility.
*)

PROCEDURE buildError4 (tinfo: tInfo; left, right: CARDINAL) ;
VAR
   s: String ;
BEGIN
   IF firstTime (tinfo^.token, left, right)
   THEN
      IF tinfo^.error = NIL
      THEN
         (* need to create top level error message first.  *)
         tinfo^.error := NewError (tinfo^.token) ;
         (* The parameters to MetaString4 in buildError4 must match the order
            of paramters passed to ParameterTypeCompatible.  *)
         s := MetaString4 (tinfo^.format,
                           tinfo^.procedure,
                           tinfo^.formal, tinfo^.actual,
                           tinfo^.nth) ;
         ErrorString (tinfo^.error, s)
      END ;
      (* and also generate a sub error containing detail.  *)
      IF (left # tinfo^.left) OR (right # tinfo^.right)
      THEN
         tinfo^.error := ChainError (tinfo^.token, tinfo^.error) ;
         s := MetaString2 (InitString ("{%1Ead} and {%2ad} are incompatible as formal and actual procedure parameters"),
                           left, right) ;
         ErrorString (tinfo^.error, s)
      END
   END
END buildError4 ;


(*
   buildError2 - generate a MetaString2 error.  This is called by all three kinds of errors.
*)

PROCEDURE buildError2 (tinfo: tInfo; left, right: CARDINAL) ;
VAR
   s: String ;
BEGIN
   IF firstTime (tinfo^.token, left, right)
   THEN
      IF tinfo^.error = NIL
      THEN
         (* Need to create top level error message first.  *)
         tinfo^.error := NewError (tinfo^.token) ;
         s := MetaString2 (tinfo^.format,
                           tinfo^.left, tinfo^.right) ;
         ErrorString (tinfo^.error, s)
      END ;
      (* Also generate a sub error containing detail.  *)
      IF (left # tinfo^.left) OR (right # tinfo^.right)
      THEN
         tinfo^.error := ChainError (tinfo^.token, tinfo^.error) ;
         CASE tinfo^.kind OF

         parameter:  s := MetaString2 (InitString ("{%1Ead} and {%2ad} are incompatible as formal and actual procedure parameters"),
                                       left, right) |
         assignment: s := MetaString2 (InitString ("{%1Ead} and {%2ad} are assignment incompatible"),
                                       left, right) |
         expression: s := MetaString2 (InitString ("{%1Ead} and {%2ad} are expression incompatible"),
                                       left, right)

         END ;
         ErrorString (tinfo^.error, s)
      END
   END
END buildError2 ;


(*
   issueError -
*)

PROCEDURE issueError (result: BOOLEAN; tinfo: tInfo; left, right: CARDINAL) : status ;
BEGIN
   IF result
   THEN
      RETURN true
   ELSE
      (* check whether errors are required.  *)
      IF tinfo^.format # NIL
      THEN
         CASE tinfo^.kind OF

         parameter :  buildError4 (tinfo, left, right) |
         assignment:  buildError2 (tinfo, left, right) |
         expression:  buildError2 (tinfo, left, right)

         END ;
         tinfo^.format := NIL    (* string is used by MetaError now.  *)
      END ;
      RETURN false
   END
END issueError ;


(*
   checkBaseEquivalence - the catch all check for types not specifically
                          handled by this module.
*)

PROCEDURE checkBaseEquivalence (result: status; tinfo: tInfo;
                                left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSE
      CASE tinfo^.kind OF

      parameter :  IF tinfo^.isvar
                   THEN
                      RETURN issueError (IsExpressionCompatible (left, right),
                                         tinfo, left, right)
                   ELSE
                      RETURN issueError (IsAssignmentCompatible (left, right),
                                         tinfo, left, right)
                   END |
      assignment:  RETURN issueError (IsAssignmentCompatible (left, right),
                                      tinfo, left, right) |
      expression:  IF tinfo^.isin
                   THEN
                      IF IsVar (right) OR IsConst (right)
                      THEN
                         right := getSType (right)
                      END
                   END ;
                   IF tinfo^.strict
                   THEN
                      RETURN issueError (IsComparisonCompatible (left, right),
                                         tinfo, left, right)
                   ELSE
                      RETURN issueError (IsExpressionCompatible (left, right),
                                         tinfo, left, right)
                   END

      ELSE
         InternalError ('unexpected kind value')
      END
   END
   (* should never reach here.  *)
END checkBaseEquivalence ;


(*
   checkPair - check whether left and right are type compatible.
               It will update the visited, unresolved list before
               calling the docheckPair for the cascaded type checking.
               Pre-condition: tinfo is initialized.
                              left and right are modula2 symbols.
               Post-condition: tinfo visited, resolved, unresolved lists
                               are updated and the result status is
                               returned.
*)

PROCEDURE checkPair (result: status; tinfo: tInfo;
                     left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      exclude (tinfo^.visited, left, right) ;
      RETURN result
   ELSE
      IF in (tinfo^.resolved, left, right)
      THEN
         exclude (tinfo^.visited, left, right) ;
         RETURN getStatus (tinfo^.resolved, left, right)
      ELSIF in (tinfo^.visited, left, right)
      THEN
         RETURN visited
      ELSE
         IF debugging
         THEN
            printf ("   marked as visited (%d, %d)\n", left, right)
         END ;
         include (tinfo^.visited, left, right, unknown) ;
         include (tinfo^.unresolved, left, right, unknown)
      END ;
      RETURN doCheckPair (result, tinfo, left, right)
   END
END checkPair ;


(*
   useBaseCheck -
*)

PROCEDURE useBaseCheck (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsBaseType (sym) OR IsSystemType (sym) OR IsMathType (sym) OR IsComplexType (sym)
END useBaseCheck ;


(*
   checkBaseTypeEquivalence -
*)

PROCEDURE checkBaseTypeEquivalence (result: status; tinfo: tInfo;
                                     left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF useBaseCheck (left) AND useBaseCheck (right)
   THEN
      RETURN checkBaseEquivalence (result, tinfo, left, right)
   ELSE
      RETURN result
   END
END checkBaseTypeEquivalence ;


(*
   IsTyped - returns TRUE if sym will have a type.
*)

PROCEDURE IsTyped (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsVar (sym) OR IsParameter (sym) OR IsConstructor (sym) OR
          (IsConst (sym) AND IsConstructor (sym)) OR IsParameter (sym) OR
          (IsConst (sym) AND (GetType (sym) # NulSym))
END IsTyped ;


(*
   isLValue -
*)

PROCEDURE isLValue (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsVar (sym) AND (GetMode (sym) = LeftValue)
END isLValue ;


(*
   checkVarEquivalence - this test must be done early as it checks the symbol mode.
                         An LValue is treated as a pointer during assignment and the
                         LValue is attached to a variable.  This function skips the variable
                         and checks the types - after it has considered a possible LValue.
*)

PROCEDURE checkVarEquivalence (result: status; tinfo: tInfo;
                               left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF IsTyped (left) OR IsTyped (right)
   THEN
      IF tinfo^.kind = assignment
      THEN
         (* LValues are only relevant during assignment.  *)
         IF isLValue (left) AND (NOT isLValue (right))
         THEN
            IF SkipType (getType (right)) = Address
            THEN
               RETURN true
            ELSIF IsPointer (SkipType (getType (right)))
            THEN
               right := GetDType (SkipType (getType (right)))
            END
         ELSIF isLValue (right) AND (NOT isLValue (left))
         THEN
            IF SkipType (getType (left)) = Address
            THEN
               RETURN true
            ELSIF IsPointer (SkipType (getType (left)))
            THEN
               left := GetDType (SkipType (getType (left)))
            END
         END
      END ;
      RETURN doCheckPair (result, tinfo, getType (left), getType (right))
   ELSE
      RETURN result
   END
END checkVarEquivalence ;


(*
   checkConstMeta - performs a very course grained check against
                    obviously incompatible type kinds.
                    If left is a const string then it checks right against char.
*)

PROCEDURE checkConstMeta (result: status; tinfo: tInfo;
                          left, right: CARDINAL) : status ;
VAR
   typeRight: CARDINAL ;
BEGIN
   Assert (IsConst (left)) ;
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF IsConstString (left)
   THEN
      IF IsConstString (right)
      THEN
         RETURN true
      ELSIF IsTyped (right)
      THEN
         typeRight := GetDType (right) ;
         IF typeRight = NulSym
         THEN
            RETURN result
         ELSIF IsSet (typeRight) OR IsEnumeration (typeRight) OR
               IsProcedure (typeRight) OR IsRecord (typeRight) OR
               IsReallyPointer (typeRight)
         THEN
            RETURN false
         ELSIF IsArray (typeRight)
         THEN
            RETURN doCheckPair (result, tinfo, Char, GetType (typeRight))
         ELSIF GetStringLength (tinfo^.token, left) = 1
         THEN
            RETURN doCheckPair (result, tinfo, Char, typeRight)
         END
      END
   END ;
   RETURN result
END checkConstMeta ;


(*
   checkConstEquivalence - this check can be done first as it checks symbols which
                           may have no type.  Ie constant strings.  These constants
                           will likely have their type set during quadruple folding.
                           But we can check the meta type for obvious mismatches
                           early on.  For example adding a string to an enum or set.
*)

PROCEDURE checkConstEquivalence (result: status; tinfo: tInfo;
                                 left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF (left = NulSym) OR (right = NulSym)
   THEN
      (* No option but to return true.  *)
      RETURN true
   ELSIF IsConst (left)
   THEN
      RETURN checkConstMeta (result, tinfo, left, right)
   ELSIF IsConst (right)
   THEN
      RETURN checkConstMeta (result, tinfo, right, left)
   END ;
   RETURN result
END checkConstEquivalence ;


(*
   checkSubrangeTypeEquivalence -
*)

PROCEDURE checkSubrangeTypeEquivalence (result: status; tinfo: tInfo;
                                        left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSE
      IF IsSubrange (left)
      THEN
         RETURN doCheckPair (result, tinfo, GetDType (left), right)
      END ;
      IF IsSubrange (right)
      THEN
         RETURN doCheckPair (result, tinfo, left, GetDType (right))
      END ;
      IF left = right
      THEN
         RETURN true
      ELSE
         RETURN result
      END
   END
END checkSubrangeTypeEquivalence ;


(*
   isZRC -
*)

PROCEDURE isZRC (zrc, sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsConst (sym)
   THEN
      sym := SkipType (GetType (sym))
   END ;
   IF (zrc = CType) AND (IsComplexN (sym) OR IsComplexType (sym))
   THEN
      RETURN TRUE
   END ;
   RETURN (zrc = sym) OR ((zrc = ZType) OR (zrc = RType) AND (NOT IsComposite (sym)))
END isZRC ;


(*
   isSameSizeConst -

*)

PROCEDURE isSameSizeConst (a, b: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsConst (a)
   THEN
      a := SkipType (GetType (a)) ;
      RETURN isZRC (a, b) OR (a = b) OR ((a # NulSym) AND isSameSize (a, b))
   ELSIF IsConst (b)
   THEN
      b := SkipType (GetType (b)) ;
      RETURN isZRC (b, a) OR (a = b) OR ((b # NulSym) AND isSameSize (a, b))
   END ;
   RETURN FALSE
END isSameSizeConst ;


(*
   isSameSize - should only be called if either a or b are WORD, BYTE, etc.
*)

PROCEDURE isSameSize (a, b: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN isSameSizeConst (a, b) OR IsSameSize (a, b)
END isSameSize ;


(*
   checkSystemEquivalence - check whether left and right are system types and whether they have the same size.
*)

PROCEDURE checkSystemEquivalence (result: status; left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result) OR (result = visited)
   THEN
      RETURN result
   ELSE
      IF (IsGenericSystemType (left) OR IsGenericSystemType (right)) AND
         isSameSize (left, right)
      THEN
         RETURN true
      END
   END ;
   RETURN result
END checkSystemEquivalence ;


(*
   checkTypeKindViolation - returns false if one operand left or right is
                            a set, record or array.
*)

PROCEDURE checkTypeKindViolation (result: status;
                                  left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result) OR (result = visited)
   THEN
      RETURN result
   ELSE
      (* We have checked IsSet (left) and IsSet (right) etc in doCheckPair.  *)
      IF (IsSet (left) OR IsSet (right)) OR
         (IsRecord (left) OR IsRecord (right)) OR
         (IsArray (left) OR IsArray (right))
      THEN
         RETURN false
      END
   END ;
   RETURN result
END checkTypeKindViolation ;


(*
   doCheckPair - invoke a series of ordered type checks checking compatibility
                 between left and right modula2 symbols.
                 Pre-condition: left and right are modula-2 symbols.
                                tinfo is configured.
                 Post-condition: status is returned determining the
                                 correctness of the type check.
                                 The tinfo resolved, unresolved, visited
                                 lists will be updated.
*)

PROCEDURE doCheckPair (result: status; tinfo: tInfo;
                       left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result) OR (result = visited)
   THEN
      RETURN return (result, tinfo, left, right)
   ELSIF left = right
   THEN
      RETURN return (true, tinfo, left, right)
   ELSE
      result := checkConstEquivalence (unknown, tinfo, left, right) ;
      IF NOT isKnown (result)
      THEN
         result := checkVarEquivalence (unknown, tinfo, left, right) ;
         IF NOT isKnown (result)
         THEN
            result := checkSystemEquivalence (unknown, left, right) ;
            IF NOT isKnown (result)
            THEN
               result := checkSubrangeTypeEquivalence (unknown, tinfo, left, right) ;
               IF NOT isKnown (result)
               THEN
                  result := checkBaseTypeEquivalence (unknown, tinfo, left, right) ;
                  IF NOT isKnown (result)
                  THEN
                     result := checkTypeEquivalence (unknown, left, right) ;
                     IF NOT isKnown (result)
                     THEN
                        result := checkArrayTypeEquivalence (result, tinfo, left, right) ;
                        IF NOT isKnown (result)
                        THEN
                           result := checkGenericTypeEquivalence (result, left, right) ;
                           IF NOT isKnown (result)
                           THEN
                              result := checkTypeKindEquivalence (result, tinfo, left, right) ;
                              IF NOT isKnown (result)
                              THEN
                                 result := checkTypeKindViolation (result, left, right)
                              END
                           END
                        END
                     END
                  END
               END
            END
         END
      END
   END ;
   RETURN return (result, tinfo, left, right)
END doCheckPair ;


(*
   checkProcType -
*)

PROCEDURE checkProcType (result: status; tinfo: tInfo;
                         left, right: CARDINAL) : status ;
VAR
   i, n  : CARDINAL ;
   lt, rt: CARDINAL ;
BEGIN
   Assert (IsProcType (right)) ;
   Assert (IsProcType (left)) ;
   IF isFalse (result)
   THEN
      RETURN result
   ELSE
      lt := GetDType (left) ;
      rt := GetDType (right) ;
      IF (lt = NulSym) AND (rt = NulSym)
      THEN
         result := unknown
      ELSIF lt = NulSym
      THEN
         IF tinfo^.format # NIL
         THEN
            MetaErrorStringT3 (tinfo^.token, InitString ("procedure type {%1a} does not have a {%kRETURN} type whereas procedure type {%2ad} has a {%kRETURN} type {%3ad}"), left, right, rt)
         END ;
         RETURN return (false, tinfo, left, right)
      ELSIF rt = NulSym
      THEN
         IF tinfo^.format # NIL
         THEN
            MetaErrorStringT3 (tinfo^.token, InitString ("procedure type {%1a} does not have a {%kRETURN} type whereas procedure type {%2ad} has a {%kRETURN} type {%3ad}"), right, left, lt)
         END ;
         RETURN return (false, tinfo, left, right)
      ELSE
         (* two return type seen so we check them.  *)
         result := checkPair (unknown, tinfo, lt, rt)
      END ;

      IF NoOfParamAny (left) # NoOfParamAny (right)
      THEN
         IF tinfo^.format # NIL
         THEN
            MetaErrorStringT2 (tinfo^.token, InitString ("procedure type {%1a} has a different number of parameters from procedure type {%2ad}"), right, left)
         END ;
         RETURN return (false, tinfo, left, right)
      END ;
      i := 1 ;
      n := NoOfParamAny (left) ;
      WHILE i <= n DO
         IF IsVarParamAny (left, i) # IsVarParamAny (right, i)
         THEN
            IF IsVarParamAny (left, i)
            THEN
               IF tinfo^.format # NIL
               THEN
                  MetaErrorStringT3 (tinfo^.token, InitString ("procedure type {%2a} {%3n} parameter was declared as a {%kVAR} whereas procedure type {%1ad} {%3n} parameter was not"), right, left, i)
               END
            ELSE
               IF tinfo^.format # NIL
               THEN
                  MetaErrorStringT3 (tinfo^.token, InitString ("procedure type {%1a} {%3n} parameter was declared as a {%kVAR} whereas procedure type {%2ad} {%3n} parameter was not"), right, left, i)
               END
            END ;
            RETURN return (false, tinfo, left, right)
         END ;
         result := checkPair (result, tinfo, GetDType (GetNthParamAny (left, i)), GetDType (GetNthParamAny (right, i))) ;
         INC (i)
      END
   END ;
   RETURN return (result, tinfo, left, right)
END checkProcType ;


(*
   checkProcedureProcType -
*)

PROCEDURE checkProcedureProcType (result: status; tinfo: tInfo;
                                  left, right: CARDINAL) : status ;
VAR
   i, n  : CARDINAL ;
   lt, rt: CARDINAL ;
BEGIN
   Assert (IsProcedure (right)) ;
   Assert (IsProcType (left)) ;
   IF NOT isFalse (result)
   THEN
      lt := GetDType (left) ;
      rt := GetDType (right) ;
      IF (lt = NulSym) AND (rt = NulSym)
      THEN
         (* nothing.  *)
      ELSIF lt = NulSym
      THEN
         IF tinfo^.format # NIL
         THEN
            MetaErrorStringT3 (tinfo^.token, InitString ("procedure type {%1a} does not have a {%kRETURN} type whereas procedure {%2ad} has a {%kRETURN} type {%3ad}"), left, right, rt)
         END ;
         RETURN return (false, tinfo, left, right)
      ELSIF rt = NulSym
      THEN
         IF tinfo^.format # NIL
         THEN
            MetaErrorStringT3 (tinfo^.token, InitString ("procedure {%1a} does not have a {%kRETURN} type whereas procedure type {%2ad} has a {%kRETURN} type {%3ad}"), right, left, lt)
         END ;
         RETURN return (false, tinfo, left, right)
      ELSE
         (* two return type seen so we check them.  *)
         result := checkPair (result, tinfo, lt, rt)
      END ;

      IF NoOfParamAny (left) # NoOfParamAny (right)
      THEN
         IF tinfo^.format # NIL
         THEN
            MetaErrorStringT2 (tinfo^.token, InitString ("procedure {%1a} has a different number of parameters from procedure type {%2ad}"), right, left)
         END ;
         RETURN return (false, tinfo, left, right)
      END ;
      i := 1 ;
      n := NoOfParamAny (left) ;
      WHILE i <= n DO
         IF IsVarParamAny (left, i) # IsVarParamAny (right, i)
         THEN
            IF IsVarParamAny (left, i)
            THEN
               IF tinfo^.format # NIL
               THEN
                  MetaErrorStringT3 (tinfo^.token, InitString ("procedure type {%2a} {%3n} parameter was declared as a {%kVAR} whereas procedure {%1ad} {%3n} parameter was not"), right, left, i)
               END
            ELSE
               IF tinfo^.format # NIL
               THEN
                  MetaErrorStringT3 (tinfo^.token, InitString ("procedure {%1a} {%3n} parameter was declared as a {%kVAR} whereas procedure type {%2ad} {%3n} parameter was not"), right, left, i)
               END
            END ;
            RETURN return (false, tinfo, left, right)
         END ;
         result := checkPair (result, tinfo, GetDType (GetNthParamAny (left, i)), GetDType (GetNthParamAny (right, i))) ;
         INC (i)
      END
   END ;
   RETURN return (result, tinfo, left, right)
END checkProcedureProcType ;


(*
   checkProcedure -
*)

PROCEDURE checkProcedure (result: status; tinfo: tInfo;
                          left, right: CARDINAL) : status ;
BEGIN
   Assert (IsProcedure (right)) ;
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF IsVar (left)
   THEN
      RETURN checkProcedure (result, tinfo,
                             GetDType (left), right)
   ELSIF left = Address
   THEN
      RETURN true
   ELSIF IsProcType (left)
   THEN
      RETURN checkProcedureProcType (result, tinfo, left, right)
   ELSE
      RETURN result
   END
END checkProcedure ;


(*
   checkEnumerationEquivalence -
*)

PROCEDURE checkEnumerationEquivalence (result: status;
                                       left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF left = right
   THEN
      RETURN true
   ELSE
      RETURN false
   END
END checkEnumerationEquivalence ;


(*
   checkPointerType - check whether left and right are equal or are of type ADDRESS.
*)

PROCEDURE checkPointerType (result: status; left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF (left = right) OR (left = Address) OR (right = Address)
   THEN
      RETURN true
   ELSE
      RETURN false
   END
END checkPointerType ;


(*
   checkProcTypeEquivalence - allow proctype to be compared against another
                              proctype or procedure.  It is legal to be compared
                              against an address.
*)

PROCEDURE checkProcTypeEquivalence (result: status; tinfo: tInfo;
                                    left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF IsProcedure (left) AND IsProcType (right)
   THEN
      RETURN checkProcedure (result, tinfo, right, left)
   ELSIF IsProcType (left) AND IsProcedure (right)
   THEN
      RETURN checkProcedure (result, tinfo, left, right)
   ELSIF IsProcType (left) AND IsProcType (right)
   THEN
      RETURN checkProcType (result, tinfo, left, right)
   ELSIF (left = Address) OR (right = Address)
   THEN
      RETURN true
   ELSE
      RETURN false
   END
END checkProcTypeEquivalence ;



(*
   checkTypeKindEquivalence -
*)

PROCEDURE checkTypeKindEquivalence (result: status; tinfo: tInfo;
                                    left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF (left = NulSym) OR (right = NulSym)
   THEN
      RETURN true
   ELSE
      (* Long cascade of all type kinds.  *)
      IF IsSet (left) AND IsSet (right)
      THEN
         RETURN checkSetEquivalent (result, tinfo, left, right)
      ELSIF IsArray (left) AND IsArray (right)
      THEN
         RETURN checkArrayTypeEquivalence (result, tinfo, left, right)
      ELSIF IsRecord (left) AND IsRecord (right)
      THEN
         RETURN checkRecordEquivalence (result, left, right)
      ELSIF IsEnumeration (left) AND IsEnumeration (right)
      THEN
         RETURN checkEnumerationEquivalence (result, left, right)
      ELSIF IsProcType (left) OR IsProcType (right)
      THEN
         RETURN checkProcTypeEquivalence (result, tinfo, right, left)
      ELSIF IsReallyPointer (left) AND IsReallyPointer (right)
      THEN
         RETURN checkPointerType (result, left, right)
      ELSE
         RETURN result
      END
   END
END checkTypeKindEquivalence ;


(*
   isSkipEquivalence -
*)

PROCEDURE isSkipEquivalence (left, right: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN SkipType (left) = SkipType (right)
END isSkipEquivalence ;


(*
   checkValueEquivalence - check to see if left and right values are the same.
*)

PROCEDURE checkValueEquivalence (result: status; left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSIF left = right
   THEN
      RETURN true
   ELSE
      IF AreConstantsEqual (Mod2Gcc (left), Mod2Gcc (right))
      THEN
         RETURN true
      ELSE
         RETURN false
      END
   END
END checkValueEquivalence ;


(*
   and -
*)

PROCEDURE and (left, right: status) : status ;
BEGIN
   IF (left = true) AND (right = true)
   THEN
      RETURN true
   ELSE
      RETURN false
   END
END and ;


(*
   checkTypeRangeEquivalence -
*)

PROCEDURE checkTypeRangeEquivalence (result: status; tinfo: tInfo;
                                     left, right: CARDINAL) : status ;
VAR
   result2, result3: status ;
BEGIN
   result := checkSkipEquivalence (result, left, right) ;
   result2 := checkValueEquivalence (result, GetTypeMin (left), GetTypeMin (right)) ;
   result3 := checkValueEquivalence (result, GetTypeMax (left), GetTypeMax (right)) ;
   RETURN return (and (result2, result3), tinfo, left, right)
END checkTypeRangeEquivalence ;


(*
   include - include pair left:right into pairs with status, s.
*)

PROCEDURE include (pairs: Index; left, right: CARDINAL; s: status) ;
VAR
   p: pair ;
BEGIN
   p := newPair () ;
   p^.left := left ;
   p^.right := right ;
   p^.pairStatus := s ;
   p^.next := NIL ;
   IncludeIndiceIntoIndex (pairs, p)
END include ;


(*
   exclude - exclude pair left:right from pairs.
*)

PROCEDURE exclude (pairs: Index; left, right: CARDINAL) ;
VAR
   p   : pair ;
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF (p # NIL) AND (p^.left = left) AND (p^.right = right)
      THEN
         PutIndice (pairs, i, NIL) ;
         disposePair (p) ;
         RETURN
      END ;
      INC (i)
   END
END exclude ;


(*
   getStatus -
*)

PROCEDURE getStatus (pairs: Index; left, right: CARDINAL) : status ;
VAR
   p   : pair ;
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF (p # NIL) AND (p^.left = left) AND (p^.right = right)
      THEN
         RETURN p^.pairStatus
      END ;
      INC (i)
   END ;
   RETURN unknown
END getStatus ;


(*
   return -
*)

PROCEDURE return (result: status; tinfo: tInfo; left, right: CARDINAL) : status ;
BEGIN
   IF result # unknown
   THEN
      IF isKnown (result)
      THEN
         include (tinfo^.resolved, left, right, result) ;
         exclude (tinfo^.unresolved, left, right) ;
         exclude (tinfo^.visited, left, right)   (* no longer visiting as it is resolved.  *)
      END
   END ;
   IF result = false
   THEN
      RETURN issueError (FALSE, tinfo, left, right)
   END ;
   RETURN result
END return ;


(*
   checkSkipEquivalence - return true if left right are equivalent.
*)

PROCEDURE checkSkipEquivalence (result: status; left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSIF isSkipEquivalence (left, right)
   THEN
      RETURN true
   ELSE
      RETURN result
   END
END checkSkipEquivalence ;


(*
   checkSetEquivalent - compares set types, left and right.
*)

PROCEDURE checkSetEquivalent (result: status; tinfo: tInfo;
                              left, right: CARDINAL) : status ;
BEGIN
   result := checkSkipEquivalence (result, left, right) ;
   result := checkTypeKindEquivalence (result, tinfo, GetDType (left), GetDType (right)) ;
   result := checkTypeRangeEquivalence (result, tinfo, GetDType (left), GetDType (right)) ;
   RETURN return (result, tinfo, left, right)
END checkSetEquivalent ;


(*
   checkRecordEquivalence - compares record types, left and right.
*)

PROCEDURE checkRecordEquivalence (result: status; left, right: CARDINAL) : status ;
BEGIN
   IF isFalse (result)
   THEN
      RETURN result
   ELSIF left = right
   THEN
      RETURN true
   ELSE
      RETURN false
   END
END checkRecordEquivalence ;


(*
   getType - only returns the type of symbol providing it is not a procedure.
*)

PROCEDURE getType (sym: CARDINAL) : CARDINAL ;
BEGIN
   IF (sym # NulSym) AND IsProcedure (sym)
   THEN
      RETURN GetProcedureProcType (sym)
   ELSIF IsTyped (sym)
   THEN
      RETURN GetDType (sym)
   ELSE
      RETURN sym
   END
END getType ;


(*
   getSType -
*)

PROCEDURE getSType (sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsProcedure (sym)
   THEN
      RETURN Address
   ELSE
      RETURN GetSType (sym)
   END
END getSType ;


(*
   determineCompatible - check for compatibility by checking
                         equivalence, array, generic and type kind.
*)

PROCEDURE determineCompatible (result: status; tinfo: tInfo; left, right: CARDINAL) : status ;
BEGIN
   result := checkPair (result, tinfo, left, right) ;
   RETURN return (result, tinfo, left, right)
END determineCompatible ;


(*
   get -
*)

PROCEDURE get (pairs: Index; VAR left, right: CARDINAL; s: status) : BOOLEAN ;
VAR
   i, n: CARDINAL ;
   p   : pair ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF (p # NIL) AND (p^.pairStatus = s)
      THEN
         left := p^.left ;
         right := p^.right ;
         RETURN TRUE
      END ;
      INC (i)
   END ;
   RETURN FALSE
END get ;


(*
   isInternal - return TRUE if sym is a constant lit which was declared
                as internal.
*)

PROCEDURE isInternal (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsConstLit (sym) AND IsConstLitInternal (sym)
END isInternal ;


(*
   doCheck - keep obtaining an unresolved pair and check for the
             type compatibility.  This is the main check routine used by
             parameter, assignment and expression compatibility.
             It tests all unknown pairs and calls the appropriate
             check function
*)

PROCEDURE doCheck (tinfo: tInfo) : BOOLEAN ;
VAR
   result     : status ;
   left, right: CARDINAL ;
BEGIN
   IF debugging
   THEN
      dumptInfo (tinfo)
   END ;
   WHILE get (tinfo^.unresolved, left, right, unknown) DO
      IF debugging
      THEN
         printf ("doCheck (%d, %d)\n", left, right) ;
         dumptInfo (tinfo)
      END ;
      IF isInternal (left) OR isInternal (right)
      THEN
         (* Do not check constants which have been generated internally.
            Currently these are generated by the default BY constant value
            in a FOR loop.  *)
         RETURN TRUE
      END ;
      (*
      IF in (tinfo^.visited, left, right)
      THEN
         IF debugging
         THEN
            printf ("   already visited (%d, %d)\n", left, right)
         END ;
      ELSE
         IF debugging
         THEN
            printf ("   not visited (%d, %d)\n", left, right)
         END ;
      *)
      result := tinfo^.checkFunc (unknown, tinfo, left, right) ;
      IF isKnown (result)
      THEN
         (* remove this pair from the unresolved list.  *)
         exclude (tinfo^.unresolved, left, right) ;
         (* add it to the resolved list.  *)
         include (tinfo^.resolved, left, right, result) ;
         IF result = false
         THEN
            IF debugging
            THEN
               printf ("   known (%d, %d)  false\n", left, right)
            END ;
            RETURN FALSE
         ELSE
            IF debugging
            THEN
               printf ("   known (%d, %d)  true\n", left, right)
            END
         END
      END
   END ;
   RETURN TRUE
END doCheck ;


(*
   in - returns TRUE if the pair is in the list.
*)

PROCEDURE in (pairs: Index; left, right: CARDINAL) : BOOLEAN ;
VAR
   i, n: CARDINAL ;
   p   : pair ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF (p # NIL) AND (p^.left = left) AND (p^.right = right)
      THEN
         RETURN TRUE
      END ;
      INC (i)
   END ;
   RETURN FALSE
END in ;


(*
   newPair -
*)

PROCEDURE newPair () : pair ;
VAR
   p: pair ;
BEGIN
   IF pairFreeList = NIL
   THEN
      NEW (p)
   ELSE
      p := pairFreeList ;
      pairFreeList := p^.next
   END ;
   Assert (p # NIL) ;
   RETURN p
END newPair ;


(*
   disposePair - adds pair, p, to the free list.
*)

PROCEDURE disposePair (p: pair) ;
BEGIN
   p^.next := pairFreeList ;
   pairFreeList := p
END disposePair ;


(*
   deconstructIndex -
*)

PROCEDURE deconstructIndex (pairs: Index) : Index ;
VAR
   p   : pair ;
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF p # NIL
      THEN
         disposePair (p)
      END ;
      INC (i)
   END ;
   RETURN KillIndex (pairs)
END deconstructIndex ;


(*
   deconstruct - deallocate the List data structure.
*)

PROCEDURE deconstruct (tinfo: tInfo) ;
BEGIN
   tinfo^.format := KillString (tinfo^.format) ;
   tinfo^.visited := deconstructIndex (tinfo^.visited) ;
   tinfo^.resolved := deconstructIndex (tinfo^.resolved) ;
   tinfo^.unresolved := deconstructIndex (tinfo^.unresolved)
END deconstruct ;


(*
   newtInfo -
*)

PROCEDURE newtInfo () : tInfo ;
VAR
   tinfo: tInfo ;
BEGIN
   IF tinfoFreeList = NIL
   THEN
      NEW (tinfo)
   ELSE
      tinfo := tinfoFreeList ;
      tinfoFreeList := tinfoFreeList^.next
   END ;
   RETURN tinfo
END newtInfo ;


(*
   collapseString - if the string, a, is "" then return NIL otherwise create
                    and return a dynamic string.
*)

PROCEDURE collapseString (a: ARRAY OF CHAR) : String ;
BEGIN
   IF StrEqual (a, "")
   THEN
      RETURN NIL
   ELSE
      RETURN InitString (a)
   END
END collapseString ;


(*
   AssignmentTypeCompatible - returns TRUE if the des and the expr are assignment compatible.
*)

PROCEDURE AssignmentTypeCompatible (token: CARDINAL; format: ARRAY OF CHAR;
                                    des, expr: CARDINAL) : BOOLEAN ;
VAR
   tinfo: tInfo ;
BEGIN
   tinfo := newtInfo () ;
   tinfo^.format := collapseString (format) ;
   tinfo^.token := token ;
   tinfo^.kind := assignment ;
   tinfo^.actual := NulSym ;
   tinfo^.formal := NulSym ;
   tinfo^.procedure := NulSym ;
   tinfo^.nth := 0 ;
   tinfo^.isvar := FALSE ;
   tinfo^.error := NIL ;
   tinfo^.left := des ;
   tinfo^.right := expr ;
   tinfo^.checkFunc := determineCompatible ;
   tinfo^.visited := InitIndex (1) ;
   tinfo^.resolved := InitIndex (1) ;
   tinfo^.unresolved := InitIndex (1) ;
   include (tinfo^.unresolved, des, expr, unknown) ;
   tinfo^.strict := FALSE ;
   tinfo^.isin := FALSE ;
   IF doCheck (tinfo)
   THEN
      deconstruct (tinfo) ;
      RETURN TRUE
   ELSE
      deconstruct (tinfo) ;
      RETURN FALSE
   END
END AssignmentTypeCompatible ;


(*
   ParameterTypeCompatible - returns TRUE if the nth procedure parameter formal
                             is compatible with actual.
*)

PROCEDURE ParameterTypeCompatible (token: CARDINAL; format: ARRAY OF CHAR;
                                   procedure, formal, actual, nth: CARDINAL;
                                   isvar: BOOLEAN) : BOOLEAN ;
VAR
   formalT, actualT: CARDINAL ;
   tinfo           : tInfo ;
BEGIN
   tinfo := newtInfo () ;
   formalT := getSType (formal) ;
   actualT := getSType (actual) ;
   tinfo^.format := collapseString (format) ;
   tinfo^.token := token ;
   tinfo^.kind := parameter ;
   tinfo^.actual := actual ;
   tinfo^.formal := formal ;
   tinfo^.procedure := procedure ;
   tinfo^.nth := nth ;
   tinfo^.isvar := isvar ;
   tinfo^.error := NIL ;
   tinfo^.left := formalT ;
   tinfo^.right := actualT ;
   tinfo^.checkFunc := determineCompatible ;
   tinfo^.visited := InitIndex (1) ;
   tinfo^.resolved := InitIndex (1) ;
   tinfo^.unresolved := InitIndex (1) ;
   tinfo^.strict := FALSE ;
   tinfo^.isin := FALSE ;
   include (tinfo^.unresolved, actual, formal, unknown) ;
   IF debugging
   THEN
      dumptInfo (tinfo)
   END ;
   IF doCheck (tinfo)
   THEN
      deconstruct (tinfo) ;
      RETURN TRUE
   ELSE
      deconstruct (tinfo) ;
      RETURN FALSE
   END
END ParameterTypeCompatible ;


(*
   doExpressionTypeCompatible -
*)

PROCEDURE doExpressionTypeCompatible (token: CARDINAL; format: ARRAY OF CHAR;
                                      left, right: CARDINAL;
                                      strict: BOOLEAN) : BOOLEAN ;
VAR
   tinfo: tInfo ;
BEGIN
   tinfo := newtInfo () ;
   tinfo^.format := collapseString (format) ;
   tinfo^.token := token ;
   tinfo^.kind := expression ;
   tinfo^.actual := NulSym ;
   tinfo^.formal := NulSym ;
   tinfo^.procedure := NulSym ;
   tinfo^.nth := 0 ;
   tinfo^.isvar := FALSE ;
   tinfo^.error := NIL ;
   tinfo^.left := left ;
   tinfo^.right := right ;
   tinfo^.checkFunc := determineCompatible ;
   tinfo^.visited := InitIndex (1) ;
   tinfo^.resolved := InitIndex (1) ;
   tinfo^.unresolved := InitIndex (1) ;
   tinfo^.strict := strict ;
   tinfo^.isin := FALSE ;
   include (tinfo^.unresolved, left, right, unknown) ;
   IF doCheck (tinfo)
   THEN
      deconstruct (tinfo) ;
      RETURN TRUE
   ELSE
      deconstruct (tinfo) ;
      RETURN FALSE
   END
END doExpressionTypeCompatible ;


(*
   ExpressionTypeCompatible - returns TRUE if the expressions, left and right,
                              are expression compatible.
*)

PROCEDURE ExpressionTypeCompatible (token: CARDINAL; format: ARRAY OF CHAR;
                                    left, right: CARDINAL;
                                    strict, isin: BOOLEAN) : BOOLEAN ;
BEGIN
   IF (left#NulSym) AND (right#NulSym)
   THEN
      IF isin
      THEN
         IF IsConst (right) OR IsVar (right)
         THEN
            right := getSType (right)
         END ;
         IF IsSet (right)
         THEN
            right := getSType (right)
         END
      END
   END ;
   RETURN doExpressionTypeCompatible (token, format, left, right, strict)
END ExpressionTypeCompatible ;


(*
   init - initialise all global data structures for this module.
*)

PROCEDURE init ;
BEGIN
   pairFreeList  := NIL ;
   tinfoFreeList := NIL ;
   errors        := InitIndex (1)
END init ;


BEGIN
   init
END M2Check.
