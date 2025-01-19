(* M2Const.mod maintain and resolve the types of constants.

Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Const ;

(*
CONST
   Debugging   = FALSE ;
   DebugConsts = FALSE ;

TYPE
   constList = POINTER TO cList ;
   cList     = RECORD
                  constsym : CARDINAL ;
                  constmeta: constType ;
                  expr     : CARDINAL ;
                  type     : CARDINAL ;
                  next     : constList ;
               END ;


VAR
   headOfConsts: constList ;


PROCEDURE stop ; BEGIN END stop ;


(*
   addToConstList - add a constant, sym, to the head of the constants list.
*)

PROCEDURE addToConstList (sym: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      IF h^.constsym=sym
      THEN
         InternalError ('should never see the same symbol id declared twice')
      END ;
      h := h^.next
   END ;
   NEW(h) ;
   WITH h^ DO
      constsym  := sym ;
      constmeta := unknown ;
      expr      := NulSym ;
      type      := NulSym ;
      next      := headOfConsts
   END ;
   headOfConsts := h
END addToConstList ;


(*
   FixupConstAsString - fixes up a constant, sym, which will have the string type.
*)

PROCEDURE FixupConstAsString (sym: CARDINAL) ;
BEGIN
   fixupConstMeta(sym, str)
END FixupConstAsString ;


(*
   FixupConstType - fixes up a constant, sym, which will have the type, consttype.
*)

PROCEDURE FixupConstType (sym: CARDINAL; consttype: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            IF constmeta=str
            THEN
               InternalError ('cannot fix up a constant to have a type if it is already known as a string')
            END ;
            type := consttype ;
            PutConst(sym, consttype) ;
            RETURN
         END
      END ;
      h := h^.next
   END
END FixupConstType ;


(*
   FixupProcedureType - creates a proctype from a procedure.
*)

PROCEDURE FixupProcedureType (p: CARDINAL) : CARDINAL ;
VAR
   par,
   t   : CARDINAL ;
   n, i: CARDINAL ;
BEGIN
   IF IsProcedure(p)
   THEN
      t := MakeProcType(CheckAnonymous(NulName)) ;
      i := 1 ;
      n := NoOfParam(p) ;
      WHILE i<=n DO
         par := GetParam(p, i) ;
         IF IsParameterVar(par)
         THEN
            PutProcTypeVarParam(t, GetType(par), IsParameterUnbounded(par))
         ELSE
            PutProcTypeParam(t, GetType(par), IsParameterUnbounded(par))
         END ;
         INC(i)
      END ;
      IF GetType(p)#NulSym
      THEN
         PutFunction(t, GetType(p))
      END ;
      RETURN( t )
   ELSE
      InternalError ('expecting a procedure')
   END ;
   RETURN( NulSym )
END FixupProcedureType ;


(*
   FixupConstProcedure - fixes up a constant, sym, which will be equivalent to e.
*)

PROCEDURE FixupConstProcedure (sym: CARDINAL; e: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            expr := e ;
            type := FixupProcedureType(e) ;
            PutConst(sym, type) ;
            RETURN
         END
      END ;
      h := h^.next
   END
END FixupConstProcedure ;


(*
   FixupConstExpr - fixes up a constant, sym, which will be equivalent to e.
*)

PROCEDURE FixupConstExpr (sym: CARDINAL; e: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            expr := e ;
            RETURN
         END
      END ;
      h := h^.next
   END
END FixupConstExpr ;


(*
   fixupConstMeta - fixes up symbol, sym, to have the, meta, constType.
*)

PROCEDURE FixupConstMeta (sym: CARDINAL; meta: constType) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            constmeta := meta ;
            RETURN
         END
      END ;
      h := h^.next
   END
END FixupConstMeta ;


(*
   fixupConstCast -
*)

PROCEDURE fixupConstCast (sym: CARDINAL; castType: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            type := castType ;
            RETURN
         END
      END ;
      h := h^.next
   END
END fixupConstCast ;


(*
   findConstType -
*)

PROCEDURE findConstType (sym: CARDINAL) : CARDINAL ;
VAR
   h: constList ;
   t: CARDINAL ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            t := GetType(sym) ;
            IF t=NulSym
            THEN
               RETURN( NulSym )
            ELSE
               RETURN( t )
            END
         END
      END ;
      h := h^.next
   END ;
   RETURN( NulSym )
END findConstType ;


(*
   findConstMeta -
*)

PROCEDURE findConstMeta (sym: CARDINAL) : constType ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            RETURN( constmeta )
         END
      END ;
      h := h^.next
   END ;
   RETURN( unknown )
END findConstMeta ;


(*
   ReportUnresolvedConstTypes - emits an error message for any unresolved constant type.
*)

PROCEDURE ReportUnresolvedConstTypes ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF (constmeta#unknown) AND (constmeta#str) AND (type=NulSym)
         THEN
            MetaError1('unable to resolve the type of the constant {%1Dad}', h^.constsym)
         END
      END ;
      h := h^.next
   END
END ReportUnresolvedConstTypes ;


(*
   DebugMeta -
*)

PROCEDURE DebugMeta (h: constList) ;
VAR
   n: Name ;
BEGIN
   IF DebugConsts
   THEN
      WITH h^ DO
         n := GetSymName(constsym) ;
         printf1('constant %a ', n) ;
         IF type=NulSym
         THEN
            printf0('type is unknown\n')
         ELSE
            printf0('type is known\n')
         END
      END
   END
END DebugMeta ;


(*
   constTypeResolved -
*)

PROCEDURE constTypeResolved (h: constList) : BOOLEAN ;
BEGIN
   RETURN( h^.type#NulSym )
END constTypeResolved ;


(*
   constExprResolved -
*)

PROCEDURE constExprResolved (h: constList) : BOOLEAN ;
BEGIN
   RETURN( h^.expr#NulSym )
END constExprResolved ;


(*
   findConstMetaExpr -
*)

PROCEDURE findConstMetaExpr (h: constList) : constType ;
BEGIN
   RETURN( h^.constmeta )
END findConstMetaExpr ;


(*
   constResolveViaMeta -
*)

PROCEDURE constResolveViaMeta (h: constList) : BOOLEAN ;
VAR
   n: Name ;
BEGIN
   WITH h^ DO
      IF findConstMetaExpr(h)=str
      THEN
         PutConstStringKnown (constsym, MakeKey(''), FALSE, FALSE) ;
         IF DebugConsts
         THEN
            n := GetSymName(constsym) ;
            printf1('resolved constant %a as a string\n', n)
         END ;
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END constResolveViaMeta ;


(*
   constResolvedViaType -
*)

PROCEDURE constResolvedViaType (h: constList) : BOOLEAN ;
VAR
   n: Name ;
BEGIN
   WITH h^ DO
      type := findConstType(expr) ;
      IF type#NulSym
      THEN
         PutConst(constsym, type) ;
         IF DebugConsts
         THEN
            n := GetSymName(constsym) ;
            printf1('resolved type of constant %a\n', n)
         END ;
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END constResolvedViaType ;


(*
   resolveConstType -
*)

PROCEDURE resolveConstType (h: constList) : BOOLEAN ;
BEGIN
   WITH h^ DO
      IF (constmeta=unknown) OR (constmeta=str)
      THEN
         (* do nothing *)
      ELSE
         DebugMeta(h) ;
         IF constTypeResolved(h)
         THEN
            (* nothing to do *)
         ELSE
            IF constExprResolved(h)
            THEN
               IF constResolveViaMeta(h)
               THEN
                  RETURN( TRUE )
               ELSIF constResolvedViaType(h)
               THEN
                  RETURN( TRUE )
               END
            END
         END
      END
   END ;
   RETURN( FALSE )
END resolveConstType ;


(*
   ResolveConstTypes - resolves the types of all aggegrate constants.
*)

PROCEDURE ResolveConstTypes ;
VAR
   h      : constList ;
   changed: BOOLEAN ;
BEGIN
   REPEAT
      changed := FALSE ;
      h := headOfConsts ;
      WHILE h#NIL DO
         changed := resolveConstType(h) ;
         h := h^.next
      END
   UNTIL NOT changed ;
   ReportUnresolvedConstTypes
END ResolveConstTypes ;


(*
   SkipConst - returns the symbol which is a pseudonum of, sym.
*)

PROCEDURE SkipConst (sym: CARDINAL) : CARDINAL ;
VAR
   init: CARDINAL ;
   h   : constList ;
BEGIN
   init := sym ;
   h := headOfConsts ;
   WHILE h#NIL DO
      IF (h^.constsym=sym) AND (h^.expr#NulSym)
      THEN
         sym := h^.expr ;
         IF sym=init
         THEN
            (* circular definition found *)
            RETURN( sym )
         END ;
         h := headOfConsts
      ELSE
         h := h^.next
      END
   END ;
   RETURN( sym )
END SkipConst ;


BEGIN
   headOfConsts := NIL
*)
BEGIN
END M2Const.
