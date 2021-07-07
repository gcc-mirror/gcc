(* M2DebugStack.mod display parameter stack.

Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2DebugStack ;

FROM DynamicStrings IMPORT InitString, KillString, Dup, Index, Slice, char,
                           ConCat, ConCatChar, InitStringCharStar, Length, Mark ;

FROM SymbolTable IMPORT IsConstLit, IsConstSet, IsConstructor, IsConst,
                        IsArray, IsVar, IsEnumeration, IsFieldEnumeration,
                        IsUnbounded, IsProcType, IsProcedure, IsPointer, IsParameter,
                        IsParameterVar, IsType, IsRecord, IsRecordField, IsVarient,
                        IsModule, IsDefImp, IsSet, IsSubrange, GetSymName, NulSym ;

FROM StringConvert IMPORT CardinalToString ;
FROM NameKey IMPORT Name, KeyToCharStar ;
FROM FIO IMPORT File, StdOut ;
FROM SFIO IMPORT WriteS ;
FROM M2Error IMPORT InternalError ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;

CONST
   Debugging = FALSE ;

VAR
   OperandTok,
   OperandT,
   OperandF,
   OperandA,
   OperandD,
   OperandRW  : ProcedureWord ;
   OperandAnno: ProcedureString ;


(*
   x - checks to see that a=b.
*)

PROCEDURE x (a, b: String) : String ;
BEGIN
   IF a#b
   THEN
      InternalError ('different string returned')
   END ;
   RETURN( a )
END x ;


(*
   IsWhite - returns TRUE if, ch, is a space.
*)

PROCEDURE IsWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( ch=' ' )
END IsWhite ;


(*
   ConCatWord - joins sentances, a, b, together.
*)

PROCEDURE ConCatWord (a, b: String) : String ;
BEGIN
   IF (Length(a)=1) AND (char(a, 0)='a')
   THEN
      a := x(a, ConCatChar(a, 'n'))
   ELSIF (Length(a)>1) AND (char(a, -1)='a') AND IsWhite(char(a, -2))
   THEN
      a := x(a, ConCatChar(a, 'n'))
   END ;
   IF (Length(a)>0) AND (NOT IsWhite(char(a, -1)))
   THEN
      a := x(a, ConCatChar(a, ' '))
   END ;
   RETURN( x(a, ConCat(a, b)) )
END ConCatWord ;


(*
   symDesc -
*)

PROCEDURE symDesc (sym: CARDINAL; o: String) : String ;
BEGIN
   IF sym = NulSym
   THEN
      RETURN( ConCatWord(o, Mark(InitString('NulSym'))) )
   ELSIF IsConstLit(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('constant literal'))) )
   ELSIF IsConstSet(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('constant set'))) )
   ELSIF IsConstructor(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('constructor'))) )
   ELSIF IsConst(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('constant'))) )
   ELSIF IsArray(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('array'))) )
   ELSIF IsVar(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('variable'))) )
   ELSIF IsEnumeration(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('enumeration type'))) )
   ELSIF IsFieldEnumeration(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('enumeration field'))) )
   ELSIF IsUnbounded(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('unbounded parameter'))) )
   ELSIF IsProcType(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('procedure type'))) )
   ELSIF IsProcedure(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('procedure'))) )
   ELSIF IsPointer(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('pointer'))) )
   ELSIF IsParameter(sym)
   THEN
      IF IsParameterVar(sym)
      THEN
         RETURN( ConCatWord(o, Mark(InitString('var parameter'))) )
      ELSE
         RETURN( ConCatWord(o, Mark(InitString('parameter'))) )
      END
   ELSIF IsType(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('type'))) )
   ELSIF IsRecord(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('record'))) )
   ELSIF IsRecordField(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('record field'))) )
   ELSIF IsVarient(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('varient record'))) )
   ELSIF IsModule(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('module'))) )
   ELSIF IsDefImp(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('definition or implementation module'))) )
   ELSIF IsSet(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('set'))) )
   ELSIF IsSubrange(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('subrange'))) )
   ELSE
      RETURN( o )
   END
END symDesc ;


(*
   Output - output string, s, to Stdout.  It also disposes of the string, s.
*)

PROCEDURE Output (s: String) ;
BEGIN
   s := WriteS(StdOut, s) ;
   s := KillString(s)
END Output ;


(*
   GetComment -
*)

PROCEDURE GetComment (s: String) : INTEGER ;
VAR
   c: INTEGER ;
BEGIN
   c := Index(s, '|', 0) ;
   WHILE c>=0 DO
      INC(c) ;
      IF c>=VAL(INTEGER, Length(s))
      THEN
         RETURN -1
      ELSIF char(s, c)='|'
      THEN
         RETURN c+1
      END ;
      c := Index(s, '|', c)
   END ;
   RETURN -1
END GetComment ;


(*
   doName - concatenate namekey, o, to, p.
*)

PROCEDURE doName (p: String; o: WORD) : String ;
BEGIN
   RETURN ConCat(p, InitStringCharStar(KeyToCharStar(o))) ;
END doName ;


(*
   doSymName - concatenate symbol, o, name to, p.
*)

PROCEDURE doSymName (p: String; o: WORD) : String ;
BEGIN
   RETURN ConCat(p, InitStringCharStar(KeyToCharStar(GetSymName(o)))) ;
END doSymName ;


(*
   doNumber - convert, o, to a cardinal and increment the length, l,
              by the number of characters required to represent, o.
*)

PROCEDURE doNumber (p: String; o: WORD) : String ;
BEGIN
   RETURN ConCat(p, CardinalToString(VAL(CARDINAL, o), 0, ' ', 10, TRUE))
END doNumber ;


(*
   doSymbol - handles a symbol indicated by, o.
*)

PROCEDURE doSymbol (p: String; o: WORD) : String ;
BEGIN
   RETURN symDesc(o, p)
END doSymbol ;


(*
   doOperand -
*)

PROCEDURE doOperand (p, s: String; VAR i: INTEGER; e: INTEGER; o: WORD) : String ;
BEGIN
   INC(i) ;
   IF i<e
   THEN
      CASE char(s, i) OF

      's':  (* symbol number *)
            INC(i) ;
            RETURN doSymbol(p, o) |
      'd':  (* decimal number *)
            INC(i) ;
            RETURN doNumber(p, o) |
      'a':  (* symbol name key *)
            INC(i) ;
            RETURN doSymName(p, o) |
      'n':  (* ascii name key *)
            INC(i) ;
            RETURN doName(p, o)

      ELSE
         InternalError ("incorrect format specifier expecting one of 's', 'd' or 'a'")
      END
   END ;
   RETURN p
END doOperand ;


(*
   doPercent -
*)

PROCEDURE doPercent (o, s: String;
                     VAR i: INTEGER; e: INTEGER; n: CARDINAL) : String ;
BEGIN
   INC(i) ;
   IF i<e
   THEN
      CASE char(s, i) OF

      '1':  RETURN doOperand(o, s, i, e, OperandT(n)) |
      '2':  RETURN doOperand(o, s, i, e, OperandF(n)) |
      '3':  RETURN doOperand(o, s, i, e, OperandTok(n))

      ELSE
         InternalError ('unrecognised format specifier - expecting 1, 2 or 3 after the %')
      END
   END ;
   InternalError ('end of field found before format specifier - expecting 1, 2 or 3 after the %')
END doPercent ;


(*
   doNameLength - increment, l, by the ascii length of string determined by, o.
*)

PROCEDURE doNameLength (VAR l: CARDINAL; o: WORD) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar(KeyToCharStar(o)) ;
   INC(l, Length(s)) ;
   s := KillString(s)
END doNameLength ;


(*
   doSymNameLength - increment, l, by the ascii length of symbol, o.
*)

PROCEDURE doSymNameLength (VAR l: CARDINAL; o: WORD) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar(KeyToCharStar(GetSymName(o))) ;
   INC(l, Length(s)) ;
   s := KillString(s)
END doSymNameLength ;


(*
   doNumberLength - convert, o, to a cardinal and increment the length, l,
                    by the number of characters required to represent, o.
*)

PROCEDURE doNumberLength (VAR l: CARDINAL; o: WORD) ;
VAR
   s: String ;
BEGIN
   s := CardinalToString(VAL(CARDINAL, o), 0, ' ', 10, TRUE) ;
   INC(l, Length(s)) ;
   s := KillString(s)
END doNumberLength ;


(*
   doSymbolLength - handles a symbol indicated by, o.
*)

PROCEDURE doSymbolLength (VAR l: CARDINAL; o: WORD) ;
VAR
   s: String ;
BEGIN
   s := symDesc(o, InitString('')) ;
   INC(l, Length(s)) ;
   s := KillString(s)
END doSymbolLength ;


(*
   doOperandLength -
*)

PROCEDURE doOperandLength (s: String; VAR i: INTEGER; e: INTEGER; VAR l: CARDINAL; o: WORD) ;
BEGIN
   INC(i) ;
   IF i<e
   THEN
      CASE char(s, i) OF

      's':  (* symbol number *)
            INC(i) ;
            doSymbolLength(l, o) |
      'd':  (* decimal number *)
            INC(i) ;
            doNumberLength(l, o) |
      'a':  (* ascii name key *)
            INC(i) ;
            doSymNameLength(l, o) |
      'n':  (* ascii name key *)
            INC(i) ;
            doNameLength(l, o)

      ELSE
         InternalError ("incorrect format specifier expecting one of 's', 'd' or 'a'")
      END
   END
END doOperandLength ;


(*
   doPercentLength -
*)

PROCEDURE doPercentLength (s: String; VAR i: INTEGER; e: INTEGER;
                           VAR l: CARDINAL; n: CARDINAL) ;
BEGIN
   INC(i) ;
   IF i<e
   THEN
      CASE char(s, i) OF

      '1':  doOperandLength(s, i, e, l, OperandT(n)) |
      '2':  doOperandLength(s, i, e, l, OperandF(n)) |
      '3':  doOperandLength(s, i, e, l, OperandTok(n)) |

      ELSE
         InternalError ('unrecognised format specifier - expecting 1, 2 or 3 after the %')
      END
   END
END doPercentLength ;


(*
   doFieldLength - compute the string length given in annotation
                   at position, n, on the stack between characters
                   b and e.

                   The string description between: b..e can contain any
                   of these patterns:

                   %a           ascii name key.
                   %s           symbol number.
                   %d           decimal cardinal number.
                   |            indicates the next field.
*)

PROCEDURE doFieldLength (b, e: INTEGER; n: CARDINAL) : CARDINAL ;
VAR
   l: CARDINAL ;
   i: INTEGER ;
   s: String ;
BEGIN
   IF b=-1
   THEN
      RETURN( 0 )
   END ;
   s := OperandAnno(n) ;
   IF e=-1
   THEN
      e := Length(s)
   END ;
   l := 0 ;
   i := b ;
   WHILE i<e DO
      CASE char(s, i) OF

      '|':  RETURN l |
      '%':  doPercentLength(s, i, e, l, n) ;

      ELSE
         INC(l)
      END ;
      INC(i)
   END ;
   RETURN l
END doFieldLength ;


(*
   stop -
*)

PROCEDURE stop ;
BEGIN
END stop ;


(*
   doMaxCard - returns the maximum of two CARDINALs.
*)

PROCEDURE doMaxCard (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF (a>100) OR (b>100)
   THEN
      stop
   END ;
   IF a>b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END doMaxCard ;


(*
   GetAnnotationFieldLength -
*)

PROCEDURE GetAnnotationFieldLength (n: CARDINAL; f: CARDINAL) : CARDINAL ;
VAR
   c, e: INTEGER ;
BEGIN
   c := GetComment(OperandAnno(n)) ;
   IF c>0
   THEN
      IF Debugging
      THEN
         printf0('full anno is: ') ; Output(Dup(OperandAnno(n))) ; printf0('\n') ;
         printf0('comment field is: ') ; Output(Slice(OperandAnno(n), c, 0)) ; printf0('\n')
      END ;
      e := Index(OperandAnno(n), '|', c) ;
      IF f=0
      THEN
         RETURN doFieldLength(c, e, n)
      ELSE
         IF e>=0
         THEN
            INC(e)
         END ;
         RETURN doFieldLength(e, -1, n)
      END
   ELSE
      RETURN 0
   END
END GetAnnotationFieldLength ;


(*
   GetAnnotationLength -
*)

PROCEDURE GetAnnotationLength (n: CARDINAL; f: CARDINAL) : CARDINAL ;
VAR
   l: CARDINAL ;
BEGIN
   IF OperandAnno(n)=NIL
   THEN
      l := 0 ;
      IF f=0
      THEN
         doNumberLength(l, OperandT(n))
      ELSE
         doNumberLength(l, OperandF(n))
      END ;
      RETURN l
   ELSE
      RETURN GetAnnotationFieldLength(n, f)
   END
END GetAnnotationLength ;


(*
   GetFieldLength - returns the number of characters used in field, f,
                    at position, n, on the stack.
*)

PROCEDURE GetFieldLength (n: CARDINAL; f: CARDINAL) : CARDINAL ;
VAR
   c, b, e: INTEGER ;
BEGIN
   c := GetComment(OperandAnno(n)) ;
   IF c>1
   THEN
      e := c-2
   ELSE
      e := Length(OperandAnno(n))
   END ;
   IF f=0
   THEN
      b := 0
   ELSE
      b := Index(OperandAnno(n), '|', 0) ;
      IF b=-1
      THEN
         RETURN 0
      ELSE
         INC(b)
      END
   END ;
   RETURN doFieldLength(b, e, n)
END GetFieldLength ;


(*
   GetMaxFieldAnno - returns the maximum number of characters required
                     by either the annotation or field, f, at position, n,
                     on the stack.
*)

PROCEDURE GetMaxFieldAnno (n: CARDINAL; f: CARDINAL) : CARDINAL ;
BEGIN
   RETURN doMaxCard(GetAnnotationLength(n, f), GetFieldLength(n, f))
END GetMaxFieldAnno ;


(*
   GetStackFieldLengths - assigns, tn, and, fn, with the
                          maximum field width values.
*)

PROCEDURE GetStackFieldLengths (VAR tn, fn, tk: CARDINAL; amount: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   tn := 0 ;
   fn := 0 ;
   tk := 0 ;
   WHILE i<=amount DO
      tn := doMaxCard(tn, GetMaxFieldAnno(i, 0)) ;
      fn := doMaxCard(fn, GetMaxFieldAnno(i, 1)) ;
      tk := doMaxCard(tk, GetMaxFieldAnno(i, 2)) ;
      INC(i)
   END
END GetStackFieldLengths ;


(*
   DisplayRow -
*)

PROCEDURE DisplayRow (tn, fn, tk: CARDINAL; initOrFinal: BOOLEAN) ;
VAR
   i: CARDINAL ;
BEGIN
   printf0('+-') ;
   FOR i := 1 TO tn DO
      printf0('-')
   END ;
   IF (fn=0) AND (tk=0)
   THEN
      IF initOrFinal
      THEN
         printf0('-+-')
      ELSE
         printf0('-|-')
      END
   ELSE
      IF initOrFinal
      THEN
         printf0('-+-')
      ELSE
         printf0('-|-')
      END ;
      IF fn#0
      THEN
         FOR i := 1 TO fn DO
            printf0('-')
         END
      END ;
      IF initOrFinal
      THEN
         printf0('-+-')
      ELSE
         printf0('-|-')
      END ;
      IF tk#0
      THEN
         FOR i := 1 TO tk DO
            printf0('-')
         END ;
         printf0('-+\n')
      END
   END
END DisplayRow ;


(*
   SkipToField -
*)

PROCEDURE SkipToField (s: String; n: CARDINAL) : INTEGER ;
VAR
   i, h: INTEGER ;
BEGIN
   i := 0 ;
   h := Length(s) ;
   WHILE (n>0) AND (i<h) DO
      IF Index(s, '|', i)>0
      THEN
         DEC(n) ;
         IF (i<h) AND (char(s, i+1)='|')
         THEN
            (* comment seen, no field available *)
            RETURN -1
         END ;
         i := Index(s, '|', i)
      ELSE
         RETURN -1
      END ;
      INC(i)
   END ;
   IF i=h
   THEN
      i := -1
   END ;
   RETURN i
END SkipToField ;


(*
   Pad - padds out string, s, to paddedLength characters.
*)

PROCEDURE Pad (o: String; paddedLength: CARDINAL) : String ;
VAR
   i: CARDINAL ;
BEGIN
   i := Length(o) ;
   IF i<paddedLength
   THEN
      REPEAT
         o := ConCatChar(o, ' ') ;
         INC(i)
      UNTIL i=paddedLength
   END ;
   RETURN o
END Pad ;


(*
   doField - compute the string length given in annotation
             at position, n, on the stack between characters
             b and e.

             The string description between: b..e can contain any
             of these patterns:

             %a           ascii name key.
             %s           symbol number.
             %d           decimal cardinal number.
             |            indicates the next field.
*)

PROCEDURE doField (s: String; n: CARDINAL; f: CARDINAL; l: CARDINAL) : String ;
VAR
   h, i, j: INTEGER ;
   o      : String ;
BEGIN
   h := Length(s) ;
   i := SkipToField(s, f) ;
   o := InitString('') ;
   IF i>=0
   THEN
      j := SkipToField(s, f+1) ;
      IF j=-1
      THEN
         j := h
      END ;
      WHILE i<h DO
         CASE char(s, i) OF

         '|':  i := h |
         '%':  o := doPercent(o, s, i, h, n)

         ELSE
            o := ConCatChar(o, char(s, i)) ;
            INC(i)
         END
      END
   END ;
   o := Pad(o, l) ;
   RETURN o
END doField ;


(*
   doAnnotation -
*)

PROCEDURE doAnnotation (s: String; n: CARDINAL;
                        field: CARDINAL; width: CARDINAL) : String ;
VAR
   c    : INTEGER ;
   cf, o: String ;
BEGIN
   c := GetComment(s) ;
   IF c>=0
   THEN
      cf := Slice(s, c, 0) ;
      o := doField(cf, n, field, width) ;
      cf := KillString(cf) ;
      RETURN o
   ELSE
      RETURN InitString('')
   END
END doAnnotation ;


(*
   DisplayFields -
*)

PROCEDURE DisplayFields (n: CARDINAL; tn, fn, tk: CARDINAL) ;
VAR
   s      : String ;
   t, f, k: CARDINAL ;
BEGIN
   s := OperandAnno(n) ;
   IF s=NIL
   THEN
      t := OperandT(n) ;
      f := OperandF(n) ;
      k := OperandTok(n) ;
      printf0('| ') ;
      Output(Pad(CardinalToString(VAL(CARDINAL, t), 0, ' ', 10, TRUE), tn)) ;
      printf0(' | ') ;
      Output(Pad(CardinalToString(VAL(CARDINAL, f), 0, ' ', 10, TRUE), fn)) ;
      printf0(' | ') ;
      Output(Pad(CardinalToString(VAL(CARDINAL, k), 0, ' ', 10, TRUE), tk)) ;
      printf0(' |\n')
   ELSE
      IF tn>0
      THEN
         printf0('| ') ;
         Output(doField(s, n, 0, tn))
      END ;
      IF fn>0
      THEN
         printf0(' | ') ;
         Output(doField(s, n, 1, fn))
      END ;
      IF tk>0
      THEN
         printf0(' | ') ;
         Output(doField(s, n, 2, tk))
      END ;
      printf0(' |\n') ;
      IF tn>0
      THEN
         printf0('| ') ;
         Output(doAnnotation(s, n, 0, tn))
      END ;
      IF fn>0
      THEN
         printf0(' | ') ;
         Output(doAnnotation(s, n, 1, fn))
      END ;
      IF tk>0
      THEN
         printf0(' | ') ;
         Output(doAnnotation(s, n, 2, tk))
      END ;
      printf0(' |\n')
   END
END DisplayFields ;


(*
   DebugStack - displays the stack.
*)

PROCEDURE DebugStack (amount: CARDINAL;
                      opt, opf, opa, opd, oprw, optk: ProcedureWord;
                      opanno: ProcedureString) ;
VAR
   i         : CARDINAL ;
   tn, fn, tk: CARDINAL ;
BEGIN
   OperandT := opt ;
   OperandF := opf ;
   OperandA := opa ;
   OperandD := opd ;
   OperandRW := oprw ;
   OperandAnno := opanno ;
   OperandTok := optk ;
   GetStackFieldLengths(tn, fn, tk, amount) ;
   i := 1 ;
   WHILE i<=amount DO
      IF i=1
      THEN
         DisplayRow(tn, fn, tk, TRUE)
      END ;
      DisplayFields(i, tn, fn, tk) ;
      DisplayRow(tn, fn, tk, i=amount) ;
      INC(i)
   END
END DebugStack ;


END M2DebugStack.
