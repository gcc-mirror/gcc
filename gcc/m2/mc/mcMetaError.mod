(* Copyright (C) 2015
                 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE mcMetaError ;


FROM nameKey IMPORT Name, keyToCharStar, NulName ;
FROM StrLib IMPORT StrLen ;
FROM mcLexBuf IMPORT getTokenNo ;
FROM mcError IMPORT error, newError, newWarning, errorString, internalError, chainError, flushErrors ;
FROM FIO IMPORT StdOut, WriteLine ;
FROM SFIO IMPORT WriteS ;
FROM StringConvert IMPORT ctos ;
FROM varargs IMPORT vararg ;

IMPORT varargs ;

FROM DynamicStrings IMPORT String, InitString, InitStringCharStar,
                           ConCat, ConCatChar, Mark, string, KillString,
                           Dup, char, Length, Mult ;

FROM decl IMPORT node, isType, isTemporary, getType, getSymName, getScope, isDef,
                 isExported, isZtype, isRtype, skipType, getDeclaredMod, getDeclaredDef,
		 getFirstUsed, isLiteral, isConst, isConstSet, isArray, isVar,
		 isEnumeration, isEnumerationField, isUnbounded, isProcType, isProcedure,
		 isPointer, isParameter, isVarParam, isRecord, isRecordField,
		 isVarient, isModule, isImp, isSet, isSubrange ;

TYPE
   errorType = (newerror, newwarning, chained) ;


(*
   ebnf := { percent
             | lbra
             | any                  % copy ch %
           }
         =:

   percent := '%' anych             % copy anych %
            =:

   lbra := '{' [ '!' ] percenttoken '}' =:

   percenttoken := '%' (
                         '1'        % doOperand(1) %
                             op
                       | '2'        % doOperand(2) %
                             op
                       | '3'        % doOperand(3) %
                             op
                       | '4'        % doOperand(4) %
                             op
                       )
                       } =:

   op := {'a'|'q'|'t'|'d'|'n'|'s'|'D'|'I'|'U'|'E'|'W'} then =:

   then := [ ':' ebnf ] =:
*)


(*
   internalFormat - produces an informative internal error.
*)

PROCEDURE internalFormat (s: String; i: INTEGER; m: ARRAY OF CHAR) ;
VAR
   e: error ;
BEGIN
   e := newError (getTokenNo()) ;
   s := WriteS (StdOut, s) ;
   WriteLine (StdOut) ;
   s := KillString (s) ;
   IF i>0
   THEN
      DEC(i)
   END ;
   s := Mult (InitString (' '), i) ;
   s := ConCatChar (s, '^') ;
   s := WriteS (StdOut, s) ;
   WriteLine (StdOut) ;
   internalError (m, __FILE__, __LINE__)
END internalFormat ;


(*
   x - checks to see that a=b.
*)

PROCEDURE x (a, b: String) : String ;
BEGIN
   IF a#b
   THEN
      internalError('different string returned', __FILE__, __LINE__)
   END ;
   RETURN a
END x ;


(*
   isWhite - returns TRUE if, ch, is a space.
*)

PROCEDURE isWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN ch=' '
END isWhite ;


(*
   then := [ ':' ebnf ] =:
*)

PROCEDURE then (VAR e: error; VAR t: errorType;
                VAR r: String; s: String;
                sym: vararg;
                VAR i: INTEGER; l: INTEGER;
                o: String; positive: BOOLEAN) ;
BEGIN
   IF char (s, i) = ':'
   THEN
      INC (i) ;
      ebnf (e, t, r, s, sym, i, l) ;
      IF (i<l) AND (char (s, i) # '}')
      THEN
         internalFormat (s, i, 'expecting to see }')
      END
   END
END then ;


(*
   doNumber -
*)

PROCEDURE doNumber (bol: CARDINAL;
                    sym: vararg; o: String;
                    VAR quotes: BOOLEAN) : String ;
VAR
   c: CARDINAL ;
BEGIN
   IF Length(o) > 0
   THEN
      RETURN o
   ELSE
      quotes := FALSE ;
      varargs.next (sym, bol) ;
      varargs.arg (sym, c) ;
      RETURN ConCat (o, ctos (c, 0, ' '))
   END
END doNumber ;


(*
   doCount -
*)

PROCEDURE doCount (bol: CARDINAL;
                   sym: vararg; o: String;
                   VAR quotes: BOOLEAN) : String ;
VAR
   c: CARDINAL ;
BEGIN
   IF Length(o) > 0
   THEN
      RETURN o
   ELSE
      quotes := FALSE ;
      varargs.next (sym, bol) ;
      varargs.arg (sym, c) ;
      o := ConCat (o, ctos (c, 0, ' ')) ;
      CASE c MOD 100 OF

      11..13:  o := ConCat (o, Mark (InitString ('th')))

      ELSE
         CASE c MOD 10 OF

         1:  o := ConCat (o, Mark (InitString ('st'))) |
         2:  o := ConCat (o, Mark (InitString ('nd'))) |
         3:  o := ConCat (o, Mark (InitString ('rd')))

         ELSE
            o := ConCat (o, Mark (InitString ('th')))
         END
      END ;
      RETURN o
   END
END doCount ;


PROCEDURE doAscii (bol: CARDINAL; sym: vararg; o: String) : String ;
VAR
   n: node ;
BEGIN
   varargs.next (sym, bol) ;
   varargs.arg (sym, n) ;
   IF (Length (o) > 0) OR isTemporary (n)
   THEN
      RETURN o
   ELSE
      RETURN ConCat (o, InitStringCharStar (keyToCharStar (getSymName (n))))
   END
END doAscii ;


PROCEDURE doName (bol: CARDINAL; sym: vararg; o: String; VAR quotes: BOOLEAN) : String ;
VAR
   n: node ;
BEGIN
   varargs.next (sym, bol) ;
   varargs.arg (sym, n) ;
   IF (Length (o) > 0) OR isTemporary (n)
   THEN
      RETURN o
   ELSE
      IF isZtype (n)
      THEN
         quotes := FALSE ;
         RETURN ConCat (o, Mark (InitString ('the ZType')))
      ELSIF isRtype (n)
      THEN
         quotes := FALSE ;
         RETURN ConCat (o, Mark (InitString ('the RType')))
      ELSIF getSymName (n) # NulName
      THEN
         RETURN ConCat (o, InitStringCharStar (keyToCharStar (getSymName (n))))
      ELSE
         RETURN o
      END
   END
END doName ;


PROCEDURE doQualified (bol: CARDINAL; sym: vararg; o: String) : String ;
VAR
   s, n: node ;
   mod : vararg ;
BEGIN
   varargs.next (sym, bol) ;
   varargs.arg (sym, n) ;
   IF (Length (o) > 0) OR isTemporary (n)
   THEN
      RETURN o
   ELSE
      s := getScope (n) ;
      mod := varargs.start1 (s) ;
      IF isDef(s) AND isExported(n)
      THEN
         o := x (o, doAscii (0, mod, o)) ;
         o := x (o, ConCatChar (o, '.')) ;
         o := x (o, ConCat (o, InitStringCharStar (keyToCharStar (getSymName (n)))))
      ELSE
         o := x (o, doAscii (bol, sym, o))
      END ;
      varargs.end (mod) ;
      RETURN o
   END
END doQualified ;


(*
   doType - returns a string containing the type name of
            sym.  It will skip pseudonym types.  It also
            returns the type symbol found.
*)

PROCEDURE doType (bol: CARDINAL;
                  VAR sym: vararg; o: String) : String ;
VAR
   n: node ;
BEGIN
   varargs.next (sym, bol) ;
   varargs.arg (sym, n) ;
   IF (Length (o) > 0) OR (getType (n) = NIL)
   THEN
      RETURN o
   ELSE
      n := skipType (getType (n)) ;
      varargs.next (sym, bol) ;
      varargs.replace (sym, n) ;
      RETURN x (o, doAscii (bol, sym, o))
   END
END doType ;


(*
   doSkipType - will skip all pseudonym types.  It also
                returns the type symbol found and name.
*)

PROCEDURE doSkipType (bol: CARDINAL; VAR sym: vararg; o: String) : String ;
VAR
   n: node ;
BEGIN
   varargs.next (sym, bol) ;
   varargs.arg (sym, n) ;
   IF Length (o) > 0
   THEN
      RETURN o
   ELSE
      n := skipType (getType (n)) ;
      varargs.next (sym, bol) ;
      varargs.replace (sym, n) ;
      IF getSymName(n) = NulName
      THEN
         RETURN o
      ELSE
         RETURN x (o, doAscii (bol, sym, o))
      END
   END
END doSkipType ;


PROCEDURE doKey (bol: CARDINAL; sym: vararg; o: String) : String ;
VAR
   n: Name ;
BEGIN
   IF Length (o) > 0
   THEN
      RETURN o
   ELSE
      varargs.next (sym, bol) ;
      varargs.arg (sym, n) ;
      RETURN ConCat (o, InitStringCharStar (keyToCharStar (n)))
   END
END doKey ;


(*
   doError - creates and returns an error note.
*)

PROCEDURE doError (e: error; t: errorType; tok: CARDINAL) : error ;
BEGIN
   CASE t OF

   chained:    IF e=NIL
               THEN
                  internalError ('should not be chaining an error onto an empty error note', __FILE__, __LINE__)
               ELSE
                  e := chainError (tok, e)
               END |
   newerror:   IF e=NIL
               THEN
                  e := newError (tok)
               END |
   newwarning: IF e=NIL
               THEN
                  e := newWarning (tok)
               END

   ELSE
      internalError ('unexpected enumeration value', __FILE__, __LINE__)
   END ;
   RETURN e
END doError ;


(*
   doDeclaredDef - creates an error note where sym[bol] was declared.
*)

PROCEDURE doDeclaredDef (e: error; t: errorType;
                         bol: CARDINAL;
                         sym: vararg) : error ;
VAR
   n: node ;
BEGIN
   IF bol <= varargs.nargs (sym)
   THEN
      varargs.next (sym, bol) ;
      varargs.arg (sym, n) ;
      e := doError (e, t, getDeclaredDef (n))
   END ;
   RETURN e
END doDeclaredDef ;


(*
   doDeclaredMod - creates an error note where sym[bol] was declared.
*)

PROCEDURE doDeclaredMod (e: error; t: errorType;
                         bol: CARDINAL;
                         sym: vararg) : error ;
VAR
   n: node ;
BEGIN
   IF bol <= varargs.nargs (sym)
   THEN
      varargs.next (sym, bol) ;
      varargs.arg (sym, n) ;
      e := doError (e, t, getDeclaredMod (n))
   END ;
   RETURN e
END doDeclaredMod ;


(*
   doUsed - creates an error note where sym[bol] was first used.
*)

PROCEDURE doUsed (e: error; t: errorType;
                  bol: CARDINAL;
                  sym: vararg) : error ;
VAR
   n: node ;
BEGIN
   IF bol <= varargs.nargs (sym)
   THEN
      varargs.next (sym, bol) ;
      varargs.arg (sym, n) ;
      e := doError (e, t, getFirstUsed (n))
   END ;
   RETURN e
END doUsed ;


(*
   ConCatWord - joins sentances, a, b, together.
*)

PROCEDURE ConCatWord (a, b: String) : String ;
BEGIN
   IF (Length(a) = 1) AND (char (a, 0) = 'a')
   THEN
      a := x (a, ConCatChar (a, 'n'))
   ELSIF (Length(a) > 1) AND (char (a, -1) = 'a') AND isWhite (char (a, -2))
   THEN
      a := x (a, ConCatChar (a, 'n'))
   END ;
   IF (Length(a) > 0) AND (NOT isWhite (char (a, -1)))
   THEN
      a := x (a, ConCatChar (a, ' '))
   END ;
   RETURN x (a, ConCat (a, b))
END ConCatWord ;


(*
   symDesc -
*)

PROCEDURE symDesc (n: node; o: String) : String ;
BEGIN
   IF isLiteral (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('literal')))
   ELSIF isConstSet (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('constant set')))
(*
   ELSIF IsConstructor(n)
   THEN
      RETURN( ConCatWord (o, Mark (InitString ('constructor'))) )
*)
   ELSIF isConst (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('constant')))
   ELSIF isArray (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('array')))
   ELSIF isVar (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('variable')))
   ELSIF isEnumeration (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('enumeration type')))
   ELSIF isEnumerationField (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('enumeration field')))
   ELSIF isUnbounded (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('unbounded parameter')))
   ELSIF isProcType (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('procedure type')))
   ELSIF isProcedure (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('procedure')))
   ELSIF isPointer (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('pointer')))
   ELSIF isParameter (n)
   THEN
      IF isVarParam (n)
      THEN
         RETURN ConCatWord (o, Mark (InitString ('var parameter')))
      ELSE
         RETURN ConCatWord (o, Mark (InitString ('parameter')))
      END
   ELSIF isType (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('type')))
   ELSIF isRecord (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('record')))
   ELSIF isRecordField (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('record field')))
   ELSIF isVarient (n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('varient record')))
   ELSIF isModule(n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('module')))
   ELSIF isDef(n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('definition module')))
   ELSIF isImp(n)
   THEN
      RETURN ConCatWord (o, Mark (InitString ('implementation module')))
   ELSIF isSet (n)
   THEN
      RETURN ConCatWord(o, Mark (InitString ('set')))
   ELSIF isSubrange (n)
   THEN
      RETURN ConCatWord(o, Mark (InitString ('subrange')))
   ELSE
      RETURN o
   END
END symDesc ;


(*
   doDesc -
*)

PROCEDURE doDesc (bol: CARDINAL;
                  sym: vararg; o: String;
                  VAR quotes: BOOLEAN) : String ;
VAR
   n: node ;
BEGIN
   IF Length (o) = 0
   THEN
      varargs.next (sym, bol) ;
      varargs.arg (sym, n) ;
      o := symDesc (n, o) ;
      IF Length (o) > 0
      THEN
         quotes := FALSE
      END
   END ;
   RETURN o
END doDesc ;


(*
   addQuoted - if, o, is not empty then add it to, r.
*)

PROCEDURE addQuoted (r, o: String; quotes: BOOLEAN) : String ;
BEGIN
   IF Length (o) > 0
   THEN
      IF NOT isWhite (char (r, -1))
      THEN
         r := x (r, ConCatChar (r, " "))
      END ;
      IF quotes
      THEN
         r := x (r, ConCatChar (r, "'"))
      END ;
      r := x (r, ConCat (r, o)) ;
      IF quotes
      THEN
         r := x (r, ConCatChar (r, "'"))
      END
   END ;
   RETURN r
END addQuoted ;


(*
   op := {'a'|'q'|'t'|'d'|'k'|'n'|'s'|'D'|'I'|'U'|'E'|'W'} then =:
*)

PROCEDURE op (VAR e: error; VAR t: errorType;
              VAR r: String; s: String;
              sym: vararg;
              VAR i: INTEGER; l: INTEGER;
              bol: CARDINAL; positive: BOOLEAN) ;
VAR
   o     : String ;
   c     : vararg ;
   quotes: BOOLEAN ;
BEGIN
   c := varargs.copy (sym) ;
   o := InitString ('') ;
   quotes := TRUE ;
   WHILE (i<l) AND (char (s, i)#'}') DO
      CASE char(s, i) OF

      'a':  o := x(o, doName (bol, sym, o, quotes)) |
      'q':  o := x(o, doQualified (bol, sym, o)) |
      't':  o := x(o, doType (bol, sym, o)) |
      'd':  o := x(o, doDesc (bol, sym, o, quotes)) |
      'n':  o := x(o, doNumber (bol, sym, o, quotes)) |
      'N':  o := x(o, doCount (bol, sym, o, quotes)) |
      's':  o := x(o, doSkipType (bol, sym, o)) |
      'k':  o := x(o, doKey (bol, sym, o)) |
      'D':  e := doDeclaredDef (e, t, bol, sym) |
      'M':  e := doDeclaredMod (e, t, bol, sym) |
      'U':  e := doUsed (e, t, bol, sym) |
      'E':  t := newerror |
      'W':  t := newwarning |
      ':':  varargs.end (sym) ;
            sym := varargs.copy (c) ;
            then (e, t, r, s, sym, i, l, o, positive) ;
            o := KillString (o) ;
            o := InitString ('') ;
            IF (i<l) AND (char (s, i) # '}')
            THEN
               internalFormat (s, i, 'expecting to see }')
            END ;
            DEC (i)

      ELSE
         internalFormat (s, i, 'expecting one of [aqtdnNsDUEW:]')
      END ;
      INC (i) ;
   END ;
   r := x (r, addQuoted (r, o, quotes)) ;
   o := KillString (o)
END op ;


(*
   percenttoken := '%' (
                         '1'        % doOperand(1) %
                             op
                       | '2'        % doOperand(2) %
                             op
                       | '3'        % doOperand(3) %
                             op
                       | '4'        % doOperand(4) %
                             op
                       )
                       } =:
*)

PROCEDURE percenttoken (VAR e: error; t: errorType;
                        VAR r: String; s: String;
                        sym: vararg;
                        VAR i: INTEGER; l: INTEGER; positive: BOOLEAN) ;
BEGIN
   IF char (s, i) = '%'
   THEN
      INC (i) ;
      CASE char (s, i) OF

      '1':  INC (i) ;
            op (e, t, r, s, sym, i, l, 0, positive) |
      '2':  INC (i) ;
            op (e, t, r, s, sym, i, l, 1, positive) |
      '3':  INC (i) ;
            op (e, t, r, s, sym, i, l, 2, positive) |
      '4':  INC (i) ;
            op (e, t, r, s, sym, i, l, 3, positive)

      ELSE
         internalFormat (s, i, 'expecting one of [123]')
      END ;
      IF (i<l) AND (char (s, i) # '}')
      THEN
         internalFormat (s, i, 'expecting to see }')
      END
   END
END percenttoken ;


(*
   percent := '%' anych           % copy anych %
            =:
*)

PROCEDURE percent (VAR r: String; s: String;
                   sym: vararg;
                   VAR i: INTEGER; l: INTEGER) ;
BEGIN
   IF char(s, i)='%'
   THEN
      INC (i) ;
      IF i<l
      THEN
         r := x (r, ConCatChar (r, char (s, i))) ;
         INC (i)
      END
   END
END percent ;


(*
   lbra := '{' [ '!' ] percenttoken '}' =:
*)

PROCEDURE lbra (VAR e: error; VAR t: errorType;
                VAR r: String; s: String;
                sym: vararg;
                VAR i: INTEGER; l: INTEGER) ;
VAR
   positive: BOOLEAN ;
BEGIN
   IF char (s, i) = '{'
   THEN
      positive := TRUE ;
      INC (i) ;
      IF char (s, i) = '!'
      THEN
         positive := FALSE ;
         INC (i) ;
      END ;
      IF char (s, i) # '%'
      THEN
         internalFormat (s, i, 'expecting to see %')
      END ;
      percenttoken (e, t, r, s, sym, i, l, positive) ;
      IF (i<l) AND (char (s, i) # '}')
      THEN
         internalFormat (s, i, 'expecting to see }')
      END
   END
END lbra ;


PROCEDURE stop ; BEGIN END stop ;

(*
   ebnf := { percent
             | lbra
             | any                    % copy ch %
           }
         =:
*)

PROCEDURE ebnf (VAR e: error; VAR t: errorType;
                VAR r: String; s: String;
                sym: vararg;
                VAR i: INTEGER; l: INTEGER) ;
BEGIN
   WHILE i<l DO
      CASE char(s, i) OF

      '%':  percent (r, s, sym, i, l) |
      '{':  lbra (e, t, r, s, sym, i, l) ;
            IF (i<l) AND (char (s, i) # '}')
            THEN
               internalFormat (s, i, 'expecting to see }')
            END |
      '}':  RETURN

      ELSE
         IF ((isWhite (char(s, i)) AND (Length (r) > 0) AND (NOT isWhite (char (r, -1)))) OR
            (NOT isWhite (char (s, i))))
         THEN
            r := x (r, ConCatChar (r, char (s, i)))
         END
      END ;
      INC (i)
   END
END ebnf ;


(*
   doFormat -
*)

PROCEDURE doFormat (VAR e: error; VAR t: errorType;
                    s: String; sym: vararg) : String ;
VAR
   r   : String ;
   i, l: INTEGER ;
BEGIN
   r := InitString ('') ;
   i := 0 ;
   l := Length (s) ;
   ebnf (e, t, r, s, sym, i, l) ;
   s := KillString (s) ;
   RETURN r
END doFormat ;


PROCEDURE metaErrorStringT1 (tok: CARDINAL; m: String; s: ARRAY OF BYTE) ;
VAR
   str: String ;
   e  : error ;
   sym: vararg ;
   t  : errorType ;
BEGIN
   e := NIL ;
   sym := varargs.start1 (s) ;
   t := newerror ;
   str := doFormat (e, t, m, sym) ;
   e := doError (e, t, tok) ;
   errorString (e, str) ;
   varargs.end (sym)
END metaErrorStringT1 ;


PROCEDURE metaErrorT1 (tok: CARDINAL; m: ARRAY OF CHAR; s: ARRAY OF BYTE) ;
BEGIN
   metaErrorStringT1 (tok, InitString (m), s)
END metaErrorT1 ;


PROCEDURE metaErrorStringT2 (tok: CARDINAL; m: String; s1, s2: ARRAY OF BYTE) ;
VAR
   str: String ;
   e  : error ;
   sym: vararg ;
   t  : errorType ;
BEGIN
   e := NIL ;
   sym := varargs.start2 (s1, s2) ;
   t := newerror ;
   str := doFormat (e, t, m, sym) ;
   e := doError (e, t, tok) ;
   errorString (e, str) ;
   varargs.end (sym)
END metaErrorStringT2 ;


PROCEDURE metaErrorT2 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2: ARRAY OF BYTE) ;
BEGIN
   metaErrorStringT2 (tok, InitString (m), s1, s2)
END metaErrorT2 ;


PROCEDURE metaErrorStringT3 (tok: CARDINAL; m: String; s1, s2, s3: ARRAY OF BYTE) ;
VAR
   str: String ;
   e  : error ;
   sym: vararg ;
   t  : errorType ;
BEGIN
   e := NIL ;
   sym := varargs.start3 (s1, s2, s3) ;
   t := newerror ;
   str := doFormat (e, t, m, sym) ;
   e := doError (e, t, tok) ;
   errorString (e, str) ;
   varargs.end (sym)
END metaErrorStringT3 ;


PROCEDURE metaErrorT3 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2, s3: ARRAY OF BYTE) ;
BEGIN
   metaErrorStringT3 (tok, InitString (m), s1, s2, s3)
END metaErrorT3 ;


PROCEDURE metaErrorStringT4 (tok: CARDINAL; m: String; s1, s2, s3, s4: ARRAY OF BYTE) ;
VAR
   str: String ;
   e  : error ;
   sym: vararg ;
   t  : errorType ;
BEGIN
   e := NIL ;
   sym := varargs.start4 (s1, s2, s3, s4) ;
   t := newerror ;
   str := doFormat (e, t, m, sym) ;
   e := doError (e, t, tok) ;
   errorString (e, str) ;
   varargs.end (sym)
END metaErrorStringT4 ;


PROCEDURE metaErrorT4 (tok: CARDINAL; m: ARRAY OF CHAR;
                       s1, s2, s3, s4: ARRAY OF BYTE) ;
BEGIN
   metaErrorStringT4 (tok, InitString (m), s1, s2, s3, s4)
END metaErrorT4 ;


PROCEDURE metaError1 (m: ARRAY OF CHAR; s: ARRAY OF BYTE) ;
BEGIN
   metaErrorT1 (getTokenNo (), m, s)
END metaError1 ;


PROCEDURE metaError2 (m: ARRAY OF CHAR; s1, s2: ARRAY OF BYTE) ;
BEGIN
   metaErrorT2 (getTokenNo (), m, s1, s2)
END metaError2 ;


PROCEDURE metaError3 (m: ARRAY OF CHAR; s1, s2, s3: ARRAY OF BYTE) ;
BEGIN
   metaErrorT3 (getTokenNo (), m, s1, s2, s3)
END metaError3 ;


PROCEDURE metaError4 (m: ARRAY OF CHAR; s1, s2, s3, s4: ARRAY OF BYTE) ;
BEGIN
   metaErrorT4 (getTokenNo (), m, s1, s2, s3, s4)
END metaError4 ;


(*
   wrapErrors -
*)

PROCEDURE wrapErrors (tok: CARDINAL;
                      m1, m2: ARRAY OF CHAR;
                      sym: vararg) ;
VAR
   e, f: error ;
   str : String ;
   t   : errorType ;
BEGIN
   e := NIL ;
   t := newerror ;
   str := doFormat (e, t, InitString(m1), sym) ;
   e := doError (e, t, tok) ;
   errorString (e, str) ;
   f := e ;
   t := chained ;
   str := doFormat (f, t, InitString (m2), sym) ;
   IF e=f
   THEN
      t := chained ;
      f := doError (e, t, tok)
   END ;
   errorString (f, str)
END wrapErrors ;


PROCEDURE metaErrorsT1 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s: ARRAY OF BYTE) ;
VAR
   sym: vararg ;
BEGIN
   sym := varargs.start1 (s) ;
   wrapErrors (tok, m1, m2, sym) ;
   varargs.end (sym)
END metaErrorsT1 ;


PROCEDURE metaErrorsT2 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2: ARRAY OF BYTE) ;
VAR
   sym: vararg ;
BEGIN
   sym := varargs.start2 (s1, s2) ;
   wrapErrors (tok, m1, m2, sym) ;
   varargs.end (sym)
END metaErrorsT2 ;


PROCEDURE metaErrorsT3 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2, s3: ARRAY OF BYTE) ;
VAR
   sym: vararg ;
BEGIN
   sym := varargs.start3 (s1, s2, s3) ;
   wrapErrors (tok, m1, m2, sym) ;
   varargs.end (sym)
END metaErrorsT3 ;


PROCEDURE metaErrorsT4 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2, s3, s4: ARRAY OF BYTE) ;
VAR
   sym: vararg ;
BEGIN
   sym := varargs.start4 (s1, s2, s3, s4) ;
   wrapErrors (tok, m1, m2, sym) ;
   varargs.end (sym)
END metaErrorsT4 ;


PROCEDURE metaErrors1 (m1, m2: ARRAY OF CHAR; s: ARRAY OF BYTE) ;
BEGIN
   metaErrorsT1 (getTokenNo (), m1, m2, s)
END metaErrors1 ;


PROCEDURE metaErrors2 (m1, m2: ARRAY OF CHAR; s1, s2: ARRAY OF BYTE) ;
BEGIN
   metaErrorsT2 (getTokenNo (), m1, m2, s1, s2)
END metaErrors2 ;


PROCEDURE metaErrors3 (m1, m2: ARRAY OF CHAR; s1, s2, s3: ARRAY OF BYTE) ;
BEGIN
   metaErrorsT3 (getTokenNo (), m1, m2, s1, s2, s3)
END metaErrors3 ;


PROCEDURE metaErrors4 (m1, m2: ARRAY OF CHAR; s1, s2, s3, s4: ARRAY OF BYTE) ;
BEGIN
   metaErrorsT4 (getTokenNo (), m1, m2, s1, s2, s3, s4)
END metaErrors4 ;


PROCEDURE metaErrorString1 (m: String; s: ARRAY OF BYTE) ;
BEGIN
   metaErrorStringT1 (getTokenNo (), m, s)
END metaErrorString1 ;


PROCEDURE metaErrorString2 (m: String; s1, s2: ARRAY OF BYTE) ;
BEGIN
   metaErrorStringT2 (getTokenNo (), m, s1, s2)
END metaErrorString2 ;


PROCEDURE metaErrorString3 (m: String; s1, s2, s3: ARRAY OF BYTE) ;
BEGIN
   metaErrorStringT3 (getTokenNo (), m, s1, s2, s3)
END metaErrorString3 ;


PROCEDURE metaErrorString4 (m: String; s1, s2, s3, s4: ARRAY OF BYTE) ;
BEGIN
   metaErrorStringT4 (getTokenNo (), m, s1, s2, s3, s4)
END metaErrorString4 ;


END mcMetaError.
