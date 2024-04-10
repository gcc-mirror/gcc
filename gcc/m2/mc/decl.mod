(* decl.mod declaration nodes used to create the AST.

Copyright (C) 2015-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

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

IMPLEMENTATION MODULE decl ; (*!m2pim*)

FROM ASCII IMPORT lf, tab ;
FROM symbolKey IMPORT NulKey, symbolTree, initTree, getSymKey, putSymKey, foreachNodeDo ;
FROM mcDebug IMPORT assert ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM nameKey IMPORT NulName, makeKey, lengthKey, makekey, keyToCharStar ;
FROM SFIO IMPORT OpenToWrite, WriteS ;
FROM FIO IMPORT File, Close, FlushBuffer, StdOut, WriteLine, WriteChar ;
FROM DynamicStrings IMPORT String, InitString, EqualArray, InitStringCharStar, KillString, ConCat, Mark, RemoveWhitePostfix, RemoveWhitePrefix ;
FROM StringConvert IMPORT CardinalToString, ostoc ;

FROM mcOptions IMPORT getOutputFile, getDebugTopological, getHPrefix, getIgnoreFQ,
                      getExtendedOpaque, writeGPLheader, getGccConfigSystem,
                      getScaffoldDynamic, getScaffoldMain, getSuppressNoReturn,
                      useBool, getCRealType, getCShortRealType,
                      getCLongRealType ;

FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;
FROM libc IMPORT printf, memset ;
FROM mcMetaError IMPORT metaError1, metaError2, metaError3, metaErrors1, metaErrors2 ;
FROM mcError IMPORT errorAbort0, flushErrors ;

FROM mcLexBuf IMPORT findFileNameFromToken, tokenToLineNo, tokenToColumnNo,
                     getProcedureComment, getBodyComment, getAfterComment,
		     lastcomment ;

FROM mcComment IMPORT commentDesc, isProcedureComment, isAfterComment, isBodyComment, getContent, initComment, addText ;

FROM StrLib IMPORT StrEqual, StrLen ;

FROM mcPretty IMPORT pretty, initPretty, dupPretty, killPretty, print, prints, raw,
                     setNeedSpace, noSpace, setindent, getindent, getcurpos,
		     getseekpos, getcurline,
		     pushPretty, popPretty ;

FROM Indexing IMPORT Index, InitIndex, ForeachIndiceInIndexDo,
                     IncludeIndiceIntoIndex, IsIndiceInIndex,
		     HighIndice, LowIndice, GetIndice, RemoveIndiceFromIndex,
		     PutIndice, InBounds ;

IMPORT DynamicStrings ;
IMPORT alists, wlists ;
IMPORT keyc ;
IMPORT mcStream ;

FROM alists IMPORT alist ;
FROM wlists IMPORT wlist ;


CONST
   indentation     = 3 ;
   indentationC    = 2 ;
   debugScopes     = FALSE ;
   debugDecl       = FALSE ;
   caseException   = TRUE ;
   returnException = TRUE ;
   (* this is a work around to avoid ever having to handle dangling else.  *)
   forceCompoundStatement = TRUE ;    (* TRUE will avoid dangling else, by always using {}.  *)
   enableDefForCStrings   = FALSE ;   (* currently disabled.  *)
   enableMemsetOnAllocation = TRUE ;  (* Should we memset (..., 0, ...) the allocated mem?  *)
   forceQualified  = TRUE ;

TYPE
   language = (ansiC, ansiCP, pim4) ;

   nodeT = (explist, funccall,
            exit, return, stmtseq, comment, halt,
            new, dispose, inc, dec, incl, excl,
	    length,
            (* base constants.  *)
	    nil, true, false,
            (* system types.  *)
   	    address, loc, byte, word,
            csizet, cssizet,
            (* base types.  *)
	    char,
	    cardinal, longcard, shortcard,
            integer, longint, shortint,
	    real, longreal, shortreal,
	    bitset, boolean, proc,
	    ztype, rtype,
	    complex, longcomplex, shortcomplex,
	    (* language features and compound type attributes.  *)
            type, record, varient, var, enumeration,
            subrange, array, subscript,
            string, const, literal, varparam, param, varargs, optarg,
	    pointer, recordfield, varientfield, enumerationfield,
            set, proctype,
	    (* blocks.  *)
	    procedure, def, imp, module,
	    (* statements.  *)
            loop, while, for, repeat,
	    case, caselabellist, caselist, range,
	    assignment,
	    if, elsif,
	    (* expressions.  *)
	    constexp,
	    neg,
	    cast, val,
	    plus, sub, div, mod, mult, divide, in,
	    adr, size, tsize, ord, float, trunc, chr, abs, cap,
	    high, throw, unreachable,
	    cmplx, re, im,
	    min, max,
            componentref, pointerref, arrayref, deref,
	    equal, notequal, less, greater, greequal, lessequal,
	    lsl, lsr, lor, land, lnot, lxor,
	    and, or, not, identlist, vardecl, setvalue) ;

    node = POINTER TO nodeRec ;

    nodeRec =         RECORD
                         CASE kind: nodeT OF

                         unreachable,
			 throw,
                         new,
                         dispose,
                         inc,
                         dec,
                         incl,
                         excl,
                         halt            : intrinsicF: intrinsicT |
			 explist         : explistF: explistT |
                         exit            : exitF   : exitT |
			 return          : returnF : returnT |
			 stmtseq         : stmtF   : stmtT |
			 comment         : commentF: commentT |
                         (* base constants.  *)
                         nil,
			 true,
			 false,
			 (* system types.  *)
                         address,
			 loc,
                         byte,
			 word,
                         csizet,
                         cssizet         : |
                         (* base types.  *)
			 boolean,
 			 proc,
                         char,
                         integer,
                         cardinal,
                         longcard,
                         shortcard,
                         longint,
                         shortint,
                         real,
                         longreal,
			 shortreal,
                         bitset,
                         ztype,
                         rtype,
			 complex,
			 longcomplex,
			 shortcomplex    :  |
                         (* language features and compound type attributes.  *)
                         type            :  typeF            : typeT |
                         record          :  recordF          : recordT |
                         varient         :  varientF         : varientT |
                         var             :  varF             : varT |
                         enumeration     :  enumerationF     : enumerationT |
                         subrange        :  subrangeF        : subrangeT |
                         subscript       :  subscriptF       : subscriptT |
                         array           :  arrayF           : arrayT |
                         string          :  stringF          : stringT |
                         const           :  constF           : constT |
                         literal         :  literalF         : literalT |
			 varparam        :  varparamF        : varparamT |
                         param           :  paramF           : paramT |
			 varargs         :  varargsF         : varargsT |
			 optarg          :  optargF          : optargT |
			 pointer         :  pointerF         : pointerT |
                         recordfield     :  recordfieldF     : recordfieldT |
			 varientfield    :  varientfieldF    : varientfieldT |
			 enumerationfield:  enumerationfieldF: enumerationfieldT |
                         set             :  setF             : setT |
			 proctype        :  proctypeF        : proctypeT |
                         (* blocks.  *)
                         procedure       :  procedureF       : procedureT |
                         def             :  defF             : defT |
			 imp             :  impF             : impT |
			 module          :  moduleF          : moduleT |
                         (* statements.  *)
                         loop            :  loopF            : loopT |
                         while           :  whileF           : whileT |
                         for             :  forF             : forT |
                         repeat          :  repeatF          : repeatT |
			 case            :  caseF            : caseT |
                         caselabellist   :  caselabellistF   : caselabellistT |
			 caselist        :  caselistF        : caselistT |
			 range           :  rangeF           : rangeT |
                         if              :  ifF              : ifT |
                         elsif           :  elsifF           : elsifT |
			 assignment      :  assignmentF      : assignmentT |
                         (* expressions.  *)
 			 arrayref        :  arrayrefF        : arrayrefT |
                         pointerref      :  pointerrefF      : pointerrefT |
                         componentref    :  componentrefF    : componentrefT |
			 cmplx,
			 and,
			 or,
                         equal,
                         notequal,
			 less,
			 greater,
			 greequal,
			 lessequal,
                         val,
                         cast,
                         plus,
                         sub,
                         div,
			 mod,
			 mult,
			 divide,
			 in              :  binaryF          : binaryT |
			 constexp,
			 deref,
			 abs,
			 chr,
			 cap,
                         high,
                         ord,
			 float,
			 trunc,
			 re,
			 im,
			 not,
			 neg,
                         adr,
                         size,
			 tsize,
			 min,
			 max             :  unaryF           : unaryT |
			 identlist       :  identlistF       : identlistT |
			 vardecl         :  vardeclF         : vardeclT |
                         funccall        :  funccallF        : funccallT |
			 setvalue        :  setvalueF        : setvalueT

                         END ;
                         at: where ;
                      END ;

       intrinsicT = RECORD
                       args            : node ;
                       noArgs          : CARDINAL ;
                       type            : node ;
                       intrinsicComment: commentPair ;
                       postUnreachable : BOOLEAN ;
                    END ;

       fixupInfo = RECORD
                      count: CARDINAL ;
                      info : Index ;
                   END ;

       explistT = RECORD
                     exp:  Index ;
                  END ;

       setvalueT = RECORD
                      type:  node ;
		      values:  Index ;
                   END ;

       identlistT = RECORD
                       names : wlist ;
		       cnamed: BOOLEAN ;
                    END ;

       funccallT = RECORD
                      function       : node ;
		      args           : node ;
		      type           : node ;
		      funccallComment: commentPair ;
                   END ;

       commentT = RECORD
                     content:  commentDesc ;
                  END ;

       stmtT = RECORD
                  statements:  Index ;
               END ;

       returnT = RECORD
                    exp          : node ;
                    scope        : node ;
		    returnComment: commentPair ;
                 END ;

       exitT = RECORD
                  loop:  node ;
               END ;

       vardeclT = RECORD
                     names:  wlist ;
		     type :  node ;
		     scope:  node ;
                  END ;

       typeT = RECORD
                  name      :  Name ;
		  type      :  node ;
		  scope     :  node ;
		  isHidden,
		  isInternal:  BOOLEAN ;
               END ;

       recordT = RECORD
		    localSymbols:  symbolTree ;
                    listOfSons  :  Index ;
		    scope       :  node ;
                 END ;

       varientT = RECORD
		     listOfSons:  Index ;
		     varient   :  node ;
		     tag       :  node ;
		     scope     :  node ;
                  END ;

       varT = RECORD
                 name          :  Name ;
		 type          :  node ;
		 decl          :  node ;
		 scope         :  node ;
		 isInitialised,
		 isParameter,
		 isVarParameter,
                 isUsed        :  BOOLEAN ;
                 cname         :  cnameT ;
              END ;

       enumerationT = RECORD
			 noOfElements: CARDINAL ;
                         localSymbols: symbolTree ;
			 listOfSons  : Index ;
                         low, high   : node ;
			 scope       : node ;
                      END ;

       subrangeT = RECORD
		      low,
		      high :  node ;
		      type :  node ;
		      scope:  node ;
                   END ;

       subscriptT = RECORD
                       type:  node ;
		       expr:  node ;
                    END ;

       arrayT = RECORD
		   subr       :  node ;
		   type,
		   scope      :  node ;
		   isUnbounded:  BOOLEAN ;
                END ;

       stringT = RECORD
                    name            :  Name ;
		    length          :  CARDINAL ;
		    isCharCompatible:  BOOLEAN ;
		    cstring         :  String ;
		    clength         :  CARDINAL ;
		    cchar           :  String ;
                 END ;

       literalT = RECORD
                     name :  Name ;
		     type :  node ;
                  END ;

       constT = RECORD
                   name :  Name ;
		   type :  node ;
                   value:  node ;
                   scope:  node ;
                END ;

       varparamT = RECORD
                      namelist   :  node ;
                      type       :  node ;
		      scope      :  node ;
                      isUnbounded:  BOOLEAN ;
                      isForC     :  BOOLEAN ;
                      isUsed     :  BOOLEAN ;
                   END ;

       paramT = RECORD
                   namelist   :  node ;
                   type       :  node ;
                   scope      :  node ;
                   isUnbounded:  BOOLEAN ;
                   isForC     :  BOOLEAN ;
                   isUsed     :  BOOLEAN ;
                END ;

       varargsT = RECORD
                     scope    :  node ;
                  END ;

       optargT = RECORD
                    namelist  :  node ;
		    type      :  node ;
		    scope     :  node ;
		    init      :  node ;
                 END ;

       pointerT = RECORD
                     type :  node ;
                     scope:  node ;
                  END ;

       recordfieldT = RECORD
                         name   :  Name ;
			 type   :  node ;
			 tag    :  BOOLEAN ;
			 parent :  node ;
			 varient:  node ;
			 scope  :  node ;
			 cname  :  cnameT ;
                      END ;

       varientfieldT = RECORD
                          name      :  Name ;
			  parent    :  node ;
			  varient   :  node ;
                          simple    :  BOOLEAN ;
			  listOfSons:  Index ;
			  scope     :  node ;
                       END ;

       enumerationfieldT = RECORD
                              name :  Name ;
                              type :  node ;
			      scope:  node ;
                              value:  CARDINAL ;
                              cname:  cnameT ;
                           END ;

       setT = RECORD
                 type :  node ;
                 scope:  node ;
              END ;

       componentrefT = RECORD
                          rec       :  node ;
                          field     :  node ;
                          resultType:  node ;
                       END ;

       pointerrefT = RECORD
                        ptr       :  node ;
                        field     :  node ;
                        resultType:  node ;
                     END ;

       arrayrefT = RECORD
                      array     :  node ;
                      index     :  node ;
                      resultType:  node ;
                   END ;

       commentPair = RECORD
			after,
                        body :  node ;
                     END ;

       assignmentT = RECORD
                        des,
                        expr         :  node ;
			assignComment: commentPair ;
                     END ;

       ifT = RECORD
                expr,
                elsif,      (* either else or elsif must be NIL.  *)
                then,
                else        :  node ;
		ifComment,
		elseComment,  (* used for else or elsif *)
		endComment  : commentPair ;
             END ;

       elsifT = RECORD
                   expr,
                   elsif,     (* either else or elsif must be NIL.  *)
                   then,
                   else       :  node ;
                   elseComment:  commentPair ;  (* used for else or elsif *)
                END ;

       loopT = RECORD
                  statements:  node ;
		  labelno   :  CARDINAL ;  (* 0 means no label.  *)
               END ;

       whileT = RECORD
                   expr,
                   statements:  node ;
                   doComment,
                   endComment:  commentPair ;
                END ;

       repeatT = RECORD
                    expr,
                    statements   :  node ;
                    repeatComment,
		    untilComment :  commentPair ;
                 END ;

       caseT = RECORD
                  expression   :  node ;
                  caseLabelList:  Index ;
		  else         :  node ;
               END ;

       caselabellistT = RECORD
                           caseList  :  node ;
			   statements:  node ;
                        END ;

       caselistT = RECORD
                      rangePairs:  Index ;
                   END ;

       rangeT = RECORD
                   lo,
                   hi:  node ;
                END ;

       forT = RECORD
                 des,
                 start,
		 end,
                 increment,
                 statements:  node ;
              END ;

       statementT = RECORD
                       sequence:  Index ;
                    END ;

       scopeT = RECORD
                   symbols   :  symbolTree ;
                   constants,
                   types,
		   procedures,
                   variables :  Index ;
                END ;

       procedureT = RECORD
                       name           :  Name ;
                       decls          :  scopeT ;
                       scope          :  node ;
                       parameters     :  Index ;
                       isForC,
		       built,
		       checking,
                       returnopt,
		       vararg,
		       noreturnused,
		       noreturn       :  BOOLEAN ;
		       paramcount     :  CARDINAL ;
                       optarg         :  node ;
		       returnType     :  node ;
                       beginStatements:  node ;
                       cname          :  cnameT ;
		       defComment,
		       modComment     :  commentDesc ;
                    END ;

       proctypeT = RECORD
                      parameters:  Index ;
                      returnopt,
                      vararg    :  BOOLEAN ;
                      optarg    :  node ;
		      scope     :  node ;
                      returnType:  node ;
                   END ;

       binaryT = RECORD
                    left,
		    right,
                    resultType:  node ;
                 END ;

       unaryT = RECORD
                   arg,
                   resultType:  node ;
                END ;

       moduleT = RECORD
                    name             :  Name ;
                    source           :  Name ;
                    importedModules  :  Index ;
                    constFixup,
                    enumFixup        :  fixupInfo ;
                    decls            :  scopeT ;
                    beginStatements,
                    finallyStatements:  node ;
                    enumsComplete,
		    constsComplete,
		    visited          :  BOOLEAN ;
                    com              :  commentPair ;
                 END ;

       defT = RECORD
                 name             :  Name ;
                 source           :  Name ;
		 hasHidden,
                 forC             :  BOOLEAN ;
                 exported,
                 importedModules  :  Index ;
                 constFixup,
                 enumFixup        :  fixupInfo ;
                 decls            :  scopeT ;
		 enumsComplete,
                 constsComplete,
                 visited          :  BOOLEAN ;
                 com              :  commentPair ;
              END ;

       impT = RECORD
                 name             :  Name ;
                 source           :  Name ;
                 importedModules  :  Index ;
                 constFixup,
                 enumFixup        :  fixupInfo ;
                 beginStatements,
                 finallyStatements:  node ;
		 definitionModule :  node ;
                 decls            :  scopeT ;
                 enumsComplete,
                 constsComplete,
                 visited          :  BOOLEAN ;
                 com              :  commentPair ;
              END ;

       where = RECORD
                  defDeclared,
                  modDeclared,
                  firstUsed  :  CARDINAL ;
               END ;

       outputStates = (text, punct, space) ;

       nodeProcedure = PROCEDURE (node) ;

       dependentState = (completed, blocked, partial, recursive) ;

       cnameT = RECORD
                   name :  Name ;
                   init :  BOOLEAN ;
                END ;

VAR
   outputFile    : File ;
   lang          : language ;
   bitsperunitN,
   bitsperwordN,
   bitspercharN,
   unitsperwordN,
   mainModule,
   currentModule,
   defModule,
   systemN,
   addressN,
   locN,
   byteN,
   wordN,
   csizetN,
   cssizetN,
   adrN,
   sizeN,
   tsizeN,
   newN,
   disposeN,
   lengthN,
   incN,
   decN,
   inclN,
   exclN,
   highN,
   m2rtsN,
   haltN,
   throwN,
   chrN,
   capN,
   absN,
   floatN,
   truncN,
   ordN,
   valN,
   minN,
   maxN,
   booleanN,
   procN,
   charN,
   integerN,
   cardinalN,
   longcardN,
   shortcardN,
   longintN,
   shortintN,
   bitsetN,
   bitnumN,
   ztypeN,
   rtypeN,
   complexN,
   longcomplexN,
   shortcomplexN,
   cmplxN,
   reN,
   imN,
   realN,
   longrealN,
   shortrealN,
   nilN,
   trueN,
   falseN        : node ;
   scopeStack,
   defUniverseI,
   modUniverseI  : Index ;
   modUniverse,
   defUniverse   : symbolTree ;
   baseSymbols   : symbolTree ;
   outputState   : outputStates ;
   doP           : pretty ;
   todoQ,
   partialQ,
   doneQ         : alist ;
   mustVisitScope,
   simplified    : BOOLEAN ;
   tempCount     : CARDINAL ;


(*
   newNode - create and return a new node of kind k.
*)

PROCEDURE newNode (k: nodeT) : node ;
VAR
   d: node ;
BEGIN
   NEW (d) ;
   IF enableMemsetOnAllocation
   THEN
      d := memset (d, 0, SIZE (d^))
   END ;
   IF d=NIL
   THEN
      HALT
   ELSE
      d^.kind := k ;
      d^.at.defDeclared := 0 ;
      d^.at.modDeclared := 0 ;
      d^.at.firstUsed := 0 ;
      RETURN d
   END
END newNode ;


(*
   disposeNode - dispose node, n.
*)

PROCEDURE disposeNode (VAR n: node) ;
BEGIN
   DISPOSE (n) ;
   n := NIL
END disposeNode ;


(*
   getDeclaredDef - returns the token number associated with the nodes declaration
                    in the definition module.
*)

PROCEDURE getDeclaredDef (n: node) : CARDINAL ;
BEGIN
   RETURN n^.at.defDeclared
END getDeclaredDef ;


(*
   getDeclaredMod - returns the token number associated with the nodes declaration
                    in the implementation or program module.
*)

PROCEDURE getDeclaredMod (n: node) : CARDINAL ;
BEGIN
   RETURN n^.at.modDeclared
END getDeclaredMod ;


(*
   getFirstUsed - returns the token number associated with the first use of
                  node, n.
*)

PROCEDURE getFirstUsed (n: node) : CARDINAL ;
BEGIN
   RETURN n^.at.firstUsed
END getFirstUsed ;


(*
   setVisited - set the visited flag on a def/imp/module node.
*)

PROCEDURE setVisited (n: node) ;
BEGIN
   CASE n^.kind OF

   def   :  n^.defF.visited := TRUE |
   imp   :  n^.impF.visited := TRUE |
   module:  n^.moduleF.visited := TRUE

   END
END setVisited ;


(*
   unsetVisited - unset the visited flag on a def/imp/module node.
*)

PROCEDURE unsetVisited (n: node) ;
BEGIN
   CASE n^.kind OF

   def   :  n^.defF.visited := FALSE |
   imp   :  n^.impF.visited := FALSE |
   module:  n^.moduleF.visited := FALSE

   END
END unsetVisited ;


(*
   isVisited - returns TRUE if the node was visited.
*)

PROCEDURE isVisited (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   def   :  RETURN n^.defF.visited |
   imp   :  RETURN n^.impF.visited |
   module:  RETURN n^.moduleF.visited

   END
END isVisited ;


(*
   isDef - return TRUE if node, n, is a definition module.
*)

PROCEDURE isDef (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = def
END isDef ;


(*
   isImp - return TRUE if node, n, is an implementation module.
*)

PROCEDURE isImp (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = imp
END isImp ;


(*
   isModule - return TRUE if node, n, is a program module.
*)

PROCEDURE isModule (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = module
END isModule ;


(*
   isImpOrModule - returns TRUE if, n, is a program module or implementation module.
*)

PROCEDURE isImpOrModule (n: node) : BOOLEAN ;
BEGIN
   RETURN isImp (n) OR isModule (n)
END isImpOrModule ;


(*
   isProcedure - returns TRUE if node, n, is a procedure.
*)

PROCEDURE isProcedure (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = procedure
END isProcedure ;


(*
   isConst - returns TRUE if node, n, is a const.
*)

PROCEDURE isConst (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = const
END isConst ;


(*
   isType - returns TRUE if node, n, is a type.
*)

PROCEDURE isType (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = type
END isType ;


(*
   isVar - returns TRUE if node, n, is a type.
*)

PROCEDURE isVar (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = var
END isVar ;


(*
   isTemporary - returns TRUE if node, n, is a variable and temporary.
*)

PROCEDURE isTemporary (n: node) : BOOLEAN ;
BEGIN
   RETURN FALSE
END isTemporary ;


(*
   isExported - returns TRUE if symbol, n, is exported from
                the definition module.
*)

PROCEDURE isExported (n: node) : BOOLEAN ;
VAR
   s: node ;
BEGIN
   s := getScope (n) ;
   IF s#NIL
   THEN
      CASE s^.kind OF

      def:  RETURN IsIndiceInIndex (s^.defF.exported, n)

      ELSE
         RETURN FALSE
      END
   END ;
   RETURN FALSE
END isExported ;


(*
   isLocal - returns TRUE if symbol, n, is locally declared in a procedure.
*)

PROCEDURE isLocal (n: node) : BOOLEAN ;
VAR
   s: node ;
BEGIN
   s := getScope (n) ;
   IF s#NIL
   THEN
      RETURN isProcedure (s)
   END ;
   RETURN FALSE
END isLocal ;


(*
   lookupExported - attempts to lookup a node named, i, from definition
                    module, n.  The node is returned if found.
                    NIL is returned if not found.
*)

PROCEDURE lookupExported (n: node; i: Name) : node ;
VAR
   r: node ;
BEGIN
   assert (isDef (n)) ;
   r := getSymKey (n^.defF.decls.symbols, i) ;
   IF (r#NIL) AND isExported (r)
   THEN
      RETURN r
   END ;
   RETURN NIL
END lookupExported ;


(*
   importEnumFields - if, n, is an enumeration type import the all fields into module, m.
*)

PROCEDURE importEnumFields (m, n: node) ;
VAR
   r, e: node ;
   i, h: CARDINAL ;
BEGIN
   assert (isDef (m) OR isModule (m) OR isImp (m)) ;
   n := skipType (n) ;
   IF (n#NIL) AND isEnumeration (n)
   THEN
      i := LowIndice (n^.enumerationF.listOfSons) ;
      h := HighIndice (n^.enumerationF.listOfSons) ;
      WHILE i<=h DO
         e := GetIndice (n^.enumerationF.listOfSons, i) ;
         r := import (m, e) ;
         IF e#r
         THEN
            metaError2 ('enumeration field {%1ad} cannot be imported implicitly into {%2d} due to a name clash',
                        e, m)
         END ;
         INC (i)
      END
   END
END importEnumFields ;


(*
   import - attempts to add node, n, into the scope of module, m.
            It might fail due to a name clash in which case the
            previous named symbol is returned.  On success, n,
            is returned.
*)

PROCEDURE import (m, n: node) : node ;
VAR
   name: Name ;
   r   : node ;
BEGIN
   assert (isDef (m) OR isModule (m) OR isImp (m)) ;
   name := getSymName (n) ;
   r := lookupInScope (m, name) ;
   IF r=NIL
   THEN
      CASE m^.kind OF

      def   :  putSymKey (m^.defF.decls.symbols, name, n) |
      imp   :  putSymKey (m^.impF.decls.symbols, name, n) |
      module:  putSymKey (m^.moduleF.decls.symbols, name, n)

      END ;
      importEnumFields (m, n) ;
      RETURN n
   END ;
   RETURN r
END import ;


(*
   isZtype - returns TRUE if, n, is the Z type.
*)

PROCEDURE isZtype (n: node) : BOOLEAN ;
BEGIN
   RETURN n = ztypeN
END isZtype ;


(*
   isRtype - returns TRUE if, n, is the R type.
*)

PROCEDURE isRtype (n: node) : BOOLEAN ;
BEGIN
   RETURN n = rtypeN
END isRtype ;


(*
   isComplex - returns TRUE if, n, is the complex type.
*)

PROCEDURE isComplex (n: node) : BOOLEAN ;
BEGIN
   RETURN n = complexN
END isComplex ;


(*
   isLongComplex - returns TRUE if, n, is the longcomplex type.
*)

PROCEDURE isLongComplex (n: node) : BOOLEAN ;
BEGIN
   RETURN n = longcomplexN
END isLongComplex ;


(*
   isShortComplex - returns TRUE if, n, is the shortcomplex type.
*)

PROCEDURE isShortComplex (n: node) : BOOLEAN ;
BEGIN
   RETURN n = shortcomplexN
END isShortComplex ;


(*
   isLiteral - returns TRUE if, n, is a literal.
*)

PROCEDURE isLiteral (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = literal
END isLiteral ;


(*
   isConstSet - returns TRUE if, n, is a constant set.
*)

PROCEDURE isConstSet (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   IF isLiteral (n) OR isConst (n)
   THEN
      RETURN isSet (skipType (getType (n)))
   END ;
   RETURN FALSE
END isConstSet ;


(*
   isEnumerationField - returns TRUE if, n, is an enumeration field.
*)

PROCEDURE isEnumerationField (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = enumerationfield
END isEnumerationField ;


(*
   isUnbounded - returns TRUE if, n, is an unbounded array.
*)

PROCEDURE isUnbounded (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN (n^.kind = array) AND (n^.arrayF.isUnbounded)
END isUnbounded ;


(*
   isParameter - returns TRUE if, n, is a parameter.
*)

PROCEDURE isParameter (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN (n^.kind = param) OR (n^.kind = varparam)
END isParameter ;


(*
   isVarParam - returns TRUE if, n, is a var parameter.
*)

PROCEDURE isVarParam (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = varparam
END isVarParam ;


(*
   isParam - returns TRUE if, n, is a non var parameter.
*)

PROCEDURE isParam (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = param
END isParam ;


(*
   isNonVarParam - is an alias to isParam.
*)

PROCEDURE isNonVarParam (n: node) : BOOLEAN ;
BEGIN
   RETURN isParam (n)
END isNonVarParam ;


(*
   isRecord - returns TRUE if, n, is a record.
*)

PROCEDURE isRecord (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = record
END isRecord ;


(*
   isRecordField - returns TRUE if, n, is a record field.
*)

PROCEDURE isRecordField (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = recordfield
END isRecordField ;


(*
   isArray - returns TRUE if, n, is an array.
*)

PROCEDURE isArray (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = array
END isArray ;


(*
   isProcType - returns TRUE if, n, is a procedure type.
*)

PROCEDURE isProcType (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = proctype
END isProcType ;


(*
   isAProcType - returns TRUE if, n, is a proctype or proc node.
*)

PROCEDURE isAProcType (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN isProcType (n) OR (n = procN)
END isAProcType ;


(*
   isProcedure - returns TRUE if, n, is a procedure.
*)

PROCEDURE isProcedure (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = procedure
END isProcedure ;


(*
   isPointer - returns TRUE if, n, is a pointer.
*)

PROCEDURE isPointer (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = pointer
END isPointer ;


(*
   isVarient - returns TRUE if, n, is a varient record.
*)

PROCEDURE isVarient (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = varient
END isVarient ;


(*
   isVarientField - returns TRUE if, n, is a varient field.
*)

PROCEDURE isVarientField (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = varientfield
END isVarientField ;


(*
   isSet - returns TRUE if, n, is a set type.
*)

PROCEDURE isSet (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = set
END isSet ;


(*
   isSubrange - returns TRUE if, n, is a subrange type.
*)

PROCEDURE isSubrange (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = subrange
END isSubrange ;


(*
   isMainModule - return TRUE if node, n, is the main module specified
                  by the source file.  This might be a definition,
                  implementation or program module.
*)

PROCEDURE isMainModule (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n = mainModule
END isMainModule ;


(*
   setMainModule - sets node, n, as the main module to be compiled.
*)

PROCEDURE setMainModule (n: node) ;
BEGIN
   assert (n#NIL) ;
   mainModule := n
END setMainModule ;


(*
   getMainModule - returns the main module node.
*)

PROCEDURE getMainModule () : node ;
BEGIN
   RETURN mainModule
END getMainModule ;


(*
   setCurrentModule - sets node, n, as the current module being compiled.
*)

PROCEDURE setCurrentModule (n: node) ;
BEGIN
   assert (n#NIL) ;
   currentModule := n
END setCurrentModule ;


(*
   getCurrentModule - returns the current module being compiled.
*)

PROCEDURE getCurrentModule () : node ;
BEGIN
   RETURN currentModule
END getCurrentModule ;


(*
   initFixupInfo - initialize the fixupInfo record.
*)

PROCEDURE initFixupInfo () : fixupInfo ;
VAR
   f: fixupInfo ;
BEGIN
   f.count := 0 ;
   f.info := InitIndex (1) ;
   RETURN f
END initFixupInfo ;


(*
   makeDef - returns a definition module node named, n.
*)

PROCEDURE makeDef (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (def) ;
   WITH d^ DO
      defF.name := n ;
      defF.source := NulName ;
      defF.hasHidden := FALSE ;
      defF.forC := FALSE ;
      defF.exported := InitIndex (1) ;
      defF.importedModules := InitIndex (1) ;
      defF.constFixup := initFixupInfo () ;
      defF.enumFixup := initFixupInfo () ;
      initDecls (defF.decls) ;
      defF.enumsComplete := FALSE ;
      defF.constsComplete := FALSE ;
      defF.visited := FALSE ;
      initPair (defF.com)
   END ;
   RETURN d
END makeDef ;


(*
   makeImp - returns an implementation module node named, n.
*)

PROCEDURE makeImp (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (imp) ;
   WITH d^ DO
      impF.name := n ;
      impF.source := NulName ;
      impF.importedModules := InitIndex (1) ;
      impF.constFixup := initFixupInfo () ;
      impF.enumFixup := initFixupInfo () ;
      initDecls (impF.decls) ;
      impF.beginStatements := NIL ;
      impF.finallyStatements := NIL ;
      impF.definitionModule := NIL ;
      impF.enumsComplete := FALSE ;
      impF.constsComplete := FALSE ;
      impF.visited := FALSE ;
      initPair (impF.com)
   END ;
   RETURN d
END makeImp ;


(*
   makeModule - returns a module node named, n.
*)

PROCEDURE makeModule (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (module) ;
   WITH d^ DO
      moduleF.name := n ;
      moduleF.source := NulName ;
      moduleF.importedModules := InitIndex (1) ;
      moduleF.constFixup := initFixupInfo () ;
      moduleF.enumFixup := initFixupInfo () ;
      initDecls (moduleF.decls) ;
      moduleF.beginStatements := NIL ;
      moduleF.finallyStatements := NIL ;
      moduleF.enumsComplete := FALSE ;
      moduleF.constsComplete := FALSE ;
      moduleF.visited := FALSE ;
      initPair (moduleF.com)
   END ;
   RETURN d
END makeModule ;


(*
   putDefForC - the definition module was defined FOR "C".
*)

PROCEDURE putDefForC (n: node) ;
BEGIN
   assert (isDef (n)) ;
   n^.defF.forC := TRUE
END putDefForC ;


(*
   isDefForC - returns TRUE if the definition module was defined FOR "C".
*)

PROCEDURE isDefForC (n: node) : BOOLEAN ;
BEGIN
   RETURN isDef (n) AND n^.defF.forC
END isDefForC ;


(*
   lookupDef - returns a definition module node named, n.
*)

PROCEDURE lookupDef (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := getSymKey (defUniverse, n) ;
   IF d=NIL
   THEN
      d := makeDef (n) ;
      putSymKey (defUniverse, n, d) ;
      IncludeIndiceIntoIndex (defUniverseI, d)
   END ;
   RETURN d
END lookupDef ;


(*
   lookupImp - returns an implementation module node named, n.
*)

PROCEDURE lookupImp (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := getSymKey (modUniverse, n) ;
   IF m=NIL
   THEN
      m := makeImp (n) ;
      putSymKey (modUniverse, n, m) ;
      IncludeIndiceIntoIndex (modUniverseI, m)
   END ;
   assert (NOT isModule (m)) ;
   RETURN m
END lookupImp ;


(*
   lookupModule - returns a module node named, n.
*)

PROCEDURE lookupModule (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := getSymKey (modUniverse, n) ;
   IF m=NIL
   THEN
      m := makeModule (n) ;
      putSymKey (modUniverse, n, m) ;
      IncludeIndiceIntoIndex (modUniverseI, m)
   END ;
   assert (NOT isImp (m)) ;
   RETURN m
END lookupModule ;


(*
   setSource - sets the source filename for module, n, to s.
*)

PROCEDURE setSource (n: node; s: Name) ;
BEGIN
   WITH n^ DO
      CASE kind OF

      def   :  defF.source := s |
      module:  moduleF.source := s |
      imp   :  impF.source := s

      END
   END
END setSource ;


(*
   getSource - returns the source filename for module, n.
*)

PROCEDURE getSource (n: node) : Name ;
BEGIN
   WITH n^ DO
      CASE kind OF

      def   :  RETURN defF.source |
      module:  RETURN moduleF.source |
      imp   :  RETURN impF.source

      END
   END
END getSource ;


(*
   initDecls - initialize the decls, scopeT.
*)

PROCEDURE initDecls (VAR decls: scopeT) ;
BEGIN
   decls.symbols := initTree () ;
   decls.constants := InitIndex (1) ;
   decls.types := InitIndex (1) ;
   decls.procedures := InitIndex (1) ;
   decls.variables := InitIndex (1)
END initDecls ;


(*
   enterScope - pushes symbol, n, to the scope stack.
*)

PROCEDURE enterScope (n: node) ;
BEGIN
   IF IsIndiceInIndex (scopeStack, n)
   THEN
      HALT
   ELSE
      IncludeIndiceIntoIndex (scopeStack, n)
   END ;
   IF debugScopes
   THEN
      printf ("enter scope\n") ;
      dumpScopes
   END
END enterScope ;


(*
   leaveScope - removes the top level scope.
*)

PROCEDURE leaveScope ;
VAR
   i: CARDINAL ;
   n: node ;
BEGIN
   i := HighIndice (scopeStack) ;
   n := GetIndice (scopeStack, i) ;
   RemoveIndiceFromIndex (scopeStack, n) ;
   IF debugScopes
   THEN
      printf ("leave scope\n") ;
      dumpScopes
   END
END leaveScope ;


(*
   getDeclScope - returns the node representing the
                  current declaration scope.
*)

PROCEDURE getDeclScope () : node ;
VAR
   i: CARDINAL ;
BEGIN
   i := HighIndice (scopeStack) ;
   RETURN GetIndice (scopeStack, i)
END getDeclScope ;


(*
   addTo - adds node, d, to scope decls and returns, d.
           It stores, d, in the symbols tree associated with decls.
*)

PROCEDURE addTo (VAR decls: scopeT; d: node) : node ;
VAR
   n: Name ;
BEGIN
   n := getSymName (d) ;
   IF n#NulName
   THEN
      IF getSymKey (decls.symbols, n)=NIL
      THEN
         putSymKey (decls.symbols, n, d)
      ELSE
         metaError1 ('{%1DMad} was declared', d) ;
         metaError1 ('{%1k} and is being declared again', n)
      END
   END ;
   IF isConst (d)
   THEN
      IncludeIndiceIntoIndex (decls.constants, d)
   ELSIF isVar (d)
   THEN
      IncludeIndiceIntoIndex (decls.variables, d)
   ELSIF isType (d)
   THEN
      IncludeIndiceIntoIndex (decls.types, d)
   ELSIF isProcedure (d)
   THEN
      IncludeIndiceIntoIndex (decls.procedures, d) ;
      IF debugDecl
      THEN
         printf ("%d procedures on the dynamic array\n",
                 HighIndice (decls.procedures))
      END
   END ;
   RETURN d
END addTo ;


(*
   export - export node, n, from definition module, d.
*)

PROCEDURE export (d, n: node) ;
BEGIN
   assert (isDef (d)) ;
   IncludeIndiceIntoIndex (d^.defF.exported, n)
END export ;


(*
   addToScope - adds node, n, to the current scope and returns, n.
*)

PROCEDURE addToScope (n: node) : node ;
VAR
   s: node ;
   i: CARDINAL ;
BEGIN
   i := HighIndice (scopeStack) ;
   s := GetIndice (scopeStack, i) ;
   IF isProcedure (s)
   THEN
      IF debugDecl
      THEN
         outText (doP, "adding ") ;
         doNameC (doP, n) ;
         outText (doP, " to procedure\n")
      END ;
      RETURN addTo (s^.procedureF.decls, n)
   ELSIF isModule (s)
   THEN
      IF debugDecl
      THEN
         outText (doP, "adding ") ;
         doNameC (doP, n) ;
         outText (doP, " to module\n")
      END ;
      RETURN addTo (s^.moduleF.decls, n)
   ELSIF isDef (s)
   THEN
      IF debugDecl
      THEN
         outText (doP, "adding ") ;
         doNameC (doP, n) ;
         outText (doP, " to definition module\n")
      END ;
      export (s, n) ;
      RETURN addTo (s^.defF.decls, n)
   ELSIF isImp (s)
   THEN
      IF debugDecl
      THEN
         outText (doP, "adding ") ;
         doNameC (doP, n) ;
         outText (doP, " to implementation module\n")
      END ;
      RETURN addTo (s^.impF.decls, n)
   END ;
   HALT
END addToScope ;


(*
   addModuleToScope - adds module, i, to module, m, scope.
*)

PROCEDURE addModuleToScope (m, i: node) ;
BEGIN
   assert (getDeclScope () = m) ;
   IF lookupSym (getSymName (i))=NIL
   THEN
      i := addToScope (i)
   END
END addModuleToScope ;


(*
   addImportedModule - add module, i, to be imported by, m.
                       If scoped then module, i, is added to the
                       module, m, scope.
*)

PROCEDURE addImportedModule (m, i: node; scoped: BOOLEAN) ;
BEGIN
   assert (isDef (i) OR isModule (i)) ;
   IF isDef (m)
   THEN
      IncludeIndiceIntoIndex (m^.defF.importedModules, i)
   ELSIF isImp (m)
   THEN
      IncludeIndiceIntoIndex (m^.impF.importedModules, i)
   ELSIF isModule (m)
   THEN
      IncludeIndiceIntoIndex (m^.moduleF.importedModules, i)
   ELSE
      HALT
   END ;
   IF scoped
   THEN
      addModuleToScope (m, i)
   END
END addImportedModule ;


(*
   completedEnum - assign boolean enumsComplete to TRUE if a definition,
                   implementation or module symbol.
*)

PROCEDURE completedEnum (n: node) ;
BEGIN
   assert (isDef (n) OR isImp (n) OR isModule (n)) ;
   IF isDef (n)
   THEN
      n^.defF.enumsComplete := TRUE
   ELSIF isImp (n)
   THEN
      n^.impF.enumsComplete := TRUE
   ELSIF isModule (n)
   THEN
      n^.moduleF.enumsComplete := TRUE
   END
END completedEnum ;


(*
   setUnary - sets a unary node to contain, arg, a, and type, t.
*)

PROCEDURE setUnary (u: node; k: nodeT; a, t: node) ;
BEGIN
   CASE k OF

   constexp,
   deref,
   chr,
   cap,
   abs,
   float,
   trunc,
   ord,
   high,
   throw,
   re,
   im,
   not,
   neg,
   adr,
   size,
   tsize,
   min,
   max     :  u^.kind := k ;
              u^.unaryF.arg := a ;
	      u^.unaryF.resultType := t

   END
END setUnary ;


(*
   makeConst - create, initialise and return a const node.
*)

PROCEDURE makeConst (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (const) ;
   WITH d^ DO
      constF.name := n ;
      constF.type := NIL ;
      constF.scope := getDeclScope () ;
      constF.value := NIL
   END ;
   RETURN addToScope (d)
END makeConst ;


(*
   makeType - create, initialise and return a type node.
*)

PROCEDURE makeType (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (type) ;
   WITH d^ DO
      typeF.name := n ;
      typeF.type := NIL ;
      typeF.scope := getDeclScope () ;
      typeF.isHidden := FALSE ;
      typeF.isInternal := FALSE
   END ;
   RETURN addToScope (d)
END makeType ;


(*
   makeTypeImp - lookup a type in the definition module
                 and return it.  Otherwise create a new type.
*)

PROCEDURE makeTypeImp (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := lookupSym (n) ;
   IF d#NIL
   THEN
      d^.typeF.isHidden := FALSE ;
      RETURN addToScope (d)
   ELSE
      d := newNode (type) ;
      WITH d^ DO
         typeF.name := n ;
         typeF.type := NIL ;
         typeF.scope := getDeclScope () ;
         typeF.isHidden := FALSE
      END ;
      RETURN addToScope (d)
   END
END makeTypeImp ;


(*
   makeVar - create, initialise and return a var node.
*)

PROCEDURE makeVar (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (var) ;
   WITH d^ DO
      varF.name := n ;
      varF.type := NIL ;
      varF.decl := NIL ;
      varF.scope := getDeclScope () ;
      varF.isInitialised := FALSE ;
      varF.isParameter := FALSE ;
      varF.isVarParameter := FALSE ;
      initCname (varF.cname)
   END ;
   RETURN addToScope (d)
END makeVar ;


(*
   putVar - places, type, as the type for var.
*)

PROCEDURE putVar (var, type, decl: node) ;
BEGIN
   assert (var#NIL) ;
   assert (isVar (var)) ;
   var^.varF.type := type ;
   var^.varF.decl := decl
END putVar ;


(*
   putVarBool - assigns the four booleans associated with a variable.
*)

PROCEDURE putVarBool (v: node; init, param, isvar, isused: BOOLEAN) ;
BEGIN
   assert (isVar (v)) ;
   v^.varF.isInitialised := init ;
   v^.varF.isParameter := param ;
   v^.varF.isVarParameter := isvar ;
   v^.varF.isUsed := isused
END putVarBool ;


(*
   checkPtr - in C++ we need to create a typedef for a pointer
              in case we need to use reinterpret_cast.
*)

PROCEDURE checkPtr (n: node) : node ;
VAR
   s: String ;
   p: node ;
BEGIN
   IF lang = ansiCP
   THEN
      IF isPointer (n)
      THEN
         s := tempName () ;
         p := makeType (makekey (DynamicStrings.string (s))) ;
         putType (p, n) ;
         s := KillString (s) ;
	 RETURN p
      END
   END ;
   RETURN n
END checkPtr ;


(*
   makeVarDecl - create a vardecl node and create a shadow variable in the
                 current scope.
*)

PROCEDURE makeVarDecl (i: node; type: node) : node ;
VAR
   d, v: node ;
   j, n: CARDINAL ;
BEGIN
   type := checkPtr (type) ;
   d := newNode (vardecl) ;
   WITH d^ DO
      vardeclF.names := i^.identlistF.names ;
      vardeclF.type := type ;
      vardeclF.scope := getDeclScope ()
   END ;
   n := wlists.noOfItemsInList (d^.vardeclF.names) ;
   j := 1 ;
   WHILE j<=n DO
      v := lookupSym (wlists.getItemFromList (d^.vardeclF.names, j)) ;
      assert (isVar (v)) ;
      putVar (v, type, d) ;
      INC (j)
   END ;
   RETURN d
END makeVarDecl ;


(*
   isVarDecl - returns TRUE if, n, is a vardecl node.
*)

PROCEDURE isVarDecl (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = vardecl
END isVarDecl ;


(*
   makeVariablesFromParameters - creates variables which are really parameters.
*)

PROCEDURE makeVariablesFromParameters (proc, id, type: node; isvar, isused: BOOLEAN) ;
VAR
   v   : node ;
   i, n: CARDINAL ;
   m   : Name ;
   s   : String ;
BEGIN
   assert (isProcedure (proc)) ;
   assert (isIdentList (id)) ;
   i := 1 ;
   n := wlists.noOfItemsInList (id^.identlistF.names) ;
   WHILE i<=n DO
      m := wlists.getItemFromList (id^.identlistF.names, i) ;
      v := makeVar (m) ;
      putVar (v, type, NIL) ;
      putVarBool (v, TRUE, TRUE, isvar, isused) ;
      IF debugScopes
      THEN
         printf ("adding parameter variable into top scope\n") ;
         dumpScopes ;
         printf (" variable name is: ") ;
         s := InitStringCharStar (keyToCharStar (m)) ;
         IF KillString (WriteS (StdOut, s))=NIL
         THEN
         END ;
         printf ("\n")
      END ;
      INC (i)
   END
END makeVariablesFromParameters ;


(*
   addProcedureToScope - add a procedure name n and node d to the
                         current scope.
*)

PROCEDURE addProcedureToScope (d: node; n: Name) : node ;
VAR
   m: node ;
   i: CARDINAL ;
BEGIN
   i := HighIndice (scopeStack) ;
   m := GetIndice (scopeStack, i) ;
   IF isDef (m) AND
      (getSymName (m) = makeKey ('M2RTS')) AND
      (getSymName (d) = makeKey ('HALT'))
   THEN
      haltN := d ;
      putSymKey (baseSymbols, n, haltN)
   END ;
   RETURN addToScope (d)
END addProcedureToScope ;


(*
   makeProcedure - create, initialise and return a procedure node.
*)

PROCEDURE makeProcedure (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := lookupSym (n) ;
   IF d=NIL
   THEN
      d := newNode (procedure) ;
      WITH d^ DO
         procedureF.name := n ;
         initDecls (procedureF.decls) ;
         procedureF.scope := getDeclScope () ;
         procedureF.parameters := InitIndex (1) ;
         procedureF.isForC := isDefForCNode (getDeclScope ()) ;
	 procedureF.built := FALSE ;
         procedureF.returnopt := FALSE ;
         procedureF.optarg := NIL ;
	 procedureF.noreturnused := FALSE ;
         procedureF.noreturn := FALSE ;
         procedureF.vararg := FALSE ;
         procedureF.checking := FALSE ;
	 procedureF.paramcount := 0 ;
         procedureF.returnType := NIL ;
         procedureF.beginStatements := NIL ;
         initCname (procedureF.cname) ;
	 procedureF.defComment := NIL ;
	 procedureF.modComment := NIL ;
      END
   END ;
   RETURN addProcedureToScope (d, n)
END makeProcedure ;


(*
   putCommentDefProcedure - remembers the procedure comment (if it exists) as a
                            definition module procedure heading.  NIL is placed
                            if there is no procedure comment available.
*)

PROCEDURE putCommentDefProcedure (n: node) ;
BEGIN
   assert (isProcedure (n)) ;
   IF isProcedureComment (lastcomment)
   THEN
      n^.procedureF.defComment := lastcomment
   END
END putCommentDefProcedure ;


(*
   putCommentModProcedure - remembers the procedure comment (if it exists) as an
                            implementation/program module procedure heading.  NIL is placed
                            if there is no procedure comment available.
*)

PROCEDURE putCommentModProcedure (n: node) ;
BEGIN
   assert (isProcedure (n)) ;
   IF isProcedureComment (lastcomment)
   THEN
      n^.procedureF.modComment := lastcomment
   END
END putCommentModProcedure ;


(*
   paramEnter - reset the parameter count.
*)

PROCEDURE paramEnter (n: node) ;
BEGIN
   assert (isProcedure (n)) ;
   n^.procedureF.paramcount := 0
END paramEnter ;


(*
   paramLeave - set paramater checking to TRUE from now onwards.
*)

PROCEDURE paramLeave (n: node) ;
BEGIN
   assert (isProcedure (n)) ;
   n^.procedureF.checking := TRUE ;
   IF isImp (currentModule) OR isModule (currentModule)
   THEN
      n^.procedureF.built := TRUE
   END
END paramLeave ;


(*
   putReturnType - sets the return type of procedure or proctype, proc, to, type.
*)

PROCEDURE putReturnType (proc, type: node) ;
BEGIN
   assert (isProcedure (proc) OR isProcType (proc)) ;
   IF isProcedure (proc)
   THEN
      proc^.procedureF.returnType := type
   ELSE
      proc^.proctypeF.returnType := type
   END
END putReturnType ;


(*
   putOptReturn - sets, proctype or procedure, proc, to have an optional return type.
*)

PROCEDURE putOptReturn (proc: node) ;
BEGIN
   assert (isProcedure (proc) OR isProcType (proc)) ;
   IF isProcedure (proc)
   THEN
      proc^.procedureF.returnopt := TRUE
   ELSE
      proc^.proctypeF.returnopt := TRUE
   END
END putOptReturn ;


(*
   makeProcType - returns a proctype node.
*)

PROCEDURE makeProcType () : node ;
VAR
   d: node ;
BEGIN
   d := newNode (proctype) ;
   WITH d^ DO
      proctypeF.scope := getDeclScope () ;
      proctypeF.parameters := InitIndex (1) ;
      proctypeF.returnopt := FALSE ;
      proctypeF.optarg := NIL ;
      proctypeF.vararg := FALSE ;
      proctypeF.returnType := NIL
   END ;
   RETURN d
END makeProcType ;


(*
   putProcTypeReturn - sets the return type of, proc, to, type.
*)

PROCEDURE putProcTypeReturn (proc, type: node) ;
BEGIN
   assert (isProcType (proc)) ;
   proc^.proctypeF.returnType := type
END putProcTypeReturn ;


(*
   putProcTypeOptReturn - sets, proc, to have an optional return type.
*)

PROCEDURE putProcTypeOptReturn (proc: node) ;
BEGIN
   assert (isProcType (proc)) ;
   proc^.proctypeF.returnopt := TRUE
END putProcTypeOptReturn ;


(*
   makeNonVarParameter - returns a non var parameter node with, name: type.
*)

PROCEDURE makeNonVarParameter (l: node; type, proc: node; isused: BOOLEAN) : node ;
VAR
   d: node ;
BEGIN
   assert ((l=NIL) OR isIdentList (l)) ;
   d := newNode (param) ;
   d^.paramF.namelist := l ;
   d^.paramF.type := type ;
   d^.paramF.scope := proc ;
   d^.paramF.isUnbounded := FALSE ;
   d^.paramF.isForC := isDefForCNode (proc) ;
   d^.paramF.isUsed := isused ;
   RETURN d
END makeNonVarParameter ;


(*
   makeVarParameter - returns a var parameter node with, name: type.
*)

PROCEDURE makeVarParameter (l: node; type, proc: node; isused: BOOLEAN) : node ;
VAR
   d: node ;
BEGIN
   assert ((l=NIL) OR isIdentList (l)) ;
   d := newNode (varparam) ;
   d^.varparamF.namelist := l ;
   d^.varparamF.type := type ;
   d^.varparamF.scope := proc ;
   d^.varparamF.isUnbounded := FALSE ;
   d^.varparamF.isForC := isDefForCNode (proc) ;
   d^.varparamF.isUsed := isused ;
   RETURN d
END makeVarParameter ;


(*
   makeVarargs - returns a varargs node.
*)

PROCEDURE makeVarargs () : node ;
VAR
   d: node ;
BEGIN
   d := newNode (varargs) ;
   d^.varargsF.scope := NIL ;
   RETURN d
END makeVarargs ;


(*
   isVarargs - returns TRUE if, n, is a varargs node.
*)

PROCEDURE isVarargs (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = varargs
END isVarargs ;


(*
   addParameter - adds a parameter, param, to procedure or proctype, proc.
*)

PROCEDURE addParameter (proc, param: node) ;
BEGIN
   assert (isVarargs (param) OR isParam (param) OR isVarParam (param) OR isOptarg (param)) ;
   CASE proc^.kind OF

   procedure:  IncludeIndiceIntoIndex (proc^.procedureF.parameters, param) ;
               IF isVarargs (param)
               THEN
                  proc^.procedureF.vararg := TRUE
               END ;
	       IF isOptarg (param)
               THEN
                  proc^.procedureF.optarg := param
               END |
   proctype :  IncludeIndiceIntoIndex (proc^.proctypeF.parameters, param) ;
               IF isVarargs (param)
               THEN
                  proc^.proctypeF.vararg := TRUE
               END ;
               IF isOptarg (param)
               THEN
                  proc^.proctypeF.optarg := param
               END

   END
END addParameter ;


(*
   isOptarg - returns TRUE if, n, is an optarg.
*)

PROCEDURE isOptarg (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = optarg
END isOptarg ;


(*
   makeOptParameter - creates and returns an optarg.
*)

PROCEDURE makeOptParameter (l, type, init: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (optarg) ;
   n^.optargF.namelist := l ;
   n^.optargF.type := type ;
   n^.optargF.init := init ;
   n^.optargF.scope := NIL ;
   RETURN n
END makeOptParameter ;


(*
   addOptParameter - returns an optarg which has been created and added to
                     procedure node, proc.  It has a name, id, and, type,
                     and an initial value, init.
*)

PROCEDURE addOptParameter (proc: node; id: Name; type, init: node) : node ;
VAR
   p, l: node ;
BEGIN
   assert (isProcedure (proc)) ;
   l := makeIdentList () ;
   assert (putIdent (l, id)) ;
   checkMakeVariables (proc, l, type, FALSE, TRUE) ;
   IF NOT proc^.procedureF.checking
   THEN
      p := makeOptParameter (l, type, init) ;
      addParameter (proc, p)
   END ;
   RETURN p
END addOptParameter ;


VAR
   globalNode: node ;


(*
   setwatch - assign the globalNode to n.
*)

PROCEDURE setwatch (n: node) : BOOLEAN ;
BEGIN
   globalNode := n ;
   RETURN TRUE
END setwatch ;


(*
   runwatch - set the globalNode to an identlist.
*)

PROCEDURE runwatch () : BOOLEAN ;
BEGIN
   RETURN globalNode^.kind = identlist
END runwatch ;


(*
   makeIdentList - returns a node which will be used to maintain an ident list.
*)

PROCEDURE makeIdentList () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (identlist) ;
   n^.identlistF.names := wlists.initList () ;
   n^.identlistF.cnamed := FALSE ;
   RETURN n
END makeIdentList ;


(*
   isIdentList - returns TRUE if, n, is an identlist.
*)

PROCEDURE isIdentList (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = identlist
END isIdentList ;


(*
   putIdent - places ident, i, into identlist, n.  It returns TRUE if
              ident, i, is unique.
*)

PROCEDURE putIdent (n: node; i: Name) : BOOLEAN ;
BEGIN
   assert (isIdentList (n)) ;
   IF wlists.isItemInList (n^.identlistF.names, i)
   THEN
      RETURN FALSE
   ELSE
      wlists.putItemIntoList (n^.identlistF.names, i) ;
      RETURN TRUE
   END
END putIdent ;


(*
   identListLen - returns the length of identlist.
*)

PROCEDURE identListLen (n: node) : CARDINAL ;
BEGIN
   IF n=NIL
   THEN
      RETURN 0
   ELSE
      assert (isIdentList (n)) ;
      RETURN wlists.noOfItemsInList (n^.identlistF.names)
   END
END identListLen ;


(*
   checkParameters - placeholder for future parameter checking.
*)

PROCEDURE checkParameters (p: node; i: node; type: node; isvar, isused: BOOLEAN) ;
BEGIN
   (* do check.  *)
   disposeNode (i)
END checkParameters ;

(*
(*
   avoidCnames - checks each name in, n, against C reserved
                 keywords and macros.
*)

PROCEDURE avoidCnames (n: node) ;
VAR
   i, j: CARDINAL ;
BEGIN
   assert (isIdentList (n)) ;
   IF NOT n^.identlistF.cnamed
   THEN
      n^.identlistF.cnamed := TRUE ;
      j := wlists.noOfItemsInList (n^.identlistF.names) ;
      i := 1 ;
      WHILE i<=j DO
         wlists.replaceItemInList (n^.identlistF.names,
                                   i,
                                   keyc.cnamen (wlists.getItemFromList (n^.identlistF.names, i), FALSE)) ;
         INC (i)
      END
   END
END avoidCnames ;
*)


(*
   checkMakeVariables - create shadow local variables for parameters providing that
                        procedure n has not already been built and we are compiling
                        a module or an implementation module.
*)

PROCEDURE checkMakeVariables (n, i, type: node; isvar, isused: BOOLEAN) ;
BEGIN
   IF (isImp (currentModule) OR isModule (currentModule)) AND
      (NOT n^.procedureF.built)
   THEN
      makeVariablesFromParameters (n, i, type, isvar, isused)
   END ;
END checkMakeVariables ;


(*
   addVarParameters - adds the identlist, i, of, type, to be VAR parameters
                      in procedure, n.
*)

PROCEDURE addVarParameters (n: node; i: node; type: node; isused: BOOLEAN) ;
VAR
   p: node ;
BEGIN
   assert (isIdentList (i)) ;
   assert (isProcedure (n)) ;
   checkMakeVariables (n, i, type, TRUE, isused) ;
   IF n^.procedureF.checking
   THEN
      checkParameters (n, i, type, TRUE, isused)  (* will destroy, i.  *)
   ELSE
      p := makeVarParameter (i, type, n, isused) ;
      IncludeIndiceIntoIndex (n^.procedureF.parameters, p) ;
   END ;
END addVarParameters ;


(*
   addNonVarParameters - adds the identlist, i, of, type, to be parameters
                         in procedure, n.
*)

PROCEDURE addNonVarParameters (n: node; i: node; type: node; isused: BOOLEAN) ;
VAR
   p: node ;
BEGIN
   assert (isIdentList (i)) ;
   assert (isProcedure (n)) ;
   checkMakeVariables (n, i, type, FALSE, isused) ;
   IF n^.procedureF.checking
   THEN
      checkParameters (n, i, type, FALSE, isused)  (* will destroy, i.  *)
   ELSE
      p := makeNonVarParameter (i, type, n, isused) ;
      IncludeIndiceIntoIndex (n^.procedureF.parameters, p)
   END ;
END addNonVarParameters ;


(*
   makeSubrange - returns a subrange node, built from range: low..high.
*)

PROCEDURE makeSubrange (low, high: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (subrange) ;
   n^.subrangeF.low := low ;
   n^.subrangeF.high := high ;
   n^.subrangeF.type := NIL ;
   n^.subrangeF.scope := getDeclScope () ;
   RETURN n
END makeSubrange ;


(*
   putSubrangeType - assigns, type, to the subrange type, sub.
*)

PROCEDURE putSubrangeType (sub, type: node) ;
BEGIN
   assert (isSubrange (sub)) ;
   sub^.subrangeF.type := type
END putSubrangeType ;


(*
   makeSet - returns a set of, type, node.
*)

PROCEDURE makeSet (type: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (set) ;
   n^.setF.type := type ;
   n^.setF.scope := getDeclScope () ;
   RETURN n
END makeSet ;


(*
   makeSetValue - creates and returns a setvalue node.
*)

PROCEDURE makeSetValue () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (setvalue) ;
   n^.setvalueF.type := bitsetN ;
   n^.setvalueF.values := InitIndex (1) ;
   RETURN n
END makeSetValue ;


(*
   isSetValue - returns TRUE if, n, is a setvalue node.
*)

PROCEDURE isSetValue (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = setvalue
END isSetValue ;


(*
   putSetValue - assigns the type, t, to the set value, n.  The
                 node, n, is returned.
*)

PROCEDURE putSetValue (n, t: node) : node ;
BEGIN
   assert (isSetValue (n)) ;
   n^.setvalueF.type := t ;
   RETURN n
END putSetValue ;


(*
   includeSetValue - includes the range l..h into the setvalue.
                     h might be NIL indicating that a single element
                       is to be included into the set.
                     n is returned.
*)

PROCEDURE includeSetValue (n: node; l, h: node) : node ;
BEGIN
   assert (isSetValue (n)) ;
   IncludeIndiceIntoIndex (n^.setvalueF.values, l) ;
   RETURN n
END includeSetValue ;


(*
   makePointer - returns a pointer of, type, node.
*)

PROCEDURE makePointer (type: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (pointer) ;
   n^.pointerF.type := type ;
   n^.pointerF.scope := getDeclScope () ;
   RETURN n
END makePointer ;


(*
   makeArray - returns a node representing ARRAY subr OF type.
*)

PROCEDURE makeArray (subr, type: node) : node ;
VAR
   n, s: node ;
BEGIN
   s := skipType (subr) ;
   assert (isSubrange (s) OR isOrdinal (s) OR isEnumeration (s)) ;
   n := newNode (array) ;
   n^.arrayF.subr := subr ;
   n^.arrayF.type := type ;
   n^.arrayF.scope := getDeclScope () ;
   n^.arrayF.isUnbounded := FALSE ;
   RETURN n
END makeArray ;


(*
   makeRecord - creates and returns a record node.
*)

PROCEDURE makeRecord () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (record) ;
   n^.recordF.localSymbols := initTree () ;
   n^.recordF.listOfSons := InitIndex (1) ;
   n^.recordF.scope := getDeclScope () ;
   RETURN n
END makeRecord ;


(*
   addFieldsToRecord - adds fields, i, of type, t, into a record, r.
                       It returns, r.
*)

PROCEDURE addFieldsToRecord (r, v, i, t: node) : node ;
VAR
   p, fj: node ;
   j, n : CARDINAL ;
   fn   : Name ;
BEGIN
   IF isRecord (r)
   THEN
      p := r ;
      v := NIL
   ELSE
      p := getRecord (getParent (r)) ;
      assert (isVarientField (r)) ;
      assert (isVarient (v)) ;
      putFieldVarient (r, v)
   END ;
   n := wlists.noOfItemsInList (i^.identlistF.names) ;
   j := 1 ;
   WHILE j<=n DO
      fn := wlists.getItemFromList (i^.identlistF.names, j) ;
      fj := getSymKey (p^.recordF.localSymbols, n) ;
      IF fj=NIL
      THEN
         fj := putFieldRecord (r, fn, t, v)
      ELSE
         metaErrors2 ('record field {%1ad} has already been declared inside a {%2Dd} {%2a}',
                      'attempting to declare a duplicate record field', fj, p)
      END ;
      INC (j)
   END ;
   RETURN r;
END addFieldsToRecord ;


(*
   makeVarient - creates a new symbol, a varient symbol for record or varient field
                 symbol, r.
*)

PROCEDURE makeVarient (r: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (varient) ;
   WITH n^ DO
      varientF.listOfSons := InitIndex (1) ;
      (* do we need to remember our parent (r) ?  *)
      (* if so use this   n^.varientF.parent := r  *)
      IF isRecord (r)
      THEN
         varientF.varient := NIL
      ELSE
         varientF.varient := r
      END ;
      varientF.tag := NIL ;
      varientF.scope := getDeclScope () ;
   END ;
   (* now add, n, to the record/varient, r, field list *)
   WITH r^ DO
      CASE kind OF

      record      :  IncludeIndiceIntoIndex (recordF.listOfSons, n) |
      varientfield:  IncludeIndiceIntoIndex (varientfieldF.listOfSons, n)

      END
   END ;
   RETURN n
END makeVarient ;


(*
   buildVarientFieldRecord - builds a varient field into a varient symbol, v.
                             The varient field is returned.
*)

PROCEDURE buildVarientFieldRecord (v: node; p: node) : node ;
VAR
   f: node ;
BEGIN
   assert (isVarient (v)) ;
   f := makeVarientField (v, p) ;
   assert (isVarientField (f)) ;
   putFieldVarient (f, v) ;
   RETURN f
END buildVarientFieldRecord ;


(*
   makeVarientField - create a varient field within varient, v,
                      The new varient field is returned.
*)

PROCEDURE makeVarientField (v: node; p: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (varientfield) ;
   WITH n^.varientfieldF DO
      name := NulName ;
      parent := p ;
      varient := v ;
      simple := FALSE ;
      listOfSons := InitIndex (1) ;
      scope := getDeclScope ()
   END ;
   RETURN n
END makeVarientField ;


(*
   putFieldVarient - places the field varient, f, as a brother to, the
                     varient symbol, v, and also tells, f, that its varient
                     parent is, v.
*)

PROCEDURE putFieldVarient (f, v: node) ;
BEGIN
   assert (isVarient (v)) ;
   assert (isVarientField (f)) ;
   WITH v^ DO
      CASE kind OF

      varient:  IncludeIndiceIntoIndex (varientF.listOfSons, f)

      END
   END ;
   WITH f^ DO
      CASE kind OF

      varientfield:  varientfieldF.varient := v

      END
   END
END putFieldVarient ;


(*
   putFieldRecord - create a new recordfield and place it into record r.
                    The new field has a tagname and type and can have a
                    variant field v.
*)

PROCEDURE putFieldRecord (r: node; tag: Name; type, v: node) : node ;
VAR
   f, n, p: node ;
BEGIN
   n := newNode (recordfield) ;
   WITH r^ DO
      CASE kind OF

      record:  IncludeIndiceIntoIndex (recordF.listOfSons, n) ;
               (* ensure that field, n, is in the parents Local Symbols.  *)
               IF tag#NulName
               THEN
                  IF getSymKey (recordF.localSymbols, tag) = NulKey
                  THEN
                     putSymKey (recordF.localSymbols, tag, n)
                  ELSE
                     f := getSymKey (recordF.localSymbols, tag) ;
                     metaErrors1 ('field record {%1Dad} has already been declared',
                                  'field record duplicate', f)
                  END
               END |
      varientfield:  IncludeIndiceIntoIndex (varientfieldF.listOfSons, n) ;
                     p := getParent (r) ;
                     assert (p^.kind=record) ;
                     IF tag#NulName
                     THEN
                        putSymKey (p^.recordF.localSymbols, tag, n)
                     END

      END
   END ;
   (* fill in, n.  *)
   n^.recordfieldF.type := type ;
   n^.recordfieldF.name := tag ;
   n^.recordfieldF.parent := r ;
   n^.recordfieldF.varient := v ;
   n^.recordfieldF.tag := FALSE ;
   n^.recordfieldF.scope := NIL ;
   initCname (n^.recordfieldF.cname) ;
   (*
   IF r^.kind=record
   THEN
      doRecordM2 (doP, r)
   END ;
   *)
   RETURN n
END putFieldRecord ;


(*
   buildVarientSelector - builds a field of name, tag, of, type onto:
                          record or varient field, r.
                          varient, v.
*)

PROCEDURE buildVarientSelector (r, v: node; tag: Name; type: node) ;
VAR
   f: node ;
BEGIN
   assert (isRecord (r) OR isVarientField (r)) ;
   IF isRecord (r) OR isVarientField (r)
   THEN
      IF (type=NIL) AND (tag=NulName)
      THEN
         metaError1 ('expecting a tag field in the declaration of a varient record {%1Ua}', r)
      ELSIF type=NIL
      THEN
         f := lookupSym (tag) ;
         putVarientTag (v, f)
      ELSE
         f := putFieldRecord (r, tag, type, v) ;
	 assert (isRecordField (f)) ;
         f^.recordfieldF.tag := TRUE ;
         putVarientTag (v, f)
      END
   END
END buildVarientSelector ;


(*
   ensureOrder - ensures that, a, and, b, exist in, i, and also
                 ensure that, a, is before, b.
*)

PROCEDURE ensureOrder (i: Index; a, b: node) ;
BEGIN
   assert (IsIndiceInIndex (i, a)) ;
   assert (IsIndiceInIndex (i, b)) ;
   RemoveIndiceFromIndex (i, a) ;
   RemoveIndiceFromIndex (i, b) ;
   IncludeIndiceIntoIndex (i, a) ;
   IncludeIndiceIntoIndex (i, b) ;
   assert (IsIndiceInIndex (i, a)) ;
   assert (IsIndiceInIndex (i, b))
END ensureOrder ;


(*
   putVarientTag - places tag into variant v.
*)

PROCEDURE putVarientTag (v: node; tag: node) ;
VAR
   p: node ;
BEGIN
   assert (isVarient (v)) ;
   CASE v^.kind OF

   varient:  v^.varientF.tag := tag

   END
END putVarientTag ;


(*
   getParent - returns the parent field of recordfield or varientfield symbol, n.
*)

PROCEDURE getParent (n: node) : node ;
BEGIN
   CASE n^.kind OF

   recordfield:  RETURN n^.recordfieldF.parent |
   varientfield:  RETURN n^.varientfieldF.parent

   END
END getParent ;


(*
   getRecord - returns the record associated with node, n.
               (Parental record).
*)

PROCEDURE getRecord (n: node) : node ;
BEGIN
   assert (n^.kind # varient) ;  (* if this fails then we need to add parent field to varient.  *)
   CASE n^.kind OF

   record      :  RETURN n |
   varientfield:  RETURN getRecord (getParent (n))

   END
END getRecord ;


(*
   putUnbounded - sets array, n, as unbounded.
*)

PROCEDURE putUnbounded (n: node) ;
BEGIN
   assert (n^.kind = array) ;
   n^.arrayF.isUnbounded := TRUE
END putUnbounded ;


(*
   isConstExp - return TRUE if the node kind is a constexp.
*)

PROCEDURE isConstExp (c: node) : BOOLEAN ;
BEGIN
   assert (c#NIL) ;
   RETURN c^.kind = constexp
END isConstExp ;


(*
   addEnumToModule - adds enumeration type, e, into the list of enums
                     in module, m.
*)

PROCEDURE addEnumToModule (m, e: node) ;
BEGIN
   assert (isEnumeration (e) OR isEnumerationField (e)) ;
   assert (isModule (m) OR isDef (m) OR isImp (m)) ;
   IF isModule (m)
   THEN
      IncludeIndiceIntoIndex (m^.moduleF.enumFixup.info, e)
   ELSIF isDef (m)
   THEN
      IncludeIndiceIntoIndex (m^.defF.enumFixup.info, e)
   ELSIF isImp (m)
   THEN
      IncludeIndiceIntoIndex (m^.impF.enumFixup.info, e)
   END
END addEnumToModule ;


(*
   getNextFixup - return the next fixup from from f.
*)

PROCEDURE getNextFixup (VAR f: fixupInfo) : node ;
BEGIN
   INC (f.count) ;
   RETURN GetIndice (f.info, f.count)
END getNextFixup ;


(*
   getNextEnum - returns the next enumeration node.
*)

PROCEDURE getNextEnum () : node ;
VAR
   n: node ;
BEGIN
   n := NIL ;
   assert (isDef (currentModule) OR isImp (currentModule) OR isModule (currentModule)) ;
   WITH currentModule^ DO
      IF isDef (currentModule)
      THEN
         n := getNextFixup (defF.enumFixup)
      ELSIF isImp (currentModule)
      THEN
         n := getNextFixup (impF.enumFixup)
      ELSIF isModule (currentModule)
      THEN
         n := getNextFixup (moduleF.enumFixup)
      END
   END ;
   assert (n # NIL) ;
   assert (isEnumeration (n) OR isEnumerationField (n)) ;
   RETURN n
END getNextEnum ;


(*
   resetEnumPos - resets the index into the saved list of enums inside
                  module, n.
*)

PROCEDURE resetEnumPos (n: node) ;
BEGIN
   assert (isDef (n) OR isImp (n) OR isModule (n)) ;
   IF isDef (n)
   THEN
      n^.defF.enumFixup.count := 0
   ELSIF isImp (n)
   THEN
      n^.impF.enumFixup.count := 0
   ELSIF isModule (n)
   THEN
      n^.moduleF.enumFixup.count := 0
   END
END resetEnumPos ;


(*
   getEnumsComplete - gets the field from the def or imp or module, n.
*)

PROCEDURE getEnumsComplete (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   def   :  RETURN n^.defF.enumsComplete |
   imp   :  RETURN n^.impF.enumsComplete |
   module:  RETURN n^.moduleF.enumsComplete

   END
END getEnumsComplete ;


(*
   setEnumsComplete - sets the field inside the def or imp or module, n.
*)

PROCEDURE setEnumsComplete (n: node) ;
BEGIN
   CASE n^.kind OF

   def   :  n^.defF.enumsComplete := TRUE |
   imp   :  n^.impF.enumsComplete := TRUE |
   module:  n^.moduleF.enumsComplete := TRUE

   END
END setEnumsComplete ;


(*
   doMakeEnum - create an enumeration type and add it to the current module.
*)

PROCEDURE doMakeEnum () : node ;
VAR
   e: node ;
BEGIN
   e := newNode (enumeration) ;
   WITH e^ DO
      enumerationF.noOfElements := 0 ;
      enumerationF.localSymbols := initTree () ;
      enumerationF.scope := getDeclScope () ;
      enumerationF.listOfSons := InitIndex (1) ;
      enumerationF.low := NIL ;
      enumerationF.high := NIL ;
   END ;
   addEnumToModule (currentModule, e) ;
   RETURN e
END doMakeEnum ;


(*
   makeEnum - creates an enumerated type and returns the node.
*)

PROCEDURE makeEnum () : node ;
BEGIN
   IF (currentModule#NIL) AND getEnumsComplete (currentModule)
   THEN
      RETURN getNextEnum ()
   ELSE
      RETURN doMakeEnum ()
   END
END makeEnum ;


(*
   doMakeEnumField - create an enumeration field name and add it to enumeration e.
                     Return the new field.
*)

PROCEDURE doMakeEnumField (e: node; n: Name) : node ;
VAR
   f: node ;
BEGIN
   assert (isEnumeration (e)) ;
   f := lookupSym (n) ;
   IF f=NIL
   THEN
      f := newNode (enumerationfield) ;
      putSymKey (e^.enumerationF.localSymbols, n, f) ;
      IncludeIndiceIntoIndex (e^.enumerationF.listOfSons, f) ;
      WITH f^ DO
         enumerationfieldF.name := n ;
         enumerationfieldF.type := e ;
         enumerationfieldF.scope := getDeclScope () ;
         enumerationfieldF.value := e^.enumerationF.noOfElements ;
         initCname (enumerationfieldF.cname)
      END ;
      INC (e^.enumerationF.noOfElements) ;
      assert (GetIndice (e^.enumerationF.listOfSons, e^.enumerationF.noOfElements) = f) ;
      addEnumToModule (currentModule, f) ;
      IF e^.enumerationF.low = NIL
      THEN
         e^.enumerationF.low := f
      END ;
      e^.enumerationF.high := f ;
      RETURN addToScope (f)
   ELSE
      metaErrors2 ('cannot create enumeration field {%1k} as the name is already in use',
                   '{%2DMad} was declared elsewhere', n, f)
   END ;
   RETURN f
END doMakeEnumField ;


(*
   makeEnumField - returns an enumeration field, named, n.
*)

PROCEDURE makeEnumField (e: node; n: Name) : node ;
BEGIN
   IF (currentModule#NIL) AND getEnumsComplete (currentModule)
   THEN
      RETURN getNextEnum ()
   ELSE
      RETURN doMakeEnumField (e, n)
   END
END makeEnumField ;


(*
   isEnumeration - returns TRUE if node, n, is an enumeration type.
*)

PROCEDURE isEnumeration (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = enumeration
END isEnumeration ;


(*
   makeExpList - creates and returns an expList node.
*)

PROCEDURE makeExpList () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (explist) ;
   n^.explistF.exp := InitIndex (1) ;
   RETURN n
END makeExpList ;


(*
   isExpList - returns TRUE if, n, is an explist node.
*)

PROCEDURE isExpList (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = explist
END isExpList ;


(*
   putExpList - places, expression, e, within the explist, n.
*)

PROCEDURE putExpList (n: node; e: node) ;
BEGIN
   assert (n # NIL) ;
   assert (isExpList (n)) ;
   PutIndice (n^.explistF.exp, HighIndice (n^.explistF.exp) + 1, e)
END putExpList ;


(*
   getExpList - returns the, n, th argument in an explist.
*)

PROCEDURE getExpList (p: node; n: CARDINAL) : node ;
BEGIN
   assert (p#NIL) ;
   assert (isExpList (p)) ;
   assert (n <= HighIndice (p^.explistF.exp)) ;
   RETURN GetIndice (p^.explistF.exp, n)
END getExpList ;


(*
   expListLen - returns the length of explist, p.
*)

PROCEDURE expListLen (p: node) : CARDINAL ;
BEGIN
   IF p = NIL
   THEN
      RETURN 0
   ELSE
      assert (isExpList (p)) ;
      RETURN HighIndice (p^.explistF.exp)
   END
END expListLen ;


(*
   getConstExpComplete - gets the field from the def or imp or module, n.
*)

PROCEDURE getConstExpComplete (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   def   :  RETURN n^.defF.constsComplete |
   imp   :  RETURN n^.impF.constsComplete |
   module:  RETURN n^.moduleF.constsComplete

   END
END getConstExpComplete ;


(*
   setConstExpComplete - sets the field inside the def or imp or module, n.
*)

PROCEDURE setConstExpComplete (n: node) ;
BEGIN
   CASE n^.kind OF

   def   :  n^.defF.constsComplete := TRUE |
   imp   :  n^.impF.constsComplete := TRUE |
   module:  n^.moduleF.constsComplete := TRUE

   END
END setConstExpComplete ;


(*
   getNextConstExp - returns the next constexp node.
*)

PROCEDURE getNextConstExp () : node ;
VAR
   n: node ;
BEGIN
   assert (isDef (currentModule) OR isImp (currentModule) OR isModule (currentModule)) ;
   WITH currentModule^ DO
      IF isDef (currentModule)
      THEN
         RETURN getNextFixup (defF.constFixup)
      ELSIF isImp (currentModule)
      THEN
         RETURN getNextFixup (impF.constFixup)
      ELSIF isModule (currentModule)
      THEN
         RETURN getNextFixup (moduleF.constFixup)
      END
   END ;
   RETURN n
END getNextConstExp ;


(*
   resetConstExpPos - resets the index into the saved list of constexps inside
                      module, n.
*)

PROCEDURE resetConstExpPos (n: node) ;
BEGIN
   assert (isDef (n) OR isImp (n) OR isModule (n)) ;
   IF isDef (n)
   THEN
      n^.defF.constFixup.count := 0
   ELSIF isImp (n)
   THEN
      n^.impF.constFixup.count := 0
   ELSIF isModule (n)
   THEN
      n^.moduleF.constFixup.count := 0
   END
END resetConstExpPos ;


(*
   addConstToModule - adds const exp, e, into the list of constant
                      expressions in module, m.
*)

PROCEDURE addConstToModule (m, e: node) ;
BEGIN
   assert (isModule (m) OR isDef (m) OR isImp (m)) ;
   IF isModule (m)
   THEN
      IncludeIndiceIntoIndex (m^.moduleF.constFixup.info, e)
   ELSIF isDef (m)
   THEN
      IncludeIndiceIntoIndex (m^.defF.constFixup.info, e)
   ELSIF isImp (m)
   THEN
      IncludeIndiceIntoIndex (m^.impF.constFixup.info, e)
   END
END addConstToModule ;


(*
   doMakeConstExp - create a constexp node and add it to the current module.
*)

PROCEDURE doMakeConstExp () : node ;
VAR
   c: node ;
BEGIN
   c := makeUnary (constexp, NIL, NIL) ;
   addConstToModule (currentModule, c) ;
   RETURN c
END doMakeConstExp ;


(*
   makeConstExp - returns a constexp node.
*)

PROCEDURE makeConstExp () : node ;
BEGIN
   IF (currentModule#NIL) AND getConstExpComplete (currentModule)
   THEN
      RETURN getNextConstExp ()
   ELSE
      RETURN doMakeConstExp ()
   END
END makeConstExp ;


(*
   fixupConstExp - assign fixup expression, e, into the argument of, c.
*)

PROCEDURE fixupConstExp (c, e: node) : node ;
BEGIN
   assert (isConstExp (c)) ;
   c^.unaryF.arg := e ;
   RETURN c
END fixupConstExp ;


(*
   isAnyType - return TRUE if node n is any type kind.
*)

PROCEDURE isAnyType (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   CASE n^.kind OF

   address,
   loc,
   byte,
   word,
   char,
   cardinal,
   longcard,
   shortcard,
   integer,
   longint,
   shortint,
   complex,
   longcomplex,
   shortcomplex,
   bitset,
   boolean,
   proc,
   type     :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isAnyType ;


(*
   makeVal - creates a VAL (type, expression) node.
*)

PROCEDURE makeVal (params: node) : node ;
BEGIN
   assert (isExpList (params)) ;
   IF expListLen (params) = 2
   THEN
      RETURN makeBinary (val,
                         getExpList (params, 1),
                         getExpList (params, 2),
                         getExpList (params, 1))
   ELSE
      HALT
   END
END makeVal ;


(*
   makeCast - creates a cast node TYPENAME (expr).
*)

PROCEDURE makeCast (c, p: node) : node ;
BEGIN
   assert (isExpList (p)) ;
   IF expListLen (p) = 1
   THEN
      RETURN makeBinary (cast, c, getExpList (p, 1), c)
   ELSE
      HALT
   END
END makeCast ;


(*
   makeIntrisicProc - create an intrinsic node.
*)

PROCEDURE makeIntrinsicProc (k: nodeT; noArgs: CARDINAL; p: node) : node ;
VAR
   f: node ;
BEGIN
   f := newNode (k) ;
   f^.intrinsicF.args := p ;
   f^.intrinsicF.noArgs := noArgs ;
   f^.intrinsicF.type := NIL ;
   f^.intrinsicF.postUnreachable := (k = halt) ;
   initPair (f^.intrinsicF.intrinsicComment) ;
   RETURN f
END makeIntrinsicProc ;


(*
   makeIntrinsicUnaryType - create an intrisic unary type.
*)

PROCEDURE makeIntrinsicUnaryType (k: nodeT; paramList: node; returnType: node) : node ;
BEGIN
   RETURN makeUnary (k, getExpList (paramList, 1), returnType)
END makeIntrinsicUnaryType ;


(*
   makeIntrinsicBinaryType - create an intrisic binary type.
*)

PROCEDURE makeIntrinsicBinaryType (k: nodeT; paramList: node; returnType: node) : node ;
BEGIN
   RETURN makeBinary (k, getExpList (paramList, 1), getExpList (paramList, 2), returnType)
END makeIntrinsicBinaryType ;


(*
   checkIntrinsic - checks to see if the function call to, c, with
                    parameter list, n, is really an intrinic.  If it
                    is an intrinic then an intrinic node is created
                    and returned.  Otherwise NIL is returned.
*)

PROCEDURE checkIntrinsic (c, n: node) : node ;
BEGIN
   IF isAnyType (c)
   THEN
      RETURN makeCast (c, n)
   ELSIF c = maxN
   THEN
      RETURN makeIntrinsicUnaryType (max, n, NIL)
   ELSIF c = minN
   THEN
      RETURN makeIntrinsicUnaryType (min, n, NIL)
   ELSIF c = haltN
   THEN
      RETURN makeIntrinsicProc (halt, expListLen (n), n)
   ELSIF c = valN
   THEN
      RETURN makeVal (n)
   ELSIF c = adrN
   THEN
      RETURN makeIntrinsicUnaryType (adr, n, addressN)
   ELSIF c = sizeN
   THEN
      RETURN makeIntrinsicUnaryType (size, n, cardinalN)
   ELSIF c = tsizeN
   THEN
      RETURN makeIntrinsicUnaryType (tsize, n, cardinalN)
   ELSIF c = floatN
   THEN
      RETURN makeIntrinsicUnaryType (float, n, realN)
   ELSIF c = truncN
   THEN
      RETURN makeIntrinsicUnaryType (trunc, n, integerN)
   ELSIF c = ordN
   THEN
      RETURN makeIntrinsicUnaryType (ord, n, cardinalN)
   ELSIF c = chrN
   THEN
      RETURN makeIntrinsicUnaryType (chr, n, charN)
   ELSIF c = capN
   THEN
      RETURN makeIntrinsicUnaryType (cap, n, charN)
   ELSIF c = absN
   THEN
      RETURN makeIntrinsicUnaryType (abs, n, NIL)
   ELSIF c = imN
   THEN
      RETURN makeIntrinsicUnaryType (im, n, NIL)
   ELSIF c = reN
   THEN
      RETURN makeIntrinsicUnaryType (re, n, NIL)
   ELSIF c = cmplxN
   THEN
      RETURN makeIntrinsicBinaryType (cmplx, n, NIL)
   ELSIF c = highN
   THEN
      RETURN makeIntrinsicUnaryType (high, n, cardinalN)
   ELSIF c = incN
   THEN
      RETURN makeIntrinsicProc (inc, expListLen (n), n)
   ELSIF c = decN
   THEN
      RETURN makeIntrinsicProc (dec, expListLen (n), n)
   ELSIF c = inclN
   THEN
      RETURN makeIntrinsicProc (incl, expListLen (n), n)
   ELSIF c = exclN
   THEN
      RETURN makeIntrinsicProc (excl, expListLen (n), n)
   ELSIF c = newN
   THEN
      RETURN makeIntrinsicProc (new, 1, n)
   ELSIF c = disposeN
   THEN
      RETURN makeIntrinsicProc (dispose, 1, n)
   ELSIF c = lengthN
   THEN
      RETURN makeIntrinsicUnaryType (length, n, cardinalN)
   ELSIF c = throwN
   THEN
      keyc.useThrow ;
      RETURN makeIntrinsicProc (throw, 1, n)
   END ;
   RETURN NIL
END checkIntrinsic ;


(*
   checkCHeaders - check to see if the function is a C system function and
                   requires a header file included.
*)

PROCEDURE checkCHeaders (c: node) ;
VAR
   name: Name ;
   s   : node ;
BEGIN
   IF isProcedure (c)
   THEN
      s := getScope (c) ;
      IF getSymName (s) = makeKey ('libc')
      THEN
         name := getSymName (c) ;
         IF (name = makeKey ('read')) OR
            (name = makeKey ('write')) OR
            (name = makeKey ('open')) OR
            (name = makeKey ('close'))
         THEN
            keyc.useUnistd
         END
      END
   END
END checkCHeaders ;


(*
   makeFuncCall - builds a function call to c with param list, n.
*)

PROCEDURE makeFuncCall (c, n: node) : node ;
VAR
   f: node ;
BEGIN
   assert ((n=NIL) OR isExpList (n)) ;
   IF (c = haltN) AND
      (getMainModule () # lookupDef (makeKey ('M2RTS'))) AND
      (getMainModule () # lookupImp (makeKey ('M2RTS')))
   THEN
      addImportedModule (getMainModule (), lookupDef (makeKey ('M2RTS')), FALSE)
   END ;
   f := checkIntrinsic (c, n) ;
   checkCHeaders (c) ;
   IF f = NIL
   THEN
      f := newNode (funccall) ;
      f^.funccallF.function := c ;
      f^.funccallF.args := n ;
      f^.funccallF.type := NIL ;
      initPair (f^.funccallF.funccallComment)
   END ;
   RETURN f
END makeFuncCall ;


(*
   isFuncCall - returns TRUE if, n, is a function/procedure call.
*)

PROCEDURE isFuncCall (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = funccall
END isFuncCall ;


(*
   putType - places, exp, as the type alias to des.
             TYPE des = exp ;
*)

PROCEDURE putType (des, exp: node) ;
BEGIN
   assert (des#NIL) ;
   assert (isType (des)) ;
   des^.typeF.type := exp
END putType ;


(*
   putTypeHidden - marks type, des, as being a hidden type.
                   TYPE des ;
*)

PROCEDURE putTypeHidden (des: node) ;
VAR
   s: node ;
BEGIN
   assert (des#NIL) ;
   assert (isType (des)) ;
   des^.typeF.isHidden := TRUE ;
   s := getScope (des) ;
   assert (isDef (s)) ;
   s^.defF.hasHidden := TRUE
END putTypeHidden ;


(*
   isTypeHidden - returns TRUE if type, n, is hidden.
*)

PROCEDURE isTypeHidden (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   assert (isType (n)) ;
   RETURN n^.typeF.isHidden
END isTypeHidden ;


(*
   hasHidden - returns TRUE if module, n, has a hidden type.
*)

PROCEDURE hasHidden (n: node) : BOOLEAN ;
BEGIN
   assert (isDef (n)) ;
   RETURN n^.defF.hasHidden
END hasHidden ;


(*
   putTypeInternal - marks type, des, as being an internally generated type.
*)

PROCEDURE putTypeInternal (des: node) ;
BEGIN
   assert (des#NIL) ;
   assert (isType (des)) ;
   des^.typeF.isInternal := TRUE
END putTypeInternal ;


(*
   isTypeInternal - returns TRUE if type, n, is internal.
*)

PROCEDURE isTypeInternal (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   assert (isType (n)) ;
   RETURN n^.typeF.isInternal
END isTypeInternal ;


(*
   putConst - places value, v, into node, n.
*)

PROCEDURE putConst (n: node; v: node) ;
BEGIN
   assert (isConst (n)) ;
   n^.constF.value := v
END putConst ;


(*
   makeLiteralInt - creates and returns a literal node based on an integer type.
*)

PROCEDURE makeLiteralInt (n: Name) : node ;
VAR
   m: node ;
   s: String ;
BEGIN
   m := newNode (literal) ;
   s := InitStringCharStar (keyToCharStar (n)) ;
   WITH m^ DO
      literalF.name := n ;
      IF DynamicStrings.char (s, -1)='C'
      THEN
         literalF.type := charN
      ELSE
         literalF.type := ztypeN
      END
   END ;
   s := KillString (s) ;
   RETURN m
END makeLiteralInt ;


(*
   makeLiteralReal - creates and returns a literal node based on a real type.
*)

PROCEDURE makeLiteralReal (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := newNode (literal) ;
   WITH m^ DO
      literalF.name := n ;
      literalF.type := rtypeN
   END ;
   RETURN m
END makeLiteralReal ;


(*
   makeString - creates and returns a node containing string, n.
*)

PROCEDURE makeString (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := newNode (string) ;
   WITH m^ DO
      stringF.name := n ;
      stringF.length := lengthKey (n) ;
      stringF.isCharCompatible := (stringF.length <= 3) ;
      stringF.cstring := toCstring (n) ;
      stringF.clength := lenCstring (stringF.cstring) ;
      IF stringF.isCharCompatible
      THEN
         stringF.cchar := toCchar (n)
      ELSE
         stringF.cchar := NIL
      END
   END ;
   RETURN m
END makeString ;


(*
   getBuiltinConst - creates and returns a builtin const if available.
*)

PROCEDURE getBuiltinConst (n: Name) : node ;
BEGIN
   IF n=makeKey ('BITS_PER_UNIT')
   THEN
      RETURN bitsperunitN
   ELSIF n=makeKey ('BITS_PER_WORD')
   THEN
      RETURN bitsperwordN
   ELSIF n=makeKey ('BITS_PER_CHAR')
   THEN
      RETURN bitspercharN
   ELSIF n=makeKey ('UNITS_PER_WORD')
   THEN
      RETURN unitsperwordN
   ELSE
      RETURN NIL
   END
END getBuiltinConst ;


(*
   lookupInScope - looks up a symbol named, n, from, scope.
*)

PROCEDURE lookupInScope (scope: node; n: Name) : node ;
BEGIN
   CASE scope^.kind OF

   def      :  RETURN getSymKey (scope^.defF.decls.symbols, n) |
   module   :  RETURN getSymKey (scope^.moduleF.decls.symbols, n) |
   imp      :  RETURN getSymKey (scope^.impF.decls.symbols, n) |
   procedure:  RETURN getSymKey (scope^.procedureF.decls.symbols, n) |
   record   :  RETURN getSymKey (scope^.recordF.localSymbols, n)

   END
END lookupInScope ;


(*
   lookupBase - return node named n from the base symbol scope.
*)

PROCEDURE lookupBase (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := getSymKey (baseSymbols, n) ;
   IF m=procN
   THEN
      keyc.useProc
   ELSIF (m=complexN) OR (m=longcomplexN) OR (m=shortcomplexN)
   THEN
      keyc.useComplex
   END ;
   RETURN m
END lookupBase ;


(*
   dumpScopes - display the names of all the scopes stacked.
*)

PROCEDURE dumpScopes ;
VAR
   h: CARDINAL ;
   s: node ;
BEGIN
   h := HighIndice (scopeStack) ;
   printf ("total scopes stacked %d\n", h);

   WHILE h>=1 DO
      s := GetIndice (scopeStack, h) ;
      out2 (" scope [%d] is %s\n", h, s) ;
      DEC (h)
   END
END dumpScopes ;


(*
   out0 - write string a to StdOut.
*)

PROCEDURE out0 (a: ARRAY OF CHAR) ;
VAR
   m: String ;
BEGIN
   m := Sprintf0 (InitString (a)) ;
   m := KillString (WriteS (StdOut, m))
END out0 ;


(*
   out1 - write string a to StdOut using format specifier a.
*)

PROCEDURE out1 (a: ARRAY OF CHAR; s: node) ;
VAR
   m: String ;
   d: CARDINAL ;
BEGIN
   m := getFQstring (s) ;
   IF EqualArray (m, '')
   THEN
      d := VAL (CARDINAL, VAL (LONGCARD, s)) ;
      m := KillString (m) ;
      m := Sprintf1 (InitString ('[%d]'), d)
   END ;
   m := Sprintf1 (InitString (a), m) ;
   m := KillString (WriteS (StdOut, m))
END out1 ;


(*
   out2 - write string a to StdOut using format specifier a.
*)

PROCEDURE out2 (a: ARRAY OF CHAR; c: CARDINAL; s: node) ;
VAR
   m, m1: String ;
BEGIN
   m1 := getString (s) ;
   m := Sprintf2 (InitString (a), c, m1) ;
   m := KillString (WriteS (StdOut, m)) ;
   m1 := KillString (m1)
END out2 ;


(*
   out3 - write string a to StdOut using format specifier a.
*)

PROCEDURE out3 (a: ARRAY OF CHAR; l: CARDINAL; n: Name; s: node) ;
VAR
   m, m1, m2: String ;
BEGIN
   m1 := InitStringCharStar (keyToCharStar (n)) ;
   m2 := getString (s) ;
   m := Sprintf3 (InitString (a), l, m1, m2) ;
   m := KillString (WriteS (StdOut, m)) ;
   m1 := KillString (m1) ;
   m2 := KillString (m2)
END out3 ;


(*
   lookupSym - returns the symbol named, n, from the scope stack.
*)

PROCEDURE lookupSym (n: Name) : node ;
VAR
   s, m: node ;
   l, h: CARDINAL ;
BEGIN
   l := LowIndice (scopeStack) ;
   h := HighIndice (scopeStack) ;

   WHILE h>=l DO
      s := GetIndice (scopeStack, h) ;
      m := lookupInScope (s, n) ;
      IF debugScopes AND (m=NIL)
      THEN
         out3 (" [%d] search for symbol name %s in scope %s\n", h, n, s)
      END ;
      IF m#NIL
      THEN
         IF debugScopes
         THEN
            out3 (" [%d] search for symbol name %s in scope %s (found)\n", h, n, s)
         END ;
         RETURN m
      END ;
      DEC (h)
   END ;
   RETURN lookupBase (n)
END lookupSym ;


(*
   getSymName - returns the name of symbol, n.
*)

PROCEDURE getSymName (n: node) : Name ;
BEGIN
   WITH n^ DO
      CASE kind OF

      new             :  RETURN makeKey ('NEW') |
      dispose         :  RETURN makeKey ('DISPOSE') |
      length          :  RETURN makeKey ('LENGTH') |
      inc             :  RETURN makeKey ('INC') |
      dec             :  RETURN makeKey ('DEC') |
      incl            :  RETURN makeKey ('INCL') |
      excl            :  RETURN makeKey ('EXCL') |
      nil             :  RETURN makeKey ('NIL') |
      true            :  RETURN makeKey ('TRUE') |
      false           :  RETURN makeKey ('FALSE') |
      address         :  RETURN makeKey ('ADDRESS') |
      loc             :  RETURN makeKey ('LOC') |
      byte            :  RETURN makeKey ('BYTE') |
      word            :  RETURN makeKey ('WORD') |
      csizet          :  RETURN makeKey ('CSIZE_T') |
      cssizet         :  RETURN makeKey ('CSSIZE_T') |
      (* base types.  *)
      boolean         :  RETURN makeKey ('BOOLEAN') |
      proc            :  RETURN makeKey ('PROC') |
      char            :  RETURN makeKey ('CHAR') |
      cardinal        :  RETURN makeKey ('CARDINAL') |
      longcard        :  RETURN makeKey ('LONGCARD') |
      shortcard       :  RETURN makeKey ('SHORTCARD') |
      integer         :  RETURN makeKey ('INTEGER') |
      longint         :  RETURN makeKey ('LONGINT') |
      shortint        :  RETURN makeKey ('SHORTINT') |
      real            :  RETURN makeKey ('REAL') |
      longreal        :  RETURN makeKey ('LONGREAL') |
      shortreal       :  RETURN makeKey ('SHORTREAL') |
      bitset          :  RETURN makeKey ('BITSET') |
      ztype           :  RETURN makeKey ('_ZTYPE') |
      rtype           :  RETURN makeKey ('_RTYPE') |
      complex         :  RETURN makeKey ('COMPLEX') |
      longcomplex     :  RETURN makeKey ('LONGCOMPLEX') |
      shortcomplex    :  RETURN makeKey ('SHORTCOMPLEX') |

      (* language features and compound type attributes.  *)
      type            :  RETURN typeF.name |
      record          :  RETURN NulName |
      varient         :  RETURN NulName |
      var             :  RETURN varF.name |
      enumeration     :  RETURN NulName |
      subrange        :  RETURN NulName |
      pointer         :  RETURN NulName |
      array           :  RETURN NulName |
      string          :  RETURN stringF.name |
      const           :  RETURN constF.name |
      literal         :  RETURN literalF.name |
      varparam        :  RETURN NulName |
      param           :  RETURN NulName |
      optarg          :  RETURN NulName |
      recordfield     :  RETURN recordfieldF.name |
      varientfield    :  RETURN varientfieldF.name |
      enumerationfield:  RETURN enumerationfieldF.name |
      set             :  RETURN NulName |
      proctype        :  RETURN NulName |
      subscript       :  RETURN NulName |
      (* blocks.  *)
      procedure       :  RETURN procedureF.name |
      def             :  RETURN defF.name |
      imp             :  RETURN impF.name |
      module          :  RETURN moduleF.name |
      (* statements.  *)
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  RETURN NulName |
      (* expressions.  *)
      constexp,
      deref,
      arrayref,
      componentref,
      cast,
      val,
      plus,
      sub,
      div,
      mod,
      mult,
      divide,
      in,
      neg,
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN NulName |
      adr             :  RETURN makeKey ('ADR') |
      size            :  RETURN makeKey ('SIZE') |
      tsize           :  RETURN makeKey ('TSIZE') |
      chr             :  RETURN makeKey ('CHR') |
      abs             :  RETURN makeKey ('ABS') |
      ord             :  RETURN makeKey ('ORD') |
      float           :  RETURN makeKey ('FLOAT') |
      trunc           :  RETURN makeKey ('TRUNC') |
      high            :  RETURN makeKey ('HIGH') |
      throw           :  RETURN makeKey ('THROW') |
      unreachable     :  RETURN makeKey ('builtin_unreachable') |
      cmplx           :  RETURN makeKey ('CMPLX') |
      re              :  RETURN makeKey ('RE') |
      im              :  RETURN makeKey ('IM') |
      max             :  RETURN makeKey ('MAX') |
      min             :  RETURN makeKey ('MIN') |
      pointerref      :  RETURN NulName |
      funccall        :  RETURN NulName |
      identlist       :  RETURN NulName

      ELSE
         HALT
      END
   END
END getSymName ;


(*
   isUnary - returns TRUE if, n, is an unary node.
*)

PROCEDURE isUnary (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   CASE n^.kind OF

   length,
   re,
   im,
   deref,
   high,
   chr,
   cap,
   abs,
   ord,
   float,
   trunc,
   constexp,
   not,
   neg,
   adr,
   size,
   tsize,
   min,
   max     :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isUnary ;


(*
   isBinary - returns TRUE if, n, is an binary node.
*)

PROCEDURE isBinary (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   CASE n^.kind OF

   cmplx,
   and,
   or,
   equal,
   notequal,
   less,
   greater,
   greequal,
   lessequal,
   val,
   cast,
   plus,
   sub,
   div,
   mod,
   mult,
   divide,
   in      :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isBinary ;


(*
   makeUnary - create a unary expression node with, e, as the argument
               and res as the return type.
*)

PROCEDURE makeUnary (k: nodeT; e: node; res: node) : node ;
VAR
   n: node ;
BEGIN
   IF k=plus
   THEN
      RETURN e
   ELSE
      n := newNode (k) ;
      WITH n^ DO
         CASE kind OF

         min,
         max,
         throw,
         re,
         im,
         deref,
         high,
         chr,
	 cap,
         abs,
         ord,
         float,
         trunc,
         length,
         constexp,
         not,
         neg,
         adr,
         size,
         tsize:  WITH unaryF DO
                    arg := e ;
                    resultType := res
                 END

         END
      END
   END ;
   RETURN n
END makeUnary ;


(*
   isLeafString - returns TRUE if n is a leaf node which is a string constant.
*)

PROCEDURE isLeafString (n: node) : BOOLEAN ;
BEGIN
   RETURN isString (n) OR
          (isLiteral (n) AND (getType (n) = charN)) OR
	  (isConst (n) AND (getExprType (n) = charN))
END isLeafString ;


(*
   getLiteralStringContents - return the contents of a literal node as a string.
*)

PROCEDURE getLiteralStringContents (n: node) : String ;
VAR
   number,
   content,
   s      : String ;
BEGIN
   assert (n^.kind = literal) ;
   s := InitStringCharStar (keyToCharStar (n^.literalF.name)) ;
   content := NIL ;
   IF n^.literalF.type = charN
   THEN
      IF DynamicStrings.char (s, -1) = 'C'
      THEN
         IF DynamicStrings.Length (s) > 1
         THEN
            number := DynamicStrings.Slice (s, 0, -1) ;
            content := DynamicStrings.InitStringChar (VAL (CHAR, ostoc (number))) ;
            number := DynamicStrings.KillString (number)
         ELSE
            content := DynamicStrings.InitStringChar ('C')
         END
      ELSE
         content := DynamicStrings.Dup (s)
      END
   ELSE
      metaError1 ('cannot obtain string contents from {%1k}', n^.literalF.name)
   END ;
   s := DynamicStrings.KillString (s) ;
   RETURN content
END getLiteralStringContents ;


(*
   getStringContents - return the string contents of a constant, literal,
                       string or a constexp node.
*)

PROCEDURE getStringContents (n: node) : String ;
BEGIN
   IF isConst (n)
   THEN
      RETURN getStringContents (n^.constF.value)
   ELSIF isLiteral (n)
   THEN
      RETURN getLiteralStringContents (n)
   ELSIF isString (n)
   THEN
      RETURN getString (n)
   ELSIF isConstExp (n)
   THEN
      RETURN getStringContents (n^.unaryF.arg)
   END ;
   HALT
END getStringContents ;


(*
   addNames -
*)

PROCEDURE addNames (a, b: node) : Name ;
VAR
   sa, sb: String ;
   n     : Name ;
BEGIN
   sa := DynamicStrings.InitStringCharStar (keyToCharStar (getSymName (a))) ;
   sb := DynamicStrings.InitStringCharStar (keyToCharStar (getSymName (b))) ;
   sa := ConCat (sa, sb) ;
   n := makekey (DynamicStrings.string (sa)) ;
   sa := KillString (sa) ;
   sb := KillString (sb) ;
   RETURN n
END addNames ;


(*
   resolveString -
*)

PROCEDURE resolveString (n: node) : node ;
BEGIN
   WHILE isConst (n) OR isConstExp (n) DO
      IF isConst (n)
      THEN
         n := n^.constF.value
      ELSE
         n := n^.unaryF.arg
      END
   END ;
   IF n^.kind = plus
   THEN
      n := makeString (addNames (resolveString (n^.binaryF.left),
                                 resolveString (n^.binaryF.right)))
   END ;
   RETURN n
END resolveString ;


(*
   foldBinary -
*)

PROCEDURE foldBinary (k: nodeT; l, r: node; res: node) : node ;
VAR
   n : node ;
   ls,
   rs: String ;
BEGIN
   n := NIL ;
   IF (k = plus) AND isLeafString (l) AND isLeafString (r)
   THEN
      ls := getStringContents (l) ;
      rs := getStringContents (r) ;
      ls := DynamicStrings.Add (ls, rs) ;
      n := makeString (makekey (DynamicStrings.string (ls))) ;
      ls := DynamicStrings.KillString (ls) ;
      rs := DynamicStrings.KillString (rs)
   END ;
   RETURN n
END foldBinary ;


(*
   makeBinary - create a binary node with left/right/result type:  l, r and resultType.
*)

PROCEDURE makeBinary (k: nodeT; l, r: node; resultType: node) : node ;
VAR
   n: node ;
BEGIN
   n := foldBinary (k, l, r, resultType) ;
   IF n = NIL
   THEN
      n := doMakeBinary (k, l, r, resultType)
   END ;
   RETURN n
END makeBinary ;


(*
   doMakeBinary - returns a binary node containing left/right/result values
                  l, r, res, with a node operator, k.
*)

PROCEDURE doMakeBinary (k: nodeT; l, r: node; res: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (k) ;
   WITH n^ DO
      CASE kind OF

      cmplx,
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal,
      and,
      or,
      cast,
      val,
      plus,
      sub,
      div,
      mod,
      mult,
      divide,
      in  :  WITH binaryF DO
                left := l ;
                right := r ;
                resultType := res
             END

      END
   END ;
   RETURN n
END doMakeBinary ;


(*
   doMakeComponentRef -
*)

PROCEDURE doMakeComponentRef (rec, field: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (componentref) ;
   n^.componentrefF.rec := rec ;
   n^.componentrefF.field := field ;
   n^.componentrefF.resultType := getType (field) ;
   RETURN n
END doMakeComponentRef ;


(*
   makeComponentRef - build a componentref node which accesses, field,
                      within, record, rec.
*)

PROCEDURE makeComponentRef (rec, field: node) : node ;
VAR
   n, a: node ;
BEGIN
(*
   n := getLastOp (rec) ;
   IF (n#NIL) AND (isDeref (n) OR isPointerRef (n)) AND
      (skipType (getType (rec)) = skipType (getType (n)))
   THEN
      a := n^.unaryF.arg ;
      n^.kind := pointerref ;
      n^.pointerrefF.ptr := a ;
      n^.pointerrefF.field := field ;
      n^.pointerrefF.resultType := getType (field) ;
      RETURN n
   ELSE
      RETURN doMakeComponentRef (rec, field)
   END
*)
   IF isDeref (rec)
   THEN
      a := rec^.unaryF.arg ;
      rec^.kind := pointerref ;
      rec^.pointerrefF.ptr := a ;
      rec^.pointerrefF.field := field ;
      rec^.pointerrefF.resultType := getType (field) ;
      RETURN rec
   ELSE
      RETURN doMakeComponentRef (rec, field)
   END
END makeComponentRef ;


(*
   isComponentRef -
*)

PROCEDURE isComponentRef (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = componentref
END isComponentRef ;


(*
   makePointerRef - build a pointerref node which accesses, field,
                    within, pointer to record, ptr.
*)

PROCEDURE makePointerRef (ptr, field: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (pointerref) ;
   n^.pointerrefF.ptr := ptr ;
   n^.pointerrefF.field := field ;
   n^.pointerrefF.resultType := getType (field) ;
   RETURN n
END makePointerRef ;


(*
   isPointerRef - returns TRUE if, n, is a pointerref node.
*)

PROCEDURE isPointerRef (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = pointerref
END isPointerRef ;


(*
   makeArrayRef - build an arrayref node which access element,
                  index, in, array.  array is a variable/expression/constant
                  which has a type array.
*)

PROCEDURE makeArrayRef (array, index: node) : node ;
VAR
   n, t: node ;
   i, j: CARDINAL ;
BEGIN
   n := newNode (arrayref) ;
   n^.arrayrefF.array := array ;
   n^.arrayrefF.index := index ;
   t := array ;
   j := expListLen (index) ;
   i := 1 ;
   t := skipType (getType (t)) ;
   REPEAT
      IF isArray (t)
      THEN
         t := skipType (getType (t))
      ELSE
         metaError2 ('cannot access {%1N} dimension of array {%2a}', i, t)
      END ;
      INC (i)
   UNTIL i > j ;
   n^.arrayrefF.resultType := t ;
   RETURN n
END makeArrayRef ;


(*
   isArrayRef - returns TRUE if the node was an arrayref.
*)

PROCEDURE isArrayRef (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = arrayref
END isArrayRef ;


(*
   makeDeRef - dereferences the pointer defined by, n.
*)

PROCEDURE makeDeRef (n: node) : node ;
VAR
   t: node ;
BEGIN
   t := skipType (getType (n)) ;
   assert (isPointer (t)) ;
   RETURN makeUnary (deref, n, getType (t))
END makeDeRef ;


(*
   isDeref - returns TRUE if, n, is a deref node.
*)

PROCEDURE isDeref (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = deref
END isDeref ;


(*
   makeBase - create a base type or constant.
              It only supports the base types and constants
              enumerated below.
*)

PROCEDURE makeBase (k: nodeT) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (k) ;
   WITH n^ DO
      CASE k OF

      new,
      dispose,
      length,
      inc,
      dec,
      incl,
      excl,
      nil,
      true,
      false,
      address,
      loc,
      byte,
      word,
      csizet,
      cssizet,
      char,
      cardinal,
      longcard,
      shortcard,
      integer,
      longint,
      shortint,
      real,
      longreal,
      shortreal,
      bitset,
      boolean,
      proc,
      ztype,
      rtype,
      complex,
      longcomplex,
      shortcomplex,
      adr,
      chr,
      cap,
      abs,
      float,
      trunc,
      ord,
      high,
      throw,
      re,
      im,
      cmplx,
      size,
      tsize,
      val,
      min,
      max     :  (* legal kind.  *) |

      ELSE
         HALT
      END
   END ;
   RETURN n
END makeBase ;


(*
   makeBinaryTok - creates and returns a boolean type node with,
                   l, and, r, nodes.
*)

PROCEDURE makeBinaryTok (op: toktype; l, r: node) : node ;
BEGIN
   IF op=equaltok
   THEN
      RETURN makeBinary (equal, l, r, booleanN)
   ELSIF (op=hashtok) OR (op=lessgreatertok)
   THEN
      RETURN makeBinary (notequal, l, r, booleanN)
   ELSIF op=lesstok
   THEN
      RETURN makeBinary (less, l, r, booleanN)
   ELSIF op=greatertok
   THEN
      RETURN makeBinary (greater, l, r, booleanN)
   ELSIF op=greaterequaltok
   THEN
      RETURN makeBinary (greequal, l, r, booleanN)
   ELSIF op=lessequaltok
   THEN
      RETURN makeBinary (lessequal, l, r, booleanN)
   ELSIF op=andtok
   THEN
      RETURN makeBinary (and, l, r, booleanN)
   ELSIF op=ortok
   THEN
      RETURN makeBinary (or, l, r, booleanN)
   ELSIF op=plustok
   THEN
      RETURN makeBinary (plus, l, r, NIL)
   ELSIF op=minustok
   THEN
      RETURN makeBinary (sub, l, r, NIL)
   ELSIF op=divtok
   THEN
      RETURN makeBinary (div, l, r, NIL)
   ELSIF op=timestok
   THEN
      RETURN makeBinary (mult, l, r, NIL)
   ELSIF op=modtok
   THEN
      RETURN makeBinary (mod, l, r, NIL)
   ELSIF op=intok
   THEN
      RETURN makeBinary (in, l, r, NIL)
   ELSIF op=dividetok
   THEN
      RETURN makeBinary (divide, l, r, NIL)
   ELSE
      HALT  (* most likely op needs a clause as above.  *)
   END
END makeBinaryTok ;


(*
   makeUnaryTok - creates and returns a boolean type node with,
                  e, node.
*)

PROCEDURE makeUnaryTok (op: toktype; e: node) : node ;
BEGIN
   IF op=nottok
   THEN
      RETURN makeUnary (not, e, booleanN)
   ELSIF op=plustok
   THEN
      RETURN makeUnary (plus, e, NIL)
   ELSIF op=minustok
   THEN
      RETURN makeUnary (neg, e, NIL)
   ELSE
      HALT  (* most likely op needs a clause as above.  *)
   END
END makeUnaryTok ;


(*
   isOrdinal - returns TRUE if, n, is an ordinal type.
*)

PROCEDURE isOrdinal (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   address,
   loc,
   byte,
   word,
   csizet,
   cssizet,
   char,
   integer,
   longint,
   shortint,
   cardinal,
   longcard,
   shortcard,
   bitset   :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isOrdinal ;


(*
   getType - returns the type associated with node, n.
*)

PROCEDURE getType (n: node) : node ;
BEGIN
   WITH n^ DO
      CASE kind OF

      new,
      dispose         :  RETURN NIL |
      length          :  RETURN cardinalN |
      inc,
      dec,
      incl,
      excl            :  RETURN NIL |
      nil             :  RETURN addressN |
      true,
      false           :  RETURN booleanN |
      address         :  RETURN n |
      loc             :  RETURN n |
      byte            :  RETURN n |
      word            :  RETURN n |
      csizet          :  RETURN n |
      cssizet         :  RETURN n |
      (* base types.  *)
      boolean         :  RETURN n |
      proc            :  RETURN n |
      char            :  RETURN n |
      cardinal        :  RETURN n |
      longcard        :  RETURN n |
      shortcard       :  RETURN n |
      integer         :  RETURN n |
      longint         :  RETURN n |
      shortint        :  RETURN n |
      real            :  RETURN n |
      longreal        :  RETURN n |
      shortreal       :  RETURN n |
      bitset          :  RETURN n |
      ztype           :  RETURN n |
      rtype           :  RETURN n |
      complex         :  RETURN n |
      longcomplex     :  RETURN n |
      shortcomplex    :  RETURN n |

      (* language features and compound type attributes.  *)
      type            :  RETURN typeF.type |
      record          :  RETURN n |
      varient         :  RETURN n |
      var             :  RETURN varF.type |
      enumeration     :  RETURN n |
      subrange        :  RETURN subrangeF.type |
      array           :  RETURN arrayF.type |
      string          :  RETURN charN |
      const           :  RETURN constF.type |
      literal         :  RETURN literalF.type |
      varparam        :  RETURN varparamF.type |
      param           :  RETURN paramF.type |
      optarg          :  RETURN optargF.type |
      pointer         :  RETURN pointerF.type |
      recordfield     :  RETURN recordfieldF.type |
      varientfield    :  RETURN n |
      enumerationfield:  RETURN enumerationfieldF.type |
      set             :  RETURN setF.type |
      proctype        :  RETURN proctypeF.returnType |
      subscript       :  RETURN subscriptF.type |
      (* blocks.  *)
      procedure       :  RETURN procedureF.returnType |
      throw           :  RETURN NIL |
      unreachable     :  RETURN NIL |
      def,
      imp,
      module,
      (* statements.  *)
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  HALT |
      (* expressions.  *)
      cmplx,
      cast,
      val,
      plus,
      sub,
      div,
      mod,
      mult,
      divide          :  RETURN binaryF.resultType |
      in              :  RETURN booleanN |
      max,
      min,
      re,
      im,
      abs,
      constexp,
      deref,
      neg,
      adr,
      size,
      tsize           :  RETURN unaryF.resultType |
      and,
      or,
      not,
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN booleanN |
      trunc           :  RETURN integerN |
      float           :  RETURN realN |
      high            :  RETURN cardinalN |
      ord             :  RETURN cardinalN |
      chr             :  RETURN charN |
      cap             :  RETURN charN |
      arrayref        :  RETURN arrayrefF.resultType |
      componentref    :  RETURN componentrefF.resultType |
      pointerref      :  RETURN pointerrefF.resultType |
      funccall        :  RETURN funccallF.type |
      setvalue        :  RETURN setvalueF.type

      END
   END ;
   HALT
END getType ;


(*
   mixTypes -
*)

PROCEDURE mixTypes (a, b: node) : node ;
BEGIN
   IF (a = addressN) OR (b = addressN)
   THEN
      RETURN addressN
   END ;
   RETURN a
END mixTypes ;


(*
   doSetExprType -
*)

PROCEDURE doSetExprType (VAR t: node; n: node) : node ;
BEGIN
   IF t = NIL
   THEN
      t := n
   END ;
   RETURN t
END doSetExprType ;


(*
   getMaxMinType -
*)

PROCEDURE getMaxMinType (n: node) : node ;
BEGIN
   IF isVar (n) OR isConst (n)
   THEN
      RETURN getType (n)
   ELSIF isConstExp (n)
   THEN
      n := getExprType (n^.unaryF.arg) ;
      IF n = bitsetN
      THEN
         RETURN ztypeN
      ELSE
         RETURN n
      END
   ELSE
      RETURN n
   END
END getMaxMinType ;


(*
   doGetFuncType -
*)

PROCEDURE doGetFuncType (n: node) : node ;
BEGIN
   assert (isFuncCall (n)) ;
   RETURN doSetExprType (n^.funccallF.type, getType (n^.funccallF.function))
END doGetFuncType ;


(*
   doGetExprType - works out the type which is associated with node, n.
*)

PROCEDURE doGetExprType (n: node) : node ;
BEGIN
   WITH n^ DO
      CASE kind OF

      max,
      min             :  RETURN getMaxMinType (n^.unaryF.arg) |
      cast,
      val             :  RETURN doSetExprType (n^.binaryF.resultType, n^.binaryF.left) |
      halt,
      new,
      dispose         :  RETURN NIL |
      inc,
      dec,
      incl,
      excl            :  RETURN NIL |
      nil             :  RETURN addressN |
      true,
      false           :  RETURN booleanN |
      address         :  RETURN n |
      loc             :  RETURN n |
      byte            :  RETURN n |
      word            :  RETURN n |
      csizet          :  RETURN n |
      cssizet         :  RETURN n |
      (* base types.  *)
      boolean         :  RETURN n |
      proc            :  RETURN n |
      char            :  RETURN n |
      cardinal        :  RETURN n |
      longcard        :  RETURN n |
      shortcard       :  RETURN n |
      integer         :  RETURN n |
      longint         :  RETURN n |
      shortint        :  RETURN n |
      real            :  RETURN n |
      longreal        :  RETURN n |
      shortreal       :  RETURN n |
      bitset          :  RETURN n |
      ztype           :  RETURN n |
      rtype           :  RETURN n |
      complex         :  RETURN n |
      longcomplex     :  RETURN n |
      shortcomplex    :  RETURN n |

      (* language features and compound type attributes.  *)
      type            :  RETURN typeF.type |
      record          :  RETURN n |
      varient         :  RETURN n |
      var             :  RETURN varF.type |
      enumeration     :  RETURN n |
      subrange        :  RETURN subrangeF.type |
      array           :  RETURN arrayF.type |
      string          :  RETURN charN |
      const           :  RETURN doSetExprType (constF.type, getExprType (constF.value)) |
      literal         :  RETURN literalF.type |
      varparam        :  RETURN varparamF.type |
      param           :  RETURN paramF.type |
      optarg          :  RETURN optargF.type |
      pointer         :  RETURN pointerF.type |
      recordfield     :  RETURN recordfieldF.type |
      varientfield    :  RETURN n |
      enumerationfield:  RETURN enumerationfieldF.type |
      set             :  RETURN setF.type |
      proctype        :  RETURN proctypeF.returnType |
      subscript       :  RETURN subscriptF.type |
      (* blocks.  *)
      procedure       :  RETURN procedureF.returnType |
      throw           :  RETURN NIL |
      unreachable     :  RETURN NIL |
      def,
      imp,
      module,
      (* statements.  *)
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  HALT |
      (* expressions.  *)
      plus,
      sub,
      div,
      mod,
      mult,
      divide          :  RETURN doSetExprType (binaryF.resultType, mixTypes (getExprType (binaryF.left), getExprType (binaryF.right))) |
      in,
      and,
      or,
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN doSetExprType (binaryF.resultType, booleanN) |
      cmplx           :  RETURN doSetExprType (binaryF.resultType, complexN) |
      abs,
      constexp,
      deref,
      neg             :  RETURN doSetExprType (unaryF.resultType, getExprType (unaryF.arg)) |
      adr             :  RETURN doSetExprType (unaryF.resultType, addressN) |
      size,
      tsize           :  RETURN doSetExprType (unaryF.resultType, cardinalN) |
      high,
      ord             :  RETURN doSetExprType (unaryF.resultType, cardinalN) |
      float           :  RETURN doSetExprType (unaryF.resultType, realN) |
      trunc           :  RETURN doSetExprType (unaryF.resultType, integerN) |
      chr             :  RETURN doSetExprType (unaryF.resultType, charN) |
      cap             :  RETURN doSetExprType (unaryF.resultType, charN) |
      not             :  RETURN doSetExprType (unaryF.resultType, booleanN) |
      re              :  RETURN doSetExprType (unaryF.resultType, realN) |
      im              :  RETURN doSetExprType (unaryF.resultType, realN) |
      arrayref        :  RETURN arrayrefF.resultType |
      componentref    :  RETURN componentrefF.resultType |
      pointerref      :  RETURN pointerrefF.resultType |
      funccall        :  RETURN doSetExprType (funccallF.type, doGetFuncType (n)) |
      setvalue        :  RETURN setvalueF.type

      END
   END ;
   HALT
END doGetExprType ;


(*
   getExprType - return the expression type.
*)

PROCEDURE getExprType (n: node) : node ;
VAR
   t: node ;
BEGIN
   IF isFuncCall (n) AND (getType (n) # NIL) AND isProcType (skipType (getType (n)))
   THEN
      RETURN getType (skipType (getType (n)))
   END ;
   t := getType (n) ;
   IF t = NIL
   THEN
      t := doGetExprType (n)
   END ;
   RETURN t
END getExprType ;


(*
   skipType - skips over type aliases.
*)

PROCEDURE skipType (n: node) : node ;
BEGIN
   WHILE (n#NIL) AND isType (n) DO
      IF getType (n) = NIL
      THEN
         (* this will occur if, n, is an opaque type.  *)
         RETURN n
      END ;
      n := getType (n)
   END ;
   RETURN n
END skipType ;


(*
   getScope - returns the scope associated with node, n.
*)

PROCEDURE getScope (n: node) : node ;
BEGIN
   WITH n^ DO
      CASE kind OF

      stmtseq,
      exit,
      return,
      comment,
      identlist,
      setvalue,
      halt,
      new,
      dispose,
      length,
      inc,
      dec,
      incl,
      excl,
      nil,
      true,
      false           :  RETURN NIL |
      address,
      loc,
      byte,
      word,
      csizet,
      cssizet         : RETURN systemN |
      (* base types.  *)
      boolean,
      proc,
      char,
      cardinal,
      longcard,
      shortcard,
      integer,
      longint,
      shortint,
      real,
      longreal,
      shortreal,
      bitset,
      ztype,
      rtype,
      complex,
      longcomplex,
      shortcomplex    :  RETURN NIL |
      (* language features and compound type attributes.  *)
      type            :  RETURN typeF.scope |
      record          :  RETURN recordF.scope |
      varient         :  RETURN varientF.scope |
      var             :  RETURN varF.scope |
      enumeration     :  RETURN enumerationF.scope |
      subrange        :  RETURN subrangeF.scope |
      array           :  RETURN arrayF.scope |
      string          :  RETURN NIL |
      const           :  RETURN constF.scope |
      literal         :  RETURN NIL |
      varparam        :  RETURN varparamF.scope |
      param           :  RETURN paramF.scope |
      optarg          :  RETURN optargF.scope |
      pointer         :  RETURN pointerF.scope |
      recordfield     :  RETURN recordfieldF.scope |
      varientfield    :  RETURN varientfieldF.scope |
      enumerationfield:  RETURN enumerationfieldF.scope |
      set             :  RETURN setF.scope |
      proctype        :  RETURN proctypeF.scope |
      subscript       :  RETURN NIL |
      (* blocks.  *)
      procedure       :  RETURN procedureF.scope |
      def,
      imp,
      module,
      (* statements.  *)
      case,
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  RETURN NIL |
      (* expressions.  *)
      componentref,
      pointerref,
      arrayref,
      chr,
      cap,
      ord,
      float,
      trunc,
      high,
      cast,
      val,
      plus,
      sub,
      div,
      mod,
      mult,
      divide,
      in              :  RETURN NIL |
      neg             :  RETURN NIL |
      lsl,
      lsr,
      lor,
      land,
      lnot,
      lxor,
      and,
      or,
      not,
      constexp,
      deref,
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN NIL |
      adr,
      size,
      tsize,
      throw           :  RETURN systemN |
      unreachable,
      cmplx, re, im,
      min,
      max             :  RETURN NIL |
      vardecl         :  RETURN vardeclF.scope |
      funccall        :  RETURN NIL |
      explist         :  RETURN NIL |
      caselabellist   :  RETURN NIL |
      caselist        :  RETURN NIL |
      range           :  RETURN NIL |
      varargs         :  RETURN varargsF.scope

      END
   END
END getScope ;


(*
   foreachDefModuleDo - foreach definition node, n, in the module universe,
                        call p (n).
*)

PROCEDURE foreachDefModuleDo (p: performOperation) ;
BEGIN
   ForeachIndiceInIndexDo (defUniverseI, p)
END foreachDefModuleDo ;


(*
   foreachModModuleDo - foreach implementation or module node, n, in the module universe,
                        call p (n).
*)

PROCEDURE foreachModModuleDo (p: performOperation) ;
BEGIN
   ForeachIndiceInIndexDo (modUniverseI, p)
END foreachModModuleDo ;


(*
   openOutput -
*)

PROCEDURE openOutput ;
VAR
   s: String ;
BEGIN
   s := getOutputFile () ;
   IF EqualArray (s, '-')
   THEN
      outputFile := StdOut
   ELSE
      outputFile := OpenToWrite (s)
   END ;
   mcStream.setDest (outputFile)
END openOutput ;


(*
   closeOutput -
*)

PROCEDURE closeOutput ;
VAR
   s: String ;
BEGIN
   s := getOutputFile () ;
   outputFile := mcStream.combine () ;
   IF NOT EqualArray (s, '-')
   THEN
      Close (outputFile)
   END
END closeOutput ;


(*
   write - outputs a single char, ch.
*)

PROCEDURE write (ch: CHAR) ;
BEGIN
   WriteChar (outputFile, ch) ;
   FlushBuffer (outputFile)
END write ;


(*
   writeln -
*)

PROCEDURE writeln ;
BEGIN
   WriteLine (outputFile) ;
   FlushBuffer (outputFile)
END writeln ;


(*
   doIncludeC - include header file for definition module, n.
*)

PROCEDURE doIncludeC (n: node) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   IF getExtendedOpaque ()
   THEN
      (* no include in this case.  *)
   ELSIF isDef (n)
   THEN
      print (doP, '#   include "') ;
      prints (doP, getHPrefix ()) ;
      prints (doP, s) ;
      print (doP, '.h"\n') ;
      foreachNodeDo (n^.defF.decls.symbols, addDoneDef)
   END ;
   s := KillString (s)
END doIncludeC ;


(*
   getSymScope - returns the scope where node, n, was declared.
*)

PROCEDURE getSymScope (n: node) : node ;
BEGIN
   WITH n^ DO
      CASE kind OF

      const    :  RETURN constF.scope |
      type     :  RETURN typeF.scope |
      var      :  RETURN varF.scope |
      procedure:  RETURN procedureF.scope

      END
   END ;
   HALT
END getSymScope ;


(*
   isQualifiedForced - should the node be written with a module prefix?
*)

PROCEDURE isQualifiedForced (n: node) : BOOLEAN ;
BEGIN
   RETURN (forceQualified AND
           (isType (n) OR isRecord (n) OR isArray (n) OR isEnumeration (n) OR isEnumerationField (n)))
END isQualifiedForced ;


(*
   getFQstring -
*)

PROCEDURE getFQstring (n: node) : String ;
VAR
   i, s: String ;
BEGIN
   IF getScope (n) = NIL
   THEN
      RETURN InitStringCharStar (keyToCharStar (getSymName (n)))
   ELSIF isQualifiedForced (n)
   THEN
      i := InitStringCharStar (keyToCharStar (getSymName (n))) ;
      s := InitStringCharStar (keyToCharStar (getSymName (getScope (n)))) ;
      RETURN Sprintf2 (InitString ("%s_%s"), s, i)
   ELSIF (NOT isExported (n)) OR getIgnoreFQ ()
   THEN
      RETURN InitStringCharStar (keyToCharStar (getSymName (n)))
   ELSE
      i := InitStringCharStar (keyToCharStar (getSymName (n))) ;
      s := InitStringCharStar (keyToCharStar (getSymName (getScope (n)))) ;
      RETURN Sprintf2 (InitString ("%s_%s"), s, i)
   END
END getFQstring ;


(*
   getFQDstring -
*)

PROCEDURE getFQDstring (n: node; scopes: BOOLEAN) : String ;
VAR
   i, s: String ;
BEGIN
   IF getScope (n) = NIL
   THEN
      RETURN InitStringCharStar (keyToCharStar (getDName (n, scopes)))
   ELSIF isQualifiedForced (n)
   THEN
      (* we assume a qualified name will never conflict.  *)
      i := InitStringCharStar (keyToCharStar (getSymName (n))) ;
      s := InitStringCharStar (keyToCharStar (getSymName (getScope (n)))) ;
      RETURN Sprintf2 (InitString ("%s_%s"), s, i)
   ELSIF (NOT isExported (n)) OR getIgnoreFQ ()
   THEN
      RETURN InitStringCharStar (keyToCharStar (getDName (n, scopes)))
   ELSE
      (* we assume a qualified name will never conflict.  *)
      i := InitStringCharStar (keyToCharStar (getSymName (n))) ;
      s := InitStringCharStar (keyToCharStar (getSymName (getScope (n)))) ;
      RETURN Sprintf2 (InitString ("%s_%s"), s, i)
   END
END getFQDstring ;


(*
   getString - returns the name as a string.
*)

PROCEDURE getString (n: node) : String ;
BEGIN
   IF getSymName (n) = NulName
   THEN
      RETURN InitString ('')
   ELSE
      RETURN InitStringCharStar (keyToCharStar (getSymName (n)))
   END
END getString ;


(*
   getCardinal - returns the cardinal type node.
*)

PROCEDURE getCardinal () : node ;
BEGIN
   RETURN cardinalN
END getCardinal ;


(*
   doNone - call HALT.
*)

PROCEDURE doNone (n: node) ;
BEGIN
   HALT
END doNone ;


(*
   doNothing - does nothing!
*)

PROCEDURE doNothing (n: node) ;
BEGIN
END doNothing ;


(*
   doConstC -
*)

PROCEDURE doConstC (n: node) ;
BEGIN
   IF NOT alists.isItemInList (doneQ, n)
   THEN
      print (doP, "#   define ") ;
      doFQNameC (doP, n) ;
      setNeedSpace (doP) ;
      doExprC (doP, n^.constF.value) ;
      print (doP, '\n') ;
      alists.includeItemIntoList (doneQ, n)
   END
END doConstC ;


(*
   needsParen - returns TRUE if expression, n, needs to be enclosed in ().
*)

PROCEDURE needsParen (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   WITH n^ DO
      CASE kind OF

      nil,
      true,
      false           :  RETURN FALSE |
      constexp        :  RETURN needsParen (unaryF.arg) |
      neg             :  RETURN needsParen (unaryF.arg) |
      not             :  RETURN needsParen (unaryF.arg) |
      adr,
      size,
      tsize,
      ord,
      float,
      trunc,
      chr,
      cap,
      high            :  RETURN FALSE |
      deref           :  RETURN FALSE |
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN TRUE |
      componentref    :  RETURN FALSE |
      pointerref      :  RETURN FALSE |
      cast            :  RETURN TRUE |
      val             :  RETURN TRUE |
      abs             :  RETURN FALSE |
      plus,
      sub,
      div,
      mod,
      mult,
      divide,
      in              :  RETURN TRUE |
      literal,
      const,
      enumerationfield,
      string          :  RETURN FALSE |
      max             :  RETURN TRUE |
      min             :  RETURN TRUE |
      var             :  RETURN FALSE |
      arrayref        :  RETURN FALSE |
      and,
      or              :  RETURN TRUE |
      funccall        :  RETURN TRUE |
      recordfield     :  RETURN FALSE |
      loc,
      byte,
      word,
      type,
      char,
      cardinal,
      longcard,
      shortcard,
      integer,
      longint,
      shortint,
      real,
      longreal,
      shortreal,
      complex,
      longcomplex,
      shortcomplex,
      bitset,
      boolean,
      proc            :  RETURN FALSE |
      setvalue        :  RETURN FALSE |
      address         :  RETURN TRUE |
      procedure       :  RETURN FALSE |
      length,
      cmplx, re, im   :  RETURN TRUE

      END
   END ;
   RETURN TRUE
END needsParen ;


(*
   doUnary -
*)

PROCEDURE doUnary (p: pretty; op: ARRAY OF CHAR; expr, type: node; l, r: BOOLEAN) ;
BEGIN
   IF l
   THEN
      setNeedSpace (p)
   END ;
   print (p, op) ;
   IF r
   THEN
      setNeedSpace (p)
   END ;
   IF needsParen (expr)
   THEN
      outText (p, '(') ;
      doExprC (p, expr) ;
      outText (p, ')')
   ELSE
      doExprC (p, expr)
   END
END doUnary ;


(*
   doSetSub - perform  l & (~ r)
*)

PROCEDURE doSetSub (p: pretty; left, right: node) ;
BEGIN
   IF needsParen (left)
   THEN
      outText (p, '(') ;
      doExprC (p, left) ;
      outText (p, ')')
   ELSE
      doExprC (p, left)
   END ;
   setNeedSpace (p) ;
   outText (p, '&') ;
   setNeedSpace (p) ;
   IF needsParen (right)
   THEN
      outText (p, '(~(') ;
      doExprC (p, right) ;
      outText (p, '))')
   ELSE
      outText (p, '(~') ;
      doExprC (p, right) ;
      outText (p, ')')
   END
END doSetSub ;


(*
   doPolyBinary -
*)

PROCEDURE doPolyBinary (p: pretty; op: nodeT; left, right: node; l, r: BOOLEAN) ;
VAR
   lt, rt: node ;
BEGIN
   lt := skipType (getExprType (left)) ;
   rt := skipType (getExprType (right)) ;
   IF ((lt # NIL) AND (isSet (lt) OR isBitset (lt))) OR
      ((rt # NIL) AND (isSet (rt) OR isBitset (rt)))
   THEN
      CASE op OF

      plus   :  doBinary (p, '|', left, right, l, r, FALSE) |
      sub    :  doSetSub (p, left, right) |
      mult   :  doBinary (p, '&', left, right, l, r, FALSE) |
      divide :  doBinary (p, '^', left, right, l, r, FALSE)

      END
   ELSE
      CASE op OF

      plus   :  doBinary (p, '+', left, right, l, r, FALSE) |
      sub    :  doBinary (p, '-', left, right, l, r, FALSE) |
      mult   :  doBinary (p, '*', left, right, l, r, FALSE) |
      divide :  doBinary (p, '/', left, right, l, r, FALSE)

      END
   END
END doPolyBinary ;


(*
   doBinary -
*)

PROCEDURE doBinary (p: pretty; op: ARRAY OF CHAR; left, right: node; l, r, unpackProc: BOOLEAN) ;
BEGIN
   IF needsParen (left)
   THEN
      outText (p, '(') ;
      doExprCup (p, left, unpackProc) ;
      outText (p, ')')
   ELSE
      doExprCup (p, left, unpackProc)
   END ;
   IF l
   THEN
      setNeedSpace (p)
   END ;
   outText (p, op) ;
   IF r
   THEN
      setNeedSpace (p)
   END ;
   IF needsParen (right)
   THEN
      outText (p, '(') ;
      doExprCup (p, right, unpackProc) ;
      outText (p, ')')
   ELSE
      doExprCup (p, right, unpackProc)
   END
END doBinary ;


(*
   doPostUnary -
*)

PROCEDURE doPostUnary (p: pretty; op: ARRAY OF CHAR; expr: node) ;
BEGIN
   doExprC (p, expr) ;
   outText (p, op)
END doPostUnary ;


(*
   doDeRefC -
*)

PROCEDURE doDeRefC (p: pretty; expr: node) ;
BEGIN
   outText (p, '(*') ;
   doExprC (p, expr) ;
   outText (p, ')')
END doDeRefC ;


(*
   doGetLastOp - returns, a, if b is a terminal otherwise walk right.
*)

PROCEDURE doGetLastOp (a, b: node) : node ;
BEGIN
   WITH b^ DO
      CASE kind OF

      nil             :  RETURN a |
      true            :  RETURN a |
      false           :  RETURN a |
      constexp        :  RETURN doGetLastOp (b, unaryF.arg) |
      neg             :  RETURN doGetLastOp (b, unaryF.arg) |
      not             :  RETURN doGetLastOp (b, unaryF.arg) |
      adr             :  RETURN doGetLastOp (b, unaryF.arg) |
      size            :  RETURN doGetLastOp (b, unaryF.arg) |
      tsize           :  RETURN doGetLastOp (b, unaryF.arg) |
      ord             :  RETURN doGetLastOp (b, unaryF.arg) |
      float,
      trunc           :  RETURN doGetLastOp (b, unaryF.arg) |
      chr             :  RETURN doGetLastOp (b, unaryF.arg) |
      cap             :  RETURN doGetLastOp (b, unaryF.arg) |
      high            :  RETURN doGetLastOp (b, unaryF.arg) |
      deref           :  RETURN doGetLastOp (b, unaryF.arg) |
      re,
      im              :  RETURN doGetLastOp (b, unaryF.arg) |
      equal           :  RETURN doGetLastOp (b, binaryF.right) |
      notequal        :  RETURN doGetLastOp (b, binaryF.right) |
      less            :  RETURN doGetLastOp (b, binaryF.right) |
      greater         :  RETURN doGetLastOp (b, binaryF.right) |
      greequal        :  RETURN doGetLastOp (b, binaryF.right) |
      lessequal       :  RETURN doGetLastOp (b, binaryF.right) |
      componentref    :  RETURN doGetLastOp (b, componentrefF.field) |
      pointerref      :  RETURN doGetLastOp (b, pointerrefF.field) |
      cast            :  RETURN doGetLastOp (b, binaryF.right) |
      val             :  RETURN doGetLastOp (b, binaryF.right) |
      plus            :  RETURN doGetLastOp (b, binaryF.right) |
      sub             :  RETURN doGetLastOp (b, binaryF.right) |
      div             :  RETURN doGetLastOp (b, binaryF.right) |
      mod             :  RETURN doGetLastOp (b, binaryF.right) |
      mult            :  RETURN doGetLastOp (b, binaryF.right) |
      divide          :  RETURN doGetLastOp (b, binaryF.right) |
      in              :  RETURN doGetLastOp (b, binaryF.right) |
      and             :  RETURN doGetLastOp (b, binaryF.right) |
      or              :  RETURN doGetLastOp (b, binaryF.right) |
      cmplx           :  RETURN doGetLastOp (b, binaryF.right) |
      literal         :  RETURN a |
      const           :  RETURN a |
      enumerationfield:  RETURN a |
      string          :  RETURN a |
      max             :  RETURN doGetLastOp (b, unaryF.arg) |
      min             :  RETURN doGetLastOp (b, unaryF.arg) |
      var             :  RETURN a |
      arrayref        :  RETURN a |
      funccall        :  RETURN a |
      procedure       :  RETURN a |
      recordfield     :  RETURN a

      END
   END
END doGetLastOp ;


(*
   getLastOp - return the right most non leaf node.
*)

PROCEDURE getLastOp (n: node) : node ;
BEGIN
   RETURN doGetLastOp (n, n)
END getLastOp ;


(*
   doComponentRefC -
*)

PROCEDURE doComponentRefC (p: pretty; l, r: node) ;
BEGIN
   doExprC (p, l) ;
   outText (p, '.') ;
   doExprC (p, r)
END doComponentRefC ;


(*
   doPointerRefC -
*)

PROCEDURE doPointerRefC (p: pretty; l, r: node) ;
BEGIN
   doExprC (p, l) ;
   outText (p, '->') ;
   doExprC (p, r)
END doPointerRefC ;


(*
   doPreBinary -
*)

PROCEDURE doPreBinary (p: pretty; op: ARRAY OF CHAR; left, right: node; l, r: BOOLEAN) ;
BEGIN
   IF l
   THEN
      setNeedSpace (p)
   END ;
   outText (p, op) ;
   IF r
   THEN
      setNeedSpace (p)
   END ;
   outText (p, '(') ;
   doExprC (p, left) ;
   outText (p, ',') ;
   setNeedSpace (p) ;
   doExprC (p, right) ;
   outText (p, ')')
END doPreBinary ;


(*
   doConstExpr -
*)

PROCEDURE doConstExpr (p: pretty; n: node) ;
BEGIN
   doFQNameC (p, n)
END doConstExpr ;


(*
   doEnumerationField -
*)

PROCEDURE doEnumerationField (p: pretty; n: node) ;
BEGIN
   doFQDNameC (p, n, FALSE)
END doEnumerationField ;


(*
   isZero - returns TRUE if node, n, is zero.
*)

PROCEDURE isZero (n: node) : BOOLEAN ;
BEGIN
   IF isConstExp (n)
   THEN
      RETURN isZero (n^.unaryF.arg)
   END ;
   RETURN getSymName (n)=makeKey ('0')
END isZero ;


(*
   doArrayRef -
*)

PROCEDURE doArrayRef (p: pretty; n: node) ;
VAR
   t   : node ;
   i, c: CARDINAL ;
BEGIN
   assert (n # NIL) ;
   assert (isArrayRef (n)) ;
   t := skipType (getType (n^.arrayrefF.array)) ;
   IF isUnbounded (t)
   THEN
      outTextN (p, getSymName (n^.arrayrefF.array))
   ELSE
      doExprC (p, n^.arrayrefF.array) ;
      assert (isArray (t)) ;
      outText (p, '.array')
   END ;
   outText (p, '[') ;
   i := 1 ;
   c := expListLen (n^.arrayrefF.index) ;
   WHILE i<=c DO
      doExprC (p, getExpList (n^.arrayrefF.index, i)) ;
      IF isUnbounded (t)
      THEN
         assert (c = 1)
      ELSE
         doSubtractC (p, getMin (t^.arrayF.subr)) ;
	 IF i<c
         THEN
            assert (isArray (t)) ;
            outText (p, '].array[') ;
            t := skipType (getType (t))
         END
      END ;
      INC (i)
   END ;
   outText (p, ']')
END doArrayRef ;


(*
   doProcedure -
*)

PROCEDURE doProcedure (p: pretty; n: node) ;
BEGIN
   assert (isProcedure (n)) ;
   doFQDNameC (p, n, TRUE)
END doProcedure ;


(*
   doRecordfield -
*)

PROCEDURE doRecordfield (p: pretty; n: node) ;
BEGIN
   doDNameC (p, n, FALSE)
END doRecordfield ;


(*
   doCastC -
*)

PROCEDURE doCastC (p: pretty; t, e: node) ;
VAR
   et: node ;
BEGIN
   outText (p, '(') ;
   doTypeNameC (p, t) ;
   outText (p, ')') ;
   setNeedSpace (p) ;
   et := skipType (getType (e)) ;
   IF (et # NIL) AND isAProcType (et) AND isAProcType (skipType (t))
   THEN
      outText (p, '{(') ;
      doFQNameC (p, t) ;
      outText (p, '_t)') ;
      setNeedSpace (p) ;
      doExprC (p, e) ;
      outText (p, '.proc}')
   ELSE
      outText (p, '(') ;
      doExprC (p, e) ;
      outText (p, ')')
   END
END doCastC ;


(*
   doSetValueC -
*)

PROCEDURE doSetValueC (p: pretty; n: node) ;
VAR
   lo  : node ;
   i, h: CARDINAL ;
BEGIN
   assert (isSetValue (n)) ;
   lo := getSetLow (n) ;
   IF n^.setvalueF.type # NIL
   THEN
      outText (p, '(') ;
      doTypeNameC (p, n^.setvalueF.type) ;
      noSpace (p) ;
      outText (p, ')') ;
      setNeedSpace (p)
   END ;
   IF HighIndice (n^.setvalueF.values) = 0
   THEN
      outText (p, '0')
   ELSE
      i := LowIndice (n^.setvalueF.values) ;
      h := HighIndice (n^.setvalueF.values) ;
      outText (p, '(') ;
      WHILE i<=h DO
         outText (p, '(1') ;
         setNeedSpace (p) ;
         outText (p, '<<') ;
         setNeedSpace (p) ;
         outText (p, '(') ;
         doExprC (p, GetIndice (n^.setvalueF.values, i)) ;
         doSubtractC (p, lo) ;
         outText (p, ')') ;
         outText (p, ')') ;
         IF i<h
         THEN
            setNeedSpace (p) ;
            outText (p, '|') ;
            setNeedSpace (p)
         END ;
         INC (i)
      END ;
      outText (p, ')')
   END
END doSetValueC ;


(*
   getSetLow - returns the low value of the set type from
               expression, n.
*)

PROCEDURE getSetLow (n: node) : node ;
VAR
   type: node ;
BEGIN
   IF getType (n) = NIL
   THEN
      RETURN makeLiteralInt (makeKey ('0'))
   ELSE
      type := skipType (getType (n)) ;
      IF isSet (type)
      THEN
         RETURN getMin (skipType (getType (type)))
      ELSE
         RETURN makeLiteralInt (makeKey ('0'))
      END
   END
END getSetLow ;


(*
   doInC - performs (((1 << (l)) & (r)) != 0)
*)

PROCEDURE doInC (p: pretty; l, r: node) ;
VAR
   lo: node ;
BEGIN
   lo := getSetLow (r) ;
   outText (p, '(((1') ;
   setNeedSpace (p) ;
   outText (p, '<<') ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   doExprC (p, l) ;
   doSubtractC (p, lo) ;
   outText (p, '))') ;
   setNeedSpace (p) ;
   outText (p, '&') ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   doExprC (p, r) ;
   outText (p, '))') ;
   setNeedSpace (p) ;
   outText (p, '!=') ;
   setNeedSpace (p) ;
   outText (p, '0)')
END doInC ;


(*
   doThrowC -
*)

PROCEDURE doThrowC (p: pretty; n: node) ;
BEGIN
   assert (isIntrinsic (n)) ;
   outText (p, "throw") ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   IF expListLen (n^.intrinsicF.args) = 1
   THEN
      doExprC (p, getExpList (n^.intrinsicF.args, 1))
   END ;
   outText (p, ')')
END doThrowC ;


(*
   doUnreachableC -
*)

PROCEDURE doUnreachableC (p: pretty; n: node) ;
BEGIN
   assert (isIntrinsic (n)) ;
   outText (p, "__builtin_unreachable") ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   assert (expListLen (n^.intrinsicF.args) = 0) ;
   outText (p, ')')
END doUnreachableC ;


(*
   outNull -
*)

PROCEDURE outNull (p: pretty) ;
BEGIN
   keyc.useNull ;
   outText (p, 'NULL')
END outNull ;


(*
   outTrue -
*)

PROCEDURE outTrue (p: pretty) ;
BEGIN
   keyc.useTrue ;
   IF useBool () AND (lang = ansiCP)
   THEN
      outText (p, 'true')
   ELSE
      outText (p, 'TRUE')
   END
END outTrue ;


(*
   outFalse -
*)

PROCEDURE outFalse (p: pretty) ;
BEGIN
   keyc.useFalse ;
   IF useBool () AND (lang = ansiCP)
   THEN
      outText (p, 'false')
   ELSE
      outText (p, 'FALSE')
   END
END outFalse ;


(*
   doExprC -
*)

PROCEDURE doExprC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (n#NIL) ;
   t := getExprType (n) ;
   WITH n^ DO
      CASE kind OF

      nil             :  outNull (p) |
      true            :  outTrue (p) |
      false           :  outFalse (p) |
      constexp        :  doUnary (p, '', unaryF.arg, unaryF.resultType, FALSE, FALSE) |
      neg             :  doUnary (p, '-', unaryF.arg, unaryF.resultType, FALSE, FALSE) |
      not             :  doUnary (p, '!', unaryF.arg, unaryF.resultType, FALSE, TRUE) |
      val             :  doValC (p, n) |
      adr             :  doAdrC (p, n) |
      size,
      tsize           :  doSizeC (p, n) |
      float           :  doConvertSC (p, n, getCRealType ()) |
      trunc           :  doConvertC (p, n, "int") |
      ord             :  doConvertC (p, n, "unsigned int") |
      chr             :  doConvertC (p, n, "char") |
      cap             :  doCapC (p, n) |
      abs             :  doAbsC (p, n) |
      high            :  doFuncHighC (p, n^.unaryF.arg) |
      length          :  doLengthC (p, n) |
      min             :  doMinC (p, n) |
      max             :  doMaxC (p, n) |
      throw           :  doThrowC (p, n) |
      unreachable     :  doUnreachableC (p, n) |
      re              :  doReC (p, n) |
      im              :  doImC (p, n) |
      cmplx           :  doCmplx (p, n) |

      deref           :  doDeRefC (p, unaryF.arg) |
      equal           :  doBinary (p, '==', binaryF.left, binaryF.right, TRUE, TRUE, TRUE) |
      notequal        :  doBinary (p, '!=', binaryF.left, binaryF.right, TRUE, TRUE, TRUE) |
      less            :  doBinary (p, '<', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      greater         :  doBinary (p, '>', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      greequal        :  doBinary (p, '>=', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      lessequal       :  doBinary (p, '<=', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      componentref    :  doComponentRefC (p, componentrefF.rec, componentrefF.field) |
      pointerref      :  doPointerRefC (p, pointerrefF.ptr, pointerrefF.field) |
      cast            :  doCastC (p, binaryF.left, binaryF.right) |
      plus            :  doPolyBinary (p, plus, binaryF.left, binaryF.right, FALSE, FALSE) |
      sub             :  doPolyBinary (p, sub, binaryF.left, binaryF.right, FALSE, FALSE) |
      div             :  doBinary (p, '/', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      mod             :  doBinary (p, '%', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      mult            :  doPolyBinary (p, mult, binaryF.left, binaryF.right, FALSE, FALSE) |
      divide          :  doPolyBinary (p, divide, binaryF.left, binaryF.right, FALSE, FALSE) |
      in              :  doInC (p, binaryF.left, binaryF.right) |
      and             :  doBinary (p, '&&', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      or              :  doBinary (p, '||', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      literal         :  doLiteralC (p, n) |
      const           :  doConstExpr (p, n) |
      enumerationfield:  doEnumerationField (p, n) |
      string          :  doStringC (p, n) |
      var             :  doVar (p, n) |
      arrayref        :  doArrayRef (p, n) |
      funccall        :  doFuncExprC (p, n) |
      procedure       :  doProcedure (p, n) |
      recordfield     :  doRecordfield (p, n) |
      setvalue        :  doSetValueC (p, n) |
      char,
      cardinal,
      longcard,
      shortcard,
      integer,
      longint,
      shortint,
      complex,
      longcomplex,
      shortcomplex,
      real,
      longreal,
      shortreal,
      bitset,
      boolean,
      proc            :  doBaseC (p, n) |
      address,
      loc,
      byte,
      word,
      csizet,
      cssizet         :  doSystemC (p, n) |
      type            :  doTypeNameC (p, n) |
      pointer         :  doTypeNameC (p, n)

      END
   END
END doExprC ;


(*
   doExprCup -
*)

PROCEDURE doExprCup (p: pretty; n: node; unpackProc: BOOLEAN) ;
VAR
   t: node ;
BEGIN
   doExprC (p, n) ;
   IF unpackProc
   THEN
      t := skipType (getExprType (n)) ;
      IF (t # NIL) AND isAProcType (t)
      THEN
         outText (p, '.proc')
      END
   END
END doExprCup ;


(*
   doExprM2 -
*)

PROCEDURE doExprM2 (p: pretty; n: node) ;
BEGIN
   assert (n#NIL) ;
   WITH n^ DO
      CASE kind OF

      nil             :  outText (p, 'NIL') |
      true            :  outText (p, 'TRUE') |
      false           :  outText (p, 'FALSE') |
      constexp        :  doUnary (p, '', unaryF.arg, unaryF.resultType, FALSE, FALSE) |
      neg             :  doUnary (p, '-', unaryF.arg, unaryF.resultType, FALSE, FALSE) |
      not             :  doUnary (p, 'NOT', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      adr             :  doUnary (p, 'ADR', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      size            :  doUnary (p, 'SIZE', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      tsize           :  doUnary (p, 'TSIZE', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      float           :  doUnary (p, 'FLOAT', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      trunc           :  doUnary (p, 'TRUNC', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      ord             :  doUnary (p, 'ORD', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      chr             :  doUnary (p, 'CHR', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      cap             :  doUnary (p, 'CAP', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      high            :  doUnary (p, 'HIGH', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      re              :  doUnary (p, 'RE', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      im              :  doUnary (p, 'IM', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      deref           :  doPostUnary (p, '^', unaryF.arg) |
      equal           :  doBinary (p, '=', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      notequal        :  doBinary (p, '#', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      less            :  doBinary (p, '<', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      greater         :  doBinary (p, '>', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      greequal        :  doBinary (p, '>=', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      lessequal       :  doBinary (p, '<=', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      componentref    :  doBinary (p, '.', componentrefF.rec, componentrefF.field, FALSE, FALSE, FALSE) |
      pointerref      :  doBinary (p, '^.', pointerrefF.ptr, pointerrefF.field, FALSE, FALSE, FALSE) |
      cast            :  doPreBinary (p, 'CAST', binaryF.left, binaryF.right, TRUE, TRUE) |
      val             :  doPreBinary (p, 'VAL', binaryF.left, binaryF.right, TRUE, TRUE) |
      cmplx           :  doPreBinary (p, 'CMPLX', binaryF.left, binaryF.right, TRUE, TRUE) |
      plus            :  doBinary (p, '+', binaryF.left, binaryF.right, FALSE, FALSE, FALSE) |
      sub             :  doBinary (p, '-', binaryF.left, binaryF.right, FALSE, FALSE, FALSE) |
      div             :  doBinary (p, 'DIV', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      mod             :  doBinary (p, 'MOD', binaryF.left, binaryF.right, TRUE, TRUE, FALSE) |
      mult            :  doBinary (p, '*', binaryF.left, binaryF.right, FALSE, FALSE, FALSE) |
      divide          :  doBinary (p, '/', binaryF.left, binaryF.right, FALSE, FALSE, FALSE) |
      literal         :  doLiteral (p, n) |
      const           :  doConstExpr (p, n) |
      enumerationfield:  doEnumerationField (p, n) |
      string          :  doString (p, n) |
      max             :  doUnary (p, 'MAX', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      min             :  doUnary (p, 'MIN', unaryF.arg, unaryF.resultType, TRUE, TRUE) |
      var             :  doVar (p, n)

      END
   END
END doExprM2 ;


(*
   doVar -
*)

PROCEDURE doVar (p: pretty; n: node) ;
BEGIN
   assert (isVar (n)) ;
   IF n^.varF.isVarParameter
   THEN
      outText (p, '(*') ;
      doFQDNameC (p, n, TRUE) ;
      outText (p, ')')
   ELSE
      doFQDNameC (p, n, TRUE)
   END
END doVar ;


(*
   doLiteralC -
*)

PROCEDURE doLiteralC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   assert (isLiteral (n)) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   IF n^.literalF.type=charN
   THEN
      IF DynamicStrings.char (s, -1)='C'
      THEN
         s := DynamicStrings.Slice (DynamicStrings.Mark (s), 0, -1) ;
	 IF DynamicStrings.char (s, 0)#'0'
         THEN
            s := DynamicStrings.ConCat (InitString('0'), DynamicStrings.Mark (s))
         END
      END ;
      outText (p, "(char)") ;
      setNeedSpace (p)
   ELSIF DynamicStrings.char (s, -1) = 'H'
   THEN
      outText (p, "0x") ;
      s := DynamicStrings.Slice (DynamicStrings.Mark (s), 0, -1)
   ELSIF DynamicStrings.char (s, -1) = 'B'
   THEN
      outText (p, "0") ;
      s := DynamicStrings.Slice (DynamicStrings.Mark (s), 0, -1)
   END ;
   outTextS (p, s) ;
   s := KillString (s)
END doLiteralC ;


(*
   doLiteral -
*)

PROCEDURE doLiteral (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   assert (isLiteral (n)) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   IF n^.literalF.type=charN
   THEN
      IF DynamicStrings.char (s, -1)='C'
      THEN
         s := DynamicStrings.Slice (DynamicStrings.Mark (s), 0, -1) ;
	 IF DynamicStrings.char (s, 0)#'0'
         THEN
            s := DynamicStrings.ConCat (InitString('0'), DynamicStrings.Mark (s))
         END
      END ;
      outText (p, "(char)") ;
      setNeedSpace (p)
   END ;
   outTextS (p, s) ;
   s := KillString (s)
END doLiteral ;


(*
   isString - returns TRUE if node, n, is a string.
*)

PROCEDURE isString (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind=string
END isString ;


(*
   doString -
*)

PROCEDURE doString (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   assert (isString (n)) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   outTextS (p, s) ;
   s := KillString (s)
   ; HALT
   (*
   IF DynamicStrings.Index (s, '"', 0)=-1
   THEN
      outText (p, '"') ;
      outTextS (p, s) ;
      outText (p, '"')
   ELSIF DynamicStrings.Index (s, "'", 0)=-1
   THEN
      outText (p, '"') ;
      outTextS (p, s) ;
      outText (p, '"')
   ELSE
      metaError1 ('illegal string {%1k}', n)
   END
   *)
END doString ;


(*
   replaceChar - replace every occurance of, ch, by, a and return modified string, s.
*)

PROCEDURE replaceChar (s: String; ch: CHAR; a: ARRAY OF CHAR) : String ;
VAR
   i: INTEGER ;
BEGIN
   i := 0 ;
   LOOP
      i := DynamicStrings.Index (s, ch, i) ;
      IF i = 0
      THEN
         s := ConCat (InitString (a), DynamicStrings.Slice (s, 1, 0)) ;
         i := StrLen (a)
      ELSIF i > 0
      THEN
         s := ConCat (ConCat (DynamicStrings.Slice (s, 0, i), Mark (InitString (a))), DynamicStrings.Slice (s, i+1, 0)) ;
         INC (i, StrLen (a))
      ELSE
         RETURN s
      END
   END
END replaceChar ;


(*
   toCstring - translates string, n, into a C string
               and returns the new String.
*)

PROCEDURE toCstring (n: Name) : String ;
VAR
   s: String ;
BEGIN
   s := DynamicStrings.Slice (InitStringCharStar (keyToCharStar (n)), 1, -1) ;
   RETURN replaceChar (replaceChar (s, '\', '\\'), '"', '\"')
END toCstring ;


(*
   toCchar -
*)

PROCEDURE toCchar (n: Name) : String ;
VAR
   s: String ;
BEGIN
   s := DynamicStrings.Slice (InitStringCharStar (keyToCharStar (n)), 1, -1) ;
   RETURN replaceChar (replaceChar (s, '\', '\\'), "'", "\'")
END toCchar ;


(*
   countChar -
*)

PROCEDURE countChar (s: String; ch: CHAR) : CARDINAL ;
VAR
   i: INTEGER ;
   c: CARDINAL ;
BEGIN
   c := 0 ;
   i := 0 ;
   LOOP
      i := DynamicStrings.Index (s, ch, i) ;
      IF i >= 0
      THEN
         INC (i) ;
         INC (c)
      ELSE
         RETURN c
      END
   END
END countChar ;


(*
   lenCstring -
*)

PROCEDURE lenCstring (s: String) : CARDINAL ;
BEGIN
   RETURN DynamicStrings.Length (s) - countChar (s, '\')
END lenCstring ;


(*
   outCstring -
*)

PROCEDURE outCstring (p: pretty; s: node; aString: BOOLEAN) ;
BEGIN
   IF aString
   THEN
      outText (p, '"') ;
      outRawS (p, s^.stringF.cstring) ;
      outText (p, '"')
   ELSE
      outText (p, "'") ;
      outRawS (p, s^.stringF.cchar) ;
      outText (p, "'")
   END
END outCstring ;


(*
   doStringC -
*)

PROCEDURE doStringC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   assert (isString (n)) ;
   outCstring (p, n, NOT n^.stringF.isCharCompatible)
(*
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   IF DynamicStrings.Length (s)>3
   THEN
      IF DynamicStrings.Index (s, '"', 0)=-1
      THEN
         s := DynamicStrings.Slice (s, 1, -1) ;
         outText (p, '"') ;
         outCstring (p, s) ;
         outText (p, '"')
      ELSIF DynamicStrings.Index (s, "'", 0)=-1
      THEN
         s := DynamicStrings.Slice (s, 1, -1) ;
         outText (p, '"') ;
         outCstring (p, s) ;
         outText (p, '"')
      ELSE
         metaError1 ('illegal string {%1k}', n)
      END
   ELSIF DynamicStrings.Length (s) = 3
   THEN
      s := DynamicStrings.Slice (s, 1, -1) ;
      outText (p, "'") ;
      IF DynamicStrings.char (s, 0) = "'"
      THEN
         outText (p, "\'")
      ELSIF DynamicStrings.char (s, 0) = "\"
      THEN
         outText (p, "\\")
      ELSE
         outTextS (p, s)
      END ;
      outText (p, "'")
   ELSE
      outText (p, "'\0'")
   END ;
   s := KillString (s)
*)
END doStringC ;


(*
   isPunct -
*)

PROCEDURE isPunct (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch = '.') OR (ch = '(') OR (ch = ')') OR
          (ch = '^') OR (ch = ':') OR (ch = ';') OR
	  (ch = '{') OR (ch = '}') OR (ch = ',') OR
	  (ch = '*')
END isPunct ;


(*
   isWhite -
*)

PROCEDURE isWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch = ' ') OR (ch = tab) OR (ch = lf)
END isWhite ;


(*
   outText -
*)

PROCEDURE outText (p: pretty; a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := InitString (a) ;
   outTextS (p, s) ;
   s := KillString (s)
END outText ;


(*
   outRawS -
*)

PROCEDURE outRawS (p: pretty; s: String) ;
BEGIN
   raw (p, s)
END outRawS ;


(*
   outKm2 -
*)

PROCEDURE outKm2 (p: pretty; a: ARRAY OF CHAR) : pretty ;
VAR
   i: CARDINAL ;
   s: String ;
BEGIN
   IF StrEqual (a, 'RECORD')
   THEN
      p := pushPretty (p) ;
      i := getcurpos (p) ;
      setindent (p, i) ;
      outText (p, a) ;
      p := pushPretty (p) ;
      setindent (p, i + indentation)
   ELSIF StrEqual (a, 'END')
   THEN
      p := popPretty (p) ;
      outText (p, a) ;
      p := popPretty (p)
   END ;
   RETURN p
END outKm2 ;


(*
   outKc -
*)

PROCEDURE outKc (p: pretty; a: ARRAY OF CHAR) : pretty ;
VAR
   i   : INTEGER ;
   c   : CARDINAL ;
   s, t: String ;
BEGIN
   s := InitString (a) ;
   i := DynamicStrings.Index (s, '\', 0) ;
   IF i=-1
   THEN
      t := NIL
   ELSE
      t := DynamicStrings.Slice (s, i, 0) ;
      s := DynamicStrings.Slice (Mark (s), 0, i)
   END ;
   IF DynamicStrings.char (s, 0)='{'
   THEN
      p := pushPretty (p) ;
      c := getcurpos (p) ;
      setindent (p, c) ;
      outTextS (p, s) ;
      p := pushPretty (p) ;
      setindent (p, c + indentationC)
   ELSIF DynamicStrings.char (s, 0)='}'
   THEN
      p := popPretty (p) ;
      outTextS (p, s) ;
      p := popPretty (p)
   END ;
   outTextS (p, t) ;
   t := KillString (t) ;
   s := KillString (s) ;
   RETURN p
END outKc ;


(*
   outTextS -
*)

PROCEDURE outTextS (p: pretty; s: String) ;
BEGIN
   IF s # NIL
   THEN
      prints (p, s)
   END
END outTextS ;


(*
   outCard -
*)

PROCEDURE outCard (p: pretty; c: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := CardinalToString (c, 0, ' ', 10, FALSE) ;
   outTextS (p, s) ;
   s := KillString (s)
END outCard ;


(*
   outTextN -
*)

PROCEDURE outTextN (p: pretty; n: Name) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (n)) ;
   prints (p, s) ;
   s := KillString (s)
END outTextN ;


(*
   doTypeAliasC -
*)

PROCEDURE doTypeAliasC (p: pretty; n: node; VAR m: node) ;
BEGIN
   print (p, "typedef") ; setNeedSpace (p) ;
   IF isTypeHidden (n) AND (isDef (getMainModule ()) OR (getScope (n) # getMainModule ()))
   THEN
      outText (p, "void *")
   ELSE
      doTypeC (p, getType (n), m)
   END ;
   IF m#NIL
   THEN
      doFQNameC (p, m)
   END ;
   print (p, ';\n\n')
END doTypeAliasC ;


(*
   doEnumerationC -
*)

PROCEDURE doEnumerationC (p: pretty; n: node) ;
VAR
   i, h: CARDINAL ;
   s   : node ;
   t   : String ;
BEGIN
   outText (p, "enum {") ;
   i := LowIndice (n^.enumerationF.listOfSons) ;
   h := HighIndice (n^.enumerationF.listOfSons) ;
   WHILE i <= h DO
      s := GetIndice (n^.enumerationF.listOfSons, i) ;
      doFQDNameC (p, s, FALSE) ;
      IF i < h
      THEN
         outText (p, ",") ; setNeedSpace (p)
      END ;
      INC (i)
   END ;
   outText (p, "}")
END doEnumerationC ;


(*
   doNamesC -
*)

PROCEDURE doNamesC (p: pretty; n: Name) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (n)) ;
   outTextS (p, s) ;
   s := KillString (s)
END doNamesC ;


(*
   doNameC -
*)

PROCEDURE doNameC (p: pretty; n: node) ;
BEGIN
   IF (n#NIL) AND (getSymName (n)#NulName)
   THEN
      doNamesC (p, getSymName (n))
   END
END doNameC ;


(*
   initCname -
*)

PROCEDURE initCname (VAR c: cnameT) ;
BEGIN
   c.init := FALSE
END initCname ;


(*
   doCname -
*)

PROCEDURE doCname (n: Name; VAR c: cnameT; scopes: BOOLEAN) : Name ;
VAR
   s: String ;
BEGIN
   IF c.init
   THEN
      RETURN c.name
   ELSE
      c.init := TRUE ;
      s := keyc.cname (n, scopes) ;
      IF s=NIL
      THEN
         c.name := n
      ELSE
         c.name := makekey (DynamicStrings.string (s)) ;
         s := KillString (s)
      END ;
      RETURN c.name
   END
END doCname ;


(*
   getDName -
*)

PROCEDURE getDName (n: node; scopes: BOOLEAN) : Name ;
VAR
   m: Name ;
BEGIN
   m := getSymName (n) ;
   CASE n^.kind OF

   procedure       :  RETURN doCname (m, n^.procedureF.cname, scopes) |
   var             :  RETURN doCname (m, n^.varF.cname, scopes) |
   recordfield     :  RETURN doCname (m, n^.recordfieldF.cname, scopes) |
   enumerationfield:  RETURN doCname (m, n^.enumerationfieldF.cname, scopes)

   ELSE
   END ;
   RETURN m
END getDName ;


(*
   doDNameC -
*)

PROCEDURE doDNameC (p: pretty; n: node; scopes: BOOLEAN) ;
BEGIN
   IF (n#NIL) AND (getSymName (n)#NulName)
   THEN
      doNamesC (p, getDName (n, scopes))
   END
END doDNameC ;


(*
   doFQDNameC -
*)

PROCEDURE doFQDNameC (p: pretty; n: node; scopes: BOOLEAN) ;
VAR
   s: String ;
BEGIN
   s := getFQDstring (n, scopes) ;
   outTextS (p, s) ;
   s := KillString (s)
END doFQDNameC ;


(*
   doFQNameC -
*)

PROCEDURE doFQNameC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   s := getFQstring (n) ;
   outTextS (p, s) ;
   s := KillString (s)
END doFQNameC ;


(*
   doNameM2 -
*)

PROCEDURE doNameM2 (p: pretty; n: node) ;
BEGIN
   doNameC (p, n)
END doNameM2 ;


(*
   doUsed -
*)

PROCEDURE doUsed (p: pretty; used: BOOLEAN) ;
BEGIN
   IF NOT used
   THEN
      setNeedSpace (p) ;
      outText (p, "__attribute__((unused))")
   END
END doUsed ;


(*
   doHighC -
*)

PROCEDURE doHighC (p: pretty; a: node; n: Name; isused: BOOLEAN) ;
BEGIN
   IF isArray (a) AND isUnbounded (a)
   THEN
      (* need to display high.  *)
      print (p, ",") ; setNeedSpace (p) ;
      doTypeNameC (p, cardinalN) ; setNeedSpace (p) ;
      print (p, "_") ; outTextN (p, n) ; print (p, "_high") ;
      doUsed (p, isused)
   END
END doHighC ;


(*
   doParamConstCast -
*)

PROCEDURE doParamConstCast (p: pretty; n: node) ;
VAR
   ptype: node ;
BEGIN
   ptype := getType (n) ;
   IF isArray (ptype) AND isUnbounded (ptype) AND (lang = ansiCP)
   THEN
      outText (p, "const") ;
      setNeedSpace (p)
   END
END doParamConstCast ;


(*
   getParameterVariable - returns the variable which shadows the parameter
                          named, m, in parameter block, n.
*)

PROCEDURE getParameterVariable (n: node; m: Name) : node ;
VAR
   p: node ;
BEGIN
   assert (isParam (n) OR isVarParam (n)) ;
   IF isParam (n)
   THEN
      p := n^.paramF.scope
   ELSE
      p := n^.varparamF.scope
   END ;
   assert (isProcedure (p)) ;
   RETURN lookupInScope (p, m)
END getParameterVariable ;


(*
   doParamTypeEmit - emit parameter type for C/C++.  It checks to see if the
                     parameter type is a procedure type and if it were declared
                     in a definition module for "C" and if so it uses the "C"
                     definition for a procedure type, rather than the mc
                     C++ version.
*)

PROCEDURE doParamTypeEmit (p: pretty; paramnode, paramtype: node) ;
BEGIN
   assert (isParam (paramnode) OR isVarParam (paramnode)) ;
   IF isForC (paramnode) AND isProcType (skipType (paramtype))
   THEN
      doFQNameC (p, paramtype) ;
      outText (p, "_C")
   ELSE
      doTypeNameC (p, paramtype)
   END
END doParamTypeEmit ;


(*
   doParamC - emit parameter for C/C++.
*)

PROCEDURE doParamC (p: pretty; n: node) ;
VAR
   v,
   ptype: node ;
   i    : Name ;
   c, t : CARDINAL ;
   l    : wlist ;
BEGIN
   assert (isParam (n)) ;
   ptype := getType (n) ;
   IF n^.paramF.namelist = NIL
   THEN
      doParamConstCast (p, n) ;
      doTypeNameC (p, ptype) ;
      doUsed (p, n^.paramF.isUsed) ;
      IF isArray (ptype) AND isUnbounded (ptype)
      THEN
         outText (p, ',') ; setNeedSpace (p) ;
         outText (p, 'unsigned int')
      END
   ELSE
      assert (isIdentList (n^.paramF.namelist)) ;
      l := n^.paramF.namelist^.identlistF.names ;
      IF l=NIL
      THEN
         doParamConstCast (p, n) ;
         doParamTypeEmit (p, n, ptype) ;
         IF isArray (ptype) AND isUnbounded (ptype)
         THEN
            doUsed (p, n^.paramF.isUsed) ;
            outText (p, ',') ; setNeedSpace (p) ;
            outText (p, 'unsigned int')
         END
      ELSE
         t := wlists.noOfItemsInList (l) ;
         c := 1 ;
         WHILE c <= t DO
            doParamConstCast (p, n) ;
            doParamTypeEmit (p, n, ptype) ;
            i := wlists.getItemFromList (l, c) ;
            IF isArray (ptype) AND isUnbounded (ptype)
            THEN
               noSpace (p)
            ELSE
               setNeedSpace (p)
            END ;
            v := getParameterVariable (n, i) ;
            IF v=NIL
            THEN
               doNamesC (p, keyc.cnamen (i, TRUE))
            ELSE
               doFQDNameC (p, v, TRUE)
            END ;
            IF isArray (ptype) AND isUnbounded (ptype)
            THEN
               outText (p, '_')
            END ;
            doUsed (p, n^.paramF.isUsed) ;
            doHighC (p, ptype, i, n^.paramF.isUsed) ;
            IF c<t
            THEN
               outText (p, ',') ; setNeedSpace (p)
            END ;
            INC (c)
         END
      END
   END
END doParamC ;


(*
   doVarParamC - emit a VAR parameter for C/C++.
*)

PROCEDURE doVarParamC (p: pretty; n: node) ;
VAR
   v,
   ptype: node ;
   i    : Name ;
   c, t : CARDINAL ;
   l    : wlist ;
BEGIN
   assert (isVarParam (n)) ;
   ptype := getType (n) ;
   IF n^.varparamF.namelist = NIL
   THEN
      doTypeNameC (p, ptype) ;
      (* doTypeC (p, ptype, n) ; *)
      IF NOT isArray (ptype)
      THEN
         setNeedSpace (p) ;
         outText (p, "*")
      END ;
      doUsed (p, n^.varparamF.isUsed) ;
      IF isArray (ptype) AND isUnbounded (ptype)
      THEN
         outText (p, ',') ; setNeedSpace (p) ;
         outText (p, 'unsigned int')
      END
   ELSE
      assert (isIdentList (n^.varparamF.namelist)) ;
      l := n^.varparamF.namelist^.identlistF.names ;
      IF l=NIL
      THEN
         doParamTypeEmit (p, n, ptype) ;
         doUsed (p, n^.varparamF.isUsed)
      ELSE
         t := wlists.noOfItemsInList (l) ;
         c := 1 ;
         WHILE c <= t DO
            doParamTypeEmit (p, n, ptype) ;
	    IF NOT isArray (ptype)
            THEN
               setNeedSpace (p) ;
               outText (p, "*")
            END ;
            i := wlists.getItemFromList (l, c) ;
            v := getParameterVariable (n, i) ;
            IF v=NIL
            THEN
               doNamesC (p, keyc.cnamen (i, TRUE))
            ELSE
               doFQDNameC (p, v, TRUE)
            END ;
            doUsed (p, n^.varparamF.isUsed) ;
            doHighC (p, ptype, i, n^.varparamF.isUsed) ;
            IF c<t
            THEN
               outText (p, ',') ; setNeedSpace (p)
            END ;
            INC (c)
         END
      END
   END
END doVarParamC ;


(*
   doOptargC -
*)

PROCEDURE doOptargC (p: pretty; n: node) ;
VAR
   ptype: node ;
   i    : Name ;
   t    : CARDINAL ;
   l    : wlist ;
BEGIN
   assert (isOptarg (n)) ;
   ptype := getType (n) ;
   assert (n^.optargF.namelist # NIL) ;
   assert (isIdentList (n^.paramF.namelist)) ;
   l := n^.paramF.namelist^.identlistF.names ;
   assert (l # NIL) ;
   t := wlists.noOfItemsInList (l) ;
   assert (t = 1) ;
   doTypeNameC (p, ptype) ;
   i := wlists.getItemFromList (l, 1) ;
   setNeedSpace (p) ;
   doNamesC (p, i)
END doOptargC ;


(*
   doParameterC -
*)

PROCEDURE doParameterC (p: pretty; n: node) ;
BEGIN
   IF isParam (n)
   THEN
      doParamC (p, n)
   ELSIF isVarParam (n)
   THEN
      doVarParamC (p, n)
   ELSIF isVarargs (n)
   THEN
      print (p, "...")
   ELSIF isOptarg (n)
   THEN
      doOptargC (p, n)
   END
END doParameterC ;


(*
   doProcTypeC -
*)

PROCEDURE doProcTypeC (p: pretty; t, n: node) ;
BEGIN
   assert (isType (t)) ;
   outputPartial (t) ;
   doCompletePartialProcType (p, t, n)
END doProcTypeC ;


(*
   doTypesC -
*)

PROCEDURE doTypesC (n: node) ;
VAR
   m: node ;
BEGIN
   IF isType (n)
   THEN
      m := getType (n) ;
      IF isProcType (m)
      THEN
         doProcTypeC (doP, n, m)
      ELSIF isType (m) OR isPointer (m)
      THEN
         outText (doP, "typedef") ; setNeedSpace (doP) ;
         doTypeC (doP, m, m) ;
	 IF isType (m)
         THEN
            setNeedSpace (doP)
         END ;
         doTypeNameC (doP, n) ;
         outText (doP, ";\n\n")
      ELSIF isEnumeration (m)
      THEN
         outText (doP, "typedef") ; setNeedSpace (doP) ;
         doTypeC (doP, m, m) ;
         setNeedSpace (doP) ;
	 doTypeNameC (doP, n) ;
         outText (doP, ";\n\n")
      ELSE
         outText (doP, "typedef") ; setNeedSpace (doP) ;
         doTypeC (doP, m, m) ;
	 IF isType (m)
         THEN
            setNeedSpace (doP)
         END ;
         doTypeNameC (doP, n) ;
         outText (doP, ";\n\n")
      END
   END
END doTypesC ;


(*
   doCompletePartialC -
*)

PROCEDURE doCompletePartialC (n: node) ;
VAR
   m: node ;
BEGIN
   IF isType (n)
   THEN
      m := getType (n) ;
      IF isRecord (m)
      THEN
         doCompletePartialRecord (doP, n, m)
      ELSIF isArray (m)
      THEN
         doCompletePartialArray (doP, n, m)
      ELSIF isProcType (m)
      THEN
         doCompletePartialProcType (doP, n, m)
      END
   END
END doCompletePartialC ;


(*
   doCompletePartialRecord -
*)

PROCEDURE doCompletePartialRecord (p: pretty; t, r: node) ;
VAR
   i, h: CARDINAL ;
   f   : node ;
BEGIN
   assert (isRecord (r)) ;
   assert (isType (t)) ;
   outText (p, "struct") ; setNeedSpace (p) ;
   doFQNameC (p, t) ;
   outText (p, "_r") ; setNeedSpace (p) ;
   p := outKc (p, "{\n") ;
   i := LowIndice (r^.recordF.listOfSons) ;
   h := HighIndice (r^.recordF.listOfSons) ;
   WHILE i<=h DO
      f := GetIndice (r^.recordF.listOfSons, i) ;
      IF isRecordField (f)
      THEN
         IF NOT f^.recordfieldF.tag
         THEN
            setNeedSpace (p) ;
            doRecordFieldC (p, f) ;
            outText (p, ";\n")
         END
      ELSIF isVarient (f)
      THEN
         doVarientC (p, f) ;
         outText (p, ";\n")
      ELSIF isVarientField (f)
      THEN
         doVarientFieldC (p, f)
      END ;
      INC (i)
   END ;
   p := outKc (p, "};\n\n")
END doCompletePartialRecord ;


(*
   doCompletePartialArray -
*)

PROCEDURE doCompletePartialArray (p: pretty; t, r: node) ;
VAR
   type, s: node ;
BEGIN
   assert (isArray (r)) ;
   type := r^.arrayF.type ;
   s := NIL ;
   outText (p, "struct") ; setNeedSpace (p) ;
   doFQNameC (p, t) ;
   outText (p, "_a {") ;
   setNeedSpace (p) ;
   doTypeC (p, type, s) ;
   setNeedSpace (p) ;
   outText (p, "array[") ;
   doSubrC (p, r^.arrayF.subr) ;
   outText (p, "];") ;
   setNeedSpace (p) ;
   outText (p, "};\n")
END doCompletePartialArray ;


(*
   lookupConst -
*)

PROCEDURE lookupConst (type: node; n: Name) : node ;
BEGIN
   RETURN makeLiteralInt (n)
END lookupConst ;


(*
   doMin -
*)

PROCEDURE doMin (n: node) : node ;
BEGIN
   IF n=booleanN
   THEN
      RETURN falseN
   ELSIF n=integerN
   THEN
      keyc.useIntMin ;
      RETURN lookupConst (integerN, makeKey ('INT_MIN'))
   ELSIF n=cardinalN
   THEN
      keyc.useUIntMin ;
      RETURN lookupConst (cardinalN, makeKey ('UINT_MIN'))
   ELSIF n=longintN
   THEN
      keyc.useLongMin ;
      RETURN lookupConst (longintN, makeKey ('LONG_MIN'))
   ELSIF n=longcardN
   THEN
      keyc.useULongMin ;
      RETURN lookupConst (longcardN, makeKey ('LONG_MIN'))
   ELSIF n=charN
   THEN
      keyc.useCharMin ;
      RETURN lookupConst (charN, makeKey ('CHAR_MIN'))
   ELSIF n=bitsetN
   THEN
      assert (isSubrange (bitnumN)) ;
      RETURN bitnumN^.subrangeF.low
   ELSIF n=locN
   THEN
      keyc.useUCharMin ;
      RETURN lookupConst (locN, makeKey ('UCHAR_MIN'))
   ELSIF n=byteN
   THEN
      keyc.useUCharMin ;
      RETURN lookupConst (byteN, makeKey ('UCHAR_MIN'))
   ELSIF n=wordN
   THEN
      keyc.useUIntMin ;
      RETURN lookupConst (wordN, makeKey ('UCHAR_MIN'))
   ELSIF n=addressN
   THEN
      RETURN lookupConst (addressN, makeKey ('((void *) 0)'))
   ELSE
      HALT  (* finish the cacading elsif statement.  *)
   END
END doMin ;


(*
   doMax -
*)

PROCEDURE doMax (n: node) : node ;
BEGIN
   IF n=booleanN
   THEN
      RETURN trueN
   ELSIF n=integerN
   THEN
      keyc.useIntMax ;
      RETURN lookupConst (integerN, makeKey ('INT_MAX'))
   ELSIF n=cardinalN
   THEN
      keyc.useUIntMax ;
      RETURN lookupConst (cardinalN, makeKey ('UINT_MAX'))
   ELSIF n=longintN
   THEN
      keyc.useLongMax ;
      RETURN lookupConst (longintN, makeKey ('LONG_MAX'))
   ELSIF n=longcardN
   THEN
      keyc.useULongMax ;
      RETURN lookupConst (longcardN, makeKey ('ULONG_MAX'))
   ELSIF n=charN
   THEN
      keyc.useCharMax ;
      RETURN lookupConst (charN, makeKey ('CHAR_MAX'))
   ELSIF n=bitsetN
   THEN
      assert (isSubrange (bitnumN)) ;
      RETURN bitnumN^.subrangeF.high
   ELSIF n=locN
   THEN
      keyc.useUCharMax ;
      RETURN lookupConst (locN, makeKey ('UCHAR_MAX'))
   ELSIF n=byteN
   THEN
      keyc.useUCharMax ;
      RETURN lookupConst (byteN, makeKey ('UCHAR_MAX'))
   ELSIF n=wordN
   THEN
      keyc.useUIntMax ;
      RETURN lookupConst (wordN, makeKey ('UINT_MAX'))
   ELSIF n=addressN
   THEN
      metaError1 ('trying to obtain MAX ({%1ad}) is illegal', n) ;
      RETURN NIL
   ELSE
      HALT  (* finish the cacading elsif statement.  *)
   END
END doMax ;


(*
   getMax -
*)

PROCEDURE getMax (n: node) : node ;
BEGIN
   n := skipType (n) ;
   IF isSubrange (n)
   THEN
      RETURN n^.subrangeF.high
   ELSIF isEnumeration (n)
   THEN
      RETURN n^.enumerationF.high
   ELSE
      assert (isOrdinal (n)) ;
      RETURN doMax (n)
   END
END getMax ;


(*
   getMin -
*)

PROCEDURE getMin (n: node) : node ;
BEGIN
   n := skipType (n) ;
   IF isSubrange (n)
   THEN
      RETURN n^.subrangeF.low
   ELSIF isEnumeration (n)
   THEN
      RETURN n^.enumerationF.low
   ELSE
      assert (isOrdinal (n)) ;
      RETURN doMin (n)
   END
END getMin ;


(*
   doSubtractC -
*)

PROCEDURE doSubtractC (p: pretty; s: node) ;
BEGIN
   IF NOT isZero (s)
   THEN
      outText (p, "-") ;
      doExprC (p, s)
   END
END doSubtractC ;


(*
   doSubrC -
*)

PROCEDURE doSubrC (p: pretty; s: node) ;
VAR
   low, high: node ;
BEGIN
   s := skipType (s) ;
   IF isOrdinal (s)
   THEN
      low := getMin (s) ;
      high := getMax (s) ;
      doExprC (p, high) ;
      doSubtractC (p, low) ;
      outText (p, "+1")
   ELSIF isEnumeration (s)
   THEN
      low := getMin (s) ;
      high := getMax (s) ;
      doExprC (p, high) ;
      doSubtractC (p, low) ;
      outText (p, "+1")
   ELSE
      assert (isSubrange (s)) ;
      IF (s^.subrangeF.high = NIL) OR (s^.subrangeF.low = NIL)
      THEN
         doSubrC (p, getType (s))
      ELSE
         doExprC (p, s^.subrangeF.high) ;
         doSubtractC (p, s^.subrangeF.low) ;
         outText (p, "+1")
      END
   END
END doSubrC ;


(*
   doCompletePartialProcType -
*)

PROCEDURE doCompletePartialProcType (p: pretty; t, n: node) ;
VAR
   i, h: CARDINAL ;
   v, u: node ;
BEGIN
   assert (isProcType (n)) ;
   u := NIL ;
   outText (p, "typedef") ; setNeedSpace (p) ;
   doTypeC (p, n^.proctypeF.returnType, u) ; setNeedSpace (p) ;
   outText (p, "(*") ;
   doFQNameC (p, t) ;
   outText (p, "_t) (") ;
   i := LowIndice (n^.proctypeF.parameters) ;
   h := HighIndice (n^.proctypeF.parameters) ;
   WHILE i <= h DO
      v := GetIndice (n^.proctypeF.parameters, i) ;
      doParameterC (p, v) ;
      noSpace (p) ;
      IF i < h
      THEN
         outText (p, ",") ; setNeedSpace (p)
      END ;
      INC (i)
   END ;
   IF h=0
   THEN
      outText (p, "void")
   END ;
   outText (p, ");\n") ;
   IF isDefForCNode (n)
   THEN
      (* emit a C named type which differs from the m2 proctype.  *)
      outText (p, "typedef") ; setNeedSpace (p) ;
      doFQNameC (p, t) ;
      outText (p, "_t") ; setNeedSpace (p) ;
      doFQNameC (p, t) ;
      outText (p, "_C;\n\n")
   END ;
   outText (p, "struct") ; setNeedSpace (p) ;
   doFQNameC (p, t) ;
   outText (p, "_p {") ; setNeedSpace (p) ;
   doFQNameC (p, t) ;
   outText (p, "_t proc; };\n\n")
END doCompletePartialProcType ;


(*
   isBase -
*)

PROCEDURE isBase (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   char,
   cardinal,
   longcard,
   shortcard,
   integer,
   longint,
   shortint,
   complex,
   longcomplex,
   shortcomplex,
   real,
   longreal,
   shortreal,
   bitset,
   boolean,
   proc     :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isBase ;


(*
   doBoolC -
*)

PROCEDURE doBoolC (p: pretty) ;
BEGIN
   IF useBool ()
   THEN
      outText (p, 'bool')
   ELSE
      outText (p, 'unsigned int')
   END
END doBoolC ;


(*
   doBaseC -
*)

PROCEDURE doBaseC (p: pretty; n: node) ;
BEGIN
   CASE n^.kind OF

   char        :  outText (p, 'char') |
   cardinal    :  outText (p, 'unsigned int') |
   longcard    :  outText (p, 'long unsigned int') |
   shortcard   :  outText (p, 'short unsigned int') |
   integer     :  outText (p, 'int') |
   longint     :  outText (p, 'long int') |
   shortint    :  outText (p, 'short int') |
   complex     :  outText (p, 'double complex') |
   longcomplex :  outText (p, 'long double complex') |
   shortcomplex:  outText (p, 'float complex') |
   real        :  outTextS (p, getCRealType ()) |
   longreal    :  outTextS (p, getCLongRealType ()) |
   shortreal   :  outTextS (p, getCShortRealType ()) |
   bitset      :  outText (p, 'unsigned int') |
   boolean     :  doBoolC (p) |
   proc        :  outText (p, 'PROC')

   END ;
   setNeedSpace (p)
END doBaseC ;


(*
   isSystem -
*)

PROCEDURE isSystem (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   address:  RETURN TRUE |
   loc    :  RETURN TRUE |
   byte   :  RETURN TRUE |
   word   :  RETURN TRUE |
   csizet :  RETURN TRUE |
   cssizet:  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isSystem ;


(*
   doSystemC -
*)

PROCEDURE doSystemC (p: pretty; n: node) ;
BEGIN
   CASE n^.kind OF

   address:  outText (p, 'void *') |
   loc    :  outText (p, 'unsigned char') ; setNeedSpace (p) |
   byte   :  outText (p, 'unsigned char') ; setNeedSpace (p) |
   word   :  outText (p, 'unsigned int') ; setNeedSpace (p) |
   csizet :  outText (p, 'size_t') ; setNeedSpace (p) ; keyc.useSize_t |
   cssizet:  outText (p, 'ssize_t') ; setNeedSpace (p) ; keyc.useSSize_t

   END
END doSystemC ;


(*
   doArrayC -
*)

PROCEDURE doArrayC (p: pretty; n: node) ;
VAR
   t, s, u: node ;
BEGIN
   assert (isArray (n)) ;
   t := n^.arrayF.type ;
   s := n^.arrayF.subr ;
   u := NIL ;
   IF s=NIL
   THEN
      doTypeC (p, t, u) ;
      setNeedSpace (p) ;
      outText (p, "*")
   ELSE
      outText (p, "struct") ;
      setNeedSpace (p) ;
      outText (p, "{") ;
      setNeedSpace (p) ;
      doTypeC (p, t, u) ;
      setNeedSpace (p) ;
      outText (p, "array[") ;
      IF isZero (getMin (s))
      THEN
         doExprC (p, getMax (s))
      ELSE
         doExprC (p, getMax (s)) ;
         doSubtractC (p, getMin (s))
      END ;
      outText (p, "];") ;
      setNeedSpace (p) ;
      outText (p, "}") ;
      setNeedSpace (p)
   END
END doArrayC ;


(*
   doPointerC -
*)

PROCEDURE doPointerC (p: pretty; n: node; VAR m: node) ;
VAR
   t, s: node ;
BEGIN
   t := n^.pointerF.type ;
   s := NIL ;
   doTypeC (p, t, s) ;
   setNeedSpace (p) ;
   outText (p, "*")
END doPointerC ;


(*
   doRecordFieldC -
*)

PROCEDURE doRecordFieldC (p: pretty; f: node) ;
VAR
   m: node ;
BEGIN
   m := NIL ;
   setNeedSpace (p) ;
   doTypeC (p, f^.recordfieldF.type, m) ;
   doDNameC (p, f, FALSE)
END doRecordFieldC ;


(*
   doVarientFieldC -
*)

PROCEDURE doVarientFieldC (p: pretty; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   assert (isVarientField (n)) ;
   IF NOT n^.varientfieldF.simple
   THEN
      outText (p, "struct") ; setNeedSpace (p) ;
      p := outKc (p, "{\n")
   END ;
   i := LowIndice (n^.varientfieldF.listOfSons) ;
   t := HighIndice (n^.varientfieldF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.varientfieldF.listOfSons, i) ;
      IF isRecordField (q)
      THEN
         IF NOT q^.recordfieldF.tag
         THEN
            doRecordFieldC (p, q) ;
            outText (p, ";\n")
         END
      ELSIF isVarient (q)
      THEN
         doVarientC (p, q) ;
         outText (p, ";\n")
      ELSE
         HALT
      END ;
      INC (i)
   END ;
   IF NOT n^.varientfieldF.simple
   THEN
      p := outKc (p, "};\n")
   END
END doVarientFieldC ;


(*
   doVarientC -
*)

PROCEDURE doVarientC (p: pretty; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   assert (isVarient (n)) ;
   IF n^.varientF.tag # NIL
   THEN
      IF isRecordField (n^.varientF.tag)
      THEN
         doRecordFieldC (p, n^.varientF.tag) ;
	 outText (p, ";  /* case tag */\n")
      ELSIF isVarientField (n^.varientF.tag)
      THEN
         HALT
         (* doVarientFieldC (p, n^.varientF.tag) *)
      ELSE
         HALT
      END
   END ;
   outText (p, "union") ;
   setNeedSpace (p) ;
   p := outKc (p, "{\n") ;
   i := LowIndice (n^.varientF.listOfSons) ;
   t := HighIndice (n^.varientF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.varientF.listOfSons, i) ;
      IF isRecordField (q)
      THEN
         IF NOT q^.recordfieldF.tag
         THEN
            doRecordFieldC (p, q) ;
            outText (p, ";\n")
         END
      ELSIF isVarientField (q)
      THEN
         doVarientFieldC (p, q)
      ELSE
         HALT
      END ;
      INC (i)
   END ;
   p := outKc (p, "}")
END doVarientC ;


(*
   doRecordC -
*)

PROCEDURE doRecordC (p: pretty; n: node; VAR m: node) ;
VAR
   i, h: CARDINAL ;
   f   : node ;
BEGIN
   assert (isRecord (n)) ;
   outText (p, "struct") ;
   setNeedSpace (p) ;
   p := outKc (p, "{") ;
   i := LowIndice (n^.recordF.listOfSons) ;
   h := HighIndice (n^.recordF.listOfSons) ;
   setindent (p, getcurpos (p) + indentation) ;
   outText (p, "\n") ;
   WHILE i<=h DO
      f := GetIndice (n^.recordF.listOfSons, i) ;
      IF isRecordField (f)
      THEN
         IF NOT f^.recordfieldF.tag
         THEN
            doRecordFieldC (p, f) ;
            outText (p, ";\n")
         END
      ELSIF isVarient (f)
      THEN
         doVarientC (p, f) ;
         outText (p, ";\n")
      ELSIF isVarientField (f)
      THEN
         doVarientFieldC (p, f)
      END ;
      INC (i)
   END ;
   p := outKc (p, "}") ;
   setNeedSpace (p)
END doRecordC ;


(*
   isBitset -
*)

PROCEDURE isBitset (n: node) : BOOLEAN ;
BEGIN
   RETURN n = bitsetN
END isBitset ;


(*
   isNegative - returns TRUE if expression, n, is negative.
*)

PROCEDURE isNegative (n: node) : BOOLEAN ;
BEGIN
   (* --fixme-- needs to be completed.  *)
   RETURN FALSE
END isNegative ;


(*
   doSubrangeC -
*)

PROCEDURE doSubrangeC (p: pretty; n: node) ;
BEGIN
   assert (isSubrange (n)) ;
   IF isNegative (n^.subrangeF.low)
   THEN
      outText (p, "int") ; setNeedSpace (p)
   ELSE
      outText (p, "unsigned int") ; setNeedSpace (p)
   END
END doSubrangeC ;


(*
   doSetC - generates a C type which holds the set.
            Currently we only support sets of size WORD.
*)

PROCEDURE doSetC (p: pretty; n: node) ;
BEGIN
   assert (isSet (n)) ;
   outText (p, "unsigned int") ; setNeedSpace (p)
END doSetC ;


(*
   doTypeC -
*)

PROCEDURE doTypeC (p: pretty; n: node; VAR m: node) ;
BEGIN
   IF n=NIL
   THEN
      outText (p, "void")
   ELSIF isBase (n)
   THEN
      doBaseC (p, n)
   ELSIF isSystem (n)
   THEN
      doSystemC (p, n)
   ELSIF isEnumeration (n)
   THEN
      doEnumerationC (p, n)
   ELSIF isType (n)
   THEN
      doFQNameC (p, n) ;
      setNeedSpace (p)
      (* doTypeAliasC (p, n, n) *)  (* type, n, has a name, so we choose this over, m.  *)
(*
   ELSIF isProcType (n) OR isArray (n) OR isRecord (n)
   THEN
      HALT  (* n should have been simplified.  *)
*)
   ELSIF isProcType (n)
   THEN
      doProcTypeC (p, n, m)
   ELSIF isArray (n)
   THEN
      doArrayC (p, n)
   ELSIF isRecord (n)
   THEN
      doRecordC (p, n, m)
   ELSIF isPointer (n)
   THEN
      doPointerC (p, n, m)
   ELSIF isSubrange (n)
   THEN
      doSubrangeC (p, n)
   ELSIF isSet (n)
   THEN
      doSetC (p, n)
   ELSE
      (* --fixme--  *)
      print (p, "to do ...  typedef etc etc ") ; doFQNameC (p, n) ; print (p, ";\n") ;
      HALT
   END
END doTypeC ;


(*
   doArrayNameC - it displays the array declaration (it might be an unbounded).
*)

PROCEDURE doArrayNameC (p: pretty; n: node) ;
BEGIN
   doTypeNameC (p, getType (n)) ; setNeedSpace (p) ; outText (p, "*")
END doArrayNameC ;


(*
   doRecordNameC - emit the C/C++ record name <name of n>"_r".
*)

PROCEDURE doRecordNameC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   s := getFQstring (n) ;
   s := ConCat (s, Mark (InitString ("_r"))) ;
   outTextS (p, s) ;
   s := KillString (s)
END doRecordNameC ;


(*
   doPointerNameC - emit the C/C++ pointer type <name of n>*.
*)

PROCEDURE doPointerNameC (p: pretty; n: node) ;
BEGIN
   doTypeNameC (p, getType (n)) ; setNeedSpace (p) ; outText (p, "*")
END doPointerNameC ;


(*
   doTypeNameC -
*)

PROCEDURE doTypeNameC (p: pretty; n: node) ;
VAR
   t: String ;
BEGIN
   IF n=NIL
   THEN
      outText (p, "void") ;
      setNeedSpace (p)
   ELSIF isBase (n)
   THEN
      doBaseC (p, n)
   ELSIF isSystem (n)
   THEN
      doSystemC (p, n)
   ELSIF isEnumeration (n)
   THEN
      print (p, "is enumeration type name required\n")
   ELSIF isType (n)
   THEN
      doFQNameC (p, n) ;
   ELSIF isProcType (n)
   THEN
      doFQNameC (p, n) ;
      outText (p, "_t")
   ELSIF isArray (n)
   THEN
      doArrayNameC (p, n)
   ELSIF isRecord (n)
   THEN
      doRecordNameC (p, n)
   ELSIF isPointer (n)
   THEN
      doPointerNameC (p, n)
   ELSIF isSubrange (n)
   THEN
      doSubrangeC (p, n)
   ELSE
      print (p, "is type unknown required\n") ;
      stop
   END
END doTypeNameC ;


(*
   isExternal - returns TRUE if symbol, n, was declared in another module.
*)

PROCEDURE isExternal (n: node) : BOOLEAN ;
VAR
   s: node ;
BEGIN
   s := getScope (n) ;
   RETURN (s # NIL) AND isDef (s) AND
           ((isImp (getMainModule ()) AND (s # lookupDef (getSymName (getMainModule ())))) OR
            isModule (getMainModule ()))
END isExternal ;


(*
   doVarC -
*)

PROCEDURE doVarC (n: node) ;
VAR
   s: node ;
BEGIN
   IF isDef (getMainModule ())
   THEN
      print (doP, "EXTERN") ; setNeedSpace (doP)
   ELSIF (NOT isExported (n)) AND (NOT isLocal (n))
   THEN
      print (doP, "static") ; setNeedSpace (doP)
   ELSIF getExtendedOpaque ()
   THEN
      IF isExternal (n)
      THEN
         (* different module declared this variable, therefore it is extern.  *)
         print (doP, "extern") ; setNeedSpace (doP)
      END
   END ;
   s := NIL ;
   doTypeC (doP, getType (n), s) ;
   setNeedSpace (doP) ;
   doFQDNameC (doP, n, FALSE) ;
   print (doP, ";\n")
END doVarC ;


(*
   doExternCP -
*)

PROCEDURE doExternCP (p: pretty) ;
BEGIN
   IF lang = ansiCP
   THEN
      outText (p, 'extern "C"') ; setNeedSpace (p)
   END
END doExternCP ;


(*
   doProcedureCommentText -
*)

PROCEDURE doProcedureCommentText (p: pretty; s: String) ;
BEGIN
   (* remove \n from the start of the comment.  *)
   WHILE (DynamicStrings.Length (s) > 0) AND (DynamicStrings.char (s, 0) = lf) DO
      s := DynamicStrings.Slice (s, 1, 0)
   END ;
   outTextS (p, s)
END doProcedureCommentText ;


(*
   doProcedureComment -
*)

PROCEDURE doProcedureComment (p: pretty; s: String) ;
BEGIN
   IF s # NIL
   THEN
      outText (p, '\n/*\n') ;
      doProcedureCommentText (p, s) ;
      outText (p, '*/\n\n')
   END
END doProcedureComment ;


(*
   doProcedureHeadingC -
*)

PROCEDURE doProcedureHeadingC (n: node; prototype: BOOLEAN) ;
VAR
   i, h: CARDINAL ;
   p, q: node ;
BEGIN
   assert (isProcedure (n)) ;
   noSpace (doP) ;
   IF isDef (getMainModule ())
   THEN
      doProcedureComment (doP, getContent (n^.procedureF.defComment)) ;
      outText (doP, "EXTERN") ; setNeedSpace (doP)
   ELSIF isExported (n)
   THEN
      doProcedureComment (doP, getContent (n^.procedureF.modComment)) ;
      doExternCP (doP)
   ELSE
      doProcedureComment (doP, getContent (n^.procedureF.modComment)) ;
      outText (doP, "static") ; setNeedSpace (doP)
   END ;
   q := NIL ;
   doTypeC (doP, n^.procedureF.returnType, q) ; setNeedSpace (doP) ;
   doFQDNameC (doP, n, FALSE) ;
   setNeedSpace (doP) ;
   outText (doP, "(") ;
   i := LowIndice (n^.procedureF.parameters) ;
   h := HighIndice (n^.procedureF.parameters) ;
   WHILE i <= h DO
      p := GetIndice (n^.procedureF.parameters, i) ;
      doParameterC (doP, p) ;
      noSpace (doP) ;
      IF i < h
      THEN
         print (doP, ",") ; setNeedSpace (doP)
      END ;
      INC (i)
   END ;
   IF h=0
   THEN
      outText (doP, "void")
   END ;
   print (doP, ")") ;
   IF n^.procedureF.noreturn AND prototype AND (NOT getSuppressNoReturn ())
   THEN
      setNeedSpace (doP) ;
      outText (doP, "__attribute__ ((noreturn))")
   END
END doProcedureHeadingC ;


(*
   checkDeclareUnboundedParamCopyC -
*)

PROCEDURE checkDeclareUnboundedParamCopyC (p: pretty; n: node) : BOOLEAN ;
VAR
   t   : node ;
   i, c: CARDINAL ;
   l   : wlist ;
   seen: BOOLEAN ;
BEGIN
   seen := FALSE ;
   t := getType (n) ;
   l := n^.paramF.namelist^.identlistF.names ;
   IF isArray (t) AND isUnbounded (t) AND (l#NIL)
   THEN
      t := getType (t) ;
      c := wlists.noOfItemsInList (l) ;
      i := 1 ;
      WHILE i <= c DO
         doTypeNameC (p, t) ;
         setNeedSpace (p) ;
         doNamesC (p, wlists.getItemFromList (l, i)) ;
         outText (p, '[_');
         doNamesC (p, wlists.getItemFromList (l, i)) ;
         outText (p, '_high+1];\n');
         seen := TRUE ;
         INC (i)
      END
   END ;
   RETURN seen
END checkDeclareUnboundedParamCopyC ;


(*
   checkUnboundedParamCopyC -
*)

PROCEDURE checkUnboundedParamCopyC (p: pretty; n: node) ;
VAR
   t, s: node ;
   i, c: CARDINAL ;
   l   : wlist ;
BEGIN
   t := getType (n) ;
   l := n^.paramF.namelist^.identlistF.names ;
   IF isArray (t) AND isUnbounded (t) AND (l#NIL)
   THEN
      c := wlists.noOfItemsInList (l) ;
      i := 1 ;
      t := getType (t) ;
      s := skipType (t) ;
      WHILE i <= c DO
         keyc.useMemcpy ;
         outText (p, 'memcpy (') ;
         doNamesC (p, wlists.getItemFromList (l, i)) ;
         outText (p, ',') ;
         setNeedSpace (p) ;
         doNamesC (p, wlists.getItemFromList (l, i)) ;
         outText (p, '_, ') ;
         IF (s = charN) OR (s = byteN) OR (s = locN)
         THEN
            outText (p, '_') ;
            doNamesC (p, wlists.getItemFromList (l, i)) ;
            outText (p, '_high+1);\n')
         ELSE
            outText (p, '(_') ;
            doNamesC (p, wlists.getItemFromList (l, i)) ;
            outText (p, '_high+1)') ;
            setNeedSpace (p) ;
            doMultiplyBySize (p, t) ;
            outText (p, ');\n')
         END ;
         INC (i)
      END
   END
END checkUnboundedParamCopyC ;


(*
   doUnboundedParamCopyC -
*)

PROCEDURE doUnboundedParamCopyC (p: pretty; n: node) ;
VAR
   i, h: CARDINAL ;
   q   : node ;
   seen: BOOLEAN ;
BEGIN
   assert (isProcedure (n)) ;
   i := LowIndice (n^.procedureF.parameters) ;
   h := HighIndice (n^.procedureF.parameters) ;
   seen := FALSE ;
   WHILE i <= h DO
      q := GetIndice (n^.procedureF.parameters, i) ;
      IF isParam (q)
      THEN
         seen := checkDeclareUnboundedParamCopyC (p, q) OR seen
      END ;
      INC (i)
   END ;
   IF seen
   THEN
      outText (p, "\n") ;
      outText (p, "/* make a local copy of each unbounded array.  */\n") ;
      i := LowIndice (n^.procedureF.parameters) ;
      WHILE i <= h DO
         q := GetIndice (n^.procedureF.parameters, i) ;
         IF isParam (q)
         THEN
            checkUnboundedParamCopyC (p, q)
         END ;
         INC (i)
      END
   END
END doUnboundedParamCopyC ;


(*
   doPrototypeC -
*)

PROCEDURE doPrototypeC (n: node) ;
BEGIN
   IF NOT isExported (n)
   THEN
      keyc.enterScope (n) ;
      doProcedureHeadingC (n, TRUE) ;
      print (doP, ";\n") ;
      keyc.leaveScope (n)
   END
END doPrototypeC ;


(*
   addTodo - adds, n, to the todo list.
*)

PROCEDURE addTodo (n: node) ;
BEGIN
   IF (n#NIL) AND
      (NOT alists.isItemInList (partialQ, n)) AND
      (NOT alists.isItemInList (doneQ, n))
   THEN
      assert (NOT isVarient (n)) ;
      assert (NOT isVarientField (n)) ;
      assert (NOT isDef (n)) ;
      alists.includeItemIntoList (todoQ, n)
   END
END addTodo ;


(*
   addVariablesTodo -
*)

PROCEDURE addVariablesTodo (n: node) ;
BEGIN
   IF isVar (n)
   THEN
      IF n^.varF.isParameter OR n^.varF.isVarParameter
      THEN
         addDone (n) ;
         addTodo (getType (n))
      ELSE
         addTodo (n)
      END
   END
END addVariablesTodo ;


(*
   addTypesTodo -
*)

PROCEDURE addTypesTodo (n: node) ;
BEGIN
   IF isUnbounded (n)
   THEN
      addDone (n)
   ELSE
      addTodo (n)
   END
END addTypesTodo ;


(*
   tempName -
*)

PROCEDURE tempName () : String ;
BEGIN
   INC (tempCount) ;
   RETURN Sprintf1 (InitString ("_T%d"), tempCount) ;
END tempName ;


(*
   makeIntermediateType -
*)

PROCEDURE makeIntermediateType (s: String; p: node) : node ;
VAR
   n: Name ;
   o: node ;
BEGIN
   n := makekey (DynamicStrings.string (s)) ;
   enterScope (getScope (p)) ;
   o := p ;
   p := makeType (makekey (DynamicStrings.string (s))) ;
   putType (p, o) ;
   putTypeInternal (p) ;
   leaveScope ;
   RETURN p
END makeIntermediateType ;


(*
   simplifyType -
*)

PROCEDURE simplifyType (l: alist; VAR p: node) ;
VAR
   s: String ;
BEGIN
   IF (p#NIL) AND (isRecord (p) OR isArray (p) OR isProcType (p)) AND (NOT isUnbounded (p))
   THEN
      s := tempName () ;
      p := makeIntermediateType (s, p) ;
      s := KillString (s) ;
      simplified := FALSE
   END ;
   simplifyNode (l, p)
END simplifyType ;


(*
   simplifyVar -
*)

PROCEDURE simplifyVar (l: alist; n: node) ;
VAR
   i, t: CARDINAL ;
   v,
   d, o: node ;
BEGIN
   assert (isVar (n)) ;
   o := n^.varF.type ;
   simplifyType (l, n^.varF.type) ;
   IF o # n^.varF.type
   THEN
      (* simplification has occurred, make sure that all other variables of this type
         use the new type.  *)
      d := n^.varF.decl ;
      assert (isVarDecl (d)) ;
      t := wlists.noOfItemsInList (d^.vardeclF.names) ;
      i := 1 ;
      WHILE i<=t DO
         v := lookupInScope (n^.varF.scope, wlists.getItemFromList (d^.vardeclF.names, i)) ;
         assert (isVar (v)) ;
         v^.varF.type := n^.varF.type ;
         INC (i)
      END
   END
END simplifyVar ;


(*
   simplifyRecord -
*)

PROCEDURE simplifyRecord (l: alist; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.recordF.listOfSons) ;
   t := HighIndice (n^.recordF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.recordF.listOfSons, i) ;
      simplifyNode (l, q) ;
      INC (i)
   END
END simplifyRecord ;


(*
   simplifyVarient -
*)

PROCEDURE simplifyVarient (l: alist; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   simplifyNode (l, n^.varientF.tag) ;
   i := LowIndice (n^.varientF.listOfSons) ;
   t := HighIndice (n^.varientF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.varientF.listOfSons, i) ;
      simplifyNode (l, q) ;
      INC (i)
   END
END simplifyVarient ;


(*
   simplifyVarientField -
*)

PROCEDURE simplifyVarientField (l: alist; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.varientfieldF.listOfSons) ;
   t := HighIndice (n^.varientfieldF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.varientfieldF.listOfSons, i) ;
      simplifyNode (l, q) ;
      INC (i)
   END
END simplifyVarientField ;


(*
   doSimplifyNode -
*)

PROCEDURE doSimplifyNode (l: alist; n: node) ;
BEGIN
   IF n=NIL
   THEN
      (* nothing.  *)
   ELSIF isType (n)
   THEN
      (* no need to simplify a type.  *)
      simplifyNode (l, getType (n))
   ELSIF isVar (n)
   THEN
      simplifyVar (l, n)
   ELSIF isRecord (n)
   THEN
      simplifyRecord (l, n)
   ELSIF isRecordField (n)
   THEN
      simplifyType (l, n^.recordfieldF.type)
   ELSIF isArray (n)
   THEN
      simplifyType (l, n^.arrayF.type)
   ELSIF isVarient (n)
   THEN
      simplifyVarient (l, n)
   ELSIF isVarientField (n)
   THEN
      simplifyVarientField (l, n)
   ELSIF isPointer (n)
   THEN
      simplifyType (l, n^.pointerF.type)
   END
END doSimplifyNode ;


(*
   simplifyNode -
*)

PROCEDURE simplifyNode (l: alist; n: node) ;
BEGIN
   IF NOT alists.isItemInList (l, n)
   THEN
      alists.includeItemIntoList (l, n) ;
      doSimplifyNode (l, n)
   END
END simplifyNode ;


(*
   doSimplify -
*)

PROCEDURE doSimplify (n: node) ;
VAR
   l: alist ;
BEGIN
   l := alists.initList () ;
   simplifyNode (l, n) ;
   alists.killList (l)
END doSimplify ;


(*
   simplifyTypes -
*)

PROCEDURE simplifyTypes (s: scopeT) ;
BEGIN
   REPEAT
      simplified := TRUE ;
      ForeachIndiceInIndexDo (s.types, doSimplify) ;
      ForeachIndiceInIndexDo (s.variables, doSimplify)
   UNTIL simplified
END simplifyTypes ;


(*
   outDeclsDefC -
*)

PROCEDURE outDeclsDefC (p: pretty; n: node) ;
VAR
   s: scopeT ;
BEGIN
   s := n^.defF.decls ;
   simplifyTypes (s) ;
   includeConstType (s) ;

   doP := p ;

   topologicallyOut (doConstC, doTypesC, doVarC,
                     outputPartial,
                     doNone, doCompletePartialC, doNone) ;

   (* try and output types, constants before variables and procedures.  *)
   includeDefVarProcedure (n) ;

   topologicallyOut (doConstC, doTypesC, doVarC,
                     outputPartial,
                     doNone, doCompletePartialC, doNone) ;

   ForeachIndiceInIndexDo (s.procedures, doPrototypeC)
END outDeclsDefC ;


(*
   includeConstType -
*)

PROCEDURE includeConstType (s: scopeT) ;
BEGIN
   ForeachIndiceInIndexDo (s.constants, addTodo) ;
   ForeachIndiceInIndexDo (s.types, addTypesTodo)
END includeConstType ;


(*
   includeVarProcedure -
*)

PROCEDURE includeVarProcedure (s: scopeT) ;
BEGIN
   ForeachIndiceInIndexDo (s.procedures, addTodo) ;
   ForeachIndiceInIndexDo (s.variables, addVariablesTodo)
END includeVarProcedure ;


(*
   includeVar -
*)

PROCEDURE includeVar (s: scopeT) ;
BEGIN
   ForeachIndiceInIndexDo (s.variables, addTodo)
END includeVar ;


(*
   includeExternals -
*)

PROCEDURE includeExternals (n: node) ;
VAR
   l: alist ;
BEGIN
   l := alists.initList () ;
   visitNode (l, n, addExported) ;
   alists.killList (l)
END includeExternals ;


(*
   checkSystemInclude -
*)

PROCEDURE checkSystemInclude (n: node) ;
BEGIN

END checkSystemInclude ;


(*
   addExported -
*)

PROCEDURE addExported (n: node) ;
VAR
   s: node ;
BEGIN
   s := getScope (n) ;
   IF (s # NIL) AND isDef (s) AND (s # defModule)
   THEN
      IF isType (n) OR isVar (n) OR isConst (n)
      THEN
         addTodo (n)
      END
   END
END addExported ;


(*
   addExternal - only adds, n, if this symbol is external to the
                 implementation module and is not a hidden type.
*)

PROCEDURE addExternal (n: node) ;
BEGIN
   IF (getScope (n) = defModule) AND isType (n) AND
      isTypeHidden (n) AND (NOT getExtendedOpaque ())
   THEN
      (* do nothing.  *)
   ELSIF NOT isDef (n)
   THEN
      addTodo (n)
   END
END addExternal ;


(*
   includeDefConstType -
*)

PROCEDURE includeDefConstType (n: node) ;
VAR
   d: node ;
BEGIN
   IF isImp (n)
   THEN
      defModule := lookupDef (getSymName (n)) ;
      IF defModule#NIL
      THEN
         simplifyTypes (defModule^.defF.decls) ;
         includeConstType (defModule^.defF.decls) ;
	 foreachNodeDo (defModule^.defF.decls.symbols, addExternal)
      END
   END
END includeDefConstType ;


(*
   runIncludeDefConstType -
*)

PROCEDURE runIncludeDefConstType (n: node) ;
VAR
   d: node ;
BEGIN
   IF isDef (n)
   THEN
      simplifyTypes (n^.defF.decls) ;
      includeConstType (n^.defF.decls) ;
      foreachNodeDo (n^.defF.decls.symbols, addExternal)
   END
END runIncludeDefConstType ;


(*
   joinProcedures - copies procedures from definition module,
                    d, into implementation module, i.
*)

PROCEDURE joinProcedures (i, d: node) ;
VAR
   h, j: CARDINAL ;
BEGIN
   assert (isDef (d)) ;
   assert (isImp (i)) ;
   j := 1 ;
   h := HighIndice (d^.defF.decls.procedures) ;
   WHILE j<=h DO
      IncludeIndiceIntoIndex (i^.impF.decls.procedures,
                              GetIndice (d^.defF.decls.procedures, j)) ;
      INC (j)
   END
END joinProcedures ;


(*
   includeDefVarProcedure -
*)

PROCEDURE includeDefVarProcedure (n: node) ;
VAR
   d: node ;
BEGIN
   IF isImp (n)
   THEN
      defModule := lookupDef (getSymName (n)) ;
      IF defModule#NIL
      THEN
(*
         includeVar (defModule^.defF.decls) ;
         simplifyTypes (defModule^.defF.decls) ;
*)
         joinProcedures (n, defModule)
      END
   ELSIF isDef (n)
   THEN
      includeVar (n^.defF.decls) ;
      simplifyTypes (n^.defF.decls)
   END
END includeDefVarProcedure ;


(*
   foreachModuleDo -
*)

PROCEDURE foreachModuleDo (n: node; p: performOperation) ;
BEGIN
   foreachDefModuleDo (p) ;
   foreachModModuleDo (p)
END foreachModuleDo ;


(*
   outDeclsImpC -
*)

PROCEDURE outDeclsImpC (p: pretty; s: scopeT) ;
BEGIN
   simplifyTypes (s) ;
   includeConstType (s) ;

   doP := p ;

   topologicallyOut (doConstC, doTypesC, doVarC,
                     outputPartial,
                     doNone, doCompletePartialC, doNone) ;

   (* try and output types, constants before variables and procedures.  *)
   includeVarProcedure (s) ;

   topologicallyOut (doConstC, doTypesC, doVarC,
                     outputPartial,
                     doNone, doCompletePartialC, doNone) ;

END outDeclsImpC ;


(*
   doStatementSequenceC -
*)

PROCEDURE doStatementSequenceC (p: pretty; s: node) ;
VAR
   i, h: CARDINAL ;
BEGIN
   assert (isStatementSequence (s)) ;
   h := HighIndice (s^.stmtF.statements) ;
   i := 1 ;
   WHILE i<=h DO
      doStatementsC (p, GetIndice (s^.stmtF.statements, i)) ;
      INC (i)
   END
END doStatementSequenceC ;


(*
   isStatementSequenceEmpty -
*)

PROCEDURE isStatementSequenceEmpty (s: node) : BOOLEAN ;
BEGIN
   assert (isStatementSequence (s)) ;
   RETURN HighIndice (s^.stmtF.statements) = 0
END isStatementSequenceEmpty ;


(*
   isSingleStatement - returns TRUE if the statement sequence, s, has
                       only one statement.
*)

PROCEDURE isSingleStatement (s: node) : BOOLEAN ;
VAR
   h: CARDINAL ;
BEGIN
   assert (isStatementSequence (s)) ;
   h := HighIndice (s^.stmtF.statements) ;
   IF (h = 0) OR (h > 1)
   THEN
      RETURN FALSE
   END ;
   s := GetIndice (s^.stmtF.statements, 1) ;
   RETURN (NOT isStatementSequence (s)) OR isSingleStatement (s)
END isSingleStatement ;


(*
   doCommentC -
*)

PROCEDURE doCommentC (p: pretty; s: node) ;
VAR
   c: String ;
BEGIN
   IF s # NIL
   THEN
      assert (isComment (s)) ;
      IF NOT isProcedureComment (s^.commentF.content)
      THEN
         IF isAfterComment (s^.commentF.content)
         THEN
            setNeedSpace (p) ;
            outText (p, " /* ")
         ELSE
            outText (p, "/* ")
         END ;
         c := getContent (s^.commentF.content) ;
         c := RemoveWhitePrefix (RemoveWhitePostfix (c)) ;
         outTextS (p, c) ;
         outText (p, "  */\n")
      END
   END
END doCommentC ;


(*
   doAfterCommentC - emit an after comment, c, or a newline if, c, is empty.
*)

PROCEDURE doAfterCommentC (p: pretty; c: node) ;
BEGIN
   IF c = NIL
   THEN
      outText (p, "\n")
   ELSE
      doCommentC (p, c)
   END
END doAfterCommentC ;


(*
   doReturnC - issue a return statement and also place in an after comment if one exists.
*)

PROCEDURE doReturnC (p: pretty; s: node) ;
BEGIN
   assert (isReturn (s)) ;
   doCommentC (p, s^.returnF.returnComment.body) ;
   outText (p, "return") ;
   IF s^.returnF.scope#NIL
   THEN
      setNeedSpace (p) ;
      IF (NOT isProcedure (s^.returnF.scope)) OR (getType (s^.returnF.scope)=NIL)
      THEN
         metaError1 ('{%1DMad} has no return type', s^.returnF.scope) ;
      ELSE
         doExprCastC (p, s^.returnF.exp, getType (s^.returnF.scope))
      END
   END ;
   outText (p, ";") ;
   doAfterCommentC (p, s^.returnF.returnComment.after)
END doReturnC ;


(*
   isZtypeEquivalent -
*)

PROCEDURE isZtypeEquivalent (type: node) : BOOLEAN ;
BEGIN
   CASE type^.kind OF

   cardinal,
   longcard,
   shortcard,
   integer,
   longint,
   shortint,
   ztype    :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isZtypeEquivalent ;


(*
   isEquivalentType - returns TRUE if type1 and type2 are equivalent.
*)

PROCEDURE isEquivalentType (type1, type2: node) : BOOLEAN ;
BEGIN
   type1 := skipType (type1) ;
   type2 := skipType (type2) ;
   RETURN ((type1 = type2) OR
           (isZtypeEquivalent (type1) AND isZtypeEquivalent (type2)))
END isEquivalentType ;


(*
   doExprCastC - build a cast if necessary.
*)

PROCEDURE doExprCastC (p: pretty; e, type: node) ;
VAR
   stype: node ;
BEGIN
   stype := skipType (type) ;
   IF (NOT isEquivalentType (type, getExprType (e))) AND
      (NOT ((e^.kind = nil) AND (isPointer (stype) OR (stype^.kind = address))))
   THEN
      IF lang = ansiCP
      THEN
         (* potentially a cast is required.  *)
         IF isPointer (type) OR (type = addressN)
         THEN
            outText (p, 'reinterpret_cast<') ;
            doTypeNameC (p, type) ;
            noSpace (p) ;
            outText (p, '> (') ;
            doExprC (p, e) ;
	    outText (p, ')') ;
            RETURN
         ELSE
            outText (p, 'static_cast<') ;
            IF isProcType (skipType (type))
            THEN
               doTypeNameC (p, type) ;
               outText (p, "_t")
            ELSE
               doTypeNameC (p, type)
            END ;
            noSpace (p) ;
            outText (p, '> (') ;
            doExprC (p, e) ;
	    outText (p, ')') ;
            RETURN
         END
      END
   END ;
   doExprC (p, e)
END doExprCastC ;


(*
   requiresUnpackProc - returns TRUE if either the expr is a procedure or the proctypes differ.
*)

PROCEDURE requiresUnpackProc (s: node) : BOOLEAN ;
BEGIN
   assert (isAssignment (s)) ;
   RETURN isProcedure (s^.assignmentF.expr) OR
          (skipType (getType (s^.assignmentF.des)) # skipType (getType (s^.assignmentF.expr)))
END requiresUnpackProc ;


(*
   doAssignmentC -
*)

PROCEDURE doAssignmentC (p: pretty; s: node) ;
BEGIN
   assert (isAssignment (s)) ;
   doCommentC (p, s^.assignmentF.assignComment.body) ;
   doExprCup (p, s^.assignmentF.des, requiresUnpackProc (s)) ;
   setNeedSpace (p) ;
   outText (p, "=") ;
   setNeedSpace (p) ;
   doExprCastC (p, s^.assignmentF.expr, getType (s^.assignmentF.des)) ;
   outText (p, ";") ;
   doAfterCommentC (p, s^.assignmentF.assignComment.after)
END doAssignmentC ;


(*
   containsStatement -
*)

PROCEDURE containsStatement (s: node) : BOOLEAN ;
BEGIN
   RETURN (s # NIL) AND isStatementSequence (s) AND (NOT isStatementSequenceEmpty (s))
END containsStatement ;


(*
   doCompoundStmt -
*)

PROCEDURE doCompoundStmt (p: pretty; s: node) ;
BEGIN
   IF (s = NIL) OR (isStatementSequence (s) AND isStatementSequenceEmpty (s))
   THEN
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      outText (p, "{}  /* empty.  */\n") ;
      p := popPretty (p)
   ELSIF isStatementSequence (s) AND isSingleStatement (s) AND (NOT forceCompoundStatement)
   THEN
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      doStatementSequenceC (p, s) ;
      p := popPretty (p)
   ELSE
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      outText (p, "{\n") ;
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      doStatementSequenceC (p, s) ;
      p := popPretty (p) ;
      outText (p, "}\n") ;
      p := popPretty (p)
   END
END doCompoundStmt ;


(*
   doElsifC -
*)

PROCEDURE doElsifC (p: pretty; s: node) ;
BEGIN
   assert (isElsif (s)) ;
   outText (p, "else if") ;
   setNeedSpace (p) ;
   outText (p, "(") ;
   doExprC (p, s^.elsifF.expr) ;
   outText (p, ")\n") ;
   assert ((s^.elsifF.else = NIL) OR (s^.elsifF.elsif = NIL)) ;
   IF forceCompoundStatement OR
      (hasIfAndNoElse (s^.elsifF.then) AND
       ((s^.elsifF.else # NIL) OR (s^.elsifF.elsif # NIL)))
   THEN
      (* avoid dangling else.  *)
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      outText (p, "{\n") ;
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      outText (p, "/* avoid dangling else.  */\n") ;
      doStatementSequenceC (p, s^.elsifF.then) ;
      p := popPretty (p) ;
      outText (p, "}\n") ;
      p := popPretty (p)
   ELSE
      doCompoundStmt (p, s^.elsifF.then)
   END ;
   IF containsStatement (s^.elsifF.else)
   THEN
      outText (p, "else\n") ;
      IF forceCompoundStatement
      THEN
         (* avoid dangling else.  *)
         p := pushPretty (p) ;
         setindent (p, getindent (p) + indentationC) ;
         outText (p, "{\n") ;
         p := pushPretty (p) ;
         setindent (p, getindent (p) + indentationC) ;
         outText (p, "/* avoid dangling else.  */\n") ;
         doStatementSequenceC (p, s^.elsifF.else) ;
         p := popPretty (p) ;
         outText (p, "}\n") ;
         p := popPretty (p)
      ELSE
         doCompoundStmt (p, s^.elsifF.else)
      END
   ELSIF (s^.elsifF.elsif#NIL) AND isElsif (s^.elsifF.elsif)
   THEN
      doElsifC (p, s^.elsifF.elsif)
   END
END doElsifC ;


(*
   noIfElse -
*)

PROCEDURE noIfElse (n: node) : BOOLEAN ;
BEGIN
   RETURN (n # NIL) AND isIf (n) AND (n^.ifF.else = NIL) AND (n^.ifF.elsif = NIL)
END noIfElse ;


(*
   noIfElseChained - returns TRUE if, n, is an IF statement which
                     has no associated ELSE statement.  An IF with an
                     ELSIF is also checked for no ELSE and will result
                     in a return value of TRUE.
*)

PROCEDURE noIfElseChained (n: node) : BOOLEAN ;
VAR
   e: node ;
BEGIN
   IF n # NIL
   THEN
      IF isIf (n)
      THEN
         IF n^.ifF.else # NIL
         THEN
            (* we do have an else, continue to check this statement.  *)
            RETURN hasIfAndNoElse (n^.ifF.else)
         ELSIF n^.ifF.elsif = NIL
         THEN
            (* neither else or elsif.  *)
            RETURN TRUE
         ELSE
            (* test elsif for lack of else.  *)
            e := n^.ifF.elsif ;
	    assert (isElsif (e)) ;
            RETURN noIfElseChained (e)
         END
      ELSIF isElsif (n)
      THEN
         IF n^.elsifF.else # NIL
         THEN
            (* we do have an else, continue to check this statement.  *)
            RETURN hasIfAndNoElse (n^.elsifF.else)
         ELSIF n^.elsifF.elsif = NIL
         THEN
            (* neither else or elsif.  *)
            RETURN TRUE
         ELSE
            (* test elsif for lack of else.  *)
            e := n^.elsifF.elsif ;
	    assert (isElsif (e)) ;
            RETURN noIfElseChained (e)
         END
      END
   END ;
   RETURN FALSE
END noIfElseChained ;


(*
   hasIfElse -
*)

PROCEDURE hasIfElse (n: node) : BOOLEAN ;
BEGIN
   IF n # NIL
   THEN
      IF isStatementSequence (n)
      THEN
         IF isStatementSequenceEmpty (n)
         THEN
            RETURN FALSE
         ELSIF isSingleStatement (n)
         THEN
            n := GetIndice (n^.stmtF.statements, 1) ;
	    RETURN isIfElse (n)
         END
      END
   END ;
   RETURN FALSE
END hasIfElse ;


(*
   isIfElse -
*)

PROCEDURE isIfElse (n: node) : BOOLEAN ;
BEGIN
   RETURN (n # NIL) AND isIf (n) AND ((n^.ifF.else # NIL) OR (n^.ifF.elsif # NIL))
END isIfElse ;


(*
   hasIfAndNoElse - returns TRUE if statement, n, is a single statement
                    which is an IF and it has no else statement.
*)

PROCEDURE hasIfAndNoElse (n: node) : BOOLEAN ;
BEGIN
   IF n # NIL
   THEN
      IF isStatementSequence (n)
      THEN
         IF isStatementSequenceEmpty (n)
         THEN
            RETURN FALSE
         ELSIF isSingleStatement (n)
         THEN
            n := GetIndice (n^.stmtF.statements, 1) ;
	    RETURN hasIfAndNoElse (n)
         ELSE
            n := GetIndice (n^.stmtF.statements, HighIndice (n^.stmtF.statements)) ;
	    RETURN hasIfAndNoElse (n)
         END
      ELSIF isElsif (n) OR isIf (n)
      THEN
         RETURN noIfElseChained (n)
      END
   END ;
   RETURN FALSE
END hasIfAndNoElse ;


(*
   doIfC - issue an if statement and also place in an after comment if one exists.
           The if statement might contain an else or elsif which are also handled.
*)

PROCEDURE doIfC (p: pretty; s: node) ;
BEGIN
   assert (isIf (s)) ;
   doCommentC (p, s^.ifF.ifComment.body) ;
   outText (p, "if") ;
   setNeedSpace (p) ;
   outText (p, "(") ;
   doExprC (p, s^.ifF.expr) ;
   outText (p, ")") ;
   doAfterCommentC (p, s^.ifF.ifComment.after) ;
   IF hasIfAndNoElse (s^.ifF.then) AND
      ((s^.ifF.else # NIL) OR (s^.ifF.elsif # NIL))
   THEN
      (* avoid dangling else.  *)
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      outText (p, "{\n") ;
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      outText (p, "/* avoid dangling else.  */\n") ;
      doStatementSequenceC (p, s^.ifF.then) ;
      p := popPretty (p) ;
      outText (p, "}\n") ;
      p := popPretty (p)
   ELSIF noIfElse (s) AND hasIfElse (s^.ifF.then)
   THEN
      (* gcc does not like legal non dangling else, as it is poor style.
         So we will avoid getting a warning.  *)
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      outText (p, "{\n") ;
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      outText (p, "/* avoid gcc warning by using compound statement even if not strictly necessary.  */\n") ;
      doStatementSequenceC (p, s^.ifF.then) ;
      p := popPretty (p) ;
      outText (p, "}\n") ;
      p := popPretty (p)
   ELSE
      doCompoundStmt (p, s^.ifF.then)
   END ;
   assert ((s^.ifF.else = NIL) OR (s^.ifF.elsif = NIL)) ;
   IF containsStatement (s^.ifF.else)
   THEN
      doCommentC (p, s^.ifF.elseComment.body) ;
      outText (p, "else") ;
      doAfterCommentC (p, s^.ifF.elseComment.after) ;
      doCompoundStmt (p, s^.ifF.else)
   ELSIF (s^.ifF.elsif#NIL) AND isElsif (s^.ifF.elsif)
   THEN
      doCommentC (p, s^.ifF.elseComment.body) ;
      doCommentC (p, s^.ifF.elseComment.after) ;
      doElsifC (p, s^.ifF.elsif)
   END ;
   doCommentC (p, s^.ifF.endComment.after) ;
   doCommentC (p, s^.ifF.endComment.body)
END doIfC ;


(*
   doForIncCP -
*)

PROCEDURE doForIncCP (p: pretty; s: node) ;
VAR
   t: node ;
BEGIN
   assert (isFor (s)) ;
   t := skipType (getType (s^.forF.des)) ;
   IF isEnumeration (t)
   THEN
      IF s^.forF.increment = NIL
      THEN
         doExprC (p, s^.forF.des) ;
         outText (p, "= static_cast<") ;
         doTypeNameC (p, getType (s^.forF.des)) ;
	 noSpace (p) ;
         outText (p, ">(static_cast<int>(") ;
         doExprC (p, s^.forF.des) ;
         outText (p, "+1))")
      ELSE
         doExprC (p, s^.forF.des) ;
         outText (p, "= static_cast<") ;
         doTypeNameC (p, getType (s^.forF.des)) ;
         noSpace (p) ;
         outText (p, ">(static_cast<int>(") ;
         doExprC (p, s^.forF.des) ;
         outText (p, "+") ;
         doExprC (p, s^.forF.increment) ;
         outText (p, "))")
      END
   ELSE
      doForIncC (p, s)
   END
END doForIncCP ;


(*
   doForIncC -
*)

PROCEDURE doForIncC (p: pretty; s: node) ;
BEGIN
   IF s^.forF.increment = NIL
   THEN
      doExprC (p, s^.forF.des) ;
      outText (p, "++")
   ELSE
      doExprC (p, s^.forF.des) ;
      outText (p, "=") ;
      doExprC (p, s^.forF.des) ;
      outText (p, "+") ;
      doExprC (p, s^.forF.increment)
   END
END doForIncC ;


(*
   doForInc -
*)

PROCEDURE doForInc (p: pretty; s: node) ;
BEGIN
   IF lang = ansiCP
   THEN
      doForIncCP (p, s)
   ELSE
      doForIncC (p, s)
   END
END doForInc ;


(*
   doForC -
*)

PROCEDURE doForC (p: pretty; s: node) ;
BEGIN
   assert (isFor (s)) ;
   outText (p, "for (") ;
   doExprC (p, s^.forF.des) ;
   outText (p, "=") ;
   doExprC (p, s^.forF.start) ;
   outText (p, ";") ;
   setNeedSpace (p) ;
   doExprC (p, s^.forF.des) ;
   outText (p, "<=") ;
   doExprC (p, s^.forF.end) ;
   outText (p, ";") ;
   setNeedSpace (p) ;
   doForInc (p, s) ;
   outText (p, ")\n") ;
   doCompoundStmt (p, s^.forF.statements)
END doForC ;


(*
   doRepeatC -
*)

PROCEDURE doRepeatC (p: pretty; s: node) ;
BEGIN
   assert (isRepeat (s)) ;
   doCommentC (p, s^.repeatF.repeatComment.body) ;
   outText (p, "do {") ;
   doAfterCommentC (p, s^.repeatF.repeatComment.after) ;
   p := pushPretty (p) ;
   setindent (p, getindent (p) + indentationC) ;
   doStatementSequenceC (p, s^.repeatF.statements) ;
   doCommentC (p, s^.repeatF.untilComment.body) ;
   p := popPretty (p) ;
   outText (p, "} while (! (") ;
   doExprC (p, s^.repeatF.expr) ;
   outText (p, "));") ;
   doAfterCommentC (p, s^.repeatF.untilComment.after)
END doRepeatC ;


(*
   doWhileC -
*)

PROCEDURE doWhileC (p: pretty; s: node) ;
BEGIN
   assert (isWhile (s)) ;
   doCommentC (p, s^.whileF.doComment.body) ;
   outText (p, "while (") ;
   doExprC (p, s^.whileF.expr) ;
   outText (p, ")") ;
   doAfterCommentC (p, s^.whileF.doComment.after) ;
   doCompoundStmt (p, s^.whileF.statements) ;
   doCommentC (p, s^.whileF.endComment.body) ;
   doCommentC (p, s^.whileF.endComment.after)
END doWhileC ;


(*
   doFuncHighC -
*)

PROCEDURE doFuncHighC (p: pretty; a: node) ;
VAR
   s, n: node ;
BEGIN
   IF isLiteral (a) AND (getType (a) = charN)
   THEN
      outCard (p, 0)
   ELSIF isString (a)
   THEN
      outCard (p, a^.stringF.length-2)
   ELSIF isConst (a) AND isString (a^.constF.value)
   THEN
      doFuncHighC (p, a^.constF.value)
   ELSIF isUnbounded (getType (a))
   THEN
      outText (p, '_') ;
      outTextN (p, getSymName (a)) ;
      outText (p, '_high')
   ELSIF isArray (skipType (getType (a)))
   THEN
      n := skipType (getType (a)) ;
      s := n^.arrayF.subr ;
      IF isZero (getMin (s))
      THEN
         doExprC (p, getMax (s))
      ELSE
         outText (p, '(') ;
         doExprC (p, getMax (s)) ;
         doSubtractC (p, getMin (s)) ;
         outText (p, ')')
      END
   ELSE
      (* output sizeof (a) in bytes for the high.  *)
      outText (p, '(sizeof') ;
      setNeedSpace (p) ;
      outText (p, '(') ;
      doExprC (p, a) ;
      outText (p, ')-1)')
   END
END doFuncHighC ;


(*
   doMultiplyBySize -
*)

PROCEDURE doMultiplyBySize (p: pretty; a: node) ;
BEGIN
   IF (a # charN) AND (a # byteN) AND (a # locN)
   THEN
      setNeedSpace (p) ;
      outText (p, '* sizeof (') ;
      doTypeNameC (p, a) ;
      noSpace (p) ;
      outText (p, ')')
   END
END doMultiplyBySize ;


(*
   doTotype -
*)

PROCEDURE doTotype (p: pretty; a, t: node) ;
BEGIN
   IF (NOT isString (a)) AND (NOT isLiteral (a))
   THEN
      IF isVar (a)
      THEN
         IF (a^.varF.isParameter OR a^.varF.isVarParameter) AND
	    isUnbounded (getType (a)) AND (skipType (getType (getType (a))) = skipType (getType (t)))
         THEN
            (* do not multiply by size as the existing high value is correct.  *)
            RETURN
         END ;
         a := getType (a) ;
         IF isArray (a)
         THEN
            doMultiplyBySize (p, skipType (getType (a)))
         END
      END
   END ;
   IF t = wordN
   THEN
      setNeedSpace (p) ;
      outText (p, '/ sizeof (') ;
      doTypeNameC (p, wordN) ;
      noSpace (p) ;
      outText (p, ')')
   END
END doTotype ;


(*
   doFuncUnbounded -
*)

PROCEDURE doFuncUnbounded (p: pretty; actual, formalParam, formal, func: node) ;
VAR
   h: node ;
   s: String ;
BEGIN
   assert (isUnbounded (formal)) ;
   outText (p, '(') ;
   IF (lang = ansiCP) AND isParam (formalParam)
   THEN
      outText (p, "const") ;
      setNeedSpace (p)
   END ;
   doTypeC (p, getType (formal), formal) ;
   setNeedSpace (p) ;
   outText (p, '*)') ;
   setNeedSpace (p) ;
   IF isLiteral (actual) AND (getType (actual) = charN)
   THEN
      outText (p, '"\0') ;
      s := InitStringCharStar (keyToCharStar (actual^.literalF.name)) ;
      s := DynamicStrings.Slice (DynamicStrings.Mark (s), 0, -1) ;
      outTextS (p, s) ;
      outText (p, '"') ;
      s := KillString (s)
   ELSIF isString (actual)
   THEN
      outCstring (p, actual, TRUE)
   ELSIF isConst (actual)
   THEN
      actual := resolveString (actual) ;
      assert (isString (actual)) ;
      outCstring (p, actual, TRUE)
   ELSIF isFuncCall (actual)
   THEN
      IF getExprType (actual) = NIL
      THEN
         metaError3 ('there is no return type to the procedure function {%3ad} which is being passed as the parameter {%1ad} to {%2ad}', formal, func, actual)
      ELSE
         outText (p, '&') ;
         doExprC (p, actual)
      END
   ELSIF isUnbounded (getType (actual))
   THEN
      doFQNameC (p, actual)
      (* doExprC (p, actual).  *)
   ELSE
      outText (p, '&') ;
      doExprC (p, actual) ;
      IF isArray (skipType (getType (actual)))
      THEN
         outText (p, '.array[0]')
      END
   END ;
   IF NOT (enableDefForCStrings AND isDefForC (getScope (func)))
   THEN
      outText (p, ',') ;
      setNeedSpace (p) ;
      doFuncHighC (p, actual) ;
      doTotype (p, actual, formal)
   END
END doFuncUnbounded ;


(*
   doProcedureParamC -
*)

PROCEDURE doProcedureParamC (p: pretty; actual, formal: node) ;
BEGIN
   IF isForC (formal)
   THEN
      outText (p, '(') ;
      doFQNameC (p, getType (formal)) ;
      outText (p, "_C") ;
      outText (p, ')') ;
      setNeedSpace (p) ;
      doExprC (p, actual)
   ELSE
      outText (p, '(') ;
      doTypeNameC (p, getType (formal)) ;
      outText (p, ')') ;
      setNeedSpace (p) ;
      outText (p, '{') ;
      outText (p, '(') ;
      doFQNameC (p, getType (formal)) ;
      outText (p, '_t)') ;
      setNeedSpace (p) ;
      doExprC (p, actual) ;
      outText (p, '}')
    END
END doProcedureParamC ;


(*
   doAdrExprC -
*)

PROCEDURE doAdrExprC (p: pretty; n: node) ;
BEGIN
   IF isDeref (n)
   THEN
      (* (* no point in issuing & ( * n )  *) *)
      doExprC (p, n^.unaryF.arg)
   ELSIF isVar (n) AND n^.varF.isVarParameter
   THEN
      (* (* no point in issuing & ( * n )  *) *)
      doFQNameC (p, n)
   ELSE
      outText (p, '&') ;
      doExprC (p, n)
   END
END doAdrExprC ;


(*
   typePair -
*)

PROCEDURE typePair (a, b, x, y: node) : BOOLEAN ;
BEGIN
   RETURN ((a = x) AND (b = y)) OR ((a = y) AND (b = x))
END typePair ;


(*
   needsCast - return TRUE if the actual type parameter needs to be cast to
               the formal type.
*)

PROCEDURE needsCast (at, ft: node) : BOOLEAN ;
BEGIN
   at := skipType (at) ;
   ft := skipType (ft) ;
   IF (at = nilN) OR (at^.kind = nil) OR
      (at = ft) OR
      typePair (at, ft, cardinalN, wordN) OR
      typePair (at, ft, cardinalN, ztypeN) OR
      typePair (at, ft, integerN, ztypeN) OR
      typePair (at, ft, longcardN, ztypeN) OR
      typePair (at, ft, shortcardN, ztypeN) OR
      typePair (at, ft, longintN, ztypeN) OR
      typePair (at, ft, shortintN, ztypeN) OR
      typePair (at, ft, realN, rtypeN) OR
      typePair (at, ft, longrealN, rtypeN) OR
      typePair (at, ft, shortrealN, rtypeN)
   THEN
      RETURN FALSE
   ELSE
      RETURN TRUE
   END
END needsCast ;


(*
   checkSystemCast - checks to see if we are passing to/from
                     a system generic type (WORD, BYTE, ADDRESS)
                     and if so emit a cast.  It returns the number of
                     open parenthesis.
*)

PROCEDURE checkSystemCast (p: pretty; actual, formal: node) : CARDINAL ;
VAR
   at, ft: node ;
BEGIN
   at := getExprType (actual) ;
   ft := getType (formal) ;
   IF needsCast (at, ft)
   THEN
      IF lang = ansiCP
      THEN
         IF isString (actual) AND (skipType (ft) = addressN)
         THEN
            outText (p, "const_cast<void*> (reinterpret_cast<const void*> (") ;
	    RETURN 2
         ELSIF isPointer (skipType (ft)) OR (skipType (ft) = addressN)
         THEN
            IF actual = nilN
            THEN
               IF isVarParam (formal)
               THEN
                  metaError1 ('NIL is being passed to a VAR parameter {%1DMad}', formal)
               END ;
               (* NULL is compatible with pointers/address.  *)
               RETURN 0
            ELSE
               outText (p, 'reinterpret_cast<') ;
               doTypeNameC (p, ft) ;
               IF isVarParam (formal)
               THEN
                  outText (p, '*')
               END ;
               noSpace (p) ;
               outText (p, '> (')
            END
         ELSE
            outText (p, 'static_cast<') ;
            doTypeNameC (p, ft) ;
            IF isVarParam (formal)
            THEN
               outText (p, '*')
            END ;
            noSpace (p) ;
            outText (p, '> (')
         END ;
         RETURN 1
      ELSE
         outText (p, '(') ;
         doTypeNameC (p, ft) ;
	 IF isVarParam (formal)
         THEN
            outText (p, '*')
         END ;
         noSpace (p) ;
         outText (p, ')') ;
         setNeedSpace (p)
      END
   END ;
   RETURN 0
END checkSystemCast ;


(*
   emitN -
*)

PROCEDURE emitN (p: pretty; a: ARRAY OF CHAR; n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      outText (p, a) ;
      DEC (n)
   END
END emitN ;


(*
   isForC - return true if node n is a varparam, param or procedure
            which was declared inside a definition module for "C".
*)

PROCEDURE isForC (n: node) : BOOLEAN ;
BEGIN
   IF isVarParam (n)
   THEN
      RETURN n^.varparamF.isForC
   ELSIF isParam (n)
   THEN
      RETURN n^.paramF.isForC
   ELSIF isProcedure (n)
   THEN
      RETURN n^.procedureF.isForC
   END ;
   RETURN FALSE
END isForC ;


(*
   isDefForCNode - return TRUE if node n was declared inside a definition module for "C".
*)

PROCEDURE isDefForCNode (n: node) : BOOLEAN ;
VAR
   name: Name ;
BEGIN
   WHILE (n # NIL) AND (NOT (isImp (n) OR isDef (n) OR isModule (n))) DO
      n := getScope (n)
   END ;
   IF (n # NIL) AND isImp (n)
   THEN
      name := getSymName (n) ;
      n := lookupDef (name) ;
   END ;
   RETURN (n # NIL) AND isDef (n) AND isDefForC (n)
END isDefForCNode ;


(*
   doFuncParamC -
*)

PROCEDURE doFuncParamC (p: pretty; actual, formal, func: node) ;
VAR
   ft, at: node ;
   lbr   : CARDINAL ;
BEGIN
   IF formal = NIL
   THEN
      doExprC (p, actual)
   ELSE
      ft := skipType (getType (formal)) ;
      IF isUnbounded (ft)
      THEN
         doFuncUnbounded (p, actual, formal, ft, func)
      ELSE
         IF isAProcType (ft) AND isProcedure (actual)
         THEN
            IF isVarParam (formal)
            THEN
               metaError1 ('{%1MDad} cannot be passed as a VAR parameter', actual)
            ELSE
               doProcedureParamC (p, actual, formal)
            END
         ELSIF (getType (actual) # NIL) AND isProcType (skipType (getType (actual))) AND isAProcType (ft) AND isForC (formal)
         THEN
            IF isVarParam (formal)
            THEN
               metaError2 ('{%1MDad} cannot be passed as a VAR parameter to the definition for C module as the parameter requires a cast to the formal type {%2MDtad}',
                           actual, formal)
            ELSE
               outText (p, '(') ;
               doFQNameC (p, getType (formal)) ;
               outText (p, "_C") ;
               outText (p, ')') ;
               setNeedSpace (p) ;
               doExprC (p, actual) ;
               outText (p, ".proc")
            END
         ELSIF (getType (actual) # NIL) AND isProcType (skipType (getType (actual))) AND (getType (actual) # getType (formal))
         THEN
            IF isVarParam (formal)
            THEN
               metaError2 ('{%1MDad} cannot be passed as a VAR parameter as the parameter requires a cast to the formal type {%2MDtad}',
                           actual, formal)
            ELSE
               doCastC (p, getType (formal), actual)
            END
         ELSE
            lbr := checkSystemCast (p, actual, formal) ;
            IF isVarParam (formal)
            THEN
               doAdrExprC (p, actual)
            ELSE
               doExprC (p, actual)
            END ;
            emitN (p, ")", lbr)
         END
      END
   END
END doFuncParamC ;


(*
   getNthParamType - return the type of parameter, i, in list, l.
                     If the parameter is a vararg NIL is returned.
*)

PROCEDURE getNthParamType (l: Index; i: CARDINAL) : node ;
VAR
   p: node ;
BEGIN
   p := getNthParam (l, i) ;
   IF p # NIL
   THEN
      RETURN getType (p)
   END ;
   RETURN NIL
END getNthParamType ;


(*
   getNthParam - return the parameter, i, in list, l.
                 If the parameter is a vararg NIL is returned.
*)

PROCEDURE getNthParam (l: Index; i: CARDINAL) : node ;
VAR
   p      : node ;
   j, k, h: CARDINAL ;
BEGIN
   IF l # NIL
   THEN
      j := LowIndice (l) ;
      h := HighIndice (l) ;
      WHILE j <= h DO
         p := GetIndice (l, j) ;
         IF isParam (p)
         THEN
            k := identListLen (p^.paramF.namelist)
         ELSIF isVarParam (p)
         THEN
            k := identListLen (p^.varparamF.namelist)
         ELSE
            assert (isVarargs (p)) ;
            RETURN NIL
         END ;
         IF i <= k
         THEN
            RETURN p
         ELSE
            DEC (i, k) ;
            INC (j)
         END
      END
   END ;
   RETURN NIL
END getNthParam ;


(*
   doFuncArgsC -
*)

PROCEDURE doFuncArgsC (p: pretty; s: node; l: Index; needParen: BOOLEAN) ;
VAR
   actual, formal: node ;
   i, n          : CARDINAL ;
BEGIN
   IF needParen
   THEN
      outText (p, "(")
   END ;
   IF s^.funccallF.args # NIL
   THEN
      i := 1 ;
      n := expListLen (s^.funccallF.args) ;
      WHILE i<=n DO
         actual := getExpList (s^.funccallF.args, i) ;
         formal := getNthParam (l, i) ;
         doFuncParamC (p, actual, formal, s^.funccallF.function) ;
         IF i<n
         THEN
            outText (p, ",") ;
            setNeedSpace (p)
         END ;
         INC (i)
      END
   END ;
   IF needParen
   THEN
      noSpace (p) ;
      outText (p, ")")
   END
END doFuncArgsC ;


(*
   doProcTypeArgsC -
*)

PROCEDURE doProcTypeArgsC (p: pretty; s: node; args: Index; needParen: BOOLEAN) ;
VAR
   a, b: node ;
   i, n: CARDINAL ;
BEGIN
   IF needParen
   THEN
      outText (p, "(")
   END ;
   IF s^.funccallF.args # NIL
   THEN
      i := 1 ;
      n := expListLen (s^.funccallF.args) ;
      WHILE i<=n DO
         a := getExpList (s^.funccallF.args, i) ;
         b := GetIndice (args, i) ;
         doFuncParamC (p, a, b, s^.funccallF.function) ;
         IF i<n
         THEN
            outText (p, ",") ;
            setNeedSpace (p)
         END ;
         INC (i)
      END
   END ;
   IF needParen
   THEN
      noSpace (p) ;
      outText (p, ")")
   END
END doProcTypeArgsC ;


(*
   doAdrArgC -
*)

PROCEDURE doAdrArgC (p: pretty; n: node) ;
BEGIN
   IF isDeref (n)
   THEN
      (* & and * cancel each other out.  *)
      doExprC (p, n^.unaryF.arg)
   ELSIF isVar (n) AND (n^.varF.isVarParameter)
   THEN
      (* & and * cancel each other out.  *)
      outTextN (p, getSymName (n))   (* --fixme-- does the caller need to cast it?  *)
   ELSE
      IF isString (n)
      THEN
         IF lang = ansiCP
         THEN
            outText (p, "const_cast<void*> (reinterpret_cast<const void*>") ;
            outText (p, "(") ;
            doExprC (p, n) ;
            outText (p, "))")
         ELSE
            doExprC (p, n)
         END
      ELSE
         outText (p, "&") ;
         doExprC (p, n)
      END
   END
END doAdrArgC ;


(*
   doAdrC -
*)

PROCEDURE doAdrC (p: pretty; n: node) ;
BEGIN
   assert (isUnary (n)) ;
   doAdrArgC (p, n^.unaryF.arg)
END doAdrC ;


(*
   doInc -
*)

PROCEDURE doInc (p: pretty; n: node) ;
BEGIN
   assert (isIntrinsic (n)) ;
   IF lang = ansiCP
   THEN
      doIncDecCP (p, n, "+")
   ELSE
      doIncDecC (p, n, "+=")
   END
END doInc ;


(*
   doDec -
*)

PROCEDURE doDec (p: pretty; n: node) ;
BEGIN
   assert (isIntrinsic (n)) ;
   IF lang = ansiCP
   THEN
      doIncDecCP (p, n, "-")
   ELSE
      doIncDecC (p, n, "-=")
   END
END doDec ;


(*
   doIncDecC -
*)

PROCEDURE doIncDecC (p: pretty; n: node; op: ARRAY OF CHAR) ;
BEGIN
   assert (isIntrinsic (n)) ;
   IF n^.intrinsicF.args # NIL
   THEN
      doExprC (p, getExpList (n^.intrinsicF.args, 1)) ;
      setNeedSpace (p) ;
      outText (p, op) ;
      setNeedSpace (p) ;
      IF expListLen (n^.intrinsicF.args) = 1
      THEN
         outText (p, '1')
      ELSE
         doExprC (p, getExpList (n^.intrinsicF.args, 2))
      END
   END
END doIncDecC ;


(*
   doIncDecCP -
*)

PROCEDURE doIncDecCP (p: pretty; n: node; op: ARRAY OF CHAR) ;
VAR
   lhs,
   type: node ;
BEGIN
   assert (isIntrinsic (n)) ;
   IF n^.intrinsicF.args # NIL
   THEN
      lhs := getExpList (n^.intrinsicF.args, 1) ;
      doExprC (p, lhs) ;
      setNeedSpace (p) ;
      type := getType (lhs) ;
      IF isPointer (type) OR (type = addressN)
      THEN
         (* cast to (char * ) and then back again after the arithmetic is complete.  *)
         outText (p, "=") ;
	 setNeedSpace (p) ;
         outText (p, 'reinterpret_cast<') ;
         doTypeNameC (p, type) ;
         noSpace (p) ;
         outText (p, '> (reinterpret_cast<char *> (') ;
         doExprC (p, lhs) ;
         noSpace (p) ;
         outText (p, ')') ;
         outText (p, op) ;
         IF expListLen (n^.intrinsicF.args) = 1
         THEN
            outText (p, '1')
         ELSE
            doExprC (p, getExpList (n^.intrinsicF.args, 2))
         END ;
         outText (p, ')')
      ELSIF isEnumeration (skipType (type))
      THEN
         outText (p, "= static_cast<") ;
         doTypeNameC (p, type) ;
         noSpace (p) ;
         outText (p, ">(static_cast<int>(") ;
         doExprC (p, lhs) ;
         outText (p, ")") ;
         outText (p, op) ;
         IF expListLen (n^.intrinsicF.args) = 1
         THEN
            outText (p, '1')
         ELSE
            doExprC (p, getExpList (n^.intrinsicF.args, 2))
         END ;
         outText (p, ")")
      ELSE
         outText (p, op) ;
         outText (p, "=") ;
         setNeedSpace (p) ;
         IF expListLen (n^.intrinsicF.args) = 1
         THEN
            outText (p, '1')
         ELSE
            doExprC (p, getExpList (n^.intrinsicF.args, 2))
         END
      END
   END
END doIncDecCP ;


(*
   doInclC -
*)

PROCEDURE doInclC (p: pretty; n: node) ;
VAR
   lo: node ;
BEGIN
   assert (isIntrinsic (n)) ;
   IF n^.intrinsicF.args # NIL
   THEN
      IF expListLen (n^.intrinsicF.args) = 2
      THEN
         doExprC (p, getExpList (n^.intrinsicF.args, 1)) ;
         lo := getSetLow (getExpList (n^.intrinsicF.args, 1)) ;
         setNeedSpace (p) ;
         outText (p, '|=') ;
         setNeedSpace (p) ;
         outText (p, '(1') ;
         setNeedSpace (p) ;
         outText (p, '<<') ;
         setNeedSpace (p) ;
         outText (p, '(') ;
         doExprC (p, getExpList (n^.intrinsicF.args, 2)) ;
         doSubtractC (p, lo) ;
         setNeedSpace (p) ;
         outText (p, '))')
      ELSE
         HALT (* metaError0 ('expecting two parameters to INCL') *)
      END
   END
END doInclC ;


(*
   doExclC -
*)

PROCEDURE doExclC (p: pretty; n: node) ;
VAR
   lo: node ;
BEGIN
   assert (isIntrinsic (n)) ;
   IF n^.intrinsicF.args # NIL
   THEN
      IF expListLen (n^.intrinsicF.args) = 2
      THEN
         doExprC (p, getExpList (n^.intrinsicF.args, 1)) ;
         lo := getSetLow (getExpList (n^.intrinsicF.args, 1)) ;
         setNeedSpace (p) ;
         outText (p, '&=') ;
         setNeedSpace (p) ;
         outText (p, '(~(1') ;
         setNeedSpace (p) ;
         outText (p, '<<') ;
         setNeedSpace (p) ;
         outText (p, '(') ;
         doExprC (p, getExpList (n^.intrinsicF.args, 2)) ;
         doSubtractC (p, lo) ;
         setNeedSpace (p) ;
         outText (p, ')))')
      ELSE
         HALT (* metaError0 ('expecting two parameters to EXCL') *)
      END
   END
END doExclC ;


(*
   doNewC -
*)

PROCEDURE doNewC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (isIntrinsic (n)) ;
   IF n^.intrinsicF.args = NIL
   THEN
      HALT
   ELSE
      IF expListLen (n^.intrinsicF.args) = 1
      THEN
         keyc.useStorage ;
         outText (p, 'Storage_ALLOCATE') ;
         setNeedSpace (p) ;
         outText (p, '((void **)') ;
         setNeedSpace (p) ;
         outText (p, '&') ;
         doExprC (p, getExpList (n^.intrinsicF.args, 1)) ;
         outText (p, ',') ;
         setNeedSpace (p) ;
         t := skipType (getType (getExpList (n^.intrinsicF.args, 1))) ;
         IF isPointer (t)
         THEN
            t := getType (t) ;
            outText (p, 'sizeof') ;
            setNeedSpace (p) ;
            outText (p, '(') ;
            doTypeNameC (p, t) ;
            noSpace (p) ;
            outText (p, '))')
         ELSE
            metaError1 ('expecting a pointer type variable as the argument to NEW, rather than {%1ad}', t)
         END
      END
   END
END doNewC ;


(*
   doDisposeC -
*)

PROCEDURE doDisposeC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (isIntrinsic (n)) ;
   IF n^.intrinsicF.args = NIL
   THEN
      HALT
   ELSE
      IF expListLen (n^.intrinsicF.args) = 1
      THEN
         keyc.useStorage ;
         outText (p, 'Storage_DEALLOCATE') ;
         setNeedSpace (p) ;
         outText (p, '((void **)') ;
         setNeedSpace (p) ;
         outText (p, '&') ;
         doExprC (p, getExpList (n^.intrinsicF.args, 1)) ;
         outText (p, ',') ;
         setNeedSpace (p) ;
         t := skipType (getType (getExpList (n^.intrinsicF.args, 1))) ;
         IF isPointer (t)
         THEN
            t := getType (t) ;
            outText (p, 'sizeof') ;
            setNeedSpace (p) ;
            outText (p, '(') ;
            doTypeNameC (p, t) ;
            noSpace (p) ;
            outText (p, '))')
         ELSE
            metaError1 ('expecting a pointer type variable as the argument to DISPOSE, rather than {%1ad}', t)
         END
      ELSE
         HALT (* metaError0 ('expecting a single parameter to DISPOSE') *)
      END
   END
END doDisposeC ;


(*
   doCapC -
*)

PROCEDURE doCapC (p: pretty; n: node) ;
BEGIN
   assert (isUnary (n)) ;
   IF n^.unaryF.arg = NIL
   THEN
      HALT (* metaError0 ('expecting a single parameter to CAP') *)
   ELSE
      keyc.useCtype ;
      IF getGccConfigSystem ()
      THEN
         outText (p, 'TOUPPER')
      ELSE
         outText (p, 'toupper')
      END ;
      setNeedSpace (p) ;
      outText (p, '(') ;
      doExprC (p, n^.unaryF.arg) ;
      outText (p, ')')
   END
END doCapC ;


(*
   doLengthC -
*)

PROCEDURE doLengthC (p: pretty; n: node) ;
BEGIN
   assert (isUnary (n)) ;
   IF n^.unaryF.arg = NIL
   THEN
      HALT (* metaError0 ('expecting a single parameter to LENGTH') *)
   ELSE
      keyc.useM2RTS ;
      outText (p, 'M2RTS_Length') ;
      setNeedSpace (p) ;
      outText (p, '(') ;
      doExprC (p, n^.unaryF.arg) ;
      outText (p, ',') ;
      setNeedSpace (p) ;
      doFuncHighC (p, n^.unaryF.arg) ;
      outText (p, ')')
   END
END doLengthC ;


(*
   doAbsC -
*)

PROCEDURE doAbsC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (isUnary (n)) ;
   IF n^.unaryF.arg = NIL
   THEN
      HALT
   ELSE
      t := getExprType (n)
   END ;
   IF t = longintN
   THEN
      keyc.useLabs ;
      outText (p, "labs")
   ELSIF t = integerN
   THEN
      keyc.useAbs ;
      outText (p, "abs")
   ELSIF t = realN
   THEN
      keyc.useFabs ;
      outText (p, "fabs")
   ELSIF t = longrealN
   THEN
      keyc.useFabsl ;
      outText (p, "fabsl")
   ELSIF t = cardinalN
   THEN
      (* do nothing.  *)
   ELSE
      HALT
   END ;
   setNeedSpace (p) ;
   outText (p, "(") ;
   doExprC (p, n^.unaryF.arg) ;
   outText (p, ")")
END doAbsC ;


(*
   doValC -
*)

PROCEDURE doValC (p: pretty; n: node) ;
BEGIN
   assert (isBinary (n)) ;
   outText (p, '(') ;
   doTypeNameC (p, n^.binaryF.left) ;
   outText (p, ')') ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   doExprC (p, n^.binaryF.right) ;
   outText (p, ')')
END doValC ;


(*
   doMinC -
*)

PROCEDURE doMinC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (isUnary (n)) ;
   t := getExprType (n^.unaryF.arg) ;
   doExprC (p, getMin (t)) ;
END doMinC ;


(*
   doMaxC -
*)

PROCEDURE doMaxC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (isUnary (n)) ;
   t := getExprType (n^.unaryF.arg) ;
   doExprC (p, getMax (t)) ;
END doMaxC ;


(*
   isIntrinsic - returns if, n, is an intrinsic procedure.
                 The intrinsic functions are represented as unary and binary nodes.
*)

PROCEDURE isIntrinsic (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   unreachable,
   throw,
   inc,
   dec,
   incl,
   excl,
   new,
   dispose,
   halt   :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isIntrinsic ;


(*
   doHalt -
*)

PROCEDURE doHalt (p: pretty; n: node) ;
BEGIN
   assert (n^.kind = halt) ;
   IF (n^.intrinsicF.args = NIL) OR (expListLen (n^.intrinsicF.args) = 0)
   THEN
      outText (p, 'M2RTS_HALT') ;
      setNeedSpace (p) ;
      outText (p, '(-1)')
   ELSIF expListLen (n^.intrinsicF.args) = 1
   THEN
      outText (p, 'M2RTS_HALT') ;
      setNeedSpace (p) ;
      outText (p, '(') ;
      doExprC (p, getExpList (n^.intrinsicF.args, 1)) ;
      outText (p, ')')
   END
END doHalt ;


(*
   doCreal - emit the appropriate creal function.
*)

PROCEDURE doCreal (p: pretty; t: node) ;
BEGIN
   CASE t^.kind OF

   complex     :  keyc.useComplex ;
                  outText (p, "creal") |
   longcomplex :  keyc.useComplex ;
                  outText (p, "creall") |
   shortcomplex:  keyc.useComplex ;
                  outText (p, "crealf")

   END
END doCreal ;


(*
   doCimag - emit the appropriate cimag function.
*)

PROCEDURE doCimag (p: pretty; t: node) ;
BEGIN
   CASE t^.kind OF

   complex     :  keyc.useComplex ;
                  outText (p, "cimag") |
   longcomplex :  keyc.useComplex ;
                  outText (p, "cimagl") |
   shortcomplex:  keyc.useComplex ;
                  outText (p, "cimagf")

   END
END doCimag ;


(*
   doReC -
*)

PROCEDURE doReC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (n^.kind = re) ;
   IF n^.unaryF.arg # NIL
   THEN
      t := getExprType (n^.unaryF.arg)
   ELSE
      HALT
   END ;
   doCreal (p, t) ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   doExprC (p, n^.unaryF.arg) ;
   outText (p, ')')
END doReC ;


(*
   doImC -
*)

PROCEDURE doImC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (n^.kind = im) ;
   IF n^.unaryF.arg # NIL
   THEN
      t := getExprType (n^.unaryF.arg)
   ELSE
      HALT
   END ;
   doCimag (p, t) ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   doExprC (p, n^.unaryF.arg) ;
   outText (p, ')')
END doImC ;


(*
   doCmplx -
*)

PROCEDURE doCmplx (p: pretty; n: node) ;
BEGIN
   assert (isBinary (n)) ;
   keyc.useComplex ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   doExprC (p, n^.binaryF.left) ;
   outText (p, ')') ;
   setNeedSpace (p) ;
   outText (p, '+') ;
   setNeedSpace (p) ;
   outText (p, '(') ;
   doExprC (p, n^.binaryF.right) ;
   setNeedSpace (p) ;
   outText (p, '*') ;
   setNeedSpace (p) ;
   outText (p, 'I') ;
   outText (p, ')')
END doCmplx ;


(*
   doIntrinsicC -
*)

PROCEDURE doIntrinsicC (p: pretty; n: node) ;
BEGIN
   assert (isIntrinsic (n)) ;
   doCommentC (p, n^.intrinsicF.intrinsicComment.body) ;
   CASE n^.kind OF

   unreachable: doUnreachableC (p, n) |
   throw      : doThrowC (p, n) |
   halt       : doHalt (p, n) |
   inc        : doInc (p, n) |
   dec        : doDec (p, n) |
   incl       : doInclC (p, n) |
   excl       : doExclC (p, n) |
   new        : doNewC (p, n) |
   dispose    : doDisposeC (p, n)

   END ;
   outText (p, ";") ;
   doAfterCommentC (p, n^.intrinsicF.intrinsicComment.after)
END doIntrinsicC ;


(*
   isIntrinsicFunction - returns true if, n, is an instrinsic function.
*)

PROCEDURE isIntrinsicFunction (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   val,
   adr,
   size,
   tsize,
   float,
   trunc,
   ord,
   chr,
   cap,
   abs,
   high,
   length,
   min,
   max,
   re,
   im,
   cmplx:  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isIntrinsicFunction ;


(*
   doSizeC -
*)

PROCEDURE doSizeC (p: pretty; n: node) ;
BEGIN
   assert (isUnary (n)) ;
   outText (p, "sizeof (") ;
   doExprC (p, n^.unaryF.arg) ;
   outText (p, ")")
END doSizeC ;


(*
   doConvertC -
*)

PROCEDURE doConvertC (p: pretty; n: node; conversion: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := InitString (conversion) ;
   doConvertSC (p, n, s) ;
   s := KillString (s)
END doConvertC ;


(*
   doConvertSC -
*)

PROCEDURE doConvertSC (p: pretty; n: node; conversion: String) ;
BEGIN
   assert (isUnary (n)) ;
   setNeedSpace (p) ;
   outText (p, "((") ;
   outTextS (p, conversion) ;
   outText (p, ")") ;
   setNeedSpace (p) ;
   outText (p, "(") ;
   doExprC (p, n^.unaryF.arg) ;
   outText (p, "))")
END doConvertSC ;


(*  not needed?
   val:     doValC (p, n) |
   adr:     doAdrC (p, n) |
   size,
   tsize:   doSizeC (p, n) |
   float:   doConvertC (p, n, "(double)") |
   trunc:   doConvertC (p, n, "(int)") |
   ord:     doConvertC (p, n, "(unsigned int)") |
   chr:     doConvertC (p, n, "(char)") |
   cap:     doCapC (p, n) |
   abs:     doAbsC (p, n) |
   high:    doFuncHighC (p, n^.unaryF.arg, 1)) |
   length:  doLengthC (p, n) |
   min:     doMinC (p, n) |
   max:     doMaxC (p, n) |
   throw:   doThrowC (p, n) |
   re:      doReC (p, n) |
   im:      doImC (p, n) |
   cmplx:   doCmplx (p, n)
*)


(*
   getFuncFromExpr -
*)

PROCEDURE getFuncFromExpr (n: node) : node ;
BEGIN
   n := skipType (getType (n)) ;
   WHILE (n # procN) AND (NOT isProcType (n)) DO
      n := skipType (getType (n))
   END ;
   RETURN n
END getFuncFromExpr ;


(*
   doFuncExprC -
*)

PROCEDURE doFuncExprC (p: pretty; n: node) ;
VAR
   t: node ;
BEGIN
   assert (isFuncCall (n)) ;
   IF isProcedure (n^.funccallF.function)
   THEN
      doFQDNameC (p, n^.funccallF.function, TRUE) ;
      setNeedSpace (p) ;
      doFuncArgsC (p, n, n^.funccallF.function^.procedureF.parameters, TRUE)
   ELSE
      outText (p, "(*") ;
      doExprC (p, n^.funccallF.function) ;
      outText (p, ".proc") ;
      outText (p, ")") ;
      t := getFuncFromExpr (n^.funccallF.function) ;
      setNeedSpace (p) ;
      IF t = procN
      THEN
         doProcTypeArgsC (p, n, NIL, TRUE)
      ELSE
         assert (isProcType (t)) ;
         doProcTypeArgsC (p, n, t^.proctypeF.parameters, TRUE)
      END
   END
END doFuncExprC ;


(*
   doFuncCallC -
*)

PROCEDURE doFuncCallC (p: pretty; n: node) ;
BEGIN
   doCommentC (p, n^.funccallF.funccallComment.body) ;
   doFuncExprC (p, n) ;
   outText (p, ";") ;
   doAfterCommentC (p, n^.funccallF.funccallComment.after)
END doFuncCallC ;


(*
   doCaseStatementC -
*)

PROCEDURE doCaseStatementC (p: pretty; n: node; needBreak: BOOLEAN) ;
BEGIN
   p := pushPretty (p) ;
   setindent (p, getindent (p) + indentationC) ;
   doStatementSequenceC (p, n) ;
   IF needBreak
   THEN
      outText (p, "break;\n")
   END ;
   p := popPretty (p)
END doCaseStatementC ;


(*
   doExceptionC -
*)

PROCEDURE doExceptionC (p: pretty; a: ARRAY OF CHAR; n: node) ;
VAR
   w: CARDINAL ;
BEGIN
   w := getDeclaredMod (n) ;
   outText (p, a) ;
   setNeedSpace (p) ;
   outText (p, '("') ;
   outTextS (p, findFileNameFromToken (w, 0)) ;
   outText (p, '",') ;
   setNeedSpace (p) ;
   outCard (p, tokenToLineNo (w, 0)) ;
   outText (p, ',') ;
   setNeedSpace (p) ;
   outCard (p, tokenToColumnNo (w, 0)) ;
   outText (p, ');\n') ;
   outText (p, '__builtin_unreachable ();\n')
END doExceptionC ;


(*
   doExceptionCP -
*)

PROCEDURE doExceptionCP (p: pretty; a: ARRAY OF CHAR; n: node) ;
VAR
   w: CARDINAL ;
BEGIN
   w := getDeclaredMod (n) ;
   outText (p, a) ;
   setNeedSpace (p) ;
   outText (p, '("') ;
   outTextS (p, findFileNameFromToken (w, 0)) ;
   outText (p, '",') ;
   setNeedSpace (p) ;
   outCard (p, tokenToLineNo (w, 0)) ;
   outText (p, ',') ;
   setNeedSpace (p) ;
   outCard (p, tokenToColumnNo (w, 0)) ;
   outText (p, ');\n') ;
   outText (p, '__builtin_unreachable ();\n')
END doExceptionCP ;


(*
   doException -
*)

PROCEDURE doException (p: pretty; a: ARRAY OF CHAR; n: node) ;
BEGIN
   keyc.useException ;
   IF lang = ansiCP
   THEN
      doExceptionCP (p, a, n)
   ELSE
      doExceptionC (p, a, n)
   END
END doException ;


(*
   doRangeListC -
*)

PROCEDURE doRangeListC (p: pretty; c: node) ;
VAR
   r   : node ;
   i, h: CARDINAL ;
BEGIN
   assert (isCaseList (c)) ;
   i := 1 ;
   h := HighIndice (c^.caselistF.rangePairs) ;
   WHILE i<=h DO
      r := GetIndice (c^.caselistF.rangePairs, i) ;
      assert ((r^.rangeF.hi = NIL) OR (r^.rangeF.lo = r^.rangeF.hi)) ;
      outText (p, "case") ;
      setNeedSpace (p) ;
      doExprC (p, r^.rangeF.lo) ;
      outText (p, ":\n") ;
      INC (i)
   END
END doRangeListC ;


(*
   doRangeIfListC -
*)

PROCEDURE doRangeIfListC (p: pretty; e, c: node) ;
VAR
   r   : node ;
   i, h: CARDINAL ;
BEGIN
   assert (isCaseList (c)) ;
   i := 1 ;
   h := HighIndice (c^.caselistF.rangePairs) ;
   WHILE i<=h DO
      r := GetIndice (c^.caselistF.rangePairs, i) ;
      IF (r^.rangeF.lo # r^.rangeF.hi) AND (r^.rangeF.hi # NIL)
      THEN
         outText (p, "((") ;
         doExprC (p, e) ;
         outText (p, ")") ;
         setNeedSpace (p) ;
         outText (p, ">=") ;
         setNeedSpace (p) ;
         doExprC (p, r^.rangeF.lo) ;
         outText (p, ")") ;
         setNeedSpace (p) ;
         outText (p, "&&") ;
         setNeedSpace (p) ;
         outText (p, "((") ;
         doExprC (p, e) ;
         outText (p, ")") ;
         setNeedSpace (p) ;
         outText (p, "<=") ;
         setNeedSpace (p) ;
         doExprC (p, r^.rangeF.hi) ;
         outText (p, ")")
      ELSE
         outText (p, "((") ;
         doExprC (p, e) ;
         outText (p, ")") ;
         setNeedSpace (p) ;
         outText (p, "==") ;
         setNeedSpace (p) ;
         doExprC (p, r^.rangeF.lo) ;
         outText (p, ")")
      END ;
      IF i<h
      THEN
         setNeedSpace (p) ;
         outText (p, "||") ;
         setNeedSpace (p)
      END ;
      INC (i)
   END
END doRangeIfListC ;


(*
   doCaseLabels -
*)

PROCEDURE doCaseLabels (p: pretty; n: node; needBreak: BOOLEAN) ;
BEGIN
   assert (isCaseLabelList (n)) ;
   doRangeListC (p, n^.caselabellistF.caseList) ;
   p := pushPretty (p) ;
   setindent (p, getindent (p) + indentationC) ;
   doStatementSequenceC (p, n^.caselabellistF.statements) ;
   IF needBreak
   THEN
      outText (p, "break;\n\n")
   END ;
   p := popPretty (p)
END doCaseLabels ;


(*
   doCaseLabelListC -
*)

PROCEDURE doCaseLabelListC (p: pretty; n: node; haveElse: BOOLEAN) ;
VAR
   i, h: CARDINAL ;
   c   : node ;
BEGIN
   assert (isCase (n)) ;
   i := 1 ;
   h := HighIndice (n^.caseF.caseLabelList) ;
   WHILE i<=h DO
      c := GetIndice (n^.caseF.caseLabelList, i) ;
      doCaseLabels (p, c, (i<h) OR haveElse OR caseException) ;
      INC (i)
   END
END doCaseLabelListC ;


(*
   doCaseIfLabels -
*)

PROCEDURE doCaseIfLabels (p: pretty; e, n: node;
                          i, h: CARDINAL) ;
BEGIN
   assert (isCaseLabelList (n)) ;
   IF i > 1
   THEN
      outText (p, "else") ;
      setNeedSpace (p) ;
   END ;
   outText (p, "if") ;
   setNeedSpace (p) ;
   outText (p, "(") ;
   doRangeIfListC (p, e, n^.caselabellistF.caseList) ;
   outText (p, ")\n") ;
   IF h = 1
   THEN
      doCompoundStmt (p, n^.caselabellistF.statements)
   ELSE
      outText (p, "{\n") ;
      doStatementSequenceC (p, n^.caselabellistF.statements) ;
      outText (p, "}\n")
   END
END doCaseIfLabels ;


(*
   doCaseIfLabelListC -
*)

PROCEDURE doCaseIfLabelListC (p: pretty; n: node) ;
VAR
   i, h: CARDINAL ;
   c   : node ;
BEGIN
   assert (isCase (n)) ;
   i := 1 ;
   h := HighIndice (n^.caseF.caseLabelList) ;
   WHILE i<=h DO
      c := GetIndice (n^.caseF.caseLabelList, i) ;
      doCaseIfLabels (p, n^.caseF.expression, c, i, h) ;
      INC (i)
   END
END doCaseIfLabelListC ;


(*
   doCaseElseC -
*)

PROCEDURE doCaseElseC (p: pretty; n: node) ;
BEGIN
   assert (isCase (n)) ;
   IF n^.caseF.else = NIL
   THEN
      IF caseException
      THEN
         outText (p, "\ndefault:\n") ;
         p := pushPretty (p) ;
         setindent (p, getindent (p) + indentationC) ;
         doException (p, 'CaseException', n) ;
         p := popPretty (p)
      END
   ELSE
      outText (p, "\ndefault:\n") ;
      doCaseStatementC (p, n^.caseF.else, TRUE)
   END
END doCaseElseC ;


(*
   doCaseIfElseC -
*)

PROCEDURE doCaseIfElseC (p: pretty; n: node) ;
BEGIN
   assert (isCase (n)) ;
   IF n^.caseF.else = NIL
   THEN
      IF TRUE
      THEN
         outText (p, "\n") ;
         outText (p, "else {\n") ;
         p := pushPretty (p) ;
         setindent (p, getindent (p) + indentationC) ;
         doException (p, 'CaseException', n) ;
         p := popPretty (p) ;
         outText (p, "}\n")
      END
   ELSE
      outText (p, "\n") ;
      outText (p, "else {\n") ;
      doCaseStatementC (p, n^.caseF.else, FALSE) ;
      outText (p, "}\n")
   END
END doCaseIfElseC ;


(*
   canUseSwitchCaseLabels - returns TRUE if all the case labels are
                            single values and not ranges.
*)

PROCEDURE canUseSwitchCaseLabels (n: node) : BOOLEAN ;
VAR
   i, h: CARDINAL ;
   r, l: node ;
BEGIN
   assert (isCaseLabelList (n)) ;
   l := n^.caselabellistF.caseList ;
   i := 1 ;
   h := HighIndice (l^.caselistF.rangePairs) ;
   WHILE i<=h DO
      r := GetIndice (l^.caselistF.rangePairs, i) ;
      IF (r^.rangeF.hi # NIL) AND (r^.rangeF.lo # r^.rangeF.hi)
      THEN
         RETURN FALSE
      END ;
      INC (i)
   END ;
   RETURN TRUE
END canUseSwitchCaseLabels ;


(*
   canUseSwitch - returns TRUE if the case statement can be implement
                  by a switch statement.  This will be TRUE if all case
                  selectors are single values rather than ranges.
*)

PROCEDURE canUseSwitch (n: node) : BOOLEAN ;
VAR
   i, h: CARDINAL ;
   c   : node ;
BEGIN
   assert (isCase (n)) ;
   i := 1 ;
   h := HighIndice (n^.caseF.caseLabelList) ;
   WHILE i<=h DO
      c := GetIndice (n^.caseF.caseLabelList, i) ;
      IF NOT canUseSwitchCaseLabels (c)
      THEN
         RETURN FALSE
      END ;
      INC (i)
   END ;
   RETURN TRUE
END canUseSwitch ;


(*
   doCaseC -
*)

PROCEDURE doCaseC (p: pretty; n: node) ;
VAR
   i: CARDINAL ;
BEGIN
   assert (isCase (n)) ;
   IF canUseSwitch (n)
   THEN
      i := getindent (p) ;
      outText (p, "switch") ;
      setNeedSpace (p) ;
      outText (p, "(") ;
      doExprC (p, n^.caseF.expression) ;
      p := pushPretty (p) ;
      outText (p, ")") ;
      setindent (p, i + indentationC) ;
      outText (p, "\n{\n") ;
      p := pushPretty (p) ;
      setindent (p, getindent (p) + indentationC) ;
      doCaseLabelListC (p, n, n^.caseF.else # NIL) ;
      doCaseElseC (p, n) ;
      p := popPretty (p) ;
      outText (p, "}\n") ;
      p := popPretty (p)
   ELSE
      doCaseIfLabelListC (p, n) ;
      doCaseIfElseC (p, n)
   END
END doCaseC ;


(*
   doLoopC -
*)

PROCEDURE doLoopC (p: pretty; s: node) ;
BEGIN
   assert (isLoop (s)) ;
   outText (p, 'for (;;)\n') ;
   outText (p, "{\n") ;
   p := pushPretty (p) ;
   setindent (p, getindent (p) + indentationC) ;
   doStatementSequenceC (p, s^.loopF.statements) ;
   p := popPretty (p) ;
   outText (p, "}\n")
END doLoopC ;


(*
   doExitC -
*)

PROCEDURE doExitC (p: pretty; s: node) ;
BEGIN
   assert (isExit (s)) ;
   outText (p, "/* exit.  */\n")
END doExitC ;


(*
   doStatementsC -
*)

PROCEDURE doStatementsC (p: pretty; s: node) ;
BEGIN
   IF s = NIL
   THEN
      (* do nothing.  *)
   ELSIF isStatementSequence (s)
   THEN
      doStatementSequenceC (p, s)
   ELSIF isComment (s)
   THEN
      doCommentC (p, s)
   ELSIF isExit (s)
   THEN
      doExitC (p, s)
   ELSIF isReturn (s)
   THEN
      doReturnC (p, s)
   ELSIF isAssignment (s)
   THEN
      doAssignmentC (p, s)
   ELSIF isIf (s)
   THEN
      doIfC (p, s)
   ELSIF isFor (s)
   THEN
      doForC (p, s)
   ELSIF isRepeat (s)
   THEN
      doRepeatC (p, s)
   ELSIF isWhile (s)
   THEN
      doWhileC (p, s)
   ELSIF isIntrinsic (s)
   THEN
      doIntrinsicC (p, s)
   ELSIF isFuncCall (s)
   THEN
      doFuncCallC (p, s)
   ELSIF isCase (s)
   THEN
      doCaseC (p, s)
   ELSIF isLoop (s)
   THEN
      doLoopC (p, s)
   ELSIF isExit (s)
   THEN
      doExitC (p, s)
   ELSE
      HALT  (* need to handle another s^.kind.  *)
   END
END doStatementsC ;


PROCEDURE stop ; END stop ;

(*
   doLocalVarC -
*)

PROCEDURE doLocalVarC (p: pretty; s: scopeT) ;
BEGIN
   includeVarProcedure (s) ;
   debugLists ;

   topologicallyOut (doConstC, doTypesC, doVarC,
                     outputPartial,
                     doNone, doCompletePartialC, doNone)
END doLocalVarC ;


(*
   doLocalConstTypesC -
*)

PROCEDURE doLocalConstTypesC (p: pretty; s: scopeT) ;
BEGIN
   simplifyTypes (s) ;
   includeConstType (s) ;

   doP := p ;

   topologicallyOut (doConstC, doTypesC, doVarC,
                     outputPartial,
                     doNone, doCompletePartialC, doNone) ;

END doLocalConstTypesC ;


(*
   addParamDone -
*)

PROCEDURE addParamDone (n: node) ;
BEGIN
   IF isVar (n) AND n^.varF.isParameter
   THEN
      addDone (n) ;
      addDone (getType (n))
   END
END addParamDone ;


(*
   includeParameters -
*)

PROCEDURE includeParameters (n: node) ;
BEGIN
   assert (isProcedure (n)) ;
   ForeachIndiceInIndexDo (n^.procedureF.decls.variables, addParamDone)
END includeParameters ;


(*
   isHalt -
*)

PROCEDURE isHalt (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = halt
END isHalt ;


(*
   isReturnOrHalt -
*)

PROCEDURE isReturnOrHalt (n: node) : BOOLEAN ;
BEGIN
   RETURN isHalt (n) OR isReturn (n)
END isReturnOrHalt ;


(*
   isLastStatementReturn -
*)

PROCEDURE isLastStatementReturn (n: node) : BOOLEAN ;
BEGIN
   RETURN isLastStatement (n, isReturnOrHalt)
END isLastStatementReturn ;


(*
   isLastStatementSequence -
*)

PROCEDURE isLastStatementSequence (n: node; q: isNodeF) : BOOLEAN ;
VAR
   h  : CARDINAL ;
BEGIN
   assert (isStatementSequence (n)) ;
   h := HighIndice (n^.stmtF.statements) ;
   IF h > 0
   THEN
      RETURN isLastStatement (GetIndice (n^.stmtF.statements, h), q)
   END ;
   RETURN FALSE
END isLastStatementSequence ;


(*
   isLastStatementIf -
*)

PROCEDURE isLastStatementIf (n: node; q: isNodeF) : BOOLEAN ;
VAR
   ret: BOOLEAN ;
BEGIN
   assert (isIf (n)) ;
   ret := TRUE ;
   IF (n^.ifF.elsif # NIL) AND ret
   THEN
      ret := isLastStatement (n^.ifF.elsif, q)
   END ;
   IF (n^.ifF.then # NIL) AND ret
   THEN
      ret := isLastStatement (n^.ifF.then, q)
   END ;
   IF (n^.ifF.else # NIL) AND ret
   THEN
      ret := isLastStatement (n^.ifF.else, q)
   END ;
   RETURN ret
END isLastStatementIf ;


(*
   isLastStatementElsif -
*)

PROCEDURE isLastStatementElsif (n: node; q: isNodeF) : BOOLEAN ;
VAR
   ret: BOOLEAN ;
BEGIN
   assert (isElsif (n)) ;
   ret := TRUE ;
   IF (n^.elsifF.elsif # NIL) AND ret
   THEN
      ret := isLastStatement (n^.elsifF.elsif, q)
   END ;
   IF (n^.elsifF.then # NIL) AND ret
   THEN
      ret := isLastStatement (n^.elsifF.then, q)
   END ;
   IF (n^.elsifF.else # NIL) AND ret
   THEN
      ret := isLastStatement (n^.elsifF.else, q)
   END ;
   RETURN ret
END isLastStatementElsif ;


(*
   isLastStatementCase -
*)

PROCEDURE isLastStatementCase (n: node; q: isNodeF) : BOOLEAN ;
VAR
   ret : BOOLEAN ;
   i, h: CARDINAL ;
   c   : node ;
BEGIN
   ret := TRUE ;
   assert (isCase (n)) ;
   i := 1 ;
   h := HighIndice (n^.caseF.caseLabelList) ;
   WHILE i<=h DO
      c := GetIndice (n^.caseF.caseLabelList, i) ;
      assert (isCaseLabelList (c)) ;
      ret := ret AND isLastStatement (c^.caselabellistF.statements, q) ;
      INC (i)
   END ;
   IF n^.caseF.else # NIL
   THEN
      ret := ret AND isLastStatement (n^.caseF.else, q)
   END ;
   RETURN ret
END isLastStatementCase ;


(*
   isLastStatement - returns TRUE if the last statement in, n, is, q.
*)

PROCEDURE isLastStatement (n: node; q: isNodeF) : BOOLEAN ;
VAR
   ret: BOOLEAN ;
BEGIN
   IF n = NIL
   THEN
      RETURN FALSE
   ELSIF isStatementSequence (n)
   THEN
      RETURN isLastStatementSequence (n, q)
   ELSIF isProcedure (n)
   THEN
      assert (isProcedure (n)) ;
      RETURN isLastStatement (n^.procedureF.beginStatements, q)
   ELSIF isIf (n)
   THEN
      RETURN isLastStatementIf (n, q)
   ELSIF isElsif (n)
   THEN
      RETURN isLastStatementElsif (n, q)
   ELSIF isCase (n)
   THEN
      RETURN isLastStatementCase (n, q)
   ELSIF q (n)
   THEN
      RETURN TRUE
   END ;
   RETURN FALSE
END isLastStatement ;


(*
   doProcedureC -
*)

PROCEDURE doProcedureC (n: node) ;
VAR
   s: CARDINAL ;
BEGIN
   outText (doP, "\n") ;
   includeParameters (n) ;

   keyc.enterScope (n) ;

   doProcedureHeadingC (n, FALSE) ;
   outText (doP, "\n") ;
   doP := outKc (doP, "{\n") ;
   s := getcurline (doP) ;
   doLocalConstTypesC (doP, n^.procedureF.decls) ;
   doLocalVarC (doP, n^.procedureF.decls) ;
   doUnboundedParamCopyC (doP, n) ;

   IF s # getcurline (doP)
   THEN
      outText (doP, "\n")
   END ;

   doStatementsC (doP, n^.procedureF.beginStatements) ;
   IF n^.procedureF.returnType # NIL
   THEN
      IF returnException
      THEN
         IF isLastStatementReturn (n)
	 THEN
            outText (doP, "/* static analysis guarentees a RETURN statement will be used before here.  */\n") ;
            outText (doP, "__builtin_unreachable ();\n") ;
         ELSE
            doException (doP, 'ReturnException', n)
         END
      END
   END ;
   doP := outKc (doP, "}\n") ;
   keyc.leaveScope (n)
END doProcedureC ;


(*
   outProceduresC -
*)

PROCEDURE outProceduresC (p: pretty; s: scopeT) ;
BEGIN
   doP := p ;
   IF debugDecl
   THEN
      printf ("seen %d procedures\n", HighIndice (s.procedures))
   END ;

   ForeachIndiceInIndexDo (s.procedures, doProcedureC)
END outProceduresC ;


(*
   output -
*)

PROCEDURE output (n: node; c, t, v: nodeProcedure) ;
BEGIN
   IF isConst (n)
   THEN
      c (n)
   ELSIF isVar (n)
   THEN
      v (n)
   ELSE
      t (n)
   END
END output ;


(*
   allDependants -
*)

PROCEDURE allDependants (n: node) : dependentState ;
VAR
   l: alist ;
   s: dependentState ;
BEGIN
   l := alists.initList () ;
   s := walkDependants (l, n) ;
   alists.killList (l) ;
   RETURN s
END allDependants ;


(*
   walkDependants -
*)

PROCEDURE walkDependants (l: alist; n: node) : dependentState ;
BEGIN
   IF (n=NIL) OR alists.isItemInList (doneQ, n)
   THEN
      RETURN completed
   ELSIF alists.isItemInList (l, n)
   THEN
      RETURN recursive
   ELSE
      alists.includeItemIntoList (l, n) ;
      RETURN doDependants (l, n)
   END
END walkDependants ;


(*
   walkType -
*)

PROCEDURE walkType (l: alist; n: node) : dependentState ;
VAR
   t: node ;
BEGIN
   t := getType (n) ;
   IF alists.isItemInList (doneQ, t)
   THEN
      RETURN completed
   ELSIF alists.isItemInList (partialQ, t)
   THEN
      RETURN blocked
   ELSE
      queueBlocked (t) ;
      RETURN blocked
   END
END walkType ;


(*
   db -
*)

PROCEDURE db (a: ARRAY OF CHAR; n: node) ;
BEGIN
   IF getDebugTopological ()
   THEN
      outText (doP, a) ;
      IF n#NIL
      THEN
         outTextS (doP, gen (n))
      END
   END
END db ;


(*
   dbt -
*)

PROCEDURE dbt (a: ARRAY OF CHAR) ;
BEGIN
   IF getDebugTopological ()
   THEN
      outText (doP, a)
   END
END dbt ;


(*
   dbs -
*)

PROCEDURE dbs (s: dependentState; n: node) ;
BEGIN
   IF getDebugTopological ()
   THEN
      CASE s OF

      completed:  outText (doP, '{completed ') |
      blocked  :  outText (doP, '{blocked ') |
      partial  :  outText (doP, '{partial ') |
      recursive:  outText (doP, '{recursive ')

      END ;
      IF n#NIL
      THEN
         outTextS (doP, gen (n))
      END ;
      outText (doP, '}\n')
   END
END dbs ;


(*
   dbq -
*)

PROCEDURE dbq (n: node) ;
BEGIN
   IF getDebugTopological ()
   THEN
      IF alists.isItemInList (todoQ, n)
      THEN
         db ('{T', n) ; outText (doP, '}')
      ELSIF alists.isItemInList (partialQ, n)
      THEN
         db ('{P', n) ; outText (doP, '}')
      ELSIF alists.isItemInList (doneQ, n)
      THEN
         db ('{D', n) ; outText (doP, '}')
      END
   END
END dbq ;


(*
   walkRecord -
*)

PROCEDURE walkRecord (l: alist; n: node) : dependentState ;
VAR
   s   : dependentState ;
   o,
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.recordF.listOfSons) ;
   t := HighIndice (n^.recordF.listOfSons) ;
   db ('\nwalking ', n) ; o := getindent (doP) ; setindent (doP, getcurpos (doP)+3) ;
   dbq (n) ;
   WHILE i<=t DO
      q := GetIndice (n^.recordF.listOfSons, i) ;
      db ('', q) ;
      IF isRecordField (q) AND q^.recordfieldF.tag
      THEN
         (* do nothing as it is a tag selector processed in the varient.  *)
      ELSE
         s := walkDependants (l, q) ;
         IF s#completed
         THEN
            dbs (s, q) ;
            addTodo (n) ;
            dbq (n) ;
            db ('\n', NIL) ;
            setindent (doP, o) ;
            RETURN s
         END
      END ;
      INC (i)
   END ;
   db ('{completed', n) ; dbt ('}\n') ;
   setindent (doP, o) ;
   RETURN completed
END walkRecord ;


(*
   walkVarient -
*)

PROCEDURE walkVarient (l: alist; n: node) : dependentState ;
VAR
   s   : dependentState ;
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   db ('\nwalking', n) ;
   s := walkDependants (l, n^.varientF.tag) ;
   IF s#completed
   THEN
      dbs (s, n^.varientF.tag) ;
      dbq (n^.varientF.tag) ;
      db ('\n', NIL) ;
      RETURN s
   END ;
   i := LowIndice (n^.varientF.listOfSons) ;
   t := HighIndice (n^.varientF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.varientF.listOfSons, i) ;
      db ('', q) ;
      s := walkDependants (l, q) ;
      IF s#completed
      THEN
         dbs (s, q) ;
         db ('\n', NIL) ;
         RETURN s
      END ;
      INC (i)
   END ;
   db ('{completed', n) ; dbt ('}\n') ;
   RETURN completed
END walkVarient ;


(*
   queueBlocked -
*)

PROCEDURE queueBlocked (n: node) ;
BEGIN
   IF NOT (alists.isItemInList (doneQ, n) OR alists.isItemInList (partialQ, n))
   THEN
      addTodo (n)
   END
END queueBlocked ;


(*
   walkVar -
*)

PROCEDURE walkVar (l: alist; n: node) : dependentState ;
VAR
   t: node ;
BEGIN
   t := getType (n) ;
   IF alists.isItemInList (doneQ, t)
   THEN
      RETURN completed
   ELSE
      queueBlocked (t) ;
      RETURN blocked
   END
END walkVar ;


(*
   walkEnumeration -
*)

PROCEDURE walkEnumeration (l: alist; n: node) : dependentState ;
VAR
   s   : dependentState ;
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.enumerationF.listOfSons) ;
   t := HighIndice (n^.enumerationF.listOfSons) ;
   s := completed ;
   WHILE i<=t DO
      q := GetIndice (n^.enumerationF.listOfSons, i) ;
      s := walkDependants (l, q) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      INC (i)
   END ;
   RETURN s
END walkEnumeration ;


(*
   walkSubrange -
*)

PROCEDURE walkSubrange (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.subrangeF DO
      s := walkDependants (l, low) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, high) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, type) ;
      IF s#completed
      THEN
         RETURN s
      END
   END ;
   RETURN completed
END walkSubrange ;


(*
   walkSubscript -
*)

PROCEDURE walkSubscript (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.subscriptF DO
      s := walkDependants (l, expr) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, type) ;
      IF s#completed
      THEN
         RETURN s
      END
   END ;
   RETURN completed
END walkSubscript ;


(*
   walkPointer -
*)

PROCEDURE walkPointer (l: alist; n: node) : dependentState ;
VAR
   t: node ;
BEGIN
   (* if the type of, n, is done or partial then we can output pointer.  *)
   t := getType (n) ;
   IF alists.isItemInList (partialQ, t) OR alists.isItemInList (doneQ, t)
   THEN
      (* pointer to partial can always generate a complete type.  *)
      RETURN completed
   END ;
   RETURN walkType (l, n)
END walkPointer ;


(*
   walkArray -
*)

PROCEDURE walkArray (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.arrayF DO
(*
      s := walkDependants (l, type) ;
      IF s#completed
      THEN
         RETURN s
      END ;
*)
      (* an array can only be declared if its data type has already been emitted.  *)
      IF NOT alists.isItemInList (doneQ, type)
      THEN
         s := walkDependants (l, type) ;
         queueBlocked (type) ;
         IF s=completed
         THEN
            (* downgrade the completed to partial as it has not yet been written.  *)
            RETURN partial
         ELSE
            RETURN s
         END
      END ;
      RETURN walkDependants (l, subr)
   END
END walkArray ;


(*
   walkConst -
*)

PROCEDURE walkConst (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.constF DO
      s := walkDependants (l, type) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, value) ;
      IF s#completed
      THEN
         RETURN s
      END
   END ;
   RETURN completed
END walkConst ;


(*
   walkVarParam -
*)

PROCEDURE walkVarParam (l: alist; n: node) : dependentState ;
VAR
   t: node ;
BEGIN
   t := getType (n) ;
   IF alists.isItemInList (partialQ, t)
   THEN
      (* parameter can be issued from a partial.  *)
      RETURN completed
   END ;
   RETURN walkDependants (l, t)
END walkVarParam ;


(*
   walkParam -
*)

PROCEDURE walkParam (l: alist; n: node) : dependentState ;
VAR
   t: node ;
BEGIN
   t := getType (n) ;
   IF alists.isItemInList (partialQ, t)
   THEN
      (* parameter can be issued from a partial.  *)
      RETURN completed
   END ;
   RETURN walkDependants (l, t)
END walkParam ;


(*
   walkOptarg -
*)

PROCEDURE walkOptarg (l: alist; n: node) : dependentState ;
VAR
   t: node ;
BEGIN
   t := getType (n) ;
   IF alists.isItemInList (partialQ, t)
   THEN
      (* parameter can be issued from a partial.  *)
      RETURN completed
   END ;
   RETURN walkDependants (l, t)
END walkOptarg ;


(*
   walkRecordField -
*)

PROCEDURE walkRecordField (l: alist; n: node) : dependentState ;
VAR
   t: node ;
   s: dependentState ;
BEGIN
   assert (isRecordField (n)) ;
   t := getType (n) ;
   IF alists.isItemInList (partialQ, t)
   THEN
      dbs (partial, n) ;
      RETURN partial
   ELSIF alists.isItemInList (doneQ, t)
   THEN
      dbs (completed, n) ;
      RETURN completed
   ELSE
      addTodo (t) ;
      dbs (blocked, n) ;
      dbq (n) ;
      dbq (t) ;
      (*  s := walkDependants (l, t) *)
      RETURN blocked
   END
END walkRecordField ;


(*
   walkVarientField -
*)

PROCEDURE walkVarientField (l: alist; n: node) : dependentState ;
VAR
   s   : dependentState ;
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.varientfieldF.listOfSons) ;
   t := HighIndice (n^.varientfieldF.listOfSons) ;
   s := completed ;
   WHILE i<=t DO
      q := GetIndice (n^.varientfieldF.listOfSons, i) ;
      s := walkDependants (l, q) ;
      IF s#completed
      THEN
         dbs (s, n) ;
         RETURN s
      END ;
      INC (i)
   END ;
   n^.varientfieldF.simple := (t <= 1) ;
   dbs (s, n) ;
   RETURN s
END walkVarientField ;


(*
   walkEnumerationField -
*)

PROCEDURE walkEnumerationField (l: alist; n: node) : dependentState ;
BEGIN
   RETURN completed
END walkEnumerationField ;


(*
   walkSet -
*)

PROCEDURE walkSet (l: alist; n: node) : dependentState ;
BEGIN
   RETURN walkDependants (l, getType (n))
END walkSet ;


(*
   walkProcType -
*)

PROCEDURE walkProcType (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
   t: node ;
BEGIN
   t := getType (n) ;
   IF alists.isItemInList (partialQ, t)
   THEN
      (* proctype can be generated from partial types.  *)
   ELSE
      s := walkDependants (l, t) ;
      IF s#completed
      THEN
         RETURN s
      END
   END ;
   RETURN walkParameters (l, n^.proctypeF.parameters)
END walkProcType ;


(*
   walkProcedure -
*)

PROCEDURE walkProcedure (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   s := walkDependants (l, getType (n)) ;
   IF s#completed
   THEN
      RETURN s
   END ;
   RETURN walkParameters (l, n^.procedureF.parameters)
END walkProcedure ;


(*
   walkParameters -
*)

PROCEDURE walkParameters (l: alist; p: Index) : dependentState ;
VAR
   s   : dependentState ;
   i, h: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (p) ;
   h := HighIndice (p) ;
   WHILE i<=h DO
      q := GetIndice (p, i) ;
      s := walkDependants (l, q) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      INC (i)
   END ;
   RETURN completed
END walkParameters ;


(*
   walkFuncCall -
*)

PROCEDURE walkFuncCall (l: alist; n: node) : dependentState ;
BEGIN
   RETURN completed
END walkFuncCall ;


(*
   walkUnary -
*)

PROCEDURE walkUnary (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.unaryF DO
      s := walkDependants (l, arg) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      RETURN walkDependants (l, resultType)
   END
END walkUnary ;


(*
   walkBinary -
*)

PROCEDURE walkBinary (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.binaryF DO
      s := walkDependants (l, left) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, right) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      RETURN walkDependants (l, resultType)
   END
END walkBinary ;


(*
   walkComponentRef -
*)

PROCEDURE walkComponentRef (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.componentrefF DO
      s := walkDependants (l, rec) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, field) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      RETURN walkDependants (l, resultType)
   END
END walkComponentRef ;


(*
   walkPointerRef -
*)

PROCEDURE walkPointerRef (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.pointerrefF DO
      s := walkDependants (l, ptr) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, field) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      RETURN walkDependants (l, resultType)
   END
END walkPointerRef ;


(*
   walkSetValue -
*)

PROCEDURE walkSetValue (l: alist; n: node) : dependentState ;
VAR
   s   : dependentState ;
   i, j: CARDINAL ;
BEGIN
   assert (isSetValue (n)) ;
   WITH n^.setvalueF DO
      s := walkDependants (l, type) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      i := LowIndice (values) ;
      j := HighIndice (values) ;
      WHILE  i <= j DO
         s := walkDependants (l, GetIndice (values, i)) ;
	 IF s#completed
         THEN
            RETURN s
         END ;
         INC (i)
      END
   END ;
   RETURN completed
END walkSetValue ;


(*
   doDependants - return the dependentState depending upon whether
                  all dependants have been declared.
*)

PROCEDURE doDependants (l: alist; n: node) : dependentState ;
BEGIN
   WITH n^ DO
      CASE kind OF

      throw,          (* --fixme--  *)
      varargs,
      address,
      loc,
      byte,
      word,
      csizet,
      cssizet,
      (* base types.  *)
      boolean,
      char,
      cardinal,
      longcard,
      shortcard,
      integer,
      longint,
      shortint,
      real,
      longreal,
      shortreal,
      bitset,
      ztype,
      rtype,
      complex,
      longcomplex,
      shortcomplex,
      proc            :  RETURN completed |
      (* language features and compound type attributes.  *)
      type            :  RETURN walkType (l, n) |
      record          :  RETURN walkRecord (l, n) |
      varient         :  RETURN walkVarient (l, n) |
      var             :  RETURN walkVar (l, n) |
      enumeration     :  RETURN walkEnumeration (l, n) |
      subrange        :  RETURN walkSubrange (l, n) |
      pointer         :  RETURN walkPointer (l, n) |
      array           :  RETURN walkArray (l, n) |
      string          :  RETURN completed |
      const           :  RETURN walkConst (l, n) |
      literal         :  RETURN completed |
      varparam        :  RETURN walkVarParam (l, n) |
      param           :  RETURN walkParam (l, n) |
      optarg          :  RETURN walkOptarg (l, n) |
      recordfield     :  RETURN walkRecordField (l, n) |
      varientfield    :  RETURN walkVarientField (l, n) |
      enumerationfield:  RETURN walkEnumerationField (l, n) |
      set             :  RETURN walkSet (l, n) |
      proctype        :  RETURN walkProcType (l, n) |
      subscript       :  RETURN walkSubscript (l, n) |
      (* blocks.  *)
      procedure       :  RETURN walkProcedure (l, n) |
      def,
      imp,
      module,
      (* statements.  *)
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  HALT |
      (* expressions.  *)
      componentref    :  RETURN walkComponentRef (l, n) |
      pointerref      :  RETURN walkPointerRef (l, n) |
      not,
      abs,
      min,
      max,
      chr,
      cap,
      ord,
      float,
      trunc,
      high            :  RETURN walkUnary (l, n) |
      cast,
      val,
      plus,
      sub,
      div,
      mod,
      mult,
      divide          :  RETURN walkBinary (l, n) |
      constexp,
      neg,
      adr,
      size,
      tsize,
      deref           :  RETURN walkUnary (l, n) |
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN walkBinary (l, n) |
      funccall        :  RETURN walkFuncCall (l, n) |
      setvalue        :  RETURN walkSetValue (l, n)

      END
   END
END doDependants ;


(*
   tryComplete - returns TRUE if node, n, can be and was completed.
*)

PROCEDURE tryComplete (n: node; c, t, v: nodeProcedure) : BOOLEAN ;
BEGIN
   IF isEnumeration (n)
   THEN
      (* can always emit enumerated types.  *)
      output (n, c, t, v) ;
      RETURN TRUE
   ELSIF isType (n) AND isTypeHidden (n) AND (getType (n)=NIL)
   THEN
      (* can always emit hidden types.  *)
      outputHidden (n) ;
      RETURN TRUE
   ELSIF allDependants (n) = completed
   THEN
      output (n, c, t, v) ;
      RETURN TRUE
   END ;
   RETURN FALSE
END tryComplete ;


(*
   tryCompleteFromPartial -
*)

PROCEDURE tryCompleteFromPartial (n: node; t: nodeProcedure) : BOOLEAN ;
BEGIN
   IF isType (n) AND (getType (n)#NIL) AND isPointer (getType (n)) AND (allDependants (getType (n)) = completed)
   THEN
      (* alists.includeItemIntoList (partialQ, getType (n)) ; *)
      outputHiddenComplete (n) ;
      RETURN TRUE
   ELSIF allDependants (n) = completed
   THEN
      t (n) ;
      RETURN TRUE
   END ;
   RETURN FALSE
END tryCompleteFromPartial ;


(*
   visitIntrinsicFunction -
*)

PROCEDURE visitIntrinsicFunction (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isIntrinsicFunction (n)) ;
   CASE n^.kind OF

   val,
   cmplx:  WITH n^.binaryF DO
             visitNode (v, left, p) ;
             visitNode (v, right, p) ;
             visitNode (v, resultType, p)
           END |
   length,
   adr,
   size,
   tsize,
   float,
   trunc,
   ord,
   chr,
   cap,
   abs,
   high,
   min,
   max,
   re,
   im   : WITH n^.unaryF DO
             visitNode (v, arg, p) ;
             visitNode (v, resultType, p)
          END

   END
END visitIntrinsicFunction ;


(*
   visitUnary -
*)

PROCEDURE visitUnary (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isUnary (n)) ;
   WITH n^.unaryF DO
      visitNode (v, arg, p) ;
      visitNode (v, resultType, p)
   END
END visitUnary ;


(*
   visitBinary -
*)

PROCEDURE visitBinary (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   WITH n^.binaryF DO
      visitNode (v, left, p) ;
      visitNode (v, right, p) ;
      visitNode (v, resultType, p)
   END
END visitBinary ;


(*
   visitBoolean -
*)

PROCEDURE visitBoolean (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   visitNode (v, falseN, p) ;
   visitNode (v, trueN, p)
END visitBoolean ;


(*
   visitScope -
*)

PROCEDURE visitScope (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   IF mustVisitScope
   THEN
      visitNode (v, n, p)
   END
END visitScope ;


(*
   visitType -
*)

PROCEDURE visitType (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isType (n)) ;
   visitNode (v, n^.typeF.type, p) ;
   visitScope (v, n^.typeF.scope, p)
END visitType ;


(*
   visitIndex -
*)

PROCEDURE visitIndex (v: alist; i: Index; p: nodeProcedure) ;
VAR
   j, h: CARDINAL ;
BEGIN
   j := 1 ;
   h := HighIndice (i) ;
   WHILE j <= h DO
      visitNode (v, GetIndice (i, j), p) ;
      INC (j)
   END
END visitIndex ;


(*
   visitRecord -
*)

PROCEDURE visitRecord (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isRecord (n)) ;
   visitScope (v, n^.recordF.scope, p) ;
   visitIndex (v, n^.recordF.listOfSons, p)
END visitRecord ;


(*
   visitVarient -
*)

PROCEDURE visitVarient (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isVarient (n)) ;
   visitIndex (v, n^.varientF.listOfSons, p) ;
   visitNode (v, n^.varientF.varient, p) ;
   visitNode (v, n^.varientF.tag, p) ;
   visitScope (v, n^.varientF.scope, p)
END visitVarient ;


(*
   visitVar -
*)

PROCEDURE visitVar (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isVar (n)) ;
   visitNode (v, n^.varF.type, p) ;
   visitNode (v, n^.varF.decl, p) ;
   visitScope (v, n^.varF.scope, p)
END visitVar ;


(*
   visitEnumeration -
*)

PROCEDURE visitEnumeration (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isEnumeration (n)) ;
   visitIndex (v, n^.enumerationF.listOfSons, p) ;
   visitScope (v, n^.enumerationF.scope, p)
END visitEnumeration ;


(*
   visitSubrange -
*)

PROCEDURE visitSubrange (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isSubrange (n)) ;
   visitNode (v, n^.subrangeF.low, p) ;
   visitNode (v, n^.subrangeF.high, p) ;
   visitNode (v, n^.subrangeF.type, p) ;
   visitScope (v, n^.subrangeF.scope, p)
END visitSubrange ;


(*
   visitPointer -
*)

PROCEDURE visitPointer (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isPointer (n)) ;
   visitNode (v, n^.pointerF.type, p) ;
   visitScope (v, n^.pointerF.scope, p)
END visitPointer ;


(*
   visitArray -
*)

PROCEDURE visitArray (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isArray (n)) ;
   visitNode (v, n^.arrayF.subr, p) ;
   visitNode (v, n^.arrayF.type, p) ;
   visitScope (v, n^.arrayF.scope, p)
END visitArray ;


(*
   visitConst -
*)

PROCEDURE visitConst (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isConst (n)) ;
   visitNode (v, n^.constF.type, p) ;
   visitNode (v, n^.constF.value, p) ;
   visitScope (v, n^.constF.scope, p)
END visitConst ;


(*
   visitVarParam -
*)

PROCEDURE visitVarParam (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isVarParam (n)) ;
   visitNode (v, n^.varparamF.namelist, p) ;
   visitNode (v, n^.varparamF.type, p) ;
   visitScope (v, n^.varparamF.scope, p)
END visitVarParam ;


(*
   visitParam -
*)

PROCEDURE visitParam (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isParam (n)) ;
   visitNode (v, n^.paramF.namelist, p) ;
   visitNode (v, n^.paramF.type, p) ;
   visitScope (v, n^.paramF.scope, p)
END visitParam ;


(*
   visitOptarg -
*)

PROCEDURE visitOptarg (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isOptarg (n)) ;
   visitNode (v, n^.optargF.namelist, p) ;
   visitNode (v, n^.optargF.type, p) ;
   visitNode (v, n^.optargF.init, p) ;
   visitScope (v, n^.optargF.scope, p)
END visitOptarg ;


(*
   visitRecordField -
*)

PROCEDURE visitRecordField (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isRecordField (n)) ;
   visitNode (v, n^.recordfieldF.type, p) ;
   visitNode (v, n^.recordfieldF.parent, p) ;
   visitNode (v, n^.recordfieldF.varient, p) ;
   visitScope (v, n^.recordfieldF.scope, p)
END visitRecordField ;


(*
   visitVarientField -
*)

PROCEDURE visitVarientField (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isVarientField (n)) ;
   visitNode (v, n^.varientfieldF.parent, p) ;
   visitNode (v, n^.varientfieldF.varient, p) ;
   visitIndex (v, n^.varientfieldF.listOfSons, p) ;
   visitScope (v, n^.varientfieldF.scope, p)
END visitVarientField ;


(*
   visitEnumerationField -
*)

PROCEDURE visitEnumerationField (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isEnumerationField (n)) ;
   visitNode (v, n^.enumerationfieldF.type, p) ;
   visitScope (v, n^.enumerationfieldF.scope, p)
END visitEnumerationField ;


(*
   visitSet -
*)

PROCEDURE visitSet (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isSet (n)) ;
   visitNode (v, n^.setF.type, p) ;
   visitScope (v, n^.setF.scope, p)
END visitSet ;


(*
   visitProcType -
*)

PROCEDURE visitProcType (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isProcType (n)) ;
   visitIndex (v, n^.proctypeF.parameters, p) ;
   visitNode (v, n^.proctypeF.optarg, p) ;
   visitNode (v, n^.proctypeF.returnType, p) ;
   visitScope (v, n^.proctypeF.scope, p)
END visitProcType ;


(*
   visitSubscript -
*)

PROCEDURE visitSubscript (v: alist; n: node; p: nodeProcedure) ;
BEGIN
(*
   assert (isSubscript (n)) ;
   visitNode (v, n^.subscriptF.type, p) ;
   visitNode (v, n^.subscriptF.expr, p)
*)
END visitSubscript ;


(*
   visitDecls -
*)

PROCEDURE visitDecls (v: alist; s: scopeT; p: nodeProcedure) ;
BEGIN
   visitIndex (v, s.constants, p) ;
   visitIndex (v, s.types, p) ;
   visitIndex (v, s.procedures, p) ;
   visitIndex (v, s.variables, p)
END visitDecls ;


(*
   visitProcedure -
*)

PROCEDURE visitProcedure (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isProcedure (n)) ;
   visitDecls (v, n^.procedureF.decls, p) ;
   visitScope (v, n^.procedureF.scope, p) ;
   visitIndex (v, n^.procedureF.parameters, p) ;
   visitNode (v, n^.procedureF.optarg, p) ;
   visitNode (v, n^.procedureF.returnType, p) ;
   visitNode (v, n^.procedureF.beginStatements, p)
END visitProcedure ;


(*
   visitDef -
*)

PROCEDURE visitDef (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isDef (n)) ;
   visitDecls (v, n^.defF.decls, p)
END visitDef ;


(*
   visitImp -
*)

PROCEDURE visitImp (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isImp (n)) ;
   visitDecls (v, n^.impF.decls, p) ;
   visitNode (v, n^.impF.beginStatements, p) ;
   visitNode (v, n^.impF.finallyStatements, p)
   (* --fixme-- do we need to visit definitionModule?  *)
END visitImp ;


(*
   visitModule -
*)

PROCEDURE visitModule (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isModule (n)) ;
   visitDecls (v, n^.moduleF.decls, p) ;
   visitNode (v, n^.moduleF.beginStatements, p) ;
   visitNode (v, n^.moduleF.finallyStatements, p)
END visitModule ;


(*
   visitLoop -
*)

PROCEDURE visitLoop (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isLoop (n)) ;
   visitNode (v, n^.loopF.statements, p)
END visitLoop ;


(*
   visitWhile -
*)

PROCEDURE visitWhile (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isWhile (n)) ;
   visitNode (v, n^.whileF.expr, p) ;
   visitNode (v, n^.whileF.statements, p)
END visitWhile ;


(*
   visitRepeat -
*)

PROCEDURE visitRepeat (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isRepeat (n)) ;
   visitNode (v, n^.repeatF.expr, p) ;
   visitNode (v, n^.repeatF.statements, p)
END visitRepeat ;


(*
   visitCase -
*)

PROCEDURE visitCase (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isCase (n)) ;
   visitNode (v, n^.caseF.expression, p) ;
   visitIndex (v, n^.caseF.caseLabelList, p) ;
   visitNode (v, n^.caseF.else, p)
END visitCase ;


(*
   visitCaseLabelList -
*)

PROCEDURE visitCaseLabelList (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isCaseLabelList (n)) ;
   visitNode (v, n^.caselabellistF.caseList, p) ;
   visitNode (v, n^.caselabellistF.statements, p)
END visitCaseLabelList ;


(*
   visitCaseList -
*)

PROCEDURE visitCaseList (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isCaseList (n)) ;
   visitIndex (v, n^.caselistF.rangePairs, p)
END visitCaseList ;


(*
   visitRange -
*)

PROCEDURE visitRange (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isRange (n)) ;
   visitNode (v, n^.rangeF.lo, p) ;
   visitNode (v, n^.rangeF.hi, p)
END visitRange ;


(*
   visitIf -
*)

PROCEDURE visitIf (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isIf (n)) ;
   visitNode (v, n^.ifF.expr, p) ;
   visitNode (v, n^.ifF.elsif, p) ;
   visitNode (v, n^.ifF.then, p) ;
   visitNode (v, n^.ifF.else, p)
END visitIf ;


(*
   visitElsif -
*)

PROCEDURE visitElsif (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isElsif (n)) ;
   visitNode (v, n^.elsifF.expr, p) ;
   visitNode (v, n^.elsifF.elsif, p) ;
   visitNode (v, n^.elsifF.then, p) ;
   visitNode (v, n^.elsifF.else, p)
END visitElsif ;


(*
   visitFor -
*)

PROCEDURE visitFor (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isFor (n)) ;
   visitNode (v, n^.forF.des, p) ;
   visitNode (v, n^.forF.start, p) ;
   visitNode (v, n^.forF.end, p) ;
   visitNode (v, n^.forF.increment, p) ;
   visitNode (v, n^.forF.statements, p)
END visitFor ;


(*
   visitAssignment -
*)

PROCEDURE visitAssignment (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isAssignment (n)) ;
   visitNode (v, n^.assignmentF.des, p) ;
   visitNode (v, n^.assignmentF.expr, p)
END visitAssignment ;


(*
   visitComponentRef -
*)

PROCEDURE visitComponentRef (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isComponentRef (n)) ;
   visitNode (v, n^.componentrefF.rec, p) ;
   visitNode (v, n^.componentrefF.field, p) ;
   visitNode (v, n^.componentrefF.resultType, p)
END visitComponentRef ;


(*
   visitPointerRef -
*)

PROCEDURE visitPointerRef (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isPointerRef (n)) ;
   visitNode (v, n^.pointerrefF.ptr, p) ;
   visitNode (v, n^.pointerrefF.field, p) ;
   visitNode (v, n^.pointerrefF.resultType, p)
END visitPointerRef ;


(*
   visitArrayRef -
*)

PROCEDURE visitArrayRef (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isArrayRef (n)) ;
   visitNode (v, n^.arrayrefF.array, p) ;
   visitNode (v, n^.arrayrefF.index, p) ;
   visitNode (v, n^.arrayrefF.resultType, p)
END visitArrayRef ;


(*
   visitFunccall -
*)

PROCEDURE visitFunccall (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isFuncCall (n)) ;
   visitNode (v, n^.funccallF.function, p) ;
   visitNode (v, n^.funccallF.args, p) ;
   visitNode (v, n^.funccallF.type, p)
END visitFunccall ;


(*
   visitVarDecl -
*)

PROCEDURE visitVarDecl (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isVarDecl (n)) ;
   visitNode (v, n^.vardeclF.type, p) ;
   visitScope (v, n^.vardeclF.scope, p)
END visitVarDecl ;


(*
   visitExplist -
*)

PROCEDURE visitExplist (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isExpList (n)) ;
   visitIndex (v, n^.explistF.exp, p)
END visitExplist ;


(*
   visitExit -
*)

PROCEDURE visitExit (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isExit (n)) ;
   visitNode (v, n^.exitF.loop, p)
END visitExit ;


(*
   visitReturn -
*)

PROCEDURE visitReturn (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isReturn (n)) ;
   visitNode (v, n^.returnF.exp, p)
END visitReturn ;


(*
   visitStmtSeq -
*)

PROCEDURE visitStmtSeq (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isStatementSequence (n)) ;
   visitIndex (v, n^.stmtF.statements, p)
END visitStmtSeq ;


(*
   visitVarargs -
*)

PROCEDURE visitVarargs (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isVarargs (n)) ;
   visitScope (v, n^.varargsF.scope, p)
END visitVarargs ;


(*
   visitSetValue -
*)

PROCEDURE visitSetValue (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isSetValue (n)) ;
   visitNode (v, n^.setvalueF.type, p) ;
   visitIndex (v, n^.setvalueF.values, p)
END visitSetValue ;


(*
   visitIntrinsic -
*)

PROCEDURE visitIntrinsic (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (isIntrinsic (n)) ;
   visitNode (v, n^.intrinsicF.args, p)
END visitIntrinsic ;


(*
   visitDependants - helper procedure function called from visitNode.
                     node n has just been visited, this procedure will
                     visit node, n, dependants.
*)

PROCEDURE visitDependants (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   assert (n # NIL) ;
   assert (alists.isItemInList (v, n)) ;
   CASE n^.kind OF

   explist         :  visitExplist (v, n, p) |
   funccall        :  visitFunccall (v, n, p) |
   exit            :  visitExit (v, n, p) |
   return          :  visitReturn (v, n, p) |
   stmtseq         :  visitStmtSeq (v, n, p) |
   comment         :  |
   length          :  visitIntrinsicFunction (v, n, p) |
   unreachable,
   throw,
   halt,
   new,
   dispose,
   inc,
   dec,
   incl,
   excl            :  visitIntrinsic (v, n, p) |
   boolean         :  visitBoolean (v, n, p) |
   nil,
   false,
   true            :  |
   varargs         :  visitVarargs (v, n, p) |
   address,
   loc,
   byte,
   word,
   csizet,
   cssizet,
   (* base types.  *)
   char,
   cardinal,
   longcard,
   shortcard,
   integer,
   longint,
   shortint,
   real,
   longreal,
   shortreal,
   bitset,
   ztype,
   rtype,
   complex,
   longcomplex,
   shortcomplex,
   proc            :  |
   (* language features and compound type attributes.  *)
   type            :  visitType (v, n, p) |
   record          :  visitRecord (v, n, p) |
   varient         :  visitVarient (v, n, p) |
   var             :  visitVar (v, n, p) |
   enumeration     :  visitEnumeration (v, n, p) |
   subrange        :  visitSubrange (v, n, p) |
   pointer         :  visitPointer (v, n, p) |
   array           :  visitArray (v, n, p) |
   string          :  |
   const           :  visitConst (v, n, p) |
   literal         :  |
   varparam        :  visitVarParam (v, n, p) |
   param           :  visitParam (v, n, p) |
   optarg          :  visitOptarg (v, n, p) |
   recordfield     :  visitRecordField (v, n, p) |
   varientfield    :  visitVarientField (v, n, p) |
   enumerationfield:  visitEnumerationField (v, n, p) |
   set             :  visitSet (v, n, p) |
   proctype        :  visitProcType (v, n, p) |
   subscript       :  visitSubscript (v, n, p) |
   (* blocks.  *)
   procedure       :  visitProcedure (v, n, p) |
   def             :  visitDef (v, n, p) |
   imp             :  visitImp (v, n, p) |
   module          :  visitModule (v, n, p) |
   (* statements.  *)
   loop            :  visitLoop (v, n, p) |
   while           :  visitWhile (v, n, p) |
   for             :  visitFor (v, n, p) |
   repeat          :  visitRepeat (v, n, p) |
   case            :  visitCase (v, n, p) |
   caselabellist   :  visitCaseLabelList (v, n, p) |
   caselist        :  visitCaseList (v, n, p) |
   range           :  visitRange (v, n, p) |
   if              :  visitIf (v, n, p) |
   elsif           :  visitElsif (v, n, p) |
   assignment      :  visitAssignment (v, n, p) |
   (* expressions.  *)
   componentref    :  visitComponentRef (v, n, p) |
   pointerref      :  visitPointerRef (v, n, p) |
   arrayref        :  visitArrayRef (v, n, p) |
   cmplx,
   equal,
   notequal,
   less,
   greater,
   greequal,
   lessequal,
   and,
   or,
   in,
   cast,
   val,
   plus,
   sub,
   div,
   mod,
   mult,
   divide         :  visitBinary (v, n, p) |
   re             :  visitUnary (v, n, p) |
   im             :  visitUnary (v, n, p) |
   abs            :  visitUnary (v, n, p) |
   chr            :  visitUnary (v, n, p) |
   cap            :  visitUnary (v, n, p) |
   high           :  visitUnary (v, n, p) |
   ord            :  visitUnary (v, n, p) |
   float          :  visitUnary (v, n, p) |
   trunc          :  visitUnary (v, n, p) |
   not            :  visitUnary (v, n, p) |
   neg            :  visitUnary (v, n, p) |
   adr            :  visitUnary (v, n, p) |
   size           :  visitUnary (v, n, p) |
   tsize          :  visitUnary (v, n, p) |
   min            :  visitUnary (v, n, p) |
   max            :  visitUnary (v, n, p) |
   constexp       :  visitUnary (v, n, p) |
   deref          :  visitUnary (v, n, p) |
   identlist      :  |
   vardecl        :  visitVarDecl (v, n, p) |
   setvalue       :  visitSetValue (v, n, p)

   END
END visitDependants ;


(*
   visitNode - visits node, n, if it is not already in the alist, v.
               It calls p(n) if the node is unvisited.
*)

PROCEDURE visitNode (v: alist; n: node; p: nodeProcedure) ;
BEGIN
   IF (n#NIL) AND (NOT alists.isItemInList (v, n))
   THEN
      alists.includeItemIntoList (v, n) ;
      p (n) ;
      visitDependants (v, n, p)
   END
END visitNode ;


(*
   genKind - returns a string depending upon the kind of node, n.
*)

PROCEDURE genKind (n: node) : String ;
BEGIN
   CASE n^.kind OF

   (* types, no need to generate a kind string as it it contained in the name.  *)
   nil,
   true,
   false,
   address,
   loc,
   byte,
   word,
   csizet,
   cssizet,
   char,
   cardinal,
   longcard,
   shortcard,
   integer,
   longint,
   shortint,
   real,
   longreal,
   shortreal,
   bitset,
   boolean,
   proc,
   ztype,
   rtype,
   complex,
   longcomplex,
   shortcomplex    :  RETURN NIL |

   (* language features and compound type attributes.  *)
   type            :  RETURN InitString ('type') |
   record          :  RETURN InitString ('record') |
   varient         :  RETURN InitString ('varient') |
   var             :  RETURN InitString ('var') |
   enumeration     :  RETURN InitString ('enumeration') |
   subrange        :  RETURN InitString ('subrange') |
   array           :  RETURN InitString ('array') |
   subscript       :  RETURN InitString ('subscript') |
   string          :  RETURN InitString ('string') |
   const           :  RETURN InitString ('const') |
   literal         :  RETURN InitString ('literal') |
   varparam        :  RETURN InitString ('varparam') |
   param           :  RETURN InitString ('param') |
   varargs         :  RETURN InitString ('varargs') |
   pointer         :  RETURN InitString ('pointer') |
   recordfield     :  RETURN InitString ('recordfield') |
   varientfield    :  RETURN InitString ('varientfield') |
   enumerationfield:  RETURN InitString ('enumerationfield') |
   set             :  RETURN InitString ('set') |
   proctype        :  RETURN InitString ('proctype') |
   (* blocks.  *)
   procedure       :  RETURN InitString ('procedure') |
   def             :  RETURN InitString ('def') |
   imp             :  RETURN InitString ('imp') |
   module          :  RETURN InitString ('module') |
   (* statements.  *)
   loop            :  RETURN InitString ('loop') |
   while           :  RETURN InitString ('while') |
   for             :  RETURN InitString ('for') |
   repeat          :  RETURN InitString ('repeat') |
   assignment      :  RETURN InitString ('assignment') |
   if              :  RETURN InitString ('if') |
   elsif           :  RETURN InitString ('elsif') |
   (* expressions.  *)
   constexp        :  RETURN InitString ('constexp') |
   neg             :  RETURN InitString ('neg') |
   cast            :  RETURN InitString ('cast') |
   val             :  RETURN InitString ('val') |
   plus            :  RETURN InitString ('plus') |
   sub             :  RETURN InitString ('sub') |
   div             :  RETURN InitString ('div') |
   mod             :  RETURN InitString ('mod') |
   mult            :  RETURN InitString ('mult') |
   divide          :  RETURN InitString ('divide') |
   adr             :  RETURN InitString ('adr') |
   size            :  RETURN InitString ('size') |
   tsize           :  RETURN InitString ('tsize') |
   chr             :  RETURN InitString ('chr') |
   ord             :  RETURN InitString ('ord') |
   float           :  RETURN InitString ('float') |
   trunc           :  RETURN InitString ('trunc') |
   high            :  RETURN InitString ('high') |
   componentref    :  RETURN InitString ('componentref') |
   pointerref      :  RETURN InitString ('pointerref') |
   arrayref        :  RETURN InitString ('arrayref') |
   deref           :  RETURN InitString ('deref') |
   equal           :  RETURN InitString ('equal') |
   notequal        :  RETURN InitString ('notequal') |
   less            :  RETURN InitString ('less') |
   greater         :  RETURN InitString ('greater') |
   greequal        :  RETURN InitString ('greequal') |
   lessequal       :  RETURN InitString ('lessequal') |
   lsl             :  RETURN InitString ('lsl') |
   lsr             :  RETURN InitString ('lsr') |
   lor             :  RETURN InitString ('lor') |
   land            :  RETURN InitString ('land') |
   lnot            :  RETURN InitString ('lnot') |
   lxor            :  RETURN InitString ('lxor') |
   and             :  RETURN InitString ('and') |
   or              :  RETURN InitString ('or') |
   not             :  RETURN InitString ('not') |
   identlist       :  RETURN InitString ('identlist') |
   vardecl         :  RETURN InitString ('vardecl')

   END ;
   HALT
END genKind ;


(*
   gen - generate a small string describing node, n.
*)

PROCEDURE gen (n: node) : String ;
VAR
   s: String ;
   d: CARDINAL ;
BEGIN
   d := VAL (CARDINAL, VAL (LONGCARD, n)) ;
   s := Sprintf1 (InitString ('< %d '), d) ;  (* use 0x%x once FormatStrings has been released.  *)
   s := ConCat (s, genKind (n)) ;
   s := ConCat (s, InitString (' ')) ;
   s := ConCat (s, getFQstring (n)) ;
   s := ConCat (s, InitString (' >')) ;
   RETURN s
END gen ;


(*
   dumpQ -
*)

PROCEDURE dumpQ (q: ARRAY OF CHAR; l: alist) ;
VAR
   m   : String ;
   n   : node ;
   d,
   h, i: CARDINAL ;
BEGIN
   m := Sprintf0 (InitString ('Queue ')) ;
   m := KillString (WriteS (StdOut, m)) ;
   m := Sprintf0 (InitString (q)) ;
   m := KillString (WriteS (StdOut, m)) ;
   m := Sprintf0 (InitString ('\n')) ;
   m := KillString (WriteS (StdOut, m)) ;
   i := 1 ;
   h := alists.noOfItemsInList (l) ;
   WHILE i<=h DO
      n := alists.getItemFromList (l, i) ;
      m := KillString (WriteS (StdOut, gen (n))) ;
      INC (i)
   END ;
   m := Sprintf0 (InitString ('\n')) ;
   m := KillString (WriteS (StdOut, m))
END dumpQ ;


(*
   dumpLists -
*)

PROCEDURE dumpLists ;
VAR
   m: String ;
BEGIN
   IF getDebugTopological ()
   THEN
      m := Sprintf0 (InitString ('\n')) ;
      m := KillString (WriteS (StdOut, m)) ;
      dumpQ ('todo', todoQ) ;
      dumpQ ('partial', partialQ) ;
      dumpQ ('done', doneQ)
   END
END dumpLists ;


(*
   outputHidden -
*)

PROCEDURE outputHidden (n: node) ;
BEGIN
   outText (doP, "#if !defined (") ; doFQNameC (doP, n) ; outText (doP, "_D)\n") ;
   outText (doP, "#  define ") ; doFQNameC (doP, n) ; outText (doP, "_D\n") ;
   outText (doP, "   typedef void *") ; doFQNameC (doP, n) ; outText (doP, ";\n") ;
   outText (doP, "#endif\n\n")
END outputHidden ;


(*
   outputHiddenComplete -
*)

PROCEDURE outputHiddenComplete (n: node) ;
VAR
   t: node ;
BEGIN
   assert (isType (n)) ;
   t := getType (n) ;
   assert (isPointer (t)) ;
   outText (doP, "#define ") ; doFQNameC (doP, n) ; outText (doP, "_D\n") ;
   outText (doP, "typedef ") ; doTypeNameC (doP, getType (t)) ;
   setNeedSpace (doP) ; outText (doP, "*") ; doFQNameC (doP, n) ; outText (doP, ";\n")
END outputHiddenComplete ;


(*
   tryPartial -
*)
(*
PROCEDURE tryPartial (n: node; pt: nodeProcedure) : BOOLEAN ;
VAR
   q          : node ;
   seenPointer: BOOLEAN ;
BEGIN
   IF (n#NIL) AND isType (n)
   THEN
      seenPointer := FALSE ;
      q := getType (n) ;
      WHILE isPointer (q) DO
         seenPointer := TRUE ;
         q := getType (q)
      END ;
      IF q # NIL
      THEN
         IF isRecord (q) OR isProcType (q)
         THEN
            pt (n) ;
            addTodo (q) ;
            RETURN TRUE
         ELSIF isArray (q) AND (seenPointer OR alists.isItemInList (doneQ, getType (q)))
         THEN
            pt (n) ;
            addTodo (q) ;
            RETURN TRUE
         ELSIF isType (q) AND seenPointer
         THEN
            pt (n) ;
            addTodo (q) ;
            RETURN TRUE
         END
      END
   END ;
   RETURN FALSE
END tryPartial ;
*)


(*
   tryPartial -
*)

PROCEDURE tryPartial (n: node; pt: nodeProcedure) : BOOLEAN ;
VAR
   q: node ;
BEGIN
   IF (n#NIL) AND isType (n)
   THEN
      q := getType (n) ;
      WHILE isPointer (q) DO
         q := getType (q)
      END ;
      IF q # NIL
      THEN
         IF isRecord (q) OR isProcType (q)
         THEN
            pt (n) ;
            addTodo (q) ;
            RETURN TRUE
         ELSIF isArray (q)
         THEN
            pt (n) ;
            addTodo (q) ;
            RETURN TRUE
         END
      END
   END ;
   RETURN FALSE
END tryPartial ;


(*
   outputPartialRecordArrayProcType -
*)

PROCEDURE outputPartialRecordArrayProcType (n, q: node; indirection: CARDINAL) ;
VAR
   s: String ;
BEGIN
   outText (doP, "typedef struct") ; setNeedSpace (doP) ;
   s := getFQstring (n) ;
   IF isRecord (q)
   THEN
      s := ConCat (s, Mark (InitString ("_r")))
   ELSIF isArray (q)
   THEN
      s := ConCat (s, Mark (InitString ("_a")))
   ELSIF isProcType (q)
   THEN
      s := ConCat (s, Mark (InitString ("_p")))
   END ;
   outTextS (doP, s) ;
   setNeedSpace (doP) ;
   s := KillString (s) ;
   WHILE indirection>0 DO
      outText (doP, "*") ;
      DEC (indirection)
   END ;
   doFQNameC (doP, n) ;
   outText (doP, ";\n\n")
END outputPartialRecordArrayProcType ;


(*
   outputPartial -
*)

PROCEDURE outputPartial (n: node) ;
VAR
   q          : node ;
   indirection: CARDINAL ;
BEGIN
   q := getType (n) ;
   indirection := 0 ;
   WHILE isPointer (q) DO
      q := getType (q) ;
      INC (indirection)
   END ;
   outputPartialRecordArrayProcType (n, q, indirection)
END outputPartial ;


(*
   tryOutputTodo -
*)

PROCEDURE tryOutputTodo (c, t, v, pt: nodeProcedure) ;
VAR
   i, n: CARDINAL ;
   d   : node ;
BEGIN
   i := 1 ;
   n := alists.noOfItemsInList (todoQ) ;
   WHILE i<=n DO
      d := alists.getItemFromList (todoQ, i) ;
      IF tryComplete (d, c, t, v)
      THEN
         alists.removeItemFromList (todoQ, d) ;
	 alists.includeItemIntoList (doneQ, d) ;
         i := 1
      ELSIF tryPartial (d, pt)
      THEN
         alists.removeItemFromList (todoQ, d) ;
         alists.includeItemIntoList (partialQ, d) ;
         i := 1
      ELSE
         INC (i)
      END ;
      n := alists.noOfItemsInList (todoQ)
   END
END tryOutputTodo ;


(*
   tryOutputPartial -
*)

PROCEDURE tryOutputPartial (t: nodeProcedure) ;
VAR
   i, n: CARDINAL ;
   d   : node ;
BEGIN
   i := 1 ;
   n := alists.noOfItemsInList (partialQ) ;
   WHILE i<=n DO
      d := alists.getItemFromList (partialQ, i) ;
      IF tryCompleteFromPartial (d, t)
      THEN
         alists.removeItemFromList (partialQ, d) ;
         alists.includeItemIntoList (doneQ, d) ;
         i := 1 ;
         DEC (n)
      ELSE
         INC (i)
      END
   END
END tryOutputPartial ;


(*
   debugList -
*)

PROCEDURE debugList (a: ARRAY OF CHAR; l: alist) ;
VAR
   i, h: CARDINAL ;
   n   : node ;
BEGIN
   h := alists.noOfItemsInList (l) ;
   IF h>0
   THEN
      outText (doP, a) ;
      outText (doP, ' still contains node(s)\n') ;
      i := 1 ;
      REPEAT
         n := alists.getItemFromList (l, i) ;
         dbg (n) ;
         INC (i)
      UNTIL i > h
   END
END debugList ;


(*
   debugLists -
*)

PROCEDURE debugLists ;
BEGIN
   IF getDebugTopological ()
   THEN
      debugList ('todo', todoQ) ;
      debugList ('partial', partialQ)
   END
END debugLists ;


(*
   addEnumConst -
*)

PROCEDURE addEnumConst (n: node) ;
VAR
   s: String ;
BEGIN
   IF isConst (n) OR isEnumeration (n)
   THEN
      addTodo (n)
   END
END addEnumConst ;


(*
   populateTodo -
*)

PROCEDURE populateTodo (p: nodeProcedure) ;
VAR
   n   : node ;
   i, h: CARDINAL ;
   l   : alist ;
BEGIN
   h := alists.noOfItemsInList (todoQ) ;
   i := 1 ;
   WHILE i <= h DO
      n := alists.getItemFromList (todoQ, i) ;
      l := alists.initList () ;
      visitNode (l, n, p) ;
      alists.killList (l) ;
      h := alists.noOfItemsInList (todoQ) ;
      INC (i)
   END
END populateTodo ;


(*
   topologicallyOut -
*)

PROCEDURE topologicallyOut (c, t, v, tp,
                            pc, pt, pv: nodeProcedure) ;
VAR
   tol, pal,
   to,  pa : CARDINAL ;
BEGIN
   populateTodo (addEnumConst) ;
   tol := 0 ;
   pal := 0 ;
   to := alists.noOfItemsInList (todoQ) ;
   pa := alists.noOfItemsInList (partialQ) ;
   WHILE (tol#to) OR (pal#pa) DO
      dumpLists ;
      tryOutputTodo (c, t, v, tp) ;
      dumpLists ;
      tryOutputPartial (pt) ;
      tol := to ;
      pal := pa ;
      to := alists.noOfItemsInList (todoQ) ;
      pa := alists.noOfItemsInList (partialQ)
   END ;
   dumpLists ;
   debugLists
END topologicallyOut ;


(*
   scaffoldStatic -
*)

PROCEDURE scaffoldStatic (p: pretty; n: node) ;
BEGIN
   outText (p, "\n") ;
   doExternCP (p) ;
   outText (p, "void") ;
   setNeedSpace (p) ;
   outText (p, "_M2_") ;
   doFQNameC (p, n) ;
   outText (p, "_init") ;
   setNeedSpace (p) ;
   outText (p, "(__attribute__((unused)) int argc") ;
   outText (p, ",__attribute__((unused)) char *argv[]") ;
   outText (p, ",__attribute__((unused)) char *envp[])\n");
   p := outKc (p, "{\n") ;
   doStatementsC (p, n^.impF.beginStatements) ;
   p := outKc (p, "}\n") ;
   outText (p, "\n") ;
   doExternCP (p) ;
   outText (p, "void") ;
   setNeedSpace (p) ;
   outText (p, "_M2_") ;
   doFQNameC (p, n) ;
   outText (p, "_fini") ;
   setNeedSpace (p) ;
   outText (p, "(__attribute__((unused)) int argc") ;
   outText (p, ",__attribute__((unused)) char *argv[]") ;
   outText (p, ",__attribute__((unused)) char *envp[])\n");
   p := outKc (p, "{\n") ;
   doStatementsC (p, n^.impF.finallyStatements) ;
   p := outKc (p, "}\n")
END scaffoldStatic ;


(*
   emitCtor -
*)

PROCEDURE emitCtor (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   outText (p, "\n") ;
   outText (p, "static void") ;
   setNeedSpace (p) ;
   outText (p, "ctorFunction ()\n") ;
   doFQNameC (p, n) ;
   p := outKc (p, "{\n") ;
   outText (p, 'M2RTS_RegisterModule ("') ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   prints (p, s) ;
   outText (p, '",\n') ;
   outText (p, 'init, fini, dependencies);\n') ;
   p := outKc (p, "}\n\n") ;
   p := outKc (p, "struct ") ;
   prints (p, s) ;
   p := outKc (p, "_module_m2 { ") ;
   prints (p, s) ;
   p := outKc (p, "_module_m2 (); ~") ;
   prints (p, s) ;
   p := outKc (p, "_module_m2 (); } global_module_") ;
   prints (p, s) ;
   outText (p, ';\n\n') ;
   prints (p, s) ;
   p := outKc (p, "_module_m2::") ;
   prints (p, s) ;
   p := outKc (p, "_module_m2 ()\n") ;
   p := outKc (p, "{\n") ;
   outText (p, 'M2RTS_RegisterModule ("') ;
   prints (p, s) ;
   outText (p, '", init, fini, dependencies);') ;
   p := outKc (p, "}\n") ;
   prints (p, s) ;
   p := outKc (p, "_module_m2::~") ;
   prints (p, s) ;
   p := outKc (p, "_module_m2 ()\n") ;
   p := outKc (p, "{\n") ;
   p := outKc (p, "}\n") ;
   s := KillString (s)
END emitCtor ;


(*
   scaffoldDynamic -
*)

PROCEDURE scaffoldDynamic (p: pretty; n: node) ;
BEGIN
   outText (p, "\n") ;
   doExternCP (p) ;
   outText (p, "void") ;
   setNeedSpace (p) ;
   outText (p, "_M2_") ;
   doFQNameC (p, n) ;
   outText (p, "_init") ;
   setNeedSpace (p) ;
   outText (p, "(__attribute__((unused)) int argc,") ;
   outText (p, " __attribute__((unused)) char *argv[]") ;
   outText (p, " __attribute__((unused)) char *envp[])\n") ;
   p := outKc (p, "{\n") ;
   doStatementsC (p, n^.impF.beginStatements) ;
   p := outKc (p, "}\n") ;
   outText (p, "\n") ;
   doExternCP (p) ;
   outText (p, "void") ;
   setNeedSpace (p) ;
   outText (p, "_M2_") ;
   doFQNameC (p, n) ;
   outText (p, "_fini") ;
   setNeedSpace (p) ;
   outText (p, "(__attribute__((unused)) int argc,") ;
   outText (p, " __attribute__((unused)) char *argv[]") ;
   outText (p, " __attribute__((unused)) char *envp[])\n") ;
   p := outKc (p, "{\n") ;
   doStatementsC (p, n^.impF.finallyStatements) ;
   p := outKc (p, "}\n") ;
   emitCtor (p, n)
END scaffoldDynamic ;


(*
   scaffoldMain -
*)

PROCEDURE scaffoldMain (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   outText (p, "int\n") ;
   outText (p, "main") ;
   setNeedSpace (p) ;
   outText (p, "(int argc, char *argv[], char *envp[])\n") ;
   p := outKc (p, "{\n") ;
   outText (p, "M2RTS_ConstructModules (") ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   prints (p, s) ;
   outText (p, ", argc, argv, envp);\n");
   outText (p, "M2RTS_DeconstructModules (") ;
   prints (p, s) ;
   outText (p, ", argc, argv, envp);\n");
   outText (p, "return 0;") ;
   p := outKc (p, "}\n") ;
   s := KillString (s)
END scaffoldMain ;


(*
   outImpInitC - emit the init/fini functions and main function if required.
*)

PROCEDURE outImpInitC (p: pretty; n: node) ;
BEGIN
   IF getScaffoldDynamic ()
   THEN
      scaffoldDynamic (p, n)
   ELSE
      scaffoldStatic (p, n)
   END ;
   IF getScaffoldMain ()
   THEN
      scaffoldMain (p, n)
   END
END outImpInitC ;


(*
   runSimplifyTypes -
*)

PROCEDURE runSimplifyTypes (n: node) ;
BEGIN
   IF isImp (n)
   THEN
      simplifyTypes (n^.impF.decls)
   ELSIF isModule (n)
   THEN
      simplifyTypes (n^.moduleF.decls)
   ELSIF isDef (n)
   THEN
      simplifyTypes (n^.defF.decls)
   END
END runSimplifyTypes ;


(*
   outDefC -
*)

PROCEDURE outDefC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   assert (isDef (n)) ;
   outputFile := mcStream.openFrag (1) ;  (* first fragment.  *)
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   print (p, "/* do not edit automatically generated by mc from ") ;
   prints (p, s) ; print (p, ".  */\n") ;
   writeGPLheader (outputFile) ;
   doCommentC (p, n^.defF.com.body) ;
   print (p, "\n\n#if !defined (_") ; prints (p, s) ; print (p, "_H)\n") ;
   print (p, "#   define _") ; prints (p, s) ; print (p, "_H\n\n") ;

   keyc.genConfigSystem (p) ;

   print (p, "#   ifdef __cplusplus\n") ;
   print (p, 'extern "C" {\n') ;
   print (p, "#   endif\n") ;

   outputFile := mcStream.openFrag (3) ;  (* third fragment.  *)

   doP := p ;
   ForeachIndiceInIndexDo (n^.defF.importedModules, doIncludeC) ;

   print (p, "\n") ;
   print (p, "#   if defined (_") ; prints (p, s) ; print (p, "_C)\n") ;
   print (p, "#      define EXTERN\n") ;
   print (p, "#   else\n") ;
   print (p, '#      define EXTERN extern\n') ;
   print (p, "#   endif\n\n") ;

   outDeclsDefC (p, n) ;
   runPrototypeDefC (n) ;

   print (p, "#   ifdef __cplusplus\n") ;
   print (p, "}\n") ;
   print (p, "#   endif\n") ;

   print (p, "\n") ;
   print (p, "#   undef EXTERN\n") ;
   print (p, "#endif\n") ;

   outputFile := mcStream.openFrag (2) ;  (* second fragment.  *)
   keyc.genDefs (p) ;

   s := KillString (s)
END outDefC ;


(*
   runPrototypeExported -
*)

PROCEDURE runPrototypeExported (n: node) ;
BEGIN
   IF isExported (n)
   THEN
      keyc.enterScope (n) ;
      doProcedureHeadingC (n, TRUE) ;
      print (doP, ";\n") ;
      keyc.leaveScope (n)
   END
END runPrototypeExported ;


(*
   runPrototypeDefC -
*)

PROCEDURE runPrototypeDefC (n: node) ;
BEGIN
   IF isDef (n)
   THEN
      ForeachIndiceInIndexDo (n^.defF.decls.procedures, runPrototypeExported)
   END
END runPrototypeDefC ;


(*
   outImpC -
*)

PROCEDURE outImpC (p: pretty; n: node) ;
VAR
   s        : String ;
   defModule: node ;
BEGIN
   assert (isImp (n)) ;
   outputFile := mcStream.openFrag (1) ;  (* first fragment.  *)
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   print (p, "/* do not edit automatically generated by mc from ") ;
   prints (p, s) ; print (p, ".  */\n") ;
   writeGPLheader (outputFile) ;
   doCommentC (p, n^.impF.com.body) ;
   outText (p, "\n") ;
   outputFile := mcStream.openFrag (3) ;  (* third fragment.  *)
   IF getExtendedOpaque ()
   THEN
      doP := p ;
      (* ForeachIndiceInIndexDo (n^.impF.importedModules, doIncludeC) ; *)

      includeExternals (n) ;
      foreachModuleDo (n, runSimplifyTypes) ;
      printf ("/*  --extended-opaque seen therefore no #include will be used and everything will be declared in full.  */\n") ;
      foreachDefModuleDo (runIncludeDefConstType) ;
      includeDefVarProcedure (n) ;
      outDeclsImpC (p, n^.impF.decls) ;
      foreachDefModuleDo (runPrototypeDefC)
   ELSE
      s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
      (* we don't want to include the .h file for this implementation module.  *)
      print (p, "#define _") ; prints (p, s) ; print (p, "_H\n") ;
      print (p, "#define _") ; prints (p, s) ; print (p, "_C\n\n") ;
      s := KillString (s) ;

      doP := p ;
      ForeachIndiceInIndexDo (n^.impF.importedModules, doIncludeC) ;
      print (p, "\n") ;
      includeDefConstType (n) ;
      includeDefVarProcedure (n) ;
      outDeclsImpC (p, n^.impF.decls) ;

      defModule := lookupDef (getSymName (n)) ;
      IF defModule # NIL
      THEN
         runPrototypeDefC (defModule)
      END
   END ;

   ForeachIndiceInIndexDo (n^.impF.decls.procedures, doPrototypeC) ;

   outProceduresC (p, n^.impF.decls) ;
   outImpInitC (p, n) ;

   outputFile := mcStream.openFrag (2) ;  (* second fragment.  *)
   keyc.genConfigSystem (p) ;
   keyc.genDefs (p)
END outImpC ;


(*
   outDeclsModuleC -
*)

PROCEDURE outDeclsModuleC (p: pretty; s: scopeT) ;
BEGIN
   simplifyTypes (s) ;
   includeConstType (s) ;

   doP := p ;

   topologicallyOut (doConstC, doTypesC, doVarC,
                     outputPartial,
                     doNone, doCompletePartialC, doNone) ;

   (* try and output types, constants before variables and procedures.  *)
   includeVarProcedure (s) ;

   topologicallyOut (doConstC, doTypesC, doVarC,
                     outputPartial,
                     doNone, doCompletePartialC, doNone) ;

   ForeachIndiceInIndexDo (s.procedures, doPrototypeC)
END outDeclsModuleC ;


(*
   outModuleInitC -
*)

PROCEDURE outModuleInitC (p: pretty; n: node) ;
BEGIN
   outText (p, "\n") ;
   doExternCP (p) ;
   outText (p, "void") ;
   setNeedSpace (p) ;
   outText (p, "_M2_") ;
   doFQNameC (p, n) ;
   outText (p, "_init") ;
   setNeedSpace (p) ;
   outText (p, "(__attribute__((unused)) int argc") ;
   outText (p, ",__attribute__((unused)) char *argv[]") ;
   outText (p, ",__attribute__((unused)) char *envp[])\n");
   p := outKc (p, "{\n") ;
   doStatementsC (p, n^.moduleF.beginStatements) ;
   p := outKc (p, "}\n") ;
   outText (p, "\n") ;
   doExternCP (p) ;
   outText (p, "void") ;
   setNeedSpace (p) ;
   outText (p, "_M2_") ;
   doFQNameC (p, n) ;
   outText (p, "_fini") ;
   setNeedSpace (p) ;
   outText (p, "(__attribute__((unused)) int argc") ;
   outText (p, ",__attribute__((unused)) char *argv[]") ;
   outText (p, ",__attribute__((unused)) char *envp[])\n");
   p := outKc (p, "{\n") ;
   doStatementsC (p, n^.moduleF.finallyStatements) ;
   p := outKc (p, "}\n")
END outModuleInitC ;


(*
   outModuleC -
*)

PROCEDURE outModuleC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   assert (isModule (n)) ;
   outputFile := mcStream.openFrag (1) ;  (* first fragment.  *)
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   print (p, "/* do not edit automatically generated by mc from ") ;
   prints (p, s) ; print (p, ".  */\n") ;
   writeGPLheader (outputFile) ;
   doCommentC (p, n^.moduleF.com.body) ;
   outText (p, "\n") ;
   outputFile := mcStream.openFrag (3) ;  (* third fragment.  *)
   IF getExtendedOpaque ()
   THEN
      doP := p ;
      includeExternals (n) ;
      foreachModuleDo (n, runSimplifyTypes) ;
      printf ("/*  --extended-opaque seen therefore no #include will be used and everything will be declared in full.  */\n") ;
      foreachDefModuleDo (runIncludeDefConstType) ;
      outDeclsModuleC (p, n^.moduleF.decls) ;
      foreachDefModuleDo (runPrototypeDefC)
   ELSE
      doP := p ;
      ForeachIndiceInIndexDo (n^.moduleF.importedModules, doIncludeC) ;
      print (p, "\n") ;
      outDeclsModuleC (p, n^.moduleF.decls)
   END ;

   ForeachIndiceInIndexDo (n^.moduleF.decls.procedures, doPrototypeC) ;

   outProceduresC (p, n^.moduleF.decls) ;
   outModuleInitC (p, n) ;

   outputFile := mcStream.openFrag (2) ;  (* second fragment.  *)
   keyc.genConfigSystem (p) ;
   keyc.genDefs (p)
END outModuleC ;


(*
   outC -
*)

PROCEDURE outC (p: pretty; n: node) ;
BEGIN
   keyc.enterScope (n) ;
   IF isDef (n)
   THEN
      outDefC (p, n)
   ELSIF isImp (n)
   THEN
      outImpC (p, n)
   ELSIF isModule (n)
   THEN
      outModuleC (p, n)
   ELSE
      HALT
   END ;
   keyc.leaveScope (n)
END outC ;


(*
   doIncludeM2 - include modules in module, n.
*)

PROCEDURE doIncludeM2 (n: node) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   print (doP, 'IMPORT ') ;
   prints (doP, s) ;
   print (doP, ' ;\n') ;
   s := KillString (s) ;

   IF isDef (n)
   THEN
      foreachNodeDo (n^.defF.decls.symbols, addDone)
   ELSIF isImp (n)
   THEN
      foreachNodeDo (n^.impF.decls.symbols, addDone)
   ELSIF isModule (n)
   THEN
      foreachNodeDo (n^.moduleF.decls.symbols, addDone)
   END
END doIncludeM2 ;


(*
   doConstM2 -
*)

PROCEDURE doConstM2 (n: node) ;
BEGIN
   print (doP, "CONST\n") ;
   doFQNameC (doP, n) ;
   setNeedSpace (doP) ;
   doExprC (doP, n^.constF.value) ;
   print (doP, '\n')
END doConstM2 ;


(*
   doProcTypeM2 -
*)

PROCEDURE doProcTypeM2 (p: pretty; n: node) ;
BEGIN
   outText (p, "proc type to do..")
END doProcTypeM2 ;


(*
   doRecordFieldM2 -
*)

PROCEDURE doRecordFieldM2 (p: pretty; f: node) ;
BEGIN
   doNameM2 (p, f) ;
   outText (p, ":") ;
   setNeedSpace (p) ;
   doTypeM2 (p, getType (f)) ;
   setNeedSpace (p)
END doRecordFieldM2 ;


(*
   doVarientFieldM2 -
*)

PROCEDURE doVarientFieldM2 (p: pretty; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   assert (isVarientField (n)) ;
   doNameM2 (p, n) ;
   outText (p, ":") ;
   setNeedSpace (p) ;
   i := LowIndice (n^.varientfieldF.listOfSons) ;
   t := HighIndice (n^.varientfieldF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.varientfieldF.listOfSons, i) ;
      IF isRecordField (q)
      THEN
         doRecordFieldM2 (p, q) ;
         outText (p, ";\n")
      ELSIF isVarient (q)
      THEN
         doVarientM2 (p, q) ;
         outText (p, ";\n")
      ELSE
         HALT
      END ;
      INC (i)
   END
END doVarientFieldM2 ;


(*
   doVarientM2 -
*)

PROCEDURE doVarientM2 (p: pretty; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   assert (isVarient (n)) ;
   outText (p, "CASE") ; setNeedSpace (p) ;
   IF n^.varientF.tag # NIL
   THEN
      IF isRecordField (n^.varientF.tag)
      THEN
         doRecordFieldM2 (p, n^.varientF.tag)
      ELSIF isVarientField (n^.varientF.tag)
      THEN
         doVarientFieldM2 (p, n^.varientF.tag)
      ELSE
         HALT
      END
   END ;
   setNeedSpace (p) ;
   outText (p, "OF\n") ;
   i := LowIndice (n^.varientF.listOfSons) ;
   t := HighIndice (n^.varientF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.varientF.listOfSons, i) ;
      IF isRecordField (q)
      THEN
         IF NOT q^.recordfieldF.tag
         THEN
            doRecordFieldM2 (p, q) ;
            outText (p, ";\n")
         END
      ELSIF isVarientField (q)
      THEN
         doVarientFieldM2 (p, q)
      ELSE
         HALT
      END ;
      INC (i)
   END ;
   outText (p, "END") ; setNeedSpace (p)
END doVarientM2 ;


(*
   doRecordM2 -
*)

PROCEDURE doRecordM2 (p: pretty; n: node) ;
VAR
   i, h: CARDINAL ;
   f   : node ;
BEGIN
   assert (isRecord (n)) ;
   p := outKm2 (p, "RECORD") ;
   i := LowIndice (n^.recordF.listOfSons) ;
   h := HighIndice (n^.recordF.listOfSons) ;
   outText (p, "\n") ;
   WHILE i<=h DO
      f := GetIndice (n^.recordF.listOfSons, i) ;
      IF isRecordField (f)
      THEN
         IF NOT f^.recordfieldF.tag
         THEN
            doRecordFieldM2 (p, f) ;
            outText (p, ";\n")
         END
      ELSIF isVarient (f)
      THEN
         doVarientM2 (p, f) ;
         outText (p, ";\n")
      ELSIF isVarientField (f)
      THEN
         doVarientFieldM2 (p, f)
      END ;
      INC (i)
   END ;
   p := outKm2 (p, "END") ; setNeedSpace (p)
END doRecordM2 ;


(*
   doPointerM2 -
*)

PROCEDURE doPointerM2 (p: pretty; n: node) ;
BEGIN
   outText (p, "POINTER TO") ;
   setNeedSpace (doP) ;
   doTypeM2 (p, getType (n)) ;
   setNeedSpace (p) ;
   outText (p, ";\n")
END doPointerM2 ;


(*
   doTypeAliasM2 -
*)

PROCEDURE doTypeAliasM2 (p: pretty; n: node) ;
BEGIN
   doTypeNameC (p, n) ;
   setNeedSpace (p) ;
   outText (doP, "=") ;
   setNeedSpace (p) ;
   doTypeM2 (p, getType (n)) ;
   setNeedSpace (p) ;
   outText (p, "\n")
END doTypeAliasM2 ;


(*
   doEnumerationM2 -
*)

PROCEDURE doEnumerationM2 (p: pretty; n: node) ;
VAR
   i, h: CARDINAL ;
   s   : node ;
   t   : String ;
BEGIN
   outText (p, "(") ;
   i := LowIndice (n^.enumerationF.listOfSons) ;
   h := HighIndice (n^.enumerationF.listOfSons) ;
   WHILE i <= h DO
      s := GetIndice (n^.enumerationF.listOfSons, i) ;
      doFQNameC (p, s) ;
      IF i < h
      THEN
         outText (p, ",") ; setNeedSpace (p)
      END ;
      INC (i)
   END ;
   outText (p, ")")
END doEnumerationM2 ;


(*
   doBaseM2 -
*)

PROCEDURE doBaseM2 (p: pretty; n: node) ;
BEGIN
   CASE n^.kind OF

   char,
   cardinal,
   longcard,
   shortcard,
   integer,
   longint,
   shortint,
   complex,
   longcomplex,
   shortcomplex,
   real,
   longreal,
   shortreal,
   bitset,
   boolean,
   proc        :  doNameM2 (p, n)

   END ;
   setNeedSpace (p)
END doBaseM2 ;


(*
   doSystemM2 -
*)

PROCEDURE doSystemM2 (p: pretty; n: node) ;
BEGIN
   CASE n^.kind OF

   address,
   loc,
   byte   ,
   word   ,
   csizet ,
   cssizet:  doNameM2 (p, n)

   END
END doSystemM2 ;


(*
   doTypeM2 -
*)

PROCEDURE doTypeM2 (p: pretty; n: node) ;
BEGIN
   IF isBase (n)
   THEN
      doBaseM2 (p, n)
   ELSIF isSystem (n)
   THEN
      doSystemM2 (p, n)
   ELSIF isType (n)
   THEN
      doTypeAliasM2 (p, n)
   ELSIF isProcType (n)
   THEN
      doProcTypeM2 (p, n)
   ELSIF isPointer (n)
   THEN
      doPointerM2 (p, n)
   ELSIF isEnumeration (n)
   THEN
      doEnumerationM2 (p, n)
   ELSIF isRecord (n)
   THEN
      doRecordM2 (p, n)
   END
END doTypeM2 ;


(*
   doTypesM2 -
*)

PROCEDURE doTypesM2 (n: node) ;
VAR
   m: node ;
BEGIN
   outText (doP, "TYPE\n") ;
   doTypeM2 (doP, n)
END doTypesM2 ;


(*
   doVarM2 -
*)

PROCEDURE doVarM2 (n: node) ;
BEGIN
   assert (isVar (n)) ;
   doNameC (doP, n) ;
   outText (doP, ":") ;
   setNeedSpace (doP) ;
   doTypeM2 (doP, getType (n)) ;
   setNeedSpace (doP) ;
   outText (doP, ";\n")
END doVarM2 ;


(*
   doVarsM2 -
*)

PROCEDURE doVarsM2 (n: node) ;
VAR
   m: node ;
BEGIN
   outText (doP, "VAR\n") ;
   doVarM2 (n)
END doVarsM2 ;


(*
   doTypeNameM2 -
*)

PROCEDURE doTypeNameM2 (p: pretty; n: node) ;
BEGIN
   doNameM2 (p, n)
END doTypeNameM2 ;


(*
   doParamM2 -
*)

PROCEDURE doParamM2 (p: pretty; n: node) ;
VAR
   ptype: node ;
   i    : Name ;
   c, t : CARDINAL ;
   l    : wlist ;
BEGIN
   assert (isParam (n)) ;
   ptype := getType (n) ;
   IF n^.paramF.namelist = NIL
   THEN
      doTypeNameM2 (p, ptype)
   ELSE
      assert (isIdentList (n^.paramF.namelist)) ;
      l := n^.paramF.namelist^.identlistF.names ;
      IF l=NIL
      THEN
         doTypeNameM2 (p, ptype)
      ELSE
         t := wlists.noOfItemsInList (l) ;
         c := 1 ;
         WHILE c <= t DO
            i := wlists.getItemFromList (l, c) ;
            setNeedSpace (p) ;
            doNamesC (p, i) ;
            IF c<t
            THEN
               outText (p, ',') ; setNeedSpace (p)
            END ;
            INC (c)
         END ;
         outText (p, ':') ; setNeedSpace (p) ;
         doTypeNameM2 (p, ptype)
      END
   END
END doParamM2 ;


(*
   doVarParamM2 -
*)

PROCEDURE doVarParamM2 (p: pretty; n: node) ;
VAR
   ptype: node ;
   i    : Name ;
   c, t : CARDINAL ;
   l    : wlist ;
BEGIN
   assert (isVarParam (n)) ;
   outText (p, 'VAR') ; setNeedSpace (p) ;
   ptype := getType (n) ;
   IF n^.varparamF.namelist = NIL
   THEN
      doTypeNameM2 (p, ptype)
   ELSE
      assert (isIdentList (n^.varparamF.namelist)) ;
      l := n^.varparamF.namelist^.identlistF.names ;
      IF l=NIL
      THEN
         doTypeNameM2 (p, ptype)
      ELSE
         t := wlists.noOfItemsInList (l) ;
         c := 1 ;
         WHILE c <= t DO
            i := wlists.getItemFromList (l, c) ;
            setNeedSpace (p) ;
            doNamesC (p, i) ;
            IF c<t
            THEN
               outText (p, ',') ; setNeedSpace (p)
            END ;
            INC (c)
         END ;
         outText (p, ':') ; setNeedSpace (p) ;
         doTypeNameM2 (p, ptype)
      END
   END
END doVarParamM2 ;


(*
   doParameterM2 -
*)

PROCEDURE doParameterM2 (p: pretty; n: node) ;
BEGIN
   IF isParam (n)
   THEN
      doParamM2 (p, n)
   ELSIF isVarParam (n)
   THEN
      doVarParamM2 (p, n)
   ELSIF isVarargs (n)
   THEN
      print (p, "...")
   END
END doParameterM2 ;


(*
   doPrototypeM2 -
*)

PROCEDURE doPrototypeM2 (n: node) ;
VAR
   i, h: CARDINAL ;
   p   : node ;
BEGIN
   assert (isProcedure (n)) ;
   noSpace (doP) ;

   doNameM2 (doP, n) ;
   setNeedSpace (doP) ;
   outText (doP, "(") ;
   i := LowIndice (n^.procedureF.parameters) ;
   h := HighIndice (n^.procedureF.parameters) ;
   WHILE i <= h DO
      p := GetIndice (n^.procedureF.parameters, i) ;
      doParameterM2 (doP, p) ;
      noSpace (doP) ;
      IF i < h
      THEN
         print (doP, ";") ; setNeedSpace (doP)
      END ;
      INC (i)
   END ;
   outText (doP, ")") ;
   IF n^.procedureF.returnType#NIL
   THEN
      setNeedSpace (doP) ;
      outText (doP, ":") ;
      doTypeM2 (doP, n^.procedureF.returnType) ; setNeedSpace (doP)
   END ;
   outText (doP, ";\n")
END doPrototypeM2 ;


(*
   outputPartialM2 - just writes out record, array, and proctypes.
                     No need for forward declarations in Modula-2
                     but we need to keep topological sort happy.
                     So when asked to output partial we emit the
                     full type for these types and then do nothing
                     when trying to complete partial to full.
*)

PROCEDURE outputPartialM2 (n: node) ;
VAR
   q: node ;
BEGIN
   q := getType (n) ;
   IF isRecord (q)
   THEN
      doTypeM2 (doP, n)
   ELSIF isArray (q)
   THEN
      doTypeM2 (doP, n)
   ELSIF isProcType (q)
   THEN
      doTypeM2 (doP, n)
   END
END outputPartialM2 ;


(*
   outDeclsDefM2 -
*)

PROCEDURE outDeclsDefM2 (p: pretty; s: scopeT) ;
BEGIN
   simplifyTypes (s) ;
   includeConstType (s) ;

   doP := p ;

   topologicallyOut (doConstM2, doTypesM2, doVarsM2,
                     outputPartialM2,
                     doNothing, doNothing, doNothing) ;

   includeVarProcedure (s) ;

   topologicallyOut (doConstM2, doTypesM2, doVarsM2,
                     outputPartialM2,
                     doNothing, doNothing, doNothing) ;

   ForeachIndiceInIndexDo (s.procedures, doPrototypeM2)
END outDeclsDefM2 ;


(*
   outDefM2 -
*)

PROCEDURE outDefM2 (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (getSource (n))) ;
   print (p, "(* automatically created by mc from ") ; prints (p, s) ; print (p, ".  *)\n\n") ;
   s := KillString (s) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   print (p, "DEFINITION MODULE ") ; prints (p, s) ; print (p, " ;\n\n") ;

   doP := p ;
   ForeachIndiceInIndexDo (n^.defF.importedModules, doIncludeM2) ;

   print (p, "\n") ;

   outDeclsDefM2 (p, n^.defF.decls) ;

   print (p, "\n") ;
   print (p, "END ") ;
   prints (p, s) ;
   print (p, ".\n") ;
   s := KillString (s)
END outDefM2 ;


(*
   outDeclsImpM2 -
*)

PROCEDURE outDeclsImpM2 (p: pretty; s: scopeT) ;
BEGIN
   simplifyTypes (s) ;
   includeConstType (s) ;

   doP := p ;

   topologicallyOut (doConstM2, doTypesM2, doVarM2,
                     outputPartialM2,
                     doNothing, doNothing, doNothing) ;

   includeVarProcedure (s) ;

   topologicallyOut (doConstM2, doTypesM2, doVarsM2,
                     outputPartialM2,
                     doNothing, doNothing, doNothing) ;

   outText (p, "\n") ;
   ForeachIndiceInIndexDo (s.procedures, doPrototypeC)
END outDeclsImpM2 ;


(*
   outImpM2 -
*)

PROCEDURE outImpM2 (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (getSource (n))) ;
   print (p, "(* automatically created by mc from ") ; prints (p, s) ; print (p, ".  *)\n\n") ;
   print (p, "IMPLEMENTATION MODULE ") ; prints (p, s) ; print (p, " ;\n\n") ;

   doP := p ;
   ForeachIndiceInIndexDo (n^.impF.importedModules, doIncludeM2) ;
   print (p, "\n") ;

   includeDefConstType (n) ;
   outDeclsImpM2 (p, n^.impF.decls) ;

   print (p, "\n") ;
   print (p, "END ") ;
   prints (p, s) ;
   print (p, ".\n") ;

   s := KillString (s)
END outImpM2 ;


(*
   outModuleM2 -
*)

PROCEDURE outModuleM2 (p: pretty; n: node) ;
BEGIN

END outModuleM2 ;


(*
   outM2 -
*)

PROCEDURE outM2 (p: pretty; n: node) ;
BEGIN
   IF isDef (n)
   THEN
      outDefM2 (p, n)
   ELSIF isImp (n)
   THEN
      outImpM2 (p, n)
   ELSIF isModule (n)
   THEN
      outModuleM2 (p, n)
   ELSE
      HALT
   END
END outM2 ;


(*
   out - walks the tree of node declarations for the main module
         and writes the output to the outputFile specified in
         mcOptions.  It outputs the declarations in the language
         specified above.
*)

PROCEDURE out ;
VAR
   p: pretty ;
BEGIN
   openOutput ;
   p := initPretty (write, writeln) ;
   CASE lang OF

   ansiC :  outC (p, getMainModule ()) |
   ansiCP:  outC (p, getMainModule ()) |
   pim4  :  outM2 (p, getMainModule ())

   END ;
   closeOutput
END out ;


(*
   setLangC -
*)

PROCEDURE setLangC ;
BEGIN
   lang := ansiC
END setLangC ;


(*
   setLangCP -
*)

PROCEDURE setLangCP ;
BEGIN
   lang := ansiCP ;
   keyc.cp
END setLangCP ;


(*
   setLangM2 -
*)

PROCEDURE setLangM2 ;
BEGIN
   lang := pim4
END setLangM2 ;


(*
   addDone - adds node, n, to the doneQ.
*)

PROCEDURE addDone (n: node) ;
BEGIN
   alists.includeItemIntoList (doneQ, n)
END addDone ;


(*
   addDoneDef - adds node, n, to the doneQ providing
                it is not an opaque of the main module we are compiling.
*)

PROCEDURE addDoneDef (n: node) ;
BEGIN
   IF isDef (n)
   THEN
      addDone (n) ;
      RETURN
   END ;
   IF (NOT isDef (n)) AND (lookupImp (getSymName (getScope (n))) = getMainModule ())
   THEN
      metaError1 ('cyclic dependancy found between another module using {%1ad} from the definition module of the implementation main being compiled, use the --extended-opaque option to compile', n) ;
      flushErrors ;
      errorAbort0 ('terminating compilation')
   ELSE
      addDone (n)
   END
END addDoneDef ;


(*
   dbgAdd -
*)

PROCEDURE dbgAdd (l: alist; n: node) : node ;
BEGIN
   IF n#NIL
   THEN
      alists.includeItemIntoList (l, n)
   END ;
   RETURN n
END dbgAdd ;


(*
   dbgType -
*)

PROCEDURE dbgType (l: alist; n: node) ;
VAR
   t: node ;
BEGIN
   t := dbgAdd (l, getType (n)) ;
   out1 ("<%s type", n) ;
   IF t = NIL
   THEN
      out0 (", type = NIL\n")
   ELSE
      out1 (", type = %s>\n", t)
   END
END dbgType ;


(*
   dbgPointer -
*)

PROCEDURE dbgPointer (l: alist; n: node) ;
VAR
   t: node ;
BEGIN
   t := dbgAdd (l, getType (n)) ;
   out1 ("<%s pointer", n) ;
   out1 (" to %s>\n", t)
END dbgPointer ;


(*
   dbgRecord -
*)

PROCEDURE dbgRecord (l: alist; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   out1 ("<%s record:\n", n) ;
   i := LowIndice (n^.recordF.listOfSons) ;
   t := HighIndice (n^.recordF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.recordF.listOfSons, i) ;
      IF isRecordField (q)
      THEN
         out1 (" <recordfield %s", q)
      ELSIF isVarientField (q)
      THEN
         out1 (" <varientfield %s", q)
      ELSIF isVarient (q)
      THEN
         out1 (" <varient %s", q)
      ELSE
         HALT
      END ;
      q := dbgAdd (l, getType (q)) ;
      out1 (": %s>\n", q) ;
      INC (i)
   END ;
   outText (doP, ">\n")
END dbgRecord ;


(*
   dbgVarient -
*)

PROCEDURE dbgVarient (l: alist; n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   out1 ("<%s varient: ", n) ;
   out1 ("tag %s", n^.varientF.tag) ;
   q := getType (n^.varientF.tag) ;
   IF q=NIL
   THEN
      outText (doP, "\n")
   ELSE
      out1 (": %s\n", q) ;
      q := dbgAdd (l, q)
   END ;
   i := LowIndice (n^.varientF.listOfSons) ;
   t := HighIndice (n^.varientF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.varientF.listOfSons, i) ;
      IF isRecordField (q)
      THEN
         out1 (" <recordfield %s", q)
      ELSIF isVarientField (q)
      THEN
         out1 (" <varientfield %s", q)
      ELSIF isVarient (q)
      THEN
         out1 (" <varient %s", q)
      ELSE
         HALT
      END ;
      q := dbgAdd (l, getType (q)) ;
      out1 (": %s>\n", q) ;
      INC (i)
   END ;
   outText (doP, ">\n")
END dbgVarient ;


(*
   dbgEnumeration -
*)

PROCEDURE dbgEnumeration (l: alist; n: node) ;
VAR
   e   : node ;
   i, h: CARDINAL ;
BEGIN
   outText (doP, "< enumeration ") ;
   i := LowIndice (n^.enumerationF.listOfSons) ;
   h := HighIndice (n^.enumerationF.listOfSons) ;
   WHILE i<=h DO
      e := GetIndice (n^.enumerationF.listOfSons, i) ;
      out1 ("%s, ", e) ;
      INC (i)
   END ;
   outText (doP, ">\n")
END dbgEnumeration ;


(*
   dbgVar -
*)

PROCEDURE dbgVar (l: alist; n: node) ;
VAR
   t: node ;
BEGIN
   t := dbgAdd (l, getType (n)) ;
   out1 ("<%s var", n) ;
   out1 (", type = %s>\n", t)
END dbgVar ;


(*
   dbgSubrange -
*)

PROCEDURE dbgSubrange (l: alist; n: node) ;
BEGIN
   IF n^.subrangeF.low = NIL
   THEN
      out1 ('%s', n^.subrangeF.type)
   ELSE
      out1 ('[%s', n^.subrangeF.low) ;
      out1 ('..%s]', n^.subrangeF.high)
   END
END dbgSubrange ;


(*
   dbgArray -
*)

PROCEDURE dbgArray (l: alist; n: node) ;
VAR
   t: node ;
BEGIN
   t := dbgAdd (l, getType (n)) ;
   out1 ("<%s array ", n) ;
   IF n^.arrayF.subr # NIL
   THEN
      dbgSubrange (l, n^.arrayF.subr)
   END ;
   out1 (" of %s>\n", t)
END dbgArray ;


(*
   doDbg -
*)

PROCEDURE doDbg (l: alist; n: node) ;
BEGIN
   IF n=NIL
   THEN
      (* do nothing.  *)
   ELSIF isSubrange (n)
   THEN
      dbgSubrange (l, n)
   ELSIF isType (n)
   THEN
      dbgType (l, n)
   ELSIF isRecord (n)
   THEN
      dbgRecord (l, n)
   ELSIF isVarient (n)
   THEN
      dbgVarient (l, n)
   ELSIF isEnumeration (n)
   THEN
      dbgEnumeration (l, n)
   ELSIF isPointer (n)
   THEN
      dbgPointer (l, n)
   ELSIF isArray (n)
   THEN
      dbgArray (l, n)
   ELSIF isVar (n)
   THEN
      dbgVar (l, n)
   END
END doDbg ;


(*
   dbg -
*)

PROCEDURE dbg (n: node) ;
VAR
   l: alist ;
   o: pretty ;
   f: File ;
   s: String ;
   i: CARDINAL ;
BEGIN
   o := doP ;
   f := outputFile ;
   outputFile := StdOut ;
   doP := initPretty (write, writeln) ;

   l := alists.initList () ;
   alists.includeItemIntoList (l, n) ;
   i := 1 ;
   out1 ("dbg (%s)\n", n) ;
   REPEAT
      n := alists.getItemFromList (l, i) ;
      doDbg (l, n) ;
      INC (i)
   UNTIL i>alists.noOfItemsInList (l) ;
   doP := o ;
   outputFile := f
END dbg ;


(*
   makeStatementSequence - create and return a statement sequence node.
*)

PROCEDURE makeStatementSequence () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (stmtseq) ;
   n^.stmtF.statements := InitIndex (1) ;
   RETURN n
END makeStatementSequence ;


(*
   addStatement - adds node, n, as a statement to statememt sequence, s.
*)

PROCEDURE addStatement (s: node; n: node) ;
BEGIN
   IF n#NIL
   THEN
      assert (isStatementSequence (s)) ;
      PutIndice (s^.stmtF.statements, HighIndice (s^.stmtF.statements) + 1, n) ;
      IF isIntrinsic (n) AND (n^.intrinsicF.postUnreachable)
      THEN
         n^.intrinsicF.postUnreachable := FALSE ;
         addStatement (s, makeIntrinsicProc (unreachable, 0, NIL))
      END
   END
END addStatement ;


(*
   isStatementSequence - returns TRUE if node, n, is a statement sequence.
*)

PROCEDURE isStatementSequence (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = stmtseq
END isStatementSequence ;


(*
   addGenericBody - adds comment node to funccall, return, assignment
                    nodes.
*)

PROCEDURE addGenericBody (n, c: node);
BEGIN
   CASE n^.kind OF

   unreachable,
   throw,
   halt,
   new,
   dispose,
   inc,
   dec,
   incl,
   excl      :  n^.intrinsicF.intrinsicComment.body := c |
   funccall  :  n^.funccallF.funccallComment.body := c |
   return    :  n^.returnF.returnComment.body := c |
   assignment:  n^.assignmentF.assignComment.body := c |
   module    :  n^.moduleF.com.body := c |
   def       :  n^.defF.com.body := c |
   imp       :  n^.impF.com.body := c

   ELSE
   END
END addGenericBody;


(*
   addGenericAfter - adds comment node to funccall, return, assignment
                     nodes.
*)

PROCEDURE addGenericAfter (n, c: node);
BEGIN
   CASE n^.kind OF

   unreachable,
   throw,
   halt,
   new,
   dispose,
   inc,
   dec,
   incl,
   excl      :  n^.intrinsicF.intrinsicComment.after := c |
   funccall  :  n^.funccallF.funccallComment.after := c |
   return    :  n^.returnF.returnComment.after := c |
   assignment:  n^.assignmentF.assignComment.after := c |
   module    :  n^.moduleF.com.after := c |
   def       :  n^.defF.com.after := c |
   imp       :  n^.impF.com.after := c

   ELSE
   END
END addGenericAfter ;


(*
   addCommentBody - adds a body comment to a statement sequence node.
*)

PROCEDURE addCommentBody (n: node) ;
VAR
   b: commentDesc ;
BEGIN
   IF n # NIL
   THEN
      b := getBodyComment () ;
      IF b # NIL
      THEN
         addGenericBody (n, makeCommentS (b))
      END
   END
END addCommentBody ;


(*
   addCommentAfter - adds an after comment to a statement sequence node.
*)

PROCEDURE addCommentAfter (n: node) ;
VAR
   a: commentDesc ;
BEGIN
   IF n # NIL
   THEN
      a := getAfterComment () ;
      IF a # NIL
      THEN
         addGenericAfter (n, makeCommentS (a))
      END
   END
END addCommentAfter ;


(*
   addIfComments - adds the, body, and, after, comments to if node, n.
*)

PROCEDURE addIfComments (n: node; body, after: node) ;
BEGIN
   assert (isIf (n)) ;
   n^.ifF.ifComment.after := after ;
   n^.ifF.ifComment.body := body
END addIfComments ;


(*
   addElseComments - adds the, body, and, after, comments to an, if, or an elsif, node, n.
*)

PROCEDURE addElseComments (n: node; body, after: node) ;
BEGIN
   assert (isIf (n) OR isElsif (n)) ;
   IF isIf (n)
   THEN
      n^.ifF.elseComment.after := after ;
      n^.ifF.elseComment.body := body
   ELSE
      n^.elsifF.elseComment.after := after ;
      n^.elsifF.elseComment.body := body
   END
END addElseComments ;


(*
   addIfEndComments - adds the, body, and, after, comments to an, if, node, n.
*)

PROCEDURE addIfEndComments (n: node; body, after: node) ;
BEGIN
   assert (isIf (n)) ;
   n^.ifF.endComment.after := after ;
   n^.ifF.endComment.body := body
END addIfEndComments ;


(*
   makeReturn - creates and returns a return node.
*)

PROCEDURE makeReturn () : node ;
VAR
   type,
   n   : node ;
BEGIN
   n := newNode (return) ;
   n^.returnF.exp := NIL ;
   IF isProcedure (getDeclScope ())
   THEN
      n^.returnF.scope := getDeclScope ()
   ELSE
      n^.returnF.scope := NIL
   END ;
   initPair (n^.returnF.returnComment) ;
   RETURN n
END makeReturn ;


(*
   isReturn - returns TRUE if node, n, is a return.
*)

PROCEDURE isReturn (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = return
END isReturn ;


(*
   putReturn - assigns node, e, as the expression on the return node.
*)

PROCEDURE putReturn (n: node; e: node) ;
BEGIN
   assert (isReturn (n)) ;
   n^.returnF.exp := e
END putReturn ;


(*
   makeWhile - creates and returns a while node.
*)

PROCEDURE makeWhile () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (while) ;
   n^.whileF.expr := NIL ;
   n^.whileF.statements := NIL ;
   initPair (n^.whileF.doComment) ;
   initPair (n^.whileF.endComment) ;
   RETURN n
END makeWhile ;


(*
   putWhile - places an expression, e, and statement sequence, s, into the while
              node, n.
*)

PROCEDURE putWhile (n: node; e, s: node) ;
BEGIN
   assert (isWhile (n)) ;
   n^.whileF.expr := e ;
   n^.whileF.statements := s
END putWhile ;


(*
   isWhile - returns TRUE if node, n, is a while.
*)

PROCEDURE isWhile (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = while
END isWhile ;


(*
   addWhileDoComment - adds body and after comments to while node, w.
*)

PROCEDURE addWhileDoComment (w: node; body, after: node) ;
BEGIN
   assert (isWhile (w)) ;
   w^.whileF.doComment.after := after ;
   w^.whileF.doComment.body := body
END addWhileDoComment ;


(*
   addWhileEndComment - adds body and after comments to the end of a while node, w.
*)

PROCEDURE addWhileEndComment (w: node; body, after: node) ;
BEGIN
   assert (isWhile (w)) ;
   w^.whileF.endComment.after := after ;
   w^.whileF.endComment.body := body
END addWhileEndComment ;


(*
   makeAssignment - creates and returns an assignment node.
                    The designator is, d, and expression, e.
*)

PROCEDURE makeAssignment (d, e: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (assignment) ;
   n^.assignmentF.des := d ;
   n^.assignmentF.expr := e ;
   initPair (n^.assignmentF.assignComment) ;
   RETURN n
END makeAssignment ;


(*
   isAssignment -
*)

PROCEDURE isAssignment (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = assignment
END isAssignment ;


(*
   putBegin - assigns statements, s, to be the normal part in
              block, b.  The block may be a procedure or module,
              or implementation node.
*)

PROCEDURE putBegin (b: node; s: node) ;
BEGIN
   assert (isImp (b) OR isProcedure (b) OR isModule (b)) ;
   CASE b^.kind OF

   imp      :  b^.impF.beginStatements := s |
   module   :  b^.moduleF.beginStatements := s |
   procedure:  b^.procedureF.beginStatements := s

   END
END putBegin ;


(*
   putFinally - assigns statements, s, to be the final part in
                block, b.  The block may be a module
                or implementation node.
*)

PROCEDURE putFinally (b: node; s: node) ;
BEGIN
   assert (isImp (b) OR isProcedure (b) OR isModule (b)) ;
   CASE b^.kind OF

   imp      :  b^.impF.finallyStatements := s |
   module   :  b^.moduleF.finallyStatements := s

   END
END putFinally ;


(*
   makeExit - creates and returns an exit node.
*)

PROCEDURE makeExit (l: node; n: CARDINAL) : node ;
VAR
   e: node ;
BEGIN
   assert (isLoop (l)) ;
   e := newNode (exit) ;
   e^.exitF.loop := l ;
   l^.loopF.labelno := n ;
   RETURN e
END makeExit ;


(*
   isExit - returns TRUE if node, n, is an exit.
*)

PROCEDURE isExit (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = exit
END isExit ;


(*
   makeLoop - creates and returns a loop node.
*)

PROCEDURE makeLoop () : node ;
VAR
   l: node ;
BEGIN
   l := newNode (loop) ;
   l^.loopF.statements := NIL ;
   l^.loopF.labelno := 0 ;
   RETURN l
END makeLoop ;


(*
   putLoop - places statement sequence, s, into loop, l.
*)

PROCEDURE putLoop (l, s: node) ;
BEGIN
   assert (isLoop (l)) ;
   l^.loopF.statements := s
END putLoop ;


(*
   isLoop - returns TRUE if, n, is a loop node.
*)

PROCEDURE isLoop (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = loop
END isLoop ;


(*
   makeComment - creates and returns a comment node.
*)

PROCEDURE makeComment (a: ARRAY OF CHAR) : node ;
VAR
   c: commentDesc ;
   s: String ;
BEGIN
   c := initComment (TRUE) ;
   s := InitString (a) ;
   addText (c, DynamicStrings.string (s)) ;
   s := KillString (s) ;
   RETURN makeCommentS (c)
END makeComment ;


(*
   makeCommentS - creates and returns a comment node.
*)

PROCEDURE makeCommentS (c: commentDesc) : node ;
VAR
   n: node ;
BEGIN
   IF c = NIL
   THEN
      RETURN NIL
   ELSE
      n := newNode (comment) ;
      n^.commentF.content := c ;
      RETURN n
   END
END makeCommentS ;


(*
   isComment - returns TRUE if node, n, is a comment.
*)

PROCEDURE isComment (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = comment
END isComment ;


(*
   initPair - initialise the commentPair, c.
*)

PROCEDURE initPair (VAR c: commentPair) ;
BEGIN
   c.after := NIL ;
   c.body := NIL
END initPair ;


(*
   makeIf - creates and returns an if node.  The if node
            will have expression, e, and statement sequence, s,
            as the then component.
*)

PROCEDURE makeIf (e, s: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (if) ;
   n^.ifF.expr := e ;
   n^.ifF.then := s ;
   n^.ifF.else := NIL ;
   n^.ifF.elsif := NIL ;
   initPair (n^.ifF.ifComment) ;
   initPair (n^.ifF.elseComment) ;
   initPair (n^.ifF.endComment) ;
   RETURN n
END makeIf ;


(*
   isIf - returns TRUE if, n, is an if node.
*)

PROCEDURE isIf (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = if
END isIf ;


(*
   makeElsif - creates and returns an elsif node.
               This node has an expression, e, and statement
               sequence, s.
*)

PROCEDURE makeElsif (i, e, s: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (elsif) ;
   n^.elsifF.expr := e ;
   n^.elsifF.then := s ;
   n^.elsifF.elsif := NIL ;
   n^.elsifF.else := NIL ;
   initPair (n^.elsifF.elseComment) ;
   assert (isIf (i) OR isElsif (i)) ;
   IF isIf (i)
   THEN
      i^.ifF.elsif := n ;
      assert (i^.ifF.else = NIL)
   ELSE
      i^.elsifF.elsif := n ;
      assert (i^.elsifF.else = NIL)
   END ;
   RETURN n
END makeElsif ;


(*
   isElsif - returns TRUE if node, n, is an elsif node.
*)

PROCEDURE isElsif (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = elsif
END isElsif ;


(*
   putElse - the else is grafted onto the if/elsif node, i,
             and the statement sequence will be, s.
*)

PROCEDURE putElse (i, s: node) ;
BEGIN
   assert (isIf (i) OR isElsif (i)) ;
   IF isIf (i)
   THEN
      assert (i^.ifF.elsif = NIL) ;
      assert (i^.ifF.else = NIL) ;
      i^.ifF.else := s
   ELSE
      assert (i^.elsifF.elsif = NIL) ;
      assert (i^.elsifF.else = NIL) ;
      i^.elsifF.else := s
   END
END putElse ;


(*
   makeFor - creates and returns a for node.
*)

PROCEDURE makeFor () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (for) ;
   n^.forF.des := NIL ;
   n^.forF.start := NIL ;
   n^.forF.end := NIL ;
   n^.forF.increment := NIL ;
   n^.forF.statements := NIL ;
   RETURN n
END makeFor ;


(*
   isFor - returns TRUE if node, n, is a for node.
*)

PROCEDURE isFor (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = for
END isFor ;


(*
   putFor - assigns the fields of the for node with
            ident, i,
            start, s,
            end, e,
            increment, i,
            statements, sq.
*)

PROCEDURE putFor (f, i, s, e, b, sq: node) ;
BEGIN
   assert (isFor (f)) ;
   f^.forF.des := i ;
   f^.forF.start := s ;
   f^.forF.end := e ;
   f^.forF.increment := b ;
   f^.forF.statements := sq
END putFor ;


(*
   makeRepeat - creates and returns a repeat node.
*)

PROCEDURE makeRepeat () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (repeat) ;
   n^.repeatF.expr := NIL ;
   n^.repeatF.statements := NIL ;
   initPair (n^.repeatF.repeatComment) ;
   initPair (n^.repeatF.untilComment) ;
   RETURN n
END makeRepeat ;


(*
   isRepeat - returns TRUE if node, n, is a repeat node.
*)

PROCEDURE isRepeat (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = repeat
END isRepeat ;


(*
   putRepeat - places statements, s, and expression, e, into
               repeat statement, n.
*)

PROCEDURE putRepeat (n, s, e: node) ;
BEGIN
   n^.repeatF.expr := e ;
   n^.repeatF.statements := s
END putRepeat ;


(*
   addRepeatComment - adds body and after comments to repeat node, r.
*)

PROCEDURE addRepeatComment (r: node; body, after: node) ;
BEGIN
   assert (isRepeat (r)) ;
   r^.repeatF.repeatComment.after := after ;
   r^.repeatF.repeatComment.body := body
END addRepeatComment ;


(*
   addUntilComment - adds body and after comments to the until section of a repeat node, r.
*)

PROCEDURE addUntilComment (r: node; body, after: node) ;
BEGIN
   assert (isRepeat (r)) ;
   r^.repeatF.untilComment.after := after ;
   r^.repeatF.untilComment.body := body
END addUntilComment ;


(*
   makeCase - builds and returns a case statement node.
*)

PROCEDURE makeCase () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (case) ;
   n^.caseF.expression := NIL ;
   n^.caseF.caseLabelList := InitIndex (1) ;
   n^.caseF.else := NIL ;
   RETURN n
END makeCase ;


(*
   isCase - returns TRUE if node, n, is a case statement.
*)

PROCEDURE isCase (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = case
END isCase ;


(*
   putCaseExpression - places expression, e, into case statement, n.
                       n is returned.
*)

PROCEDURE putCaseExpression (n: node; e: node) : node ;
BEGIN
   assert (isCase (n)) ;
   n^.caseF.expression := e ;
   RETURN n
END putCaseExpression ;


(*
   putCaseElse - places else statement, e, into case statement, n.
                 n is returned.
*)

PROCEDURE putCaseElse (n: node; e: node) : node ;
BEGIN
   assert (isCase (n)) ;
   n^.caseF.else := e ;
   RETURN n
END putCaseElse ;


(*
   putCaseStatement - places a caselist, l, and associated
                      statement sequence, s, into case statement, n.
                      n is returned.
*)

PROCEDURE putCaseStatement (n: node; l: node; s: node) : node ;
BEGIN
   assert (isCase (n)) ;
   assert (isCaseList (l)) ;
   IncludeIndiceIntoIndex (n^.caseF.caseLabelList, makeCaseLabelList (l, s)) ;
   RETURN n
END putCaseStatement ;


(*
   makeCaseLabelList - creates and returns a caselabellist node.
*)

PROCEDURE makeCaseLabelList (l, s: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (caselabellist) ;
   n^.caselabellistF.caseList := l ;
   n^.caselabellistF.statements := s ;
   RETURN n
END makeCaseLabelList ;


(*
   isCaseLabelList - returns TRUE if, n, is a caselabellist.
*)

PROCEDURE isCaseLabelList (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = caselabellist
END isCaseLabelList ;


(*
   makeCaseList - creates and returns a case statement node.
*)

PROCEDURE makeCaseList () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (caselist) ;
   n^.caselistF.rangePairs := InitIndex (1) ;
   RETURN n
END makeCaseList ;


(*
   isCaseList - returns TRUE if, n, is a case list.
*)

PROCEDURE isCaseList (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = caselist
END isCaseList ;


(*
   putCaseRange - places the case range lo..hi into caselist, n.
*)

PROCEDURE putCaseRange (n: node; lo, hi: node) : node ;
BEGIN
   assert (isCaseList (n)) ;
   IncludeIndiceIntoIndex (n^.caselistF.rangePairs, makeRange (lo, hi)) ;
   RETURN n
END putCaseRange ;


(*
   makeRange - creates and returns a case range.
*)

PROCEDURE makeRange (lo, hi: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (range) ;
   n^.rangeF.lo := lo ;
   n^.rangeF.hi := hi ;
   RETURN n
END makeRange ;


(*
   isRange - returns TRUE if node, n, is a range.
*)

PROCEDURE isRange (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = range
END isRange ;


(*
   dupExplist -
*)

PROCEDURE dupExplist (n: node) : node ;
VAR
   m: node ;
   i: CARDINAL ;
BEGIN
   assert (isExpList (n)) ;
   m := makeExpList () ;
   i := LowIndice (n^.explistF.exp) ;
   WHILE i <= HighIndice (n^.explistF.exp) DO
      putExpList (m, dupExpr (GetIndice (n^.explistF.exp, i))) ;
      INC (i)
   END ;
   RETURN m
END dupExplist ;


(*
   dupArrayref -
*)

PROCEDURE dupArrayref (n: node) : node ;
BEGIN
   assert (isArrayRef (n)) ;
   RETURN makeArrayRef (dupExpr (n^.arrayrefF.array), dupExpr (n^.arrayrefF.index))
END dupArrayref ;


(*
   dupPointerref -
*)

PROCEDURE dupPointerref (n: node) : node ;
BEGIN
   assert (isPointerRef (n)) ;
   RETURN makePointerRef (dupExpr (n^.pointerrefF.ptr), dupExpr (n^.pointerrefF.field))
END dupPointerref ;


(*
   dupComponentref -
*)

PROCEDURE dupComponentref (n: node) : node ;
BEGIN
   assert (isComponentRef (n)) ;
   RETURN doMakeComponentRef (dupExpr (n^.componentrefF.rec), dupExpr (n^.componentrefF.field))
END dupComponentref ;


(*
   dupBinary -
*)

PROCEDURE dupBinary (n: node) : node ;
BEGIN
   (* assert (isBinary (n)) ; *)
   RETURN makeBinary (n^.kind,
                      dupExpr (n^.binaryF.left), dupExpr (n^.binaryF.right),
                      n^.binaryF.resultType)
END dupBinary ;


(*
   dupUnary -
*)

PROCEDURE dupUnary (n: node) : node ;
BEGIN
   (* assert (isUnary (n)) ; *)
   RETURN makeUnary (n^.kind, dupExpr (n^.unaryF.arg), n^.unaryF.resultType)
END dupUnary ;


(*
   dupFunccall -
*)

PROCEDURE dupFunccall (n: node) : node ;
VAR
   m: node ;
BEGIN
   assert (isFuncCall (n)) ;
   m := makeFuncCall (dupExpr (n^.funccallF.function), dupExpr (n^.funccallF.args)) ;
   m^.funccallF.type := n^.funccallF.type ;
   RETURN m
END dupFunccall ;


(*
   dupSetValue -
*)

PROCEDURE dupSetValue (n: node) : node ;
VAR
   m: node ;
   i: CARDINAL ;
BEGIN
   m := newNode (setvalue) ;
   m^.setvalueF.type := n^.setvalueF.type ;
   i := LowIndice (n^.setvalueF.values) ;
   WHILE i <= HighIndice (n^.setvalueF.values) DO
      m := putSetValue (m, dupExpr (GetIndice (n^.setvalueF.values, i))) ;
      INC (i)
   END ;
   RETURN m
END dupSetValue ;


(*
   dupExpr - duplicate the expression nodes, it does not duplicate
             variables, literals, constants but only the expression
             operators (including function calls and parameter lists).
*)

PROCEDURE dupExpr (n: node) : node ;
BEGIN
   IF n = NIL
   THEN
      RETURN NIL
   ELSE
      RETURN doDupExpr (n)
   END
END dupExpr ;


(*
   doDupExpr -
*)

PROCEDURE doDupExpr (n: node) : node ;
BEGIN
   assert (n # NIL) ;
   CASE n^.kind OF

   explist         : RETURN dupExplist (n) |
   exit,
   return,
   stmtseq,
   comment         : HALT |  (* should not be duplicating code.  *)
   length          : HALT |  (* length should have been converted into unary.  *)
   (* base constants.  *)
   nil,
   true,
   false,
   (* system types.  *)
   address,
   loc,
   byte,
   word,
   csizet,
   cssizet,
   (* base types.  *)
   boolean,
   proc,
   char,
   integer,
   cardinal,
   longcard,
   shortcard,
   longint,
   shortint,
   real,
   longreal,
   shortreal,
   bitset,
   ztype,
   rtype,
   complex,
   longcomplex,
   shortcomplex     :  RETURN n |
   (* language features and compound type attributes.  *)
   type,
   record,
   varient,
   var,
   enumeration,
   subrange,
   subscript,
   array,
   string,
   const,
   literal,
   varparam,
   param,
   varargs,
   optarg,
   pointer,
   recordfield,
   varientfield,
   enumerationfield,
   set,
   proctype        :  RETURN n |
   (* blocks.  *)
   procedure,
   def,
   imp,
   module          :  RETURN n |
   (* statements.  *)
   loop,
   while,
   for,
   repeat,
   case,
   caselabellist,
   caselist,
   range,
   if,
   elsif,
   assignment      :  RETURN n |
   (* expressions.  *)
   arrayref        :  RETURN dupArrayref (n) |
   pointerref      :  RETURN dupPointerref (n) |
   componentref    :  RETURN dupComponentref (n) |
   cmplx,
   and,
   or,
   equal,
   notequal,
   less,
   greater,
   greequal,
   lessequal,
   cast,
   val,
   plus,
   sub,
   div,
   mod,
   mult,
   divide,
   in              :  RETURN dupBinary (n) |
   re,
   im,
   constexp,
   deref,
   abs,
   chr,
   cap,
   high,
   float,
   trunc,
   ord,
   not,
   neg,
   adr,
   size,
   tsize,
   min,
   max             :  RETURN dupUnary (n) |
   identlist       :  RETURN n |
   vardecl         :  RETURN n |
   funccall        :  RETURN dupFunccall (n) |
   setvalue        :  RETURN dupSetValue (n)

   END
END doDupExpr ;


(*
   setNoReturn - sets noreturn field inside procedure.
*)

PROCEDURE setNoReturn (n: node; value: BOOLEAN) ;
BEGIN
   assert (n#NIL) ;
   assert (isProcedure (n)) ;
   IF n^.procedureF.noreturnused AND (n^.procedureF.noreturn # value)
   THEN
      metaError1 ('{%1DMad} definition module and implementation module have different <* noreturn *> attributes', n) ;
   END ;
   n^.procedureF.noreturn := value ;
   n^.procedureF.noreturnused := TRUE
END setNoReturn ;


(*
   makeSystem -
*)

PROCEDURE makeSystem ;
BEGIN
   systemN := lookupDef (makeKey ('SYSTEM')) ;

   addressN := makeBase (address) ;
   locN := makeBase (loc) ;
   byteN := makeBase (byte) ;
   wordN := makeBase (word) ;
   csizetN := makeBase (csizet) ;
   cssizetN := makeBase (cssizet) ;

   adrN := makeBase (adr) ;
   tsizeN := makeBase (tsize) ;
   throwN := makeBase (throw) ;

   enterScope (systemN) ;
   addressN := addToScope (addressN) ;
   locN := addToScope (locN) ;
   byteN := addToScope (byteN) ;
   wordN := addToScope (wordN) ;
   csizetN := addToScope (csizetN) ;
   cssizetN := addToScope (cssizetN) ;
   adrN := addToScope (adrN) ;
   tsizeN := addToScope (tsizeN) ;
   throwN := addToScope (throwN) ;

   assert (sizeN#NIL) ;  (* assumed to be built already.  *)
   sizeN := addToScope (sizeN) ;  (* also export size from system.  *)
   leaveScope ;

   addDone (addressN) ;
   addDone (locN) ;
   addDone (byteN) ;
   addDone (wordN) ;
   addDone (csizetN) ;
   addDone (cssizetN)
END makeSystem ;


(*
   makeM2rts -
*)

PROCEDURE makeM2rts ;
BEGIN
   m2rtsN := lookupDef (makeKey ('M2RTS'))
END makeM2rts ;


(*
   makeBitnum -
*)

PROCEDURE makeBitnum () : node ;
VAR
   b: node ;
BEGIN
   b := newNode (subrange) ;
   b^.subrangeF.type := NIL ;
   b^.subrangeF.scope := NIL ;
   b^.subrangeF.low := lookupConst (b, makeKey ('0')) ;
   b^.subrangeF.high := lookupConst (b, makeKey ('31')) ;
   RETURN b
END makeBitnum ;


(*
   makeBaseSymbols -
*)

PROCEDURE makeBaseSymbols ;
BEGIN
   baseSymbols := initTree () ;

   booleanN := makeBase (boolean) ;
   charN := makeBase (char) ;
   procN := makeBase (proc) ;
   cardinalN := makeBase (cardinal) ;
   longcardN := makeBase (longcard) ;
   shortcardN := makeBase (shortcard) ;
   integerN := makeBase (integer) ;
   longintN := makeBase (longint) ;
   shortintN := makeBase (shortint) ;
   bitsetN := makeBase (bitset) ;
   bitnumN := makeBitnum () ;
   ztypeN := makeBase (ztype) ;
   rtypeN := makeBase (rtype) ;
   complexN := makeBase (complex) ;
   longcomplexN := makeBase (longcomplex) ;
   shortcomplexN := makeBase (shortcomplex) ;
   realN := makeBase (real) ;
   longrealN := makeBase (longreal) ;
   shortrealN := makeBase (shortreal) ;

   nilN := makeBase (nil) ;
   trueN := makeBase (true) ;
   falseN := makeBase (false) ;

   sizeN := makeBase (size) ;
   minN := makeBase (min) ;
   maxN := makeBase (max) ;
   floatN := makeBase (float) ;
   truncN := makeBase (trunc) ;
   ordN := makeBase (ord) ;
   valN := makeBase (val) ;
   chrN := makeBase (chr) ;
   capN := makeBase (cap) ;
   absN := makeBase (abs) ;
   newN := makeBase (new) ;
   disposeN := makeBase (dispose) ;
   lengthN := makeBase (length) ;
   incN := makeBase (inc) ;
   decN := makeBase (dec) ;
   inclN := makeBase (incl) ;
   exclN := makeBase (excl) ;
   highN := makeBase (high) ;
   imN := makeBase (im) ;
   reN := makeBase (re) ;
   cmplxN := makeBase (cmplx) ;

   putSymKey (baseSymbols, makeKey ('BOOLEAN'), booleanN) ;
   putSymKey (baseSymbols, makeKey ('PROC'), procN) ;
   putSymKey (baseSymbols, makeKey ('CHAR'), charN) ;
   putSymKey (baseSymbols, makeKey ('CARDINAL'), cardinalN) ;
   putSymKey (baseSymbols, makeKey ('SHORTCARD'), shortcardN) ;
   putSymKey (baseSymbols, makeKey ('LONGCARD'), longcardN) ;
   putSymKey (baseSymbols, makeKey ('INTEGER'), integerN) ;
   putSymKey (baseSymbols, makeKey ('LONGINT'), longintN) ;
   putSymKey (baseSymbols, makeKey ('SHORTINT'), shortintN) ;
   putSymKey (baseSymbols, makeKey ('BITSET'), bitsetN) ;
   putSymKey (baseSymbols, makeKey ('REAL'), realN) ;
   putSymKey (baseSymbols, makeKey ('SHORTREAL'), shortrealN) ;
   putSymKey (baseSymbols, makeKey ('LONGREAL'), longrealN) ;
   putSymKey (baseSymbols, makeKey ('COMPLEX'), complexN) ;
   putSymKey (baseSymbols, makeKey ('LONGCOMPLEX'), longcomplexN) ;
   putSymKey (baseSymbols, makeKey ('SHORTCOMPLEX'), shortcomplexN) ;

   putSymKey (baseSymbols, makeKey ('NIL'), nilN) ;
   putSymKey (baseSymbols, makeKey ('TRUE'), trueN) ;
   putSymKey (baseSymbols, makeKey ('FALSE'), falseN) ;
   putSymKey (baseSymbols, makeKey ('SIZE'), sizeN) ;
   putSymKey (baseSymbols, makeKey ('MIN'), minN) ;
   putSymKey (baseSymbols, makeKey ('MAX'), maxN) ;
   putSymKey (baseSymbols, makeKey ('FLOAT'), floatN) ;
   putSymKey (baseSymbols, makeKey ('TRUNC'), truncN) ;
   putSymKey (baseSymbols, makeKey ('ORD'), ordN) ;
   putSymKey (baseSymbols, makeKey ('VAL'), valN) ;
   putSymKey (baseSymbols, makeKey ('CHR'), chrN) ;
   putSymKey (baseSymbols, makeKey ('CAP'), capN) ;
   putSymKey (baseSymbols, makeKey ('ABS'), absN) ;
   putSymKey (baseSymbols, makeKey ('NEW'), newN) ;
   putSymKey (baseSymbols, makeKey ('DISPOSE'), disposeN) ;
   putSymKey (baseSymbols, makeKey ('LENGTH'), lengthN) ;
   putSymKey (baseSymbols, makeKey ('INC'), incN) ;
   putSymKey (baseSymbols, makeKey ('DEC'), decN) ;
   putSymKey (baseSymbols, makeKey ('INCL'), inclN) ;
   putSymKey (baseSymbols, makeKey ('EXCL'), exclN) ;
   putSymKey (baseSymbols, makeKey ('HIGH'), highN) ;
   putSymKey (baseSymbols, makeKey ('CMPLX'), cmplxN) ;
   putSymKey (baseSymbols, makeKey ('RE'), reN) ;
   putSymKey (baseSymbols, makeKey ('IM'), imN) ;

   addDone (booleanN) ;
   addDone (charN) ;
   addDone (cardinalN) ;
   addDone (longcardN) ;
   addDone (shortcardN) ;
   addDone (integerN) ;
   addDone (longintN) ;
   addDone (shortintN) ;
   addDone (bitsetN) ;
   addDone (bitnumN) ;
   addDone (ztypeN) ;
   addDone (rtypeN) ;
   addDone (realN) ;
   addDone (longrealN) ;
   addDone (shortrealN) ;
   addDone (complexN) ;
   addDone (longcomplexN) ;
   addDone (shortcomplexN) ;
   addDone (procN) ;
   addDone (nilN) ;
   addDone (trueN) ;
   addDone (falseN)

END makeBaseSymbols ;


(*
   makeBuiltins -
*)

PROCEDURE makeBuiltins ;
BEGIN
   bitsperunitN := makeLiteralInt (makeKey ('8')) ;
   bitsperwordN := makeLiteralInt (makeKey ('32')) ;
   bitspercharN := makeLiteralInt (makeKey ('8')) ;
   unitsperwordN := makeLiteralInt (makeKey ('4')) ;

   addDone (bitsperunitN) ;
   addDone (bitsperwordN) ;
   addDone (bitspercharN) ;
   addDone (unitsperwordN)
END makeBuiltins ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   lang := ansiC ;
   outputFile := StdOut ;
   doP := initPretty (write, writeln) ;
   todoQ := alists.initList () ;
   partialQ := alists.initList () ;
   doneQ := alists.initList () ;
   modUniverse := initTree () ;
   defUniverse := initTree () ;
   modUniverseI := InitIndex (1) ;
   defUniverseI := InitIndex (1) ;
   scopeStack := InitIndex (1) ;
   makeBaseSymbols ;
   makeSystem ;
   makeBuiltins ;
   makeM2rts ;
   outputState := punct ;
   tempCount := 0 ;
   mustVisitScope := FALSE
END init ;


BEGIN
   init
END decl.
