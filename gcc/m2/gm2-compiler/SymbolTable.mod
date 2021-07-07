(* SymbolTable.mod provides access to the symbol table.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE SymbolTable ;


FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Debug IMPORT Assert ;
FROM libc IMPORT printf ;

IMPORT Indexing ;
FROM Indexing IMPORT InitIndex, InBounds, LowIndice, HighIndice, PutIndice, GetIndice ;
FROM Sets IMPORT Set, InitSet, IncludeElementIntoSet, IsElementInSet ;

FROM M2Options IMPORT Pedantic, ExtendedOpaque, DebugFunctionLineNumbers ;
FROM M2LexBuf IMPORT UnknownTokenNo, TokenToLineNo, FindFileNameFromToken ;

FROM M2ALU IMPORT InitValue, PtrToValue, PushCard, PopInto,
                  PushString, PushFrom, PushChar, PushInt,
                  IsSolved, IsValueConst ;

FROM M2Error IMPORT Error, NewError, ChainError, InternalError,
                    ErrorFormat0, ErrorFormat1, ErrorFormat2,
                    WriteFormat0, WriteFormat1, WriteFormat2, ErrorString,
                    ErrorAbort0, FlushErrors ;

FROM M2MetaError IMPORT MetaError1, MetaError2, MetaError3, MetaErrors1,
                        MetaErrorT0,
                        MetaErrorString1,
                        MetaErrorStringT0, MetaErrorStringT1,
                        MetaErrorT1 ;

FROM M2LexBuf IMPORT GetTokenNo ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;

FROM DynamicStrings IMPORT String, string, InitString,
                           InitStringCharStar, Mark, KillString, Length, ConCat,
                           Index, char ;

FROM Lists IMPORT List, InitList, GetItemFromList, PutItemIntoList,
                  IsItemInList, IncludeItemIntoList, NoOfItemsInList,
                  RemoveItemFromList, ForeachItemInListDo ;

FROM NameKey IMPORT Name, MakeKey, makekey, NulName, WriteKey, LengthKey, GetKey, KeyToCharStar ;

FROM SymbolKey IMPORT NulKey, SymbolTree,
                      InitTree,
                      GetSymKey, PutSymKey, DelSymKey, IsEmptyTree,
                      DoesTreeContainAny, ForeachNodeDo ;

FROM M2Base IMPORT MixTypes, InitBase, Char, Integer, LongReal,
                   Cardinal, LongInt, LongCard, ZType, RType ;

FROM M2System IMPORT Address ;
FROM m2decl IMPORT DetermineSizeOfConstant ;
FROM m2tree IMPORT Tree ;
FROM m2linemap IMPORT BuiltinsLocation ;
FROM StrLib IMPORT StrEqual ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule ;

FROM FormatStrings IMPORT HandleEscape ;

IMPORT Indexing ;


CONST
   DebugUnknowns        =  FALSE ;

   (*
      The Unbounded is a pseudo type used within the compiler
      to implement dynamic parameter arrays.  It is implmented
      as a record structure which has the following fields:

      RECORD
         _m2_contents: POINTER TO type ;
         _m2_high    : CARDINAL ;
      END ;
   *)

   UnboundedAddressName = "_m2_contents" ;
   UnboundedHighName    = "_m2_high_%d" ;

TYPE
   LRLists = ARRAY [RightValue..LeftValue] OF List ;

   TypeOfSymbol = (RecordSym, VarientSym, DummySym,
                   VarSym, EnumerationSym, SubrangeSym, ArraySym,
                   ConstStringSym, ConstVarSym, ConstLitSym,
                   VarParamSym, ParamSym, PointerSym,
                   UndefinedSym, TypeSym,
                   RecordFieldSym, VarientFieldSym, EnumerationFieldSym,
                   DefImpSym, ModuleSym, SetSym, ProcedureSym, ProcTypeSym,
                   SubscriptSym, UnboundedSym, GnuAsmSym, InterfaceSym,
                   ObjectSym, PartialUnboundedSym, TupleSym, OAFamilySym,
                   EquivSym, ErrorSym) ;

   Where = RECORD
              DefDeclared,
              ModDeclared,
              FirstUsed  : CARDINAL ;
           END ;

   PackedInfo = RECORD
                   IsPacked    : BOOLEAN ;    (* is this type packed?        *)
                   PackedEquiv : CARDINAL ;   (* the equivalent packed type  *)
                END ;

   PtrToAsmConstraint = POINTER TO AsmConstraint ;
   AsmConstraint = RECORD
                      name: Name ;
                      str : CARDINAL ;   (* regnames or constraints     *)
                      obj : CARDINAL ;   (* list of M2 syms             *)
                   END ;

   SymEquiv = RECORD
                 packedInfo: PackedInfo ;
                 nonPacked : CARDINAL ;
              END ;

   SymOAFamily = RECORD
                    MaxDimensions: CARDINAL ;
                    SimpleType   : CARDINAL ;
                    Dimensions   : Indexing.Index ;
                 END ;

   SymTuple = RECORD
                 At    : Where ;
                 nTuple: CARDINAL ;
                 list  : Indexing.Index ;
              END ;

   SymError     = RECORD
                     name      : Name ;
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;

   SymObject    = RECORD
                     name      : Name ;
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;

   SymUndefined = RECORD
                     name      : Name ;       (* Index into name array, name *)
                                              (* of record.                  *)
                     oafamily  : CARDINAL ;   (* The oafamily for this sym   *)
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;

   SymGnuAsm    = RECORD
                     String    : CARDINAL ;   (* (ConstString) the assembly  *)
                                              (* instruction.                *)
                     At        : Where ;      (* Where was sym declared/used *)
                     Inputs,
                     Outputs,
                     Trashed   : CARDINAL ;   (* The interface symbols.      *)
                     Volatile  : BOOLEAN ;    (* Declared as ASM VOLATILE ?  *)
                     Simple    : BOOLEAN ;    (* is a simple kind?           *)
                  END ;

   SymInterface = RECORD
                     Parameters: Indexing.Index ;
                                              (* regnames or constraints     *)
                                              (* list of M2 syms.            *)
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;

   SymVarient = RECORD
                   Size        : PtrToValue ; (* Size at runtime of symbol.  *)
                   ListOfSons  : List ;       (* ListOfSons contains a list  *)
                                              (* of SymRecordField and       *)
                                              (* SymVarients                 *)
                                              (* declared by the source      *)
                                              (* file.                       *)
                   DeclPacked  : BOOLEAN ;    (* Is this varient packed?     *)
                   DeclResolved: BOOLEAN ;    (* has we resolved packed?     *)
                   Parent      : CARDINAL ;   (* Points to the parent symbol *)
                   Varient     : CARDINAL ;   (* Index into symbol table to  *)
                                              (* determine the associated    *)
                                              (* varient symbol.             *)
                   tag         : CARDINAL ;   (* The tag of the varient      *)
                                              (* this can either be a type   *)
                                              (* or a varient field.         *)
                   Scope       : CARDINAL ;   (* Scope of declaration.       *)
                   At          : Where ;      (* Where was sym declared/used *)
               END ;

   SymRecord = RECORD
                  name         : Name ;       (* Index into name array, name *)
                                              (* of record.                  *)
                  LocalSymbols : SymbolTree ; (* Contains all record fields. *)
                  Size         : PtrToValue ; (* Size at runtime of symbol.  *)
                  ListOfSons   : List ;       (* ListOfSons contains a list  *)
                                              (* of SymRecordField and       *)
                                              (* SymVarients                 *)
                                              (* declared by the source      *)
                                              (* file.                       *)
                  Align        : CARDINAL ;   (* The alignment of this type. *)
                  DefaultAlign : CARDINAL ;   (* The default field alignment *)
                  DeclPacked   : BOOLEAN ;    (* Is this record packed?      *)
                  DeclResolved : BOOLEAN ;    (* has we resolved packed?     *)
                  oafamily     : CARDINAL ;   (* The oafamily for this sym.  *)
                  Parent       : CARDINAL ;   (* Points to the parent symbol *)
                  Scope        : CARDINAL ;   (* Scope of declaration.       *)
                  At           : Where ;      (* Where was sym declared/used *)
               END ;

   SymSubrange = RECORD
                    name        : Name ;       (* Index into name array, name *)
                                               (* of subrange.                *)
                    Low         : CARDINAL ;   (* Index to symbol for lower   *)
                    High        : CARDINAL ;   (* Index to symbol for higher  *)
                    Size        : PtrToValue ; (* Size of subrange type.      *)
                    Type        : CARDINAL ;   (* Index to type symbol for    *)
                                               (* the type of subrange.       *)
                    ConstLitTree: SymbolTree ; (* constants of this type.     *)
                    packedInfo  : PackedInfo ; (* the equivalent packed type  *)
                    oafamily    : CARDINAL ;   (* The oafamily for this sym   *)
                    Scope       : CARDINAL ;   (* Scope of declaration.       *)
                    At          : Where ;      (* Where was sym declared/used *)
                 END ;

   SymEnumeration =
                RECORD
                   name        : Name ;       (* Index into name array, name *)
                                              (* of enumeration.             *)
                   NoOfElements: CARDINAL ;   (* No elements in enumeration  *)
                   LocalSymbols: SymbolTree ; (* Contains all enumeration    *)
                                              (* fields.                     *)
                   Size        : PtrToValue ; (* Size at runtime of symbol.  *)
                   packedInfo  : PackedInfo ; (* the equivalent packed type  *)
                   oafamily    : CARDINAL ;   (* The oafamily for this sym   *)
                   Scope       : CARDINAL ;   (* Scope of declaration.       *)
                   At          : Where ;      (* Where was sym declared/used *)
                END ;

   SymArray = RECORD
                 name        : Name ;         (* Index into name array, name *)
                                              (* of array.                   *)
                 Subscript   : CARDINAL ;     (* the subscript for this      *)
                                              (* array.                      *)
                 Size        : PtrToValue ;   (* Size at runtime of symbol.  *)
                 Offset      : PtrToValue ;   (* Offset at runtime of symbol *)
                 Type        : CARDINAL ;     (* Type of the Array.          *)
                 Align       : CARDINAL ;     (* Alignment for this type.    *)
		 Large       : BOOLEAN ;      (* is this a large array?      *)
                 oafamily    : CARDINAL ;     (* The oafamily for this sym   *)
                 Scope       : CARDINAL ;     (* Scope of declaration.       *)
                 At          : Where ;        (* Where was sym declared/used *)
              END ;

  SymSubscript = RECORD
                    Type       : CARDINAL ;   (* Index to a subrange symbol. *)
                    Size       : PtrToValue ; (* Size of this indice in*Size *)
                    Offset     : PtrToValue ; (* Offset at runtime of symbol *)
                                              (* Pseudo ie: Offset+Size*i    *)
                                              (* 1..n. The array offset is   *)
                                              (* the real memory offset.     *)
                                              (* This offset allows the a[i] *)
                                              (* to be calculated without    *)
                                              (* the need to perform         *)
                                              (* subtractions when a[4..10]  *)
                                              (* needs to be indexed.        *)
                    At         : Where ;      (* Where was sym declared/used *)
                 END ;

   SymUnbounded = RECORD
                     Type       : CARDINAL ;  (* Index to Simple type symbol *)
                     Size       : PtrToValue ;(* Max No of words ever        *)
                                              (* passed to this type.        *)
                     RecordType : CARDINAL ;  (* Record type used to         *)
                                              (* implement the unbounded.    *)
                     Dimensions : CARDINAL ;  (* No of dimensions this
                                                 open array uses.            *)
                     Scope      : CARDINAL ;  (* Scope of declaration.       *)
                     At         : Where ;     (* Where was sym declared/used *)
                  END ;

   SymPartialUnbounded = RECORD
                            Type: CARDINAL ;  (* Index to Simple type symbol *)
                            NDim: CARDINAL ;  (* dimensions associated       *)
                         END ;

   SymProcedure
          = RECORD
               name          : Name ;       (* Index into name array, name   *)
                                            (* of procedure.                 *)
               ListOfParam   : List ;       (* Contains a list of all the    *)
                                            (* parameters in this procedure. *)
               ParamDefined  : BOOLEAN ;    (* Have the parameters been      *)
                                            (* defined yet?                  *)
               DefinedInDef  : BOOLEAN ;    (* Were the parameters defined   *)
                                            (* in the Definition module?     *)
                                            (* Note that this depends on     *)
                                            (* whether the compiler has read *)
                                            (* the .def or .mod first.       *)
                                            (* The second occurence is       *)
                                            (* compared to the first.        *)
               DefinedInImp  : BOOLEAN ;    (* Were the parameters defined   *)
                                            (* in the Implementation module? *)
                                            (* Note that this depends on     *)
                                            (* whether the compiler has read *)
                                            (* the .def or .mod first.       *)
                                            (* The second occurence is       *)
                                            (* compared to the first.        *)
               HasVarArgs    : BOOLEAN ;    (* Does this procedure use ... ? *)
               HasOptArg     : BOOLEAN ;    (* Does this procedure use [ ] ? *)
               OptArgInit    : CARDINAL ;   (* The optarg initial value.     *)
               IsBuiltin     : BOOLEAN ;    (* Was it declared __BUILTIN__ ? *)
               BuiltinName   : Name ;       (* name of equivalent builtin    *)
               IsInline      : BOOLEAN ;    (* Was is declared __INLINE__ ?  *)
               ReturnOptional: BOOLEAN ;    (* Is the return value optional? *)
               Unresolved    : SymbolTree ; (* All symbols currently         *)
                                            (* unresolved in this procedure. *)
               ScopeQuad     : CARDINAL ;   (* Index into quads for scope    *)
               StartQuad     : CARDINAL ;   (* Index into quads for start    *)
                                            (* of procedure.                 *)
               EndQuad       : CARDINAL ;   (* Index into quads for end of   *)
                                            (* procedure.                    *)
               Reachable     : BOOLEAN ;    (* Defines if procedure will     *)
                                            (* ever be called by the main    *)
                                            (* Module.                       *)
               SavePriority  : BOOLEAN ;    (* Does procedure need to save   *)
                                            (* and restore interrupts?       *)
               ReturnType    : CARDINAL ;   (* Return type for function.     *)
               Offset        : CARDINAL ;   (* Location of procedure used    *)
                                            (* in Pass 2 and if procedure    *)
                                            (* is a syscall.                 *)
               LocalSymbols: SymbolTree ;   (* Contains all symbols declared *)
                                            (* within this procedure.        *)
               EnumerationScopeList: List ;
                                            (* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visable within this scope.    *)
               ListOfVars    : List ;       (* List of variables in this     *)
                                            (* scope.                        *)
               ListOfProcs   : List ;       (* List of all procedures        *)
                                            (* declared within this          *)
                                            (* procedure.                    *)
               NamedObjects  : SymbolTree ; (* Names of all items declared.  *)
               Size          : PtrToValue ; (* Activation record size.       *)
               TotalParamSize: PtrToValue ; (* size of all parameters.       *)
               ExceptionFinally,
               ExceptionBlock: BOOLEAN ;    (* does it have an exception?    *)
               Scope         : CARDINAL ;   (* Scope of declaration.         *)
               ListOfModules : List ;       (* List of all inner modules.    *)
               Begin, End    : CARDINAL ;   (* Tokens marking the BEGIN END  *)
               At            : Where ;      (* Where was sym declared/used   *)
            END ;

   SymProcType
          = RECORD
               name          : Name ;       (* Index into name array, name   *)
                                            (* of procedure.                 *)
               ListOfParam   : List ;       (* Contains a list of all the    *)
                                            (* parameters in this procedure. *)
               HasVarArgs    : BOOLEAN ;    (* Does this proc type use ... ? *)
               HasOptArg     : BOOLEAN ;    (* Does this procedure use [ ] ? *)
               OptArgInit    : CARDINAL ;   (* The optarg initial value.     *)
               ReturnType    : CARDINAL ;   (* Return type for function.     *)
               ReturnOptional: BOOLEAN ;    (* Is the return value optional? *)
               Scope         : CARDINAL ;   (* Scope of declaration.         *)
               Size          : PtrToValue ; (* Runtime size of symbol.       *)
               TotalParamSize: PtrToValue ; (* size of all parameters.       *)
               oafamily      : CARDINAL ;   (* The oafamily for this sym     *)
               At            : Where ;      (* Where was sym declared/used   *)
            END ;

   SymParam = RECORD
                 name          : Name ;       (* Index into name array, name *)
                                              (* of param.                   *)
                 Type          : CARDINAL ;   (* Index to the type of param. *)
                 IsUnbounded   : BOOLEAN ;    (* ARRAY OF Type?              *)
                 ShadowVar     : CARDINAL ;   (* The local variable used to  *)
                                              (* shadow this parameter.      *)
                 At            : Where ;      (* Where was sym declared/used *)
              END ;

   SymVarParam = RECORD
                    name          : Name ;    (* Index into name array, name *)
                                              (* of param.                   *)
                    Type          : CARDINAL ;(* Index to the type of param. *)
                    IsUnbounded   : BOOLEAN ; (* ARRAY OF Type?              *)
                    ShadowVar     : CARDINAL ;(* The local variable used to  *)
                                              (* shadow this parameter.      *)
                    At            : Where ;   (* Where was sym declared/used *)
                 END ;

   ConstStringVariant = (m2str, cstr, m2nulstr, cnulstr) ;

   SymConstString
               = RECORD
                    name           : Name ;       (* Index into name array, name *)
                                                  (* of const.                   *)
                    Contents       : Name ;       (* Contents of the string.     *)
                    Length         : CARDINAL ;   (* StrLen (Contents)           *)
                    M2Variant,
                    NulM2Variant,
                    CVariant,
                    NulCVariant    : CARDINAL ;   (* variants of the same string *)
                    StringVariant  : ConstStringVariant ;
                    At             : Where ;      (* Where was sym declared/used *)
                 END ;

   SymConstLit = RECORD
                    name         : Name ;         (* Index into name array, name *)
                                                  (* of const.                   *)
                    Value        : PtrToValue ;   (* Value of the constant.      *)
                    Type         : CARDINAL ;     (* TYPE of constant, char etc  *)
                    IsSet        : BOOLEAN ;      (* is the constant a set?      *)
                    IsConstructor: BOOLEAN ;      (* is the constant a set?      *)
                    FromType     : CARDINAL ;     (* type is determined FromType *)
                    UnresFromType: BOOLEAN ;      (* is Type unresolved?         *)
                    At           : Where ;        (* Where was sym declared/used *)
                 END ;

   SymConstVar = RECORD
                    name         : Name ;     (* Index into name array, name *)
                                              (* of const.                   *)
                    Value        : PtrToValue ; (* Value of the constant     *)
                    Type         : CARDINAL ; (* TYPE of constant, char etc  *)
                    IsSet        : BOOLEAN ;  (* is the constant a set?      *)
                    IsConstructor: BOOLEAN ;  (* is the constant a set?      *)
                    FromType     : CARDINAL ; (* type is determined FromType *)
                    UnresFromType: BOOLEAN ;  (* is Type resolved?           *)
                    IsTemp       : BOOLEAN ;  (* is it a temporary?          *)
                    At           : Where ;    (* Where was sym declared/used *)
                 END ;

   SymVar = RECORD
               name          : Name ;         (* Index into name array, name *)
                                              (* of const.                   *)
               Type          : CARDINAL ;     (* Index to a type symbol.     *)
               BackType      : CARDINAL ;     (* specific back end symbol.   *)
               Size          : PtrToValue ;   (* Runtime size of symbol.     *)
               Offset        : PtrToValue ;   (* Offset at runtime of symbol *)
               AddrMode      : ModeOfAddr ;   (* Type of Addressing mode.    *)
               Scope         : CARDINAL ;     (* Scope of declaration.       *)
               AtAddress     : BOOLEAN ;      (* Is declared at address?     *)
               Address       : CARDINAL ;     (* Address at which declared   *)
               IsComponentRef: BOOLEAN ;      (* Is temporary referencing a  *)
                                              (* record field?               *)
               list          : Indexing.Index ;  (* the record and fields    *)
               IsTemp        : BOOLEAN ;      (* Is variable a temporary?    *)
               IsParam       : BOOLEAN ;      (* Is variable a parameter?    *)
               IsPointerCheck: BOOLEAN ;      (* Is variable used to         *)
                                              (* dereference a pointer?      *)
               IsWritten     : BOOLEAN ;      (* Is variable written to?     *)
               IsSSA         : BOOLEAN ;      (* Is variable a SSA?          *)
               At            : Where ;        (* Where was sym declared/used *)
               ReadUsageList,                 (* list of var read quads      *)
               WriteUsageList: LRLists ;      (* list of var write quads     *)
            END ;

   SymType = RECORD
                name        : Name ;          (* Index into name array, name *)
                                              (* of type.                    *)
                Type        : CARDINAL ;      (* Index to a type symbol.     *)
                IsHidden    : BOOLEAN ;       (* Was it declared as hidden?  *)
                ConstLitTree: SymbolTree ;    (* constants of this type.     *)
                Size        : PtrToValue ;    (* Runtime size of symbol.     *)
                packedInfo  : PackedInfo ;    (* the equivalent packed type  *)
                oafamily    : CARDINAL ;      (* The oafamily for this sym   *)
                Align       : CARDINAL ;      (* The alignment of this type  *)
                Scope       : CARDINAL ;      (* Scope of declaration.       *)
                At          : Where ;         (* Where was sym declared/used *)
             END ;

   SymPointer
           = RECORD
                name        : Name ;          (* Index into name array, name *)
                                              (* of pointer.                 *)
                Type        : CARDINAL ;      (* Index to a type symbol.     *)
                Size        : PtrToValue ;    (* Runtime size of symbol.     *)
                Align       : CARDINAL ;      (* The alignment of this type  *)
                ConstLitTree: SymbolTree ;    (* constants of this type.     *)
                oafamily    : CARDINAL ;      (* The oafamily for this sym   *)
                Scope       : CARDINAL ;      (* Scope of declaration.       *)
                At          : Where ;         (* Where was sym declared/used *)
             END ;

   SymRecordField =
             RECORD
                name      : Name ;            (* Index into name array, name *)
                                              (* of record field.            *)
                Type      : CARDINAL ;        (* Index to a type symbol.     *)
                Tag       : BOOLEAN ;         (* is the record field really  *)
                                              (* a varient tag?              *)
                Size      : PtrToValue ;      (* Runtime size of symbol.     *)
                Offset    : PtrToValue ;      (* Offset at runtime of symbol *)
                Parent    : CARDINAL ;        (* Index into symbol table to  *)
                                              (* determine the parent symbol *)
                                              (* for this record field. Used *)
                                              (* for BackPatching.           *)
                Varient   : CARDINAL ;        (* Index into symbol table to  *)
                                              (* determine the associated    *)
                                              (* varient symbol.             *)
                Align     : CARDINAL ;        (* The alignment of this type  *)
                Used      : BOOLEAN ;         (* pragma usused unsets this.  *)
                DeclPacked: BOOLEAN ;         (* Is this declared packed?    *)
                DeclResolved: BOOLEAN ;       (* has we resolved packed?     *)
                Scope     : CARDINAL ;        (* Scope of declaration.       *)
                At        : Where ;           (* Where was sym declared/used *)
             END ;

   SymVarientField =
             RECORD
                name     : Name ;             (* Index into name array, name *)
                                              (* of varient field (internal) *)
                Size     : PtrToValue ;       (* Runtime size of symbol.     *)
                Offset   : PtrToValue ;       (* Offset at runtime of symbol *)
                Parent   : CARDINAL ;         (* Index into symbol table to  *)
                                              (* determine the parent symbol *)
                                              (* for this record field. Used *)
                                              (* for BackPatching.           *)
                Varient  : CARDINAL ;         (* Index into symbol table to  *)
                                              (* determine the associated    *)
                                              (* varient symbol.             *)
                ListOfSons: List ;            (* Contains a list of the      *)
                                              (* RecordField symbols and     *)
                                              (* SymVarients                 *)
                DeclPacked: BOOLEAN ;         (* Is this varient field       *)
                                              (* packed?                     *)
                DeclResolved: BOOLEAN ;       (* is it resolved?             *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymEnumerationField =
             RECORD
                name     : Name ;             (* Index into name array, name *)
                                              (* of enumeration field.       *)
                Value    : PtrToValue ;       (* Enumeration field value.    *)
                Type     : CARDINAL ;         (* Index to the enumeration.   *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymSet  = RECORD
      	        name     : Name ;             (* Index into name array, name *)
                                              (* of set.                     *)
                Type     : CARDINAL ;         (* Index to a type symbol.     *)
      	       	     	      	       	      (* (subrange or enumeration).  *)
                packedInfo: PackedInfo ;      (* the equivalent packed type  *)
                ispacked : BOOLEAN ;
                Size     : PtrToValue ;       (* Runtime size of symbol.     *)
                oafamily : CARDINAL ;         (* The oafamily for this sym   *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymDefImp =
            RECORD
               name          : Name ;       (* Index into name array, name   *)
                                            (* of record field.              *)
               ExportQualifiedTree: SymbolTree ;
                                            (* Holds all the export          *)
                                            (* Qualified identifiers.        *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
               ExportUnQualifiedTree: SymbolTree ;
                                            (* Holds all the export          *)
                                            (* UnQualified identifiers.      *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
               ExportRequest : SymbolTree ; (* Contains all identifiers that *)
                                            (* have been requested by other  *)
                                            (* modules before this module    *)
                                            (* declared its export list.     *)
                                            (* This tree should be empty at  *)
                                            (* the end of the compilation.   *)
                                            (* Each time a symbol is         *)
                                            (* exported it is removed from   *)
                                            (* this list.                    *)
               IncludeList   : List ;       (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* modulename.Symbol             *)
               DefIncludeList: List ;       (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* in the definition module only *)
               ImportTree    : SymbolTree ; (* Contains all IMPORTed         *)
                                            (* identifiers.                  *)
               ExportUndeclared: SymbolTree ;
                                            (* ExportUndeclared contains all *)
                                            (* the identifiers which were    *)
                                            (* exported but have not yet     *)
                                            (* been declared.                *)
               NeedToBeImplemented: SymbolTree ;
                                            (* NeedToBeImplemented contains  *)
                                            (* the identifiers which have    *)
                                            (* been exported and declared    *)
                                            (* but have not yet been         *)
                                            (* implemented.                  *)
               LocalSymbols  : SymbolTree ; (* The LocalSymbols hold all the *)
                                            (* variables declared local to   *)
                                            (* the block. It contains the    *)
                                            (* IMPORT r ;                    *)
                                            (* FROM _ IMPORT x, y, x ;       *)
                                            (*    and also                   *)
                                            (* MODULE WeAreHere ;            *)
                                            (*    x y z visible by localsym  *)
                                            (*    MODULE Inner ;             *)
                                            (*       EXPORT x, y, z ;        *)
                                            (*    END Inner ;                *)
                                            (* END WeAreHere.                *)
               EnumerationScopeList: List ; (* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visible within this scope.    *)
               NamedObjects  : SymbolTree ; (* Names of all items declared.  *)
               NamedImports  : SymbolTree ; (* Names of items imported.      *)
               WhereImported : SymbolTree ; (* Sym to TokenNo where import   *)
                                            (* occurs. Error message use.    *)
               Priority      : CARDINAL ;   (* Priority of the module. This  *)
                                            (* is an index to a constant.    *)
               Unresolved    : SymbolTree ; (* All symbols currently         *)
                                            (* unresolved in this module.    *)
               StartQuad     : CARDINAL ;   (* Signify the initialization    *)
                                            (* code.                         *)
               EndQuad       : CARDINAL ;   (* EndQuad should point to a     *)
                                            (* goto quad.                    *)
               StartFinishQuad: CARDINAL ;  (* Signify the finalization      *)
                                            (* code.                         *)
               EndFinishQuad : CARDINAL ;   (* should point to a finish      *)
               FinallyFunction: Tree ;      (* The GCC function for finally  *)
               ExceptionFinally,
               ExceptionBlock: BOOLEAN ;    (* does it have an exception?    *)
               ContainsHiddenType: BOOLEAN ;(* True if this module           *)
                                            (* implements a hidden type.     *)
               ContainsBuiltin: BOOLEAN ;   (* Does the module define a      *)
                                            (* builtin procedure?            *)
               ForC          : BOOLEAN ;    (* Is it a definition for "C"    *)
               NeedExportList: BOOLEAN ;    (* Must user supply export list? *)
               ListOfVars    : List ;       (* List of variables in this     *)
                                            (* scope.                        *)
               ListOfProcs   : List ;       (* List of all procedures        *)
                                            (* declared within this module.  *)
               ListOfModules : List ;       (* List of all inner modules.    *)
               At            : Where ;      (* Where was sym declared/used *)
            END ;

   SymModule =
            RECORD
               name          : Name ;       (* Index into name array, name   *)
                                            (* of record field.              *)
               LocalSymbols  : SymbolTree ; (* The LocalSymbols hold all the *)
                                            (* variables declared local to   *)
                                            (* the block. It contains the    *)
                                            (* IMPORT r ;                    *)
                                            (* FROM _ IMPORT x, y, x ;       *)
                                            (*    and also                   *)
                                            (* MODULE WeAreHere ;            *)
                                            (*    x y z visible by localsym  *)
                                            (*    MODULE Inner ;             *)
                                            (*       EXPORT x, y, z ;        *)
                                            (*    END Inner ;                *)
                                            (* END WeAreHere.                *)
               ExportTree    : SymbolTree ; (* Holds all the exported        *)
                                            (* identifiers.                  *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
               IncludeList   : List ;       (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* modulename.Symbol             *)
               ImportTree    : SymbolTree ; (* Contains all IMPORTed         *)
                                            (* identifiers.                  *)
               ExportUndeclared: SymbolTree ;
                                            (* ExportUndeclared contains all *)
                                            (* the identifiers which were    *)
                                            (* exported but have not yet     *)
                                            (* been declared.                *)
               EnumerationScopeList: List ; (* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visable within this scope.    *)
               NamedObjects  : SymbolTree ; (* Names of all items declared.  *)
               NamedImports  : SymbolTree ; (* Names of items imported.      *)
               WhereImported : SymbolTree ; (* Sym to TokenNo where import   *)
                                            (* occurs. Error message use.    *)
               Scope         : CARDINAL ;   (* Scope of declaration.         *)
               Priority      : CARDINAL ;   (* Priority of the module. This  *)
                                            (* is an index to a constant.    *)
               Unresolved    : SymbolTree ; (* All symbols currently         *)
                                            (* unresolved in this module.    *)
               StartQuad     : CARDINAL ;   (* Signify the initialization    *)
                                            (* code.                         *)
               EndQuad       : CARDINAL ;   (* EndQuad should point to a     *)
                                            (* goto quad.                    *)
               StartFinishQuad: CARDINAL ;  (* Signify the finalization      *)
                                            (* code.                         *)
               EndFinishQuad : CARDINAL ;   (* should point to a finish      *)
               FinallyFunction: Tree ;      (* The GCC function for finally  *)
               ExceptionFinally,
               ExceptionBlock: BOOLEAN ;    (* does it have an exception?    *)
               ListOfVars    : List ;       (* List of variables in this     *)
                                            (* scope.                        *)
               ListOfProcs   : List ;       (* List of all procedures        *)
                                            (* declared within this module.  *)
               ListOfModules : List ;       (* List of all inner modules.    *)
               At            : Where ;      (* Where was sym declared/used   *)
            END ;

   SymDummy =
            RECORD
               NextFree     : CARDINAL ;    (* Link to the next free symbol. *)
            END ;


   Symbol = RECORD
               CASE SymbolType : TypeOfSymbol OF
                                            (* Determines the type of symbol *)

               OAFamilySym         : OAFamily         : SymOAFamily |
               ObjectSym           : Object           : SymObject |
               EquivSym            : Equiv            : SymEquiv |
               RecordSym           : Record           : SymRecord |
               VarientSym          : Varient          : SymVarient |
               VarSym              : Var              : SymVar |
               EnumerationSym      : Enumeration      : SymEnumeration |
               SubrangeSym         : Subrange         : SymSubrange |
               SubscriptSym        : Subscript        : SymSubscript |
               ArraySym            : Array            : SymArray |
               UnboundedSym        : Unbounded        : SymUnbounded |
               PartialUnboundedSym : PartialUnbounded : SymPartialUnbounded |
               ConstVarSym         : ConstVar         : SymConstVar |
               ConstLitSym         : ConstLit         : SymConstLit |
               ConstStringSym      : ConstString      : SymConstString |
               VarParamSym         : VarParam         : SymVarParam |
               ParamSym            : Param            : SymParam |
               ErrorSym            : Error            : SymError |
               UndefinedSym        : Undefined        : SymUndefined |
               TypeSym             : Type             : SymType |
               PointerSym          : Pointer          : SymPointer |
               RecordFieldSym      : RecordField      : SymRecordField |
               VarientFieldSym     : VarientField     : SymVarientField |
               EnumerationFieldSym : EnumerationField : SymEnumerationField |
               DefImpSym           : DefImp           : SymDefImp |
               ModuleSym           : Module           : SymModule |
               SetSym              : Set              : SymSet |
               ProcedureSym        : Procedure        : SymProcedure |
               ProcTypeSym         : ProcType         : SymProcType |
               GnuAsmSym           : GnuAsm           : SymGnuAsm |
               InterfaceSym        : Interface        : SymInterface |
               TupleSym            : Tuple            : SymTuple |
               DummySym            : Dummy            : SymDummy

               END
            END ;

   CallFrame = RECORD
                  Main  : CARDINAL ;  (* Main scope for insertions        *)
                  Search: CARDINAL ;  (* Search scope for symbol searches *)
                  Start : CARDINAL ;  (* ScopePtr value before StartScope *)
                                      (* was called.                      *)
               END ;

   PtrToSymbol = POINTER TO Symbol ;
   PtrToCallFrame = POINTER TO CallFrame ;

   CheckProcedure = PROCEDURE (CARDINAL) ;

VAR
   Symbols       : Indexing.Index ;       (* ARRAY [1..MaxSymbols] OF Symbol.   *)
   ScopeCallFrame: Indexing.Index ;       (* ARRAY [1..MaxScopes] OF CallFrame. *)
   FreeSymbol    : CARDINAL ;    (* The next free symbol indice.       *)
   DefModuleTree : SymbolTree ;
   ModuleTree    : SymbolTree ;  (* Tree of all modules ever used.     *)
   ConstLitStringTree
                 : SymbolTree ;  (* String Literal Constants only need *)
                                 (* to be declared once.               *)
   ConstLitTree  : SymbolTree ;  (* Numerical Literal Constants only   *)
                                 (* need to be declared once.          *)
   CurrentModule : CARDINAL ;    (* Index into symbols determining the *)
                                 (* current module being compiled.     *)
                                 (* This maybe an inner module.        *)
   MainModule    : CARDINAL ;    (* Index into symbols determining the *)
                                 (* module the user requested to       *)
                                 (* compile.                           *)
   FileModule    : CARDINAL ;    (* Index into symbols determining     *)
                                 (* which module (file) is being       *)
                                 (* compiled. (Maybe an import def)    *)
   ScopePtr      : CARDINAL ;    (* An index to the ScopeCallFrame.    *)
                                 (* ScopePtr determines the top of the *)
                                 (* ScopeCallFrame.                    *)
   BaseScopePtr  : CARDINAL ;    (* An index to the ScopeCallFrame of  *)
                                 (* the top of BaseModule. BaseModule  *)
                                 (* is always left at the bottom of    *)
                                 (* stack since it is used so          *)
                                 (* frequently. When the BaseModule    *)
                                 (* needs to be searched the ScopePtr  *)
                                 (* is temporarily altered to          *)
                                 (* BaseScopePtr and GetScopeSym is    *)
                                 (* called.                            *)
   BaseModule    : CARDINAL ;    (* Index to the symbol table of the   *)
                                 (* Base pseudo modeule declaration.   *)
   TemporaryNo   : CARDINAL ;    (* The next temporary number.         *)
   CurrentError  : Error ;       (* Current error chain.               *)
   AddressTypes  : List ;        (* A list of type symbols which must  *)
                                 (* be declared as ADDRESS or pointer  *)
(*
   FreeFVarientList,             (* Lists used to maintain GC of field *)
   UsedFVarientList: List ;      (* varients.                          *)
*)
   UnresolvedConstructorType: List ;  (* all constructors whose type   *)
                                 (* is not yet known.                  *)
   AnonymousName     : CARDINAL ;(* anonymous type name unique id      *)
   ReportedUnknowns  : Set ;     (* set of symbols already reported as *)
                                 (* unknowns to the user.              *)


(*
   CheckAnonymous - checks to see whether the name is NulName and if so
                    it creates a unique anonymous name.
*)

PROCEDURE CheckAnonymous (name: Name) : Name ;
BEGIN
   IF name=NulName
   THEN
      INC(AnonymousName) ;
      name := makekey(string(Mark(Sprintf1(Mark(InitString('$$%d')), AnonymousName))))
   END ;
   RETURN( name )
END CheckAnonymous ;


(*
   IsNameAnonymous - returns TRUE if the symbol, sym, has an anonymous name
                     or no name.
*)

PROCEDURE IsNameAnonymous (sym: CARDINAL) : BOOLEAN ;
VAR
   a: ARRAY [0..1] OF CHAR ;
   n: Name ;
BEGIN
   n := GetSymName(sym) ;
   IF n=NulName
   THEN
      RETURN( TRUE )
   ELSE
      GetKey(n, a) ;
      RETURN( StrEqual(a, '$$') )
   END
END IsNameAnonymous ;


(*
   InitWhereDeclared - sets the Declared and FirstUsed fields of record, at.
*)

PROCEDURE InitWhereDeclaredTok (tok: CARDINAL; VAR at: Where) ;
BEGIN
   WITH at DO
      IF CompilingDefinitionModule()
      THEN
         DefDeclared := tok ;
         ModDeclared := UnknownTokenNo
      ELSE
         DefDeclared := UnknownTokenNo ;
         ModDeclared := tok
      END ;
      FirstUsed := tok   (* we assign this field to something legal *)
   END
END InitWhereDeclaredTok ;


(*
   InitWhereDeclared - sets the Declared and FirstUsed fields of record, at.
*)

PROCEDURE InitWhereDeclared (VAR at: Where) ;
BEGIN
   InitWhereDeclaredTok (GetTokenNo (), at)
END InitWhereDeclared ;


(*
   InitWhereFirstUsed - sets the FirstUsed field of record, at.
*)

PROCEDURE InitWhereFirstUsed (VAR at: Where) ;
BEGIN
   InitWhereFirstUsedTok (GetTokenNo (), at)
END InitWhereFirstUsed ;


(*
   InitWhereFirstUsedTok - sets the FirstUsed field of record, at.
*)

PROCEDURE InitWhereFirstUsedTok (tok: CARDINAL; VAR at: Where) ;
BEGIN
   WITH at DO
      FirstUsed := tok
   END
END InitWhereFirstUsedTok ;


(*
   FinalSymbol - returns the highest number symbol used.
*)

PROCEDURE FinalSymbol () : CARDINAL ;
BEGIN
   RETURN( FreeSymbol-1 )
END FinalSymbol ;


(*
   NewSym - Sets Sym to a new symbol index.
*)

PROCEDURE NewSym (VAR sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   sym := FreeSymbol ;
   IF sym=12066
   THEN
      stop
   END ;
   NEW(pSym) ;
   WITH pSym^ DO
      SymbolType := DummySym
   END ;
   PutIndice(Symbols, sym, pSym) ;
   INC(FreeSymbol)
END NewSym ;


(*
   GetPsym - returns the pointer to, sym.
*)

PROCEDURE GetPsym (sym: CARDINAL) : PtrToSymbol ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF InBounds(Symbols, sym)
   THEN
      pSym := GetIndice(Symbols, sym) ;
      RETURN( pSym )
   ELSE
      InternalError ('symbol out of bounds')
   END
END GetPsym ;


(*
   GetPcall - returns the pointer to the CallFrame.
*)

PROCEDURE GetPcall (call: CARDINAL) : PtrToCallFrame ;
VAR
   pCall: PtrToCallFrame ;
BEGIN
   IF InBounds(ScopeCallFrame, call)
   THEN
      pCall := GetIndice(ScopeCallFrame, call) ;
      RETURN( pCall )
   ELSE
      InternalError ('symbol out of bounds')
   END
END GetPcall ;


(*
   DebugProcedureLineNumber -
*)

PROCEDURE DebugProcedureLineNumber (sym: CARDINAL) ;
VAR
   begin, end: CARDINAL ;
   n         : Name ;
   f         : String ;
   l         : CARDINAL ;
BEGIN
   GetProcedureBeginEnd (sym, begin, end) ;
   n := GetSymName(sym) ;
   IF begin#0
   THEN
      f := FindFileNameFromToken (begin, 0) ;
      l := TokenToLineNo(begin, 0) ;
      printf3 ('%s:%d:%a:begin\n', f, l, n)
   END ;
   IF end#0
   THEN
      f := FindFileNameFromToken (end, 0) ;
      l := TokenToLineNo(end, 0) ;
      printf3 ('%s:%d:%a:end\n', f, l, n)
   END
END DebugProcedureLineNumber ;


(*
   DebugLineNumbers - internal debugging, emit all procedure names in this module
                      together with the line numbers for the corresponding begin/end
                      tokens.
*)

PROCEDURE DebugLineNumbers (sym: CARDINAL) ;
BEGIN
   IF DebugFunctionLineNumbers
   THEN
      printf0 ('<lines>\n') ;
      ForeachProcedureDo(sym, DebugProcedureLineNumber) ;
      printf0 ('</lines>\n')
   END
END DebugLineNumbers ;


(*
   IsPartialUnbounded - returns TRUE if, sym, is a partially unbounded symbol.
*)

PROCEDURE IsPartialUnbounded (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF sym>0
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         PartialUnboundedSym:  RETURN( TRUE )

         ELSE
            RETURN( FALSE )
         END
      END
   ELSE
      RETURN( FALSE )
   END
END IsPartialUnbounded ;


(*
   PutPartialUnbounded -
*)

PROCEDURE PutPartialUnbounded (sym: CARDINAL; type: CARDINAL; ndim: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   IF IsDummy(sym)
   THEN
      pSym^.SymbolType := PartialUnboundedSym
   END ;
   WITH pSym^ DO
      CASE SymbolType OF

      PartialUnboundedSym:  PartialUnbounded.Type := type ;
                            PartialUnbounded.NDim := ndim

      ELSE
         InternalError ('not expecting this type')
      END
   END
END PutPartialUnbounded ;


(*
   AlreadyDeclaredError - generate an error message, a, and two areas of code showing
                          the places where the symbols were declared.
*)

PROCEDURE AlreadyDeclaredError (s: String; name: Name; OtherOccurance: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF (OtherOccurance=0) OR (OtherOccurance=GetTokenNo())
   THEN
      e := NewError(GetTokenNo()) ;
      ErrorString(e, s)
   ELSE
      e := NewError(GetTokenNo()) ;
      ErrorString(e, s) ;
      e := ChainError(OtherOccurance, e) ;
      ErrorFormat1(e, 'and symbol (%a) is also declared here', name)
   END
END AlreadyDeclaredError ;


(*
   AlreadyImportedError - generate an error message, a, and two areas of code showing
                          the places where the symbols was imported and also declared.
*)

PROCEDURE AlreadyImportedError (s: String; name: Name; OtherOccurance: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF (OtherOccurance=0) OR (OtherOccurance=GetTokenNo())
   THEN
      e := NewError(GetTokenNo()) ;
      ErrorString(e, s)
   ELSE
      e := NewError(GetTokenNo()) ;
      ErrorString(e, s) ;
      e := ChainError(OtherOccurance, e) ;
      ErrorFormat1(e, 'and symbol (%a) was also seen here', name)
   END
END AlreadyImportedError ;


(*
   MakeError - creates an error node, which can be used in MetaError messages.
               It will be removed from ExportUndeclared and Unknown trees.
*)

PROCEDURE MakeError (tok: CARDINAL; name: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   (* if Sym is present on the unknown tree then remove it *)
   Sym := FetchUnknownSym(name) ;
   IF Sym=NulSym
   THEN
      NewSym(Sym)
   ELSE
      (*
         remove symbol from this tree as we have already generated
         a meaningful error message
      *)
      RemoveExportUndeclared(GetCurrentModuleScope(), Sym)
   END ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := ErrorSym ;
      Error.name := name ;
      InitWhereDeclaredTok(tok, Error.At) ;
      InitWhereFirstUsedTok(tok, Error.At)
   END ;
   RETURN( Sym )
END MakeError ;


(*
   MakeErrorS - creates an error node from a string, which can be used
                in MetaError messages.
                It will be removed from ExportUndeclared and Unknown trees.
*)

PROCEDURE MakeErrorS (tok: CARDINAL; name: String) : CARDINAL ;
BEGIN
   RETURN MakeError (tok, makekey (string (name)))
END MakeErrorS ;


(*
   IsError - returns TRUE if the symbol is an error symbol.
*)

PROCEDURE IsError (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=ErrorSym )
END IsError ;


(*
   MakeObject - creates an object node.
*)

PROCEDURE MakeObject (name: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   NewSym(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := ObjectSym ;
      Object.name := name ;
      InitWhereDeclared(Object.At) ;
      InitWhereFirstUsed(Object.At)
   END ;
   RETURN( Sym )
END MakeObject ;


(*
   IsTuple - returns TRUE if the symbol is a tuple symbol.
*)

PROCEDURE IsTuple (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=TupleSym )
END IsTuple ;


(*
   IsObject - returns TRUE if the symbol is an object symbol.
*)

PROCEDURE IsObject (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=ObjectSym )
END IsObject ;


PROCEDURE stop ; BEGIN END stop ;

(*
   DeclareSym - returns a symbol which was either in the unknown tree or
                a New symbol, since name is about to be declared.
*)

PROCEDURE DeclareSym (tok: CARDINAL; name: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   IF name = NulName
   THEN
      NewSym (Sym)
   ELSIF IsAlreadyDeclaredSym (name)
   THEN
      Sym := GetSym (name) ;
      IF IsImported (GetCurrentModuleScope (), Sym)
      THEN
         MetaErrorT1 (GetWhereImported(Sym),
                      'symbol {%1Rad} is already present in this scope, check both definition and implementation modules, use a different name or remove the import',
                      Sym) ;
         MetaErrorT1 (tok, 'symbol {%1Cad} also declared in this module', Sym) ;
         IF Sym # GetVisibleSym (name)
         THEN
            MetaErrorT1 (tok, 'symbol {%1CMad} also declared in this module', GetVisibleSym (name))
         END
      ELSE
         MetaErrorT1 (tok, 'symbol {%1RMad} is already declared in this scope, use a different name or remove the declaration', Sym) ;
         MetaErrorT1 (tok, 'symbol {%1Cad} also declared in this module', Sym) ;
         IF Sym # GetVisibleSym(name)
         THEN
            MetaErrorT1(tok, 'symbol {%1CMad} also declared in this module', GetVisibleSym (name))
         END
      END ;
      Sym := MakeError (tok, name)
   ELSE
      Sym := FetchUnknownSym (name) ;
      IF Sym=NulSym
      THEN
         NewSym (Sym)
      END ;
      CheckForExportedDeclaration (Sym)
   END ;
   RETURN Sym
END DeclareSym ;


(*
   Init - Initializes the data structures and variables in this module.
          Initialize the trees.
*)

PROCEDURE Init ;
VAR
   pCall: PtrToCallFrame ;
BEGIN
   AnonymousName := 0 ;
   CurrentError := NIL ;
   InitTree(ConstLitTree) ;
   InitTree(ConstLitStringTree) ;
   InitTree(DefModuleTree) ;
   InitTree(ModuleTree) ;
   Symbols := InitIndex(1) ;
   FreeSymbol := 1 ;
   ScopePtr := 1 ;
   ScopeCallFrame := InitIndex(1) ;
   NEW(pCall) ;
   WITH pCall^ DO
      Main := NulSym ;
      Search := NulSym
   END ;
   PutIndice(ScopeCallFrame, ScopePtr, pCall) ;
   CurrentModule     := NulSym ;
   MainModule        := NulSym ;
   FileModule        := NulSym ;
   TemporaryNo       := 0 ;
(*
   InitList(FreeFVarientList) ;             (* Lists used to maintain GC of field *)
   InitList(UsedFVarientList) ;             (* varients.                          *)
*)
   InitList(UnresolvedConstructorType) ;

   InitBase(BuiltinsLocation(), BaseModule) ;
   StartScope(BaseModule) ;   (* BaseModule scope placed at the bottom of the stack *)
   BaseScopePtr := ScopePtr ; (* BaseScopePtr points to the top of the BaseModule scope *)
   InitList(AddressTypes) ;
   ReportedUnknowns := InitSet(1)
END Init ;


(*
   FromModuleGetSym - attempts to find a symbol of name, n, in the
                      module, mod, scope.  An unknown symbol is created
                      at token position tok if necessary.
*)

PROCEDURE FromModuleGetSym (tok: CARDINAL; n: Name; mod: CARDINAL) : CARDINAL ;
VAR
   n1         : Name ;
   sym        : CARDINAL ;
   OldScopePtr: CARDINAL ;
BEGIN
   OldScopePtr := ScopePtr ;
   StartScope (mod) ;
   sym := RequestSym (tok, n) ;
   EndScope ;
   IF sym=NulSym
   THEN
      (* --fixme-- can sym ever be NulSym?  *)
      n1 := GetSymName(mod) ;
      WriteFormat2('cannot find procedure %a in module, %a',
                   n, n1)
   END ;
   ScopePtr := OldScopePtr ;
   RETURN( sym )
END FromModuleGetSym ;


(*
   AddSymToUnknown -
*)

PROCEDURE AddSymToUnknown (scope: CARDINAL; name: Name; Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
   n   : Name ;
BEGIN
   IF DebugUnknowns
   THEN
      n := GetSymName(scope) ;
      printf3('adding unknown %a (%d) to scope %a\n', name, Sym, n)
   END ;

   (* Add symbol to unknown tree *)
   pSym := GetPsym(scope) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : PutSymKey(DefImp.Unresolved, name, Sym) |
      ModuleSym   : PutSymKey(Module.Unresolved, name, Sym) |
      ProcedureSym: PutSymKey(Procedure.Unresolved, name, Sym)

      ELSE
         InternalError ('expecting DefImp, Module or Procedure symbol')
      END
   END
END AddSymToUnknown ;


(*
   AddSymToUnknownTree - adds a symbol with name, name, and Sym to the
                         unknown tree.
*)

PROCEDURE AddSymToUnknownTree (ScopeId: INTEGER; name: Name; Sym: CARDINAL) ;
VAR
   pCall   : PtrToCallFrame ;
   ScopeSym: CARDINAL ;
BEGIN
   IF ScopeId>0
   THEN
      (* choose to place the unknown symbol in the first module scope
         outside the current scope *)
      REPEAT
         pCall := GetPcall(ScopeId) ;
         ScopeSym := pCall^.Main ;
         IF (ScopeSym>0) AND (IsDefImp(ScopeSym) OR IsModule(ScopeSym))
         THEN
            AddSymToUnknown(ScopeSym, name, Sym) ;
            RETURN
         END ;
         DEC(ScopeId)
      UNTIL ScopeId=0
   END ;
   AddSymToUnknown(CurrentModule, name, Sym)
END AddSymToUnknownTree ;


(*
   SubSymFromUnknownTree - removes a symbol with name, name, from the
                           unknown tree.
*)

PROCEDURE SubSymFromUnknownTree (name: Name) ;
VAR
   pCall   : PtrToCallFrame ;
   ScopeSym,
   ScopeId : CARDINAL ;
BEGIN
   IF ScopePtr>0
   THEN
      ScopeId := ScopePtr ;
      REPEAT
         pCall := GetPcall(ScopeId) ;
         ScopeSym := pCall^.Search ;
         IF IsModule(ScopeSym) OR IsDefImp(ScopeSym) OR IsProcedure(ScopeSym)
         THEN
            IF RemoveFromUnresolvedTree(ScopeSym, name)
            THEN
               RETURN
            END
         END ;
         DEC(ScopeId) ;
      UNTIL (ScopeId>0) AND (IsModule(ScopeSym) OR IsDefImp(ScopeSym))
   END ;
   IF RemoveFromUnresolvedTree(CurrentModule, name)
   THEN
   END
END SubSymFromUnknownTree ;


(*
   GetSymFromUnknownTree - returns a symbol with name, name, from the
                           unknown tree.
                           If no symbol with name is found then NulSym
                           is returned.
*)

PROCEDURE GetSymFromUnknownTree (name: Name) : CARDINAL ;
VAR
   pCall   : PtrToCallFrame ;
   ScopeSym,
   ScopeId ,
   Sym     : CARDINAL ;
BEGIN
   IF ScopePtr>0
   THEN
      ScopeId := ScopePtr ;
      REPEAT
         pCall := GetPcall(ScopeId) ;
         ScopeSym := pCall^.Search ;
         IF IsModule(ScopeSym) OR IsDefImp(ScopeSym) OR IsProcedure(ScopeSym)
         THEN
            Sym := ExamineUnresolvedTree(ScopeSym, name) ;
            IF Sym#NulSym
            THEN
               RETURN( Sym )
            END
         END ;
         DEC(ScopeId) ;
      UNTIL (ScopeId>0) AND (IsModule(ScopeSym) OR IsDefImp(ScopeSym))
   END ;
   (* Get symbol from unknown tree *)
   RETURN( ExamineUnresolvedTree(CurrentModule, name) )
END GetSymFromUnknownTree ;


(*
   ExamineUnresolvedTree - returns a symbol with name, name, from the
                           unresolved tree of module, ModSym.
                           If no symbol with name is found then NulSym
                           is returned.
*)

PROCEDURE ExamineUnresolvedTree (ScopeSym: CARDINAL; name: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   (* Get symbol from unknown tree *)
   pSym := GetPsym(ScopeSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : Sym := GetSymKey(DefImp.Unresolved, name) |
      ModuleSym   : Sym := GetSymKey(Module.Unresolved, name) |
      ProcedureSym: Sym := GetSymKey(Procedure.Unresolved, name)

      ELSE
         InternalError ('expecting DefImp, Module or Procedure symbol')
      END
   END ;
   RETURN( Sym )
END ExamineUnresolvedTree ;


(*
   TryMoveUndeclaredSymToInnerModule - attempts to move a symbol of
                                       name, name, which is
                                       currently undefined in the
                                       outer scope to the inner scope.
                                       If successful then the symbol is
                                       returned otherwise NulSym is
                                       returned.
*)

PROCEDURE TryMoveUndeclaredSymToInnerModule (OuterScope,
                                             InnerScope: CARDINAL;
                                             name: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   sym : CARDINAL ;
BEGIN
   (* assume this should not be called if OuterScope was a procedure
      as this case is handled by the caller (P1SymBuild)
   *)
   Assert(IsModule(OuterScope) OR IsDefImp(OuterScope)) ;
   sym := GetExportUndeclared(OuterScope, name) ;
   IF sym#NulSym
   THEN
      Assert(IsUnknown(sym)) ;
      RemoveExportUndeclared(OuterScope, sym) ;
      AddSymToModuleScope(OuterScope, sym) ;
      AddVarToScopeList(OuterScope, sym) ;
      pSym := GetPsym(OuterScope) ;
      WITH pSym^ DO
         CASE SymbolType OF

         DefImpSym: IF GetSymKey(DefImp.Unresolved, name)=sym
                    THEN
                       DelSymKey(DefImp.Unresolved, name)
                    END |
         ModuleSym: IF GetSymKey(Module.Unresolved, name)=sym
                    THEN
                       DelSymKey(Module.Unresolved, name)
                    END

         ELSE
            InternalError ('expecting DefImp, Module symbol')
         END
      END ;
      AddSymToUnknown(InnerScope, name, sym) ;
      PutExportUndeclared(InnerScope, sym)
   END ;
   RETURN( sym )
END TryMoveUndeclaredSymToInnerModule ;


(*
   RemoveFromUnresolvedTree - removes a symbol with name, name, from the
                              unresolved tree of symbol, ScopeSym.
*)

PROCEDURE RemoveFromUnresolvedTree (ScopeSym: CARDINAL; name: Name) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   (* Get symbol from unknown tree *)
   pSym := GetPsym(ScopeSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : IF GetSymKey(DefImp.Unresolved, name)#NulKey
                    THEN
                       DelSymKey(DefImp.Unresolved, name) ;
                       RETURN( TRUE )
                    END |
      ModuleSym   : IF GetSymKey(Module.Unresolved, name)#NulKey
                    THEN
                       DelSymKey(Module.Unresolved, name) ;
                       RETURN( TRUE )
                    END |
      ProcedureSym: IF GetSymKey(Procedure.Unresolved, name)#NulKey
                    THEN
                       DelSymKey(Procedure.Unresolved, name) ;
                       RETURN( TRUE )
                    END

      ELSE
         InternalError ('expecting DefImp, Module or Procedure symbol')
      END
   END ;
   RETURN( FALSE )
END RemoveFromUnresolvedTree ;


(*
   FetchUnknownSym - returns a symbol from the unknown tree if one is
                     available. It also updates the unknown tree.
*)

PROCEDURE FetchUnknownSym (name: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := GetSymFromUnknownTree(name) ;
   IF Sym#NulSym
   THEN
      SubSymFromUnknownTree(name)
   END ;
   RETURN( Sym )
END FetchUnknownSym ;


(*
   TransparentScope - returns true is the scope symbol Sym is allowed
                      to look to an outer level for a symbol.
                      ie is the symbol allowed to look to the parent
                      scope for a symbol.
*)

PROCEDURE TransparentScope (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      RETURN( (SymbolType#DefImpSym) AND (SymbolType#ModuleSym) )
   END
END TransparentScope ;


(*
   AddSymToModuleScope - adds a symbol, Sym, to the scope of the module
                         ModSym.
*)

PROCEDURE AddSymToModuleScope (ModSym: CARDINAL; Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : IF GetSymKey(DefImp.LocalSymbols, GetSymName(Sym))=NulKey
                    THEN
                       PutSymKey(DefImp.LocalSymbols, GetSymName(Sym), Sym)
                    ELSE
                       MetaError1 ('{%kIMPORT} name clash with symbol {%1Ead} symbol already declared ', Sym)
                    END |
      ModuleSym   : IF GetSymKey(Module.LocalSymbols, GetSymName(Sym))=NulKey
                    THEN
                       PutSymKey(Module.LocalSymbols, GetSymName(Sym), Sym)
                    ELSE
                       MetaError1 ('{%kIMPORT} name clash with symbol {%1Ead} symbol already declared ', Sym)
                    END |
      ProcedureSym: IF GetSymKey(Procedure.LocalSymbols, GetSymName(Sym))=NulKey
                    THEN
                       PutSymKey(Procedure.LocalSymbols, GetSymName(Sym), Sym)
                    ELSE
                       MetaError1 ('{%kIMPORT} name clash with symbol {%1Ead} symbol already declared ', Sym)
                    END

      ELSE
         InternalError ('expecting Module or DefImp symbol')
      END
   END
END AddSymToModuleScope ;


(*
   GetCurrentModuleScope - returns the module symbol which forms the
                           current (possibly inner most) module.
*)

PROCEDURE GetCurrentModuleScope () : CARDINAL ;
VAR
   pCall: PtrToCallFrame ;
   i    : CARDINAL ;
BEGIN
   i := ScopePtr ;
   pCall := GetPcall(i) ;
   WHILE (NOT IsModule(pCall^.Search)) AND
         (NOT IsDefImp(pCall^.Search)) DO
      Assert(i>0) ;
      DEC(i) ;
      pCall := GetPcall(i)
   END ;
   RETURN( pCall^.Search )
END GetCurrentModuleScope ;


(*
   GetLastModuleScope - returns the last module scope encountered,
                        the module scope before the Current Module Scope.
*)

PROCEDURE GetLastModuleScope () : CARDINAL ;
VAR
   pCall: PtrToCallFrame ;
   i    : CARDINAL ;
BEGIN
   i := ScopePtr ;
   pCall := GetPcall(i) ;
   WHILE (NOT IsModule(pCall^.Search)) AND
         (NOT IsDefImp(pCall^.Search)) DO
      Assert(i>0) ;
      DEC(i) ;
      pCall := GetPcall(i)
   END ;
   (* Found module at position, i. *)
   DEC(i) ;  (* Move to an outer level module scope *)
   pCall := GetPcall(i) ;
   WHILE (NOT IsModule(pCall^.Search)) AND
         (NOT IsDefImp(pCall^.Search)) DO
      Assert(i>0) ;
      DEC(i) ;
      pCall := GetPcall(i)
   END ;
   (* Found module at position, i. *)
   RETURN( pCall^.Search )
END GetLastModuleScope ;


(*
   GetLastModuleOrProcedureScope - returns the last module or procedure scope encountered,
                                   the scope before the current module scope.
*)

PROCEDURE GetLastModuleOrProcedureScope () : CARDINAL ;
VAR
   pCall: PtrToCallFrame ;
   i    : CARDINAL ;
BEGIN
   (* find current inner module *)
   i := ScopePtr ;
   pCall := GetPcall(i) ;
   WHILE (NOT IsModule(pCall^.Search)) AND
         (NOT IsDefImp(pCall^.Search)) DO
      Assert(i>0) ;
      DEC(i) ;
      pCall := GetPcall(i)
   END ;
   (* found module at position, i. *)
   DEC(i) ;  (* Move to an outer level module or procedure scope *)
   pCall := GetPcall(i) ;
   WHILE (NOT IsModule(pCall^.Search)) AND
         (NOT IsDefImp(pCall^.Search)) AND
         (NOT IsProcedure(pCall^.Search)) DO
      Assert(i>0) ;
      DEC(i) ;
      pCall := GetPcall(i)
   END ;
   (* Found module at position, i. *)
   RETURN( pCall^.Search )
END GetLastModuleOrProcedureScope ;


(*
   AddSymToScope - adds a symbol Sym with name name to
                   the current scope symbol tree.
*)

PROCEDURE AddSymToScope (Sym: CARDINAL; name: Name) ;
VAR
   pSym   : PtrToSymbol ;
   pCall  : PtrToCallFrame ;
   ScopeId: CARDINAL ;
BEGIN
   pCall := GetPcall(ScopePtr) ;
   ScopeId := pCall^.Main ;
   (*
      WriteString('Adding ') ; WriteKey(name) ; WriteString(' :') ; WriteCard(Sym, 4) ; WriteString(' to scope: ') ;
      WriteKey(GetSymName(ScopeId)) ; WriteLn ;
   *)
   pSym := GetPsym(ScopeId) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : IF name#NulName
                    THEN
                       PutSymKey(DefImp.LocalSymbols, name, Sym)
                    END ;
                    IF IsEnumeration(Sym)
                    THEN
                       CheckEnumerationInList(DefImp.EnumerationScopeList, Sym)
                    END |
      ModuleSym   : IF name#NulName
                    THEN
                       PutSymKey(Module.LocalSymbols, name, Sym)
                    END ;
                    IF IsEnumeration(Sym)
                    THEN
                       CheckEnumerationInList(Module.EnumerationScopeList, Sym)
                    END |
      ProcedureSym: IF name#NulName
                    THEN
                       PutSymKey(Procedure.LocalSymbols, name, Sym)
                    END ;
                    IF IsEnumeration(Sym)
                    THEN
                       CheckEnumerationInList(Procedure.EnumerationScopeList, Sym)
                    END

      ELSE
         InternalError ('should never get here')
      END
   END
END AddSymToScope ;


(*
   GetCurrentScope - returns the symbol who is responsible for the current
                     scope. Note that it ignore pseudo scopes.
*)

PROCEDURE GetCurrentScope () : CARDINAL ;
VAR
   pCall: PtrToCallFrame ;
BEGIN
   pCall := GetPcall(ScopePtr) ;
   RETURN( pCall^.Main )
END GetCurrentScope ;


(*
   StartScope - starts a block scope at Sym. Transparent determines
                whether the search for a symbol will look at the
                previous ScopeCallFrame if Sym does not contain the
                symbol that GetSym is searching.

                WITH statements are partially implemented by calling
                StartScope. Therefore we must retain the old Main from
                the previous ScopePtr when a record is added to the scope
                stack. (Main contains the symbol where all identifiers
                should be added.)
*)

PROCEDURE StartScope (Sym: CARDINAL) ;
VAR
   oCall,
   pCall: PtrToCallFrame ;
BEGIN
   Sym := SkipType(Sym) ;
(*
   WriteString('New scope is: ') ; WriteKey(GetSymName(Sym)) ; WriteLn ;
*)
   INC(ScopePtr) ;
   IF InBounds(ScopeCallFrame, ScopePtr)
   THEN
      pCall := GetPcall(ScopePtr)
   ELSE
      NEW(pCall) ;
      PutIndice(ScopeCallFrame, ScopePtr, pCall)
   END ;
   WITH pCall^ DO
      Start := ScopePtr-1 ;  (* Previous ScopePtr value before StartScope *)
      Search := Sym ;

      (* If Sym is a record then maintain the old Main scope for adding   *)
      (* new symbols to ie temporary variables.                           *)
      IF IsRecord(Sym)
      THEN
         oCall := GetPcall(ScopePtr-1) ;
         Main := oCall^.Main
      ELSE
         Main := Sym ;
         PlaceMajorScopesEnumerationListOntoStack(Sym)
      END
   END
   (* ; DisplayScopes *)
END StartScope ;


(*
   PlaceMajorScopesEnumerationListOntoStack - places the DefImp, Module and
                                              Procedure symbols enumeration
                                              list onto the scope stack.
*)

PROCEDURE PlaceMajorScopesEnumerationListOntoStack (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : PlaceEnumerationListOntoScope(DefImp.EnumerationScopeList) |
      ModuleSym   : PlaceEnumerationListOntoScope(Module.EnumerationScopeList) |
      ProcedureSym: PlaceEnumerationListOntoScope(Procedure.EnumerationScopeList)

      ELSE
         InternalError ('expecting - DefImp, Module or Procedure symbol')
      END
   END
END PlaceMajorScopesEnumerationListOntoStack ;


(*
   PlaceEnumerationListOntoScope - places an enumeration list, l, onto the
                                   scope stack. This list will automatically
                                   removed via one call to EndScope which
                                   matches the StartScope by which this
                                   procedure is invoked.
*)

PROCEDURE PlaceEnumerationListOntoScope (l: List) ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(l) ;
   i := 1 ;
   WHILE i<=n DO
      PseudoScope(GetItemFromList(l, i)) ;
      INC(i)
   END
END PlaceEnumerationListOntoScope ;


(*
   EndScope - ends a block scope started by StartScope. The current
              head of the symbol scope reverts back to the symbol
              which was the Head of the symbol scope before the
              last StartScope was called.
*)

PROCEDURE EndScope ;
VAR
   pCall: PtrToCallFrame ;
BEGIN
(*
   ; WriteString('EndScope - ending scope: ') ;
   pCall := GetPcall(ScopePtr) ;
   ; WriteKey(GetSymName(pCall^.Search)) ; WriteLn ;
*)
   pCall := GetPcall(ScopePtr) ;
   ScopePtr := pCall^.Start
   (* ; DisplayScopes *)
END EndScope ;


(*
   PseudoScope - starts a pseudo scope at Sym.
                 We always connect parent up to the last scope,
                 to determine the transparancy of a scope we call
                 TransparentScope.

                 A Pseudo scope has no end block,
                 but is terminated when the next EndScope is used.
                 The function of the pseudo scope is to provide an
                 automatic mechanism to solve enumeration types.
                 A declared enumeration type is a Pseudo scope and
                 identifiers used with the name of an enumeration
                 type field will find the enumeration symbol by
                 the scoping algorithm.
*)

PROCEDURE PseudoScope (Sym: CARDINAL) ;
VAR
   oCall,
   pCall: PtrToCallFrame ;
BEGIN
   IF IsEnumeration(Sym)
   THEN
      INC(ScopePtr) ;
      IF InBounds(ScopeCallFrame, ScopePtr)
      THEN
         pCall := GetPcall(ScopePtr)
      ELSE
         NEW(pCall) ;
         PutIndice(ScopeCallFrame, ScopePtr, pCall)
      END ;
      WITH pCall^ DO
         oCall := GetPcall(ScopePtr-1) ;
         Main := oCall^.Main ;
         Start := oCall^.Start ;
         Search := Sym
      END
   ELSE
      InternalError ('expecting EnumerationSym')
   END
END PseudoScope ;


(*
   IsDeclaredIn - returns TRUE if a symbol was declared in, scope.
*)

PROCEDURE IsDeclaredIn (scope, sym: CARDINAL) : BOOLEAN ;
VAR
   s: CARDINAL ;
BEGIN
   s := GetScope(sym) ;
   WHILE s#scope DO
      IF (s=NulSym) OR IsProcedure(s) OR IsModule(s) OR IsDefImp(s)
      THEN
         RETURN( FALSE )
      ELSE
         s := GetScope(s)
      END
   END ;
   RETURN( TRUE )
END IsDeclaredIn ;


(*
   MakeGnuAsm - create a GnuAsm symbol.
*)

PROCEDURE MakeGnuAsm () : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   NewSym(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := GnuAsmSym ;
      WITH GnuAsm DO
         String   := NulSym ;
         InitWhereDeclared(At) ;
         Inputs   := NulSym ;
         Outputs  := NulSym ;
         Trashed  := NulSym ;
         Volatile := FALSE ;
         Simple   := FALSE
      END
   END ;
   RETURN( Sym )
END MakeGnuAsm ;


(*
   PutGnuAsm - places the instruction textual name into the GnuAsm symbol.
*)

PROCEDURE PutGnuAsm (sym: CARDINAL; string: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(IsConstString(string)) ;
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.String := string

      ELSE
         InternalError ('expecting PutGnuAsm symbol')
      END
   END
END PutGnuAsm ;


(*
   GetGnuAsm - returns the string symbol, representing the instruction textual
               of the GnuAsm symbol. It will return a ConstString.
*)

PROCEDURE GetGnuAsm (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.String )

      ELSE
         InternalError ('expecting GnuAsm symbol')
      END
   END
END GetGnuAsm ;


(*
   PutGnuAsmOutput - places the interface object, out, into GnuAsm symbol, sym.
*)

PROCEDURE PutGnuAsmOutput (sym: CARDINAL; out: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Outputs := out

      ELSE
         InternalError ('expecting PutGnuAsm symbol')
      END
   END
END PutGnuAsmOutput ;


(*
   PutGnuAsmInput - places the interface object, in, into GnuAsm symbol, sym.
*)

PROCEDURE PutGnuAsmInput (sym: CARDINAL; in: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Inputs := in

      ELSE
         InternalError ('expecting PutGnuAsm symbol')
      END
   END
END PutGnuAsmInput ;


(*
   PutGnuAsmTrash - places the interface object, trash, into GnuAsm symbol, sym.
*)

PROCEDURE PutGnuAsmTrash (sym: CARDINAL; trash: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Trashed := trash

      ELSE
         InternalError ('expecting PutGnuAsm symbol')
      END
   END
END PutGnuAsmTrash ;


(*
   GetGnuAsmInput - returns the input list of registers.
*)

PROCEDURE GetGnuAsmInput (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Inputs )

      ELSE
         InternalError ('expecting PutGnuAsm symbol')
      END
   END
END GetGnuAsmInput ;


(*
   GetGnuAsmOutput - returns the output list of registers.
*)

PROCEDURE GetGnuAsmOutput (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Outputs )

      ELSE
         InternalError ('expecting PutGnuAsm symbol')
      END
   END
END GetGnuAsmOutput ;


(*
   GetGnuAsmTrash - returns the list of trashed registers.
*)

PROCEDURE GetGnuAsmTrash (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Trashed )

      ELSE
         InternalError ('expecting PutGnuAsm symbol')
      END
   END
END GetGnuAsmTrash ;


(*
   PutGnuAsmVolatile - defines a GnuAsm symbol as VOLATILE.
*)

PROCEDURE PutGnuAsmVolatile (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Volatile := TRUE

      ELSE
         InternalError ('expecting GnuAsm symbol')
      END
   END
END PutGnuAsmVolatile ;


(*
   PutGnuAsmSimple - defines a GnuAsm symbol as a simple kind.
*)

PROCEDURE PutGnuAsmSimple (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Simple := TRUE

      ELSE
         InternalError ('expecting GnuAsm symbol')
      END
   END
END PutGnuAsmSimple ;


(*
   MakeRegInterface - creates and returns a register interface symbol.
*)

PROCEDURE MakeRegInterface () : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   NewSym(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := InterfaceSym ;
      WITH Interface DO
         Parameters := InitIndex(1) ;
         InitWhereDeclared(At)
      END
   END ;
   RETURN( Sym )
END MakeRegInterface ;


(*
   PutRegInterface - places a, name, string, and, object, into the interface array,
                     sym, at position, i.
                     The string symbol will either be a register name or a constraint.
                     The object is an optional Modula-2 variable or constant symbol.
*)

PROCEDURE PutRegInterface (sym: CARDINAL; i: CARDINAL; n: Name; string, object: CARDINAL) ;
VAR
   pSym : PtrToSymbol ;
   p    : PtrToAsmConstraint ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      InterfaceSym: IF Indexing.InBounds(Interface.Parameters, i)
                    THEN
                       p := Indexing.GetIndice(Interface.Parameters, i)
                    ELSIF i=Indexing.HighIndice(Interface.Parameters)+1
                    THEN
                       NEW(p) ;
                       Indexing.PutIndice(Interface.Parameters, i, p)
                    ELSE
                       InternalError ('expecting to add parameters sequentially')
                    END ;
                    WITH p^ DO
                       name := n ;
                       str  := string ;
                       obj  := object
                    END

      ELSE
         InternalError ('expecting Interface symbol')
      END
   END
END PutRegInterface ;


(*
   GetRegInterface - gets a, name, string, and, object, from the interface array,
                     sym, from position, i.
*)

PROCEDURE GetRegInterface (sym: CARDINAL; i: CARDINAL; VAR n: Name; VAR string, object: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
   p   : PtrToAsmConstraint ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      InterfaceSym: IF Indexing.InBounds(Interface.Parameters, i)
                    THEN
                       p := Indexing.GetIndice(Interface.Parameters, i) ;
                       WITH p^ DO
                          n      := name ;
                          string := str ;
                          object := obj
                       END
                    ELSE
                       n      := NulName ;
                       string := NulSym ;
                       object := NulSym
                    END

      ELSE
         InternalError ('expecting Interface symbol')
      END
   END
END GetRegInterface ;


(*
   GetSubrange - returns HighSym and LowSym - two constants which make up the
                 subrange.
*)

PROCEDURE GetSubrange (Sym: CARDINAL; VAR HighSym, LowSym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      SubrangeSym: HighSym := Subrange.High ;
                   LowSym := Subrange.Low

      ELSE
         InternalError ('expecting Subrange symbol')
      END
   END
END GetSubrange ;


(*
   PutSubrange - places LowSym and HighSym as two symbols
                 which provide the limits of the range.
*)

PROCEDURE PutSubrange (Sym: CARDINAL; LowSym, HighSym: CARDINAL;
                       TypeSymbol: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      SubrangeSym:  Subrange.Low := LowSym ;      (* Index to symbol for lower   *)
                    Subrange.High := HighSym ;    (* Index to symbol for higher  *)
                    Subrange.Type := TypeSymbol ; (* Index to type symbol for    *)
                                                  (* the type of subrange.       *)
      ELSE
         InternalError ('expecting Subrange symbol')
      END
   END
END PutSubrange ;


(*
   SetCurrentModule - Used to set the CurrentModule to a symbol, Sym.
                      This Sym must represent the module name of the
                      file currently being compiled.
*)

PROCEDURE SetCurrentModule (Sym: CARDINAL) ;
BEGIN
   CurrentModule := Sym
END SetCurrentModule ;


(*
   GetCurrentModule - returns the current module Sym that is being
                      compiled.
*)

PROCEDURE GetCurrentModule () : CARDINAL ;
BEGIN
   RETURN( CurrentModule )
END GetCurrentModule ;


(*
   SetMainModule - Used to set the MainModule to a symbol, Sym.
                   This Sym must represent the main module which was
                   envoked by the user to be compiled.
*)

PROCEDURE SetMainModule (Sym: CARDINAL) ;
BEGIN
   MainModule := Sym
END SetMainModule ;


(*
   GetMainModule - returns the main module symbol that was requested by
                   the user to be compiled.
*)

PROCEDURE GetMainModule () : CARDINAL ;
BEGIN
   RETURN( MainModule )
END GetMainModule ;


(*
   SetFileModule - Used to set the FileModule to a symbol, Sym.
                   This Sym must represent the current program module
                   file which is being parsed.
*)

PROCEDURE SetFileModule (Sym: CARDINAL) ;
BEGIN
   FileModule := Sym
END SetFileModule ;


(*
   GetFileModule - returns the FileModule symbol that was requested by
                   the user to be compiled.
*)

PROCEDURE GetFileModule () : CARDINAL ;
BEGIN
   RETURN( FileModule )
END GetFileModule ;


(*
   GetBaseModule - returns the base module symbol that contains Modula-2
                   base types, procedures and functions.
*)

PROCEDURE GetBaseModule () : CARDINAL ;
BEGIN
   RETURN( BaseModule )
END GetBaseModule ;


(*
   GetSym - searches the current scope (and previous scopes if the
            scope tranparent allows) for a symbol with name.
*)

PROCEDURE GetSym (name: Name) : CARDINAL ;
VAR
   Sym        : CARDINAL ;
   OldScopePtr: CARDINAL ;
BEGIN
   Sym := GetScopeSym(name, TRUE) ;
   IF Sym=NulSym
   THEN
      (* Check default base types for symbol *)
      OldScopePtr := ScopePtr ;  (* Save ScopePtr *)
      ScopePtr := BaseScopePtr ; (* Alter ScopePtr to point to top of BaseModule *)
      Sym := GetScopeSym(name, FALSE) ; (* Search BaseModule for name *)
      ScopePtr := OldScopePtr    (* Restored ScopePtr *)
   END ;
   RETURN( Sym )
END GetSym ;


(*
   CanLookThroughScope - by default this procedure returns TRUE.  It only returns
                         FALSE if, throughProcedure, is FALSE and the ScopeSym is
                         a procedure.
*)

PROCEDURE CanLookThroughScope (ScopeSym: CARDINAL; throughProcedure: BOOLEAN) : BOOLEAN ;
BEGIN
   IF IsProcedure(ScopeSym)
   THEN
      RETURN( throughProcedure )
   ELSE
      RETURN( TRUE )
   END
END CanLookThroughScope ;


(*
   GetScopeSym - searches the current scope and below, providing that the
                 scopes are transparent, for a symbol with name, name.
                 It only passes over procedure scopes if, throughProcedure,
                 is TRUE.
*)

PROCEDURE GetScopeSym (name: Name; throughProcedure: BOOLEAN) : CARDINAL ;
VAR
   pCall   : PtrToCallFrame ;
   ScopeSym,
   ScopeId ,
   Sym     : CARDINAL ;
BEGIN
   (* DisplayScopes ; *)
   ScopeId := ScopePtr ;
   pCall := GetPcall(ScopeId) ;
   ScopeSym := pCall^.Search ;
   (* WriteString(' scope: ') ; WriteKey(GetSymName(ScopeSym)) ; *)
   Sym := CheckScopeForSym(ScopeSym, name) ;
   WHILE (ScopeId>0) AND (Sym=NulSym) AND TransparentScope(ScopeSym) AND
         CanLookThroughScope(ScopeSym, throughProcedure) DO
      DEC(ScopeId) ;
      pCall := GetPcall(ScopeId) ;
      ScopeSym := pCall^.Search ;
      Sym := CheckScopeForSym(ScopeSym, name) ;
      (* WriteString(' scope: ') ; WriteKey(GetSymName(ScopeSym)) *)
   END ;
   (* IF Sym#NulSym THEN WriteKey(GetSymName(Sym)) END ; WriteLn ; *)
   RETURN( Sym )
END GetScopeSym ;


(*
   CheckScopeForSym - checks the scope, ScopeSym, for an identifier
                      of name, name. CheckScopeForSym checks for
                      the symbol by the GetLocalSym and also
                      ExamineUnresolvedTree.
*)

PROCEDURE CheckScopeForSym (ScopeSym: CARDINAL; name: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := GetLocalSym(ScopeSym, name) ;
   IF (Sym=NulSym) AND (IsModule(ScopeSym) OR IsDefImp(ScopeSym) OR
                        IsProcedure(ScopeSym))
   THEN
      Sym := ExamineUnresolvedTree(ScopeSym, name)
   END ;
   RETURN( Sym )
END CheckScopeForSym ;


(*
   DisplayScopes - displays the scopes that will be searched to find
                   a requested symbol.
*)

PROCEDURE DisplayScopes ;
VAR
   pCall: PtrToCallFrame ;
   n    : Name ;
   i    : CARDINAL ;
   Sym  : CARDINAL ;
BEGIN
   i := ScopePtr ;
   printf0('Displaying scopes\n') ;
   WHILE i>=1 DO
      pCall := GetPcall(i) ;
      Sym := pCall^.Search ;
      printf1('Symbol %4d', Sym) ;
      IF Sym#NulSym
      THEN
         n := GetSymName(Sym) ;
         printf1(' : name %a is ', n) ;
         IF NOT TransparentScope(Sym)
         THEN
            printf0('not')
         END ;
         printf0(' transparent\n')
      END ;
      DEC(i)
   END ;
   printf0('\n')
END DisplayScopes ;


(*
   GetModuleScopeId - returns the scope index to the next module starting
                      at index, Id.
                      Id will either point to a null scope (NulSym) or
                      alternatively point to a Module or DefImp symbol.
*)

PROCEDURE GetModuleScopeId (Id: CARDINAL) : CARDINAL ;
VAR
   pCall: PtrToCallFrame ;
   s    : CARDINAL ;
BEGIN
   pCall := GetPcall(Id) ;
   s := pCall^.Search ;
   WHILE (Id>0) AND (s#NulSym) AND
         ((NOT IsModule(s)) AND
          (NOT IsDefImp(s))) DO
      DEC(Id) ;
      pCall := GetPcall(Id) ;
      s := pCall^.Search ;
   END ;
   RETURN( Id )
END GetModuleScopeId ;


(*
   GetVisibleSym -
*)

PROCEDURE GetVisibleSym (name: Name) : CARDINAL ;
VAR
   pCall: PtrToCallFrame ;
   Sym,
   i    : CARDINAL ;
BEGIN
   i := ScopePtr ;
   WHILE i>=1 DO
      pCall := GetPcall(i) ;
      WITH pCall^ DO
         IF Search=Main
         THEN
            RETURN( GetLocalSym(Main, name) )
         ELSE
            IF IsEnumeration(Search)
            THEN
               Sym := GetLocalSym(Search, name) ;
               IF Sym#NulSym
               THEN
                  RETURN( Sym )
               END
            END
         END
      END ;
      DEC(i)
   END ;
   RETURN( NulSym )
END GetVisibleSym ;


(*
   IsAlreadyDeclaredSym - returns true if Sym has already been declared
                          in the current main scope.
*)

PROCEDURE IsAlreadyDeclaredSym (name: Name) : BOOLEAN ;
VAR
   pCall: PtrToCallFrame ;
   i    : CARDINAL ;
BEGIN
   i := ScopePtr ;
   WHILE i>=1 DO
      pCall := GetPcall(i) ;
      WITH pCall^ DO
         IF Search=Main
         THEN
            RETURN( GetLocalSym(Main, name)#NulSym )
         ELSE
            IF IsEnumeration(Search) AND (GetLocalSym(Search, name)#NulSym)
            THEN
               RETURN( TRUE )
            END
         END
      END ;
      DEC(i)
   END ;
   RETURN( FALSE )
END IsAlreadyDeclaredSym ;


(*
   IsImplicityExported - returns TRUE if, Sym, is implicitly exported from module, ModSym.
                         ModSym must be a defimp symbol.
*)

PROCEDURE IsImplicityExported (ModSym, Sym: CARDINAL) : BOOLEAN ;
VAR
   type: CARDINAL ;
   pSym: PtrToSymbol ;
BEGIN
   IF IsDefImp(ModSym) AND IsFieldEnumeration(Sym)
   THEN
      pSym := GetPsym(ModSym) ;
      type := SkipType(GetType(Sym)) ;
      RETURN( IsItemInList(pSym^.DefImp.EnumerationScopeList, type) )
   END ;
   RETURN( FALSE )
END IsImplicityExported ;


(*
   MakeModule - creates a module sym with ModuleName. It returns the
                symbol index.
*)

PROCEDURE MakeModule (tok: CARDINAL; ModuleName: Name) : CARDINAL ;
VAR
   pSym : PtrToSymbol ;
   pCall: PtrToCallFrame ;
   Sym  : CARDINAL ;
BEGIN
   (*
      Make a new symbol since we are at the outer scope level.
      DeclareSym examines the current scope level for any symbols
      that have the correct name, but are yet undefined.
      Therefore we must not call DeclareSym but create a symbol
      directly.
   *)
   NewSym(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := ModuleSym ;
      WITH Module DO
         name := ModuleName ;               (* Index into name array, name   *)
                                            (* of record field.              *)
         InitTree(LocalSymbols) ;           (* The LocalSymbols hold all the *)
                                            (* variables declared local to   *)
                                            (* the block. It contains the    *)
                                            (* FROM _ IMPORT x, y, x ;       *)
                                            (* IMPORT A ;                    *)
                                            (*    and also                   *)
                                            (* MODULE WeAreHere ;            *)
                                            (*    x y z visiable by localsym *)
                                            (*    MODULE Inner ;             *)
                                            (*       EXPORT x, y, z ;        *)
                                            (*    END Inner ;                *)
                                            (* END WeAreHere.                *)
         InitTree(ExportTree) ;             (* Holds all the exported        *)
                                            (* identifiers.                  *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
         InitTree(ImportTree) ;             (* Contains all IMPORTed         *)
                                            (* identifiers.                  *)
         InitList(IncludeList) ;            (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* modulename.Symbol             *)
         InitTree(ExportUndeclared) ;       (* ExportUndeclared contains all *)
                                            (* the identifiers which were    *)
                                            (* exported but have not yet     *)
                                            (* been declared.                *)
         InitList(EnumerationScopeList) ;   (* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visable within this scope.    *)
                                            (* Outer Module.                 *)
         InitTree(NamedObjects) ;           (* Names of all items declared.  *)
         InitTree(NamedImports) ;           (* Names of items imported.      *)
         InitTree(WhereImported) ;          (* Sym to TokenNo where import   *)
                                            (* occurs. Error message use.    *)
         Priority := NulSym ;               (* Priority of the module. This  *)
                                            (* is an index to a constant.    *)
         InitTree(Unresolved) ;             (* All symbols currently         *)
                                            (* unresolved in this module.    *)
         StartQuad := 0 ;                   (* Signify the initialization    *)
                                            (* code.                         *)
         EndQuad := 0 ;                     (* EndQuad should point to a     *)
                                            (* goto quad.                    *)
         StartFinishQuad := 0 ;             (* Signify the finalization      *)
                                            (* code.                         *)
         EndFinishQuad := 0 ;               (* should point to a finish      *)
         FinallyFunction := NIL ;           (* The GCC function for finally  *)
         ExceptionFinally := FALSE ;        (* does it have an exception?    *)
         ExceptionBlock := FALSE ;          (* does it have an exception?    *)
         InitList(ListOfVars) ;             (* List of variables in this     *)
                                            (* scope.                        *)
         InitList(ListOfProcs) ;            (* List of all procedures        *)
                                            (* declared within this module.  *)
         InitList(ListOfModules) ;          (* List of all inner modules.    *)
         InitWhereDeclaredTok(tok, At) ;    (* Where symbol declared.        *)
         InitWhereFirstUsedTok(tok, At) ;   (* Where symbol first used.      *)
         pCall := GetPcall(ScopePtr) ;
         IF pCall^.Main=GetBaseModule()
         THEN
            Scope := NulSym
         ELSE
            Scope := pCall^.Main
         END
      END
   END ;
   PutSymKey(ModuleTree, ModuleName, Sym) ;
   RETURN( Sym )
END MakeModule ;


(*
   AddModuleToParent - adds symbol, Sym, to module, Parent.
*)

PROCEDURE AddModuleToParent (Sym: CARDINAL; Parent: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Parent) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   :  PutItemIntoList(DefImp.ListOfModules, Sym) |
      ModuleSym   :  PutItemIntoList(Module.ListOfModules, Sym) |
      ProcedureSym:  PutItemIntoList(Procedure.ListOfModules, Sym)

      ELSE
         InternalError ('expecting DefImp or Module symbol')
      END
   END
END AddModuleToParent ;


(*
   MakeInnerModule - creates an inner module sym with ModuleName. It returns the
                     symbol index.
*)

PROCEDURE MakeInnerModule (tok: CARDINAL; ModuleName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   Sym := DeclareSym (tok, ModuleName) ;
   IF NOT IsError(Sym)
   THEN
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         SymbolType := ModuleSym ;
         WITH Module DO
            name := ModuleName ;            (* Index into name array, name   *)
                                            (* of record field.              *)
            InitTree(LocalSymbols) ;        (* The LocalSymbols hold all the *)
                                            (* variables declared local to   *)
                                            (* the block. It contains the    *)
                                            (* FROM _ IMPORT x, y, x ;       *)
                                            (* IMPORT A ;                    *)
                                            (*    and also                   *)
                                            (* MODULE WeAreHere ;            *)
                                            (*    x y z visiable by localsym *)
                                            (*    MODULE Inner ;             *)
                                            (*       EXPORT x, y, z ;        *)
                                            (*    END Inner ;                *)
                                            (* END WeAreHere.                *)
            InitTree(ExportTree) ;          (* Holds all the exported        *)
                                            (* identifiers.                  *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
            InitTree(ImportTree) ;          (* Contains all IMPORTed         *)
                                            (* identifiers.                  *)
            InitList(IncludeList) ;         (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* modulename.Symbol             *)
            InitTree(ExportUndeclared) ;    (* ExportUndeclared contains all *)
                                            (* the identifiers which were    *)
                                            (* exported but have not yet     *)
                                            (* been declared.                *)
            InitList(EnumerationScopeList) ;(* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visable within this scope.    *)
            InitTree(NamedObjects) ;        (* Names of all items declared.  *)
            InitTree(NamedImports) ;        (* Names of items imported.      *)
            InitTree(WhereImported) ;       (* Sym to TokenNo where import   *)
                                            (* occurs. Error message use.    *)
            Priority := NulSym ;            (* Priority of the module. This  *)
                                            (* is an index to a constant.    *)
            InitTree(Unresolved) ;          (* All symbols currently         *)
                                            (* unresolved in this module.    *)
            StartQuad := 0 ;                (* Signify the initialization    *)
                                            (* code.                         *)
            EndQuad := 0 ;                  (* EndQuad should point to a     *)
                                            (* goto quad.                    *)
            StartFinishQuad := 0 ;          (* Signify the finalization      *)
                                            (* code.                         *)
            EndFinishQuad := 0 ;            (* should point to a finish      *)
            FinallyFunction := NIL ;        (* The GCC function for finally  *)
            ExceptionFinally := FALSE ;     (* does it have an exception?    *)
            ExceptionBlock := FALSE ;       (* does it have an exception?    *)
            InitList(ListOfVars) ;          (* List of variables in this     *)
                                            (* scope.                        *)
            InitList(ListOfProcs) ;         (* List of all procedures        *)
                                            (* declared within this module.  *)
            InitList(ListOfModules) ;       (* List of all inner modules.    *)
            InitWhereDeclaredTok(tok, At) ;   (* Where symbol declared.        *)
            InitWhereFirstUsedTok(tok, At) ;  (* Where symbol first used.      *)
            IF GetCurrentScope()=GetBaseModule()
            THEN
               Scope := NulSym
            ELSE
               Scope := GetCurrentScope() ;
               AddModuleToParent(Sym, Scope)
            END
         END ;
      END ;
      AddSymToScope(Sym, ModuleName)
   END ;
   RETURN Sym
END MakeInnerModule ;


(*
   MakeDefImp - creates a definition and implementation module sym
                with name DefImpName. It returns the symbol index.
*)

PROCEDURE MakeDefImp (tok: CARDINAL; DefImpName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   (*
      Make a new symbol since we are at the outer scope level.
      DeclareSym examines the current scope level for any symbols
      that have the correct name, but are yet undefined.
      Therefore we must not call DeclareSym but create a symbol
      directly.
   *)
   NewSym(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := DefImpSym ;
      WITH DefImp DO
         name := DefImpName ;         (* Index into name array, name   *)
                                      (* of record field.              *)
         InitTree(ExportQualifiedTree) ;
                                      (* Holds all the EXPORT          *)
                                      (* QUALIFIED identifiers.        *)
                                      (* This tree may be              *)
                                      (* deleted at the end of Pass 1. *)
         InitTree(ExportUnQualifiedTree) ;
                                      (* Holds all the EXPORT          *)
                                      (* UNQUALIFIED identifiers.      *)
                                      (* This tree may be              *)
                                      (* deleted at the end of Pass 1. *)
         InitTree(ExportRequest) ;    (* Contains all identifiers that *)
                                      (* have been requested by other  *)
                                      (* modules before this module    *)
                                      (* declared its export list.     *)
                                      (* This tree should be empty at  *)
                                      (* the end of the compilation.   *)
                                      (* Each time a symbol is         *)
                                      (* exported it is removed from   *)
                                      (* this list.                    *)
         InitTree(ImportTree) ;       (* Contains all IMPORTed         *)
                                      (* identifiers.                  *)
         InitList(IncludeList) ;      (* Contains all included symbols *)
                                      (* which are included by         *)
                                      (* IMPORT modulename ;           *)
                                      (* modulename.Symbol             *)
         InitList(DefIncludeList) ;   (* Contains all included symbols *)
                                      (* which are included by         *)
                                      (* IMPORT modulename ;           *)
                                      (* in the definition module only *)
         InitTree(ExportUndeclared) ; (* ExportUndeclared contains all *)
                                      (* the identifiers which were    *)
                                      (* exported but have not yet     *)
                                      (* been declared.                *)
         InitTree(NeedToBeImplemented) ;
                                      (* NeedToBeImplemented contains  *)
                                      (* the identifiers which have    *)
                                      (* been exported and declared    *)
                                      (* but have not yet been         *)
                                      (* implemented.                  *)
         InitTree(LocalSymbols) ;     (* The LocalSymbols hold all the *)
                                      (* variables declared local to   *)
                                      (* the block. It contains the    *)
                                      (* IMPORT r ;                    *)
                                      (* FROM _ IMPORT x, y, x ;       *)
                                      (*    and also                   *)
                                      (* MODULE WeAreHere ;            *)
                                      (*    x y z visiable by localsym *)
                                      (*    MODULE Inner ;             *)
                                      (*       EXPORT x, y, z ;        *)
                                      (*    END Inner ;                *)
                                      (* END WeAreHere.                *)
         InitList(EnumerationScopeList) ;
                                      (* Enumeration scope list which  *)
                                      (* contains a list of all        *)
                                      (* enumerations which are        *)
                                      (* visable within this scope.    *)
         InitTree(NamedObjects) ;     (* names of all items declared.  *)
         InitTree(NamedImports) ;     (* Names of items imported.      *)
         InitTree(WhereImported) ;    (* Sym to TokenNo where import   *)
                                      (* occurs. Error message use.    *)
         Priority := NulSym ;         (* Priority of the module. This  *)
                                      (* is an index to a constant.    *)
         InitTree(Unresolved) ;       (* All symbols currently         *)
                                      (* unresolved in this module.    *)
         StartQuad := 0 ;             (* Signify the initialization    *)
                                      (* code.                         *)
         EndQuad := 0 ;               (* EndQuad should point to a     *)
                                      (* goto quad.                    *)
         StartFinishQuad := 0 ;       (* Signify the finalization      *)
                                      (* code.                         *)
         EndFinishQuad := 0 ;         (* should point to a finish      *)
         FinallyFunction := NIL ;     (* The GCC function for finally  *)
         ExceptionFinally := FALSE ;  (* does it have an exception?    *)
         ExceptionBlock := FALSE ;    (* does it have an exception?    *)
         ContainsHiddenType := FALSE ;(* True if this module           *)
                                      (* implements a hidden type.     *)
         ContainsBuiltin := FALSE ;   (* Does module define a builtin  *)
                                      (* procedure?                    *)
         ForC := FALSE ;              (* Is it a definition for "C"    *)
         NeedExportList := FALSE ;    (* Must user supply export list? *)
         InitList(ListOfVars) ;       (* List of variables in this     *)
                                      (* scope.                        *)
         InitList(ListOfProcs) ;      (* List of all procedures        *)
                                      (* declared within this module.  *)
         InitList(ListOfModules) ;    (* List of all inner modules.    *)
         InitWhereDeclaredTok(tok, At) ;  (* Where symbol declared.        *)
         InitWhereFirstUsedTok(tok, At) ; (* Where symbol first used.      *)
      END
   END ;
   PutSymKey(ModuleTree, DefImpName, Sym) ;
   RETURN Sym
END MakeDefImp ;


(*
   MakeProcedure - creates a procedure sym with name. It returns
                   the symbol index.
*)

PROCEDURE MakeProcedure (tok: CARDINAL; ProcedureName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   Sym := DeclareSym(tok, ProcedureName) ;
   IF NOT IsError(Sym)
   THEN
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         SymbolType := ProcedureSym ;
         WITH Procedure DO
            name := ProcedureName ;
            InitList(ListOfParam) ;      (* Contains a list of all the    *)
                                         (* parameters in this procedure. *)
            ParamDefined := FALSE ;      (* Have the parameters been      *)
                                         (* defined yet?                  *)
            DefinedInDef := FALSE ;      (* Were the parameters defined   *)
                                         (* in the Definition module?     *)
                                         (* Note that this depends on     *)
                                         (* whether the compiler has read *)
                                         (* the .def or .mod first.       *)
                                         (* The second occurence is       *)
                                         (* compared to the first.        *)
            DefinedInImp := FALSE ;      (* Were the parameters defined   *)
                                         (* in the Implementation module? *)
                                         (* Note that this depends on     *)
                                         (* whether the compiler has read *)
                                         (* the .def or .mod first.       *)
                                         (* The second occurence is       *)
                                         (* compared to the first.        *)
            HasVarArgs := FALSE ;        (* Does the procedure use ... ?  *)
            HasOptArg := FALSE ;         (* Does this procedure use [ ] ? *)
            OptArgInit := NulSym ;       (* The optarg initial value.     *)
            IsBuiltin := FALSE ;         (* Was it declared __BUILTIN__ ? *)
            BuiltinName := NulName ;     (* name of equivalent builtin    *)
            IsInline := FALSE ;          (* Was is declared __INLINE__ ?  *)
            ReturnOptional := FALSE ;    (* Is the return value optional? *)
            Scope := GetCurrentScope() ; (* Scope of procedure.           *)
            InitTree(Unresolved) ;       (* All symbols currently         *)
                                         (* unresolved in this procedure. *)
            ScopeQuad := 0 ;             (* Index into list of quads,     *)
            StartQuad := 0 ;             (* defining the scope, start and *)
            EndQuad := 0 ;               (* end of the procedure.         *)
            Reachable := FALSE ;         (* Procedure not known to be     *)
                                         (* reachable.                    *)
            SavePriority := FALSE ;      (* Does procedure need to save   *)
                                         (* and restore interrupts?       *)
            ReturnType := NulSym ;       (* Not a function yet!           *)
            Offset := 0 ;                (* Location of procedure.        *)
            InitTree(LocalSymbols) ;
            InitList(EnumerationScopeList) ;
                                         (* Enumeration scope list which  *)
                                         (* contains a list of all        *)
                                         (* enumerations which are        *)
                                         (* visable within this scope.    *)
            InitTree(NamedObjects) ;     (* Names of all items declared.  *)
            InitList(ListOfVars) ;       (* List of variables in this     *)
                                         (* scope.                        *)
            InitList(ListOfProcs) ;      (* List of all procedures        *)
                                         (* declared within this          *)
                                         (* procedure.                    *)
            InitList(ListOfModules) ;    (* List of all inner modules.    *)
            ExceptionFinally := FALSE ;  (* does it have an exception?    *)
            ExceptionBlock := FALSE ;    (* does it have an exception?    *)
            Size := InitValue() ;        (* Activation record size.       *)
            TotalParamSize
                       := InitValue() ;  (* size of all parameters.       *)
            Begin := 0 ;                 (* token number for BEGIN        *)
            End := 0 ;                   (* token number for END          *)
            InitWhereDeclaredTok(tok, At) ;  (* Where symbol declared.        *)
         END
      END ;
      (* Now add this procedure to the symbol table of the current scope *)
      AddSymToScope(Sym, ProcedureName) ;
      AddProcedureToList(GetCurrentScope(), Sym)
   END ;
   RETURN Sym
END MakeProcedure ;


(*
   AddProcedureToList - adds a procedure, Proc, to the list of procedures
                        in module, Mod.
*)

PROCEDURE AddProcedureToList (Mod, Proc: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Mod) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : PutItemIntoList(DefImp.ListOfProcs, Proc) |
      ModuleSym   : PutItemIntoList(Module.ListOfProcs, Proc) |
      ProcedureSym: PutItemIntoList(Procedure.ListOfProcs, Proc)

      ELSE
         InternalError ('expecting ModuleSym, DefImpSym or ProcedureSym symbol')
      END
   END
END AddProcedureToList ;


(*
   AddVarToScopeList - adds symbol, sym, to, scope.
*)

PROCEDURE AddVarToScopeList (scope, sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(scope) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: PutItemIntoList(Procedure.ListOfVars, sym) |
      ModuleSym   : PutItemIntoList(Module.ListOfVars, sym) |
      DefImpSym   : PutItemIntoList(DefImp.ListOfVars, sym)

      ELSE
         InternalError ('expecting Procedure or Module symbol')
      END
   END
END AddVarToScopeList ;


(*
   AddVarToList - add a variable symbol to the list of variables maintained
                  by the inner most scope. (Procedure or Module).
*)

PROCEDURE AddVarToList (Sym: CARDINAL) ;
VAR
   pCall: PtrToCallFrame ;
BEGIN
   pCall := GetPcall(ScopePtr) ;
   AddVarToScopeList(pCall^.Main, Sym)
END AddVarToList ;


(*
   MakeVar - creates a variable sym with VarName. It returns the
             symbol index.
*)

PROCEDURE MakeVar (tok: CARDINAL; VarName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   Sym := DeclareSym (tok, VarName) ;
   IF NOT IsError(Sym)
   THEN
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         SymbolType := VarSym ;
         WITH Var DO
            name := VarName ;
            Type := NulSym ;
            BackType := NulSym ;
            Size := InitValue() ;
            Offset := InitValue() ;
            AddrMode := RightValue ;
            Scope := GetCurrentScope() ;  (* Procedure or Module?  *)
            AtAddress := FALSE ;
            Address := NulSym ;           (* Address at which declared.  *)
            IsTemp := FALSE ;
            IsComponentRef := FALSE ;
            IsParam := FALSE ;
            IsPointerCheck := FALSE ;
            IsWritten := FALSE ;
            IsSSA := FALSE ;
            InitWhereDeclaredTok(tok, At) ;
            InitWhereFirstUsedTok(tok, At) ;   (* Where symbol first used.  *)
            InitList(ReadUsageList[RightValue]) ;
            InitList(WriteUsageList[RightValue]) ;
            InitList(ReadUsageList[LeftValue]) ;
            InitList(WriteUsageList[LeftValue])
         END
      END ;
      (* Add Var to Procedure or Module variable list.  *)
      AddVarToList(Sym) ;
      (* Now add this Var to the symbol table of the current scope.  *)
      AddSymToScope(Sym, VarName)
   END ;
   RETURN Sym
END MakeVar ;


(*
   PutExceptionBlock - sets a BOOLEAN in block module/procedure/defimp,
                       sym, indicating that this block as an EXCEPT
                       statement sequence.
*)

PROCEDURE PutExceptionBlock (sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.ExceptionBlock := TRUE |
      ModuleSym   : Module.ExceptionBlock := TRUE |
      DefImpSym   : DefImp.ExceptionBlock := TRUE

      ELSE
         InternalError ('expecting Procedure')
      END
   END
END PutExceptionBlock ;


(*
   HasExceptionBlock - returns a BOOLEAN determining whether
                       module/procedure/defimp, sym, has
                       an EXCEPT statement sequence.
*)

PROCEDURE HasExceptionBlock (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.ExceptionBlock ) |
      ModuleSym   : RETURN( Module.ExceptionBlock ) |
      DefImpSym   : RETURN( DefImp.ExceptionBlock )

      ELSE
         InternalError ('expecting Procedure')
      END
   END
END HasExceptionBlock ;


(*
   PutExceptionFinally - sets a BOOLEAN in block module/defimp,
                         sym, indicating that this FINALLY block
                         as an EXCEPT statement sequence.
*)

PROCEDURE PutExceptionFinally (sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.ExceptionFinally := TRUE |
      ModuleSym   : Module.ExceptionFinally := TRUE |
      DefImpSym   : DefImp.ExceptionFinally := TRUE

      ELSE
         InternalError ('expecting DefImp or Module symbol')
      END
   END
END PutExceptionFinally ;


(*
   HasExceptionFinally - returns a BOOLEAN determining whether
                         module/defimp, sym, has
                         an EXCEPT statement sequence.
*)

PROCEDURE HasExceptionFinally (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.ExceptionFinally ) |
      ModuleSym   : RETURN( Module.ExceptionFinally ) |
      DefImpSym   : RETURN( DefImp.ExceptionFinally )

      ELSE
         InternalError ('expecting DefImp or Module symbol')
      END
   END
END HasExceptionFinally ;


(*
   FillInRecordFields - given a new symbol, sym, make it a record symbol
                        and initialize its fields.
*)

PROCEDURE FillInRecordFields (tok: CARDINAL; sym: CARDINAL; RecordName: Name;
                              scope: CARDINAL; oaf: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF NOT IsError(sym)
   THEN
      pSym := GetPsym (sym) ;
      WITH pSym^ DO
         SymbolType := RecordSym ;
         WITH Record DO
            name := RecordName ;
            InitTree (LocalSymbols) ;
            Size := InitValue () ;
            InitList (ListOfSons) ;   (* List of RecordFieldSym and VarientSym *)
            oafamily := oaf ;
            Parent := NulSym ;
            Align := NulSym ;
            DefaultAlign := NulSym ;
            DeclPacked := FALSE ;
            DeclResolved := FALSE ;
            Scope := scope ;
            InitWhereDeclaredTok (tok, At)
         END
      END
   END
END FillInRecordFields ;


(*
   HandleHiddenOrDeclare -
*)

PROCEDURE HandleHiddenOrDeclare (tok: CARDINAL; name: Name; VAR oaf: CARDINAL) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := CheckForHiddenType (name) ;
   IF sym=NulSym
   THEN
      sym := DeclareSym (tok, name) ;
      IF NOT IsError (sym)
      THEN
         (* Now add this type to the symbol table of the current scope *)
         AddSymToScope (sym, name)
      END
   END ;
   oaf := GetOAFamily (sym) ;
   RETURN sym
END HandleHiddenOrDeclare ;


(*
   MakeRecord - makes a Record symbol with name RecordName.
*)

PROCEDURE MakeRecord (tok: CARDINAL; RecordName: Name) : CARDINAL ;
VAR
   oaf, sym: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare (tok, RecordName, oaf) ;
   FillInRecordFields (tok, sym, RecordName, GetCurrentScope (), oaf) ;
   ForeachOAFamily (oaf, doFillInOAFamily) ;
   RETURN sym
END MakeRecord ;


(*
   MakeVarient - creates a new symbol, a varient symbol for record or varient field
                 symbol, RecOrVarFieldSym.
*)

PROCEDURE MakeVarient (tok: CARDINAL; RecOrVarFieldSym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   NewSym (Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := VarientSym ;
      WITH Varient DO
         Size := InitValue() ;
         Parent := RecOrVarFieldSym ; (* GetRecord(RecOrVarFieldSym) ; *)
         IF IsRecord(RecOrVarFieldSym)
         THEN
            Varient := NulSym
         ELSE
            Varient := RecOrVarFieldSym
         END ;
         tag := NulSym ;
         DeclPacked := FALSE ;
         Scope := GetCurrentScope() ;
         InitList(ListOfSons) ;
         InitWhereDeclaredTok(tok, At)
      END
   END ;
   (* Now add Sym to the record RecSym field list *)
   pSym := GetPsym(RecOrVarFieldSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym      : PutItemIntoList(Record.ListOfSons, Sym) |
      VarientFieldSym: PutItemIntoList(VarientField.ListOfSons, Sym)

      ELSE
         InternalError ('expecting Record or VarientField symbol')
      END
   END ;
   RETURN Sym
END MakeVarient ;


(*
   GetRecord - fetches the record symbol from the parent of Sym.
               Sym maybe a varient symbol in which case its parent is searched
               etc.
*)

PROCEDURE GetRecord (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym      : RETURN Sym  |
      VarientSym     : RETURN GetRecord(Varient.Parent)  |
      VarientFieldSym: RETURN GetRecord(VarientField.Parent)

      ELSE
         InternalError ('expecting Record or Varient symbol')
      END
   END
END GetRecord ;


(*
   PutDeclaredPacked - sets the Packed field of the record or record field symbol.
*)

PROCEDURE PutDeclaredPacked (sym: CARDINAL; b: BOOLEAN) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym      :  Record.DeclPacked := b ;
                        Record.DeclResolved := TRUE |
      RecordFieldSym :  RecordField.DeclPacked := b ;
                        RecordField.DeclResolved := TRUE |
      VarientFieldSym:  VarientField.DeclPacked := b ;
                        VarientField.DeclResolved := TRUE |
      VarientSym     :  Varient.DeclPacked := b ;
                        Varient.DeclResolved := TRUE

      ELSE
         InternalError ('expecting a record or field record symbol')
      END
   END
END PutDeclaredPacked ;


(*
   IsDeclaredPacked - was the record symbol or record field, sym,
                      declared as packed?
*)

PROCEDURE IsDeclaredPacked (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym      :  RETURN Record.DeclPacked |
      RecordFieldSym :  RETURN RecordField.DeclPacked |
      VarientFieldSym:  RETURN VarientField.DeclPacked |
      VarientSym     :  RETURN Varient.DeclPacked

      ELSE
         InternalError ('expecting a record or a record field symbol')
      END
   END
END IsDeclaredPacked ;


(*
   IsDeclaredPackedResolved - do we know if the record symbol or record
                              field, sym, declared as packed or not packed?
*)

PROCEDURE IsDeclaredPackedResolved (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym      :  RETURN Record.DeclResolved |
      RecordFieldSym :  RETURN RecordField.DeclResolved |
      VarientFieldSym:  RETURN VarientField.DeclResolved |
      VarientSym     :  RETURN Varient.DeclResolved

      ELSE
         InternalError ('expecting a record or a record field symbol')
      END
   END
END IsDeclaredPackedResolved ;


(*
   MakeEnumeration - places a new symbol in the current scope, the symbol
                     is an enumeration symbol. The symbol index is returned.
*)

PROCEDURE MakeEnumeration (tok: CARDINAL; EnumerationName: Name) : CARDINAL ;
VAR
   pSym    : PtrToSymbol ;
   sym, oaf: CARDINAL ;
BEGIN
   sym := CheckForHiddenType (EnumerationName) ;
   IF sym=NulSym
   THEN
      sym := DeclareSym (tok, EnumerationName) ;
      oaf := GetOAFamily (sym) ;
      IF NOT IsError (sym)
      THEN
         pSym := GetPsym (sym) ;
         pSym^.SymbolType := EnumerationSym ; (* To satisfy AddSymToScope *)
         (* Now add this type to the symbol table of the current scope *)
         AddSymToScope (sym, EnumerationName)
      END
   ELSE
      oaf := GetOAFamily (sym)
   END ;
   IF NOT IsError (sym)
   THEN
      pSym := GetPsym (sym) ;
      WITH pSym^ DO
         SymbolType := EnumerationSym ;
         WITH Enumeration DO
            name := EnumerationName ;      (* Name of enumeration.   *)
            NoOfElements := 0 ;            (* No of elements in the  *)
                                           (* enumeration type.      *)
            Size := InitValue () ;         (* Size at runtime of sym *)
            InitTree (LocalSymbols) ;      (* Enumeration fields.    *)
            InitPacked (packedInfo) ;      (* not packed and no      *)
                                           (* equivalent (yet).      *)
            oafamily := oaf ;              (* The open array family  *)
            Scope := GetCurrentScope () ;  (* Which scope created it *)
            InitWhereDeclaredTok (tok, At) (* Declared here          *)
         END
      END ;
      CheckIfEnumerationExported (sym, ScopePtr)
   END ;
   ForeachOAFamily (oaf, doFillInOAFamily) ;
   RETURN sym
END MakeEnumeration ;


(*
   MakeType - makes a type symbol with name TypeName.
*)

PROCEDURE MakeType (tok: CARDINAL; TypeName: Name) : CARDINAL ;
VAR
   pSym    : PtrToSymbol ;
   sym, oaf: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare (tok, TypeName, oaf) ;
   IF NOT IsError(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         SymbolType := TypeSym ;
         WITH Type DO
            name := TypeName ;        (* Index into name array, name *)
                                      (* of type.                    *)
            Type := NulSym ;          (* Index to a type symbol.     *)
            IsHidden := FALSE ;       (* Was it declared as hidden?  *)
	    InitTree(ConstLitTree) ;  (* constants of this type.     *)
            Size := InitValue() ;     (* Runtime size of symbol.     *)
            Align := NulSym ;         (* Alignment of this type.     *)
            InitPacked(packedInfo) ;  (* not packed and no           *)
                                      (* equivalent yet.             *)
            oafamily := oaf ;         (* The open array family.      *)
            Scope := GetCurrentScope() ;   (* Which scope created it *)
            InitWhereDeclaredTok(tok, At)  (* Declared here               *)
         END
      END
   END ;
   ForeachOAFamily(oaf, doFillInOAFamily) ;
   RETURN sym
END MakeType ;


(*
   MakeHiddenType - makes a type symbol that is hidden from the
                    definition module.
                    This symbol is placed into the UnImplemented list of
                    the definition/implementation module.
                    The type will be filled in when the implementation module
                    is reached.
*)

PROCEDURE MakeHiddenType (tok: CARDINAL; TypeName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   Sym := DeclareSym (tok, TypeName) ;
   IF NOT IsError(Sym)
   THEN
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         SymbolType := TypeSym ;
         WITH Type DO
            name   := TypeName ;    (* Index into name array, name *)
                                    (* of type.                    *)
            IsHidden := GetMainModule()#GetCurrentScope() ;
            IF ExtendedOpaque OR (NOT IsHidden)
            THEN
               Type := NulSym       (* will be filled in later     *)
            ELSE
               Type := Address
            END ;
            Align := NulSym ;       (* Alignment of this type.     *)
            Scope := GetCurrentScope() ; (* Which scope created it *)
            oafamily := NulSym ;
            IF NOT ExtendedOpaque
            THEN
               IncludeItemIntoList(AddressTypes, Sym)
            END ;
            Size := InitValue() ;   (* Runtime size of symbol.     *)
            InitWhereDeclaredTok(tok, At) (* Declared here         *)
         END
      END ;
      PutExportUnImplemented (tok, Sym) ;
      IF ExtendedOpaque OR (GetMainModule()=GetCurrentScope())
      THEN
         PutHiddenTypeDeclared
      END ;
      (* Now add this type to the symbol table of the current scope *)
      AddSymToScope(Sym, TypeName)
   END ;
   RETURN Sym
END MakeHiddenType ;


(*
   GetConstFromTypeTree - return a constant symbol from the tree owned by constType.
                          NulSym is returned if the symbol is unknown.
*)

PROCEDURE GetConstFromTypeTree (constName: Name; constType: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF constType=NulSym
   THEN
      RETURN GetSymKey(ConstLitTree, constName)
   ELSE
      pSym := GetPsym(constType) ;
      Assert(IsType(constType) OR IsSubrange(constType) OR IsPointer(constType)) ;
      WITH pSym^ DO
         CASE SymbolType OF

         TypeSym    :  RETURN GetSymKey (Type.ConstLitTree, constName) |
	 SubrangeSym:  RETURN GetSymKey (Subrange.ConstLitTree, constName) |
	 PointerSym :  RETURN GetSymKey (Pointer.ConstLitTree, constName)

         ELSE
            InternalError ('expecting Type symbol')
         END
      END
   END
END GetConstFromTypeTree ;


(*
   PutConstIntoTypeTree - places, constSym, into the tree of constants owned by, constType.
                          constName is the name of constSym.
*)

PROCEDURE PutConstIntoTypeTree (constName: Name; constType: CARDINAL; constSym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF constType=NulSym
   THEN
      PutSymKey(ConstLitTree, constName, constSym)
   ELSE
      pSym := GetPsym(constType) ;
      Assert(IsType(constType) OR IsSubrange(constType) OR IsPointer(constType)) ;
      WITH pSym^ DO
         CASE SymbolType OF

         TypeSym    :  PutSymKey (Type.ConstLitTree, constName, constSym) |
         SubrangeSym:  PutSymKey (Subrange.ConstLitTree, constName, constSym) |
         PointerSym :  PutSymKey (Pointer.ConstLitTree, constName, constSym)

         ELSE
            InternalError ('expecting Type symbol')
         END
      END
   END
END PutConstIntoTypeTree ;


(*
   MakeConstLit - returns a constant literal of type, constType, with a constName,
                  at location, tok.
*)

PROCEDURE MakeConstLit (tok: CARDINAL; constName: Name; constType: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   IF constType=NulSym
   THEN
      constType := GetConstLitType (constName)
   END ;
   NewSym (Sym) ;
   pSym := GetPsym (Sym) ;
   WITH pSym^ DO
      SymbolType := ConstLitSym ;
      CASE SymbolType OF

      ConstLitSym : ConstLit.name := constName ;
                    ConstLit.Value := InitValue () ;
                    PushString (constName) ;
                    PopInto (ConstLit.Value) ;
                    ConstLit.Type := constType ;
                    ConstLit.IsSet := FALSE ;
                    ConstLit.IsConstructor := FALSE ;
                    ConstLit.FromType := NulSym ;     (* type is determined FromType *)
                    ConstLit.UnresFromType := FALSE ; (* is Type resolved?           *)
                    InitWhereDeclaredTok (tok, ConstLit.At) ;
                    InitWhereFirstUsedTok (tok, ConstLit.At)

      ELSE
         InternalError ('expecting ConstLit symbol')
      END
   END ;
   RETURN Sym
END MakeConstLit ;


(*
   MakeConstVar - makes a ConstVar type with
                  name ConstVarName.
*)

PROCEDURE MakeConstVar (tok: CARDINAL; ConstVarName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   Sym := DeclareSym (tok, ConstVarName) ;
   IF NOT IsError(Sym)
   THEN
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         SymbolType := ConstVarSym ;
         WITH ConstVar DO
            name  := ConstVarName ;
            Value := InitValue() ;
            Type  := NulSym ;
            IsSet := FALSE ;
            IsConstructor := FALSE ;
            FromType := NulSym ;     (* type is determined FromType *)
            UnresFromType := FALSE ; (* is Type resolved?           *)
            IsTemp := FALSE ;
            InitWhereDeclaredTok (tok, At)
         END
      END ;
      (* Now add this constant to the symbol table of the current scope *)
      AddSymToScope(Sym, ConstVarName)
   END ;
   RETURN( Sym )
END MakeConstVar ;


(*
   MakeConstLitString - put a constant which has the string described by
                        ConstName into the ConstantTree.
                        The symbol number is returned.
                        This symbol is known as a String Constant rather than a
                        ConstLit which indicates a number.
                        If the constant already exits
                        then a duplicate constant is not entered in the tree.
                        All values of constant strings
                        are ignored in Pass 1 and evaluated in Pass 2 via
                        character manipulation.
                        In this procedure ConstName is the string.
*)

PROCEDURE MakeConstLitString (tok: CARDINAL; ConstName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   sym : CARDINAL ;
BEGIN
   sym := GetSymKey (ConstLitStringTree, ConstName) ;
   IF sym=NulSym
   THEN
      NewSym (sym) ;
      PutSymKey (ConstLitStringTree, ConstName, sym) ;
      pSym := GetPsym (sym) ;
      WITH pSym^ DO
         SymbolType := ConstStringSym ;
         CASE SymbolType OF

         ConstStringSym: InitConstString (tok, sym, ConstName, ConstName,
                                          m2str,
                                          sym, NulSym, NulSym, NulSym)

         ELSE
            InternalError ('expecting ConstString symbol')
         END
      END
   END ;
   RETURN sym
END MakeConstLitString ;


(*
   BackFillString -
*)

PROCEDURE BackFillString (sym, m2sym, m2nulsym, csym, cnulsym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF sym # NulSym
   THEN
      pSym := GetPsym (sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ConstStringSym:  ConstString.M2Variant := m2sym ;
                          ConstString.NulM2Variant := m2nulsym ;
                          ConstString.CVariant := csym ;
                          ConstString.NulCVariant := cnulsym

         ELSE
            InternalError ('expecting ConstStringSym')
         END
      END
   END
END BackFillString ;


(*
   InitConstString - initialize the constant string and back fill any
                     previous string variants.
*)

PROCEDURE InitConstString (tok: CARDINAL; sym: CARDINAL; name, contents: Name;
                           kind: ConstStringVariant;
                           m2sym, m2nulsym, csym, cnulsym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      SymbolType := ConstStringSym ;
      CASE SymbolType OF

      ConstStringSym:  ConstString.name := name ;
                       ConstString.StringVariant := kind ;
                       PutConstString (tok, sym, contents) ;
                       BackFillString (sym,
                                       m2sym, m2nulsym, csym, cnulsym) ;
                       BackFillString (m2sym,
                                       m2sym, m2nulsym, csym, cnulsym) ;
                       BackFillString (m2nulsym,
                                       m2sym, m2nulsym, csym, cnulsym) ;
                       BackFillString (csym,
                                       m2sym, m2nulsym, csym, cnulsym) ;
                       BackFillString (cnulsym,
                                       m2sym, m2nulsym, csym, cnulsym) ;
                       InitWhereDeclaredTok (tok, ConstString.At)

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END InitConstString ;


(*
   GetConstStringM2 - returns the Modula-2 variant of a string
                      (with no added nul terminator).
*)

PROCEDURE GetConstStringM2 (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym:  RETURN ConstString.M2Variant

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END GetConstStringM2 ;


(*
   GetConstStringC - returns the C variant of a string
                     (with no added nul terminator).
*)

PROCEDURE GetConstStringC (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym:  RETURN ConstString.CVariant

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END GetConstStringC ;


(*
   GetConstStringM2nul - returns the Modula-2 variant of a string
                         (with added nul terminator).
*)

PROCEDURE GetConstStringM2nul (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym:  RETURN ConstString.NulM2Variant

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END GetConstStringM2nul ;


(*
   GetConstStringCnul - returns the C variant of a string
                        (with no added nul terminator).
*)

PROCEDURE GetConstStringCnul (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym:  RETURN ConstString.NulCVariant

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END GetConstStringCnul ;


(*
   IsConstStringNulTerminated - returns TRUE if the constant string, sym,
                                should be created with a nul terminator.
*)

PROCEDURE IsConstStringNulTerminated (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym:  RETURN ((ConstString.StringVariant = m2nulstr) OR
                               (ConstString.StringVariant = cnulstr))

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END IsConstStringNulTerminated ;


(*
   MakeConstStringCnul - creates a constant string nul terminated string suitable for C.
                         sym is a ConstString and a new symbol is returned
                         with the escape sequences converted into characters.
*)

PROCEDURE MakeConstStringCnul (tok: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   pSym  : PtrToSymbol ;
   newstr: CARDINAL ;
BEGIN
   pSym := GetPsym (GetConstStringM2 (sym)) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym:  Assert (ConstString.StringVariant = m2str) ;
                       ConstString.CVariant := MakeConstStringC (tok, sym) ;
                       IF ConstString.NulCVariant = NulSym
                       THEN
                          NewSym (newstr) ;
                          ConstString.NulCVariant := newstr ;
                          InitConstString (tok, newstr, ConstString.name, GetString (ConstString.CVariant),
                                           cnulstr,
                                           ConstString.M2Variant, ConstString.NulM2Variant, ConstString.CVariant, ConstString.NulCVariant)
                       END ;
                       RETURN ConstString.NulCVariant

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END MakeConstStringCnul ;


(*
   MakeConstStringM2nul - creates a constant string nul terminated string.
                          sym is a ConstString and a new symbol is returned.
*)

PROCEDURE MakeConstStringM2nul (tok: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (GetConstStringM2 (sym)) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym:  Assert (ConstString.StringVariant = m2str) ;
                       IF ConstString.NulM2Variant = NulSym
                       THEN
                          NewSym (ConstString.NulM2Variant) ;
                          InitConstString (tok, ConstString.NulM2Variant,
                                           ConstString.name, ConstString.Contents,
                                           m2nulstr,
                                           ConstString.M2Variant, ConstString.NulM2Variant,
                                           ConstString.CVariant, ConstString.NulCVariant)
                       END ;
                       RETURN ConstString.NulM2Variant

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END MakeConstStringM2nul ;


(*
   MakeConstStringC - creates a constant string suitable for C.
                      sym is a Modula-2 ConstString and a new symbol is returned
                      with the escape sequences converted into characters.
                      It is not nul terminated.
*)

PROCEDURE MakeConstStringC (tok: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   pSym  : PtrToSymbol ;
   s     : String ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym:  IF ConstString.StringVariant = cstr
                       THEN
                          RETURN sym   (* this is already the C variant.  *)
                       ELSIF ConstString.CVariant = NulSym
                       THEN
                          Assert (ConstString.StringVariant = m2str) ;  (* we can only derive string variants from Modula-2 strings.  *)
                          Assert (sym = ConstString.M2Variant) ;
                          (* we need to create a new one and return the new symbol.  *)
                          s := HandleEscape (InitStringCharStar (KeyToCharStar (GetString (ConstString.M2Variant)))) ;
                          NewSym (ConstString.CVariant) ;
                          InitConstString (tok, ConstString.CVariant, ConstString.name, makekey (string (s)),
                                           cstr,
                                           ConstString.M2Variant, ConstString.NulM2Variant, ConstString.CVariant, ConstString.NulCVariant) ;
                          s := KillString (s)
                       END ;
                       RETURN ConstString.CVariant

      ELSE
         InternalError ('expecting ConstStringSym')
      END
   END
END MakeConstStringC ;


(*
   MakeConstString - puts a constant into the symboltable which is a string.
                     The string value is unknown at this time and will be
                     filled in later by PutString.
*)

PROCEDURE MakeConstString (tok: CARDINAL; ConstName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   sym : CARDINAL ;
BEGIN
   NewSym (sym) ;
   PutSymKey (ConstLitStringTree, ConstName, sym) ;
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      SymbolType := ConstStringSym ;
      CASE SymbolType OF

      ConstStringSym :  InitConstString (tok, sym, ConstName, NulName,
                                         m2str, sym, NulSym, NulSym, NulSym)

      ELSE
         InternalError ('expecting ConstString symbol')
      END
   END ;
   RETURN sym
END MakeConstString ;


(*
   PutConstString - places a string, String, into a constant symbol, Sym.
                    Sym maybe a ConstString or a ConstVar.  If the later is
                    true then the ConstVar is converted to a ConstString.
*)

PROCEDURE PutConstString (tok: CARDINAL; sym: CARDINAL; contents: Name) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym: ConstString.Length := LengthKey (contents) ;
                      ConstString.Contents := contents ;
                      InitWhereFirstUsedTok (tok, ConstString.At) |

      ConstVarSym   : (* ok altering this to ConstString *)
                      (* copy name and alter symbol.     *)
                      InitConstString (tok, sym, ConstVar.name, contents,
                                       m2str,
                                       sym, NulSym, NulSym, NulSym)

      ELSE
         InternalError ('expecting ConstString or ConstVar symbol')
      END
   END
END PutConstString ;


(*
   IsConstStringM2 - returns whether this conststring is an unaltered Modula-2 string.
*)

PROCEDURE IsConstStringM2 (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym: RETURN ConstString.StringVariant = m2str

      ELSE
         InternalError ('expecting ConstString symbol')
      END
   END
END IsConstStringM2 ;


(*
   IsConstStringC - returns whether this conststring is a C style string
                    which will have any escape translated.
*)

PROCEDURE IsConstStringC (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym: RETURN ConstString.StringVariant = cstr

      ELSE
         InternalError ('expecting ConstString symbol')
      END
   END
END IsConstStringC ;


(*
   IsConstStringM2nul - returns whether this conststring is a Modula-2 string which
                        contains a nul terminator.
*)

PROCEDURE IsConstStringM2nul (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym: RETURN ConstString.StringVariant = m2nulstr

      ELSE
         InternalError ('expecting ConstString symbol')
      END
   END
END IsConstStringM2nul ;


(*
   IsConstStringCnul - returns whether this conststring is a C style string
                       which will have any escape translated and also contains
                       a nul terminator.
*)

PROCEDURE IsConstStringCnul (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym: RETURN ConstString.StringVariant = cnulstr

      ELSE
         InternalError ('expecting ConstString symbol')
      END
   END
END IsConstStringCnul ;


(*
   GetString - returns the contents of the string symbol sym, note that
               this is not the same as GetName (unless it was a literal).
*)

PROCEDURE GetString (Sym: CARDINAL) : Name ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym: RETURN ConstString.Contents

      ELSE
         InternalError ('expecting ConstString symbol')
      END
   END
END GetString ;


(*
   GetStringLength - returns the length of the string symbol Sym.
*)

PROCEDURE GetStringLength (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym: RETURN ConstString.Length

      ELSE
         InternalError ('expecting ConstString symbol')
      END
   END
END GetStringLength ;


(*
   PutVariableAtAddress - determines that a variable, sym, is declared at
                          a specific address.
*)

PROCEDURE PutVariableAtAddress (sym: CARDINAL; address: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(sym#NulSym) ;
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:  Var.AtAddress := TRUE ;
               Var.Address := address

      ELSE
         InternalError ('expecting a variable symbol')
      END
   END
END PutVariableAtAddress ;


(*
   GetVariableAtAddress - returns the address at which variable, sym, is declared.
*)

PROCEDURE GetVariableAtAddress (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(sym#NulSym) ;
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:  RETURN( Var.Address )

      ELSE
         InternalError ('expecting a variable symbol')
      END
   END
END GetVariableAtAddress ;


(*
   IsVariableAtAddress - returns TRUE if a variable, sym, was declared at
                         a specific address.
*)

PROCEDURE IsVariableAtAddress (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(sym#NulSym) ;
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:  RETURN( Var.AtAddress )

      ELSE
         InternalError ('expecting a variable symbol')
      END
   END
END IsVariableAtAddress ;


(*
   PutVariableSSA - assigns value to the SSA field within variable sym.
*)

PROCEDURE PutVariableSSA (sym: CARDINAL; value: BOOLEAN) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert (sym#NulSym) ;
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:  Var.IsSSA := value

      ELSE
         InternalError ('expecting a variable symbol')
      END
   END
END PutVariableSSA ;


(*
   IsVariableSSA - returns TRUE if variable is known to be a SSA.
*)

PROCEDURE IsVariableSSA (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert (sym#NulSym) ;
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:  RETURN Var.IsSSA

      ELSE
         InternalError ('expecting a variable symbol')
      END
   END
END IsVariableSSA ;


(*
   PutPriority - places a interrupt, priority, value into module, module.
*)

PROCEDURE PutPriority (module: CARDINAL; priority: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(module#NulSym) ;
   pSym := GetPsym(module) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym:  DefImp.Priority := priority |
      ModuleSym:  Module.Priority := priority

      ELSE
         InternalError ('expecting DefImp or Module symbol')
      END
   END
END PutPriority ;


(*
   GetPriority - returns the interrupt priority which was assigned to
                 module, module.
*)

PROCEDURE GetPriority (module: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(module#NulSym) ;
   pSym := GetPsym(module) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym:  RETURN( DefImp.Priority ) |
      ModuleSym:  RETURN( Module.Priority )

      ELSE
         InternalError ('expecting DefImp or Module symbol')
      END
   END
END GetPriority ;


(*
   PutNeedSavePriority - set a boolean flag indicating that this procedure
                         needs to save and restore interrupts.
*)

PROCEDURE PutNeedSavePriority (sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.SavePriority := TRUE

      ELSE
         InternalError ('expecting procedure symbol')
      END
   END
END PutNeedSavePriority ;


(*
   GetNeedSavePriority - returns the boolean flag indicating whether this procedure
                         needs to save and restore interrupts.
*)

PROCEDURE GetNeedSavePriority (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.SavePriority )

      ELSE
         InternalError ('expecting procedure symbol')
      END
   END
END GetNeedSavePriority ;


(*
   GetProcedureBuiltin - returns the builtin name for the equivalent procedure, Sym.
*)

PROCEDURE GetProcedureBuiltin (Sym: CARDINAL) : Name ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.BuiltinName )

      ELSE
         InternalError ('expecting procedure symbol')
      END
   END
END GetProcedureBuiltin ;


(*
   PutProcedureBuiltin - assigns the builtin name for the equivalent procedure, Sym.
*)

PROCEDURE PutProcedureBuiltin (Sym: CARDINAL; name: Name) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym   : Procedure.BuiltinName := name ;
                       Procedure.IsBuiltin := TRUE ;
                       (* we use the same extra pass method as hidden types for builtins *)
                       PutHiddenTypeDeclared

      ELSE
         InternalError ('expecting procedure symbol')
      END
   END
END PutProcedureBuiltin ;


(*
   IsProcedureBuiltin - returns TRUE if this procedure has a builtin equivalent.
*)

PROCEDURE IsProcedureBuiltin (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym   : RETURN( Procedure.IsBuiltin )

      ELSE
         InternalError ('expecting procedure symbol')
      END
   END
END IsProcedureBuiltin ;


(*
   PutProcedureInline - determines that procedure, Sym, has been requested to be inlined.
*)

PROCEDURE PutProcedureInline (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym   : Procedure.IsInline := TRUE ;

      ELSE
         InternalError ('expecting procedure symbol')
      END
   END
END PutProcedureInline ;


(*
   IsProcedureBuiltin - returns TRUE if this procedure was declared as inlined.
*)

PROCEDURE IsProcedureInline (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym   : RETURN( Procedure.IsInline )

      ELSE
         InternalError ('expecting procedure symbol')
      END
   END
END IsProcedureInline ;


(*
   PutConstSet - informs the const var symbol, sym, that it is or will contain
                 a set value.
*)

PROCEDURE PutConstSet (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstVarSym:  ConstVar.IsSet := TRUE |
      ConstLitSym:  ConstLit.IsSet := TRUE

      ELSE
         InternalError ('expecting ConstVar symbol')
      END
   END
END PutConstSet ;


(*
   IsConstSet - returns TRUE if the constant is declared as a set.
*)

PROCEDURE IsConstSet (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstVarSym:  RETURN( ConstVar.IsSet ) |
      ConstLitSym:  RETURN( ConstLit.IsSet )

      ELSE
         RETURN( FALSE )
      END
   END
END IsConstSet ;


(*
   PutConstructor - informs the const var symbol, sym, that it is or
                    will contain a constructor (record, set or array)
                    value.
*)

PROCEDURE PutConstructor (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstVarSym:  ConstVar.IsConstructor := TRUE |
      ConstLitSym:  ConstLit.IsConstructor := TRUE

      ELSE
         InternalError ('expecting ConstVar or ConstLit symbol')
      END
   END
END PutConstructor ;


(*
   IsConstructor - returns TRUE if the constant is declared as a
                   constant set, array or record.
*)

PROCEDURE IsConstructor (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstVarSym:  RETURN( ConstVar.IsConstructor ) |
      ConstLitSym:  RETURN( ConstLit.IsConstructor )

      ELSE
         RETURN( FALSE )
      END
   END
END IsConstructor ;


(*
   PutConstructorFrom - sets the from type field in constructor,
                        Sym, to, from.
*)

PROCEDURE PutConstructorFrom (Sym: CARDINAL; from: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstVarSym:  ConstVar.FromType := from ;
                    ConstVar.UnresFromType := TRUE |
      ConstLitSym:  ConstLit.FromType := from ;
                    ConstLit.UnresFromType := TRUE

      ELSE
         InternalError ('expecting ConstVar or ConstLit symbol')
      END
   END ;
   IncludeItemIntoList(UnresolvedConstructorType, Sym)
END PutConstructorFrom ;


(*
   InitPacked - initialise packedInfo to FALSE and NulSym.
*)

PROCEDURE InitPacked (VAR packedInfo: PackedInfo) ;
BEGIN
   WITH packedInfo DO
      IsPacked := FALSE ;
      PackedEquiv := NulSym
   END
END InitPacked ;


(*
   doEquivalent - create a packed equivalent symbol for, sym, and return the
                  new symbol.  It sets both fields in packedInfo to FALSE
                  and the new symbol.
*)

PROCEDURE doEquivalent (VAR packedInfo: PackedInfo; sym: CARDINAL) : CARDINAL ;
VAR
   nSym: CARDINAL ;
   pSym: PtrToSymbol ;
BEGIN
   NewSym(nSym) ;
   pSym := GetPsym(nSym) ;
   WITH pSym^ DO
      SymbolType := EquivSym ;
      WITH Equiv DO
         nonPacked := sym ;
         packedInfo.IsPacked := TRUE ;
         packedInfo.PackedEquiv := NulSym
      END
   END ;
   packedInfo.IsPacked := FALSE ;
   packedInfo.PackedEquiv := nSym ;
   RETURN( nSym )
END doEquivalent ;


(*
   MakeEquivalent - return the equivalent packed symbol for, sym.
*)

PROCEDURE MakeEquivalent (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      EnumerationSym:  RETURN( doEquivalent(Enumeration.packedInfo, sym) ) |
      SubrangeSym   :  RETURN( doEquivalent(Subrange.packedInfo, sym) ) |
      TypeSym       :  RETURN( doEquivalent(Type.packedInfo, sym) ) |
      SetSym        :  RETURN( doEquivalent(Set.packedInfo, sym) )

      ELSE
         InternalError ('expecting type, subrange or enumerated type symbol')
      END
   END
END MakeEquivalent ;


(*
   GetEquivalent -
*)

PROCEDURE GetEquivalent (VAR packedInfo: PackedInfo; sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH packedInfo DO
      IF IsPacked
      THEN
         RETURN( sym )
      ELSIF PackedEquiv=NulSym
      THEN
         PackedEquiv := MakeEquivalent(sym)
      END ;
      RETURN( PackedEquiv )
   END
END GetEquivalent ;


(*
   GetPackedEquivalent - returns the packed equivalent of type, sym.
                         sym must be a type, subrange or enumerated type.
*)

PROCEDURE GetPackedEquivalent (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      EnumerationSym:  RETURN( GetEquivalent(Enumeration.packedInfo, sym) ) |
      SubrangeSym   :  RETURN( GetEquivalent(Subrange.packedInfo, sym) ) |
      TypeSym       :  RETURN( GetEquivalent(Type.packedInfo, sym) ) |
      SetSym        :  RETURN( GetEquivalent(Set.packedInfo, sym) )

      ELSE
         InternalError ('expecting type, subrange or enumerated type symbol')
      END
   END
END GetPackedEquivalent ;


(*
   GetNonPackedEquivalent - returns the equivalent non packed symbol associated with, sym.
*)

PROCEDURE GetNonPackedEquivalent (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      EquivSym:  RETURN( Equiv.nonPacked )

      ELSE
         InternalError ('expecting equivalent symbol')
      END
   END
END GetNonPackedEquivalent ;


(*
   IsEquivalent - returns TRUE if, sym, is an equivalent symbol.
*)

PROCEDURE IsEquivalent (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      EquivSym:  RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END
   END
END IsEquivalent ;


(*
   MakeSubrange - makes a new symbol into a subrange type with
                  name SubrangeName.
*)

PROCEDURE MakeSubrange (tok: CARDINAL; SubrangeName: Name) : CARDINAL ;
VAR
   pSym    : PtrToSymbol ;
   sym, oaf: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare (tok, SubrangeName, oaf) ;
   IF NOT IsError(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         SymbolType := SubrangeSym ;
         WITH Subrange DO
            name := SubrangeName ;
            Low := NulSym ;             (* Index to a symbol determining *)
                                        (* the lower bound of subrange.  *)
                                        (* Points to a constant -        *)
                                        (* possibly created by           *)
                                        (* ConstExpression.              *)
            High := NulSym ;            (* Index to a symbol determining *)
                                        (* the lower bound of subrange.  *)
                                        (* Points to a constant -        *)
                                        (* possibly created by           *)
                                        (* ConstExpression.              *)
            Type := NulSym ;            (* Index to a type. Determines   *)
                                        (* the type of subrange.         *)
            InitPacked(packedInfo) ;    (* not packed and no equivalent  *)
            InitTree(ConstLitTree) ;    (* constants of this type.       *)
            Size := InitValue() ;       (* Size determines the type size *)
            oafamily := oaf ;           (* The unbounded sym for this    *)
            Scope := GetCurrentScope() ;      (* Which scope created it  *)
            InitWhereDeclaredTok(tok, At)     (* Declared here           *)
         END
      END
   END ;
   ForeachOAFamily(oaf, doFillInOAFamily) ;
   RETURN sym
END MakeSubrange ;


(*
   MakeArray - makes an Array symbol with name ArrayName.
*)

PROCEDURE MakeArray (tok: CARDINAL; ArrayName: Name) : CARDINAL ;
VAR
   pSym    : PtrToSymbol ;
   sym, oaf: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare (tok, ArrayName, oaf) ;
   IF NOT IsError(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         SymbolType := ArraySym ;
         WITH Array DO
            name := ArrayName ;
            Subscript := NulSym ;   (* Contains the array subscripts.      *)
            Size := InitValue() ;   (* Size of array.                      *)
            Offset := InitValue() ; (* Offset of array.                    *)
            Type := NulSym ;        (* The Array Type. ARRAY OF Type.      *)
	    Large := FALSE ;        (* is this array large?                *)
            Align := NulSym ;       (* The alignment of this type.         *)
            oafamily := oaf ;       (* The unbounded for this array        *)
            Scope := GetCurrentScope() ;        (* Which scope created it  *)
            InitWhereDeclaredTok(tok, At)   (* Declared here               *)
         END
      END
   END ;
   ForeachOAFamily(oaf, doFillInOAFamily) ;
   RETURN( sym )
END MakeArray ;


(*
   PutArrayLarge - indicates that this is a large array in which case
                   the interface to gcc maps this array from 0..high-low,
                   using an integer indice.
*)

PROCEDURE PutArrayLarge (array: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF NOT IsError(array)
   THEN
      Assert(IsArray(array)) ;
      pSym := GetPsym(array) ;
      WITH pSym^.Array DO
         Large := TRUE
      END
   END
END PutArrayLarge ;


(*
   IsArrayLarge - returns TRUE if we need to treat this as a large array.
*)

PROCEDURE IsArrayLarge (array: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(IsArray(array)) ;
   pSym := GetPsym(array) ;
   RETURN( pSym^.Array.Large )
END IsArrayLarge ;


(*
   GetModule - Returns the Module symbol for the module with name, name.
*)

PROCEDURE GetModule (name: Name) : CARDINAL ;
BEGIN
   RETURN( GetSymKey(ModuleTree, name) )
END GetModule ;


(*
   GetLowestType - Returns the lowest type in the type chain of
                   symbol Sym.
                   If NulSym is returned then we assume type unknown or
                   you have reqested the type of a base type.
*)

PROCEDURE GetLowestType (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   type: CARDINAL ;
BEGIN
   Assert(Sym#NulSym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym              : type := Var.Type |
      ConstLitSym         : type := ConstLit.Type |
      ConstVarSym         : type := ConstVar.Type |
      ConstStringSym      : type := NulSym |  (* No type for a string *)
      TypeSym             : type := Type.Type |
      RecordFieldSym      : type := RecordField.Type |
      RecordSym           : type := NulSym |  (* No type for a record *)
      EnumerationFieldSym : type := EnumerationField.Type |
      EnumerationSym      : type := NulSym |  (* No type for enumeration *)
      PointerSym          : type := Sym |     (* we don't go to Pointer.Type *)
      ProcedureSym        : type := Procedure.ReturnType |
      ProcTypeSym         : type := ProcType.ReturnType |
      ParamSym            : type := Param.Type |
      VarParamSym         : type := VarParam.Type |
      SubrangeSym         : type := Subrange.Type |
      ArraySym            : type := Array.Type |
      SubscriptSym        : type := Subscript.Type |
      SetSym              : type := Set.Type |
      UnboundedSym        : type := Unbounded.Type |
      UndefinedSym        : type := NulSym |
      DummySym            : type := NulSym

      ELSE
         InternalError ('not implemented yet')
      END
   END ;
   pSym := GetPsym(Sym) ;
   IF (pSym^.SymbolType=TypeSym) AND (type=NulSym)
   THEN
      type := Sym             (* Base Type *)
   ELSIF (type#NulSym) AND IsType(type) AND (GetAlignment(type)=NulSym)
   THEN
      type := GetLowestType(type)   (* Type def *)
   END ;
   RETURN( type )
END GetLowestType ;


(*
   doGetType - subsiduary helper procedure function of GetDType, GetSType and GetLType.
*)

PROCEDURE doGetType (sym: CARDINAL; skipEquiv, skipAlign, skipHidden, skipBase: BOOLEAN) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   type: CARDINAL ;
BEGIN
   type := NulSym ;
   Assert (sym # NulSym) ;
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      OAFamilySym         : type := OAFamily.SimpleType |
      VarSym              : type := GetTypeOfVar(sym) |
      ConstLitSym         : type := ConstLit.Type |
      ConstVarSym         : type := ConstVar.Type |
      ConstStringSym      : IF ConstString.Length=1
                            THEN
                               type := Char
                            ELSE
                               type := NulSym  (* No type for a string *)
                            END |
      TypeSym             : type := Type.Type |
      RecordFieldSym      : type := RecordField.Type |
      RecordSym           : type := NulSym |  (* No type for a record *)
      VarientSym          : type := NulSym |  (* No type for a record *)
      EnumerationFieldSym : type := EnumerationField.Type |
      EnumerationSym      : type := NulSym |  (* No type for enumeration *)
      PointerSym          : type := Pointer.Type |
      ProcedureSym        : type := Procedure.ReturnType |
      ProcTypeSym         : type := ProcType.ReturnType |
      ParamSym            : type := Param.Type |
      VarParamSym         : type := VarParam.Type |
      SubrangeSym         : type := Subrange.Type |
      ArraySym            : type := Array.Type |
      SubscriptSym        : type := Subscript.Type |
      SetSym              : type := Set.Type |
      UnboundedSym        : type := Unbounded.Type |
      UndefinedSym        : type := NulSym |
      PartialUnboundedSym : type := PartialUnbounded.Type |
      ObjectSym           : type := NulSym

      ELSE
         InternalError ('not implemented yet')
      END
   END ;
   IF (type=NulSym) AND IsType(sym) AND (NOT skipBase)
   THEN
      RETURN sym             (* sym is a base type *)
   ELSIF type#NulSym
   THEN
      IF IsType(type) AND skipEquiv
      THEN
         IF (NOT IsHiddenType(type)) OR skipHidden
         THEN
            IF (GetAlignment(type)=NulSym) OR skipAlign
            THEN
               RETURN doGetType (type, skipEquiv, skipAlign, skipHidden, skipBase)
            END
         END
      END
   END ;
   RETURN type
END doGetType ;


(*
   GetLType - get lowest type.  It returns the lowest type
              of symbol, sym.  It skips over type equivalences.
              It will not skip over base types.
*)

PROCEDURE GetLType (sym: CARDINAL) : CARDINAL ;
BEGIN
(*
   Assert (doGetType (sym, TRUE, TRUE, TRUE, FALSE) = GetLowestType (sym)) ;
*)
   RETURN doGetType (sym, TRUE, TRUE, TRUE, FALSE)
END GetLType ;


(*
   GetSType - get source type.  It returns the type closest
              to the object.  It does not skip over type
              equivalences.  It will skip over base types.
*)

PROCEDURE GetSType (sym: CARDINAL) : CARDINAL ;
BEGIN
   Assert (doGetType (sym, FALSE, FALSE, FALSE, TRUE) = GetType (sym)) ;
   RETURN doGetType (sym, FALSE, FALSE, FALSE, TRUE)
END GetSType ;


(*
   GetDType - get gcc declared type.  It returns the type
              of the object which is declared to GCC.
              It does skip over type equivalences but only
              if they do not contain a user alignment.
              It does not skip over hidden types.
              It does not skip over base types.
*)

PROCEDURE GetDType (sym: CARDINAL) : CARDINAL ;
BEGIN
(*
   Assert (doGetType (sym, TRUE, FALSE, FALSE, FALSE) = SkipType(GetType(sym))) ;
*)
   RETURN doGetType (sym, TRUE, FALSE, FALSE, FALSE)
END GetDType ;


(*
   GetTypeOfVar - returns the type of symbol, var.
*)

PROCEDURE GetTypeOfVar (var: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   high: CARDINAL ;
BEGIN
   pSym := GetPsym(var) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:  IF Var.IsTemp AND Var.IsComponentRef
               THEN
                  high := Indexing.HighIndice(Var.list) ;
                  RETURN( GetType(GetFromIndex(Var.list, high)) )
               ELSE
                  RETURN( Var.Type )
               END

      ELSE
         InternalError ('expecting a var symbol')
      END
   END
END GetTypeOfVar ;


(*
   GetType - Returns the symbol that is the TYPE symbol to Sym.
             If zero is returned then we assume type unknown.
*)

PROCEDURE GetType (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   type: CARDINAL ;
BEGIN
   Assert(Sym#NulSym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      OAFamilySym         : type := OAFamily.SimpleType |
      VarSym              : type := GetTypeOfVar(Sym) |
      ConstLitSym         : type := ConstLit.Type |
      ConstVarSym         : type := ConstVar.Type |
      ConstStringSym      : IF ConstString.Length=1
                            THEN
                               type := Char
                            ELSE
                               type := NulSym  (* No type for a string *)
                            END |
      TypeSym             : type := Type.Type |
      RecordFieldSym      : type := RecordField.Type |
      RecordSym           : type := NulSym |  (* No type for a record *)
      VarientSym          : type := NulSym |  (* No type for a record *)
      EnumerationFieldSym : type := EnumerationField.Type |
      EnumerationSym      : type := NulSym |  (* No type for enumeration *)
      PointerSym          : type := Pointer.Type |
      ProcedureSym        : type := Procedure.ReturnType |
      ProcTypeSym         : type := ProcType.ReturnType |
      ParamSym            : type := Param.Type |
      VarParamSym         : type := VarParam.Type |
      SubrangeSym         : type := Subrange.Type |
      ArraySym            : type := Array.Type |
      SubscriptSym        : type := Subscript.Type |
      SetSym              : type := Set.Type |
      UnboundedSym        : type := Unbounded.Type |
      UndefinedSym        : type := NulSym |
      PartialUnboundedSym : type := PartialUnbounded.Type |
      ObjectSym           : type := NulSym

      ELSE
         InternalError ('not implemented yet')
      END
   END ;
   RETURN( type )
END GetType ;


(*
   SkipType - if sym is a TYPE foo = bar
              then call SkipType(bar)
              else return sym

              it does not skip over hidden types.
*)

PROCEDURE SkipType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF (Sym#NulSym) AND IsType(Sym) AND
      (NOT IsHiddenType(Sym)) AND (GetType(Sym)#NulSym)
   THEN
      RETURN( SkipType(GetType(Sym)) )
   ELSE
      RETURN( Sym )
   END
END SkipType ;


(*
   SkipTypeAndSubrange - if sym is a TYPE foo = bar OR
                            sym is declared as a subrange of bar
                         then call SkipTypeAndSubrange(bar)
                         else return sym

                         it does not skip over hidden types.
*)

PROCEDURE SkipTypeAndSubrange (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF (Sym#NulSym) AND (IsType(Sym) OR IsSubrange(Sym)) AND
      (NOT IsHiddenType(Sym)) AND (GetType(Sym)#NulSym)
   THEN
      RETURN( SkipTypeAndSubrange(GetType(Sym)) )
   ELSE
      RETURN( Sym )
   END
END SkipTypeAndSubrange ;


(*
   IsHiddenType - returns TRUE if, Sym, is a Type and is also declared as a hidden type.
*)

PROCEDURE IsHiddenType (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      TypeSym:  RETURN( Type.IsHidden )

      ELSE
         RETURN( FALSE )
      END
   END
END IsHiddenType ;


(*
   GetConstLitType - returns the type of the constant of, name.
                     All floating point constants have type LONGREAL.
                     Character constants are type CHAR.
                     Integer values are INTEGER, LONGINT or LONGCARD
                     depending upon their value.
*)

PROCEDURE GetConstLitType (name: Name) : CARDINAL ;
VAR
   s            : String ;
   needsLong,
   needsUnsigned: BOOLEAN ;
BEGIN
   s := InitStringCharStar (KeyToCharStar (name)) ;
   IF char (s, -1) = 'C'
   THEN
      s := KillString (s) ;
      RETURN Char
   ELSE
      IF Index (s, '.', 0) # -1   (* found a '.' in our constant *)
      THEN
         s := KillString (s) ;
         RETURN RType
      END ;
      CASE char (s, -1) OF

      'H':  DetermineSizeOfConstant (string (s), 16,
                                     needsLong, needsUnsigned) |
      'B':  DetermineSizeOfConstant (string (s), 8,
                                     needsLong, needsUnsigned) |
      'A':  DetermineSizeOfConstant (string (s), 2,
                                     needsLong, needsUnsigned)

      ELSE
         DetermineSizeOfConstant (string (s), 10,
                                  needsLong, needsUnsigned)
      END ;
      s := KillString (s) ;
      IF needsLong AND needsUnsigned
      THEN
         RETURN LongCard
      ELSIF needsLong AND (NOT needsUnsigned)
      THEN
         RETURN LongInt
      END ;
      RETURN ZType
   END
END GetConstLitType ;


(*
   GetLocalSym - only searches the scope Sym for a symbol with name
                 and returns the index to the symbol.
*)

PROCEDURE GetLocalSym (Sym: CARDINAL; name: Name) : CARDINAL ;
VAR
   pSym    : PtrToSymbol ;
   LocalSym: CARDINAL ;
BEGIN
   (*
   WriteString('Attempting to retrieve symbol from ') ; WriteKey(GetSymName(Sym)) ;
   WriteString(' local symbol table') ; WriteLn ;
   *)
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      EnumerationSym : LocalSym := GetSymKey(Enumeration.LocalSymbols, name) |
      RecordSym      : LocalSym := GetSymKey(Record.LocalSymbols, name) |
      ProcedureSym   : LocalSym := GetSymKey(Procedure.LocalSymbols, name) |
      ModuleSym      : LocalSym := GetSymKey(Module.LocalSymbols, name) |
      DefImpSym      : LocalSym := GetSymKey(DefImp.LocalSymbols, name)

      ELSE
         InternalError ('symbol does not have a LocalSymbols field')
      END
   END ;
   RETURN( LocalSym )
END GetLocalSym ;


(*
   GetNthFromComponent -
*)

PROCEDURE GetNthFromComponent (Sym: CARDINAL; n: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:  IF IsComponent(Sym)
               THEN
                  IF InBounds(Var.list, n)
                  THEN
                     RETURN( GetFromIndex(Var.list, n) )
                  ELSE
                     RETURN( NulSym )
                  END
               ELSE
                  InternalError ('cannot GetNth from this symbol')
               END

      ELSE
         InternalError ('cannot GetNth from this symbol')
      END
   END
END GetNthFromComponent ;


(*
   GetNth - returns the n th symbol in the list of father Sym.
            Sym may be a Module, DefImp, Procedure or Record symbol.
*)

PROCEDURE GetNth (Sym: CARDINAL; n: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   i   : CARDINAL ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym       : i := GetItemFromList(Record.ListOfSons, n) |
      VarientSym      : i := GetItemFromList(Varient.ListOfSons, n) |
      VarientFieldSym : i := GetItemFromList(VarientField.ListOfSons, n) |
      ProcedureSym    : i := GetItemFromList(Procedure.ListOfVars, n) |
      DefImpSym       : i := GetItemFromList(DefImp.ListOfVars, n) |
      ModuleSym       : i := GetItemFromList(Module.ListOfVars, n) |
      TupleSym        : i := GetFromIndex(Tuple.list, n) |
      VarSym          : i := GetNthFromComponent(Sym, n)

      ELSE
         InternalError ('cannot GetNth from this symbol')
      END
   END ;
   RETURN( i )
END GetNth ;


(*
   GetNthParam - returns the n th parameter of a procedure Sym.
*)

PROCEDURE GetNthParam (Sym: CARDINAL; ParamNo: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   i   : CARDINAL ;
BEGIN
   IF ParamNo=0
   THEN
      (* Demands the return type of the function *)
      i := GetType(Sym)
   ELSE
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ProcedureSym: i := GetItemFromList(Procedure.ListOfParam, ParamNo) |
         ProcTypeSym : i := GetItemFromList(ProcType.ListOfParam, ParamNo)

         ELSE
            InternalError ('expecting ProcedureSym or ProcTypeSym')
         END
      END
   END ;
   RETURN( i )
END GetNthParam ;


(*
   The Following procedures fill in the symbol table with the
   symbol entities.
*)

(*
   PutVar - gives the VarSym symbol Sym a type Type.
*)

PROCEDURE PutVar (Sym: CARDINAL; VarType: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym     : Var.Type := VarType |
      ConstVarSym: ConstVar.Type := VarType

      ELSE
         InternalError ('expecting VarSym or ConstVarSym')
      END
   END
END PutVar ;


(*
   PutLeftValueFrontBackType - gives the variable symbol a front and backend type.
                               The variable must be a LeftValue.
*)

PROCEDURE PutLeftValueFrontBackType (Sym: CARDINAL; FrontType, BackType: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(GetMode(Sym)=LeftValue) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym : Var.Type := FrontType ;
               Var.BackType := BackType ;
               PushSize(Address) ;
               PopInto(Var.Size)

      ELSE
         InternalError ('expecting VarSym')
      END
   END
END PutLeftValueFrontBackType ;


(*
   GetVarBackEndType - returns the back end type if specified.
*)

PROCEDURE GetVarBackEndType (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(Sym#NulSym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym: RETURN( Var.BackType )

      ELSE
         RETURN( NulSym )
      END
   END
END GetVarBackEndType ;


(*
   PutVarPointerCheck - marks variable, sym, as requiring (or not
                        depending upon the, value), a NIL pointer check
                        when this symbol is dereferenced.
*)

PROCEDURE PutVarPointerCheck (sym: CARDINAL; value: BOOLEAN) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF IsVar(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^.Var DO
         IsPointerCheck := value
      END
   END
END PutVarPointerCheck ;


(*
   GetVarPointerCheck - returns TRUE if this symbol is a variable and
                        has been marked as needing a pointer via NIL check.
*)

PROCEDURE GetVarPointerCheck (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF IsVar(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^.Var DO
         RETURN( IsPointerCheck )
      END
   END
END GetVarPointerCheck ;


(*
   PutVarWritten - marks variable, sym, as being written to (or not
                   depending upon the, value).
*)

PROCEDURE PutVarWritten (sym: CARDINAL; value: BOOLEAN) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF IsVar(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^.Var DO
         IsWritten := value
      END
   END
END PutVarWritten ;


(*
   GetVarWritten - returns TRUE if this symbol is a variable and
                   has been marked as being written.
*)

PROCEDURE GetVarWritten (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym: RETURN( Var.IsWritten )

      ELSE
         InternalError ('expecting VarSym')
      END
   END
END GetVarWritten ;


(*
   PutConst - gives the constant symbol Sym a type ConstType.
*)

PROCEDURE PutConst (Sym: CARDINAL; ConstType: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstVarSym: ConstVar.Type := ConstType

      ELSE
         InternalError ('expecting ConstVarSym')
      END
   END
END PutConst ;


(*
   PutFieldRecord - places a field, FieldName and FieldType into a record, Sym.
                    VarSym is a optional varient symbol which can be returned
                    by a call to GetVarient(fieldsymbol).  The created field
                    is returned.
*)

PROCEDURE PutFieldRecord (Sym: CARDINAL;
                          FieldName: Name; FieldType: CARDINAL;
                          VarSym: CARDINAL) : CARDINAL ;
VAR
   oSym,
   pSym  : PtrToSymbol ;
   esym,
   ParSym,
   SonSym: CARDINAL ;
BEGIN
   NewSym(SonSym) ; (* Cannot be used before declared since use occurs *)
                    (* in pass 3 and it will be declared in pass 2.    *)
   (* Fill in the SonSym and connect it to its brothers (if any) and   *)
   (* ensure that it is connected its parent.                          *)
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym       : WITH Record DO
                           PutItemIntoList(ListOfSons, SonSym) ;
                           Assert(IsItemInList(Record.ListOfSons, SonSym)) ;
(*
                           n := NoOfItemsInList(ListOfSons) ;
                           printf3('record %d no of fields in ListOfSons = %d, field %d\n', Sym, n, SonSym) ;
*)
                           (* Ensure that the Field is in the Parents Local Symbols *)
                           IF FieldName#NulName
                           THEN
                              IF GetSymKey(LocalSymbols, FieldName)=NulKey
                              THEN
                                 PutSymKey(LocalSymbols, FieldName, SonSym)
                              ELSE
                                 esym := GetSymKey(LocalSymbols, FieldName) ;
                                 MetaErrors1('field record {%1Dad} has already been declared',
                                             'field record duplicate', esym)
                              END
                           END
                        END ;
                        CheckRecordConsistency(Sym) |
      VarientFieldSym : WITH VarientField DO
                           PutItemIntoList(ListOfSons, SonSym) ;
                           ParSym := GetRecord(Parent)
                        END ;
                        oSym := GetPsym(ParSym) ;
                        Assert(oSym^.SymbolType=RecordSym) ;
                        IF FieldName#NulName
                        THEN
                           oSym := GetPsym(ParSym) ;
                           PutSymKey(oSym^.Record.LocalSymbols, FieldName, SonSym)
                        END

      ELSE
         InternalError ('expecting Record symbol')
      END
   END ;
   (* Fill in SonSym *)
   oSym := GetPsym(SonSym) ;
   WITH oSym^ DO
      SymbolType := RecordFieldSym ;
      WITH RecordField DO
         Type := FieldType ;
         name := FieldName ;
         Tag := FALSE ;
         Parent := Sym ;
         Varient := VarSym ;
         Align := NulSym ;
         Used := TRUE ;
         DeclPacked := FALSE ;     (* not known as packed (yet). *)
         DeclResolved := FALSE ;
         Scope := GetScope(Sym) ;
         Size := InitValue() ;
         Offset := InitValue() ;
         InitWhereDeclared(At)
      END
   END ;
   RETURN( SonSym )
END PutFieldRecord ;


(*
   MakeFieldVarient - returns a FieldVarient symbol which has been
                      assigned to the Varient symbol, Sym.
*)

PROCEDURE MakeFieldVarient (n: Name; Sym: CARDINAL) : CARDINAL ;
VAR
   pSym  : PtrToSymbol ;
   SonSym: CARDINAL ;
BEGIN
   NewSym(SonSym) ;
(*
   IF NoOfItemsInList(FreeFVarientList)=0
   THEN
      NewSym(SonSym)
   ELSE
      SonSym := GetItemFromList(FreeFVarientList, 1) ;
      RemoveItemFromList(FreeFVarientList, SonSym)
   END ;
*)
   (* Fill in Sym *)
   pSym := GetPsym(SonSym) ;
   WITH pSym^ DO
      SymbolType := VarientFieldSym ;
      WITH VarientField DO
         name := n ;
         InitList(ListOfSons) ;
         Parent := GetRecord(Sym) ;
         Varient := NulSym ;
         Size := InitValue() ;
         Offset := InitValue() ;
         DeclPacked := FALSE ;
         DeclResolved := FALSE ;
         Scope := GetCurrentScope() ;
         InitWhereDeclared(At)
      END
   END ;
   RETURN( SonSym )
END MakeFieldVarient ;


(*
   PutFieldVarient - places the field varient, Field, as a brother to, the
                     varient symbol, sym, and also tells Field that its varient
                     parent is Sym.
*)

PROCEDURE PutFieldVarient (Field, Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(IsVarient(Sym)) ;
   Assert(IsFieldVarient(Field)) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarientSym : IncludeItemIntoList(Varient.ListOfSons, Field)

      ELSE
         InternalError ('expecting Varient symbol')
      END
   END ;
   pSym := GetPsym(Field) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarientFieldSym : VarientField.Varient := Sym

      ELSE
         InternalError ('expecting VarientField symbol')
      END
   END ;
   (* PutItemIntoList(UsedFVarientList, Field) *)
END PutFieldVarient ;


(*
   GetVarient - returns the varient symbol associated with the
                record or varient field symbol, Field.
*)

PROCEDURE GetVarient (Field: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Field) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarientFieldSym : RETURN( VarientField.Varient ) |
      RecordFieldSym  : RETURN( RecordField.Varient ) |
      VarientSym      : RETURN( Varient.Varient )

      ELSE
         RETURN( NulSym )
      END
   END
END GetVarient ;


(*
   GCFieldVarient - garbage collect the field varient symbol, Sym.
                    This must only be called once per Sym.
*)

PROCEDURE GCFieldVarient (Sym: CARDINAL) ;
BEGIN
(*
   IF IsItemInList(UsedFVarientList, Sym)
   THEN
      RemoveItemFromList(UsedFVarientList, Sym)
   ELSE
      RemoveItemFromList(UsedFVarientList, Sym) ;
      PutItemIntoList(FreeFVarientList, Sym)
   END
*)
END GCFieldVarient ;


(*
   EnsureOrder - providing that both symbols, a, and, b, exist in
                 list, l.  Ensure that, b, is placed after a.
*)

PROCEDURE EnsureOrder (l: List; a, b: CARDINAL) ;
VAR
   n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(l) ;
   IF IsItemInList(l, a) AND IsItemInList(l, b)
   THEN
      RemoveItemFromList(l, b) ;
      IncludeItemIntoList(l, b)
   END ;
   Assert(n=NoOfItemsInList(l))
END EnsureOrder ;


VAR
   recordConsist: CARDINAL ;   (* is used by CheckRecordConsistency and friends.  *)


(*
   DumpSons -
*)

PROCEDURE DumpSons (sym: CARDINAL) ;
VAR
   pSym   : PtrToSymbol ;
   f, n, i: CARDINAL ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym:  n := NoOfItemsInList(Record.ListOfSons) ;
                  i := 1 ;
                  WHILE i<=n DO
                     f := GetItemFromList(Record.ListOfSons, i) ;
                     printf3('record %d field %d is %d\n', sym, i, f) ;
                     INC(i)
                  END

      ELSE
         InternalError ('expecting record symbol')
      END
   END
END DumpSons ;



(*
   CheckListOfSons - checks to see that sym, is present in, recordConsist, ListOfSons.
*)

PROCEDURE CheckListOfSons (sym: WORD) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(recordConsist) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym:  IF NOT IsItemInList(Record.ListOfSons, sym)
                  THEN
                     DumpSons(recordConsist) ;
                     MetaError1('internal error:  expecting {%1ad} to exist in record ListOfSons', sym)
                  END

      ELSE
         InternalError ('expecting record symbol')
      END
   END
END CheckListOfSons ;


(*
   CheckRecordConsistency -
*)

PROCEDURE CheckRecordConsistency (sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   RETURN ;
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym:  recordConsist := sym ;
                  WITH Record DO
                     ForeachNodeDo(LocalSymbols, CheckListOfSons)
                  END |

      ELSE
         InternalError ('record symbol expected')
      END
   END
END CheckRecordConsistency ;


(*
   IsEmptyFieldVarient - returns TRUE if the field variant has
                         no fields.  This will occur then the
                         compiler constructs 'else end' variants.
*)

PROCEDURE IsEmptyFieldVarient (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarientFieldSym:  RETURN( NoOfItemsInList(VarientField.ListOfSons)=0 )

      ELSE
         InternalError ('varient field symbol expected')
      END
   END
END IsEmptyFieldVarient ;


(*
   IsRecordFieldAVarientTag - returns TRUE if record field, sym, is
                              a varient tag.
*)

PROCEDURE IsRecordFieldAVarientTag (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF IsRecordField(sym)
   THEN
      pSym := GetPsym(sym) ;
      RETURN( pSym^.RecordField.Tag )
   ELSE
      InternalError ('record field symbol expected')
   END
END IsRecordFieldAVarientTag ;


(*
   PutVarientTag - places, Tag, into varient, Sym.
*)

PROCEDURE PutVarientTag (Sym, Tag: CARDINAL) ;
VAR
   pSym  : PtrToSymbol ;
   parent: CARDINAL ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarientSym:  Varient.tag := Tag

      ELSE
         InternalError ('varient symbol expected')
      END
   END ;
   (* now ensure that if Tag is a RecordField then it must be
      placed before the varient symbol in its parent ListOfSons.
      This allows M2GCCDeclare to declare record fields in order
      and preserve the order of fields.  Otherwise it will add the
      tag field after the C union. *)
   IF IsRecordField(Tag)
   THEN
      pSym := GetPsym(Tag) ;
      pSym^.RecordField.Tag := TRUE ;
      parent := GetParent(Sym) ;
      pSym := GetPsym(parent) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ErrorSym: |
         VarientSym     : EnsureOrder(Varient.ListOfSons, Tag, Sym) |
         VarientFieldSym: EnsureOrder(VarientField.ListOfSons, Tag, Sym) |
         RecordSym      : EnsureOrder(Record.ListOfSons, Tag, Sym) ;
                          CheckRecordConsistency(parent)

         ELSE
            InternalError ('not expecting this symbol type')
         END
      END
   END
END PutVarientTag ;


(*
   GetVarientTag - returns the varient tag from, Sym.
*)

PROCEDURE GetVarientTag (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarientSym:  RETURN( Varient.tag )

      ELSE
         InternalError ('varient symbol expected')
      END
   END
END GetVarientTag ;


(*
   IsFieldVarient - returns true if the symbol, Sym, is a
                    varient field.
*)

PROCEDURE IsFieldVarient (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=VarientFieldSym )
END IsFieldVarient ;


(*
   IsFieldEnumeration - returns true if the symbol, Sym, is an
                        enumeration field.
*)

PROCEDURE IsFieldEnumeration (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=EnumerationFieldSym )
END IsFieldEnumeration ;


(*
   IsVarient - returns true if the symbol, Sym, is a
               varient symbol.
*)

PROCEDURE IsVarient (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=VarientSym )
END IsVarient ;


(*
   PutUnused - sets, sym, as unused.  This is a gm2 pragma.
*)

PROCEDURE PutUnused (sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordFieldSym:  RecordField.Used := FALSE

      ELSE
         MetaError1("cannot use pragma 'unused' on symbol {%1ad}", sym)
      END
   END
END PutUnused ;


(*
   IsUnused - returns TRUE if the symbol was declared as unused with a
              gm2 pragma.
*)

PROCEDURE IsUnused (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordFieldSym:  RETURN( NOT RecordField.Used )

      ELSE
         InternalError ('expecting a record field symbol')
      END
   END
END IsUnused ;


(*
   PutFieldEnumeration - places a field into the enumeration type
                         Sym. The field has a name FieldName and a
                         value FieldVal.
*)

PROCEDURE PutFieldEnumeration (tok: CARDINAL; Sym: CARDINAL; FieldName: Name) ;
VAR
   oSym,
   pSym : PtrToSymbol ;
   s    : String ;
   Field: CARDINAL ;
BEGIN
   Field := CheckForHiddenType(FieldName) ;
   IF Field=NulSym
   THEN
      Field := DeclareSym (tok, FieldName)
   END ;
   IF NOT IsError(Field)
   THEN
      pSym := GetPsym(Field) ;
      WITH pSym^ DO
         SymbolType := EnumerationFieldSym ;
         WITH EnumerationField DO
            name := FieldName ;  (* Index into name array, name *)
                                 (* of type.                    *)
            oSym := GetPsym(Sym) ;
            PushCard(oSym^.Enumeration.NoOfElements) ;
            Value := InitValue() ;
            PopInto(Value) ;
            Type := Sym ;
            Scope := GetCurrentScope() ;
            InitWhereDeclaredTok (tok, At)  (* Declared here *)
         END
      END ;
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         EnumerationSym: WITH Enumeration DO
                            INC(NoOfElements) ;
                            IF GetSymKey(LocalSymbols, FieldName)#NulSym
                            THEN
                               s := Mark(InitStringCharStar(KeyToCharStar(FieldName))) ;
                               AlreadyDeclaredError(Sprintf1(Mark(InitString('enumeration field (%s) is already declared elsewhere, use a different name or remove the declaration')), s),
                                                    FieldName,
                                                    GetDeclaredMod(GetSymKey(LocalSymbols, FieldName)))
                            ELSE
                               PutSymKey(LocalSymbols, FieldName, Field)
                            END
                         END

         ELSE
            InternalError ('expecting Sym=Enumeration')
         END
      END
   END
END PutFieldEnumeration ;


(*
   PutType - gives a type symbol Sym type TypeSymbol.
*)

PROCEDURE PutType (Sym: CARDINAL; TypeSymbol: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF TypeSymbol=Sym
   THEN
      InternalError ('not expecting a type to be declared as itself')
   END ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      TypeSym : Type.Type := TypeSymbol

      ELSE
         InternalError ('expecting a Type symbol')
      END
   END
END PutType ;


(*
   IsDefImp - returns true is the Sym is a DefImp symbol.
              Definition/Implementation module symbol.
*)

PROCEDURE IsDefImp (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=DefImpSym )
END IsDefImp ;


(*
   IsModule - returns true is the Sym is a Module symbol.
              Program module symbol.
*)

PROCEDURE IsModule (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=ModuleSym )
END IsModule ;


(*
   IsInnerModule - returns true if the symbol, Sym, is an inner module.
*)

PROCEDURE IsInnerModule (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsModule(Sym)
   THEN
      RETURN( GetScope(Sym)#NulSym )
   ELSE
      RETURN( FALSE )
   END
END IsInnerModule ;


(*
   GetSymName - returns the symbol name.
*)

PROCEDURE GetSymName (Sym: CARDINAL) : Name ;
VAR
   pSym: PtrToSymbol ;
   n   : Name ;
BEGIN
   IF Sym=NulSym
   THEN
      n := NulKey
   ELSE
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ErrorSym            : n := Error.name |
         ObjectSym           : n := Object.name |
         DefImpSym           : n := DefImp.name |
         ModuleSym           : n := Module.name |
         TypeSym             : n := Type.name |
         VarSym              : n := Var.name |
         ConstLitSym         : n := ConstLit.name |
         ConstVarSym         : n := ConstVar.name |
         ConstStringSym      : n := ConstString.name |
         EnumerationSym      : n := Enumeration.name |
         EnumerationFieldSym : n := EnumerationField.name |
         UndefinedSym        : n := Undefined.name |
         ProcedureSym        : n := Procedure.name |
         ProcTypeSym         : n := ProcType.name |
         RecordFieldSym      : n := RecordField.name |
         RecordSym           : n := Record.name |
         VarientSym          : n := NulName |
         VarientFieldSym     : n := VarientField.name |
         VarParamSym         : n := VarParam.name |
         ParamSym            : n := Param.name |
         PointerSym          : n := Pointer.name |
         ArraySym            : n := Array.name |
         UnboundedSym        : n := NulName |
         SubrangeSym         : n := Subrange.name |
      	 SetSym              : n := Set.name |
         SubscriptSym        : n := NulName |
         DummySym            : n := NulName |
         PartialUnboundedSym : n := GetSymName(PartialUnbounded.Type) |
         TupleSym            : n := NulName |
         GnuAsmSym           : n := NulName |
         InterfaceSym        : n := NulName

         ELSE
            InternalError ('unexpected symbol type')
         END
      END
   END ;
   RETURN( n )
END GetSymName ;


(*
   PutConstVarTemporary - indicates that constant, sym, is a temporary.
*)

PROCEDURE PutConstVarTemporary (sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstVarSym:  ConstVar.IsTemp := TRUE

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END
END PutConstVarTemporary ;


(*
   buildTemporary - builds the temporary filling in componentRef, record and sets mode.
*)

PROCEDURE buildTemporary (tok: CARDINAL;
                          Mode: ModeOfAddr; componentRef: BOOLEAN; record: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   s   : String ;
   Sym : CARDINAL ;
BEGIN
   INC(TemporaryNo) ;
   (* Make the name *)
   s := Sprintf1(Mark(InitString('_T%d')), TemporaryNo) ;
   IF Mode=ImmediateValue
   THEN
      Sym := MakeConstVar(tok, makekey(string(s))) ;
      PutConstVarTemporary(Sym)
   ELSE
      Sym := MakeVar(tok, makekey(string(s))) ;
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         VarSym : Var.AddrMode := Mode ;
                  Var.IsComponentRef := componentRef ;
                  Var.IsTemp := TRUE ;       (* Variable is a temporary var *)
                  IF componentRef
                  THEN
                     Var.list := Indexing.InitIndex(1) ;
                     PutIntoIndex(Var.list, 1, record)
                  END ;
                  InitWhereDeclaredTok(tok, Var.At) ;    (* Declared here             *)
                  InitWhereFirstUsedTok(tok, Var.At) ;   (* Where symbol first used.  *)

         ELSE
            InternalError ('expecting a Var symbol')
         END
      END
   END ;
   s := KillString(s) ;
   RETURN Sym
END buildTemporary ;


(*
   MakeComponentRef - use, sym, to reference, field, sym is returned.
*)

PROCEDURE MakeComponentRef (sym: CARDINAL; field: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   high: CARDINAL ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:  IF NOT Var.IsTemp
               THEN
                  InternalError ('variable must be a temporary')
               ELSIF Var.IsComponentRef
               THEN
                  high := Indexing.HighIndice (Var.list) ;
                  PutIntoIndex (Var.list, high+1, field)
               ELSE
                  InternalError ('temporary is not a component reference')
               END

      ELSE
         InternalError ('expecting a variable symbol')
      END
   END ;
   RETURN( sym )
END MakeComponentRef ;


(*
   MakeComponentRecord - make a temporary which will be used to reference and field
                         (or sub field) of record.
*)

PROCEDURE MakeComponentRecord (tok: CARDINAL; Mode: ModeOfAddr; record: CARDINAL) : CARDINAL ;
BEGIN
   RETURN buildTemporary (tok, Mode, TRUE, record)
END MakeComponentRecord ;


(*
   IsComponent - returns TRUE if symbol, sym, is a temporary and a component
                 reference.
*)

PROCEDURE IsComponent (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym:   RETURN( Var.IsComponentRef )

      ELSE
         RETURN( FALSE )
      END
   END
END IsComponent ;


(*
   MakeTemporary - Makes a new temporary variable at the highest real scope.
                   The addressing mode of the temporary is set to NoValue.
*)

PROCEDURE MakeTemporary (tok: CARDINAL; Mode: ModeOfAddr) : CARDINAL ;
BEGIN
   RETURN buildTemporary (tok, Mode, FALSE, NulSym)
END MakeTemporary ;


(*
   MakeTemporaryFromExpressions - makes a new temporary variable at the
                                  highest real scope.  The addressing
                                  mode of the temporary is set and the
                                  type is determined by expressions,
                                  e1 and e2.
*)

PROCEDURE MakeTemporaryFromExpressions (tok: CARDINAL;
                                        e1, e2: CARDINAL;
                                        mode: ModeOfAddr) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   s   : String ;
   t,
   Sym : CARDINAL ;
BEGIN
   INC(TemporaryNo) ;
   (* Make the name *)
   s := Sprintf1(Mark(InitString('_T%d')), TemporaryNo) ;
   IF mode=ImmediateValue
   THEN
      Sym := MakeConstVar(tok, makekey(string(s))) ;
      IF IsConstructor(e1)
      THEN
         PutConstructor(Sym) ;
         PutConstructorFrom(Sym, e1)
      ELSIF IsConstructor(e2)
      THEN
         PutConstructor(Sym) ;
         PutConstructorFrom(Sym, e2)
      ELSE
         PutVar(Sym, MixTypes(GetType(e1), GetType(e2), tok))
      END ;
      PutConstVarTemporary(Sym)
   ELSE
      Sym := MakeVar(tok, makekey(string(s))) ;
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         VarSym : Var.AddrMode := mode ;
                  Var.IsComponentRef := FALSE ;
                  Var.IsTemp := TRUE ;       (* Variable is a temporary var *)
                  InitWhereDeclaredTok(tok, Var.At)
                                             (* Declared here               *)

         ELSE
            InternalError ('expecting a Var symbol')
         END
      END ;
      t := MixTypes(GetType(e1), GetType(e2), tok) ;
      IF t#NulSym
      THEN
         Assert(NOT IsConstructor(t)) ;
         PutVar(Sym, t)
      END
   END ;
   s := KillString(s) ;
   RETURN( Sym )
END MakeTemporaryFromExpressions ;


(*
   MakeTemporaryFromExpression - makes a new temporary variable at the
                                 highest real scope.  The addressing
                                 mode of the temporary is set and the
                                 type is determined by expressions, e.
*)

PROCEDURE MakeTemporaryFromExpression (tok: CARDINAL;
                                       e: CARDINAL;
                                       mode: ModeOfAddr) : CARDINAL ;
BEGIN
   RETURN MakeTemporaryFromExpressions (tok, e, e, mode)
END MakeTemporaryFromExpression ;


(*
   PutMode - Puts the addressing mode, SymMode, into symbol Sym.
             The mode may only be altered if the mode
             is None.
*)

PROCEDURE PutMode (Sym: CARDINAL; SymMode: ModeOfAddr) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      VarSym  : Var.AddrMode := SymMode

      ELSE
         InternalError ('Expecting VarSym')
      END
   END
END PutMode ;


(*
   GetMode - Returns the addressing mode of a symbol.
*)

PROCEDURE GetMode (Sym: CARDINAL) : ModeOfAddr ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym           : ErrorAbort0('') |
      VarSym             : RETURN( Var.AddrMode ) |
      ConstLitSym        : RETURN( ImmediateValue ) |
      ConstVarSym        : RETURN( ImmediateValue ) |
      ConstStringSym     : RETURN( ImmediateValue ) |
      EnumerationFieldSym: RETURN( ImmediateValue ) |
      ProcedureSym       : RETURN( ImmediateValue ) |
      RecordFieldSym     : RETURN( ImmediateValue ) |
      VarientFieldSym    : RETURN( ImmediateValue ) |
      TypeSym            : RETURN( NoValue ) |
      ArraySym           : RETURN( NoValue ) |
      SubrangeSym        : RETURN( NoValue ) |
      EnumerationSym     : RETURN( NoValue ) |
      RecordSym          : RETURN( NoValue ) |
      PointerSym         : RETURN( NoValue ) |
      SetSym             : RETURN( NoValue ) |
      ProcTypeSym        : RETURN( NoValue ) |
      UnboundedSym       : RETURN( NoValue ) |
      UndefinedSym       : RETURN( NoValue )

      ELSE
         InternalError ('not expecting this type')
      END
   END
END GetMode ;


(*
   RenameSym - renames a symbol, Sym, with SymName.
               It also checks the unknown tree for a symbol
               with this new name. Must only be renamed in
               the same scope of being declared.
*)

PROCEDURE RenameSym (Sym: CARDINAL; SymName: Name) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF GetSymName(Sym)=NulName
   THEN
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ErrorSym            : ErrorAbort0('') |
         TypeSym             : Type.name      := SymName |
         VarSym              : Var.name       := SymName |
         ConstLitSym         : ConstLit.name  := SymName |
         ConstVarSym         : ConstVar.name  := SymName |
         UndefinedSym        : Undefined.name := SymName |
         RecordSym           : Record.name    := SymName |
         PointerSym          : Pointer.name   := SymName

         ELSE
            InternalError ('not implemented yet')
         END
      END ;
      AddSymToScope(Sym, SymName)
   ELSE
      InternalError ('old name of symbol must be nul')
   END
END RenameSym ;


(*
   IsUnknown - returns true is the symbol Sym is unknown.
*)

PROCEDURE IsUnknown (Sym: WORD) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal (Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN pSym^.SymbolType=UndefinedSym
END IsUnknown ;


(*
   CheckLegal - determines whether the Sym is a legal symbol.
*)

PROCEDURE CheckLegal (Sym: CARDINAL) ;
BEGIN
   IF (Sym<1) OR (Sym>FinalSymbol())
   THEN
      InternalError ('illegal symbol')
   END
END CheckLegal ;


(*
   CheckForHiddenType - scans the NeedToBeImplemented tree providing
                        that we are currently compiling an implementation
                        module. If a symbol is found with TypeName
                        then its Sym is returned.
                        Otherwise NulSym is returned.
                        CheckForHiddenType is called before any type is
                        created, therefore the compiler allows hidden
                        types to be implemented using any type.
*)

PROCEDURE CheckForHiddenType (TypeName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   Sym := NulSym ;
   IF CompilingImplementationModule() AND
      IsDefImp(CurrentModule) AND
      IsHiddenTypeDeclared(CurrentModule) AND
      (TypeName#NulName)
   THEN
      (* Check to see whether we are declaring a HiddenType. *)
      pSym := GetPsym(CurrentModule) ;
      WITH pSym^ DO
         CASE SymbolType OF

         DefImpSym: Sym := GetSymKey(DefImp.NeedToBeImplemented, TypeName)

         ELSE
            InternalError ('expecting a DefImp symbol')
         END
      END
   END ;
   RETURN( Sym )
END CheckForHiddenType ;


(*
   IsReallyPointer - returns TRUE is sym is a pointer, address or a
                     type declared as a pointer or address.
*)

PROCEDURE IsReallyPointer (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsVar(Sym)
   THEN
      Sym := GetType(Sym)
   END ;
   Sym := SkipType(Sym) ;
   RETURN( IsPointer(Sym) OR (Sym=Address) )
END IsReallyPointer ;


(*
   SkipHiddenType - if sym is a TYPE foo = bar
                    then call SkipType(bar)
                    else return sym

                    it does skip over hidden type.
*)

PROCEDURE SkipHiddenType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF (Sym#NulSym) AND IsType(Sym) AND (GetType(Sym)#NulSym)
   THEN
      RETURN( SkipType(GetType(Sym)) )
   ELSE
      RETURN( Sym )
   END
END SkipHiddenType ;


(*
   IsHiddenReallyPointer - returns TRUE is sym is a pointer, address or a
                           type declared as a pointer or address.
*)

PROCEDURE IsHiddenReallyPointer (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsVar (Sym)
   THEN
      Sym := GetType (Sym)
   END ;
   WHILE (Sym # NulSym) AND IsType (Sym) DO
      Sym := SkipType (GetType (Sym))
   END ;
   RETURN (Sym # NulSym) AND (IsPointer (Sym) OR (Sym = Address))
END IsHiddenReallyPointer ;


(*
   CheckHiddenTypeAreAddress - checks to see that any hidden types
                               which we have declared are actually
                               of type ADDRESS or map onto a POINTER type.
*)

PROCEDURE CheckHiddenTypeAreAddress ;
VAR
   name: Name ;
   e   : Error ;
   sym,
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := NoOfItemsInList(AddressTypes) ;
   WHILE i<=n DO
      sym := GetItemFromList(AddressTypes, i) ;
      IF NOT IsHiddenReallyPointer(sym)
      THEN
         name := GetSymName(sym) ;
         e := NewError(GetDeclaredDef(sym)) ;
         ErrorFormat1(e, 'opaque type (%a) should be equivalent to a POINTER or an ADDRESS', name) ;
         e := NewError(GetDeclaredMod(sym)) ;
         ErrorFormat0(e, 'if you really need a non POINTER type use the -fextended-opaque switch')
      END ;
      INC(i)
   END
END CheckHiddenTypeAreAddress ;


(*
   GetLastMainScopeId - returns the, id, containing the last main scope.
*)

PROCEDURE GetLastMainScopeId (id: CARDINAL) : CARDINAL ;
VAR
   pCall: PtrToCallFrame ;
   sym  : CARDINAL ;
BEGIN
   IF id>0
   THEN
      pCall := GetPcall(id) ;
      sym := pCall^.Main ;
      WHILE id>1 DO
         DEC(id) ;
         pCall := GetPcall(id) ;
         IF sym#pCall^.Main
         THEN
            RETURN( id )
         END
      END
   END ;
   RETURN( 0 )
END GetLastMainScopeId ;


(*
   GetDeclareSym - searches for a symbol with a name SymName in the
                   current and previous scopes.
                   If the symbol is found then it is returned
                   else an unknown symbol is returned.
                   This procedure assumes that SymName is being
                   declared at this point and therefore it does
                   not examine the base scope (for pervasive
                   identifiers).
*)

PROCEDURE GetDeclareSym (tok: CARDINAL; SymName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := GetScopeSym (SymName, FALSE) ;  (* must not be allowed to fetch a symbol through a procedure scope *)
   IF Sym=NulSym
   THEN
      Sym := GetSymFromUnknownTree (SymName) ;
      IF Sym=NulSym
      THEN
         (* Make unknown *)
         NewSym (Sym) ;
         FillInUnknownFields (tok, Sym, SymName) ;
         (* Add to unknown tree *)
         AddSymToUnknownTree (ScopePtr, SymName, Sym)
         (*
           ; WriteKey(SymName) ; WriteString(' unknown demanded') ; WriteLn
         *)
      END
   END ;
   RETURN Sym
END GetDeclareSym ;


(*
   RequestSym - searches for a symbol with a name SymName in the
                current and previous scopes.
                If the symbol is found then it is returned
                else an unknown symbol is returned create at token
                position, tok.
                This procedure does search the base scope (for
                pervasive identifiers).
*)

PROCEDURE RequestSym (tok: CARDINAL; SymName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   (*
      WriteString('RequestSym for: ') ; WriteKey(SymName) ; WriteLn ;
   *)
   Sym := GetSym (SymName) ;
   IF Sym=NulSym
   THEN
      Sym := GetSymFromUnknownTree (SymName) ;
      IF Sym=NulSym
      THEN
         (* Make unknown *)
         NewSym (Sym) ;
         FillInUnknownFields (tok, Sym, SymName) ;
         (* Add to unknown tree *)
         AddSymToUnknownTree (ScopePtr, SymName, Sym)
         (*
           ; WriteKey(SymName) ; WriteString(' unknown demanded') ; WriteLn
         *)
      END
   END ;
   RETURN( Sym )
END RequestSym ;


(*
   PutImported - places a symbol, Sym, into the current main scope.
*)

PROCEDURE PutImported (Sym: CARDINAL) ;
VAR
   pSym  : PtrToSymbol ;
   ModSym: CARDINAL ;
   n     : Name ;
BEGIN
   (*
      We have currently imported Sym, now place it into the current module.
   *)
   ModSym := GetCurrentModuleScope() ;
   Assert(IsDefImp(ModSym) OR IsModule(ModSym)) ;
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: IF GetSymKey(Module.ImportTree, GetSymName(Sym))=Sym
                 THEN
                    IF Pedantic
                    THEN
                       n := GetSymName(Sym) ;
                       WriteFormat1('symbol (%a) has already been imported', n)
                    END
                 ELSIF GetSymKey(Module.ImportTree, GetSymName(Sym))=NulKey
                 THEN
                    IF GetSymKey(Module.WhereImported, Sym)=NulKey
                    THEN
                       PutSymKey(Module.WhereImported, Sym, GetTokenNo())
                    END ;
                    PutSymKey(Module.ImportTree, GetSymName(Sym), Sym) ;
                    AddSymToModuleScope(ModSym, Sym)
                 ELSE
                    n := GetSymName(Sym) ;
                    WriteFormat1('name clash when trying to import (%a)', n)
                 END |
      DefImpSym: IF GetSymKey(DefImp.ImportTree, GetSymName(Sym))=Sym
                 THEN
                    IF Pedantic
                    THEN
                       n := GetSymName(Sym) ;
                       WriteFormat1('symbol (%a) has already been imported', n)
                    END
                 ELSIF GetSymKey(DefImp.ImportTree, GetSymName(Sym))=NulKey
                 THEN
                    IF GetSymKey(DefImp.WhereImported, Sym)=NulKey
                    THEN
                       PutSymKey(DefImp.WhereImported, Sym, GetTokenNo())
                    END ;
                    PutSymKey(DefImp.ImportTree, GetSymName(Sym), Sym) ;
                    AddSymToModuleScope(ModSym, Sym)
                 ELSE
                    n := GetSymName(Sym) ;
                    WriteFormat1('name clash when trying to import (%a)', n)
                 END

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END PutImported ;


(*
   PutIncluded - places a symbol, Sym, into the included list of the
                 current module.
                 Symbols that are placed in this list are indirectly declared
                 by:

                 IMPORT modulename ;

                 modulename.identifier
*)

PROCEDURE PutIncluded (Sym: CARDINAL) ;
VAR
   pSym  : PtrToSymbol ;
   ModSym: CARDINAL ;
   n1, n2: Name ;
BEGIN
   (*
      We have referenced Sym, via modulename.Sym
      now place it into the current module include list.
   *)
   ModSym := GetCurrentModuleScope() ;
   Assert(IsDefImp(ModSym) OR IsModule(ModSym)) ;
   IF DebugUnknowns
   THEN
      n1 := GetSymName(Sym) ;
      n2 := GetSymName(ModSym) ;
      printf2('including %a into scope %a\n', n1, n2)
   END ;
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: IncludeItemIntoList(Module.IncludeList, Sym) |
      DefImpSym: IncludeItemIntoList(DefImp.IncludeList, Sym)

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END PutIncluded ;


(*
   PutExported - places a symbol, Sym into the next level out module.
                 Sym is also placed in the ExportTree of the current inner
                 module.
*)

PROCEDURE PutExported (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
(*
   WriteString('PutExported') ; WriteLn ;
*)
   AddSymToModuleScope(GetLastModuleOrProcedureScope(), Sym) ;
   pSym := GetPsym(GetCurrentModuleScope()) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: PutSymKey(Module.ExportTree, GetSymName(Sym), Sym) ;
                 IF IsUnknown(Sym)
                 THEN
                    PutExportUndeclared(GetCurrentModuleScope(), Sym)
                 END
(*
                 ; WriteKey(Module.name) ; WriteString(' exports ') ;
                 ; WriteKey(GetSymName(Sym)) ; WriteLn ;
*)

      ELSE
         InternalError ('expecting a Module symbol')
      END
   END
END PutExported ;


(*
   PutExportQualified - places a symbol with the name, SymName,
                        into the export tree of the
                        Definition module being compiled.
                        The symbol with name has been EXPORT QUALIFIED
                        by the definition module and therefore any reference
                        to this symbol in the code generation phase
                        will be in the form _Module_Name.
*)

PROCEDURE PutExportQualified (tokenno: CARDINAL; SymName: Name) ;
VAR
   pSym  : PtrToSymbol ;
   n     : Name ;
   Sym,
   ModSym: CARDINAL ;
BEGIN
   ModSym := GetCurrentModule () ;
   Assert (IsDefImp (ModSym)) ;
   Assert (CompilingDefinitionModule () OR
           (GetSymName(ModSym) = MakeKey ('SYSTEM'))) ;
(* printf2('module %a exporting %a\n', GetSymName(ModSym), SymName) ; *)
(*
   WriteString('1st MODULE ') ; WriteKey(GetSymName(ModSym)) ;
   WriteString(' identifier ') ; WriteKey(SymName) ; WriteLn ;
*)
   pSym := GetPsym (ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    IF (GetSymKey (ExportQualifiedTree, SymName) # NulKey) AND
                       (GetSymKey (ExportRequest, SymName) = NulKey)
                    THEN
                       n := GetSymName(ModSym) ;
                       WriteFormat2('identifier (%a) has already been exported from MODULE %a',
                                    SymName, n)
                    ELSIF GetSymKey(ExportRequest, SymName)#NulKey
                    THEN
                       Sym := GetSymKey(ExportRequest, SymName) ;
                       DelSymKey(ExportRequest, SymName) ;
                       PutSymKey(ExportQualifiedTree, SymName, Sym) ;
                       PutExportUndeclared (ModSym, Sym)
                    ELSE
                       Sym := GetDeclareSym(tokenno, SymName) ;
                       PutSymKey(ExportQualifiedTree, SymName, Sym) ;
                       PutExportUndeclared (ModSym, Sym)
                    END
                 END

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutExportQualified ;


(*
   PutExportUnQualified - places a symbol with the name, SymName,
                          into the export tree of the
                          Definition module being compiled.
                          The symbol with Name has been EXPORT UNQUALIFIED
                          by the definition module and therefore any reference
                          to this symbol in the code generation phase
                          will be in the form _Name.
*)

PROCEDURE PutExportUnQualified (tokenno: CARDINAL; SymName: Name) ;
VAR
   pSym  : PtrToSymbol ;
   n     : Name ;
   Sym,
   ModSym: CARDINAL ;
BEGIN
   ModSym := GetCurrentModule() ;
   Assert(IsDefImp(ModSym)) ;
   Assert(CompilingDefinitionModule() OR (GetSymName(ModSym)=MakeKey('SYSTEM'))) ;
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    IF (GetSymKey(ExportUnQualifiedTree, SymName)#NulKey) AND
                       (GetSymKey(ExportRequest, SymName)=NulKey)
                    THEN
                       n := GetSymName(ModSym) ;
                       WriteFormat2('identifier (%a) has already been exported from MODULE %a',
                                    SymName, n)
                    ELSIF GetSymKey(ExportRequest, SymName)#NulKey
                    THEN
                       Sym := GetSymKey(ExportRequest, SymName) ;
                       DelSymKey(ExportRequest, SymName) ;
                       PutSymKey(ExportUnQualifiedTree, SymName, Sym) ;
                       PutExportUndeclared(ModSym, Sym)
                    ELSE
                       Sym := GetDeclareSym(tokenno, SymName) ;
                       PutSymKey(ExportUnQualifiedTree, SymName, Sym) ;
                       PutExportUndeclared(ModSym, Sym)
                    END
                 END

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutExportUnQualified ;


(*
   GetExported - returns the symbol which has a name SymName,
                 and is exported from the definition module ModSym.

*)

PROCEDURE GetExported (tokenno: CARDINAL;
                       ModSym: CARDINAL;
                       SymName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: Sym := RequestFromDefinition (tokenno, ModSym, SymName) |
      ModuleSym: Sym := RequestFromModule (tokenno, ModSym, SymName)

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END ;
   RETURN( Sym )
END GetExported ;


(*
   RequestFromModule - returns a symbol from module ModSym with name, SymName.
*)

PROCEDURE RequestFromModule (tok: CARDINAL; ModSym: CARDINAL; SymName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    Sym := GetSymKey (LocalSymbols, SymName) ;
                    IF Sym=NulSym
                    THEN
                       Sym := FetchUnknownFromDefImp (tok, ModSym, SymName)
                    END
                 END |

      ModuleSym: WITH Module DO
                    Sym := GetSymKey (LocalSymbols, SymName) ;
                    IF Sym=NulSym
                    THEN
                       Sym := FetchUnknownFromModule (tok, ModSym, SymName)
                    END
                 END

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END ;
   RETURN( Sym )
END RequestFromModule ;


(*
   RequestFromDefinition - returns a symbol from module ModSym with name,
                           SymName.
*)

PROCEDURE RequestFromDefinition (tok: CARDINAL;
                                 ModSym: CARDINAL; SymName: Name) : CARDINAL ;
VAR
   pSym       : PtrToSymbol ;
   Sym        : CARDINAL ;
   OldScopePtr: CARDINAL ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    Sym := GetSymKey (ExportQualifiedTree, SymName) ;
                    IF Sym=NulSym
                    THEN
                       Sym := GetSymKey (ExportUnQualifiedTree, SymName) ;
                       IF Sym=NulSym
                       THEN
                          Sym := GetSymKey (ExportRequest, SymName) ;
                          IF Sym=NulSym
                          THEN
                             OldScopePtr := ScopePtr ;
                             StartScope (ModSym) ;
                             Sym := GetScopeSym (SymName, TRUE) ;
                             EndScope ;
                             Assert (OldScopePtr=ScopePtr) ;
                             IF Sym=NulSym
                             THEN
                                Sym := FetchUnknownFromDefImp (tok, ModSym, SymName)
                             ELSE
                                IF IsFieldEnumeration (Sym)
                                THEN
                                   IF IsExported (ModSym, GetType (Sym))
                                   THEN
                                      RETURN( Sym )
                                   END
                                END
                             END ;
                             PutSymKey (ExportRequest, SymName, Sym)
                          END
                       END
                    END
                 END

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END ;
   RETURN( Sym )
END RequestFromDefinition ;


(*
   PutIncludedByDefinition - places a module symbol, Sym, into the
                             included list of the current definition module.
*)

PROCEDURE PutIncludedByDefinition (Sym: CARDINAL) ;
VAR
   pSym  : PtrToSymbol ;
   ModSym: CARDINAL ;
BEGIN
   ModSym := GetCurrentModuleScope() ;
   Assert(IsDefImp(ModSym)) ;
   Assert(IsDefImp(Sym)) ;
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: IncludeItemIntoList(DefImp.DefIncludeList, Sym)

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutIncludedByDefinition ;


(*
   IsIncludedByDefinition - returns TRUE if definition module symbol, Sym, was included
                            by ModSym's definition module.
*)

PROCEDURE IsIncludedByDefinition (ModSym, Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   Assert(IsDefImp(ModSym)) ;
   Assert(IsDefImp(Sym)) ;
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: RETURN( IsItemInList(DefImp.DefIncludeList, Sym) )

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END IsIncludedByDefinition ;


(*
   GetWhereImported - returns the token number where this symbol
                      was imported into the current module.
*)

PROCEDURE GetWhereImported (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(GetCurrentModuleScope()) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym:  RETURN( GetSymKey(DefImp.WhereImported, Sym) ) |
      ModuleSym:  RETURN( GetSymKey(Module.WhereImported, Sym) )

      ELSE
         InternalError ('expecting DefImp or Module symbol')
      END
   END
END GetWhereImported ;


(*
   DisplayName - displays the name.
*)

PROCEDURE DisplayName (sym: WORD) ;
BEGIN
   printf1('   %a', sym)
END DisplayName ;


(*
   DisplaySymbol - displays the name of a symbol
*)

PROCEDURE DisplaySymbol (sym: WORD) ;
VAR
   s: String ;
BEGIN
   s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym)))) ;
   printf2('   %s (%d)', s, sym)
END DisplaySymbol ;


(*
   DisplayTrees - displays the SymbolTrees for Module symbol, ModSym.
*)

PROCEDURE DisplayTrees (ModSym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
   n   : Name ;
BEGIN
   n := GetSymName(ModSym) ;
   printf1('Symbol trees for module/procedure: %a\n', n) ;
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    n := GetSymName(ModSym) ;
                    printf1('%a  UndefinedTree', n) ;
                    ForeachNodeDo(Unresolved, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  Local symbols', n) ;
                    ForeachNodeDo(LocalSymbols, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportRequest', n) ;
                    ForeachNodeDo(ExportRequest, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportQualified', n) ;
                    ForeachNodeDo(ExportQualifiedTree, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportUnQualified', n) ;
                    ForeachNodeDo(ExportUnQualifiedTree, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportUndeclared', n) ;
                    ForeachNodeDo(ExportUndeclared, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  DeclaredObjects', n) ;
                    ForeachNodeDo(NamedObjects, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ImportedObjects', n) ;
                    ForeachNodeDo(NamedImports, DisplayName) ; printf0('\n')
                 END |
      ModuleSym: WITH Module DO
                    n := GetSymName(ModSym) ;
                    printf1('%a  UndefinedTree', n) ;
                    ForeachNodeDo(Unresolved, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  Local symbols', n) ;
                    ForeachNodeDo(LocalSymbols, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ImportTree', n) ;
                    ForeachNodeDo(ImportTree, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportTree', n) ;
                    ForeachNodeDo(ExportTree, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportUndeclared', n) ;
                    ForeachNodeDo(ExportUndeclared, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  DeclaredObjects', n) ;
                    ForeachNodeDo(NamedObjects, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ImportedObjects', n) ;
                    ForeachNodeDo(NamedImports, DisplayName) ; printf0('\n')
                 END |
      ProcedureSym: WITH Procedure DO
                       n := GetSymName(ModSym) ;
                       printf1('%a  UndefinedTree', n) ;
                       ForeachNodeDo(Unresolved, DisplaySymbol) ; printf0('\n') ;
                       printf1('%a  Local symbols', n) ;
                       ForeachNodeDo(LocalSymbols, DisplaySymbol) ; printf0('\n') ;
                       printf1('%a  DeclaredObjects', n) ;
                       ForeachNodeDo(NamedObjects, DisplayName) ; printf0('\n')
                    END

      ELSE
         InternalError ('expecting DefImp symbol')
      END
   END
END DisplayTrees ;


(*
   FetchUnknownFromModule - returns an Unknown symbol from module, ModSym.
*)

PROCEDURE FetchUnknownFromModule (tok: CARDINAL;
                                  ModSym: CARDINAL;
                                  SymName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   pSym := GetPsym (ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF
         ModuleSym: WITH Module DO
                       Sym := GetSymKey (Unresolved, SymName) ;
                       IF Sym=NulSym
                       THEN
                          NewSym (Sym) ;
                          FillInUnknownFields (tok, Sym, SymName) ;
                          PutSymKey (Unresolved, SymName, Sym)
                       END
                    END
      ELSE
         InternalError ('expecting a Module symbol')
      END
   END ;
   RETURN( Sym )
END FetchUnknownFromModule ;


(*
   FetchUnknownFromDefImp - returns an Unknown symbol from module, ModSym.
*)

PROCEDURE FetchUnknownFromDefImp (tok: CARDINAL;
                                  ModSym: CARDINAL;
                                  SymName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   pSym := GetPsym (ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF
         DefImpSym: WITH DefImp DO
                       Sym := GetSymKey (Unresolved , SymName) ;
                       IF Sym=NulSym
                       THEN
                          NewSym(Sym) ;
                          FillInUnknownFields (tok, Sym, SymName) ;
                          PutSymKey (Unresolved, SymName, Sym)
                       END
                    END
      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END ;
   RETURN( Sym )
END FetchUnknownFromDefImp ;


PROCEDURE FetchUnknownFrom (tok: CARDINAL;
                            scope: CARDINAL;
                            SymName: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   pSym := GetPsym(scope) ;
   WITH pSym^ DO
      CASE SymbolType OF
         DefImpSym: WITH DefImp DO
                       Sym := GetSymKey(Unresolved, SymName) ;
                       IF Sym=NulSym
                       THEN
                          NewSym(Sym) ;
                          FillInUnknownFields (tok, Sym, SymName) ;
                          PutSymKey(Unresolved, SymName, Sym)
                       END
                    END |
         ModuleSym: WITH Module DO
                       Sym := GetSymKey(Unresolved, SymName) ;
                       IF Sym=NulSym
                       THEN
                          NewSym(Sym) ;
                          FillInUnknownFields (tok, Sym, SymName) ;
                          PutSymKey(Unresolved, SymName, Sym)
                       END
                    END |
         ProcedureSym: WITH Procedure DO
                          Sym := GetSymKey(Unresolved, SymName) ;
                          IF Sym=NulSym
                          THEN
                             NewSym(Sym) ;
                             FillInUnknownFields (tok, Sym, SymName) ;
                             PutSymKey(Unresolved, SymName, Sym)
                          END
                       END

      ELSE
         InternalError ('expecting a DefImp or Module or Procedure symbol')
      END
   END ;
   RETURN( Sym )
END FetchUnknownFrom ;


(*
   GetFromOuterModule - returns a symbol with name, SymName, which comes
                        from outside the current module.
*)

PROCEDURE GetFromOuterModule (tokenno: CARDINAL; SymName: Name) : CARDINAL ;
VAR
   pCall   : PtrToCallFrame ;
   ScopeId : CARDINAL ;
   Sym,
   ScopeSym: CARDINAL ;
BEGIN
   ScopeId := ScopePtr ;
   pCall := GetPcall(ScopeId) ;
   WHILE (NOT IsModule(pCall^.Search)) AND
         (NOT IsDefImp(pCall^.Search)) DO
      Assert (ScopeId>0) ;
      DEC (ScopeId) ;
      pCall := GetPcall (ScopeId)
   END ;
   DEC (ScopeId) ;
   (* we are now below the current module *)
   WHILE ScopeId>0 DO
      pCall := GetPcall(ScopeId) ;
      ScopeSym := pCall^.Search ;
      IF ScopeSym#NulSym
      THEN
         Sym := GetLocalSym(ScopeSym, SymName) ;
         IF Sym=NulSym
         THEN
            IF IsModule(ScopeSym) OR IsProcedure(ScopeSym) OR IsDefImp(ScopeSym)
            THEN
               IF Sym=NulSym
               THEN
                  Sym := ExamineUnresolvedTree(ScopeSym, SymName) ;
                  IF Sym#NulSym
                  THEN
                     RETURN( Sym )
                  END
               END
            END
         ELSE
            RETURN( Sym )
         END
      END ;
      DEC(ScopeId) ;
      pCall := GetPcall(ScopeId)
   END ;
   (* at this point we force an unknown from the last module scope *)
   RETURN( RequestFromModule (tokenno, GetLastModuleScope(), SymName) )
END GetFromOuterModule ;


(*
   IsExportUnQualified - returns true if a symbol, Sym, was defined as
                         being EXPORT UNQUALIFIED.
                         Sym is expected to be either a procedure or a
                         variable.
*)

PROCEDURE IsExportUnQualified (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym       : PtrToSymbol ;
   OuterModule: CARDINAL ;
BEGIN
   Assert(IsVar(Sym) OR IsProcedure(Sym)) ;
   OuterModule := Sym ;
   REPEAT
      OuterModule := GetScope(OuterModule)
   UNTIL GetScope(OuterModule)=NulSym ;
   pSym := GetPsym(OuterModule) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: RETURN( FALSE ) |
      DefImpSym: RETURN( GetSymKey(
                                    DefImp.ExportUnQualifiedTree,
                                    GetSymName(Sym)
                                  )=Sym
                       )

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END IsExportUnQualified ;


(*
   IsExportQualified - returns true if a symbol, Sym, was defined as
                       being EXPORT QUALIFIED.
                       Sym is expected to be either a procedure or a
                       variable.
*)

PROCEDURE IsExportQualified (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym       : PtrToSymbol ;
   OuterModule: CARDINAL ;
BEGIN
   OuterModule := Sym ;
   REPEAT
      OuterModule := GetScope(OuterModule)
   UNTIL GetScope(OuterModule)=NulSym ;
   pSym := GetPsym(OuterModule) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: RETURN( FALSE ) |
      DefImpSym: RETURN( GetSymKey(DefImp.ExportQualifiedTree, GetSymName(Sym))=Sym )

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END IsExportQualified ;


(*
   ForeachImportedDo - calls a procedure, P, foreach imported symbol
                       in module, ModSym.
*)

PROCEDURE ForeachImportedDo (ModSym: CARDINAL; P: PerformOperation) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    ForeachNodeDo( ImportTree, P ) ;
                    ForeachItemInListDo( IncludeList, P )
                 END |
      ModuleSym: WITH Module DO
                    ForeachNodeDo( ImportTree, P ) ;
                    ForeachItemInListDo( IncludeList, P )
                 END

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END ForeachImportedDo ;


(*
   ForeachExportedDo - calls a procedure, P, foreach imported symbol
                       in module, ModSym.
*)

PROCEDURE ForeachExportedDo (ModSym: CARDINAL; P: PerformOperation) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    ForeachNodeDo( ExportQualifiedTree, P ) ;
                    ForeachNodeDo( ExportUnQualifiedTree, P )
                 END |
      ModuleSym: WITH Module DO
                    ForeachNodeDo( ExportTree, P )
                 END

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END ForeachExportedDo ;


(*
   ForeachLocalSymDo - foreach local symbol in module, Sym, or procedure, Sym,
                       perform the procedure, P.
*)

PROCEDURE ForeachLocalSymDo (Sym: CARDINAL; P: PerformOperation) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym:      WITH DefImp DO
                         ForeachNodeDo( LocalSymbols, P )
                      END |
      ModuleSym:      WITH Module DO
                         ForeachNodeDo( LocalSymbols, P )
                      END |
      ProcedureSym:   WITH Procedure DO
                         ForeachNodeDo( LocalSymbols, P )
                      END |
      RecordSym:      WITH Record DO
                         ForeachNodeDo( LocalSymbols, P )
                      END |
      EnumerationSym: WITH Enumeration DO
                         ForeachNodeDo( LocalSymbols, P )
                      END

      ELSE
         InternalError ('expecting a DefImp, Module or Procedure symbol')
      END
   END
END ForeachLocalSymDo ;


(*
   CheckForUnknownInModule - checks for any unknown symbols in the
                             current module.
                             If any unknown symbols are found then
                             an error message is displayed.
*)

PROCEDURE CheckForUnknownInModule ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(GetCurrentModuleScope()) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    CheckForUnknowns( name, ExportQualifiedTree,
                                      'EXPORT QUALIFIED' ) ;
                    CheckForUnknowns( name, ExportUnQualifiedTree,
                                      'EXPORT UNQUALIFIED' ) ;
                    CheckForSymbols ( ExportRequest,
                                      'requested by another modules import (symbols have not been exported by the appropriate definition module)' ) ;
                    CheckForUnknowns( name, Unresolved, 'unresolved' ) ;
                    CheckForUnknowns( name, LocalSymbols, 'locally used' )
                 END |
      ModuleSym: WITH Module DO
                    CheckForUnknowns( name, Unresolved, 'unresolved' ) ;
                    CheckForUnknowns( name, ExportUndeclared, 'exported but undeclared' ) ;
                    CheckForUnknowns( name, ExportTree, 'exported but undeclared' ) ;
                    CheckForUnknowns( name, LocalSymbols, 'locally used' )
                 END

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END CheckForUnknownInModule ;


(*
   UnknownSymbolError - displays symbol name for symbol, sym.
*)

PROCEDURE UnknownSymbolError (sym: WORD) ;
BEGIN
   IF IsUnreportedUnknown (sym)
   THEN
      IncludeElementIntoSet (ReportedUnknowns, sym) ;
      MetaErrorStringT1 (GetFirstUsed (sym), InitString ("unknown symbol {%1EUad}"), sym)
   END
END UnknownSymbolError ;


(*
   UnknownReported - if sym is an unknown symbol and has not been reported
                     then include it into the set of reported unknowns.
*)

PROCEDURE UnknownReported (sym: CARDINAL) ;
BEGIN
   IF IsUnreportedUnknown (sym)
   THEN
      IncludeElementIntoSet (ReportedUnknowns, sym)
   END
END UnknownReported ;


(*
   IsUnreportedUnknown - returns TRUE if symbol, sym, has not been
                         reported and is an unknown symbol.
*)

PROCEDURE IsUnreportedUnknown (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsUnknown (sym) AND (NOT IsElementInSet (ReportedUnknowns, sym))
END IsUnreportedUnknown ;


(*
   CheckForUnknowns - checks a binary tree, Tree, to see whether it contains
                      an unknown symbol. All unknown symbols are displayed
                      together with an error message.
*)

PROCEDURE CheckForUnknowns (name: Name; Tree: SymbolTree;
                            a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   IF DoesTreeContainAny(Tree, IsUnreportedUnknown)
   THEN
      CurrentError := NewError(GetTokenNo()) ;
      s := InitString("{%E} the following unknown symbols in module %<") ;
      s := ConCat(s, Mark(InitStringCharStar(KeyToCharStar(name)))) ;
      s := ConCat(s, Mark(InitString('%> were '))) ;
      s := ConCat(s, Mark(InitString(a))) ;
      MetaErrorStringT0(GetTokenNo(), s) ;
      ForeachNodeDo(Tree, UnknownSymbolError)
   END
END CheckForUnknowns ;


(*
   SymbolError - displays symbol name for symbol, Sym.
*)

PROCEDURE SymbolError (Sym: WORD) ;
VAR
   e: Error ;
   n: Name ;
BEGIN
   n := GetSymName(Sym) ;
   e := ChainError(GetFirstUsed(Sym), CurrentError) ;
   ErrorFormat1(e, "unknown symbol '%a' found", n)
END SymbolError ;


(*
   CheckForSymbols  - checks a binary tree, Tree, to see whether it contains
                      any symbol. The tree is expected to be empty, if not
                      then an error has occurred.
*)

PROCEDURE CheckForSymbols (Tree: SymbolTree; a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   IF NOT IsEmptyTree(Tree)
   THEN
      s := InitString ("the symbols are unknown at the end of module {%1Ea} when ") ;
      s := ConCat (s, Mark(InitString(a))) ;
      MetaErrorString1 (s, MainModule) ;
      ForeachNodeDo(Tree, SymbolError)
   END
END CheckForSymbols ;


(*
   PutExportUndeclared - places a symbol, Sym, into module, ModSym,
                         ExportUndeclared list provided that Sym
                         is unknown.
*)

PROCEDURE PutExportUndeclared (ModSym: CARDINAL; Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF IsUnknown (Sym)
   THEN
      pSym := GetPsym (ModSym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ModuleSym: PutSymKey (Module.ExportUndeclared, GetSymName (Sym), Sym) |
         DefImpSym: PutSymKey (DefImp.ExportUndeclared, GetSymName (Sym), Sym)

         ELSE
            InternalError ('expecting a DefImp or Module symbol')
         END
      END
   END
END PutExportUndeclared ;


(*
   GetExportUndeclared - returns a symbol which has, name, from module, ModSym,
                         which is in the ExportUndeclared list.
*)

PROCEDURE GetExportUndeclared (ModSym: CARDINAL; name: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: RETURN( GetSymKey(Module.ExportUndeclared, name) ) |
      DefImpSym: RETURN( GetSymKey(DefImp.ExportUndeclared, name) )

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END GetExportUndeclared ;


(*
   RemoveExportUndeclared - removes a symbol, Sym, from the module, ModSym,
                            ExportUndeclaredTree.
*)

PROCEDURE RemoveExportUndeclared (ModSym: CARDINAL; Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: IF GetSymKey(Module.ExportUndeclared, GetSymName(Sym))=Sym
                 THEN
                    DelSymKey(Module.ExportUndeclared, GetSymName(Sym))
                 END |
      DefImpSym: IF GetSymKey(DefImp.ExportUndeclared, GetSymName(Sym))=Sym
                 THEN
                    DelSymKey(DefImp.ExportUndeclared, GetSymName(Sym))
                 END

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END RemoveExportUndeclared ;


(*
   CheckForExportedDeclaration - checks to see whether a definition module
                                 is currently being compiled, if so,
                                 symbol, Sym, is removed from the
                                 ExportUndeclared list.
                                 This procedure is called whenever a symbol
                                 is declared, thus attempting to reduce
                                 the ExportUndeclared list.
*)

PROCEDURE CheckForExportedDeclaration (Sym: CARDINAL) ;
BEGIN
   IF CompilingDefinitionModule()
   THEN
      RemoveExportUndeclared(GetCurrentModule(), Sym)
   END
END CheckForExportedDeclaration ;


(*
   CheckForUndeclaredExports - displays an error and the offending symbols
                               which have been exported but not declared
                               from module, ModSym.
*)

PROCEDURE CheckForUndeclaredExports (ModSym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   (* WriteString('Inside CheckForUndeclaredExports') ; WriteLn ; *)
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: IF NOT IsEmptyTree(Module.ExportUndeclared)
                 THEN
                    MetaError1('undeclared identifier(s) in EXPORT list of {%1ERd} {%1a}', ModSym) ;
                    ForeachNodeDo(Module.ExportUndeclared, UndeclaredSymbolError)
                 END |
      DefImpSym: IF NOT IsEmptyTree(DefImp.ExportUndeclared)
                 THEN
                    IF DoesNotNeedExportList(ModSym)
                    THEN
                       MetaError1('undeclared identifier(s) in {%1ERd} {%1a}', ModSym) ;
                    ELSE
                       MetaError1('undeclared identifier(s) in export list of {%1ERd} {%1a}', ModSym) ;
                    END ;
                    ForeachNodeDo(DefImp.ExportUndeclared, UndeclaredSymbolError)
                 END

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END CheckForUndeclaredExports ;


(*
   UndeclaredSymbolError - displays symbol name for symbol, Sym.
*)

PROCEDURE UndeclaredSymbolError (Sym: WORD) ;
BEGIN
   IF DebugUnknowns
   THEN
      printf1('undeclared symbol (%d)\n', Sym)
   END ;
   MetaError1('{%1UC} undeclared symbol {%1a}', Sym)
END UndeclaredSymbolError ;


(*
   PutExportUnImplemented - places a symbol, Sym, into the currently compiled
                            DefImp module NeedToBeImplemented list.
*)

PROCEDURE PutExportUnImplemented (tokenno: CARDINAL; Sym: CARDINAL) ;
VAR
   pSym  : PtrToSymbol ;
   n1, n2: Name ;
BEGIN
   pSym := GetPsym (CurrentModule) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: IF GetSymKey (DefImp.NeedToBeImplemented, GetSymName (Sym)) = Sym
                 THEN
                    n1 := GetSymName (Sym) ;
                    n2 := GetSymName (CurrentModule) ;
                    WriteFormat2 ('symbol (%a) already exported from module (%a)', n1, n2)
                ELSE
                    PutSymKey (DefImp.NeedToBeImplemented, GetSymName(Sym), Sym)
                 END

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutExportUnImplemented ;


(*
   RemoveExportUnImplemented - removes a symbol, Sym, from the module, ModSym,
                               NeedToBeImplemented list.
*)

PROCEDURE RemoveExportUnImplemented (ModSym: CARDINAL; Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: IF GetSymKey(DefImp.NeedToBeImplemented, GetSymName(Sym))=Sym
                 THEN
                    DelSymKey(DefImp.NeedToBeImplemented, GetSymName(Sym))
                 END

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END RemoveExportUnImplemented ;


VAR
   ExportRequestModule: CARDINAL ;


(*
   RemoveFromExportRequest -
*)

PROCEDURE RemoveFromExportRequest (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(ExportRequestModule) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: IF GetSymKey(DefImp.ExportRequest, GetSymName(Sym))=Sym
                 THEN
                    DelSymKey(DefImp.ExportRequest, GetSymName(Sym))
                 END

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END RemoveFromExportRequest ;


(*
   RemoveEnumerationFromExportRequest - removes enumeration symbol, sym,
                                        (and its fields) from the ExportRequest tree.
*)

PROCEDURE RemoveEnumerationFromExportRequest (ModSym: CARDINAL; Sym: CARDINAL) ;
BEGIN
   IF IsEnumeration(Sym)
   THEN
      ExportRequestModule := ModSym ;
      RemoveFromExportRequest(Sym) ;
      ForeachLocalSymDo(Sym, RemoveFromExportRequest)
   END
END RemoveEnumerationFromExportRequest ;


(*
   CheckForExportedImplementation - checks to see whether an implementation
                                    module is currently being compiled, if so,
                                    symbol, Sym, is removed from the
                                    NeedToBeImplemented list.
                                    This procedure is called whenever a symbol
                                    is declared, thus attempting to reduce
                                    the NeedToBeImplemented list.
                                    Only needs to be called when a TYPE or
                                    PROCEDURE is built since the implementation
                                    module can only implement these objects
                                    declared in the definition module.

                                    It also checks whether a definition module
                                    is currently being compiled and, if so,
                                    it will ensure that symbol, Sym, is removed
                                    from the ExportRequest list. If Sym is an
                                    enumerated type it ensures that its fields
                                    are also removed.
*)

PROCEDURE CheckForExportedImplementation (Sym: CARDINAL) ;
BEGIN
   IF CompilingImplementationModule()
   THEN
      RemoveExportUnImplemented(GetCurrentModule(), Sym)
   END ;
   IF CompilingDefinitionModule() AND IsEnumeration(Sym)
   THEN
      RemoveEnumerationFromExportRequest(GetCurrentModule(), Sym)
   END
END CheckForExportedImplementation ;


(*
   CheckForUnImplementedExports - displays an error and the offending symbols
                                  which have been exported but not implemented
                                  from the current compiled module.
*)

PROCEDURE CheckForUnImplementedExports ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   (* WriteString('Inside CheckForImplementedExports') ; WriteLn ; *)
   pSym := GetPsym (CurrentModule) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: IF NOT IsEmptyTree (DefImp.NeedToBeImplemented)
                 THEN
                    CurrentError := NewError (GetTokenNo ()) ;
                    ErrorFormat1 (CurrentError, 'unimplemented identifier(s) in EXPORT list of DEFINITION MODULE %a\nthe implementation module fails to implement the following exported identifier(s)', DefImp.name) ;
                    ForeachNodeDo (DefImp.NeedToBeImplemented, UnImplementedSymbolError)
                 END

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END CheckForUnImplementedExports ;


(*
   UnImplementedSymbolError - displays symbol name for symbol, Sym.
*)

PROCEDURE UnImplementedSymbolError (Sym: WORD) ;
VAR
   n: Name ;
BEGIN
   CurrentError := ChainError (GetFirstUsed (Sym), CurrentError) ;
   IF IsType (Sym)
   THEN
      n := GetSymName(Sym) ;
      ErrorFormat1 (CurrentError, 'hidden type is undeclared (%a)', n)
   ELSIF IsProcedure (Sym)
   THEN
      n := GetSymName(Sym) ;
      ErrorFormat1 (CurrentError, 'procedure is undeclared (%a)', n)
   ELSIF IsProcType (Sym)
   THEN
      n := GetSymName(Sym) ;
      ErrorFormat1 (CurrentError, 'procedure type is undeclared (%a)', n)
   ELSE
      ErrorFormat0 (CurrentError, 'undeclared symbol')
   END
END UnImplementedSymbolError ;


(*
   PutHiddenTypeDeclared - sets a flag in the current compiled module which
                           indicates that a Hidden Type is declared within
                           the implementation part of the module.
                           This procedure is expected to be called while
                           compiling the associated definition module.
*)

PROCEDURE PutHiddenTypeDeclared ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(CurrentModule) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: DefImp.ContainsHiddenType := TRUE

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutHiddenTypeDeclared ;


(*
   IsHiddenTypeDeclared - returns true if a Hidden Type was declared in
                          the module, Sym.
*)

PROCEDURE IsHiddenTypeDeclared (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: RETURN( DefImp.ContainsHiddenType )

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END IsHiddenTypeDeclared ;


(*
   PutModuleContainsBuiltin - sets a flag in the current compiled module which
                              indicates that a builtin procedure is being declared.
                              This is only expected to be called when we are
                              parsing the definition module.
*)

PROCEDURE PutModuleContainsBuiltin ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   PutHiddenTypeDeclared ;
   pSym := GetPsym(CurrentModule) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: DefImp.ContainsBuiltin := TRUE

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutModuleContainsBuiltin ;


(*
   IsBuiltinInModule - returns true if a module, Sym, has declared a builtin procedure.
*)

PROCEDURE IsBuiltinInModule (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: RETURN( DefImp.ContainsBuiltin )

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END IsBuiltinInModule ;


(*
   PutDefinitionForC - sets a flag in the current compiled module which
                       indicates that this module is a wrapper for a C
                       file. Parameters passes to procedures in this module
                       will adopt the C calling convention.
*)

PROCEDURE PutDefinitionForC (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: DefImp.ForC := TRUE

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutDefinitionForC ;


(*
   IsDefinitionForC - returns true if this definition module was declared
                      as a DEFINITION MODULE FOR "C".
*)

PROCEDURE IsDefinitionForC (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: RETURN( DefImp.ForC )

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END IsDefinitionForC ;


(*
   PutDoesNeedExportList - sets a flag in module, Sym, which
                           indicates that this module requires an explicit
                           EXPORT QUALIFIED or UNQUALIFIED list. PIM-2
*)

PROCEDURE PutDoesNeedExportList (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: DefImp.NeedExportList := TRUE

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutDoesNeedExportList ;


(*
   PutDoesNotNeedExportList - sets a flag in module, Sym, which
                              indicates that this module does not require an explicit
                              EXPORT QUALIFIED or UNQUALIFIED list. PIM-3|4
*)

PROCEDURE PutDoesNotNeedExportList (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: DefImp.NeedExportList := FALSE

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END PutDoesNotNeedExportList ;


(*
   DoesNotNeedExportList - returns TRUE if module, Sym, does not require an explicit
                           EXPORT QUALIFIED list.
*)

PROCEDURE DoesNotNeedExportList (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: RETURN( NOT DefImp.NeedExportList )

      ELSE
         InternalError ('expecting a DefImp symbol')
      END
   END
END DoesNotNeedExportList ;


(*
   CheckForEnumerationInCurrentModule - checks to see whether the enumeration
                                        type symbol, Sym, has been entered into
                                        the current modules scope list.
*)

PROCEDURE CheckForEnumerationInCurrentModule (Sym: CARDINAL) ;
VAR
   pSym  : PtrToSymbol ;
   ModSym: CARDINAL ;
BEGIN
   IF (SkipType(Sym)#NulSym) AND IsEnumeration(SkipType(Sym))
   THEN
      Sym := SkipType(Sym)
   END ;

   IF IsEnumeration(Sym)
   THEN
      ModSym := GetCurrentModuleScope() ;
      pSym := GetPsym(ModSym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         DefImpSym: CheckEnumerationInList(DefImp.EnumerationScopeList, Sym) |
         ModuleSym: CheckEnumerationInList(Module.EnumerationScopeList, Sym)

         ELSE
            InternalError ('expecting a DefImp or Module symbol')
         END
      END
   END
END CheckForEnumerationInCurrentModule ;


(*
   CheckEnumerationInList - places symbol, Sym, in the list, l,
                            providing it does not already exist.
                            PseudoScope(Sym) is called if Sym needs to
                            be added to the enumeration list, l.
*)

PROCEDURE CheckEnumerationInList (l: List; Sym: CARDINAL) ;
BEGIN
   IF NOT IsItemInList(l, Sym)
   THEN
      PutItemIntoList(l, Sym) ;
      PseudoScope(Sym)
   END
END CheckEnumerationInList ;


(*
   CheckIfEnumerationExported - An outer module may use an enumeration that
                                is declared inside an inner module. The usage
                                may occur before definition. The first pass
                                exports a symbol, later the symbol is declared
                                as an emumeration type. At this stage the
                                CheckIfEnumerationExported procedure should be
                                called. This procedure ripples from the current
                                (inner) module to outer module and every time
                                it is exported it must be added to the outer
                                module EnumerationScopeList.
*)

PROCEDURE CheckIfEnumerationExported (Sym: CARDINAL; ScopeId: CARDINAL) ;
VAR
   pCall      : PtrToCallFrame ;
   InnerModId,
   OuterModId : CARDINAL ;
   InnerModSym,
   OuterModSym: CARDINAL ;
BEGIN
   InnerModId := GetModuleScopeId(ScopeId) ;
   IF InnerModId>0
   THEN
      OuterModId := GetModuleScopeId(InnerModId-1) ;
      IF OuterModId>0
      THEN
         pCall := GetPcall(InnerModId) ;
         InnerModSym := pCall^.Search ;
         pCall := GetPcall(OuterModId) ;
         OuterModSym := pCall^.Search ;
         IF (InnerModSym#NulSym) AND (OuterModSym#NulSym)
         THEN
            IF IsExported(InnerModSym, Sym)
            THEN
               CheckForEnumerationInOuterModule(Sym, OuterModSym) ;
               CheckIfEnumerationExported(Sym, OuterModId)
            END
         END
      END
   END
END CheckIfEnumerationExported ;


(*
   CheckForEnumerationInOuterModule - checks to see whether the enumeration
                                      type symbol, Sym, has been entered into
                                      the outer module, OuterModule, scope list.
                                      OuterModule may be internal to the
                                      program module.
*)

PROCEDURE CheckForEnumerationInOuterModule (Sym: CARDINAL;
                                            OuterModule: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(OuterModule) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: IncludeItemIntoList(DefImp.EnumerationScopeList, Sym) |
      ModuleSym: IncludeItemIntoList(Module.EnumerationScopeList, Sym)

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END CheckForEnumerationInOuterModule ;


(*
   IsExported - returns true if a symbol, Sym, is exported
                from module, ModSym.
                If ModSym is a DefImp symbol then its
                ExportQualified and ExportUnQualified lists are examined.
*)

PROCEDURE IsExported (ModSym: CARDINAL; Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym   : PtrToSymbol ;
   SymName: Name ;
BEGIN
   SymName := GetSymName(Sym) ;
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    RETURN(
                            (GetSymKey(ExportQualifiedTree, SymName)=Sym) OR
                            (GetSymKey(ExportUnQualifiedTree, SymName)=Sym)
                          )
                 END |
      ModuleSym: WITH Module DO
                    RETURN( GetSymKey(ExportTree, SymName)=Sym )
                 END

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END IsExported ;


(*
   IsImported - returns true if a symbol, Sym, in module, ModSym,
                was imported.
*)

PROCEDURE IsImported (ModSym: CARDINAL; Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym   : PtrToSymbol ;
   SymName: Name ;
BEGIN
   SymName := GetSymName(Sym) ;
   pSym := GetPsym(ModSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    RETURN(
                            (GetSymKey(ImportTree, SymName)=Sym) OR
                            IsItemInList(IncludeList, Sym)
                          )
                 END |
      ModuleSym: WITH Module DO
                    RETURN(
                            (GetSymKey(ImportTree, SymName)=Sym) OR
                            IsItemInList(IncludeList, Sym)
                          )
                 END

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END IsImported ;


(*
   IsType - returns true if the Sym is a type symbol.
*)

PROCEDURE IsType (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=TypeSym )
END IsType ;


(*
   IsReturnOptional - returns TRUE if the return value for, sym, is
                      optional.
*)

PROCEDURE IsReturnOptional (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.ReturnOptional ) |
      ProcTypeSym : RETURN( ProcType.ReturnOptional )

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END
END IsReturnOptional ;


(*
   SetReturnOptional - sets the ReturnOptional field in the Procedure or
                       ProcType symboltable entry.
*)

PROCEDURE SetReturnOptional (sym: CARDINAL; isopt: BOOLEAN) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.ReturnOptional := isopt |
      ProcTypeSym : ProcType.ReturnOptional := isopt

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END
END SetReturnOptional ;


(*
   CheckOptFunction - checks to see whether the optional return value
                      has been set before and if it differs it will
                      generate an error message.  It will set the
                      new value to, isopt.
*)

PROCEDURE CheckOptFunction (sym: CARDINAL; isopt: BOOLEAN) ;
VAR
   n: Name ;
   e: Error ;
BEGIN
   IF GetType(sym)#NulSym
   THEN
      IF IsReturnOptional(sym) AND (NOT isopt)
      THEN
         n := GetSymName(sym) ;
         e := NewError(GetTokenNo()) ;
         ErrorFormat1(e, 'function (%a) has no optional return value here', n) ;
         e := ChainError(GetDeclaredMod(sym), e) ;
         ErrorFormat1(e, 'whereas the same function (%a) was declared to have an optional return value at this point', n)
      ELSIF (NOT IsReturnOptional(sym)) AND isopt
      THEN
         n := GetSymName(sym) ;
         e := NewError(GetTokenNo()) ;
         ErrorFormat1(e, 'function (%a) has an optional return value', n) ;
         e := ChainError(GetDeclaredMod(sym), e) ;
         ErrorFormat1(e, 'whereas the same function (%a) was declared to have no optional return value at this point', n)
      END
   END ;
   SetReturnOptional(sym, isopt)
END CheckOptFunction ;


(*
   PutFunction - Places a TypeSym as the return type to a procedure Sym.
*)

PROCEDURE PutFunction (Sym: CARDINAL; TypeSym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: CheckOptFunction(Sym, FALSE) ; Procedure.ReturnType := TypeSym |
      ProcTypeSym : CheckOptFunction(Sym, FALSE) ; ProcType.ReturnType := TypeSym

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END
END PutFunction ;


(*
   PutOptFunction - places a TypeSym as the optional return type to a procedure Sym.
*)

PROCEDURE PutOptFunction (Sym: CARDINAL; TypeSym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: CheckOptFunction(Sym, TRUE) ; Procedure.ReturnType := TypeSym |
      ProcTypeSym : CheckOptFunction(Sym, TRUE) ; ProcType.ReturnType := TypeSym

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END
END PutOptFunction ;


(*
   MakeVariableForParam -
*)

PROCEDURE MakeVariableForParam (tok      : CARDINAL;
                                ParamName: Name;
                                ProcSym  : CARDINAL ;
                                no       : CARDINAL) : CARDINAL ;
VAR
   pSym       : PtrToSymbol ;
   VariableSym: CARDINAL ;
BEGIN
   VariableSym := MakeVar(tok, ParamName) ;
   pSym := GetPsym(VariableSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: RETURN( NulSym ) |
      VarSym  : Var.IsParam := TRUE     (* Variable is really a parameter *)

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END ;
   (* Note that the parameter is now treated as a local variable *)
   PutVar(VariableSym, GetType(GetNthParam(ProcSym, no))) ;
   PutDeclared(tok, VariableSym) ;
   (*
      Normal VAR parameters have LeftValue,
      however Unbounded VAR parameters have RightValue.
      Non VAR parameters always have RightValue.
   *)
   IF IsVarParam(ProcSym, no) AND (NOT IsUnboundedParam(ProcSym, no))
   THEN
      PutMode(VariableSym, LeftValue)
   ELSE
      PutMode(VariableSym, RightValue)
   END ;
   RETURN( VariableSym )
END MakeVariableForParam ;


(*
   PutParam - Places a Non VAR parameter ParamName with type ParamType into
              procedure Sym. The parameter number is ParamNo.
              If the procedure Sym already has this parameter then
              the parameter is checked for consistancy and the
              consistancy test is returned.
*)

PROCEDURE PutParam (tok: CARDINAL; Sym: CARDINAL; ParamNo: CARDINAL;
                    ParamName: Name; ParamType: CARDINAL;
                    isUnbounded: BOOLEAN) : BOOLEAN ;
VAR
   pSym       : PtrToSymbol ;
   ParSym     : CARDINAL ;
   VariableSym: CARDINAL ;
BEGIN
   IF ParamNo<=NoOfParam(Sym)
   THEN
      InternalError ('why are we trying to put parameters again')
   ELSE
      (* Add a new parameter *)
      NewSym(ParSym) ;
      pSym := GetPsym(ParSym) ;
      WITH pSym^ DO
         SymbolType := ParamSym ;
         WITH Param DO
            name := ParamName ;
            Type := ParamType ;
            IsUnbounded := isUnbounded ;
            ShadowVar := NulSym ;
            InitWhereDeclaredTok(tok, At)
         END
      END ;
      AddParameter(Sym, ParSym) ;
      IF ParamName#NulName
      THEN
         VariableSym := MakeVariableForParam(tok, ParamName, Sym, ParamNo) ;
         IF VariableSym=NulSym
         THEN
            RETURN( FALSE )
         ELSE
            pSym := GetPsym(ParSym) ;
            pSym^.Param.ShadowVar := VariableSym
         END
      END
   END ;
   RETURN( TRUE )
END PutParam ;


(*
   PutVarParam - Places a Non VAR parameter ParamName with type
                 ParamType into procedure Sym.
                 The parameter number is ParamNo.
                 If the procedure Sym already has this parameter then
                 the parameter is checked for consistancy and the
                 consistancy test is returned.
*)

PROCEDURE PutVarParam (tok: CARDINAL; Sym: CARDINAL; ParamNo: CARDINAL;
                       ParamName: Name; ParamType: CARDINAL;
                       isUnbounded: BOOLEAN) : BOOLEAN ;
VAR
   pSym       : PtrToSymbol ;
   ParSym     : CARDINAL ;
   VariableSym: CARDINAL ;
BEGIN
   IF ParamNo<=NoOfParam(Sym)
   THEN
      InternalError ('why are we trying to put parameters again')
   ELSE
      (* Add a new parameter *)
      NewSym(ParSym) ;
      pSym := GetPsym(ParSym) ;
      WITH pSym^ DO
         SymbolType := VarParamSym ;
         WITH VarParam DO
            name := ParamName ;
            Type := ParamType ;
            IsUnbounded := isUnbounded ;
            ShadowVar := NulSym ;
            InitWhereDeclaredTok(tok, At)
         END
      END ;
      AddParameter(Sym, ParSym) ;
      IF ParamName#NulName
      THEN
         VariableSym := MakeVariableForParam(tok, ParamName, Sym, ParamNo) ;
         IF VariableSym=NulSym
         THEN
            RETURN( FALSE )
         ELSE
            pSym := GetPsym(ParSym) ;
            pSym^.VarParam.ShadowVar := VariableSym
         END
      END ;
      RETURN( TRUE )
   END
END PutVarParam ;


(*
   PutParamName - assigns a name, name, to paramater, no, of procedure,
                  ProcSym.
*)

PROCEDURE PutParamName (tok: CARDINAL; ProcSym: CARDINAL; no: CARDINAL; name: Name) ;
VAR
   pSym  : PtrToSymbol ;
   ParSym: CARDINAL ;
BEGIN
   pSym := GetPsym(ProcSym) ;
   ParSym := NulSym ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : RETURN |
      ProcedureSym: ParSym := GetItemFromList(Procedure.ListOfParam, no) |
      ProcTypeSym : ParSym := GetItemFromList(ProcType.ListOfParam, no)

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END ;
   pSym := GetPsym(ParSym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ParamSym:    IF Param.name=NulName
                   THEN
                      Param.name := name ;
                      Param.ShadowVar := MakeVariableForParam(tok, name, ProcSym, no)
                   ELSE
                      InternalError ('name of parameter has already been assigned')
                   END |
      VarParamSym: IF VarParam.name=NulName
                   THEN
                      VarParam.name := name ;
                      VarParam.ShadowVar := MakeVariableForParam(tok, name, ProcSym, no)
                   ELSE
                      InternalError ('name of parameter has already been assigned')
                   END

      ELSE
         InternalError ('expecting a VarParam or Param symbol')
      END
   END
END PutParamName ;


(*
   AddParameter - adds a parameter ParSym to a procedure Sym.
*)

PROCEDURE AddParameter (Sym: CARDINAL; ParSym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: PutItemIntoList(Procedure.ListOfParam, ParSym) |
      ProcTypeSym : PutItemIntoList(ProcType.ListOfParam, ParSym)

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END AddParameter ;


(*
   IsVarParam - Returns a conditional depending whether parameter ParamNo
                is a VAR parameter.
*)

PROCEDURE IsVarParam (Sym: CARDINAL; ParamNo: CARDINAL) : BOOLEAN ;
VAR
   pSym : PtrToSymbol ;
   IsVar: BOOLEAN ;
BEGIN
   IsVar := FALSE ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : |
      ProcedureSym: IsVar := IsNthParamVar(Procedure.ListOfParam, ParamNo) |
      ProcTypeSym : IsVar := IsNthParamVar(ProcType.ListOfParam, ParamNo)

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END ;
   RETURN( IsVar )
END IsVarParam ;


(*
   IsNthParamVar - returns true if the n th parameter of the parameter list,
                   List, is a VAR parameter.
*)

PROCEDURE IsNthParamVar (Head: List; n: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
   p   : CARDINAL ;
BEGIN
   p := GetItemFromList(Head, n) ;
   IF p=NulSym
   THEN
      InternalError ('parameter does not exist')
   ELSE
      pSym := GetPsym(p) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ErrorSym   : RETURN( FALSE ) |
         VarParamSym: RETURN( TRUE ) |
         ParamSym   : RETURN( FALSE )

         ELSE
            InternalError ('expecting Param or VarParam symbol')
         END
      END
   END
END IsNthParamVar ;


(*
   NoOfParam - Returns the number of parameters that procedure Sym contains.
*)

PROCEDURE NoOfParam (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   n   : CARDINAL ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : n := 0 |
      ProcedureSym: n := NoOfItemsInList(Procedure.ListOfParam) |
      ProcTypeSym : n := NoOfItemsInList(ProcType.ListOfParam)

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END ;
   RETURN( n )
END NoOfParam ;


(*
   HasVarParameters - returns TRUE if procedure, p, has any VAR parameters.
*)

PROCEDURE HasVarParameters (p: CARDINAL) : BOOLEAN ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := NoOfParam(p) ;
   i := 1 ;
   WHILE i<=n DO
      IF IsVarParam(p, i)
      THEN
         RETURN TRUE
      END ;
      INC(i)
   END ;
   RETURN FALSE
END HasVarParameters ;


(*
   PutUseVarArgs - tell the symbol table that this procedure, Sym,
                   uses varargs.
                   The procedure _must_ be declared inside a
                   DEFINITION FOR "C"

*)

PROCEDURE PutUseVarArgs (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: Procedure.HasVarArgs := TRUE |
      ProcTypeSym : ProcType.HasVarArgs := TRUE

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END
END PutUseVarArgs ;


(*
   UsesVarArgs - returns TRUE if procedure, Sym, uses varargs.
                 The procedure _must_ be declared inside a
                 DEFINITION FOR "C"
*)

PROCEDURE UsesVarArgs (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.HasVarArgs ) |
      ProcTypeSym : RETURN( ProcType.HasVarArgs )

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END
END UsesVarArgs ;


(*
   PutUseOptArg - tell the symbol table that this procedure, Sym,
                  uses an optarg.
*)

PROCEDURE PutUseOptArg (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: Procedure.HasOptArg := TRUE |
      ProcTypeSym : ProcType.HasOptArg := TRUE

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END
END PutUseOptArg ;


(*
   UsesOptArg - returns TRUE if procedure, Sym, uses varargs.
*)

PROCEDURE UsesOptArg (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.HasOptArg ) |
      ProcTypeSym : RETURN( ProcType.HasOptArg )

      ELSE
         InternalError ('expecting a Procedure or ProcType symbol')
      END
   END
END UsesOptArg ;


(*
   PutOptArgInit - makes symbol, Sym, the initializer value to
                   procedure, ProcSym.
*)

PROCEDURE PutOptArgInit (ProcSym, Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   IF NOT IsError(ProcSym)
   THEN
      IF UsesOptArg(ProcSym)
      THEN
         pSym := GetPsym(ProcSym) ;
         WITH pSym^ DO
            CASE SymbolType OF

            ErrorSym    : |
            ProcedureSym: Procedure.OptArgInit := Sym |
            ProcTypeSym : ProcType.OptArgInit := Sym

            ELSE
               InternalError ('expecting a Procedure or ProcType symbol')
            END
         END
      END
   END
END PutOptArgInit ;


(*
   GetOptArgInit - returns the initializer value to the optional parameter in
                   procedure, ProcSym.
*)

PROCEDURE GetOptArgInit (ProcSym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF NOT IsError(ProcSym)
   THEN
      IF UsesOptArg(ProcSym)
      THEN
         pSym := GetPsym(ProcSym) ;
         WITH pSym^ DO
            CASE SymbolType OF

            ErrorSym    : |
            ProcedureSym: RETURN( Procedure.OptArgInit ) |
            ProcTypeSym : RETURN( ProcType.OptArgInit )

            ELSE
               InternalError ('expecting a Procedure or ProcType symbol')
            END
         END
      END
   END ;
   RETURN( NulSym )
END GetOptArgInit ;


(*
   NoOfVariables - returns the number of variables in scope.  The scope maybe
                   a procedure, module or defimp scope.
*)

PROCEDURE NoOfVariables (scope: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF IsProcedure (scope)
   THEN
      RETURN NoOfLocalVar (scope)
   ELSIF IsModule (scope)
   THEN
      pSym := GetPsym (scope) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ModuleSym:  RETURN NoOfItemsInList (Module.ListOfVars)

         ELSE
            InternalError ('expecting module symbol')
         END
      END
   ELSIF IsDefImp (scope)
   THEN
      pSym := GetPsym (scope) ;
      WITH pSym^ DO
         CASE SymbolType OF

         DefImpSym:  RETURN NoOfItemsInList (DefImp.ListOfVars)

         ELSE
            InternalError ('expecting defimp symbol')
         END
      END
   ELSE
      InternalError ('expecting procedure, module or defimp symbol')
   END
END NoOfVariables ;


(*
   NoOfLocalVar - returns the number of local variables that exist in
                  procedure Sym. Parameters are NOT included in the
                  count.
*)

PROCEDURE NoOfLocalVar (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   n   : CARDINAL ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : n := 0 |
      ProcedureSym: n := NoOfItemsInList(Procedure.ListOfVars)

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END ;
   (*
      Parameters are actually included in the list of local varaibles,
      therefore we must subtract the Parameter Number from local variable
      total.
   *)
   RETURN( n-NoOfParam(Sym) )
END NoOfLocalVar ;


(*
   IsParameterVar - returns true if parameter symbol Sym
                    was declared as a VAR.
*)

PROCEDURE IsParameterVar (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ParamSym   :  RETURN( FALSE ) |
      VarParamSym:  RETURN( TRUE )

      ELSE
         InternalError ('expecting Param or VarParam symbol')
      END
   END
END IsParameterVar ;


(*
   IsParameterUnbounded - returns TRUE if parameter, Sym, is
                          unbounded.
*)

PROCEDURE IsParameterUnbounded (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ParamSym   :  RETURN( Param.IsUnbounded ) |
      VarParamSym:  RETURN( VarParam.IsUnbounded )

      ELSE
         InternalError ('expecting Param or VarParam symbol')
      END
   END
END IsParameterUnbounded ;


(*
   IsUnboundedParam - Returns a conditional depending whether parameter
                      ParamNo is an unbounded array procedure parameter.
*)

PROCEDURE IsUnboundedParam (Sym: CARDINAL; ParamNo: CARDINAL) : BOOLEAN ;
VAR
   param: CARDINAL ;
BEGIN
   Assert(IsProcedure(Sym) OR IsProcType(Sym)) ;
   param := GetNthParam(Sym, ParamNo) ;
   RETURN( IsParameterUnbounded(param) )
END IsUnboundedParam ;


(*
   IsParameter - returns true if Sym is a parameter symbol.
*)

PROCEDURE IsParameter (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ParamSym,
      VarParamSym:  RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END
   END
END IsParameter ;


(*
   GetParameterShadowVar - returns the local variable associated with the
                           parameter symbol, sym.
*)

PROCEDURE GetParameterShadowVar (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ParamSym   :  RETURN( Param.ShadowVar ) |
      VarParamSym:  RETURN( VarParam.ShadowVar )

      ELSE
         InternalError ('expecting a ParamSym or VarParamSym')
      END
   END
END GetParameterShadowVar ;


(*
   IsProcedure - returns true if Sym is a procedure symbol.
*)

PROCEDURE IsProcedure (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=ProcedureSym )
END IsProcedure ;


(*
   ProcedureParametersDefined - dictates to procedure symbol, Sym,
                                that its parameters have been defined.
*)

PROCEDURE ProcedureParametersDefined (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : |
      ProcedureSym: Assert(NOT Procedure.ParamDefined) ;
                    Procedure.ParamDefined := TRUE

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END ProcedureParametersDefined ;


(*
   AreProcedureParametersDefined - returns true if the parameters to procedure
                                   symbol, Sym, have been defined.
*)

PROCEDURE AreProcedureParametersDefined (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.ParamDefined )

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END AreProcedureParametersDefined ;


(*
   ParametersDefinedInDefinition - dictates to procedure symbol, Sym,
                                   that its parameters have been defined in
                                   a definition module.
*)

PROCEDURE ParametersDefinedInDefinition (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : |
      ProcedureSym: Assert(NOT Procedure.DefinedInDef) ;
                    Procedure.DefinedInDef := TRUE

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END ParametersDefinedInDefinition ;


(*
   AreParametersDefinedInDefinition - returns true if procedure symbol, Sym,
                                      has had its parameters been defined in
                                      a definition module.
*)

PROCEDURE AreParametersDefinedInDefinition (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.DefinedInDef )

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END AreParametersDefinedInDefinition ;


(*
   ParametersDefinedInImplementation - dictates to procedure symbol, Sym,
                                       that its parameters have been defined in
                                       a implemtation module.
*)

PROCEDURE ParametersDefinedInImplementation (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : |
      ProcedureSym: Assert(NOT Procedure.DefinedInImp) ;
                    Procedure.DefinedInImp := TRUE

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END ParametersDefinedInImplementation ;


(*
   AreParametersDefinedInImplementation - returns true if procedure symbol, Sym,
                                          has had its parameters been defined in
                                          an implementation module.
*)

PROCEDURE AreParametersDefinedInImplementation (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.DefinedInImp )

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END AreParametersDefinedInImplementation ;


(*
   FillInUnknownFields -
*)

PROCEDURE FillInUnknownFields (tok: CARDINAL; sym: CARDINAL; SymName: Name) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      SymbolType := UndefinedSym ;
      WITH Undefined DO
         name     := SymName ;
         oafamily := NulSym ;
         InitWhereFirstUsedTok (tok, At)
      END
   END
END FillInUnknownFields ;


(*
   FillInPointerFields - given a new symbol, sym, make it a pointer symbol
                         and initialize its fields.
*)

PROCEDURE FillInPointerFields (Sym: CARDINAL; PointerName: Name;
                               scope: CARDINAL; oaf: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF NOT IsError(Sym)
   THEN
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         SymbolType := PointerSym ;
         CASE SymbolType OF

         PointerSym: Pointer.Type := NulSym ;
                     Pointer.name := PointerName ;
                     Pointer.oafamily := oaf ;         (* The unbounded for this *)
		     InitTree(Pointer.ConstLitTree) ;  (* constants of this type *)
                     Pointer.Scope := scope ;          (* Which scope created it *)
                     Pointer.Size := InitValue() ;
                     Pointer.Align := NulSym ;         (* Alignment of this type *)

         ELSE
            InternalError ('expecting a Pointer symbol')
         END
      END
   END
END FillInPointerFields ;


(*
   MakePointer - returns a pointer symbol with PointerName.
*)

PROCEDURE MakePointer (tok: CARDINAL; PointerName: Name) : CARDINAL ;
VAR
   oaf, sym: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(tok, PointerName, oaf) ;
   FillInPointerFields(sym, PointerName, GetCurrentScope(), oaf) ;
   ForeachOAFamily(oaf, doFillInOAFamily) ;
   RETURN( sym )
END MakePointer ;


(*
   PutPointer - gives a pointer symbol a type, PointerType.
*)

PROCEDURE PutPointer (Sym: CARDINAL; PointerType: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym  : |
      PointerSym: Pointer.Type := PointerType

      ELSE
         InternalError ('expecting a Pointer symbol')
      END
   END
END PutPointer ;


(*
   IsPointer - returns true is Sym is a pointer type symbol.
*)

PROCEDURE IsPointer (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=PointerSym )
END IsPointer ;


(*
   IsRecord - returns true is Sym is a record type symbol.
*)

PROCEDURE IsRecord (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=RecordSym )
END IsRecord ;


(*
   IsArray - returns true is Sym is an array type symbol.
*)

PROCEDURE IsArray (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=ArraySym )
END IsArray ;


(*
   IsEnumeration - returns true if Sym is an enumeration symbol.
*)

PROCEDURE IsEnumeration (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=EnumerationSym )
END IsEnumeration ;


(*
   IsUnbounded - returns true if Sym is an unbounded symbol.
*)

PROCEDURE IsUnbounded (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=UnboundedSym )
END IsUnbounded ;


(*
   GetVarScope - returns the symbol which is the scope of variable Sym.
                 ie a Module, DefImp or Procedure Symbol.
*)

PROCEDURE GetVarScope (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: RETURN( NulSym ) |
      VarSym  : RETURN( Var.Scope )

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END
END GetVarScope ;


(*
   NoOfElements - Returns the number of elements in array Sym,
                  or the number of elements in an enumeration Sym or
                  the number of interface symbols in an Interface list.
*)

PROCEDURE NoOfElements (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   n   : CARDINAL ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym      : n := 0 |
(*
      ArraySym      ,
      UnboundedSym  : n := 1 |   (* Standard language limitation *)
*)
      EnumerationSym: n := pSym^.Enumeration.NoOfElements |
      InterfaceSym  : n := HighIndice(Interface.Parameters)

      ELSE
         InternalError ('expecting an Array or UnBounded symbol')
      END
   END ;
   RETURN( n )
END NoOfElements ;


(*
   PutArraySubscript - places an index field into the array Sym. The
                       index field is a subscript sym.
*)

PROCEDURE PutArraySubscript (Sym: CARDINAL; SubscriptSymbol: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      ArraySym: Array.Subscript := SubscriptSymbol

      ELSE
         InternalError ('expecting an Array symbol')
      END
   END
END PutArraySubscript ;


(*
   GetArraySubscript - returns the subscript symbol for array, Sym.
*)

PROCEDURE GetArraySubscript (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: RETURN( NulSym ) |
      ArraySym: RETURN( Array.Subscript )

      ELSE
         InternalError ('expecting an Array symbol')
      END
   END
END GetArraySubscript ;


(*
   MakeSubscript - makes a subscript Symbol.
                   No name is required.
*)

PROCEDURE MakeSubscript () : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   NewSym(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := SubscriptSym ;
      WITH Subscript DO
         Type := NulSym ;        (* Index to a subrange symbol. *)
         Size := InitValue() ;   (* Size of this indice in*Size *)
         Offset := InitValue() ; (* Offset at runtime of symbol *)
                                 (* Pseudo ie: Offset+Size*i    *)
                                 (* 1..n. The array offset is   *)
                                 (* the real memory offset.     *)
                                 (* This offset allows the a[i] *)
                                 (* to be calculated without    *)
                                 (* the need to perform         *)
                                 (* subtractions when a[4..10]  *)
                                 (* needs to be indexed.        *)
         InitWhereDeclared(At)   (* Declared here               *)
      END
   END ;
   RETURN( Sym )
END MakeSubscript ;


(*
   PutSubscript - gives a subscript symbol a type, SimpleType.
*)

PROCEDURE PutSubscript (Sym: CARDINAL; SimpleType: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      SubscriptSym: Subscript.Type := SimpleType ;

      ELSE
         InternalError ('expecting a SubScript symbol')
      END
   END
END PutSubscript ;


(*
   MakeSet - makes a set Symbol with name, SetName.
*)

PROCEDURE MakeSet (tok: CARDINAL; SetName: Name) : CARDINAL ;
VAR
   pSym    : PtrToSymbol ;
   oaf, sym: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(tok, SetName, oaf) ;
   IF NOT IsError(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         SymbolType := SetSym ;
         WITH Set DO
            name := SetName ;          (* The name of the set.        *)
            Type := NulSym ;           (* Index to a subrange symbol. *)
            Size := InitValue() ;      (* Size of this set            *)
            InitPacked(packedInfo) ;        (* not packed and no      *)
                                            (* equivalent (yet).      *)
            ispacked := FALSE ;        (* Not yet known to be packed. *)
            oafamily := oaf ;          (* The unbounded sym for this  *)
            Scope := GetCurrentScope() ;    (* Which scope created it *)
            InitWhereDeclaredTok(tok, At)   (* Declared here          *)
         END
      END
   END ;
   ForeachOAFamily(oaf, doFillInOAFamily) ;
   RETURN( sym )
END MakeSet ;


(*
   PutSet - places SimpleType as the type for set, Sym.
*)

PROCEDURE PutSet (Sym: CARDINAL; SimpleType: CARDINAL; packed: BOOLEAN) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      SetSym: WITH Set DO
      	         Type := SimpleType ;  (* Index to a subrange symbol  *)
      	       	     	      	       (* or an enumeration type.     *)
                 ispacked := packed
              END
      ELSE
         InternalError ('expecting a Set symbol')
      END
   END
END PutSet ;


(*
   IsSet - returns TRUE if Sym is a set symbol.
*)

PROCEDURE IsSet (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=SetSym )
END IsSet ;


(*
   IsSetPacked - returns TRUE if Sym is packed.
*)

PROCEDURE IsSetPacked (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal (Sym) ;
   pSym := GetPsym (Sym) ;
   RETURN (pSym^.SymbolType=SetSym) AND pSym^.Set.ispacked
END IsSetPacked ;


(*
   ForeachParameterDo -
*)

PROCEDURE ForeachParameterDo (p: CheckProcedure) ;
VAR
   l, h: CARDINAL ;
BEGIN
   l := LowIndice(Symbols) ;
   h := HighIndice(Symbols) ;
   WHILE l<=h DO
      IF IsParameter(l)
      THEN
         p(l)
      END ;
      INC(l)
   END
END ForeachParameterDo ;


(*
   CheckUnbounded - checks to see if parameter, Sym, is now an unbounded parameter.
*)

PROCEDURE CheckUnbounded (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ParamSym   :   IF IsUnbounded(Param.Type)
                     THEN
                        Param.IsUnbounded := TRUE
                     END |
      VarParamSym:   IF IsUnbounded(VarParam.Type)
                     THEN
                        VarParam.IsUnbounded := TRUE
                     END

      ELSE
         HALT
      END
   END
END CheckUnbounded ;


(*
   IsOAFamily - returns TRUE if, Sym, is an OAFamily symbol.
*)

PROCEDURE IsOAFamily (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=OAFamilySym )
END IsOAFamily ;


(*
   MakeOAFamily - makes an OAFamily symbol based on SimpleType.
                  It returns the OAFamily symbol.  A new symbol
                  is created if one does not already exist for
                  SimpleType.
*)

PROCEDURE MakeOAFamily (SimpleType: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   sym : CARDINAL ;
BEGIN
   sym := GetOAFamily(SimpleType) ;
   IF sym=NulSym
   THEN
      NewSym(sym) ;
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         SymbolType := OAFamilySym ;
         OAFamily.MaxDimensions := 0 ;
         OAFamily.SimpleType := SimpleType ;
         OAFamily.Dimensions := Indexing.InitIndex(1)
      END ;
      PutOAFamily(SimpleType, sym)
   END ;
   RETURN( sym )
END MakeOAFamily ;


(*
   GetOAFamily - returns the oafamily symbol associated with
                 SimpleType.
*)

PROCEDURE GetOAFamily (SimpleType: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(SimpleType) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym      :  RETURN( NulSym ) |
      RecordSym     :  RETURN( Record.oafamily ) |
      SubrangeSym   :  RETURN( Subrange.oafamily ) |
      EnumerationSym:  RETURN( Enumeration.oafamily ) |
      ArraySym      :  RETURN( Array.oafamily ) |
      ProcTypeSym   :  RETURN( ProcType.oafamily ) |
      TypeSym       :  RETURN( Type.oafamily ) |
      PointerSym    :  RETURN( Pointer.oafamily ) |
      SetSym        :  RETURN( Set.oafamily ) |
      UndefinedSym  :  RETURN( Undefined.oafamily )

      ELSE
         RETURN( NulSym )
      END
   END
END GetOAFamily ;


(*
   PutOAFamily - places the, oaf, into, SimpleType, oafamily field.
*)

PROCEDURE PutOAFamily (SimpleType: CARDINAL; oaf: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(SimpleType) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym      :  |
      RecordSym     :  Record.oafamily := oaf |
      SubrangeSym   :  Subrange.oafamily := oaf |
      EnumerationSym:  Enumeration.oafamily := oaf |
      ArraySym      :  Array.oafamily := oaf |
      ProcTypeSym   :  ProcType.oafamily := oaf |
      TypeSym       :  Type.oafamily := oaf |
      PointerSym    :  Pointer.oafamily := oaf |
      SetSym        :  Set.oafamily := oaf |
      UndefinedSym  :  Undefined.oafamily := oaf

      ELSE
         InternalError ('not expecting this SimpleType')
      END
   END
END PutOAFamily ;


(*
   ForeachOAFamily - call, p[oaf, ndim, symbol] for every unbounded symbol,
                     sym, in the oaf.
*)

PROCEDURE ForeachOAFamily (sym: CARDINAL; p: FamilyOperation) ;
VAR
   pSym: PtrToSymbol ;
   h, i: CARDINAL ;
   pc  : POINTER TO CARDINAL ;
BEGIN
   IF sym#NulSym
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         OAFamilySym:  h := Indexing.HighIndice(OAFamily.Dimensions) ;
                       i := 1 ;
                       WHILE i<=h DO
                          pc := Indexing.GetIndice(OAFamily.Dimensions, i) ;
                          IF pc#NIL
                          THEN
                             p(sym, i, pc^)
                          END ;
                          INC(i)
                       END

         ELSE
            InternalError ('expecting OAFamily symbol')
         END
      END
   END
END ForeachOAFamily ;


(*
   doFillInOAFamily -
*)

PROCEDURE doFillInOAFamily (oaf: CARDINAL; i: CARDINAL; unbounded: CARDINAL) ;
VAR
   SimpleType: CARDINAL ;
BEGIN
   SimpleType := GetType(oaf) ;
   IF unbounded#NulSym
   THEN
      FillInUnboundedFields(GetTokenNo(), unbounded, SimpleType, i)
   END
END doFillInOAFamily ;


(*
   FillInUnboundedFields -
*)

PROCEDURE FillInUnboundedFields (tok: CARDINAL;
                                 sym: CARDINAL; SimpleType: CARDINAL; ndim: CARDINAL) ;
VAR
   pSym    : PtrToSymbol ;
   field   : CARDINAL ;
   Contents: CARDINAL ;
   i       : CARDINAL ;
BEGIN
   IF sym#NulSym
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         SymbolType := UnboundedSym ;
         WITH Unbounded DO
            Type := SimpleType ;            (* Index to a simple type.     *)
            Size := InitValue() ;           (* Size in bytes for this sym  *)
            Scope := GetScope(SimpleType) ; (* Which scope will create it  *)
            InitWhereDeclaredTok(tok, At) ; (* Declared here               *)
            NewSym(RecordType) ;
            FillInRecordFields(tok, RecordType, NulName, GetScope(SimpleType), NulSym) ;
            NewSym(Contents) ;
            FillInPointerFields(Contents, NulName, GetScope(SimpleType), NulSym) ;
            PutPointer(Contents, SimpleType) ;
	    (* create the contents field for the unbounded array.  *)
            field := PutFieldRecord(RecordType,
                                    MakeKey(UnboundedAddressName),
                                    Contents, NulSym) ;
	    (* create all the high fields for the unbounded array.  *)
            i := 1 ;
            WHILE i<=ndim DO
               field := PutFieldRecord(RecordType,
                                       makekey(string(Mark(Sprintf1(Mark(InitString(UnboundedHighName)), i)))),
                                       Cardinal, NulSym) ;
               INC(i)
            END ;
            Dimensions := ndim
         END
      END ;
      ForeachParameterDo(CheckUnbounded)
   END
END FillInUnboundedFields ;


(*
   MakeUnbounded - makes an unbounded array Symbol.
                   ndim is the number of dimensions required.
                   No name is required.
*)

PROCEDURE MakeUnbounded (tok: CARDINAL;
                         SimpleType: CARDINAL; ndim: CARDINAL) : CARDINAL ;
VAR
   sym, oaf: CARDINAL ;
BEGIN
   oaf := MakeOAFamily(SimpleType) ;
   sym := GetUnbounded(oaf, ndim) ;
   IF sym=NulSym
   THEN
      NewSym(sym) ;
      IF IsUnknown (SimpleType)
      THEN
         PutPartialUnbounded(sym, SimpleType, ndim)
      ELSE
         FillInUnboundedFields(tok, sym, SimpleType, ndim)
      END ;
      PutUnbounded(oaf, sym, ndim)
   END ;
   RETURN( sym )
END MakeUnbounded ;


(*
   GetUnbounded - returns the unbounded symbol associated with
                  the OAFamily symbol, oaf, and the number of
                  dimensions, ndim, of the open array.
*)

PROCEDURE GetUnbounded (oaf: CARDINAL; ndim: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(oaf) ;
   WITH pSym^ DO
      CASE SymbolType OF

      OAFamilySym:  WITH OAFamily DO
                       IF ndim>MaxDimensions
                       THEN
                          RETURN( NulSym )
                       ELSE
                          RETURN( GetFromIndex(Dimensions, ndim) )
                       END
                    END

      ELSE
         InternalError ('expecting OAFamily symbol')
      END
   END
END GetUnbounded ;


(*
   PutUnbounded - associates the unbounded symbol, open, with
                  SimpleType.
*)

PROCEDURE PutUnbounded (oaf: CARDINAL; sym: CARDINAL; ndim: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(oaf) ;
   WITH pSym^ DO
      CASE SymbolType OF

      OAFamilySym:  WITH OAFamily DO
                       (* need to check to see if we need to add NulSym for all dimensions < ndim
                          which have not been used.  *)
                       WHILE MaxDimensions<ndim DO
                          INC(MaxDimensions) ;
			  IF MaxDimensions<ndim
			  THEN
			     (* add NulSym to an unused dimension.  *)
                             PutIntoIndex(Dimensions, MaxDimensions, NulSym)
			  END
                       END ;
                       (* and finally add the known sym.  *)
                       PutIntoIndex(Dimensions, ndim, sym)
                    END

      ELSE
         InternalError ('expecting OAFamily symbol')
      END
   END
END PutUnbounded ;


(*
   GetUnboundedRecordType - returns the record type used to
                            implement the unbounded array.
*)

PROCEDURE GetUnboundedRecordType (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      UnboundedSym: RETURN( Unbounded.RecordType )

      ELSE
         InternalError ('expecting an UnBounded symbol')
      END
   END
END GetUnboundedRecordType ;


(*
   GetUnboundedAddressOffset - returns the offset of the address field
                               inside the record used to implement the
                               unbounded type.
*)

PROCEDURE GetUnboundedAddressOffset (sym: CARDINAL) : CARDINAL ;
VAR
   field,
   rec  : CARDINAL ;
BEGIN
   rec := GetUnboundedRecordType(sym) ;
   IF rec=NulSym
   THEN
      InternalError ('expecting record type to be declared')
   ELSE
      field := GetLocalSym(rec, MakeKey(UnboundedAddressName)) ;
      IF field=NulSym
      THEN
         InternalError ('expecting address field to be present inside unbounded record')
      ELSE
         RETURN( field )
      END
   END
END GetUnboundedAddressOffset ;


(*
   GetUnboundedHighOffset - returns the offset of the high field
                            inside the record used to implement the
                            unbounded type.
*)

PROCEDURE GetUnboundedHighOffset (sym: CARDINAL; ndim: CARDINAL) : CARDINAL ;
VAR
   rec: CARDINAL ;
BEGIN
   rec := GetUnboundedRecordType(sym) ;
   IF rec=NulSym
   THEN
      InternalError ('expecting record type to be declared')
   ELSE
      RETURN GetLocalSym(rec,
                         makekey(string(Mark(Sprintf1(Mark(InitString(UnboundedHighName)),
                                                      ndim)))))
   END
END GetUnboundedHighOffset ;


(*
   GetArrayDimension - returns the number of dimensions defined.
*)

PROCEDURE GetArrayDimension (sym: CARDINAL) : CARDINAL ;
VAR
   n: CARDINAL ;
BEGIN
   n := 0 ;
   WHILE IsArray(sym) DO
      sym := SkipType(GetType(sym)) ;
      INC(n)
   END ;
   RETURN( n )
END GetArrayDimension ;


(*
   GetDimension - return the number of dimensions associated with
                  this unbounded ARRAY parameter.
*)

PROCEDURE GetDimension (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      PartialUnboundedSym:  RETURN( PartialUnbounded.NDim ) |
      UnboundedSym       :  RETURN( Unbounded.Dimensions ) |
      OAFamilySym        :  RETURN( OAFamily.MaxDimensions ) |
      ParamSym           :  IF Param.IsUnbounded
                            THEN
                               RETURN( GetDimension(GetType(sym)) )
                            ELSE
                               InternalError ('expecting unbounded paramater')
                            END |
      VarParamSym        :  IF VarParam.IsUnbounded
                            THEN
                               RETURN( GetDimension(GetType(sym)) )
                            ELSE
                               InternalError ('expecting unbounded paramater')
                            END |
      ArraySym           :  RETURN( GetArrayDimension(sym) ) |
      TypeSym            :  RETURN( GetDimension(GetType(sym)) ) |
      VarSym             :  RETURN( GetDimension(GetType(sym)) )

      ELSE
         InternalError ('expecting PartialUnbounded')
      END
   END
END GetDimension ;


(*
   PutArray - places a type symbol into an Array.
*)

PROCEDURE PutArray (Sym, TypeSymbol: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      ArraySym: WITH Array DO
                   Type := TypeSymbol (* The Array Type. ARRAY OF Type.      *)
                END
      ELSE
         InternalError ('expecting an Array symbol')
      END
   END
END PutArray ;


(*
   ResolveConstructorType - if, sym, has an unresolved constructor type
                            then attempt to resolve it by examining the
                            from, type.
*)

PROCEDURE ResolveConstructorType (sym: CARDINAL;
                                  VAR type: CARDINAL;
                                  VAR from: CARDINAL;
                                  VAR unres: BOOLEAN) ;
BEGIN
   IF unres
   THEN
      IF IsConstructor(from)
      THEN
         IF IsConstructorResolved(from)
         THEN
            unres := FALSE ;
            type := GetType(from) ;
            IF (type#NulSym) AND IsSet(SkipType(type))
            THEN
               PutConstSet(sym)
            END
         END
      ELSIF (from#NulSym) AND IsSet(SkipType(from))
      THEN
         unres := FALSE ;
         type := from ;
         PutConstSet(sym)
      ELSIF (from#NulSym) AND (IsRecord(SkipType(from)) OR IsArray(SkipType(from)))
      THEN
         unres := FALSE ;
         type := from
      END
   END
END ResolveConstructorType ;


(*
   IsConstructorResolved - returns TRUE if the constructor does not
                           have an unresolved type.
*)

PROCEDURE IsConstructorResolved (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstVarSym:  RETURN( NOT ConstVar.UnresFromType ) |
      ConstLitSym:  RETURN( NOT ConstLit.UnresFromType )

      ELSE
         InternalError ('expecting ConstVar or ConstLit symbol')
      END
   END
END IsConstructorResolved ;


(*
   CanResolveConstructor - returns TRUE if the type of the constructor,
                           sym, is known.
*)

PROCEDURE CanResolveConstructor (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF NOT IsConstructorResolved(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ConstVarSym:  WITH ConstVar DO
                          ResolveConstructorType(sym, Type, FromType, UnresFromType)
                       END |
         ConstLitSym:  WITH ConstLit DO
                          ResolveConstructorType(sym, Type, FromType, UnresFromType)
                       END |

         ELSE
            InternalError ('expecting ConstVar or ConstLit symbol')
         END
      END
   END ;
   RETURN( IsConstructorResolved(sym) )
END CanResolveConstructor ;


(*
   CheckAllConstructorsResolved - checks to see that the
                                  UnresolvedConstructorType list is
                                  empty and if it is not then it
                                  generates error messages.
*)

PROCEDURE CheckAllConstructorsResolved ;
VAR
   i, n, s: CARDINAL ;
   e      : Error ;
BEGIN
   n := NoOfItemsInList(UnresolvedConstructorType) ;
   IF n>0
   THEN
      FOR i := 1 TO n DO
         s := GetItemFromList(UnresolvedConstructorType, i) ;
         e := NewError(GetDeclaredMod(s)) ;
         ErrorFormat0(e, 'constructor has an unknown type')
      END ;
      FlushErrors
   END
END CheckAllConstructorsResolved ;


(*
   ResolveConstructorTypes - to be called at the end of pass three.  Its
                             purpose is to fix up all constructors whose
                             types are unknown.
*)

PROCEDURE ResolveConstructorTypes ;
VAR
   finished: BOOLEAN ;
   i, n, s : CARDINAL ;
BEGIN
   REPEAT
      n := NoOfItemsInList(UnresolvedConstructorType) ;
      finished := TRUE ;
      i := 1 ;
      WHILE i<=n DO
         s := GetItemFromList(UnresolvedConstructorType, i) ;
         Assert(IsConstructor(s)) ;
         IF CanResolveConstructor(s)
         THEN
            finished := FALSE ;
            RemoveItemFromList(UnresolvedConstructorType, s) ;
            i := n
         END ;
         INC(i)
      END
   UNTIL finished ;
   CheckAllConstructorsResolved
END ResolveConstructorTypes ;


(*
   SanityCheckParameters -
*)

PROCEDURE SanityCheckParameters (sym: CARDINAL) ;
VAR
   p   : CARDINAL ;
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := NoOfParam(sym) ;
   WHILE i<=n DO
      p := GetType(GetParam(sym, i)) ;
      IF IsConst(p)
      THEN
         MetaError3('the {%1N} formal parameter in procedure {%2Dad} should have a type rather than a constant {%3Dad}',
                    i, sym, p)
      END ;
      INC(i)
   END
END SanityCheckParameters ;


(*
   SanityCheckArray - checks to see that an array has a correct subrange type.
*)

PROCEDURE SanityCheckArray (sym: CARDINAL) ;
VAR
   type     : CARDINAL ;
   subscript: CARDINAL ;
BEGIN
   IF IsArray(sym)
   THEN
      subscript := GetArraySubscript(sym) ;
      IF subscript#NulSym
      THEN
         type := SkipType(GetType(subscript)) ;
         IF IsAModula2Type(type)
         THEN
            (* ok all is good *)
         ELSE
            MetaError2('the array {%1Dad} must be declared with a simpletype in the [..] component rather than a {%2d}',
                       sym, type)
         END
      END
   END
END SanityCheckArray ;


(*
   ForeachSymbolDo - foreach symbol, call, P(sym).
*)

PROCEDURE ForeachSymbolDo (P: PerformOperation) ;
VAR
   i, n: CARDINAL ;
BEGIN
   i := Indexing.LowIndice(Symbols) ;
   n := Indexing.HighIndice(Symbols) ;
   WHILE i<=n DO
      P(i) ;
      INC(i)
   END
END ForeachSymbolDo ;


(*
   SanityCheckProcedure - check to see that procedure parameters do not use constants
                          instead of types in their formal parameter section.
*)

PROCEDURE SanityCheckProcedure (sym: CARDINAL) ;
BEGIN
   SanityCheckParameters(sym)
END SanityCheckProcedure ;


(*
   SanityCheckModule -
*)

PROCEDURE SanityCheckModule (sym: CARDINAL) ;
BEGIN
   ForeachInnerModuleDo(sym, SanityCheckModule) ;
   ForeachProcedureDo(sym, SanityCheckProcedure) ;
   ForeachLocalSymDo(sym, SanityCheckArray)
END SanityCheckModule ;


(*
   SanityCheckConstants - must only be called once all constants, types, procedures
                          have been declared.  It checks to see that constants are
                          not used as procedure parameter types.
*)

PROCEDURE SanityCheckConstants ;
BEGIN
   ForeachModuleDo(SanityCheckModule) ;
   ForeachSymbolDo(SanityCheckArray)
END SanityCheckConstants ;


(*
   AddNameTo - adds Name, n, to tree, s.
*)

PROCEDURE AddNameTo (s: SymbolTree; o: CARDINAL) ;
BEGIN
   IF GetSymKey(s, GetSymName(o))=NulKey
   THEN
      PutSymKey(s, GetSymName(o), o)
   END
END AddNameTo ;


(*
   AddNameToScope - adds a Name, n, to the list of objects declared at the
                    current scope.
*)

PROCEDURE AddNameToScope (n: Name) ;
VAR
   pSym : PtrToSymbol ;
   scope: CARDINAL ;
BEGIN
   scope := GetCurrentScope() ;
   pSym := GetPsym(scope) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym:  AddNameTo(Procedure.NamedObjects, MakeObject(n)) |
      ModuleSym   :  AddNameTo(Module.NamedObjects, MakeObject(n)) |
      DefImpSym   :  AddNameTo(DefImp.NamedObjects, MakeObject(n))

      ELSE
         InternalError ('expecting - DefImp')
      END
   END
END AddNameToScope ;


(*
   AddNameToImportList - adds a Name, n, to the import list of the current
                         module.
*)

PROCEDURE AddNameToImportList (n: Name) ;
VAR
   pSym : PtrToSymbol ;
   scope: CARDINAL ;
BEGIN
   scope := GetCurrentScope() ;
   pSym := GetPsym(scope) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym:  AddNameTo(Module.NamedImports, MakeObject(n)) |
      DefImpSym:  AddNameTo(DefImp.NamedImports, MakeObject(n))

      ELSE
         InternalError ('expecting - DefImp or Module symbol')
      END
   END
END AddNameToImportList ;


VAR
   ResolveModule: CARDINAL ;


(*
   CollectSymbolFrom -
*)

PROCEDURE CollectSymbolFrom (tok: CARDINAL; scope: CARDINAL; n: Name) : CARDINAL ;
VAR
   n1 : Name ;
   sym: CARDINAL ;
BEGIN
   n1 := GetSymName (scope) ;
   IF DebugUnknowns
   THEN
      printf2('declaring %a in %a', n, n1)
   END ;
   sym := CheckScopeForSym (scope, n) ;
   IF sym=NulSym
   THEN
      sym := FetchUnknownFrom (tok, scope, n)
   END ;
   IF DebugUnknowns
   THEN
      printf1(' symbol created (%d)\n', sym)
   END ;
   RETURN( sym )
END CollectSymbolFrom ;


(*
   CollectUnknown -
*)

PROCEDURE CollectUnknown (tok: CARDINAL; sym: CARDINAL; n: Name) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   s   : CARDINAL ;
BEGIN
   IF IsModule (sym) OR IsDefImp (sym)
   THEN
      RETURN( CollectSymbolFrom (tok, sym, n) )
   ELSIF IsProcedure(sym)
   THEN
      s := CheckScopeForSym (sym, n) ;
      IF s=NulSym
      THEN
         pSym := GetPsym (sym) ;
         WITH pSym^ DO
            CASE SymbolType OF

            ProcedureSym:  IF GetSymKey (Procedure.NamedObjects, n) # NulKey
                           THEN
                              RETURN( CollectSymbolFrom (tok, sym, n) )
                           END

            ELSE
               InternalError ('expecting - Procedure symbol')
            END
         END ;
         s := CollectUnknown (tok, GetScope (sym), n)
      END ;
      RETURN( s )
   END
END CollectUnknown ;


(*
   ResolveImport -
*)

PROCEDURE ResolveImport (o: WORD) ;
VAR
   n1, n2: Name ;
   tok   : CARDINAL ;
   sym   : CARDINAL ;
BEGIN
   IF DebugUnknowns
   THEN
      n1 := GetSymName(o) ;
      printf1('attempting to find out where %a was declared\n', n1) ;
      n1 := GetSymName(ResolveModule) ;
      n2 := GetSymName(GetScope(ResolveModule)) ;
      printf2('scope of module %a is %a\n', n1, n2)
   END ;
   tok := GetFirstUsed (o) ;
   sym := CollectUnknown (tok, GetScope(ResolveModule), GetSymName(o)) ;
   IF sym=NulSym
   THEN
      MetaError2('unknown symbol {%1Uad} found in import list of module {%2a}',
                 o, ResolveModule)
   ELSE
      AddSymToModuleScope(ResolveModule, sym)
   END
END ResolveImport ;


(*
   ResolveRelativeImport -
*)

PROCEDURE ResolveRelativeImport (sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF IsModule(sym)
   THEN
      ResolveModule := sym ;
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ModuleSym:  ForeachNodeDo(Module.NamedImports,
                                   ResolveImport)

         ELSE
            InternalError ('expecting - Module symbol')
         END
      END
   END ;
   ForeachProcedureDo(sym, ResolveRelativeImport) ;
   ForeachInnerModuleDo(sym, ResolveRelativeImport)
END ResolveRelativeImport ;


(*
   ResolveImports - it examines the import list of all inner modules
                    and resolves all relative imports.
*)

PROCEDURE ResolveImports ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetCurrentScope() ;
   IF DebugUnknowns
   THEN
      DisplayTrees(scope)
   END ;
   ForeachProcedureDo(scope, ResolveRelativeImport) ;
   ForeachInnerModuleDo(scope, ResolveRelativeImport)
END ResolveImports ;


(*
   GetScope - returns the declaration scope of the symbol.
*)

PROCEDURE GetScope (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym           : ErrorAbort0('') |
      DefImpSym          : RETURN( NulSym ) |
      ModuleSym          : RETURN( Module.Scope ) |
      VarSym             : RETURN( Var.Scope ) |
      ProcedureSym       : RETURN( Procedure.Scope ) |
      ProcTypeSym        : RETURN( ProcType.Scope ) |
      RecordFieldSym     : RETURN( RecordField.Scope ) |
      VarientSym         : RETURN( Varient.Scope ) |
      VarientFieldSym    : RETURN( VarientField.Scope ) |
      EnumerationSym     : RETURN( Enumeration.Scope ) |
      EnumerationFieldSym: RETURN( EnumerationField.Scope ) |
      SubrangeSym        : RETURN( Subrange.Scope ) |
      ArraySym           : RETURN( Array.Scope ) |
      TypeSym            : RETURN( Type.Scope ) |
      PointerSym         : RETURN( Pointer.Scope ) |
      RecordSym          : RETURN( Record.Scope ) |
      SetSym             : RETURN( Set.Scope ) |
      UnboundedSym       : RETURN( Unbounded.Scope ) |
      PartialUnboundedSym: InternalError ('should not be requesting the scope of a PartialUnbounded symbol')

      ELSE
         InternalError ('not implemented yet')
      END
   END
END GetScope ;


(*
   GetModuleScope - returns the module scope of symbol, sym.
                    If sym was declared within a nested procedure
                    then return the module which defines the
                    procedure.
*)

PROCEDURE GetModuleScope (sym: CARDINAL) : CARDINAL ;
VAR
   mod: CARDINAL ;
BEGIN
   mod := GetScope(sym) ;
   WHILE (mod#NulSym) AND (NOT IsDefImp(mod)) AND (NOT IsModule(mod)) DO
      mod := GetScope(mod)
   END ;
   RETURN( mod )
END GetModuleScope ;


(*
   GetProcedureScope - returns the innermost procedure (if any)
                       in which the symbol, sym, resides.
                       A module inside the procedure is skipped
                       over.
*)

PROCEDURE GetProcedureScope (sym: CARDINAL) : CARDINAL ;
BEGIN
   WHILE (sym#NulSym) AND (NOT IsProcedure(sym)) DO
      sym := GetScope(sym)
   END ;
   IF (sym#NulSym) AND IsProcedure(sym)
   THEN
      RETURN( sym )
   ELSE
      RETURN( NulSym )
   END
END GetProcedureScope ;


(*
   IsModuleWithinProcedure - returns TRUE if module, sym, is
                             inside a procedure.
*)

PROCEDURE IsModuleWithinProcedure (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( GetProcedureScope(sym)#NulSym )
END IsModuleWithinProcedure ;


(*
   GetParent - returns the parent of symbol, Sym.
*)

PROCEDURE GetParent (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym           : ErrorAbort0('') |
      VarientSym         : RETURN( Varient.Parent ) |
      VarientFieldSym    : RETURN( VarientField.Parent ) |
      RecordFieldSym     : RETURN( RecordField.Parent ) |
      EnumerationFieldSym: RETURN( EnumerationField.Type )

      ELSE
         InternalError ('not implemented yet')
      END
   END
END GetParent ;


(*
   IsRecordField - returns true if Sym is a record field.
*)

PROCEDURE IsRecordField (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=RecordFieldSym )
END IsRecordField ;


(*
   MakeProcType - returns a procedure type symbol with ProcTypeName.
*)

PROCEDURE MakeProcType (tok: CARDINAL; ProcTypeName: Name) : CARDINAL ;
VAR
   pSym    : PtrToSymbol ;
   oaf, sym: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare (tok, ProcTypeName, oaf) ;
   IF NOT IsError(sym)
   THEN
      pSym := GetPsym(sym) ;
      WITH pSym^ DO
         SymbolType := ProcTypeSym ;
         CASE SymbolType OF

         ProcTypeSym: ProcType.ReturnType := NulSym ;
                      ProcType.name := ProcTypeName ;
                      InitList(ProcType.ListOfParam) ;
                      ProcType.HasVarArgs := FALSE ;     (* Does this proc type use ... ? *)
                      ProcType.HasOptArg  := FALSE ;     (* Does this proc type use [ ] ? *)
                      ProcType.OptArgInit := NulSym ;    (* The optarg initial value.     *)
                      ProcType.ReturnOptional := FALSE ; (* Is the return value optional? *)
                      ProcType.Scope := GetCurrentScope() ;
                                                         (* scope of procedure.           *)
                      ProcType.Size := InitValue() ;
                      ProcType.TotalParamSize := InitValue() ;  (* size of all parameters *)
                      ProcType.oafamily := oaf ;         (* The oa family for this symbol *)
                      InitWhereDeclaredTok(tok, ProcType.At)     (* Declared here *)

         ELSE
            InternalError ('expecting ProcType symbol')
         END
      END
   END ;
   ForeachOAFamily(oaf, doFillInOAFamily) ;
   RETURN( sym )
END MakeProcType ;


(*
   PutProcTypeParam - Places a Non VAR parameter ParamName with type
                      ParamType into ProcType Sym.
*)

PROCEDURE PutProcTypeParam (Sym: CARDINAL;
                            ParamType: CARDINAL; isUnbounded: BOOLEAN) ;
VAR
   pSym  : PtrToSymbol ;
   ParSym: CARDINAL ;
BEGIN
   NewSym(ParSym) ;
   pSym := GetPsym(ParSym) ;
   WITH pSym^ DO
      SymbolType := ParamSym ;
      WITH Param DO
         name := NulName ;
         Type := ParamType ;
         IsUnbounded := isUnbounded ;
         ShadowVar := NulSym ;
         InitWhereDeclared(At)
      END
   END ;
   AddParameter(Sym, ParSym)
END PutProcTypeParam ;


(*
   PutProcTypeVarParam - Places a Non VAR parameter ParamName with type
                         ParamType into ProcType Sym.
*)

PROCEDURE PutProcTypeVarParam (Sym: CARDINAL;
                               ParamType: CARDINAL; isUnbounded: BOOLEAN) ;
VAR
   pSym  : PtrToSymbol ;
   ParSym: CARDINAL ;
BEGIN
   NewSym(ParSym) ;
   pSym := GetPsym(ParSym) ;
   WITH pSym^ DO
      SymbolType := VarParamSym ;
      WITH Param DO
         name := NulName ;
         Type := ParamType ;
         IsUnbounded := isUnbounded ;
         ShadowVar := NulSym ;
         InitWhereDeclared(At)
      END
   END ;
   AddParameter(Sym, ParSym)
END PutProcTypeVarParam ;


(*
   PutProcedureReachable - Sets the procedure, Sym, to be reachable by the
                           main Module.
*)

PROCEDURE PutProcedureReachable (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: Procedure.Reachable := TRUE

      ELSE
         InternalError ('expecting Procedure symbol')
      END
   END
END PutProcedureReachable ;


(*
   PutModuleStartQuad - Places QuadNumber into the Module symbol, Sym.
                        QuadNumber is the start quad of Module,
                        Sym.
*)

PROCEDURE PutModuleStartQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: Module.StartQuad := QuadNumber |
      DefImpSym: DefImp.StartQuad := QuadNumber

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END PutModuleStartQuad ;


(*
   PutModuleEndQuad - Places QuadNumber into the Module symbol, Sym.
                      QuadNumber is the end quad of Module,
                      Sym.
*)

PROCEDURE PutModuleEndQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: Module.EndQuad := QuadNumber |
      DefImpSym: DefImp.EndQuad := QuadNumber

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END PutModuleEndQuad ;


(*
   PutModuleFinallyStartQuad - Places QuadNumber into the Module symbol, Sym.
                               QuadNumber is the finally start quad of
                               Module, Sym.
*)

PROCEDURE PutModuleFinallyStartQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: Module.StartFinishQuad := QuadNumber |
      DefImpSym: DefImp.StartFinishQuad := QuadNumber

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END PutModuleFinallyStartQuad ;


(*
   PutModuleFinallyEndQuad - Places QuadNumber into the Module symbol, Sym.
                             QuadNumber is the end quad of the finally block
                             in Module, Sym.
*)

PROCEDURE PutModuleFinallyEndQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: Module.EndFinishQuad := QuadNumber |
      DefImpSym: DefImp.EndFinishQuad := QuadNumber

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END PutModuleFinallyEndQuad ;


(*
   GetModuleQuads - Returns, StartInit EndInit StartFinish EndFinish,
                    Quads of a Module, Sym.
                    Start and End represent the initialization code
                    of the Module, Sym.
*)

PROCEDURE GetModuleQuads (Sym: CARDINAL;
                          VAR StartInit, EndInit,
                          StartFinish, EndFinish: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: WITH Module DO
                    StartInit := StartQuad ;
                    EndInit := EndQuad ;
                    StartFinish := StartFinishQuad ;
                    EndFinish := EndFinishQuad
                 END |
      DefImpSym: WITH DefImp DO
                    StartInit := StartQuad ;
                    EndInit := EndQuad ;
                    StartFinish := StartFinishQuad ;
                    EndFinish := EndFinishQuad
                 END

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END GetModuleQuads ;


(*
   PutModuleFinallyFunction - Places Tree, finally, into the Module symbol, Sym.
*)

PROCEDURE PutModuleFinallyFunction (Sym: CARDINAL; finally: Tree) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: Module.FinallyFunction := finally |
      DefImpSym: DefImp.FinallyFunction := finally

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END PutModuleFinallyFunction ;


(*
   GetModuleFinallyFunction - returns the finally tree from the Module symbol, Sym.
*)

PROCEDURE GetModuleFinallyFunction (Sym: CARDINAL) : Tree ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ModuleSym: RETURN( Module.FinallyFunction) |
      DefImpSym: RETURN( DefImp.FinallyFunction)

      ELSE
         InternalError ('expecting a Module or DefImp symbol')
      END
   END
END GetModuleFinallyFunction ;


(*
   PutProcedureScopeQuad - Places QuadNumber into the Procedure symbol, Sym.
                           QuadNumber is the start quad of scope for procedure,
                           Sym.
*)

PROCEDURE PutProcedureScopeQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.ScopeQuad := QuadNumber

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END PutProcedureScopeQuad ;


(*
   PutProcedureStartQuad - Places QuadNumber into the Procedure symbol, Sym.
                           QuadNumber is the start quad of procedure,
                           Sym.
*)

PROCEDURE PutProcedureStartQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.StartQuad := QuadNumber

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END PutProcedureStartQuad ;


(*
   PutProcedureEndQuad - Places QuadNumber into the Procedure symbol, Sym.
                         QuadNumber is the end quad of procedure,
                         Sym.
*)

PROCEDURE PutProcedureEndQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.EndQuad := QuadNumber

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END PutProcedureEndQuad ;


(*
   GetProcedureQuads - Returns, Start and End, Quads of a procedure, Sym.
*)

PROCEDURE GetProcedureQuads (Sym: CARDINAL; VAR scope, start, end: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: WITH Procedure DO
                       scope := ScopeQuad ;
                       start := StartQuad ;
                       end := EndQuad
                    END

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END GetProcedureQuads ;


(*
   GetReadQuads - assigns Start and End to the beginning and end of
                  symbol, Sym, read history usage.
*)

PROCEDURE GetReadQuads (Sym: CARDINAL; m: ModeOfAddr;
                        VAR Start, End: CARDINAL) ;
BEGIN
   GetReadLimitQuads(Sym, m, 0, 0, Start, End)
END GetReadQuads ;


(*
   GetWriteQuads - assigns Start and End to the beginning and end of
                   symbol, Sym, usage.
*)

PROCEDURE GetWriteQuads (Sym: CARDINAL; m: ModeOfAddr;
                         VAR Start, End: CARDINAL) ;
BEGIN
   GetWriteLimitQuads(Sym, m, 0, 0, Start, End)
END GetWriteQuads ;


(*
   PutProcedureBegin - assigns begin as the token number matching the
                       procedure BEGIN.
*)

PROCEDURE PutProcedureBegin (Sym: CARDINAL; begin: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.Begin := begin

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END PutProcedureBegin ;


(*
   PutProcedureEnd - assigns end as the token number matching the
                     procedure END.
*)

PROCEDURE PutProcedureEnd (Sym: CARDINAL; end: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: Procedure.End := end

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END PutProcedureEnd ;


(*
   GetProcedureBeginEnd - assigns, begin, end, to the stored token values.
*)

PROCEDURE GetProcedureBeginEnd (Sym: CARDINAL; VAR begin, end: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: begin := Procedure.Begin ;
                    end := Procedure.End

      ELSE
         InternalError ('expecting a Procedure symbol')
      END
   END
END GetProcedureBeginEnd ;


(*
   Max -
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Min -
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   GetQuads - assigns Start and End to the beginning and end of
              symbol, Sym, usage.
*)

PROCEDURE GetQuads (Sym: CARDINAL; m: ModeOfAddr; VAR Start, End: CARDINAL) ;
VAR
   StartRead, EndRead,
   StartWrite, EndWrite: CARDINAL ;
BEGIN
   GetReadQuads(Sym, m, StartRead, EndRead) ;
   GetWriteQuads(Sym, m, StartWrite, EndWrite) ;
   IF StartRead=0
   THEN
      Start := StartWrite
   ELSIF StartWrite=0
   THEN
      Start := StartRead
   ELSE
      Start := Min(StartRead, StartWrite)
   END ;
   IF EndRead=0
   THEN
      End := EndWrite
   ELSIF EndWrite=0
   THEN
      End := EndRead
   ELSE
      End := Max(EndRead, EndWrite)
   END
END GetQuads ;


(*
   PutReadQuad - places Quad into the list of symbol usage.
*)

PROCEDURE PutReadQuad (Sym: CARDINAL; m: ModeOfAddr; Quad: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym: IncludeItemIntoList(Var.ReadUsageList[m], Quad)

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END
END PutReadQuad ;


(*
   RemoveReadQuad - places Quad into the list of symbol usage.
*)

PROCEDURE RemoveReadQuad (Sym: CARDINAL; m: ModeOfAddr; Quad: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym: RemoveItemFromList(Var.ReadUsageList[m], Quad)

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END
END RemoveReadQuad ;


(*
   PutWriteQuad - places Quad into the list of symbol usage.
*)

PROCEDURE PutWriteQuad (Sym: CARDINAL; m: ModeOfAddr; Quad: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym: IncludeItemIntoList(Var.WriteUsageList[m], Quad)

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END
END PutWriteQuad ;


(*
   RemoveWriteQuad - places Quad into the list of symbol usage.
*)

PROCEDURE RemoveWriteQuad (Sym: CARDINAL; m: ModeOfAddr; Quad: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym: RemoveItemFromList(Var.WriteUsageList[m], Quad)

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END
END RemoveWriteQuad ;


(*
   DoFindLimits - assigns, Start, and, End, to the start and end
                  limits contained in the list, l.  It ensures that
                  Start and End are within StartLimit..EndLimit.
                  If StartLimit or EndLimit are 0 then Start is
                  is set to the first value and End to the last.
*)

PROCEDURE DoFindLimits (StartLimit, EndLimit: CARDINAL;
                        VAR Start, End: CARDINAL; l: List) ;
VAR
   i, j, n: CARDINAL ;
BEGIN
   End := 0 ;
   Start := 0 ;
   i := 1 ;
   n := NoOfItemsInList(l) ;
   WHILE i<=n DO
      j := GetItemFromList(l, i) ;
      IF (j>End) AND (j>=StartLimit) AND ((j<=EndLimit) OR (EndLimit=0))
      THEN
         End := j
      END ;
      IF ((Start=0) OR (j<Start)) AND (j#0) AND (j>=StartLimit) AND
         ((j<=EndLimit) OR (EndLimit=0))
      THEN
         Start := j
      END ;
      INC(i)
   END
END DoFindLimits ;


(*
   GetReadLimitQuads - returns Start and End which have been assigned
                       the start and end of when the symbol was read
                       to within: StartLimit..EndLimit.
*)

PROCEDURE GetReadLimitQuads (Sym: CARDINAL; m: ModeOfAddr;
                             StartLimit, EndLimit: CARDINAL;
                             VAR Start, End: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym: DoFindLimits(StartLimit, EndLimit, Start, End,
                           Var.ReadUsageList[m])

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END
END GetReadLimitQuads ;


(*
   GetWriteLimitQuads - returns Start and End which have been assigned
                        the start and end of when the symbol was written
                        to within: StartLimit..EndLimit.
*)

PROCEDURE GetWriteLimitQuads (Sym: CARDINAL; m: ModeOfAddr;
                              StartLimit, EndLimit: CARDINAL;
                              VAR Start, End: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym     : DoFindLimits(StartLimit, EndLimit, Start, End,
                                Var.WriteUsageList[m])

      ELSE
         InternalError ('expecting a Var symbol')
      END
   END
END GetWriteLimitQuads ;


(*
   GetNthProcedure - Returns the Nth procedure in Module, Sym.
*)

PROCEDURE GetNthProcedure (Sym: CARDINAL; n: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym: RETURN( GetItemFromList(DefImp.ListOfProcs, n) ) |
      ModuleSym: RETURN( GetItemFromList(Module.ListOfProcs, n) )

      ELSE
         InternalError ('expecting a DefImp or Module symbol')
      END
   END
END GetNthProcedure ;


(*
   GetDeclaredDefinition - returns the token where this symbol
                           was declared in the definition module.
*)

PROCEDURE GetDeclaredDefinition (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym           : RETURN( Error.At.DefDeclared ) |
      ObjectSym          : RETURN( Object.At.DefDeclared ) |
      VarientSym         : RETURN( Varient.At.DefDeclared ) |
      RecordSym          : RETURN( Record.At.DefDeclared ) |
      SubrangeSym        : RETURN( Subrange.At.DefDeclared ) |
      EnumerationSym     : RETURN( Enumeration.At.DefDeclared ) |
      ArraySym           : RETURN( Array.At.DefDeclared ) |
      SubscriptSym       : RETURN( Subscript.At.DefDeclared ) |
      UnboundedSym       : RETURN( Unbounded.At.DefDeclared ) |
      ProcedureSym       : RETURN( Procedure.At.DefDeclared ) |
      ProcTypeSym        : RETURN( ProcType.At.DefDeclared ) |
      ParamSym           : RETURN( Param.At.DefDeclared ) |
      VarParamSym        : RETURN( VarParam.At.DefDeclared ) |
      ConstStringSym     : RETURN( ConstString.At.DefDeclared ) |
      ConstLitSym        : RETURN( ConstLit.At.DefDeclared ) |
      ConstVarSym        : RETURN( ConstVar.At.DefDeclared ) |
      VarSym             : RETURN( Var.At.DefDeclared ) |
      TypeSym            : RETURN( Type.At.DefDeclared ) |
      PointerSym         : RETURN( Pointer.At.DefDeclared ) |
      RecordFieldSym     : RETURN( RecordField.At.DefDeclared ) |
      VarientFieldSym    : RETURN( VarientField.At.DefDeclared ) |
      EnumerationFieldSym: RETURN( EnumerationField.At.DefDeclared ) |
      SetSym             : RETURN( Set.At.DefDeclared ) |
      DefImpSym          : RETURN( DefImp.At.DefDeclared ) |
      ModuleSym          : RETURN( Module.At.DefDeclared ) |
      UndefinedSym       : RETURN( GetFirstUsed(Sym) ) |
      PartialUnboundedSym: RETURN( GetDeclaredDefinition(PartialUnbounded.Type) )

      ELSE
         InternalError ('not expecting this type of symbol')
      END
   END
END GetDeclaredDefinition ;


(*
   GetDeclaredModule - returns the token where this symbol was declared
                       in an implementation or program module.
*)

PROCEDURE GetDeclaredModule (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym           : RETURN( Error.At.ModDeclared ) |
      ObjectSym          : RETURN( Object.At.ModDeclared ) |
      VarientSym         : RETURN( Varient.At.ModDeclared ) |
      RecordSym          : RETURN( Record.At.ModDeclared ) |
      SubrangeSym        : RETURN( Subrange.At.ModDeclared ) |
      EnumerationSym     : RETURN( Enumeration.At.ModDeclared ) |
      ArraySym           : RETURN( Array.At.ModDeclared ) |
      SubscriptSym       : RETURN( Subscript.At.ModDeclared ) |
      UnboundedSym       : RETURN( Unbounded.At.ModDeclared ) |
      ProcedureSym       : RETURN( Procedure.At.ModDeclared ) |
      ProcTypeSym        : RETURN( ProcType.At.ModDeclared ) |
      ParamSym           : RETURN( Param.At.ModDeclared ) |
      VarParamSym        : RETURN( VarParam.At.ModDeclared ) |
      ConstStringSym     : RETURN( ConstString.At.ModDeclared ) |
      ConstLitSym        : RETURN( ConstLit.At.ModDeclared ) |
      ConstVarSym        : RETURN( ConstVar.At.ModDeclared ) |
      VarSym             : RETURN( Var.At.ModDeclared ) |
      TypeSym            : RETURN( Type.At.ModDeclared ) |
      PointerSym         : RETURN( Pointer.At.ModDeclared ) |
      RecordFieldSym     : RETURN( RecordField.At.ModDeclared ) |
      VarientFieldSym    : RETURN( VarientField.At.ModDeclared ) |
      EnumerationFieldSym: RETURN( EnumerationField.At.ModDeclared ) |
      SetSym             : RETURN( Set.At.ModDeclared ) |
      DefImpSym          : RETURN( DefImp.At.ModDeclared ) |
      ModuleSym          : RETURN( Module.At.ModDeclared ) |
      UndefinedSym       : RETURN( GetFirstUsed(Sym) ) |
      PartialUnboundedSym: RETURN( GetDeclaredModule(PartialUnbounded.Type) )

      ELSE
         InternalError ('not expecting this type of symbol')
      END
   END
END GetDeclaredModule ;


(*
   PutDeclaredDefinition - associates the current tokenno with
                           the symbols declaration in the definition
                           module.
*)

PROCEDURE PutDeclaredDefinition (tok: CARDINAL; Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym           : Error.At.DefDeclared := tok |
      ObjectSym          : Object.At.DefDeclared := tok |
      VarientSym         : Varient.At.DefDeclared := tok |
      RecordSym          : Record.At.DefDeclared := tok |
      SubrangeSym        : Subrange.At.DefDeclared := tok |
      EnumerationSym     : Enumeration.At.DefDeclared := tok |
      ArraySym           : Array.At.DefDeclared := tok |
      SubscriptSym       : Subscript.At.DefDeclared := tok |
      UnboundedSym       : Unbounded.At.DefDeclared := tok |
      ProcedureSym       : Procedure.At.DefDeclared := tok |
      ProcTypeSym        : ProcType.At.DefDeclared := tok |
      ParamSym           : Param.At.DefDeclared := tok |
      VarParamSym        : VarParam.At.DefDeclared := tok |
      ConstStringSym     : ConstString.At.DefDeclared := tok |
      ConstLitSym        : ConstLit.At.DefDeclared := tok |
      ConstVarSym        : ConstVar.At.DefDeclared := tok |
      VarSym             : Var.At.DefDeclared := tok |
      TypeSym            : Type.At.DefDeclared := tok |
      PointerSym         : Pointer.At.DefDeclared := tok |
      RecordFieldSym     : RecordField.At.DefDeclared := tok |
      VarientFieldSym    : VarientField.At.DefDeclared := tok |
      EnumerationFieldSym: EnumerationField.At.DefDeclared := tok |
      SetSym             : Set.At.DefDeclared := tok |
      DefImpSym          : DefImp.At.DefDeclared := tok |
      ModuleSym          : Module.At.DefDeclared := tok |
      UndefinedSym       : |
      PartialUnboundedSym: PutDeclaredDefinition(tok, PartialUnbounded.Type)

      ELSE
         InternalError ('not expecting this type of symbol')
      END
   END
END PutDeclaredDefinition ;


(*
   PutDeclaredModule - returns the token where this symbol was declared
                       in an implementation or program module.
*)

PROCEDURE PutDeclaredModule (tok: CARDINAL; Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym           : Error.At.ModDeclared := tok |
      ObjectSym          : Object.At.ModDeclared := tok |
      VarientSym         : Varient.At.ModDeclared := tok |
      RecordSym          : Record.At.ModDeclared := tok |
      SubrangeSym        : Subrange.At.ModDeclared := tok |
      EnumerationSym     : Enumeration.At.ModDeclared := tok |
      ArraySym           : Array.At.ModDeclared := tok |
      SubscriptSym       : Subscript.At.ModDeclared := tok |
      UnboundedSym       : Unbounded.At.ModDeclared := tok |
      ProcedureSym       : Procedure.At.ModDeclared := tok |
      ProcTypeSym        : ProcType.At.ModDeclared := tok |
      ParamSym           : Param.At.ModDeclared := tok |
      VarParamSym        : VarParam.At.ModDeclared := tok |
      ConstStringSym     : ConstString.At.ModDeclared := tok |
      ConstLitSym        : ConstLit.At.ModDeclared := tok |
      ConstVarSym        : ConstVar.At.ModDeclared := tok |
      VarSym             : Var.At.ModDeclared := tok |
      TypeSym            : Type.At.ModDeclared := tok |
      PointerSym         : Pointer.At.ModDeclared := tok |
      RecordFieldSym     : RecordField.At.ModDeclared := tok |
      VarientFieldSym    : VarientField.At.ModDeclared := tok |
      EnumerationFieldSym: EnumerationField.At.ModDeclared := tok |
      SetSym             : Set.At.ModDeclared := tok |
      DefImpSym          : DefImp.At.ModDeclared := tok |
      ModuleSym          : Module.At.ModDeclared := tok |
      UndefinedSym       : |
      PartialUnboundedSym: PutDeclaredModule(tok, PartialUnbounded.Type)

      ELSE
         InternalError ('not expecting this type of symbol')
      END
   END
END PutDeclaredModule ;


(*
   PutDeclared - adds an entry to symbol, Sym, indicating that it
                 was declared at, tok.  This routine
                 may be called twice, once for definition module
                 partial declaration and once when parsing the
                 implementation module.
*)

PROCEDURE PutDeclared (tok: CARDINAL; Sym: CARDINAL) ;
BEGIN
   IF CompilingDefinitionModule ()
   THEN
      PutDeclaredDefinition (tok, Sym)
   ELSE
      PutDeclaredModule (tok, Sym)
   END
END PutDeclared ;


(*
   GetDeclaredDef - returns the tokenno where the symbol was declared.
                    The priority of declaration is definition, implementation
                    and program module.
*)

PROCEDURE GetDeclaredDef (Sym: CARDINAL) : CARDINAL ;
VAR
   declared: CARDINAL ;
BEGIN
   declared := GetDeclaredDefinition (Sym) ;
   IF declared = UnknownTokenNo
   THEN
      RETURN GetDeclaredModule (Sym)
   END ;
   RETURN declared
END GetDeclaredDef ;


(*
   GetDeclaredMod - returns the tokenno where the symbol was declared.
                    The priority of declaration is program,
                    implementation and definition module.
*)

PROCEDURE GetDeclaredMod (Sym: CARDINAL) : CARDINAL ;
VAR
   declared: CARDINAL ;
BEGIN
   declared := GetDeclaredModule (Sym) ;
   IF declared = UnknownTokenNo
   THEN
      RETURN GetDeclaredDefinition (Sym)
   END ;
   RETURN declared
END GetDeclaredMod ;


(*
   GetFirstUsed - returns the token where this symbol was first used.
*)

PROCEDURE GetFirstUsed (Sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ErrorSym           : RETURN( Error.At.FirstUsed ) |
      ObjectSym          : RETURN( Object.At.FirstUsed ) |
      UndefinedSym       : RETURN( Undefined.At.FirstUsed ) |
      VarientSym         : RETURN( Varient.At.FirstUsed ) |
      RecordSym          : RETURN( Record.At.FirstUsed ) |
      SubrangeSym        : RETURN( Subrange.At.FirstUsed ) |
      EnumerationSym     : RETURN( Enumeration.At.FirstUsed ) |
      ArraySym           : RETURN( Array.At.FirstUsed ) |
      SubscriptSym       : RETURN( Subscript.At.FirstUsed ) |
      UnboundedSym       : RETURN( Unbounded.At.FirstUsed ) |
      ProcedureSym       : RETURN( Procedure.At.FirstUsed ) |
      ProcTypeSym        : RETURN( ProcType.At.FirstUsed ) |
      ParamSym           : RETURN( Param.At.FirstUsed ) |
      VarParamSym        : RETURN( VarParam.At.FirstUsed ) |
      ConstStringSym     : RETURN( ConstString.At.FirstUsed ) |
      ConstLitSym        : RETURN( ConstLit.At.FirstUsed ) |
      ConstVarSym        : RETURN( ConstVar.At.FirstUsed ) |
      VarSym             : RETURN( Var.At.FirstUsed ) |
      TypeSym            : RETURN( Type.At.FirstUsed ) |
      PointerSym         : RETURN( Pointer.At.FirstUsed ) |
      RecordFieldSym     : RETURN( RecordField.At.FirstUsed ) |
      VarientFieldSym    : RETURN( VarientField.At.FirstUsed ) |
      EnumerationFieldSym: RETURN( EnumerationField.At.FirstUsed ) |
      SetSym             : RETURN( Set.At.FirstUsed ) |
      DefImpSym          : RETURN( DefImp.At.FirstUsed ) |
      ModuleSym          : RETURN( Module.At.FirstUsed )

      ELSE
         InternalError ('not expecting this type of symbol')
      END
   END
END GetFirstUsed ;


(*
   ForeachProcedureDo - for each procedure in module, Sym, do procedure, P.
*)

PROCEDURE ForeachProcedureDo (Sym: CARDINAL; P: PerformOperation) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : ForeachItemInListDo( DefImp.ListOfProcs, P) |
      ModuleSym   : ForeachItemInListDo( Module.ListOfProcs, P) |
      ProcedureSym: ForeachItemInListDo( Procedure.ListOfProcs, P)

      ELSE
         InternalError ('expecting DefImp or Module symbol')
      END
   END
END ForeachProcedureDo ;


(*
   ForeachInnerModuleDo - for each inner module in module, Sym,
                          do procedure, P.
*)

PROCEDURE ForeachInnerModuleDo (Sym: CARDINAL; P: PerformOperation) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      DefImpSym   : ForeachItemInListDo( DefImp.ListOfModules, P) |
      ModuleSym   : ForeachItemInListDo( Module.ListOfModules, P) |
      ProcedureSym: ForeachItemInListDo( Procedure.ListOfModules, P)

      ELSE
         InternalError ('expecting DefImp or Module symbol')
      END
   END
END ForeachInnerModuleDo ;


(*
   ForeachModuleDo - for each module do procedure, P.
*)

PROCEDURE ForeachModuleDo (P: PerformOperation) ;
BEGIN
   ForeachNodeDo(ModuleTree, P)
END ForeachModuleDo ;


(*
   ForeachFieldEnumerationDo - for each field in enumeration, Sym,
                               do procedure, P.
*)

PROCEDURE ForeachFieldEnumerationDo (Sym: CARDINAL; P: PerformOperation) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      EnumerationSym: ForeachNodeDo( Enumeration.LocalSymbols, P)

      ELSE
         InternalError ('expecting Enumeration symbol')
      END
   END
END ForeachFieldEnumerationDo ;


(*
   IsProcedureReachable - Returns true if the procedure, Sym, is
                          reachable from the main Module.
*)

PROCEDURE IsProcedureReachable (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.Reachable )

      ELSE
         InternalError ('expecting Procedure symbol')
      END
   END
END IsProcedureReachable ;


(*
   IsProcType - returns true if Sym is a ProcType Symbol.
*)

PROCEDURE IsProcType (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=ProcTypeSym )
END IsProcType ;


(*
   IsVar - returns true if Sym is a Var Symbol.
*)

PROCEDURE IsVar (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=VarSym )
END IsVar ;


(*
   DoIsConst - returns TRUE if Sym is defined as a constant
               or is an enumeration field or string.
*)

PROCEDURE DoIsConst (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      RETURN( (SymbolType=ConstVarSym) OR
              (SymbolType=ConstLitSym) OR
              (SymbolType=ConstStringSym) OR
              ((SymbolType=VarSym) AND (Var.AddrMode=ImmediateValue)) OR
              (SymbolType=EnumerationFieldSym)
            )
   END
END DoIsConst ;


(*
   IsConst - returns true if Sym contains a constant value.
*)

PROCEDURE IsConst (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsConstructor(Sym)
   THEN
      RETURN( IsConstructorConstant(Sym) )
   ELSE
      RETURN( DoIsConst(Sym) )
   END
END IsConst ;


(*
   IsConstString - returns whether sym is a conststring of any variant.
*)

PROCEDURE IsConstString (sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym (sym) ;
   WITH pSym^ DO
      RETURN SymbolType = ConstStringSym
   END
END IsConstString ;


(*
   IsConstLit - returns true if Sym is a literal constant.
*)

PROCEDURE IsConstLit (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      RETURN( SymbolType=ConstLitSym )
   END
END IsConstLit ;


(*
   IsDummy - returns true if Sym is a Dummy symbol.
*)

PROCEDURE IsDummy (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=DummySym )
END IsDummy ;


(*
   IsTemporary - returns true if Sym is a Temporary symbol.
*)

PROCEDURE IsTemporary (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym     :  RETURN( Var.IsTemp ) |
      ConstVarSym:  RETURN( ConstVar.IsTemp )

      ELSE
         RETURN( FALSE )
      END
   END
END IsTemporary ;


(*
   IsVarAParam - returns true if Sym is a variable declared as a parameter.
*)

PROCEDURE IsVarAParam (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym: RETURN( Var.IsParam )

      ELSE
         RETURN( FALSE )
      END
   END
END IsVarAParam ;


(*
   IsSubscript - returns true if Sym is a subscript symbol.
*)

PROCEDURE IsSubscript (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=SubscriptSym )
END IsSubscript ;


(*
   IsSubrange - returns true if Sym is a subrange symbol.
*)

PROCEDURE IsSubrange (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   RETURN( pSym^.SymbolType=SubrangeSym )
END IsSubrange ;


(*
   IsProcedureVariable - returns true if a Sym is a variable and
                         it was declared within a procedure.
*)

PROCEDURE IsProcedureVariable (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( IsVar(Sym) AND IsProcedure(GetVarScope(Sym)) )
END IsProcedureVariable ;


(*
   IsProcedureNested - returns TRUE if procedure, Sym, was
                       declared as a nested procedure.
*)

PROCEDURE IsProcedureNested (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsProcedure(Sym) AND (IsProcedure(GetScope(Sym))) )
END IsProcedureNested ;


(*
   IsAModula2Type - returns true if Sym, is a:
                    IsType, IsPointer, IsRecord, IsEnumeration,
                    IsSubrange, IsArray, IsUnbounded, IsProcType.
                    NOTE that it different from IsType.
*)

PROCEDURE IsAModula2Type (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN(
           IsType(Sym) OR IsRecord(Sym) OR IsPointer(Sym) OR
           IsEnumeration(Sym) OR IsSubrange(Sym) OR IsArray(Sym) OR
           IsUnbounded(Sym) OR IsProcType(Sym) OR IsSet(Sym)
         )
END IsAModula2Type ;


(*
   IsGnuAsmVolatile - returns TRUE if a GnuAsm symbol was defined as VOLATILE.
*)

PROCEDURE IsGnuAsmVolatile (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Volatile )

      ELSE
         InternalError ('expecting GnuAsm symbol')
      END
   END
END IsGnuAsmVolatile ;


(*
   IsGnuAsmSimple - returns TRUE if a GnuAsm symbol is a simple kind.
*)

PROCEDURE IsGnuAsmSimple (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Simple )

      ELSE
         InternalError ('expecting GnuAsm symbol')
      END
   END
END IsGnuAsmSimple ;


(*
   IsGnuAsm - returns TRUE if Sym is a GnuAsm symbol.
*)

PROCEDURE IsGnuAsm (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      RETURN( SymbolType=GnuAsmSym )
   END
END IsGnuAsm ;


(*
   IsRegInterface - returns TRUE if Sym is a RegInterface symbol.
*)

PROCEDURE IsRegInterface (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      RETURN( SymbolType=InterfaceSym )
   END
END IsRegInterface ;


(*
   GetParam - returns the ParamNo parameter from procedure ProcSym
*)

PROCEDURE GetParam (Sym: CARDINAL; ParamNo: CARDINAL) : CARDINAL ;
BEGIN
   CheckLegal(Sym) ;
   IF ParamNo=0
   THEN
      (* Parameter Zero is the return argument for the Function *)
      RETURN(GetType(Sym))
   ELSE
      RETURN(GetNthParam(Sym, ParamNo))
   END
END GetParam ;


(*
   GetFromIndex - return a value from list, i, at position, n.
*)

PROCEDURE GetFromIndex (i: Indexing.Index; n: CARDINAL) : CARDINAL ;
VAR
   p: POINTER TO CARDINAL ;
BEGIN
   p := Indexing.GetIndice(i, n) ;
   RETURN( p^ )
END GetFromIndex ;


(*
   PutIntoIndex - places value, v, into list, i, at position, n.
*)

PROCEDURE PutIntoIndex (VAR i: Indexing.Index; n: CARDINAL; v: CARDINAL) ;
VAR
   p: POINTER TO CARDINAL ;
BEGIN
   NEW(p) ;
   p^ := v ;
   Indexing.PutIndice(i, n, p)
END PutIntoIndex ;


(*
   Make2Tuple - creates and returns a 2 tuple from, a, and, b.
*)

PROCEDURE Make2Tuple (a, b: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
   Sym : CARDINAL ;
BEGIN
   NewSym(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      SymbolType := TupleSym ;
      WITH Tuple DO
         nTuple := 2 ;
         list := Indexing.InitIndex(1) ;
         PutIntoIndex(list, 1, a) ;
         PutIntoIndex(list, 2, b) ;
         InitWhereDeclared(At) ;
         InitWhereFirstUsed(At)
      END
   END ;
   RETURN( Sym )
END Make2Tuple ;


(*
   IsSizeSolved - returns true if the size of Sym is solved.
*)

PROCEDURE IsSizeSolved (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym    : RETURN( IsSolved(Procedure.Size) ) |
      VarSym          : RETURN( IsSolved(Var.Size) ) |
      TypeSym         : RETURN( IsSolved(Type.Size) ) |
      SetSym          : RETURN( IsSolved(Set.Size) ) |
      RecordSym       : RETURN( IsSolved(Record.Size) ) |
      VarientSym      : RETURN( IsSolved(Varient.Size) ) |
      EnumerationSym  : RETURN( IsSolved(Enumeration.Size) ) |
      PointerSym      : RETURN( IsSolved(Pointer.Size) ) |
      ArraySym        : RETURN( IsSolved(Array.Size) ) |
      RecordFieldSym  : RETURN( IsSolved(RecordField.Size) ) |
      VarientFieldSym : RETURN( IsSolved(VarientField.Size) ) |
      SubrangeSym     : RETURN( IsSolved(Subrange.Size) ) |
      SubscriptSym    : RETURN( IsSolved(Subscript.Size) ) |
      ProcTypeSym     : RETURN( IsSolved(ProcType.Size) ) |
      UnboundedSym    : RETURN( IsSolved(Unbounded.Size) )

      ELSE
         InternalError ('not expecting this kind of symbol')
      END
   END
END IsSizeSolved ;


(*
   IsOffsetSolved - returns true if the Offset of Sym is solved.
*)

PROCEDURE IsOffsetSolved (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym          : RETURN( IsSolved(Var.Offset) ) |
      RecordFieldSym  : RETURN( IsSolved(RecordField.Offset) ) |
      VarientFieldSym : RETURN( IsSolved(VarientField.Offset) )

      ELSE
         InternalError ('not expecting this kind of symbol')
      END
   END
END IsOffsetSolved ;


(*
   IsValueSolved - returns true if the value of Sym is solved.
*)

PROCEDURE IsValueSolved (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstLitSym         : RETURN( IsSolved(ConstLit.Value) ) |
      ConstVarSym         : RETURN( IsSolved(ConstVar.Value) ) |
      EnumerationFieldSym : RETURN( IsSolved(EnumerationField.Value) ) |
      ConstStringSym      : RETURN( TRUE )

      ELSE
         InternalError ('not expecting this kind of symbol')
      END
   END
END IsValueSolved ;


(*
   IsConstructorConstant - returns TRUE if constructor, Sym, is
                           defined by only constants.
*)

PROCEDURE IsConstructorConstant (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   IF IsConstructor(Sym) OR IsConstSet(Sym)
   THEN
      pSym := GetPsym(Sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         ConstVarSym:  RETURN( IsValueConst(ConstVar.Value) ) |
         ConstLitSym:  RETURN( IsValueConst(ConstLit.Value) )

         ELSE
            InternalError ('expecting Constructor')
         END
      END
   ELSE
      InternalError ('expecting Constructor')
   END
END IsConstructorConstant ;


(*
   IsComposite - returns TRUE if symbol, sym, is a composite
                 type:  ie an ARRAY or RECORD.
*)

PROCEDURE IsComposite (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF sym=NulSym
   THEN
      RETURN( FALSE )
   ELSE
      sym := SkipType(sym) ;
      RETURN( IsArray(sym) OR IsRecord(sym) )
   END
END IsComposite ;


(*
   IsSumOfParamSizeSolved - has the sum of parameters been solved yet?
*)

PROCEDURE IsSumOfParamSizeSolved (Sym: CARDINAL) : BOOLEAN ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: RETURN( IsSolved(Procedure.TotalParamSize) ) |
      ProcTypeSym : RETURN( IsSolved(ProcType.TotalParamSize) )

      ELSE
         InternalError ('expecting Procedure or ProcType symbol')
      END
   END
END IsSumOfParamSizeSolved ;


(*
   PushSize - pushes the size of Sym.
*)

PROCEDURE PushSize (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym    : PushFrom(Procedure.Size) |
      VarSym          : PushFrom(Var.Size) |
      TypeSym         : PushFrom(Type.Size) |
      SetSym          : PushFrom(Set.Size) |
      VarientSym      : PushFrom(Varient.Size) |
      RecordSym       : PushFrom(Record.Size) |
      EnumerationSym  : PushFrom(Enumeration.Size) |
      PointerSym      : PushFrom(Pointer.Size) |
      ArraySym        : PushFrom(Array.Size) |
      RecordFieldSym  : PushFrom(RecordField.Size) |
      VarientFieldSym : PushFrom(VarientField.Size) |
      SubrangeSym     : PushFrom(Subrange.Size) |
      SubscriptSym    : PushFrom(Subscript.Size) |
      ProcTypeSym     : PushFrom(ProcType.Size) |
      UnboundedSym    : PushFrom(Unbounded.Size)

      ELSE
         InternalError ('not expecting this kind of symbol')
      END
   END
END PushSize ;


(*
   PushOffset - pushes the Offset of Sym.
*)

PROCEDURE PushOffset (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym          : PushFrom(Var.Offset) |
      RecordFieldSym  : PushFrom(RecordField.Offset) |
      VarientFieldSym : PushFrom(VarientField.Offset)

      ELSE
         InternalError ('not expecting this kind of symbol')
      END
   END
END PushOffset ;


(*
   PushValue - pushes the Value of Sym onto the ALU stack.
*)

PROCEDURE PushValue (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstLitSym         : PushFrom(ConstLit.Value) |
      ConstVarSym         : PushFrom(ConstVar.Value) |
      EnumerationFieldSym : PushFrom(EnumerationField.Value) |
      ConstStringSym      : PushConstString(Sym)

      ELSE
         InternalError ('not expecting this kind of symbol')
      END
   END
END PushValue ;


(*
   PushConstString - pushes the character string onto the ALU stack.
                     It assumes that the character string is only
                     one character long.
*)

PROCEDURE PushConstString (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
   a   : ARRAY [0..10] OF CHAR ;
BEGIN
   CheckLegal (Sym) ;
   pSym := GetPsym (Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstStringSym: WITH ConstString DO
                         IF Length = 1
                         THEN
                            GetKey (Contents, a) ;
                            PushChar (a[0])
                         ELSE
                            WriteFormat0 ('ConstString must be length 1')
                         END
                      END

      ELSE
         InternalError  ('expecting ConstString symbol')
      END
   END
END PushConstString ;


(*
   PushParamSize - push the size of parameter, ParamNo,
                   of procedure Sym onto the ALU stack.
*)

PROCEDURE PushParamSize (Sym: CARDINAL; ParamNo: CARDINAL) ;
VAR
   p, Type: CARDINAL ;
BEGIN
   CheckLegal(Sym) ;
   Assert(IsProcedure(Sym) OR IsProcType(Sym)) ;
   IF ParamNo=0
   THEN
      PushSize(GetType(Sym))
   ELSE
      (*
         can use GetNthParam but 1..n returns parameter.
         But 0 yields the function return type.

         Note that VAR Unbounded parameters and non VAR Unbounded parameters
              contain the unbounded descriptor. VAR unbounded parameters
              do NOT JUST contain an address re: other VAR parameters.
      *)
      IF IsVarParam(Sym, ParamNo) AND (NOT IsUnboundedParam(Sym, ParamNo))
      THEN
         PushSize(Address)     (* VAR parameters point to the variable *)
      ELSE
         p := GetNthParam(Sym, ParamNo) ; (* nth Parameter *)
         (*
            N.B. chose to get the Type of the parameter rather than the Var
            because ProcType's have Type but no Var associated with them.
         *)
         Type := GetType(p) ;  (* ie Variable from Procedure Sym *)
         Assert(p#NulSym) ;    (* If this fails then ParamNo is out of range *)
         PushSize(Type)
      END
   END
END PushParamSize ;


(*
   PushSumOfLocalVarSize - push the total size of all local variables
                           onto the ALU stack.
*)

PROCEDURE PushSumOfLocalVarSize (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym,
      DefImpSym,
      ModuleSym   : PushSize(Sym)

      ELSE
         InternalError ('expecting Procedure, DefImp or Module symbol')
      END
   END
END PushSumOfLocalVarSize ;


(*
   PushSumOfParamSize - push the total size of all parameters onto
                        the ALU stack.
*)

PROCEDURE PushSumOfParamSize (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: PushFrom(Procedure.TotalParamSize) |
      ProcTypeSym : PushFrom(ProcType.TotalParamSize)

      ELSE
         InternalError ('expecting Procedure or ProcType symbol')
      END
   END
END PushSumOfParamSize ;


(*
   PushVarSize - pushes the size of a variable, Sym.
                 The runtime size of Sym will depend upon its addressing mode,
                 RightValue has size PushSize(GetType(Sym)) and
                 LeftValue has size PushSize(Address) since it points to a
                 variable.
*)

PROCEDURE PushVarSize (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   Assert(IsVar(Sym)) ;
   IF GetMode(Sym)=LeftValue
   THEN
      PushSize(Address)
   ELSE
      Assert(GetMode(Sym)=RightValue) ;
      PushSize(GetType(Sym))
   END
END PushVarSize ;


(*
   PopValue - pops the ALU stack into Value of Sym.
*)

PROCEDURE PopValue (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ConstLitSym         : PopInto(ConstLit.Value) |
      ConstVarSym         : PopInto(ConstVar.Value) |
      EnumerationFieldSym : InternalError ('cannot pop into an enumeration field')

      ELSE
         InternalError ('symbol type not expected')
      END
   END
END PopValue ;


(*
   PopSize - pops the ALU stack into Size of Sym.
*)

PROCEDURE PopSize (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym    : PopInto(Procedure.Size) |
      VarSym          : PopInto(Var.Size) |
      TypeSym         : PopInto(Type.Size) |
      RecordSym       : PopInto(Record.Size) |
      VarientSym      : PopInto(Varient.Size) |
      EnumerationSym  : PopInto(Enumeration.Size) |
      PointerSym      : PopInto(Pointer.Size) |
      ArraySym        : PopInto(Array.Size) |
      RecordFieldSym  : PopInto(RecordField.Size) |
      VarientFieldSym : PopInto(VarientField.Size) |
      SubrangeSym     : PopInto(Subrange.Size) |
      SubscriptSym    : PopInto(Subscript.Size) |
      ProcTypeSym     : PopInto(ProcType.Size) |
      UnboundedSym    : PopInto(Unbounded.Size) |
      SetSym          : PopInto(Set.Size)

      ELSE
         InternalError ('not expecting this kind of symbol')
      END
   END
END PopSize ;


(*
   PopOffset - pops the ALU stack into Offset of Sym.
*)

PROCEDURE PopOffset (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      VarSym          : PopInto(Var.Offset) |
      RecordFieldSym  : PopInto(RecordField.Offset) |
      VarientFieldSym : PopInto(VarientField.Offset)

      ELSE
         InternalError ('not expecting this kind of symbol')
      END
   END
END PopOffset ;


(*
   PopSumOfParamSize - pop the total value on the ALU stack as the
                       sum of all parameters.
*)

PROCEDURE PopSumOfParamSize (Sym: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   CheckLegal(Sym) ;
   pSym := GetPsym(Sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      ProcedureSym: PopInto(Procedure.TotalParamSize) |
      ProcTypeSym : PopInto(ProcType.TotalParamSize)

      ELSE
         InternalError ('expecting Procedure or ProcType symbol')
      END
   END
END PopSumOfParamSize ;


(*
   PutAlignment - assigns the alignment constant associated with,
                  type, with, align.
*)

PROCEDURE PutAlignment (type: CARDINAL; align: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(type) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym     :  Record.Align := align |
      RecordFieldSym:  RecordField.Align := align |
      TypeSym       :  Type.Align := align |
      ArraySym      :  Array.Align := align |
      PointerSym    :  Pointer.Align := align

      ELSE
         InternalError ('expecting record, field, pointer, type or an array symbol')
      END
   END
END PutAlignment ;


(*
   GetAlignment - returns the alignment constant associated with,
                  type.
*)

PROCEDURE GetAlignment (type: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(type) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym      :  RETURN( Record.Align ) |
      RecordFieldSym :  RETURN( RecordField.Align ) |
      TypeSym        :  RETURN( Type.Align ) |
      ArraySym       :  RETURN( Array.Align ) |
      PointerSym     :  RETURN( Pointer.Align ) |
      VarientFieldSym:  RETURN( GetAlignment(VarientField.Parent) ) |
      VarientSym     :  RETURN( GetAlignment(Varient.Parent) )

      ELSE
         InternalError ('expecting record, field, pointer, type or an array symbol')
      END
   END
END GetAlignment ;


(*
   PutDefaultRecordFieldAlignment - assigns, align, as the default alignment
                                    to record, sym.
*)

PROCEDURE PutDefaultRecordFieldAlignment (sym: CARDINAL; align: CARDINAL) ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym:  Record.DefaultAlign := align

      ELSE
         InternalError ('expecting record symbol')
      END
   END
END PutDefaultRecordFieldAlignment ;


(*
   GetDefaultRecordFieldAlignment - assigns, align, as the default alignment
                                    to record, sym.
*)

PROCEDURE GetDefaultRecordFieldAlignment (sym: CARDINAL) : CARDINAL ;
VAR
   pSym: PtrToSymbol ;
BEGIN
   pSym := GetPsym(sym) ;
   WITH pSym^ DO
      CASE SymbolType OF

      RecordSym      :  RETURN( Record.DefaultAlign ) |
      VarientFieldSym:  RETURN( GetDefaultRecordFieldAlignment(GetParent(sym)) ) |
      VarientSym     :  RETURN( GetDefaultRecordFieldAlignment(GetParent(sym)) )

      ELSE
         InternalError ('expecting record symbol')
      END
   END
END GetDefaultRecordFieldAlignment ;


(*
   DumpSymbols - display all symbol numbers and their type.
*)

PROCEDURE DumpSymbols ;
VAR
   pSym: PtrToSymbol ;
   sym : CARDINAL ;
BEGIN
   sym := 1 ;
   WHILE sym <= FinalSymbol () DO
      pSym := GetPsym(sym) ;
      printf ("%d  ", sym) ;
      WITH pSym^ DO
         CASE SymbolType OF

         RecordSym:  printf ("RecordSym") |
         VarientSym:  printf ("VarientSym") |
         DummySym:  printf ("DummySym") |
         VarSym:  printf ("VarSym") |
         EnumerationSym:  printf ("EnumerationSym") |
         SubrangeSym:  printf ("SubrangeSym") |
         ArraySym:  printf ("ArraySym") |
         ConstStringSym:  printf ("ConstStringSym") |
         ConstVarSym:  printf ("ConstVarSym") |
         ConstLitSym:  printf ("ConstLitSym") |
         VarParamSym:  printf ("VarParamSym") |
         ParamSym:  printf ("ParamSym") |
         PointerSym:  printf ("PointerSym") |
         UndefinedSym:  printf ("UndefinedSym") |
         TypeSym:  printf ("TypeSym") |
         RecordFieldSym:  printf ("RecordFieldSym") |
         VarientFieldSym:  printf ("VarientFieldSym") |
         EnumerationFieldSym:  printf ("EnumerationFieldSym") |
         DefImpSym:  printf ("DefImpSym") |
         ModuleSym:  printf ("ModuleSym") |
         SetSym:  printf ("SetSym") |
         ProcedureSym:  printf ("ProcedureSym") |
         ProcTypeSym:  printf ("ProcTypeSym") |
         SubscriptSym:  printf ("SubscriptSym") |
         UnboundedSym:  printf ("UnboundedSym") |
         GnuAsmSym:  printf ("GnuAsmSym") |
         InterfaceSym:  printf ("InterfaceSym") |
         ObjectSym:  printf ("ObjectSym") |
         PartialUnboundedSym:  printf ("PartialUnboundedSym") |
         TupleSym:  printf ("TupleSym") |
         OAFamilySym:  printf ("OAFamilySym") |
         EquivSym:  printf ("EquivSym") |
         ErrorSym:  printf ("ErrorSym")

         END
      END ;
      printf ("\n") ;
      INC (sym)
   END
END DumpSymbols ;


(*
   IsLegal - returns TRUE if, sym, is a legal symbol.
*)

PROCEDURE IsLegal (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN sym < FreeSymbol
END IsLegal ;


BEGIN
   Init
END SymbolTable.
