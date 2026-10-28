(* M2Options.mod initializes the user options.

Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Options ;


IMPORT CmdArgs ;
IMPORT BinDict ;
IMPORT m2gccopt ;
IMPORT FIO ;
IMPORT SFIO ;

FROM StringConvert IMPORT itos ;
FROM BinDict IMPORT Node ;
FROM SArgs IMPORT GetArg, Narg ;
FROM M2Search IMPORT SetDefExtension, SetModExtension ;
FROM PathName IMPORT DumpPathName, AddInclude ;
FROM M2Printf IMPORT printf0, printf1, fprintf1, printf2, printf3 ;
FROM FIO IMPORT StdErr ;
FROM libc IMPORT exit, printf, memset ;
FROM Debug IMPORT Halt ;
FROM gcctypes IMPORT location_t ;
FROM m2configure IMPORT FullPathCPP, TargetIEEEQuadDefault ;
FROM M2Error IMPORT InternalError ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1 ;
FROM m2misc IMPORT cerror ;
FROM M2MetaError IMPORT MetaErrorT0, MetaErrorStringT0, MetaErrorString0 ;
FROM M2LexBuf IMPORT BuiltinTokenNo, UnknownTokenNo, CommandTokenNo ;
FROM M2Debug IMPORT Assert ;
FROM StrLib IMPORT StrLen ;
FROM Storage IMPORT ALLOCATE ;
FROM NameKey IMPORT Name, MakeKey ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           Dup, string, char, Index, Equal, ReplaceChar,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

CONST
   Debugging = FALSE ;
   TraceBoolean = FALSE ;
   DefaultRuntimeModuleOverride = "m2iso:RTentity,m2iso:Storage,m2iso:SYSTEM,m2iso:M2RTS,m2iso:RTExceptions,m2iso:IOLink" ;

TYPE
   Options = POINTER TO RECORD
                           DebugName  : String ;
                           ReadOnly,
                           Modified   : BOOLEAN ;
                           BooleanTree,
                           IntegerTree,
                           StringTree : BinDict.Dictionary ;
                           gm2        : Gm2 ;
                           gcc        : Gcc ;
                           next       : Options ;
                           chain      : Chain ;
                        END ;

   Gm2 = RECORD
            option: Gm2Option ;
         END ;

   (* Record containing all user gm2 documented options.  *)
   Gm2Option = RECORD
                  Wcase_enum,
                  Wpedantic_param_names,
                  Wpedantic_cast,
                  Wuninit_variable_checking,
                  Wuninit_variable_conditional_checking,
                  Wverbose_unbounded,
                  Wstyle,
                  fauto_init,
                  fbounds,
                  fcase,
                  fcpp,
                  fd,
                  fdebug_builtins,
                  fdump_system_exports,
                  fdump_builtins,
                  fextended_opaque,
                  findex,
                  fiso,
                  fforward,
                  ffloatvalue,
                  fgen_module_list,
                  fm2_g,
                  fm2_lower_case,
                  fm2_statistics,
                  fm2_strict_type,
                  fm2_strict_type_reason,
                  fm2_whole_program,
                  fmem_report,
                  fnil,
                  fpim,
                  fpim2,
                  fpim3,
                  fpim4,
                  fpositive_mod_floor_div,
                  fpthread,
                  fq,
                  frange,
                  freturn,
                  fscaffold_dynamic,
                  fscaffold_c,
                  fscaffold_cpp,
                  fscaffold_main,
                  fscaffold_static,
                  fshared,
                  fsources,
                  fswig,
                  ftime_report,
                  funbounded_by_reference,
                  fwideset,
                  fwholediv,
                  fwholevalue,
                  Wpedantic,
                  Wreturn_type,
                  Wunused_variable,
                  Wunused_parameter,
                  c,
                  fexceptions,
                  g,
                  v,
                  fm2_quiet: OptionBoolean ;
               END ;

   Gcc = RECORD
            option: GccOption ;
         END ;

   (* Record containing all user gcc options available from gm2.  *)
   GccOption = RECORD
                  O: OptionInteger ;
               END ;

   OptionBoolean = POINTER TO RECORD
                                 Name       : String ;
                                 Value,     (* The current value.      *)
                                 Used,      (* Has it ever been read?  *)
                                 ReadOnly,  (* Is it read only?        *)
                                 WriteOnce, (* Can it only be set once?  *)
                                 WriteAfter : BOOLEAN ;   (* Can it be written after write?  *)
                                 ReadCount,               (* No. of times read.  *)
                                 WriteCount : CARDINAL ;  (* No. of times written.  *)
                                 LastTok    : CARDINAL ;  (* Where was it set?  *)
                                 Next       : OptionBoolean ;
                              END ;

   OptionInteger = POINTER TO RECORD
                                 Name       : String ;
                                 Value      : INTEGER ;
                                 Used,      (* Has it ever been read?  *)
                                 ReadOnly,  (* Is it read only?        *)
                                 WriteOnce, (* Can it only be set once?  *)
                                 WriteAfter : BOOLEAN ;   (* Can it be written after write?  *)
                                 ReadCount,               (* No. of times read.  *)
                                 WriteCount : CARDINAL ;  (* No. of times written.  *)
                                 LastTok    : CARDINAL ;  (* Where was it set?  *)
                                 Next : OptionInteger ;
                              END ;

   OptionString = POINTER TO RECORD
                                Name       : String ;
                                Value      : String ;
                                Used,      (* Has it ever been read?  *)
                                ReadOnly,  (* Is it read only?        *)
                                WriteOnce, (* Can it only be set once?  *)
                                WriteAfter : BOOLEAN ;   (* Can it be written after write?  *)
                                ReadCount,               (* No. of times read.  *)
                                WriteCount : CARDINAL ;  (* No. of times written.  *)
                                LastTok    : CARDINAL ;  (* Where was it set?  *)
                                Next : OptionString ;
                             END ;

   Chain = RECORD
              Bool: BoolChain ;
              Int : IntChain ;
              Str : StrChain ;
           END ;

   BoolChain = RECORD
                  head, tail: OptionBoolean ;
               END ;

   IntChain = RECORD
                 head, tail: OptionInteger ;
              END ;

   StrChain = RECORD
                 head, tail: OptionString ;
              END ;


VAR
   ForcedLocationValue : location_t ;
   (* -E/M/MM present? - preprocessing only    *)
   PPonly              : BOOLEAN ;
   StrFreeList         : OptionString ;
   IntFreeList         : OptionInteger ;
   BoolFreeList        : OptionBoolean ;
   DefaultOption,
   CommandLineOption,
   CurrentOption       : Options ;
   SaveTempsDir,
   DumpDeclFilename,
   DumpQuadFilename,
   DumpGimpleFilename,
   M2Dump,
   M2DumpFilter,
   M2Prefix,
   M2PathName,
   Barg,
   MFarg,
   MTFlag,
   MQFlag,
   DepTarget,
   CmdLineObj,
   DumpDir,
   GenModuleListFilename,
   UselistFilename,
   RuntimeModuleOverride,
   CppArgs              : String ;
   DumpDecl,
   DumpGimple,
   DumpPragmas,
   IBMLongDouble,
   IEEELongDouble,
   SaveTemps,
   GenerateStatementNote,
   ForcedLocation,
   DebugFunctionLineNumbers,
   DebugTraceQuad,
   DebugTraceToken,
   DebugTraceLine,
   DebugTraceTree,
   DumpQuad,
   CPreProcessor,
   CC1Quiet,
   MPFlag,
   MMDFlag,
   MDFlag,
   MFlag,
   MMFlag,
   LineDirectives,
   UselistFlag          : BOOLEAN ;
   OffTBits             : CARDINAL ;
   FilterName           : String ;


(*
   DumpBoolean - dump the boolean value and qualident.
*)

PROCEDURE DumpBoolean (node: Node) ;
VAR
   str: String ;
   val: OptionBoolean ;
BEGIN
   FIO.WriteString (FIO.StdOut, "BOOLEAN ") ;
   val := BinDict.Value (node) ;
   IF val^.Value
   THEN
      FIO.WriteString (FIO.StdOut, "TRUE ")
   ELSE
      FIO.WriteString (FIO.StdOut, "FALSE ")
   END ;
   str := BinDict.Key (node) ;
   str := SFIO.WriteS (FIO.StdOut, str) ;
   FIO.WriteLine (FIO.StdOut)
END DumpBoolean ;


(*
   DumpInteger - dump the integer value and qualident.
*)

PROCEDURE DumpInteger (node: Node) ;
VAR
   str: String ;
   val: OptionInteger ;
BEGIN
   FIO.WriteString (FIO.StdOut, "INTEGER ") ;
   val := BinDict.Value (node) ;
   str := itos (val^.Value, 3, ' ', FALSE) ;
   str := SFIO.WriteS (FIO.StdOut, str) ;
   str := KillString (str) ;
   str := BinDict.Key (node) ;
   FIO.WriteString (FIO.StdOut, " ") ;
   str := SFIO.WriteS (FIO.StdOut, str) ;
   FIO.WriteLine (FIO.StdOut)
END DumpInteger ;


(*
   DumpString - dump the boolean value and qualident.
*)

PROCEDURE DumpString (node: Node) ;
VAR
   str: String ;
   val: OptionString ;
BEGIN
   FIO.WriteString (FIO.StdOut, "string ") ;
   val := BinDict.Value (node) ;
   str := SFIO.WriteS (FIO.StdOut, val^.Value) ;
   FIO.WriteString (FIO.StdOut, " ") ;
   str := BinDict.Key (node) ;
   str := SFIO.WriteS (FIO.StdOut, str) ;
   FIO.WriteLine (FIO.StdOut)
END DumpString ;


(*
   DumpDomain - dumps the boolean, integer and string
                contents and their default values.
*)

PROCEDURE DumpDomain (opt: Options) ;
BEGIN
   BinDict.PostOrder (opt^.BooleanTree, DumpBoolean) ;
   BinDict.PostOrder (opt^.IntegerTree, DumpInteger) ;
   BinDict.PostOrder (opt^.StringTree, DumpString) ;
   FIO.FlushBuffer (FIO.StdOut)
END DumpDomain ;


(*
   DumpDefaultPragmas - dump the supported pragma qualidents and
                        their types and initial values.
*)

PROCEDURE DumpDefaultPragmas ;
BEGIN
   printf0 ("Supported default pragmas\n");
   DumpDomain (DefaultOption)
END DumpDefaultPragmas ;


(*
   DupLocalBool - return a duplicate boolean of cb.
*)

PROCEDURE DupLocalBool (tokno: CARDINAL; opt: Options; cb: OptionBoolean) : OptionBoolean ;
VAR
   nb: OptionBoolean ;
BEGIN
   IF cb^.WriteOnce
   THEN
      (* WriteOnce booleans are global, therefore we do not create a copy.  *)
      RETURN cb
   ELSE
      (* Local boolean option, create and return a copy.  *)
      nb := NewBool (opt) ;
      nb^.Name := Dup (cb^.Name) ;
      nb^.Value := cb^.Value ;
      nb^.ReadCount := cb^.ReadCount ;
      nb^.WriteCount := cb^.WriteCount ;
      nb^.Used := cb^.Used ;
      nb^.ReadOnly := cb^.ReadOnly ;
      nb^.WriteOnce := cb^.WriteOnce ;
      nb^.LastTok := tokno ;
      RETURN nb
   END
END DupLocalBool ;


(*
   DupLocalInt - return a duplicate boolean of cb.
*)

PROCEDURE DupLocalInt (tokno: CARDINAL; opt: Options; cb: OptionInteger) : OptionInteger ;
VAR
   nb: OptionInteger ;
BEGIN
   IF cb^.WriteOnce
   THEN
      (* WriteOnce integers are global, therefore we do not create a copy.  *)
      RETURN cb
   ELSE
      (* Local boolean option, create and return a copy.  *)
      nb := NewInt (opt) ;
      nb^.Name := Dup (cb^.Name) ;
      nb^.Value := cb^.Value ;
      nb^.ReadCount := cb^.ReadCount ;
      nb^.WriteCount := cb^.WriteCount ;
      nb^.Used := cb^.Used ;
      nb^.ReadOnly := cb^.ReadOnly ;
      nb^.WriteOnce := cb^.WriteOnce ;
      nb^.LastTok := tokno ;
      RETURN nb
   END
END DupLocalInt ;


(*
   MakeOptionName - return a new string referring to the
                    domain.option.qualident.
*)

PROCEDURE MakeOptionName (name: String) : String ;
VAR
   fno,
   qual,
   newqual,
   optionName: String ;
BEGIN
   optionName := ExtractDomain (name) ;
   qual := ExtractQualident (name) ;
   optionName := ConCat (optionName, Mark (InitString ('.option.'))) ;
   IF Length (optionName) > StrLen ("fno_")
   THEN
      fno := Slice (qual, 0, StrLen ("fno_")) ;
      IF EqualArray (fno, "fno_")
      THEN
         newqual := ConCat (InitString ("f"), Mark (Slice (qual, StrLen ("fno_"), 0))) ;
         qual := KillString (qual) ;
         qual := newqual
      END
   END ;
   optionName := ConCat (optionName, qual) ;
   qual := KillString (qual) ;
   RETURN optionName
END MakeOptionName ;


(*
   IsInvertOptionBoolean - return TRUE if name begins with -fno_.
*)

PROCEDURE IsInvertOptionBoolean (name: String) : BOOLEAN ;
VAR
   fno   : String ;
   result: BOOLEAN ;
BEGIN
   result := FALSE ;
   IF Length (name) > StrLen ("fno_")
   THEN
      fno := Slice (name, 0, StrLen ("fno_")) ;
      result := EqualArray (fno, "fno_") ;
      fno := KillString (fno)
   END ;
   RETURN result
END IsInvertOptionBoolean ;


(*
   OptionExistsBoolean - return TRUE if option name exists and it
                         was defined to contain a boolean value.
*)

PROCEDURE OptionExistsBoolean (name: String) : BOOLEAN ;
VAR
   entry     : OptionBoolean ;
   optionName: String ;
BEGIN
   IF IsAggregateOptionBoolean (name)
   THEN
      RETURN TRUE
   ELSE
      (* Every option is defined with a default value in the
         DefaultOption scope.
         So we can change the name to refer to the option scope
         and return whether an entry exists.  *)
      optionName := MakeOptionName (name) ;
      (* Look it up.  *)
      entry := BinDict.Get (DefaultOption^.BooleanTree, optionName) ;
      optionName := KillString (optionName) ;
      (* Return TRUE if entry found.  *)
      RETURN entry # NIL
   END
END OptionExistsBoolean ;


(*
   OptionExistsInteger - return TRUE if option name exists and it
                         was defined to contain an integer value.
*)

PROCEDURE OptionExistsInteger (name: String) : BOOLEAN ;
VAR
   entry     : OptionInteger ;
   optionName: String ;
BEGIN
   (* Every option is defined with a default value in the
      DefaultOption scope.
      So we can change the name to refer to the option scope
      and return whether an entry exists.  *)
   optionName := MakeOptionName (name) ;
   (* Look it up.  *)
   entry := BinDict.Get (DefaultOption^.IntegerTree, optionName) ;
   optionName := KillString (optionName) ;
   (* Return TRUE if entry found.  *)
   RETURN entry # NIL
END OptionExistsInteger ;


(*
   OptionExistsName - return TRUE if option name exists and it
                      was defined to contain a name value.
*)

PROCEDURE OptionExistsName (name: String) : BOOLEAN ;
VAR
   entry     : OptionString ;
   optionName: String ;
BEGIN
   (* Every option is defined with a default value in the
      DefaultOption scope.
      So we can change the name to refer to the option scope
      and return whether an entry exists.  *)
   optionName := MakeOptionName (name) ;
   (* Look it up.  *)
   entry := BinDict.Get (DefaultOption^.StringTree, optionName) ;
   optionName := KillString (optionName) ;
   (* Return TRUE if entry found.  *)
   RETURN entry # NIL
END OptionExistsName ;


(*
   InitOptions - initialize an empty option block.
*)

PROCEDURE InitOptions (name: ARRAY OF CHAR) : Options ;
VAR
   opt: Options ;
BEGIN
   NEW (opt) ;
   memset (opt, 0, SIZE (opt^)) ;  (* All pointers set to NIL and BOOLEANs to FALSE.  *)
   opt^.DebugName := InitString (name) ;
   RETURN opt
END InitOptions ;


(*
   IsReadOnly - is option block read only?
*)

PROCEDURE IsReadOnly (opt: Options) : BOOLEAN ;
BEGIN
   RETURN opt^.ReadOnly
END IsReadOnly ;


(*
   SetReadOnly - set option block read only to value.
*)

PROCEDURE SetReadOnly (opt: Options; value: BOOLEAN) ;
BEGIN
   opt^.ReadOnly := value
END SetReadOnly ;


(*
   IsModified - has any option been modified?
*)

PROCEDURE IsModified (opt: Options) : BOOLEAN ;
BEGIN
   RETURN opt^.Modified
END IsModified ;


(*
   SetModified - set option block modified to value.
*)

PROCEDURE SetModified (opt: Options; value: BOOLEAN) ;
BEGIN
   opt^.Modified := value
END SetModified ;


(*
   GetDefaultOptions - return the default option block.
*)

PROCEDURE GetDefaultOptions () : Options ;
BEGIN
   RETURN DefaultOption
END GetDefaultOptions ;


(*
   GetCommandOptions - return the command line option block.
*)

PROCEDURE GetCommandOptions () : Options ;
BEGIN
   RETURN CommandLineOption
END GetCommandOptions ;


(*
   KillOptions - destroys opt and sets it to NIL.
*)

(*
PROCEDURE KillOptions (VAR opt: Options) ;
BEGIN
   IF opt # NIL
   THEN
      KillBooleanTree (opt) ;
   END
END KillOptions ;


(*
   KillBooleanTree -
*)

PROCEDURE KillBooleanTree (opt: Options) ;
BEGIN
   BinDict.Kill (opt^.BooleanTree) ;
   IF opt^.chain.Bool.tail # NIL
   THEN
      opt^.chain.Bool.tail := BoolFreeList
   END ;
   BoolFreeList := opt^.chain.Bool.head
END KillBooleanTree ;
*)


(*
   FailBooleanWriteOnce - return TRUE if bl will fail the write once restriction.
*)

PROCEDURE FailBooleanWriteOnce (bl: OptionBoolean; value: BOOLEAN) : BOOLEAN ;
BEGIN
   RETURN bl^.WriteOnce AND (bl^.Value # value) AND (bl^.WriteCount >= 1)
END FailBooleanWriteOnce ;


(*
   SetBoolean -
*)

PROCEDURE SetBoolean (opt: Options; bl: OptionBoolean; tokno: CARDINAL; value: BOOLEAN) ;
BEGIN
   IF bl^.Value # value
   THEN
      (* The value is being changed.  *)
      IF (tokno # CommandTokenNo) AND (opt^.ReadOnly OR bl^.ReadOnly)
      THEN
         MetaErrorT0 (tokno, '{%E}boolean option can only be read by a pragma')
      ELSIF FailBooleanWriteOnce (bl, value)
      THEN
         MetaErrorT0 (tokno, '{%E}boolean option can only be written once') ;
         MetaErrorT0 (bl^.LastTok, '{%E}boolean command line option was previously set')
(*
      ELSIF (bl^.ReadCount > 0) AND (NOT bl^.WriteAfter)
      THEN
         MetaErrorT0 (tokno, '{%E}boolean option cannot be modified after it has been read')
*)
      ELSE
         opt^.Modified := TRUE ;
         bl^.Value := value ;
         INC (bl^.WriteCount)
      END ;
      bl^.LastTok := tokno
   END
END SetBoolean ;


(*
   InitBooleanTree - ensure that the boolean tree has been initialized.
*)

PROCEDURE InitBooleanTree (opt: Options) ;
BEGIN
   IF opt^.BooleanTree = NIL
   THEN
      opt^.BooleanTree := BinDict.Init (StrCmp, NoDelete, NoDelete)
   END
END InitBooleanTree ;


(*
   SetOptionBoolean -
*)

PROCEDURE SetOptionBoolean (tokno: CARDINAL; prev, next: Options; name: String;
                            value: BOOLEAN) ;
VAR
   valueNode, newb: OptionBoolean ;
BEGIN
   InitBooleanTree (next) ;
   valueNode := BinDict.Get (prev^.BooleanTree, name) ;
   IF valueNode = NIL
   THEN
      MetaErrorT0 (tokno, '{%E}unknown boolean command line option')
   ELSIF prev = next
   THEN
      SetBoolean (next, valueNode, tokno, value)
   ELSE
      newb := DupLocalBool (tokno, next, valueNode) ;
      SetBoolean (next, newb, tokno, value) ;
      BinDict.Insert (next^.BooleanTree, name, newb) ;
      IF next = CommandLineOption
      THEN
         Assert (prev = DefaultOption) ;
         (* And set the same default variable name.  *)
         SetBoolean (prev, valueNode, tokno, value)
      END
   END
END SetOptionBoolean ;


(*
   NewBool -
*)

PROCEDURE NewBool (opt: Options) : OptionBoolean ;
VAR
   ob: OptionBoolean ;
BEGIN
   IF BoolFreeList = NIL
   THEN
      NEW (ob)
   ELSE
      ob := BoolFreeList ;
      BoolFreeList := BoolFreeList^.Next
   END ;
   ob^.Next := opt^.chain.Bool.head ;
   opt^.chain.Bool.head := ob ;
   IF opt^.chain.Bool.tail = NIL
   THEN
      opt^.chain.Bool.tail := ob
   END ;
   RETURN ob
END NewBool ;


(*
   InitBoolean - return a OptionBoolean in an initial state with value.
*)

PROCEDURE InitBoolean (tokno: CARDINAL; opt: Options;
                       name: String;
                       value, readonly,
                       writeonce, writeafter: BOOLEAN) : OptionBoolean ;
VAR
   bl: OptionBoolean ;
BEGIN
   bl := NewBool (opt) ;
   WITH bl^ DO
      Name := name ;
      Used := FALSE ;
      Value := value ;
      WriteCount := 0 ;
      ReadCount := 0 ;
      ReadOnly := readonly ;
      WriteOnce := writeonce ;
      WriteAfter := writeafter ;
      LastTok := tokno
   END ;
   RETURN bl
END InitBoolean ;


(*
   CacheOptionBoolean - returns an OptionBoolean which has been registered
                        in opt and contains value.
*)

PROCEDURE CacheOptionBoolean (opt: Options; name: ARRAY OF CHAR;
                              value, readonly, writeonce, writeafter: BOOLEAN) : OptionBoolean ;
VAR
   ob : OptionBoolean ;
   str: String ;
BEGIN
   str := InitString (name) ;
   ob := InitBoolean (CommandTokenNo, opt, str, value,
                      readonly, writeonce, writeafter) ;
   BinDict.Insert (opt^.BooleanTree, str, ob) ;
   RETURN ob
END CacheOptionBoolean ;


(*
   GetRegisteredOptionBoolean -
*)

PROCEDURE GetRegisteredOptionBoolean (tokno: CARDINAL;
                                      opt: Options; name: String) : BOOLEAN ;
VAR
   valueNode: OptionBoolean ;
BEGIN
   valueNode := BinDict.Get (opt^.BooleanTree, name) ;
   IF valueNode = NIL
   THEN
      IF opt^.next = NIL
      THEN
         (* Not found the option.  *)
         IF tokno = UnknownTokenNo
         THEN
            InternalError ('expecting this option to be registered')
         ELSE
            MetaErrorT0 (tokno, 'option does not exist')
         END ;
         RETURN FALSE
      ELSE
         RETURN GetRegisteredOptionBoolean (tokno, opt^.next, name)
      END
   ELSE
      valueNode^.Used := TRUE ;
      INC (valueNode^.ReadCount) ;
      RETURN valueNode^.Value
   END
END GetRegisteredOptionBoolean ;


(*
   NewStr -
*)
(*
PROCEDURE NewStr (opt: Options) : OptionString ;
VAR
   oi: OptionString ;
BEGIN
   IF StrFreeList = NIL
   THEN
      NEW (oi)
   ELSE
      oi := StrFreeList ;
      StrFreeList := StrFreeList^.Next
   END ;
   oi^.Next := opt^.chain.Str.head ;
   opt^.chain.Str.head := oi ;
   IF opt^.chain.Str.tail = NIL
   THEN
      opt^.chain.Str.tail := oi
   END ;
   RETURN oi
END NewStr ;


(*
   InitStr - return a OptionString in an initial state with value.
*)

PROCEDURE InitStr (tokno: CARDINAL; opt: Options;
                   name: String; value: String;
                   readonly, writeonce, writeafter: BOOLEAN) : OptionString ;
VAR
   bl: OptionString ;
BEGIN
   bl := NewStr (opt) ;
   WITH bl^ DO
      Name := name ;
      Used := FALSE ;
      Value := value ;
      WriteCount := 0 ;
      ReadCount := 0 ;
      ReadOnly := readonly ;
      WriteOnce := writeonce ;
      WriteAfter := writeafter ;
      LastTok := tokno
   END ;
   RETURN bl
END InitStr ;


(*
   CacheOptionString - returns an OptionString which has been registered
                        in opt and contains value.
*)

PROCEDURE CacheOptionString (opt: Options; name: ARRAY OF CHAR;
                             value: String;
                             readonly, writeonce, writeafter: BOOLEAN) : OptionString ;
VAR
   ob : OptionString ;
   str: String ;
   val,
   key: Name ;
BEGIN
   str := InitString (name) ;
   ob := InitStr (CommandTokenNo, opt, str, value, readonly, writeonce, writeafter) ;
   BinDict.Insert (opt^.StringTree, str, ob) ;
   IF GetDumpPragmas (BuiltinTokenNo)
   THEN
      key := MakeKey (name) ;
      val := MakeKey (value) ;
      printf2 ('Supported pragma qualident %a is a string with an initial value %a\n', key, val)
   END ;
   RETURN ob
END CacheOptionString ;


(*
   GetNamedOptionString - returns the string value associated with name.
                          It assumes the binary tree contains name.
*)

PROCEDURE GetNamedOptionString (opt: Options; name: String) : String ;
VAR
   os: OptionString ;
BEGIN
   InitStringTree (opt) ;
   os := BinDict.Get (opt^.StringTree, name) ;
   os^.Used := TRUE ;
   INC (os^.ReadCount) ;
   RETURN os^.Value
END GetNamedOptionString ;


(*
   FailStringWriteOnce - return TRUE if bl will fail the write once restriction.
*)

PROCEDURE FailStringWriteOnce (os: OptionString; value: String) : BOOLEAN ;
BEGIN
   RETURN os^.WriteOnce AND (NOT Equal (os^.Value, value)) AND (os^.WriteCount >= 1)
END FailStringWriteOnce ;


(*
   SetString - set option string to value.
*)

PROCEDURE SetString (opt: Options; os: OptionString; tokno: CARDINAL; value: String) ;
BEGIN
   IF os^.Value # value
   THEN
      (* The value is being changed.  *)
      IF opt^.ReadOnly OR os^.ReadOnly
      THEN
         MetaErrorT0 (tokno, '{%E}string command line option can only be read by a pragma')
      ELSIF FailStringWriteOnce (os, value)
      THEN
         MetaErrorT0 (tokno, '{%E}string option can only be written once') ;
         MetaErrorT0 (os^.LastTok, '{%E}string command line option was previously set')
(*
      ELSIF (bl^.ReadCount > 0) AND (NOT bl^.WriteAfter)
      THEN
         MetaErrorT0 (tokno, '{%E}string option cannot be modified after it has been read')
*)
      ELSE
         opt^.Modified := TRUE ;
         os^.Value := value ;
         INC (os^.WriteCount)
      END ;
      os^.LastTok := tokno
   END
END SetString ;
*)

(*
   InitStringTree - ensure that the boolean tree has been initialized.
*)

PROCEDURE InitStringTree (opt: Options) ;
BEGIN
   IF opt^.StringTree = NIL
   THEN
      opt^.StringTree := BinDict.Init (StrCmp, NoDelete, NoDelete)
   END
END InitStringTree ;


(*
(*
   SetOptionString - set an option string in opt.  It creates an
                     OptionString entry in opt if one does not already
                     exist.
*)

PROCEDURE SetOptionString (tokno: CARDINAL; opt: Options; name: String;
                           value: String) ;
VAR
   valueNode, newb: OptionString ;
   cur            : Options ;
BEGIN
   InitStringTree (next) ;
   valueNode := BinDict.Get (prev^.StringTree, name) ;
   IF valueNode = NIL
   THEN
      MetaErrorT0 (tokno, '{%E}unknown string command line option')
   ELSIF prev = next
   THEN
      SetString (next, valueNode, tokno, value)
   ELSE
      newb := DupLocalString (tokno, next, valueNode) ;
      SetString (next, newb, tokno, value) ;
      BinDict.Insert (next^.StringTree, name, newb)
   END
END SetOptionString ;


(*
   SetNamedOptionString - looks up the previous option block defining name
                          and changes its value.
*)

PROCEDURE SetNamedOptionString (tokno: CARDINAL;
                                name: String; value: String) ;
VAR
   optionName: String ;
   cur       : Options ;
   prev      : String ;
BEGIN
   cur := GetOptionBlockForScope (tokno, name) ;
   IF cur = NIL
   THEN
      MetaErrorT0 (tokno, 'the scope specified in the qualident unknown')
   ELSE
      cur := LookupScopeForName (cur, name) ;
      optionName := MakeOptionName (name) ;
      IF GetOptionAvailable (cur, optionName)
      THEN
         prev := GetNamedOptionString (cur, optionName) ;
         IF NOT Equal (prev, value)
         THEN
            IF cur^.ReadOnly
            THEN
               (* Diff pragma found.  *)
               MetaErrorT0 (tokno, 'command line option conflicts with source pragma')
            ELSE
               SetOptionString (tokno, cur, CurrentOption, optionName, value)
            END
         END
      ELSE
         MetaErrorT0 (tokno, 'option is unknown')
      END
   END
END SetNamedOptionString ;
*)

(*
   GetOptionAvailable - returns TRUE if the option name exists in either
                        the boolean, integer or string tree.
*)

PROCEDURE GetOptionAvailable (opt: Options; name: String) : BOOLEAN ;
VAR
   entry: ADDRESS ;
BEGIN
   InitBooleanTree (opt) ;
   IF opt^.BooleanTree # NIL
   THEN
      entry := BinDict.Get (opt^.BooleanTree, name) ;
      IF entry # NIL
      THEN
         RETURN TRUE
      END
   END ;
   InitIntegerTree (opt) ;
   IF opt^.IntegerTree # NIL
   THEN
      entry := BinDict.Get (opt^.IntegerTree, name) ;
      IF entry # NIL
      THEN
         RETURN TRUE
      END
   END ;
   InitStringTree (opt) ;
   IF opt^.StringTree # NIL
   THEN
      entry := BinDict.Get (opt^.StringTree, name) ;
      IF entry # NIL
      THEN
         RETURN TRUE
      END
   END ;
   RETURN FALSE
END GetOptionAvailable ;


(*
   ExtractScope - return the scope component from the fullname.
*)

PROCEDURE ExtractScope (fullname: String) : String ;
VAR
   first,
   second: INTEGER ;
BEGIN
   first := Index (fullname, '.', 0) ;
   IF first > 0
   THEN
      INC (first) ;
      second := Index (fullname, '.', first) ;
      IF second > 0
      THEN
         RETURN Slice (fullname, first, second)
      END
   END ;
   RETURN NIL
END ExtractScope ;


(*
   ExtractDomain - return the domain component from the fullname.
*)

PROCEDURE ExtractDomain (fullname: String) : String ;
VAR
   first: INTEGER ;
BEGIN
   first := Index (fullname, '.', 0) ;
   IF first > 0
   THEN
      RETURN Slice (fullname, 0, first)
   END ;
   RETURN NIL
END ExtractDomain ;


(*
   ExtractQualident - return the qualident component from fullname.
*)

PROCEDURE ExtractQualident (fullname: String) : String ;
VAR
   first,
   second: INTEGER ;
BEGIN
   first := Index (fullname, '.', 0) ;
   IF first > 0
   THEN
      INC (first) ;
      second := Index (fullname, '.', first) ;
      IF second > 0
      THEN
         RETURN Slice (fullname, second+1, 0)
      END
   END ;
   RETURN NIL
END ExtractQualident ;


(*
   GetOptionBlockForScope - returns the option block which set name.
                            NIL is returned if the option does not exist.
*)

PROCEDURE GetOptionBlockForScope (tok: CARDINAL; name: String) : Options ;
VAR
   scope: String ;
BEGIN
   scope := ExtractScope (name) ;
   IF EqualArray (scope, 'default')
   THEN
      RETURN DefaultOption
   ELSIF EqualArray (scope, 'option')
   THEN
      RETURN CommandLineOption
   ELSIF EqualArray (scope, 'local')
   THEN
      RETURN CurrentOption
   ELSIF EqualArray (scope, 'scope')
   THEN
      RETURN CurrentOption
   ELSE
      MetaErrorT0 (tok, 'unknown scope in name specified') ;
      RETURN NIL
   END
END GetOptionBlockForScope ;


(*
   LookupScopeForName - return the scope containing name looking at cur outwards.
*)

PROCEDURE LookupScopeForName (cur: Options; name: String) : Options ;
VAR
   optionName: String ;
BEGIN
   optionName := MakeOptionName (name) ;
   WHILE cur # NIL DO
      IF GetOptionAvailable (cur, optionName)
      THEN
         optionName := KillString (optionName) ;
         RETURN cur
      ELSE
         cur := cur^.next
      END
   END ;
   optionName := KillString (optionName) ;
   RETURN NIL
END LookupScopeForName ;


(*
   GetNamedOptionBoolean - returns the boolean value associated with name.
                           It assumes the binary tree contains name.
*)

PROCEDURE GetNamedOptionBoolean (opt: Options; name: String) : BOOLEAN ;
VAR
   ob: OptionBoolean ;
BEGIN
   InitBooleanTree (opt) ;
   ob := BinDict.Get (opt^.BooleanTree, name) ;
   ob^.Used := TRUE ;
   INC (ob^.ReadCount) ;
   RETURN ob^.Value
END GetNamedOptionBoolean ;


(*
   IsOptionConflict - returns TRUE if name was specified in the options block.
*)

PROCEDURE IsOptionConflict (tokno: CARDINAL; opt: Options; name: String) : BOOLEAN ;
BEGIN
   IF (tokno = CommandTokenNo) AND (opt = CommandLineOption)
   THEN
      (* Users can specify multiple overlapping options for example:
         -fsoft-check-all -fno-return.  *)
      RETURN FALSE
   ELSE
      RETURN GetOptionAvailable (CommandLineOption, name)
   END
END IsOptionConflict ;


(*
   DebugBoolean -
*)

PROCEDURE DebugBoolean (tokno: CARDINAL;
                        function: ARRAY OF CHAR; cur: Options;
                        value, invert: BOOLEAN; name: String) ;
VAR
   msg, val: String ;
BEGIN
   RETURN ;
   val := Dup (name) ;
   val := ConCat (val, Mark (InitString (' <'))) ;
   val := ConCat (val, cur^.DebugName) ;
   val := ConCat (val, Mark (InitString ('> '))) ;
   IF invert
   THEN
      value := NOT value
   ELSE
      value := value
   END ;
   IF value
   THEN
      val := ConCat (val, Mark (InitString ('TRUE')))
   ELSE
      val := ConCat (val, Mark (InitString ('FALSE')))
   END ;
   IF tokno = CommandTokenNo
   THEN
      msg := InitString ('command line value, ') ;
      msg := ConCat (msg, Mark (InitString (function))) ;
      msg := ConCat (msg, Mark (InitString (' '))) ;
      msg := ConCat (msg, Mark (val)) ;
      MetaErrorString0 (msg)
   ELSIF tokno = BuiltinTokenNo
   THEN
      msg := InitString ('builtin, ') ;
      msg := ConCat (msg, Mark (InitString (function))) ;
      msg := ConCat (msg, Mark (InitString (' '))) ;
      msg := ConCat (msg, Mark (val)) ;
      MetaErrorString0 (msg)
   ELSIF tokno = UnknownTokenNo
   THEN
      msg := InitString ('unknown, ') ;
      msg := ConCat (msg, Mark (InitString (function))) ;
      msg := ConCat (msg, Mark (InitString (' '))) ;
      msg := ConCat (msg, Mark (val)) ;
      MetaErrorString0 (msg)
   ELSE
      msg := InitString ('{%O}') ;
      msg := ConCat (msg, Mark (InitString (function))) ;
      msg := ConCat (msg, Mark (InitString (' '))) ;
      msg := ConCat (msg, Mark (val)) ;
      MetaErrorStringT0 (tokno, msg)
   END
END DebugBoolean ;


(*
   OutS -
*)

PROCEDURE OutS (format: ARRAY OF CHAR; arg: String) ;
VAR
   message: String ;
BEGIN
   IF TraceBoolean
   THEN
      IF arg = NIL
      THEN
         message := Sprintf0 (Mark (InitString (format)))
      ELSE
         message := Sprintf1 (Mark (InitString (format)), arg)
      END ;
      message := SFIO.WriteS (FIO.StdErr, message) ;
      message := KillString (message)
   END
END OutS ;


(*
   DebugFilterName -
*)

PROCEDURE DebugFilterName (tokno: CARDINAL; desc: ARRAY OF CHAR; name: String) ;
VAR
   optionName: String ;
   ob        : OptionBoolean ;
   cur       : Options ;
BEGIN
   IF TraceBoolean
   THEN
      OutS (desc, NIL) ;
      OutS (": looking for %s\n", name) ;
      optionName := MakeOptionName (name) ;
      cur := CurrentOption ;
      WHILE cur # NIL DO
         OutS ("scope: %s\n", cur^.DebugName) ;
         IF GetOptionAvailable (cur, optionName)
         THEN
            ob := BinDict.Get (cur^.BooleanTree, optionName) ;
            IF ob # NIL
            THEN
               IF ob^.Value
               THEN
                  OutS ("  %s seen, has value TRUE\n", optionName)
               ELSE
                  OutS ("  %s seen, has value FALSE\n", optionName) ;
               END ;
               optionName := KillString (optionName) ;
               RETURN
            END
         END ;
         cur := cur^.next
      END ;
      optionName := KillString (optionName) ;
      OutS (" failed to find: %s\n", name)
   END
END DebugFilterName ;


(*
   SetBooleanFromArray -
*)

PROCEDURE SetBooleanFromArray (tokno: CARDINAL; domain, scope: String;
                               name: ARRAY OF CHAR; value: BOOLEAN) ;
VAR
   full: String ;
BEGIN
   full := Dup (domain) ;
   full := ConCatChar (full, ".") ;
   full := ConCat (full, scope) ;
   full := ConCatChar (full, ".") ;
   full := ConCat (full, Mark (InitString (name))) ;
   SetNamedOptionVariableBoolean (tokno, full, value) ;
   full := KillString (full)
END SetBooleanFromArray ;


(*
   SetNamedOptionAggregateBoolean -
*)

PROCEDURE SetNamedOptionAggregateBoolean (tokno: CARDINAL;
                                          name: String;
                                          value: BOOLEAN) : BOOLEAN ;
VAR
   result    : BOOLEAN ;
   scope,
   domain,
   qualident,
   optionName: String ;
BEGIN
   optionName := MakeOptionName (name) ;
   domain := NIL ;
   scope := NIL ;
   qualident := NIL ;
   result := FALSE ;
   IF EqualArray (optionName, 'gm2.option.fsoft_check_all')
   THEN
      domain := ExtractDomain (name) ;
      scope := ExtractScope (name) ;
      qualident := ExtractQualident (name) ;
      SetBooleanFromArray (tokno, domain, scope, 'fcase', value) ;
      SetBooleanFromArray (tokno, domain, scope, 'ffloatvalue', value) ;
      SetBooleanFromArray (tokno, domain, scope, 'findex', value) ;
      SetBooleanFromArray (tokno, domain, scope, 'fnil', value) ;
      SetBooleanFromArray (tokno, domain, scope, 'frange', value) ;
      SetBooleanFromArray (tokno, domain, scope, 'freturn', value) ;
      SetBooleanFromArray (tokno, domain, scope, 'fwholevalue', value) ;
      SetBooleanFromArray (tokno, domain, scope, 'fwholediv', value) ;
      result := TRUE
   ELSIF EqualArray (optionName, 'gm2.option.Wall')
   THEN
      domain := ExtractDomain (name) ;
      scope := ExtractScope (name) ;
      qualident := ExtractQualident (name) ;
      SetBooleanFromArray (tokno, domain, scope, 'Wcase_enum', value) ;
      (* -Wuninit-variable-checking=known.  *)
      result := TRUE
   END ;
   optionName := KillString (optionName) ;
   domain := KillString (domain) ;
   scope := KillString (scope) ;
   qualident := KillString (qualident) ;
   RETURN result
END SetNamedOptionAggregateBoolean ;


(*
   SetNamedOptionBoolean - attempt to set the named boolean to value.
                           If the named boolean does not exist or conflicts
                           then a user error is generated at tokno.
*)

PROCEDURE SetNamedOptionBoolean (tokno: CARDINAL;
                                 name: String;
                                 value: BOOLEAN) ;
BEGIN
   IF NOT SetNamedOptionAggregateBoolean (tokno, name, value)
   THEN
      SetNamedOptionVariableBoolean (tokno, name, value)
   END
END SetNamedOptionBoolean ;


(*
   SetNamedOptionVariableBoolean -
*)

PROCEDURE SetNamedOptionVariableBoolean (tokno: CARDINAL;
                                         name: String;
                                         value: BOOLEAN) ;
VAR
   optionName: String ;
   specified,
   actual    : Options ;
   invert,
   prev      : BOOLEAN ;
BEGIN
   DebugFilterName (tokno, "set", name) ;
   specified := GetOptionBlockForScope (tokno, name) ;
   IF specified = NIL
   THEN
      MetaErrorT0 (tokno, 'unknown scope specified in the qualident')
   ELSE
      actual := LookupScopeForName (specified, name) ;
      IF actual = NIL
      THEN
         MetaErrorT0 (tokno, 'pragma qualident is unknown') ;
         OutS (" scope not found\n", NIL)
      ELSE
         invert := IsInvertOptionBoolean (name) ;
         IF invert
         THEN
            value := NOT value
         END ;
         optionName := MakeOptionName (name) ;
         Assert (GetOptionAvailable (actual, optionName)) ;
         prev := GetNamedOptionBoolean (actual, optionName) ;
         (* DebugBoolean (tokno, 'SetNamedOptionBoolean actual', actual, prev, FALSE, name) ; *)
         IF prev # value
         THEN
            (* Changing an existing value.  *)
            IF IsOptionConflict (tokno, actual, optionName)
            THEN
               (* Diff pragma found.  *)
               MetaErrorT0 (tokno, 'command line option conflicts with source pragma')
            ELSE
               SetOptionBoolean (tokno, actual, specified, optionName, value) ;
               specified := GetOptionBlockForScope (tokno, name) ;
               actual := LookupScopeForName (specified, name) ;
               prev := GetNamedOptionBoolean (actual, optionName) ;
               Assert (prev = value) ;
               IF value
               THEN
                  OutS (" found and set to TRUE\n", NIL)
               ELSE
                  OutS (" found and set to FALSE\n", NIL)
               END ;
               DebugFilterName (tokno, "set finished", name)
            END
         END
      END
   END
END SetNamedOptionVariableBoolean ;


(*
   GetBooleanFromArray -
*)

PROCEDURE GetBooleanFromArray (tokno: CARDINAL; domain, scope: String;
                               name: ARRAY OF CHAR) : BOOLEAN ;
VAR
   full  : String ;
   result: BOOLEAN ;
BEGIN
   full := Dup (domain) ;
   full := ConCatChar (full, ".") ;
   full := ConCat (full, scope) ;
   full := ConCatChar (full, ".") ;
   full := ConCat (full, Mark (InitString (name))) ;
   result := GetNamedOptionVariableBoolean (tokno, full) ;
   full := KillString (full) ;
   RETURN result
END GetBooleanFromArray ;


(*
   GetOptionBoolean - return the boolean value associated with option name.
*)

PROCEDURE GetOptionBoolean (tokno: CARDINAL; name: String) : BOOLEAN ;
VAR
   result: BOOLEAN ;
BEGIN
   IF GetNamedOptionAggregateBoolean (tokno, name, result)
   THEN
      RETURN result
   ELSE
      RETURN GetNamedOptionVariableBoolean (tokno, name)
   END
END GetOptionBoolean ;


(*
   IsAggregateOptionBoolean - return TRUE if name is an aggregate option.
*)

PROCEDURE IsAggregateOptionBoolean (name: String) : BOOLEAN ;
VAR
   optionName: String ;
   result    : BOOLEAN ;
BEGIN
   optionName := MakeOptionName (name) ;
   result := EqualArray (optionName, 'gm2.option.fsoft_check_all') ;
   optionName := KillString (optionName) ;
   RETURN result
END IsAggregateOptionBoolean ;


(*
   GetNamedOptionAggregateBoolean -
*)

PROCEDURE GetNamedOptionAggregateBoolean (tokno: CARDINAL; name: String;
                                          VAR result: BOOLEAN) : BOOLEAN ;
VAR
   found     : BOOLEAN ;
   scope,
   domain,
   qualident,
   optionName: String ;
BEGIN
   optionName := MakeOptionName (name) ;
   domain := NIL ;
   scope := NIL ;
   qualident := NIL ;
   result := FALSE ;
   found := FALSE ;
   IF EqualArray (optionName, 'gm2.option.fsoft_check_all')
   THEN
      domain := ExtractDomain (name) ;
      scope := ExtractScope (name) ;
      qualident := ExtractQualident (name) ;
      result := GetBooleanFromArray (tokno, domain, scope, 'fcase') AND
                GetBooleanFromArray (tokno, domain, scope, 'ffloatvalue') AND
                GetBooleanFromArray (tokno, domain, scope, 'findex') AND
                GetBooleanFromArray (tokno, domain, scope, 'fnil') AND
                GetBooleanFromArray (tokno, domain, scope, 'frange') AND
                GetBooleanFromArray (tokno, domain, scope, 'freturn') AND
                GetBooleanFromArray (tokno, domain, scope, 'fwholevalue') AND
                GetBooleanFromArray (tokno, domain, scope, 'fwholediv') ;
      found := TRUE
   END ;
   optionName := KillString (optionName) ;
   domain := KillString (domain) ;
   scope := KillString (scope) ;
   qualident := KillString (qualident) ;
   RETURN found
END GetNamedOptionAggregateBoolean ;


(*
   GetNamedOptionVariableBoolean -
*)

PROCEDURE GetNamedOptionVariableBoolean (tokno: CARDINAL; name: String) : BOOLEAN ;
VAR
   cur       : Options ;
   cb        : OptionBoolean ;
   result,
   invert    : BOOLEAN ;
   optionName: String ;
BEGIN
   DebugFilterName (tokno, "get", name) ;
   optionName := NIL ;
   cur := GetOptionBlockForScope (tokno, name) ;
   IF cur = NIL
   THEN
      MetaErrorStringT0 (tokno, ConCat (ConCat (InitString ('{%E} %{kBOOLEAN} option '),
                                                name), InitString (' does not exist')))
   ELSE
      invert := IsInvertOptionBoolean (name) ;
      optionName := MakeOptionName (name) ;
      (* Search outer scopes.  *)
      WHILE cur # NIL DO
         IF cur^.BooleanTree = NIL
         THEN
            cur := cur^.next
         ELSE
            cb := BinDict.Get (cur^.BooleanTree, optionName) ;
            IF cb = NIL
            THEN
               cur := cur^.next
            ELSE
               (* DebugBoolean (tokno, 'GetOptionBoolean', cur, cb^.Value, invert, optionName) ; *)
               cb^.Used := TRUE ;
               INC (cb^.ReadCount) ;
               optionName := KillString (optionName) ;
               (* --fixme-- implement cb^.Value / invert xor in mc.  *)
               IF invert
               THEN
                  result := NOT cb^.Value
               ELSE
                  result := cb^.Value
               END ;
               IF result
               THEN
                  OutS (" variable found as TRUE\n", NIL)
               ELSE
                  OutS (" variable found as FALSE\n", NIL)
               END ;
               RETURN result
            END
         END
      END
   END ;
   (* MetaErrorT0 (tokno, '{%O}(odd the boolean was not seen)') ; *)
   optionName := KillString (optionName) ;
   OutS (" variable not found\n", NIL) ;
   RETURN FALSE
END GetNamedOptionVariableBoolean ;


(*
   GetCacheOptionBoolean - look up the boolean matching name from
                           the appropiate scope.
                           It updates the read count and used flag.
*)

PROCEDURE GetCacheOptionBoolean (tokno: CARDINAL; name: String;
                                 VAR cb: OptionBoolean) : BOOLEAN ;
VAR
   cur       : Options ;
   optionName: String ;
BEGIN
   cur := LookupScopeForName (CurrentOption, name) ;
   optionName := MakeOptionName (name) ;
   cb := BinDict.Get (cur^.BooleanTree, optionName) ;
   IF cb = NIL
   THEN
      MetaErrorT0 (tokno, 'pragma qualident does not exist') ;
      optionName := KillString (optionName)
   ELSE
      cb^.Used := TRUE ;
      INC (cb^.ReadCount) ;
      RETURN cb^.Value
   END ;
   RETURN FALSE
END GetCacheOptionBoolean ;


(*
   GetCFlag - returns the boolean indicating -c is active.
*)

PROCEDURE GetCFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.c^.Name,
                                 CurrentOption^.gm2.option.c)
END GetCFlag ;


(*
   SetCFlag - attempts to set the boolean indicating -c is active or inactive.
*)

PROCEDURE SetCFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.c^.Name, on)
END SetCFlag ;


(*
   RegisterOptions - initialize the binary dictionary trees and register
                     all user options.
*)

PROCEDURE RegisterOptions (opt: Options) ;
BEGIN
   RegisterOptionsBoolean (opt) ;
   RegisterOptionsInteger (opt) ;
   RegisterOptionsString (opt)
END RegisterOptions ;


(*
   NoDelete - a nop call back.
*)

PROCEDURE NoDelete (adr: ADDRESS <* unused *> ) ;
END NoDelete ;


(*
   StrCmp - return -1 if a < b
                    0 if a = b
                   +1 if a > b.
*)

PROCEDURE StrCmp (a, b: String) : INTEGER ;
VAR
   i, lengthA, lengthB: CARDINAL ;
BEGIN
   i := 0 ;
   lengthA := Length (a) ;
   lengthB := Length (b) ;
   WHILE (i < lengthA) AND (i < lengthB) DO
      IF char (a, i) < char (b, i)
      THEN
         RETURN -1
      ELSIF char (a, i) > char (b, i)
      THEN
         RETURN +1
      ELSE
         INC (i)
      END
   END ;
   IF lengthA < lengthB
   THEN
      RETURN -1
   ELSIF lengthA > lengthB
   THEN
      RETURN +1
   ELSE
      RETURN 0
   END
END StrCmp ;


(*
   RegisterOptionsBoolean - construct a binary tree containing each boolean option
                            and set up the cached value.
*)

PROCEDURE RegisterOptionsBoolean (opt: Options) ;
BEGIN
   InitBooleanTree (opt) ;
   (* Boolean flags are: Value, ReadOnly, WriteOnce, WriteAfterRead.  *)
   opt^.gm2.option.Wcase_enum := CacheOptionBoolean (opt, 'gm2.option.Wcase_enum',
                                                     FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.Wpedantic_param_names := CacheOptionBoolean (opt, 'gm2.option.Wpedantic_param_names',
                                                                FALSE, FALSE, FALSE, TRUE) ;

   opt^.gm2.option.Wuninit_variable_checking := CacheOptionBoolean (opt, 'gm2.option.Wuninit_variable_checking',
                                                                    FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.Wuninit_variable_conditional_checking := CacheOptionBoolean (opt, 'gm2.option.Wuninit_variable_conditional_checking',
                                                                                FALSE, FALSE, FALSE, TRUE) ;

   opt^.gm2.option.Wpedantic_cast := CacheOptionBoolean (opt, 'gm2.option.Wpedantic_cast',
                                                         FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.Wverbose_unbounded := CacheOptionBoolean (opt, 'gm2.option.Wverbose_unbounded',
                                                             FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.Wstyle := CacheOptionBoolean (opt, 'gm2.option.Wstyle',
                                                 FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fauto_init := CacheOptionBoolean (opt, 'gm2.option.fauto_init',
                                                     FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fbounds := CacheOptionBoolean (opt, 'gm2.option.fbounds',
                                                  FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fcase := CacheOptionBoolean (opt, 'gm2.option.fcase',
                                                FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fcpp := CacheOptionBoolean (opt, 'gm2.option.fcpp',
                                               FALSE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.fd := CacheOptionBoolean (opt, 'gm2.option.fd', FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fdebug_builtins := CacheOptionBoolean (opt, 'gm2.option.fdebug_builtins',
                                                          FALSE, FALSE, FALSE, TRUE) ;
   (* _fdef= string option.  *)
   opt^.gm2.option.fdump_system_exports := CacheOptionBoolean (opt, 'gm2.option.fdump_system_exports',
                                                               FALSE, FALSE, FALSE, FALSE) ;
   opt^.gm2.option.fdump_builtins := CacheOptionBoolean (opt, 'gm2.option.fdump_builtins',
                                                               FALSE, FALSE, FALSE, FALSE) ;
   opt^.gm2.option.fextended_opaque := CacheOptionBoolean (opt, 'gm2.option.fextended_opaque',
                                                           FALSE, FALSE, FALSE, FALSE) ;
   opt^.gm2.option.ffloatvalue := CacheOptionBoolean (opt, 'gm2.option.ffloatvalue',
                                                      FALSE, FALSE, FALSE, TRUE) ;
   (* _fgen_module_list= string option.  *)
   opt^.gm2.option.fgen_module_list := CacheOptionBoolean (opt, 'gm2.option.fgen_module_list',
                                                           FALSE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.findex := CacheOptionBoolean (opt, 'gm2.option.findex',
                                                 FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fiso := CacheOptionBoolean (opt, 'gm2.option.fiso',
                                               FALSE, FALSE, FALSE, TRUE) ;
   (* -fforward is not available to users on the command line yet.  *)
   opt^.gm2.option.fforward := CacheOptionBoolean (opt, 'gm2.option.fforward',
                                                   TRUE, FALSE, FALSE, TRUE) ;
   (* loads of string options omitted.  *)
   opt^.gm2.option.fm2_g := CacheOptionBoolean (opt, 'gm2.option.fm2_g',
                                                FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fm2_lower_case := CacheOptionBoolean (opt, 'gm2.option.fm2_lower_case',
                                                         FALSE, FALSE, FALSE, TRUE) ;
   (* fm2_pathname= string.  *)
   (* fm2_pathname_root= string.  *)
   (* fm2_prefix= string.  *)
   opt^.gm2.option.fm2_statistics := CacheOptionBoolean (opt, 'gm2.option.fm2_statistics',
                                                         FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fm2_strict_type := CacheOptionBoolean (opt, 'gm2.option.fm2_strict_type',
                                                         FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fm2_strict_type_reason := CacheOptionBoolean (opt, 'gm2.option.fm2_strict_type_reason',
                                                                 FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fm2_whole_program := CacheOptionBoolean (opt, 'gm2.option.fm2_whole_program',
                                                            FALSE, FALSE, TRUE, TRUE) ;
   opt^.gm2.option.fmem_report := CacheOptionBoolean (opt, 'gm2.option.fmem_report',
                                                      FALSE, FALSE, FALSE, TRUE) ;
   (* fmod= string.  *)
   opt^.gm2.option.fnil := CacheOptionBoolean (opt, 'gm2.option.fnil',
                                               FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fpim := CacheOptionBoolean (opt, 'gm2.option.fpim', TRUE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fpim2 := CacheOptionBoolean (opt, 'gm2.option.fpim2', FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fpim3 := CacheOptionBoolean (opt, 'gm2.option.fpim3', FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fpim4 := CacheOptionBoolean (opt, 'gm2.option.fpim4', TRUE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fpositive_mod_floor_div :=
      CacheOptionBoolean (opt, 'gm2.option.fpositive_mod_floor_div',
                          FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fpthread := CacheOptionBoolean (opt, 'gm2.option.fpthread',
                                                   FALSE, FALSE, FALSE, FALSE) ;
   opt^.gm2.option.fq := CacheOptionBoolean (opt, 'gm2.option.fq',
                                             FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.frange := CacheOptionBoolean (opt, 'gm2.option.frange',
                                                 FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.freturn := CacheOptionBoolean (opt, 'gm2.option.freturn',
                                                 FALSE, FALSE, FALSE, TRUE) ;
   (* fruntime_modules= string.  *)
   opt^.gm2.option.fscaffold_dynamic := CacheOptionBoolean (opt, 'gm2.option.fscaffold_dynamic',
                                                            FALSE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.fscaffold_c := CacheOptionBoolean (opt, 'gm2.option.fscaffold_c',
                                                      FALSE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.fscaffold_cpp := CacheOptionBoolean (opt, 'gm2.option.fscaffold_cpp',
                                                        FALSE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.fscaffold_main := CacheOptionBoolean (opt, 'gm2.option.fscaffold_main',
                                                         FALSE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.fscaffold_static := CacheOptionBoolean (opt, 'gm2.option.fscaffold_static',
                                                           FALSE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.fshared := CacheOptionBoolean (opt, 'gm2.option.fshared',
                                                  FALSE, TRUE, FALSE, FALSE) ;
   (* This option should be derived.  *)
   (* opt^.gm2.option.fsoft_check_all := *) (* *) ;  (* --fixme-- *)
   opt^.gm2.option.fsources := CacheOptionBoolean (opt, 'gm2.option.fsources',
                                                   FALSE, FALSE, TRUE, TRUE) ;
   opt^.gm2.option.fswig := CacheOptionBoolean (opt, 'gm2.option.fswig',
                                                FALSE, FALSE, TRUE, TRUE) ;
   opt^.gm2.option.ftime_report := CacheOptionBoolean (opt, 'gm2.option.ftime_report',
                                                       FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.funbounded_by_reference := CacheOptionBoolean (opt, 'gm2.option.funbounded_by_reference',
                                                                  FALSE, FALSE, FALSE, FALSE) ;
   (* fuse_list= string.  *)
   (*
   opt^.gm2.option.fversion := CacheOptionBoolean (opt, 'gm2.option.fversion',
                                                   ?, ?, ?) ;
   *)
   opt^.gm2.option.fwideset := CacheOptionBoolean (opt, 'gm2.option.fwideset',
                                                   TRUE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.fwholediv := CacheOptionBoolean (opt, 'gm2.option.fwholediv',
                                                    FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.fwholevalue := CacheOptionBoolean (opt, 'gm2.option.fwholevalue',
                                                    FALSE, FALSE, FALSE, TRUE) ;

   (* opt^.gm2.option.Wall := --fixme *)  (* derived.  *) ;
   opt^.gm2.option.Wpedantic := CacheOptionBoolean (opt, 'gm2.option.Wpedantic',
                                                    FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.Wreturn_type := CacheOptionBoolean (opt, 'gm2.option.Wreturn_type',
                                                    FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.Wunused_variable := CacheOptionBoolean (opt, 'gm2.option.Wunused_variable',
                                                           FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.Wunused_parameter := CacheOptionBoolean (opt, 'gm2.option.Wunused_parameter',
                                                           FALSE, FALSE, FALSE, TRUE) ;
   opt^.gm2.option.c := CacheOptionBoolean (opt, 'gm2.option.c', FALSE, TRUE, FALSE, FALSE) ;
   opt^.gm2.option.fexceptions := CacheOptionBoolean (opt, 'gm2.option.fexceptions',
                                                      TRUE, FALSE, FALSE, FALSE) ;
   opt^.gm2.option.g := CacheOptionBoolean (opt, 'gm2.option.g', FALSE, FALSE, FALSE, FALSE) ;
   (* I string.  *)
   opt^.gm2.option.v := CacheOptionBoolean (opt, 'gm2.option.v', FALSE, FALSE, TRUE, TRUE) ;
   opt^.gm2.option.fm2_quiet := CacheOptionBoolean (opt, 'gm2.option.fm2_quiet', TRUE, FALSE, FALSE, TRUE)
END RegisterOptionsBoolean ;


(*
   RegisterOptionsInteger - construct a binary tree containing each integer option
                            and set up the cached value.
*)

PROCEDURE RegisterOptionsInteger (opt: Options) ;
BEGIN
   InitIntegerTree (opt) ;
   opt^.gcc.option.O := CacheOptionInteger (opt, 'gcc.option.O', 0, FALSE, TRUE, FALSE) ;
   (* --fixme-- complete this list.  *)
END RegisterOptionsInteger ;


(*
   RegisterOptionsString - construct a binary tree containing each string option
                           and set up the cached value.
*)

PROCEDURE RegisterOptionsString (opt: Options) ;
BEGIN
   opt^.StringTree := BinDict.Init (StrCmp, NoDelete, NoDelete) ;
   (* --fixme-- complete this list.  *)
END RegisterOptionsString ;


(*
   FailIntegerWriteOnce - return TRUE if bl will fail the write once restriction.
*)

PROCEDURE FailIntegerWriteOnce (bl: OptionInteger; value: INTEGER) : BOOLEAN ;
BEGIN
   RETURN bl^.WriteOnce AND (bl^.Value # value) AND (bl^.WriteCount >= 1)
END FailIntegerWriteOnce ;


(*
   SetInteger -
*)

PROCEDURE SetInteger (opt: Options; bl: OptionInteger; tokno: CARDINAL; value: INTEGER) ;
BEGIN
   IF bl^.Value # value
   THEN
      (* The value is being changed.  *)
      IF (tokno # CommandTokenNo) AND (opt^.ReadOnly OR bl^.ReadOnly)
      THEN
         MetaErrorT0 (tokno, '{%E}integer command line option can only be read by a pragma')
      ELSIF FailIntegerWriteOnce (bl, value)
      THEN
         MetaErrorT0 (tokno, '{%E}integer option can only be written once') ;
         MetaErrorT0 (bl^.LastTok, '{%E}integer command line option was previously set')
(*
      ELSIF (bl^.ReadCount > 0) AND (NOT bl^.WriteAfter)
      THEN
         MetaErrorT0 (tokno, '{%E}integer option cannot be modified after it has been read')
*)
      ELSE
         opt^.Modified := TRUE ;
         bl^.Value := value ;
         INC (bl^.WriteCount)
      END ;
      bl^.LastTok := tokno
   END
END SetInteger ;


(*
   InitIntegerTree - ensure that the boolean tree has been initialized.
*)

PROCEDURE InitIntegerTree (opt: Options) ;
BEGIN
   IF opt^.IntegerTree = NIL
   THEN
      opt^.IntegerTree := BinDict.Init (StrCmp, NoDelete, NoDelete)
   END
END InitIntegerTree ;


(*
   SetOptionInteger -
*)

PROCEDURE SetOptionInteger (tokno: CARDINAL; prev, next: Options; name: String;
                            value: INTEGER) ;
VAR
   valueNode, newb: OptionInteger ;
BEGIN
   InitIntegerTree (next) ;
   valueNode := BinDict.Get (prev^.IntegerTree, name) ;
   IF valueNode = NIL
   THEN
      MetaErrorT0 (tokno, '{%E}unknown integer command line option')
   ELSIF prev = next
   THEN
      SetInteger (next, valueNode, tokno, value)
   ELSE
      newb := DupLocalInt (tokno, next, valueNode) ;
      SetInteger (next, newb, tokno, value) ;
      BinDict.Insert (next^.IntegerTree, name, newb) ;
      IF next = CommandLineOption
      THEN
         Assert (prev = DefaultOption) ;
         (* And set the same default variable name.  *)
         SetInteger (prev, valueNode, tokno, value)
      END
   END
END SetOptionInteger ;


(*
   NewInt -
*)

PROCEDURE NewInt (opt: Options) : OptionInteger ;
VAR
   oi: OptionInteger ;
BEGIN
   IF IntFreeList = NIL
   THEN
      NEW (oi)
   ELSE
      oi := IntFreeList ;
      IntFreeList := IntFreeList^.Next
   END ;
   oi^.Next := opt^.chain.Int.head ;
   opt^.chain.Int.head := oi ;
   IF opt^.chain.Int.tail = NIL
   THEN
      opt^.chain.Int.tail := oi
   END ;
   RETURN oi
END NewInt ;


(*
   InitInteger - return a OptionInteger in an initial state with value.
*)

PROCEDURE InitInteger (tokno: CARDINAL; opt: Options; name: String;
                       value: INTEGER; readonly, writeonce, writeafter: BOOLEAN) : OptionInteger ;
VAR
   int: OptionInteger ;
BEGIN
   int := NewInt (opt) ;
   WITH int^ DO
      Name := name ;
      Used := FALSE ;
      Value := value ;
      WriteCount := 0 ;
      ReadCount := 0 ;
      ReadOnly := readonly ;
      WriteOnce := writeonce ;
      WriteAfter := writeafter ;
      LastTok := tokno
   END ;
   RETURN int
END InitInteger ;


(*
   CacheOptionInteger - returns an OptionInteger which has been registered
                        in opt and contains value.
*)

PROCEDURE CacheOptionInteger (opt: Options; name: ARRAY OF CHAR;
                              value: INTEGER;
                              readonly, writeonce, writeafter: BOOLEAN) : OptionInteger ;
VAR
   oi : OptionInteger ;
   str: String ;
   key: Name ;
BEGIN
   str := InitString (name) ;
   oi := InitInteger (CommandTokenNo, opt, str, value,
                      readonly, writeonce, writeafter) ;
   BinDict.Insert (opt^.IntegerTree, str, oi) ;
   IF GetDumpPragmas (BuiltinTokenNo)
   THEN
      key := MakeKey (name) ;
      printf2 ('Supported pragma qualident %a is an INTEGER with an initial value %d\n', key, value)
   END ;
   RETURN oi
END CacheOptionInteger ;


(*
   GetRegisteredOptionInteger -
*)

PROCEDURE GetRegisteredOptionInteger (tokno: CARDINAL;
                                      opt: Options; name: String) : INTEGER ;
VAR
   valueNode: OptionInteger ;
BEGIN
   valueNode := BinDict.Get (opt^.IntegerTree, name) ;
   IF valueNode = NIL
   THEN
      IF opt^.next = NIL
      THEN
         (* Not found the option.  *)
         IF tokno = UnknownTokenNo
         THEN
            InternalError ('expecting this option to be registered')
         ELSE
            MetaErrorT0 (tokno, 'option does not exist')
         END ;
         RETURN 0
      ELSE
         RETURN GetRegisteredOptionInteger (tokno, opt^.next, name)
      END
   ELSE
      valueNode^.Used := TRUE ;
      INC (valueNode^.ReadCount) ;
      RETURN valueNode^.Value
   END
END GetRegisteredOptionInteger ;


(*
   GetOptimizing - get the optimization level.
*)

PROCEDURE GetOptimizing (tokno: CARDINAL) : INTEGER ;
BEGIN
   RETURN GetCacheOptionInteger (tokno,
                                 DefaultOption^.gcc.option.O^.Name,
                                 CurrentOption^.gcc.option.O)
END GetOptimizing ;


(*
   GetNamedOptionInteger - returns the integer value associated with name.
                           It assumes the binary tree contains name.
*)

PROCEDURE GetNamedOptionInteger (opt: Options; name: String) : INTEGER ;
VAR
   ob: OptionInteger ;
BEGIN
   InitIntegerTree (opt) ;
   ob := BinDict.Get (opt^.IntegerTree, name) ;
   ob^.Used := TRUE ;
   INC (ob^.ReadCount) ;
   RETURN ob^.Value
END GetNamedOptionInteger ;


(*
   SetNamedOptionInteger - attempt to set the named integer to value.
                           If the named integer does not exist or conflicts
                           then a user error is generated at tokno.
*)

PROCEDURE SetNamedOptionInteger (tokno: CARDINAL;
                                 name: String;
                                 value: INTEGER) ;
VAR
   optionName: String ;
   specified,
   actual    : Options ;
   prev      : INTEGER ;
BEGIN
   specified := GetOptionBlockForScope (tokno, name) ;
   IF specified = NIL
   THEN
      MetaErrorT0 (tokno, 'the scope specified in the qualident unknown')
   ELSE
      actual := LookupScopeForName (specified, name) ;
      IF actual # NIL
      THEN
         optionName := MakeOptionName (name) ;
         Assert (GetOptionAvailable (actual, optionName)) ;
         prev := GetNamedOptionInteger (actual, optionName) ;
         IF prev # value
         THEN
            (* Changing an existing value.  *)
            IF actual^.ReadOnly OR IsOptionConflict (tokno, actual, optionName)
            THEN
               (* Diff pragma found.  *)
               MetaErrorT0 (tokno, 'command line option conflicts with source pragma')
            ELSE
               SetOptionInteger (tokno, actual, specified, optionName, value)
            END
         END
      ELSE
         MetaErrorT0 (tokno, 'option is unknown')
      END
   END
END SetNamedOptionInteger ;


(*
   GetOptionInteger - return the integer value associated with option name.
*)

PROCEDURE GetOptionInteger (tokno: CARDINAL; name: String) : INTEGER ;
VAR
   cur       : Options ;
   cb        : OptionInteger ;
   optionName: String ;
BEGIN
   optionName := NIL ;
   cur := GetOptionBlockForScope (tokno, name) ;
   IF cur = NIL
   THEN
      MetaErrorStringT0 (tokno, ConCat (ConCat (InitString ('{%E} %{kINTEGER} option '),
                                                name), InitString (' does not exist')))
   ELSE
      optionName := MakeOptionName (name) ;
      (* Search outer scopes.  *)
      WHILE cur # NIL DO
         cb := BinDict.Get (cur^.IntegerTree, optionName) ;
         IF cb = NIL
         THEN
            cur := cur^.next
         ELSE
            cb^.Used := TRUE ;
            INC (cb^.ReadCount) ;
            optionName := KillString (optionName) ;
            RETURN cb^.Value
         END
      END
   END ;
   optionName := KillString (optionName) ;
   RETURN 0
END GetOptionInteger ;


(*
   GetCacheOptionInteger - if cb is NIL then look up the integer
                           matching name from the appropiate scope.
                           It updates the read count and used flag.
*)

PROCEDURE GetCacheOptionInteger (tokno: CARDINAL;
                                 name: String;
                                 VAR cb: OptionInteger) : INTEGER ;
VAR
   ob        : OptionInteger ;
   cur       : Options ;
   optionName: String ;
BEGIN
   IF cb = NIL
   THEN
      cur := GetOptionBlockForScope (tokno, name) ;
      IF cur = NIL
      THEN
         MetaErrorT0 (tokno, '{%E}unknown integer command line option')
      ELSE
         cur := LookupScopeForName (cur, name) ;
         optionName := MakeOptionName (name) ;
         cb := BinDict.Get (cur^.IntegerTree, optionName) ;
         IF cb = NIL
         THEN
            MetaErrorT0 (tokno, 'option qualident does not exist') ;
            optionName := KillString (optionName)
         ELSIF cur # CurrentOption
         THEN
            InitIntegerTree (CurrentOption) ;
            (* Check if value is present in CurrentOption.  *)
            ob := BinDict.Get (CurrentOption^.IntegerTree, optionName) ;
            IF ob = NIL
            THEN
               cb := DupLocalInt (tokno, CurrentOption, cb) ;
               BinDict.Insert (CurrentOption^.IntegerTree, optionName, cb)
            ELSE
               cb := ob
            END
         END
      END
   END ;
   IF cb = NIL
   THEN
      RETURN 0
   ELSE
      cb^.Used := TRUE ;
      INC (cb^.ReadCount) ;
      RETURN cb^.Value
   END
END GetCacheOptionInteger ;


(*
   SetOptimizing - set the value representing -Ovalue.
*)

PROCEDURE SetOptimizing (value: INTEGER) ;
BEGIN
   SetNamedOptionInteger (CommandTokenNo,
                          DefaultOption^.gcc.option.O^.Name,
                          value)
END SetOptimizing ;


(*
   GetOptimizeBasicBlock - return TRUE if front end basic block optimization
                           should be applied.
*)

PROCEDURE GetOptimizeBasicBlock (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetOptimizing (tokno) > 0
END GetOptimizeBasicBlock ;


(*
   GetOptimizeUncalledProcedures - return TRUE if front end uncalled procedures
                                   should be removed.
*)

PROCEDURE GetOptimizeUncalledProcedures (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetOptimizing (tokno) > 0
END GetOptimizeUncalledProcedures ;


(*
   GetOptimizeCommonSubExpressions - return TRUE if the front end should remove
                                     common sub expressions.
*)

PROCEDURE GetOptimizeCommonSubExpressions (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetOptimizing (tokno) > 0
END GetOptimizeCommonSubExpressions ;


(*
   GetOptimizeSets - return TRUE if the front end should optimize large
                     set by inlining trees.
*)

PROCEDURE GetOptimizeSets (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetOptimizing (tokno) > 0
END GetOptimizeSets ;


(*
   SetStatisticsFlag - set the fm2_statistics flag.
*)

PROCEDURE SetStatisticsFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_statistics^.Name, on)
END SetStatisticsFlag ;


(*
   GetStatisticsFlag - return the value of the fm2_statistics flag.
*)

PROCEDURE GetStatisticsFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_statistics^.Name,
                                 CurrentOption^.gm2.option.fm2_statistics)
END GetStatisticsFlag ;


(*
   SetExtendedOpaque - sets the ExtendedOpaque flag.
*)

PROCEDURE SetExtendedOpaque (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fextended_opaque^.Name, on)
END SetExtendedOpaque ;


(*
   GetExtendedOpaque - returns the value of the -fextended-opaque flag.
*)

PROCEDURE GetExtendedOpaque (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fextended_opaque^.Name,
                                 CurrentOption^.gm2.option.fextended_opaque)
END GetExtendedOpaque ;


(*
   SetDebugBuiltinFlag - sets the -fdebug-builtins flag.
*)

PROCEDURE SetDebugBuiltinFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fdebug_builtins^.Name, on)
END SetDebugBuiltinFlag ;


(*
   GetDebugBuiltinFlag - returns the value of -fdebug-builtins.
*)

PROCEDURE GetDebugBuiltinFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fdebug_builtins^.Name,
                                 CurrentOption^.gm2.option.fdebug_builtins)
END GetDebugBuiltinFlag ;


(*
   SetScaffoldDynamicFlag - set the -fscaffold-dynamic flag.
*)

PROCEDURE SetScaffoldDynamicFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fscaffold_dynamic^.Name, on) ;
   IF on
   THEN
      SetNamedOptionBoolean (CommandTokenNo,
                             DefaultOption^.gm2.option.fscaffold_static^.Name, FALSE)
   END
END SetScaffoldDynamicFlag ;


(*
   GetScaffoldDynamicFlag - returns the value of the -fscaffold-dynamic flag.
*)

PROCEDURE GetScaffoldDynamicFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fscaffold_dynamic^.Name,
                                 CurrentOption^.gm2.option.fscaffold_dynamic)
END GetScaffoldDynamicFlag ;


(*
   SetISO - set the -fiso flag.
*)

PROCEDURE SetISO (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fiso^.Name, on) ;
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim^.Name, NOT on) ;
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim2^.Name, NOT on)
   (* Pim4 is the default, leave it alone since the DIV and MOD rules are the
      same as ISO.  *)
END SetISO ;


(*
   GetISO - get the -fiso flag.
*)

PROCEDURE GetISO (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fiso^.Name,
                                 CurrentOption^.gm2.option.fiso)
END GetISO ;


(*
   GetVariantValueFlag - return the internal flag indicating whether
                         variant fields should be checked.  Currently
                         this is force enabled if -fiso flag is active.
                         It might be useful to allow user access in the
                         future.  --hint--.
*)

PROCEDURE GetVariantValueFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetISO (tokno)
END GetVariantValueFlag ;


(*
   SetPIM - set the -fpim flag.
*)

PROCEDURE SetPIM (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim^.Name, on) ;
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fiso^.Name, NOT on)
END SetPIM ;


(*
   GetPIM - get the -fpim flag.
*)

PROCEDURE GetPIM (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fpim^.Name,
                                 CurrentOption^.gm2.option.fpim)
END GetPIM ;


(*
   SetPIM2 - set the -fpim2 flag.
*)

PROCEDURE SetPIM2 (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim^.Name, on) ;
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim2^.Name, on) ;
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fiso^.Name, NOT on) ;
   IF on
   THEN
      (* Pim4 is the default, turn it off.  *)
      SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim4^.Name, FALSE)
   END
END SetPIM2 ;


(*
   GetPIM2 - get the -fpim2 flag.
*)

PROCEDURE GetPIM2 (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fpim2^.Name,
                                 CurrentOption^.gm2.option.fpim2)
END GetPIM2 ;


(*
   SetPIM3 - set the -fpim3 flag.
*)

PROCEDURE SetPIM3 (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim^.Name, on) ;
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim3^.Name, on) ;
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fiso^.Name, NOT on) ;
   IF on
   THEN
      (* Pim4 is the default, turn it off.  *)
      SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpim4^.Name, FALSE)
   END
END SetPIM3 ;


(*
   GetPIM3 - get the -fpim3 flag.
*)

PROCEDURE GetPIM3 (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fpim3^.Name,
                                 CurrentOption^.gm2.option.fpim3)
END GetPIM3 ;


(*
   SetPIM4 - set the -fpim4 flag.
*)

PROCEDURE SetPIM4 (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fpim^.Name,
                          on) ;
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fpim4^.Name,
                          on) ;
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fiso^.Name,
                          NOT on) ;
END SetPIM4 ;


(*
   GetPIM4 - get the -fpim4 flag.
*)

PROCEDURE GetPIM4 (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fpim4^.Name,
                                 CurrentOption^.gm2.option.fpim4)
END GetPIM4 ;


(*
   SetDumpSystemExports - set the -fdump-system-exports flag.
*)

PROCEDURE SetDumpSystemExports (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fdump_system_exports^.Name,
                          on)
END SetDumpSystemExports ;


(*
   GetDumpSystemExports - get the -fdump-system-exports flag.
*)

PROCEDURE GetDumpSystemExports (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fdump_system_exports^.Name,
                                 CurrentOption^.gm2.option.fdump_system_exports)
END GetDumpSystemExports ;


(*
   SetDumpBuiltins - set the value of -fdump-builtins.
*)

PROCEDURE SetDumpBuiltins (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fdump_builtins^.Name,
                          on)
END SetDumpBuiltins ;


(*
   GetDumpBuiltins - return the value of -fdump-builtins.
*)

PROCEDURE GetDumpBuiltins (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fdump_builtins^.Name,
                                 CurrentOption^.gm2.option.fdump_builtins)
END GetDumpBuiltins ;


(*
   SetDumpPragmas - set the -fdump-pragmas flag.
*)

PROCEDURE SetDumpPragmas (on: BOOLEAN) ;
BEGIN
   DumpPragmas := on
END SetDumpPragmas ;


(*
   GetDumpPragmas - get the -fdump-pragmas flag.
*)

PROCEDURE GetDumpPragmas (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN DumpPragmas
END GetDumpPragmas ;


(*
   SetWholeProgramFlag - set the fm2-whole-program flag.
*)

PROCEDURE SetWholeProgramFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_whole_program^.Name,
                          on)
END SetWholeProgramFlag ;


(*
   GetWholeProgramFlag - return the value of the fm2-whole-program flag.
*)

PROCEDURE GetWholeProgramFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_whole_program^.Name,
                                 CurrentOption^.gm2.option.fm2_whole_program)
END GetWholeProgramFlag ;


(*
   SetScaffoldMainFlag - set the fscaffold_main flag.
*)

PROCEDURE SetScaffoldMainFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fscaffold_main^.Name,
                          on)
END SetScaffoldMainFlag ;


(*
   GetScaffoldMainFlag - return the value of the fscaffold_main flag.
*)

PROCEDURE GetScaffoldMainFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fscaffold_main^.Name,
                                 CurrentOption^.gm2.option.fscaffold_main)
END GetScaffoldMainFlag ;


(*
   SetCaseEnumFlag - set the Wcase_enum flag.
*)

PROCEDURE SetCaseEnumFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.Wcase_enum^.Name, on)
END SetCaseEnumFlag ;


(*
   GetCaseEnumFlag - return the value of the Wcase_enum flag.
*)

PROCEDURE GetCaseEnumFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wcase_enum^.Name,
                                 CurrentOption^.gm2.option.Wcase_enum)
END GetCaseEnumFlag ;


(*
   SetStyleFlag - set the Wstyle flag.
*)

PROCEDURE SetStyleFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.Wstyle^.Name, on)
END SetStyleFlag ;


(*
   GetStyleFlag - return the value of the Wstyle flag.
*)

PROCEDURE GetStyleFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wstyle^.Name,
                                 CurrentOption^.gm2.option.Wstyle)
END GetStyleFlag ;


(*
   SetPositiveModFloorFlag - set the fpositive-mod-floor-div flag.
*)

PROCEDURE SetPositiveModFloorFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fpositive_mod_floor_div^.Name, on)
END SetPositiveModFloorFlag ;


(*
   GetPositiveModFloorFlag - return the value of the fpositive-mod-floor-div flag.
*)

PROCEDURE GetPositiveModFloorFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fpositive_mod_floor_div^.Name,
                                 CurrentOption^.gm2.option.fpositive_mod_floor_div)
END GetPositiveModFloorFlag ;


(*
   SetWholeDivFlag - set the fwholediv flag.
*)

PROCEDURE SetWholeDivFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fwholediv^.Name, on)
END SetWholeDivFlag ;


(*
   GetWholeDivFlag - return the value of the fwholediv flag.
*)

PROCEDURE GetWholeDivFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fwholediv^.Name,
                                 CurrentOption^.gm2.option.fwholediv)
END GetWholeDivFlag ;


(*
   SetIndexFlag - set the findex flag.
*)

PROCEDURE SetIndexFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.findex^.Name, on)
END SetIndexFlag ;


(*
   GetIndexFlag - return the value of the findex flag.
*)

PROCEDURE GetIndexFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.findex^.Name,
                                 CurrentOption^.gm2.option.findex)
END GetIndexFlag ;


(*
   SetRangeFlag - set the frange flag.
*)

PROCEDURE SetRangeFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.frange^.Name, on)
END SetRangeFlag ;


(*
   GetRangeFlag - return the value of the frange flag.
*)

PROCEDURE GetRangeFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.frange^.Name,
                                 CurrentOption^.gm2.option.frange)
END GetRangeFlag ;


(*
   SetExceptionsFlag - set the fexceptions flag.
*)

PROCEDURE SetExceptionsFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.fexceptions^.Name, on)
END SetExceptionsFlag ;


(*
   GetExceptionsFlag - return the value of the fexceptions flag.
*)

PROCEDURE GetExceptionsFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fexceptions^.Name,
                                 CurrentOption^.gm2.option.fexceptions)
END GetExceptionsFlag ;


(*
   SetReturnFlag - set the freturn flag.
*)

PROCEDURE SetReturnFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.freturn^.Name, on)
END SetReturnFlag ;


(*
   GetReturnFlag - return the value of the freturn flag.
*)

PROCEDURE GetReturnFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.freturn^.Name,
                                 CurrentOption^.gm2.option.freturn)
END GetReturnFlag ;


(*
   SetCaseElseFlag - set the fcase flag.
*)

PROCEDURE SetCaseElseFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fcase^.Name, on)
END SetCaseElseFlag ;


(*
   GetCaseElseFlag - return the value of the fcase flag.
*)

PROCEDURE GetCaseElseFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fcase^.Name,
                                 CurrentOption^.gm2.option.fcase)
END GetCaseElseFlag ;


(*
   SetWidesetFlag - set the fwideset flag.
*)

PROCEDURE SetWidesetFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fwideset^.Name, on)
END SetWidesetFlag ;


(*
   GetWidesetFlag - return the value of the fwideset flag.
*)

PROCEDURE GetWidesetFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fwideset^.Name,
                                 CurrentOption^.gm2.option.fwideset)
END GetWidesetFlag ;


(*
   SetNilFlag - set the fnil flag.
*)

PROCEDURE SetNilFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fnil^.Name, on)
END SetNilFlag ;


(*
   GetNilFlag - return the value of the fnil flag.
*)

PROCEDURE GetNilFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fnil^.Name,
                                 CurrentOption^.gm2.option.fnil)
END GetNilFlag ;


(*
   SetSharedFlag - set the fshared flag.
*)

PROCEDURE SetSharedFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fshared^.Name, on)
END SetSharedFlag ;


(*
   GetSharedFlag - return the value of the fshared flag.
*)

PROCEDURE GetSharedFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fshared^.Name,
                                 CurrentOption^.gm2.option.fshared)
END GetSharedFlag ;


(*
   SetInternalDebuggingFlag - set the fd flag.
*)

PROCEDURE SetInternalDebuggingFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fd^.Name, on)
END SetInternalDebuggingFlag ;


(*
   GetInternalDebuggingFlag - return the value of the fd flag.
*)

PROCEDURE GetInternalDebuggingFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fd^.Name,
                                 CurrentOption^.gm2.option.fd)
END GetInternalDebuggingFlag ;


(*
   SetGFlag - set the g flag.
*)

PROCEDURE SetGFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.g^.Name, on)
END SetGFlag ;


(*
   GetGFlag - return the value of the g flag.
*)

PROCEDURE GetGFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.g^.Name,
                                 CurrentOption^.gm2.option.g)
END GetGFlag ;


(*
   SetSwigFlag - set the fswig flag.
*)

PROCEDURE SetSwigFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fswig^.Name, on)
END SetSwigFlag ;


(*
   GetSwigFlag - return the value of the fswig flag.
*)

PROCEDURE GetSwigFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fswig^.Name,
                                 CurrentOption^.gm2.option.fswig)
END GetSwigFlag ;


(*
   SetM2gFlag - set the fm2_g flag.
*)

PROCEDURE SetM2gFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_g^.Name, on)
END SetM2gFlag ;


(*
   GetM2gFlag - return the value of the fm2_g flag.
*)

PROCEDURE GetM2gFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_g^.Name,
                                 CurrentOption^.gm2.option.fm2_g)
END GetM2gFlag ;


(*
   SetLowerCaseKeywordFlag - set the fm2_lower_case flag.
*)

PROCEDURE SetLowerCaseKeywordFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_lower_case^.Name, on)
END SetLowerCaseKeywordFlag ;


(*
   GetLowerCaseKeywordFlag - return the value of the fm2_lower_case flag.
*)

PROCEDURE GetLowerCaseKeywordFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_lower_case^.Name,
                                 CurrentOption^.gm2.option.fm2_lower_case)
END GetLowerCaseKeywordFlag ;


(*
   SetVerboseFlag - set the v flag.
*)

PROCEDURE SetVerboseFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.v^.Name, on)
END SetVerboseFlag ;


(*
   GetVerboseFlag - return the value of the v flag.
*)

PROCEDURE GetVerboseFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.v^.Name,
                                 CurrentOption^.gm2.option.v)
END GetVerboseFlag ;


(*
   SetUnboundedByReference - set the funbounded_by_reference flag.
*)

PROCEDURE SetUnboundedByReference (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.funbounded_by_reference^.Name, on)
END SetUnboundedByReference ;


(*
   GetUnboundedByReference - return the value of the funbounded_by_reference flag.
*)

PROCEDURE GetUnboundedByReference (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.funbounded_by_reference^.Name,
                                 CurrentOption^.gm2.option.funbounded_by_reference)
END GetUnboundedByReference ;


(*
   SetPPOnly - set the PPonly (preprocess only) to value.
*)

PROCEDURE SetPPOnly (value: BOOLEAN) ;
BEGIN
   PPonly := value
END SetPPOnly ;


(*
   GetPPOnly - get the PPonly (preprocess only).
*)

PROCEDURE GetPPOnly () : BOOLEAN ;
BEGIN
   RETURN PPonly
END GetPPOnly ;


(*
   SetUnusedVariableFlag - set the Wunused_variable flag.
*)

PROCEDURE SetUnusedVariableFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo, DefaultOption^.gm2.option.Wunused_variable^.Name, on)
END SetUnusedVariableFlag ;


(*
   GetUnusedVariableFlag - return the value of the Wunused_variable flag.
*)

PROCEDURE GetUnusedVariableFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wunused_variable^.Name,
                                 CurrentOption^.gm2.option.Wunused_variable)
END GetUnusedVariableFlag ;


(*
   SetUnusedParameterFlag - set the Wunused_parameter flag.
*)

PROCEDURE SetUnusedParameterFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.Wunused_parameter^.Name, on)
END SetUnusedParameterFlag ;


(*
   GetUnusedParameterFlag - return the value of the Wunused_parameter flag.
*)

PROCEDURE GetUnusedParameterFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wunused_parameter^.Name,
                                 CurrentOption^.gm2.option.Wunused_parameter)
END GetUnusedParameterFlag ;


(*
   SetStrictTypeFlag - set the fm2_strict_type flag.
*)

PROCEDURE SetStrictTypeFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_strict_type^.Name, on)
END SetStrictTypeFlag ;


(*
   GetStrictTypeFlag - return the value of the fm2_strict_type flag.
*)

PROCEDURE GetStrictTypeFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_strict_type^.Name,
                                 CurrentOption^.gm2.option.fm2_strict_type)
END GetStrictTypeFlag ;


(*
   SetStrictTypeReason - set the fm2_strict_type_reason flag.
*)

PROCEDURE SetStrictTypeReason (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_strict_type_reason^.Name, on)
END SetStrictTypeReason ;


(*
   GetStrictTypeReason - return the value of the fm2_strict_type_reason flag.
*)

PROCEDURE GetStrictTypeReason (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_strict_type_reason^.Name,
                                 CurrentOption^.gm2.option.fm2_strict_type_reason)
END GetStrictTypeReason ;


(* Start of the uninitialized option procedures.  *)

(*
   SetAutoInitFlag - set the fauto_init flag.
*)

PROCEDURE SetAutoInitFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fauto_init^.Name, on)
END SetAutoInitFlag ;


(*
   GetAutoInitFlag - return the value of the fauto_init flag.
*)

PROCEDURE GetAutoInitFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fauto_init^.Name,
                                 CurrentOption^.gm2.option.fauto_init)
END GetAutoInitFlag ;


(*
   SetUninitVariableConditionalFlag - set the Wuninit_variable_conditional_checking flag.
*)

PROCEDURE SetUninitVariableConditionalFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.Wuninit_variable_conditional_checking^.Name, on)
END SetUninitVariableConditionalFlag ;


(*
   GetUninitVariableConditionalFlag - return the value of the Wuninit_variable_conditional_checking flag.
*)

PROCEDURE GetUninitVariableConditionalFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wuninit_variable_conditional_checking^.Name,
                                 CurrentOption^.gm2.option.Wuninit_variable_conditional_checking)
END GetUninitVariableConditionalFlag ;


(*
   SetUninitVariableFlag - set the Wuninit_variable_checking flag.
*)

PROCEDURE SetUninitVariableFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.Wuninit_variable_checking^.Name, on)
END SetUninitVariableFlag ;


(*
   GetUninitVariableFlag - return the value of the Wuninit_variable_checking flag.
*)

PROCEDURE GetUninitVariableFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wuninit_variable_checking^.Name,
                                 CurrentOption^.gm2.option.Wuninit_variable_checking)
END GetUninitVariableFlag ;


(*
   SetUninitVariableCheckingString - set the Wuninit_variable_checking string.
*)

(*
PROCEDURE SetUninitVariableCheckingString (contents: String; on: BOOLEAN) ;
BEGIN
   IF SetUninitVariableCheckingBooleans (contents, on) = 0
   THEN
      MetaErrorT0 (CommandTokenNo, "malformed directive to -Wuninit_variable_checking")
   END
END SetUninitVariableCheckingString ;
*)


(* End of the uninitialized option procedures and the start of the unused option
   procedures.  *)


(*
   SetUnusedVariableCheckingFlag - set the Wuninit_variable_checking flag.
*)

PROCEDURE SetUnusedVariableCheckingFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.Wuninit_variable_checking^.Name, on)
END SetUnusedVariableCheckingFlag ;


(*
   GetUnusedVariableFlag - return the value of the Wuninit_variable_checking flag.
*)

PROCEDURE GetUnusedVariableCheckingFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wuninit_variable_checking^.Name,
                                 CurrentOption^.gm2.option.Wuninit_variable_checking)
END GetUnusedVariableCheckingFlag ;


(*
   SetStrictTypeReasonFlag - set the fm2_strict_type_reason flag.
*)

PROCEDURE SetStrictTypeReasonFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_strict_type_reason^.Name, on)
END SetStrictTypeReasonFlag ;


(*
   GetStrictTypeReasonFlag - return the value of the fm2_strict_type_reason flag.
*)

PROCEDURE GetStrictTypeReasonFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_strict_type_reason^.Name,
                                 CurrentOption^.gm2.option.fm2_strict_type_reason)
END GetStrictTypeReasonFlag ;


(*
   SetVerboseUnbounded - set the Wverbose_unbounded flag.
*)

PROCEDURE SetVerboseUnbounded (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.Wverbose_unbounded^.Name, on)
END SetVerboseUnbounded ;


(*
   GetVerboseUnbounded - return the value of the Wverbose_unbounded flag.
*)

PROCEDURE GetVerboseUnbounded (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wverbose_unbounded^.Name,
                                 CurrentOption^.gm2.option.Wverbose_unbounded)
END GetVerboseUnbounded ;


(*
   SetQuietFlag - set the fm2_quiet flag.
*)

PROCEDURE SetQuietFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_quiet^.Name, on)
END SetQuietFlag ;


(*
   GetQuietFlag - return the value of the fm2_quiet flag.
*)

PROCEDURE GetQuietFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_quiet^.Name,
                                 CurrentOption^.gm2.option.fm2_quiet)
END GetQuietFlag ;


(*
   SetMemReport - set the fmem_report flag.
*)

PROCEDURE SetMemReport (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fmem_report^.Name, on)
END SetMemReport ;


(*
   GetMemReport - return the value of the fmem_report flag.
*)

PROCEDURE GetMemReport (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fmem_report^.Name,
                                 CurrentOption^.gm2.option.fmem_report)
END GetMemReport ;


(*
   SetTimeReport - set the ftime_report flag.
*)

PROCEDURE SetTimeReport (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.ftime_report^.Name, on)
END SetTimeReport ;


(*
   GetTimeReport - return the value of the ftime_report flag.
*)

PROCEDURE GetTimeReport (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.ftime_report^.Name,
                                 CurrentOption^.gm2.option.ftime_report)
END GetTimeReport ;


(*
   SetLowerCaseKeyword - set the fm2-lower-case flag.
*)

PROCEDURE SetLowerCaseKeyword (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fm2_lower_case^.Name, on)
END SetLowerCaseKeyword ;


(*
   GetLowerCaseKeyword - return the value of the fm2-lower-case flag.
*)

PROCEDURE GetLowerCaseKeyword (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fm2_lower_case^.Name,
                                 CurrentOption^.gm2.option.fm2_lower_case)
END GetLowerCaseKeyword ;


(*
   SetSourcesFlag - set the fsources flag.
*)

PROCEDURE SetSourcesFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fsources^.Name, on)
END SetSourcesFlag ;


(*
   GetSourcesFlag - return the value of the fsources flag.
*)

PROCEDURE GetSourcesFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fsources^.Name,
                                 CurrentOption^.gm2.option.fsources)
END GetSourcesFlag ;


(*
   SetWholeValueFlag - set the fwholevalue flag.
*)

PROCEDURE SetWholeValueFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fwholevalue^.Name, on)
END SetWholeValueFlag ;


(*
   GetWholeValueFlag - return the value of the fwholevalue flag.
*)

PROCEDURE GetWholeValueFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fwholevalue^.Name,
                                 CurrentOption^.gm2.option.fwholevalue)
END GetWholeValueFlag ;


(*
   SetScaffoldStaticFlag - set the fscaffold_static flag.
*)

PROCEDURE SetScaffoldStaticFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fscaffold_static^.Name,
                          on) ;
   IF on
   THEN
      SetNamedOptionBoolean (CommandTokenNo,
                             DefaultOption^.gm2.option.fscaffold_dynamic^.Name,
                             FALSE)
   END
END SetScaffoldStaticFlag ;


(*
   GetScaffoldStaticFlag - return the value of the fscaffold_static flag.
*)

PROCEDURE GetScaffoldStaticFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fscaffold_static^.Name,
                                 CurrentOption^.gm2.option.fscaffold_static)
END GetScaffoldStaticFlag ;


(*
   SetFloatValueFlag - set the ffloatvalue flag.
*)

PROCEDURE SetFloatValueFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.ffloatvalue^.Name, on)
END SetFloatValueFlag ;


(*
   GetFloatValueFlag - return the value of the ffloatvalue flag.
*)

PROCEDURE GetFloatValueFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.ffloatvalue^.Name,
                                 CurrentOption^.gm2.option.ffloatvalue)
END GetFloatValueFlag ;


(*
   PushOptions - push opt to the stack of visible option blocks.
*)

PROCEDURE PushOptions (opt: Options) ;
BEGIN
   AssertStacked (opt, FALSE) ;
   opt^.next := CurrentOption ;
   CurrentOption := opt
END PushOptions ;


(*
   PopOptions - pop including.  Pops the stack until opt is seen
                and then it pops opt.
*)

PROCEDURE PopOptions (opt: Options) ;
BEGIN
   AssertStacked (opt, TRUE) ;
   WHILE CurrentOption # opt DO
      PopTop
   END ;
   PopTop
END PopOptions ;


(*
   PopTop - pops the top of stack.  It is assumed that the options
            live forever and are held elsewhere.
*)

PROCEDURE PopTop ;
BEGIN
   IF CurrentOption = CommandLineOption
   THEN
      InternalError ('should not be popping the command line option block')
   ELSE
      CurrentOption := CurrentOption^.next
   END
END PopTop ;


(*
   AssertStacked - asserts opt is inlist.
*)

PROCEDURE AssertStacked (opt: Options; inlist: BOOLEAN) ;
VAR
   cur: Options ;
BEGIN
   cur := CurrentOption ;
   WHILE cur # NIL DO
      IF cur = opt
      THEN
         IF inlist
         THEN
            RETURN
         ELSE
            InternalError ('option block should not already be stacked')
         END
      END ;
      cur := cur^.next
   END ;
   IF inlist
   THEN
      InternalError ('option block is not stacked')
   END
END AssertStacked ;


(* ---- End of new option processing.  *)

(* String garbage collection debugging routines.

(*
   doDSdbEnter -
*)

PROCEDURE doDSdbEnter ;
BEGIN
   PushAllocation
END doDSdbEnter ;


(*
   doDSdbExit -
*)

PROCEDURE doDSdbExit (s: String) ;
BEGIN
   s := PopAllocationExemption (TRUE, s)
END doDSdbExit ;


(*
   DSdbEnter -
*)

PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;


(*
   DSdbExit -
*)

PROCEDURE DSdbExit (s: String) ;
BEGIN
END DSdbExit ;
*)

(*
#define DSdbEnter doDSdbEnter
#define DSdbExit  doDSdbExit
*)


(*
   SetM2Prefix - assign arg to M2Prefix.
*)

PROCEDURE SetM2Prefix (arg: ADDRESS) ;
BEGIN
   M2Prefix := KillString (M2Prefix) ;
   M2Prefix := InitStringCharStar (arg)
END SetM2Prefix ;


(*
   GetM2Prefix - return M2Prefix as a C string.
*)

PROCEDURE GetM2Prefix () : ADDRESS ;
BEGIN
   RETURN string (M2Prefix)
END GetM2Prefix ;


(*
   SetM2PathName - assign arg to M2PathName.
*)

PROCEDURE SetM2PathName (arg: ADDRESS) ;
BEGIN
   M2PathName := KillString (M2PathName) ;
   M2PathName := InitStringCharStar (arg) ;
   (* fprintf1 (StdErr, "M2PathName = %s\n", M2PathName)  *)
END SetM2PathName ;


(*
   GetM2PathName - return M2PathName as a C string.
*)

PROCEDURE GetM2PathName () : ADDRESS ;
BEGIN
   RETURN string (M2PathName)
END GetM2PathName ;


(*
   SetB - assigns Barg to arg.
*)

PROCEDURE SetB (arg: ADDRESS) ;
BEGIN
   Barg := KillString (Barg) ;
   Barg := InitStringCharStar (arg)
END SetB ;


(*
   GetB - returns Barg value as a C string or NIL if it was never set.
*)

PROCEDURE GetB () : ADDRESS ;
BEGIN
   RETURN string (Barg)
END GetB ;


(*
   SetM - set the MFlag.
*)

PROCEDURE SetM (value: BOOLEAN) ;
BEGIN
   MFlag := value
END SetM ;


(*
   GetM - set the MFlag.
*)

PROCEDURE GetM () : BOOLEAN ;
BEGIN
   RETURN MFlag
END GetM ;


(*
   SetMM - set the MMFlag.
*)

PROCEDURE SetMM (value: BOOLEAN) ;
BEGIN
   MMFlag := value
END SetMM ;


(*
   GetMM - set the MMFlag.
*)

PROCEDURE GetMM () : BOOLEAN ;
BEGIN
   RETURN MMFlag
END GetMM ;


(*
   SetMD - set the MDFlag to value.
*)

PROCEDURE SetMD (value: BOOLEAN) ;
BEGIN
   MDFlag := value
END SetMD ;


(*
   GetMD - return the MDFlag.
*)

PROCEDURE GetMD () : BOOLEAN ;
BEGIN
   RETURN MDFlag
END GetMD ;


(*
   SetMMD - set the MMDFlag to value.
*)

PROCEDURE SetMMD (value: BOOLEAN) ;
BEGIN
   MMDFlag := value
END SetMMD ;


(*
   GetMMD - return the MMDFlag.
*)

PROCEDURE GetMMD () : BOOLEAN ;
BEGIN
   RETURN MMDFlag
END GetMMD ;


(*
   SetMF - assigns MFarg to the filename from arg.
*)

PROCEDURE SetMF (arg: ADDRESS) ;
BEGIN
   MFarg := KillString (MFarg) ;
   MFarg := InitStringCharStar (arg)
END SetMF ;


(*
   GetMF - returns MFarg or NIL if never set.
*)

PROCEDURE GetMF () : ADDRESS ;
BEGIN
   RETURN string (MFarg)
END GetMF ;


(*
   SetMP - set the MPflag to value.
*)

PROCEDURE SetMP (value: BOOLEAN) ;
BEGIN
   MPFlag := value
END SetMP ;


(*
   GetMP - get the MPflag.
*)

PROCEDURE GetMP () : BOOLEAN ;
BEGIN
   RETURN MPFlag
END GetMP ;


(*
   errors1 -
*)

PROCEDURE errors1 (format: ARRAY OF CHAR; arg: String) ;
VAR
   message: String ;
   cstr   : ADDRESS ;
BEGIN
   message := Sprintf1 (InitString (format), arg) ;
   cstr := string (message) ;
   cerror (cstr) ;
   exit (1)
END errors1 ;


(*
   AddWord - concats a word to sentence inserting a space if necessary.
             sentence is returned.  sentence will be created if it is NIL.
*)

PROCEDURE AddWord (sentence, word: String) : String ;
BEGIN
   IF word # NIL
   THEN
      IF sentence = NIL
      THEN
         sentence := Dup (word)
      ELSE
         sentence := ConCatChar (sentence, ' ') ;
         sentence := ConCat (sentence, word)
      END
   END ;
   RETURN sentence
END AddWord ;


(*
   QuoteTarget - quote the '$' character.
*)

PROCEDURE QuoteTarget (target: String) : String ;
VAR
   quoted: String ;
   i, n  : CARDINAL ;
BEGIN
   quoted := InitString ('') ;
   i := 0 ;
   n := Length (target) ;
   WHILE i < n DO
      CASE char (target, i) OF

      '$':  quoted := ConCat (quoted, Mark (InitString ('$$')))

      ELSE
         quoted := ConCatChar (quoted, char (target, i))
      END ;
      INC (i)
   END ;
   RETURN quoted
END QuoteTarget ;


(*
   SetMQ - adds a quoted target arg to the DepTarget sentence.
*)

PROCEDURE SetMQ (arg: ADDRESS) ;
BEGIN
   DepTarget := AddWord (DepTarget, QuoteTarget (InitStringCharStar (arg))) ;
   MQFlag := AddWord (MQFlag, Mark (InitString ('-MQ'))) ;
   MQFlag := AddWord (MQFlag, Mark (InitStringCharStar (arg)))
END SetMQ ;


(*
   GetMQ - returns a C string containing all the -MQ arg values.
*)

PROCEDURE GetMQ () : ADDRESS ;
BEGIN
   RETURN string (MQFlag)
END GetMQ ;


(*
   SetMT - adds a target arg to the DepTarget sentence.
*)

PROCEDURE SetMT (arg: ADDRESS) ;
BEGIN
   DepTarget := AddWord (DepTarget, InitStringCharStar (arg)) ;
   MTFlag := AddWord (MTFlag, Mark (InitString ('-MT'))) ;
   MTFlag := AddWord (MTFlag, Mark (InitStringCharStar (arg)))
END SetMT ;


(*
   GetMT - returns a C string containing all the -MT arg values.
*)

PROCEDURE GetMT () : ADDRESS ;
BEGIN
   RETURN string (MTFlag)
END GetMT ;


(*
   GetDepTarget - returns the DepTarget as a C string.
*)

PROCEDURE GetDepTarget () : ADDRESS ;
BEGIN
   RETURN string (DepTarget)
END GetDepTarget ;


(*
   SetObj - assigns CmdLineObj to the filename from arg.
*)

PROCEDURE SetObj (arg: ADDRESS) ;
BEGIN
   CmdLineObj := KillString (CmdLineObj) ;
   CmdLineObj := InitStringCharStar (arg)
END SetObj ;


(*
   GetObj - returns CmdLineObj filename as a c-string or NIL if it was never set.
*)

PROCEDURE GetObj () : ADDRESS ;
BEGIN
   RETURN string (CmdLineObj)
END GetObj ;


(*
   CppCommandLine - returns the Cpp command line and all arguments.
                    NIL is returned if the -fcpp is absent.
*)

PROCEDURE CppCommandLine () : String ;
VAR
   s: String ;
BEGIN
   IF CPreProcessor
   THEN
      s := InitStringCharStar (FullPathCPP ()) ;
      s := ConCat (ConCatChar (s, ' '), CppArgs) ;
      IF CC1Quiet
      THEN
         s := ConCat (ConCatChar (s, ' '), Mark (InitString ('-quiet')))
      END ;
      RETURN s
   ELSE
      RETURN NIL
   END
END CppCommandLine ;


(*
   CppArg - sets the option and arg in the cpp command line.
*)

PROCEDURE CppArg (opt, arg: ADDRESS; joined: BOOLEAN) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar(opt) ;
   IF EqualArray(s, '-fcpp-begin') OR EqualArray(s, '-fcpp-end')
   THEN
      (* do nothing *)
   ELSE
      IF NOT EqualArray(CppArgs, '')
      THEN
         CppArgs := ConCatChar(CppArgs, ' ')
      END ;
      CppArgs := ConCat(CppArgs, Mark(s)) ;
      IF arg#NIL
      THEN
         s := InitStringCharStar(arg) ;
         IF NOT joined
         THEN
            CppArgs := ConCatChar(CppArgs, ' ')
         END ;
         CppArgs := ConCat(CppArgs, s)
      END
   END
END CppArg ;


(*
   CppRemember - remember a string, s, as a cpp related argument.
                 The string, s, is not garbage collected.
*)

PROCEDURE CppRemember (s: String) ;
BEGIN
   IF (CppArgs=NIL) OR EqualArray (CppArgs, '')
   THEN
      CppArgs := Dup (s)
   ELSE
      CppArgs := ConCatChar (CppArgs, ' ') ;
      CppArgs := ConCat (CppArgs, s)
   END
END CppRemember ;


(*
   FinaliseOptions - once all options have been parsed we set any inferred
                     values.
*)

PROCEDURE FinaliseOptions ;
BEGIN
   (* Nothing to do.  *)
END FinaliseOptions ;


(*
   SetCheckAll - set all runtime checking to on.
*)

PROCEDURE SetCheckAll (on: BOOLEAN) ;
BEGIN
   SetNilFlag (on) ;
   SetWholeDivFlag (on) ;
   SetIndexFlag (on) ;
   SetRangeFlag (on) ;
   SetReturnFlag (on) ;
   SetCaseElseFlag (on) ;
   SetFloatValueFlag (on) ;
   SetWholeValueFlag (on)
END SetCheckAll ;


(*
   SetWallFlag - set all warning flags to, value.
*)

PROCEDURE SetWallFlag (on: BOOLEAN) ;
BEGIN
   SetPedantic (on) ;
   SetPedanticParamNames (on) ;
   SetPedanticCast (on)
END SetWallFlag ;


(*
   SetCpp - enables the source to be preprocessed and enables the
            recognition of C preprocessor line directives.
*)

PROCEDURE SetCpp (value: BOOLEAN) : BOOLEAN ;
BEGIN
   CPreProcessor := value ;
   LineDirectives := value ;
   RETURN TRUE
END SetCpp ;


(*
   GetCpp - returns TRUE if the C preprocessor was used.
*)

PROCEDURE GetCpp () : BOOLEAN ;
BEGIN
   RETURN CPreProcessor
END GetCpp ;


(*
   GetLineDirectives - returns TRUE if line directives are allowed.
*)

PROCEDURE GetLineDirectives () : BOOLEAN ;
BEGIN
   RETURN LineDirectives
END GetLineDirectives ;


(*
   SetUselist - set the uselist flag to value and remember the filename.
*)

PROCEDURE SetUselist (value: BOOLEAN; filename: ADDRESS) ;
BEGIN
   UselistFlag := value ;
   UselistFilename := KillString (UselistFilename) ;
   IF filename # NIL
   THEN
      UselistFilename := InitStringCharStar (filename)
   END
END SetUselist ;


(*
   GetUselist - return the uselist flag.
*)

PROCEDURE GetUselist () : BOOLEAN ;
BEGIN
   RETURN UselistFlag
END GetUselist ;


(*
   GetUselistFilename - return the uselist filename as a String.
*)

PROCEDURE GetUselistFilename () : String ;
BEGIN
   RETURN UselistFilename
END GetUselistFilename ;


(*
   SetPedantic - set the value of -Wpedantic.
*)

PROCEDURE SetPedantic (value: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.Wpedantic^.Name, value)
END SetPedantic ;


(*
   GetPedantic - return the value of -Wpedantic.
*)

PROCEDURE GetPedantic (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wpedantic^.Name,
                                 CurrentOption^.gm2.option.Wpedantic)
END GetPedantic ;


(*
   SetPedanticParamNames - set the Wpedantic-param-names flag.
*)

PROCEDURE SetPedanticParamNames (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.Wpedantic_param_names^.Name, on)
END SetPedanticParamNames ;


(*
   GetPedanticParamNames - return the value of the Wpedantic-param-names flag.
*)

PROCEDURE GetPedanticParamNames (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wpedantic_param_names^.Name,
                                 CurrentOption^.gm2.option.Wpedantic_param_names)
END GetPedanticParamNames ;


(*
   SetPedanticCast - set the Wpedantic-cast flag.
*)

PROCEDURE SetPedanticCast (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.Wpedantic_cast^.Name, on)
END SetPedanticCast ;


(*
   GetPedanticCast - return the value of the Wpedantic-cast flag.
*)

PROCEDURE GetPedanticCast (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.Wpedantic_cast^.Name,
                                 CurrentOption^.gm2.option.Wpedantic_cast)
END GetPedanticCast ;


(*
   SetQuadDebugging - display the quadruples (internal debugging).
*)

PROCEDURE SetQuadDebugging (value: BOOLEAN) ;
BEGIN
   DumpQuad := value ;
   DumpQuadFilename := KillString (DumpQuadFilename) ;
   DumpQuadFilename := InitString ('-')
END SetQuadDebugging ;


(*
   SetM2DebugTraceFilter - set internal debug flags.  The flags should be
                           specified as a comma separated list.  The full
                           list allowed is quad,line,token,all.
*)

PROCEDURE SetM2DebugTraceFilter (value: BOOLEAN; filter: ADDRESS) ;
VAR
   word,
   full  : String ;
   start,
   i     : INTEGER ;
BEGIN
   full := InitStringCharStar (filter) ;
   start := 0 ;
   REPEAT
      i := Index (full, ',', start) ;
      IF i = -1
      THEN
         word := Slice (full, start, 0)
      ELSE
         word := Slice (full, start, i)
      END ;
      SetM2DebugTrace (word, value) ;
      word := KillString (word) ;
      start := i+1 ;
   UNTIL i = -1 ;
   full := KillString (full) ;
END SetM2DebugTraceFilter ;


(*
   SetM2DebugTrace -
*)

PROCEDURE SetM2DebugTrace (word: String; value: BOOLEAN) ;
BEGIN
   IF EqualArray (word, 'all')
   THEN
      (* DebugTraceTree := value ;  *)
      DebugTraceQuad := value ;
      DebugTraceToken := value ;
      DebugTraceLine := value
   ELSIF EqualArray (word, 'quad')
   THEN
      DebugTraceQuad := value
   ELSIF EqualArray (word, 'token')
   THEN
      DebugTraceToken := value
   ELSIF EqualArray (word, 'line')
   THEN
      DebugTraceLine := value
   ELSE
      errors1 ("unrecognized filter %s seen in -fm2-debug-trace= option\n", word)
   END
END SetM2DebugTrace ;


(*
   SetDebugFunctionLineNumbers - set DebugFunctionLineNumbers.
*)

PROCEDURE SetDebugFunctionLineNumbers (value: BOOLEAN) ;
BEGIN
   DebugFunctionLineNumbers := value
END SetDebugFunctionLineNumbers ;


(*
   GetDebugTraceQuad - return DebugTraceQuad.
*)

PROCEDURE GetDebugTraceQuad () : BOOLEAN ;
BEGIN
   RETURN DebugTraceQuad
END GetDebugTraceQuad ;


(*
   GetDebugTraceTree - return DebugTraceTree.
*)

PROCEDURE GetDebugTraceTree () : BOOLEAN ;
BEGIN
   RETURN DebugTraceTree
END GetDebugTraceTree ;


(*
   GetDebugTraceToken - return DebugTraceToken.
*)

PROCEDURE GetDebugTraceToken () : BOOLEAN ;
BEGIN
   RETURN DebugTraceToken
END GetDebugTraceToken ;


(*
   GetDebugTraceLine - return DebugTraceLine.
*)

PROCEDURE GetDebugTraceLine () : BOOLEAN ;
BEGIN
   RETURN DebugTraceLine
END GetDebugTraceLine ;


(*
   GetDebugFunctionLineNumbers - return DebugFunctionLineNumbers.
*)

PROCEDURE GetDebugFunctionLineNumbers () : BOOLEAN ;
BEGIN
   RETURN DebugFunctionLineNumbers
END GetDebugFunctionLineNumbers ;


(*
   SetSearchPath -
*)

PROCEDURE SetSearchPath (arg: ADDRESS) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (arg) ;
   AddInclude (M2PathName, s) ;
   IF Debugging
   THEN
      DumpPathName ("path name entries: ")
   END ;
   s := KillString (s)
END SetSearchPath ;


(*
   setdefextension - set the source file definition module extension to arg.
                     This should include the . and by default it is set to .def.
*)

PROCEDURE setdefextension (arg: ADDRESS) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (arg) ;
   SetDefExtension (s) ;
   s := KillString (s)
END setdefextension ;


(*
   setmodextension - set the source file module extension to arg.
                     This should include the . and by default it is set to .mod.
*)

PROCEDURE setmodextension (arg: ADDRESS) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (arg) ;
   SetModExtension (s) ;
   s := KillString (s)
END setmodextension ;


(*
   SetForcedLocation - sets the location for the lifetime of this compile to, location.
                       This is primarily an internal debugging switch.
*)

PROCEDURE SetForcedLocation (location: location_t) ;
BEGIN
   ForcedLocation := TRUE ;
   ForcedLocationValue := location
END SetForcedLocation ;


(*
   SetCC1Quiet - sets the cc1quiet flag to, value.
*)

PROCEDURE SetCC1Quiet (value: BOOLEAN) ;
BEGIN
   CC1Quiet := value
END SetCC1Quiet ;


(*
   OverrideLocation - possibly override the location value, depending upon
                      whether the -flocation= option was used.
*)

PROCEDURE OverrideLocation (location: location_t) : location_t ;
BEGIN
   IF ForcedLocation
   THEN
      RETURN( ForcedLocationValue )
   ELSE
      RETURN( location )
   END
END OverrideLocation ;


(*
   SetGenerateStatementNote - turn on generation of nops if necessary
                              to generate pedalogical single stepping.
*)

PROCEDURE SetGenerateStatementNote (value: BOOLEAN) ;
BEGIN
   GenerateStatementNote := value
END SetGenerateStatementNote ;


(*
   SetSaveTemps - turn on/off -save-temps.
*)

PROCEDURE SetSaveTemps (value: BOOLEAN) ;
BEGIN
   SaveTemps := value
END SetSaveTemps ;


(*
   GetSaveTemps - return whether -save-temps was set.
*)

PROCEDURE GetSaveTemps () : BOOLEAN ;
BEGIN
   RETURN SaveTemps
END GetSaveTemps ;


(*
   SetSaveTempsDir - turn on/off -save-temps and specify the directory.
*)

PROCEDURE SetSaveTempsDir (arg: ADDRESS) ;
BEGIN
   SaveTempsDir := InitStringCharStar (arg) ;
   SaveTemps := TRUE
END SetSaveTempsDir ;


(*
   GetSaveTempsDir - return SaveTempsDir or NIL if -save-temps was not used.
*)

PROCEDURE GetSaveTempsDir () : String ;
BEGIN
   RETURN SaveTempsDir
END GetSaveTempsDir ;


(*
   SetDumpDir - Set the dump dir.
*)

PROCEDURE SetDumpDir (arg: ADDRESS) ;
BEGIN
   DumpDir := InitStringCharStar (arg)
END SetDumpDir ;


(*
   GetDumpDir - return DumpDir or NIL.
*)

PROCEDURE GetDumpDir () : String ;
BEGIN
   RETURN DumpDir
END GetDumpDir ;


(*
   SetRuntimeModuleOverride - set the override sequence used for module
                              initialization and finialization.
*)

PROCEDURE SetRuntimeModuleOverride (override: ADDRESS) ;
BEGIN
   RuntimeModuleOverride := KillString (RuntimeModuleOverride) ;
   RuntimeModuleOverride := InitStringCharStar (override)
END SetRuntimeModuleOverride ;


(*
   GetRuntimeModuleOverride - return a string containing any user override
                              or the default module initialization override
                              sequence.
*)

PROCEDURE GetRuntimeModuleOverride () : ADDRESS ;
BEGIN
   RETURN string (RuntimeModuleOverride)
END GetRuntimeModuleOverride ;


(*
   SetGenModuleListFileName - set the fgen-module-list= flag and filename.
*)

PROCEDURE SetGenModuleListFileName (on: BOOLEAN; filename: ADDRESS) ;
BEGIN
   GenModuleListFilename := KillString (GenModuleListFilename) ;
   IF filename # NIL
   THEN
      GenModuleListFilename := InitStringCharStar (filename)
   END ;
   SetGenModuleListFlag (on)
END SetGenModuleListFileName ;


(*
   GetGenModuleFilename - returns the filename set by SetGenModuleList.
*)

PROCEDURE GetGenModuleFilename () : String ;
BEGIN
   RETURN GenModuleListFilename
END GetGenModuleFilename ;


(*
   SetGenModuleListFlag - set the fgen-module-list flag.
*)

PROCEDURE SetGenModuleListFlag (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fgen_module_list^.Name, on)
END SetGenModuleListFlag ;


(*
   GetGenModuleListFlag - return the value of the fgen-module-list flag.
*)

PROCEDURE GetGenModuleListFlag (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fgen_module_list^.Name,
                                 CurrentOption^.gm2.option.fgen_module_list)
END GetGenModuleListFlag ;


(*
   SetUninitVariableChecking - sets the UninitVariableChecking and
                               UninitVariableConditionalChecking flags to value
                               depending upon arg string.  The arg string
                               can be: "all", "known,cond", "cond,known", "known"
                               or "cond".
*)

PROCEDURE SetUninitVariableChecking (on: BOOLEAN; arg: ADDRESS) : INTEGER ;
VAR
   contents: String ;
   result  : INTEGER ;
BEGIN
   contents := InitStringCharStar (arg) ;
   result := SetUninitVariableCheckingBooleans (contents, on) ;
   contents := KillString (contents) ;
   RETURN result
END SetUninitVariableChecking ;


(*
   SetUninitVariableCheckingBooleans - set the booleans described by contents to value
                                       and return 1 if successful 0 if the strings dont
                                       match.
*)

PROCEDURE SetUninitVariableCheckingBooleans (contents: String; value: BOOLEAN) : INTEGER ;
BEGIN
   IF EqualArray (contents, "all") OR
      EqualArray (contents, "known,cond") OR
      EqualArray (contents, "cond,known")
   THEN
      SetUninitVariableFlag (value) ;
      SetUninitVariableConditionalFlag (value) ;
      RETURN 1
   ELSIF EqualArray (contents, "known")
   THEN
      SetUninitVariableFlag (value) ;
      RETURN 1
   ELSIF EqualArray (contents, "cond")
   THEN
      SetUninitVariableConditionalFlag (value) ;
      RETURN 1
   ELSE
      RETURN 0
   END
END SetUninitVariableCheckingBooleans ;


(*
   SetIBMLongDouble - enable/disable LONGREAL to map onto the
                      IBM long double 128 bit data type.
                      (Only available on the ppc).
*)

PROCEDURE SetIBMLongDouble (value: BOOLEAN) ;
BEGIN
   IBMLongDouble := value ;
   IF value
   THEN
      IEEELongDouble := FALSE
   END
END SetIBMLongDouble ;


(*
   GetIBMLongDouble - return the value of IBMLongDouble.
*)

PROCEDURE GetIBMLongDouble () : BOOLEAN ;
BEGIN
   RETURN IBMLongDouble
END GetIBMLongDouble ;


(*
   SetIEEELongDouble - enable/disable LONGREAL to map onto the
                       IEEE long double 128 bit data type.
                       (Only available on the ppc).
*)

PROCEDURE SetIEEELongDouble (value: BOOLEAN) ;
BEGIN
   IEEELongDouble := value ;
   IF value
   THEN
      IBMLongDouble := FALSE
   END
END SetIEEELongDouble ;


(*
   GetIEEELongDouble - return the value of IEEELongDouble.
*)

PROCEDURE GetIEEELongDouble () : BOOLEAN ;
BEGIN
   RETURN IEEELongDouble
END GetIEEELongDouble ;


(*
   InitializeLongDoubleFlags - initialize the long double related flags
                               with default values given during gcc configure.
*)

PROCEDURE InitializeLongDoubleFlags ;
BEGIN
   IBMLongDouble := FALSE ;
   IEEELongDouble := FALSE ;
   CASE TargetIEEEQuadDefault () OF

   -1: |
    0: IBMLongDouble := TRUE |
    1: IEEELongDouble := TRUE

   ELSE
      InternalError ('unexpected value returned from TargetIEEEQuadDefault ()')
   END
END InitializeLongDoubleFlags ;


(*
   GetDumpDeclFilename - returns the DumpDeclFilename.
*)

PROCEDURE GetDumpDeclFilename () : String ;
BEGIN
   RETURN DumpDeclFilename
END GetDumpDeclFilename ;


(*
   SetDumpDeclFilename - set DumpDeclFilename to filename.
*)

PROCEDURE SetDumpDeclFilename (value: BOOLEAN; filename: ADDRESS) ;
BEGIN
   DumpDecl := value ;
   DumpDeclFilename := KillString (DumpDeclFilename) ;
   IF filename # NIL
   THEN
      DumpDeclFilename := InitStringCharStar (filename)
   END
END SetDumpDeclFilename ;


(*
   GetDumpQuadFilename - returns the DumpQuadFilename.
*)

PROCEDURE GetDumpQuadFilename () : String ;
BEGIN
   RETURN DumpQuadFilename
END GetDumpQuadFilename ;


(*
   SetDumpQuadFilename - set DumpQuadFilename to filename.
*)

PROCEDURE SetDumpQuadFilename (value: BOOLEAN; filename: ADDRESS) ;
BEGIN
   DumpQuad := value ;
   DumpQuadFilename := KillString (DumpQuadFilename) ;
   IF filename # NIL
   THEN
      DumpQuadFilename := InitStringCharStar (filename)
   END
END SetDumpQuadFilename ;


(*
   GetDumpGimpleFilename - returns the DumpGimpleFilename.
*)

PROCEDURE GetDumpGimpleFilename () : String ;
BEGIN
   RETURN DumpGimpleFilename
END GetDumpGimpleFilename ;


(*
   SetDumpGimpleFilename - set DumpGimpleFilename to filename.
*)

PROCEDURE SetDumpGimpleFilename (value: BOOLEAN; filename: ADDRESS) ;
BEGIN
   DumpGimple := value ;
   DumpGimpleFilename := KillString (DumpGimpleFilename) ;
   IF value AND (filename # NIL)
   THEN
      DumpGimpleFilename := InitStringCharStar (filename)
   END
END SetDumpGimpleFilename ;


(*
   SetM2DumpFilter - sets the filter to a comma separated list of procedures
                     and modules.  Not to be confused with SetM2Dump below
                     which enables the class of data structures to be dumped.
*)

PROCEDURE SetM2DumpFilter (value: BOOLEAN; filter: ADDRESS) ;
BEGIN
   M2DumpFilter := KillString (M2DumpFilter) ;
   IF value AND (filter # NIL)
   THEN
      M2DumpFilter := InitStringCharStar (filter)
   END
END SetM2DumpFilter ;


(*
   GetM2DumpFilter - returns the dump filter.
*)

PROCEDURE GetM2DumpFilter () : ADDRESS ;
BEGIN
   IF M2DumpFilter = NIL
   THEN
      RETURN NIL
   ELSE
      RETURN string (M2DumpFilter)
   END
END GetM2DumpFilter ;


(*
   MatchDump - enable/disable dump using value.  It returns TRUE if dump
               is valid.
*)

PROCEDURE MatchDump (dump: String; value: BOOLEAN) : BOOLEAN ;
BEGIN
   IF EqualArray (dump, 'all')
   THEN
      DumpDecl := value ;
      DumpQuad := value ;
      DumpGimple := value ;
      RETURN TRUE
   ELSIF EqualArray (dump, 'decl')
   THEN
      DumpDecl := value ;
      RETURN TRUE
   ELSIF EqualArray (dump, 'gimple')
   THEN
      DumpGimple := value ;
      RETURN TRUE
   ELSIF EqualArray (dump, 'quad')
   THEN
      DumpQuad := value ;
      RETURN TRUE
   END ;
   RETURN FALSE
END MatchDump ;


(*
   SetM2Dump - sets the dump via a comma separated list: quad,decl,gimple,all.
               It returns TRUE if the comma separated list is valid.
*)

PROCEDURE SetM2Dump (value: BOOLEAN; filter: ADDRESS) : BOOLEAN ;
VAR
   result: BOOLEAN ;
   dump  : String ;
   start,
   i     : INTEGER ;
BEGIN
   IF filter = NIL
   THEN
      RETURN FALSE
   END ;
   IF M2Dump # NIL
   THEN
      M2Dump := KillString (M2Dump)
   END ;
   M2Dump := InitStringCharStar (filter) ;
   start := 0 ;
   REPEAT
      i := Index (M2Dump, ',', start) ;
      IF i = -1
      THEN
         dump := Slice (M2Dump, start, 0)
      ELSE
         dump := Slice (M2Dump, start, i)
      END ;
      result := MatchDump (dump, value) ;
      dump := KillString (dump) ;
      IF NOT result
      THEN
         RETURN FALSE
      END ;
      start := i+1 ;
   UNTIL i = -1 ;
   RETURN TRUE
END SetM2Dump ;


(*
   GetDumpGimple - return TRUE if the dump gimple flag is set from SetM2Dump.
*)

PROCEDURE GetDumpGimple () : BOOLEAN ;
BEGIN
   RETURN DumpGimple
END GetDumpGimple ;


(*
   GetDumpQuad - return TRUE if the dump quad flag is set from SetM2Dump.
*)

PROCEDURE GetDumpQuad () : BOOLEAN ;
BEGIN
   RETURN DumpQuad
END GetDumpQuad ;


(*
   GetDumpDecl - return TRUE if the dump decl flag is set from SetM2Dump.
*)

PROCEDURE GetDumpDecl () : BOOLEAN ;
BEGIN
   RETURN DumpDecl
END GetDumpDecl ;


(*
   SetFileOffsetBits - create SYSTEM.COFF_T as a signed integer of size bits.
*)

PROCEDURE SetFileOffsetBits (value: BOOLEAN; bits: CARDINAL) : BOOLEAN ;
BEGIN
   IF value
   THEN
      OffTBits := bits
   END ;
   RETURN TRUE
END SetFileOffsetBits ;


(*
   GetFileOffsetBits - return the number of bits used to create SYSTEM.COFF_T.
*)

PROCEDURE GetFileOffsetBits () : CARDINAL ;
BEGIN
   RETURN OffTBits
END GetFileOffsetBits ;


(*
   SetForward - attempts to set the boolean indicating -fforward is active
                or inactive.
*)

PROCEDURE SetForward (on: BOOLEAN) ;
BEGIN
   SetNamedOptionBoolean (CommandTokenNo,
                          DefaultOption^.gm2.option.fforward^.Name, on)
END SetForward ;


(*
   GetForward - return the value of the -fforward flag.
*)

PROCEDURE GetForward (tokno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN GetCacheOptionBoolean (tokno,
                                 DefaultOption^.gm2.option.fforward^.Name,
                                 CurrentOption^.gm2.option.fforward)
END GetForward ;


(*
   UnmangleArg - unmangle the pragma name to the GCC option name.
*)

PROCEDURE UnmangleArg (name: String) : String ;
VAR
   result: String ;
BEGIN
   result := Dup (name) ;
   result := ReplaceChar (result, '.', '-') ;
   result := ReplaceChar (result, '_', '-') ;
   RETURN result
END UnmangleArg ;


(*
   SetGccOptionBoolean -
*)

PROCEDURE SetGccOptionBoolean (ob: OptionBoolean) ;
VAR
   arg: String ;
BEGIN
   IF ob^.WriteCount > 0
   THEN
      IF ob^.Value
      THEN
         arg := InitString ('-')
      ELSE
         arg := InitString ('-no-')
      END ;
      arg := ConCat (arg, Mark (UnmangleArg (ob^.Name))) ;
      IF NOT m2gccopt.SetOption (string (arg))
      THEN
         arg := ConCat (arg, Mark (InitString (' is either unknown or cannot be set'))) ;
         MetaErrorStringT0 (ob^.LastTok, arg)
      END ;
      arg := KillString (arg)
   END
END SetGccOptionBoolean ;


(*
   SetGccOptimizing -
*)

PROCEDURE SetGccOptimizing (oi: OptionInteger) ;
VAR
   arg: String ;
BEGIN
   IF oi^.WriteCount > 0
   THEN
      arg := NIL ;
      CASE oi^.Value OF

      0:  arg := InitString ('-O0') |
      1:  arg := InitString ('-O1') |
      2:  arg := InitString ('-O2') |
      3:  arg := InitString ('-O3')

      ELSE
         MetaErrorStringT0 (oi^.LastTok, InitString ('unsupported level of optimization')) ;
         arg := NIL
      END ;
      IF (arg # NIL) AND (NOT m2gccopt.SetOption (string (arg)))
      THEN
         arg := InitString ('cannot set this level of optimization') ;
         MetaErrorStringT0 (oi^.LastTok, arg)
      END ;
      arg := KillString (arg)
   END
END SetGccOptimizing ;


(*
   FlushGccOptions - changes any gcc option modified by a gm2 pragma back to libgcc.
*)

PROCEDURE FlushGccOptions ;
BEGIN
   SetGccOptionBoolean (DefaultOption^.gm2.option.g) ;
   SetGccOptimizing (DefaultOption^.gcc.option.O)
END FlushGccOptions ;


(*
   InitGlobals - initialize global variables.
*)

PROCEDURE InitGlobals ;
BEGIN
   GenerateStatementNote := FALSE ;
   DebugFunctionLineNumbers := FALSE ;
   LineDirectives := TRUE ;
   UselistFlag := FALSE ;
   UselistFilename := NIL ;
   DumpDeclFilename := NIL ;
   DumpQuadFilename := NIL ;
   DumpGimpleFilename := NIL ;
   M2Dump := NIL ;
   M2DumpFilter := NIL ;
   M2Prefix := NIL ;
   M2PathName := NIL ;
   Barg := NIL ;
   MFarg := NIL ;
   MTFlag := NIL ;
   MQFlag := NIL ;
   DepTarget := NIL ;
   CmdLineObj := NIL ;
   SaveTempsDir := NIL ;
   DumpDir := NIL ;
   GenModuleListFilename := NIL ;
   UselistFilename := NIL ;
   RuntimeModuleOverride := InitString (DefaultRuntimeModuleOverride) ;
   CppArgs := InitString ('') ;
   DumpQuad := FALSE ;
   DebugTraceQuad := FALSE ;
   DebugTraceToken := FALSE ;
   DebugTraceLine := FALSE ;
   DebugTraceTree := FALSE ;
   SaveTemps := FALSE ;
   IBMLongDouble := FALSE ;
   IEEELongDouble := FALSE ;
   DumpDecl := FALSE ;
   DumpGimple := FALSE ;
   DumpPragmas := FALSE ;
   OffTBits := 0
END InitGlobals ;


(*
   Init - initialize the option blocks and chain the current,
          command line and default blocks together.
*)

PROCEDURE Init ;
BEGIN
   FilterName := NIL ;
   BoolFreeList := NIL ;
   IntFreeList := NIL ;
   StrFreeList := NIL ;
   (* Set all defaults option values.  *)
   DefaultOption := InitOptions ('default') ;
   RegisterOptions (DefaultOption) ;
   (* No command line options yet, so set up an empty block.  *)
   CommandLineOption := InitOptions ('commandline') ;
   CurrentOption := InitOptions ('mutable') ;
   (* Chain the blocks in search order.  *)
   CommandLineOption^.next := DefaultOption ;
   CurrentOption^.next := CommandLineOption
END Init ;


BEGIN
   InitGlobals ;
   Init ;
END M2Options.
