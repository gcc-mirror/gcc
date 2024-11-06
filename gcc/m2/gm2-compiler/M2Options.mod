(* M2Options.mod initializes the user options.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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
FROM SArgs IMPORT GetArg, Narg ;
FROM M2Search IMPORT SetDefExtension, SetModExtension ;
FROM PathName IMPORT DumpPathName, AddInclude ;
FROM M2Printf IMPORT printf0, printf1, fprintf1 ;
FROM FIO IMPORT StdErr ;
FROM libc IMPORT exit, printf ;
FROM Debug IMPORT Halt ;
FROM gcctypes IMPORT location_t ;
FROM m2configure IMPORT FullPathCPP, TargetIEEEQuadDefault ;
FROM M2Error IMPORT InternalError ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM m2misc IMPORT cerror ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           Dup, string, char, Index,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

(*
#define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#define Dup(X) DupDB(X, __FILE__, __LINE__)
#define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
*)

CONST
   Debugging = FALSE ;
   DefaultRuntimeModuleOverride = "m2iso:RTentity,m2iso:Storage,m2iso:SYSTEM,m2iso:M2RTS,m2iso:RTExceptions,m2iso:IOLink" ;

VAR
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
   SaveTempsDir,
   DumpDir,
   GenModuleListFilename,
   UselistFilename,
   RuntimeModuleOverride,
   CppArgs              : String ;
   DebugFunctionLineNumbers,
   DebugTraceQuad,   (* -fm2-debug-trace=quad.  *)
   DebugTraceLine,   (* -fm2-debug-trace=line.  *)
   DebugTraceToken,  (* -fm2-debug-trace=token. *)
   DebugTraceTree,   (* -fm2-debug-trace=tree.  (not yet implemented).  *)
   DumpDecl,         (* -fm2-dump=decl.  *)
   DumpGimple,       (* -fm2-dump=gimple.  *)
   DumpQuad,         (* -fq, -fm2-dump=quad dump quadruples.  *)
   MFlag,
   MMFlag,
   MPFlag,
   MDFlag,
   MMDFlag,
   IBMLongDouble,
   IEEELongDouble,
   UselistFlag,
   CC1Quiet,
   SeenSources          : BOOLEAN ;
   ForcedLocationValue  : location_t ;


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
   (* currently only one value, this could be make an option in the future *)
   VariantValueChecking := Iso
END FinaliseOptions ;


(*
   SetWholeProgram - sets the WholeProgram flag (-fwhole-program).
*)

PROCEDURE SetWholeProgram (value: BOOLEAN) ;
BEGIN
   WholeProgram := value
END SetWholeProgram ;


(*
   SetReturnCheck -
*)

PROCEDURE SetReturnCheck (value: BOOLEAN) : BOOLEAN ;
BEGIN
   ReturnChecking := value ;
   RETURN TRUE
END SetReturnCheck ;


(*
   SetNilCheck -
*)

PROCEDURE SetNilCheck (value: BOOLEAN) : BOOLEAN ;
BEGIN
   NilChecking := value ;
   RETURN TRUE
END SetNilCheck ;


(*
   SetCaseCheck - set else case checking to, value.
*)

PROCEDURE SetCaseCheck (value: BOOLEAN) : BOOLEAN ;
BEGIN
   CaseElseChecking := value ;
   RETURN TRUE
END SetCaseCheck ;


(*
   SetCheckAll - set all runtime checking to, value.
*)

PROCEDURE SetCheckAll (value: BOOLEAN) : BOOLEAN ;
BEGIN
   NilChecking := value ;
   WholeDivChecking := value ;
   IndexChecking := value ;
   RangeChecking := value ;
   ReturnChecking := value ;
   NilChecking := value ;
   CaseElseChecking := value ;
   FloatValueChecking := value ;
   WholeValueChecking := value ;
   RETURN TRUE
END SetCheckAll ;


(*
   SetAutoInit - -fauto-init turns on automatic initialization of pointers to NIL.
                  TRUE is returned.
*)

PROCEDURE SetAutoInit (value: BOOLEAN) : BOOLEAN ;
BEGIN
   AutoInit := value ;
   RETURN TRUE
END SetAutoInit ;


(*
   SetUnusedVariableChecking - assigns the UnusedVariableChecking to value.
*)

PROCEDURE SetUnusedVariableChecking (value: BOOLEAN) ;
BEGIN
   UnusedVariableChecking := value
END SetUnusedVariableChecking ;


(*
   SetUnusedParameterChecking - assigns the UnusedParameterChecking to value.
*)

PROCEDURE SetUnusedParameterChecking (value: BOOLEAN) ;
BEGIN
   UnusedParameterChecking := value
END SetUnusedParameterChecking ;


(*
   SetStrictTypeChecking - assigns the StrictTypeChecking flag to value.
*)

PROCEDURE SetStrictTypeChecking (value: BOOLEAN) ;
BEGIN
   StrictTypeChecking := value
END SetStrictTypeChecking ;


(*
   SetVerboseUnbounded - sets the VerboseUnbounded flag to, value.
*)

PROCEDURE SetVerboseUnbounded (value: BOOLEAN) : BOOLEAN ;
BEGIN
   VerboseUnbounded := value ;
   RETURN TRUE
END SetVerboseUnbounded ;


(*
   SetQuiet - sets the quiet flag to, value.
*)

PROCEDURE SetQuiet (value: BOOLEAN) : BOOLEAN ;
BEGIN
   Quiet := value ;
   RETURN TRUE
END SetQuiet ;


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
   Setc - set the cflag (compile only flag -c) to value.
*)

PROCEDURE Setc (value: BOOLEAN) ;
BEGIN
   cflag := value
END Setc ;


(*
   Getc - get the cflag (compile only flag -c).
*)

PROCEDURE Getc () : BOOLEAN ;
BEGIN
   RETURN cflag
END Getc ;


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
   SetM2g - set GenerateStatementNote to value and return value.
            Corresponds to the -fm2-g flag.
*)

PROCEDURE SetM2g (value: BOOLEAN) : BOOLEAN ;
BEGIN
   GenerateStatementNote := value ;
   RETURN GenerateStatementNote
END SetM2g ;


(*
   GetM2g - returns TRUE if the -fm2-g flags was used.
*)

PROCEDURE GetM2g () : BOOLEAN ;
BEGIN
   RETURN GenerateStatementNote
END GetM2g ;


(*
   SetLowerCaseKeywords - set the lower case keyword flag and return the result.
*)

PROCEDURE SetLowerCaseKeywords (value: BOOLEAN) : BOOLEAN ;
BEGIN
   LowerCaseKeywords := value ;
   RETURN LowerCaseKeywords
END SetLowerCaseKeywords ;


(*
   SetVerbose - set the Verbose flag to, value.  It returns TRUE.
*)

PROCEDURE SetVerbose (value: BOOLEAN) : BOOLEAN ;
BEGIN
   Verbose := value ;
   RETURN( TRUE )
END SetVerbose ;


(*
   SetMakeall -

PROCEDURE SetMakeall (value: BOOLEAN) : BOOLEAN ;
BEGIN
   (* value is unused *)
   RETURN( TRUE )
END SetMakeall ;
*)


(*
   SetMakeall0 -

PROCEDURE SetMakeall0 (value: BOOLEAN) : BOOLEAN ;
BEGIN
   (* value is unused *)
   RETURN( TRUE )
END SetMakeall0 ;
*)


(*
   SetIncludePath -

PROCEDURE SetIncludePath (arg: ADDRESS) : BOOLEAN ;
BEGIN
   RETURN( TRUE )
END SetIncludePath ;
*)


(*
   SetUnboundedByReference -
*)

PROCEDURE SetUnboundedByReference (value: BOOLEAN) : BOOLEAN ;
BEGIN
   UnboundedByReference := value ;
   RETURN( TRUE )
END SetUnboundedByReference ;


(*
(*
   SetDebugging - sets the debugging flag to, v.
*)

PROCEDURE SetDebugging (value: BOOLEAN) ;
BEGIN
   GenerateDebugging := value
END SetDebugging ;


(*
   SetProfiling - dummy procedure, as profiling is implemented in the gcc backend.
*)

PROCEDURE SetProfiling (value: BOOLEAN) ;
BEGIN
   (* nothing to do *)
END SetProfiling ;
*)


(*
   SetISO -
*)

PROCEDURE SetISO (value: BOOLEAN) ;
BEGIN
   Iso := value ;
   Pim := NOT value ;
   Pim2 := NOT value ;
   (* Pim4 is the default, leave it alone since the DIV and MOD rules are the
      same as ISO.  *)
END SetISO ;


(*
   SetPIM -
*)

PROCEDURE SetPIM (value: BOOLEAN) ;
BEGIN
   Pim := value ;
   Iso := NOT value
END SetPIM ;


(*
   SetPIM2 -
*)

PROCEDURE SetPIM2 (value: BOOLEAN) ;
BEGIN
   Pim := value ;
   Pim2 := value ;
   Iso := NOT value ;
   IF value
   THEN
      (* Pim4 is the default, turn it off.  *)
      Pim4 := FALSE
   END
END SetPIM2 ;


(*
   SetPIM3 -
*)

PROCEDURE SetPIM3 (value: BOOLEAN) ;
BEGIN
   Pim := value ;
   Pim3 := value ;
   Iso := NOT value ;
   IF value
   THEN
      (* Pim4 is the default, turn it off.  *)
      Pim4 := FALSE
   END
END SetPIM3 ;


(*
   SetPIM4 -
*)

PROCEDURE SetPIM4 (value: BOOLEAN) ;
BEGIN
   Pim := value ;
   Pim4 := value ;
   Iso := NOT value
END SetPIM4 ;


(*
   SetPositiveModFloor - sets the positive mod floor option.
*)

PROCEDURE SetPositiveModFloor (value: BOOLEAN) ;
BEGIN
   PositiveModFloorDiv := value
END SetPositiveModFloor ;


(*
   SetWholeDiv - sets the whole division flag.
*)

PROCEDURE SetWholeDiv (value: BOOLEAN) ;
BEGIN
   WholeDivChecking := value
END SetWholeDiv ;


(*
   SetIndex - sets the runtime array index checking flag.
*)

PROCEDURE SetIndex (value: BOOLEAN) ;
BEGIN
   IndexChecking := value
END SetIndex ;


(*
   SetRange -  sets the runtime range checking flag.
*)

PROCEDURE SetRange (value: BOOLEAN) ;
BEGIN
   RangeChecking := value
END SetRange ;


(*
   SetExceptions - sets the enable runtime exceptions flag.
*)

PROCEDURE SetExceptions (value: BOOLEAN) ;
BEGIN
   Exceptions := value
END SetExceptions ;


(*
   SetStyle -
*)

PROCEDURE SetStyle (value: BOOLEAN) ;
BEGIN
   StyleChecking := value
END SetStyle ;


(*
   SetPedantic -
*)

PROCEDURE SetPedantic (value: BOOLEAN) ;
BEGIN
   Pedantic := value
END SetPedantic ;


(*
   SetPedanticParamNames - sets the pedantic parameter name flag.
*)

PROCEDURE SetPedanticParamNames (value: BOOLEAN) ;
BEGIN
   PedanticParamNames := value
END SetPedanticParamNames ;


(*
   SetPedanticCast - sets the pedantic cast flag.
*)

PROCEDURE SetPedanticCast (value: BOOLEAN) ;
BEGIN
   PedanticCast := value
END SetPedanticCast ;


(*
   SetExtendedOpaque - sets the ExtendedOpaque flag.
*)

PROCEDURE SetExtendedOpaque (value: BOOLEAN) ;
BEGIN
   ExtendedOpaque := value
END SetExtendedOpaque ;


(*
   SetXCode - sets the xcode flag.
*)

PROCEDURE SetXCode (value: BOOLEAN) ;
BEGIN
   Xcode := value
END SetXCode ;


(*
   SetSwig -
*)

PROCEDURE SetSwig (value: BOOLEAN) ;
BEGIN
   GenerateSwig := value
END SetSwig ;


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
   SetCompilerDebugging - turn on internal compiler debugging.
*)

PROCEDURE SetCompilerDebugging (value: BOOLEAN) ;
BEGIN
   CompilerDebugging := value
END SetCompilerDebugging ;


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
   SetSources -
*)

PROCEDURE SetSources (value: BOOLEAN) ;
BEGIN
   Quiet := NOT value ;
   SeenSources := value
END SetSources ;


(*
   SetDumpSystemExports -
*)

PROCEDURE SetDumpSystemExports (value: BOOLEAN) ;
BEGIN
   DumpSystemExports := value
END SetDumpSystemExports ;


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
   SetOptimizing -
*)

PROCEDURE SetOptimizing (value: CARDINAL) ;
BEGIN
   IF value>0
   THEN
      Optimizing := TRUE ;
      OptimizeBasicBlock := TRUE ;
      OptimizeUncalledProcedures := TRUE ;
      OptimizeCommonSubExpressions := TRUE
   ELSE
      Optimizing := FALSE ;
      OptimizeBasicBlock := FALSE ;
      OptimizeUncalledProcedures := FALSE ;
      OptimizeCommonSubExpressions := FALSE
   END
END SetOptimizing ;


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
   SetStatistics - turn on/off generate of compile time statistics.
*)

PROCEDURE SetStatistics (on: BOOLEAN) ;
BEGIN
   Statistics := on
END SetStatistics ;


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
   GetISO - return TRUE if -fiso was present on the command line.
*)

PROCEDURE GetISO () : BOOLEAN ;
BEGIN
   RETURN Iso
END GetISO ;


(*
   GetPIM - return TRUE if -fpim was present on the command line.
*)

PROCEDURE GetPIM () : BOOLEAN ;
BEGIN
   RETURN Pim
END GetPIM ;


(*
   GetPIM2 - return TRUE if -fpim2 was present on the command line.
*)

PROCEDURE GetPIM2 () : BOOLEAN ;
BEGIN
   RETURN Pim2
END GetPIM2 ;


(*
   GetPIM3 - return TRUE if -fpim3 was present on the command line.
*)

PROCEDURE GetPIM3 () : BOOLEAN ;
BEGIN
   RETURN Pim3
END GetPIM3 ;


(*
   GetPIM4 - return TRUE if -fpim4 was present on the command line.
*)

PROCEDURE GetPIM4 () : BOOLEAN ;
BEGIN
   RETURN Pim4
END GetPIM4 ;


(*
   GetPositiveModFloor - return TRUE if -fpositive-mod-floor was present
                         on the command line.
*)

PROCEDURE GetPositiveModFloor () : BOOLEAN ;
BEGIN
   RETURN PositiveModFloorDiv
END GetPositiveModFloor ;


(*
   GetFloatValueCheck - return TRUE if -ffloatvalue was present on the
                        command line.
*)

PROCEDURE GetFloatValueCheck () : BOOLEAN ;
BEGIN
   RETURN FloatValueChecking
END GetFloatValueCheck ;


(*
   SetFloatValueCheck - set depending upon the -ffloatvalue.
*)

PROCEDURE SetFloatValueCheck (value: BOOLEAN) ;
BEGIN
   FloatValueChecking := value
END SetFloatValueCheck ;


(*
   GetWholeValueCheck - return TRUE if -fwholevalue was present on the
                        command line.
*)

PROCEDURE GetWholeValueCheck () : BOOLEAN ;
BEGIN
   RETURN WholeValueChecking
END GetWholeValueCheck ;


(*
   SetWholeValueCheck - set depending upon the -fwholevalue.
*)

PROCEDURE SetWholeValueCheck (value: BOOLEAN) ;
BEGIN
   WholeValueChecking := value
END SetWholeValueCheck ;


(*
   SetWall - set all warnings to, value.
*)

PROCEDURE SetWall (value: BOOLEAN) ;
BEGIN
   UnusedVariableChecking  := value ;
   UnusedParameterChecking := value ;
   UninitVariableChecking := value ;
   PedanticCast := value ;
   PedanticParamNames := value ;
   StyleChecking := value ;
   CaseEnumChecking := value
END SetWall ;


(*
   SetSaveTemps - turn on/off -save-temps.
*)

PROCEDURE SetSaveTemps (value: BOOLEAN) ;
BEGIN
   SaveTemps := value
END SetSaveTemps ;


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
   SetScaffoldDynamic - set the -fscaffold-dynamic flag.
*)

PROCEDURE SetScaffoldDynamic (value: BOOLEAN) ;
BEGIN
   ScaffoldDynamic := value ;
   IF ScaffoldDynamic
   THEN
      ScaffoldStatic := FALSE
   END
END SetScaffoldDynamic ;


(*
   SetScaffoldStatic - set the -fscaffold-static flag.
*)

PROCEDURE SetScaffoldStatic (value: BOOLEAN) ;
BEGIN
   ScaffoldStatic := value ;
   IF ScaffoldStatic
   THEN
      ScaffoldDynamic := FALSE
   END
END SetScaffoldStatic ;


(*
   GetScaffoldDynamic - get the -fscaffold-dynamic flag.
*)

PROCEDURE GetScaffoldDynamic () : BOOLEAN ;
BEGIN
   RETURN ScaffoldDynamic
END GetScaffoldDynamic ;


(*
   GetScaffoldStatic - get the -fscaffold-static flag.
*)

PROCEDURE GetScaffoldStatic () : BOOLEAN ;
BEGIN
   RETURN ScaffoldStatic
END GetScaffoldStatic ;


(*
   SetScaffoldMain - set the -fscaffold-main flag.
*)

PROCEDURE SetScaffoldMain (value: BOOLEAN) ;
BEGIN
   ScaffoldMain := value
END SetScaffoldMain ;


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
   SetGenModuleList - set the GenModuleList flag to true and pass
                      set GenModuleListFilename to filename.
*)

PROCEDURE SetGenModuleList (value: BOOLEAN; filename: ADDRESS) ;
BEGIN
   GenModuleListFilename := KillString (GenModuleListFilename) ;
   IF filename # NIL
   THEN
      GenModuleListFilename := InitStringCharStar (filename)
   END ;
   GenModuleList := value
END SetGenModuleList ;


(*
   GetGenModuleFilename - returns the filename set by SetGenModuleList.
*)

PROCEDURE GetGenModuleFilename () : String ;
BEGIN
   RETURN GenModuleListFilename
END GetGenModuleFilename ;


(*
   SetShared - sets the SharedFlag to value.
*)

PROCEDURE SetShared (value: BOOLEAN) ;
BEGIN
   SharedFlag := value
END SetShared ;


(*
   SetUninitVariableChecking - sets the UninitVariableChecking and
                               UninitVariableConditionalChecking flags to value
                               depending upon arg string.  The arg string
                               can be: "all", "known,cond", "cond,known", "known"
                               or "cond".
*)

PROCEDURE SetUninitVariableChecking (value: BOOLEAN; arg: ADDRESS) : INTEGER ;
VAR
   s: String ;
BEGIN
   IF Debugging
   THEN
      IF value
      THEN
         printf ("SetUninitVariableChecking (TRUE, %s)\n", arg)
      ELSE
         printf ("SetUninitVariableChecking (FALSE, %s)\n", arg)
      END
   END ;
   s := InitStringCharStar (arg) ;
   IF EqualArray (s, "all") OR
      EqualArray (s, "known,cond") OR
      EqualArray (s, "cond,known")
   THEN
      UninitVariableChecking := value ;
      UninitVariableConditionalChecking := value ;
      s := KillString (s) ;
      RETURN 1
   ELSIF EqualArray (s, "known")
   THEN
      UninitVariableChecking := value ;
      s := KillString (s) ;
      RETURN 1
   ELSIF EqualArray (s, "cond")
   THEN
      UninitVariableConditionalChecking := value ;
      s := KillString (s) ;
      RETURN 1
   ELSE
      s := KillString (s) ;
      RETURN 0
   END
END SetUninitVariableChecking ;


(*
   SetCaseEnumChecking - sets the CaseEnumChecking to value.
*)

PROCEDURE SetCaseEnumChecking (value: BOOLEAN) ;
BEGIN
   CaseEnumChecking := value
END SetCaseEnumChecking ;


(*
   SetDebugBuiltins - sets the DebugBuiltins to value.
*)

PROCEDURE SetDebugBuiltins (value: BOOLEAN) ;
BEGIN
   DebugBuiltins := value
END SetDebugBuiltins ;


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
   SetDumpDeclFilename -
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
   SetDumpQuadFilename -
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


BEGIN
   cflag                             := FALSE ;  (* -c.  *)
   RuntimeModuleOverride             := InitString (DefaultRuntimeModuleOverride) ;
   CppArgs                           := InitString ('') ;
   Pim                               :=  TRUE ;
   Pim2                              := FALSE ;
   Pim3                              := FALSE ;
   Pim4                              :=  TRUE ;
   PositiveModFloorDiv               := FALSE ;
   Iso                               := FALSE ;
   SeenSources                       := FALSE ;
   Statistics                        := FALSE ;
   StyleChecking                     := FALSE ;
   CompilerDebugging                 := FALSE ;
   GenerateDebugging                 := FALSE ;
   Optimizing                        := FALSE ;
   Pedantic                          := FALSE ;
   Verbose                           := FALSE ;
   Quiet                             :=  TRUE ;
   CC1Quiet                          :=  TRUE ;
   Profiling                         := FALSE ;
   DumpQuad                          := FALSE ;
   OptimizeBasicBlock                := FALSE ;
   OptimizeUncalledProcedures        := FALSE ;
   OptimizeCommonSubExpressions      := FALSE ;
   NilChecking                       := FALSE ;
   WholeDivChecking                  := FALSE ;
   WholeValueChecking                := FALSE ;
   FloatValueChecking                := FALSE ;
   IndexChecking                     := FALSE ;
   RangeChecking                     := FALSE ;
   ReturnChecking                    := FALSE ;
   CaseElseChecking                  := FALSE ;
   CPreProcessor                     := FALSE ;
   LineDirectives                    := TRUE ;
   ExtendedOpaque                    := FALSE ;
   UnboundedByReference              := FALSE ;
   VerboseUnbounded                  := FALSE ;
   PedanticParamNames                := FALSE ;
   PedanticCast                      := FALSE ;
   Xcode                             := FALSE ;
   DumpSystemExports                 := FALSE ;
   GenerateSwig                      := FALSE ;
   Exceptions                        :=  TRUE ;
   DebugBuiltins                     := FALSE ;
   ForcedLocation                    := FALSE ;
   WholeProgram                      := FALSE ;
   DebugTraceQuad                    := FALSE ;
   DebugTraceTree                    := FALSE ;
   DebugTraceLine                    := FALSE ;
   DebugTraceToken                   := FALSE ;
   DebugFunctionLineNumbers          := FALSE ;
   GenerateStatementNote             := FALSE ;
   LowerCaseKeywords                 := FALSE ;
   UnusedVariableChecking            := FALSE ;
   UnusedParameterChecking           := FALSE ;
   StrictTypeChecking                := TRUE ;
   AutoInit                          := FALSE ;
   SaveTemps                         := FALSE ;
   ScaffoldDynamic                   := TRUE ;
   ScaffoldStatic                    := FALSE ;
   ScaffoldMain                      := FALSE ;
   UselistFilename                   := NIL ;
   GenModuleList                     := FALSE ;
   GenModuleListFilename             := NIL ;
   SharedFlag                        := FALSE ;
   Barg                              := NIL ;
   MDFlag                            := FALSE ;
   MMDFlag                           := FALSE ;
   DepTarget                         := NIL ;
   MPFlag                            := FALSE ;
   SaveTempsDir                      := NIL ;
   DumpDir                           := NIL ;
   UninitVariableChecking            := FALSE ;
   UninitVariableConditionalChecking := FALSE ;
   CaseEnumChecking                  := FALSE ;
   MFlag                             := FALSE ;
   MMFlag                            := FALSE ;
   MFarg                             := NIL ;
   MTFlag                            := NIL ;
   MQFlag                            := NIL ;
   InitializeLongDoubleFlags ;
   M2Prefix                          := InitString ('') ;
   M2PathName                        := InitString ('') ;
   DumpQuadFilename                  := NIL ;
   DumpGimpleFilename                := NIL ;
   DumpDeclFilename                  := NIL ;
   DumpDecl                          := FALSE ;
   DumpQuad                          := FALSE ;
   DumpGimple                        := FALSE ;
   M2Dump                            := NIL ;
   M2DumpFilter                      := NIL
END M2Options.
