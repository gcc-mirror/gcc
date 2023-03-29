(* M2Options.mod initializes the user options.

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

IMPLEMENTATION MODULE M2Options ;


IMPORT CmdArgs ;
FROM SArgs IMPORT GetArg, Narg ;
FROM M2Search IMPORT SetDefExtension, SetModExtension ;
FROM PathName IMPORT DumpPathName, AddInclude ;
FROM M2Printf IMPORT printf0, printf1, fprintf1 ;
FROM FIO IMPORT StdErr ;
FROM libc IMPORT exit ;
FROM Debug IMPORT Halt ;
FROM m2linemap IMPORT location_t ;
FROM m2configure IMPORT FullPathCPP ;


FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           Dup, string,
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
   M2Prefix,
   M2PathName,
   Barg,
   MDarg,
   MMDarg,
   MQarg,
   CmdLineObj,
   SaveTempsDir,
   DumpDir,
   GenModuleListFilename,
   UselistFilename,
   RuntimeModuleOverride,
   CppArgs              : String ;
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
   SetMD - assigns MDarg to the filename from arg.
   This overrides any previous MMD.
*)

PROCEDURE SetMD (arg: ADDRESS) ;
BEGIN
   MMDarg := KillString (MMDarg) ;
   MDarg := KillString (MDarg) ;
   MDarg := InitStringCharStar (arg)
END SetMD ;


(*
   GetMD - returns MDarg filename as a c-string or NIL if it was never set.
*)

PROCEDURE GetMD () : ADDRESS ;
BEGIN
   RETURN string (MDarg)
END GetMD ;


(*
   SetMMD - assigns MMDarg to the filename from arg.
   This overrides any previous MD.
*)

PROCEDURE SetMMD (arg: ADDRESS) ;
BEGIN
   MDarg := KillString (MDarg) ;
   MMDarg := KillString (MMDarg) ;
   MMDarg := InitStringCharStar (arg)
END SetMMD ;


(*
   GetMMD - returns MMDarg filename as a c-string or NIL if it was never set.
*)

PROCEDURE GetMMD () : ADDRESS ;
BEGIN
   RETURN string (MMDarg)
END GetMMD ;


(*
   SetMQ - assigns MQarg to the filename from arg.
*)

PROCEDURE SetMQ (arg: ADDRESS) ;
BEGIN
   MQarg := KillString (MQarg) ;
   MQarg := InitStringCharStar (arg)
END SetMQ ;


(*
   GetMMD - returns MQarg filename as a c-string or NIL if it was never set.
*)

PROCEDURE GetMQ () : ADDRESS ;
BEGIN
   RETURN string (MQarg)
END GetMQ ;


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

PROCEDURE SetAutoInit (value: BOOLEAN) ;
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
   DisplayQuadruples := value
END SetQuadDebugging ;


(*
   SetCompilerDebugging - turn on internal compiler debugging.
*)

PROCEDURE SetCompilerDebugging (value: BOOLEAN) ;
BEGIN
   CompilerDebugging := value
END SetCompilerDebugging ;


(*
   SetDebugTraceQuad -
*)

PROCEDURE SetDebugTraceQuad (value: BOOLEAN) ;
BEGIN
   DebugTraceQuad := value
END SetDebugTraceQuad ;


(*
   SetDebugTraceAPI -
*)

PROCEDURE SetDebugTraceAPI (value: BOOLEAN) ;
BEGIN
   DebugTraceAPI := value
END SetDebugTraceAPI ;


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
   SetDebugFunctionLineNumbers - turn DebugFunctionLineNumbers on/off
                                 (used internally for debugging).
*)

PROCEDURE SetDebugFunctionLineNumbers (value: BOOLEAN) ;
BEGIN
   DebugFunctionLineNumbers := value
END SetDebugFunctionLineNumbers ;


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
   PedanticCast := value ;
   PedanticParamNames := value ;
   StyleChecking := value
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
   RETURN RuntimeModuleOverride
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


BEGIN
   cflag                        := FALSE ;  (* -c.  *)
   RuntimeModuleOverride        := InitString (DefaultRuntimeModuleOverride) ;
   CppArgs                      := InitString ('') ;
   Pim                          :=  TRUE ;
   Pim2                         := FALSE ;
   Pim3                         := FALSE ;
   Pim4                         :=  TRUE ;
   PositiveModFloorDiv          := FALSE ;
   Iso                          := FALSE ;
   SeenSources                  := FALSE ;
   Statistics                   := FALSE ;
   StyleChecking                := FALSE ;
   CompilerDebugging            := FALSE ;
   GenerateDebugging            := FALSE ;
   Optimizing                   := FALSE ;
   Pedantic                     := FALSE ;
   Verbose                      := FALSE ;
   Quiet                        :=  TRUE ;
   CC1Quiet                     :=  TRUE ;
   Profiling                    := FALSE ;
   DisplayQuadruples            := FALSE ;
   OptimizeBasicBlock           := FALSE ;
   OptimizeUncalledProcedures   := FALSE ;
   OptimizeCommonSubExpressions := FALSE ;
   NilChecking                  := FALSE ;
   WholeDivChecking             := FALSE ;
   WholeValueChecking           := FALSE ;
   FloatValueChecking           := FALSE ;
   IndexChecking                := FALSE ;
   RangeChecking                := FALSE ;
   ReturnChecking               := FALSE ;
   CaseElseChecking             := FALSE ;
   CPreProcessor                := FALSE ;
   LineDirectives               := FALSE ;
   ExtendedOpaque               := FALSE ;
   UnboundedByReference         := FALSE ;
   VerboseUnbounded             := FALSE ;
   PedanticParamNames           := FALSE ;
   PedanticCast                 := FALSE ;
   Xcode                        := FALSE ;
   DumpSystemExports            := FALSE ;
   GenerateSwig                 := FALSE ;
   Exceptions                   :=  TRUE ;
   DebugBuiltins                := FALSE ;
   ForcedLocation               := FALSE ;
   WholeProgram                 := FALSE ;
   DebugTraceQuad               := FALSE ;
   DebugTraceAPI                := FALSE ;
   DebugFunctionLineNumbers     := FALSE ;
   GenerateStatementNote        := FALSE ;
   LowerCaseKeywords            := FALSE ;
   UnusedVariableChecking       := FALSE ;
   UnusedParameterChecking      := FALSE ;
   StrictTypeChecking           := TRUE ;
   AutoInit                     := FALSE ;
   SaveTemps                    := FALSE ;
   ScaffoldDynamic              := TRUE ;
   ScaffoldStatic               := FALSE ;
   ScaffoldMain                 := FALSE ;
   UselistFilename              := NIL ;
   GenModuleList                := FALSE ;
   GenModuleListFilename        := NIL ;
   SharedFlag                   := FALSE ;
   Barg                         := NIL ;
   MDarg                        := NIL ;
   MMDarg                       := NIL ;
   MQarg                        := NIL ;
   SaveTempsDir                 := NIL ;
   DumpDir                      := NIL ;
   M2Prefix                     := InitString ('') ;
   M2PathName                   := InitString ('')
END M2Options.
