/* m2options.h header file for M2Options.mod.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
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
<http://www.gnu.org/licenses/>.  */

#if !defined(m2options_h)

#define m2options_h
#if defined(m2options_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2options_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2options_c.  */

#include "input.h"

EXTERN void M2Options_SetSearchPath (const char *arg);
EXTERN void M2Options_setdefextension (const char *arg);
EXTERN void M2Options_setmodextension (const char *arg);

EXTERN void M2Options_SetISO (bool value);
EXTERN void M2Options_SetPIM (bool value);
EXTERN void M2Options_SetPIM2 (bool value);
EXTERN void M2Options_SetPIM3 (bool value);
EXTERN void M2Options_SetPIM4 (bool value);
EXTERN void M2Options_SetFloatValueCheck (bool value);
EXTERN void M2Options_SetWholeValueCheck (bool value);

EXTERN bool M2Options_GetISO (void);
EXTERN bool M2Options_GetPIM (void);
EXTERN bool M2Options_GetPIM2 (void);
EXTERN bool M2Options_GetPIM3 (void);
EXTERN bool M2Options_GetPIM4 (void);
EXTERN bool M2Options_GetPositiveModFloor (void);
EXTERN bool M2Options_GetFloatValueCheck (void);
EXTERN bool M2Options_GetWholeValueCheck (void);

EXTERN void M2Options_Setc (bool value);
EXTERN bool M2Options_Getc (void);
EXTERN void M2Options_SetPPOnly (bool value);
EXTERN bool M2Options_GetPPOnly (void);

EXTERN void M2Options_SetUselist (bool value, const char *filename);
EXTERN bool M2Options_SetAutoInit (bool value);
EXTERN void M2Options_SetPositiveModFloor (bool value);
EXTERN bool M2Options_SetNilCheck (bool value);
EXTERN void M2Options_SetWholeDiv (bool value);
EXTERN void M2Options_SetIndex (bool value);
EXTERN void M2Options_SetRange (bool value);
EXTERN bool M2Options_SetReturnCheck (bool value);
EXTERN bool M2Options_SetCaseCheck (bool value);
EXTERN bool M2Options_SetCheckAll (bool value);
EXTERN void M2Options_SetExceptions (bool value);
EXTERN void M2Options_SetStyle (bool value);
EXTERN void M2Options_SetPedantic (bool value);
EXTERN void M2Options_SetPedanticParamNames (bool value);
EXTERN void M2Options_SetPedanticCast (bool value);
EXTERN void M2Options_SetExtendedOpaque (bool value);
EXTERN bool M2Options_SetVerboseUnbounded (bool value);
EXTERN void M2Options_SetXCode (bool value);
EXTERN void M2Options_SetCompilerDebugging (bool value);
EXTERN void M2Options_SetQuadDebugging (bool value);
EXTERN bool M2Options_GetDebugTraceToken (void);
EXTERN bool M2Options_GetDebugTraceLine (void);
EXTERN void M2Options_SetDebugFunctionLineNumbers (bool value);
EXTERN bool M2Options_GetDebugFunctionLineNumbers (void);
EXTERN void M2Options_SetSources (bool value);
EXTERN bool M2Options_SetUnboundedByReference (bool value);
EXTERN void M2Options_SetDumpSystemExports (bool value);
EXTERN void M2Options_SetOptimizing (unsigned int value);
EXTERN bool M2Options_SetQuiet (bool value);
EXTERN void M2Options_SetCC1Quiet (bool value);
EXTERN bool M2Options_SetCpp (bool value);
EXTERN void M2Options_SetSwig (bool value);
EXTERN void M2Options_SetForcedLocation (location_t location);
EXTERN location_t M2Options_OverrideLocation (location_t location);
EXTERN void M2Options_SetStatistics (bool on);
EXTERN bool M2Options_SetFileOffsetBits (bool value, unsigned int bits);
EXTERN unsigned int M2Options_GetFileOffsetBits (void);
EXTERN void M2Options_CppProg (const char *program);
EXTERN void M2Options_CppArg (const char *opt, const char *arg, bool joined);
EXTERN void M2Options_SetWholeProgram (bool value);
EXTERN void M2Options_FinaliseOptions (void);
EXTERN void M2Options_SetDebugFunctionLineNumbers (bool value);
EXTERN void M2Options_SetGenerateStatementNote (bool value);
EXTERN bool M2Options_GetCpp (void);
EXTERN bool M2Options_GetLineDirectives (void);
EXTERN bool M2Options_GetM2g (void);
EXTERN bool M2Options_SetM2g (bool value);
EXTERN bool M2Options_SetLowerCaseKeywords (bool value);
EXTERN bool M2Options_SetVerbose (bool value);
EXTERN void M2Options_SetUnusedVariableChecking (bool value);
EXTERN void M2Options_SetUnusedParameterChecking (bool value);
EXTERN void M2Options_SetStrictTypeChecking (bool value);
EXTERN void M2Options_SetWall (bool value);
EXTERN void M2Options_SetSaveTemps (bool value);
EXTERN void M2Options_SetSaveTempsDir (const char *arg);
EXTERN void M2Options_SetDumpDir (const char *arg);
EXTERN bool M2Options_GetSaveTemps (void);
EXTERN void M2Options_SetScaffoldStatic (bool value);
EXTERN void M2Options_SetScaffoldDynamic (bool value);
EXTERN void M2Options_SetScaffoldMain (bool value);
EXTERN void M2Options_SetRuntimeModuleOverride (const char *override);
EXTERN void M2Options_SetGenModuleList (bool value, const char *filename);
EXTERN void M2Options_SetShared (bool value);
EXTERN void M2Options_SetB (const char *arg);
EXTERN char *M2Options_GetB (void);
EXTERN void M2Options_SetM (bool value);
EXTERN bool M2Options_GetM (void);
EXTERN void M2Options_SetMM (bool value);
EXTERN bool M2Options_GetMM (void);
EXTERN void M2Options_SetMD (bool value);
EXTERN bool M2Options_GetMD (void);
EXTERN void M2Options_SetMMD (bool value);
EXTERN bool M2Options_GetMMD (void);
EXTERN void M2Options_SetMQ (const char *arg);
EXTERN void M2Options_SetMF (const char *arg);
EXTERN char *M2Options_GetMF (void);
EXTERN void M2Options_SetMT (const char *arg);
EXTERN void M2Options_SetMP (bool value);
EXTERN bool M2Options_GetMP (void);
EXTERN char *M2Options_GetDepTarget (void);
EXTERN void M2Options_SetObj (const char *arg);
EXTERN char *M2Options_GetObj (void);
EXTERN void M2Options_SetM2Prefix (const char *arg);
EXTERN char *M2Options_GetM2Prefix (void);
EXTERN void M2Options_SetM2PathName (const char *arg);
EXTERN char *M2Options_GetM2PathName (void);
EXTERN int M2Options_SetUninitVariableChecking (bool value, const char *arg);
EXTERN void M2Options_SetCaseEnumChecking (bool value);
EXTERN void M2Options_SetDebugBuiltins (bool value);
EXTERN void M2Options_SetIBMLongDouble (bool value);
EXTERN bool M2Options_GetIBMLongDouble (void);
EXTERN void M2Options_SetIEEELongDouble (bool value);
EXTERN bool M2Options_GetIEEELongDouble (void);
EXTERN bool M2Options_GetDumpDeclFilename (void);
EXTERN void M2Options_SetDumpDeclFilename (bool value, const char *arg);
EXTERN bool M2Options_GetDumpQuadFilename (void);
EXTERN void M2Options_SetDumpQuadFilename (bool value, const char *arg);
EXTERN bool M2Options_GetDumpGimpleFilename (void);
EXTERN void M2Options_SetDumpGimpleFilename (bool value, const char *arg);
EXTERN void M2Options_SetM2DumpFilter (bool value, const char *args);
EXTERN char *M2Options_GetM2DumpFilter (void);
EXTERN void M2Options_SetM2DebugTraceFilter (bool value, const char *arg);
EXTERN bool M2Options_SetM2Dump (bool value, const char *arg);
EXTERN bool M2Options_GetDumpGimple (void);

#undef EXTERN
#endif /* m2options_h.  */
