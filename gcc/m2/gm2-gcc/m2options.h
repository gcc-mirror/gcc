/* m2options.h header file for M2Options.mod.

Copyright (C) 2012-2026 Free Software Foundation, Inc.
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
EXTERN void M2Options_SetFloatValueFlag (bool value);
EXTERN void M2Options_SetWholeValueFlag (bool value);

EXTERN bool M2Options_GetISO (unsigned int token);
EXTERN bool M2Options_GetPIM (unsigned int token);
EXTERN bool M2Options_GetPIM2 (unsigned int token);
EXTERN bool M2Options_GetPIM3 (unsigned int token);
EXTERN bool M2Options_GetPIM4 (unsigned int token);
EXTERN bool M2Options_GetPositiveModFloorFlag (unsigned int token);
EXTERN bool M2Options_GetFloatValueFlag (unsigned int token);
EXTERN bool M2Options_GetWholeValueFlag (unsigned int token);

EXTERN void M2Options_SetCFlag (bool value);
EXTERN bool M2Options_GetCFlag (unsigned int token);
EXTERN void M2Options_SetPPOnly (bool value);
EXTERN bool M2Options_GetPPOnly (void);

EXTERN bool M2Options_SetGFlag (bool value);
EXTERN void M2Options_SetUselist (bool value, const char *filename);
EXTERN bool M2Options_SetAutoInitFlag (bool value);
EXTERN bool M2Options_GetAutoInitFlag (unsigned int tokno);
EXTERN void M2Options_SetPositiveModFloorFlag (bool value);
EXTERN bool M2Options_SetNilFlag (bool value);
EXTERN void M2Options_SetWholeDivFlag (bool value);
EXTERN void M2Options_SetIndexFlag (bool value);
EXTERN void M2Options_SetRangeFlag (bool value);
EXTERN bool M2Options_SetReturnFlag (bool value);
EXTERN bool M2Options_SetCaseElseFlag (bool value);
EXTERN bool M2Options_SetCheckAll (bool value);
EXTERN bool M2Options_SetWallFlag (bool value);
EXTERN void M2Options_SetExceptionsFlag (bool value);
EXTERN void M2Options_SetStyleFlag (bool value);
EXTERN void M2Options_SetPedantic (bool value);
EXTERN void M2Options_SetPedanticParamNames (bool value);
EXTERN void M2Options_SetPedanticCast (bool value);
EXTERN void M2Options_SetExtendedOpaque (bool value);
EXTERN bool M2Options_SetVerboseUnbounded (bool value);
EXTERN void M2Options_SetXCode (bool value);
EXTERN void M2Options_SetCompilerDebugging (bool value);
EXTERN void M2Options_SetInternalDebuggingFlag (bool on);
EXTERN void M2Options_SetQuadDebugging (bool value);
EXTERN bool M2Options_GetDebugTraceToken (void);
EXTERN bool M2Options_GetDebugTraceLine (void);
EXTERN void M2Options_SetDebugFunctionLineNumbers (bool value);
EXTERN bool M2Options_GetDebugFunctionLineNumbers (void);
EXTERN void M2Options_SetSources (bool value);
EXTERN void M2Options_SetSourcesFlag (bool value);
EXTERN bool M2Options_SetUnboundedByReference (bool value);
EXTERN void M2Options_SetDumpPragmas (bool value);
EXTERN void M2Options_SetDumpSystemExports (bool value);
EXTERN void M2Options_SetDumpBuiltins (bool value);
EXTERN bool M2Options_GetDumpBuiltins (void);
EXTERN void M2Options_SetOptimizing (unsigned int value);
EXTERN bool M2Options_SetQuiet (bool value);
EXTERN void M2Options_SetCC1Quiet (bool value);
EXTERN bool M2Options_SetQuietFlag (bool value);
EXTERN bool M2Options_SetCpp (bool value);
EXTERN void M2Options_SetSwig (bool value);
EXTERN void M2Options_SetSwigFlag (bool value);
EXTERN void M2Options_SetForcedLocation (location_t location);
EXTERN location_t M2Options_OverrideLocation (location_t location);
EXTERN void M2Options_SetStatistics (bool on);
EXTERN void M2Options_SetStatisticsFlag (bool on);
EXTERN bool M2Options_SetFileOffsetBits (bool value, unsigned int bits);
EXTERN unsigned int M2Options_GetFileOffsetBits (void);
EXTERN void M2Options_CppProg (const char *program);
EXTERN void M2Options_CppArg (const char *opt, const char *arg, bool joined);
EXTERN void M2Options_SetWholeProgramFlag (bool value);
EXTERN void M2Options_FinaliseOptions (void);
EXTERN void M2Options_SetDebugFunctionLineNumbers (bool value);
EXTERN void M2Options_SetGenerateStatementNote (bool value);
EXTERN bool M2Options_GetCpp (void);
EXTERN bool M2Options_GetLineDirectives (void);
EXTERN bool M2Options_GetM2gFlag (unsigned int token);
EXTERN bool M2Options_SetM2gFlag (bool value);
EXTERN bool M2Options_SetLowerCaseKeywordFlag (bool value);
EXTERN bool M2Options_SetVerboseFlag (bool value);
EXTERN void M2Options_SetUnusedVariableFlag (bool value);
EXTERN void M2Options_SetUnusedParameterFlag (bool value);
EXTERN void M2Options_SetStrictTypeFlag (bool value);
EXTERN void M2Options_SetCheckall (bool value);
EXTERN void M2Options_SetSaveTemps (bool value);
EXTERN void M2Options_SetSaveTempsDir (const char *arg);
EXTERN void M2Options_SetDumpDir (const char *arg);
EXTERN bool M2Options_GetSaveTemps (void);
EXTERN void M2Options_SetScaffoldStaticFlag (bool value);
EXTERN void M2Options_SetScaffoldDynamicFlag (bool value);
EXTERN void M2Options_SetScaffoldMainFlag (bool value);
EXTERN void M2Options_SetRuntimeModuleOverride (const char *override);
EXTERN void M2Options_SetGenModuleListFileName (bool value, const char *filename);
EXTERN void M2Options_SetSharedFlag (bool value);
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
EXTERN void M2Options_SetCaseEnumFlag (bool value);
EXTERN void M2Options_SetDebugBuiltinFlag (bool on);
EXTERN bool M2Options_GetDebugBuiltinFlag (unsigned int tokno);
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
EXTERN void M2Options_SetTimeReport (bool value);
EXTERN void M2Options_SetMemReport (bool value);
EXTERN void M2Options_SetWidesetFlag (bool value);
EXTERN bool M2Options_GetWidesetFlag (void);
EXTERN void M2Options_SetStrictTypeAssignmentFlag (bool value);
EXTERN void M2Options_SetStrictTypeReasonFlag (bool value);

#undef EXTERN
#endif /* m2options_h.  */
