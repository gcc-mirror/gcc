/* init.c initializes the modules of the GNU Modula-2 front end.

Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

#include "init.h"
#include "config.h"
#include "system.h"

#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__  */
#define EXTERN extern
#endif /* !__GNUG__  */

EXTERN void _M2_M2Bitset_init (int argc, char *argv[]);
EXTERN void _M2_Debug_init (int argc, char *argv[]);
EXTERN void _M2_M2Defaults_init (int argc, char *argv[]);
EXTERN void _M2_Environment_init (int argc, char *argv[]);
EXTERN void _M2_RTExceptions_init (int argc, char *argv[]);
EXTERN void _M2_M2EXCEPTION_init (int argc, char *argv[]);
EXTERN void _M2_M2RTS_init (int argc, char *argv[]);
EXTERN void _M2_SysExceptions_init (int argc, char *argv[]);
EXTERN void _M2_DynamicStrings_init (int argc, char *argv[]);
EXTERN void _M2_Assertion_init (int argc, char *argv[]);
EXTERN void _M2_FormatStrings_init (int argc, char *argv[]);
EXTERN void _M2_FIO_init (int argc, char *argv[]);
EXTERN void _M2_SFIO_init (int argc, char *argv[]);
EXTERN void _M2_SArgs_init (int argc, char *argv[]);
EXTERN void _M2_Lists_init (int argc, char *argv[]);
EXTERN void _M2_UnixArgs_init (int argc, char *argv[]);
EXTERN void _M2_Args_init (int argc, char *argv[]);
EXTERN void _M2_wrapc_init (int argc, char *argv[]);
EXTERN void _M2_TimeString_init (int argc, char *argv[]);
EXTERN void _M2_IO_init (int argc, char *argv[]);
EXTERN void _M2_StdIO_init (int argc, char *argv[]);
EXTERN void _M2_CmdArgs_init (int argc, char *argv[]);
EXTERN void _M2_M2Preprocess_init (int argc, char *argv[]);
EXTERN void _M2_M2Error_init (int argc, char *argv[]);
EXTERN void _M2_M2Search_init (int argc, char *argv[]);
EXTERN void _M2_Indexing_init (int argc, char *argv[]);
EXTERN void _M2_NameKey_init (int argc, char *argv[]);
EXTERN void _M2_NumberIO_init (int argc, char *argv[]);
EXTERN void _M2_FpuIO_init (int argc, char *argv[]);
EXTERN void _M2_SysStorage_init (int argc, char *argv[]);
EXTERN void _M2_Storage_init (int argc, char *argv[]);
EXTERN void _M2_StrIO_init (int argc, char *argv[]);
EXTERN void _M2_M2Debug_init (int argc, char *argv[]);
EXTERN void _M2_M2Batch_init (int argc, char *argv[]);
EXTERN void _M2_StrLib_init (int argc, char *argv[]);
EXTERN void _M2_M2ALU_init (int argc, char *argv[]);
EXTERN void _M2_M2Options_init (int argc, char *argv[]);
EXTERN void _M2_M2Comp_init (int argc, char *argv[]);
EXTERN void _M2_M2LexBuf_init (int argc, char *argv[]);
EXTERN void _M2_SymbolTable_init (int argc, char *argv[]);
EXTERN void _M2_M2Base_init (int argc, char *argv[]);
EXTERN void _M2_M2Quads_init (int argc, char *argv[]);
EXTERN void _M2_SymbolKey_init (int argc, char *argv[]);
EXTERN void _M2_FifoQueue_init (int argc, char *argv[]);
EXTERN void _M2_M2Reserved_init (int argc, char *argv[]);
EXTERN void _M2_M2Const_init (int argc, char *argv[]);
EXTERN void _M2_P1SymBuild_init (int argc, char *argv[]);
EXTERN void _M2_P2SymBuild_init (int argc, char *argv[]);
EXTERN void _M2_P3SymBuild_init (int argc, char *argv[]);
EXTERN void _M2_M2System_init (int argc, char *argv[]);
EXTERN void _M2_M2BasicBlock_init (int argc, char *argv[]);
EXTERN void _M2_M2Pass_init (int argc, char *argv[]);
EXTERN void _M2_M2Code_init (int argc, char *argv[]);
EXTERN void _M2_M2AsmUtil_init (int argc, char *argv[]);
EXTERN void _M2_M2FileName_init (int argc, char *argv[]);
EXTERN void _M2_M2Version_init (int argc, char *argv[]);
EXTERN void _M2_M2Students_init (int argc, char *argv[]);
EXTERN void _M2_StrCase_init (int argc, char *argv[]);
EXTERN void _M2_SymbolConversion_init (int argc, char *argv[]);
EXTERN void _M2_M2GCCDeclare_init (int argc, char *argv[]);
EXTERN void _M2_M2GenGCC_init (int argc, char *argv[]);
EXTERN void _M2_M2Range_init (int argc, char *argv[]);
EXTERN void _M2_M2Swig_init (int argc, char *argv[]);
EXTERN void _M2_M2MetaError_init (int argc, char *argv[]);
EXTERN void _M2_M2CaseList_init (int argc, char *argv[]);
EXTERN void _M2_PCSymBuild_init (int argc, char *argv[]);
EXTERN void _M2_PCBuild_init (int argc, char *argv[]);
EXTERN void _M2_Sets_init (int argc, char *argv[]);
EXTERN void _M2_dtoa_init (int argc, char *argv[]);
EXTERN void _M2_ldtoa_init (int argc, char *argv[]);
EXTERN void _M2_M2Check_init (int argc, char *argv[]);
EXTERN void _M2_M2SSA_init (int argc, char *argv[]);
EXTERN void exit (int);
EXTERN void M2Comp_compile (const char *filename);
EXTERN void RTExceptions_DefaultErrorCatch (void);


/* FrontEndInit - initialise the modules, this is a global
   initialisation.  This is called once.  */

void
init_FrontEndInit (void)
{
  _M2_Debug_init (0, NULL);
  _M2_RTExceptions_init (0, NULL);
  _M2_M2Defaults_init (0, NULL);
  _M2_Environment_init (0, NULL);
  _M2_M2EXCEPTION_init (0, NULL);
  _M2_M2RTS_init (0, NULL);
  _M2_SysExceptions_init (0, NULL);
  _M2_DynamicStrings_init (0, NULL);
  _M2_Assertion_init (0, NULL);
  _M2_FormatStrings_init (0, NULL);
  _M2_FIO_init (0, NULL);
  _M2_SFIO_init (0, NULL);
  _M2_SArgs_init (0, NULL);
  _M2_Lists_init (0, NULL);
  _M2_UnixArgs_init (0, NULL);
  _M2_Args_init (0, NULL);
  _M2_wrapc_init (0, NULL);
  _M2_TimeString_init (0, NULL);
  _M2_IO_init (0, NULL);
  _M2_StdIO_init (0, NULL);
  _M2_CmdArgs_init (0, NULL);
  _M2_FpuIO_init (0, NULL);
  _M2_SysStorage_init (0, NULL);
  _M2_Storage_init (0, NULL);
  _M2_StrIO_init (0, NULL);
  _M2_StrLib_init (0, NULL);
  _M2_dtoa_init (0, NULL);
  _M2_ldtoa_init (0, NULL);
  _M2_M2Search_init (0, NULL);
  _M2_M2Options_init (0, NULL);
}

/* PerCompilationInit - initialise the modules before compiling,
   filename.  This is to be called every time we compile a new file.  */

void
init_PerCompilationInit (const char *filename)
{
  _M2_M2Bitset_init (0, NULL);
  _M2_M2Preprocess_init (0, NULL);
  _M2_M2Error_init (0, NULL);
  _M2_Indexing_init (0, NULL);
  _M2_NameKey_init (0, NULL);
  _M2_NumberIO_init (0, NULL);
  _M2_M2Debug_init (0, NULL);
  _M2_M2Batch_init (0, NULL);
  _M2_M2ALU_init (0, NULL);
  _M2_M2Comp_init (0, NULL);
  _M2_M2LexBuf_init (0, NULL);
  _M2_SymbolTable_init (0, NULL);
  _M2_M2Base_init (0, NULL);
  _M2_M2Quads_init (0, NULL);
  _M2_SymbolKey_init (0, NULL);
  _M2_FifoQueue_init (0, NULL);
  _M2_M2Reserved_init (0, NULL);
  _M2_M2Const_init (0, NULL);
  _M2_P1SymBuild_init (0, NULL);
  _M2_P2SymBuild_init (0, NULL);
  _M2_P3SymBuild_init (0, NULL);
  _M2_M2System_init (0, NULL);
  _M2_M2BasicBlock_init (0, NULL);
  _M2_M2Pass_init (0, NULL);
  _M2_M2Code_init (0, NULL);
  _M2_M2AsmUtil_init (0, NULL);
  _M2_M2FileName_init (0, NULL);
  _M2_M2Version_init (0, NULL);
  _M2_M2Students_init (0, NULL);
  _M2_StrCase_init (0, NULL);
  _M2_SymbolConversion_init (0, NULL);
  _M2_M2GCCDeclare_init (0, NULL);
  _M2_M2GenGCC_init (0, NULL);
  _M2_M2Range_init (0, NULL);
  _M2_M2Swig_init (0, NULL);
  _M2_M2MetaError_init (0, NULL);
  _M2_M2CaseList_init (0, NULL);
  _M2_PCSymBuild_init (0, NULL);
  _M2_PCBuild_init (0, NULL);
  _M2_Sets_init (0, NULL);
  _M2_M2SSA_init (0, NULL);
  _M2_M2Check_init (0, NULL);
  M2Comp_compile (filename);
}
