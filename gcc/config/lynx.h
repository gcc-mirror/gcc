/* Target independent definitions for LynxOS.
   Copyright (C) 1993, 1994, 1995, 1996, 1999, 2000, 2002
   Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* LynxOS is a multi-platform Unix, similar to SVR3, but not identical.
   We can get quite a bit from generic svr3, but have to do some overrides.  */

#include "svr3.h"

/* Define various macros, depending on the combination of flags.  */

#undef CPP_SPEC
#define CPP_SPEC "%{mthreads:-D_MULTITHREADED}  \
  %{mposix:-D_POSIX_SOURCE}  \
  %{msystem-v:-I/usr/include_v}"

/* No asm spec needed, since using GNU assembler always.  */

/* No linker spec needed, since using GNU linker always.  */

#undef LIB_SPEC
#define LIB_SPEC "%{mthreads:-L/lib/thread/}  \
  %{msystem-v:-lc_v}  \
  %{!msystem-v:%{mposix:-lc_p} -lc -lm}"

/* Set the appropriate names for the Lynx startfiles.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{p:%{mthreads:thread/pinit1.o%s}%{!mthreads:pinit1.o%s}}%{!p:%{msystem-v:vinit1.o%s -e_start}%{!msystem-v:%{mthreads:thread/init1.o%s}%{!mthreads:init1.o%s}}}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "%{p:_etext.o%s}%{!p:initn.o%s}"

/* Override the svr3 versions.  */

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

/* We want to output DBX (stabs) debugging information normally.  */

#define DBX_DEBUGGING_INFO 1
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* It is convenient to be able to generate standard coff debugging
   if requested via -gcoff.  */

#define SDB_DEBUGGING_INFO 1

/* Be function-relative for block and source line stab directives.  */

#define DBX_BLOCKS_FUNCTION_RELATIVE 1

/* but, to make this work, functions must appear prior to line info */

#define DBX_FUNCTION_FIRST

/* Generate a blank trailing N_SO to mark the end of the .o file, since
   we can't depend upon the linker to mark .o file boundaries with
   embedded stabs.  */

#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)			\
  fprintf (FILE,							\
	   "\t.text\n\t.stabs \"\",%d,0,0,Letext\nLetext:\n", N_SO)

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    fprintf (file, ".stabn 68,0,%d,.LM%d-",		\
	     line, sym_lineno);				\
    assemble_name (file,				\
		   XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0)); \
    fprintf (file, "\n.LM%d:\n", sym_lineno);		\
    sym_lineno += 1; }

/* Handle #pragma pack and sometimes #pragma weak.  */

#define HANDLE_SYSV_PRAGMA 1

/* Some additional command-line options.  */

#define TARGET_THREADS	(target_flags & MASK_THREADS)
#define MASK_THREADS	0x40000000

#define TARGET_POSIX	(target_flags & MASK_POSIX)
#define MASK_POSIX	0x20000000

#define TARGET_SYSTEM_V	(target_flags & MASK_SYSTEM_V)
#define MASK_SYSTEM_V	0x10000000

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
    {"threads",		MASK_THREADS},		\
    {"posix",		MASK_POSIX},		\
    {"system-v",	MASK_SYSTEM_V},

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS \
do {								\
  if (TARGET_SYSTEM_V && profile_flag)				\
    warning ("-msystem-v and -p are incompatible");		\
  if (TARGET_SYSTEM_V && TARGET_THREADS)			\
    warning ("-msystem-v and -mthreads are incompatible");	\
} while (0)

/* Since init.o et al put all sorts of stuff into the init section,
   we can't use the standard init section support in crtbegin.o.  */

#undef INIT_SECTION_ASM_OP

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_fini

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  FINI_SECTION_FUNCTION

#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors"
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors"

#undef DO_GLOBAL_CTORS_BODY
#undef DO_GLOBAL_DTORS_BODY

/* LynxOS doesn't have mcount.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(file, profile_label_no)
