/* Configuration file for an alpha OpenBSD target.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* We settle for little endian for now.  */
#define TARGET_ENDIAN_DEFAULT 0

#define OBSD_NO_DYNAMIC_LIBRARIES
#define OBSD_HAS_DECLARE_FUNCTION_NAME
#define OBSD_HAS_DECLARE_FUNCTION_SIZE
#define OBSD_HAS_DECLARE_OBJECT

/* alpha ecoff supports only weak aliases, see below.  */
#define ASM_WEAKEN_LABEL(FILE,NAME) ASM_OUTPUT_WEAK_ALIAS (FILE,NAME,0)

#include <openbsd.h>

/* Controlling the compilation driver.  */

/* alpha needs __start.  */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{!nostdlib:%{!r*:%{!e*:-e __start}}} -dc -dp %{assert*}"

/* run-time target specifications */
#define TARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define ("__OpenBSD__");		\
	builtin_define ("__ANSI_COMPAT");	\
	builtin_define ("__unix__");		\
	builtin_assert ("system=unix");		\
    } while (0)

/* Layout of source language data types.  */

/* This must agree with <machine/ansi.h> */
#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define LOCAL_LABEL_PREFIX	"."

/* We don't have an init section yet.  */
#undef HAS_INIT_SECTION

/* collect2 support (assembler format: macros for initialization).  */

/* Don't tell collect2 we use COFF as we don't have (yet ?) a dynamic ld
   library with the proper functions to handle this -> collect2 will
   default to using nm.  */
#undef OBJECT_FORMAT_COFF
#undef EXTENDED_COFF

/* Assembler format: exception region output.  */

/* All configurations that don't use elf must be explicit about not using
   dwarf unwind information. egcs doesn't try too hard to check internal
   configuration files...  */
#ifdef INCOMING_RETURN_ADDR_RTX
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 0
#endif

/* Assembler format: file framework.  */

/* Taken from alpha/osf.h. This used to be common to all alpha
   configurations, but elf has departed from it.
   Check alpha/alpha.h, alpha/osf.h for it when egcs is upgraded.  */
#ifndef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");                          \
  fprintf (FILE, "\t.set noat\n");				\
  if (TARGET_SUPPORT_ARCH)					\
    fprintf (FILE, "\t.arch %s\n",				\
             TARGET_CPU_EV6 ? "ev6"				\
	     : (TARGET_CPU_EV5					\
		? (TARGET_MAX ? "pca56" : TARGET_BWX ? "ev56" : "ev5") \
		: "ev4"));					\
								\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}
#endif

/* Assembler format: label output.  */

#define ASM_OUTPUT_WEAK_ALIAS(FILE,NAME,VALUE)	\
 do {						\
  fputs ("\t.weakext\t", FILE);			\
  assemble_name (FILE, NAME);			\
  if (VALUE)					\
    {						\
      fputs (" , ", FILE);			\
      assemble_name (FILE, VALUE);		\
    }						\
  fputc ('\n', FILE);				\
 } while (0)


