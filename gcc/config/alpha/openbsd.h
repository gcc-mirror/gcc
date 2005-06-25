/* Configuration file for an alpha OpenBSD target.
   Copyright (C) 1999, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* We settle for little endian for now.  */
#define TARGET_ENDIAN_DEFAULT 0

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
   dwarf unwind information.  */
#ifdef INCOMING_RETURN_ADDR_RTX
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 0
#endif

/* Assembler format: label output.  */

/* alpha ecoff supports only weak aliases.  */
#undef ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE,NAME) ASM_OUTPUT_WEAK_ALIAS (FILE,NAME,0)

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


