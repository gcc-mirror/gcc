/* Definitions of target machine for GNU compiler, for PRO.
   Copyright (C) 1996, 1997, 2002 Free Software Foundation, Inc.

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

/* Make GCC agree with types.h.  */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	if (c_language != clk_cplusplus		\
	    && !flag_iso)			\
	  {					\
	    builtin_define ("hppa");		\
	    builtin_define_std ("PWB");		\
	  }					\
	builtin_define ("__pro__");		\
	builtin_assert ("system=pro");		\
    }						\
  while (0)

/* Like the default, except no -lg.  */
#undef LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p: -L/lib/libp/ -lc}%{pg: -L/lib/libp/ -lc}"

/* hpux8 and later have C++ compatible include files, so do not
   pretend they are `extern "C"'.  */
#define NO_IMPLICIT_EXTERN_C

/* We don't want a crt0.o to get linked in automatically, we want the
   linker script to pull it in.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

/* The following two macros are identical to the ones in pa.h.  We need
   to override the macros in elfos.h on the rtems and pro ports.  */

/* This says how to output an assembler line to define a global common symbol
   with size SIZE (in bytes) and alignment ALIGN (in bits).  */

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGNED)		\
{ bss_section ();							\
  assemble_name ((FILE), (NAME));					\
  fputs ("\t.comm ", (FILE));						\
  fprintf ((FILE), "%d\n", MAX ((SIZE), ((ALIGNED) / BITS_PER_UNIT)));}

/* This says how to output an assembler line to define a local common symbol
   with size SIZE (in bytes) and alignment ALIGN (in bits).  */

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGNED)		\
{ bss_section ();							\
  fprintf ((FILE), "\t.align %d\n", ((ALIGNED) / BITS_PER_UNIT));	\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), "\n\t.block %d\n", (SIZE));}
