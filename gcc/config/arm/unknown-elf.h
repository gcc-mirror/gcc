/* Definitions for non-Linux based ARM systems using ELF
   Copyright (C) 1998-2024 Free Software Foundation, Inc.
   Contributed by Catherine Moore <clm@cygnus.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* elfos.h should have already been included.  Now just override
   any conflicting definitions and add any extras.  */

/* Run-time Target Specification.  */

/* Default to using software floating point.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT	(0)
#endif

/* Now we define the strings used to build the spec file.  */
#define UNKNOWN_ELF_STARTFILE_SPEC	" crti%O%s crtbegin%O%s crt0%O%s"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC	\
  "%{Ofast|ffast-math|funsafe-math-optimizations:%{!shared:crtfastmath.o%s}} "	\
  UNKNOWN_ELF_STARTFILE_SPEC

#define UNKNOWN_ELF_ENDFILE_SPEC	"crtend%O%s crtn%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC	UNKNOWN_ELF_ENDFILE_SPEC

/* The __USES_INITFINI__ define is tested in newlib/libc/sys/arm/crt0.S
   to see if it needs to invoked _init() and _fini().  */
#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC  "-D__USES_INITFINI__"

#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Return a nonzero value if DECL has a section attribute.  */
#define IN_NAMED_SECTION_P(DECL)					\
  ((TREE_CODE (DECL) == FUNCTION_DECL || VAR_P (DECL))	\
   && DECL_SECTION_NAME (DECL) != NULL)

#undef  ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)   	\
  do									\
    {									\
      if (IN_NAMED_SECTION_P (DECL))					\
	switch_to_section (get_named_section (DECL, NULL, 0));		\
      else								\
	switch_to_section (bss_section);				\
      									\
      ASM_OUTPUT_ALIGN (FILE, floor_log2 (ALIGN / BITS_PER_UNIT));	\
									\
      last_assemble_variable_decl = DECL;				\
      ASM_DECLARE_OBJECT_NAME (FILE, NAME, DECL);			\
      ASM_OUTPUT_SKIP (FILE, SIZE ? (int)(SIZE) : 1);			\
    } 									\
  while (0)

#undef  ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
  do									\
    {									\
      if ((DECL) != NULL && IN_NAMED_SECTION_P (DECL))			\
	switch_to_section (get_named_section (DECL, NULL, 0));		\
      else								\
	switch_to_section (bss_section);				\
									\
      ASM_OUTPUT_ALIGN (FILE, floor_log2 (ALIGN / BITS_PER_UNIT));	\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
      fprintf (FILE, "\t.space\t%d\n", SIZE ? (int) SIZE : 1);		\
      fprintf (FILE, "\t.size\t%s, %d\n",				\
	       NAME, SIZE ? (int) SIZE : 1);				\
    }									\
  while (0)

/* The libgcc udivmod functions may throw exceptions.  If newlib is
   configured to support long longs in I/O, then printf will depend on
   udivmoddi4, which will depend on the exception unwind routines,
   which will depend on abort, which is defined in libc.  */
#undef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC "--start-group %G %{!nolibc:%L} --end-group"
