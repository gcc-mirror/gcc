/* Xtensa/Elf configuration.
   Derived from the configuration for GCC for Intel i386 running Linux.
   Copyright (C) 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#define TARGET_OS_CPP_BUILTINS()				\
  do {								\
    builtin_define ("__ELF__");					\
  } while (0)

#define TARGET_SECTION_TYPE_FLAGS xtensa_multibss_section_type_flags

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#undef TARGET_VERSION
#define TARGET_VERSION fputs (" (Xtensa/ELF)", stderr);

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#undef ASM_SPEC
#define ASM_SPEC "%{v} %{mno-density:--no-density} \
                  %{mtext-section-literals:--text-section-literals} \
                  %{mno-text-section-literals:--no-text-section-literals} \
		  %{mtarget-align:--target-align} \
		  %{mno-target-align:--no-target-align} \
		  %{mlongcalls:--longcalls} \
		  %{mno-longcalls:--no-longcalls}"

#undef ASM_FINAL_SPEC

#undef LIB_SPEC
#define LIB_SPEC "-lc -lsim -lc -lhandlers-sim -lhal"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt1-sim%O%s crti%O%s crtbegin%O%s _vectors%O%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s crtn%O%s"  

#undef LINK_SPEC
#define LINK_SPEC \
 "%{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
    %{static:-static}}}"

/* Local compiler-generated symbols must have a prefix that the assembler
   understands.   By default, this is $, although some targets (e.g.,
   NetBSD-ELF) need to override this. */

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"."
#endif

/* By default, external symbols do not have an underscore prepended. */

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX	""
#endif

/* Define this macro if the assembler does not accept the character
   "." in label names.  By default constructors and destructors in G++
   have names that use ".".  If this macro is defined, these names
   are rewritten to avoid ".". */
#define NO_DOT_IN_LABEL

/* Define NO_DOLLAR_IN_LABEL in your favorite tm file if your assembler
   doesn't allow $ in symbol names.  */
#undef NO_DOLLAR_IN_LABEL

/* Do not force "-fpic" for this target.  */
#define XTENSA_ALWAYS_PIC 0

/* Redefine the standard ELF version of ASM_DECLARE_FUNCTION_SIZE to
   allow adding the ".end literal_prefix" directive at the end of the
   function.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do								\
    {								\
      if (!flag_inhibit_size_directive)				\
	ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);			\
      XTENSA_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL);		\
    }								\
  while (0)
