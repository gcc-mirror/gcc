/* Definitions of target machine for GCC, for SPARC64, ELF.
   Copyright (C) 1994-2013 Free Software Foundation, Inc.
   Contributed by Doug Evans, dje@cygnus.com.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* A 64 bit v9 compiler in a Medium/Anywhere code model environment.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
(MASK_V9 + MASK_PTR64 + MASK_64BIT + MASK_HARD_QUAD \
 + MASK_APP_REGS + MASK_FPU + MASK_STACK_BIAS + MASK_LONG_DOUBLE_128)

#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_EMBMEDANY

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

/* __svr4__ is used by the C library (FIXME) */
#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "-D__svr4__"

#undef ASM_SPEC
#define ASM_SPEC "\
-s %{fpic|fPIC|fpie|fPIE:-K PIC} \
%(asm_cpu) %(asm_arch) \
"

/* This is taken from sol2.h.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{v:-V} \
"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   crtend.o%s crtn.o%s"

/* Use the default (for now).  */
#undef LIB_SPEC

#undef  LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX  "."

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf ((LABEL), "*.L%s%ld", (PREFIX), (long)(NUM))

/* ??? This should be 32 bits for v9 but what can we do?  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 128
