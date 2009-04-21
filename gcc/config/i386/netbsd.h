/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
   2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      NETBSD_OS_CPP_BUILTINS_AOUT();		\
    }						\
  while (0)

#define TARGET_VERSION fprintf (stderr, " (NetBSD/i386 a.out)");

/* This goes away when the math-emulator is fixed */
#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT \
  (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_NO_FANCY_MATH_387)

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS			\
  { "netbsd_cpp_spec", NETBSD_CPP_SPEC },

#undef CPP_SPEC
#define CPP_SPEC "%(netbsd_cpp_spec)"


#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* i386 netbsd still uses old binutils that don't insert nops by default
   when the .align directive demands to insert extra space in the text
   segment.  */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.align %d,0x90\n", (LOG))

/* Profiling routines, partially copied from i386/osfrose.h.  */

/* Redefine this to use %eax instead of %edx.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
{									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tcall mcount@PLT\n");				\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tcall mcount\n");				\
    }									\
}

/* Until they use ELF or something that handles dwarf2 unwinds
   and initialization stuff better.  */
#define DWARF2_UNWIND_INFO 0

/* Redefine this so that it becomes "_GLOBAL_OFFSET_TABLE_" when the label
   prefix is added.  */
#undef GOT_SYMBOL_NAME
#define GOT_SYMBOL_NAME "GLOBAL_OFFSET_TABLE_"

/* Attempt to enable execute permissions on the stack.  */
#define ENABLE_EXECUTE_STACK NETBSD_ENABLE_EXECUTE_STACK
