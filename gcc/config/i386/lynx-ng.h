/* Definitions for Intel 386 running LynxOS, using Lynx's old as and ld.
   Copyright (C) 1993, 1995, 2002 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define TARGET_VERSION fprintf (stderr, " (80386, LYNX BSD syntax)"); 

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("unix");		\
	builtin_define_std ("I386");		\
	builtin_define_std ("Lynx");		\
	builtin_define_std ("IBITS32");		\
	builtin_assert ("system=unix");		\
	builtin_assert ("system=lynx");		\
    }						\
  while (0)

/* Provide required defaults for linker switches.  */

#undef LINK_SPEC
#define LINK_SPEC "-P1000 %{msystem-v:-V} %{mcoff:-k}"

/* Apparently LynxOS clobbers ebx when you call into the OS.  */

#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg*/ \
{  1, 1, 1, 1, 0, 0, 0, 1, 1,  1,  1,  1,  1,  1,  1,  1,  1 }

/* Prefix for internally generated assembler labels.  If we aren't using
   underscores, we are using prefix `.'s to identify labels that should
   be ignored, as in `i386/gas.h' --karl@cs.umb.edu  */

#undef  LPREFIX
#define LPREFIX ".L"

/* The prefix to add to user-visible assembler symbols.  */

#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* If user-symbols don't have underscores,
   then it must take more than `L' to identify
   a label that should be ignored.  */

/* This is how to store into the string BUF
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
  sprintf ((BUF), ".%s%ld", (PREFIX), (long)(NUMBER))
