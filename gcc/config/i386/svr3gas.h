/* Definitions for Intel 386 running system V, using gas.
   Copyright (C) 1992, 1996, 2000, 2002 Free Software Foundation, Inc.

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

#define TARGET_VERSION fprintf (stderr, " (80386, ATT syntax)"); 

/* Add stuff that normally comes from i386/sysv3.h */

/* longjmp may fail to restore the registers if called from the same
   function that called setjmp.  To compensate, the compiler avoids
   putting variables in registers in functions that use both setjmp
   and longjmp.  */

#define NON_SAVING_SETJMP \
  (current_function_calls_setjmp && current_function_calls_longjmp)

/* longjmp may fail to restore the stack pointer if the saved frame
   pointer is the same as the caller's frame pointer.  Requiring a frame
   pointer in any function that calls setjmp or longjmp avoids this
   problem, unless setjmp and longjmp are called from the same function.
   Since a frame pointer will be required in such a function, it is OK
   that the stack pointer is not restored.  */

#undef SUBTARGET_FRAME_POINTER_REQUIRED
#define SUBTARGET_FRAME_POINTER_REQUIRED \
  (current_function_calls_setjmp || current_function_calls_longjmp)

/* Modify ASM_OUTPUT_LOCAL slightly to test -msvr3-shlib, adapted to gas  */
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do {							\
    int align = exact_log2 (ROUNDED);			\
    if (align > 2) align = 2;				\
    if (TARGET_SVR3_SHLIB)				\
      {							\
	data_section ();				\
	ASM_OUTPUT_ALIGN ((FILE), align == -1 ? 2 : align); \
	ASM_OUTPUT_LABEL ((FILE), (NAME));		\
	fprintf ((FILE), "\t.set .,.+%u\n", (int)(ROUNDED));	\
      }							\
    else						\
      {							\
	fputs (".lcomm ", (FILE));			\
	assemble_name ((FILE), (NAME));			\
	fprintf ((FILE), ",%u\n", (int)(ROUNDED));	\
      }							\
  } while (0)

/* Add stuff that normally comes from i386/sysv3.h via svr3.h */

/* Define the actual types of some ANSI-mandated types.  These
   definitions should work for most SVR3 systems.  */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* ??? This stuff is copied from config/svr3.h.  In the future,
   this file should be rewritten to include config/svr3.h
   and override what isn't right.  */

#define INIT_SECTION_ASM_OP     "\t.section\t.init"
#define FINI_SECTION_ASM_OP     "\t.section .fini,\"x\""
#define CTORS_SECTION_ASM_OP	INIT_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP    FINI_SECTION_ASM_OP

/* CTOR_LIST_BEGIN and CTOR_LIST_END are machine-dependent
   because they push on the stack.  */
/* This is copied from i386/sysv3.h.  */

#define CTOR_LIST_BEGIN				\
  asm (INIT_SECTION_ASM_OP);			\
  asm ("pushl $0")
#define CTOR_LIST_END CTOR_LIST_BEGIN

/* Constructor list on stack is in reverse order.  Go to the end of the
   list and go backwards to call constructors in the right order.  */
#define DO_GLOBAL_CTORS_BODY					\
do {								\
  func_ptr *p, *beg = alloca (0);				\
  for (p = beg; *p; p++)					\
    ;								\
  while (p != beg)						\
    (*--p) ();							\
} while (0)

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_init, in_fini

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  INIT_SECTION_FUNCTION						\
  FINI_SECTION_FUNCTION

#define INIT_SECTION_FUNCTION					\
void								\
init_section ()							\
{								\
  if (in_section != in_init)					\
    {								\
      fprintf (asm_out_file, "%s\n", INIT_SECTION_ASM_OP);	\
      in_section = in_init;					\
    }								\
}

#define FINI_SECTION_FUNCTION					\
void								\
fini_section ()							\
{								\
  if (in_section != in_fini)					\
    {								\
      fprintf (asm_out_file, "%s\n", FINI_SECTION_ASM_OP);	\
      in_section = in_fini;					\
    }								\
}

#define TARGET_ASM_CONSTRUCTOR  ix86_svr3_asm_out_constructor
