/* Definitions for Intel 386 running system V, using gas.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "i386/gas.h"

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

#undef FRAME_POINTER_REQUIRED
#define FRAME_POINTER_REQUIRED \
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
	fprintf ((FILE), "\t.set .,.+%u\n", (ROUNDED));	\
      }							\
    else						\
      {							\
	fputs (".lcomm ", (FILE));			\
	assemble_name ((FILE), (NAME));			\
	fprintf ((FILE), ",%u\n", (ROUNDED));		\
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

/* Support const sections and the ctors and dtors sections for g++.
   Note that there appears to be two different ways to support const
   sections at the moment.  You can either #define the symbol
   READONLY_DATA_SECTION (giving it some code which switches to the
   readonly data section) or else you can #define the symbols
   EXTRA_SECTIONS, EXTRA_SECTION_FUNCTIONS, SELECT_SECTION, and
   SELECT_RTX_SECTION.  We do both here just to be on the safe side.
   However, use of the const section is turned off by default
   unless the specific tm.h file turns it on by defining
   USE_CONST_SECTION as 1.  */

/* Define a few machine-specific details of the implementation of
   constructors.

   The __CTORS_LIST__ goes in the .init section.  Define CTOR_LIST_BEGIN
   and CTOR_LIST_END to contribute to the .init section an instruction to
   push a word containing 0 (or some equivalent of that).

   Define ASM_OUTPUT_CONSTRUCTOR to push the address of the constructor.  */

#define USE_CONST_SECTION	0

#define INIT_SECTION_ASM_OP     ".section\t.init"
#define FINI_SECTION_ASM_OP     ".section .fini,\"x\""
#define CONST_SECTION_ASM_OP	".section\t.rodata, \"x\""
#define CTORS_SECTION_ASM_OP	INIT_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP    FINI_SECTION_ASM_OP

/* CTOR_LIST_BEGIN and CTOR_LIST_END are machine-dependent
   because they push on the stack.  */

#ifdef STACK_GROWS_DOWNWARD

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

#else

/* Constructor list on stack is in correct order.  Just call them.  */
#define DO_GLOBAL_CTORS_BODY					\
do {								\
  func_ptr *p, *beg = alloca (0);				\
  for (p = beg; *p; )						\
    (*p++) ();							\
} while (0)

#endif /* STACK_GROWS_DOWNWARD */

/* Add extra sections .init and .fini, in addition to .bss from att386.h. */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_bss, in_init, in_fini

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  CONST_SECTION_FUNCTION					\
  BSS_SECTION_FUNCTION						\
  INIT_SECTION_FUNCTION						\
  FINI_SECTION_FUNCTION

#define BSS_SECTION_FUNCTION					\
void								\
bss_section ()							\
{								\
  if (in_section != in_bss)					\
    {								\
      fprintf (asm_out_file, "\t%s\n", BSS_SECTION_ASM_OP);	\
      in_section = in_bss;					\
    }								\
}

#define INIT_SECTION_FUNCTION					\
void								\
init_section ()							\
{								\
  if (in_section != in_init)					\
    {								\
      fprintf (asm_out_file, "\t%s\n", INIT_SECTION_ASM_OP);	\
      in_section = in_init;					\
    }								\
}

#define FINI_SECTION_FUNCTION					\
void								\
fini_section ()							\
{								\
  if (in_section != in_fini)					\
    {								\
      fprintf (asm_out_file, "\t%s\n", FINI_SECTION_ASM_OP);	\
      in_section = in_fini;					\
    }								\
}

#define READONLY_DATA_SECTION() const_section ()

#define CONST_SECTION_FUNCTION						\
void									\
const_section ()							\
{									\
  extern void text_section();						\
  if (!USE_CONST_SECTION)						\
    text_section();							\
  else if (in_section != in_const)					\
    {									\
      fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP);		\
      in_section = in_const;						\
    }									\
}

/* The ctors and dtors sections are not normally put into use 
   by EXTRA_SECTIONS and EXTRA_SECTION_FUNCTIONS as defined in svr3.h,
   but it can't hurt to define these macros for whatever systems use them.  */
#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

/* This is machine-dependent
   because it needs to push something on the stack.  */
#undef ASM_OUTPUT_CONSTRUCTOR

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    fini_section ();                   				\
    fprintf (FILE, "%s\t ", ASM_LONG);					\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

#define SELECT_SECTION(DECL,RELOC)					\
{									\
  if (TREE_CODE (DECL) == STRING_CST)					\
    {									\
      if (! flag_writable_strings)					\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (DECL) == VAR_DECL)				\
    {									\
      if ((0 && RELOC)	/* should be (flag_pic && RELOC) */		\
	  || !TREE_READONLY (DECL) || TREE_SIDE_EFFECTS (DECL)		\
	  || !DECL_INITIAL (DECL)					\
	  || (DECL_INITIAL (DECL) != error_mark_node 			\
	      && !TREE_CONSTANT (DECL_INITIAL (DECL))))			\
	data_section ();						\
      else								\
	const_section ();						\
    }									\
  else									\
    const_section ();							\
}

/* A C statement or statements to switch to the appropriate
   section for output of RTX in mode MODE.  RTX is some kind
   of constant in RTL.  The argument MODE is redundant except
   in the case of a `const_int' rtx.  Currently, these always
   go into the const section.  */

#define SELECT_RTX_SECTION(MODE,RTX) const_section()

/* This is copied from i386/sysv3.h.  */

/* Define a few machine-specific details of the implementation of
   constructors.

   The __CTORS_LIST__ goes in the .init section.  Define CTOR_LIST_BEGIN
   and CTOR_LIST_END to contribute to the .init section an instruction to
   push a word containing 0 (or some equivalent of that).

   ASM_OUTPUT_CONSTRUCTOR should be defined to push the address of the
   constructor.  */

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP     ".section .init,\"x\""

#define CTOR_LIST_BEGIN				\
  asm (INIT_SECTION_ASM_OP);			\
  asm ("pushl $0")
#define CTOR_LIST_END CTOR_LIST_BEGIN

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    init_section ();				\
    fprintf (FILE, "\tpushl $");		\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, "\n");			\
  } while (0)
