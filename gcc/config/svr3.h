/* Operating system specific defines to be used when targeting GCC for
   generic System V Release 3 system.
   Copyright (C) 1991, 1996 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@monkeys.com).

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
Boston, MA 02111-1307, USA.

   To use this file, make up a file with a name like:

	?????svr3.h

   where ????? is replaced by the name of the basic hardware that you
   are targeting for.  Then, in the file ?????svr3.h, put something
   like:

	#include "?????.h"
	#include "svr3.h"

   followed by any really system-specific defines (or overrides of
   defines) which you find that you need.  For example, CPP_PREDEFINES
   is defined here with only the defined -Dunix and -DSVR3.  You should
   probably override that in your target-specific ?????svr3.h file
   with a set of defines that includes these, but also contains an
   appropriate define for the type of hardware that you are targeting.
*/

/* Define a symbol indicating that we are using svr3.h.  */
#define USING_SVR3_H

/* Define a symbol so that libgcc* can know what sort of operating
   environment and assembler syntax we are targeting for.  */
#define SVR3_target

/* Cpp, assembler, linker, library, and startfile spec's.  */

/* You should redefine CPP_PREDEFINES in any file which includes this one.
   The definition should be appropriate for the type of target system
   involved, and it should include any -A (assertion) options which are
   appropriate for the given target system.  */

#undef CPP_PREDEFINES

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
  do { output_file_directive ((FILE), main_input_filename);	\
       if (optimize) { ASM_FILE_START_1 (FILE); }		\
     } while (0)

/* By default, do nothing: a few machines support .optim, but not most.  */
#undef ASM_FILE_START_1
#define ASM_FILE_START_1(FILE)

/* This says how to output an assembler line
   to define a global common symbol.  */
/* We don't use ROUNDED because the standard compiler doesn't,
   and the linker gives error messages if a common symbol
   has more than one length value.  */

#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

/* Note that using bss_section here caused errors
   in building shared libraries on system V.3.  */
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do {							\
    int align = exact_log2 (ROUNDED);			\
    if (align > 2) align = 2;				\
    data_section ();					\
    ASM_OUTPUT_ALIGN ((FILE), align == -1 ? 2 : align);	\
    ASM_OUTPUT_LABEL ((FILE), (NAME));			\
    fprintf ((FILE), "\t.set .,.+%u\n", (ROUNDED));	\
  } while (0)

#if 0 /* For now, let's leave these machine-specific.  */
/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}"

#ifdef CROSS_COMPILE
#define LIB_SPEC "-lc crtn.o%s"
#else
#define LIB_SPEC "%{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp} -lc crtn.o%s"
#endif

/* Special flags for the linker.  I don't know what they do.  */

#define LINK_SPEC "%{T*} %{z:-lm}"
#endif

/* Allow #sccs in preprocessor.  */

#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "\t.ident \"%s\"\n", NAME);

/* Use periods rather than dollar signs in special g++ assembler names.  */

#define NO_DOLLAR_IN_LABEL

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* System V Release 3 uses COFF debugging info.  */

#define SDB_DEBUGGING_INFO

/* We don't want to output DBX debugging information.  */

#undef DBX_DEBUGGING_INFO

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

/* Assembler pseudos to introduce constants of various size.  These
   definitions should work for most svr3 systems.  */

#undef ASM_BYTE_OP
#define ASM_BYTE_OP "\t.byte"

/* The prefix to add to user-visible assembler symbols.

   For System V Release 3 the convention is to prepend a leading
   underscore onto user-level symbol names.  */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.

   For most svr3 systems, the convention is that any symbol which begins
   with a period is not put into the linker symbol table by the assembler.  */

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  asm_fprintf (FILE, "%0L%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.

   For most svr3 systems, the convention is that any symbol which begins
   with a period is not put into the linker symbol table by the assembler.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%s%d", LOCAL_LABEL_PREFIX, PREFIX, NUM)

/* We want local labels to start with period if made with asm_fprintf.  */
#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

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

#ifndef STACK_GROWS_DOWNWARD

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

/* Add extra sections .rodata, .init and .fini.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_init, in_fini

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  CONST_SECTION_FUNCTION					\
  INIT_SECTION_FUNCTION						\
  FINI_SECTION_FUNCTION

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
	  || (DECL_INITIAL (DECL) != error_mark_node			\
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
