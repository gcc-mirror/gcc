/* Target definitions for GNU compiler for mc680x0 running System V.4
   Copyright (C) 1991, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 2003
   Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@monkeys.com) and
   Fred Fish (fnf@cygnus.com).

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

/* Target OS builtins.  */
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("unix");		\
	builtin_define_std ("m68k");		\
	builtin_define ("__svr4__");		\
	builtin_define ("__motorola__");	\
	builtin_assert ("system=unix");		\
	builtin_assert ("system=svr4");		\
   }						\
  while (0)

#ifndef TARGET_DEFAULT
#define	TARGET_DEFAULT (MASK_BITFIELD|MASK_68881|MASK_68020)
#endif

/*  Override the definition of NO_DOLLAR_IN_LABEL in svr4.h, for special
    g++ assembler names.  When this is defined, g++ uses embedded '.'
    characters and some m68k assemblers have problems with this.  The
    chances are much greater that any particular assembler will permit
    embedded '$' characters.  */

#undef NO_DOLLAR_IN_LABEL

/* Define PCC_STATIC_STRUCT_RETURN if the convention on the target machine
   is to use the nonreentrant technique for returning structure and union
   values, as commonly implemented by the AT&T Portable C Compiler (PCC).
   When defined, the gcc option -fpcc-struct-return can be used to cause
   this form to be generated.  When undefined, the option does nothing.
   For m68k SVR4, the convention is to use a reentrant technique compatible
   with the gcc default, so override the definition of this macro in m68k.h */

#undef PCC_STATIC_STRUCT_RETURN

/* Test to see if the target includes a 68881 by default, and use CPP_SPEC
   to control whether or not __HAVE_68881__ is defined by default or not.
   If a 68881 is the default, gcc will use inline 68881 instructions, by
   predefining __HAVE_68881__, unless -msoft-float is specified.
   If a 68881 is not the default, gcc will only define __HAVE_68881__ if
   -m68881 is specified.  */

#if TARGET_DEFAULT & MASK_68881
#define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__}"
#else
#define CPP_SPEC "%{m68881:-D__HAVE_68881__}"
#endif

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  We override the definition in m68k.h
   and match the way the native m68k/SVR4 compiler does profiling, with the
   address of the profile counter in a1, not a0, and using bsr rather
   than jsr.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
  asm_fprintf ((FILE), "\tlea.l\t(%LLP%d,%Rpc),%Ra1\n\tbsr\t_mcount\n", \
	       (LABELNO))

/* Local common symbols are declared to the assembler with ".lcomm" rather
   than ".bss", so override the definition in svr4.h */
/* ??? svr4.h no longer defines this, and this is only used by m68k/amix.h.  */

#undef BSS_ASM_OP
#define BSS_ASM_OP	"\t.lcomm\t"

/* Register in which address to store a structure value is passed to a
   function.  The default in m68k.h is a1.  For m68k/SVR4 it is a0.  */

#undef STRUCT_VALUE_REGNUM
#define STRUCT_VALUE_REGNUM 8

/* Register in which static-chain is passed to a function.  The
   default in m68k.h is a0, but that is already the struct value
   regnum.  Make it a1 instead.  */

#undef STATIC_CHAIN_REGNUM
#define STATIC_CHAIN_REGNUM 9

#define ASM_COMMENT_START "#"

/* Define how the m68k registers should be numbered for Dwarf output.
   The numbering provided here should be compatible with the native
   SVR4 SDB debugger in the m68k/SVR4 reference port, where d0-d7
   are 0-7, a0-a8 are 8-15, and fp0-fp7 are 16-23.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* The ASM_OUTPUT_SKIP macro is first defined in m68k.h, using ".skip".
   It is then overridden by m68k/sgs.h to use ".space", and again by svr4.h
   to use ".zero".  The m68k/SVR4 assembler uses ".space", so repeat the
   definition from m68k/sgs.h here.  Note that ASM_NO_SKIP_IN_TEXT is
   defined in m68k/sgs.h, so we don't have to repeat it here.  */

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "%s%u\n", SPACE_ASM_OP, (int)(SIZE))

/* 1 if N is a possible register number for a function value.
   For m68k/SVR4 allow d0, a0, or fp0 as return registers, for integral,
   pointer, or floating types, respectively. Reject fp0 if not using a
   68881 coprocessor.  */

#undef FUNCTION_VALUE_REGNO_P
#define FUNCTION_VALUE_REGNO_P(N) \
  ((N) == 0 || (N) == 8 || (TARGET_68881 && (N) == 16))

/* Define this to be true when FUNCTION_VALUE_REGNO_P is true for
   more than one register.  */

#undef NEEDS_UNTYPED_CALL
#define NEEDS_UNTYPED_CALL 1

/* Define how to generate (in the callee) the output value of a function
   and how to find (in the caller) the value returned by a function.  VALTYPE
   is the data type of the value (as a tree).  If the precise function being
   called is known, FUNC is its FUNCTION_DECL; otherwise, FUNC is 0.
   For m68k/SVR4 generate the result in d0, a0, or fp0 as appropriate.  */
   
#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE, FUNC)					\
  (TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_68881			\
   ? gen_rtx_REG (TYPE_MODE (VALTYPE), 16)				\
   : (POINTER_TYPE_P (VALTYPE)						\
      ? gen_rtx_REG (TYPE_MODE (VALTYPE), 8)				\
      : gen_rtx_REG (TYPE_MODE (VALTYPE), 0)))

/* For compatibility with the large body of existing code which does not
   always properly declare external functions returning pointer types, the
   m68k/SVR4 convention is to copy the value returned for pointer functions
   from a0 to d0 in the function epilogue, so that callers that have
   neglected to properly declare the callee can still find the correct return
   value.  */

#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)				\
do {									\
  if (current_function_returns_pointer					\
      && ! find_equiv_reg (0, get_last_insn (), 0, 0, 0, 8, Pmode))	\
    asm_fprintf (FILE, "\tmov.l %Ra0,%Rd0\n");				\
} while (0);

/* Define how to find the value returned by a library function assuming the
   value has mode MODE.
   For m68k/SVR4 look for integer values in d0, pointer values in d0
   (returned in both d0 and a0), and floating values in fp0.  */

#undef LIBCALL_VALUE
#define LIBCALL_VALUE(MODE)						\
  ((((MODE) == SFmode || (MODE) == DFmode || (MODE) == XFmode)		\
    && TARGET_68881)							\
   ? gen_rtx_REG ((MODE), 16)						\
   : gen_rtx_REG ((MODE), 0))

/* Boundary (in *bits*) on which stack pointer should be aligned.
   The m68k/SVR4 convention is to keep the stack pointer longword aligned.  */
 
#undef STACK_BOUNDARY
#define STACK_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.
   For m68k/SVR4, this is the next longword boundary.  */

#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY 32

/* No data type wants to be aligned rounder than this.
   For m68k/SVR4, some types (doubles for example) are aligned on 8 byte
   boundaries */
	
#undef BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 64

/* SVR4 m68k assembler is bitching on the `comm i,1,1' which asks for 
   1 byte alignment. Don't generate alignment for COMMON seems to be
   safer until we the assembler is fixed.  */
#undef ASM_OUTPUT_ALIGNED_COMMON
/* Same problem with this one.  */
#undef ASM_OUTPUT_ALIGNED_LOCAL

/* The `string' directive on m68k svr4 does not handle string with
   escape char (ie., `\') right. Use normal way to output ASCII bytes
   seems to be safer.  */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)				\
do {								\
  register size_t sp = 0, limit = (LEN);			\
  fputs (integer_asm_op (1, TRUE), (FILE));			\
  do {								\
    int ch = (PTR)[sp];						\
    if (ch > ' ' && ! (ch & 0x80) && ch != '\\')		\
      {								\
	fprintf ((FILE), "'%c", ch);				\
      }								\
    else							\
      {								\
	fprintf ((FILE), "0x%x", ch);				\
      }								\
    if (++sp < limit)						\
      {								\
	if ((sp % 10) == 0)					\
	  {							\
	    fprintf ((FILE), "\n%s", integer_asm_op (1, TRUE));	\
	  }							\
	else							\
	  {							\
	    putc (',', (FILE));					\
	  }							\
      }								\
  } while (sp < limit);						\
  putc ('\n', (FILE));						\
} while (0)

/* SVR4 m68k assembler is bitching on the syntax `2.b'.
   So use the "LLDnnn-LLnnn" format.  Define LLDnnn after the table.  */

#undef ASM_OUTPUT_CASE_END
#define ASM_OUTPUT_CASE_END(FILE,NUM,TABLE)				\
do {									\
  if (switch_table_difference_label_flag)				\
    asm_fprintf ((FILE), "%s%LLD%d,%LL%d\n", SET_ASM_OP, (NUM), (NUM)); \
  switch_table_difference_label_flag = 0;				\
} while (0)

extern int switch_table_difference_label_flag;

#undef ASM_OUTPUT_COMMON
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (int)(SIZE)))

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (int)(SIZE)))

/* Override the definition in svr4.h. In m68k svr4, using swbeg is the 
   standard way to do switch table.  */
#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE,PREFIX,NUM,TABLE)		\
  fprintf ((FILE), "%s&%d\n", SWBEG_ASM_OP, XVECLEN (PATTERN (TABLE), 1));

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On m68k svr4, the trampoline is different from the generic version
   in that we use a1 as the static call chain.  */

#undef TRAMPOLINE_TEMPLATE
#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  assemble_aligned_integer (2, GEN_INT (0x227a));			\
  assemble_aligned_integer (2, GEN_INT (8));				\
  assemble_aligned_integer (2, GEN_INT (0x2f3a));			\
  assemble_aligned_integer (2, GEN_INT (8));				\
  assemble_aligned_integer (2, GEN_INT (0x4e75));			\
  assemble_aligned_integer (4, const0_rtx);				\
  assemble_aligned_integer (4, const0_rtx);				\
}

/* Redefine since we are using a different trampoline */
#undef TRAMPOLINE_SIZE
#define TRAMPOLINE_SIZE 18

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)                       \
{                                                                       \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 10)), CXT); \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 14)), FNADDR); \
}
