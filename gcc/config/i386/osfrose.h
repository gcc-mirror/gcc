/* Definitions of target machine for GNU compiler.
   Intel 386 (OSF/1 with OSF/rose) version.
   Copyright (C) 1991 Free Software Foundation, Inc.

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

#include "halfpic.h"
#include "i386gas.h"

#define OSF_OS

/* Use a more friendly abort which prints internal compiler error,
   rather than just dumping core.  */

#ifndef abort
#define abort fancy_abort
#endif

#define MASK_HALF_PIC     	0x40000000	/* Mask for half-pic code */
#define MASK_HALF_PIC_DEBUG	0x20000000	/* Debug flag */

#define TARGET_HALF_PIC	(target_flags & MASK_HALF_PIC)
#define TARGET_DEBUG	(target_flags & MASK_HALF_PIC_DEBUG)
#define HALF_PIC_DEBUG	TARGET_DEBUG

#ifdef SUBTARGET_SWITCHES
#undef SUBTARGET_SWITCHES
#endif
#define SUBTARGET_SWITCHES \
     { "half-pic",	 MASK_HALF_PIC},				\
     { "no-half-pic",	-MASK_HALF_PIC},				\
     { "debugb",	 MASK_HALF_PIC_DEBUG},

/* Prefix that appears before all global/static identifiers, except for
   temporary labels.  */

#define IDENTIFIER_PREFIX "_"

/* Suffix that appears after all global/static identifiers, except for
   temporary labels.  */

#define IDENTIFIER_SUFFIX ""

/* Change default predefines.  */
#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
#define CPP_PREDEFINES "-DOSF -DOSF1 -Dunix -Di386"

#ifdef  CPP_SPEC
#undef  CPP_SPEC
#endif
#define CPP_SPEC "\
%{.S:	-D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C} \
%{!.S:	-D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}"

#ifdef  CC1_SPEC
#undef  CC1_SPEC
#endif

/* Turn on -mpic-extern by default.  */
#define CC1_SPEC "\
%{gline:%{!g:%{!g0:%{!g1:%{!g2: -g1}}}}} \
%{pic-none:   -mno-half-pic} \
%{pic-lib:    -mhalf-pic} \
%{pic-extern: -mhalf-pic} \
%{pic-calls:  -mhalf-pic} \
%{!pic-*:     -mhalf-pic}"

#ifdef ASM_SPEC
#undef ASM_SPEC
#endif
#define ASM_SPEC       ""

#ifdef  LINK_SPEC
#undef  LINK_SPEC
#endif
#define LINK_SPEC      "%{v*: -v}                           \
	               %{!noshrlib: %{pic-none: -noshrlib} %{!pic-none: -warn_nopic}} \
	               %{nostdlib} %{noshrlib} %{glue}"

#ifdef  LIB_SPEC
#undef  LIB_SPEC
#endif

#define LIB_SPEC "-lc"

#ifdef  LIBG_SPEC
#undef  LIBG_SPEC
#endif
#define LIBG_SPEC      ""

#ifdef  STARTFILE_SPEC
#undef  STARTFILE_SPEC
#endif
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#undef TARGET_VERSION_INTERNAL
#undef TARGET_VERSION

#define I386_VERSION " 80386, OSF/rose objects"

#define TARGET_VERSION_INTERNAL(STREAM) fputs (I386_VERSION, STREAM)
#define TARGET_VERSION TARGET_VERSION_INTERNAL (stderr)

#ifdef  MD_EXEC_PREFIX
#undef  MD_EXEC_PREFIX
#endif
#define MD_EXEC_PREFIX		"/usr/ccs/gcc/"

#ifdef  MD_STARTFILE_PREFIX
#undef  MD_STARTFILE_PREFIX
#endif
#define MD_STARTFILE_PREFIX	"/usr/ccs/lib/"

/* Tell final.c we don't need a label passed to mcount.  */
#define NO_PROFILE_DATA

#undef  FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) fprintf (FILE, "\tcall _mcount\n")

/* Some machines may desire to change what optimizations are
   performed for various optimization levels.   This macro, if
   defined, is executed once just after the optimization level is
   determined and before the remainder of the command options have
   been parsed.  Values set in this macro are used as the default
   values for the other command line options.

   LEVEL is the optimization level specified; 2 if -O2 is
   specified, 1 if -O is specified, and 0 if neither is specified.  */

#define OPTIMIZATION_OPTIONS(LEVEL)					\
{									\
  flag_gnu_linker = FALSE;						\
									\
  if (LEVEL >= 3)							\
    flag_inline_functions = TRUE;					\
}

/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  On most machines, this can be defined as
   `CONSTANT_P (X)', but a few machines are more restrictive in
   which constant addresses are supported.

   `CONSTANT_P' accepts integer-values expressions whose values are
   not explicitly known, such as `symbol_ref', `label_ref', and
   `high' expressions and `const' arithmetic expressions, in
   addition to `const_int' and `const_double' expressions.  */

#ifdef CONSTANT_ADDRESS_P
#undef CONSTANT_ADDRESS_P
#endif
#define CONSTANT_ADDRESS_P(X)                                           \
  (CONSTANT_P (X) && (!HALF_PIC_P () || !HALF_PIC_ADDRESS_P (X)))

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address. */

#undef GO_IF_LEGITIMATE_ADDRESS
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (CONSTANT_P (X))							\
    {									\
      if (! HALF_PIC_P () || ! HALF_PIC_ADDRESS_P (X))			\
	goto ADDR;							\
    }									\
  else									\
    {									\
      GO_IF_INDEXING (X, ADDR);						\
									\
      if (GET_CODE (X) == PLUS)						\
	{								\
	  rtx x1 = XEXP (X, 1);						\
									\
	  if (CONSTANT_P (x1))						\
	    {								\
	      if (! HALF_PIC_P () || ! HALF_PIC_ADDRESS_P (x1))		\
		{							\
		  rtx x0 = XEXP (X, 0);					\
		  GO_IF_INDEXING (x0, ADDR);				\
		}							\
	    }								\
	}								\
    }									\
}

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.  */

#define OVERRIDE_OPTIONS						\
{									\
  if (TARGET_HALF_PIC)							\
    half_pic_init ();							\
}

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL has been created and stored in `DECL_RTL (DECL)'. 
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).

   The best way to modify the name string is by adding text to the
   beginning, with suitable punctuation to prevent any ambiguity. 
   Allocate the new name in `saveable_obstack'.  You will have to
   modify `ASM_OUTPUT_LABELREF' to remove and decode the added text
   and output the name accordingly.

   You can also check the information stored in the `symbol_ref' in
   the definition of `GO_IF_LEGITIMATE_ADDRESS' or
   `PRINT_OPERAND_ADDRESS'. */

#ifdef ENCODE_SECTION_INFO
#undef ENCODE_SECTION_INFO
#endif
#define ENCODE_SECTION_INFO(DECL)					\
do									\
  {									\
   if (HALF_PIC_P ())						        \
      HALF_PIC_ENCODE (DECL);						\
  }									\
while (0)


/* A C statement (sans semicolon) to output to the stdio stream
   STREAM any text necessary for declaring the name NAME of an
   initialized variable which is being defined.  This macro must
   output the label definition (perhaps using `ASM_OUTPUT_LABEL'). 
   The argument DECL is the `VAR_DECL' tree node representing the
   variable.

   If this macro is not defined, then the variable name is defined
   in the usual manner as a label (by means of `ASM_OUTPUT_LABEL').  */

#ifdef ASM_DECLARE_OBJECT_NAME
#undef ASM_DECLARE_OBJECT_NAME
#endif
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)			\
do									\
 {									\
   ASM_OUTPUT_LABEL(STREAM,NAME);                                       \
   HALF_PIC_DECLARE (NAME);						\
 }									\
while (0)

/* This is how to declare a function name. */

#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL)			\
do									\
 {									\
   ASM_OUTPUT_LABEL(STREAM,NAME);                                       \
   HALF_PIC_DECLARE (NAME);						\
 }									\
while (0)

/* This says what to print at the end of the assembly file */
#define ASM_FILE_END(STREAM)						\
do									\
  {									\
    if (HALF_PIC_P ())							\
      HALF_PIC_FINISH (STREAM);						\
  }									\
while (0)

/* Tell collect that the object format is OSF/rose.  */
#define OBJECT_FORMAT_ROSE

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Define this macro meaning that gcc should find the library 'libgcc.a'
   by hand, rather than passing the argument '-lgcc' to tell the linker
   to do the search */
#define LINK_LIBGCC_SPECIAL

/* A C statement to output assembler commands which will identify the object
  file as having been compile with GNU CC. We don't need or want this for
  OSF1. GDB doesn't need it and kdb doesn't like it */
#define ASM_IDENTIFY_GCC(FILE)

/* This is how to output an assembler line defining a `double' constant.
   Use "word" pseudos to avoid printing NaNs, infinity, etc.  */

/* This is how to output an assembler line defining a `double' constant.  */

#ifdef ASM_OUTPUT_DOUBLE
#undef ASM_OUTPUT_DOUBLE
#endif
#define ASM_OUTPUT_DOUBLE(STREAM,VALUE)					\
{									\
  union { double d; long l[2]; } u2;					\
  u2.d = VALUE;								\
  fprintf (STREAM, "\t.long\t0x%08lx\t\t# %.20g\n\t.long\t0x%08lx\n",	\
	   u2.l[0], u2.d, u2.l[1]);					\
}

/* This is how to output an assembler line defining a `float' constant.  */

#ifdef ASM_OUTPUT_FLOAT
#undef ASM_OUTPUT_FLOAT
#endif
#define ASM_OUTPUT_FLOAT(STREAM,VALUE)					\
{									\
  union { float f; long l; } u2;					\
  u2.f = VALUE;								\
  fprintf (STREAM, "\t.long\t0x%08lx\t\t# %.12g\n", u2.l, u2.f);	\
}

/* Generate calls to memcpy, etc., not bcopy, etc. */
#define TARGET_MEM_FUNCTIONS



/* Defines to be able to build libgcc.a with GCC.  */

#define perform_udivsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  dx = 0;								\
  ax = a;								\
  asm ("divl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b), "d" (dx));	\
  return ax;								\
}

#define perform_divsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  ax = a;								\
  asm ("cltd\n\tidivl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b));	\
  return ax;								\
}

#define perform_umodsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  dx = 0;								\
  ax = a;								\
  asm ("divl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b), "d" (dx));	\
  return dx;								\
}

#define perform_modsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  ax = a;								\
  asm ("cltd\n\tidivl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b));	\
  return dx;								\
}

#define perform_fixdfsi(a)						\
{									\
  auto unsigned short ostatus;						\
  auto unsigned short nstatus;						\
  auto int ret;								\
									\
  &ostatus;			/* guarantee these land in memory */	\
  &nstatus;								\
  &ret;									\
									\
  asm volatile ("fnstcw %0" : "=m" (ostatus));				\
  nstatus = ostatus | 0x0c00;						\
  asm volatile ("fldcw %0" : /* no outputs */ : "m" (nstatus));		\
  asm volatile ("fldl %0" : /* no outputs */ : "m" (a));		\
  asm volatile ("fistpl %0" : "=m" (ret));				\
  asm volatile ("fldcw %0" : /* no outputs */ : "m" (ostatus));		\
									\
  return ret;								\
}
