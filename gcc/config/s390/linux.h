/* Definitions for Linux for S/390.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (uweigand@de.ibm.com).

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

#ifndef _LINUX_H
#define _LINUX_H

#undef SIZE_TYPE                       /* use default                      */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Linux for S/390)");

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dlinux -Asystem(linux) -Acpu(s390) -Amachine(s390) -D__s390__ -Asystem(unix) -Dunix -D__ELF__"

/* 
 * Caller save not (always) working in gcc-2.95.2
 */

#undef CC1_SPEC
#define CC1_SPEC "-fno-caller-saves"
#define CC1PLUS_SPEC "-fno-caller-saves"

#undef	LINK_SPEC
#ifdef CROSS_COMPILE
#define LINK_SPEC "-m elf_s390 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld.so.1 \
        -rpath-link=/usr/local/s390-ibm-linux/lib}} \
	%{static:-static}}}"
#else
#define LINK_SPEC "-m elf_s390 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
	%{static:-static}}}"
#endif

/* Need to define this. Otherwise define to BITS_PER_WORD in cexp.c.
   But BITS_PER_WORD depends on target flags, which are not defined in 
   cexpc.c.  */

#undef  WCHAR_TYPE
#define WCHAR_TYPE "int"
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Character to start a comment.  */

#define ASM_COMMENT_START "#"


/* Assembler pseudos to introduce constants of various size.  */

#define ASM_DOUBLE "\t.double"


/* Prefix for internally generated assembler labels.  */
#define LPREFIX ".L"

#define ASM_OUTPUT_LABELREF(FILE, NAME) \
  fprintf (FILE, "%s", NAME);  


/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#undef ASM_OUTPUT_LABEL
#define ASM_OUTPUT_LABEL(FILE, NAME)     \
  (assemble_name (FILE, NAME), fputs (":\n", FILE))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)  \
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),    \
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))


     /* internal macro to output long */
#define _ASM_OUTPUT_LONG(FILE, VALUE)                                   \
      fprintf (FILE, "\t.long\t0x%lX\n", VALUE);


/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  			\
  fprintf (FILE, "%s%s%d\n", integer_asm_op (UNITS_PER_WORD, TRUE), \
	   LPREFIX, VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) 		\
  fprintf (FILE, "%s%s%d-%s%d\n", integer_asm_op (UNITS_PER_WORD, TRUE), \
	   LPREFIX, VALUE, LPREFIX, REL)



/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE, LOG)	\
    if ((LOG)!=0) fprintf ((FILE), "\t.align\t%d\n", 1<<(LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE, SIZE)  \
  fprintf ((FILE), "\t.set\t.,.+%u\n", (SIZE))

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable (initialized) data.  */

#define DATA_SECTION_ASM_OP ".data"

/* Output before writable (uninitialized) data.  */

#define BSS_SECTION_ASM_OP ".bss"

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE, NAME)  \
  (fputs (".globl ", FILE), assemble_name (FILE, NAME), fputs ("\n", FILE))

/* Select section for constant in constant pool. 
   We are in the right section. 
   undef for 64 bit mode (linux64.h).
 */

#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, X, ALIGN)


/* Output code to add DELTA to the first argument, and then jump to FUNCTION.
   Used for C++ multiple inheritance.  */
#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION)              \
do {                                                                          \
  if (TARGET_64BIT)                                                           \
    {                                                                         \
      if (flag_pic)                                                           \
        {                                                                     \
          fprintf (FILE, "\tlarl  1,0f\n");                                   \
          fprintf (FILE, "\tagf   %d,0(1)\n",                                 \
                   aggregate_value_p (TREE_TYPE                               \
                                      (TREE_TYPE (FUNCTION))) ? 3 :2 );       \
          fprintf (FILE, "\tlarl  1,");                                       \
          assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));      \
          fprintf (FILE, "@GOTENT\n");                                        \
          fprintf (FILE, "\tlg    1,0(1)\n");                                 \
          fprintf (FILE, "\tbr    1\n");                                      \
          fprintf (FILE, "0:\t.long  ");	                              \
          fprintf (FILE, HOST_WIDE_INT_PRINT_DEC, (DELTA));                   \
          fprintf (FILE, "\n");			                              \
        }                                                                     \
      else                                                                    \
        {                                                                     \
          fprintf (FILE, "\tlarl  1,0f\n");                                   \
          fprintf (FILE, "\tagf   %d,0(1)\n",                                 \
          aggregate_value_p (TREE_TYPE                                        \
                             (TREE_TYPE (FUNCTION))) ? 3 :2 );                \
          fprintf (FILE, "\tjg  ");                                           \
          assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));      \
          fprintf (FILE, "\n");                                               \
          fprintf (FILE, "0:\t.long  ");		                      \
          fprintf (FILE, HOST_WIDE_INT_PRINT_DEC, (DELTA));                   \
          fprintf (FILE, "\n");			                              \
        }                                                                     \
    }                                                                         \
  else                                                                        \
    {                                                                         \
      if (flag_pic)                                                           \
        {                                                                     \
          fprintf (FILE, "\tbras  1,0f\n");                                   \
          fprintf (FILE, "\t.long _GLOBAL_OFFSET_TABLE_-.\n");                \
          fprintf (FILE, "\t.long  ");                                        \
          assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));      \
          fprintf (FILE, "@GOT\n");                                           \
          fprintf (FILE, "\t.long  ");		                              \
          fprintf (FILE, HOST_WIDE_INT_PRINT_DEC, (DELTA));                   \
          fprintf (FILE, "\n");			                              \
          fprintf (FILE, "0:\tal  %d,8(1)\n",                                 \
                   aggregate_value_p (TREE_TYPE                               \
                                      (TREE_TYPE (FUNCTION))) ? 3 : 2 );      \
          fprintf (FILE, "\tl     0,4(1)\n");                                 \
          fprintf (FILE, "\tal    1,0(1)\n");                                 \
          fprintf (FILE, "\talr   1,0\n");                                    \
          fprintf (FILE, "\tl     1,0(1)\n");                                 \
          fprintf (FILE, "\tbr    1\n");                                      \
        } else {                                                              \
          fprintf (FILE, "\tbras  1,0f\n");                                   \
          fprintf (FILE, "\t.long  ");                                        \
          assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));      \
          fprintf (FILE, "-.\n");                                             \
          fprintf (FILE, "\t.long  ");		                              \
          fprintf (FILE, HOST_WIDE_INT_PRINT_DEC, (DELTA));                   \
          fprintf (FILE, "\n");			                              \
          fprintf (FILE, "0:\tal  %d,4(1)\n",                                 \
                   aggregate_value_p (TREE_TYPE                               \
                                      (TREE_TYPE (FUNCTION))) ? 3 : 2 );      \
          fprintf (FILE, "\tal    1,0(1)\n");                                 \
          fprintf (FILE, "\tbr    1\n");                                      \
       }                                                                      \
    }                                                                         \
} while (0)

#endif
