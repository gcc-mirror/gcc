/* Definitions for Linux for S/390.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (weigand@de.ibm.com).

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

#define IEEE_FLOAT 1
#define TARGET_IBM_FLOAT           0
#define TARGET_IEEE_FLOAT          1

#include <s390/s390.h>              /* Base s390 target machine definitions*/

#include <linux.h>

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

#define ASM_SHORT "\t.word"
#define ASM_LONG "\t.long"
#define ASM_QUAD "\t.quad"
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

/* This is how to output an assembler line defining a `double' constant.  */


/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE, VALUE)			\
  {							\
    long t[2];						\
    REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);		\
    fprintf (FILE, "\t.long 0x%lx\n\t.long 0x%lx\n",	\
	     t[0] & 0xffffffff, t[1] & 0xffffffff);	\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE, VALUE)			\
  {							\
    long t;						\
    REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);		\
    fprintf (FILE, "\t.long 0x%lx\n", t & 0xffffffff);	\
  }

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)  \
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),    \
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))


#define ASM_OUTPUT_DOUBLE_INT(FILE, VALUE)      \
do { fprintf (FILE, "%s\t", ASM_QUAD);          \
  output_addr_const (FILE,(VALUE));             \
  putc ('\n',FILE);                             \
 } while (0)


/* This is how to output an assembler line defining an `int' constant.  */

#undef ASM_OUTPUT_INT
#define ASM_OUTPUT_INT(FILE, VALUE)             \
do { fprintf (FILE, "%s\t", ASM_LONG);          \
  output_addr_const (FILE,(VALUE));             \
  putc ('\n',FILE);                             \
 } while (0)

/* Likewise for `char' and `short' constants. 
   is this supposed to do align too?? */

#define ASM_OUTPUT_SHORT(FILE, VALUE)           \
( fprintf (FILE, "%s ", ASM_SHORT),             \
  output_addr_const (FILE,(VALUE)),             \
  putc ('\n',FILE))

#define ASM_OUTPUT_CHAR(FILE, VALUE)            \
( fprintf (FILE, "%s ", ASM_BYTE_OP),           \
  output_addr_const (FILE, (VALUE)),            \
  putc ('\n', FILE))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE, VALUE)  \
  fprintf ((FILE), "%s 0x%x\n", ASM_BYTE_OP, (VALUE))

     /* internal macro to output long */
#define _ASM_OUTPUT_LONG(FILE, VALUE)                                   \
      fprintf (FILE, "\t.long\t0x%lX\n", VALUE);


/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  			\
  fprintf (FILE, "%s %s%d\n", TARGET_64BIT?ASM_QUAD:ASM_LONG, 	\
	   LPREFIX, VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) 		\
  fprintf (FILE, "%s %s%d-.LT%X_%X\n" ,TARGET_64BIT?ASM_QUAD:ASM_LONG, 	\
	   LPREFIX, VALUE, s390_function_count,s390_pool_count)

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#undef ASM_OPEN_PAREN
#undef ASM_CLOSE_PAREN
#define ASM_OPEN_PAREN ""
#define ASM_CLOSE_PAREN ""



/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE, LOG)      \
    if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#undef ASM_OUTPUT_SKIP 
#define ASM_OUTPUT_SKIP(FILE, SIZE)  \
  fprintf ((FILE), "\t.set .,.+%u\n", (SIZE))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE, LOG)	\
    if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE, SIZE)  \
  fprintf ((FILE), "\t.set .,.+%u\n", (SIZE))

/* The routine used to output sequences of byte values.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable.  Note that if we find subparts of the
   character sequence which end with NUL (and which are shorter than
   STRING_LIMIT) we output those using ASM_OUTPUT_LIMITED_STRING.  */

#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)                             \
do {                                                                    \
      register unsigned char *_ascii_bytes = (unsigned char *) (STR);   \
      register unsigned char *limit = _ascii_bytes + (LENGTH);          \
      register unsigned bytes_in_chunk = 0;                             \
      for (; _ascii_bytes < limit; _ascii_bytes++)                      \
        {                                                               \
          register unsigned char *p;                                    \
          if (bytes_in_chunk >= 64)                                     \
            {                                                           \
              fputc ('\n', (FILE));                                     \
              bytes_in_chunk = 0;                                       \
            }                                                           \
          for (p = _ascii_bytes; p < limit && *p != '\0'; p++)          \
            continue;                                                   \
          if (p < limit && (p - _ascii_bytes) <= STRING_LIMIT)          \
            {                                                           \
              if (bytes_in_chunk > 0)                                   \
                {                                                       \
                  fputc ('\n', (FILE));                                 \
                  bytes_in_chunk = 0;                                   \
                }                                                       \
              ASM_OUTPUT_LIMITED_STRING ((FILE), _ascii_bytes);         \
              _ascii_bytes = p;                                         \
            }                                                           \
          else                                                          \
            {                                                           \
              if (bytes_in_chunk == 0)                                  \
                fprintf ((FILE), "%s\t", ASM_BYTE_OP);                  \
              else                                                      \
                fputc (',', (FILE));                                    \
              fprintf ((FILE), "0x%02x", *_ascii_bytes);                \
              bytes_in_chunk += 5;                                      \
            }                                                           \
        }                                                               \
      if (bytes_in_chunk > 0)                                           \
        fprintf ((FILE), "\n");                                         \
} while (0)

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable (initialized) data.  */

#define DATA_SECTION_ASM_OP ".data"

/* Output before writable (uninitialized) data.  */

#define BSS_SECTION_ASM_OP ".bss"

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)  \
  (fputs (".globl ", FILE), assemble_name (FILE, NAME), fputs ("\n", FILE))

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/*
 * This macro generates the assembly code for function entry.
 */

#define FUNCTION_PROLOGUE(FILE, LSIZE) s390_function_prologue (FILE, LSIZE)

/* This macro generates the assembly code for function exit, on machines
   that need it.  If FUNCTION_EPILOGUE is not defined then individual
   return instructions are generated for each return statement.  Args are
   same as for FUNCTION_PROLOGUE.
  
   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, LSIZE) s390_function_epilogue(FILE, LSIZE)

/* Select section for constant in constant pool. 
   We are in the right section. 
   undef for 64 bit mode (linux64.h).
 */

#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, X)


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
          fprintf (FILE, "0:\t.long  %d\n",DELTA);                            \
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
          fprintf (FILE, "0:\t.long  %d\n",DELTA);                            \
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
          fprintf (FILE, "\t.long  %d\n",DELTA);                              \
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
          fprintf (FILE, "\t.long  %d\n",DELTA);                              \
          fprintf (FILE, "0:\tal  %d,4(1)\n",                                 \
                   aggregate_value_p (TREE_TYPE                               \
                                      (TREE_TYPE (FUNCTION))) ? 3 : 2 );      \
          fprintf (FILE, "\tal    1,0(1)\n");                                 \
          fprintf (FILE, "\tbr    1\n");                                      \
       }                                                                      \
    }                                                                         \
} while (0)

#endif
