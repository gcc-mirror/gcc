/* Target definitions for GNU compiler for Intel 80860 running OSF/1AD
   Copyright (C) 1991 Free Software Foundation, Inc.

   Based upon original work of Ron Guilmette (rfg@netcom.com).
   Whacked into submission by Andy Pfiffer (andyp@ssd.intel.com).
   Partially inspired by
	Pete Beckman @ Indiana University (beckman@cs.indiana.edu)
	Harry Dolan of Intel Corporation (dolan@ssd.intel.com)

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

#include "i860/i860.h"
#include "svr3.h"

/* For the sake of libgcc2.c, indicate target supports atexit.  */
#define HAVE_ATEXIT

#undef TARGET_SWITCHES
#define TARGET_SWITCHES  \
  { {"xp", 1},                  \
    {"noxp", -1},               \
    {"xr", -1},                 \
    {"noieee", -1},		\
    {"nx", 2},                  \
    { "", TARGET_DEFAULT}}
 
#undef TARGET_DEFAULT
#define TARGET_DEFAULT 1

/* The Intel as860 assembler does not understand .stabs, must use COFF */
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i860 OSF/1AD)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES	"-Di860 -D__i860 -D__i860__ -D__PARAGON__ -D__OSF1__ -D_COFF -Dunix -DMACH -DCMU"

#define CPP_SPEC "%{mnx:-D__NODE}"

/* autoinit.o autolaunches NX applications */
#define STARTFILE_SPEC "-ycrt0.o%s %{mnx:-yoptions/autoinit.o%s}"

/* libic.a is the PGI intrinsic library */
/* libpm.o and guard.o are for the performance monitoring modules (ignored) */
/* /usr/lib/noieee contains non-IEEE compliant (but faster) math routines */
#if	HAVE_DASH_G
#define LIB_SPEC "%{mnoieee:-L/usr/lib/noieee} -L/usr/lib %{mnx:-lnx -lmach} %
{g*:-lg} -lc -lic"
#else	/* HAVE_DASH_G */
/* can't use -g for -lg; libg.a doesn't have a symbol table and ld complains */
#define LIB_SPEC "%{mnoieee:-L/usr/lib/noieee} -L/usr/lib %{mnx:-lnx -lmach} -lc -lic"
#endif	/* HAVE_DASH_G */

/* Get rid of definition from svr3.h.  */
#undef SIZE_TYPE

#undef	I860_REG_PREFIX

#undef	ASM_COMMENT_START
#define ASM_COMMENT_START "//"

#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT      "\"%s\""

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

#undef ASCII_DATA_ASM_OP
#define ASCII_DATA_ASM_OP	".byte"

/*
 *	the assembler we're using doesn't grok .ident...
 */
#undef	ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "//\t.ident \"%s\"\n", NAME);

/*
 *	the assembler doesn't grok .double INF and the like
 *	but does understand .long with hex numbers, so special
 *	case the "symbolic" IEEE numbers.
 */
#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)				\
  {								\
    if (REAL_VALUE_ISINF (VALUE)				\
        || REAL_VALUE_ISNAN (VALUE)				\
	|| REAL_VALUE_MINUS_ZERO (VALUE))			\
      {								\
	long t[2];						\
	REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);		\
	fprintf (FILE, "\t.long 0x%lx\n\t.long 0x%lx\n", t[0], t[1]); \
      }								\
    else							\
      fprintf (FILE, "\t.double %.20e\n", VALUE);		\
  }

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)				\
  {								\
    if (REAL_VALUE_ISINF (VALUE)				\
        || REAL_VALUE_ISNAN (VALUE)				\
	|| REAL_VALUE_MINUS_ZERO (VALUE))			\
      {								\
	long t;							\
	REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);		\
	fprintf (FILE, "\t.long 0x%lx\n", t);			\
      }								\
    else							\
      fprintf (FILE, "\t.float %.12e\n", VALUE);		\
  }

#undef	ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)			\
  do								\
    {								\
      register unsigned char *str = (unsigned char *) (STR);	\
      register unsigned char *limit = str + (LENGTH);		\
      register unsigned bytes_in_chunk = 0;			\
      for (; str < limit; str++)				\
        {							\
          register unsigned ch = *str;				\
          if (ch < 32 || ch == '\\' || ch == '"' || ch >= 127)	\
	    {							\
	      if (bytes_in_chunk > 0)				\
	        {						\
	          fprintf ((FILE), "\"\n");			\
	          bytes_in_chunk = 0;				\
	        }						\
	      fprintf ((FILE), "\t%s\t%d\n", ASM_BYTE_OP, ch);	\
	    }							\
          else							\
	    {							\
	      if (bytes_in_chunk >= 60)				\
	        {						\
	          fprintf ((FILE), "\"\n");			\
	          bytes_in_chunk = 0;				\
	        }						\
	      if (bytes_in_chunk == 0)				\
	        fprintf ((FILE), "\t%s\t\"", ASCII_DATA_ASM_OP);\
	      putc (ch, (FILE));				\
	      bytes_in_chunk++;					\
	    }							\
        }							\
      if (bytes_in_chunk > 0)					\
        fprintf ((FILE), "\"\n");				\
    }								\
  while (0)


/* This says how to output an assembler line
   to define a local common symbol.  */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/*
 *	not defining ASM_STABS_OP yields .stabs in the .s file
 *	when using g++ -- so, I'll define it.
 */
#define	ASM_STABS_OP	"//.stabs"

/* Define this macro if an argument declared as `char' or `short' in a
   prototype should actually be passed as an `int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for
   better code on certain machines. */
/*#define PROMOTE_PROTOTYPES*/

/* Define this macro if an instruction to load a value narrower
   than a word from memory into a register also zero-extends the
   value to the whole  register.  */
/*#define BYTE_LOADS_ZERO_EXTEND*/

/* Define this macro as a C expression which is nonzero if
   accessing less than a word of memory (i.e. a `char' or a
   `short') is no faster than accessing a word of memory, i.e., if
   such access require more than one instruction or if there is no
   difference in cost between byte and (aligned) word loads.

   On RISC machines, it tends to generate better code to define
   this as 1, since it avoids making a QI or HI mode register.  */
/*
#undef SLOW_BYTE_ACCESS
#define SLOW_BYTE_ACCESS 1
*/

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count. */
#define SHIFT_COUNT_TRUNCATED 1


#define FASTEST_ALIGNMENT 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/*
 *	disable a few things picked up from svr3.h
 */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
#undef CONST_SECTION_ASM_OP
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#undef DO_GLOBAL_CTORS_BODY
#undef ASM_OUTPUT_DESTRUCTOR
#undef SELECT_SECTION
#undef SELECT_RTX_SECTION
#undef READONLY_DATA_SECTION

#define	BSS_SECTION_ASM_OP	".bss"		/* XXX */
#undef EXTRA_SECTIONS
#define	EXTRA_SECTIONS	in_bss
#undef EXTRA_SECTION_FUNCTIONS
#define	EXTRA_SECTION_FUNCTIONS		\
	BSS_SECTION_FUNCTION
