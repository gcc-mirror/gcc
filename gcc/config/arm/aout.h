/* Definitions of target machine for GNU compiler, for ARM with a.out
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (rearnsha@armltd.co.uk).
   
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

#ifndef ARM_OS_NAME
#define ARM_OS_NAME "(generic)"
#endif

/* The text to go at the start of the assembler file */
#ifndef ASM_FILE_START
#define ASM_FILE_START(STREAM)						    \
{									    \
  fprintf (STREAM,"%srfp\t.req\t%sr9\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%ssl\t.req\t%sr10\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%sfp\t.req\t%sr11\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%sip\t.req\t%sr12\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%ssp\t.req\t%sr13\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%slr\t.req\t%sr14\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%spc\t.req\t%sr15\n", REGISTER_PREFIX, REGISTER_PREFIX); \
}
#endif

#define ASM_APP_ON  		""
#define ASM_APP_OFF  		""

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  	".text"
#define DATA_SECTION_ASM_OP  	".data"
#define BSS_SECTION_ASM_OP   	".bss"

/* Note: If USER_LABEL_PREFIX or LOCAL_LABEL_PREFIX are changed,
   make sure that this change is reflected in the function
   coff_arm_is_local_label_name() in bfd/coff-arm.c  */
#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX 	""
#endif

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX 	"_"
#endif

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX 	""
#endif


/* The assembler's names for the registers.  */
#ifndef REGISTER_NAMES
#define REGISTER_NAMES  			   \
{				                   \
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",  \
  "r8", "r9", "sl", "fp", "ip", "sp", "lr", "pc",  \
  "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",  \
  "cc", "sfp", "afp"				   \
}
#endif

#ifndef ADDITIONAL_REGISTER_NAMES
#define ADDITIONAL_REGISTER_NAMES		\
{						\
  {"a1", 0},					\
  {"a2", 1},					\
  {"a3", 2},					\
  {"a4", 3},					\
  {"v1", 4},					\
  {"v2", 5},					\
  {"v3", 6},					\
  {"v4", 7},					\
  {"v5", 8},					\
  {"v6", 9},					\
  {"rfp", 9}, /* Gcc used to call it this */	\
  {"sb", 9},					\
  {"v7", 10},					\
  {"r10", 10},	/* sl */			\
  {"r11", 11},	/* fp */			\
  {"r12", 12},	/* ip */			\
  {"r13", 13},	/* sp */			\
  {"r14", 14},	/* lr */			\
  {"r15", 15}	/* pc */			\
}
#endif

/* Arm Assembler barfs on dollars */
#define DOLLARS_IN_IDENTIFIERS 0

#define NO_DOLLAR_IN_LABEL

/* DBX register number for a given compiler register number */
#define DBX_REGISTER_NUMBER(REGNO)  (REGNO)

/* Generate DBX debugging information.  riscix.h will undefine this because
   the native assembler does not support stabs. */
#ifndef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO  1
#endif

/* Acorn dbx moans about continuation chars, so don't use any.  */
#ifndef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH  0
#endif

/* Output a source filename for the debugger. RISCiX dbx insists that the
   ``desc'' field is set to compiler version number >= 315 (sic).  */
#define DBX_OUTPUT_MAIN_SOURCE_FILENAME(STREAM,NAME) 			\
do {									\
  fprintf (STREAM, ".stabs ");						\
  output_quoted_string (STREAM, NAME);					\
  fprintf (STREAM, ",%d,0,315,%s\n", N_SO, &ltext_label_name[1]);	\
  text_section ();							\
  ASM_OUTPUT_INTERNAL_LABEL (STREAM, "Ltext", 0);			\
} while (0)
  
/* Output a function label definition.  */
#ifndef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL)	\
{							\
  if (TARGET_POKE_FUNCTION_NAME)			\
    arm_poke_function_name ((STREAM), (NAME));		\
  ASM_OUTPUT_LABEL (STREAM, NAME);			\
}
#endif

#ifndef ASM_OUTPUT_LABEL
#define ASM_OUTPUT_LABEL(STREAM,NAME)	\
do {					\
  assemble_name (STREAM,NAME);		\
  fputs (":\n", STREAM);		\
} while (0)
#endif
     
/* Output a globalising directive for a label.  */
#ifndef ASM_GLOBALIZE_LABEL
#define ASM_GLOBALIZE_LABEL(STREAM,NAME)  \
  (fprintf (STREAM, "\t.global\t"),	  \
   assemble_name (STREAM, NAME),	  \
   fputc ('\n',STREAM))                   
#endif

/* Make an internal label into a string.  */
#ifndef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*%s%s%u", LOCAL_LABEL_PREFIX, PREFIX, (unsigned int)(NUM))
#endif
     
/* Nothing special is done about jump tables */
/* #define ASM_OUTPUT_CASE_LABEL(STREAM,PREFIX,NUM,TABLE)   */
/* #define ASM_OUTPUT_CASE_END(STREAM,NUM,TABLE)	    */

/* Construct a private name.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR,NAME,NUMBER)  \
  ((OUTVAR) = (char *) alloca (strlen (NAME) + 10),  \
   sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER)))

/* Output an element of a dispatch table.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE)  \
   fprintf (STREAM, "\t.word\t%sL%d\n", LOCAL_LABEL_PREFIX, VALUE)

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)  \
   fprintf (STREAM, "\tb\t%sL%d\n", LOCAL_LABEL_PREFIX, (VALUE))

/* Output various types of constants.  For real numbers we output hex, with
   a comment containing the "human" value, this allows us to pass NaN's which
   the riscix assembler doesn't understand (it also makes cross-assembling
   less likely to fail). */

#define ASM_OUTPUT_LONG_DOUBLE(STREAM,VALUE)				\
do { char dstr[30];							\
     long l[3];								\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);			\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
     fprintf (STREAM, "\t.long 0x%lx,0x%lx,0x%lx\t%s long double %s\n", \
	      l[0], l[1], l[2], ASM_COMMENT_START, dstr);		\
   } while (0)

    
#define ASM_OUTPUT_DOUBLE(STREAM, VALUE)  				\
do { char dstr[30];							\
     long l[2];								\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);				\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.14g", dstr);			\
     fprintf (STREAM, "\t.long 0x%lx, 0x%lx\t%s double %s\n", l[0],	\
	      l[1], ASM_COMMENT_START, dstr);				\
   } while (0)

#define ASM_OUTPUT_FLOAT(STREAM, VALUE)					\
do { char dstr[30];							\
     long l;								\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);				\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.7g", dstr);			\
     fprintf (STREAM, "\t.word 0x%lx\t%s float %s\n", l,		\
	      ASM_COMMENT_START, dstr);					\
   } while (0);

#define ASM_OUTPUT_INT(STREAM, EXP)		\
  {						\
    fprintf (STREAM, "\t.word\t");		\
    OUTPUT_INT_ADDR_CONST (STREAM, (EXP));	\
    fputc ('\n', STREAM);			\
  }

#define ASM_OUTPUT_SHORT(STREAM, EXP)  \
  (fprintf (STREAM, "\t.short\t"),     \
   output_addr_const (STREAM, (EXP)),  \
   fputc ('\n', STREAM))

#define ASM_OUTPUT_CHAR(STREAM, EXP)  \
  (fprintf (STREAM, "\t.byte\t"),      \
   output_addr_const (STREAM, (EXP)),  \
   fputc ('\n', STREAM))

#define ASM_OUTPUT_BYTE(STREAM, VALUE)  	\
  fprintf (STREAM, "\t.byte\t%d\n", VALUE)

#define ASM_OUTPUT_ASCII(STREAM, PTR, LEN)  \
  output_ascii_pseudo_op ((STREAM), (unsigned char *)(PTR), (LEN))

/* Output a gap.  In fact we fill it with nulls.  */
#define ASM_OUTPUT_SKIP(STREAM, NBYTES) 	\
   fprintf (STREAM, "\t.space\t%d\n", NBYTES)

/* Align output to a power of two.  Horrible /bin/as.  */
#ifndef ASM_OUTPUT_ALIGN  
#define ASM_OUTPUT_ALIGN(STREAM, POWER)  \
  do                                                           \
    {                                                          \
      register int amount = 1 << (POWER);                      \
                                                               \
      if (amount == 2)                                         \
	fprintf (STREAM, "\t.even\n");                         \
      else if (amount != 1)                                    \
	fprintf (STREAM, "\t.align\t%d\n", amount - 4);        \
    }							       \
  while (0)
#endif

/* Output a common block */
#ifndef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)  		\
  (fprintf (STREAM, "\t.comm\t"), 		     			\
   assemble_name ((STREAM), (NAME)),		     			\
   fprintf (STREAM, ", %d\t%s %d\n", ROUNDED, ASM_COMMENT_START, SIZE))
#endif
     
/* Output a local common block.  /bin/as can't do this, so hack a
   `.space' into the bss segment.  Note that this is *bad* practice,
   which is guaranteed NOT to work since it doesn't define STATIC
   COMMON space but merely STATIC BSS space.  */
#ifndef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM,NAME,SIZE,ALIGN)		\
  do {									\
    bss_section ();							\
    ASM_OUTPUT_ALIGN (STREAM, floor_log2 (ALIGN / BITS_PER_UNIT));	\
    ASM_OUTPUT_LABEL (STREAM, NAME);					\
    fprintf (STREAM, "\t.space\t%d\n", SIZE);				\
  } while (0)
#endif
     
/* Output a zero-initialized block.  */
#ifndef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(STREAM,DECL,NAME,SIZE,ALIGN) \
  asm_output_aligned_bss (STREAM, DECL, NAME, SIZE, ALIGN)
#endif
     
/* Output a source line for the debugger.  */
/* #define ASM_OUTPUT_SOURCE_LINE(STREAM,LINE) */

/* Output a #ident directive.  */
#ifndef ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(STREAM,STRING)  \
  fprintf (STREAM, "%s - - - ident %s\n", ASM_COMMENT_START, STRING)
#endif
     
/* The assembler's parentheses characters.  */
#define ASM_OPEN_PAREN 		"("
#define ASM_CLOSE_PAREN 	")"

#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START 	"@"
#endif

/* This works for GAS and some other assemblers.  */
#define SET_ASM_OP		".set"

#include "arm/arm.h"
