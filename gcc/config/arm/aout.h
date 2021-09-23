/* Definitions of target machine for GNU compiler, for ARM with a.out
   Copyright (C) 1995-2021 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (rearnsha@armltd.co.uk).
   
   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef ASM_APP_ON
#define ASM_APP_ON  		""
#endif
#ifndef ASM_APP_OFF
#define ASM_APP_OFF  		""
#endif

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  	"\t.text"
#define DATA_SECTION_ASM_OP  	"\t.data"
#define BSS_SECTION_ASM_OP   	"\t.bss"

/* Note: If USER_LABEL_PREFIX or LOCAL_LABEL_PREFIX are changed,
   make sure that this change is reflected in the function
   coff_arm_is_local_label_name() in bfd/coff-arm.c.  */
#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX 	""
#endif

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX 	"_"
#endif

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX 	""
#endif

/* The assembler's names for the registers.  Note that the ?xx registers are
   there so that VFPv3/NEON registers D16-D31 have the same spacing as D0-D15
   (each of which is overlaid on two S registers), although there are no
   actual single-precision registers which correspond to D16-D31.  New register
   p0 is added which is used for MVE predicated cases.  */

#ifndef REGISTER_NAMES
#define REGISTER_NAMES						\
{								\
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",		\
  "r8", "r9", "r10", "fp", "ip", "sp", "lr", "pc",		\
  "s0",  "s1",  "s2",  "s3",  "s4",  "s5",  "s6",  "s7",	\
  "s8",  "s9",  "s10", "s11", "s12", "s13", "s14", "s15",	\
  "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23",	\
  "s24", "s25", "s26", "s27", "s28", "s29", "s30", "s31",	\
  "d16", "?16", "d17", "?17", "d18", "?18", "d19", "?19",	\
  "d20", "?20", "d21", "?21", "d22", "?22", "d23", "?23",	\
  "d24", "?24", "d25", "?25", "d26", "?26", "d27", "?27",	\
  "d28", "?28", "d29", "?29", "d30", "?30", "d31", "?31",	\
  "wr0",   "wr1",   "wr2",   "wr3",				\
  "wr4",   "wr5",   "wr6",   "wr7",				\
  "wr8",   "wr9",   "wr10",  "wr11",				\
  "wr12",  "wr13",  "wr14",  "wr15",				\
  "wcgr0", "wcgr1", "wcgr2", "wcgr3",				\
  "cc", "vfpcc", "sfp", "afp", "apsrq", "apsrge", "p0"		\
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
  {"rfp", 9}, /* Historical.  */		\
  {"sb", 9}, /* Historical.  */			\
  {"v7", 10},					\
  {"sl", 10},	/* Historical.  */		\
  {"r11", 11},	/* fp */			\
  {"r12", 12},	/* ip */			\
  {"r13", 13},	/* sp */			\
  {"r14", 14},	/* lr */			\
  {"r15", 15}	/* pc */			\
}
#endif

#ifndef OVERLAPPING_REGISTER_NAMES
#define OVERLAPPING_REGISTER_NAMES		\
{						\
  {"d0",  FIRST_VFP_REGNUM + 0,  2},		\
  {"d1",  FIRST_VFP_REGNUM + 2,  2},		\
  {"d2",  FIRST_VFP_REGNUM + 4,  2},		\
  {"d3",  FIRST_VFP_REGNUM + 6,  2},		\
  {"d4",  FIRST_VFP_REGNUM + 8,  2},		\
  {"d5",  FIRST_VFP_REGNUM + 10, 2},		\
  {"d6",  FIRST_VFP_REGNUM + 12, 2},		\
  {"d7",  FIRST_VFP_REGNUM + 14, 2},		\
  {"d8",  FIRST_VFP_REGNUM + 16, 2},		\
  {"d9",  FIRST_VFP_REGNUM + 18, 2},		\
  {"d10", FIRST_VFP_REGNUM + 20, 2},		\
  {"d11", FIRST_VFP_REGNUM + 22, 2},		\
  {"d12", FIRST_VFP_REGNUM + 24, 2},		\
  {"d13", FIRST_VFP_REGNUM + 26, 2},		\
  {"d14", FIRST_VFP_REGNUM + 28, 2},		\
  {"d15", FIRST_VFP_REGNUM + 30, 2},		\
  {"q0",  FIRST_VFP_REGNUM + 0,  4},		\
  {"q1",  FIRST_VFP_REGNUM + 4,  4},		\
  {"q2",  FIRST_VFP_REGNUM + 8,  4},		\
  {"q3",  FIRST_VFP_REGNUM + 12, 4},		\
  {"q4",  FIRST_VFP_REGNUM + 16, 4},		\
  {"q5",  FIRST_VFP_REGNUM + 20, 4},		\
  {"q6",  FIRST_VFP_REGNUM + 24, 4},		\
  {"q7",  FIRST_VFP_REGNUM + 28, 4},		\
  {"q8",  FIRST_VFP_REGNUM + 32, 4},		\
  {"q9",  FIRST_VFP_REGNUM + 36, 4},		\
  {"q10", FIRST_VFP_REGNUM + 40, 4},		\
  {"q11", FIRST_VFP_REGNUM + 44, 4},		\
  {"q12", FIRST_VFP_REGNUM + 48, 4},		\
  {"q13", FIRST_VFP_REGNUM + 52, 4},		\
  {"q14", FIRST_VFP_REGNUM + 56, 4},		\
  {"q15", FIRST_VFP_REGNUM + 60, 4}		\
}
#endif

#ifndef NO_DOLLAR_IN_LABEL
#define NO_DOLLAR_IN_LABEL 1
#endif

/* Generate DBX debugging information.  riscix.h will undefine this because
   the native assembler does not support stabs.  */
#define DBX_DEBUGGING_INFO 1

/* Acorn dbx moans about continuation chars, so don't use any.  */
#ifndef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH  0
#endif

/* Output a function label definition.  */
#ifndef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)	\
  do							\
    {							\
      ARM_DECLARE_FUNCTION_NAME (STREAM, NAME, DECL);   \
      ASM_OUTPUT_LABEL (STREAM, NAME);			\
    }							\
  while (0)
#endif

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* Make an internal label into a string.  */
#ifndef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*%s%s%u", LOCAL_LABEL_PREFIX, PREFIX, (unsigned int)(NUM))
#endif
     
/* Output an element of a dispatch table.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)			\
  do								\
    {								\
      gcc_assert (!TARGET_THUMB2);				\
      asm_fprintf (STREAM, "\t.word\t%LL%d\n", VALUE);		\
    }								\
  while (0)
	  

/* Thumb-2 always uses addr_diff_elf so that the Table Branch instructions
   can be used.  For non-pic code where the offsets do not suitable for
   TBB/TBH the elements are output as absolute labels.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  do									\
    {									\
      if (TARGET_ARM)							\
	asm_fprintf (STREAM, "\tb\t%LL%d\n", VALUE);			\
      else if (TARGET_THUMB1)						\
	{								\
	  if (flag_pic || optimize_size)				\
	    {								\
	      switch (GET_MODE(body))					\
		{							\
		case E_QImode:						\
		  asm_fprintf (STREAM, "\t.byte\t(%LL%d-%LL%d)/2\n",	\
			       VALUE, REL);				\
		  break;						\
		case E_HImode: /* TBH */					\
		  asm_fprintf (STREAM, "\t.2byte\t(%LL%d-%LL%d)/2\n",	\
			       VALUE, REL);				\
		  break;						\
		case E_SImode:						\
		  asm_fprintf (STREAM, "\t.word\t%LL%d-%LL%d\n",	\
			       VALUE, REL);				\
		  break;						\
		default:						\
		  gcc_unreachable();					\
		}							\
	    }								\
	  else								\
	    asm_fprintf (STREAM, "\t.word\t%LL%d+1\n", VALUE);		\
	}								\
      else /* Thumb-2 */						\
	{								\
	  switch (GET_MODE(body))					\
	    {								\
	    case E_QImode: /* TBB */					\
	      asm_fprintf (STREAM, "\t.byte\t(%LL%d-%LL%d)/2\n",	\
			   VALUE, REL);					\
	      break;							\
	    case E_HImode: /* TBH */					\
	      asm_fprintf (STREAM, "\t.2byte\t(%LL%d-%LL%d)/2\n",	\
			   VALUE, REL);					\
	      break;							\
	    case E_SImode:						\
	      if (flag_pic)						\
		asm_fprintf (STREAM, "\t.word\t%LL%d+1-%LL%d\n", VALUE, REL); \
	      else							\
		asm_fprintf (STREAM, "\t.word\t%LL%d+1\n", VALUE);	\
	      break;							\
	    default:							\
	      gcc_unreachable();					\
	    }								\
	}								\
    }									\
  while (0)


#undef  ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(STREAM, PTR, LEN)  \
  output_ascii_pseudo_op (STREAM, (const unsigned char *) (PTR), LEN)

/* Output a gap.  In fact we fill it with nulls.  */
#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM, NBYTES) 	\
  fprintf (STREAM, "\t.space\t%d\n", (int) (NBYTES))

/* Align output to a power of two.  Horrible /bin/as.  */
#ifndef ASM_OUTPUT_ALIGN  
#define ASM_OUTPUT_ALIGN(STREAM, POWER)			\
  do							\
    {							\
      int amount = 1 << (POWER);			\
							\
      if (amount == 2)					\
	fprintf (STREAM, "\t.even\n");			\
      else if (amount != 1)				\
	fprintf (STREAM, "\t.align\t%d\n", amount - 4);	\
    }							\
  while (0)
#endif

/* Output a common block.  */
#ifndef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)	\
  do							\
    {							\
      fprintf (STREAM, "\t.comm\t");			\
      assemble_name (STREAM, NAME);			\
      asm_fprintf (STREAM, ", %d\t%@ %d\n", 		\
	           (int)(ROUNDED), (int)(SIZE));	\
    }							\
  while (0)
#endif
     
/* Output a local common block.  /bin/as can't do this, so hack a
   `.space' into the bss segment.  Note that this is *bad* practice,
   which is guaranteed NOT to work since it doesn't define STATIC
   COMMON space but merely STATIC BSS space.  */
#ifndef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      switch_to_section (bss_section);					\
      ASM_OUTPUT_ALIGN (STREAM, floor_log2 (ALIGN / BITS_PER_UNIT));	\
      ASM_OUTPUT_LABEL (STREAM, NAME);					\
      fprintf (STREAM, "\t.space\t%d\n", (int)(SIZE));			\
    }									\
  while (0)
#endif
     
/* Output a zero-initialized block.  */
#ifndef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(STREAM, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (STREAM, DECL, NAME, SIZE, ALIGN)
#endif

#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START 	"@"
#endif

/* This works for GAS and some other assemblers.  */
#define SET_ASM_OP		"\t.set\t"
