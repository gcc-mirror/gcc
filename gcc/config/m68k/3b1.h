/* Definitions of target machine for GNU compiler.
   AT&T UNIX PC version (pc7300, 3b1)
   Written by Alex Crain (alex@umbc3.umd.edu).

   Copyright (C) 1987, 1993 Free Software Foundation, Inc.

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

#define SGS_SWITCH_TABLES	/* Different switch table handling */

#include "m68k/hp320.h"

/* See m68k.h.  0 means 680[01]0 with no 68881.  */

#undef TARGET_DEFAULT
#define	TARGET_DEFAULT 0

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* -m68020 requires special flags to the assembler.  */

#undef ASM_SPEC
#define ASM_SPEC "%{m68020:-68020}%{!m68020:-68010} %{m68881:-68881}"

/* we use /lib/libp/lib*  when profiling */

#undef LIB_SPEC
#define LIB_SPEC "%{!shlib:%{p:-L/lib/libp} %{pg:-L/lib/libp} -lc}"

/* shared libraries need to use crt0s.o  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shlib:%{pg:mcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}\
   %{shlib:crt0s.o%s shlib.ifile%s} "

/* cpp has to support a #sccs directive for the /usr/include files */

#define SCCS_DIRECTIVE

/* Make output for SDB.  */

#define SDB_DEBUGGING_INFO

/* The .file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
output_file_directive ((FILE), main_input_filename)

/* Don't try to define `gcc_compiled.' since the assembler might not
   accept symbols with periods and GDB doesn't run on this machine anyway.  */
#define ASM_IDENTIFY_GCC(FILE)

/* Define __HAVE_68881__ in preprocessor if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#undef CPP_SPEC
#define CPP_SPEC "%{m68881:-D__HAVE_68881__}"

/* Names to predefine in the preprocessor for this target machine.  */
/* ihnp4!lmayk!lgm@eddie.mit.edu says mc68000 and m68k should not be here.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68k -Dunix -Dunixpc -D__motorola__ -Asystem(unix)  -Asystem(svr3) -Acpu(m68k) -Amachine(m68k)"

#undef REGISTER_NAMES
#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",	\
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%fp", "%sp",	\
 "%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7"}

/* Specify how to pad function arguments.
   Value should be `upward', `downward' or `none'.
   Same as the default, except no padding for large or variable-size args.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE)				\
  (((MODE) == BLKmode							\
    ? ((TYPE) && TREE_CODE (TYPE_SIZE (TYPE)) == INTEGER_CST		\
       && int_size_in_bytes (TYPE) < PARM_BOUNDARY / BITS_PER_UNIT)	\
    : GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY)				\
   ? downward : none)

/* Override part of the obstack macros.  */

#define __PTR_TO_INT(P) ((int)(P))
#define __INT_TO_PTR(P) ((char *)(P))

/* The 3b1 does not have `atexit'.  */

#undef HAVE_ATEXIT

/* Override parts of m68k.h to fit the SGS-3b1 assembler.  */

#undef TARGET_VERSION
#undef ASM_FORMAT_PRIVATE_NAME
#undef ASM_OUTPUT_DOUBLE
#undef ASM_OUTPUT_FLOAT
#undef ASM_OUTPUT_ALIGN
#undef ASM_OUTPUT_SOURCE_FILENAME
#undef ASM_OUTPUT_SOURCE_LINE
#undef PRINT_OPERAND_ADDRESS
#undef ASM_GENERATE_INTERNAL_LABEL
#undef FUNCTION_PROFILER
#undef ASM_OUTPUT_ADDR_VEC_ELT
#undef ASM_OUTPUT_ADDR_DIFF_ELT
#undef ASM_OUTPUT_INTERNAL_LABEL
#undef ASM_OUTPUT_OPCODE
#undef ASM_OUTPUT_LOCAL
#undef ASM_OUTPUT_LABELREF
#undef ASM_OUTPUT_ASCII

#define TARGET_VERSION fprintf (stderr, " (68k, SGS/AT&T unixpc syntax)");

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "%s_%%%d", (NAME), (LABELNO)))

/* The unixpc doesn't know about double's and float's */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
do { long l[2];						\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);		\
     fprintf (FILE, "\tlong 0x%x,0x%x\n", l[0], l[1]); \
   } while (0)

#undef ASM_OUTPUT_LONG_DOUBLE
#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)  				\
do { long l[3];								\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);			\
     fprintf (FILE, "\tlong 0x%x,0x%x,0x%x\n", l[0], l[1], l[2]);	\
   } while (0)

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
do { long l;					\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);	\
     fprintf ((FILE), "\tlong 0x%x\n", l);	\
   } while (0)

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) == 1)			\
    fprintf (FILE, "\teven\n");	\
  else if ((LOG) != 0)			\
    abort ();

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\tspace %d\n", (SIZE))

/* Can't use ASM_OUTPUT_SKIP in text section; it doesn't leave 0s.  */

#define ASM_NO_SKIP_IN_TEXT 1

/* The beginnings of sdb support... */

#define ASM_OUTPUT_SOURCE_FILENAME(FILE, FILENAME) \
  do {	fprintf (FILE, "\tfile\t");		\
	output_quoted_string (FILE, FILENAME);	\
	fprintf (FILE, "\n");			\
  } while (0)

#define ASM_OUTPUT_SOURCE_LINE(FILE, LINENO)	\
  fprintf (FILE, "\tln\t%d\n",			\
	   (sdb_begin_function_line		\
	    ? last_linenum - sdb_begin_function_line : 1))

/* Yet another null terminated string format. */

#define ASM_OUTPUT_ASCII(FILE,PTR,LEN) \
  do { register int sp = 0, lp = 0; \
    fprintf ((FILE), "\tbyte\t"); \
  loop: \
    if ((PTR)[sp] > ' ' && ! ((PTR)[sp] & 0x80) && (PTR)[sp] != '\\') \
      { lp += 3; \
	fprintf ((FILE), "'%c", (PTR)[sp]); } \
    else \
      { lp += 5; \
	fprintf ((FILE), "0x%x", (PTR)[sp]); } \
    if (++sp < (LEN)) \
      {	if (lp > 60) \
	  { lp = 0; \
	    fprintf ((FILE), "\n\t%s ", ASCII_DATA_ASM_OP); }	\
	else \
	  putc (',', (FILE)); \
	goto loop; } \
    putc ('\n', (FILE)); } while (0)

/* Note that in the case of the movhi which fetches an element of
   an ADDR_DIFF_VEC the offset output is too large by 2.
   This is because the 3b1 assembler refuses to subtract 2.
   ASM_OUTPUT_CASE_LABEL, below, compensates for this.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx reg1, reg2, breg, ireg;					\
  register rtx addr = ADDR;						\
  rtx offset;								\
  switch (GET_CODE (addr))						\
    {									\
    case REG:								\
      fprintf (FILE, "(%s)", reg_names[REGNO (addr)]);			\
      break;								\
    case PRE_DEC:							\
      fprintf (FILE, "-(%s)", reg_names[REGNO (XEXP (addr, 0))]);	\
      break;								\
    case POST_INC:							\
      fprintf (FILE, "(%s)+", reg_names[REGNO (XEXP (addr, 0))]);	\
      break;								\
    case PLUS:								\
      reg1 = 0;	reg2 = 0;						\
      ireg = 0;	breg = 0;						\
      offset = 0;							\
      if (CONSTANT_ADDRESS_P (XEXP (addr, 0)))				\
	{								\
	  offset = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))			\
	{								\
	  offset = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) != PLUS) ;					\
      else if (GET_CODE (XEXP (addr, 0)) == SIGN_EXTEND)		\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == SIGN_EXTEND)		\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      else if (GET_CODE (XEXP (addr, 0)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      else if (GET_CODE (XEXP (addr, 0)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT		\
	  || GET_CODE (addr) == SIGN_EXTEND)				\
	{ if (reg1 == 0) reg1 = addr; else reg2 = addr; addr = 0; }	\
/*  for OLD_INDEXING							\
      else if (GET_CODE (addr) == PLUS)					\
	{								\
	  if (GET_CODE (XEXP (addr, 0)) == REG)				\
	    {								\
	      reg2 = XEXP (addr, 0);					\
	      addr = XEXP (addr, 1);					\
	    }								\
	  else if (GET_CODE (XEXP (addr, 1)) == REG)			\
	    {								\
	      reg2 = XEXP (addr, 1);					\
	      addr = XEXP (addr, 0);					\
	    }								\
	}								\
  */									\
      if (offset != 0) { if (addr != 0) abort (); addr = offset; }	\
      if ((reg1 && (GET_CODE (reg1) == SIGN_EXTEND			\
		    || GET_CODE (reg1) == MULT))			\
	  || (reg2 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg2))))		\
	{ breg = reg2; ireg = reg1; }					\
      else if (reg1 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg1)))		\
	{ breg = reg1; ireg = reg2; }					\
      if (ireg != 0 && breg == 0 && GET_CODE (addr) == LABEL_REF)	\
        { int scale = 1;						\
	  if (GET_CODE (ireg) == MULT)					\
	    { scale = INTVAL (XEXP (ireg, 1));				\
	      ireg = XEXP (ireg, 0); }					\
	  if (GET_CODE (ireg) == SIGN_EXTEND)				\
	    fprintf (FILE, "LD%%%d(%%pc,%s.w",				\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (XEXP (ireg, 0))]); 		\
	  else								\
	    fprintf (FILE, "LD%%%d(%%pc,%s.l",				\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (ireg)]);				\
	  if (scale != 1) fprintf (FILE, "*%d", scale);			\
	  fprintf (FILE, ")");						\
	  break; }							\
      if (breg != 0 && ireg == 0 && GET_CODE (addr) == LABEL_REF)	\
        { fprintf (FILE, "LD%%%d(%%pc,%s.l",				\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   reg_names[REGNO (breg)]);				\
	  putc (')', FILE);						\
	  break; }							\
      if (ireg != 0 || breg != 0)					\
	{ int scale = 1;						\
	  if (breg == 0)						\
	    abort ();							\
	  if (addr != 0)						\
	    output_addr_const (FILE, addr);				\
	  fprintf (FILE, "(%s", reg_names[REGNO (breg)]);		\
	  if (ireg != 0)					        \
	    putc (',', FILE);						\
	  if (ireg != 0 && GET_CODE (ireg) == MULT)			\
	    { scale = INTVAL (XEXP (ireg, 1));				\
	      ireg = XEXP (ireg, 0); }					\
	  if (ireg != 0 && GET_CODE (ireg) == SIGN_EXTEND)		\
	    fprintf (FILE, "%s.w", reg_names[REGNO (XEXP (ireg, 0))]);	\
	  else if (ireg != 0)						\
	    fprintf (FILE, "%s.l", reg_names[REGNO (ireg)]);		\
	  if (scale != 1) fprintf (FILE, "*%d", scale);			\
	  putc (')', FILE);						\
	  break;							\
	}								\
      else if (reg1 != 0 && GET_CODE (addr) == LABEL_REF)		\
	{ fprintf (FILE, "LD%%%d(%%pc,%s.w)",				\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   reg_names[REGNO (reg1)]);				\
	  break; }							\
    default:								\
      if (GET_CODE (addr) == CONST_INT					\
	  && INTVAL (addr) < 0x8000					\
	  && INTVAL (addr) >= -0x8000)					\
	fprintf (FILE, "%d", INTVAL (addr));				\
      else								\
        output_addr_const (FILE, addr);					\
    }}

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf ((LABEL), "%s%%%d", (PREFIX), (NUM))

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
    fprintf (FILE, "%s%%%d:\n", PREFIX, NUM)

/* Must put address in  %a0 , not  %d0 . -- LGM, 7/15/88 */
#define FUNCTION_PROFILER(FILE, LABEL_NO)	\
    fprintf (FILE, "\tmov.l &LP%%%d,%%a0\n\tjsr mcount\n", (LABEL_NO))

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)	\
    fprintf (FILE, "\tlong L%%%d\n", (VALUE))

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)	\
    fprintf (FILE, "\tshort L%%%d-L%%%d\n", (VALUE), (REL))

/* ihnp4!lmayk!lgm says that `short 0' triggers assembler bug;
   `short L%nn-L%nn' supposedly works.  */
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLE)			\
  if (! RTX_INTEGRATED_P (TABLE))					\
    fprintf (FILE, "\tswbeg &%d\n%s%%%d:\n",				\
	     XVECLEN (PATTERN (TABLE), 1), (PREFIX), (NUM));		\
  else									\
    fprintf (FILE, "\tswbeg &%d\n%s%%%d:\n\tshort %s%%%d-%s%%%d\n",	\
	     XVECLEN (PATTERN (TABLE), 1) + 1, (PREFIX), (NUM),		\
	     (PREFIX), (NUM), (PREFIX), (NUM))

/* At end of a switch table, define LDnnn iff the symbol LInnn was defined.
   Some SGS assemblers have a bug such that "Lnnn-LInnn-2.b(pc,d0.l*2)"
   fails to assemble.  Luckily "LDnnn(pc,d0.l*2)" produces the results
   we want.  This difference can be accommodated by making the assembler
   define such "LDnnn" to be either "Lnnn-LInnn-2.b", "Lnnn", or any other
   string, as necessary.  This is accomplished via the ASM_OUTPUT_CASE_END
   macro. */

#define ASM_OUTPUT_CASE_END(FILE,NUM,TABLE)				\
{ if (switch_table_difference_label_flag)				\
    fprintf (FILE, "\tset LD%%%d,L%%%d-LI%%%d\n", (NUM), (NUM), (NUM));	\
  switch_table_difference_label_flag = 0; }

int switch_table_difference_label_flag;

#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{ if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR);						\
      while (*(PTR) != ' ')				\
	{ putc (*(PTR), (FILE)); ++(PTR); }		\
      fprintf ((FILE), ".w"); }				\
  else if ((PTR)[0] == 's')				\
    {							\
      if (!strncmp ((PTR), "swap", 4))			\
	{ fprintf ((FILE), "swap.w"); (PTR) += 4; }	\
    }							\
  else if ((PTR)[0] == 'f')				\
    {							\
      if (!strncmp ((PTR), "fmove", 5))			\
	{ fprintf ((FILE), "fmov"); (PTR) += 5; }	\
      else if (!strncmp ((PTR), "fbne", 4))		\
	{ fprintf ((FILE), "fbneq"); (PTR) += 4; }	\
    }							\
/* MOVE, MOVEA, MOVEQ, MOVEC ==> MOV	*/		\
  else if ((PTR)[0] == 'm' && (PTR)[1] == 'o'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 'e')	\
    { fprintf ((FILE), "mov"); (PTR) += 4;		\
       if ((PTR)[0] == 'q' || (PTR)[0] == 'a'		\
	   || (PTR)[0] == 'c') (PTR)++; }		\
/* SUB, SUBQ, SUBA, SUBI ==> SUB */			\
  else if ((PTR)[0] == 's' && (PTR)[1] == 'u' 		\
	   && (PTR)[2] == 'b')				\
    { fprintf ((FILE), "sub"); (PTR) += 3;		\
       if ((PTR)[0] == 'q' || (PTR)[0] == 'i'	 	\
	   || (PTR)[0] == 'a') (PTR)++; }		\
/* CMP, CMPA, CMPI, CMPM ==> CMP	*/		\
  else if ((PTR)[0] == 'c' && (PTR)[1] == 'm'		\
	   && (PTR)[2] == 'p')				\
    { fprintf ((FILE), "cmp"); (PTR) += 3;		\
       if ((PTR)[0] == 'a' || (PTR)[0] == 'i'	 	\
	   || (PTR)[0] == 'm') (PTR)++; }		\
}

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tlcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "%s", NAME)

/* Override usual definitions of SDB output macros.
   These definitions differ only in the absence of the period
   at the beginning of the name of the directive
   and in the use of `~' as the symbol for the current location.  */

#define PUT_SDB_SCL(a) fprintf(asm_out_file, "\tscl\t%d;", (a))
#define PUT_SDB_INT_VAL(a) fprintf (asm_out_file, "\tval\t%d;", (a))
#define PUT_SDB_VAL(a)				\
( fputs ("\tval\t", asm_out_file),		\
  output_addr_const (asm_out_file, (a)),	\
  fputc (';', asm_out_file))

#define PUT_SDB_DEF(a)				\
do { fprintf (asm_out_file, "\tdef\t");	\
     ASM_OUTPUT_LABELREF (asm_out_file, a); 	\
     fprintf (asm_out_file, ";"); } while (0)

#define PUT_SDB_PLAIN_DEF(a) fprintf(asm_out_file,"\tdef\t~%s;",a)
#define PUT_SDB_ENDEF fputs("\tendef\n", asm_out_file)
#define PUT_SDB_TYPE(a) fprintf(asm_out_file, "\ttype\t0%o;", a)
#define PUT_SDB_SIZE(a) fprintf(asm_out_file, "\tsize\t%d;", a)
#define PUT_SDB_START_DIM fprintf(asm_out_file, "\tdim\t")

#define PUT_SDB_TAG(a)				\
do { fprintf (asm_out_file, "\ttag\t");	\
     ASM_OUTPUT_LABELREF (asm_out_file, a);	\
     fprintf (asm_out_file, ";"); } while (0)

#define PUT_SDB_BLOCK_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\tdef\t~bb;\tval\t~;\tscl\t100;\tline\t%d;\tendef\n",	\
	   (LINE))

#define PUT_SDB_BLOCK_END(LINE)			\
  fprintf (asm_out_file,			\
	   "\tdef\t~eb;\tval\t~;\tscl\t100;\tline\t%d;\tendef\n",	\
	   (LINE))

#define PUT_SDB_FUNCTION_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\tdef\t~bf;\tval\t~;\tscl\t101;\tline\t%d;\tendef\n",	\
	   (LINE))

#define PUT_SDB_FUNCTION_END(LINE)		\
  fprintf (asm_out_file,			\
	   "\tdef\t~ef;\tval\t~;\tscl\t101;\tline\t%d;\tendef\n",	\
	   (LINE))

#define PUT_SDB_EPILOGUE_END(NAME)		\
  fprintf (asm_out_file,			\
	   "\tdef\t%s;\tval\t~;\tscl\t-1;\tendef\n",	\
	   (NAME))

#define SDB_GENERATE_FAKE(BUFFER, NUMBER) \
  sprintf ((BUFFER), "~%dfake", (NUMBER));

/* Define subroutines to call to handle multiply, divide, and remainder.
   Use the subroutines that the 3b1's library provides.
   The `*' prevents an underscore from being prepended by the compiler.  */

#define DIVSI3_LIBCALL "*ldiv"
#define UDIVSI3_LIBCALL "*uldiv"
#define MODSI3_LIBCALL "*lrem"
#define UMODSI3_LIBCALL "*ulrem"
#define MULSI3_LIBCALL "*lmul"
#define UMULSI3_LIBCALL "*ulmul"

/* Definitions for collect2.  */

#define OBJECT_FORMAT_COFF
#define NO_SYS_SIGLIST
#define MY_ISCOFF(magic) \
 ((magic) == MC68KWRMAGIC || (magic) == MC68KROMAGIC || (magic) == MC68KPGMAGIC)
