/* Definitions of target machine for GNU compiler,
   SysV68 Motorola 3300 Delta Series.
   Copyright (C) 1987, 1993, 1994 Free Software Foundation, Inc.
   Coptributed by Abramo and Roberto Bagnara (bagnara@dipisa.di.unipi.it)
   based on Alex Crain's 3B1 definitions.

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

#define MOTOROLA		/* Use Motorola syntax rather than "MIT" */
#define MOTOROLA_BSR		/* Use Span-dependent optimized bsr */
#define SGS			/* Uses SGS assembler */
#define SGS_CMP_ORDER		/* Takes cmp operands in reverse order */
#define SGS_SWAP_W		/* Use swap.w rather than just plain swap */

#define NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

#include "m68k/m68k.h"

/* See m68k.h.  0407 means 68020-68040.  */

#ifndef TARGET_DEFAULT
#define	TARGET_DEFAULT 0407
#endif

/* -m[c]6800 requires special flag to the assembler.  */

#undef ASM_SPEC
#define ASM_SPEC "%{m68000:-p 000}%{mc68000:-p 000}"

/* NYI: FP= is equivalent to -msoft-float  */

/* We use /lib/libp/lib* when profiling.  */

/* NYI: if FP=M68881U library is -lc881u  */
/* NYI: if FP= library is -lc.  */
/* Default for us: FP=M68881 library is -lc881  */
#undef LIB_SPEC
#define LIB_SPEC "%{!shlib:%{p:-L/usr/lib/libp} %{pg:-L/usr/lib/libp} -lc881}"

#define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__}"

/* Shared libraries need to use crt0s.o  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shlib:%{pg:mcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}\
   %{shlib:crt0s.o%s shlib.ifile%s} "

/* Generate calls to memcpy, memcmp and memset.  */

#define TARGET_MEM_FUNCTIONS

/* size_t is unsigned int.  */

#define SIZE_TYPE "unsigned int"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16
  
/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
/* Be compatible with native compiler.  */
#undef PARM_BOUNDARY
#define PARM_BOUNDARY 16

/* cpp has to support a #sccs directive for the /usr/include files */

#define SCCS_DIRECTIVE

/* Make output for SDB.  */

#define SDB_DEBUGGING_INFO

#undef REGISTER_PREFIX
#define REGISTER_PREFIX "%"

#if 0
#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "~"
#endif

#undef IMMEDIATE_PREFIX
#define IMMEDIATE_PREFIX "&"

#undef REGISTER_NAMES
#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",	\
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%fp", "%sp",	\
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7"}

#undef FUNCTION_EXTRA_EPILOGUE
#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)				\
  { extern int current_function_returns_pointer;			\
    if ((current_function_returns_pointer) &&				\
      ! find_equiv_reg (0, get_last_insn (), 0, 0, 0, 8, Pmode))        \
      asm_fprintf (FILE, "\tmov.l %Ra0,%Rd0\n"); } 

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABEL_NO)	\
    fprintf (FILE, "\tmov.l &LP%%%d,%%a0\n\tjsr mcount%%\n", (LABEL_NO))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#undef ASM_OUTPUT_REG_PUSH
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmov.l %s,-(%%sp)\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#undef ASM_OUTPUT_REG_POP
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmov.l (%%sp)+,%s\n", reg_names[REGNO])

#undef ASM_APP_ON
#define ASM_APP_ON ""

#undef ASM_APP_OFF
#define ASM_APP_OFF ""

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "text"
#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "data"
#undef ASCII_DATA_ASM_OP
#define	ASCII_DATA_ASM_OP "byte"

/* The file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
output_file_directive ((FILE), main_input_filename)

/* The sysV68 assembler does not accept dots in labels.
   Let's use percent instead  */

#define ASM_IDENTIFY_GCC(FILE)        fputs("gcc2_compiled%:\n", FILE)

/* Names to predefine in the preprocessor for this target machine.  */
/* ihnp4!lmayk!lgm@eddie.mit.edu says mc68000 and m68k should not be here,
   on the other hand I don't care what he says.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dm68k -Dunix -DsysV68 -D__motorola__ -Asystem(unix) -Asystem(svr3) -Acpu(m68k) -Amachine(m68k)"

/* Override part of the obstack macros.  */

#define __PTR_TO_INT(P) ((int)(P))
#define __INT_TO_PTR(P) ((char *)(P))

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (68k, SGS/AT&T sysV68 syntax)");

/* Function calls save all but a0, a1, d0, d1, fp0, fp1.  */

#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS						\
 {1, 1, 0, 0, 0, 0, 0, 0,						\
  1, 1, 0, 0, 0, 0, 0, 1,						\
  1, 1, 0, 0, 0, 0, 0, 0}

/* If TARGET_68881, return SF and DF values in fp0 instead of d0.  */
/* NYI: If FP=M68881U return SF and DF values in d0. */
/* NYI: If -mold return pointer in a0 and d0 */

#undef FUNCTION_VALUE
/* sysV68 (brain damaged) cc convention support. */
#define FUNCTION_VALUE(VALTYPE,FUNC) \
  (TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_68881 	\
   ? gen_rtx (REG, TYPE_MODE (VALTYPE), 16)		\
   : (TREE_CODE (VALTYPE) == POINTER_TYPE 		\
      ? gen_rtx (REG, TYPE_MODE (VALTYPE), 8)		\
      : gen_rtx (REG, TYPE_MODE (VALTYPE), 0)))

/* If TARGET_68881, SF and DF values are returned in fp0 instead of d0.  */

/* Is LIBCALL_VALUE never called with a pointer ? */
#undef LIBCALL_VALUE
#define LIBCALL_VALUE(MODE)						   \
 gen_rtx (REG, (MODE),							   \
	  ((TARGET_68881						   \
	    && ((MODE) == SFmode || (MODE) == DFmode || (MODE) == XFmode)) \
	   ? 16 : 0))

/* 1 if N is a possible register number for a function value.
   d0 may be used, and fp0 as well if -msoft-float is not specified.  */

#undef FUNCTION_VALUE_REGNO_P
/* sysV68 (brain damaged) cc convention support. */
#define FUNCTION_VALUE_REGNO_P(N) \
 ((N) == 0 || (N) == 8 || (TARGET_68881 && (N) == 16))

/* Define this to be true when FUNCTION_VALUE_REGNO_P is true for
   more than one register.  */

#undef NEEDS_UNTYPED_CALL
#define NEEDS_UNTYPED_CALL 1
 
/* This is the command to make the user-level label named NAME
   defined for reference from other files.  */

#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP "global"

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "%s_%%%d", (NAME), (LABELNO)))

/* The sysV68 as doesn't know about double's and float's.  */
/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
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

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
do { long l;					\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);	\
     fprintf ((FILE), "\tlong 0x%x\n", l);	\
   } while (0)

/* This is how to output an assembler line defining an `int' constant.  */

#undef ASM_OUTPUT_INT
#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\tlong "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#undef ASM_OUTPUT_SHORT
#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\tshort "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#undef ASM_OUTPUT_CHAR
#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\tbyte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#undef ASM_OUTPUT_BYTE
#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\tbyte 0x%x\n", (VALUE))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) == 1)			\
    fprintf (FILE, "\teven\n");		\
  else if ((LOG) != 0)			\
    abort ();

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\tspace %u\n", (SIZE))

/* Can't use ASM_OUTPUT_SKIP in text section.  */

#define ASM_NO_SKIP_IN_TEXT 1

/* The beginnings of sdb support...  */

#undef ASM_OUTPUT_SOURCE_FILENAME
#define ASM_OUTPUT_SOURCE_FILENAME(FILE, FILENAME) \
  do {	fprintf (FILE, "\tfile\t");		\
	output_quoted_string (FILE, FILENAME);	\
	fprintf (FILE, "\n");			\
  } while (0)

#undef ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINENO)	\
  fprintf (FILE, "\tln\t%d\n",			\
	   (sdb_begin_function_line		\
	    ? last_linenum - sdb_begin_function_line : 1))

/* Yet another null terminated string format.  */

#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE,PTR,LEN) \
  do { register int sp = 0, lp = 0;				\
    fprintf ((FILE), "\tbyte\t");				\
  loop:								\
    if ((PTR)[sp] > ' ' && ! ((PTR)[sp] & 0x80) && (PTR)[sp] != '\\')	\
      { lp += 3;						\
	fprintf ((FILE), "'%c", (PTR)[sp]); }			\
    else							\
      { lp += 5;						\
	fprintf ((FILE), "0x%x", (PTR)[sp]); }			\
    if (++sp < (LEN))						\
      {	if (lp > 60)						\
	  { lp = 0;						\
	    fprintf ((FILE), "\n\t%s ", ASCII_DATA_ASM_OP); }	\
	else							\
	  putc (',', (FILE));					\
	goto loop; }						\
    putc ('\n', (FILE)); } while (0)

/* Output a float value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)			\
 do { long l;								\
      REAL_VALUE_TO_TARGET_SINGLE (r, l);				\
      /* Use hex representation even if CODE is f.  as needs it.  */	\
      fprintf ((FILE), "&0x%lx", l);					\
    } while (0)

/* Output a double value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */
#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
 do { long l[2];							\
      REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);				\
      fprintf ((FILE), "&0x%lx%08lx", l[0], l[1]);			\
    } while (0)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf ((LABEL), "%s%%%d", (PREFIX), (NUM))

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
    fprintf (FILE, "%s%%%d:\n", PREFIX, NUM)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "%s", NAME)

/* This is how to output an element of a case-vector that is absolute.
   (The 68000 does not use such vectors,
   but we must define this macro anyway.)  */

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)	\
    fprintf (FILE, "\tlong L%%%d\n", (VALUE))

/* This is how to output an element of a case-vector that is relative.  */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)	\
    fprintf (FILE, "\tshort L%%%d-L%%%d\n", (VALUE), (REL))

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLE)			\
    fprintf (FILE, "\tswbeg &%d\n%s%%%d:\n",				\
	     XVECLEN (PATTERN (TABLE), 1), (PREFIX), (NUM))

/* sysV68 as cannot handle LD%n(%pc,%reg) */ 
#define SGS_NO_LI

/* labelno is not used here */
#define ASM_OUTPUT_CASE_FETCH(file, labelno, regname)\
	asm_fprintf (file, "12(%Rpc,%s.", regname)

#define ASM_RETURN_CASE_JUMP   return "jmp 8(%%pc,%0.w)"
	     
/* Translate some opcodes to fit the sysV68 assembler syntax.  */
/* The opcodes fdmov and fsmov are guesses.  */

/* cliffm@netcom.com says no need for .w suffix on jumps.  */
#undef ASM_OUTPUT_OPCODE
#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{ if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR);						\
      while (*(PTR) != ' ')				\
	{ putc (*(PTR), (FILE)); ++(PTR); }		\
    }							\
  else if ((PTR)[0] == 's')				\
    {							\
      if (!strncmp ((PTR), "swap", 4))			\
	{ fprintf ((FILE), "swap.w"); (PTR) += 4; }	\
    }							\
  else if ((PTR)[0] == 'f')				\
    {							\
      if (!strncmp ((PTR), "fmove", 5))			\
	{ fprintf ((FILE), "fmov"); (PTR) += 5; }	\
      else if (!strncmp ((PTR), "f%$move", 7))		\
	{ if (TARGET_68040_ONLY)			\
	    { fprintf ((FILE), "fsmov"); (PTR) += 7; }	\
	  else						\
	    { fprintf ((FILE), "fmov"); (PTR) += 7; } }	\
      else if (!strncmp ((PTR), "f%&move", 7))		\
	{ if (TARGET_68040_ONLY)			\
	    { fprintf ((FILE), "fdmov"); (PTR) += 7; }	\
	  else						\
	    { fprintf ((FILE), "fmov"); (PTR) += 7; } }	\
      else if (!strncmp ((PTR), "ftst", 4))		\
	{ fprintf ((FILE), "ftest"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "fbne", 4))		\
	{ fprintf ((FILE), "fbneq"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "fsne", 4))		\
	{ fprintf ((FILE), "fsneq"); (PTR) += 4; }	\
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

/* phdm@info.ucl.ac.be says to pass SIZE, not ROUNDED.  */

/* This says how to output an assembler line
   to define a global common symbol.  */

#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tlcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (SIZE)))


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
#define PUT_SDB_NEXT_DIM(a) fprintf(asm_out_file, "%d,", a)
#define PUT_SDB_LAST_DIM(a) fprintf(asm_out_file, "%d;", a)

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
   Use the subroutines that the sysV68's library provides.
   The `*' prevents an underscore from being prepended by the compiler.  */
/* The '*' is also used by INIT_CUMULATIVE_ARGS */

#define DIVSI3_LIBCALL "*ldiv%%"
#define UDIVSI3_LIBCALL "*uldiv%%"
#define MODSI3_LIBCALL "*lrem%%"
#define UMODSI3_LIBCALL "*ulrem%%"
#define MULSI3_LIBCALL "*lmul%%"

struct sysV68_cumulative_args
	{
	int	offset;
	int	libcall;
	};

#undef CUMULATIVE_ARGS
#define CUMULATIVE_ARGS struct sysV68_cumulative_args

#undef INIT_CUMULATIVE_ARGS
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)	\
do {(CUM).offset = 0;\
(CUM).libcall = (LIBNAME) && (*XSTR((LIBNAME), 0) == '*');} while(0)

#undef FUNCTION_ARG_ADVANCE
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM).offset += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE) + 3) & ~3	\
	    : (int_size_in_bytes (TYPE) + 3) & ~3))

#undef FUNCTION_ARG
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
(((CUM).libcall && (CUM).offset == 0) ? gen_rtx(REG, (MODE), 0)\
: (TARGET_REGPARM && (CUM).offset < 8) ? gen_rtx (REG, (MODE), (CUM).offset / 4) : 0)

#undef FUNCTION_ARG_PARTIAL_NREGS
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM).offset < 8				\
  && 8 < ((CUM).offset + ((MODE) == BLKmode			\
		      ? int_size_in_bytes (TYPE)		\
		      : GET_MODE_SIZE (MODE))))  		\
 ? 2 - (CUM).offset / 4 : 0)

#undef FUNCTION_ARG_REGNO_P
#define FUNCTION_ARG_REGNO_P(N) (TARGET_68020 ? 0 : (N) == 0)
