/* Definitions of target machine for GNU compiler.  
   SysV68 Motorola 3300 Delta Series

   Written by Abramo and Roberto Bagnara
   after Alex Crain's 3B1 definitions.

   Bug reports to bagnara@dipisa.di.unipi.it

   Copyright (C) 1987 Free Software Foundation, Inc.

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
#define SGS			/* Uses SGS assembler */
#define SGS_CMP_ORDER		/* Takes cmp operands in reverse order */
#define SGS_SWAP_W		/* Use swap.w rather than just plain swap */
#define SGS_SWITCH_TABLES	/* Different switch table handling */

#include "m68k.h"

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define	TARGET_DEFAULT 7
#endif

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

/* cpp has to support a #sccs directive for the /usr/include files */

#define SCCS_DIRECTIVE

/* Make output for SDB.  */

#define SDB_DEBUGGING_INFO

#undef REGISTER_NAMES
#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",	\
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%fp", "%sp",	\
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7"}

#undef FUNCTION_PROLOGUE
#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno;						\
  register int mask = 0;					\
  extern char call_used_regs[];					\
  int fsize = (SIZE);						\
  if (frame_pointer_needed)					\
    { if (fsize < 0x8000)					\
        fprintf (FILE, "\tlink.w %%fp,&%d\n", -fsize);		\
      else if (TARGET_68020)					\
        fprintf (FILE, "\tlink.l %%fp,&%d\n", -fsize);		\
      else							\
	fprintf (FILE, "\tlink.w %%fp,&0\n\tsub.l &%d,%%sp\n", fsize); }  \
  for (regno = 16; regno < FIRST_PSEUDO_REGISTER; regno++)	\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (regno - 16);				\
  if (mask != 0)						\
    fprintf (FILE, "\tfmovem &0x%x,-(%%sp)\n", mask & 0xff);	\
  mask = 0;							\
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (15 - regno);				\
  if (frame_pointer_needed)					\
    mask &= ~ (1 << (15-FRAME_POINTER_REGNUM));			\
  if (exact_log2 (mask) >= 0)					\
    fprintf (FILE, "\tmov.l %s,-(%%sp)\n", reg_names[15 - exact_log2 (mask)]);  \
  else if (mask) fprintf (FILE, "\tmovm.l &0x%x,-(%%sp)\n", mask); }

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABEL_NO)	\
    fprintf (FILE, "\tmov.l &LP%%%d,%%a0\n\tjsr mcount%%\n", (LABEL_NO))

#undef FUNCTION_EPILOGUE
#define FUNCTION_EPILOGUE(FILE, SIZE) \
{ register int regno;						\
  register int mask, fmask;					\
  register int nregs;						\
  int offset, foffset;						\
  extern char call_used_regs[];					\
  int fsize = (SIZE);						\
  int big = 0;							\
  nregs = 0;  fmask = 0;					\
  for (regno = 16; regno < FIRST_PSEUDO_REGISTER; regno++)	\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; fmask |= 1 << (23 - regno); }			\
  foffset = nregs * 12;						\
  nregs = 0;  mask = 0;						\
  if (frame_pointer_needed) regs_ever_live[FRAME_POINTER_REGNUM] = 0; \
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; mask |= 1 << regno; }				\
  offset = foffset + nregs * 4;					\
  if (offset + fsize >= 0x8000 && frame_pointer_needed)		\
    { fprintf (FILE, "\tmov.l &%d,%%a0\n", -fsize);		\
      fsize = 0, big = 1; }					\
  if (exact_log2 (mask) >= 0) {					\
    if (big)							\
      fprintf (FILE, "\tmov.l -%d(%%fp,%%a0.l),%s\n",		\
	       offset + fsize, reg_names[exact_log2 (mask)]);	\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tmov.l (%%sp)+,%s\n",			\
	       reg_names[exact_log2 (mask)]);			\
    else							\
      fprintf (FILE, "\tmov.l -%d(%%fp),%s\n",			\
	       offset + fsize, reg_names[exact_log2 (mask)]); }	\
  else if (mask) {						\
    if (big)							\
      fprintf (FILE, "\tmovm.l -%d(%%fp,%%a0.l),&0x%x\n",	\
	       offset + fsize, mask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tmovm.l (%%sp)+,&0x%x\n", mask);		\
    else							\
      fprintf (FILE, "\tmovm.l -%d(%%fp),&0x%x\n",		\
	       offset + fsize, mask); }				\
  if (fmask) {							\
    if (big)							\
      fprintf (FILE, "\tfmovem -%d(%%fp,%%a0.l),&0x%x\n",	\
	       foffset + fsize, fmask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tfmovem (%%sp)+,&0x%x\n", fmask);	\
    else							\
      fprintf (FILE, "\tfmovem -%d(%%fp),&0x%x\n",		\
	       foffset + fsize, fmask); }			\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tunlk %%fp\n");				\
  if (current_function_pops_args)				\
    fprintf (FILE, "\trtd &%d\n", current_function_pops_args);	\
  else fprintf (FILE, "\trts\n"); }

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

/* Don't try to define `gcc_compiled.' since the assembler might not
   accept symbols with periods and GDB doesn't run on this machine anyway.  */

#define ASM_IDENTIFY_GCC(FILE)

/* Names to predefine in the preprocessor for this target machine.  */
/* ihnp4!lmayk!lgm@eddie.mit.edu says mc68000 and m68k should not be here,
   on the other hand I don't care what he says.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dm68k -Dunix -DsysV68"

/* Specify how to pad function arguments.
   Value should be `upward', `downward' or `none'.
   Same as the default, except no padding for large or variable-size args.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE)				\
  (((MODE) == BLKmode							\
    ? ((TYPE) && TREE_CODE (TYPE_SIZE (TYPE))	== INTEGER_CST		\
       && int_size_in_bytes (TYPE) < PARM_BOUNDARY / BITS_PER_UNIT)	\
    : GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY)				\
   ? downward : none)

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
#define FUNCTION_VALUE(VALTYPE,FUNC) LIBCALL_VALUE(TYPE_MODE(VALTYPE))

/* sysV68 (brain damaged) cc convention support. */
/* Commented out until we find a safe way to make it optional.  */
#if 0
#define FUNCTION_VALUE(VALTYPE,FUNC) \
  (TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_68881 	\
   ? gen_rtx (REG, TYPE_MODE (VALTYPE), 16)		\
   : (TREE_CODE (VALTYPE) == POINTER_TYPE 		\
      ? gen_rtx (REG, TYPE_MODE (VALTYPE), 8)		\
      : gen_rtx (REG, TYPE_MODE (VALTYPE), 0)))
#endif

/* If TARGET_68881, SF and DF values are returned in fp0 instead of d0.  */

#undef LIBCALL_VALUE
#define LIBCALL_VALUE(MODE) \
 gen_rtx (REG, (MODE), ((TARGET_68881 && ((MODE) == SFmode || (MODE) == DFmode)) ? 16 : 0))

/* 1 if N is a possible register number for a function value.
   d0 may be used, and fp0 as well if -msoft-float is not specified.  */

#undef FUNCTION_VALUE_REGNO_P
#define FUNCTION_VALUE_REGNO_P(N) \
 ((N) == 0 || (TARGET_68881 && (N) == 16))

/* sysV68 (brain damaged) cc convention support. */
/* Commented out until we find a safe way to make it optional.  */
#if 0
#define FUNCTION_VALUE_REGNO_P(N) \
 ((N) == 0 || (N) == 8 || (TARGET_68881 && (N) == 16))
#endif 
 
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

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
do { union { double d; long l[2]; } tem;			\
     tem.d = (VALUE);						\
     fprintf(FILE, "\tlong 0x%x,0x%x\n", tem.l[0], tem.l[1]);	\
   } while (0)

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
do { union { float f; long l;} tem;			\
     tem.f = (VALUE);					\
     fprintf (FILE, "\tlong 0x%x\n", tem.l);		\
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
  fprintf (FILE, "\tfile\t\"%s\"\n", FILENAME)

#undef ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINENO)	\
  fprintf (FILE, "\tln\t%d\n",			\
	   (sdb_begin_function_line		\
	    ? last_linenum - sdb_begin_function_line : 1))

/* Yet another null terminated string format.  */

#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE,PTR,LEN) \
  { register int sp = 0, lp = 0;				\
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
    putc ('\n', (FILE)); }

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   On the 68000, we use several CODE characters:
   '.' for dot needed in Motorola-style opcode names.
   '-' for an operand pushing on the stack:
       sp@-, -(sp) or -(%sp) depending on the style of syntax.
   '+' for an operand pushing on the stack:
       sp@+, (sp)+ or (%sp)+ depending on the style of syntax.
   '@' for a reference to the top word on the stack:
       sp@, (sp) or (%sp) depending on the style of syntax.
   '#' for an immediate operand prefix (# in MIT and Motorola syntax
       but & in SGS syntax).
   '!' for the fpcr register (used in some float-to-fixed conversions).
   '$' for the letter `s' in an op code, but only on the 68040.
   '&' for the letter `d' in an op code, but only on the 68040.

   'b' for byte insn (no effect, on the Sun; this is for the ISI).
   'd' to force memory addressing to be absolute, not relative.
   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   'w' for FPA insn (print a CONST_DOUBLE as a SunFPA constant rather
       than directly).  Second part of 'y' below.
   'x' for float insn (print a CONST_DOUBLE as a float rather than in hex),
       or print pair of registers as rx:ry.
   'y' for a FPA insn (print pair of registers as rx:ry).  This also outputs
       CONST_DOUBLE's as SunFPA constant RAM registers if
       possible, so it should not be used except for the SunFPA.  */

#undef PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (CODE == '.') fprintf (FILE, ".");					\
  else if (CODE == '#') fprintf (FILE, "&");				\
  else if (CODE == '-') fprintf (FILE, "-(%%sp)");			\
  else if (CODE == '+') fprintf (FILE, "(%%sp)+");			\
  else if (CODE == '@') fprintf (FILE, "(%%sp)");			\
  else if (CODE == '!') fprintf (FILE, "%%fpcr");			\
  else if (CODE == '$') { if (TARGET_68040_ONLY) fprintf (FILE, "s"); }	\
  else if (CODE == '&') { if (TARGET_68040_ONLY) fprintf (FILE, "d"); }	\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { union { double d; int i[2]; } u;					\
      union { float f; int i; } u1;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      u1.f = u.d;							\
      /* Use hex representation even if CODE is f.  as needs it.  */	\
      if (CODE == 'f')							\
        fprintf (FILE, "&0x%x", u1.i);					\
      else								\
        fprintf (FILE, "&0x%x", u1.i); }				\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == DFmode)	\
    { union { double d; int i[2]; } u;					\
      PRINT_OPERAND_EXTRACT_FLOAT (X);					\
      fprintf (FILE, "&0x%08x%08x", u.i[0], u.i[1]); }			\
  else { putc ('&', FILE); output_addr_const (FILE, X); }}

#undef PRINT_OPERAND_ADDRESS
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
	    fprintf (FILE, "12(%%pc,%s.w",				\
		     reg_names[REGNO (XEXP (ireg, 0))]); 		\
	  else								\
	    fprintf (FILE, "12(%%pc,%s.l",				\
		     reg_names[REGNO (ireg)]);				\
	  if (scale != 1) fprintf (FILE, "*%d", scale);			\
	  fprintf (FILE, ")");						\
	  break; }							\
      if (breg != 0 && ireg == 0 && GET_CODE (addr) == LABEL_REF)	\
        { fprintf (FILE, "12(%%pc,%s.l",				\
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
	{ fprintf (FILE, "12(%%pc,%s.w)",				\
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
	     
#if 0
/* At end of a switch table, define LD%n iff the symbol LI%n was defined.  */
#define ASM_OUTPUT_CASE_END(FILE,NUM,TABLE)		\
{ if (switch_table_difference_label_flag)		\
    asm_fprintf (FILE, "\t%s %LLD%d,%LL%d-%LLI%d-2.b\n",\
		 SET_ASM_OP, (NUM), (NUM), (NUM))	\
  switch_table_difference_label_flag = 0; }
#endif

/* We have to define this to avoid errors.  */
int switch_table_difference_label_flag;

/* Translate some opcodes to fit the sysV68 assembler syntax.  */
/* The opcodes fdmov and fsmov are guesses.  */

#define SWITCH_JUMP_MATCH   "jmp 6(%%pc,"

#undef ASM_OUTPUT_OPCODE
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
/* JMP to switch label */				\
  else if (!strncmp((PTR), (SWITCH_JUMP_MATCH), sizeof(SWITCH_JUMP_MATCH) - 1)) \
    { while (*(PTR)++ != '(');				\
      fprintf ((FILE), "jmp 8("); }			\
}

/* This says how to output an assembler line
   to define a global common symbol.  */

#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tlcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))


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
   Use the subroutines that the 3b1's library provides.
   The `*' prevents an underscore from being prepended by the compiler.  */

#define DIVSI3_LIBCALL "*ldiv"
#define UDIVSI3_LIBCALL "*uldiv"
#define MODSI3_LIBCALL "*lrem"
#define UMODSI3_LIBCALL "*ulrem"
#define MULSI3_LIBCALL "*lmul"
#define UMULSI3_LIBCALL "*ulmul"
