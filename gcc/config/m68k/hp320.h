/* Definitions of target machine for GNU compiler.  HP-UX 68000/68020 version.
   Copyright (C) 1987, 1988 Free Software Foundation, Inc.

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

/* Define USE_GAS if GCC is supposed to work with the GNU assembler,
   GNU linker and GNU debugger using DBX debugging information.
   (In other words, much of HPUX has been cast aside.)
   Undefine USE_GAS if you want GCC to feed the HP assembler.  */

/* #define USE_GAS */  /* Use hp320g.h if you want this.  */

/* Control assembler-syntax conditionals in m68k.md.  */

#ifndef USE_GAS
#define MOTOROLA		/* Use Motorola syntax rather than "MIT" */
#define SGS			/* Uses SGS assembler */
#define SGS_CMP_ORDER		/* Takes cmp operands in reverse order */
#define HPUX_ASM

#if !defined (CROSS_COMPILE) && !defined (NO_BUGS)
/* The assembler on HP 9k3xx machines running HPUX 8.0 doesn't translate
   floating point constants behind some operands.  The workaround is to
   use hex constants.  Reported by Thomas Nau (nau@medizin.uni-ulm.de).  */
#define AS_BUG_FLOATING_CONSTANT
/* The assembler on HP 9k3xx machines running HPUX 8.0 doesn't accept
   labels followed by a text, data, or other section directive.  Reported
   by Thomas Nau (nau@medizin.uni-ulm.de).  */
#define AS_BUG_TRAILING_LABEL
#endif

/* gcc.c should find libgcc.a itself rather than expecting linker to.  */
#define LINK_LIBGCC_SPECIAL
/* The arguments of -L must be a separate argv element.  */
#define SPACE_AFTER_L_OPTION
/* HP/UX doesn't have libg.a.  */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"
#endif

/* Be compatible with system stddef.h.  */
#define SIZE_TYPE "unsigned int"

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

#include "m68k.h"

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define	TARGET_DEFAULT 7
#endif

/* Define __HAVE_68881__ in preprocessor, unless -msoft-float is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#ifdef HPUX_ASM

#define ASM_SPEC "%{m68000:+X}%{mc68000:+X}"

#if TARGET_DEFAULT & 02  /* -m68881 is the default */

/* These definitions differ from those used for GAS by defining __HPUX_ASM__.
   This is needed because some programs, particularly GDB, need to
   know which assembler is being used so that the correct `asm'
   instructions can be used. */

#define CPP_SPEC \
"%{!msoft-float:-D__HAVE_68881__ }\
%{!ansi:%{!mc68000:%{!m68000:-Dmc68020}} -D_HPUX_SOURCE} -D__HPUX_ASM__"

#else /* default is -msoft-float */

#define CPP_SPEC \
"%{m68881:-D__HAVE_68881__ }\
%{!ansi:%{!mc68000:%{!m68000:-Dmc68020}} -D_HPUX_SOURCE} -D__HPUX_ASM__"

#endif /* default is -msoft-float */

#else /* not HPUX_ASM */

#if TARGET_DEFAULT & 02  /* -m68881 is the default */

#define CPP_SPEC \
"%{!msoft-float:-D__HAVE_68881__ }\
%{!ansi:%{!mc68000:%{!m68000:-Dmc68020}} -D_HPUX_SOURCE}"

#else /* default is -msoft-float */

#define CPP_SPEC \
"%{m68881:-D__HAVE_68881__ }\
%{!ansi:%{!mc68000:%{!m68000:-Dmc68020}} -D_HPUX_SOURCE}"

#endif /* default is -msoft-float */


/* -m68000 requires special flags to the assembler.  */
#define ASM_SPEC \
 "%{m68000:-mc68000}%{mc68000:-mc68000}%{!mc68000:%{!m68000:-mc68020}}"

/* Tell GCC to put a space after -L when generating such options.  */
#define SPACE_AFTER_L_OPTION

#endif /* Not HPUX_ASM */

/* Names to predefine in the preprocessor for this target machine
   (for non-strict-ANSI programs only).  */
/* These are the ones defined by HPUX cc, plus mc68000 for uniformity with
   GCC on other 68000 systems.  */

#define CPP_PREDEFINES "-Dhp9000s200 -Dhp9000s300 -DPWB -Dhpux -Dunix -D__hp9000s300 -D__hp9000s200 -D__PWB -D__hpux -D__unix"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* hpux doesn't use static area for struct returns. */
#undef PCC_STATIC_STRUCT_RETURN

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

#if 0  /* No longer correct in HPUX version 6.5.  */
/* Function calls don't save any fp registers on hpux.  */
#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS						\
 {1, 1, 0, 0, 0, 0, 0, 0,						\
  1, 1, 0, 0, 0, 0, 0, 1,						\
  1, 1, 1, 1, 1, 1, 1, 1}
#endif /* 0 */

#ifdef HPUX_ASM

/* Override parts of m68k.h to fit the HPUX assembler.  */

#undef TARGET_VERSION
#undef REGISTER_NAMES
#undef FUNCTION_PROLOGUE
#undef FUNCTION_EPILOGUE
#undef ASM_OUTPUT_REG_PUSH
#undef ASM_OUTPUT_REG_POP
#undef ASM_FILE_START
#undef ASM_APP_ON
#undef ASM_APP_OFF
#undef TEXT_SECTION_ASM_OP
#undef DATA_SECTION_ASM_OP
#undef ASM_OUTPUT_DOUBLE
#undef ASM_OUTPUT_FLOAT
#undef ASM_OUTPUT_INT
#undef ASM_OUTPUT_SHORT
#undef ASM_OUTPUT_CHAR
#undef ASM_OUTPUT_BYTE
#undef ASM_OUTPUT_ADDR_VEC_ELT
#undef ASM_OUTPUT_ADDR_DIFF_ELT
#undef ASM_OUTPUT_ALIGN
#undef ASM_OUTPUT_SKIP
#undef ASM_OUTPUT_COMMON
#undef ASM_OUTPUT_LOCAL
#undef ASM_FORMAT_PRIVATE_NAME
#undef PRINT_OPERAND
#undef PRINT_OPERAND_ADDRESS
#undef FUNCTION_PROFILER
#undef ASM_OUTPUT_INTERNAL_LABEL
#undef GLOBAL_ASM_OP

#define TARGET_VERSION fprintf (stderr, " (68k, SGS/hpux syntax)");

#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",	\
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%fp", "%sp",	\
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7"}

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno;						\
  register int mask = 0;					\
  extern char call_used_regs[];					\
  int fsize = (SIZE);						\
  if (frame_pointer_needed)					\
    { if (fsize < 0x8000)					\
        fprintf (FILE, "\tlink.w %%a6,&%d\n", -fsize);		\
      else if (TARGET_68020)					\
        fprintf (FILE, "\tlink.l %%a6,&%d\n", -fsize);		\
      else							\
	fprintf (FILE, "\tlink.w %%a6,&0\n\tsub.l &%d,%%sp\n", fsize); }  \
  for (regno = 16; regno < FIRST_PSEUDO_REGISTER; regno++)	\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (regno - 16);				\
  if (mask != 0)						\
    fprintf (FILE, "\tfmovem &0x%x,-(%%sp)\n", mask & 0xff);       \
  mask = 0;							\
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (15 - regno);				\
  if (frame_pointer_needed)					\
    mask &= ~ (1 << (15-FRAME_POINTER_REGNUM));			\
  if (exact_log2 (mask) >= 0)					\
    fprintf (FILE, "\tmov.l %s,-(%%sp)\n", reg_names[15 - exact_log2 (mask)]);  \
  else if (mask) fprintf (FILE, "\tmovm.l &0x%x,-(%%sp)\n", mask); }

#define FUNCTION_PROFILER(FILE, LABEL_NO) \
   fprintf (FILE, "\tmov.l &LP%d,%%a0\n\tjsr mcount\n", (LABEL_NO));

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
      fprintf (FILE, "\tmov.l -%d(%%a6,%%a0.l),%s\n",		\
	       offset + fsize, reg_names[exact_log2 (mask)]);	\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tmov.l (%%sp)+,%s\n",			\
	       reg_names[exact_log2 (mask)]);			\
    else							\
      fprintf (FILE, "\tmov.l -%d(%%a6),%s\n",			\
	       offset + fsize, reg_names[exact_log2 (mask)]); }	\
  else if (mask) {						\
    if (big)							\
      fprintf (FILE, "\tmovm.l -%d(%%a6,%%a0.l),&0x%x\n",	\
	       offset + fsize, mask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tmovm.l (%%sp)+,&0x%x\n", mask);		\
    else							\
      fprintf (FILE, "\tmovm.l -%d(%%a6),&0x%x\n",		\
	       offset + fsize, mask); }				\
  if (fmask) {							\
    if (big)							\
      fprintf (FILE, "\tfmovem -%d(%%a6,%%a0.l),&0x%x\n",	\
	       foffset + fsize, fmask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tfmovem (%%sp)+,&0x%x\n", fmask);	\
    else							\
      fprintf (FILE, "\tfmovem -%d(%%a6),&0x%x\n",		\
	       foffset + fsize, fmask); }			\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tunlk %%a6\n");				\
  if (current_function_pops_args)				\
    fprintf (FILE, "\trtd &%d\n", current_function_pops_args);	\
  else fprintf (FILE, "\trts\n"); }

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmov.l %s,-(%%sp)\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmov.l (%%sp)+,%s\n", reg_names[REGNO])

/* For HPUX versions before 6.5, define this macro as empty.  */
#define ASM_FILE_START(FILE)						\
  if (TARGET_68020)							\
    {									\
      if (TARGET_68881)							\
	 fprintf (FILE, "\tversion 3\n"); /* 68020 fp regs saved */	\
      else								\
	 fprintf (FILE, "\tversion 2\n"); /* 68020 no fp regs saved */	\
    }									\
  else									\
    fprintf (FILE, "\tversion 1\n");	/* 68010 */

#define ASM_APP_ON ""

#define ASM_APP_OFF ""

#ifdef AS_BUG_TRAILING_LABEL
#define TEXT_SECTION_ASM_OP "\tlalign\t1\ntext"
#define DATA_SECTION_ASM_OP "\tlalign\t1\ndata"
#else
#define TEXT_SECTION_ASM_OP "text"
#define DATA_SECTION_ASM_OP "data"
#endif

#define	ASCII_DATA_ASM_OP "byte"
 
/* This is the command to make the user-level label named NAME
   defined for reference from other files.  */

#define GLOBAL_ASM_OP "global"

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tlcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u,2\n", (ROUNDED)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
do{  if (PREFIX[0] == 'L' && PREFIX[1] == 'I')		\
    fprintf(FILE, "\tset %s%d,.+2\n", PREFIX, NUM);	\
  else							\
    fprintf (FILE, "%s%d:\n", PREFIX, NUM);		\
} while(0)

#define ASM_OUTPUT_DOUBLE(FILE, VALUE)					\
  fprintf (FILE, "\tdouble 0f%.20g\n", (VALUE))

#define ASM_OUTPUT_FLOAT(FILE, VALUE)					\
  fprintf (FILE, "\tfloat 0f%.9g\n", (VALUE))

#ifdef AS_BUG_FLOATING_CONSTANT
#undef  ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE, VALUE)                  \
  do {								\
    union { double d; int i[2]; } dummy_u;			\
    dummy_u.d = (VALUE);					\
    asm_fprintf (FILE, "%I0x%x%08x", dummy_u.i[0], dummy_u.i[1]); \ 
  } while (0)

#undef  ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(FILE, VALUE)                   \
  do {								\
    union { float f; int i; } dummy_u;				\
    dummy_u.f = (VALUE);					\
    asm_fprintf (FILE, "%I0x%08x", dummy_u.i);			\
  } while (0)
#endif /* AS_BUG_FLOATING_CONSTANT */

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\tlong "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\tshort "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\tbyte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\tbyte 0x%x\n", (VALUE))

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\tlong L%d\n", VALUE)

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\tshort L%d-L%d\n", VALUE, REL)

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) == 1)			\
    fprintf (FILE, "\tlalign 2\n");	\
  else if ((LOG) != 0)			\
    abort ();

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\tspace %u\n", (SIZE))

#define ASM_OUTPUT_SOURCE_FILENAME(FILE, FILENAME)
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINENO)

#ifdef AS_BUG_FLOATING_CONSTANT
#define PRINT_OPERAND_FLOAT(FILE,CODE,FLOAT,INT)	\
  fprintf (FILE, "&0x%x", (INT))
#else
#define PRINT_OPERAND_FLOAT(FILE,CODE,FLOAT,INT)	\
  if (CODE == 'f')					\
    fprintf (FILE, "&0f%.9g", (FLOAT));			\
  else							\
    fprintf (FILE, "&0x%x", (INT))
#endif /* AS_BUG_FLOATING_CONSTANT */

#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (CODE == '.') fprintf (FILE, ".");					\
  else if (CODE == '#') fprintf (FILE, "&");				\
  else if (CODE == '-') fprintf (FILE, "-(%%sp)");			\
  else if (CODE == '+') fprintf (FILE, "(%%sp)+");			\
  else if (CODE == '@') fprintf (FILE, "(%%sp)");			\
  else if (CODE == '!') fprintf (FILE, "%%fpcr");			\
  else if (CODE == '$') { if (TARGET_68040_ONLY) fprintf (FILE, "s"); } \
  else if (CODE == '&') { if (TARGET_68040_ONLY) fprintf (FILE, "d"); } \
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { union { double d; int i[2]; } u;					\
      union { float f; int i; } u1;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      u1.f = u.d;							\
      PRINT_OPERAND_FLOAT (FILE,CODE, u1.f, u1.i); }			\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == DFmode)	\
    { union { double d; int i[2]; } u;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      fprintf (FILE, "&0f%.20g", u.d); }				\
  else { putc ('&', FILE); output_addr_const (FILE, X); }}

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
	    fprintf (FILE, "L%d-LI%d(%%pc,%s.w",			\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (XEXP (ireg, 0))]); 		\
	  else								\
	    fprintf (FILE, "L%d-LI%d(%%pc,%s.l",			\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (ireg)]);				\
	  if (scale != 1) fprintf (FILE, "*%d", scale);			\
	  putc (')', FILE);						\
	  break; }							\
      if (ireg != 0 || breg != 0)					\
	{ int scale = 1;						\
	  if (breg == 0)						\
	    abort ();							\
	  if (addr != 0)						\
	    output_addr_const (FILE, addr);				\
	  fprintf (FILE, "(%s", reg_names[REGNO (breg)]);		\
	  if (ireg != 0)						\
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
	{ fprintf (FILE, "L%d-LI%d(%%pc,%s.w)",				\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   reg_names[REGNO (reg1)]);				\
	  break; }							\
    default:								\
      if (GET_CODE (addr) == CONST_INT					\
	  && INTVAL (addr) < 0x8000					\
	  && INTVAL (addr) >= -0x8000)					\
	fprintf (FILE, "%d.w", INTVAL (addr));				\
      else								\
        output_addr_const (FILE, addr);					\
    }}

#define	ASM_OUTPUT_ASCII(f, p, size)	\
{ register int i;			\
  int inside;				\
  inside = FALSE;			\
  for (i = 0; i < (size); i++) {	\
    if (i % 8 == 0) {			\
      if (i != 0) {			\
	if (inside)			\
	  putc('"', (f));		\
	putc('\n', (f));		\
	inside = FALSE;			\
      }					\
      fprintf((f), "\t%s ", ASCII_DATA_ASM_OP);	\
    }					\
    if ((p)[i] < 32 || (p)[i] == '\\' || (p)[i] == '"' || (p)[i] == 127) {	\
      if (inside) {			\
	putc('"', (f));			\
	inside = FALSE;			\
      }					\
      if (i % 8 != 0)			\
	putc(',', (f));			\
      fprintf((f), "%d", (p)[i]);	\
    } else {				\
      if (!inside) {			\
	if (i % 8 != 0)			\
	  putc(',', (f));		\
	putc('"', (f));			\
	inside = TRUE;			\
      }					\
      putc((p)[i], (f));		\
    }					\
  }					\
  if (inside)				\
    putc('"', (f));			\
  putc('\n', (f));			\
}

/* Translate Motorola opcodes such as `jbeq'
   into SGS opcodes such as `beq.w'.
   Delete the `e' in `move...' and `fmove'.
   Change `ftst' to `ftest'.  */

#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{ if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR);						\
      while (*(PTR) != ' ')				\
	{ putc (*(PTR), (FILE)); ++(PTR); }		\
      fprintf ((FILE), ".w"); }				\
  else if ((PTR)[0] == 'f')				\
    {							\
      if (!strncmp ((PTR), "fmove", 5))			\
	{ fprintf ((FILE), "fmov"); (PTR) += 5; }	\
      else if (!strncmp ((PTR), "ftst", 4))		\
	{ fprintf ((FILE), "ftest"); (PTR) += 4; }	\
    }							\
  else if ((PTR)[0] == 'm' && (PTR)[1] == 'o'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 'e')	\
    { fprintf ((FILE), "mov"); (PTR) += 4; }		\
}

/* Prevent output of `gcc_compiled.:'.  */

#define ASM_IDENTIFY_GCC(FILE)

#else /* not HPUX_ASM */

#undef FUNCTION_PROFILER

/* HP-UX needs the call to mcount before the link instruction.
   Copy the return address onto the stack before the call to fake it out.  */
#define FUNCTION_PROFILER(FILE, LABEL_NO) \
   fprintf (FILE, "\tmovel a6@(4),sp@-\n" \
                  "\tmovl #LP%d,a0\n\tjsr mcount\n" \
                  "\taddqw #4,sp\n", (LABEL_NO));

#endif /* not HPUX_ASM */
