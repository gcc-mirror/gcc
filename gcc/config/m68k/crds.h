/* Definitions of target machine for GNU compiler;
   Charles River Data Systems UNiverse/32.
   Copyright (C) 1987, 1993, 1994, 1996, 1997 Free Software Foundation, Inc.
   Contributed by Gary E. Miller (Gary_Edmunds_Miller@cup.portal.com)

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

#define MOTOROLA		/* Use Motorola syntax rather than "MIT" */
#define SGS			/* Uses SGS assembler */
#define SGS_SWITCH_TABLES	/* Different switch table handling */
#define SGS_NO_LI		/* Suppress jump table label usage */
#define CRDS			/* Charles River Data Systems assembler */

#include "m68k/m68k.h"

/* Without STRUCTURE_SIZE_BOUNDARY, we can't ensure that structures are
   aligned such that we can correctly extract bitfields from them.
   Someone should check whether the usual compiler on the crds machine
   provides the equivalent behavior of STRUCTURE_SIZE_BOUNDARY.  */
/* Set to 16 because all other m68k targets have it so */
#define STRUCTURE_SIZE_BOUNDARY 16

/* See m68k.h.  0 means 680[01]0 with no 68881.  */

#undef TARGET_DEFAULT
#define	TARGET_DEFAULT 0

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* special flags to the unos assembler.  */

#undef ASM_SPEC
#define ASM_SPEC "-g"

#undef LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lunos}}%{p:-lc_p}%{pg:-lc_p}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{pg:gcrt0.o%s}%{!pg:%{p:mc68rt0.o%s}%{!p:c68rt0.o%s}}"

/* CC1 spec */
#if 0
/* c.sac only used in _s_call_r() in libunos.a and malloc() in libmalloc.a */
/* so we do not need to bother ! */
#define CC1_SPEC "-fpcc-struct-return"
#endif

/* -O2 for MAX optimization */
#undef CC1_SPEC
#define CC1_SPEC "%{O2:-fstrength-reduce}"

/* cpp has to support a #sccs directive for the /usr/include files */

#define SCCS_DIRECTIVE

/* Make output for SDB.  */

/* #define SDB_DEBUGGING_INFO UNOS casm has no debugging :-( */

/* UNOS need stack probe :-( */

#if 0
#define HAVE_probe 1
#define gen_probe()  gen_rtx(ASM_INPUT, VOIDmode, "tstb -2048(sp)\t;probe\n")
#else
#undef NEED_PROBE
#define NEED_PROBE (-2048)
#endif

/* use memcpy, memset instead of bcopy, etc. */

#define TARGET_MEM_FUNCTIONS

/* Don't try to define `gcc_compiled.' since the assembler might not
   accept symbols with periods and GDB doesn't run on this machine anyway.  */
#define ASM_IDENTIFY_GCC(FILE)

/* Define __HAVE_68881__ in preprocessor if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#undef CPP_SPEC
#define CPP_SPEC "%{m68881:-D__HAVE_68881__}"

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68k -DM68000 -Dmc68000 -Dunos -Dunix -D__motorola__ -Asystem(unix)  -Acpu(m68k) -Amachine(m68k)"

/* Register in which address to store a structure value
   is passed to a function.  */
/* unos uses ".comm c.sac" returns &c.sac in d0 */
/* make pointer to c.sac ?
#undef STRUCT_VALUE_REGNUM
#define STRUCT_VALUE gen_rtx(MEM, Pmode, gen_rtx( , , ) )
*/

#define BSS_SECTION_ASM_OP ".bss"

/* Specify how to pad function arguments.
   Value should be `upward', `downward' or `none'.
   Same as the default, except no padding for large or variable-size args.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE)				\
  (((MODE) == BLKmode							\
    ? ((TYPE) && TREE_CODE (TYPE_SIZE (TYPE))	== INTEGER_CST		\
       && int_size_in_bytes (TYPE) < PARM_BOUNDARY / BITS_PER_UNIT)	\
    : GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY)				\
   ? downward : none)

/* Override parts of m68k.h to fit the CRuDS assembler.  */

#undef TARGET_VERSION 
#define TARGET_VERSION fprintf (stderr, " (68k, CRDS/UNOS)");

/* Specify extra dir to search for include files.  */
#define SYSTEM_INCLUDE_DIR "/include"

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)	\
  fprintf (FILE, ";#NO_APP\n");

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#undef ASM_APP_ON
#define ASM_APP_ON ";#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#undef ASM_APP_OFF 
#define ASM_APP_OFF ";#NO_APP\n"

/* The prefix for immediate operands.  */

#undef IMMEDIATE_PREFIX
#define IMMEDIATE_PREFIX "$"

/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
do { long l[2];						\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);		\
     fprintf (FILE, "\t.long 0x%x, 0x%x\n", l[0], l[1]);	\
   } while (0)

/*unos has no .skip :-( */
#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)	 	\
    fprintf (FILE, "\t. = . + %u\n", (SIZE));	

/* This says how to output an assembler line
   to define a local common symbol.  */
/* should use bss_section instead of data_section but this makes casm die ? */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
{ data_section ();				\
  if ((SIZE) > 1) fprintf (FILE, "\t.even\n");	\
  assemble_name ((FILE), (NAME));		\
  fprintf ((FILE), ":\t. = . + %u\n", (ROUNDED));}

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#undef ASM_OUTPUT_REG_PUSH
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmovel %s,-(sp)\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#undef ASM_OUTPUT_REG_POP
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmovel (sp)+,%s\n", reg_names[REGNO])

#undef  ASM_OUTPUT_ASCII
#define  ASM_OUTPUT_ASCII(FILE, P , SIZE)				\
do {  int i;								\
	  fprintf ((FILE), "\t.ascii \"");				\
	  for (i = 0; i < (SIZE); i++)					\
	    {								\
	      register int c = (P)[i];					\
	      if (i != 0 && (i / 200) * 200 == i)			\
		fprintf ((FILE), "\"\n\t.ascii \"");			\
	      if (c >= ' ' && c < 0177) {				\
	        if (c != '\"' && c != '\\') {				\
		  putc (c, (FILE));					\
		  continue;						\
	        }							\
	       }							\
	       /* brain dead asm doesn't understand char escapes */	\
	       fprintf ((FILE), "\"\n\t.byte\t%d\n\t.ascii \"", c);	\
	    }								\
	  fprintf ((FILE), "\"\n");					\
 } while (0)


/* Change all JBxx to Bxx.  Also change all DBRA to DBF.
   Also change divs.l, etc., to divs, etc.  But don't change divsl.l.  */

#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{ if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR); }					\
  else if ((PTR)[0] == 'd')				\
    {							\
      if (!strncmp ((PTR), "dbra", 4))			\
	{ fprintf ((FILE), "dbf"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "div", 3) && (PTR)[5] == ' ')  \
	{ fprintf ((FILE), "div%c", (PTR)[3]); (PTR) += 6; }   \
    }							\
}


#if 0
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
       but & in SGS syntax, $ in unos syntax).
   '!' for the fpcr register (used in some float-to-fixed conversions).

   'b' for byte insn (no effect, on the Sun; this is for the ISI).
   'd' to force memory addressing to be absolute, not relative.
   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   'w' for FPA insn (print a CONST_DOUBLE as a SunFPA constant rather
       than directly).  Second part of 'y' below.
   'x' for float insn (print a CONST_DOUBLE as a float rather than in hex),
       or print pair of registers as rx:ry.
   'y' for a FPA insn (print pair of registers as rx:ry).  This also outputs
       CONST_DOUBLE's as SunFPA constant RAM registers if
       possible, so it should not be used except for the SunFPA. */

#undef PRINT_OPERAND_PUNCT_VALID_P
#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '.' || (CODE) == '#' || (CODE) == '-'			\
   || (CODE) == '+' || (CODE) == '@' || (CODE) == '!')

#undef PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)  \
{ int i;								\
  if (CODE == '.') ;							\
  else if (CODE == '#') fprintf (FILE, "$");				\
  else if (CODE == '-') fprintf (FILE, "-(sp)");			\
  else if (CODE == '+') fprintf (FILE, "(sp)+");			\
  else if (CODE == '@') fprintf (FILE, "(sp)");				\
  else if (CODE == '!') fprintf (FILE, "fpcr");				\
  else if (CODE == '/')							\
    ;									\
  else if (GET_CODE (X) == REG)						\
    { if (REGNO (X) < 16 && (CODE == 'y' || CODE == 'x') && GET_MODE (X) == DFmode)	\
        fprintf (FILE, "%s:%s", reg_names[REGNO (X)], reg_names[REGNO (X)+1]); \
      else								\
        fprintf (FILE, "%s", reg_names[REGNO (X)]);			\
    }									\
  else if (GET_CODE (X) == MEM)						\
    {									\
      output_address (XEXP (X, 0));					\
      if (CODE == 'd' && ! TARGET_68020					\
	  && CONSTANT_ADDRESS_P (XEXP (X, 0)))				\
	/* fprintf (FILE, ".l") */;					\
    }									\
  else if ((CODE == 'y' || CODE == 'w')					\
	   && GET_CODE(X) == CONST_DOUBLE				\
	   && (i = standard_sun_fpa_constant_p (X)))			\
    fprintf (FILE, "%%%d", i & 0x1ff);					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { REAL_VALUE_TYPE r; long l;					\
      REAL_VALUE_FROM_CONST_DOUBLE (r, X);				\
      if (CODE == 'f')							\
	ASM_OUTPUT_FLOAT_OPERAND (CODE, FILE, r);			\
      else								\
        { REAL_VALUE_TO_TARGET_SINGLE (r, l);				\
          fprintf (FILE, "$0x%x", l); } }				\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == DFmode)	\
    { REAL_VALUE_TYPE r;						\
      REAL_VALUE_FROM_CONST_DOUBLE (r, X);				\
      ASM_OUTPUT_DOUBLE_OPERAND (FILE, r); }				\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == XFmode)	\
    { REAL_VALUE_TYPE r;						\
      REAL_VALUE_FROM_CONST_DOUBLE (r, X);				\
      ASM_OUTPUT_LONG_DOUBLE_OPERAND (FILE, r); }			\
  else { putc ('$', FILE); output_addr_const (FILE, X); }}
#endif

/* Note that this contains a kludge that knows that the only reason
   we have an address (plus (label_ref...) (reg...))
   is in the insn before a tablejump, and we know that m68k.md
   generates a label LInnn: on such an insn.  */
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
	    fprintf (FILE, "L%d-LI%d-2(pc,%s.w",			\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (XEXP (ireg, 0))]); 		\
	  else								\
	    fprintf (FILE, "L%d-LI%d-2(pc,%s.l",			\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (ireg)]);				\
	  if (scale != 1) fprintf (FILE, ":%d", scale);			\
	  putc (')', FILE);						\
	  break; }							\
      if (breg != 0 && ireg == 0 && GET_CODE (addr) == LABEL_REF)	\
        { fprintf (FILE, "L%d-LI%d-2(pc,%s.l",				\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   reg_names[REGNO (breg)]);				\
	  putc (')', FILE);						\
	  break; }							\
      if (ireg != 0 || breg != 0)					\
	{ int scale = 1;						\
	  if (breg == 0)						\
	    abort ();							\
	  if (addr && GET_CODE (addr) == LABEL_REF) abort ();		\
	  if (addr != 0)						\
	    output_addr_const (FILE, addr);				\
	  fprintf (FILE, "(%s", reg_names[REGNO (breg)]);		\
	  if (breg != 0 && ireg != 0)					\
	    putc (',', FILE);						\
	  if (ireg != 0 && GET_CODE (ireg) == MULT)			\
	    { scale = INTVAL (XEXP (ireg, 1));				\
	      ireg = XEXP (ireg, 0); }					\
	  if (ireg != 0 && GET_CODE (ireg) == SIGN_EXTEND)		\
	    fprintf (FILE, "%s.w", reg_names[REGNO (XEXP (ireg, 0))]);	\
	  else if (ireg != 0)						\
	    fprintf (FILE, "%s.l", reg_names[REGNO (ireg)]);		\
	  if (scale != 1) fprintf (FILE, ":%d", scale);			\
	  putc (')', FILE);						\
	  break;							\
	}								\
      else if (reg1 != 0 && GET_CODE (addr) == LABEL_REF)		\
	{ fprintf (FILE, "L%d-LI%d-2(pc,%s.l)",				\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
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

#define ASM_OUTPUT_SOURCE_FILENAME(FILE, FILENAME) \
  do {	fprintf (FILE, "\t; file\t");			\
	output_quoted_string (FILE, FILENAME);		\
	fprintf (FILE, "\n");				\
  } while (0)

#define ASM_OUTPUT_SOURCE_LINE(FILE, LINENO)	\
  fprintf (FILE, "\t; ln\t%d\n",			\
	   (sdb_begin_function_line		\
	    ? last_linenum - sdb_begin_function_line : 1))

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

/* Note that the order of the bit mask for fmovem is the opposite
   of the order for movem!  */

#undef FUNCTION_PROLOGUE
#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno;						\
  register int mask = 0;					\
  extern char call_used_regs[];					\
  int fsize = ((SIZE) + 3) & -4;				\
  /* unos stack probe */					\
  if ( fsize > 30000 ) {					\
    fprintf (FILE, "\tmovel sp,a0\n");				\
    fprintf (FILE, "\taddl $-%d,a0\n", 2048 + fsize);		\
    fprintf (FILE, "\ttstb (a0)\n");				\
  } else {							\
    fprintf (FILE, "\ttstb -%d(sp)\n", 2048 + fsize);		\
  }								\
  if (frame_pointer_needed)					\
    { if (TARGET_68020 || fsize < 0x8000)			\
        fprintf (FILE, "\tlink a6,$%d\n", -fsize);		\
      else							\
	fprintf (FILE, "\tlink a6,$0\n\tsubl $%d,sp\n", fsize); }  \
  else if (fsize)						      \
    {								      \
      /* Adding negative number is faster on the 68040.  */	      \
      if (fsize + 4 < 0x8000)					      \
	{							      \
	  fprintf (FILE, "\tadd.w #%d,sp\n", - (fsize + 4));	      \
	}							      \
      else							      \
	{							      \
	  fprintf (FILE, "\tadd.l #%d,sp\n", - (fsize + 4));          \
	}							      \
    }								      \
  for (regno = 16; regno < 24; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (regno - 16);				\
  if ((mask & 0xff) != 0)					\
    fprintf (FILE, "\tfmovem $0x%x,-(sp)\n", mask & 0xff);       \
  mask = 0;							\
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (15 - regno);				\
  if (frame_pointer_needed)					\
    mask &= ~ (1 << (15-FRAME_POINTER_REGNUM));			\
  if (exact_log2 (mask) >= 0)					\
    fprintf (FILE, "\tmovel %s,-(sp)\n", reg_names[15 - exact_log2 (mask)]);  \
  else if (mask) fprintf (FILE, "\tmovem $0x%x,-(sp)\n", mask); }

/* Must put address in  %a0 , not  %d0 . -- LGM, 7/15/88 */
/* UNOS ?? */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABEL_NO)	\
    fprintf (FILE, "\tmovl &LP%%%d,%%a0\n\tjsr mcount\n", (LABEL_NO))

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#undef FUNCTION_EPILOGUE
#define FUNCTION_EPILOGUE(FILE, SIZE) \
{ register int regno;						\
  register int mask, fmask;					\
  register int nregs;						\
  int offset, foffset, fpoffset;				\
  extern char call_used_regs[];					\
  int fsize = ((SIZE) + 3) & -4;				\
  int big = 0;							\
  nregs = 0;  fmask = 0; fpoffset = 0;				\
  for (regno = 16; regno < 24; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; fmask |= 1 << (23 - regno); }			\
  foffset = fpoffset + nregs * 12;				\
  nregs = 0;  mask = 0;						\
  if (frame_pointer_needed) regs_ever_live[FRAME_POINTER_REGNUM] = 0; \
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; mask |= 1 << regno; }				\
  offset = foffset + nregs * 4;					\
  if (offset + fsize >= 0x8000 					\
      && frame_pointer_needed 					\
      && (mask || fmask || fpoffset)) 				\
    { fprintf (FILE, "\tmovel $%d,a0\n", -fsize);		\
      fsize = 0, big = 1; }					\
  if (exact_log2 (mask) >= 0) {					\
    if (big)							\
      fprintf (FILE, "\tmovel -%d(a6,a0.l),%s\n",		\
	       offset + fsize, reg_names[exact_log2 (mask)]);	\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tmovel (sp)+,%s\n",			\
	       reg_names[exact_log2 (mask)]);			\
    else							\
      fprintf (FILE, "\tmovel -%d(a6),%s\n",			\
	       offset + fsize, reg_names[exact_log2 (mask)]); }	\
  else if (mask) {						\
    if (big)							\
      fprintf (FILE, "\tmovem -%d(a6,a0.l),$0x%x\n",		\
	       offset + fsize, mask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tmovem (sp)+,$0x%x\n", mask);		\
    else							\
      fprintf (FILE, "\tmovem -%d(a6),$0x%x\n",		\
	       offset + fsize, mask); }				\
  if (fmask) {							\
    if (big)							\
      fprintf (FILE, "\tfmovem -%d(a6,a0.l),$0x%x\n",		\
	       foffset + fsize, fmask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tfmovem (sp)+,$0x%x\n", fmask);		\
    else							\
      fprintf (FILE, "\tfmovem -%d(a6),$0x%x\n",		\
	       foffset + fsize, fmask); }			\
  if (fpoffset != 0)						\
    for (regno = 55; regno >= 24; regno--)			\
      if (regs_ever_live[regno] && ! call_used_regs[regno]) {	\
	if (big)						\
	  fprintf(FILE, "\tfpmoved -%d(a6,a0.l), %s\n",	\
		  fpoffset + fsize, reg_names[regno]);		\
	else if (! frame_pointer_needed)			\
	  fprintf(FILE, "\tfpmoved (sp)+, %s\n",			\
		  reg_names[regno]);				\
	else							\
	  fprintf(FILE, "\tfpmoved -%d(a6), %s\n",		\
		  fpoffset + fsize, reg_names[regno]);		\
	fpoffset -= 8;						\
      }								\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tunlk a6\n");				\
  else if (fsize)                                                     \
    {                                                                 \
      if (fsize + 4 < 0x8000)                                         \
	{                                                             \
	  fprintf (FILE, "\tadd.w #%d,sp\n", fsize + 4);              \
	}                                                             \
      else                                                            \
	{                                                             \
	  fprintf (FILE, "\tadd.l #%d,sp\n", fsize + 4);              \
	}                                                             \
    }                                                                 \
  if (current_function_pops_args)				\
    fprintf (FILE, "\trtd $%d\n", current_function_pops_args);	\
  else fprintf (FILE, "\trts\n"); }

