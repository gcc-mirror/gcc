/* Definitions of target machine for GNU compiler.
   Copyright (C) 1990 Free Software Foundation, Inc.

   Written by Robert Andersson, International Systems, Oslo, Norway.
   Send bug reports, questions and improvements to ra@intsys.no.

   For NCR Tower 32/4x0 and 32/6x0 running System V Release 3.
   This file outputs assembler source suitable for the native Tower as
   and with sdb debugging symbols.  See tower.h for more comments.

   This file was based on m68k.h, hp320.h and 3b1.h
   as of the 1.37.1 version.


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


#include "tower.h"
#undef SELECT_RTX_SECTION

/* Use default settings for system V.3.  */

#include "svr3.h"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dunix -Dtower32 -Dtower32_600"

/* Define __HAVE_68881 in preprocessor only if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.
   Also, define special define used to identify the Tower assembler.  */

#define CPP_SPEC "-D__TOWER_ASM__ %{m68881:-D__HAVE_68881__}"

/* The startfiles and libraries depend on the -p and -m68881 options.
   The Tower does not support the -pg option.  */

#define STARTFILE_SPEC                                         \
"%{p:%{m68881:/usr/lib/fp/mcrt1.o}%{!m68881:/lib/mcrt1.o}}     \
 %{!p:%{m68881:/usr/lib/fp/crt1.o}%{!m68881:/lib/crt1.o}}      \
 crtbegin.o%s"

/* We don't want local labels to start with period.
   See ASM_OUTPUT_INTERNAL_LABEL.  */
#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX ""

/* These four macros control how m68k.md is expanded.  */

#define MOTOROLA		/* Use Motorola syntax rather than "MIT" */
#define SGS			/* Uses SGS assembler */
#define SGS_CMP_ORDER		/* Takes cmp operands in reverse order */
#define SGS_NO_LI		/* Suppress jump table label usage */

/* Turn on SDB debugging info.  */

#define SDB_DEBUGGING_INFO

/* This is only useful if gdb is changed, but doesn't harm anyway.  */

#define ASM_IDENTIFY_GCC(FILE) \
  fprintf (FILE, "gcc2_compiled%%:\n")

/* All the ASM_OUTPUT macros need to conform to the Tower as syntax.  */

#define ASM_OUTPUT_SOURCE_FILENAME(FILE, FILENAME) \
  do {						   \
    fprintf (FILE, "\tfile\t\"%s\"\n", FILENAME);  \
    fprintf (FILE, "section ~init,\"x\"\n");	   \
    fprintf (FILE, "section ~fini,\"x\"\n");	   \
    fprintf (FILE, "section ~rodata,\"x\"\n");   \
    fprintf (FILE, "text\n");			   \
  } while (0)

#define ASM_OUTPUT_SOURCE_LINE(FILE, LINENO)	\
  fprintf (FILE, "\tln\t%d\n",			\
	   (sdb_begin_function_line		\
	    ? last_linenum - sdb_begin_function_line : 1))

#undef ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "\tident\t\"%s\" \n", NAME)

#define ASM_OUTPUT_ASCII(FILE,PTR,LEN) \
  { register int sp = 0, lp = 0; \
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
	    fprintf ((FILE), "\n\tbyte\t"); }	\
	else \
	  putc (',', (FILE)); \
	goto loop; } \
    putc ('\n', (FILE)); }

/* Translate Motorola opcodes such as `jbeq'
   into SGS/Tower opcodes such as `beq.w'.
   Change `move' to `mov'.
   Change `cmpm' to `cmp'.
   Change `divsl' to `tdivs'.
   Change `divul' to `tdivu'.
   Change `ftst' to `ftest'.
   Change `fmove' to `fmov'.  */

#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{ if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR);						\
      while (*(PTR) != ' ')				\
	{ putc (*(PTR), (FILE)); ++(PTR); }		\
      fprintf ((FILE), ".w"); }				\
  else if ((PTR)[0] == 'm' && (PTR)[1] == 'o'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 'e')	\
    { fprintf ((FILE), "mov"); (PTR) += 4; }		\
  else if ((PTR)[0] == 'c' && (PTR)[1] == 'm'		\
	   && (PTR)[2] == 'p' && (PTR)[3] == 'm')	\
    { fprintf ((FILE), "cmp"); (PTR) += 4; }		\
  else if ((PTR)[0] == 'd' && (PTR)[1] == 'i'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 's'	\
	   && (PTR)[4] == 'l')				\
    { fprintf ((FILE), "tdivs"); (PTR) += 5; }		\
  else if ((PTR)[0] == 'd' && (PTR)[1] == 'i'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 'u'	\
	   && (PTR)[4] == 'l')				\
    { fprintf ((FILE), "tdivu"); (PTR) += 5; }		\
  else if ((PTR)[0] == 'f' && (PTR)[1] == 't'		\
	   && (PTR)[2] == 's' && (PTR)[3] == 't')	\
    { fprintf ((FILE), "ftest"); (PTR) += 4; }		\
  else if ((PTR)[0] == 'f' && (PTR)[1] == 'm'		\
	   && (PTR)[2] == 'o' && (PTR)[3] == 'v' 	\
	   && (PTR)[4] == 'e')				\
    { fprintf ((FILE), "fmov"); (PTR) += 5; }		\
}



/* Override parts of m68k.h to fit the Tower assembler.
   This section needs to track changes done to m68k.h in the future.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (68k, Motorola/SGS/Tower32 syntax)");

#undef FUNCTION_BLOCK_PROFILER
#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)				\
  do {									\
    char label1[20], label2[20];					\
    ASM_GENERATE_INTERNAL_LABEL (label1, "LPBX", 0);			\
    ASM_GENERATE_INTERNAL_LABEL (label2, "LPI", LABELNO);		\
    fprintf (FILE, "\ttst.l %s\n\tbne %s\n\tpea %s\n\tjsr __bb_init_func\n\taddq.l &4,%%sp\n",	\
	     label1, label2, label1);					\
    ASM_OUTPUT_INTERNAL_LABEL (FILE, "LPI", LABELNO);			\
    putc ('\n', FILE);						\
    } while (0)

#undef BLOCK_PROFILER
#define BLOCK_PROFILER(FILE, BLOCKNO)				\
  do {								\
    char label[20];						\
    ASM_GENERATE_INTERNAL_LABEL (label, "LPBX", 2);		\
    fprintf (FILE, "\taddq.l &1,%s+%d\n", label, 4 * BLOCKNO);	\
    } while (0)

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABEL_NO)	\
    fprintf (FILE, "\tmov.l &LP%%%d,%%a0\n\tjsr mcount%%\n", (LABEL_NO))

/* The prologue is identical to the one in m68k.h except that the
   assembler syntax is different.  */

#undef FUNCTION_PROLOGUE
#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno;						\
  register int mask = 0;					\
  extern char call_used_regs[];					\
  int fsize = ((SIZE) + 3) & -4;				\
  if (frame_pointer_needed)					\
    { if (TARGET_68020 || fsize < 0x8000)			\
        fprintf (FILE, "\tlink %%a6,&%d\n", -fsize);		\
      else							\
	fprintf (FILE, "\tlink %%a6,&0\n\tsub.l &%d,%%sp\n", fsize); }  \
  for (regno = 16; regno < 24; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (regno - 16);				\
  if ((mask & 0xff) != 0)					\
    fprintf (FILE, "\tfmovm &0x%x,-(%%sp)\n", mask & 0xff);     \
  mask = 0;							\
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (15 - regno);				\
  if (frame_pointer_needed)					\
    mask &= ~ (1 << (15-FRAME_POINTER_REGNUM));			\
  if (exact_log2 (mask) >= 0)					\
    fprintf (FILE, "\tmov.l %s,-(%%sp)\n", reg_names[15 - exact_log2 (mask)]); \
  else if (mask) fprintf (FILE, "\tmovm.l &0x%x,-(%%sp)\n", mask); }

/* The epilogue is identical to the one in m68k.h except that:
   a) The assembler syntax is different.
   b) Pointers are returned both in %d0 and %a0.
   c) FUNCTION_EXTRA_EPILOGUE is not needed.  */

#undef FUNCTION_EPILOGUE
#define FUNCTION_EPILOGUE(FILE, SIZE) \
{ register int regno;						\
  register int mask, fmask;					\
  register int nregs;						\
  int offset, foffset;		   	                        \
  extern char call_used_regs[];					\
  int fsize = ((SIZE) + 3) & -4;				\
  int big = 0;							\
  nregs = 0;  fmask = 0;		  			\
  for (regno = 16; regno < 24; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; fmask |= 1 << (23 - regno); }			\
  foffset = nregs * 12;						\
  nregs = 0;  mask = 0;						\
  if (frame_pointer_needed) regs_ever_live[FRAME_POINTER_REGNUM] = 0; \
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; mask |= 1 << regno; }				\
  offset = foffset + nregs * 4;					\
  if (offset + fsize >= 0x8000 					\
      && frame_pointer_needed 					\
      && (mask || fmask))	 				\
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
      fprintf (FILE, "\tfmovm -%d(%%a6,%%a0.l),&0x%x\n",	\
	       foffset + fsize, fmask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tfmovm (%%sp)+,&0x%x\n", fmask);		\
    else							\
      fprintf (FILE, "\tfmovm -%d(%%a6),&0x%x\n",		\
	       foffset + fsize, fmask); }			\
  if (current_function_returns_pointer)                         \
    fprintf (FILE, "\tmov.l %%d0,%%a0\n");                      \
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tunlk %%a6\n");				\
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

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
( fprintf (FILE, "#NO_APP\n"), \
  output_file_directive ((FILE), main_input_filename))

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "text"

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "data"

/* This says how to output an assembler line to define a global common symbol.
   We use SIZE rather than ROUNDED, as this is what the native cc does.  */

#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", ((SIZE) == 0) ? (ROUNDED) : (SIZE)))

/* This says how to output an assembler line to define a local common symbol.
   We use SIZE rather than ROUNDED, as this is what the native cc does.  */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tlcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", ((SIZE) == 0) ? (ROUNDED) : (SIZE)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 11),	\
  sprintf ((OUTPUT), "%s%%%%%d", (NAME), (LABELNO)))
 
/* This is the command to make the user-level label named NAME
   defined for reference from other files.  */

#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP "global"

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf ((LABEL), "%s%%%d", (PREFIX), (NUM))

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf ((FILE), "%s%%%d:\n", (PREFIX), (NUM))

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLE)			\
  fprintf (FILE, "\tswbeg &%d\n%s%%%d:\n",				\
           XVECLEN (PATTERN (TABLE), 1), (PREFIX), (NUM));		\

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
do { union { double d; long l[2]; } tem;		\
     tem.d = (VALUE);					\
     fprintf(FILE, "\tlong 0x%x,0x%x\n", tem.l[0], tem.l[1]); \
   } while (0)

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
do { union { float f; long l;} tem;			\
     tem.f = (VALUE);					\
     fprintf (FILE, "\tlong 0x%x\n", tem.l);	\
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

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\tlong L%%%d\n", (VALUE))

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\tshort L%%%d-L%%%d\n", (VALUE), (REL))

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) == 1)			\
    fprintf (FILE, "\teven\n");	        \
  else if ((LOG) != 0)			\
    abort ();

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\tspace %d\n", (SIZE))

#undef PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (CODE == '.') fprintf (FILE, ".");					\
  else if (CODE == '#') fprintf (FILE, "&");				\
  else if (CODE == '-') fprintf (FILE, "-(%%sp)");			\
  else if (CODE == '+') fprintf (FILE, "(%%sp)+");			\
  else if (CODE == '@') fprintf (FILE, "(%%sp)");			\
  else if (CODE == '!') fprintf (FILE, "%%fpcr");			\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { union { double d; int i[2]; } u;					\
      union { float f; int i; } u1;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      u1.f = u.d;							\
      fprintf (FILE, "&0x%x", u1.i); }				        \
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == DFmode)	\
      fprintf (FILE, "&0x%x%08x", CONST_DOUBLE_LOW (X), CONST_DOUBLE_HIGH (X));\
  else { putc ('&', FILE); output_addr_const (FILE, X); }}

/* Note that this contains a kludge that knows that the only reason
   we have an address (plus (label_ref...) (reg...))
   is in the insn before a tablejump, and we know that the table is
   exactly 10 bytes away.  */

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
	    fprintf (FILE, "10(%%pc,%s.w",				\
		     reg_names[REGNO (XEXP (ireg, 0))]); 		\
	  else								\
	    fprintf (FILE, "10(%%pc,%s.l",				\
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
	{ fprintf (FILE, "10(%%pc,%s.w)",				\
		   reg_names[REGNO (reg1)]);				\
	  break; }							\
    default:								\
        output_addr_const (FILE, addr);					\
    }}



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

#define NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

/* The usual definitions don't work because neither $ nor . is allowed.  */
#define CONSTRUCTOR_NAME_FORMAT "_GLOBAL_%%I%%%s"

/* Define a few machine-specific details
   of the implementation of constructors.

   The __CTORS_LIST__ goes in the .init section.  Define CTOR_LIST_BEGIN
   and CTOR_LIST_END to contribute to the .init section an instruction to
   push a word containing 0 (or some equivalent of that).

   ASM_OUTPUT_CONSTRUCTOR should be defined
   to push the address of the constructor.  */

#define ASM_LONG	"\tlong"
#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP	"section\t~init"
#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP	"section\t~fini"
#undef CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP	"section\t~rodata"

#define CTOR_LIST_BEGIN				\
  asm (INIT_SECTION_ASM_OP);			\
  asm ("clr.l -(%sp)")
#define CTOR_LIST_END CTOR_LIST_BEGIN

#define BSS_SECTION_ASM_OP	"section\t~bss"

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    init_section ();				\
    fprintf (FILE, "\tmov.l &");		\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, ",-(%%sp)\n");		\
  } while (0)

/* We do not want leading underscores.  */

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)  \
  fprintf (FILE, "%s", NAME)
