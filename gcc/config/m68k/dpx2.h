/* Definitions of target machine for GNU compiler.  
   Bull DPX/2 200 and 300 systems (m68k, SysVr3).
   Copyright (C) 1987, 1993, 1994 Free Software Foundation, Inc.
   Contributed by Frederic Pierresteguy (F.Pierresteguy@frcl.bull.fr).

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


#ifndef USE_GAS
#define MOTOROLA	   /* Use Motorola syntax rather than "MIT" */
#define SGS_NO_LI	   /* Suppress jump table label usage */
#define VERSADOS           /* This is the name of the assembler we have */
#endif

#include "m68k/m68k.h"
#undef SELECT_RTX_SECTION
#include "svr3.h"

/* See m68k.h.  7 means 68020 with 68881.
 * We really have 68030 and 68882,
 * but this will get us going.  
 */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 7
#endif

#define OBJECT_FORMAT_COFF
#define NO_SYS_SIGLIST

#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
/*
 * define all the things the compiler should
 */
#ifdef ncl_mr
# define CPP_PREDEFINES "-Dunix -Dbull -DDPX2 -DSVR3 -Dmc68000 -Dmc68020 -Dncl_mr=1 -D_BULL_SOURCE -D_POSIX_SOURCE -D_XOPEN_SOURCE -Asystem(unix) -Asystem(svr3)  -Acpu(m68k) -Amachine(m68k)"
#else
# ifdef ncl_el
# define CPP_PREDEFINES "-Dunix -Dbull -DDPX2 -DSVR3 -Dmc68000 -Dmc68020 -Dncl_el -D_BULL_SOURCE -D_POSIX_SOURCE -D_XOPEN_SOURCE -Asystem(unix) -Asystem(svr3)  -Acpu(m68k) -Amachine(m68k)"
# else
#   define CPP_PREDEFINES "-Dunix -Dbull -DDPX2 -DSVR3 -Dmc68000 -Dmc68020 -D_BULL_SOURCE -D_POSIX_SOURCE -D_XOPEN_SOURCE -Asystem(unix) -Asystem(svr3)  -Acpu(m68k) -Amachine(m68k)"
# endif
#endif

#undef	CPP_SPEC
/*
 * you can't get a DPX/2 without a 68882 but allow it
 * to be ignored...
 */
# define __HAVE_68881__ 1
# define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__ }"

#define HAVE_ATEXIT
#undef DO_GLOBAL_CTORS_BODY		/* don't use svr3.h version */
#undef DO_GLOBAL_DTORS_BODY

#ifndef USE_GAS
/*
 * handle the native MOTOROLA VERSAdos assembler.
 */

/* See m68k.h.  3 means 68020 with 68881 and no bitfiled
 * bitfield instructions do not seem to work a clean way.
 */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT 3

/* The native assembler doesn't support fmovecr.  */
#define NO_ASM_FMOVECR

#undef EXTRA_SECTIONS
#undef EXTRA_SECTION_FUNCTIONS
#undef READONLY_DATA_SECTION
#define READONLY_DATA_SECTION data_section
#undef SELECT_SECTION
#undef SELECT_RTX_SECTION
#define fini_section() while (0)

#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP "\tsection 15"
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP "\tsection 15"
#undef INIT_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP     "\tsection 14"
#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP    "\tsection 10"
#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP  "\tsection 15"


/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* Define if you don't want extended real, but do want to use the
   software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
#define REAL_ARITHMETIC 

#undef ASM_OUTPUT_SOURCE_FILENAME
#define ASM_OUTPUT_SOURCE_FILENAME(FILE, NA)	\
  do { fprintf ((FILE), "\t.file\t'%s'\n", (NA)); } while (0)

/* Assembler pseudos to introduce constants of various size.  */

#undef ASM_BYTE_OP
#define ASM_BYTE_OP "\tdc.b"
#undef ASM_LONG
#define ASM_LONG "\tdc.l"

/* 
 * we don't seem to support any of:
 * .globl
 * .even
 * .align
 * .ascii
 */
#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\tdcb.b %u,0\n", (SIZE))

#undef GLOBAL_ASM_OP 
#define GLOBAL_ASM_OP "\txdef"

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) >= 1)			\
    fprintf (FILE, "\tds.w 0\n");


#define STRING_LIMIT	(0)
#undef ASM_APP_ON
#define ASM_APP_ON ""
#undef ASM_APP_OFF
#define ASM_APP_OFF ""
/*
 * dc.b 'hello, world!'
 * dc.b 10,0
 * is how we have to output "hello, world!\n"
 */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(asm_out_file, p, thissize)		\
  do { register int i, c, f=0, len=0;				\
  for (i = 0; i < thissize; i++) { 				\
    c = p[i];							\
    if (c == '\'' || c < ' ' || c > 127) {			\
      switch(f) {						\
      case 0: /* need to output dc.b etc */			\
	fprintf(asm_out_file, "\tdc.b %d", c);			\
	f=1;							\
	break;							\
      case 1:							\
	fprintf(asm_out_file, ",%d", c);			\
	break;							\
      default:							\
	/* close a string */					\
	fprintf(asm_out_file, "'\n\tdc.b %d", c);		\
	f=1;							\
	break;							\
      }								\
    } else {							\
      switch(f) {						\
      case 0:							\
	fprintf(asm_out_file, "\tdc.b '%c", c);			\
	f=2;							\
	break;							\
      case 2:							\
        if (len >= 79) {					\
          fprintf(asm_out_file, "'\n\tdc.b '%c", c); 	        \
          len = 0; }						\
        else							\
	  fprintf(asm_out_file, "%c", c);			\
	break;							\
      default:							\
	len = 0;						\
	fprintf(asm_out_file, "\n\tdc.b '%c", c);		\
	f=2;							\
	break;							\
      }								\
    }								\
    len++;                                   			\
  }								\
  if (f==2)							\
    putc('\'', asm_out_file);					\
  putc('\n', asm_out_file); } while (0)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#undef ASM_OUTPUT_REG_PUSH
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmove.l %s,-(sp)\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#undef ASM_OUTPUT_REG_POP
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmove.l (sp)+,%s\n", reg_names[REGNO])


#define PUT_SDB_FUNCTION_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.def\t.bf%s\t.val\t*%s\t.scl\t101%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_FUNCTION_END(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.def\t.ef%s\t.val\t*%s\t.scl\t101%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_BLOCK_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.def\t.bb%s\t.val\t*%s\t.scl\t100%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_BLOCK_END(LINE)			\
  fprintf (asm_out_file,			\
	   "\t.def\t.eb%s\t.val\t*%s\t.scl\t100%s\t.line\t%d%s\t.endef\n",  \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_EPILOGUE_END(NAME)	

/* Output type in decimal not in octal as done in sdbout.c */	
#define PUT_SDB_TYPE(a) fprintf(asm_out_file, "\t.type\t0%d%s", a, SDB_DELIM)
		
#undef FUNCTION_PROLOGUE
#define FUNCTION_PROLOGUE(FILE, SIZE)                                 \
{                                                                     \
  register int regno;                                                 \
  register int mask = 0;                                              \
  int num_saved_regs = 0, first = 1;                                  \
  extern char call_used_regs[];                                       \
  int fsize = ((SIZE) + 3) & -4;                                      \
                                                                      \
                                                                      \
  if (frame_pointer_needed)                                           \
    {                                                                 \
      /* Adding negative number is faster on the 68040.  */           \
      if (fsize < 0x8000 && !TARGET_68040)                            \
	{                                                             \
	  fprintf (FILE, "\tlink %s,#%d\n", 	                      \
		       reg_names[FRAME_POINTER_REGNUM], -fsize);      \
	}                                                             \
      else if (TARGET_68020)                                          \
	{                                                             \
	  fprintf (FILE, "\tlink %s,#%d\n",	                      \
		       reg_names[FRAME_POINTER_REGNUM], -fsize);      \
	}                                                             \
      else                                                            \
	{                                                             \
	  fprintf (FILE, "\tlink %s,#0\n\tadd.l #%d,sp\n",	      \
		       reg_names[FRAME_POINTER_REGNUM], -fsize);      \
	}							      \
    }								      \
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
  for (regno = 23; regno >= 16; regno--)                              \
    if (regs_ever_live[regno] && ! call_used_regs[regno])             \
      if (first) {						      \
        fprintf (FILE, "\tfmovem.x %s", reg_names[regno]);            \
	first = 0;						      \
       }							      \
      else fprintf (FILE, "/%s", reg_names[regno]);            	      \
  if (!first) fprintf (FILE, ",-(sp)\n");			      \
								      \
  mask = 0;							      \
  for (regno = 0; regno < 16; regno++)				      \
    if (regs_ever_live[regno] && ! call_used_regs[regno])	      \
      {								      \
        mask |= 1 << (15 - regno);				      \
        num_saved_regs++;                   			      \
      }                                                               \
  if (frame_pointer_needed)                                           \
    {                                                                 \
      mask &= ~ (1 << (15 - FRAME_POINTER_REGNUM));                   \
      num_saved_regs--;                                               \
    }                                                                 \
                                                                      \
                                                                      \
  if (num_saved_regs <= 2)                                            \
    {                                                                 \
      /* Store each separately in the same order moveml uses.         \
         Using two movel instructions instead of a single moveml      \
         is about 15% faster for the 68020 and 68030 at no expense    \
         in code size */                                              \
                                                                      \
      int i;                                                          \
                                                                      \
      /* Undo the work from above. */                                 \
      for (i = 0; i< 16; i++)                                         \
        if (mask & (1 << i))                                          \
          fprintf (FILE, "\tmove.l %s,-(sp)\n", reg_names[15 - i]);   \
    }                                                                 \
  else if (mask)                                                      \
    {                                                                 \
      first = 1;                                                      \
      for (regno = 0; regno < 16; regno++)                            \
        if (mask & (1 << regno))                                      \
          if (first) {                                                \
            fprintf (FILE, "\tmovem.l %s", reg_names[15 - regno]);    \
            first = 0;                                                \
           }                                                          \
          else fprintf (FILE, "/%s", reg_names[15 - regno]);	      \
      fprintf (FILE, ",-(sp)\n");	           		      \
    }                                                                 \
  if (flag_pic && current_function_uses_pic_offset_table)             \
    {                                                                 \
      fprintf (FILE, "\tmove.l #__GLOBAL_OFFSET_TABLE_, %s\n",        \
		   reg_names[PIC_OFFSET_TABLE_REGNUM]);               \
      fprintf (FILE, "\tlea.l (pc,%s.l),%s\n",                        \
		   reg_names[PIC_OFFSET_TABLE_REGNUM],                \
		   reg_names[PIC_OFFSET_TABLE_REGNUM]);               \
    }                                                                 \
}


#undef FUNCTION_EPILOGUE
#define FUNCTION_EPILOGUE(FILE, SIZE)                                 \
{                                                                     \
  register int regno;                                                 \
  register int mask, fmask;                                           \
  register int nregs;                                                 \
  int offset, foffset, fpoffset, first = 1;		              \
  extern char call_used_regs[];                                       \
  int fsize = ((SIZE) + 3) & -4;                                      \
  int big = 0;                                                        \
  rtx insn = get_last_insn ();                                        \
                                                                      \
  /* If the last insn was a BARRIER, we don't have to write any code.  */ \
  if (GET_CODE (insn) == NOTE)                                        \
    insn = prev_nonnote_insn (insn);                                  \
  if (insn && GET_CODE (insn) == BARRIER)                             \
    {                                                                 \
      /* Output just a no-op so that debuggers don't get confused     \
	 about which function the pc is in at this address.  */       \
      fprintf (FILE, "\tnop\n");                                      \
      return;                                                         \
    }                                                                 \
                                                                      \
  nregs = 0;  fmask = 0; fpoffset = 0;                                \
  for (regno = 16; regno < 24; regno++)                               \
    if (regs_ever_live[regno] && ! call_used_regs[regno])             \
      {                                                               \
        nregs++;                                                      \
	fmask |= 1 << (23 - regno);                                   \
      }                                                               \
  foffset = fpoffset + nregs * 12;                                    \
  nregs = 0;  mask = 0;                                               \
  if (frame_pointer_needed)                                           \
    regs_ever_live[FRAME_POINTER_REGNUM] = 0;                         \
  for (regno = 0; regno < 16; regno++)                                \
    if (regs_ever_live[regno] && ! call_used_regs[regno])             \
      {                                                               \
        nregs++;                                                      \
	mask |= 1 << regno;                                           \
      }                                                               \
  offset = foffset + nregs * 4;                                       \
  if (offset + fsize >= 0x8000                                        \
      && frame_pointer_needed                                         \
      && (mask || fmask || fpoffset))                                 \
    {                                                                 \
      fprintf (FILE, "\tmove.l #%d,a0\n", -fsize);                    \
      fsize = 0, big = 1;                                             \
    }                                                                 \
  if (nregs <= 2)                                                     \
    {                                                                 \
      /* Restore each separately in the same order moveml does.       \
         Using two movel instructions instead of a single moveml      \
         is about 15% faster for the 68020 and 68030 at no expense    \
         in code size. */                                             \
                                                                      \
      int i;                                                          \
                                                                      \
      /* Undo the work from above. */                                 \
      for (i = 0; i< 16; i++)                                         \
        if (mask & (1 << i))                                          \
          {                                                           \
            if (big)                                                  \
	      {                                                       \
		fprintf (FILE, "\tmove.l -%d(%s,a0.l),%s\n",          \
			     offset + fsize,                          \
			     reg_names[FRAME_POINTER_REGNUM],         \
			     reg_names[i]);                           \
	      }                                                       \
            else if (! frame_pointer_needed)                          \
	      {                                                       \
		fprintf (FILE, "\tmove.l (sp)+,%s\n",                 \
			     reg_names[i]);                           \
	      }                                                       \
            else                                                      \
	      {                                                       \
		fprintf (FILE, "\tmove.l -%d(%s),%s\n",               \
			     offset + fsize,                          \
			     reg_names[FRAME_POINTER_REGNUM],         \
			     reg_names[i]);                           \
	      }                                                       \
            offset = offset - 4;                                      \
          }                                                           \
    }                                                                 \
  else if (mask)                                                      \
    {                                                                 \
      first = 1;						      \
      for (regno = 0; regno < 16; regno++)                            \
        if (mask & (1 << regno))                                      \
          if (first && big) {                                         \
            fprintf (FILE, "\tmovem.l -%d(%s,a0.l),%s",               \
		     offset + fsize,                                  \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
            first = 0;                                                \
           }                                                          \
          else if (first && ! frame_pointer_needed) {                 \
            fprintf (FILE, "\tmovem.l (sp)+,%s",                      \
		     offset + fsize,                                  \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
            first = 0;                                                \
           }                                                          \
          else if (first) {   				              \
            fprintf (FILE, "\tmovem.l -%d(%s),%s",                    \
		     offset + fsize,                                  \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
            first = 0;                                                \
           }                                                          \
          else  						      \
	    fprintf (FILE, "/%s", reg_names[regno]);	              \
      fprintf (FILE, "\n");	                 		      \
    }                                                                 \
  if (fmask)                                                          \
    {                                                                 \
      first = 1;						      \
      for (regno = 16; regno < 24; regno++)                           \
        if (fmask & (1 << (23 - regno)))                               \
          if (first && big) {	                                      \
            fprintf (FILE, "\tfmovem.x -%d(%s,a0.l),%s",              \
		     foffset + fsize,                                 \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
	    first = 0;						      \
           }                                                          \
          else if (first && ! frame_pointer_needed) {                 \
            fprintf (FILE, "\tfmovem.x (sp)+,%s",                     \
		     foffset + fsize,                                 \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
	    first = 0;						      \
           }                                                          \
          else if (first) {    				              \
            fprintf (FILE, "\tfmovem.x -%d(%s),%s",                   \
		     foffset + fsize,                                 \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
	    first = 0;						      \
           }                                                          \
	  else fprintf (FILE, "/%s", reg_names[regno]); 	      \
      fprintf (FILE, "\n");					      \
    }                                                                 \
  if (frame_pointer_needed)                                           \
    fprintf (FILE, "\tunlk %s\n",                                     \
	     reg_names[FRAME_POINTER_REGNUM]);                        \
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
  if (current_function_pops_args)                                     \
    fprintf (FILE, "\trtd #%d\n", current_function_pops_args);        \
  else                                                                \
    fprintf (FILE, "\trts\n");                                        \
}

/* Translate Motorola opcodes such as `jbeq'
   into VERSAdos opcodes such as `beq'.
   Change `fbeq' to `fbseq', `fbne' to `fbsneq'.
*/

#undef ASM_OUTPUT_OPCODE
#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{ if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR);						\
      while (*(PTR) != ' ')				\
	{ putc (*(PTR), (FILE)); ++(PTR); }		\
    }			                          	\
  else if ((PTR)[0] == 'f')                             \
    {                                                   \
      if (!strncmp ((PTR), "fbeq", 4))                  \
        { fprintf ((FILE), "fbseq"); (PTR) += 4; }      \
      else if (!strncmp ((PTR), "fbne", 4))             \
        { fprintf ((FILE), "fbsneq"); (PTR) += 4; }     \
    }                                                   \
  else if ((PTR)[0] == 'b' && (PTR)[1] == 'f')          \
    {                                                   \
      char *s;                                          \
      if ((s = (char*)strchr ((PTR), '{')))             \
	while (*s != '}') {                             \
	  if (*s == 'b')                                \
	    /* hack, I replace it with R ie nothing */  \
	    *s = '0';                                   \
	  s++; }					\
    }                                                   \
}

/* This is how to output a `long double' extended real constant. */
#undef ASM_OUTPUT_LONG_DOUBLE 
#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)  				\
do { long l[3];								\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);			\
     if (sizeof (int) == sizeof (long))					\
       fprintf (FILE, "\tdc.l $%x,$%x,$%x\n", l[0], l[1], l[2]);	\
     else								\
       fprintf (FILE, "\tdc.l $%lx,$%lx,$%lx\n", l[0], l[1], l[2]);	\
   } while (0)

#undef ASM_OUTPUT_DOUBLE
#if 0
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  do { char dstr[30];						\
       REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);		\
       fprintf (FILE, "\tdc.d %s\n", dstr);	        	\
     } while (0)
#endif
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
do { long l[2];								\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);			        \
     fprintf (FILE, "\tdc.l $%x,$%x\n", l[0], l[1]);            	\
   } while (0)


/* This is how to output an assembler line defining a `float' constant.  */
#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
do { long l;						\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
     if (sizeof (int) == sizeof (long))			\
       fprintf (FILE, "\tdc.l $%x\n", l);		\
     else						\
       fprintf (FILE, "\tdc.l $%lx\n", l);		\
   } while (0)

/* This is how to output an assembler line defining an `int' constant.  */
#undef ASM_OUTPUT_INT 
#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\tdc.l "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */
#undef ASM_OUTPUT_SHORT
#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\tdc.w "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#undef ASM_OUTPUT_CHAR
#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\tdc.b "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */
#undef ASM_OUTPUT_BYTE
#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\tdc.b $%x\n", (VALUE))

/* This is how to output an element of a case-vector that is absolute.
   (The 68000 does not use such vectors,
   but we must define this macro anyway.)  */
#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  asm_fprintf (FILE, "\tdc.l %LL%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */
#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  asm_fprintf (FILE, "\tdc.w %LL%d-%LL%d\n", VALUE, REL)

/* Currently, JUMP_TABLES_IN_TEXT_SECTION must be defined in order to
   keep switch tables in the text section. */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Output a float value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */
#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)		\
 do {								\
      if (CODE == 'f')						\
        {							\
          char dstr[30];					\
          REAL_VALUE_TO_DECIMAL (VALUE, "%.9g", dstr);		\
          asm_fprintf ((FILE), "%I%s", dstr);			\
        }							\
      else							\
        {							\
          long l;						\
          REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
          if (sizeof (int) == sizeof (long))			\
            asm_fprintf ((FILE), "%I$%x", l);			\
          else							\
            asm_fprintf ((FILE), "%I$%lx", l);			\
        }							\
     } while (0)

/* Output a double value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */
#undef ASM_OUTPUT_DOUBLE_OPERAND 
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
 do { char dstr[30];							\
      REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
      asm_fprintf (FILE, "%I%s", dstr);					\
    } while (0)

/* Note, long double immediate operands are not actually
   generated by m68k.md.  */
#undef ASM_OUTPUT_LONG_DOUBLE_OPERAND
#define ASM_OUTPUT_LONG_DOUBLE_OPERAND(FILE,VALUE)			\
 do { char dstr[30];							\
      REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
      asm_fprintf (FILE, "%I%s", dstr);					\
    } while (0)

#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\t.comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do {							\
    int align = exact_log2 (ROUNDED);			\
    /*fprintf ((FILE), "\tsection 14\n");  */               \
    data_section ();					\
    ASM_OUTPUT_ALIGN ((FILE), align)                    \
    ASM_OUTPUT_LABEL ((FILE), (NAME));			\
    fprintf ((FILE), "\tdcb.b %u,0\n", (ROUNDED));	\
    /* fprintf ((FILE), "\tsection 10\n"); */             \
  } while (0)

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
	    fprintf (FILE, "(.L%d,pc,%s.w",				\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (XEXP (ireg, 0))]); 		\
	  else								\
	    fprintf (FILE, "(.L%d,pc,%s.l",				\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (ireg)]);				\
	  if (scale != 1) fprintf (FILE, "*%d", scale);			\
	  putc (')', FILE);						\
	  break; }							\
	if (breg != 0 && ireg == 0 && GET_CODE (addr) == LABEL_REF      \
	    && ! (flag_pic && breg == pic_offset_table_rtx))            \
	  {                                                             \
	    fprintf (FILE, "(.L%d,pc,%s.l", 	                        \
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),            \
			 reg_names[REGNO (breg)]);                      \
	    putc (')', FILE);                                           \
	    break; }                                                    \
      if (ireg != 0 || breg != 0)					\
	{ int scale = 1;						\
	  if (breg == 0)						\
	    abort ();							\
          putc ('(', FILE); 	     				        \
	  if (addr != 0)						\
	    {                                                           \
	      output_addr_const (FILE, addr);				\
	      putc (',', FILE); 					\
	    }    							\
	  fprintf (FILE, "%s", reg_names[REGNO (breg)]);		\
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
	{ fprintf (FILE, "(.L%d,pc,%s.w)",				\
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


#endif /* ! use gas */			
