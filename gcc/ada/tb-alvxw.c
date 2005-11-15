/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                T R A C E B A C K - A l p h a / V x W o r k s             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                     Copyright (C) 2000-2005, AdaCore                     *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* Alpha vxWorks requires a special, complex treatment that is extracted
   from GDB. This file is #included within tracebak.c in the appropriate
   case.  */

#include <stddef.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

extern void kerTaskEntry(void);

/* We still use a number of macros similar to the ones for the generic
   __gnat_backtrace implementation.  */
#define SKIP_FRAME 1
#define PC_ADJUST -4

#define STOP_FRAME \
   (current == NULL \
    || ((CORE_ADDR) &kerTaskEntry >= PROC_LOW_ADDR (current->proc_desc) \
        && current->pc >= (CORE_ADDR) &kerTaskEntry))

/* Register numbers of various important registers.
   Note that most of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and FP_REGNUM is a "phony" register number which is too large
   to be an actual register number as far as the user is concerned
   but serves to get the desired value when passed to read_register.  */

#define T7_REGNUM 8		/* Return address register for OSF/1 __add* */
#define GCC_FP_REGNUM 15	/* Used by gcc as frame register */
#define T9_REGNUM 23		/* Return address register for OSF/1 __div* */
#define SP_REGNUM 30		/* Contains address of top of stack */
#define RA_REGNUM 26		/* Contains return address value */
#define FP0_REGNUM 32		/* Floating point register 0 */
#define PC_REGNUM 64		/* Contains program counter */
#define NUM_REGS 66

#define VM_MIN_ADDRESS (CORE_ADDR)0x120000000

#define SIZEOF_FRAME_SAVED_REGS (sizeof (CORE_ADDR) * (NUM_REGS))
#define INIT_EXTRA_FRAME_INFO(fromleaf, fci) init_extra_frame_info(fci)

#define FRAME_CHAIN(thisframe) (CORE_ADDR) alpha_frame_chain (thisframe)

#define FRAME_CHAIN_VALID(CHAIN, THISFRAME)	\
  ((CHAIN) != 0					\
   && !inside_entry_file (FRAME_SAVED_PC (THISFRAME)))

#define FRAME_SAVED_PC(FRAME)	(alpha_frame_saved_pc (FRAME))

#define	FRAME_CHAIN_COMBINE(CHAIN, THISFRAME) (CHAIN)

#define	INIT_FRAME_PC(FROMLEAF, PREV)

#define INIT_FRAME_PC_FIRST(FROMLEAF, PREV) \
  (PREV)->pc = ((FROMLEAF) ? SAVED_PC_AFTER_CALL ((PREV)->next) \
		: (PREV)->next ? FRAME_SAVED_PC ((PREV)->next) : read_pc ());

#define SAVED_PC_AFTER_CALL(FRAME)	alpha_saved_pc_after_call (FRAME)

typedef unsigned long long int bfd_vma;

typedef bfd_vma CORE_ADDR;

typedef struct pdr
{
  bfd_vma adr;		/* memory address of start of procedure */
  long	isym;		/* start of local symbol entries */
  long	iline;		/* start of line number entries*/
  long	regmask;	/* save register mask */
  long	regoffset;	/* save register offset */
  long	iopt;		/* start of optimization symbol entries*/
  long	fregmask;	/* save floating point register mask */
  long	fregoffset;	/* save floating point register offset */
  long	frameoffset;	/* frame size */
  short	framereg;	/* frame pointer register */
  short	pcreg;		/* offset or reg of return pc */
  long	lnLow;		/* lowest line in the procedure */
  long	lnHigh;		/* highest line in the procedure */
  bfd_vma cbLineOffset;	/* byte offset for this procedure from the fd base */
  /* These fields are new for 64 bit ECOFF.  */
  unsigned gp_prologue : 8; /* byte size of GP prologue */
  unsigned gp_used : 1;	/* true if the procedure uses GP */
  unsigned reg_frame : 1; /* true if register frame procedure */
  unsigned prof : 1;	/* true if compiled with -pg */
  unsigned reserved : 13; /* reserved: must be zero */
  unsigned localoff : 8; /* offset of local variables from vfp */
} PDR;

typedef struct alpha_extra_func_info
{
  long numargs;		/* number of args to procedure (was iopt) */
  PDR pdr;			/* Procedure descriptor record */
}
*alpha_extra_func_info_t;

struct frame_info
{
  /* Nominal address of the frame described.  See comments at FRAME_FP
     about what this means outside the *FRAME* macros; in the *FRAME*
     macros, it can mean whatever makes most sense for this machine.  */
  CORE_ADDR frame;

  /* Address at which execution is occurring in this frame.  For the
     innermost frame, it's the current pc.  For other frames, it is a
     pc saved in the next frame.  */
  CORE_ADDR pc;

  /* For each register, address of where it was saved on entry to the
     frame, or zero if it was not saved on entry to this frame.  This
     includes special registers such as pc and fp saved in special
     ways in the stack frame.  The SP_REGNUM is even more special, the
     address here is the sp for the next frame, not the address where
     the sp was saved.  Allocated by frame_saved_regs_zalloc () which
     is called and initialized by FRAME_INIT_SAVED_REGS. */
  CORE_ADDR *saved_regs;	/*NUM_REGS */

  int localoff;
  int pc_reg;
  alpha_extra_func_info_t proc_desc;

  /* Pointers to the next and previous frame_info's in the frame cache.  */
  struct frame_info *next, *prev;
};

struct frame_saved_regs
{
  /* For each register R (except the SP), regs[R] is the address at
     which it was saved on entry to the frame, or zero if it was not
     saved on entry to this frame.  This includes special registers
     such as pc and fp saved in special ways in the stack frame.

     regs[SP_REGNUM] is different.  It holds the actual SP, not the
     address at which it was saved.  */

  CORE_ADDR regs[NUM_REGS];
};

static CORE_ADDR theRegisters[32];

/* Prototypes for local functions. */

static CORE_ADDR read_next_frame_reg (struct frame_info *, int);
static CORE_ADDR heuristic_proc_start (CORE_ADDR);
static int alpha_about_to_return (CORE_ADDR pc);
static void init_extra_frame_info (struct frame_info *);
static CORE_ADDR alpha_frame_chain (struct frame_info *);
static CORE_ADDR alpha_frame_saved_pc (struct frame_info *frame);
static void *trace_alloc (unsigned int);
static struct frame_info *create_new_frame (CORE_ADDR, CORE_ADDR);

static alpha_extra_func_info_t
heuristic_proc_desc (CORE_ADDR, CORE_ADDR, struct frame_info *,
		     struct frame_saved_regs *);

static alpha_extra_func_info_t
find_proc_desc (CORE_ADDR, struct frame_info *, struct frame_saved_regs *);

/* Heuristic_proc_start may hunt through the text section for a long
   time across a 2400 baud serial line.  Allows the user to limit this
   search.  */
static unsigned int heuristic_fence_post = 1<<16;

/* Layout of a stack frame on the alpha:

                |				|
 pdr members:	|  7th ... nth arg,		|
                |  `pushed' by caller.		|
                |				|
----------------|-------------------------------|<--  old_sp == vfp
   ^  ^  ^  ^	|				|
   |  |  |  |	|				|
   |  |localoff	|  Copies of 1st .. 6th		|
   |  |  |  |	|  argument if necessary.	|
   |  |  |  v	|				|
   |  |  |  ---	|-------------------------------|<-- FRAME_LOCALS_ADDRESS
   |  |  |      |				|
   |  |  |      |  Locals and temporaries.	|
   |  |  |      |				|
   |  |  |      |-------------------------------|
   |  |  |      |				|
   |-fregoffset	|  Saved float registers.	|
   |  |  |      |  F9				|
   |  |  |      |   .				|
   |  |  |      |   .				|
   |  |  |      |  F2				|
   |  |  v      |				|
   |  |  -------|-------------------------------|
   |  |         |				|
   |  |         |  Saved registers.		|
   |  |         |  S6				|
   |-regoffset	|   .				|
   |  |         |   .				|
   |  |         |  S0				|
   |  |         |  pdr.pcreg			|
   |  v         |				|
   |  ----------|-------------------------------|
   |            |				|
 frameoffset    |  Argument build area, gets	|
   |            |  7th ... nth arg for any	|
   |            |  called procedure.		|
   v            |  				|
   -------------|-------------------------------|<-- sp
                |				|            */

#define PROC_LOW_ADDR(PROC) ((PROC)->pdr.adr)		    /* least address */
#define PROC_HIGH_ADDR(PROC) ((PROC)->pdr.iline)      /* upper address bound */
#define PROC_DUMMY_FRAME(PROC) ((PROC)->pdr.cbLineOffset) /*CALL_DUMMY frame */
#define PROC_FRAME_OFFSET(PROC) ((PROC)->pdr.frameoffset)
#define PROC_FRAME_REG(PROC) ((PROC)->pdr.framereg)
#define PROC_REG_MASK(PROC) ((PROC)->pdr.regmask)
#define PROC_FREG_MASK(PROC) ((PROC)->pdr.fregmask)
#define PROC_REG_OFFSET(PROC) ((PROC)->pdr.regoffset)
#define PROC_FREG_OFFSET(PROC) ((PROC)->pdr.fregoffset)
#define PROC_PC_REG(PROC) ((PROC)->pdr.pcreg)
#define PROC_LOCALOFF(PROC) ((PROC)->pdr.localoff)

/* Local storage allocation/deallocation functions.  trace_alloc does
   a malloc, but also chains allocated blocks on trace_alloc_chain, so
   they may all be freed on exit from __gnat_backtrace. */

struct alloc_chain
{
  struct alloc_chain *next;
  double x[0];
};
struct alloc_chain *trace_alloc_chain;

static void *
trace_alloc (unsigned int n)
{
  struct alloc_chain * result = malloc (n + sizeof(struct alloc_chain));

  result->next = trace_alloc_chain;
  trace_alloc_chain = result;
  return (void*) result->x;
}

static void
free_trace_alloc (void)
{
  while (trace_alloc_chain != 0)
    {
      struct alloc_chain *old = trace_alloc_chain;

      trace_alloc_chain = trace_alloc_chain->next;
      free (old);
    }
}

/* Read value at ADDR into *DEST, returning 0 if this is valid, != 0
   otherwise. */

static int
read_memory_safe4 (CORE_ADDR addr, unsigned int *dest)
{
  *dest = *((unsigned int*) addr);
  return 0;
}

/* Read value at ADDR into *DEST, returning 0 if this is valid, != 0
   otherwise. */

static int
read_memory_safe8 (CORE_ADDR addr, CORE_ADDR *dest)
{
  *dest = *((CORE_ADDR*) addr);
  return 0;
}

static CORE_ADDR
read_register (int regno)
{
  if (regno >= 0 && regno < 31)
    return theRegisters[regno];

  return (CORE_ADDR) 0;
}

static void
frame_saved_regs_zalloc (struct frame_info *fi)
{
  fi->saved_regs = (CORE_ADDR *) trace_alloc (SIZEOF_FRAME_SAVED_REGS);
  memset (fi->saved_regs, 0, SIZEOF_FRAME_SAVED_REGS);
}

static void *
frame_obstack_alloc (unsigned long size)
{
  return (void *) trace_alloc (size);
}

static int
inside_entry_file (CORE_ADDR addr)
{
  if (addr == 0)
    return 1;
  else
    return 0;
}

static CORE_ADDR
alpha_saved_pc_after_call (struct frame_info *frame)
{
  CORE_ADDR pc = frame->pc;
  alpha_extra_func_info_t proc_desc;
  int pcreg;

  proc_desc = find_proc_desc (pc, frame->next, NULL);
  pcreg = proc_desc ? PROC_PC_REG (proc_desc) : RA_REGNUM;

  return read_register (pcreg);
}

/* Guaranteed to set frame->saved_regs to some values (it never leaves it
   NULL).  */

static void
alpha_find_saved_regs (struct frame_info *frame)
{
  int ireg;
  CORE_ADDR reg_position;
  unsigned long mask;
  alpha_extra_func_info_t proc_desc;
  int returnreg;

  frame_saved_regs_zalloc (frame);

  /* If it is the frame for __sigtramp, the saved registers are located in a
     sigcontext structure somewhere on the stack. __sigtramp passes a pointer
     to the sigcontext structure on the stack.  If the stack layout for
     __sigtramp changes, or if sigcontext offsets change, we might have to
     update this code.  */

#ifndef SIGFRAME_PC_OFF
#define SIGFRAME_PC_OFF		(2 * 8)
#define SIGFRAME_REGSAVE_OFF	(4 * 8)
#define SIGFRAME_FPREGSAVE_OFF	(SIGFRAME_REGSAVE_OFF + 32 * 8 + 8)
#endif

  proc_desc = frame->proc_desc;
  if (proc_desc == NULL)
    /* I'm not sure how/whether this can happen.  Normally when we can't
       find a proc_desc, we "synthesize" one using heuristic_proc_desc
       and set the saved_regs right away.  */
    return;

  /* Fill in the offsets for the registers which gen_mask says
     were saved.  */

  reg_position = frame->frame + PROC_REG_OFFSET (proc_desc);
  mask = PROC_REG_MASK (proc_desc);

  returnreg = PROC_PC_REG (proc_desc);

  /* Note that RA is always saved first, regardless of its actual
     register number.  */
  if (mask & (1 << returnreg))
    {
      frame->saved_regs[returnreg] = reg_position;
      reg_position += 8;
      mask &= ~(1 << returnreg);	/* Clear bit for RA so we
					   don't save again later. */
    }

  for (ireg = 0; ireg <= 31; ireg++)
    if (mask & (1 << ireg))
      {
	frame->saved_regs[ireg] = reg_position;
	reg_position += 8;
      }

  /* Fill in the offsets for the registers which float_mask says
     were saved.  */

  reg_position = frame->frame + PROC_FREG_OFFSET (proc_desc);
  mask = PROC_FREG_MASK (proc_desc);

  for (ireg = 0; ireg <= 31; ireg++)
    if (mask & (1 << ireg))
      {
	frame->saved_regs[FP0_REGNUM + ireg] = reg_position;
	reg_position += 8;
      }

  frame->saved_regs[PC_REGNUM] = frame->saved_regs[returnreg];
}

static CORE_ADDR
read_next_frame_reg (struct frame_info *fi, int regno)
{
  CORE_ADDR result;
  for (; fi; fi = fi->next)
    {
      /* We have to get the saved sp from the sigcontext
         if it is a signal handler frame.  */
      if (regno == SP_REGNUM)
	return fi->frame;
      else
	{
	  if (fi->saved_regs == 0)
	    alpha_find_saved_regs (fi);

	  if (fi->saved_regs[regno])
	    {
	      if (read_memory_safe8 (fi->saved_regs[regno], &result) == 0)
		return result;
	      else
		return 0;
	    }
	}
    }

  return read_register (regno);
}

static CORE_ADDR
alpha_frame_saved_pc (struct frame_info *frame)
{
  return read_next_frame_reg (frame, frame->pc_reg);
}

static struct alpha_extra_func_info temp_proc_desc;

/* Nonzero if instruction at PC is a return instruction.  "ret
   $zero,($ra),1" on alpha. */

static int
alpha_about_to_return (CORE_ADDR pc)
{
  int inst;

  read_memory_safe4 (pc, &inst);
  return inst == 0x6bfa8001;
}

/* A heuristically computed start address for the subprogram
   containing address PC.   Returns 0 if none detected. */

static CORE_ADDR
heuristic_proc_start (CORE_ADDR pc)
{
  CORE_ADDR start_pc = pc;
  CORE_ADDR fence = start_pc - heuristic_fence_post;

  if (start_pc == 0)
    return 0;

  if (heuristic_fence_post == UINT_MAX
      || fence < VM_MIN_ADDRESS)
    fence = VM_MIN_ADDRESS;

  /* search back for previous return */
  for (start_pc -= 4; ; start_pc -= 4)
    {
      if (start_pc < fence)
	return 0;
      else if (alpha_about_to_return (start_pc))
	break;
    }

  start_pc += 4;		/* skip return */
  return start_pc;
}

static alpha_extra_func_info_t
heuristic_proc_desc (CORE_ADDR start_pc,
                     CORE_ADDR limit_pc,
                     struct frame_info *next_frame,
                     struct frame_saved_regs *saved_regs_p)
{
  CORE_ADDR sp = read_next_frame_reg (next_frame, SP_REGNUM);
  CORE_ADDR cur_pc;
  int frame_size;
  int has_frame_reg = 0;
  unsigned long reg_mask = 0;
  int pcreg = -1;

  if (start_pc == 0)
    return 0;

  memset (&temp_proc_desc, '\0', sizeof (temp_proc_desc));
  if (saved_regs_p != 0)
    memset (saved_regs_p, '\0', sizeof (struct frame_saved_regs));

  PROC_LOW_ADDR (&temp_proc_desc) = start_pc;

  if (start_pc + 200 < limit_pc)
    limit_pc = start_pc + 200;

  frame_size = 0;
  for (cur_pc = start_pc; cur_pc < limit_pc; cur_pc += 4)
    {
      unsigned int word;
      int status;

      status = read_memory_safe4 (cur_pc, &word);
      if (status)
	return 0;

      if ((word & 0xffff0000) == 0x23de0000)	/* lda $sp,n($sp) */
	{
	  if (word & 0x8000)
	    frame_size += (-word) & 0xffff;
	  else
	    /* Exit loop if a positive stack adjustment is found, which
	       usually means that the stack cleanup code in the function
	       epilogue is reached.  */
	    break;
	}
      else if ((word & 0xfc1f0000) == 0xb41e0000	/* stq reg,n($sp) */
	       && (word & 0xffff0000) != 0xb7fe0000)	/* reg != $zero */
	{
	  int reg = (word & 0x03e00000) >> 21;

	  reg_mask |= 1 << reg;
	  if (saved_regs_p != 0)
	    saved_regs_p->regs[reg] = sp + (short) word;

	  /* Starting with OSF/1-3.2C, the system libraries are shipped
	     without local symbols, but they still contain procedure
	     descriptors without a symbol reference. GDB is currently
	     unable to find these procedure descriptors and uses
	     heuristic_proc_desc instead.
	     As some low level compiler support routines (__div*, __add*)
	     use a non-standard return address register, we have to
	     add some heuristics to determine the return address register,
	     or stepping over these routines will fail.
	     Usually the return address register is the first register
	     saved on the stack, but assembler optimization might
	     rearrange the register saves.
	     So we recognize only a few registers (t7, t9, ra) within
	     the procedure prologue as valid return address registers.
	     If we encounter a return instruction, we extract the
	     the return address register from it.

	     FIXME: Rewriting GDB to access the procedure descriptors,
	     e.g. via the minimal symbol table, might obviate this hack.  */
	  if (pcreg == -1
	      && cur_pc < (start_pc + 80)
	      && (reg == T7_REGNUM || reg == T9_REGNUM || reg == RA_REGNUM))
	    pcreg = reg;
	}
      else if ((word & 0xffe0ffff) == 0x6be08001)	/* ret zero,reg,1 */
	pcreg = (word >> 16) & 0x1f;
      else if (word == 0x47de040f)	/* bis sp,sp fp */
	has_frame_reg = 1;
    }

  if (pcreg == -1)
    {
      /* If we haven't found a valid return address register yet,
         keep searching in the procedure prologue.  */
      while (cur_pc < (limit_pc + 80) && cur_pc < (start_pc + 80))
	{
	  unsigned int word;

	  if (read_memory_safe4 (cur_pc, &word))
	    break;
	  cur_pc += 4;

	  if ((word & 0xfc1f0000) == 0xb41e0000		/* stq reg,n($sp) */
	      && (word & 0xffff0000) != 0xb7fe0000)	/* reg != $zero */
	    {
	      int reg = (word & 0x03e00000) >> 21;

	      if (reg == T7_REGNUM || reg == T9_REGNUM || reg == RA_REGNUM)
		{
		  pcreg = reg;
		  break;
		}
	    }
	  else if ((word & 0xffe0ffff) == 0x6be08001)	/* ret zero,reg,1 */
	    {
	      pcreg = (word >> 16) & 0x1f;
	      break;
	    }
	}
    }

  if (has_frame_reg)
    PROC_FRAME_REG (&temp_proc_desc) = GCC_FP_REGNUM;
  else
    PROC_FRAME_REG (&temp_proc_desc) = SP_REGNUM;

  PROC_FRAME_OFFSET (&temp_proc_desc) = frame_size;
  PROC_REG_MASK (&temp_proc_desc) = reg_mask;
  PROC_PC_REG (&temp_proc_desc) = (pcreg == -1) ? RA_REGNUM : pcreg;
  PROC_LOCALOFF (&temp_proc_desc) = 0;	/* XXX - bogus */

  return &temp_proc_desc;
}

static alpha_extra_func_info_t
find_proc_desc (CORE_ADDR pc,
                struct frame_info *next_frame,
                struct frame_saved_regs *saved_regs)
{
  CORE_ADDR startaddr;

  /* If heuristic_fence_post is non-zero, determine the procedure
     start address by examining the instructions.
     This allows us to find the start address of static functions which
     have no symbolic information, as startaddr would have been set to
     the preceding global function start address by the
     find_pc_partial_function call above.  */
  startaddr = heuristic_proc_start (pc);

  return heuristic_proc_desc (startaddr, pc, next_frame, saved_regs);
}

static CORE_ADDR
alpha_frame_chain (struct frame_info *frame)
{
  alpha_extra_func_info_t proc_desc;
  CORE_ADDR saved_pc = FRAME_SAVED_PC (frame);

  if (saved_pc == 0 || inside_entry_file (saved_pc))
    return 0;

  proc_desc = find_proc_desc (saved_pc, frame, NULL);
  if (!proc_desc)
    return 0;

  /* If no frame pointer and frame size is zero, we must be at end
     of stack (or otherwise hosed).  If we don't check frame size,
     we loop forever if we see a zero size frame.  */
  if (PROC_FRAME_REG (proc_desc) == SP_REGNUM
      && PROC_FRAME_OFFSET (proc_desc) == 0)
    return 0;
  else
    return read_next_frame_reg (frame, PROC_FRAME_REG (proc_desc))
      + PROC_FRAME_OFFSET (proc_desc);
}

static void
init_extra_frame_info (struct frame_info *frame)
{
  struct frame_saved_regs temp_saved_regs;
  alpha_extra_func_info_t proc_desc =
    find_proc_desc (frame->pc, frame->next, &temp_saved_regs);

  frame->saved_regs = NULL;
  frame->localoff = 0;
  frame->pc_reg = RA_REGNUM;
  frame->proc_desc = proc_desc;

  if (proc_desc)
    {
      /* Get the locals offset and the saved pc register from the
         procedure descriptor, they are valid even if we are in the
         middle of the prologue.  */
      frame->localoff = PROC_LOCALOFF (proc_desc);
      frame->pc_reg = PROC_PC_REG (proc_desc);

      /* Fixup frame-pointer - only needed for top frame */

      /* This may not be quite right, if proc has a real frame register.
         Get the value of the frame relative sp, procedure might have been
         interrupted by a signal at it's very start.  */
      if (frame->pc == PROC_LOW_ADDR (proc_desc))
	frame->frame = read_next_frame_reg (frame->next, SP_REGNUM);
      else
	frame->frame
	  = (read_next_frame_reg (frame->next, PROC_FRAME_REG (proc_desc))
	     + PROC_FRAME_OFFSET (proc_desc));

      frame->saved_regs
	= (CORE_ADDR *) frame_obstack_alloc (SIZEOF_FRAME_SAVED_REGS);
      memcpy
        (frame->saved_regs, temp_saved_regs.regs, SIZEOF_FRAME_SAVED_REGS);
      frame->saved_regs[PC_REGNUM] = frame->saved_regs[RA_REGNUM];
    }
}

/* Create an arbitrary (i.e. address specified by user) or innermost frame.
   Always returns a non-NULL value.  */

static struct frame_info *
create_new_frame (CORE_ADDR addr, CORE_ADDR pc)
{
  struct frame_info *fi;

  fi = (struct frame_info *)
    trace_alloc (sizeof (struct frame_info));

  /* Arbitrary frame */
  fi->next = NULL;
  fi->prev = NULL;
  fi->frame = addr;
  fi->pc = pc;

#ifdef INIT_EXTRA_FRAME_INFO
  INIT_EXTRA_FRAME_INFO (0, fi);
#endif

  return fi;
}

static CORE_ADDR current_pc;

static void
set_current_pc (void)
{
  current_pc = (CORE_ADDR) __builtin_return_address (0);
}

static CORE_ADDR
read_pc (void)
{
  return current_pc;
}

static struct frame_info *
get_current_frame (void)
{
  return create_new_frame (0, read_pc ());
}

/* Return the frame that called FI.
   If FI is the original frame (it has no caller), return 0.  */

static struct frame_info *
get_prev_frame (struct frame_info *next_frame)
{
  CORE_ADDR address = 0;
  struct frame_info *prev;
  int fromleaf = 0;

  /* If we have the prev one, return it */
  if (next_frame->prev)
    return next_frame->prev;

  /* On some machines it is possible to call a function without
     setting up a stack frame for it.  On these machines, we
     define this macro to take two args; a frameinfo pointer
     identifying a frame and a variable to set or clear if it is
     or isn't leafless.  */

  /* Two macros defined in tm.h specify the machine-dependent
     actions to be performed here.

     First, get the frame's chain-pointer.  If that is zero, the frame
     is the outermost frame or a leaf called by the outermost frame.
     This means that if start calls main without a frame, we'll return
     0 (which is fine anyway).

     Nope; there's a problem.  This also returns when the current
     routine is a leaf of main.  This is unacceptable.  We move
     this to after the ffi test; I'd rather have backtraces from
     start go curfluy than have an abort called from main not show
     main.  */

  address = FRAME_CHAIN (next_frame);
  if (!FRAME_CHAIN_VALID (address, next_frame))
    return 0;
  address = FRAME_CHAIN_COMBINE (address, next_frame);

  if (address == 0)
    return 0;

  prev = (struct frame_info *) trace_alloc (sizeof (struct frame_info));

  prev->saved_regs = NULL;
  if (next_frame)
    next_frame->prev = prev;

  prev->next = next_frame;
  prev->prev = (struct frame_info *) 0;
  prev->frame = address;

  /* This change should not be needed, FIXME!  We should
     determine whether any targets *need* INIT_FRAME_PC to happen
     after INIT_EXTRA_FRAME_INFO and come up with a simple way to
     express what goes on here.

     INIT_EXTRA_FRAME_INFO is called from two places: create_new_frame
     (where the PC is already set up) and here (where it isn't).
     INIT_FRAME_PC is only called from here, always after
     INIT_EXTRA_FRAME_INFO.

     The catch is the MIPS, where INIT_EXTRA_FRAME_INFO requires the PC
     value (which hasn't been set yet).  Some other machines appear to
     require INIT_EXTRA_FRAME_INFO before they can do INIT_FRAME_PC.  Phoo.

     We shouldn't need INIT_FRAME_PC_FIRST to add more complication to
     an already overcomplicated part of GDB.   gnu@cygnus.com, 15Sep92.

     Assuming that some machines need INIT_FRAME_PC after
     INIT_EXTRA_FRAME_INFO, one possible scheme:

     SETUP_INNERMOST_FRAME()
     Default version is just create_new_frame (read_fp ()),
     read_pc ()).  Machines with extra frame info would do that (or the
     local equivalent) and then set the extra fields.
     INIT_PREV_FRAME(fromleaf, prev)
     Replace INIT_EXTRA_FRAME_INFO and INIT_FRAME_PC.  This should
     also return a flag saying whether to keep the new frame, or
     whether to discard it, because on some machines (e.g.  mips) it
     is really awkward to have FRAME_CHAIN_VALID called *before*
     INIT_EXTRA_FRAME_INFO (there is no good way to get information
     deduced in FRAME_CHAIN_VALID into the extra fields of the new frame).
     std_frame_pc(fromleaf, prev)
     This is the default setting for INIT_PREV_FRAME.  It just does what
     the default INIT_FRAME_PC does.  Some machines will call it from
     INIT_PREV_FRAME (either at the beginning, the end, or in the middle).
     Some machines won't use it.
     kingdon@cygnus.com, 13Apr93, 31Jan94, 14Dec94.  */

#ifdef INIT_FRAME_PC_FIRST
  INIT_FRAME_PC_FIRST (fromleaf, prev);
#endif

#ifdef INIT_EXTRA_FRAME_INFO
  INIT_EXTRA_FRAME_INFO (fromleaf, prev);
#endif

  /* This entry is in the frame queue now, which is good since
     FRAME_SAVED_PC may use that queue to figure out its value
     (see tm-sparc.h).  We want the pc saved in the inferior frame. */
  INIT_FRAME_PC (fromleaf, prev);

  /* If ->frame and ->pc are unchanged, we are in the process of getting
     ourselves into an infinite backtrace.  Some architectures check this
     in FRAME_CHAIN or thereabouts, but it seems like there is no reason
     this can't be an architecture-independent check.  */
  if (next_frame != NULL)
    {
      if (prev->frame == next_frame->frame
	  && prev->pc == next_frame->pc)
	{
	  next_frame->prev = NULL;
	  free (prev);
	  return NULL;
	}
    }

  return prev;
}

#define SAVE(regno,disp) \
    "stq $" #regno ", " #disp "(%0)\n"

int
__gnat_backtrace (void **array,
                  int size,
                  void *exclude_min,
                  void *exclude_max,
                  int skip_frames)
{
  struct frame_info* top;
  struct frame_info* current;
  int cnt;

  /* This function is not thread safe, protect it */
  (*Lock_Task) ();
  asm volatile (
      SAVE (9,72)
      SAVE (10,80)
      SAVE (11,88)
      SAVE (12,96)
      SAVE (13,104)
      SAVE (14,112)
      SAVE (15,120)
      SAVE (16,128)
      SAVE (17,136)
      SAVE (18,144)
      SAVE (19,152)
      SAVE (20,160)
      SAVE (21,168)
      SAVE (22,176)
      SAVE (23,184)
      SAVE (24,192)
      SAVE (25,200)
      SAVE (26,208)
      SAVE (27,216)
      SAVE (28,224)
      SAVE (29,232)
      SAVE (30,240)
      : : "r" (&theRegisters));

  trace_alloc_chain = NULL;
  set_current_pc ();

  top = current = get_current_frame ();
  cnt = 0;

  for (cnt = 0; cnt < skip_frames; cnt += 1) {
    current = get_prev_frame (current);
  }

  cnt = 0;
  while (cnt < size)
    {
      if (STOP_FRAME)
        break;

      if (current->pc < (CORE_ADDR) exclude_min
	  || current->pc > (CORE_ADDR) exclude_max)
        array[cnt++] = (void*) (current->pc + PC_ADJUST);

      current = get_prev_frame (current);
    }

  free_trace_alloc ();
  (*Unlock_Task) ();

  return cnt;
}
