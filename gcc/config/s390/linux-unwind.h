/* DWARF2 EH unwinding support for S/390 Linux.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#define MD_FALLBACK_FRAME_STATE_FOR s390_fallback_frame_state

static _Unwind_Reason_Code
s390_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  long new_cfa;
  int i;

  typedef struct
  {
    unsigned long psw_mask;
    unsigned long psw_addr;
    unsigned long gprs[16];
    unsigned int  acrs[16];
    unsigned int  fpc;
    unsigned int  __pad;
    double        fprs[16];
  } __attribute__ ((__aligned__ (8))) sigregs_;

  sigregs_ *regs;
  int *signo = NULL;

  /* svc $__NR_sigreturn or svc $__NR_rt_sigreturn  */
  if (pc[0] != 0x0a || (pc[1] != 119 && pc[1] != 173))
    return _URC_END_OF_STACK;

  /* New-style RT frame:
     retcode + alignment (8 bytes)
     siginfo (128 bytes)
     ucontext (contains sigregs)  */
  if (context->ra == context->cfa)
    {
      struct ucontext_
      {
	unsigned long     uc_flags;
	struct ucontext_ *uc_link;
	unsigned long     uc_stack[3];
	sigregs_          uc_mcontext;
      } *uc = context->cfa + 8 + 128;

      regs = &uc->uc_mcontext;
      signo = context->cfa + sizeof(long);
    }

  /* Old-style RT frame and all non-RT frames:
     old signal mask (8 bytes)
     pointer to sigregs  */
  else
    {
      regs = *(sigregs_ **)(context->cfa + 8);

      /* Recent kernels store the signal number immediately after
	 the sigregs; old kernels have the return trampoline at
	 this location.  */
      if ((void *)(regs + 1) != context->ra)
	signo = (int *)(regs + 1);
    }

  new_cfa = regs->gprs[15] + 16*sizeof(long) + 32;
  fs->cfa_how = CFA_REG_OFFSET;
  fs->cfa_reg = 15;
  fs->cfa_offset =
    new_cfa - (long) context->cfa + 16*sizeof(long) + 32;

  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset =
	(long)&regs->gprs[i] - new_cfa;
    }
  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[16+i].how = REG_SAVED_OFFSET;
      fs->regs.reg[16+i].loc.offset =
	(long)&regs->fprs[i] - new_cfa;
    }

  /* Load return addr from PSW into dummy register 32.  */

  fs->regs.reg[32].how = REG_SAVED_OFFSET;
  fs->regs.reg[32].loc.offset = (long)&regs->psw_addr - new_cfa;
  fs->retaddr_column = 32;

  /* If we got a SIGSEGV or a SIGBUS, the PSW address points *to*
     the faulting instruction, not after it.  This causes the logic
     in unwind-dw2.c that decrements the RA to determine the correct
     CFI region to get confused.  To fix that, we *increment* the RA
     here in that case.  Note that we cannot modify the RA in place,
     and the frame state wants a *pointer*, not a value; thus we put
     the modified RA value into the unused register 33 slot of FS and
     have the register 32 save address point to that slot.

     Unfortunately, for regular signals on old kernels, we don't know
     the signal number.  We default to not fiddling with the RA;
     that can fail in rare cases.  Upgrade your kernel.  */

  if (signo && (*signo == 11 || *signo == 7))
    {
      fs->regs.reg[33].loc.exp =
	(unsigned char *)regs->psw_addr + 1;
      fs->regs.reg[32].loc.offset =
	(long)&fs->regs.reg[33].loc.exp - new_cfa;
    }

  return _URC_NO_REASON;
}
