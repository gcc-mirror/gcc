/* DWARF2 EH unwinding support for MIPS Linux.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef inhibit_libc
/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#include <signal.h>

/* The third parameter to the signal handler points to something with
 * this structure defined in asm/ucontext.h, but the name clashes with
 * struct ucontext from sys/ucontext.h so this private copy is used.  */
typedef struct _sig_ucontext {
    unsigned long         uc_flags;
    struct _sig_ucontext  *uc_link;
    stack_t               uc_stack;
    struct sigcontext uc_mcontext;
    sigset_t      uc_sigmask;
} _sig_ucontext_t;

#define MD_FALLBACK_FRAME_STATE_FOR mips_fallback_frame_state

static _Unwind_Reason_Code
mips_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  u_int32_t *pc = (u_int32_t *) context->ra;
  struct sigcontext *sc;
  _Unwind_Ptr new_cfa;
  int i;

  /* 24021061 li v0, 0x1061 (rt_sigreturn)*/
  /* 0000000c syscall    */
  /*    or */
  /* 24021017 li v0, 0x1017 (sigreturn) */
  /* 0000000c syscall  */
  if (*(pc + 1) != 0x0000000c)
    return _URC_END_OF_STACK;
  if (*(pc + 0) == 0x24021017)
    {
      struct sigframe {
	u_int32_t  trampoline[2];
	struct sigcontext sigctx;
      } *rt_ = context->ra;
      sc = &rt_->sigctx;
    }
  else if (*(pc + 0) == 0x24021061)
    {
      struct rt_sigframe {
	u_int32_t  trampoline[2];
	struct siginfo info;
	_sig_ucontext_t uc;
      } *rt_ = context->ra;
      sc = &rt_->uc.uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = (_Unwind_Ptr)sc;
  fs->cfa_how = CFA_REG_OFFSET;
  fs->cfa_reg = STACK_POINTER_REGNUM;
  fs->cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  for (i = 0; i < 32; i++) {
    fs->regs.reg[i].how = REG_SAVED_OFFSET;
    fs->regs.reg[i].loc.offset
      = (_Unwind_Ptr)&(sc->sc_regs[i]) - new_cfa;
  }
  fs->regs.reg[SIGNAL_UNWIND_RETURN_COLUMN].how = REG_SAVED_OFFSET;
  fs->regs.reg[SIGNAL_UNWIND_RETURN_COLUMN].loc.offset
    = (_Unwind_Ptr)&(sc->sc_pc) - new_cfa;
  fs->retaddr_column = SIGNAL_UNWIND_RETURN_COLUMN;

  return _URC_NO_REASON;
}
#endif
