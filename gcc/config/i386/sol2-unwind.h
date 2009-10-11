/* DWARF2 EH unwinding support for AMD x86-64 and x86.
   Copyright (C) 2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#include <ucontext.h>

#ifdef __x86_64__

#define MD_FALLBACK_FRAME_STATE_FOR x86_64_fallback_frame_state

static _Unwind_Reason_Code
x86_64_fallback_frame_state (struct _Unwind_Context *context,
			     _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  mcontext_t *mctx;
  long new_cfa;

  if (/* Solaris 2.10
	------------
	<__sighndlr+0>:      push   %rbp
	<__sighndlr+1>:      mov    %rsp,%rbp
	<__sighndlr+4>:      callq  *%rcx
	<__sighndlr+6>:      leaveq           <--- PC
	<__sighndlr+7>:      retq  */
      *(unsigned long *)(pc - 6) == 0xc3c9d1ffe5894855)
    /* We need to move up four frames (the kernel frame, the signal frame,
       the call_user_handler frame and the __sighndlr frame).  Two of them
       have the minimum stack frame size (kernel and __sighndlr frames),
       the signal frame has a stack frame size of 32 and there is another
       with a stack frame size of 112 bytes (the call_user_handler frame).
       The ucontext_t structure is after this offset.  */
    {
      int off = 16 + 16 + 32 + 112;
      mctx = &((ucontext_t *) (context->cfa + off))->uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = mctx->gregs[REG_RSP];

  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 7;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&mctx->gregs[REG_RAX] - new_cfa;
  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&mctx->gregs[REG_RDX] - new_cfa;
  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&mctx->gregs[REG_RCX] - new_cfa;
  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&mctx->gregs[REG_RBX] - new_cfa;
  fs->regs.reg[4].how = REG_SAVED_OFFSET;
  fs->regs.reg[4].loc.offset = (long)&mctx->gregs[REG_RSI] - new_cfa;
  fs->regs.reg[5].how = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&mctx->gregs[REG_RDI] - new_cfa;
  fs->regs.reg[6].how = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&mctx->gregs[REG_RBP] - new_cfa;
  fs->regs.reg[8].how = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&mctx->gregs[REG_R8] - new_cfa;
  fs->regs.reg[9].how = REG_SAVED_OFFSET;
  fs->regs.reg[9].loc.offset = (long)&mctx->gregs[REG_R9] - new_cfa;
  fs->regs.reg[10].how = REG_SAVED_OFFSET;
  fs->regs.reg[10].loc.offset = (long)&mctx->gregs[REG_R10] - new_cfa;
  fs->regs.reg[11].how = REG_SAVED_OFFSET;
  fs->regs.reg[11].loc.offset = (long)&mctx->gregs[REG_R11] - new_cfa;
  fs->regs.reg[12].how = REG_SAVED_OFFSET;
  fs->regs.reg[12].loc.offset = (long)&mctx->gregs[REG_R12] - new_cfa;
  fs->regs.reg[13].how = REG_SAVED_OFFSET;
  fs->regs.reg[13].loc.offset = (long)&mctx->gregs[REG_R13] - new_cfa;
  fs->regs.reg[14].how = REG_SAVED_OFFSET;
  fs->regs.reg[14].loc.offset = (long)&mctx->gregs[REG_R14] - new_cfa;
  fs->regs.reg[15].how = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (long)&mctx->gregs[REG_R15] - new_cfa;
  fs->regs.reg[16].how = REG_SAVED_OFFSET;
  fs->regs.reg[16].loc.offset = (long)&mctx->gregs[REG_RIP] - new_cfa;
  fs->retaddr_column = 16;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#else

#define MD_FALLBACK_FRAME_STATE_FOR x86_fallback_frame_state

static _Unwind_Reason_Code
x86_fallback_frame_state (struct _Unwind_Context *context,
			  _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  mcontext_t *mctx;
  long new_cfa;

  if (/* Solaris 2.8 - single thread
	-------------------------
	<sigacthandler+17>:  mov    0x10(%ebp),%esi
	<sigacthandler+20>:  push   %esi
	<sigacthandler+21>:  pushl  0xc(%ebp)
	<sigacthandler+24>:  mov    0x8(%ebp),%ecx
	<sigacthandler+27>:  push   %ecx
	<sigacthandler+28>:  mov    offset(%ebx),%eax
	<sigacthandler+34>:  call   *(%eax,%ecx,4)
	<sigacthandler+37>:  add    $0xc,%esp        <--- PC
	<sigacthandler+40>:  push   %esi ... */
      (*(unsigned long *)(pc - 20) == 0x5610758b
       && *(unsigned long *)(pc - 16) == 0x8b0c75ff
       && *(unsigned long *)(pc - 12) == 0x8b51084d
       && *(unsigned char *)(pc - 8)  == 0x83
       && *(unsigned long *)(pc - 4)  == 0x8814ff00
       && *(unsigned long *)(pc - 0)  == 0x560cc483)

      || /* Solaris 2.8 - multi thread
	   ---------------------------
	   <__sighndlr+0>:      push   %ebp
	   <__sighndlr+1>:      mov    %esp,%ebp
	   <__sighndlr+3>:      pushl  0x10(%ebp)
	   <__sighndlr+6>:      pushl  0xc(%ebp)
	   <__sighndlr+9>:      pushl  0x8(%ebp)
	   <__sighndlr+12>:     call   *0x14(%ebp)
	   <__sighndlr+15>:     leave               <--- PC  */
	 (*(unsigned long *)(pc - 15) == 0xffec8b55
	  && *(unsigned long *)(pc - 11) == 0x75ff1075
	  && *(unsigned long *)(pc - 7)  == 0x0875ff0c
	  && *(unsigned long *)(pc - 3)  == 0xc91455ff)

      || /* Solaris 2.10
	   ------------
	   <__sighndlr+0>:      push   %ebp
	   <__sighndlr+1>:      mov    %esp,%ebp
	   <__sighndlr+3>:      pushl  0x10(%ebp)
	   <__sighndlr+6>:      pushl  0xc(%ebp)
	   <__sighndlr+9>:      pushl  0x8(%ebp)
	   <__sighndlr+12>:     call   *0x14(%ebp)
	   <__sighndlr+15>:     add    $0xc,%esp     <--- PC
	   <__sighndlr+18>:     leave
	   <__sighndlr+19>:     ret  */
	 (*(unsigned long *)(pc - 15) == 0xffec8b55
	  && *(unsigned long *)(pc - 11) == 0x75ff1075
	  && *(unsigned long *)(pc - 7)  == 0x0875ff0c
	  && *(unsigned long *)(pc - 3)  == 0x831455ff
	  && *(unsigned long *)(pc + 1)  == 0xc3c90cc4))
    {
      struct handler_args {
	int signo;
	siginfo_t *sip;
	ucontext_t *ucontext;
      } *handler_args = context->cfa;
      mctx = &handler_args->ucontext->uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = mctx->gregs[UESP];

  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 4;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&mctx->gregs[EAX] - new_cfa;
  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&mctx->gregs[EBX] - new_cfa;
  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&mctx->gregs[ECX] - new_cfa;
  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&mctx->gregs[EDX] - new_cfa;
  fs->regs.reg[6].how = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&mctx->gregs[ESI] - new_cfa;
  fs->regs.reg[7].how = REG_SAVED_OFFSET;
  fs->regs.reg[7].loc.offset = (long)&mctx->gregs[EDI] - new_cfa;
  fs->regs.reg[5].how = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&mctx->gregs[EBP] - new_cfa;
  fs->regs.reg[8].how = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&mctx->gregs[EIP] - new_cfa;
  fs->retaddr_column = 8;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#endif
