/* DWARF2 EH unwinding support for SPARC Solaris.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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

#if defined(__arch64__)

#define MD_FALLBACK_FRAME_STATE_FOR sparc64_fallback_frame_state

static _Unwind_Reason_Code
sparc64_fallback_frame_state (struct _Unwind_Context *context,
			      _Unwind_FrameState *fs)
{
  void *pc = context->ra;
  void *this_cfa = context->cfa;
  void *new_cfa, *ra_location, *shifted_ra_location;
  int regs_off;
  int fpu_save_off;
  unsigned char fpu_save;
  int i;

  /* This is the observed pattern for the sigacthandler in Solaris 8.  */
  unsigned int sigacthandler_sol8_pattern []
    = {0x9401400f, 0xca5aafa0, 0x913e2000, 0x892a3003,
       0xe0590005, 0x9fc40000, 0x9410001a, 0x80a6e008};

  /* This is the observed pattern for the sigacthandler in Solaris 9.  */ 
  unsigned int sigacthandler_sol9_pattern []
    = {0xa33e2000, 0x00000000, 0x892c7003, 0x90100011,
       0xe0590005, 0x9fc40000, 0x9410001a, 0x80a46008};

  /* This is the observed pattern for the __sighndlr.  */
  unsigned int sighndlr_pattern []
    = {0x9de3bf50, 0x90100018, 0x92100019, 0x9fc6c000,
       0x9410001a, 0x81c7e008, 0x81e80000};

  /* Deal with frame-less function from which a signal was raised.  */
  if (_Unwind_IsSignalFrame (context))
    {
      /* The CFA is by definition unmodified in this case.  */
      fs->regs.cfa_how = CFA_REG_OFFSET;
      fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
      fs->regs.cfa_offset = 0;

      /* This is the canonical RA column.  */
      fs->retaddr_column = 15;

      return _URC_NO_REASON;
    }

  /* Look for the sigacthandler pattern.  The pattern changes slightly
     in different versions of the operating system, so we skip the
     comparison against pc-(4*6) for Solaris 9.  */
  if ((    *(unsigned int *)(pc-(4*7)) == sigacthandler_sol8_pattern[0]
	&& *(unsigned int *)(pc-(4*6)) == sigacthandler_sol8_pattern[1]
	&& *(unsigned int *)(pc-(4*5)) == sigacthandler_sol8_pattern[2]
	&& *(unsigned int *)(pc-(4*4)) == sigacthandler_sol8_pattern[3]
	&& *(unsigned int *)(pc-(4*3)) == sigacthandler_sol8_pattern[4]
	&& *(unsigned int *)(pc-(4*2)) == sigacthandler_sol8_pattern[5]
	&& *(unsigned int *)(pc-(4*1)) == sigacthandler_sol8_pattern[6]
	&& *(unsigned int *)(pc-(4*0)) == sigacthandler_sol8_pattern[7] ) ||
      (    *(unsigned int *)(pc-(4*7)) == sigacthandler_sol9_pattern[0]
	/* skip pc-(4*6) */
	&& *(unsigned int *)(pc-(4*5)) == sigacthandler_sol9_pattern[2]
	&& *(unsigned int *)(pc-(4*4)) == sigacthandler_sol9_pattern[3]
	&& *(unsigned int *)(pc-(4*3)) == sigacthandler_sol9_pattern[4]
	&& *(unsigned int *)(pc-(4*2)) == sigacthandler_sol9_pattern[5]
	&& *(unsigned int *)(pc-(4*1)) == sigacthandler_sol9_pattern[6]
	&& *(unsigned int *)(pc-(4*0)) == sigacthandler_sol9_pattern[7] ) )
    /* We need to move up two frames (the kernel frame and the handler
       frame).  Minimum stack frame size is 176 bytes (128 + 48): 128
       bytes for spilling register window (16 extended words for in
       and local registers), and 6 extended words to store at least
       6 arguments to callees, The kernel frame and the sigacthandler
       both have this minimal stack.  The ucontext_t structure is after
       this offset.  */
    regs_off = 176 + 176;

  /* Look for the __sighndlr pattern.  */
  else if (    *(unsigned int *)(pc-(4*5)) == sighndlr_pattern[0]
	    && *(unsigned int *)(pc-(4*4)) == sighndlr_pattern[1]
	    && *(unsigned int *)(pc-(4*3)) == sighndlr_pattern[2]
	    && *(unsigned int *)(pc-(4*2)) == sighndlr_pattern[3]
	    && *(unsigned int *)(pc-(4*1)) == sighndlr_pattern[4]
	    && *(unsigned int *)(pc-(4*0)) == sighndlr_pattern[5]
	    && *(unsigned int *)(pc+(4*1)) == sighndlr_pattern[6] )
    {
      /* We have observed different calling frames among different
	 versions of the operating system, so that we need to
	 discriminate using the upper frame.  We look for the return
	 address of the caller frame (there is an offset of 15 double
	 words between the frame address and the place where this return
	 address is stored) in order to do some more pattern matching.  */
      unsigned int cuh_pattern
	= *(unsigned int *)(*(unsigned long *)(this_cfa + 15*8) - 4);

      if (cuh_pattern == 0xd25fa7ef)
	{
	  /* This matches the call_user_handler pattern for Solaris 10.
	     There are 2 cases so we look for the return address of the
	     caller's caller frame in order to do more pattern matching.  */
	  unsigned long sah_address
	    = *(unsigned long *)(this_cfa + 176 + 15*8);

          if (sah_address && *(unsigned int *)(sah_address - 4) == 0x92100019)
	    /* This is the same setup as for Solaris 9, see below.  */
	    regs_off = 176 + 176 + 176 + 304;
	  else
	    /* We need to move up three frames (the kernel frame, the
	       call_user_handler frame, the __sighndlr frame).  Two of them
	       have the minimum stack frame size (kernel and __sighndlr
	       frames) of 176 bytes, and there is another with a stack frame
	       of 304 bytes (the call_user_handler frame).  The ucontext_t
	       structure is after this offset.  */
	    regs_off = 176 + 176 + 304;
	}
      else if (cuh_pattern == 0x9410001a || cuh_pattern == 0x94100013)
	/* This matches the call_user_handler pattern for Solaris 9 and
	   for Solaris 8 running inside Solaris Containers respectively.
	   We need to move up four frames (the kernel frame, the signal
	   frame, the call_user_handler frame, the __sighndlr frame).
	   Three of them have the minimum stack frame size (kernel,
	   signal, and __sighndlr frames) of 176 bytes, and there is
	   another with a stack frame of 304 bytes (the call_user_handler
	   frame).  The ucontext_t structure is after this offset.  */
	regs_off = 176 + 176 + 176 + 304;
      else
	/* We need to move up three frames (the kernel frame, the
	   sigacthandler frame, and the __sighndlr frame).  The kernel
	   frame has a stack frame size of 176, the __sighndlr frames of
	   304 bytes, and there is a stack frame of 176 bytes for the
	   sigacthandler frame.  The ucontext_t structure is after this
	   offset.  */
	regs_off = 176 + 304 + 176;
    }

  /* Exit if the pattern at the return address does not match the
     previous three patterns.  */
  else
    return _URC_END_OF_STACK;

  /* FPU information can be extracted from the ucontext_t structure 
     that is the third argument for the signal handler, that is saved
     in the stack.  There are 64 bytes between the beginning of the
     ucontext_t argument of the signal handler and the uc_mcontext
     field.  There are 176 bytes between the beginning of uc_mcontext
     and the beginning of the fpregs field.  */
  fpu_save_off = regs_off + (8*10) + 176;

  /* The fpregs field contains 32 extended words at the beginning that
     contain the FPU state.  Then there are 2 extended words and two
     bytes.  */
  fpu_save = *(unsigned char *)(this_cfa + fpu_save_off + (8*32) + (2*8) + 2);

  /* We need to get the frame pointer for the kernel frame that
     executes when the signal is raised.  This frame is just the
     following to the application code that generated the signal, so
     that the later's stack pointer is the former's frame pointer.
     The stack pointer for the interrupted application code can be
     calculated from the ucontext_t structure (third argument for the
     signal handler) that is saved in the stack.  There are 10 words
     between the beginning of the  ucontext_t argument  of the signal
     handler and the uc_mcontext.gregs field that contains the
     registers saved by the signal handler.  */
  new_cfa = *(void **)(this_cfa + regs_off + (8*10) + (REG_SP*8));
  /* The frame address is %sp + STACK_BIAS in 64-bit mode. */
  new_cfa += 2047;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
  fs->regs.cfa_offset = new_cfa - this_cfa;

  /* Restore global and out registers (in this order) from the
     ucontext_t structure, uc_mcontext.gregs field.  */
  for (i = 1; i < 16; i++)
    {
      /* We never restore %sp as everything is purely CFA-based.  */
      if ((unsigned int) i == __builtin_dwarf_sp_column ())
	continue;

      /* First the global registers and then the out registers.  */
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset
	= this_cfa + regs_off + (8*10) + ((REG_Y+i)*8) - new_cfa;
    }

  /* Just above the stack pointer there are 16 extended words in which
     the register window (in and local registers) was saved.  */
  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[i + 16].how = REG_SAVED_OFFSET;
      fs->regs.reg[i + 16].loc.offset = i*8;
    }

  /* Check whether we need to restore FPU registers.  */
  if (fpu_save)
    {
      for (i = 0; i < 64; i++)
	{
	  if (i > 32 && (i & 1))
	    continue;

	  fs->regs.reg[i + 32].how = REG_SAVED_OFFSET;
	  fs->regs.reg[i + 32].loc.offset
	    = this_cfa + fpu_save_off + (i*4) - new_cfa;
	}
    }

  /* State the rules to find the kernel's code "return address", which is
     the address of the active instruction when the signal was caught.
     On the SPARC, since RETURN_ADDR_OFFSET (essentially 8) is defined, we
     need to preventively subtract it from the purported return address.  */
  ra_location = this_cfa + regs_off + (8*10) + (REG_PC*8);
  shifted_ra_location = this_cfa + regs_off + (8*10) + (REG_Y*8);
  *(void **)shifted_ra_location = *(void **)ra_location - 8;
  fs->retaddr_column = 0;
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = shifted_ra_location - new_cfa;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#define MD_FROB_UPDATE_CONTEXT sparc64_frob_update_context

static void
sparc64_frob_update_context (struct _Unwind_Context *context,
			     _Unwind_FrameState *fs)
{
  /* The column of %sp contains the old CFA, not the old value of %sp.
     The CFA offset already comprises the stack bias so, when %sp is the
     CFA register, we must avoid counting the stack bias twice.  Do not
     do that for signal frames as the offset is artificial for them.  */
  if (fs->regs.cfa_reg == __builtin_dwarf_sp_column ()
      && fs->regs.cfa_how == CFA_REG_OFFSET
      && fs->regs.cfa_offset != 0
      && !fs->signal_frame)
    context->cfa -= 2047;
}

#else

#define MD_FALLBACK_FRAME_STATE_FOR sparc_fallback_frame_state

static _Unwind_Reason_Code
sparc_fallback_frame_state (struct _Unwind_Context *context,
			    _Unwind_FrameState *fs)
{
  void *pc = context->ra;
  void *this_cfa = context->cfa;
  void *new_cfa, *ra_location, *shifted_ra_location;
  int regs_off;
  int fpu_save_off;
  unsigned char fpu_save;
  int i;

  /* This is the observed pattern for the sigacthandler.  */
  unsigned int sigacthandler_pattern []
    = {0x9602400f, 0x92100019, 0x00000000, 0x912e2002,
       0xe002000a, 0x90100018, 0x9fc40000, 0x9410001a,
       0x80a62008};

  /* This is the observed pattern for the __libthread_segvhdlr.  */
  unsigned int segvhdlr_pattern []
    = {0x94102000, 0xe007bfe4, 0x9010001c, 0x92100019,
       0x9fc40000, 0x9410001a, 0x81c7e008, 0x81e80000,
       0x80a26000};

  /* This is the observed pattern for the __sighndlr.  */
  unsigned int sighndlr_pattern []
    = {0x9de3bfa0, 0x90100018, 0x92100019, 0x9fc6c000,
       0x9410001a, 0x81c7e008, 0x81e80000};

  /* Deal with frame-less function from which a signal was raised.  */
  if (_Unwind_IsSignalFrame (context))
    {
      /* The CFA is by definition unmodified in this case.  */
      fs->regs.cfa_how = CFA_REG_OFFSET;
      fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
      fs->regs.cfa_offset = 0;

      /* This is the canonical RA column.  */
      fs->retaddr_column = 15;

      return _URC_NO_REASON;
    }

  /* Look for the sigacthandler pattern.  The pattern changes slightly
     in different versions of the operating system, so we skip the
     comparison against pc-(4*6).  */
  if (    *(unsigned int *)(pc-(4*8)) == sigacthandler_pattern[0]
       && *(unsigned int *)(pc-(4*7)) == sigacthandler_pattern[1]
       /* skip pc-(4*6) */
       && *(unsigned int *)(pc-(4*5)) == sigacthandler_pattern[3]
       && *(unsigned int *)(pc-(4*4)) == sigacthandler_pattern[4]
       && *(unsigned int *)(pc-(4*3)) == sigacthandler_pattern[5]
       && *(unsigned int *)(pc-(4*2)) == sigacthandler_pattern[6]
       && *(unsigned int *)(pc-(4*1)) == sigacthandler_pattern[7]
       && *(unsigned int *)(pc-(4*0)) == sigacthandler_pattern[8] )
    /* We need to move up two frames (the kernel frame and the handler
       frame).  Minimum stack frame size is 96 bytes (64 + 4 + 24): 64
       bytes for spilling register window (16 words for in and local
       registers), 4 bytes for a pointer to space for callees
       returning structs, and 24 bytes to store at least six argument
       to callees.  The ucontext_t structure is after this offset.  */
    regs_off = 96 + 96;

  /* Look for the __libthread_segvhdlr pattern.  */
  else if (    *(unsigned int *)(pc-(4*6)) == segvhdlr_pattern[0]
	    && *(unsigned int *)(pc-(4*5)) == segvhdlr_pattern[1]
	    && *(unsigned int *)(pc-(4*4)) == segvhdlr_pattern[2]
	    && *(unsigned int *)(pc-(4*3)) == segvhdlr_pattern[3]
	    && *(unsigned int *)(pc-(4*2)) == segvhdlr_pattern[4]
	    && *(unsigned int *)(pc-(4*1)) == segvhdlr_pattern[5]
	    && *(unsigned int *)(pc-(4*0)) == segvhdlr_pattern[6]
	    && *(unsigned int *)(pc+(4*1)) == segvhdlr_pattern[7]
	    && *(unsigned int *)(pc+(4*2)) == segvhdlr_pattern[8] )
    /* We need to move up four frames (the kernel frame, the
       sigacthandler frame, the __sighndlr frame, and the
       __libthread_segvhdlr).  Two of them have the minimum
       stack frame size (kernel and __sighndlr frames) of 96 bytes,
       other has a stack frame of 216 bytes (the sigacthandler frame),
       and there is another with a stack frame of 128 bytes (the
       __libthread_segvhdlr).  The ucontext_t structure is after this
       offset.  */
    regs_off = 96 + 96 + 128 + 216;

  /* Look for the __sighndlr pattern.  */
  else if (    *(unsigned int *)(pc-(4*5)) == sighndlr_pattern[0]
	    && *(unsigned int *)(pc-(4*4)) == sighndlr_pattern[1]
	    && *(unsigned int *)(pc-(4*3)) == sighndlr_pattern[2]
	    && *(unsigned int *)(pc-(4*2)) == sighndlr_pattern[3]
	    && *(unsigned int *)(pc-(4*1)) == sighndlr_pattern[4]
	    && *(unsigned int *)(pc-(4*0)) == sighndlr_pattern[5]
	    && *(unsigned int *)(pc+(4*1)) == sighndlr_pattern[6] )
    {
      /* We have observed different calling frames among different
	 versions of the operating system, so that we need to
	 discriminate using the upper frame.  We look for the return
	 address of the caller frame (there is an offset of 15 words
	 between the frame address and the place where this return
	 address is stored) in order to do some more pattern matching.  */
      unsigned int cuh_pattern
	= *(unsigned int *)(*(unsigned int *)(this_cfa + 15*4) - 4);

      if (cuh_pattern == 0xd407a04c)
	{
	  /* This matches the call_user_handler pattern for Solaris 10.
	     There are 2 cases so we look for the return address of the
	     caller's caller frame in order to do more pattern matching.  */
	  unsigned int sah_address
	    = *(unsigned int *)(this_cfa + 96 + 15*4);

          if (sah_address && *(unsigned int *)(sah_address - 4) == 0x92100019)
	    /* This is the same setup as for Solaris 9, see below.  */
	    regs_off = 96 + 96 + 96 + 160;
	  else
	    /* We need to move up three frames (the kernel frame, the
	       call_user_handler frame, the __sighndlr frame).  Two of them
	       have the minimum stack frame size (kernel and __sighndlr
	       frames) of 96 bytes, and there is another with a stack frame
	       of 160 bytes (the call_user_handler frame).  The ucontext_t
	       structure is after this offset.  */
	    regs_off = 96 + 96 + 160;
	}
      else if (cuh_pattern == 0x9410001a || cuh_pattern == 0x9410001b)
	/* This matches the call_user_handler pattern for Solaris 9 and
	   for Solaris 8 running inside Solaris Containers respectively.
	   We need to move up four frames (the kernel frame, the signal
	   frame, the call_user_handler frame, the __sighndlr frame).
	   Three of them have the minimum stack frame size (kernel,
	   signal, and __sighndlr frames) of 96 bytes, and there is
	   another with a stack frame of 160 bytes (the call_user_handler
	   frame).  The ucontext_t structure is after this offset.  */
	regs_off = 96 + 96 + 96 + 160;
      else
	/* We need to move up three frames (the kernel frame, the
	   sigacthandler frame, and the __sighndlr frame).  Two of them
	   have the minimum stack frame size (kernel and __sighndlr
	   frames) of 96 bytes, and there is another with a stack frame
	   of 216 bytes (the sigacthandler frame).  The ucontext_t 
	   structure is after this offset.  */
	regs_off = 96 + 96 + 216;
    }

  /* Exit if the pattern at the return address does not match the
     previous three patterns.  */
  else
    return _URC_END_OF_STACK;

  /* FPU information can be extracted from the ucontext_t structure
     that is the third argument for the signal handler, that is saved
     in the stack.  There are 10 words between the beginning of the
     ucontext_t argument of the signal handler and the uc_mcontext
     field.  There are 80 bytes between the beginning of uc_mcontext
     and the beginning of the fpregs field.  */
  fpu_save_off = regs_off + (4*10) + (4*20);

  /* The fpregs field contains 32 words at the beginning that contain
     the FPU state.  Then there are 2 words and two bytes.  */
  fpu_save = *(unsigned char *)(this_cfa + fpu_save_off + (4*32) + (2*4) + 2);

  /* We need to get the frame pointer for the kernel frame that
     executes when the signal is raised.  This frame is just the
     following to the application code that generated the signal, so
     that the later's stack pointer is the former's frame pointer.
     The stack pointer for the interrupted application code can be
     calculated from the ucontext_t structure (third argument for the
     signal handler) that is saved in the stack.  There are 10 words
     between the beginning of the  ucontext_t argument  of the signal
     handler and the uc_mcontext.gregs field that contains the
     registers saved by the signal handler.  */
  new_cfa = *(void **)(this_cfa + regs_off + (4*10) + (REG_SP*4));
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
  fs->regs.cfa_offset = new_cfa - this_cfa;

  /* Restore global and out registers (in this order) from the
     ucontext_t structure, uc_mcontext.gregs field.  */
  for (i = 1; i < 16; i++)
    {
      /* We never restore %sp as everything is purely CFA-based.  */
      if ((unsigned int) i == __builtin_dwarf_sp_column ())
	continue;

      /* First the global registers and then the out registers */
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset
	= this_cfa + regs_off + (4*10) + ((REG_Y+i)*4) - new_cfa;
    }

  /* Just above the stack pointer there are 16 words in which the
     register window (in and local registers) was saved.  */
  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[i + 16].how = REG_SAVED_OFFSET;
      fs->regs.reg[i + 16].loc.offset = i*4;
    }

  /* Check whether we need to restore FPU registers.  */
  if (fpu_save)
    {
      for (i = 0; i < 32; i++)
	{
	  fs->regs.reg[i + 32].how = REG_SAVED_OFFSET;
	  fs->regs.reg[i + 32].loc.offset
	    = this_cfa + fpu_save_off + (i*4) - new_cfa;
	}
    }

  /* State the rules to find the kernel's code "return address", which is
     the address of the active instruction when the signal was caught.
     On the SPARC, since RETURN_ADDR_OFFSET (essentially 8) is defined, we
     need to preventively subtract it from the purported return address.  */
  ra_location = this_cfa + regs_off + (4*10) + (REG_PC*4);
  shifted_ra_location = this_cfa + regs_off + (4*10) + (REG_Y*4);
  *(void **)shifted_ra_location = *(void **)ra_location - 8;
  fs->retaddr_column = 0;
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = shifted_ra_location - new_cfa;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
};

#endif
