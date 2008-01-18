/* DWARF2 EH unwinding support for Xtensa.
   Copyright (C) 2008 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2-xtensa.c for the structs.
   Don't use this at all if inhibit_libc is used.  */

#ifndef inhibit_libc

#include <signal.h>
#include <sys/ucontext.h>

/* Encoded bytes for Xtensa instructions:
	movi a2, __NR_rt_sigreturn
	syscall
	entry (first byte only)
   Some of the bytes are endian-dependent.  */

#define MOVI_BYTE0 0x22
#define MOVI_BYTE2 225 /* __NR_rt_sigreturn */
#define SYSC_BYTE0 0
#define SYSC_BYTE2 0

#ifdef __XTENSA_EB__
#define MOVI_BYTE1 0x0a
#define SYSC_BYTE1 0x05
#define ENTRY_BYTE 0x6c
#else
#define MOVI_BYTE1 0xa0
#define SYSC_BYTE1 0x50
#define ENTRY_BYTE 0x36
#endif

#define MD_FALLBACK_FRAME_STATE_FOR xtensa_fallback_frame_state

static _Unwind_Reason_Code
xtensa_fallback_frame_state (struct _Unwind_Context *context,
			     _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  struct sigcontext *sc;

  struct rt_sigframe {
    struct siginfo info;
    struct ucontext uc;
  } *rt_;

  /* movi a2, __NR_rt_sigreturn; syscall */
  if (pc[0] != MOVI_BYTE0
      || pc[1] != MOVI_BYTE1
      || pc[2] != MOVI_BYTE2
      || pc[3] != SYSC_BYTE0
      || pc[4] != SYSC_BYTE1
      || pc[5] != SYSC_BYTE2)
    return _URC_END_OF_STACK;

  rt_ = context->sp;
  sc = &rt_->uc.uc_mcontext;
  fs->signal_regs = (_Unwind_Word *) sc->sc_a;

  /* If the signal arrived just before an ENTRY instruction, find the return
     address and pretend the signal arrived before executing the CALL.  */
  if (*(unsigned char *) sc->sc_pc == ENTRY_BYTE)
   {
     unsigned callinc = (sc->sc_ps >> 16) & 3;
     fs->signal_ra = ((sc->sc_a[callinc << 2] & XTENSA_RA_FIELD_MASK)
		      | context->ra_high_bits) - 3;
   }
  else
    fs->signal_ra = sc->sc_pc;

  fs->signal_frame = 1;
  return _URC_NO_REASON;
}

#endif /* ifdef inhibit_libc  */
