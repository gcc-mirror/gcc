/* Subroutines for insn-output.c for Windows NT.
   Contributed by Douglas Rupp (drupp@cs.washington.edu)
   Copyright (C) 1995 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "output.h"
#include "tree.h"
#include "flags.h"

/* This function generates the assembly code for function entry.
   FILE is an stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate. */

void
winnt_function_prologue (file, size)
     FILE *file;
     int size;
{
  register int regno;
  int limit;
  rtx xops[4];
  int pic_reg_used = flag_pic && (current_function_uses_pic_offset_table
				  || current_function_uses_const_pool);

  xops[0] = stack_pointer_rtx;
  xops[1] = frame_pointer_rtx;
  xops[2] = GEN_INT (size);
  xops[3] = gen_rtx (REG, Pmode, 0); /* eax */
  if (frame_pointer_needed)
    {
      output_asm_insn ("push%L1 %1", xops);
      output_asm_insn (AS2 (mov%L0,%0,%1), xops);
    }

  if (size > 4095)
    {
      output_asm_insn (AS2 (mov%L0, %2, %3), xops);
      output_asm_insn ("call __chkstk", xops);
    }
  else if (size)
    output_asm_insn (AS2 (sub%L0,%2,%0), xops);

  /* Note If use enter it is NOT reversed args.
     This one is not reversed from intel!!
     I think enter is slower.  Also sdb doesn't like it.
     But if you want it the code is:
     {
     xops[3] = const0_rtx;
     output_asm_insn ("enter %2,%3", xops);
     }
     */
  limit = (frame_pointer_needed ? FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);
  for (regno = limit - 1; regno >= 0; regno--)
    if ((regs_ever_live[regno] && ! call_used_regs[regno])
	|| (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
      {
	xops[0] = gen_rtx (REG, SImode, regno);
	output_asm_insn ("push%L0 %0", xops);
      }

  if (pic_reg_used)
    {
      xops[0] = pic_offset_table_rtx;
      xops[1] = (rtx) gen_label_rtx ();

      output_asm_insn (AS1 (call,%P1), xops);
      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (xops[1]));
      output_asm_insn (AS1 (pop%L0,%0), xops);
      output_asm_insn ("addl $_GLOBAL_OFFSET_TABLE_+[.-%P1],%0", xops);
    }
}
