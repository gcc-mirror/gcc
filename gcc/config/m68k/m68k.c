/* Subroutines for insn-output.c for Motorola 68000 family.
   Copyright (C) 1987, 93-98, 1999 Free Software Foundation, Inc.

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


/* Some output-actions in m68k.md need these.  */
#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "recog.h"
#include "toplev.h"

/* Needed for use_return_insn.  */
#include "flags.h"

#ifdef SUPPORT_SUN_FPA

/* Index into this array by (register number >> 3) to find the
   smallest class which contains that register.  */
enum reg_class regno_reg_class[]
  = { DATA_REGS, ADDR_REGS, FP_REGS,
      LO_FPA_REGS, LO_FPA_REGS, FPA_REGS, FPA_REGS };

#endif /* defined SUPPORT_SUN_FPA */

/* This flag is used to communicate between movhi and ASM_OUTPUT_CASE_END,
   if SGS_SWITCH_TABLE.  */
int switch_table_difference_label_flag;

static rtx find_addr_reg ();
rtx legitimize_pic_address ();


/* Alignment to use for loops and jumps */
/* Specify power of two alignment used for loops. */
const char *m68k_align_loops_string;
/* Specify power of two alignment used for non-loop jumps. */
const char *m68k_align_jumps_string;
/* Specify power of two alignment used for functions. */
const char *m68k_align_funcs_string;

/* Specify power of two alignment used for loops. */
int m68k_align_loops;
/* Specify power of two alignment used for non-loop jumps. */
int m68k_align_jumps;
/* Specify power of two alignment used for functions. */
int m68k_align_funcs;

/* Nonzero if the last compare/test insn had FP operands.  The
   sCC expanders peek at this to determine what to do for the
   68060, which has no fsCC instructions.  */
int m68k_last_compare_had_fp_operands;

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

void
override_options ()
{
  int def_align;

  def_align = 1;

  /* Validate -malign-loops= value, or provide default */
  if (m68k_align_loops_string)
    {
      m68k_align_loops = atoi (m68k_align_loops_string);
      if (m68k_align_loops < 1 || m68k_align_loops > MAX_CODE_ALIGN)
	fatal ("-malign-loops=%d is not between 1 and %d",
	       m68k_align_loops, MAX_CODE_ALIGN);
    }
  else
    m68k_align_loops = def_align;

  /* Validate -malign-jumps= value, or provide default */
  if (m68k_align_jumps_string)
    {
      m68k_align_jumps = atoi (m68k_align_jumps_string);
      if (m68k_align_jumps < 1 || m68k_align_jumps > MAX_CODE_ALIGN)
	fatal ("-malign-jumps=%d is not between 1 and %d",
	       m68k_align_jumps, MAX_CODE_ALIGN);
    }
  else
    m68k_align_jumps = def_align;

  /* Validate -malign-functions= value, or provide default */
  if (m68k_align_funcs_string)
    {
      m68k_align_funcs = atoi (m68k_align_funcs_string);
      if (m68k_align_funcs < 1 || m68k_align_funcs > MAX_CODE_ALIGN)
	fatal ("-malign-functions=%d is not between 1 and %d",
	       m68k_align_funcs, MAX_CODE_ALIGN);
    }
  else
    m68k_align_funcs = def_align;
}

/* This function generates the assembly code for function entry.
   STREAM is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This function is responsible for
   knowing which registers should not be saved even if used.  */


/* Note that the order of the bit mask for fmovem is the opposite
   of the order for movem!  */


void
output_function_prologue (stream, size)
     FILE *stream;
     int size;
{
  register int regno;
  register int mask = 0;
  int num_saved_regs = 0;
  extern char call_used_regs[];
  int fsize = (size + 3) & -4;
  int cfa_offset = INCOMING_FRAME_SP_OFFSET, cfa_store_offset = cfa_offset;
  

  if (frame_pointer_needed)
    {
      if (fsize == 0 && TARGET_68040)
	{
	/* on the 68040, pea + move is faster than link.w 0 */
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tpea (%s)\n\tmove.l %s,%s\n",
	       reg_names[FRAME_POINTER_REGNUM], reg_names[STACK_POINTER_REGNUM],
	       reg_names[FRAME_POINTER_REGNUM]);
#else
	  asm_fprintf (stream, "\tpea %s@\n\tmovel %s,%s\n",
	       reg_names[FRAME_POINTER_REGNUM], reg_names[STACK_POINTER_REGNUM],
	       reg_names[FRAME_POINTER_REGNUM]);
#endif
	}
      else if (fsize < 0x8000)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tlink.w %s,%0I%d\n",
		       reg_names[FRAME_POINTER_REGNUM], -fsize);
#else
	  asm_fprintf (stream, "\tlink %s,%0I%d\n",
		       reg_names[FRAME_POINTER_REGNUM], -fsize);
#endif
	}
      else if (TARGET_68020)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tlink.l %s,%0I%d\n",
		       reg_names[FRAME_POINTER_REGNUM], -fsize);
#else
	  asm_fprintf (stream, "\tlink %s,%0I%d\n",
		       reg_names[FRAME_POINTER_REGNUM], -fsize);
#endif
	}
      else
	{
      /* Adding negative number is faster on the 68040.  */
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tlink.w %s,%0I0\n\tadd.l %0I%d,%Rsp\n",
		       reg_names[FRAME_POINTER_REGNUM], -fsize);
#else
	  asm_fprintf (stream, "\tlink %s,%0I0\n\taddl %0I%d,%Rsp\n",
		       reg_names[FRAME_POINTER_REGNUM], -fsize);
#endif
	}
      if (dwarf2out_do_frame ())
	{
	  char *l;
          l = (char *) dwarf2out_cfi_label ();   
	  cfa_store_offset += 4;
	  cfa_offset = cfa_store_offset;
	  dwarf2out_def_cfa (l, FRAME_POINTER_REGNUM, cfa_offset);
	  dwarf2out_reg_save (l, FRAME_POINTER_REGNUM, -cfa_store_offset);
	  cfa_store_offset += fsize;
	}
    }
  else if (fsize)
    {
      if (fsize + 4 < 0x8000)
	{
#ifndef NO_ADDSUB_Q
	  if (fsize + 4 <= 8)
	    {
	      if (!TARGET_5200)
		{
		  /* asm_fprintf() cannot handle %. */
#ifdef MOTOROLA
		  asm_fprintf (stream, "\tsubq.w %0I%d,%Rsp\n", fsize + 4);
#else
		  asm_fprintf (stream, "\tsubqw %0I%d,%Rsp\n", fsize + 4);
#endif
		}
	      else
		{
		  /* asm_fprintf() cannot handle %. */
#ifdef MOTOROLA
		  asm_fprintf (stream, "\tsubq.l %0I%d,%Rsp\n", fsize + 4);
#else
		  asm_fprintf (stream, "\tsubql %0I%d,%Rsp\n", fsize + 4);
#endif
		}
	    }
	  else if (fsize + 4 <= 16 && TARGET_CPU32)
	    {
	      /* On the CPU32 it is faster to use two subqw instructions to
		 subtract a small integer (8 < N <= 16) to a register. */
	      /* asm_fprintf() cannot handle %. */
#ifdef MOTOROLA
	      asm_fprintf (stream, "\tsubq.w %0I8,%Rsp\n\tsubq.w %0I%d,%Rsp\n",
			   fsize + 4 - 8);
#else
	      asm_fprintf (stream, "\tsubqw %0I8,%Rsp\n\tsubqw %0I%d,%Rsp\n",
			   fsize + 4 - 8);
#endif
	    }
	  else 
#endif /* not NO_ADDSUB_Q */
	  if (TARGET_68040)
	    {
	      /* Adding negative number is faster on the 68040.  */
	      /* asm_fprintf() cannot handle %. */
#ifdef MOTOROLA
	      asm_fprintf (stream, "\tadd.w %0I%d,%Rsp\n", - (fsize + 4));
#else
	      asm_fprintf (stream, "\taddw %0I%d,%Rsp\n", - (fsize + 4));
#endif
	    }
	  else
	    {
#ifdef MOTOROLA
	      asm_fprintf (stream, "\tlea (%d,%Rsp),%Rsp\n", - (fsize + 4));
#else
	      asm_fprintf (stream, "\tlea %Rsp@(%d),%Rsp\n", - (fsize + 4));
#endif
	    }
	}
      else
	{
	/* asm_fprintf() cannot handle %. */
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tadd.l %0I%d,%Rsp\n", - (fsize + 4));
#else
	  asm_fprintf (stream, "\taddl %0I%d,%Rsp\n", - (fsize + 4));
#endif
	}
      if (dwarf2out_do_frame ())
	{
	  cfa_store_offset += fsize;
	  cfa_offset = cfa_store_offset;
	  dwarf2out_def_cfa ("", STACK_POINTER_REGNUM, cfa_offset);
	}
    }
#ifdef SUPPORT_SUN_FPA
  for (regno = 24; regno < 56; regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
#ifdef MOTOROLA
	asm_fprintf (stream, "\tfpmovd %s,-(%Rsp)\n",
		     reg_names[regno]);
#else
	asm_fprintf (stream, "\tfpmoved %s,%Rsp@-\n",
		     reg_names[regno]);
#endif
	if (dwarf2out_do_frame ())
	  {
	    char *l = dwarf2out_cfi_label ();

	    cfa_store_offset += 8;
	    if (! frame_pointer_needed)
	      {
		cfa_offset = cfa_store_offset;
		dwarf2out_def_cfa (l, STACK_POINTER_REGNUM, cfa_offset);
	      }
	    dwarf2out_reg_save (l, regno, -cfa_store_offset);
	  }
      }
#endif
  if (TARGET_68881)
    {
      for (regno = 16; regno < 24; regno++)
	if (regs_ever_live[regno] && ! call_used_regs[regno])
	  {
	    mask |= 1 << (regno - 16);
	    num_saved_regs++;
	  }
      if ((mask & 0xff) != 0)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tfmovm %0I0x%x,-(%Rsp)\n", mask & 0xff);
#else
	  asm_fprintf (stream, "\tfmovem %0I0x%x,%Rsp@-\n", mask & 0xff);
#endif
	  if (dwarf2out_do_frame ())
	    {
	      char *l = (char *) dwarf2out_cfi_label ();
	      int n_regs;

	      cfa_store_offset += num_saved_regs * 12;
	      if (! frame_pointer_needed)
		{
		  cfa_offset = cfa_store_offset;
		  dwarf2out_def_cfa (l, STACK_POINTER_REGNUM, cfa_offset);
		}
	      for (regno = 16, n_regs = 0; regno < 24; regno++)
		if (mask & (1 << (regno - 16)))
		  dwarf2out_reg_save (l, regno,
				      -cfa_store_offset + n_regs++ * 12);
	    }
	}
      mask = 0;
      num_saved_regs = 0;
    }
  for (regno = 0; regno < 16; regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
        mask |= 1 << (15 - regno);
        num_saved_regs++;
      }
  if (frame_pointer_needed)
    {
      mask &= ~ (1 << (15 - FRAME_POINTER_REGNUM));
      num_saved_regs--;
    }
  if (flag_pic && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
    {
      mask |= 1 << (15 - PIC_OFFSET_TABLE_REGNUM);
      num_saved_regs++;
    }

#if NEED_PROBE
#ifdef MOTOROLA
#ifdef CRDS
  asm_fprintf (stream, "\ttstl %d(%Rsp)\n", NEED_PROBE - num_saved_regs * 4);
#else
  asm_fprintf (stream, "\ttst.l %d(%Rsp)\n", NEED_PROBE - num_saved_regs * 4);
#endif
#else
  asm_fprintf (stream, "\ttstl %Rsp@(%d)\n", NEED_PROBE - num_saved_regs * 4);
#endif
#endif

  if (num_saved_regs <= 2)
    {
      /* Store each separately in the same order moveml uses.
         Using two movel instructions instead of a single moveml
         is about 15% faster for the 68020 and 68030 at no expense
         in code size */

      int i;

      /* Undo the work from above. */
      for (i = 0; i< 16; i++)
        if (mask & (1 << i))
	  {
	    asm_fprintf (stream,
#ifdef MOTOROLA
			 "\t%Omove.l %s,-(%Rsp)\n",
#else
			 "\tmovel %s,%Rsp@-\n",
#endif
			 reg_names[15 - i]);
	    if (dwarf2out_do_frame ())
	      {
		char *l = (char *) dwarf2out_cfi_label ();

		cfa_store_offset += 4;
 		if (! frame_pointer_needed)
 		  {
 		    cfa_offset = cfa_store_offset;
 		    dwarf2out_def_cfa (l, STACK_POINTER_REGNUM, cfa_offset);
 		  }
 		dwarf2out_reg_save (l, 15 - i, -cfa_store_offset);
	      }
	  }
    }
  else if (mask)
    {
      if (TARGET_5200)
	{
	  /* The coldfire does not support the predecrement form of the 
	     movml instruction, so we must adjust the stack pointer and
	     then use the plain address register indirect mode.  We also
	     have to invert the register save mask to use the new mode.

	     FIXME: if num_saved_regs was calculated earlier, we could
	     combine the stack pointer adjustment with any adjustment
	     done when the initial stack frame is created.  This would
	     save an instruction */
	     
	  int newmask = 0;
	  int i;

	  for (i = 0; i < 16; i++)
	    if (mask & (1 << i))
		newmask |= (1 << (15-i));

#ifdef MOTOROLA
	  asm_fprintf (stream, "\tlea (%d,%Rsp),%Rsp\n", -num_saved_regs*4);
	  asm_fprintf (stream, "\tmovm.l %0I0x%x,(%Rsp)\n", newmask);
#else
	  asm_fprintf (stream, "\tlea %Rsp@(%d),%Rsp\n", -num_saved_regs*4);
	  asm_fprintf (stream, "\tmoveml %0I0x%x,%Rsp@\n", newmask);
#endif
	}
      else
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tmovm.l %0I0x%x,-(%Rsp)\n", mask);
#else
	  asm_fprintf (stream, "\tmoveml %0I0x%x,%Rsp@-\n", mask);
#endif
	}
      if (dwarf2out_do_frame ())
	{
	  char *l = (char *) dwarf2out_cfi_label ();
	  int n_regs;

	  cfa_store_offset += num_saved_regs * 4;
	  if (! frame_pointer_needed)
	    {
	      cfa_offset = cfa_store_offset;
	      dwarf2out_def_cfa (l, STACK_POINTER_REGNUM, cfa_offset);
	    }
	  for (regno = 0, n_regs = 0; regno < 16; regno++)
	    if (mask & (1 << (15 - regno)))
	      dwarf2out_reg_save (l, regno,
				  -cfa_store_offset + n_regs++ * 4);
	}
    }
  if (flag_pic && current_function_uses_pic_offset_table)
    {
#ifdef MOTOROLA
      asm_fprintf (stream, "\t%Olea (%Rpc, %U_GLOBAL_OFFSET_TABLE_@GOTPC), %s\n",
		   reg_names[PIC_OFFSET_TABLE_REGNUM]);
#else
      asm_fprintf (stream, "\tmovel %0I__GLOBAL_OFFSET_TABLE_, %s\n",
		   reg_names[PIC_OFFSET_TABLE_REGNUM]);
      asm_fprintf (stream, "\tlea %Rpc@(0,%s:l),%s\n",
		   reg_names[PIC_OFFSET_TABLE_REGNUM],
		   reg_names[PIC_OFFSET_TABLE_REGNUM]);
#endif
    }
}

/* Return true if this function's epilogue can be output as RTL.  */

int
use_return_insn ()
{
  int regno;

  if (!reload_completed || frame_pointer_needed || get_frame_size () != 0)
    return 0;
  
  /* Copied from output_function_epilogue ().  We should probably create a
     separate layout routine to perform the common work.  */
  
  for (regno = 0 ; regno < FIRST_PSEUDO_REGISTER ; regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      return 0;
  
  return 1;
}

/* This function generates the assembly code for function exit,
   on machines that need it.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only, if there is a frame pointer.
   This is mandatory because of alloca; we also take advantage of it to
   omit stack adjustments before returning.  */

void
output_function_epilogue (stream, size)
     FILE *stream;
     int size;
{
  register int regno;
  register int mask, fmask;
  register int nregs;
  int offset, foffset, fpoffset;
  extern char call_used_regs[];
  int fsize = (size + 3) & -4;
  int big = 0;
  rtx insn = get_last_insn ();
  int restore_from_sp = 0;
  
  /* If the last insn was a BARRIER, we don't have to write any code.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn && GET_CODE (insn) == BARRIER)
    {
      /* Output just a no-op so that debuggers don't get confused
	 about which function the pc is in at this address.  */
      asm_fprintf (stream, "\tnop\n");
      return;
    }

#ifdef FUNCTION_BLOCK_PROFILER_EXIT
  if (profile_block_flag == 2)
    {
      FUNCTION_BLOCK_PROFILER_EXIT (stream);
    }
#endif

#ifdef FUNCTION_EXTRA_EPILOGUE
  FUNCTION_EXTRA_EPILOGUE (stream, size);
#endif
  nregs = 0;  fmask = 0; fpoffset = 0;
#ifdef SUPPORT_SUN_FPA
  for (regno = 24 ; regno < 56 ; regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      nregs++;
  fpoffset = nregs * 8;
#endif
  nregs = 0;
  if (TARGET_68881)
    {
      for (regno = 16; regno < 24; regno++)
	if (regs_ever_live[regno] && ! call_used_regs[regno])
	  {
	    nregs++;
	    fmask |= 1 << (23 - regno);
	  }
    }
  foffset = fpoffset + nregs * 12;
  nregs = 0;  mask = 0;
  if (frame_pointer_needed)
    regs_ever_live[FRAME_POINTER_REGNUM] = 0;
  for (regno = 0; regno < 16; regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
        nregs++;
	mask |= 1 << regno;
      }
  if (flag_pic && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
    {
      nregs++;
      mask |= 1 << PIC_OFFSET_TABLE_REGNUM;
    }
  offset = foffset + nregs * 4;
  /* FIXME : leaf_function_p below is too strong.
     What we really need to know there is if there could be pending
     stack adjustment needed at that point. */
  restore_from_sp = ! frame_pointer_needed
	     || (! current_function_calls_alloca && leaf_function_p ());
  if (offset + fsize >= 0x8000
      && ! restore_from_sp
      && (mask || fmask || fpoffset))
    {
#ifdef MOTOROLA
      asm_fprintf (stream, "\t%Omove.l %0I%d,%Ra1\n", -fsize);
#else
      asm_fprintf (stream, "\tmovel %0I%d,%Ra1\n", -fsize);
#endif
      fsize = 0, big = 1;
    }
  if (TARGET_5200 || nregs <= 2)
    {
      /* Restore each separately in the same order moveml does.
         Using two movel instructions instead of a single moveml
         is about 15% faster for the 68020 and 68030 at no expense
         in code size. */

      int i;

      /* Undo the work from above. */
      for (i = 0; i< 16; i++)
        if (mask & (1 << i))
          {
            if (big)
	      {
#ifdef MOTOROLA
		asm_fprintf (stream, "\t%Omove.l -%d(%s,%Ra1.l),%s\n",
			     offset + fsize,
			     reg_names[FRAME_POINTER_REGNUM],
			     reg_names[i]);
#else
		asm_fprintf (stream, "\tmovel %s@(-%d,%Ra1:l),%s\n",
			     reg_names[FRAME_POINTER_REGNUM],
			     offset + fsize, reg_names[i]);
#endif
	      }
            else if (restore_from_sp)
	      {
#ifdef MOTOROLA
		asm_fprintf (stream, "\t%Omove.l (%Rsp)+,%s\n",
			     reg_names[i]);
#else
		asm_fprintf (stream, "\tmovel %Rsp@+,%s\n",
			     reg_names[i]);
#endif
	      }
            else
	      {
#ifdef MOTOROLA
		asm_fprintf (stream, "\t%Omove.l -%d(%s),%s\n",
			     offset + fsize,
			     reg_names[FRAME_POINTER_REGNUM],
			     reg_names[i]);
#else
		asm_fprintf (stream, "\tmovel %s@(-%d),%s\n",
			     reg_names[FRAME_POINTER_REGNUM],
			     offset + fsize, reg_names[i]);
#endif
	      }
            offset = offset - 4;
          }
    }
  else if (mask)
    {
      if (big)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tmovm.l -%d(%s,%Ra1.l),%0I0x%x\n",
		       offset + fsize,
		       reg_names[FRAME_POINTER_REGNUM],
		       mask);
#else
	  asm_fprintf (stream, "\tmoveml %s@(-%d,%Ra1:l),%0I0x%x\n",
		       reg_names[FRAME_POINTER_REGNUM],
		       offset + fsize, mask);
#endif
	}
      else if (restore_from_sp)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tmovm.l (%Rsp)+,%0I0x%x\n", mask);
#else
	  asm_fprintf (stream, "\tmoveml %Rsp@+,%0I0x%x\n", mask);
#endif
	}
      else
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tmovm.l -%d(%s),%0I0x%x\n",
		       offset + fsize,
		       reg_names[FRAME_POINTER_REGNUM],
		       mask);
#else
	  asm_fprintf (stream, "\tmoveml %s@(-%d),%0I0x%x\n",
		       reg_names[FRAME_POINTER_REGNUM],
		       offset + fsize, mask);
#endif
	}
    }
  if (fmask)
    {
      if (big)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tfmovm -%d(%s,%Ra1.l),%0I0x%x\n",
		       foffset + fsize,
		       reg_names[FRAME_POINTER_REGNUM],
		       fmask);
#else
	  asm_fprintf (stream, "\tfmovem %s@(-%d,%Ra1:l),%0I0x%x\n",
		       reg_names[FRAME_POINTER_REGNUM],
		       foffset + fsize, fmask);
#endif
	}
      else if (restore_from_sp)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tfmovm (%Rsp)+,%0I0x%x\n", fmask);
#else
	  asm_fprintf (stream, "\tfmovem %Rsp@+,%0I0x%x\n", fmask);
#endif
	}
      else
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tfmovm -%d(%s),%0I0x%x\n",
		       foffset + fsize,
		       reg_names[FRAME_POINTER_REGNUM],
		       fmask);
#else
	  asm_fprintf (stream, "\tfmovem %s@(-%d),%0I0x%x\n",
		       reg_names[FRAME_POINTER_REGNUM],
		       foffset + fsize, fmask);
#endif
	}
    }
  if (fpoffset != 0)
    for (regno = 55; regno >= 24; regno--)
      if (regs_ever_live[regno] && ! call_used_regs[regno])
        {
	  if (big)
	    {
#ifdef MOTOROLA
	      asm_fprintf (stream, "\tfpmovd -%d(%s,%Ra1.l), %s\n",
			   fpoffset + fsize,
			   reg_names[FRAME_POINTER_REGNUM],
			   reg_names[regno]);
#else
	      asm_fprintf (stream, "\tfpmoved %s@(-%d,%Ra1:l), %s\n",
			   reg_names[FRAME_POINTER_REGNUM],
			   fpoffset + fsize, reg_names[regno]);
#endif
	    }
	  else if (restore_from_sp)
	    {
#ifdef MOTOROLA
	      asm_fprintf (stream, "\tfpmovd (%Rsp)+,%s\n",
			   reg_names[regno]);
#else
	      asm_fprintf (stream, "\tfpmoved %Rsp@+, %s\n",
			   reg_names[regno]);
#endif
	    }
	  else
	    {
#ifdef MOTOROLA
	      asm_fprintf (stream, "\tfpmovd -%d(%s), %s\n",
			   fpoffset + fsize,
			   reg_names[FRAME_POINTER_REGNUM],
			   reg_names[regno]);
#else
	      asm_fprintf (stream, "\tfpmoved %s@(-%d), %s\n",
			   reg_names[FRAME_POINTER_REGNUM],
			   fpoffset + fsize, reg_names[regno]);
#endif
	    }
	  fpoffset -= 8;
	}
  if (frame_pointer_needed)
    fprintf (stream, "\tunlk %s\n",
	     reg_names[FRAME_POINTER_REGNUM]);
  else if (fsize)
    {
#ifndef NO_ADDSUB_Q
      if (fsize + 4 <= 8) 
	{
	  if (!TARGET_5200)
	    {
#ifdef MOTOROLA
	      asm_fprintf (stream, "\taddq.w %0I%d,%Rsp\n", fsize + 4);
#else
	      asm_fprintf (stream, "\taddqw %0I%d,%Rsp\n", fsize + 4);
#endif
	    }
	  else
	    {
#ifdef MOTOROLA
	      asm_fprintf (stream, "\taddq.l %0I%d,%Rsp\n", fsize + 4);
#else
	      asm_fprintf (stream, "\taddql %0I%d,%Rsp\n", fsize + 4);
#endif
	    }
	}
      else if (fsize + 4 <= 16 && TARGET_CPU32)
	{
	  /* On the CPU32 it is faster to use two addqw instructions to
	     add a small integer (8 < N <= 16) to a register. */
	  /* asm_fprintf() cannot handle %. */
#ifdef MOTOROLA
	  asm_fprintf (stream, "\taddq.w %0I8,%Rsp\n\taddq.w %0I%d,%Rsp\n",
		       fsize + 4 - 8);
#else
	  asm_fprintf (stream, "\taddqw %0I8,%Rsp\n\taddqw %0I%d,%Rsp\n",
		       fsize + 4 - 8);
#endif
	}
      else
#endif /* not NO_ADDSUB_Q */
      if (fsize + 4 < 0x8000)
	{
	  if (TARGET_68040)
	    { 
	      /* asm_fprintf() cannot handle %. */
#ifdef MOTOROLA
	      asm_fprintf (stream, "\tadd.w %0I%d,%Rsp\n", fsize + 4);
#else
	      asm_fprintf (stream, "\taddw %0I%d,%Rsp\n", fsize + 4);
#endif
	    }
	  else
	    {
#ifdef MOTOROLA
	      asm_fprintf (stream, "\tlea (%d,%Rsp),%Rsp\n", fsize + 4);
#else
	      asm_fprintf (stream, "\tlea %Rsp@(%d),%Rsp\n", fsize + 4);
#endif
	    }
	}
      else
	{
	/* asm_fprintf() cannot handle %. */
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tadd.l %0I%d,%Rsp\n", fsize + 4);
#else
	  asm_fprintf (stream, "\taddl %0I%d,%Rsp\n", fsize + 4);
#endif
	}
    }
  if (current_function_pops_args)
    asm_fprintf (stream, "\trtd %0I%d\n", current_function_pops_args);
  else
    fprintf (stream, "\trts\n");
}

/* Similar to general_operand, but exclude stack_pointer_rtx.  */

int
not_sp_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return op != stack_pointer_rtx && general_operand (op, mode);
}

/* Return TRUE if X is a valid comparison operator for the dbcc 
   instruction.  

   Note it rejects floating point comparison operators.
   (In the future we could use Fdbcc).

   It also rejects some comparisons when CC_NO_OVERFLOW is set.  */
   
int
valid_dbcc_comparison_p (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (x))
    {
      case EQ: case NE: case GTU: case LTU:
      case GEU: case LEU:
        return 1;

      /* Reject some when CC_NO_OVERFLOW is set.  This may be over
         conservative */
      case GT: case LT: case GE: case LE:
        return ! (cc_prev_status.flags & CC_NO_OVERFLOW);
      default:
        return 0;
    }
}

/* Return non-zero if flags are currently in the 68881 flag register.  */
int
flags_in_68881 ()
{
  /* We could add support for these in the future */
  return cc_status.flags & CC_IN_68881;
}

/* Output a dbCC; jCC sequence.  Note we do not handle the 
   floating point version of this sequence (Fdbcc).  We also
   do not handle alternative conditions when CC_NO_OVERFLOW is
   set.  It is assumed that valid_dbcc_comparison_p and flags_in_68881 will
   kick those out before we get here.  */

void
output_dbcc_and_branch (operands)
     rtx *operands;
{
  switch (GET_CODE (operands[3]))
    {
      case EQ:
#ifdef MOTOROLA
        output_asm_insn ("dbeq %0,%l1\n\tjbeq %l2", operands);
#else
        output_asm_insn ("dbeq %0,%l1\n\tjeq %l2", operands);
#endif
        break;

      case NE:
#ifdef MOTOROLA
        output_asm_insn ("dbne %0,%l1\n\tjbne %l2", operands);
#else
        output_asm_insn ("dbne %0,%l1\n\tjne %l2", operands);
#endif
        break;

      case GT:
#ifdef MOTOROLA
        output_asm_insn ("dbgt %0,%l1\n\tjbgt %l2", operands);
#else
        output_asm_insn ("dbgt %0,%l1\n\tjgt %l2", operands);
#endif
        break;

      case GTU:
#ifdef MOTOROLA
        output_asm_insn ("dbhi %0,%l1\n\tjbhi %l2", operands);
#else
        output_asm_insn ("dbhi %0,%l1\n\tjhi %l2", operands);
#endif
        break;

      case LT:
#ifdef MOTOROLA
        output_asm_insn ("dblt %0,%l1\n\tjblt %l2", operands);
#else
        output_asm_insn ("dblt %0,%l1\n\tjlt %l2", operands);
#endif
        break;

      case LTU:
#ifdef MOTOROLA
        output_asm_insn ("dbcs %0,%l1\n\tjbcs %l2", operands);
#else
        output_asm_insn ("dbcs %0,%l1\n\tjcs %l2", operands);
#endif
        break;

      case GE:
#ifdef MOTOROLA
        output_asm_insn ("dbge %0,%l1\n\tjbge %l2", operands);
#else
        output_asm_insn ("dbge %0,%l1\n\tjge %l2", operands);
#endif
        break;

      case GEU:
#ifdef MOTOROLA
        output_asm_insn ("dbcc %0,%l1\n\tjbcc %l2", operands);
#else
        output_asm_insn ("dbcc %0,%l1\n\tjcc %l2", operands);
#endif
        break;

      case LE:
#ifdef MOTOROLA
        output_asm_insn ("dble %0,%l1\n\tjble %l2", operands);
#else
        output_asm_insn ("dble %0,%l1\n\tjle %l2", operands);
#endif
        break;

      case LEU:
#ifdef MOTOROLA
        output_asm_insn ("dbls %0,%l1\n\tjbls %l2", operands);
#else
        output_asm_insn ("dbls %0,%l1\n\tjls %l2", operands);
#endif
        break;

      default:
	abort ();
    }

  /* If the decrement is to be done in SImode, then we have
     to compensate for the fact that dbcc decrements in HImode. */
  switch (GET_MODE (operands[0]))
    {
      case SImode:
#ifdef MOTOROLA
        output_asm_insn ("clr%.w %0\n\tsubq%.l %#1,%0\n\tjbpl %l1", operands);
#else
        output_asm_insn ("clr%.w %0\n\tsubq%.l %#1,%0\n\tjpl %l1", operands);
#endif
        break;

      case HImode:
        break;

      default:
        abort ();
    }
}

char *
output_scc_di(op, operand1, operand2, dest)
     rtx op;
     rtx operand1;
     rtx operand2;
     rtx dest;
{
  rtx loperands[7];
  enum rtx_code op_code = GET_CODE (op);

  /* This does not produce a usefull cc.  */
  CC_STATUS_INIT;

  /* The m68k cmp.l instruction requires operand1 to be a reg as used
     below.  Swap the operands and change the op if these requirements
     are not fulfilled.  */
  if (GET_CODE (operand2) == REG && GET_CODE (operand1) != REG)
    {
      rtx tmp = operand1;

      operand1 = operand2;
      operand2 = tmp;
      op_code = swap_condition (op_code);
    }
  loperands[0] = operand1;
  if (GET_CODE (operand1) == REG)
    loperands[1] = gen_rtx_REG (SImode, REGNO (operand1) + 1);
  else
    loperands[1] = adj_offsettable_operand (operand1, 4);
  if (operand2 != const0_rtx)
    {
      loperands[2] = operand2;
      if (GET_CODE (operand2) == REG)
	loperands[3] = gen_rtx_REG (SImode, REGNO (operand2) + 1);
      else
	loperands[3] = adj_offsettable_operand (operand2, 4);
    }
  loperands[4] = gen_label_rtx();
  if (operand2 != const0_rtx)
    {
#ifdef MOTOROLA
#ifdef SGS_CMP_ORDER
      output_asm_insn ("cmp%.l %0,%2\n\tjbne %l4\n\tcmp%.l %1,%3", loperands);
#else
      output_asm_insn ("cmp%.l %2,%0\n\tjbne %l4\n\tcmp%.l %3,%1", loperands);
#endif
#else
#ifdef SGS_CMP_ORDER
      output_asm_insn ("cmp%.l %0,%2\n\tjne %l4\n\tcmp%.l %1,%3", loperands);
#else
      output_asm_insn ("cmp%.l %2,%0\n\tjne %l4\n\tcmp%.l %3,%1", loperands);
#endif
#endif
    }
  else
    {
      if (TARGET_68020 || TARGET_5200 || ! ADDRESS_REG_P (loperands[0]))
	output_asm_insn ("tst%.l %0", loperands);
      else
	{
#ifdef SGS_CMP_ORDER
	  output_asm_insn ("cmp%.w %0,%#0", loperands);
#else
	  output_asm_insn ("cmp%.w %#0,%0", loperands);
#endif
	}

#ifdef MOTOROLA
      output_asm_insn ("jbne %l4", loperands);
#else
      output_asm_insn ("jne %l4", loperands);
#endif

      if (TARGET_68020 || TARGET_5200 || ! ADDRESS_REG_P (loperands[1]))
	output_asm_insn ("tst%.l %1", loperands);
      else
	{
#ifdef SGS_CMP_ORDER
	  output_asm_insn ("cmp%.w %1,%#0", loperands);
#else
	  output_asm_insn ("cmp%.w %#0,%1", loperands);
#endif
	}
    }

  loperands[5] = dest;
  
  switch (op_code)
    {
      case EQ:
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("seq %5", loperands);
        break;

      case NE:
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sne %5", loperands);
        break;

      case GT:
        loperands[6] = gen_label_rtx();
#ifdef MOTOROLA
        output_asm_insn ("shi %5\n\tjbra %l6", loperands);
#else
        output_asm_insn ("shi %5\n\tjra %l6", loperands);
#endif
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sgt %5", loperands);
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[6]));
        break;

      case GTU:
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("shi %5", loperands);
        break;

      case LT:
        loperands[6] = gen_label_rtx();
#ifdef MOTOROLA
        output_asm_insn ("scs %5\n\tjbra %l6", loperands);
#else
        output_asm_insn ("scs %5\n\tjra %l6", loperands);
#endif
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("slt %5", loperands);
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[6]));
        break;

      case LTU:
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("scs %5", loperands);
        break;

      case GE:
        loperands[6] = gen_label_rtx();
#ifdef MOTOROLA
        output_asm_insn ("scc %5\n\tjbra %l6", loperands);
#else
        output_asm_insn ("scc %5\n\tjra %l6", loperands);
#endif
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sge %5", loperands);
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[6]));
        break;

      case GEU:
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("scc %5", loperands);
        break;

      case LE:
        loperands[6] = gen_label_rtx();
#ifdef MOTOROLA
        output_asm_insn ("sls %5\n\tjbra %l6", loperands);
#else
        output_asm_insn ("sls %5\n\tjra %l6", loperands);
#endif
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sle %5", loperands);
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[6]));
        break;

      case LEU:
        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				    CODE_LABEL_NUMBER (loperands[4]));
        output_asm_insn ("sls %5", loperands);
        break;

      default:
	abort ();
    }
  return "";
}

char *
output_btst (operands, countop, dataop, insn, signpos)
     rtx *operands;
     rtx countop, dataop;
     rtx insn;
     int signpos;
{
  operands[0] = countop;
  operands[1] = dataop;

  if (GET_CODE (countop) == CONST_INT)
    {
      register int count = INTVAL (countop);
      /* If COUNT is bigger than size of storage unit in use,
	 advance to the containing unit of same size.  */
      if (count > signpos)
	{
	  int offset = (count & ~signpos) / 8;
	  count = count & signpos;
	  operands[1] = dataop = adj_offsettable_operand (dataop, offset);
	}
      if (count == signpos)
	cc_status.flags = CC_NOT_POSITIVE | CC_Z_IN_NOT_N;
      else
	cc_status.flags = CC_NOT_NEGATIVE | CC_Z_IN_NOT_N;

      /* These three statements used to use next_insns_test_no...
	 but it appears that this should do the same job.  */
      if (count == 31
	  && next_insn_tests_no_inequality (insn))
	return "tst%.l %1";
      if (count == 15
	  && next_insn_tests_no_inequality (insn))
	return "tst%.w %1";
      if (count == 7
	  && next_insn_tests_no_inequality (insn))
	return "tst%.b %1";

      cc_status.flags = CC_NOT_NEGATIVE;
    }
  return "btst %0,%1";
}

/* Returns 1 if OP is either a symbol reference or a sum of a symbol
   reference and a constant.  */

int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);

#if 0 /* Deleted, with corresponding change in m68k.h,
	 so as to fit the specs.  No CONST_DOUBLE is ever symbolic.  */
    case CONST_DOUBLE:
      return GET_MODE (op) == mode;
#endif

    default:
      return 0;
    }
}

/* Check for sign_extend or zero_extend.  Used for bit-count operands. */

int
extend_operator(x, mode)
     rtx x;
     enum machine_mode mode;
{
    if (mode != VOIDmode && GET_MODE(x) != mode)
	return 0;
    switch (GET_CODE(x))
	{
	case SIGN_EXTEND :
	case ZERO_EXTEND :
	    return 1;
	default :
	    return 0;
	}
}


/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go to REG.  If we need more
   than one register, we lose.  

   An address is legitimized by making an indirect reference
   through the Global Offset Table with the name of the symbol
   used as an offset.  

   The assembler and linker are responsible for placing the 
   address of the symbol in the GOT.  The function prologue
   is responsible for initializing a5 to the starting address
   of the GOT.

   The assembler is also responsible for translating a symbol name
   into a constant displacement from the start of the GOT.  

   A quick example may make things a little clearer:

   When not generating PIC code to store the value 12345 into _foo
   we would generate the following code:

	movel #12345, _foo

   When generating PIC two transformations are made.  First, the compiler
   loads the address of foo into a register.  So the first transformation makes:

	lea	_foo, a0
	movel   #12345, a0@

   The code in movsi will intercept the lea instruction and call this
   routine which will transform the instructions into:

	movel   a5@(_foo:w), a0
	movel   #12345, a0@
   

   That (in a nutshell) is how *all* symbol and label references are 
   handled.  */

rtx
legitimize_pic_address (orig, mode, reg)
     rtx orig, reg;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx pic_ref = orig;

  /* First handle a simple SYMBOL_REF or LABEL_REF */
  if (GET_CODE (orig) == SYMBOL_REF || GET_CODE (orig) == LABEL_REF)
    {
      if (reg == 0)
	abort ();

      pic_ref = gen_rtx_MEM (Pmode,
			     gen_rtx_PLUS (Pmode,
					   pic_offset_table_rtx, orig));
      current_function_uses_pic_offset_table = 1;
      if (reload_in_progress)
	regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;
      RTX_UNCHANGING_P (pic_ref) = 1;
      emit_move_insn (reg, pic_ref);
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base;

      /* Make sure this is CONST has not already been legitimized */
      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	abort ();

      /* legitimize both operands of the PLUS */
      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  orig = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					 base == reg ? 0 : reg);
	}
      else abort ();

      if (GET_CODE (orig) == CONST_INT)
	return plus_constant_for_output (base, INTVAL (orig));
      pic_ref = gen_rtx_PLUS (Pmode, base, orig);
      /* Likewise, should we set special REG_NOTEs here?  */
    }
  return pic_ref;
}


typedef enum { MOVL, SWAP, NEGW, NOTW, NOTB, MOVQ } CONST_METHOD;

#define USE_MOVQ(i)	((unsigned)((i) + 128) <= 255)

CONST_METHOD
const_method (constant)
     rtx constant;
{
  int i;
  unsigned u;

  i = INTVAL (constant);
  if (USE_MOVQ (i))
    return MOVQ;

  /* The Coldfire doesn't have byte or word operations. */
  /* FIXME: This may not be useful for the m68060 either */
  if (!TARGET_5200) 
    {
      /* if -256 < N < 256 but N is not in range for a moveq
	 N^ff will be, so use moveq #N^ff, dreg; not.b dreg. */
      if (USE_MOVQ (i ^ 0xff))
	return NOTB;
      /* Likewise, try with not.w */
      if (USE_MOVQ (i ^ 0xffff))
	return NOTW;
      /* This is the only value where neg.w is useful */
      if (i == -65408)
	return NEGW;
      /* Try also with swap */
      u = i;
      if (USE_MOVQ ((u >> 16) | (u << 16)))
	return SWAP;
    }
  /* Otherwise, use move.l */
  return MOVL;
}

int
const_int_cost (constant)
     rtx constant;
{
  switch (const_method (constant))
    {
      case MOVQ :
      /* Constants between -128 and 127 are cheap due to moveq */
	return 0;
      case NOTB :
      case NOTW :
      case NEGW :
      case SWAP :
      /* Constants easily generated by moveq + not.b/not.w/neg.w/swap  */
        return 1;
      case MOVL :
	return 2;
      default :
        abort ();
    }
}

char *
output_move_const_into_data_reg (operands)
     rtx *operands;
{
  int i;

  i = INTVAL (operands[1]);
  switch (const_method (operands[1]))
    {
    case MOVQ :
#if defined (MOTOROLA) && !defined (CRDS)
      return "moveq%.l %1,%0";
#else
      return "moveq %1,%0";
#endif
    case NOTB :
      operands[1] = GEN_INT (i ^ 0xff);
#if defined (MOTOROLA) && !defined (CRDS)
      return "moveq%.l %1,%0\n\tnot%.b %0";
#else
      return "moveq %1,%0\n\tnot%.b %0";
#endif	 
    case NOTW :
      operands[1] = GEN_INT (i ^ 0xffff);
#if defined (MOTOROLA) && !defined (CRDS)
      return "moveq%.l %1,%0\n\tnot%.w %0";
#else
      return "moveq %1,%0\n\tnot%.w %0";
#endif	 
    case NEGW :
#if defined (MOTOROLA) && !defined (CRDS)
      return "moveq%.l %#-128,%0\n\tneg%.w %0";
#else
      return "moveq %#-128,%0\n\tneg%.w %0";
#endif	 
    case SWAP :
      {
	unsigned u = i;

	operands[1] = GEN_INT ((u << 16) | (u >> 16));
#if defined (MOTOROLA) && !defined (CRDS)
	return "moveq%.l %1,%0\n\tswap %0";
#else
	return "moveq %1,%0\n\tswap %0";
#endif	 
      }
    case MOVL :
	return "move%.l %1,%0";
    default :
	abort ();
    }
}

char *
output_move_simode_const (operands)
     rtx *operands;
{
  if (operands[1] == const0_rtx
      && (DATA_REG_P (operands[0])
	  || GET_CODE (operands[0]) == MEM)
      /* clr insns on 68000 read before writing.
	 This isn't so on the 68010, but we have no TARGET_68010.  */
      && ((TARGET_68020 || TARGET_5200)
	  || !(GET_CODE (operands[0]) == MEM
	       && MEM_VOLATILE_P (operands[0]))))
    return "clr%.l %0";
  else if (operands[1] == const0_rtx
	   && ADDRESS_REG_P (operands[0]))
    return "sub%.l %0,%0";
  else if (DATA_REG_P (operands[0]))
    return output_move_const_into_data_reg (operands);
  else if (ADDRESS_REG_P (operands[0])
	   && INTVAL (operands[1]) < 0x8000
	   && INTVAL (operands[1]) >= -0x8000)
    return "move%.w %1,%0";
  else if (GET_CODE (operands[0]) == MEM
      && GET_CODE (XEXP (operands[0], 0)) == PRE_DEC
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
	   && INTVAL (operands[1]) < 0x8000
	   && INTVAL (operands[1]) >= -0x8000)
    return "pea %a1";
  return "move%.l %1,%0";
}

char *
output_move_simode (operands)
     rtx *operands;
{
  if (GET_CODE (operands[1]) == CONST_INT)
    return output_move_simode_const (operands);
  else if ((GET_CODE (operands[1]) == SYMBOL_REF
	    || GET_CODE (operands[1]) == CONST)
	   && push_operand (operands[0], SImode))
    return "pea %a1";
  else if ((GET_CODE (operands[1]) == SYMBOL_REF
	    || GET_CODE (operands[1]) == CONST)
	   && ADDRESS_REG_P (operands[0]))
    return "lea %a1,%0";
  return "move%.l %1,%0";
}

char *
output_move_himode (operands)
     rtx *operands;
{
 if (GET_CODE (operands[1]) == CONST_INT)
    {
      if (operands[1] == const0_rtx
	  && (DATA_REG_P (operands[0])
	      || GET_CODE (operands[0]) == MEM)
	  /* clr insns on 68000 read before writing.
	     This isn't so on the 68010, but we have no TARGET_68010.  */
	  && ((TARGET_68020 || TARGET_5200)
	      || !(GET_CODE (operands[0]) == MEM
		   && MEM_VOLATILE_P (operands[0]))))
	return "clr%.w %0";
      else if (operands[1] == const0_rtx
	       && ADDRESS_REG_P (operands[0]))
	return "sub%.l %0,%0";
      else if (DATA_REG_P (operands[0])
	       && INTVAL (operands[1]) < 128
	       && INTVAL (operands[1]) >= -128)
	{
#if defined(MOTOROLA) && !defined(CRDS)
	  return "moveq%.l %1,%0";
#else
	  return "moveq %1,%0";
#endif
	}
      else if (INTVAL (operands[1]) < 0x8000
	       && INTVAL (operands[1]) >= -0x8000)
	return "move%.w %1,%0";
    }
  else if (CONSTANT_P (operands[1]))
    return "move%.l %1,%0";
#ifndef SGS_NO_LI
  /* Recognize the insn before a tablejump, one that refers
     to a table of offsets.  Such an insn will need to refer
     to a label on the insn.  So output one.  Use the label-number
     of the table of offsets to generate this label.  This code,
     and similar code below, assumes that there will be at most one
     reference to each table.  */
  if (GET_CODE (operands[1]) == MEM
      && GET_CODE (XEXP (operands[1], 0)) == PLUS
      && GET_CODE (XEXP (XEXP (operands[1], 0), 1)) == LABEL_REF
      && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) != PLUS)
    {
      rtx labelref = XEXP (XEXP (operands[1], 0), 1);
#if defined (MOTOROLA) && !defined (SGS_SWITCH_TABLES)
#ifdef SGS
      asm_fprintf (asm_out_file, "\tset %LLI%d,.+2\n",
		   CODE_LABEL_NUMBER (XEXP (labelref, 0)));
#else /* not SGS */
      asm_fprintf (asm_out_file, "\t.set %LLI%d,.+2\n",
		   CODE_LABEL_NUMBER (XEXP (labelref, 0)));
#endif /* not SGS */
#else /* SGS_SWITCH_TABLES or not MOTOROLA */
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LI",
				 CODE_LABEL_NUMBER (XEXP (labelref, 0)));
#ifdef SGS_SWITCH_TABLES
      /* Set flag saying we need to define the symbol
	 LD%n (with value L%n-LI%n) at the end of the switch table.  */
      switch_table_difference_label_flag = 1;
#endif /* SGS_SWITCH_TABLES */
#endif /* SGS_SWITCH_TABLES or not MOTOROLA */
    }
#endif /* SGS_NO_LI */
  return "move%.w %1,%0";
}

char *
output_move_qimode (operands)
     rtx *operands;
{
  rtx xoperands[4];

  /* This is probably useless, since it loses for pushing a struct
     of several bytes a byte at a time.	 */
  /* 68k family always modifies the stack pointer by at least 2, even for
     byte pushes.  The 5200 (coldfire) does not do this.  */
  if (GET_CODE (operands[0]) == MEM
      && GET_CODE (XEXP (operands[0], 0)) == PRE_DEC
      && XEXP (XEXP (operands[0], 0), 0) == stack_pointer_rtx
      && ! ADDRESS_REG_P (operands[1])
      && ! TARGET_5200)
    {
      xoperands[1] = operands[1];
      xoperands[2]
	= gen_rtx_MEM (QImode,
		       gen_rtx_PLUS (VOIDmode, stack_pointer_rtx, const1_rtx));
      /* Just pushing a byte puts it in the high byte of the halfword.	*/
      /* We must put it in the low-order, high-numbered byte.  */
      if (!reg_mentioned_p (stack_pointer_rtx, operands[1]))
	{
	  xoperands[3] = stack_pointer_rtx;
#ifndef NO_ADDSUB_Q
	  output_asm_insn ("subq%.l %#2,%3\n\tmove%.b %1,%2", xoperands);
#else
	  output_asm_insn ("sub%.l %#2,%3\n\tmove%.b %1,%2", xoperands);
#endif
	}
      else
	output_asm_insn ("move%.b %1,%-\n\tmove%.b %@,%2", xoperands);
      return "";
    }

  /* clr and st insns on 68000 read before writing.
     This isn't so on the 68010, but we have no TARGET_68010.  */
  if (!ADDRESS_REG_P (operands[0])
      && ((TARGET_68020 || TARGET_5200)
	  || !(GET_CODE (operands[0]) == MEM && MEM_VOLATILE_P (operands[0]))))
    {
      if (operands[1] == const0_rtx)
	return "clr%.b %0";
      if ((!TARGET_5200 || DATA_REG_P (operands[0]))
	  && GET_CODE (operands[1]) == CONST_INT
	  && (INTVAL (operands[1]) & 255) == 255)
	{
	  CC_STATUS_INIT;
	  return "st %0";
	}
    }
  if (GET_CODE (operands[1]) == CONST_INT
      && DATA_REG_P (operands[0])
      && INTVAL (operands[1]) < 128
      && INTVAL (operands[1]) >= -128)
    {
#if defined(MOTOROLA) && !defined(CRDS)
      return "moveq%.l %1,%0";
#else
      return "moveq %1,%0";
#endif
    }
  if (operands[1] == const0_rtx && ADDRESS_REG_P (operands[0]))
    return "sub%.l %0,%0";
  if (GET_CODE (operands[1]) != CONST_INT && CONSTANT_P (operands[1]))
    return "move%.l %1,%0";
  /* 68k family (including the 5200 coldfire) does not support byte moves to
     from address registers.  */
  if (ADDRESS_REG_P (operands[0]) || ADDRESS_REG_P (operands[1]))
    return "move%.w %1,%0";
  return "move%.b %1,%0";
}

char *
output_move_stricthi (operands)
     rtx *operands;
{
  if (operands[1] == const0_rtx
      /* clr insns on 68000 read before writing.
	 This isn't so on the 68010, but we have no TARGET_68010.  */
      && ((TARGET_68020 || TARGET_5200)
	  || !(GET_CODE (operands[0]) == MEM && MEM_VOLATILE_P (operands[0]))))
    return "clr%.w %0";
  return "move%.w %1,%0";
}

char *
output_move_strictqi (operands)
     rtx *operands;
{
  if (operands[1] == const0_rtx
      /* clr insns on 68000 read before writing.
         This isn't so on the 68010, but we have no TARGET_68010.  */
      && ((TARGET_68020 || TARGET_5200)
          || !(GET_CODE (operands[0]) == MEM && MEM_VOLATILE_P (operands[0]))))
    return "clr%.b %0";
  return "move%.b %1,%0";
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static char *
singlemove_string (operands)
     rtx *operands;
{
#ifdef SUPPORT_SUN_FPA
  if (FPA_REG_P (operands[0]) || FPA_REG_P (operands[1]))
    return "fpmoves %1,%0";
#endif
  if (GET_CODE (operands[1]) == CONST_INT)
    return output_move_simode_const (operands);
  return "move%.l %1,%0";
}


/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum
    {
      REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP
    } optype0, optype1;
  rtx latehalf[2];
  rtx middlehalf[2];
  rtx xops[2];
  rtx addreg0 = 0, addreg1 = 0;
  int dest_overlapped_low = 0;
  int size = GET_MODE_SIZE (GET_MODE (operands[0]));

  middlehalf[0] = 0;
  middlehalf[1] = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == POST_INC)
    optype0 = POPOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
    optype0 = PUSHOP;
  else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1]))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == POST_INC)
    optype1 = POPOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)
    optype1 = PUSHOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

  /* If one operand is decrementing and one is incrementing
     decrement the former register explicitly
     and change that operand into ordinary indexing.  */

  if (optype0 == PUSHOP && optype1 == POPOP)
    {
      operands[0] = XEXP (XEXP (operands[0], 0), 0);
      if (size == 12)
        output_asm_insn ("sub%.l %#12,%0", operands);
      else
        output_asm_insn ("subq%.l %#8,%0", operands);
      if (GET_MODE (operands[1]) == XFmode)
	operands[0] = gen_rtx_MEM (XFmode, operands[0]);
      else if (GET_MODE (operands[0]) == DFmode)
	operands[0] = gen_rtx_MEM (DFmode, operands[0]);
      else
	operands[0] = gen_rtx_MEM (DImode, operands[0]);
      optype0 = OFFSOP;
    }
  if (optype0 == POPOP && optype1 == PUSHOP)
    {
      operands[1] = XEXP (XEXP (operands[1], 0), 0);
      if (size == 12)
        output_asm_insn ("sub%.l %#12,%1", operands);
      else
        output_asm_insn ("subq%.l %#8,%1", operands);
      if (GET_MODE (operands[1]) == XFmode)
	operands[1] = gen_rtx_MEM (XFmode, operands[1]);
      else if (GET_MODE (operands[1]) == DFmode)
	operands[1] = gen_rtx_MEM (DFmode, operands[1]);
      else
	operands[1] = gen_rtx_MEM (DImode, operands[1]);
      optype1 = OFFSOP;
    }

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first,
     but if either operand is autodecrementing then we
     do the high-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (size == 12)
    {
      if (optype0 == REGOP)
	{
	  latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 2);
	  middlehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
	}
      else if (optype0 == OFFSOP)
	{
	  middlehalf[0] = adj_offsettable_operand (operands[0], 4);
	  latehalf[0] = adj_offsettable_operand (operands[0], size - 4);
	}
      else
	{
	  middlehalf[0] = operands[0];
	  latehalf[0] = operands[0];
	}

      if (optype1 == REGOP)
	{
	  latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 2);
	  middlehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
	}
      else if (optype1 == OFFSOP)
	{
	  middlehalf[1] = adj_offsettable_operand (operands[1], 4);
	  latehalf[1] = adj_offsettable_operand (operands[1], size - 4);
	}
      else if (optype1 == CNSTOP)
	{
	  if (GET_CODE (operands[1]) == CONST_DOUBLE)
	    {
	      REAL_VALUE_TYPE r;
	      long l[3];

	      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
	      REAL_VALUE_TO_TARGET_LONG_DOUBLE (r, l);
	      operands[1] = GEN_INT (l[0]);
	      middlehalf[1] = GEN_INT (l[1]);
	      latehalf[1] = GEN_INT (l[2]);
	    }
	  else if (CONSTANT_P (operands[1]))
	    {
	      /* actually, no non-CONST_DOUBLE constant should ever
		 appear here.  */
	      abort ();
	      if (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) < 0)
		latehalf[1] = constm1_rtx;
	      else
		latehalf[1] = const0_rtx;
	    }
	}
      else
	{
	  middlehalf[1] = operands[1];
	  latehalf[1] = operands[1];
	}
    }
  else
    /* size is not 12: */
    {
      if (optype0 == REGOP)
	latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
      else if (optype0 == OFFSOP)
	latehalf[0] = adj_offsettable_operand (operands[0], size - 4);
      else
	latehalf[0] = operands[0];

      if (optype1 == REGOP)
	latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
      else if (optype1 == OFFSOP)
	latehalf[1] = adj_offsettable_operand (operands[1], size - 4);
      else if (optype1 == CNSTOP)
	split_double (operands[1], &operands[1], &latehalf[1]);
      else
	latehalf[1] = operands[1];
    }

  /* If insn is effectively movd N(sp),-(sp) then we will do the
     high word first.  We should use the adjusted operand 1 (which is N+4(sp))
     for the low word as well, to compensate for the first decrement of sp.  */
  if (optype0 == PUSHOP
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    operands[1] = middlehalf[1] = latehalf[1];

  /* For (set (reg:DI N) (mem:DI ... (reg:SI N) ...)),
     if the upper part of reg N does not appear in the MEM, arrange to
     emit the move late-half first.  Otherwise, compute the MEM address
     into the upper part of N and use that as a pointer to the memory
     operand.  */
  if (optype0 == REGOP
      && (optype1 == OFFSOP || optype1 == MEMOP))
    {
      rtx testlow = gen_rtx_REG (SImode, REGNO (operands[0]));

      if (reg_overlap_mentioned_p (testlow, XEXP (operands[1], 0))
	  && reg_overlap_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	{
	  /* If both halves of dest are used in the src memory address,
	     compute the address into latehalf of dest.
	     Note that this can't happen if the dest is two data regs.  */
compadr:
	  xops[0] = latehalf[0];
	  xops[1] = XEXP (operands[1], 0);
	  output_asm_insn ("lea %a1,%0", xops);
	  if( GET_MODE (operands[1]) == XFmode )
	    {
	      operands[1] = gen_rtx_MEM (XFmode, latehalf[0]);
	      middlehalf[1] = adj_offsettable_operand (operands[1], size-8);
	      latehalf[1] = adj_offsettable_operand (operands[1], size-4);
	    }
	  else
	    {
	      operands[1] = gen_rtx_MEM (DImode, latehalf[0]);
	      latehalf[1] = adj_offsettable_operand (operands[1], size-4);
	    }
	}
      else if (size == 12
	       && reg_overlap_mentioned_p (middlehalf[0],
					   XEXP (operands[1], 0)))
	{
	  /* Check for two regs used by both source and dest.
	     Note that this can't happen if the dest is all data regs.
	     It can happen if the dest is d6, d7, a0.
	     But in that case, latehalf is an addr reg, so
	     the code at compadr does ok.  */

	  if (reg_overlap_mentioned_p (testlow, XEXP (operands[1], 0))
	      || reg_overlap_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	    goto compadr;

	  /* JRV says this can't happen: */
	  if (addreg0 || addreg1)
	    abort ();

	  /* Only the middle reg conflicts; simply put it last. */
	  output_asm_insn (singlemove_string (operands), operands);
	  output_asm_insn (singlemove_string (latehalf), latehalf);
	  output_asm_insn (singlemove_string (middlehalf), middlehalf);
	  return "";
	}
      else if (reg_overlap_mentioned_p (testlow, XEXP (operands[1], 0)))
	/* If the low half of dest is mentioned in the source memory
	   address, the arrange to emit the move late half first.  */
	dest_overlapped_low = 1;
    }

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */

  /* Likewise,  the first move would clobber the source of the second one,
     do them in the other order.  This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance.  */

  if (optype0 == PUSHOP || optype1 == PUSHOP
      || (optype0 == REGOP && optype1 == REGOP
	  && ((middlehalf[1] && REGNO (operands[0]) == REGNO (middlehalf[1]))
	      || REGNO (operands[0]) == REGNO (latehalf[1])))
      || dest_overlapped_low)
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	{
	  if (size == 12)
	    output_asm_insn ("addq%.l %#8,%0", &addreg0);
	  else
	    output_asm_insn ("addq%.l %#4,%0", &addreg0);
	}
      if (addreg1)
	{
	  if (size == 12)
	    output_asm_insn ("addq%.l %#8,%0", &addreg1);
	  else
	    output_asm_insn ("addq%.l %#4,%0", &addreg1);
	}

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("subq%.l %#4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("subq%.l %#4,%0", &addreg1);

      if (size == 12)
	{
	  output_asm_insn (singlemove_string (middlehalf), middlehalf);
	  if (addreg0)
	    output_asm_insn ("subq%.l %#4,%0", &addreg0);
	  if (addreg1)
	    output_asm_insn ("subq%.l %#4,%0", &addreg1);
	}

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Do the middle one of the three words for long double */
  if (size == 12)
    {
      if (addreg0)
	output_asm_insn ("addq%.l %#4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("addq%.l %#4,%0", &addreg1);

      output_asm_insn (singlemove_string (middlehalf), middlehalf);
    }

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("addq%.l %#4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("addq%.l %#4,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    {
      if (size == 12)
        output_asm_insn ("subq%.l %#8,%0", &addreg0);
      else
        output_asm_insn ("subq%.l %#4,%0", &addreg0);
    }
  if (addreg1)
    {
      if (size == 12)
        output_asm_insn ("subq%.l %#8,%0", &addreg1);
      else
        output_asm_insn ("subq%.l %#4,%0", &addreg1);
    }

  return "";
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG)
    return addr;
  abort ();
}

/* Output assembler code to perform a 32 bit 3 operand add.  */

char *
output_addsi3 (operands)
     rtx *operands;
{
  if (! operands_match_p (operands[0], operands[1]))
    {
      if (!ADDRESS_REG_P (operands[1]))
	{
	  rtx tmp = operands[1];

	  operands[1] = operands[2];
	  operands[2] = tmp;
	}

      /* These insns can result from reloads to access
	 stack slots over 64k from the frame pointer.  */
      if (GET_CODE (operands[2]) == CONST_INT
	  && INTVAL (operands[2]) + 0x8000 >= (unsigned) 0x10000)
        return "move%.l %2,%0\n\tadd%.l %1,%0";
#ifdef SGS
      if (GET_CODE (operands[2]) == REG)
	return "lea 0(%1,%2.l),%0";
      else
	return "lea %c2(%1),%0";
#else /* not SGS */
#ifdef MOTOROLA
      if (GET_CODE (operands[2]) == REG)
	return "lea (%1,%2.l),%0";
      else
	return "lea (%c2,%1),%0";
#else /* not MOTOROLA (MIT syntax) */
      if (GET_CODE (operands[2]) == REG)
	return "lea %1@(0,%2:l),%0";
      else
	return "lea %1@(%c2),%0";
#endif /* not MOTOROLA */
#endif /* not SGS */
    }
  if (GET_CODE (operands[2]) == CONST_INT)
    {
#ifndef NO_ADDSUB_Q
      if (INTVAL (operands[2]) > 0
	  && INTVAL (operands[2]) <= 8)
	return "addq%.l %2,%0";
      if (INTVAL (operands[2]) < 0
	  && INTVAL (operands[2]) >= -8)
        {
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "subq%.l %2,%0";
	}
      /* On the CPU32 it is faster to use two addql instructions to
	 add a small integer (8 < N <= 16) to a register.
	 Likewise for subql. */
      if (TARGET_CPU32 && REG_P (operands[0]))
	{
	  if (INTVAL (operands[2]) > 8
	      && INTVAL (operands[2]) <= 16)
	    {
	      operands[2] = GEN_INT (INTVAL (operands[2]) - 8);
	      return "addq%.l %#8,%0\n\taddq%.l %2,%0";
	    }
	  if (INTVAL (operands[2]) < -8
	      && INTVAL (operands[2]) >= -16)
	    {
	      operands[2] = GEN_INT (-INTVAL (operands[2]) - 8);
	      return "subq%.l %#8,%0\n\tsubq%.l %2,%0";
	    }
	}
#endif
      if (ADDRESS_REG_P (operands[0])
	  && INTVAL (operands[2]) >= -0x8000
	  && INTVAL (operands[2]) < 0x8000)
	{
	  if (TARGET_68040)
	    return "add%.w %2,%0";
	  else
#ifdef MOTOROLA  
	    return "lea (%c2,%0),%0";
#else
	    return "lea %0@(%c2),%0";
#endif
	}
    }
  return "add%.l %2,%0";
}

/* Store in cc_status the expressions that the condition codes will
   describe after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* On the 68000, all the insns to store in an address register fail to
   set the cc's.  However, in some cases these instructions can make it
   possibly invalid to use the saved cc's.  In those cases we clear out
   some or all of the saved cc's so they won't be used.  */

void
notice_update_cc (exp, insn)
     rtx exp;
     rtx insn;
{
  /* If the cc is being set from the fpa and the expression is not an
     explicit floating point test instruction (which has code to deal with
     this), reinit the CC.  */
  if (((cc_status.value1 && FPA_REG_P (cc_status.value1))
       || (cc_status.value2 && FPA_REG_P (cc_status.value2)))
      && !(GET_CODE (exp) == PARALLEL
	   && GET_CODE (XVECEXP (exp, 0, 0)) == SET
	   && XEXP (XVECEXP (exp, 0, 0), 0) == cc0_rtx))
    {
      CC_STATUS_INIT; 
    }
  else if (GET_CODE (exp) == SET)
    {
      if (GET_CODE (SET_SRC (exp)) == CALL)
	{
	  CC_STATUS_INIT; 
	}
      else if (ADDRESS_REG_P (SET_DEST (exp)))
	{
	  if (cc_status.value1 && modified_in_p (cc_status.value1, insn))
	    cc_status.value1 = 0;
	  if (cc_status.value2 && modified_in_p (cc_status.value2, insn))
	    cc_status.value2 = 0; 
	}
      else if (!FP_REG_P (SET_DEST (exp))
	       && SET_DEST (exp) != cc0_rtx
	       && (FP_REG_P (SET_SRC (exp))
		   || GET_CODE (SET_SRC (exp)) == FIX
		   || GET_CODE (SET_SRC (exp)) == FLOAT_TRUNCATE
		   || GET_CODE (SET_SRC (exp)) == FLOAT_EXTEND))
	{
	  CC_STATUS_INIT; 
	}
      /* A pair of move insns doesn't produce a useful overall cc.  */
      else if (!FP_REG_P (SET_DEST (exp))
	       && !FP_REG_P (SET_SRC (exp))
	       && GET_MODE_SIZE (GET_MODE (SET_SRC (exp))) > 4
	       && (GET_CODE (SET_SRC (exp)) == REG
		   || GET_CODE (SET_SRC (exp)) == MEM
		   || GET_CODE (SET_SRC (exp)) == CONST_DOUBLE))
	{
	  CC_STATUS_INIT; 
	}
      else if (GET_CODE (SET_SRC (exp)) == CALL)
	{
	  CC_STATUS_INIT; 
	}
      else if (XEXP (exp, 0) != pc_rtx)
	{
	  cc_status.flags = 0;
	  cc_status.value1 = XEXP (exp, 0);
	  cc_status.value2 = XEXP (exp, 1);
	}
    }
  else if (GET_CODE (exp) == PARALLEL
	   && GET_CODE (XVECEXP (exp, 0, 0)) == SET)
    {
      if (ADDRESS_REG_P (XEXP (XVECEXP (exp, 0, 0), 0)))
	CC_STATUS_INIT;
      else if (XEXP (XVECEXP (exp, 0, 0), 0) != pc_rtx)
	{
	  cc_status.flags = 0;
	  cc_status.value1 = XEXP (XVECEXP (exp, 0, 0), 0);
	  cc_status.value2 = XEXP (XVECEXP (exp, 0, 0), 1);
	}
    }
  else
    CC_STATUS_INIT;
  if (cc_status.value2 != 0
      && ADDRESS_REG_P (cc_status.value2)
      && GET_MODE (cc_status.value2) == QImode)
    CC_STATUS_INIT;
  if (cc_status.value2 != 0
      && !(cc_status.value1 && FPA_REG_P (cc_status.value1)))
    switch (GET_CODE (cc_status.value2))
      {
      case PLUS: case MINUS: case MULT:
      case DIV: case UDIV: case MOD: case UMOD: case NEG:
#if 0 /* These instructions always clear the overflow bit */
      case ASHIFT: case ASHIFTRT: case LSHIFTRT:
      case ROTATE: case ROTATERT:
#endif
	if (GET_MODE (cc_status.value2) != VOIDmode)
	  cc_status.flags |= CC_NO_OVERFLOW;
	break;
      case ZERO_EXTEND:
	/* (SET r1 (ZERO_EXTEND r2)) on this machine
	   ends with a move insn moving r2 in r2's mode.
	   Thus, the cc's are set for r2.
	   This can set N bit spuriously. */
	cc_status.flags |= CC_NOT_NEGATIVE; 

      default:
	break;
      }
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG
      && cc_status.value2
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))
    cc_status.value2 = 0;
  if (((cc_status.value1 && FP_REG_P (cc_status.value1))
       || (cc_status.value2 && FP_REG_P (cc_status.value2)))
      && !((cc_status.value1 && FPA_REG_P (cc_status.value1))
	   || (cc_status.value2 && FPA_REG_P (cc_status.value2))))
    cc_status.flags = CC_IN_68881;
}

char *
output_move_const_double (operands)
     rtx *operands;
{
#ifdef SUPPORT_SUN_FPA
  if (TARGET_FPA && FPA_REG_P (operands[0]))
    {
      int code = standard_sun_fpa_constant_p (operands[1]);

      if (code != 0)
	{
	  static char buf[40];

	  sprintf (buf, "fpmove%%.d %%%%%d,%%0", code & 0x1ff);
	  return buf;
	}
      return "fpmove%.d %1,%0";
    }
  else
#endif
    {
      int code = standard_68881_constant_p (operands[1]);

      if (code != 0)
	{
	  static char buf[40];

	  sprintf (buf, "fmovecr %%#0x%x,%%0", code & 0xff);
	  return buf;
	}
      return "fmove%.d %1,%0";
    }
}

char *
output_move_const_single (operands)
     rtx *operands;
{
#ifdef SUPPORT_SUN_FPA
  if (TARGET_FPA)
    {
      int code = standard_sun_fpa_constant_p (operands[1]);

      if (code != 0)
	{
	  static char buf[40];

	  sprintf (buf, "fpmove%%.s %%%%%d,%%0", code & 0x1ff);
	  return buf;
	}
      return "fpmove%.s %1,%0";
    }
  else
#endif /* defined SUPPORT_SUN_FPA */
    {
      int code = standard_68881_constant_p (operands[1]);

      if (code != 0)
	{
	  static char buf[40];

	  sprintf (buf, "fmovecr %%#0x%x,%%0", code & 0xff);
	  return buf;
	}
      return "fmove%.s %f1,%0";
    }
}

/* Return nonzero if X, a CONST_DOUBLE, has a value that we can get
   from the "fmovecr" instruction.
   The value, anded with 0xff, gives the code to use in fmovecr
   to get the desired constant.  */

/* This code has been fixed for cross-compilation. */
  
static int inited_68881_table = 0;

char *strings_68881[7] = {
  "0.0",
  "1.0",
  "10.0",
  "100.0",
  "10000.0",
  "1e8",
  "1e16"
  };

int codes_68881[7] = {
  0x0f,
  0x32,
  0x33,
  0x34,
  0x35,
  0x36,
  0x37
  };

REAL_VALUE_TYPE values_68881[7];

/* Set up values_68881 array by converting the decimal values
   strings_68881 to binary.   */

void
init_68881_table ()
{
  int i;
  REAL_VALUE_TYPE r;
  enum machine_mode mode;

  mode = SFmode;
  for (i = 0; i < 7; i++)
    {
      if (i == 6)
        mode = DFmode;
      r = REAL_VALUE_ATOF (strings_68881[i], mode);
      values_68881[i] = r;
    }
  inited_68881_table = 1;
}

int
standard_68881_constant_p (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;

#ifdef NO_ASM_FMOVECR
  return 0;
#endif

  /* fmovecr must be emulated on the 68040 and 68060, so it shouldn't be
     used at all on those chips. */
  if (TARGET_68040 || TARGET_68060)
    return 0;

#ifndef REAL_ARITHMETIC
#if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
  if (! flag_pretend_float)
    return 0;
#endif
#endif

  if (! inited_68881_table)
    init_68881_table ();

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);

  /* Use REAL_VALUES_IDENTICAL instead of REAL_VALUES_EQUAL so that -0.0
     is rejected.  */
  for (i = 0; i < 6; i++)
    {
      if (REAL_VALUES_IDENTICAL (r, values_68881[i]))
        return (codes_68881[i]);
    }
  
  if (GET_MODE (x) == SFmode)
    return 0;

  if (REAL_VALUES_EQUAL (r, values_68881[6]))
    return (codes_68881[6]);

  /* larger powers of ten in the constants ram are not used
     because they are not equal to a `double' C constant.  */
  return 0;
}

/* If X is a floating-point constant, return the logarithm of X base 2,
   or 0 if X is not a power of 2.  */

int
floating_exact_log2 (x)
     rtx x;
{
  REAL_VALUE_TYPE r, r1;
  int i;

#ifndef REAL_ARITHMETIC
#if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
  if (! flag_pretend_float)
    return 0;
#endif
#endif

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);

  if (REAL_VALUES_LESS (r, dconst0))
    return 0;

  r1 = dconst1;
  i = 0;
  while (REAL_VALUES_LESS (r1, r))
    {
      r1 = REAL_VALUE_LDEXP (dconst1, i);
      if (REAL_VALUES_EQUAL (r1, r))
        return i;
      i = i + 1;
    }
  return 0;
}

#ifdef SUPPORT_SUN_FPA
/* Return nonzero if X, a CONST_DOUBLE, has a value that we can get
   from the Sun FPA's constant RAM.
   The value returned, anded with 0x1ff, gives the code to use in fpmove
   to get the desired constant. */

static int inited_FPA_table = 0;

char *strings_FPA[38] = {
/* small rationals */
  "0.0",
  "1.0",
  "0.5",
  "-1.0",
  "2.0",
  "3.0",
  "4.0",
  "8.0",
  "0.25",
  "0.125",
  "10.0",
  "-0.5",
/* Decimal equivalents of double precision values */
  "2.718281828459045091", /* D_E */
  "6.283185307179586477", /* 2 pi */
  "3.141592653589793116", /* D_PI */
  "1.570796326794896619", /* pi/2 */
  "1.414213562373095145", /* D_SQRT2 */
  "0.7071067811865475244", /* 1/sqrt(2) */
  "-1.570796326794896619", /* -pi/2 */
  "1.442695040888963387", /* D_LOG2ofE */
  "3.321928024887362182", /* D_LOG2of10 */
  "0.6931471805599452862", /* D_LOGEof2 */
  "2.302585092994045901", /* D_LOGEof10 */
  "0.3010299956639811980", /* D_LOG10of2 */
  "0.4342944819032518167", /* D_LOG10ofE */
/* Decimal equivalents of single precision values */
  "2.718281745910644531", /* S_E */
  "6.283185307179586477", /* 2 pi */
  "3.141592741012573242", /* S_PI */
  "1.570796326794896619", /* pi/2 */
  "1.414213538169860840", /* S_SQRT2 */
  "0.7071067811865475244", /* 1/sqrt(2) */
  "-1.570796326794896619", /* -pi/2 */
  "1.442695021629333496", /* S_LOG2ofE */
  "3.321928024291992188", /* S_LOG2of10 */
  "0.6931471824645996094", /* S_LOGEof2 */
  "2.302585124969482442", /* S_LOGEof10 */
  "0.3010300099849700928", /* S_LOG10of2 */
  "0.4342944920063018799", /* S_LOG10ofE */
};


int codes_FPA[38] = {
/* small rationals */
  0x200,
  0xe,
  0xf,
  0x10,
  0x11,
  0xb1,
  0x12,
  0x13,
  0x15,
  0x16,
  0x17,
  0x2e,
/* double precision */
  0x8,
  0x9,
  0xa,
  0xb,
  0xc,
  0xd,
  0x27,
  0x28,
  0x29,
  0x2a,
  0x2b,
  0x2c,
  0x2d,
/* single precision */
  0x8,
  0x9,
  0xa,
  0xb,
  0xc,
  0xd,
  0x27,
  0x28,
  0x29,
  0x2a,
  0x2b,
  0x2c,
  0x2d
  };

REAL_VALUE_TYPE values_FPA[38];

/* This code has been fixed for cross-compilation. */

void
init_FPA_table ()
{
  enum machine_mode mode;
  int i;
  REAL_VALUE_TYPE r;

  mode = DFmode;
  for (i = 0; i < 38; i++)
    {
      if (i == 25)
        mode = SFmode;
      r = REAL_VALUE_ATOF (strings_FPA[i], mode);
      values_FPA[i] = r;
    }
  inited_FPA_table = 1;
}


int
standard_sun_fpa_constant_p (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;

#ifndef REAL_ARITHMETIC
#if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
  if (! flag_pretend_float)
    return 0;
#endif
#endif

  if (! inited_FPA_table)
    init_FPA_table ();

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);

  for (i=0; i<12; i++)
    {
      if (REAL_VALUES_EQUAL (r, values_FPA[i]))
        return (codes_FPA[i]);
    }

  if (GET_MODE (x) == SFmode)
    {
      for (i=25; i<38; i++)
        {
          if (REAL_VALUES_EQUAL (r, values_FPA[i]))
            return (codes_FPA[i]);
        }
    }
  else
    {
      for (i=12; i<25; i++)
        {
          if (REAL_VALUES_EQUAL (r, values_FPA[i]))
            return (codes_FPA[i]);
        }
    }
  return 0x0;
}
#endif /* define SUPPORT_SUN_FPA */

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  X is an RTL
   expression.

   CODE is a value that can be used to specify one of several ways
   of printing the operand.  It is used when identical operands
   must be printed differently depending on the context.  CODE
   comes from the `%' specification that was used to request
   printing of the operand.  If the specification was just `%DIGIT'
   then CODE is 0; if the specification was `%LTR DIGIT' then CODE
   is the ASCII code for LTR.

   If X is a register, this macro should print the register's name.
   The names can be found in an array `reg_names' whose type is
   `char *[]'.  `reg_names' is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%'
   followed by a punctuation character), this macro is called with
   a null pointer for X and the punctuation character for CODE.

   The m68k specific codes are:

   '.' for dot needed in Motorola-style opcode names.
   '-' for an operand pushing on the stack:
       sp@-, -(sp) or -(%sp) depending on the style of syntax.
   '+' for an operand pushing on the stack:
       sp@+, (sp)+ or (%sp)+ depending on the style of syntax.
   '@' for a reference to the top word on the stack:
       sp@, (sp) or (%sp) depending on the style of syntax.
   '#' for an immediate operand prefix (# in MIT and Motorola syntax
       but & in SGS syntax, $ in CRDS/UNOS syntax).
   '!' for the cc register (used in an `and to cc' insn).
   '$' for the letter `s' in an op code, but only on the 68040.
   '&' for the letter `d' in an op code, but only on the 68040.
   '/' for register prefix needed by longlong.h.

   'b' for byte insn (no effect, on the Sun; this is for the ISI).
   'd' to force memory addressing to be absolute, not relative.
   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   'w' for FPA insn (print a CONST_DOUBLE as a SunFPA constant rather
       than directly).  Second part of 'y' below.
   'x' for float insn (print a CONST_DOUBLE as a float rather than in hex),
       or print pair of registers as rx:ry.
   'y' for a FPA insn (print pair of registers as rx:ry).  This also outputs
       CONST_DOUBLE's as SunFPA constant RAM registers if
       possible, so it should not be used except for the SunFPA.

   */

void
print_operand (file, op, letter)
     FILE *file;		/* file to write to */
     rtx op;			/* operand to print */
     int letter;		/* %<letter> or 0 */
{
#ifdef SUPPORT_SUN_FPA
  int i;
#endif

  if (letter == '.')
    {
#if defined (MOTOROLA) && !defined (CRDS)
      asm_fprintf (file, ".");
#endif
    }
  else if (letter == '#')
    {
      asm_fprintf (file, "%0I");
    }
  else if (letter == '-')
    {
#ifdef MOTOROLA
      asm_fprintf (file, "-(%Rsp)");
#else
      asm_fprintf (file, "%Rsp@-");
#endif
    }
  else if (letter == '+')
    {
#ifdef MOTOROLA
      asm_fprintf (file, "(%Rsp)+");
#else
      asm_fprintf (file, "%Rsp@+");
#endif
    }
  else if (letter == '@')
    {
#ifdef MOTOROLA
      asm_fprintf (file, "(%Rsp)");
#else
      asm_fprintf (file, "%Rsp@");
#endif
    }
  else if (letter == '!')
    {
      asm_fprintf (file, "%Rfpcr");
    }
  else if (letter == '$')
    {
      if (TARGET_68040_ONLY)
	{
	  fprintf (file, "s");
	}
    }
  else if (letter == '&')
    {
      if (TARGET_68040_ONLY)
	{
	  fprintf (file, "d");
	}
    }
  else if (letter == '/')
    {
      asm_fprintf (file, "%R");
    }
  else if (GET_CODE (op) == REG)
    {
#ifdef SUPPORT_SUN_FPA
      if (REGNO (op) < 16
	  && (letter == 'y' || letter == 'x')
	  && GET_MODE (op) == DFmode)
	{
	  fprintf (file, "%s:%s", reg_names[REGNO (op)],
		   reg_names[REGNO (op)+1]);
	}
      else
#endif
	{
	  if (letter == 'R')
	    /* Print out the second register name of a register pair.
	       I.e., R (6) => 7.  */
	    fputs (reg_names[REGNO (op) + 1], file);
	  else
	    fputs (reg_names[REGNO (op)], file);
	}
    }
  else if (GET_CODE (op) == MEM)
    {
      output_address (XEXP (op, 0));
      if (letter == 'd' && ! TARGET_68020
	  && CONSTANT_ADDRESS_P (XEXP (op, 0))
	  && !(GET_CODE (XEXP (op, 0)) == CONST_INT
	       && INTVAL (XEXP (op, 0)) < 0x8000
	       && INTVAL (XEXP (op, 0)) >= -0x8000))
	{
#ifdef MOTOROLA
	  fprintf (file, ".l");
#else
	  fprintf (file, ":l");
#endif
	}
    }
#ifdef SUPPORT_SUN_FPA
  else if ((letter == 'y' || letter == 'w')
	   && GET_CODE (op) == CONST_DOUBLE
	   && (i = standard_sun_fpa_constant_p (op)))
    {
      fprintf (file, "%%%d", i & 0x1ff);
    }
#endif
  else if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == SFmode)
    {
      REAL_VALUE_TYPE r;
      REAL_VALUE_FROM_CONST_DOUBLE (r, op);
      ASM_OUTPUT_FLOAT_OPERAND (letter, file, r);
    }
  else if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == XFmode)
    {
      REAL_VALUE_TYPE r;
      REAL_VALUE_FROM_CONST_DOUBLE (r, op);
      ASM_OUTPUT_LONG_DOUBLE_OPERAND (file, r);
    }
  else if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == DFmode)
    {
      REAL_VALUE_TYPE r;
      REAL_VALUE_FROM_CONST_DOUBLE (r, op);
      ASM_OUTPUT_DOUBLE_OPERAND (file, r);
    }
  else
    {
      asm_fprintf (file, "%0I"); output_addr_const (file, op);
    }
}


/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   Note that this contains a kludge that knows that the only reason
   we have an address (plus (label_ref...) (reg...)) when not generating
   PIC code is in the insn before a tablejump, and we know that m68k.md
   generates a label LInnn: on such an insn.

   It is possible for PIC to generate a (plus (label_ref...) (reg...))
   and we handle that just like we would a (plus (symbol_ref...) (reg...)).

   Some SGS assemblers have a bug such that "Lnnn-LInnn-2.b(pc,d0.l*2)"
   fails to assemble.  Luckily "Lnnn(pc,d0.l*2)" produces the results
   we want.  This difference can be accommodated by using an assembler
   define such "LDnnn" to be either "Lnnn-LInnn-2.b", "Lnnn", or any other
   string, as necessary.  This is accomplished via the ASM_OUTPUT_CASE_END
   macro.  See m68k/sgs.h for an example; for versions without the bug.
   Some assemblers refuse all the above solutions.  The workaround is to
   emit "K(pc,d0.l*2)" with K being a small constant known to give the
   right behaviour.

   They also do not like things like "pea 1.w", so we simple leave off
   the .w on small constants. 

   This routine is responsible for distinguishing between -fpic and -fPIC 
   style relocations in an address.  When generating -fpic code the
   offset is output in word mode (eg movel a5@(_foo:w), a0).  When generating
   -fPIC code the offset is output in long mode (eg movel a5@(_foo:l), a0) */

#ifndef ASM_OUTPUT_CASE_FETCH
#ifdef MOTOROLA
#ifdef SGS
#define ASM_OUTPUT_CASE_FETCH(file, labelno, regname)\
	asm_fprintf (file, "%LLD%d(%Rpc,%s.", labelno, regname)
#else
#define ASM_OUTPUT_CASE_FETCH(file, labelno, regname)\
	asm_fprintf (file, "%LL%d-%LLI%d.b(%Rpc,%s.", labelno, labelno, regname)
#endif
#else
#define ASM_OUTPUT_CASE_FETCH(file, labelno, regname)\
	asm_fprintf (file, "%Rpc@(%LL%d-%LLI%d-2:b,%s:", labelno, labelno, regname)
#endif
#endif /* ASM_OUTPUT_CASE_FETCH */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  register rtx reg1, reg2, breg, ireg;
  rtx offset;

  switch (GET_CODE (addr))
    {
      case REG:
#ifdef MOTOROLA
	fprintf (file, "(%s)", reg_names[REGNO (addr)]);
#else
	fprintf (file, "%s@", reg_names[REGNO (addr)]);
#endif
	break;
      case PRE_DEC:
#ifdef MOTOROLA
	fprintf (file, "-(%s)", reg_names[REGNO (XEXP (addr, 0))]);
#else
	fprintf (file, "%s@-", reg_names[REGNO (XEXP (addr, 0))]);
#endif
	break;
      case POST_INC:
#ifdef MOTOROLA
	fprintf (file, "(%s)+", reg_names[REGNO (XEXP (addr, 0))]);
#else
	fprintf (file, "%s@+", reg_names[REGNO (XEXP (addr, 0))]);
#endif
	break;
      case PLUS:
	reg1 = reg2 = ireg = breg = offset = 0;
	if (CONSTANT_ADDRESS_P (XEXP (addr, 0)))
	  {
	    offset = XEXP (addr, 0);
	    addr = XEXP (addr, 1);
	  }
	else if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))
	  {
	    offset = XEXP (addr, 1);
	    addr = XEXP (addr, 0);
	  }
	if (GET_CODE (addr) != PLUS)
	  {
	    ;
	  }
	else if (GET_CODE (XEXP (addr, 0)) == SIGN_EXTEND)
	  {
	    reg1 = XEXP (addr, 0);
	    addr = XEXP (addr, 1);
	  }
	else if (GET_CODE (XEXP (addr, 1)) == SIGN_EXTEND)
	  {
	    reg1 = XEXP (addr, 1);
	    addr = XEXP (addr, 0);
	  }
	else if (GET_CODE (XEXP (addr, 0)) == MULT)
	  {
	    reg1 = XEXP (addr, 0);
	    addr = XEXP (addr, 1);
	  }
	else if (GET_CODE (XEXP (addr, 1)) == MULT)
	  {
	    reg1 = XEXP (addr, 1);
	    addr = XEXP (addr, 0);
	  }
	else if (GET_CODE (XEXP (addr, 0)) == REG)
	  {
	    reg1 = XEXP (addr, 0);
	    addr = XEXP (addr, 1);
	  }
	else if (GET_CODE (XEXP (addr, 1)) == REG)
	  {
	    reg1 = XEXP (addr, 1);
	    addr = XEXP (addr, 0);
	  }
	if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT
	    || GET_CODE (addr) == SIGN_EXTEND)
	  {
	    if (reg1 == 0)
	      {
		reg1 = addr;
	      }
	    else
	      {
		reg2 = addr;
	      }
	    addr = 0;
	  }
#if 0	/* for OLD_INDEXING */
	else if (GET_CODE (addr) == PLUS)
	  {
	    if (GET_CODE (XEXP (addr, 0)) == REG)
	      {
		reg2 = XEXP (addr, 0);
		addr = XEXP (addr, 1);
	      }
	    else if (GET_CODE (XEXP (addr, 1)) == REG)
	      {
		reg2 = XEXP (addr, 1);
		addr = XEXP (addr, 0);
	      }
	  }
#endif
	if (offset != 0)
	  {
	    if (addr != 0)
	      {
		abort ();
	      }
	    addr = offset;
	  }
	if ((reg1 && (GET_CODE (reg1) == SIGN_EXTEND
		      || GET_CODE (reg1) == MULT))
	    || (reg2 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg2))))
	  {
	    breg = reg2;
	    ireg = reg1;
	  }
	else if (reg1 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg1)))
	  {
	    breg = reg1;
	    ireg = reg2;
	  }
	if (ireg != 0 && breg == 0 && GET_CODE (addr) == LABEL_REF
	    && ! (flag_pic && ireg == pic_offset_table_rtx))
	  {
	    int scale = 1;
	    if (GET_CODE (ireg) == MULT)
	      {
		scale = INTVAL (XEXP (ireg, 1));
		ireg = XEXP (ireg, 0);
	      }
	    if (GET_CODE (ireg) == SIGN_EXTEND)
	      {
		ASM_OUTPUT_CASE_FETCH (file,
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     reg_names[REGNO (XEXP (ireg, 0))]);
		fprintf (file, "w");
	      }
	    else
	      {
		ASM_OUTPUT_CASE_FETCH (file,
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     reg_names[REGNO (ireg)]);
		fprintf (file, "l");
	      }
	    if (scale != 1)
	      {
#ifdef MOTOROLA
		fprintf (file, "*%d", scale);
#else
		fprintf (file, ":%d", scale);
#endif
	      }
	    putc (')', file);
	    break;
	  }
	if (breg != 0 && ireg == 0 && GET_CODE (addr) == LABEL_REF
	    && ! (flag_pic && breg == pic_offset_table_rtx))
	  {
	    ASM_OUTPUT_CASE_FETCH (file,
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 reg_names[REGNO (breg)]);
	    fprintf (file, "l)");
	    break;
	  }
	if (ireg != 0 || breg != 0)
	  {
	    int scale = 1;
	    if (breg == 0)
	      {
		abort ();
	      }
	    if (! flag_pic && addr && GET_CODE (addr) == LABEL_REF)
	      {
		abort ();
	      }
#ifdef MOTOROLA
	    if (addr != 0)
	      {
		output_addr_const (file, addr);
	        if (flag_pic && (breg == pic_offset_table_rtx))
		  {
		    fprintf (file, "@GOT");
		    if (flag_pic == 1)
		      fprintf (file, ".w");
		  }
	      }
	    fprintf (file, "(%s", reg_names[REGNO (breg)]);
	    if (ireg != 0)
	      {
		putc (',', file);
	      }
#else
	    fprintf (file, "%s@(", reg_names[REGNO (breg)]);
	    if (addr != 0)
	      {
		output_addr_const (file, addr);
	        if ((flag_pic == 1) && (breg == pic_offset_table_rtx))
	          fprintf (file, ":w");
	        if ((flag_pic == 2) && (breg == pic_offset_table_rtx))
	          fprintf (file, ":l");
	      }
	    if (addr != 0 && ireg != 0)
	      {
		putc (',', file);
	      }
#endif
	    if (ireg != 0 && GET_CODE (ireg) == MULT)
	      {
		scale = INTVAL (XEXP (ireg, 1));
		ireg = XEXP (ireg, 0);
	      }
	    if (ireg != 0 && GET_CODE (ireg) == SIGN_EXTEND)
	      {
#ifdef MOTOROLA
		fprintf (file, "%s.w", reg_names[REGNO (XEXP (ireg, 0))]);
#else
		fprintf (file, "%s:w", reg_names[REGNO (XEXP (ireg, 0))]);
#endif
	      }
	    else if (ireg != 0)
	      {
#ifdef MOTOROLA
		fprintf (file, "%s.l", reg_names[REGNO (ireg)]);
#else
		fprintf (file, "%s:l", reg_names[REGNO (ireg)]);
#endif
	      }
	    if (scale != 1)
	      {
#ifdef MOTOROLA
		fprintf (file, "*%d", scale);
#else
		fprintf (file, ":%d", scale);
#endif
	      }
	    putc (')', file);
	    break;
	  }
	else if (reg1 != 0 && GET_CODE (addr) == LABEL_REF
		 && ! (flag_pic && reg1 == pic_offset_table_rtx))	
	  {
	    ASM_OUTPUT_CASE_FETCH (file,
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 reg_names[REGNO (reg1)]);
	    fprintf (file, "l)");
	    break;
	  }
	/* FALL-THROUGH (is this really what we want? */
      default:
        if (GET_CODE (addr) == CONST_INT
	    && INTVAL (addr) < 0x8000
	    && INTVAL (addr) >= -0x8000)
	  {
#ifdef MOTOROLA
#ifdef SGS
	    /* Many SGS assemblers croak on size specifiers for constants. */
	    fprintf (file, "%d", INTVAL (addr));
#else
	    fprintf (file, "%d.w", INTVAL (addr));
#endif
#else
	    fprintf (file, "%d:w", INTVAL (addr));
#endif
	  }
	else
	  {
	    /* Special case for SYMBOL_REF if the symbol name ends in
	       `.<letter>', this can be mistaken as a size suffix.  Put
	       the name in parentheses.  */
	    if (GET_CODE (addr) == SYMBOL_REF
		&& strlen (XSTR (addr, 0)) > 2
		&& XSTR (addr, 0)[strlen (XSTR (addr, 0)) - 2] == '.')
	      {
		putc ('(', file);
		output_addr_const (file, addr);
		putc (')', file);
	      }
	    else
	      output_addr_const (file, addr);
	  }
	break;
    }
}

/* Check for cases where a clr insns can be omitted from code using
   strict_low_part sets.  For example, the second clrl here is not needed:
   clrl d0; movw a0@+,d0; use d0; clrl d0; movw a0@+; use d0; ...

   MODE is the mode of this STRICT_LOW_PART set.  FIRST_INSN is the clear
   insn we are checking for redundancy.  TARGET is the register set by the
   clear insn.  */

int
strict_low_part_peephole_ok (mode, first_insn, target)
     enum machine_mode mode;
     rtx first_insn;
     rtx target;
{
  rtx p;

  p = prev_nonnote_insn (first_insn);

  while (p)
    {
      /* If it isn't an insn, then give up.  */
      if (GET_CODE (p) != INSN)
	return 0;

      if (reg_set_p (target, p))
	{
	  rtx set = single_set (p);
	  rtx dest;

	  /* If it isn't an easy to recognize insn, then give up.  */
	  if (! set)
	    return 0;

	  dest = SET_DEST (set);

	  /* If this sets the entire target register to zero, then our
	     first_insn is redundant.  */
	  if (rtx_equal_p (dest, target)
	      && SET_SRC (set) == const0_rtx)
	    return 1;
	  else if (GET_CODE (dest) == STRICT_LOW_PART
		   && GET_CODE (XEXP (dest, 0)) == REG
		   && REGNO (XEXP (dest, 0)) == REGNO (target)
		   && (GET_MODE_SIZE (GET_MODE (XEXP (dest, 0)))
		       <= GET_MODE_SIZE (mode)))
	    /* This is a strict low part set which modifies less than
	       we are using, so it is safe.  */
	    ;
	  else
	    return 0;
	}

      p = prev_nonnote_insn (p);

    }

  return 0;
}

/* Accept integer operands in the range 0..0xffffffff.  We have to check the
   range carefully since this predicate is used in DImode contexts.  Also, we
   need some extra crud to make it work when hosted on 64-bit machines.  */

int
const_uint32_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
#if HOST_BITS_PER_WIDE_INT > 32
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= 0 && INTVAL (op) <= 0xffffffffL));
#else
  return ((GET_CODE (op) == CONST_INT && INTVAL (op) >= 0)
	  || (GET_CODE (op) == CONST_DOUBLE && CONST_DOUBLE_HIGH (op) == 0));
#endif
}

/* Accept integer operands in the range -0x80000000..0x7fffffff.  We have
   to check the range carefully since this predicate is used in DImode
   contexts.  */

int
const_sint32_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= (-0x7fffffff - 1) && INTVAL (op) <= 0x7fffffff));
}

char *
output_andsi3 (operands)
     rtx *operands;
{
  int logval;
  if (GET_CODE (operands[2]) == CONST_INT
      && (INTVAL (operands[2]) | 0xffff) == 0xffffffff
      && (DATA_REG_P (operands[0])
	  || offsettable_memref_p (operands[0]))
      && !TARGET_5200)
    {
      if (GET_CODE (operands[0]) != REG)
        operands[0] = adj_offsettable_operand (operands[0], 2);
      operands[2] = GEN_INT (INTVAL (operands[2]) & 0xffff);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      if (operands[2] == const0_rtx)
        return "clr%.w %0";
      return "and%.w %2,%0";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (logval = exact_log2 (~ INTVAL (operands[2]))) >= 0
      && (DATA_REG_P (operands[0])
          || offsettable_memref_p (operands[0])))
    {
      if (DATA_REG_P (operands[0]))
        {
          operands[1] = GEN_INT (logval);
        }
      else
        {
	  operands[0] = adj_offsettable_operand (operands[0], 3 - (logval / 8));
	  operands[1] = GEN_INT (logval % 8);
        }
      /* This does not set condition codes in a standard way.  */
      CC_STATUS_INIT;
      return "bclr %1,%0";
    }
  return "and%.l %2,%0";
}

char *
output_iorsi3 (operands)
     rtx *operands;
{
  register int logval;
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >> 16 == 0
      && (DATA_REG_P (operands[0])
	  || offsettable_memref_p (operands[0]))
      && !TARGET_5200)
    {
      if (GET_CODE (operands[0]) != REG)
        operands[0] = adj_offsettable_operand (operands[0], 2);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      if (INTVAL (operands[2]) == 0xffff)
	return "mov%.w %2,%0";
      return "or%.w %2,%0";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (logval = exact_log2 (INTVAL (operands[2]))) >= 0
      && (DATA_REG_P (operands[0])
	  || offsettable_memref_p (operands[0])))
    {
      if (DATA_REG_P (operands[0]))
	{
	  operands[1] = GEN_INT (logval);
	}
      else
        {
	  operands[0] = adj_offsettable_operand (operands[0], 3 - (logval / 8));
	  operands[1] = GEN_INT (logval % 8);
	}
      CC_STATUS_INIT;
      return "bset %1,%0";
    }
  return "or%.l %2,%0";
}

char *
output_xorsi3 (operands)
     rtx *operands;
{
  register int logval;
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >> 16 == 0
      && (offsettable_memref_p (operands[0]) || DATA_REG_P (operands[0]))
      && !TARGET_5200)
    {
      if (! DATA_REG_P (operands[0]))
	operands[0] = adj_offsettable_operand (operands[0], 2);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      if (INTVAL (operands[2]) == 0xffff)
	return "not%.w %0";
      return "eor%.w %2,%0";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (logval = exact_log2 (INTVAL (operands[2]))) >= 0
      && (DATA_REG_P (operands[0])
	  || offsettable_memref_p (operands[0])))
    {
      if (DATA_REG_P (operands[0]))
	{
	  operands[1] = GEN_INT (logval);
	}
      else
        {
	  operands[0] = adj_offsettable_operand (operands[0], 3 - (logval / 8));
	  operands[1] = GEN_INT (logval % 8);
	}
      CC_STATUS_INIT;
      return "bchg %1,%0";
    }
  return "eor%.l %2,%0";
}
