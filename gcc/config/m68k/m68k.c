/* Subroutines for insn-output.c for Motorola 68000 family.
   Copyright (C) 1987 Free Software Foundation, Inc.

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


/* Some output-actions in m68k.md need these.  */
#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"

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


/* Emit a (use pic_offset_table_rtx) if we used PIC relocation in the 
   function at any time during the compilation process.  In the future 
   we should try and eliminate the USE if we can easily determine that 
   all PIC references were deleted from the current function.  That would 
   save an address register */
   
finalize_pic ()
{
  if (flag_pic && current_function_uses_pic_offset_table)
    emit_insn (gen_rtx (USE, VOIDmode, pic_offset_table_rtx));
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
  

  if (frame_pointer_needed)
    {
      /* Adding negative number is faster on the 68040.  */
      if (fsize < 0x8000 && !TARGET_68040)
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
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tlink.w %s,%0I0\n\tadd.l %0I%d,%Rsp\n",
		       reg_names[FRAME_POINTER_REGNUM], -fsize);
#else
	  asm_fprintf (stream, "\tlink %s,%0I0\n\taddl %0I%d,%Rsp\n",
		       reg_names[FRAME_POINTER_REGNUM], -fsize);
#endif
	}
    }
  else if (fsize)
    {
      /* Adding negative number is faster on the 68040.  */
      if (fsize + 4 < 0x8000)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tadd.w %0I%d,%Rsp\n", - (fsize + 4));
#else
	  asm_fprintf (stream, "\taddw %0I%d,%Rsp\n", - (fsize + 4));
#endif
	}
      else
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tadd.l %0I%d,%Rsp\n", - (fsize + 4));
#else
	  asm_fprintf (stream, "\taddl %0I%d,%Rsp\n", - (fsize + 4));
#endif
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
      }
#endif
  for (regno = 16; regno < 24; regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
       mask |= 1 << (regno - 16);
  if ((mask & 0xff) != 0)
    {
#ifdef MOTOROLA
      asm_fprintf (stream, "\tfmovm %0I0x%x,-(%Rsp)\n", mask & 0xff);
#else
      asm_fprintf (stream, "\tfmovem %0I0x%x,%Rsp@-\n", mask & 0xff);
#endif
    }
  mask = 0;
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

#if NEED_PROBE
  fprintf (stream, "\ttstl sp@(%d)\n", NEED_PROBE - num_saved_regs * 4);
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
          asm_fprintf (stream,
#ifdef MOTOROLA
		       "\t%Omove.l %s,-(%Rsp)\n",
#else
		       "\tmovel %s,%Rsp@-\n",
#endif
		       reg_names[15 - i]);
    }
  else if (mask)
    {
#ifdef MOTOROLA
      asm_fprintf (stream, "\tmovm.l %0I0x%x,-(%Rsp)\n", mask);
#else
      asm_fprintf (stream, "\tmoveml %0I0x%x,%Rsp@-\n", mask);
#endif
    }
  if (flag_pic && current_function_uses_pic_offset_table)
    {
#ifdef MOTOROLA
      asm_fprintf (stream, "\t%Omove.l %0I__GLOBAL_OFFSET_TABLE_, %s\n",
		   reg_names[PIC_OFFSET_TABLE_REGNUM]);
      asm_fprintf (stream, "\tlea.l (%Rpc,%s.l),%s\n",
		   reg_names[PIC_OFFSET_TABLE_REGNUM],
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
  for (regno = 16; regno < 24; regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
        nregs++;
	fmask |= 1 << (23 - regno);
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
  offset = foffset + nregs * 4;
  if (offset + fsize >= 0x8000
      && frame_pointer_needed
      && (mask || fmask || fpoffset))
    {
#ifdef MOTOROLA
      asm_fprintf (stream, "\t%Omove.l %0I%d,%Ra0\n", -fsize);
#else
      asm_fprintf (stream, "\tmovel %0I%d,%Ra0\n", -fsize);
#endif
      fsize = 0, big = 1;
    }
  if (nregs <= 2)
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
		asm_fprintf (stream, "\t%Omove.l -%d(%s,%Ra0.l),%s\n",
			     offset + fsize,
			     reg_names[FRAME_POINTER_REGNUM],
			     reg_names[i]);
#else
		asm_fprintf (stream, "\tmovel %s@(-%d,%Ra0:l),%s\n",
			     reg_names[FRAME_POINTER_REGNUM],
			     offset + fsize, reg_names[i]);
#endif
	      }
            else if (! frame_pointer_needed)
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
	  asm_fprintf (stream, "\tmovm.l -%d(%s,%Ra0.l),%0I0x%x\n",
		       offset + fsize,
		       reg_names[FRAME_POINTER_REGNUM],
		       mask);
#else
	  asm_fprintf (stream, "\tmoveml %s@(-%d,%Ra0:l),%0I0x%x\n",
		       reg_names[FRAME_POINTER_REGNUM],
		       offset + fsize, mask);
#endif
	}
      else if (! frame_pointer_needed)
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
	  asm_fprintf (stream, "\tfmovm -%d(%s,%Ra0.l),%0I0x%x\n",
		       foffset + fsize,
		       reg_names[FRAME_POINTER_REGNUM],
		       fmask);
#else
	  asm_fprintf (stream, "\tfmovem %s@(-%d,%Ra0:l),%0I0x%x\n",
		       reg_names[FRAME_POINTER_REGNUM],
		       foffset + fsize, fmask);
#endif
	}
      else if (! frame_pointer_needed)
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
	      asm_fprintf (stream, "\tfpmovd -%d(%s,%Ra0.l), %s\n",
			   fpoffset + fsize,
			   reg_names[FRAME_POINTER_REGNUM],
			   reg_names[regno]);
#else
	      asm_fprintf (stream, "\tfpmoved %s@(-%d,%Ra0:l), %s\n",
			   reg_names[FRAME_POINTER_REGNUM],
			   fpoffset + fsize, reg_names[regno]);
#endif
	    }
	  else if (! frame_pointer_needed)
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
      if (fsize + 4 < 0x8000)
	{
#ifdef MOTOROLA
	  asm_fprintf (stream, "\tadd.w %0I%d,%Rsp\n", fsize + 4);
#else
	  asm_fprintf (stream, "\taddw %0I%d,%Rsp\n", fsize + 4);
#endif
	}
      else
	{
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
     enum machine_mode mode;
{
  /* We could add support for these in the future */
  if (cc_prev_status.flags & CC_IN_68881)
    return 0;

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

/* Output a dbCC; jCC sequence.  Note we do not handle the 
   floating point version of this sequence (Fdbcc).  We also
   do not handle alternative conditions when CC_NO_OVERFLOW is
   set.  It is assumed that valid_dbcc_comparison_p will kick
   those out before we get here.  */

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
     enum machine_mode mode;
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
     enum machine_mode mode;
{
  rtx pic_ref = orig;

  /* First handle a simple SYMBOL_REF or LABEL_REF */
  if (GET_CODE (orig) == SYMBOL_REF || GET_CODE (orig) == LABEL_REF)
    {
      if (reg == 0)
	abort ();

      pic_ref = gen_rtx (MEM, Pmode,
			 gen_rtx (PLUS, Pmode,
				  pic_offset_table_rtx, orig));
      current_function_uses_pic_offset_table = 1;
      RTX_UNCHANGING_P (pic_ref) = 1;
      emit_move_insn (reg, pic_ref);
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

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
      pic_ref = gen_rtx (PLUS, Pmode, base, orig);
      /* Likewise, should we set special REG_NOTEs here?  */
    }
  return pic_ref;
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
  if (DATA_REG_P (operands[0])
      && GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) < 128
      && INTVAL (operands[1]) >= -128)
    {
#if defined (MOTOROLA) && !defined (CRDS)
      return "moveq%.l %1,%0";
#else
      return "moveq %1,%0";
#endif
    }
  if (operands[1] != const0_rtx)
    return "move%.l %1,%0";
  if (! ADDRESS_REG_P (operands[0]))
    return "clr%.l %0";
  return "sub%.l %0,%0";
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

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
      output_asm_insn ("subq%.l %#8,%0", operands);
      operands[0] = gen_rtx (MEM, DImode, operands[0]);
      optype0 = OFFSOP;
    }
  if (optype0 == POPOP && optype1 == PUSHOP)
    {
      operands[1] = XEXP (XEXP (operands[1], 0), 0);
      output_asm_insn ("subq%.l %#8,%1", operands);
      operands[1] = gen_rtx (MEM, DImode, operands[1]);
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

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 4);
  else if (optype1 == CNSTOP)
    split_double (operands[1], &operands[1], &latehalf[1]);
  else
    latehalf[1] = operands[1];

  /* If insn is effectively movd N(sp),-(sp) then we will do the
     high word first.  We should use the adjusted operand 1 (which is N+4(sp))
     for the low word as well, to compensate for the first decrement of sp.  */
  if (optype0 == PUSHOP
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    operands[1] = latehalf[1];

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */

  /* Likewise,  the first move would clobber the source of the second one,
     do them in the other order.  This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance.  */

  if (optype0 == PUSHOP || optype1 == PUSHOP
      || (optype0 == REGOP && optype1 == REGOP
	  && REGNO (operands[0]) == REGNO (latehalf[1])))
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	output_asm_insn ("addql %#4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("addql %#4,%0", &addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("subql %#4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("subql %#4,%0", &addreg1);

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("addql %#4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("addql %#4,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("subql %#4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("subql %#4,%0", &addreg1);

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

/* Store in cc_status the expressions that the condition codes will
   describe after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* On the 68000, all the insns to store in an address register fail to
   set the cc's.  However, in some cases these instructions can make it
   possibly invalid to use the saved cc's.  In those cases we clear out
   some or all of the saved cc's so they won't be used.  */

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
	  if (cc_status.value1
	      && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value1))
	    cc_status.value1 = 0;
	  if (cc_status.value2
	      && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value2))
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
      case ASHIFT: case LSHIFT: case ASHIFTRT: case LSHIFTRT:
      case ROTATE: case ROTATERT:
	if (GET_MODE (cc_status.value2) != VOIDmode)
	  cc_status.flags |= CC_NO_OVERFLOW;
	break;
      case ZERO_EXTEND:
	/* (SET r1 (ZERO_EXTEND r2)) on this machine
	   ends with a move insn moving r2 in r2's mode.
	   Thus, the cc's are set for r2.
	   This can set N bit spuriously. */
	cc_status.flags |= CC_NOT_NEGATIVE; 
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

/* ??? This code should be fixed for cross-compilation. */

int
standard_68881_constant_p (x)
     rtx x;
{
  register double d;

  /* fmovecr must be emulated on the 68040, so it shouldn't be used at all. */
  if (TARGET_68040)
    return 0;

#if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
  if (! flag_pretend_float)
    return 0;
#endif

  REAL_VALUE_FROM_CONST_DOUBLE (d, x);

  if (d == 0)
    return 0x0f;
  /* Note: there are various other constants available
     but it is a nuisance to put in their values here.  */
  if (d == 1)
    return 0x32;
  if (d == 10)
    return 0x33;
  if (d == 100)
    return 0x34;
  if (d == 10000)
    return 0x35;
  if (d == 1e8)
    return 0x36;
  if (GET_MODE (x) == SFmode)
    return 0;
  if (d == 1e16)
    return 0x37;
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
  register double d, d1;
  int i;

#if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
  if (! flag_pretend_float)
    return 0;
#endif

  REAL_VALUE_FROM_CONST_DOUBLE (d, x);

  if (! (d > 0))
    return 0;

  for (d1 = 1.0, i = 0; d1 < d; d1 *= 2.0, i++)
    ;

  if (d == d1)
    return i;

  return 0;
}

#ifdef SUPPORT_SUN_FPA
/* Return nonzero if X, a CONST_DOUBLE, has a value that we can get
   from the Sun FPA's constant RAM.
   The value returned, anded with 0x1ff, gives the code to use in fpmove
   to get the desired constant. */
#define S_E (2.718281745910644531)
#define D_E (2.718281828459045091)
#define S_PI (3.141592741012573242)
#define D_PI (3.141592653589793116)
#define S_SQRT2 (1.414213538169860840)
#define D_SQRT2 (1.414213562373095145)
#define S_LOG2ofE (1.442695021629333496)
#define D_LOG2ofE (1.442695040888963387)
#define S_LOG2of10 (3.321928024291992188)
#define D_LOG2of10 (3.321928024887362182)
#define S_LOGEof2 (0.6931471824645996094)
#define D_LOGEof2 (0.6931471805599452862)
#define S_LOGEof10 (2.302585124969482442)
#define D_LOGEof10 (2.302585092994045901)
#define S_LOG10of2 (0.3010300099849700928)
#define D_LOG10of2 (0.3010299956639811980)
#define S_LOG10ofE (0.4342944920063018799)
#define D_LOG10ofE (0.4342944819032518167)

/* This code should be fixed for cross-compilation. */

int
standard_sun_fpa_constant_p (x)
     rtx x;
{
  register double d;

#if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
  if (! flag_pretend_float)
    return 0;
#endif

  REAL_VALUE_FROM_CONST_DOUBLE (d, x);

  if (d == 0.0)
    return 0x200;		/* 0 once 0x1ff is anded with it */
  if (d == 1.0)
    return 0xe;
  if (d == 0.5)
    return 0xf;
  if (d == -1.0)
    return 0x10;
  if (d == 2.0)
    return 0x11;
  if (d == 3.0)
    return 0xB1;
  if (d == 4.0)
    return 0x12;
  if (d == 8.0)
    return 0x13;
  if (d == 0.25)
    return 0x15;
  if (d == 0.125)
    return 0x16;
  if (d == 10.0)
    return 0x17;
  if (d == -(1.0/2.0))
    return 0x2E;

/*
 * Stuff that looks different if it's single or double
 */
  if (GET_MODE (x) == SFmode)
    {
      if (d == S_E)
	return 0x8;
      if (d == (2*S_PI))
	return 0x9;
      if (d == S_PI)
	return 0xA;
      if (d == (S_PI / 2.0))
	return 0xB;
      if (d == S_SQRT2)
	return 0xC;
      if (d == (1.0 / S_SQRT2))
	return 0xD;
      /* Large powers of 10 in the constant 
	 ram are not used because they are
	 not equal to a C double constant  */
      if (d == -(S_PI / 2.0))
	return 0x27;
      if (d == S_LOG2ofE)
	return 0x28;
      if (d == S_LOG2of10)
	return 0x29;
      if (d == S_LOGEof2)
	return 0x2A;
      if (d == S_LOGEof10)
	return 0x2B;
      if (d == S_LOG10of2)
	return 0x2C;
      if (d == S_LOG10ofE)
	return 0x2D;
    }
  else
    {
      if (d == D_E)
	return 0x8;
      if (d == (2*D_PI))
	return 0x9;
      if (d == D_PI)
	return 0xA;
      if (d == (D_PI / 2.0))
	return 0xB;
      if (d == D_SQRT2)
	return 0xC;
      if (d == (1.0 / D_SQRT2))
	return 0xD;
      /* Large powers of 10 in the constant 
	 ram are not used because they are
	 not equal to a C double constant  */
      if (d == -(D_PI / 2.0))
	return 0x27;
      if (d == D_LOG2ofE)
	return 0x28;
      if (d == D_LOG2of10)
	return 0x29;
      if (d == D_LOGEof2)
	return 0x2A;
      if (d == D_LOGEof10)
	return 0x2B;
      if (d == D_LOG10of2)
	return 0x2C;
      if (d == D_LOG10ofE)
	return 0x2D;
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
       but & in SGS syntax).
   '!' for the cc register (used in an `and to cc' insn).
   '$' for the letter `s' in an op code, but only on the 68040.
   '&' for the letter `d' in an op code, but only on the 68040.

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
  int i;

  if (letter == '.')
    {
#ifdef MOTOROLA
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
  else if (GET_CODE (op) == REG)
    {
      if (REGNO (op) < 16
	  && (letter == 'y' || letter == 'x')
	  && GET_MODE (op) == DFmode)
	{
	  fprintf (file, "%s:%s", reg_names[REGNO (op)],
		   reg_names[REGNO (op)+1]);
	}
      else
	{
	  fprintf (file, "%s", reg_names[REGNO (op)]);
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
	  fprintf (file, ":l");
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
      double d;
      union { float f; int i; } u1;
      REAL_VALUE_FROM_CONST_DOUBLE (d, op);
      u1.f = d;
      PRINT_OPERAND_PRINT_FLOAT (letter, file);
    }
  else if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) != DImode)
    {
      double d;
      REAL_VALUE_FROM_CONST_DOUBLE (d, op);
      ASM_OUTPUT_DOUBLE_OPERAND (file, d);
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
   macro.  See m68ksgs.h for an example; for versions without the bug.

   They also do not like things like "pea 1.w", so we simple leave off
   the .w on small constants. 

   This routine is responsible for distinguishing between -fpic and -fPIC 
   style relocations in an address.  When generating -fpic code the
   offset is output in word mode (eg movel a5@(_foo:w), a0).  When generating
   -fPIC code the offset is output in long mode (eg movel a5@(_foo:l), a0) */

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
#ifdef MOTOROLA
#ifdef SGS
		asm_fprintf (file, "%LLD%d(%Rpc,%s.w",
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     reg_names[REGNO (XEXP (ireg, 0))]);
#else
		asm_fprintf (file, "%LL%d-%LLI%d.b(%Rpc,%s.w",
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     reg_names[REGNO (XEXP (ireg, 0))]);
#endif
#else
		asm_fprintf (file, "%Rpc@(%LL%d-%LLI%d-2:b,%s:w",
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     reg_names[REGNO (XEXP (ireg, 0))]);
#endif
	      }
	    else
	      {
#ifdef MOTOROLA
#ifdef SGS
		asm_fprintf (file, "%LLD%d(%Rpc,%s.l",
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     reg_names[REGNO (ireg)]);
#else
		asm_fprintf (file, "%LL%d-%LLI%d.b(%Rpc,%s.l",
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     reg_names[REGNO (ireg)]);
#endif
#else
		asm_fprintf (file, "%Rpc@(%LL%d-%LLI%d-2:b,%s:l",
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     CODE_LABEL_NUMBER (XEXP (addr, 0)),
			     reg_names[REGNO (ireg)]);
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
	if (breg != 0 && ireg == 0 && GET_CODE (addr) == LABEL_REF
	    && ! (flag_pic && breg == pic_offset_table_rtx))
	  {
#ifdef MOTOROLA
#ifdef SGS
	    asm_fprintf (file, "%LLD%d(%Rpc,%s.l",
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 reg_names[REGNO (breg)]);
#else
	    asm_fprintf (file, "%LL%d-%LLI%d.b(%Rpc,%s.l",
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 reg_names[REGNO (breg)]);
#endif
#else
	    asm_fprintf (file, "%Rpc@(%LL%d-%LLI%d-2:b,%s:l",
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 reg_names[REGNO (breg)]);
#endif
	    putc (')', file);
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
	        if ((flag_pic == 1) && (breg == pic_offset_table_rtx))
	          fprintf (file, ".w");
	        if ((flag_pic == 2) && (breg == pic_offset_table_rtx))
	          fprintf (file, ".l");
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
#ifdef MOTOROLA
#ifdef SGS
	    asm_fprintf (file, "%LLD%d(%Rpc,%s.l)",
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 reg_names[REGNO (reg1)]);
#else
	    asm_fprintf (file, "%LL%d-%LLI%d.b(%Rpc,%s.l)",
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 reg_names[REGNO (reg1)]);
#endif
#else
	    asm_fprintf (file, "%Rpc@(%LL%d-%LLI%d-2:b,%s:l)",
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 CODE_LABEL_NUMBER (XEXP (addr, 0)),
			 reg_names[REGNO (reg1)]);
#endif
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
	    output_addr_const (file, addr);
	  }
	break;
    }
}
