/* Subroutines for insn-output.c for HPPA.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Contributed by Tim Moore (moore@cs.utah.edu), based on sparc.c

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
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "tree.h"
#include "c-tree.h"
#include "expr.h"
#include "obstack.h"

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */

rtx hppa_compare_op0, hppa_compare_op1;
enum cmp_type hppa_branch_type;

rtx hppa_save_pic_table_rtx;

/* Set by the FUNCTION_PROFILER macro. */
int hp_profile_labelno;

/* Counts for the number of callee-saved general and floating point
   registers which were saved by the current function's prologue.  */
static int gr_saved, fr_saved;

static rtx find_addr_reg ();

/* Return non-zero only if OP is a register of mode MODE,
   or CONST0_RTX.  */
int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == CONST0_RTX (mode) || register_operand (op, mode));
}

/* Return non-zero if OP is suitable for use in a call to a named
   function.

   (???) For 2.5 try to eliminate either call_operand_address or
   function_label_operand, they perform very similar functions.  */
int
call_operand_address (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (CONSTANT_P (op) && ! TARGET_LONG_CALLS);
}

/* Return 1 if X contains a symbolic expression.  We know these
   expressions will have one of a few well defined forms, so
   we need only check those forms.  */
int
symbolic_expression_p (x)
     register rtx x;
{

  /* Strip off any HIGH. */
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  return (symbolic_operand (x, VOIDmode));
}

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
    default:
      return 0;
    }
}

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == HIGH || GET_CODE (op) == LABEL_REF);
}

/* Return 1 if the operand is either a register or a memory operand that is
   not symbolic.  */

int
reg_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

/* Return 1 if the operand is either a register, zero, or a memory operand
   that is not symbolic.  */

int
reg_or_0_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (op == CONST0_RTX (mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

/* Accept any constant that can be moved in one instructions into a
   general register.  */
int
cint_ok_for_move (intval)
     HOST_WIDE_INT intval;
{
  /* OK if ldo, ldil, or zdepi, can be used.  */
  return (VAL_14_BITS_P (intval) || (intval & 0x7ff) == 0
	  || zdepi_cint_p (intval));
}

/* Accept anything that can be moved in one instruction into a general
   register.  */
int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    return cint_ok_for_move (INTVAL (op));

  if (GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);
  if (GET_CODE (op) == LO_SUM)
    return (register_operand (XEXP (op, 0), Pmode)
	    && CONSTANT_P (XEXP (op, 1)));
  return memory_address_p (mode, op);
}

/* Accept REG and any CONST_INT that can be moved in one instruction into a
   general register.  */
int
reg_or_cint_move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    return cint_ok_for_move (INTVAL (op));

  return 0;
}

int
pic_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return flag_pic && GET_CODE (op) == LABEL_REF;
}

int
fp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return reg_renumber && FP_REG_P (op);
}


extern int current_function_uses_pic_offset_table;
extern rtx force_reg (), validize_mem ();

/* The rtx for the global offset table which is a special form
   that *is* a position independent symbolic constant.  */
rtx pic_pc_rtx;

/* Ensure that we are not using patterns that are not OK with PIC.  */

int
check_pic (i)
     int i;
{
  extern rtx recog_operand[];
  switch (flag_pic)
    {
    case 1:
      if (GET_CODE (recog_operand[i]) == SYMBOL_REF
	  || (GET_CODE (recog_operand[i]) == CONST
	      && ! rtx_equal_p (pic_pc_rtx, recog_operand[i])))
	abort ();
    case 2:
    default:
      return 1;
    }
}

/* Return truth value of whether OP can be used as an operand in a
   three operand arithmetic insn that accepts registers of mode MODE
   or 14-bit signed integers.  */
int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && INT_14_BITS (op)));
}

/* Return truth value of whether OP can be used as an operand in a
   three operand arithmetic insn that accepts registers of mode MODE
   or 11-bit signed integers.  */
int
arith11_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && INT_11_BITS (op)));
}

/* A constant integer suitable for use in a PRE_MODIFY memory
   reference.  */
int
pre_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) >= -0x2000 && INTVAL (op) < 0x10);
}

/* A constant integer suitable for use in a POST_MODIFY memory
   reference.  */
int
post_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) < 0x2000 && INTVAL (op) >= -0x10);
}

int
arith_double_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_DOUBLE
	      && GET_MODE (op) == mode
	      && VAL_14_BITS_P (CONST_DOUBLE_LOW (op))
	      && (CONST_DOUBLE_HIGH (op) >= 0
		  == ((CONST_DOUBLE_LOW (op) & 0x1000) == 0))));
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in three-address insns.  */

int
int5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && INT_5_BITS (op));
}

int
uint5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && INT_U5_BITS (op));
}

int
int11_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && INT_11_BITS (op));
}

int
uint32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
#if HOST_BITS_PER_WIDE_INT > 32
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= 0 && INTVAL (op) < 0x100000000L));
#else
  return (GET_CODE (op) == CONST_INT
	  || (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_HIGH (op) == 0));
#endif
}

int
arith5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || int5_operand (op, mode);
}

/* True iff zdepi can be used to generate this CONST_INT.  */
int
zdepi_cint_p (x)
     unsigned HOST_WIDE_INT x;
{
  unsigned lsb_mask, t;

  /* This might not be obvious, but it's at least fast.
     This function is critcal; we don't have the time loops would take.  */
  lsb_mask = x & -x;
  t = ((x >> 4) + lsb_mask) & ~(lsb_mask - 1);
  /* Return true iff t is a power of two.  */
  return ((t & (t - 1)) == 0);
}

/* True iff depi or extru can be used to compute (reg & mask).
   Accept bit pattern like these:
   0....01....1
   1....10....0
   1..10..01..1  */
int
and_mask_p (mask)
     unsigned HOST_WIDE_INT mask;
{
  mask = ~mask;
  mask += mask & -mask;
  return (mask & (mask - 1)) == 0;
}

/* True iff depi or extru can be used to compute (reg & OP).  */
int
and_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && and_mask_p (INTVAL (op))));
}

/* True iff depi can be used to compute (reg | MASK).  */
int
ior_mask_p (mask)
     unsigned HOST_WIDE_INT mask;
{
  mask += mask & -mask;
  return (mask & (mask - 1)) == 0;
}

/* True iff depi can be used to compute (reg | OP).  */
int
ior_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && ior_mask_p (INTVAL (op)));
}

int
lhs_lshift_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || lhs_lshift_cint_operand (op, mode);
}

/* True iff OP is a CONST_INT of the forms 0...0xxxx or 0...01...1xxxx.
   Such values can be the left hand side x in (x << r), using the zvdepi
   instruction.  */
int
lhs_lshift_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  unsigned x;
  if (GET_CODE (op) != CONST_INT)
    return 0;
  x = INTVAL (op) >> 4;
  return (x & (x + 1)) == 0;
}

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || GET_CODE (op) == CONST_INT;
}

int
pc_or_label_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == PC || GET_CODE (op) == LABEL_REF);
}

/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go to REG.  If we need more
   than one register, we lose.  */

rtx
legitimize_pic_address (orig, mode, reg)
     rtx orig, reg;
     enum machine_mode mode;
{
  rtx pic_ref = orig;

  if (GET_CODE (orig) == SYMBOL_REF)
    {
      if (reg == 0)
	abort ();

      if (flag_pic == 2)
	{
	  emit_insn (gen_rtx (SET, VOIDmode, reg,
			      gen_rtx (HIGH, Pmode, orig)));
	  emit_insn (gen_rtx (SET, VOIDmode, reg,
			      gen_rtx (LO_SUM, Pmode, reg, orig)));
	  orig = reg;
	}
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
      rtx base;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	abort ();

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  orig = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					 base == reg ? 0 : reg);
	}
      else abort ();
      if (GET_CODE (orig) == CONST_INT)
	{
	  if (INT_14_BITS (orig))
	    return plus_constant_for_output (base, INTVAL (orig));
	  orig = force_reg (Pmode, orig);
	}
      pic_ref = gen_rtx (PLUS, Pmode, base, orig);
      /* Likewise, should we set special REG_NOTEs here?  */
    }
  return pic_ref;
}

/* Emit special PIC prologues and epilogues.  */

void
finalize_pic ()
{
  if (hppa_save_pic_table_rtx)
    {
      emit_insn_after (gen_rtx (SET, VOIDmode,
				hppa_save_pic_table_rtx,
				gen_rtx (REG, Pmode, PIC_OFFSET_TABLE_REGNUM)),
		       get_insns ());
      /* Need to emit this whether or not we obey regdecls,
	 since setjmp/longjmp can cause life info to screw up.  */
      hppa_save_pic_table_rtx = 0;
    }
  emit_insn (gen_rtx (USE, VOIDmode, pic_offset_table_rtx));
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the PA, transform:

	memory(X + <large int>)

   into:

	if (<large int> & mask) >= 16
	  Y = (<large int> & ~mask) + mask + 1	Round up.
	else
	  Y = (<large int> & ~mask)		Round down.
	Z = X + Y
	memory (Z + (<large int> - Y));

   This is for CSE to find several similar references, and only use one Z.

   X can either be a SYMBOL_REF or REG, but because combine can not
   perform a 4->2 combination we do nothing for SYMBOL_REF + D where
   D will not fit in 14 bits.

   MODE_FLOAT references allow displacements which fit in 5 bits, so use
   0x1f as the mask.

   MODE_INT references allow displacements which fit in 14 bits, so use
   0x3fff as the mask.

   This relies on the fact that most mode MODE_FLOAT references will use FP
   registers and most mode MODE_INT references will use integer registers.
   (In the rare case of an FP register used in an integer MODE, we depend
   on secondary reloads to clean things up.)


   It is also beneficial to handle (plus (mult (X) (Y)) (Z)) in a special
   manner if Y is 2, 4, or 8.  (allows more shadd insns and shifted indexed
   adressing modes to be used).

   Put X and Z into registers.  Then put the entire expression into
   a register.  */

rtx
hppa_legitimize_address (x, oldx, mode)
     rtx x, oldx;
     enum machine_mode mode;
{
  rtx orig = x;

  /* Strip off CONST. */
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  /* Note we must reject symbols which represent function addresses
     since the assembler/linker can't handle arithmetic on plabels.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && ((GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	   && !FUNCTION_NAME_P (XSTR (XEXP (x, 0), 0)))
	  || GET_CODE (XEXP (x, 0)) == REG))
    {
      rtx int_part, ptr_reg;
      int newoffset;
      int offset = INTVAL (XEXP (x, 1));
      int mask = GET_MODE_CLASS (mode) == MODE_FLOAT ? 0x1f : 0x3fff;

      /* Choose which way to round the offset.  Round up if we
	 are >= halfway to the next boundary.  */
      if ((offset & mask) >= ((mask + 1) / 2))
	newoffset = (offset & ~ mask) + mask + 1;
      else
	newoffset = (offset & ~ mask);

      /* If the newoffset will not fit in 14 bits (ldo), then
	 handling this would take 4 or 5 instructions (2 to load
	 the SYMBOL_REF + 1 or 2 to load the newoffset + 1 to
	 add the new offset and the SYMBOL_REF.)  Combine can
	 not handle 4->2 or 5->2 combinations, so do not create
	 them.  */
      if (! VAL_14_BITS_P (newoffset)
	  && GET_CODE (XEXP (x, 0)) == SYMBOL_REF)
	{
	  rtx const_part = gen_rtx (CONST, VOIDmode,
				    gen_rtx (PLUS, Pmode,
					     XEXP (x, 0),
					     GEN_INT (newoffset)));
	  rtx tmp_reg
	    = force_reg (Pmode,
			 gen_rtx (HIGH, Pmode, const_part));
	  ptr_reg
	    = force_reg (Pmode,
			 gen_rtx (LO_SUM, Pmode,
				  tmp_reg, const_part));
	}
      else
	{
	  if (! VAL_14_BITS_P (newoffset))
	    int_part = force_reg (Pmode, GEN_INT (newoffset));
	  else
	    int_part = GEN_INT (newoffset);

	  ptr_reg = force_reg (Pmode,
			       gen_rtx (PLUS, Pmode,
					force_reg (Pmode, XEXP (x, 0)),
					int_part));
	}
      return plus_constant (ptr_reg, offset - newoffset);
    }

  /* Try to arrange things so that indexing modes can be used, but
     only do so if indexing is safe.

     Indexing is safe when the second operand for the outer PLUS
     is a REG, SUBREG, SYMBOL_REF or the like.

     For 2.5, indexing is also safe for (plus (symbol_ref) (const_int))
     if the integer is > 0.  */
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == MULT
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && shadd_constant_p (INTVAL (XEXP (XEXP (x, 0), 1)))
      && (GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == 'o'
	  || GET_CODE (XEXP (x, 1)) == SUBREG)
      && GET_CODE (XEXP (x, 1)) != CONST)
    {
      int val = INTVAL (XEXP (XEXP (x, 0), 1));
      rtx reg1, reg2;
      reg1 = force_reg (Pmode, force_operand (XEXP (x, 1), 0));
      reg2 = force_reg (Pmode,
			force_operand (XEXP (XEXP (x, 0), 0), 0));
      return force_reg (Pmode,
		        gen_rtx (PLUS, Pmode,
				 gen_rtx (MULT, Pmode, reg2,
					  GEN_INT (val)),
				 reg1));
    }

  /* Uh-oh.  We might have an address for x[n-100000].  This needs
     special handling.  */

  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == MULT
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && shadd_constant_p (INTVAL (XEXP (XEXP (x, 0), 1))))
    {
      /* Ugly.  We modify things here so that the address offset specified
	 by the index expression is computed first, then added to x to form
	 the entire address.

	 For 2.5, it might be profitable to set things up so that we
	 compute the raw (unscaled) index first, then use scaled indexing
	 to access memory, or better yet have the MI parts of the compiler
	 handle this.  */

      rtx regx1, regy1, regy2, y;

      /* Strip off any CONST.  */
      y = XEXP (x, 1);
      if (GET_CODE (y) == CONST)
	y = XEXP (y, 0);

      if (GET_CODE (y) == PLUS || GET_CODE (y) == MINUS)
	{
	  regx1 = force_reg (Pmode, force_operand (XEXP (x, 0), 0));
	  regy1 = force_reg (Pmode, force_operand (XEXP (y, 0), 0));
	  regy2 = force_reg (Pmode, force_operand (XEXP (y, 1), 0));
	  regx1 = force_reg (Pmode, gen_rtx (GET_CODE (y), Pmode, regx1, regy2));
	  return force_reg (Pmode, gen_rtx (PLUS, Pmode, regx1, regy1));
	}
    }

  if (flag_pic)
    return legitimize_pic_address (x, mode, gen_reg_rtx (Pmode));

  return orig;
}

/* For the HPPA, REG and REG+CONST is cost 0
   and addresses involving symbolic constants are cost 2.

   PIC addresses are very expensive.

   It is no coincidence that this has the same structure
   as GO_IF_LEGITIMATE_ADDRESS.  */
int
hppa_address_cost (X)
     rtx X;
{
  if (GET_CODE (X) == PLUS)
      return 1;
  else if (GET_CODE (X) == LO_SUM)
    return 1;
  else if (GET_CODE (X) == HIGH)
    return 2;
  return 4;
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.  */

int
emit_move_sequence (operands, mode, scratch_reg)
     rtx *operands;
     enum machine_mode mode;
     rtx scratch_reg;
{
  register rtx operand0 = operands[0];
  register rtx operand1 = operands[1];

  /* Handle secondary reloads for loads/stores of FP registers from
     REG+D addresses where D does not fit in 5 bits, including 
     (subreg (mem (addr)) cases.  */
  if (fp_reg_operand (operand0, mode)
      && ((GET_CODE (operand1) == MEM
	   && ! memory_address_p (DFmode, XEXP (operand1, 0)))
	  || ((GET_CODE (operand1) == SUBREG
	       && GET_CODE (XEXP (operand1, 0)) == MEM
	       && !memory_address_p (DFmode, XEXP (XEXP (operand1, 0), 0)))))
      && scratch_reg)
    {
      if (GET_CODE (operand1) == SUBREG)
	operand1 = XEXP (operand1, 0);

      scratch_reg = gen_rtx (REG, SImode, REGNO (scratch_reg));
      emit_move_insn (scratch_reg, XEXP (operand1, 0));
      emit_insn (gen_rtx (SET, VOIDmode, operand0, gen_rtx (MEM, mode,
							    scratch_reg)));
      return 1;
    }
  else if (fp_reg_operand (operand1, mode)
	   && ((GET_CODE (operand0) == MEM
		&& ! memory_address_p (DFmode, XEXP (operand0, 0)))
	       || ((GET_CODE (operand0) == SUBREG)
		   && GET_CODE (XEXP (operand0, 0)) == MEM
		   && !memory_address_p (DFmode, XEXP (XEXP (operand0, 0), 0))))
	   && scratch_reg)
    {
      if (GET_CODE (operand0) == SUBREG)
	operand0 = XEXP (operand0, 0);

      scratch_reg = gen_rtx (REG, SImode, REGNO (scratch_reg));
      emit_move_insn (scratch_reg, XEXP (operand0, 0));
      emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (MEM, mode, scratch_reg),
			  operand1));
      return 1;
    }
  /* Handle secondary reloads for loads of FP registers from constant
     expressions by forcing the constant into memory.

     use scratch_reg to hold the address of the memory location.

     ??? The proper fix is to change PREFERRED_RELOAD_CLASS to return
     NO_REGS when presented with a const_int and an register class
     containing only FP registers.  Doing so unfortunately creates
     more problems than it solves.   Fix this for 2.5.  */
  else if (fp_reg_operand (operand0, mode)
	   && CONSTANT_P (operand1)
	   && scratch_reg)
    {
      rtx xoperands[2];

      /* Force the constant into memory and put the address of the
	 memory location into scratch_reg.  */
      xoperands[0] = scratch_reg;
      xoperands[1] = XEXP (force_const_mem (mode, operand1), 0);
      emit_move_sequence (xoperands, Pmode, 0);

      /* Now load the destination register.  */
      emit_insn (gen_rtx (SET, mode, operand0,
			  gen_rtx (MEM, mode, scratch_reg)));
      return 1;
    }
  /* Handle secondary reloads for SAR.  These occur when trying to load
     the SAR from memory a FP register, or with a constant.  */
  else if (GET_CODE (operand0) == REG
	   && REGNO_REG_CLASS (REGNO (operand0)) == SHIFT_REGS
	   && (GET_CODE (operand1) == MEM
	       || GET_CODE (operand1) == CONST_INT
	       || (GET_CODE (operand1) == REG
		   && FP_REG_CLASS_P (REGNO_REG_CLASS (REGNO (operand1)))))
	   && scratch_reg)
    {
      emit_move_insn (scratch_reg, operand1);
      emit_move_insn (operand0, scratch_reg);
      return 1;
    }
  /* Handle most common case: storing into a register.  */
  else if (register_operand (operand0, mode))
    {
      if (register_operand (operand1, mode)
	  || (GET_CODE (operand1) == CONST_INT && INT_14_BITS (operand1))
	  || (operand1 == CONST0_RTX (mode))
	  || (GET_CODE (operand1) == HIGH
	      && !symbolic_operand (XEXP (operand1, 0), VOIDmode))
	  /* Only `general_operands' can come here, so MEM is ok.  */
	  || GET_CODE (operand1) == MEM)
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
    }
  else if (GET_CODE (operand0) == MEM)
    {
      if (register_operand (operand1, mode) || operand1 == CONST0_RTX (mode))
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
      if (! (reload_in_progress || reload_completed))
	{
	  operands[0] = validize_mem (operand0);
	  operands[1] = operand1 = force_reg (mode, operand1);
	}
    }

  /* Simplify the source if we need to.  */
  if ((GET_CODE (operand1) != HIGH && immediate_operand (operand1, mode))
      || (GET_CODE (operand1) == HIGH
	  && symbolic_operand (XEXP (operand1, 0), mode)))
    {
      int ishighonly = 0;

      if (GET_CODE (operand1) == HIGH)
	{
	  ishighonly = 1;
	  operand1 = XEXP (operand1, 0);
	}
      if (symbolic_operand (operand1, mode))
	{
	  if (flag_pic)
	    {
	      rtx temp;

	      if (reload_in_progress || reload_completed)
		temp = operand0;
	      else
		temp = gen_reg_rtx (Pmode);

	      operands[1] = legitimize_pic_address (operand1, mode, temp);
              emit_insn (gen_rtx (SET, VOIDmode, operand0, operands[1]));
	    }
	  /* On the HPPA, references to data space are supposed to */
	  /* use dp, register 27, but showing it in the RTL inhibits various
	     cse and loop optimizations.  */
	  else
	    {
	      rtx temp, set, const_part = NULL;

	      if (reload_in_progress || reload_completed)
		temp = scratch_reg ? scratch_reg : operand0;
	      else
		temp = gen_reg_rtx (mode);

	      /* Argh.  The assembler and linker can't handle arithmetic
		 involving plabels.  We'll have to split up operand1 here
		 if it's a function label involved in an arithmetic
		 expression.  Luckily, this only happens with addition
		 of constants to plabels, which simplifies the test.  */
	     if (GET_CODE (operand1) == CONST
		 && GET_CODE (XEXP (operand1, 0)) == PLUS
		 && function_label_operand (XEXP (XEXP (operand1, 0), 0),
					    Pmode))
		{
		  /* Save away the constant part of the expression.  */
		  const_part = XEXP (XEXP (operand1, 0), 1);
		  if (GET_CODE (const_part) != CONST_INT)
		    abort ();

		  /* Set operand1 to just the SYMBOL_REF.  */
		  operand1 = XEXP (XEXP (operand1, 0), 0);
		}

	      if (ishighonly)
		set = gen_rtx (SET, mode, operand0, temp);
	      else
		set = gen_rtx (SET, VOIDmode,
			       operand0,
			       gen_rtx (LO_SUM, mode, temp, operand1));

	      emit_insn (gen_rtx (SET, VOIDmode,
				  temp,
				  gen_rtx (HIGH, mode, operand1)));
	      emit_insn (set);

	      /* Add back in the constant part if needed.  */
	      if (const_part != NULL)
		emit_insn (gen_rtx (SET, mode, operand0,
				    plus_constant (operand0,
						   XEXP (const_part, 0))));
	      return 1;
	    }
	  return 1;
	}
      else if (GET_CODE (operand1) != CONST_INT
	       || ! cint_ok_for_move (INTVAL (operand1)))
	{
	  rtx temp;

	  if (reload_in_progress || reload_completed)
	    temp = operand0;
	  else
	    temp = gen_reg_rtx (mode);

	  emit_insn (gen_rtx (SET, VOIDmode, temp,
			      gen_rtx (HIGH, mode, operand1)));
	  operands[1] = gen_rtx (LO_SUM, mode, temp, operand1);
	}
    }
  /* Now have insn-emit do whatever it normally does.  */
  return 0;
}

/* Does operand (which is a symbolic_operand) live in text space? If
   so SYMBOL_REF_FLAG, which is set by ENCODE_SECTION_INFO, will be true.  */

int
read_only_operand (operand)
     rtx operand;
{
  if (GET_CODE (operand) == CONST)
    operand = XEXP (XEXP (operand, 0), 0);
  if (GET_CODE (operand) == SYMBOL_REF)
    return SYMBOL_REF_FLAG (operand) || CONSTANT_POOL_ADDRESS_P (operand);
  return 1;
}


/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.   */
char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[0]) == MEM)
    return "stw %r1,%0";
  else if (GET_CODE (operands[1]) == MEM)
    return "ldw %1,%0";
  else if (GET_CODE (operands[1]) == CONST_DOUBLE
	   && GET_MODE (operands[1]) == SFmode)
    {
      int i;
      union real_extract u;
      union float_extract { float f; int i; } v;

      bcopy (&CONST_DOUBLE_LOW (operands[1]), &u, sizeof u);
      v.f = REAL_VALUE_TRUNCATE (SFmode, u.d);
      i = v.i;

      operands[1] = gen_rtx (CONST_INT, VOIDmode, i);

      /* See if we can handle this constant in a single instruction.  */
      if (cint_ok_for_move (INTVAL (operands[1])))
	{
	   HOST_WIDE_INT intval = INTVAL (operands[1]);

	   if (intval == 0)
	     return "copy 0,%0";
	   else if (VAL_14_BITS_P (intval))
	     return "ldi %1,%0";
	   else if ((intval & 0x7ff) == 0)
	     return "ldil L'%1,%0";
	   else if (zdepi_cint_p (intval))
	     return "zdepi %Z1,%0";
	}
      else
	return "ldil L'%1,%0\n\tldo R'%1(%0),%0";
    }

  else if (GET_CODE (operands[1]) == CONST_INT)
    {
      /* See if we can handle this in a single instruction.  */
      if (cint_ok_for_move (INTVAL (operands[1])))
	{
	   int intval = INTVAL (operands[1]);

	   if (intval == 0)
	     return "copy 0,%0";
	   else if (VAL_14_BITS_P (intval))
	     return "ldi %1,%0";
	   else if ((intval & 0x7ff) == 0)
	     return "ldil L'%1,%0";
	   else if (zdepi_cint_p (intval))
	     return "zdepi %Z1,%0";
	}
      else
	return "ldil L'%1,%0\n\tldo R'%1(%0),%0";
    }
  return "copy %1,%0";
}


/* Compute position (in OP[1]) and width (in OP[2])
   useful for copying IMM to a register using the zdepi
   instructions.  Store the immediate value to insert in OP[0].  */
void
compute_zdepi_operands (imm, op)
     unsigned HOST_WIDE_INT imm;
     unsigned *op;
{
  int lsb, len;

  /* Find the least significant set bit in IMM.  */
  for (lsb = 0; lsb < 32; lsb++)
    {
      if ((imm & 1) != 0)
        break;
      imm >>= 1;
    }

  /* Choose variants based on *sign* of the 5-bit field.  */
  if ((imm & 0x10) == 0)
    len = (lsb <= 28) ? 4 : 32 - lsb;
  else
    {
      /* Find the width of the bitstring in IMM.  */
      for (len = 5; len < 32; len++)
	{
	  if ((imm & (1 << len)) == 0)
	    break;
	}

      /* Sign extend IMM as a 5-bit value.  */
      imm = (imm & 0xf) - 0x10;
    }

  op[0] = imm;
  op[1] = 31 - lsb;
  op[2] = len;
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
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
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 != REGOP && optype1 != REGOP)
    abort ();

   /* Handle auto decrementing and incrementing loads and stores
     specifically, since the structure of the function doesn't work
     for them without major modification.  Do it better when we learn
     this port about the general inc/dec addressing of PA.
     (This was written by tege.  Chide him if it doesn't work.)  */

  if (optype0 == MEMOP)
    {
      /* We have to output the address syntax ourselves, since print_operand
	 doesn't deal with the addresses we want to use.  Fix this later.  */

      rtx addr = XEXP (operands[0], 0);
      if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[1], 0);

	  operands[0] = XEXP (addr, 0);
	  if (GET_CODE (operands[1]) != REG || GET_CODE (operands[0]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == POST_INC)
		return "stws,ma %1,8(0,%0)\n\tstw %R1,-4(0,%0)";
	      return "stws,ma %1,-8(0,%0)\n\tstw %R1,12(0,%0)";
	    }
	  else
	    abort();
	}
      else if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[1], 0);

	  operands[0] = XEXP (addr, 0);
	  if (GET_CODE (operands[1]) != REG || GET_CODE (operands[0]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == PRE_INC)
		return "stws,mb %1,8(0,%0)\n\tstw %R1,4(0,%0)";
	      return "stws,mb %1,-8(0,%0)\n\tstw %R1,4(0,%0)";
	    }
	  else
	    abort();
	}
    }
  if (optype1 == MEMOP)
    {
      /* We have to output the address syntax ourselves, since print_operand
	 doesn't deal with the addresses we want to use.  Fix this later.  */

      rtx addr = XEXP (operands[1], 0);
      if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[0], 0);

	  operands[1] = XEXP (addr, 0);
	  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[1]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == POST_INC)
		return "ldws,ma 8(0,%1),%0\n\tldw -4(0,%1),%R0";
	      return "ldws,ma -8(0,%1),%0\n\tldw 12(0,%1),%R0";
	    }
	  else
	    {
	      /* This is an undefined situation.  We should load into the
		 address register *and* update that register.  Probably
		 we don't need to handle this at all.  */
	      if (GET_CODE (addr) == POST_INC)
		return "ldw 4(0,%1),%R0\n\tldws,ma 8(0,%1),%0";
	      return "ldw 4(0,%1),%R0\n\tldws,ma -8(0,%1),%0";
	    }
	}
      else if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[0], 0);

	  operands[1] = XEXP (addr, 0);
	  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[1]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == PRE_INC)
		return "ldws,mb 8(0,%1),%0\n\tldw 4(0,%1),%R0";
	      return "ldws,mb -8(0,%1),%0\n\tldw 4(0,%1),%R0";
	    }
	  else
	    {
	      /* This is an undefined situation.  We should load into the
		 address register *and* update that register.  Probably
		 we don't need to handle this at all.  */
	      if (GET_CODE (addr) == PRE_INC)
		return "ldw 12(0,%1),%R0\n\tldws,mb 8(0,%1),%0";
	      return "ldw -4(0,%1),%R0\n\tldws,mb -8(0,%1),%0";
	    }
	}
    }

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first.

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

  /* If the first move would clobber the source of the second one,
     do them in the other order.

     RMS says "This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance."

     but it happens on the HP-PA when loading parameter registers,
     so I am going to define that circumstance, and make it work
     as expected.  */

  if (optype0 == REGOP && (optype1 == MEMOP || optype1 == OFFSOP)
	   && reg_overlap_mentioned_p (operands[0], XEXP (operands[1], 0)))
    {
      /* XXX THIS PROBABLY DOESN'T WORK.  */
      /* Do the late half first.  */
      if (addreg1)
	output_asm_insn ("ldo 4(%0),%0", &addreg1);
      output_asm_insn (singlemove_string (latehalf), latehalf);
      if (addreg1)
	output_asm_insn ("ldo -4(%0),%0", &addreg1);
      /* Then clobber.  */
      return singlemove_string (operands);
    }

  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (operands[1]) + 1)
    {
      output_asm_insn (singlemove_string (latehalf), latehalf);
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("ldo 4(%0),%0", &addreg0);
  if (addreg1)
    output_asm_insn ("ldo 4(%0),%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("ldo -4(%0),%0", &addreg0);
  if (addreg1)
    output_asm_insn ("ldo -4(%0),%0", &addreg1);

  return "";
}

char *
output_fp_move_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1])
	  || operands[1] == CONST0_RTX (GET_MODE (operands[0])))
	output_asm_insn ("fcpy,dbl %r1,%0", operands);
      else
	output_asm_insn ("fldds%F1 %1,%0", operands);
    }
  else if (FP_REG_P (operands[1]))
    {
      output_asm_insn ("fstds%F0 %1,%0", operands);
    }
  else if (operands[1] == CONST0_RTX (GET_MODE (operands[0])))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  rtx xoperands[2];
	  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  xoperands[0] = operands[0];
	  output_asm_insn ("copy %%r0,%0\n\tcopy %%r0,%1", xoperands);
	}
      /* This is a pain.  You have to be prepared to deal with an
	 arbritary address here including pre/post increment/decrement.

	 so avoid this in the MD.  */
      else
	abort ();
    }
  else abort ();
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

/* Emit code to perform a block move.

   Restriction: If the length argument is non-constant, alignment
   must be 4.

   OPERANDS[0] is the destination pointer as a REG, clobbered.
   OPERANDS[1] is the source pointer as a REG, clobbered.
   if SIZE_IS_CONSTANT
     OPERANDS[2] is a register for temporary storage.
     OPERANDS[4] is the size as a CONST_INT
   else
     OPERANDS[2] is a REG which will contain the size, clobbered.
   OPERANDS[3] is a register for temporary storage.
   OPERANDS[5] is the alignment safe to use, as a CONST_INT.  */

char *
output_block_move (operands, size_is_constant)
     rtx *operands;
     int size_is_constant;
{
  int align = INTVAL (operands[5]);
  unsigned long n_bytes;

  /* We can't move more than four bytes at a time because the PA
     has no longer integer move insns.  (Could use fp mem ops?)  */
  if (align > 4)
    align = 4;

  if (size_is_constant)
    {
      unsigned long offset;
      rtx temp;

      n_bytes = INTVAL (operands[4]);
      if (n_bytes == 0)
	return "";

      if (align >= 4)
	{
	  /* Don't unroll too large blocks.  */
	  if (n_bytes > 32)
	    goto copy_with_loop;

	  /* Read and store using two registers, and hide latency
	     by deferring the stores until three instructions after
	     the corresponding load.  The last load insn will read
	     the entire word were the last bytes are, possibly past
	     the end of the source block, but since loads are aligned,
	     this is harmless.  */

	  output_asm_insn ("ldws,ma 4(0,%1),%2", operands);

	  for (offset = 4; offset < n_bytes; offset += 4)
	    {
	      output_asm_insn ("ldws,ma 4(0,%1),%3", operands);
	      output_asm_insn ("stws,ma %2,4(0,%0)", operands);

	      temp = operands[2];
	      operands[2] = operands[3];
	      operands[3] = temp;
	    }
	  if (n_bytes % 4 == 0)
	    /* Store the last word.  */
	    output_asm_insn ("stw %2,0(0,%0)", operands);
	  else
	    {
	      /* Store the last, partial word.  */
	      operands[4] = gen_rtx (CONST_INT, VOIDmode, n_bytes % 4);
	      output_asm_insn ("stbys,e %2,%4(0,%0)", operands);
	    }
	  return "";
	}

      if (align >= 2 && n_bytes >= 2)
	{
	  output_asm_insn ("ldhs,ma 2(0,%1),%2", operands);

	  for (offset = 2; offset + 2 <= n_bytes; offset += 2)
	    {
	      output_asm_insn ("ldhs,ma 2(0,%1),%3", operands);
	      output_asm_insn ("sths,ma %2,2(0,%0)", operands);

	      temp = operands[2];
	      operands[2] = operands[3];
	      operands[3] = temp;
	    }
	  if (n_bytes % 2 != 0)
	    output_asm_insn ("ldb 0(0,%1),%3", operands);

	  output_asm_insn ("sths,ma %2,2(0,%0)", operands);

	  if (n_bytes % 2 != 0)
	    output_asm_insn ("stb %3,0(0,%0)", operands);

	  return "";
	}

      output_asm_insn ("ldbs,ma 1(0,%1),%2", operands);

      for (offset = 1; offset + 1 <= n_bytes; offset += 1)
	{
	  output_asm_insn ("ldbs,ma 1(0,%1),%3", operands);
	  output_asm_insn ("stbs,ma %2,1(0,%0)", operands);

	  temp = operands[2];
	  operands[2] = operands[3];
	  operands[3] = temp;
	}
      output_asm_insn ("stb %2,0(0,%0)", operands);

      return "";
    }

  if (align != 4)
    abort();

 copy_with_loop:

  if (size_is_constant)
    {
      /* Size is compile-time determined, and also not
	 very small (such small cases are handled above).  */
      operands[4] = gen_rtx (CONST_INT, VOIDmode, n_bytes - 4);
      output_asm_insn ("ldo %4(0),%2", operands);
    }
  else
    {
      /* Decrement counter by 4, and if it becomes negative, jump past the
	 word copying loop.  */
      output_asm_insn ("addib,<,n -4,%2,.+16", operands);
    }

  /* Copying loop.  Note that the first load is in the annulled delay slot
     of addib.  Is it OK on PA to have a load in a delay slot, i.e. is a
     possible page fault stopped in time?  */
  output_asm_insn ("ldws,ma 4(0,%1),%3", operands);
  output_asm_insn ("addib,>= -4,%2,.-4", operands);
  output_asm_insn ("stws,ma %3,4(0,%0)", operands);

  /* The counter is negative, >= -4.  The remaining number of bytes are
     determined by the two least significant bits.  */

  if (size_is_constant)
    {
      if (n_bytes % 4 != 0)
	{
	  /* Read the entire word of the source block tail.  */
	  output_asm_insn ("ldw 0(0,%1),%3", operands);
	  operands[4] = gen_rtx (CONST_INT, VOIDmode, n_bytes % 4);
	  output_asm_insn ("stbys,e %3,%4(0,%0)", operands);
	}
    }
  else
    {
      /* Add 4 to counter.  If it becomes zero, we're done.  */
      output_asm_insn ("addib,=,n 4,%2,.+16", operands);

      /* Read the entire word of the source block tail.  (Also this
	 load is in an annulled delay slot.)  */
      output_asm_insn ("ldw 0(0,%1),%3", operands);

      /* Make %0 point at the first byte after the destination block.  */
      output_asm_insn ("addl %2,%0,%0", operands);
      /* Store the leftmost bytes, up to, but not including, the address
	 in %0.  */
      output_asm_insn ("stbys,e %3,0(0,%0)", operands);
    }
  return "";
}

/* Count the number of insns necessary to handle this block move.

   Basic structure is the same as emit_block_move, except that we
   count insns rather than emit them.  */

int
compute_movstrsi_length (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);
  int size_is_constant;
  int align = INTVAL (XEXP (XVECEXP (pat, 0, 6), 0));
  unsigned long n_bytes;
  int insn_count = 0;

  if (GET_CODE (XEXP (XVECEXP (pat, 0, 5), 0)) == CONST_INT)
    {
      size_is_constant = 1;
      n_bytes = INTVAL (XEXP (XVECEXP (pat, 0, 5), 0));
    }
  else
    {
      size_is_constant = 0;
      n_bytes = 0;
    }

  /* We can't move more than four bytes at a time because the PA
     has no longer integer move insns.  (Could use fp mem ops?)  */
  if (align > 4)
    align = 4;

  if (size_is_constant)
    {
      unsigned long offset;

      if (n_bytes == 0)
	return 0;

      if (align >= 4)
	{
	  /* Don't unroll too large blocks.  */
	  if (n_bytes > 32)
	    goto copy_with_loop;

	  /* first load */
	  insn_count = 1;

	  /* Count the unrolled insns.  */
	  for (offset = 4; offset < n_bytes; offset += 4)
	    insn_count += 2;

	  /* Count last store or partial store.  */
	  insn_count += 1;
	  return insn_count * 4;
	}

      if (align >= 2 && n_bytes >= 2)
	{
	  /* initial load.  */
	  insn_count = 1;

	  /* Unrolled loop.  */
	  for (offset = 2; offset + 2 <= n_bytes; offset += 2)
	    insn_count += 2;

	  /* ??? odd load/store */
	  if (n_bytes % 2 != 0)
	    insn_count += 2;

	  /* ??? final store from loop.  */
	  insn_count += 1;

	  return insn_count * 4;
	}

      /* First load.  */
      insn_count = 1;

      /* The unrolled loop.  */
      for (offset = 1; offset + 1 <= n_bytes; offset += 1)
	insn_count += 2;

      /* Final store.  */
      insn_count += 1;

      return insn_count * 4;
    }

  if (align != 4)
    abort();

 copy_with_loop:

  /* setup for constant and non-constant case.  */
  insn_count = 1;

  /* The copying loop.  */
  insn_count += 3;

  /* The counter is negative, >= -4.  The remaining number of bytes are
     determined by the two least significant bits.  */

  if (size_is_constant)
    {
      if (n_bytes % 4 != 0)
	insn_count += 2;
    }
  else
    insn_count += 4;
  return insn_count * 4;
}


char *
output_and (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    {
      unsigned mask = INTVAL (operands[2]);
      int ls0, ls1, ms0, p, len;

      for (ls0 = 0; ls0 < 32; ls0++)
	if ((mask & (1 << ls0)) == 0)
	  break;

      for (ls1 = ls0; ls1 < 32; ls1++)
	if ((mask & (1 << ls1)) != 0)
	  break;

      for (ms0 = ls1; ms0 < 32; ms0++)
	if ((mask & (1 << ms0)) == 0)
	  break;

      if (ms0 != 32)
	abort();

      if (ls1 == 32)
	{
	  len = ls0;

	  if (len == 0)
	    abort ();

	  operands[2] = gen_rtx (CONST_INT, VOIDmode, len);
	  return "extru %1,31,%2,%0";
	}
      else
	{
	  /* We could use this `depi' for the case above as well, but `depi'
	     requires one more register file access than an `extru'.  */

	  p = 31 - ls0;
	  len = ls1 - ls0;

	  operands[2] = gen_rtx (CONST_INT, VOIDmode, p);
	  operands[3] = gen_rtx (CONST_INT, VOIDmode, len);
	  return "depi 0,%2,%3,%0";
	}
    }
  else
    return "and %1,%2,%0";
}

char *
output_ior (operands)
     rtx *operands;
{
  unsigned mask = INTVAL (operands[2]);
  int bs0, bs1, p, len;

  if (INTVAL (operands[2]) == 0)
    return "copy %1,%0";

  for (bs0 = 0; bs0 < 32; bs0++)
    if ((mask & (1 << bs0)) != 0)
      break;

  for (bs1 = bs0; bs1 < 32; bs1++)
    if ((mask & (1 << bs1)) == 0)
      break;

  if (bs1 != 32 && ((unsigned) 1 << bs1) <= mask)
    abort();

  p = 31 - bs0;
  len = bs1 - bs0;

  operands[2] = gen_rtx (CONST_INT, VOIDmode, p);
  operands[3] = gen_rtx (CONST_INT, VOIDmode, len);
  return "depi -1,%2,%3,%0";
}

/* Output an ascii string.  */
void
output_ascii (file, p, size)
     FILE *file;
     unsigned char *p;
     int size;
{
  int i;
  int chars_output;
  unsigned char partial_output[16];	/* Max space 4 chars can occupy.   */

  /* The HP assembler can only take strings of 256 characters at one
     time.  This is a limitation on input line length, *not* the
     length of the string.  Sigh.  Even worse, it seems that the
     restriction is in number of input characters (see \xnn &
     \whatever).  So we have to do this very carefully.  */

  fprintf (file, "\t.STRING \"");

  chars_output = 0;
  for (i = 0; i < size; i += 4)
    {
      int co = 0;
      int io = 0;
      for (io = 0, co = 0; io < MIN (4, size - i); io++)
	{
	  register unsigned int c = p[i + io];

	  if (c == '\"' || c == '\\')
	    partial_output[co++] = '\\';
	  if (c >= ' ' && c < 0177)
	    partial_output[co++] = c;
	  else
	    {
	      unsigned int hexd;
	      partial_output[co++] = '\\';
	      partial_output[co++] = 'x';
	      hexd =  c  / 16 - 0 + '0';
	      if (hexd > '9')
		hexd -= '9' - 'a' + 1;
	      partial_output[co++] = hexd;
	      hexd =  c % 16 - 0 + '0';
	      if (hexd > '9')
		hexd -= '9' - 'a' + 1;
	      partial_output[co++] = hexd;
	    }
	}
      if (chars_output + co > 243)
	{
	  fprintf (file, "\"\n\t.STRING \"");
	  chars_output = 0;
	}
      fwrite (partial_output, 1, co, file);
      chars_output += co;
      co = 0;
    }
  fprintf (file, "\"\n");
}

/* You may have trouble believing this, but this is the HP-PA stack
   layout.  Wow.

   Offset		Contents

   Variable arguments	(optional; any number may be allocated)

   SP-(4*(N+9))		arg word N
   	:		    :
      SP-56		arg word 5
      SP-52		arg word 4

   Fixed arguments	(must be allocated; may remain unused)

      SP-48		arg word 3
      SP-44		arg word 2
      SP-40		arg word 1
      SP-36		arg word 0

   Frame Marker

      SP-32		External Data Pointer (DP)
      SP-28		External sr4
      SP-24		External/stub RP (RP')
      SP-20		Current RP
      SP-16		Static Link
      SP-12		Clean up
      SP-8		Calling Stub RP (RP'')
      SP-4		Previous SP

   Top of Frame

      SP-0		Stack Pointer (points to next available address)

*/

/* This function saves registers as follows.  Registers marked with ' are
   this function's registers (as opposed to the previous function's).
   If a frame_pointer isn't needed, r4 is saved as a general register;
   the space for the frame pointer is still allocated, though, to keep
   things simple.


   Top of Frame

       SP (FP')		Previous FP
       SP + 4		Alignment filler (sigh)
       SP + 8		Space for locals reserved here.
       .
       .
       .
       SP + n		All call saved register used.
       .
       .
       .
       SP + o		All call saved fp registers used.
       .
       .
       .
       SP + p (SP')	points to next available address.

*/

/* Emit RTL to store REG at the memory location specified by BASE+DISP.
   Handle case where DISP > 8k by using the add_high_const pattern.

   Note in DISP > 8k case, we will leave the high part of the address
   in %r1.  There is code in expand_hppa_{prologue,epilogue} that knows this.*/
static void
store_reg (reg, disp, base)
     int reg, disp, base;
{
  if (VAL_14_BITS_P (disp))
    {
      emit_move_insn (gen_rtx (MEM, SImode,
			       gen_rtx (PLUS, SImode,
				        gen_rtx (REG, SImode, base),
				        GEN_INT (disp))),
		      gen_rtx (REG, SImode, reg));
    }
  else
    {
      emit_insn (gen_add_high_const (gen_rtx (REG, SImode, 1),
				     gen_rtx (REG, SImode, base),
				     GEN_INT (disp)));
      emit_move_insn (gen_rtx (MEM, SImode,
			       gen_rtx (LO_SUM, SImode,
					gen_rtx (REG, SImode, 1),
					GEN_INT (disp))),
		      gen_rtx (REG, SImode, reg));
    }
}

/* Emit RTL to load REG from the memory location specified by BASE+DISP.
   Handle case where DISP > 8k by using the add_high_const pattern.

   Note in DISP > 8k case, we will leave the high part of the address
   in %r1.  There is code in expand_hppa_{prologue,epilogue} that knows this.*/
static void
load_reg (reg, disp, base)
     int reg, disp, base;
{
  if (VAL_14_BITS_P (disp))
    {
      emit_move_insn (gen_rtx (REG, SImode, reg),
		      gen_rtx (MEM, SImode,
			       gen_rtx (PLUS, SImode,
				        gen_rtx (REG, SImode, base),
				        GEN_INT (disp))));
    }
  else
    {
      emit_insn (gen_add_high_const (gen_rtx (REG, SImode, 1),
				     gen_rtx (REG, SImode, base),
				     GEN_INT (disp)));
      emit_move_insn (gen_rtx (REG, SImode, reg),
		      gen_rtx (MEM, SImode,
			       gen_rtx (LO_SUM, SImode,
					gen_rtx (REG, SImode, 1),
					GEN_INT (disp))));
    }
}

/* Emit RTL to set REG to the value specified by BASE+DISP.
   Handle case where DISP > 8k by using the add_high_const pattern.

   Note in DISP > 8k case, we will leave the high part of the address
   in %r1.  There is code in expand_hppa_{prologue,epilogue} that knows this.*/
static void
set_reg_plus_d(reg, base, disp)
     int reg, base, disp;
{
  if (VAL_14_BITS_P (disp))
    {
      emit_move_insn (gen_rtx (REG, SImode, reg),
		      gen_rtx (PLUS, SImode,
			       gen_rtx (REG, SImode, base),
			       GEN_INT (disp)));
    }
  else
    {
      emit_insn (gen_add_high_const (gen_rtx (REG, SImode, 1),
				     gen_rtx (REG, SImode, base),
				     GEN_INT (disp)));
      emit_move_insn (gen_rtx (REG, SImode, reg),
		      gen_rtx (LO_SUM, SImode,
					gen_rtx (REG, SImode, 1),
					GEN_INT (disp)));
    }
}

/* Global variables set by FUNCTION_PROLOGUE.  */
/* Size of frame.  Need to know this to emit return insns from
   leaf procedures.  */
static int actual_fsize;
static int local_fsize, save_fregs;

int
compute_frame_size (size, fregs_live)
     int size;
     int *fregs_live;
{
  extern int current_function_outgoing_args_size;
  int i, fsize;

  /* 8 is space for frame pointer + filler. If any frame is allocated
     we need to add this in because of STARTING_FRAME_OFFSET. */
  fsize = size + (size || frame_pointer_needed ? 8 : 0);

  for (i = 18; i >= 4; i--)
    {
      if (regs_ever_live[i])
	fsize += 4;
    }
  /* If we don't have a frame pointer, the register normally used for that
     purpose is saved just like other registers, not in the "frame marker".  */
  if (! frame_pointer_needed)
    {
      if (regs_ever_live[FRAME_POINTER_REGNUM])
	fsize += 4;
    }
  fsize = (fsize + 7) & ~7;

  for (i = 66; i >= 48; i -= 2)
    if (regs_ever_live[i] || regs_ever_live[i + 1])
      {
	fsize += 8;
	if (fregs_live)
	  *fregs_live = 1;
      }

  fsize += current_function_outgoing_args_size;
  if (! leaf_function_p () || fsize)
    fsize += 32;
  return (fsize + 63) & ~63;
}

rtx hp_profile_label_rtx;
static char hp_profile_label_name[8];
void
output_function_prologue (file, size)
     FILE *file;
     int size;
{
  /* The function's label and associated .PROC must never be
     separated and must be output *after* any profiling declarations
     to avoid changing spaces/subspaces within a procedure.  */
  ASM_OUTPUT_LABEL (file, XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));
  fputs ("\t.PROC\n", file);

  /* hppa_expand_prologue does the dirty work now.  We just need
     to output the assembler directives which denote the start
     of a function.  */
  fprintf (file, "\t.CALLINFO FRAME=%d", actual_fsize);
  if (regs_ever_live[2] || profile_flag)
    fprintf (file, ",CALLS,SAVE_RP");
  else
    fprintf (file, ",NO_CALLS");

  if (frame_pointer_needed)
    fprintf (file, ",SAVE_SP");

  /* Pass on information about the number of callee register saves
     performed in the prologue.

     The compiler is supposed to pass the highest register number
     saved, the assembler then has to adjust that number before
     entering it into the unwind descriptor (to account for any
     caller saved registers with lower register numbers than the
     first callee saved register).  */
  if (gr_saved)
    fprintf (file, ",ENTRY_GR=%d", gr_saved + 2);

  if (fr_saved)
    fprintf (file, ",ENTRY_FR=%d", fr_saved + 11);

  fprintf (file, "\n\t.ENTRY\n");

  /* Horrid hack.  emit_function_prologue will modify this RTL in
     place to get the expected results.  */
  if (profile_flag)
    ASM_GENERATE_INTERNAL_LABEL (hp_profile_label_name, "LP",
				 hp_profile_labelno);
}

void
hppa_expand_prologue()
{
  extern char call_used_regs[];
  int size = get_frame_size ();
  int merge_sp_adjust_with_store = 0;
  int i, offset;
  rtx tmpreg, size_rtx;

  gr_saved = 0;
  fr_saved = 0;
  save_fregs = 0;
  local_fsize =  size + (size || frame_pointer_needed ? 8 : 0);
  actual_fsize = compute_frame_size (size, &save_fregs);

  /* Compute a few things we will use often.  */
  tmpreg = gen_rtx (REG, SImode, 1);
  size_rtx = GEN_INT (actual_fsize);

  /* Save RP first.  The calling conventions manual states RP will
     always be stored into the caller's frame at sp-20.  */
  if (regs_ever_live[2] || profile_flag)
    store_reg (2, -20, STACK_POINTER_REGNUM);

  /* Allocate the local frame and set up the frame pointer if needed.  */
  if (actual_fsize)
    if (frame_pointer_needed)
      {
	/* Copy the old frame pointer temporarily into %r1.  Set up the
	   new stack pointer, then store away the saved old frame pointer
	   into the stack at sp+actual_fsize and at the same time update
	   the stack pointer by actual_fsize bytes.  Two versions, first
	   handles small (<8k) frames.  The second handles large (>8k)
	   frames.  */
	emit_move_insn (tmpreg, frame_pointer_rtx);
	emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
	if (VAL_14_BITS_P (actual_fsize))
	  emit_insn (gen_post_stwm (stack_pointer_rtx,
				    stack_pointer_rtx,
				    size_rtx, tmpreg));
	else
	  {
	    /* It is incorrect to store the saved frame pointer at *sp,
	       then increment sp (writes beyond the current stack boundary).

	       So instead use stwm to store at *sp and post-increment the
	       stack pointer as an atomic operation.  Then increment sp to
	       finish allocating the new frame.  */
	    emit_insn (gen_post_stwm (stack_pointer_rtx,
				      stack_pointer_rtx,
				      GEN_INT (64), tmpreg));
	    set_reg_plus_d (STACK_POINTER_REGNUM,
			    STACK_POINTER_REGNUM,
			    actual_fsize - 64);
	  }
      }
    /* no frame pointer needed.  */
    else
      {
	/* In some cases we can perform the first callee register save
	   and allocating the stack frame at the same time.   If so, just
	   make a note of it and defer allocating the frame until saving
	   the callee registers.  */
	if (VAL_14_BITS_P (-actual_fsize)
	    && local_fsize == 0
	    && ! profile_flag
	    && ! flag_pic)
	  merge_sp_adjust_with_store = 1;
	/* Can not optimize.  Adjust the stack frame by actual_fsize bytes.  */
	else if (actual_fsize != 0)
	  set_reg_plus_d (STACK_POINTER_REGNUM,
			  STACK_POINTER_REGNUM,
			  actual_fsize);
      }
  /* The hppa calling conventions say that that %r19, the pic offset
     register, is saved at sp - 32 (in this function's frame)  when
     generating PIC code.  */
  if (flag_pic)
    store_reg (PIC_OFFSET_TABLE_REGNUM, -32, STACK_POINTER_REGNUM);

  /* Profiling code.

     Instead of taking one argument, the counter label, as most normal
     mcounts do, _mcount appears to behave differently on the HPPA.  It
     takes the return address of the caller, the address of this routine,
     and the address of the label.  Also, it isn't magic, so
     argument registre hsave to be preserved.  */
  if (profile_flag)
    {
      int pc_offset, i, arg_offset, basereg, offsetadj;

      pc_offset = 4 + (frame_pointer_needed
		       ? (VAL_14_BITS_P (actual_fsize) ? 12 : 20)
		       : (VAL_14_BITS_P (actual_fsize) ? 4 : 8));

      /* When the function has a frame pointer, use it as the base
	 register for saving/restore registers.  Else use the stack
	 pointer.  Adjust the offset according to the frame size if
	 this function does not have a frame pointer.  */

      basereg = frame_pointer_needed ? FRAME_POINTER_REGNUM
				     : STACK_POINTER_REGNUM;
      offsetadj = frame_pointer_needed ? 0 : actual_fsize;

      /* Horrid hack.  emit_function_prologue will modify this RTL in
	 place to get the expected results.   sprintf here is just to
	 put something in the name.  */
      sprintf(hp_profile_label_name, "LP$%04d", -1);
      hp_profile_label_rtx = gen_rtx (SYMBOL_REF, SImode,
				      hp_profile_label_name);
      if (current_function_returns_struct)
	store_reg (STRUCT_VALUE_REGNUM, - 12 - offsetadj, basereg);

      for (i = 26, arg_offset = -36 - offsetadj; i >= 23; i--, arg_offset -= 4)
	if (regs_ever_live [i])
	  {
	    store_reg (i, arg_offset, basereg);
	    /* Deal with arg_offset not fitting in 14 bits.  */
	    pc_offset += VAL_14_BITS_P (arg_offset) ? 4 : 8;
	  }

      emit_move_insn (gen_rtx (REG, SImode, 26), gen_rtx (REG, SImode, 2));
      emit_move_insn (tmpreg, gen_rtx (HIGH, SImode, hp_profile_label_rtx));
      emit_move_insn (gen_rtx (REG, SImode, 24),
		      gen_rtx (LO_SUM, SImode, tmpreg, hp_profile_label_rtx));
      /* %r25 is set from within the output pattern.  */
      emit_insn (gen_call_profiler (GEN_INT (- pc_offset - 20)));

      /* Restore argument registers.  */
      for (i = 26, arg_offset = -36 - offsetadj; i >= 23; i--, arg_offset -= 4)
	if (regs_ever_live [i])
	  load_reg (i, arg_offset, basereg);

      if (current_function_returns_struct)
	load_reg (STRUCT_VALUE_REGNUM, -12 - offsetadj, basereg);

    }

  /* Normal register save.

     Do not save the frame pointer in the frame_pointer_needed case.  It
     was done earlier.  */
  if (frame_pointer_needed)
    {
      for (i = 18, offset = local_fsize; i >= 4; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    store_reg (i, offset, FRAME_POINTER_REGNUM);
	    offset += 4;
	    gr_saved++;
	  }
      /* Account for %r4 which is saved in a special place.  */
      gr_saved++;
    }
  /* No frame pointer needed.  */
  else
    {
      for (i = 18, offset = local_fsize - actual_fsize; i >= 3; i--)
      	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    /* If merge_sp_adjust_with_store is nonzero, then we can
	       optimize the first GR save.  */
	    if (merge_sp_adjust_with_store)
	      {
		merge_sp_adjust_with_store = 0;
	        emit_insn (gen_post_stwm (stack_pointer_rtx,
					  stack_pointer_rtx,
					  GEN_INT (-offset),
					  gen_rtx (REG, SImode, i)));
	      }
	    else
	      store_reg (i, offset, STACK_POINTER_REGNUM);
	    offset += 4;
	    gr_saved++;
	  }

      /* If we wanted to merge the SP adjustment with a GR save, but we never
	 did any GR saves, then just emit the adjustment here.  */
      if (merge_sp_adjust_with_store)
	set_reg_plus_d (STACK_POINTER_REGNUM,
			STACK_POINTER_REGNUM,
			actual_fsize);
    }

  /* Align pointer properly (doubleword boundary).  */
  offset = (offset + 7) & ~7;

  /* Floating point register store.  */
  if (save_fregs)
    {

      /* First get the frame or stack pointer to the start of the FP register
	 save area.  */
      if (frame_pointer_needed)
	set_reg_plus_d (1, FRAME_POINTER_REGNUM, offset);
      else
	set_reg_plus_d (1, STACK_POINTER_REGNUM, offset);

      /* Now actually save the FP registers.  */
      for (i = 66; i >= 48; i -= 2)
	if (regs_ever_live[i] || regs_ever_live[i + 1])
	  {
	    emit_move_insn (gen_rtx (MEM, DFmode,
				     gen_rtx (POST_INC, DFmode, tmpreg)),
			    gen_rtx (REG, DFmode, i));
	    fr_saved++;
	  }
    }
}


void
output_function_epilogue (file, size)
     FILE *file;
     int size;
{

  rtx insn = get_last_insn ();

  /* hppa_expand_epilogue does the dirty work now.  We just need
     to output the assembler directives which denote the end
     of a function.

     To make debuggers happy, emit a nop if the epilogue was completely
     eliminated due to a volatile call as the last insn in the
     current function.  That way the return address (in %r2) will
     always point to a valid instruction in the current function.  */

  /* Get the last real insn.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_real_insn (insn);

  /* If it is a sequence, then look inside.  */
  if (insn && GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
    insn = XVECEXP (PATTERN (insn), 0, 0);

  /* If insn is a CALL_INSN, then it must be a call to a volatile
     function (otherwise there would be epilogue insns).  */
  if (insn && GET_CODE (insn) == CALL_INSN)
    fprintf (file, "\tnop\n");

  fprintf (file, "\t.EXIT\n\t.PROCEND\n");
}

void
hppa_expand_epilogue ()
{
  rtx tmpreg;
  int offset,i;
  int merge_sp_adjust_with_load  = 0;

  /* We will use this often.  */
  tmpreg = gen_rtx (REG, SImode, 1);

  /* Try to restore RP early to avoid load/use interlocks when
     RP gets used in the return (bv) instruction.  This appears to still
     be necessary even when we schedule the prologue and epilogue. */
  if (frame_pointer_needed
      && (regs_ever_live [2] || profile_flag))
    load_reg (2, -20, FRAME_POINTER_REGNUM);

  /* No frame pointer, and stack is smaller than 8k.  */
  else if (! frame_pointer_needed
	   && VAL_14_BITS_P (actual_fsize + 20)
	   && (regs_ever_live[2] || profile_flag))
    load_reg (2, - (actual_fsize + 20), STACK_POINTER_REGNUM);

  /* General register restores.  */
  if (frame_pointer_needed)
    {
      for (i = 18, offset = local_fsize; i >= 4; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    load_reg (i, offset, FRAME_POINTER_REGNUM);
	    offset += 4;
	  }
    }
  else
    {
      for (i = 18, offset = local_fsize - actual_fsize; i >= 3; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    /* Only for the first load.
	       merge_sp_adjust_with_load holds the register load
	       with which we will merge the sp adjustment.  */
	    if (VAL_14_BITS_P (actual_fsize + 20)
		&& local_fsize == 0
		&& ! merge_sp_adjust_with_load)
	      merge_sp_adjust_with_load = i;
	    else
	      load_reg (i, offset, STACK_POINTER_REGNUM);
	    offset += 4;
	  }
    }

  /* Align pointer properly (doubleword boundary).  */
  offset = (offset + 7) & ~7;

  /* FP register restores.  */
  if (save_fregs)
    {
      /* Adjust the register to index off of.  */
      if (frame_pointer_needed)
	set_reg_plus_d (1, FRAME_POINTER_REGNUM, offset);
      else
	set_reg_plus_d (1, STACK_POINTER_REGNUM, offset);

      /* Actually do the restores now.  */
      for (i = 66; i >= 48; i -= 2)
	if (regs_ever_live[i] || regs_ever_live[i + 1])
	  emit_move_insn (gen_rtx (REG, DFmode, i),
			  gen_rtx (MEM, DFmode,
				   gen_rtx (POST_INC, DFmode, tmpreg)));
    }

  /* No frame pointer, but we have a stack greater than 8k.  We restore
     %r2 very late in this case.  (All other cases are restored as early
     as possible.)  */
  if (! frame_pointer_needed
      && ! VAL_14_BITS_P (actual_fsize + 20)
      && (regs_ever_live[2] || profile_flag))
    {
      set_reg_plus_d (STACK_POINTER_REGNUM,
		      STACK_POINTER_REGNUM,
		      - actual_fsize);
      /* Uses value left over in %r1 by set_reg_plus_d.  */
      load_reg (2, - (actual_fsize + 20 + ((- actual_fsize) & ~0x7ff)), 1);
    }

  /* Reset stack pointer (and possibly frame pointer).  The stack */
  /* pointer is initially set to fp + 64 to avoid a race condition.
     ??? What race condition?!?  */
  else if (frame_pointer_needed)
    {
      /* Emit a blockage insn here to keep these insns from being moved
	 to the beginning of the prologue or into the main instruction
	 stream, doing so avoids some very obscure problems.  */
      emit_insn (gen_blockage ());
      set_reg_plus_d (STACK_POINTER_REGNUM, FRAME_POINTER_REGNUM, 64);
      emit_insn (gen_pre_ldwm (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (-64), frame_pointer_rtx));
    }
  /* If we were deferring a callee register restore, do it now.  */
  else if (! frame_pointer_needed  && merge_sp_adjust_with_load)
    emit_insn (gen_pre_ldwm (stack_pointer_rtx,
			     stack_pointer_rtx,
			     GEN_INT (- actual_fsize),
			     gen_rtx (REG, SImode,
			     merge_sp_adjust_with_load)));
  else if (actual_fsize != 0)
    set_reg_plus_d (STACK_POINTER_REGNUM,
		    STACK_POINTER_REGNUM,
		    - actual_fsize);
}

/* This is only valid once reload has completed because it depends on
   knowing exactly how much (if any) frame there is and...

   It's only valid if there is no frame marker to de-allocate and...

   It's only valid if %r2 hasn't been saved into the caller's frame
   (we're not profiling and %r2 isn't live anywhere).  */
int
hppa_can_use_return_insn_p ()
{
  return (reload_completed
	  && (compute_frame_size (get_frame_size (), 0) ? 0 : 1)
	  && ! profile_flag
	  && ! regs_ever_live[2]
	  && ! frame_pointer_needed);
}

void
emit_bcond_fp (code, operand0)
     enum rtx_code code;
     rtx operand0;
{
  emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
			   gen_rtx (IF_THEN_ELSE, VOIDmode,
				    gen_rtx (code, VOIDmode,
					     gen_rtx (REG, CCFPmode, 0),
					     const0_rtx),
				    gen_rtx (LABEL_REF, VOIDmode, operand0),
				    pc_rtx)));

}

rtx
gen_cmp_fp (code, operand0, operand1)
     enum rtx_code code;
     rtx operand0, operand1;
{
  return gen_rtx (SET, VOIDmode, gen_rtx (REG, CCFPmode, 0),
		  gen_rtx (code, CCFPmode, operand0, operand1));
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

int
pa_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  if (! recog_memoized (insn))
    return 0;

  if (REG_NOTE_KIND (link) == 0)
    {
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      if (get_attr_type (insn) == TYPE_FPSTORE)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fstXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (rtx_equal_p (SET_DEST (dep_pat), SET_SRC (pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      /* DEP_INSN is writing its result to the register
		 being stored in the fpstore INSN.  */
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPLOAD:
		  /* This cost 3 cycles, not 2 as the md says.  */
		  return cost + 1;

		case TYPE_FPALU:
		case TYPE_FPMUL:
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* In these important cases, we save one cycle compared to
		     when flop instruction feed each other.  */
		  return cost - 1;

		default:
		  return cost;
		}
	    }
	}

      /* For other data dependencies, the default cost specified in the
	 md is correct.  */
      return cost;
    }
  else if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
    {
      /* Anti dependency; DEP_INSN reads a register that INSN writes some
	 cycles later.  */

      if (get_attr_type (insn) == TYPE_FPLOAD)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_SRC (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPALU:
		case TYPE_FPMUL:
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* A fpload can't be issued until one cycle before a
		     preceeding arithmetic operation has finished, if
		     the target of the fpload is any of the sources
		     (or destination) of the arithmetic operation.  */
		  return cost - 1;

		default:
		  return 0;
		}
	    }
	}

      /* For other anti dependencies, the cost is 0.  */
      return 0;
    }

  /* For output dependencies, the cost is often one too high.  */
  return cost - 1;
}

/* Return any length adjustment needed by INSN which already has its length
   computed as LENGTH.   Return zero if no adjustment is necessary.

   For the PA: function calls, millicode calls, and backwards short
   conditional branches with unfilled delay slots need an adjustment by +1
   (to account for the NOP which will be inserted into the instruction stream).

   Also compute the length of an inline block move here as it is too
   complicated to express as a length attribute in pa.md.  */
int
pa_adjust_insn_length (insn, length)
    rtx insn;
    int length;
{
  rtx pat = PATTERN (insn);

  /* Call insns which are *not* indirect and have unfilled delay slots.  */
  if (GET_CODE (insn) == CALL_INSN)
    {

      if (GET_CODE (XVECEXP (pat, 0, 0)) == CALL
	  && GET_CODE (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0)) == SYMBOL_REF)
	return 4;
      else if (GET_CODE (XVECEXP (pat, 0, 0)) == SET
	       && GET_CODE (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0))
		  == SYMBOL_REF)
	return 4;
      else
	return 0;
    }
  /* Millicode insn with an unfilled delay slot.  */
  else if (GET_CODE (insn) == INSN
	   && GET_CODE (pat) != SEQUENCE
	   && GET_CODE (pat) != USE
	   && GET_CODE (pat) != CLOBBER
	   && get_attr_type (insn) == TYPE_MILLI)
    return 4;
  /* Block move pattern.  */
  else if (GET_CODE (insn) == INSN
	   && GET_CODE (pat) == PARALLEL
	   && GET_CODE (XEXP (XVECEXP (pat, 0, 0), 0)) == MEM
	   && GET_CODE (XEXP (XVECEXP (pat, 0, 0), 1)) == MEM
	   && GET_MODE (XEXP (XVECEXP (pat, 0, 0), 0)) == BLKmode
	   && GET_MODE (XEXP (XVECEXP (pat, 0, 0), 1)) == BLKmode)
    return compute_movstrsi_length (insn) - 4;
  /* Conditional branch with an unfilled delay slot.  */
  else if (GET_CODE (insn) == JUMP_INSN && ! simplejump_p (insn))
    {
      /* Adjust a short backwards conditional with an unfilled delay slot.  */
      if (GET_CODE (pat) == SET
	  && length == 4
	  && ! forward_branch_p (insn))
	return 4;
      /* Adjust dbra insn with short backwards conditional branch with
	 unfilled delay slot -- only for case where counter is in a
	 general register register. */
      else if (GET_CODE (pat) == PARALLEL
	       && GET_CODE (XVECEXP (pat, 0, 1)) == SET
	       && GET_CODE (XEXP (XVECEXP (pat, 0, 1), 0)) == REG
 	       && ! FP_REG_P (XEXP (XVECEXP (pat, 0, 1), 0))
	       && length == 4
	       && ! forward_branch_p (insn))
	return 4;
      else
	return 0;
    }
  else
    return 0;
}

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
    case '#':
      /* Output a 'nop' if there's nothing for the delay slot.  */
      if (dbr_sequence_length () == 0)
	fputs ("\n\tnop", file);
      return;
    case '*':
      /* Output an nullification completer if there's nothing for the */
      /* delay slot or nullification is requested.  */
      if (dbr_sequence_length () == 0 ||
	  (final_sequence &&
	   INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0))))
        fputs (",n", file);
      return;
    case 'R':
      /* Print out the second register name of a register pair.
	 I.e., R (6) => 7.  */
      fputs (reg_names[REGNO (x)+1], file);
      return;
    case 'r':
      /* A register or zero. */
      if (x == const0_rtx
	  || (x == CONST0_RTX (DFmode))
	  || (x == CONST0_RTX (SFmode)))
	{
	  fputs ("0", file);
	  return;
	}
      else
	break;
    case 'C':			/* Plain (C)ondition */
    case 'X':
      switch (GET_CODE (x))
	{
	case EQ:
	  fprintf (file, "=");  break;
	case NE:
	  fprintf (file, "<>");  break;
	case GT:
	  fprintf (file, ">");  break;
	case GE:
	  fprintf (file, ">=");  break;
	case GEU:
	  fprintf (file, ">>=");  break;
	case GTU:
	  fprintf (file, ">>");  break;
	case LT:
	  fprintf (file, "<");  break;
	case LE:
	  fprintf (file, "<=");  break;
	case LEU:
	  fprintf (file, "<<=");  break;
	case LTU:
	  fprintf (file, "<<");  break;
	default:
	  abort ();
	}
      return;
    case 'N':			/* Condition, (N)egated */
      switch (GET_CODE (x))
	{
	case EQ:
	  fprintf (file, "<>");  break;
	case NE:
	  fprintf (file, "=");  break;
	case GT:
	  fprintf (file, "<=");  break;
	case GE:
	  fprintf (file, "<");  break;
	case GEU:
	  fprintf (file, "<<");  break;
	case GTU:
	  fprintf (file, "<<=");  break;
	case LT:
	  fprintf (file, ">=");  break;
	case LE:
	  fprintf (file, ">");  break;
	case LEU:
	  fprintf (file, ">>");  break;
	case LTU:
	  fprintf (file, ">>=");  break;
	default:
	  abort ();
	}
      return;
    /* For floating point comparisons.  Need special conditions to deal
       with NaNs properly.  */
    case 'Y':
      switch (GET_CODE (x))
	{
	case EQ:
	  fprintf (file, "!=");  break;
	case NE:
	  fprintf (file, "=");  break;
	case GT:
	  fprintf (file, "!>");  break;
	case GE:
	  fprintf (file, "!>=");  break;
	case LT:
	  fprintf (file, "!<");  break;
	case LE:
	  fprintf (file, "!<=");  break;
	default:
	  abort ();
	}
      return;
    case 'S':			/* Condition, operands are (S)wapped.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fprintf (file, "=");  break;
	case NE:
	  fprintf (file, "<>");  break;
	case GT:
	  fprintf (file, "<");  break;
	case GE:
	  fprintf (file, "<=");  break;
	case GEU:
	  fprintf (file, "<<=");  break;
	case GTU:
	  fprintf (file, "<<");  break;
	case LT:
	  fprintf (file, ">");  break;
	case LE:
	  fprintf (file, ">=");  break;
	case LEU:
	  fprintf (file, ">>=");  break;
	case LTU:
	  fprintf (file, ">>");  break;
	default:
	  abort ();
	}
      return;
    case 'B':			/* Condition, (B)oth swapped and negate.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fprintf (file, "<>");  break;
	case NE:
	  fprintf (file, "=");  break;
	case GT:
	  fprintf (file, ">=");  break;
	case GE:
	  fprintf (file, ">");  break;
	case GEU:
	  fprintf (file, ">>");  break;
	case GTU:
	  fprintf (file, ">>=");  break;
	case LT:
	  fprintf (file, "<=");  break;
	case LE:
	  fprintf (file, "<");  break;
	case LEU:
	  fprintf (file, "<<");  break;
	case LTU:
	  fprintf (file, "<<=");  break;
	default:
	  abort ();
	}
      return;
    case 'k':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "%d", ~INTVAL (x));
	  return;
	}
      abort();
    case 'L':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "%d", 32 - (INTVAL (x) & 31));
	  return;
	}
      abort();
    case 'O':
      if (GET_CODE (x) == CONST_INT && exact_log2 (INTVAL (x)) >= 0)
	{
	  fprintf (file, "%d", exact_log2 (INTVAL (x)));
	  return;
	}
      abort();
    case 'P':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "%d", 31 - (INTVAL (x) & 31));
	  return;
	}
      abort();
    case 'I':
      if (GET_CODE (x) == CONST_INT)
	fputs ("i", file);
      return;
    case 'M':
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case PRE_INC:
	  fprintf (file, "s,mb");
	  break;
	case POST_DEC:
	case POST_INC:
	  fprintf (file, "s,ma");
	  break;
	default:
	  break;
	}
      return;
    case 'F':
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case PRE_INC:
	  fprintf (file, ",mb");
	  break;
	case POST_DEC:
	case POST_INC:
	  fprintf (file, ",ma");
	  break;
	default:
	  break;
	}
      return;
    case 'G':
      output_global_address (file, x);
      return;
    case 0:			/* Don't do anything special */
      break;
    case 'Z':
      {
	unsigned op[3];
	compute_zdepi_operands (INTVAL (x), op);
	fprintf (file, "%d,%d,%d", op[0], op[1], op[2]);
	return;
      }
    default:
      abort ();
    }
  if (GET_CODE (x) == REG)
    {
      if (FP_REG_P (x) && GET_MODE_SIZE (GET_MODE (x)) <= 4 && (REGNO (x) & 1) == 0)
	fprintf (file, "%sL", reg_names [REGNO (x)]);
      else
	fprintf (file, "%s", reg_names [REGNO (x)]);
    }
  else if (GET_CODE (x) == MEM)
    {
      int size = GET_MODE_SIZE (GET_MODE (x));
      rtx base = XEXP (XEXP (x, 0), 0);
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case POST_DEC:
	  fprintf (file, "-%d(0,%s)", size, reg_names [REGNO (base)]);
	  break;
	case PRE_INC:
	case POST_INC:
	  fprintf (file, "%d(0,%s)", size, reg_names [REGNO (base)]);
	  break;
	default:
	  output_address (XEXP (x, 0));
	  break;
	}
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode)
    {
      union { double d; int i[2]; } u;
      union { float f; int i; } u1;
      u.i[0] = XINT (x, 0); u.i[1] = XINT (x, 1);
      u1.f = u.d;
      if (code == 'f')
	fprintf (file, "0r%.9g", u1.f);
      else
	fprintf (file, "0x%x", u1.i);
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) != VOIDmode)
    {
      union { double d; int i[2]; } u;
      u.i[0] = XINT (x, 0); u.i[1] = XINT (x, 1);
      fprintf (file, "0r%.20g", u.d);
    }
  else
    output_addr_const (file, x);
}

/* output a SYMBOL_REF or a CONST expression involving a SYMBOL_REF. */

void
output_global_address (file, x)
     FILE *file;
     rtx x;
{

  /* Imagine  (high (const (plus ...))).  */
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  if (GET_CODE (x) == SYMBOL_REF && read_only_operand (x))
    assemble_name (file, XSTR (x, 0));
  else if (GET_CODE (x) == SYMBOL_REF)
    {
      assemble_name (file, XSTR (x, 0));
      fprintf (file, "-$global$");
    }
  else if (GET_CODE (x) == CONST)
    {
      char *sep = "";
      int offset = 0;		/* assembler wants -$global$ at end */
      rtx base;

      if (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
	{
	  base = XEXP (XEXP (x, 0), 0);
	  output_addr_const (file, base);
	}
      else if (GET_CODE (XEXP (XEXP (x, 0), 0)) == CONST_INT)
	offset = INTVAL (XEXP (XEXP (x, 0), 0));
      else abort ();

      if (GET_CODE (XEXP (XEXP (x, 0), 1)) == SYMBOL_REF)
	{
	  base = XEXP (XEXP (x, 0), 1);
	  output_addr_const (file, base);
	}
      else if (GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	offset = INTVAL (XEXP (XEXP (x, 0),1));
      else abort ();

      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  if (offset < 0)
	    {
	      offset = -offset;
	      sep = "-";
	    }
	  else
	    sep = "+";
	}
      else if (GET_CODE (XEXP (x, 0)) == MINUS
	       && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF))
	sep = "-";
      else abort ();

      if (!read_only_operand (base))
	fprintf (file, "-$global$");
      fprintf (file, "%s", sep);
      if (offset) fprintf (file,"%d", offset);
    }
  else
    output_addr_const (file, x);
}

/* HP's millicode routines mean something special to the assembler.
   Keep track of which ones we have used.  */

enum millicodes { remI, remU, divI, divU, mulI, mulU, end1000 };
static char imported[(int)end1000];
static char *milli_names[] = {"remI", "remU", "divI", "divU", "mulI", "mulU"};
static char import_string[] = ".IMPORT $$....,MILLICODE";
#define MILLI_START 10

static void
import_milli (code)
     enum millicodes code;
{
  char str[sizeof (import_string)];

  if (!imported[(int)code])
    {
      imported[(int)code] = 1;
      strcpy (str, import_string);
      strncpy (str + MILLI_START, milli_names[(int)code], 4);
      output_asm_insn (str, 0);
    }
}

/* The register constraints have put the operands and return value in
   the proper registers. */

char *
output_mul_insn (unsignedp, insn)
     int unsignedp;
     rtx insn;
{

  if (unsignedp)
    {
      import_milli (mulU);
      return output_call (insn, gen_rtx (SYMBOL_REF, SImode, "$$mulU"),
			  gen_rtx (REG, SImode, 31));
    }
  else
    {
      import_milli (mulI);
      return output_call (insn, gen_rtx (SYMBOL_REF, SImode, "$$mulI"),
			  gen_rtx (REG, SImode, 31));
    }
}

/* If operands isn't NULL, then it's a CONST_INT with which we can do
   something */


/* Emit the rtl for doing a division by a constant. */

 /* Do magic division millicodes exist for this value? */

static int magic_milli[]= {0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0,
			     1, 1};

/* We'll use an array to keep track of the magic millicodes and
   whether or not we've used them already. [n][0] is signed, [n][1] is
   unsigned. */

static int div_milli[16][2];

int
div_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (mode == SImode
	  && ((GET_CODE (op) == REG && REGNO (op) == 25)
	      || (GET_CODE (op) == CONST_INT && INTVAL (op) > 0
		  && INTVAL (op) < 16 && magic_milli[INTVAL (op)])));
}

int
emit_hpdiv_const (operands, unsignedp)
     rtx *operands;
     int unsignedp;
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && INTVAL (operands[2]) < 16
      && magic_milli[INTVAL (operands[2])])
    {
      emit_move_insn ( gen_rtx (REG, SImode, 26), operands[1]);
      emit
	(gen_rtx
	 (PARALLEL, VOIDmode,
	  gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
				 gen_rtx (unsignedp ? UDIV : DIV, SImode,
					  gen_rtx (REG, SImode, 26),
					  operands[2])),
		     gen_rtx (CLOBBER, VOIDmode, operands[3]),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
      emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
      return 1;
    }
  return 0;
}

char *
output_div_insn (operands, unsignedp, insn)
     rtx *operands;
     int unsignedp;
     rtx insn;
{
  int divisor;

  /* If the divisor is a constant, try to use one of the special
     opcodes .*/
  if (GET_CODE (operands[0]) == CONST_INT)
    {
      static char buf[100];
      divisor = INTVAL (operands[0]);
      if (!div_milli[divisor][unsignedp])
	{
	  div_milli[divisor][unsignedp] = 1;
	  if (unsignedp)
	    output_asm_insn (".IMPORT $$divU_%0,MILLICODE", operands);
	  else
	    output_asm_insn (".IMPORT $$divI_%0,MILLICODE", operands);
	}
      if (unsignedp)
	{
	  sprintf (buf, "$$divU_%d", INTVAL (operands[0]));
	  return output_call (insn, gen_rtx (SYMBOL_REF, SImode, buf),
			      gen_rtx (REG, SImode, 31));
	}
      else
	{
	  sprintf (buf, "$$divI_%d", INTVAL (operands[0]));
	  return output_call (insn, gen_rtx (SYMBOL_REF, SImode, buf),
			      gen_rtx (REG, SImode, 31));
	}
    }
  /* Divisor isn't a special constant. */
  else
    {
      if (unsignedp)
	{
	  import_milli (divU);
	  return output_call (insn, gen_rtx (SYMBOL_REF, SImode, "$$divU"),
			      gen_rtx (REG, SImode, 31));
	}
      else
	{
	  import_milli (divI);
	  return output_call (insn, gen_rtx (SYMBOL_REF, SImode, "$$divI"),
			      gen_rtx (REG, SImode, 31));
	}
    }
}

/* Output a $$rem millicode to do mod. */

char *
output_mod_insn (unsignedp, insn)
     int unsignedp;
     rtx insn;
{
  if (unsignedp)
    {
      import_milli (remU);
      return output_call (insn, gen_rtx (SYMBOL_REF, SImode, "$$remU"),
			  gen_rtx (REG, SImode, 31));
    }
  else
    {
      import_milli (remI);
      return output_call (insn, gen_rtx (SYMBOL_REF, SImode, "$$remI"),
			  gen_rtx (REG, SImode, 31));
    }
}

void
output_arg_descriptor (call_insn)
     rtx call_insn;
{
  char *arg_regs[4];
  enum machine_mode arg_mode;
  rtx link;
  int i, output_flag = 0;
  int regno;

  for (i = 0; i < 4; i++)
    arg_regs[i] = 0;

  /* Specify explicitly that no argument relocations should take place
     if using the portable runtime calling conventions.  */
  if (TARGET_PORTABLE_RUNTIME)
    {
      fprintf (asm_out_file,
	       "\t.CALL ARGW0=NO,ARGW1=NO,ARGW2=NO,ARGW3=NO,RETVAL=NO\n");
      return;
    }

  if (GET_CODE (call_insn) != CALL_INSN)
    abort ();
  for (link = CALL_INSN_FUNCTION_USAGE (call_insn); link; link = XEXP (link, 1))
    {
      rtx use = XEXP (link, 0);

      if (! (GET_CODE (use) == USE
	     && GET_CODE (XEXP (use, 0)) == REG
	     && FUNCTION_ARG_REGNO_P (REGNO (XEXP (use, 0)))))
	continue;

      arg_mode = GET_MODE (XEXP (use, 0));
      regno = REGNO (XEXP (use, 0));
      if (regno >= 23 && regno <= 26)
	{
	  arg_regs[26 - regno] = "GR";
	  if (arg_mode == DImode)
	    arg_regs[25 - regno] = "GR";
	}
      else if (regno >= 32 && regno <= 39)
	{
	  if (arg_mode == SFmode)
	    arg_regs[(regno - 32) / 2] = "FR";
	  else
	    {
#ifndef HP_FP_ARG_DESCRIPTOR_REVERSED
	      arg_regs[(regno - 34) / 2] = "FR";
	      arg_regs[(regno - 34) / 2 + 1] = "FU";
#else
	      arg_regs[(regno - 34) / 2] = "FU";
	      arg_regs[(regno - 34) / 2 + 1] = "FR";
#endif
	    }
	}
    }
  fputs ("\t.CALL ", asm_out_file);
  for (i = 0; i < 4; i++)
    {
      if (arg_regs[i])
	{
	  if (output_flag++)
	    fputc (',', asm_out_file);
	  fprintf (asm_out_file, "ARGW%d=%s", i, arg_regs[i]);
	}
    }
  fputc ('\n', asm_out_file);
}

/* Memory loads/stores to/from the shift need to go through
   the general registers.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = true_regnum (in);

  if (((regno >= FIRST_PSEUDO_REGISTER || regno == -1)
       && GET_MODE_CLASS (mode) == MODE_INT
       && FP_REG_CLASS_P (class))
      || (class == SHIFT_REGS && (regno <= 0 || regno >= 32)))
    return GENERAL_REGS;

  if (GET_CODE (in) == HIGH)
    in = XEXP (in, 0);

  if (class != R1_REGS && symbolic_operand (in, VOIDmode))
    return R1_REGS;

  if (GET_CODE (in) == SUBREG)
    in = SUBREG_REG (in);

  if (FP_REG_CLASS_P (class)
      && GET_CODE (in) == MEM
      && !memory_address_p (DFmode, XEXP (in, 0))
      && memory_address_p (SImode, XEXP (in, 0)))
    return GENERAL_REGS;

  return NO_REGS;
}

enum direction
function_arg_padding (mode, type)
     enum machine_mode mode;
     tree type;
{
  int size;

  if (mode == BLKmode)
    {
      if (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	size = int_size_in_bytes (type) * BITS_PER_UNIT;
      else
	return upward;		/* Don't know if this is right, but */
				/* same as old definition. */
    }
  else
    size = GET_MODE_BITSIZE (mode);
  if (size < PARM_BOUNDARY)
    return downward;
  else if (size % PARM_BOUNDARY)
    return upward;
  else
    return none;
}


/* Do what is necessary for `va_start'.  The argument is ignored;
   We look at the current function to determine if stdargs or varargs
   is used and fill in an initial va_list.  A pointer to this constructor
   is returned.  */

struct rtx_def *
hppa_builtin_saveregs (arglist)
     tree arglist;
{
  rtx offset;
  tree fntype = TREE_TYPE (current_function_decl);
  int argadj = ((!(TYPE_ARG_TYPES (fntype) != 0
		   && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		       != void_type_node)))
		? UNITS_PER_WORD : 0);

  if (argadj)
    offset = plus_constant (current_function_arg_offset_rtx, argadj);
  else
    offset = current_function_arg_offset_rtx;

  /* Store general registers on the stack. */
  move_block_from_reg (23,
		       gen_rtx (MEM, BLKmode,
				plus_constant
				(current_function_internal_arg_pointer, -16)),
		       4, 4 * UNITS_PER_WORD);
  return copy_to_reg (expand_binop (Pmode, add_optab,
				    current_function_internal_arg_pointer,
				    offset, 0, 0, OPTAB_LIB_WIDEN));
}

/* This routine handles all the normal conditional branch sequences we
   might need to generate.  It handles compare immediate vs compare
   register, nullification of delay slots, varying length branches,
   negated branches, and all combinations of the above.  It returns the
   output appropriate to emit the branch corresponding to all given
   parameters.  */

char *
output_cbranch (operands, nullify, length, negated, insn)
  rtx *operands;
  int nullify, length, negated;
  rtx insn;
{
  static char buf[100];
  int useskip = 0;

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  This can happen when not optimizing.

     In such cases it is safe to emit nothing.  */

  if (JUMP_LABEL (insn) == next_nonnote_insn (insn))
    return "";

  /* If this is a long branch with its delay slot unfilled, set `nullify'
     as it can nullify the delay slot and save a nop.  */
  if (length == 8 && dbr_sequence_length () == 0)
    nullify = 1;

  /* If this is a short forward conditional branch which did not get
     its delay slot filled, the delay slot can still be nullified.  */
  if (! nullify && length == 4 && dbr_sequence_length () == 0)
    nullify = forward_branch_p (insn);

  /* A forward branch over a single nullified insn can be done with a
     comclr instruction.  This avoids a single cycle penalty due to
     mis-predicted branch if we fall through (branch not taken).  */
  if (length == 4
      && next_real_insn (insn) != 0
      && get_attr_length (next_real_insn (insn)) == 4
      && JUMP_LABEL (insn) == next_nonnote_insn (next_real_insn (insn))
      && nullify)
    useskip = 1;

  switch (length)
    {
      /* All short conditional branches except backwards with an unfilled
	 delay slot.  */
      case 4:
	if (useskip)
	  strcpy (buf, "com%I2clr,");
	else
	  strcpy (buf, "com%I2b,");
	if (negated)
	  strcat (buf, "%B3");
	else
	  strcat (buf, "%S3");
	if (useskip)
	  strcat (buf, " %2,%1,0");
	else if (nullify)
	  strcat (buf, ",n %2,%1,%0");
	else
	  strcat (buf, " %2,%1,%0");
	break;

     /* All long conditionals.  Note an short backward branch with an
	unfilled delay slot is treated just like a long backward branch
	with an unfilled delay slot.  */
      case 8:
	/* Handle weird backwards branch with a filled delay slot
	   with is nullified.  */
	if (dbr_sequence_length () != 0
	    && ! forward_branch_p (insn)
	    && nullify)
	  {
	    strcpy (buf, "com%I2b,");
	    if (negated)
	      strcat (buf, "%S3");
	    else
	      strcat (buf, "%B3");
	    strcat (buf, ",n %2,%1,.+12\n\tbl %0,0");
	  }
	else
	  {
	    strcpy (buf, "com%I2clr,");
	    if (negated)
	      strcat (buf, "%S3");
	    else
	      strcat (buf, "%B3");
	    if (nullify)
	      strcat (buf, " %2,%1,0\n\tbl,n %0,0");
	    else
	      strcat (buf, " %2,%1,0\n\tbl %0,0");
	  }
	break;

      default:
	abort();
    }
  return buf;
}

/* This routine handles all the branch-on-bit conditional branch sequences we
   might need to generate.  It handles nullification of delay slots,
   varying length branches, negated branches and all combinations of the
   above.  it returns the appropriate output template to emit the branch.  */

char *
output_bb (operands, nullify, length, negated, insn, which)
  rtx *operands;
  int nullify, length, negated;
  rtx insn;
  int which;
{
  static char buf[100];
  int useskip = 0;

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  I do not think this can happen as this pattern
     is only used when optimizing; jump optimization should eliminate the
     jump.  But be prepared just in case.  */

  if (JUMP_LABEL (insn) == next_nonnote_insn (insn))
    return "";

  /* If this is a long branch with its delay slot unfilled, set `nullify'
     as it can nullify the delay slot and save a nop.  */
  if (length == 8 && dbr_sequence_length () == 0)
    nullify = 1;

  /* If this is a short forward conditional branch which did not get
     its delay slot filled, the delay slot can still be nullified.  */
  if (! nullify && length == 4 && dbr_sequence_length () == 0)
    nullify = forward_branch_p (insn);

  /* A forward branch over a single nullified insn can be done with a
     extrs instruction.  This avoids a single cycle penalty due to
     mis-predicted branch if we fall through (branch not taken).  */

  if (length == 4
      && next_real_insn (insn) != 0
      && get_attr_length (next_real_insn (insn)) == 4
      && JUMP_LABEL (insn) == next_nonnote_insn (next_real_insn (insn))
      && nullify)
    useskip = 1;

  switch (length)
    {

      /* All short conditional branches except backwards with an unfilled
	 delay slot.  */
      case 4:
	if (useskip)
	  strcpy (buf, "extrs,");
	else
	  strcpy (buf, "bb,");
	if ((which == 0 && negated)
	     || (which == 1 && ! negated))
	  strcat (buf, ">=");
	else
	  strcat (buf, "<");
	if (useskip)
	  strcat (buf, " %0,%1,1,0");
	else if (nullify && negated)
	  strcat (buf, ",n %0,%1,%3");
	else if (nullify && ! negated)
	  strcat (buf, ",n %0,%1,%2");
	else if (! nullify && negated)
	  strcat (buf, "%0,%1,%3");
	else if (! nullify && ! negated)
	  strcat (buf, " %0,%1,%2");
	break;

     /* All long conditionals.  Note an short backward branch with an
	unfilled delay slot is treated just like a long backward branch
	with an unfilled delay slot.  */
      case 8:
	/* Handle weird backwards branch with a filled delay slot
	   with is nullified.  */
	if (dbr_sequence_length () != 0
	    && ! forward_branch_p (insn)
	    && nullify)
	  {
	    strcpy (buf, "bb,");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (negated)
	      strcat (buf, " %0,%1,.+12\n\tbl %3,0");
	    else
	      strcat (buf, " %0,%1,.+12\n\tbl %2,0");
	  }
	else
	  {
	    strcpy (buf, "extrs,");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (nullify && negated)
	      strcat (buf, " %0,%1,1,0\n\tbl,n %3,0");
	    else if (nullify && ! negated)
	      strcat (buf, " %0,%1,1,0\n\tbl,n %2,0");
	    else if (negated)
	      strcat (buf, " %0,%1,1,0\n\tbl %3,0");
	    else
	      strcat (buf, " %0,%1,1,0\n\tbl %2,0");
	  }
	break;

      default:
	abort();
    }
  return buf;
}

/* Return the output template for emitting a dbra type insn.

   Note it may perform some output operations on its own before
   returning the final output string.  */
char *
output_dbra (operands, insn, which_alternative)
     rtx *operands;
     rtx insn;
     int which_alternative;
{

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  Be prepared!  */

  if (JUMP_LABEL (insn) == next_nonnote_insn (insn))
    {
      if (which_alternative == 0)
	return "ldo %1(%0),%0";
      else if (which_alternative == 1)
	{
	  output_asm_insn ("fstws %0,-16(0,%%r30)",operands);
	  output_asm_insn ("ldw -16(0,%%r30),%4",operands);
	  output_asm_insn ("ldo %1(%4),%4\n\tstw %4,-16(0,%%r30)", operands);
	  return "fldws -16(0,%%r30),%0";
	}
      else
	{
	  output_asm_insn ("ldw %0,%4", operands);
	  return "ldo %1(%4),%4\n\tstw %4,%0";
	}
    }

  if (which_alternative == 0)
    {
      int nullify = INSN_ANNULLED_BRANCH_P (insn);
      int length = get_attr_length (insn);

      /* If this is a long branch with its delay slot unfilled, set `nullify'
	 as it can nullify the delay slot and save a nop.  */
      if (length == 8 && dbr_sequence_length () == 0)
	nullify = 1;

      /* If this is a short forward conditional branch which did not get
	 its delay slot filled, the delay slot can still be nullified.  */
      if (! nullify && length == 4 && dbr_sequence_length () == 0)
	nullify = forward_branch_p (insn);

      /* Handle short versions first.  */
      if (length == 4 && nullify)
	return "addib,%C2,n %1,%0,%3";
      else if (length == 4 && ! nullify)
	return "addib,%C2 %1,%0,%3";
      else if (length == 8)
	{
	  /* Handle weird backwards branch with a fulled delay slot
	     which is nullified.  */
	  if (dbr_sequence_length () != 0
	      && ! forward_branch_p (insn)
	      && nullify)
	    return "addib,%N2,n %1,%0,.+12\n\tbl %3,0";

	  /* Handle normal cases.  */
	  if (nullify)
	    return "addi,%N2 %1,%0,%0\n\tbl,n %3,0";
	  else
	    return "addi,%N2 %1,%0,%0\n\tbl %3,0";
	}
      else
	abort();
    }
  /* Deal with gross reload from FP register case.  */
  else if (which_alternative == 1)
    {
      /* Move loop counter from FP register to MEM then into a GR,
	 increment the GR, store the GR into MEM, and finally reload
	 the FP register from MEM from within the branch's delay slot.  */
      output_asm_insn ("fstws %0,-16(0,%%r30)\n\tldw -16(0,%%r30),%4",operands);
      output_asm_insn ("ldo %1(%4),%4\n\tstw %4,-16(0,%%r30)", operands);
      if (get_attr_length (insn) == 24)
	return "comb,%S2 0,%4,%3\n\tfldws -16(0,%%r30),%0";
      else
	return "comclr,%B2 0,%4,0\n\tbl %3,0\n\tfldws -16(0,%%r30),%0";
    }
  /* Deal with gross reload from memory case.  */
  else
    {
      /* Reload loop counter from memory, the store back to memory
	 happens in the branch's delay slot.   */
      output_asm_insn ("ldw %0,%4", operands);
      if (get_attr_length (insn) == 12)
	return "addib,%C2 %1,%4,%3\n\tstw %4,%0";
      else
	return "addi,%N2 %1,%4,%4\n\tbl %3,0\n\tstw %4,%0";
    }
}

/* Return the output template for emitting a dbra type insn.

   Note it may perform some output operations on its own before
   returning the final output string.  */
char *
output_movb (operands, insn, which_alternative, reverse_comparison)
     rtx *operands;
     rtx insn;
     int which_alternative;
     int reverse_comparison;
{

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  Be prepared!  */

  if (JUMP_LABEL (insn) == next_nonnote_insn (insn))
    {
      if (which_alternative == 0)
	return "copy %1,%0";
      else if (which_alternative == 1)
	{
	  output_asm_insn ("stw %1,-16(0,%%r30)",operands);
	  return "fldws -16(0,%%r30),%0";
	}
      else
	return "stw %1,%0";
    }

  /* Support the second variant.  */
  if (reverse_comparison)
    PUT_CODE (operands[2], reverse_condition (GET_CODE (operands[2])));

  if (which_alternative == 0)
    {
      int nullify = INSN_ANNULLED_BRANCH_P (insn);
      int length = get_attr_length (insn);

      /* If this is a long branch with its delay slot unfilled, set `nullify'
	 as it can nullify the delay slot and save a nop.  */
      if (length == 8 && dbr_sequence_length () == 0)
	nullify = 1;

      /* If this is a short forward conditional branch which did not get
	 its delay slot filled, the delay slot can still be nullified.  */
      if (! nullify && length == 4 && dbr_sequence_length () == 0)
	nullify = forward_branch_p (insn);

      /* Handle short versions first.  */
      if (length == 4 && nullify)
	return "movb,%C2,n %1,%0,%3";
      else if (length == 4 && ! nullify)
	return "movb,%C2 %1,%0,%3";
      else if (length == 8)
	{
	  /* Handle weird backwards branch with a filled delay slot
	     which is nullified.  */
	  if (dbr_sequence_length () != 0
	      && ! forward_branch_p (insn)
	      && nullify)
	    return "movb,%N2,n %1,%0,.+12\n\ttbl %3,0";

	  /* Handle normal cases.  */
	  if (nullify)
	    return "or,%N2 %1,%%r0,%0\n\tbl,n %3,0";
	  else
	    return "or,%N2 %1,%%r0,%0\n\tbl %3,0";
	}
      else
	abort();
    }
  /* Deal with gross reload from FP register case.  */
  else if (which_alternative == 1)
    {
      /* Move loop counter from FP register to MEM then into a GR,
	 increment the GR, store the GR into MEM, and finally reload
	 the FP register from MEM from within the branch's delay slot.  */
      output_asm_insn ("stw %1,-16(0,%%r30)",operands);
      if (get_attr_length (insn) == 12)
	return "comb,%S2 0,%1,%3\n\tfldws -16(0,%%r30),%0";
      else
	return "comclr,%B2 0,%1,0\n\tbl %3,0\n\tfldws -16(0,%%r30),%0";
    }
  /* Deal with gross reload from memory case.  */
  else
    {
      /* Reload loop counter from memory, the store back to memory
	 happens in the branch's delay slot.   */
      if (get_attr_length (insn) == 8)
	return "comb,%S2 0,%1,%3\n\tstw %1,%0";
      else
	return "comclr,%B2 0,%1,0\n\tbl %3,0\n\tstw %1,%0";
    }
}


/* INSN is either a function call or a millicode call.  It may have an
   unconditional jump in its delay slot.

   CALL_DEST is the routine we are calling.

   RETURN_POINTER is the register which will hold the return address.
   %r2 for most calls, %r31 for millicode calls. 

   When TARGET_LONG_CALLS is true, output_call is only called for
   millicode calls.  In addition, no delay slots are available when
   TARGET_LONG_CALLS is true.  */

char *
output_call (insn, call_dest, return_pointer)
  rtx insn;
  rtx call_dest;
  rtx return_pointer;

{
  int distance;
  rtx xoperands[4];
  rtx seq_insn;

  /* Handle common case -- empty delay slot or no jump in the delay slot.  */
  if (dbr_sequence_length () == 0
      || (dbr_sequence_length () != 0
	  && GET_CODE (NEXT_INSN (insn)) != JUMP_INSN))
    {
      xoperands[0] = call_dest;
      xoperands[1] = return_pointer;
      if (TARGET_LONG_CALLS)
	{
	  output_asm_insn ("ldil L%%%0,%%r29", xoperands);
	  output_asm_insn ("ldo R%%%0(%%r29),%%r29", xoperands);
	  output_asm_insn ("blr 0,%r1\n\tbv,n 0(%%r29)\n\tnop", xoperands);
	}
      else
	output_asm_insn ("bl %0,%r1%#", xoperands);
      return "";
    }

  /* This call has an unconditional jump in its delay slot.  */

  /* Use the containing sequence insn's address.  */
  seq_insn = NEXT_INSN (PREV_INSN (XVECEXP (final_sequence, 0, 0)));

  distance = insn_addresses[INSN_UID (JUMP_LABEL (NEXT_INSN (insn)))]
	       - insn_addresses[INSN_UID (seq_insn)] - 8;

  /* If the branch was too far away, emit a normal call followed
     by a nop, followed by the unconditional branch.

     If the branch is close, then adjust %r2 from within the
     call's delay slot.  */

  xoperands[0] = call_dest;
  xoperands[1] = XEXP (PATTERN (NEXT_INSN (insn)), 1);
  xoperands[2] = return_pointer;
  if (! VAL_14_BITS_P (distance))
    output_asm_insn ("bl %0,%r2\n\tnop\n\tbl,n %1,%%r0", xoperands);
  else
    {
      xoperands[3] = gen_label_rtx ();
      output_asm_insn ("\n\tbl %0,%r2\n\tldo %1-%3(%r2),%r2", xoperands);
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				 CODE_LABEL_NUMBER (xoperands[3]));
    }

  /* Delete the jump.  */
  PUT_CODE (NEXT_INSN (insn), NOTE);
  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
  return "";
}

extern struct obstack *saveable_obstack;

/* In HPUX 8.0's shared library scheme, special relocations are needed
   for function labels if they might be passed to a function
   in a shared library (because shared libraries don't live in code
   space), and special magic is needed to construct their address. */

void
hppa_encode_label (sym)
     rtx sym;
{
  char *str = XSTR (sym, 0);
  int len = strlen (str);
  char *newstr = obstack_alloc (saveable_obstack, len + 2) ;

  if (str[0] == '*')
    *newstr++ = *str++;
  strcpy (newstr + 1, str);
  *newstr = '@';
  XSTR (sym,0) = newstr;
}

int
function_label_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == SYMBOL_REF && FUNCTION_NAME_P (XSTR (op, 0));
}

/* Returns 1 if OP is a function label involved in a simple addition
   with a constant.  Used to keep certain patterns from matching
   during instruction combination.  */
int
is_function_label_plus_const (op)
     rtx op;
{
  /* Strip off any CONST.  */
  if (GET_CODE (op) == CONST)
    op = XEXP (op, 0);

  return (GET_CODE (op) == PLUS
	  && function_label_operand (XEXP (op, 0), Pmode)
	  && GET_CODE (XEXP (op, 1)) == CONST_INT);
}

/* Returns 1 if the 6 operands specified in OPERANDS are suitable for
   use in fmpyadd instructions.  */
int
fmpyaddoperands (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);

  /* All modes must be the same.  */
  if (! (mode == GET_MODE (operands[1])
	 && mode == GET_MODE (operands[2])
	 && mode == GET_MODE (operands[3])
	 && mode == GET_MODE (operands[4])
	 && mode == GET_MODE (operands[5])))
    return 0;

  /* Both DFmode and SFmode should work.  But using SFmode makes the
     assembler complain.  Just turn it off for now.  */
  if (mode != DFmode)
    return 0;

  /* Only 2 real operands to the addition.  One of the input operands must
     be the same as the output operand.  */
  if (! rtx_equal_p (operands[3], operands[4])
      && ! rtx_equal_p (operands[3], operands[5]))
    return 0;

  /* Inout operand of add can not conflict with any operands from multiply.  */
  if (rtx_equal_p (operands[3], operands[0])
     || rtx_equal_p (operands[3], operands[1])
     || rtx_equal_p (operands[3], operands[2]))
    return 0;

  /* multiply can not feed into addition operands.  */
  if (rtx_equal_p (operands[4], operands[0])
      || rtx_equal_p (operands[5], operands[0]))
    return 0;

  /* Passed.  Operands are suitable for fmpyadd.  */
  return 1;
}

/* Returns 1 if the 6 operands specified in OPERANDS are suitable for
   use in fmpysub instructions.  */
int
fmpysuboperands (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);

  /* All modes must be the same.  */
  if (! (mode == GET_MODE (operands[1])
	 && mode == GET_MODE (operands[2])
	 && mode == GET_MODE (operands[3])
	 && mode == GET_MODE (operands[4])
	 && mode == GET_MODE (operands[5])))
    return 0;

  /* Both DFmode and SFmode should work.  But using SFmode makes the
     assembler complain.  Just turn it off for now.  */
  if (mode != DFmode)
    return 0;

  /* Only 2 real operands to the subtraction.  Subtraction is not a commutative
     operation, so operands[4] must be the same as operand[3].  */
  if (! rtx_equal_p (operands[3], operands[4]))
    return 0;

  /* multiply can not feed into subtraction.  */
  if (rtx_equal_p (operands[5], operands[0]))
    return 0;

  /* Inout operand of sub can not conflict with any operands from multiply.  */
  if (rtx_equal_p (operands[3], operands[0])
     || rtx_equal_p (operands[3], operands[1])
     || rtx_equal_p (operands[3], operands[2]))
    return 0;

  /* Passed.  Operands are suitable for fmpysub.  */
  return 1;
}

int
plus_xor_ior_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == PLUS || GET_CODE (op) == XOR
	  || GET_CODE (op) == IOR);
}

/* Return 1 if the given constant is 2, 4, or 8.  These are the valid
   constants for shadd instructions.  */
int
shadd_constant_p (val)
     int val;
{
  if (val == 2 || val == 4 || val == 8)
    return 1;
  else
    return 0;
}

/* Return 1 if OP is a CONST_INT with the value 2, 4, or 8.  These are
   the valid constant for shadd instructions.  */
int
shadd_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && shadd_constant_p (INTVAL (op)));
}

/* Return 1 if this operand is anything other than a hard register.  */

int
non_hard_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ! (GET_CODE (op) == REG && REGNO (op) < FIRST_PSEUDO_REGISTER);
}

/* Return 1 if INSN branches forward.  Should be using insn_addresses
   to avoid walking through all the insns... */
int
forward_branch_p (insn)
     rtx insn;
{
  rtx label = JUMP_LABEL (insn);

  while (insn)
    {
      if (insn == label)
	break;
      else
	insn = NEXT_INSN (insn);
    }

  return (insn == label);
}

/* Return 1 if OP is an equality comparison, else return 0.  */
int
eq_neq_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return 1 if OP is an operator suitable for use in a movb instruction.  */
int
movb_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE
	  || GET_CODE (op) == LT || GET_CODE (op) == GE);
}

/* Return 1 if INSN is in the delay slot of a call instruction.  */
int
jump_in_call_delay (insn)
     rtx insn;
{

  if (GET_CODE (insn) != JUMP_INSN)
    return 0;

  if (PREV_INSN (insn)
      && PREV_INSN (PREV_INSN (insn))
      && GET_CODE (next_active_insn (PREV_INSN (PREV_INSN (insn)))) == INSN)
    {
      rtx test_insn = next_active_insn (PREV_INSN (PREV_INSN (insn)));

      return (GET_CODE (PATTERN (test_insn)) == SEQUENCE
	      && XVECEXP (PATTERN (test_insn), 0, 1) == insn);

    }
  else
    return 0;
}
