/* Subroutines for insn-output.c for Sun SPARC.
   Copyright (C) 1987, 1988, 1989, 1992 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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
#include "flags.h"
#include "expr.h"
#include "recog.h"

/* Global variables for machine-dependent things.  */

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */

rtx sparc_compare_op0, sparc_compare_op1;

/* We may need an epilogue if we spill too many registers.
   If this is non-zero, then we branch here for the epilogue.  */
static rtx leaf_label;

#ifdef LEAF_REGISTERS

/* Vector to say how input registers are mapped to output
   registers.  FRAME_POINTER_REGNUM cannot be remapped by
   this function to eliminate it.  You must use -fomit-frame-pointer
   to get that.  */
char leaf_reg_remap[] =
{ 0, 1, 2, 3, 4, 5, 6, 7,
  -1, -1, -1, -1, -1, -1, 14, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  8, 9, 10, 11, 12, 13, -1, 15,

  32, 33, 34, 35, 36, 37, 38, 39,
  40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 59, 60, 61, 62, 63};

char leaf_reg_backmap[] =
{ 0, 1, 2, 3, 4, 5, 6, 7,
  24, 25, 26, 27, 28, 29, 14, 31,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,

  32, 33, 34, 35, 36, 37, 38, 39,
  40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 59, 60, 61, 62, 63};
#endif

/* Global variables set by FUNCTION_PROLOGUE.  */
/* Size of frame.  Need to know this to emit return insns from
   leaf procedures.  */
int apparent_fsize;
int actual_fsize;

/* Name of where we pretend to think the frame pointer points.
   Normally, this is "%fp", but if we are in a leaf procedure,
   this is "%sp+something".  */
char *frame_base_name;

static rtx find_addr_reg ();

/* Return non-zero only if OP is a register of mode MODE,
   or const0_rtx.  */
int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (op == const0_rtx || register_operand (op, mode))
    return 1;
  if (GET_CODE (op) == CONST_DOUBLE
      && CONST_DOUBLE_HIGH (op) == 0
      && CONST_DOUBLE_LOW (op) == 0)
    return 1;
  return 0;
}

/* Nonzero if OP can appear as the dest of a RESTORE insn.  */
int
restore_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == REG && GET_MODE (op) == mode
	  && (REGNO (op) < 8 || (REGNO (op) >= 24 && REGNO (op) < 32)));
}

/* PC-relative call insn on SPARC is independent of `memory_operand'.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    abort ();
  op = XEXP (op, 0);
  return (REG_P (op) || CONSTANT_P (op));
}

int
call_operand_address (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (REG_P (op) || CONSTANT_P (op));
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

      /* This clause seems to be irrelevant.  */
    case CONST_DOUBLE:
      return GET_MODE (op) == mode;

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

int
sparc_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;
  if (GET_CODE (op) == CONST_INT)
    return SMALL_INT (op);
  if (GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);
  if (GET_CODE (op) == LO_SUM)
    return (GET_CODE (XEXP (op, 0)) == REG
	    && symbolic_operand (XEXP (op, 1), Pmode));
  return memory_address_p (mode, op);
}

int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == DImode && arith_double_operand (op, mode))
    return 1;
  if (register_operand (op, mode))
    return 1;
  if (GET_CODE (op) == CONST_INT)
    return (SMALL_INT (op) || (INTVAL (op) & 0x3ff) == 0);

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

int
move_pic_label (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Special case for PIC.  */
  if (flag_pic && GET_CODE (op) == LABEL_REF)
    return 1;
  return 0;
}

/* The rtx for the global offset table which is a special form
   that *is* a position independent symbolic constant.  */
rtx pic_pc_rtx;

/* Ensure that we are not using patterns that are not OK with PIC.  */

int
check_pic (i)
     int i;
{
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

/* Return true if X is an address which needs a temporary register when 
   reloaded while generating PIC code.  */

int
pic_address_needs_scratch (x)
     rtx x;
{
  /* An address which is a symbolic plus a non SMALL_INT needs a temp reg.  */
  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && ! SMALL_INT (XEXP (XEXP (x, 0), 1)))
    return 1;

  return 0;
}

int
memop (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM)
    return (mode == VOIDmode || mode == GET_MODE (op));
  return 0;
}

/* Return truth value of whether OP is EQ or NE.  */

int
eq_or_neq (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return 1 if this is a comparison operator, but not an EQ, NE, GEU,
   or LTU for non-floating-point.  We handle those specially.  */

int
normal_comp_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  if (GET_MODE (XEXP (op, 0)) == CCFPmode
      || GET_MODE (XEXP (op, 0)) == CCFPEmode)
    return 1;

  return (code != NE && code != EQ && code != GEU && code != LTU);
}

/* Return 1 if this is a comparison operator.  This allows the use of
   MATCH_OPERATOR to recognize all the branch insns.  */

int
noov_compare_op (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  if (GET_MODE (XEXP (op, 0)) == CC_NOOVmode)
    /* These are the only branches which work with CC_NOOVmode.  */
    return (code == EQ || code == NE || code == GE || code == LT);
  return 1;
}

/* Return 1 if this is a SIGN_EXTEND or ZERO_EXTEND operation.  */

int
extend_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == SIGN_EXTEND || GET_CODE (op) == ZERO_EXTEND;
}

/* Return nonzero if OP is an operator of mode MODE which can set
   the condition codes explicitly.  We do not include PLUS and MINUS
   because these require CC_NOOVmode, which we handle explicitly.  */

int
cc_arithop (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == AND
      || GET_CODE (op) == IOR
      || GET_CODE (op) == XOR)
    return 1;

  return 0;
}

/* Return nonzero if OP is an operator of mode MODE which can bitwise
   complement its second operand and set the condition codes explicitly.  */

int
cc_arithopn (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* XOR is not here because combine canonicalizes (xor (not ...) ...)
     and (xor ... (not ...)) to (not (xor ...)).   */
  return (GET_CODE (op) == AND
	  || GET_CODE (op) == IOR);
}

/* Return truth value of whether OP can be used as an operands in a three
   address arithmetic insn (such as add %o1,7,%l2) of mode MODE.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op)));
}

/* Return truth value of whether OP is a register or a CONST_DOUBLE.  */

int
arith_double_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_DOUBLE
	      && (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	      && (unsigned) (CONST_DOUBLE_LOW (op) + 0x1000) < 0x2000
	      && ((CONST_DOUBLE_HIGH (op) == -1
		   && (CONST_DOUBLE_LOW (op) & 0x1000) == 0x1000)
		  || (CONST_DOUBLE_HIGH (op) == 0
		      && (CONST_DOUBLE_LOW (op) & 0x1000) == 0)))
	  || (GET_CODE (op) == CONST_INT
	      && (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	      && (unsigned) (INTVAL (op) + 0x1000) < 0x2000));
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in three-address insns.  */

int
small_int (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}

/* Return truth value of statement that OP is a call-clobbered register.  */
int
clobbered_register (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == REG && call_used_regs[REGNO (op)]);
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  */

rtx
gen_compare_reg (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx (REG, mode, 0);

  emit_insn (gen_rtx (SET, VOIDmode, cc_reg,
		      gen_rtx (COMPARE, mode, x, y)));

  return cc_reg;
}

/* Return nonzero if a return peephole merging return with
   setting of output register is ok.  */
int
leaf_return_peephole_ok ()
{
  return (actual_fsize == 0);
}

/* Return nonzero if TRIAL can go into the function epilogue's
   delay slot.  SLOT is the slot we are trying to fill.  */

int
eligible_for_epilogue_delay (trial, slot)
     rtx trial;
     int slot;
{
  static char *this_function_name;
  rtx pat, src;

  if (slot >= 1)
    return 0;
  if (GET_CODE (trial) != INSN
      || GET_CODE (PATTERN (trial)) != SET)
    return 0;
  if (get_attr_length (trial) != 1)
    return 0;

  /* In the case of a true leaf function, anything can go into the delay slot.
     A delay slot only exists however if the frame size is zero, otherwise
     we will put an insn to adjust the stack after the return.  */
  if (leaf_function)
    {
      if (leaf_return_peephole_ok ())
	return (get_attr_in_uncond_branch_delay (trial) == IN_BRANCH_DELAY_TRUE);
      return 0;
    }

  /* Otherwise, only operations which can be done in tandem with
     a `restore' insn can go into the delay slot.  */
  pat = PATTERN (trial);
  if (GET_CODE (SET_DEST (pat)) != REG
      || REGNO (SET_DEST (pat)) == 0
      || REGNO (SET_DEST (pat)) >= 32
      || REGNO (SET_DEST (pat)) < 24)
    return 0;

  src = SET_SRC (pat);
  if (arith_operand (src, GET_MODE (src)))
    return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (SImode);
  if (arith_double_operand (src, GET_MODE (src)))
    return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (DImode);
  if (GET_CODE (src) == PLUS)
    {
      if (register_operand (XEXP (src, 0), SImode)
	  && arith_operand (XEXP (src, 1), SImode))
	return 1;
      if (register_operand (XEXP (src, 1), SImode)
	  && arith_operand (XEXP (src, 0), SImode))
	return 1;
      if (register_operand (XEXP (src, 0), DImode)
	  && arith_double_operand (XEXP (src, 1), DImode))
	return 1;
      if (register_operand (XEXP (src, 1), DImode)
	  && arith_double_operand (XEXP (src, 0), DImode))
	return 1;
    }
  if (GET_CODE (src) == MINUS
      && register_operand (XEXP (src, 0), SImode)
      && small_int (XEXP (src, 1), VOIDmode))
    return 1;
  if (GET_CODE (src) == MINUS
      && register_operand (XEXP (src, 0), DImode)
      && !register_operand (XEXP (src, 1), DImode)
      && arith_double_operand (XEXP (src, 1), DImode))
    return 1;
  return 0;
}

int
short_branch (uid1, uid2)
     int uid1, uid2;
{
  unsigned int delta = insn_addresses[uid1] - insn_addresses[uid2];
  if (delta + 1024 < 2048)
    return 1;
  /* warning ("long branch, distance %d", delta); */
  return 0;
}

/* Return non-zero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels or calls or jumps.  */
int
reg_unused_after (reg, insn)
     rtx reg;
     rtx insn;
{
  enum rtx_code code, prev_code = UNKNOWN;

  while (insn = NEXT_INSN (insn))
    {
      if (prev_code == CALL_INSN && call_used_regs[REGNO (reg)])
	return 1;

      code = GET_CODE (insn);
      if (GET_CODE (insn) == CODE_LABEL)
	return 1;

      if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx set = single_set (insn);
	  int in_src = set && reg_overlap_mentioned_p (reg, SET_SRC (set));
	  if (set && in_src)
	    return 0;
	  if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	    return 1;
	  if (set == 0 && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	    return 0;
	}
      prev_code = code;
    }
  return 1;
}

/* Legitimize PIC addresses.  If the address is already position-independent,
   we return ORIG.  Newly generated position-independent addresses go into a
   reg.  This is REG if non zero, otherwise we allocate register(s) as
   necessary.  If this is called during reload, and we need a second temp
   register, then we use SCRATCH, which is provided via the
   SECONDARY_INPUT_RELOAD_CLASS mechanism.  */

rtx
legitimize_pic_address (orig, mode, reg, scratch)
     rtx orig;
     enum machine_mode mode;
     rtx reg, scratch;
{
  if (GET_CODE (orig) == SYMBOL_REF)
    {
      rtx pic_ref, address;
      rtx insn;

      if (reg == 0)
	{
	  if (reload_in_progress || reload_completed)
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);
	}

      if (flag_pic == 2)
	{
	  /* If not during reload, allocate another temp reg here for loading
	     in the address, so that these instructions can be optimized
	     properly.  */
	  rtx temp_reg = ((reload_in_progress || reload_completed)
			  ? reg : gen_reg_rtx (Pmode));

	  /* Must put the SYMBOL_REF inside an UNSPEC here so that cse
	     won't get confused into thinking that these two instructions
	     are loading in the true address of the symbol.  If in the
	     future a PIC rtx exists, that should be used instead.  */
	  emit_insn (gen_rtx (SET, VOIDmode, temp_reg,
			      gen_rtx (HIGH, Pmode,
				       gen_rtx (UNSPEC, Pmode,
						gen_rtvec (1, orig),
						0))));
	  emit_insn (gen_rtx (SET, VOIDmode, temp_reg,
			      gen_rtx (LO_SUM, Pmode, temp_reg,
				       gen_rtx (UNSPEC, Pmode,
						gen_rtvec (1, orig),
						0))));
	  address = temp_reg;
	}
      else
	address = orig;

      pic_ref = gen_rtx (MEM, Pmode,
			 gen_rtx (PLUS, Pmode,
				  pic_offset_table_rtx, address));
      current_function_uses_pic_offset_table = 1;
      RTX_UNCHANGING_P (pic_ref) = 1;
      insn = emit_move_insn (reg, pic_ref);
      /* Put a REG_EQUAL note on this insn, so that it can be optimized
	 by loop.  */
      REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_EQUAL, orig,
				  REG_NOTES (insn));
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	{
	  if (reload_in_progress || reload_completed)
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);
	}

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode,
					 reg, 0);
	  offset = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					 base == reg ? 0 : reg, 0);
	}
      else
	abort ();

      if (GET_CODE (offset) == CONST_INT)
	{
	  if (SMALL_INT (offset))
	    return plus_constant_for_output (base, INTVAL (offset));
	  else if (! reload_in_progress && ! reload_completed)
	    offset = force_reg (Pmode, offset);
	  /* We can't create any new registers during reload, so use the
	     SCRATCH reg provided by the reload_insi pattern.  */
	  else if (scratch)
	    {
	      emit_move_insn (scratch, offset);
	      offset = scratch;
	    }
	  else
	    /* If we reach here, then the SECONDARY_INPUT_RELOAD_CLASS
	       macro needs to be adjusted so that a scratch reg is provided
	       for this address.  */
	    abort ();
	}
      return gen_rtx (PLUS, Pmode, base, offset);
    }
  else if (GET_CODE (orig) == LABEL_REF)
    current_function_uses_pic_offset_table = 1;

  return orig;
}

/* Set up PIC-specific rtl.  This should not cause any insns
   to be emitted.  */

void
initialize_pic ()
{
}

/* Emit special PIC prologues and epilogues.  */

void
finalize_pic ()
{
  /* The table we use to reference PIC data.  */
  rtx global_offset_table;
  /* Labels to get the PC in the prologue of this function.  */
  rtx l1, l2;
  rtx seq;
  int orig_flag_pic = flag_pic;

  if (current_function_uses_pic_offset_table == 0)
    return;

  if (! flag_pic)
    abort ();

  flag_pic = 0;
  l1 = gen_label_rtx ();
  l2 = gen_label_rtx ();

  start_sequence ();

  emit_label (l1);
  /* Note that we pun calls and jumps here!  */
  emit_jump_insn (gen_rtx (PARALLEL, VOIDmode,
                         gen_rtvec (2,
                                    gen_rtx (SET, VOIDmode, pc_rtx, gen_rtx (LABEL_REF, VOIDmode, l2)),
                                    gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 15), gen_rtx (LABEL_REF, VOIDmode, l2)))));
  emit_label (l2);

  /* Initialize every time through, since we can't easily
     know this to be permanent.  */
  global_offset_table = gen_rtx (SYMBOL_REF, Pmode, "_GLOBAL_OFFSET_TABLE_");
  pic_pc_rtx = gen_rtx (CONST, Pmode,
			gen_rtx (MINUS, Pmode,
				 global_offset_table,
				 gen_rtx (CONST, Pmode,
					  gen_rtx (MINUS, Pmode,
						   gen_rtx (LABEL_REF, VOIDmode, l1),
						   pc_rtx))));

  emit_insn (gen_rtx (SET, VOIDmode, pic_offset_table_rtx,
		      gen_rtx (HIGH, Pmode, pic_pc_rtx)));
  emit_insn (gen_rtx (SET, VOIDmode,
		      pic_offset_table_rtx,
		      gen_rtx (LO_SUM, Pmode,
			       pic_offset_table_rtx, pic_pc_rtx)));
  emit_insn (gen_rtx (SET, VOIDmode,
		      pic_offset_table_rtx,
		      gen_rtx (PLUS, Pmode,
			       pic_offset_table_rtx, gen_rtx (REG, Pmode, 15))));
  /* emit_insn (gen_rtx (ASM_INPUT, VOIDmode, "!#PROLOGUE# 1")); */
  LABEL_PRESERVE_P (l1) = 1;
  LABEL_PRESERVE_P (l2) = 1;
  flag_pic = orig_flag_pic;

  seq = gen_sequence ();
  end_sequence ();
  emit_insn_after (seq, get_insns ());

  /* Need to emit this whether or not we obey regdecls,
     since setjmp/longjmp can cause life info to screw up.  */
  emit_insn (gen_rtx (USE, VOIDmode, pic_offset_table_rtx));
}

/* For the SPARC, REG and REG+CONST is cost 0, REG+REG is cost 1,
   and addresses involving symbolic constants are cost 2.

   We make REG+REG slightly more expensive because it might keep
   a register live for longer than we might like.

   PIC addresses are very expensive.

   It is no coincidence that this has the same structure
   as GO_IF_LEGITIMATE_ADDRESS.  */
int
sparc_address_cost (X)
     rtx X;
{
#if 0
  /* Handled before calling here.  */
  if (GET_CODE (X) == REG)
    { return 1; }
#endif
  if (GET_CODE (X) == PLUS)
    {
      if (GET_CODE (XEXP (X, 0)) == REG
	  && GET_CODE (XEXP (X, 1)) == REG)
	return 2;
      return 1;
    }
  else if (GET_CODE (X) == LO_SUM)
    return 1;
  else if (GET_CODE (X) == HIGH)
    return 2;
  return 4;
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.

   SCRATCH_REG if non zero can be used as a scratch register for the move
   operation.  It is provided by a SECONDARY_RELOAD_* macro if needed.  */

int
emit_move_sequence (operands, mode, scratch_reg)
     rtx *operands;
     enum machine_mode mode;
     rtx scratch_reg;
{
  register rtx operand0 = operands[0];
  register rtx operand1 = operands[1];

  /* Handle most common case first: storing into a register.  */
  if (register_operand (operand0, mode))
    {
      if (register_operand (operand1, mode)
	  || (GET_CODE (operand1) == CONST_INT && SMALL_INT (operand1))
	  || (GET_CODE (operand1) == CONST_DOUBLE
	      && arith_double_operand (operand1, DImode))
	  || (GET_CODE (operand1) == HIGH && GET_MODE (operand1) != DImode)
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
      if (register_operand (operand1, mode) || operand1 == const0_rtx)
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operand0);
	  operands[1] = operand1 = force_reg (mode, operand1);
	}
    }

  /* Simplify the source if we need to.  Must handle DImode HIGH operators
     here because such a move needs a clobber added.  */
  if ((GET_CODE (operand1) != HIGH && immediate_operand (operand1, mode))
      || (GET_CODE (operand1) == HIGH && GET_MODE (operand1) == DImode))
    {
      if (flag_pic && symbolic_operand (operand1, mode))
	{
	  rtx temp_reg = reload_in_progress ? operand0 : 0;

	  operands[1] = legitimize_pic_address (operand1, mode, temp_reg,
						scratch_reg);
	}
      else if (GET_CODE (operand1) == CONST_INT
	       ? (! SMALL_INT (operand1)
		  && (INTVAL (operand1) & 0x3ff) != 0)
	       : (GET_CODE (operand1) == CONST_DOUBLE
		  ? ! arith_double_operand (operand1, DImode)
		  : 1))
	{
	  /* For DImode values, temp must be operand0 because of the way
	     HI and LO_SUM work.  The LO_SUM operator only copies half of
	     the LSW from the dest of the HI operator.  If the LO_SUM dest is
	     not the same as the HI dest, then the MSW of the LO_SUM dest will
	     never be set.

	     ??? The real problem here is that the ...(HI:DImode pattern emits
	     multiple instructions, and the ...(LO_SUM:DImode pattern emits
	     one instruction.  This fails, because the compiler assumes that
	     LO_SUM copies all bits of the first operand to its dest.  Better
	     would be to have the HI pattern emit one instruction and the
	     LO_SUM pattern multiple instructions.  Even better would be
	     to use four rtl insns.  */
	  rtx temp = ((reload_in_progress || mode == DImode)
		      ? operand0 : gen_reg_rtx (mode));

	  emit_insn (gen_rtx (SET, VOIDmode, temp,
			      gen_rtx (HIGH, mode, operand1)));
	  operands[1] = gen_rtx (LO_SUM, mode, temp, operand1);
	}
    }

  if (GET_CODE (operand1) == LABEL_REF && flag_pic)
    {
      /* The procedure for doing this involves using a call instruction to
	 get the pc into o7.  We need to indicate this explicitly because
	 the tablejump pattern assumes that it can use this value also.  */
      emit_insn (gen_rtx (PARALLEL, VOIDmode,
			  gen_rtvec (2,
				     gen_rtx (SET, VOIDmode, operand0,
					      operand1),
				     gen_rtx (SET, VOIDmode,
					      gen_rtx (REG, mode, 15),
					      pc_rtx))));
      return 1;
    }

  /* Now have insn-emit do whatever it normally does.  */
  return 0;
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (GET_CODE (operands[1]) != MEM)
	return "st %r1,%0";
      else
	abort ();
    }
  else if (GET_CODE (operands[1]) == MEM)
    return "ld %1,%0";
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      int i;
      union real_extract u;
      union float_extract { float f; int i; } v;

      /* Must be SFmode, otherwise this doesn't make sense.  */
      if (GET_MODE (operands[1]) != SFmode)
	abort ();

      bcopy (&CONST_DOUBLE_LOW (operands[1]), &u, sizeof u);
      v.f = REAL_VALUE_TRUNCATE (SFmode, u.d);
      i = v.i;

      operands[1] = gen_rtx (CONST_INT, VOIDmode, i);

      if (CONST_OK_FOR_LETTER_P (i, 'I'))
	return "mov %1,%0";
      else if ((i & 0x000003FF) != 0)
	return "sethi %%hi(%a1),%0\n\tor %0,%%lo(%a1),%0";
      else
	return "sethi %%hi(%a1),%0";
    }
  else if (GET_CODE (operands[1]) == CONST_INT
	   && ! CONST_OK_FOR_LETTER_P (INTVAL (operands[1]), 'I'))
    {
      int i = INTVAL (operands[1]);

      /* If all low order 10 bits are clear, then we only need a single
	 sethi insn to load the constant.  */
      if ((i & 0x000003FF) != 0)
	return "sethi %%hi(%a1),%0\n\tor %0,%%lo(%a1),%0";
      else
	return "sethi %%hi(%a1),%0";
    }
  /* Operand 1 must be a register, or a 'I' type CONST_INT.  */
  return "mov %1,%0";
}

/* Return non-zero if it is OK to assume that the given memory operand is
   aligned at least to a 8-byte boundary.  This should only be called
   for memory accesses whose size is 8 bytes or larger.  */

int
mem_aligned_8 (mem)
     register rtx mem;
{
  register rtx addr;
  register rtx base;
  register rtx offset;

  if (GET_CODE (mem) != MEM)
    return 0;	/* It's gotta be a MEM! */

  addr = XEXP (mem, 0);

#if 1
  /* Now that all misaligned double parms are copied on function entry,
     we can assume any 64-bit object is 64-bit aligned.  */

  /* See what register we use in the address.  */
  base = 0;
  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG
	  && GET_CODE (XEXP (addr, 1)) == CONST_INT)
	{
	  base = XEXP (addr, 0);
	  offset = XEXP (addr, 1);
	}
    }
  else if (GET_CODE (addr) == REG)
    {
      base = addr;
      offset = const0_rtx;
    }

  /* If it's the stack or frame pointer, check offset alignment.
     We can have improper alignment in the function entry code.  */
  if (base
      && (REGNO (base) == FRAME_POINTER_REGNUM
	  || REGNO (base) == STACK_POINTER_REGNUM))
    {
      if ((INTVAL (offset) & 0x7) == 0)
	return 1;
    }
  else
    /* Anything else, we know is properly aligned.  */
    return 1;
#else
  /* If the operand is known to have been allocated in static storage, then
     it must be aligned.  */

  if (CONSTANT_P (addr) || GET_CODE (addr) == LO_SUM)
    return 1;

  base = 0;
  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG
          && GET_CODE (XEXP (addr, 1)) == CONST_INT)
        {
          base = XEXP (addr, 0);
          offset = XEXP (addr, 1);
        }
    }
  else if (GET_CODE (addr) == REG)
    {
      base = addr;
      offset = const0_rtx;
    }

  /* Trust round enough offsets from the stack or frame pointer.
     If TARGET_HOPE_ALIGN, trust round enough offset from any register.
     If it is obviously unaligned, don't ever return true.  */
  if (base
      && (REGNO (base) == FRAME_POINTER_REGNUM
          || REGNO (base) == STACK_POINTER_REGNUM
	  || TARGET_HOPE_ALIGN))
    {
      if ((INTVAL (offset) & 0x7) == 0)
	return 1;
    }
  /* Otherwise, we can assume that an access is aligned if it is to an
     aggregate.  Also, if TARGET_HOPE_ALIGN, then assume everything that isn't
     obviously unaligned is aligned.  */
  else if (MEM_IN_STRUCT_P (mem) || TARGET_HOPE_ALIGN)
    return 1;
#endif

  /* An obviously unaligned address.  */
  return 0;
}

enum optype { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP };

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  This is very similar to the following
   output_move_quad function.  */

char *
output_move_double (operands)
     rtx *operands;
{
  register rtx op0 = operands[0];
  register rtx op1 = operands[1];
  register enum optype optype0;
  register enum optype optype1;
  rtx latehalf[2];
  rtx addreg0 = 0;
  rtx addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (op0))
    optype0 = REGOP;
  else if (offsettable_memref_p (op0))
    optype0 = OFFSOP;
  else if (GET_CODE (op0) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (op1))
    optype1 = REGOP;
  else if (CONSTANT_P (op1))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (op1))
    optype1 = OFFSOP;
  else if (GET_CODE (op1) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP
      || (optype0 == MEM && optype1 == MEM))
    abort ();

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (op0, 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (op1, 0));

  /* Ok, we can do one word at a time.
     Set up in LATEHALF the operands to use for the
     high-numbered (least significant) word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx (REG, SImode, REGNO (op0) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (op0, 4);
  else
    latehalf[0] = op0;

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx (REG, SImode, REGNO (op1) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (op1, 4);
  else if (optype1 == CNSTOP)
    split_double (op1, &operands[1], &latehalf[1]);
  else
    latehalf[1] = op1;

  /* Easy case: try moving both words at once.  Check for moving between
     an even/odd register pair and a memory location.  */
  if ((optype0 == REGOP && optype1 != REGOP && optype1 != CNSTOP
       && (REGNO (op0) & 1) == 0)
      || (optype0 != REGOP && optype0 != CNSTOP && optype1 == REGOP
	  && (REGNO (op1) & 1) == 0))
    {
      register rtx mem;

      if (optype0 == REGOP)
	mem = op1;
      else
	mem = op0;

      if (mem_aligned_8 (mem))
	return (mem == op1 ? "ldd %1,%0" : "std %1,%0");
    }

  /* If the first move would clobber the source of the second one,
     do them in the other order.  */

  /* Overlapping registers.  */
  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (op0) == REGNO (latehalf[1]))
    {
      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);
      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }
  /* Loading into a register which overlaps a register used in the address.  */
  else if (optype0 == REGOP && optype1 != REGOP
	   && reg_overlap_mentioned_p (op0, op1))
    {
      /* ??? This fails if the address is a double register address, each
	 of which is clobbered by operand 0.  */
      /* Do the late half first.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);
      /* Then clobber.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("add %0,0x4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,0x4,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("add %0,-0x4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,-0x4,%0", &addreg1);

  return "";
}

/* Output assembler code to perform a quadword move insn
   with operands OPERANDS.  This is very similar to the preceding
   output_move_double function.  */

char *
output_move_quad (operands)
     rtx *operands;
{
  register rtx op0 = operands[0];
  register rtx op1 = operands[1];
  register enum optype optype0;
  register enum optype optype1;
  rtx wordpart[4][2];
  rtx addreg0 = 0;
  rtx addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (op0))
    optype0 = REGOP;
  else if (offsettable_memref_p (op0))
    optype0 = OFFSOP;
  else if (GET_CODE (op0) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (op1))
    optype1 = REGOP;
  else if (CONSTANT_P (op1))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (op1))
    optype1 = OFFSOP;
  else if (GET_CODE (op1) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP
      || (optype0 == MEM && optype1 == MEM))
    abort ();

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the later words.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (op0, 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (op1, 0));

  /* Ok, we can do one word at a time.
     Set up in wordpart the operands to use for each word of the arguments.  */

  if (optype0 == REGOP)
    {
      wordpart[0][0] = gen_rtx (REG, SImode, REGNO (op0) + 0);
      wordpart[1][0] = gen_rtx (REG, SImode, REGNO (op0) + 1);
      wordpart[2][0] = gen_rtx (REG, SImode, REGNO (op0) + 2);
      wordpart[3][0] = gen_rtx (REG, SImode, REGNO (op0) + 3);
    }
  else if (optype0 == OFFSOP)
    {
      wordpart[0][0] = adj_offsettable_operand (op0, 0);
      wordpart[1][0] = adj_offsettable_operand (op0, 4);
      wordpart[2][0] = adj_offsettable_operand (op0, 8);
      wordpart[3][0] = adj_offsettable_operand (op0, 12);
    }
  else
    {
      wordpart[0][0] = op0;
      wordpart[1][0] = op0;
      wordpart[2][0] = op0;
      wordpart[3][0] = op0;
    }

  if (optype1 == REGOP)
    {
      wordpart[0][1] = gen_rtx (REG, SImode, REGNO (op1) + 0);
      wordpart[1][1] = gen_rtx (REG, SImode, REGNO (op1) + 1);
      wordpart[2][1] = gen_rtx (REG, SImode, REGNO (op1) + 2);
      wordpart[3][1] = gen_rtx (REG, SImode, REGNO (op1) + 3);
    }
  else if (optype1 == OFFSOP)
    {
      wordpart[0][1] = adj_offsettable_operand (op1, 0);
      wordpart[1][1] = adj_offsettable_operand (op1, 4);
      wordpart[2][1] = adj_offsettable_operand (op1, 8);
      wordpart[3][1] = adj_offsettable_operand (op1, 12);
    }
  else if (optype1 == CNSTOP)
    {
      /* This case isn't implemented yet, because there is no internal
	 representation for quad-word constants, and there is no split_quad
	 function.  */
#if 0
      split_quad (op1, &wordpart[0][1], &wordpart[1][1],
		  &wordpart[2][1], &wordpart[3][1]);
#else
      abort ();
#endif
    }
  else
    {
      wordpart[0][1] = op1;
      wordpart[1][1] = op1;
      wordpart[2][1] = op1;
      wordpart[3][1] = op1;
    }

  /* Easy case: try moving the quad as two pairs.  Check for moving between
     an even/odd register pair and a memory location.  */
  /* ??? Should also handle the case of non-offsettable addresses here.
     We can at least do the first pair as a ldd/std, and then do the third
     and fourth words individually.  */
  if ((optype0 == REGOP && optype1 == OFFSOP && (REGNO (op0) & 1) == 0)
      || (optype0 == OFFSOP && optype1 == REGOP && (REGNO (op1) & 1) == 0))
    {
      rtx mem;

      if (optype0 == REGOP)
	mem = op1;
      else
	mem = op0;

      if (mem_aligned_8 (mem))
	{
	  operands[2] = adj_offsettable_operand (mem, 8);
	  if (mem == op1)
	    return "ldd %1,%0;ldd %2,%S0";
	  else
	    return "std %1,%0;std %S1,%2";
	}
    }

  /* If the first move would clobber the source of the second one,
     do them in the other order.  */

  /* Overlapping registers.  */
  if (optype0 == REGOP && optype1 == REGOP
      && (REGNO (op0) == REGNO (wordpart[1][3])
	  || REGNO (op0) == REGNO (wordpart[1][2])
	  || REGNO (op0) == REGNO (wordpart[1][1])))
    {
      /* Do fourth word.  */
      output_asm_insn (singlemove_string (wordpart[3]), wordpart[3]);
      /* Do the third word.  */
      output_asm_insn (singlemove_string (wordpart[2]), wordpart[2]);
      /* Do the second word.  */
      output_asm_insn (singlemove_string (wordpart[1]), wordpart[1]);
      /* Do lowest-numbered word.  */
      return singlemove_string (wordpart[0]);
    }
  /* Loading into a register which overlaps a register used in the address.  */
  if (optype0 == REGOP && optype1 != REGOP
      && reg_overlap_mentioned_p (op0, op1))
    {
      /* ??? Not implemented yet.  This is a bit complicated, because we
	 must load which ever part overlaps the address last.  If the address
	 is a double-reg address, then there are two parts which need to
	 be done last, which is impossible.  We would need a scratch register
	 in that case.  */
      abort ();
    }

  /* Normal case: move the four words in lowest to higest address order.  */

  output_asm_insn (singlemove_string (wordpart[0]), wordpart[0]);

  /* Make any unoffsettable addresses point at the second word.  */
  if (addreg0)
    output_asm_insn ("add %0,0x4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,0x4,%0", &addreg1);

  /* Do the second word.  */
  output_asm_insn (singlemove_string (wordpart[1]), wordpart[1]);

  /* Make any unoffsettable addresses point at the third word.  */
  if (addreg0)
    output_asm_insn ("add %0,0x4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,0x4,%0", &addreg1);

  /* Do the third word.  */
  output_asm_insn (singlemove_string (wordpart[2]), wordpart[2]);

  /* Make any unoffsettable addresses point at the fourth word.  */
  if (addreg0)
    output_asm_insn ("add %0,0x4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,0x4,%0", &addreg1);

  /* Do the fourth word.  */
  output_asm_insn (singlemove_string (wordpart[3]), wordpart[3]);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("add %0,-0xc,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,-0xc,%0", &addreg1);

  return "";
}

/* Output assembler code to perform a doubleword move insn with operands
   OPERANDS, one of which must be a floating point register.  */

char *
output_fp_move_double (operands)
     rtx *operands;
{
  rtx addr;

  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))
	return "fmovs %1,%0\n\tfmovs %R1,%R0";
      else if (GET_CODE (operands[1]) == REG)
	{
	  if ((REGNO (operands[1]) & 1) == 0)
	    return "std %1,[%@-8]\n\tldd [%@-8],%0";
	  else
	    return "st %R1,[%@-4]\n\tst %1,[%@-8]\n\tldd [%@-8],%0";
	}
      else
	return output_move_double (operands);
    }
  else if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  if ((REGNO (operands[0]) & 1) == 0)
	    return "std %1,[%@-8]\n\tldd [%@-8],%0";
	  else
	    return "std %1,[%@-8]\n\tld [%@-4],%R0\n\tld [%@-8],%0";
	}
      else
	return output_move_double (operands);
    }
  else abort ();
}

/* Output assembler code to perform a quadword move insn with operands
   OPERANDS, one of which must be a floating point register.  */

char *
output_fp_move_quad (operands)
     rtx *operands;
{
  register rtx op0 = operands[0];
  register rtx op1 = operands[1];
  register rtx addr;

  if (FP_REG_P (op0))
    {
      if (FP_REG_P (op1))
	return "fmovs %1,%0\n\tfmovs %R1,%R0\n\tfmovs %S1,%S0\n\tfmovs %T1,%T0";
      if (GET_CODE (op1) == REG)
	{
	  if ((REGNO (op1) & 1) == 0)
	    return "std %1,[%@-8]\n\tldd [%@-8],%0\n\tstd %S1,[%@-8]\n\tldd [%@-8],%S0";
	  else
	    return "st %R1,[%@-4]\n\tst %1,[%@-8]\n\tldd [%@-8],%0\n\tst %T1,[%@-4]\n\tst %S1,[%@-8]\n\tldd [%@-8],%S0";
	}
      else
	return output_move_quad (operands);
    }
  else if (FP_REG_P (op1))
    {
      if (GET_CODE (op0) == REG)
	{
	  if ((REGNO (op0) & 1) == 0)
	    return "std %1,[%@-8]\n\tldd [%@-8],%0\n\tstd %S1,[%@-8]\n\tldd [%@-8],%S0";
	  else
	    return "std %S1,[%@-8]\n\tld [%@-4],%T0\n\tld [%@-8],%S0\n\tstd %1,[%@-8]\n\tld [%@-4],%R0\n\tld [%@-8],%0";
	}
      else
	return output_move_quad (operands);
    }
  else
    abort ();
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      /* We absolutely can not fudge the frame pointer here, because the
	 frame pointer must always be 8 byte aligned.  It also confuses
	 debuggers.  */
      if (GET_CODE (XEXP (addr, 0)) == REG
	  && REGNO (XEXP (addr, 0)) != FRAME_POINTER_REGNUM)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG
	       && REGNO (XEXP (addr, 1)) != FRAME_POINTER_REGNUM)
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

void
output_sized_memop (opname, mode, signedp)
     char *opname;
     enum machine_mode mode;
     int signedp;
{
  static char *ld_size_suffix_u[] = { "ub", "uh", "", "?", "d" };
  static char *ld_size_suffix_s[] = { "sb", "sh", "", "?", "d" };
  static char *st_size_suffix[] = { "b", "h", "", "?", "d" };
  char **opnametab, *modename;

  if (opname[0] == 'l')
    if (signedp)
      opnametab = ld_size_suffix_s;
    else
      opnametab = ld_size_suffix_u;
  else
    opnametab = st_size_suffix;
  modename = opnametab[GET_MODE_SIZE (mode) >> 1];

  fprintf (asm_out_file, "\t%s%s", opname, modename);
}

void
output_move_with_extension (operands)
     rtx *operands;
{
  if (GET_MODE (operands[2]) == HImode)
    output_asm_insn ("sll %2,0x10,%0", operands);
  else if (GET_MODE (operands[2]) == QImode)
    output_asm_insn ("sll %2,0x18,%0", operands);
  else
    abort ();
}

/* Load the address specified by OPERANDS[3] into the register
   specified by OPERANDS[0].

   OPERANDS[3] may be the result of a sum, hence it could either be:

   (1) CONST
   (2) REG
   (2) REG + CONST_INT
   (3) REG + REG + CONST_INT
   (4) REG + REG  (special case of 3).

   Note that (3) is not a legitimate address.
   All cases are handled here.  */

void
output_load_address (operands)
     rtx *operands;
{
  rtx base, offset;

  if (CONSTANT_P (operands[3]))
    {
      output_asm_insn ("set %3,%0", operands);
      return;
    }

  if (REG_P (operands[3]))
    {
      if (REGNO (operands[0]) != REGNO (operands[3]))
	output_asm_insn ("mov %3,%0", operands);
      return;
    }

  if (GET_CODE (operands[3]) != PLUS)
    abort ();

  base = XEXP (operands[3], 0);
  offset = XEXP (operands[3], 1);

  if (GET_CODE (base) == CONST_INT)
    {
      rtx tmp = base;
      base = offset;
      offset = tmp;
    }

  if (GET_CODE (offset) != CONST_INT)
    {
      /* Operand is (PLUS (REG) (REG)).  */
      base = operands[3];
      offset = const0_rtx;
    }

  if (REG_P (base))
    {
      operands[6] = base;
      operands[7] = offset;
      if (SMALL_INT (offset))
	output_asm_insn ("add %6,%7,%0", operands);
      else
	output_asm_insn ("set %7,%0\n\tadd %0,%6,%0", operands);
    }
  else if (GET_CODE (base) == PLUS)
    {
      operands[6] = XEXP (base, 0);
      operands[7] = XEXP (base, 1);
      operands[8] = offset;

      if (SMALL_INT (offset))
	output_asm_insn ("add %6,%7,%0\n\tadd %0,%8,%0", operands);
      else
	output_asm_insn ("set %8,%0\n\tadd %0,%6,%0\n\tadd %0,%7,%0", operands);
    }
  else
    abort ();
}

/* Output code to place a size count SIZE in register REG.
   ALIGN is the size of the unit of transfer.

   Because block moves are pipelined, we don't include the
   first element in the transfer of SIZE to REG.  */

static void
output_size_for_block_move (size, reg, align)
     rtx size, reg;
     rtx align;
{
  rtx xoperands[3];

  xoperands[0] = reg;
  xoperands[1] = size;
  xoperands[2] = align;
  if (GET_CODE (size) == REG)
    output_asm_insn ("sub %1,%2,%0", xoperands);
  else
    {
      xoperands[1]
	= gen_rtx (CONST_INT, VOIDmode, INTVAL (size) - INTVAL (align));
      output_asm_insn ("set %1,%0", xoperands);
    }
}

/* Emit code to perform a block move.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.
   OPERANDS[2] is the size.
   OPERANDS[3] is the alignment safe to use.
   OPERANDS[4] is a register we can safely clobber as a temp.  */

char *
output_block_move (operands)
     rtx *operands;
{
  /* A vector for our computed operands.  Note that load_output_address
     makes use of (and can clobber) up to the 8th element of this vector.  */
  rtx xoperands[10];
  rtx zoperands[10];
  static int movstrsi_label = 0;
  int i;
  rtx temp1 = operands[4];
  rtx sizertx = operands[2];
  rtx alignrtx = operands[3];
  int align = INTVAL (alignrtx);
  char label3[30], label5[30];

  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = temp1;

  /* We can't move more than this many bytes at a time because we have only
     one register, %g1, to move them through.  */
  if (align > UNITS_PER_WORD)
    {
      align = UNITS_PER_WORD;
      alignrtx = gen_rtx (CONST_INT, VOIDmode, UNITS_PER_WORD);
    }

  /* We consider 8 ld/st pairs, for a total of 16 inline insns to be
     reasonable here.  (Actually will emit a maximum of 18 inline insns for
     the case of size == 31 and align == 4).  */

  if (GET_CODE (sizertx) == CONST_INT && (INTVAL (sizertx) / align) <= 8
      && memory_address_p (QImode, plus_constant_for_output (xoperands[0],
							     INTVAL (sizertx)))
      && memory_address_p (QImode, plus_constant_for_output (xoperands[1],
							     INTVAL (sizertx))))
    {
      int size = INTVAL (sizertx);
      int offset = 0;

      /* We will store different integers into this particular RTX.  */
      xoperands[2] = rtx_alloc (CONST_INT);
      PUT_MODE (xoperands[2], VOIDmode);

      /* This case is currently not handled.  Abort instead of generating
	 bad code.  */
      if (align > 4)
	abort ();

      if (align >= 4)
	{
	  for (i = (size >> 2) - 1; i >= 0; i--)
	    {
	      INTVAL (xoperands[2]) = (i << 2) + offset;
	      output_asm_insn ("ld [%a1+%2],%%g1\n\tst %%g1,[%a0+%2]",
			       xoperands);
	    }
	  offset += (size & ~0x3);
	  size = size & 0x3;
	  if (size == 0)
	    return "";
	}

      if (align >= 2)
	{
	  for (i = (size >> 1) - 1; i >= 0; i--)
	    {
	      INTVAL (xoperands[2]) = (i << 1) + offset;
	      output_asm_insn ("lduh [%a1+%2],%%g1\n\tsth %%g1,[%a0+%2]",
			       xoperands);
	    }
	  offset += (size & ~0x1);
	  size = size & 0x1;
	  if (size == 0)
	    return "";
	}

      if (align >= 1)
	{
	  for (i = size - 1; i >= 0; i--)
	    {
	      INTVAL (xoperands[2]) = i + offset;
	      output_asm_insn ("ldub [%a1+%2],%%g1\n\tstb %%g1,[%a0+%2]",
			       xoperands);
	    }
	  return "";
	}

      /* We should never reach here.  */
      abort ();
    }

  /* If the size isn't known to be a multiple of the alignment,
     we have to do it in smaller pieces.  If we could determine that
     the size was a multiple of 2 (or whatever), we could be smarter
     about this.  */
  if (GET_CODE (sizertx) != CONST_INT)
    align = 1;
  else
    {
      int size = INTVAL (sizertx);
      while (size % align)
	align >>= 1;
    }

  if (align != INTVAL (alignrtx))
    alignrtx = gen_rtx (CONST_INT, VOIDmode, align);

  xoperands[3] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);
  xoperands[4] = gen_rtx (CONST_INT, VOIDmode, align);
  xoperands[5] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);

  ASM_GENERATE_INTERNAL_LABEL (label3, "Lm", INTVAL (xoperands[3]));
  ASM_GENERATE_INTERNAL_LABEL (label5, "Lm", INTVAL (xoperands[5]));

  /* This is the size of the transfer.  Emit code to decrement the size
     value by ALIGN, and store the result in the temp1 register.  */
  output_size_for_block_move (sizertx, temp1, alignrtx);

  /* Must handle the case when the size is zero or negative, so the first thing
     we do is compare the size against zero, and only copy bytes if it is
     zero or greater.  Note that we have already subtracted off the alignment
     once, so we must copy 1 alignment worth of bytes if the size is zero
     here.

     The SUN assembler complains about labels in branch delay slots, so we
     do this before outputting the load address, so that there will always
     be a harmless insn between the branch here and the next label emitted
     below.  */

  {
    char pattern[100];

    sprintf (pattern, "cmp %%2,0\n\tbl %s", &label5[1]);
    output_asm_insn (pattern, xoperands);
  }

  zoperands[0] = operands[0];
  zoperands[3] = plus_constant_for_output (operands[0], align);
  output_load_address (zoperands);

  /* ??? This might be much faster if the loops below were preconditioned
     and unrolled.

     That is, at run time, copy enough bytes one at a time to ensure that the
     target and source addresses are aligned to the the largest possible
     alignment.  Then use a preconditioned unrolled loop to copy say 16
     bytes at a time.  Then copy bytes one at a time until finish the rest.  */

  /* Output the first label separately, so that it is spaced properly.  */

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "Lm", INTVAL (xoperands[3]));

  {
    char pattern[200];
    register char *ld_suffix = (align == 1) ? "ub" : (align == 2) ? "uh" : "";
    register char *st_suffix = (align == 1) ? "b" : (align == 2) ? "h" : "";

    sprintf (pattern, "ld%s [%%1+%%2],%%%%g1\n\tsubcc %%2,%%4,%%2\n\tbge %s\n\tst%s %%%%g1,[%%0+%%2]\n%s:", ld_suffix, &label3[1], st_suffix, &label5[1]);
    output_asm_insn (pattern, xoperands);
  }

  return "";
}

/* Output reasonable peephole for set-on-condition-code insns.
   Note that these insns assume a particular way of defining
   labels.  Therefore, *both* sparc.h and this function must
   be changed if a new syntax is needed.    */

char *
output_scc_insn (operands, insn)
     rtx operands[];
     rtx insn;
{
  static char string[100];
  rtx label = 0, next = insn;
  int need_label = 0;

  /* Try doing a jump optimization which jump.c can't do for us
     because we did not expose that setcc works by using branches.

     If this scc insn is followed by an unconditional branch, then have
     the jump insn emitted here jump to that location, instead of to
     the end of the scc sequence as usual.  */

  do
    {
      if (GET_CODE (next) == CODE_LABEL)
	label = next;
      next = NEXT_INSN (next);
      if (next == 0)
	break;
    }
  while (GET_CODE (next) == NOTE || GET_CODE (next) == CODE_LABEL);

  /* If we are in a sequence, and the following insn is a sequence also,
     then just following the current insn's next field will take us to the
     first insn of the next sequence, which is the wrong place.  We don't
     want to optimize with a branch that has had its delay slot filled.
     Avoid this by verifying that NEXT_INSN (PREV_INSN (next)) == next
     which fails only if NEXT is such a branch.  */

  if (next && GET_CODE (next) == JUMP_INSN && simplejump_p (next)
      && (! final_sequence || NEXT_INSN (PREV_INSN (next)) == next))
    label = JUMP_LABEL (next);
  /* If not optimizing, jump label fields are not set.  To be safe, always
     check here to whether label is still zero.  */
  if (label == 0)
    {
      label = gen_label_rtx ();
      need_label = 1;
    }

  LABEL_NUSES (label) += 1;

  operands[2] = label;

  /* If we are in a delay slot, assume it is the delay slot of an fpcc
     insn since our type isn't allowed anywhere else.  */

  /* ??? Fpcc instructions no longer have delay slots, so this code is
     probably obsolete.  */

  /* The fastest way to emit code for this is an annulled branch followed
     by two move insns.  This will take two cycles if the branch is taken,
     and three cycles if the branch is not taken.

     However, if we are in the delay slot of another branch, this won't work,
     because we can't put a branch in the delay slot of another branch.
     The above sequence would effectively take 3 or 4 cycles respectively
     since a no op would have be inserted between the two branches.
     In this case, we want to emit a move, annulled branch, and then the
     second move.  This sequence always takes 3 cycles, and hence is faster
     when we are in a branch delay slot.  */

  if (final_sequence)
    {
      strcpy (string, "mov 0,%0\n\t");
      strcat (string, output_cbranch (operands[1], 2, 0, 1, 0));
      strcat (string, "\n\tmov 1,%0");
    }
  else
    {
      strcpy (string, output_cbranch (operands[1], 2, 0, 1, 0));
      strcat (string, "\n\tmov 1,%0\n\tmov 0,%0");
    }

  if (need_label)
    strcat (string, "\n%l2:");

  return string;
}

/* Vectors to keep interesting information about registers where
   it can easily be got.  */

/* Modes for condition codes.  */
#define C_MODES						\
  ((1 << (int) CCmode) | (1 << (int) CC_NOOVmode)	\
   | (1 << (int) CCFPmode) | (1 << (int) CCFPEmode))

/* Modes for single-word (and smaller) quantities.  */
#define S_MODES						\
 (~C_MODES						\
  & ~ ((1 << (int) DImode) | (1 << (int) TImode)	\
      | (1 << (int) DFmode) | (1 << (int) TFmode)))

/* Modes for double-word (and smaller) quantities.  */
#define D_MODES					\
  (~C_MODES					\
   & ~ ((1 << (int) TImode) | (1 << (int) TFmode)))

/* Modes for quad-word quantities.  */
#define T_MODES (~C_MODES)

/* Modes for single-float quantities.  We must allow any single word or
   smaller quantity.  This is because the fix/float conversion instructions
   take integer inputs/outputs from the float registers.  */
#define SF_MODES (S_MODES)

/* Modes for double-float quantities.  */
#define DF_MODES (SF_MODES | (1 << (int) DFmode) | (1 << (int) SCmode))

/* Modes for quad-float quantities.  */
#define TF_MODES (DF_MODES | (1 << (int) TFmode) | (1 << (int) DCmode))

/* Value is 1 if register/mode pair is acceptable on sparc.
   The funny mixture of D and T modes is because integer operations
   do not specially operate on tetra quantities, so non-quad-aligned
   registers can hold quadword quantities (except %o4 and %i4 because
   they cross fixed registers.  */

int hard_regno_mode_ok[] = {
  C_MODES, S_MODES, T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES, D_MODES, S_MODES,

  TF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,
  TF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,
  TF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,
  TF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES};

#ifdef __GNUC__
inline
#endif
static int
save_regs (file, low, high, base, offset, n_fregs)
     FILE *file;
     int low, high;
     char *base;
     int offset;
     int n_fregs;
{
  int i;

  for (i = low; i < high; i += 2)
    {
      if (regs_ever_live[i] && ! call_used_regs[i])
	if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	  fprintf (file, "\tstd %s,[%s+%d]\n",
		   reg_names[i], base, offset + 4 * n_fregs),
	  n_fregs += 2;
	else
	  fprintf (file, "\tst %s,[%s+%d]\n",
		   reg_names[i], base, offset + 4 * n_fregs),
	  n_fregs += 2;
      else if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	fprintf (file, "\tst %s,[%s+%d]\n",
		 reg_names[i+1], base, offset + 4 * n_fregs),
	n_fregs += 2;
    }
  return n_fregs;
}

#ifdef __GNUC__
inline
#endif
static int
restore_regs (file, low, high, base, offset, n_fregs)
     FILE *file;
     int low, high;
     char *base;
     int offset;
{
  int i;

  for (i = low; i < high; i += 2)
    {
      if (regs_ever_live[i] && ! call_used_regs[i])
	if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	  fprintf (file, "\tldd [%s+%d], %s\n",
		   base, offset + 4 * n_fregs, reg_names[i]),
	  n_fregs += 2;
	else
	  fprintf (file, "\tld [%s+%d],%s\n",
		   base, offset + 4 * n_fregs, reg_names[i]),
	  n_fregs += 2;
      else if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	fprintf (file, "\tld [%s+%d],%s\n",
		 base, offset + 4 * n_fregs, reg_names[i+1]),
	n_fregs += 2;
    }
  return n_fregs;
}

/* Static variables we want to share between prologue and epilogue.  */

/* Number of live floating point registers needed to be saved.  */
static int num_fregs;

/* Nonzero if any floating point register was ever used.  */
static int fregs_ever_live;

int
compute_frame_size (size, leaf_function)
     int size;
     int leaf_function;
{
  int fregs_ever_live = 0;
  int n_fregs = 0, i;
  int outgoing_args_size = (current_function_outgoing_args_size
			    + REG_PARM_STACK_SPACE (current_function_decl));

  apparent_fsize = ((size) + 7 - STARTING_FRAME_OFFSET) & -8;
  for (i = 32; i < FIRST_PSEUDO_REGISTER; i += 2)
    fregs_ever_live |= regs_ever_live[i]|regs_ever_live[i+1];

  if (TARGET_EPILOGUE && fregs_ever_live)
    {
      for (i = 32; i < FIRST_PSEUDO_REGISTER; i += 2)
	if ((regs_ever_live[i] && ! call_used_regs[i])
	    || (regs_ever_live[i+1] && ! call_used_regs[i+1]))
	  n_fregs += 2;
    }

  /* Set up values for use in `function_epilogue'.  */
  num_fregs = n_fregs;

  apparent_fsize += (outgoing_args_size+7) & -8;
  if (leaf_function && n_fregs == 0
      && apparent_fsize == (REG_PARM_STACK_SPACE (current_function_decl)
			    - STARTING_FRAME_OFFSET))
    apparent_fsize = 0;

  actual_fsize = apparent_fsize + n_fregs*4;

  /* Make sure nothing can clobber our register windows.
     If a SAVE must be done, or there is a stack-local variable,
     the register window area must be allocated.  */
  if (leaf_function == 0 || size > 0)
    actual_fsize += (16 * UNITS_PER_WORD)+8;

  return actual_fsize;
}

/* Output code for the function prologue.  */

void
output_function_prologue (file, size, leaf_function)
     FILE *file;
     int size;
     int leaf_function;
{
  if (leaf_function)
    frame_base_name = "%sp+80";
  else
    frame_base_name = "%fp";

  /* Need to use actual_fsize, since we are also allocating
     space for our callee (and our own register save area).  */
  actual_fsize = compute_frame_size (size, leaf_function);

  fprintf (file, "\t!#PROLOGUE# 0\n");
  if (actual_fsize == 0)
    /* do nothing.  */ ;
  else if (actual_fsize <= 4096)
    {
      if (! leaf_function)
	fprintf (file, "\tsave %%sp,-%d,%%sp\n", actual_fsize);
      else
	fprintf (file, "\tadd %%sp,-%d,%%sp\n", actual_fsize);
    }
  else if (actual_fsize <= 8192)
    {
      /* For frames in the range 4097..8192, we can use just two insns.  */
      if (! leaf_function)
	{
	  fprintf (file, "\tsave %%sp,-4096,%%sp\n");
	  fprintf (file, "\tadd %%sp,-%d,%%sp\n", actual_fsize - 4096);
	}
      else
	{
	  fprintf (file, "\tadd %%sp,-4096,%%sp\n");
	  fprintf (file, "\tadd %%sp,-%d,%%sp\n", actual_fsize - 4096);
	}
    }
  else
    {
      if (! leaf_function)
	{
	  fprintf (file, "\tsethi %%hi(-%d),%%g1\n", actual_fsize);
	  if ((actual_fsize & 0x3ff) != 0)
	    fprintf (file, "\tor %%g1,%%lo(-%d),%%g1\n", actual_fsize);
	  fprintf (file, "\tsave %%sp,%%g1,%%sp\n");
	}
      else
	{
	  fprintf (file, "\tsethi %%hi(-%d),%%g1\n", actual_fsize);
	  if ((actual_fsize & 0x3ff) != 0)
	    fprintf (file, "\tor %%g1,%%lo(-%d),%%g1\n", actual_fsize);
	  fprintf (file, "\tadd %%sp,%%g1,%%sp\n");
	}
    }

  /* If doing anything with PIC, do it now.  */
  if (! flag_pic)
    fprintf (file, "\t!#PROLOGUE# 1\n");

  /* Figure out where to save any special registers.  */
  if (num_fregs)
    {
      int offset, n_fregs = num_fregs;

      if (! leaf_function)
	offset = -apparent_fsize;
      else
	offset = 0;

      if (TARGET_EPILOGUE && ! leaf_function)
	n_fregs = save_regs (file, 0, 16, frame_base_name, offset, 0);
      else if (leaf_function)
	n_fregs = save_regs (file, 0, 32, frame_base_name, offset, 0);
      if (TARGET_EPILOGUE)
	save_regs (file, 32, FIRST_PSEUDO_REGISTER,
		   frame_base_name, offset, n_fregs);
    }

  if (regs_ever_live[62])
    fprintf (file, "\tst %s,[%s-16]\n\tst %s,[%s-12]\n",
	     reg_names[0], frame_base_name,
	     reg_names[0], frame_base_name);

  leaf_label = 0;
  if (leaf_function && actual_fsize != 0)
    {
      /* warning ("leaf procedure with frame size %d", actual_fsize); */
      if (! TARGET_EPILOGUE)
	leaf_label = gen_label_rtx ();
    }
}

/* Output code for the function epilogue.  */

void
output_function_epilogue (file, size, leaf_function)
     FILE *file;
     int size;
     int leaf_function;
{
  int n_fregs, i;
  char *ret;

  if (leaf_label)
    {
      emit_label_after (leaf_label, get_last_insn ());
      final_scan_insn (get_last_insn (), file, 0, 0, 1);
    }

  if (num_fregs)
    {
      int offset, n_fregs = num_fregs;

      if (! leaf_function)
	offset = -apparent_fsize;
      else
	offset = 0;

      if (TARGET_EPILOGUE && ! leaf_function)
	n_fregs = restore_regs (file, 0, 16, frame_base_name, offset, 0);
      else if (leaf_function)
	n_fregs = restore_regs (file, 0, 32, frame_base_name, offset, 0);
      if (TARGET_EPILOGUE)
	restore_regs (file, 32, FIRST_PSEUDO_REGISTER,
		      frame_base_name, offset, n_fregs);
    }

  /* Work out how to skip the caller's unimp instruction if required.  */
  if (leaf_function)
    ret = (current_function_returns_struct ? "jmp %o7+12" : "retl");
  else
    ret = (current_function_returns_struct ? "jmp %i7+12" : "ret");

  if (TARGET_EPILOGUE || leaf_label)
    {
      int old_target_epilogue = TARGET_EPILOGUE;
      target_flags &= ~old_target_epilogue;

      if (! leaf_function)
	{
	  /* If we wound up with things in our delay slot, flush them here.  */
	  if (current_function_epilogue_delay_list)
	    {
	      rtx insn = emit_jump_insn_after (gen_rtx (RETURN, VOIDmode),
					       get_last_insn ());
	      PATTERN (insn) = gen_rtx (PARALLEL, VOIDmode,
					gen_rtvec (2,
						   PATTERN (XEXP (current_function_epilogue_delay_list, 0)),
						   PATTERN (insn)));
	      final_scan_insn (insn, file, 1, 0, 1);
	    }
	  else
	    fprintf (file, "\t%s\n\trestore\n", ret);
	}
      /* All of the following cases are for leaf functions.  */
      else if (current_function_epilogue_delay_list)
	{
	  /* eligible_for_epilogue_delay_slot ensures that if this is a
	     leaf function, then we will only have insn in the delay slot
	     if the frame size is zero, thus no adjust for the stack is
	     needed here.  */
	  if (actual_fsize != 0)
	    abort ();
	  fprintf (file, "\t%s\n", ret);
	  final_scan_insn (XEXP (current_function_epilogue_delay_list, 0),
			   file, 1, 0, 1);
	}
      else if (actual_fsize <= 4096)
	fprintf (file, "\t%s\n\tsub %%sp,-%d,%%sp\n", ret, actual_fsize);
      else if (actual_fsize <= 8192)
	fprintf (file, "\tsub %%sp,-4096,%%sp\n\t%s\n\tsub %%sp,-%d,%%sp\n",
		 ret, actual_fsize - 4096);
      else if ((actual_fsize & 0x3ff) == 0)
	fprintf (file, "\tsethi %%hi(%d),%%g1\n\t%s\n\tadd %%sp,%%g1,%%sp\n",
		 actual_fsize, ret);
      else		 
	fprintf (file, "\tsethi %%hi(%d),%%g1\n\tor %%g1,%%lo(%d),%%g1\n\t%s\n\tadd %%sp,%%g1,%%sp\n",
		 actual_fsize, actual_fsize, ret);
      target_flags |= old_target_epilogue;
    }
}

/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label.  OP is the conditional expression.  The
   mode of register 0 says what kind of comparison we made.

   REVERSED is non-zero if we should reverse the sense of the comparison.

   ANNUL is non-zero if we should generate an annulling branch.

   NOOP is non-zero if we have to follow this branch by a noop.  */

char *
output_cbranch (op, label, reversed, annul, noop)
     rtx op;
     int label;
     int reversed, annul, noop;
{
  static char string[20];
  enum rtx_code code = GET_CODE (op);
  enum machine_mode mode = GET_MODE (XEXP (op, 0));
  static char labelno[] = " %lX";

  /* ??? FP branches can not be preceded by another floating point insn.
     Because there is currently no concept of pre-delay slots, we can fix
     this only by always emitting a nop before a floating point branch.  */

  if (mode == CCFPmode || mode == CCFPEmode)
    strcpy (string, "nop\n\t");

  /* If not floating-point or if EQ or NE, we can just reverse the code.  */
  if (reversed
      && ((mode != CCFPmode && mode != CCFPEmode) || code == EQ || code == NE))
    code = reverse_condition (code), reversed = 0;

  /* Start by writing the branch condition.  */
  switch (code)
    {
    case NE:
      if (mode == CCFPmode || mode == CCFPEmode)
	strcat (string, "fbne");
      else
	strcpy (string, "bne");
      break;

    case EQ:
      if (mode == CCFPmode || mode == CCFPEmode)
	strcat (string, "fbe");
      else
	strcpy (string, "be");
      break;

    case GE:
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  if (reversed)
	    strcat (string, "fbul");
	  else
	    strcat (string, "fbge");
	}
      else if (mode == CC_NOOVmode)
	strcpy (string, "bpos");
      else
	strcpy (string, "bge");
      break;

    case GT:
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  if (reversed)
	    strcat (string, "fbule");
	  else
	    strcat (string, "fbg");
	}
      else
	strcpy (string, "bg");
      break;

    case LE:
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  if (reversed)
	    strcat (string, "fbug");
	  else
	    strcat (string, "fble");
	}
      else
	strcpy (string, "ble");
      break;

    case LT:
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  if (reversed)
	    strcat (string, "fbuge");
	  else
	    strcat (string, "fbl");
	}
      else if (mode == CC_NOOVmode)
	strcpy (string, "bneg");
      else
	strcpy (string, "bl");
      break;

    case GEU:
      strcpy (string, "bgeu");
      break;

    case GTU:
      strcpy (string, "bgu");
      break;

    case LEU:
      strcpy (string, "bleu");
      break;

    case LTU:
      strcpy (string, "blu");
      break;
    }

  /* Now add the annulling, the label, and a possible noop.  */
  if (annul)
    strcat (string, ",a");

  labelno[3] = label + '0';
  strcat (string, labelno);

  if (noop)
    strcat (string, "\n\tnop");

  return string;
}

/* Output assembler code to return from a function.  */

char *
output_return (operands)
     rtx *operands;
{
  if (leaf_label)
    {
      operands[0] = leaf_label;
      return "b,a %l0";
    }
  else if (leaf_function)
    {
      /* If we didn't allocate a frame pointer for the current function,
	 the stack pointer might have been adjusted.  Output code to
	 restore it now.  */

      operands[0] = gen_rtx (CONST_INT, VOIDmode, actual_fsize);

      /* Use sub of negated value in first two cases instead of add to
	 allow actual_fsize == 4096.  */

      if (actual_fsize <= 4096)
	{
	  if (current_function_returns_struct)
	    return "jmp %%o7+12\n\tsub %%sp,-%0,%%sp";
	  else
	    return "retl\n\tsub %%sp,-%0,%%sp";
	}
      else if (actual_fsize <= 8192)
	{
	  operands[0] = gen_rtx (CONST_INT, VOIDmode, actual_fsize - 4096);
	  if (current_function_returns_struct)
	    return "sub %%sp,-4096,%%sp\n\tjmp %%o7+12\n\tsub %%sp,-%0,%%sp";
	  else
	    return "sub %%sp,-4096,%%sp\n\tretl\n\tsub %%sp,-%0,%%sp";
	}
      else if (current_function_returns_struct)
	{
	  if ((actual_fsize & 0x3ff) != 0)
	    return "sethi %%hi(%a0),%%g1\n\tor %%g1,%%lo(%a0),%%g1\n\tjmp %%o7+12\n\tadd %%sp,%%g1,%%sp";
	  else
	    return "sethi %%hi(%a0),%%g1\n\tjmp %%o7+12\n\tadd %%sp,%%g1,%%sp";
	}
      else
	{
	  if ((actual_fsize & 0x3ff) != 0)
	    return "sethi %%hi(%a0),%%g1\n\tor %%g1,%%lo(%a0),%%g1\n\tretl\n\tadd %%sp,%%g1,%%sp";
	  else
	    return "sethi %%hi(%a0),%%g1\n\tretl\n\tadd %%sp,%%g1,%%sp";
	}
    }
  else
    {
      if (current_function_returns_struct)
	return "jmp %%i7+12\n\trestore";
      else
	return "ret\n\trestore";
    }
}

/* Leaf functions and non-leaf functions have different needs.  */

static int
reg_leaf_alloc_order[] = REG_LEAF_ALLOC_ORDER;

static int
reg_nonleaf_alloc_order[] = REG_ALLOC_ORDER;

static int *reg_alloc_orders[] = {
  reg_leaf_alloc_order,
  reg_nonleaf_alloc_order};

void
order_regs_for_local_alloc ()
{
  static int last_order_nonleaf = 1;

  if (regs_ever_live[15] != last_order_nonleaf)
    {
      last_order_nonleaf = !last_order_nonleaf;
      bcopy (reg_alloc_orders[last_order_nonleaf], reg_alloc_order,
	     FIRST_PSEUDO_REGISTER * sizeof (int));
    }
}

/* Machine dependent routines for the branch probability, arc profiling
   code.  */

/* The label used by the arc profiling code.  */

static rtx profiler_label;

void
init_arc_profiler ()
{
  /* Generate and save a copy of this so it can be shared.  */
  profiler_label = gen_rtx (SYMBOL_REF, Pmode, "*LPBX2");
}

void
output_arc_profiler (arcno, insert_after)
     int arcno;
     rtx insert_after;
{
  rtx profiler_target_addr
    = gen_rtx (CONST, Pmode,
	       gen_rtx (PLUS, Pmode, profiler_label,
			gen_rtx (CONST_INT, VOIDmode, 4 * arcno)));
  register rtx profiler_reg = gen_reg_rtx (SImode);
  register rtx address_reg = gen_reg_rtx (Pmode);
  rtx mem_ref;

  insert_after = emit_insn_after (gen_rtx (SET, VOIDmode, address_reg,
					   gen_rtx (HIGH, Pmode,
						    profiler_target_addr)),
				  insert_after);

  mem_ref = gen_rtx (MEM, SImode, gen_rtx (LO_SUM, Pmode, address_reg,
					   profiler_target_addr));
  insert_after = emit_insn_after (gen_rtx (SET, VOIDmode, profiler_reg,
					   mem_ref),
				  insert_after);

  insert_after = emit_insn_after (gen_rtx (SET, VOIDmode, profiler_reg,
					   gen_rtx (PLUS, SImode, profiler_reg,
						    const1_rtx)),
				  insert_after);

  /* This is the same rtx as above, but it is not legal to share this rtx.  */
  mem_ref = gen_rtx (MEM, SImode, gen_rtx (LO_SUM, Pmode, address_reg,
					   profiler_target_addr));
  emit_insn_after (gen_rtx (SET, VOIDmode, mem_ref, profiler_reg),
		   insert_after);
}

/* Return 1 if REGNO (reg1) is even and REGNO (reg1) == REGNO (reg2) - 1.
   This makes them candidates for using ldd and std insns. 

   Note reg1 and reg2 *must* be hard registers.  To be sure we will
   abort if we are passed pseudo registers.  */

int
registers_ok_for_ldd_peep (reg1, reg2)
     rtx reg1, reg2;
{

  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg1) != REG || GET_CODE (reg2) != REG) 
    return 0;

  /* Should never happen.  */
  if (REGNO (reg1) > FIRST_PSEUDO_REGISTER 
      || REGNO (reg2) > FIRST_PSEUDO_REGISTER)
    abort ();

  if (REGNO (reg1) % 2 != 0)
    return 0;

  return (REGNO (reg1) == REGNO (reg2) - 1);
  
}

/* Return 1 if addr1 and addr2 are suitable for use in an ldd or 
   std insn.

   This can only happen when addr1 and addr2 are consecutive memory
   locations (addr1 + 4 == addr2).  addr1 must also be aligned on a 
   64 bit boundary (addr1 % 8 == 0).  

   We know %sp and %fp are kept aligned on a 64 bit boundary.  Other
   registers are assumed to *never* be properly aligned and are 
   rejected.

   Knowing %sp and %fp are kept aligned on a 64 bit boundary, we 
   need only check that the offset for addr1 % 8 == 0.  */

int
addrs_ok_for_ldd_peep (addr1, addr2)
      rtx addr1, addr2;
{
  int reg1, offset1;

  /* Extract a register number and offset (if used) from the first addr.  */
  if (GET_CODE (addr1) == PLUS)
    {
      /* If not a REG, return zero.  */
      if (GET_CODE (XEXP (addr1, 0)) != REG)
	return 0;
      else
	{
          reg1 = REGNO (XEXP (addr1, 0));
	  /* The offset must be constant!  */
	  if (GET_CODE (XEXP (addr1, 1)) != CONST_INT)
            return 0;
          offset1 = INTVAL (XEXP (addr1, 1));
	}
    }
  else if (GET_CODE (addr1) != REG)
    return 0;
  else
    {
      reg1 = REGNO (addr1);
      /* This was a simple (mem (reg)) expression.  Offset is 0.  */
      offset1 = 0;
    }

  /* Make sure the second address is a (mem (plus (reg) (const_int).  */
  if (GET_CODE (addr2) != PLUS)
    return 0;

  if (GET_CODE (XEXP (addr2, 0)) != REG
      || GET_CODE (XEXP (addr2, 1)) != CONST_INT)
    return 0;

  /* Only %fp and %sp are allowed.  Additionally both addresses must
     use the same register.  */
  if (reg1 != FRAME_POINTER_REGNUM && reg1 != STACK_POINTER_REGNUM)
    return 0;

  if (reg1 != REGNO (XEXP (addr2, 0)))
    return 0;

  /* The first offset must be evenly divisible by 8 to ensure the 
     address is 64 bit aligned.  */
  if (offset1 % 8 != 0)
    return 0;

  /* The offset for the second addr must be 4 more than the first addr.  */
  if (INTVAL (XEXP (addr2, 1)) != offset1 + 4)
    return 0;

  /* All the tests passed.  addr1 and addr2 are valid for ldd and std
     instructions.  */
  return 1;
}

/* Return 1 if reg is a pseudo, or is the first register in 
   a hard register pair.  This makes it a candidate for use in
   ldd and std insns.  */

int
register_ok_for_ldd (reg)
     rtx reg;
{

  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg) != REG) 
    return 0;

  if (REGNO (reg) < FIRST_PSEUDO_REGISTER)
    return (REGNO (reg) % 2 == 0);
  else 
    return 1;

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
      /* Output an annul flag if there's nothing for the delay slot and we
	 are optimizing.  This is always used with '(' below.  */
      /* Sun OS 4.1.1 dbx can't handle an annulled unconditional branch;
	 this is a dbx bug.  So, we only do this when optimizing.  */
      if (dbr_sequence_length () == 0 && optimize)
	fputs (",a", file);
      return;
    case '(':
      /* Output a 'nop' if there's nothing for the delay slot and we are
	 not optimizing.  This is always used with '*' above.  */
      if (dbr_sequence_length () == 0 && ! optimize)
	fputs ("\n\tnop", file);
      return;
    case 'Y':
      /* Adjust the operand to take into account a RESTORE operation.  */
      if (GET_CODE (x) != REG)
	abort ();
      if (REGNO (x) < 8)
	fputs (reg_names[REGNO (x)], file);
      else if (REGNO (x) >= 24 && REGNO (x) < 32)
	fputs (reg_names[REGNO (x)-16], file);
      else
	abort ();
      return;
    case '@':
      /* Print out what we are using as the frame pointer.  This might
	 be %fp, or might be %sp+offset.  */
      fputs (frame_base_name, file);
      return;
    case 'R':
      /* Print out the second register name of a register pair or quad.
	 I.e., R (%o0) => %o1.  */
      fputs (reg_names[REGNO (x)+1], file);
      return;
    case 'S':
      /* Print out the third register name of a register quad.
	 I.e., S (%o0) => %o2.  */
      fputs (reg_names[REGNO (x)+2], file);
      return;
    case 'T':
      /* Print out the fourth register name of a register quad.
	 I.e., T (%o0) => %o3.  */
      fputs (reg_names[REGNO (x)+3], file);
      return;
    case 'm':
      /* Print the operand's address only.  */
      output_address (XEXP (x, 0));
      return;
    case 'r':
      /* In this case we need a register.  Use %g0 if the
	 operand is const0_rtx.  */
      if (x == const0_rtx
	  || (GET_MODE (x) != VOIDmode && x == CONST0_RTX (GET_MODE (x))))
	{
	  fputs ("%g0", file);
	  return;
	}
      else
	break;

    case  'A':
      switch (GET_CODE (x))
	{
	case IOR: fputs ("or", file); break;
	case AND: fputs ("and", file); break;
	case XOR: fputs ("xor", file); break;
	default: abort ();
	}
      return;

    case 'B':
      switch (GET_CODE (x))
	{
	case IOR: fputs ("orn", file); break;
	case AND: fputs ("andn", file); break;
	case XOR: fputs ("xnor", file); break;
	default: abort ();
	}
      return;

    case 'b':
      {
	/* Print a sign-extended character.  */
	int i = INTVAL (x) & 0xff;
	if (i & 0x80)
	  i |= 0xffffff00;
	fprintf (file, "%d", i);
	return;
      }

    case 0:
      /* Do nothing special.  */
      break;

    default:
      /* Undocumented flag.  */
      output_operand_lossage ("invalid operand output code");
    }

  if (GET_CODE (x) == REG)
    fputs (reg_names[REGNO (x)], file);
  else if (GET_CODE (x) == MEM)
    {
      fputc ('[', file);
      if (CONSTANT_P (XEXP (x, 0)))
	/* Poor Sun assembler doesn't understand absolute addressing.  */
	fputs ("%g0+", file);
      output_address (XEXP (x, 0));
      fputc (']', file);
    }
  else if (GET_CODE (x) == HIGH)
    {
      fputs ("%hi(", file);
      output_addr_const (file, XEXP (x, 0));
      fputc (')', file);
    }
  else if (GET_CODE (x) == LO_SUM)
    {
      print_operand (file, XEXP (x, 0), 0);
      fputs ("+%lo(", file);
      output_addr_const (file, XEXP (x, 1));
      fputc (')', file);
    }
  else if (GET_CODE (x) == CONST_DOUBLE)
    {
      if (CONST_DOUBLE_HIGH (x) == 0)
	fprintf (file, "%u", CONST_DOUBLE_LOW (x));
      else if (CONST_DOUBLE_HIGH (x) == -1
	       && CONST_DOUBLE_LOW (x) < 0)
	fprintf (file, "%d", CONST_DOUBLE_LOW (x));
      else
	abort ();
    }
  else { output_addr_const (file, x); }
}

/* This function outputs assembler code for VALUE to FILE, where VALUE is
   a 64 bit (DImode) value.  */

/* ??? If there is a 64 bit counterpart to .word that the assembler
   understands, then using that would simply this code greatly.  */

void
output_double_int (file, value)
     FILE *file;
     rtx value;
{
  if (GET_CODE (value) == CONST_INT)
    {
      if (INTVAL (value) < 0)
	ASM_OUTPUT_INT (file, constm1_rtx);
      else
	ASM_OUTPUT_INT (file, const0_rtx);
      ASM_OUTPUT_INT (file, value);
    }
  else if (GET_CODE (value) == CONST_DOUBLE)
    {
      ASM_OUTPUT_INT (file, gen_rtx (CONST_INT, VOIDmode,
				     CONST_DOUBLE_HIGH (value)));
      ASM_OUTPUT_INT (file, gen_rtx (CONST_INT, VOIDmode,
				     CONST_DOUBLE_LOW (value)));
    }
  else if (GET_CODE (value) == SYMBOL_REF
	   || GET_CODE (value) == CONST
	   || GET_CODE (value) == PLUS)
    {
      /* Addresses are only 32 bits.  */
      ASM_OUTPUT_INT (file, const0_rtx);
      ASM_OUTPUT_INT (file, value);
    }
  else
    abort ();
}

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * 2)
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

unsigned long
sparc_type_code (type)
     register tree type;
{
  register unsigned long qualifiers = 0;
  register unsigned shift = 6;

  for (;;)
    {
      switch (TREE_CODE (type))
	{
	case ERROR_MARK:
	  return qualifiers;
  
	case ARRAY_TYPE:
	  qualifiers |= (3 << shift);
	  shift += 2;
	  type = TREE_TYPE (type);
	  break;

	case FUNCTION_TYPE:
	case METHOD_TYPE:
	  qualifiers |= (2 << shift);
	  shift += 2;
	  type = TREE_TYPE (type);
	  break;

	case POINTER_TYPE:
	case REFERENCE_TYPE:
	case OFFSET_TYPE:
	  qualifiers |= (1 << shift);
	  shift += 2;
	  type = TREE_TYPE (type);
	  break;

	case RECORD_TYPE:
	  return (qualifiers | 8);

	case UNION_TYPE:
	  return (qualifiers | 9);

	case ENUMERAL_TYPE:
	  return (qualifiers | 10);

	case VOID_TYPE:
	  return (qualifiers | 16);

	case INTEGER_TYPE:
	  /* Carefully distinguish all the standard types of C,
	     without messing up if the language is not C.
	     Note that we check only for the names that contain spaces;
	     other names might occur by coincidence in other languages.  */
	  if (TYPE_NAME (type) != 0
	      && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	      && DECL_NAME (TYPE_NAME (type)) != 0
	      && TREE_CODE (DECL_NAME (TYPE_NAME (type))) == IDENTIFIER_NODE)
	    {
	      char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
  
	      if (!strcmp (name, "unsigned char"))
		return (qualifiers | 12);
	      if (!strcmp (name, "signed char"))
		return (qualifiers | 2);
	      if (!strcmp (name, "unsigned int"))
		return (qualifiers | 14);
	      if (!strcmp (name, "short int"))
		return (qualifiers | 3);
	      if (!strcmp (name, "short unsigned int"))
		return (qualifiers | 13);
	      if (!strcmp (name, "long int"))
		return (qualifiers | 5);
	      if (!strcmp (name, "long unsigned int"))
		return (qualifiers | 15);
	      if (!strcmp (name, "long long int"))
		return (qualifiers | 5);	/* Who knows? */
	      if (!strcmp (name, "long long unsigned int"))
		return (qualifiers | 15);	/* Who knows? */
	    }
  
	  /* Most integer types will be sorted out above, however, for the
	     sake of special `array index' integer types, the following code
	     is also provided.  */
  
	  if (TYPE_PRECISION (type) == INT_TYPE_SIZE)
	    return (qualifiers | (TREE_UNSIGNED (type) ? 14 : 4));
  
	  if (TYPE_PRECISION (type) == LONG_TYPE_SIZE)
	    return (qualifiers | (TREE_UNSIGNED (type) ? 15 : 5));
  
	  if (TYPE_PRECISION (type) == LONG_LONG_TYPE_SIZE)
	    return (qualifiers | (TREE_UNSIGNED (type) ? 15 : 5));
  
	  if (TYPE_PRECISION (type) == SHORT_TYPE_SIZE)
	    return (qualifiers | (TREE_UNSIGNED (type) ? 13 : 3));
  
	  if (TYPE_PRECISION (type) == CHAR_TYPE_SIZE)
	    return (qualifiers | (TREE_UNSIGNED (type) ? 12 : 2));
  
	  abort ();
  
	case REAL_TYPE:
	  /* Carefully distinguish all the standard types of C,
	     without messing up if the language is not C.  */
	  if (TYPE_NAME (type) != 0
	      && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	      && DECL_NAME (TYPE_NAME (type)) != 0
	      && TREE_CODE (DECL_NAME (TYPE_NAME (type))) == IDENTIFIER_NODE)
	    {
	      char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
  
	      if (!strcmp (name, "long double"))
		return (qualifiers | 7);	/* Who knows? */
	    }
  
	  if (TYPE_PRECISION (type) == DOUBLE_TYPE_SIZE)
	    return (qualifiers | 7);
	  if (TYPE_PRECISION (type) == FLOAT_TYPE_SIZE)
	    return (qualifiers | 6);
	  if (TYPE_PRECISION (type) == LONG_DOUBLE_TYPE_SIZE)
	    return (qualifiers | 7);	/* Who knows? */
	  abort ();
  
	case COMPLEX_TYPE:	/* GNU Fortran COMPLEX type.  */
	  /* ??? We need to dinguish between double and float complex types,
	     but I don't know how yet because I can't reach this code from
	     existing front-ends.  */
	  return (qualifiers | 7);	/* Who knows? */

	case CHAR_TYPE:		/* GNU Pascal CHAR type.  Not used in C.  */
	case BOOLEAN_TYPE:	/* GNU Fortran BOOLEAN type.  */
	case FILE_TYPE:		/* GNU Pascal FILE type.  */
	case STRING_TYPE:	/* GNU Fortran STRING type. */
	case LANG_TYPE:		/* ? */
	  abort ();
  
	default:
	  abort ();		/* Not a type! */
        }
    }
}

/* Subroutines to support a flat (single) register window calling
   convention.  */

/* Single-register window sparc stack frames look like:

             Before call		        After call
        +-----------------------+	+-----------------------+
   high |			|       |      			|
   mem. |		        |	|			|
        |  caller's temps.    	|       |  caller's temps.    	|
	|       		|       |       	        |
        +-----------------------+	+-----------------------+
 	|       		|	|		        |
        |  arguments on stack.  |	|  arguments on stack.  |
	|       		|FP+92->|			|
        +-----------------------+	+-----------------------+
 	|  6 words to save     	|	|  6 words to save	|
	|  arguments passed	|	|  arguments passed	|
	|  in registers, even	|	|  in registers, even	|
 SP+68->|  if not passed.       |FP+68->|  if not passed.	|
	+-----------------------+       +-----------------------+
	| 1 word struct addr	|FP+64->| 1 word struct addr	|
	+-----------------------+       +-----------------------+
	|			|	|			|
	| 16 word reg save area	|	| 16 word reg save area |
    SP->|			|   FP->|			|
	+-----------------------+	+-----------------------+
					| 4 word area for	|
				 FP-16->| fp/alu reg moves	|
					+-----------------------+
					|			|
					|  local variables	|
					|			|
					+-----------------------+
					|		        |
                                        |  fp register save     |
					|			|
					+-----------------------+
					|		        |
                                        |  gp register save     |
                                        |       		|
					+-----------------------+
					|			|
                                        |  alloca allocations   |
        				|			|
					+-----------------------+
					|			|
                                        |  arguments on stack   |
        			 SP+92->|		        |
					+-----------------------+
                                        |  6 words to save      |
					|  arguments passed     |
                                        |  in registers, even   |
   low                           SP+68->|  if not passed.       |
   memory        			+-----------------------+
				 SP+64->| 1 word struct addr	|
					+-----------------------+
					|			|
					I 16 word reg save area |
				    SP->|			|
					+-----------------------+  */

/* Structure to be filled in by sparc_frw_compute_frame_size with register
   save masks, and offsets for the current function.  */

struct sparc_frame_info
{
  unsigned long total_size;	/* # bytes that the entire frame takes up.  */
  unsigned long var_size;	/* # bytes that variables take up.  */
  unsigned long args_size;	/* # bytes that outgoing arguments take up.  */
  unsigned long extra_size;	/* # bytes of extra gunk.  */
  unsigned int  gp_reg_size;	/* # bytes needed to store gp regs.  */
  unsigned int  fp_reg_size;	/* # bytes needed to store fp regs.  */
  unsigned long mask;		/* Mask of saved gp registers.  */
  unsigned long fmask;		/* Mask of saved fp registers.  */
  unsigned long gp_sp_offset;	/* Offset from new sp to store gp regs.  */
  unsigned long fp_sp_offset;	/* Offset from new sp to store fp regs.  */
  int		initialized;	/* Nonzero if frame size already calculated.  */
};

/* Current frame information calculated by sparc_frw_compute_frame_size.  */
struct sparc_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
struct sparc_frame_info zero_frame_info;

/* Tell prologue and epilogue if register REGNO should be saved / restored.  */

#define MUST_SAVE_REGISTER(regno) \
 ((regs_ever_live[regno] && !call_used_regs[regno])		\
  || (regno == FRAME_POINTER_REGNUM && frame_pointer_needed)	\
  || (regno == 15 && regs_ever_live[15]))

#ifndef SPARC_STACK_ALIGN
#define STACK_BYTES (STACK_BOUNDARY / 8)
#define SPARC_STACK_ALIGN(X) (((X) + STACK_BYTES -  1) & -STACK_BYTES)
#endif

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.  */

unsigned long
sparc_frw_compute_frame_size (size)
     int size;			/* # of var. bytes allocated.  */
{
  int regno;
  unsigned long total_size;	/* # bytes that the entire frame takes up.  */
  unsigned long var_size;	/* # bytes that variables take up.  */
  unsigned long args_size;	/* # bytes that outgoing arguments take up.  */
  unsigned long extra_size;	/* # extra bytes.  */
  unsigned int  gp_reg_size;	/* # bytes needed to store gp regs.  */
  unsigned int  fp_reg_size;	/* # bytes needed to store fp regs.  */
  unsigned long mask;		/* Mask of saved gp registers.  */
  unsigned long fmask;		/* Mask of saved fp registers.  */

  /* This is the size of the 16 word reg save area, 1 word struct addr
     area, and 4 word fp/alu register copy area.  */
  extra_size	 = -STARTING_FRAME_OFFSET + FIRST_PARM_OFFSET(0);
  var_size	 = size;
  /* Also include the size needed for the 6 parameter registers.  */
  args_size	 = current_function_outgoing_args_size + 24;
  total_size	 = var_size + args_size + extra_size;
  gp_reg_size	 = 0;
  fp_reg_size	 = 0;
  mask		 = 0;
  fmask		 = 0;

  /* Calculate space needed for gp registers.  */
  for (regno = 1; regno <= 31; regno++)
    {
      if (MUST_SAVE_REGISTER (regno))
	{
	  if ((regno & 0x1) == 0 && MUST_SAVE_REGISTER (regno+1))
	    {
	      if (gp_reg_size % 8 != 0)
		gp_reg_size += UNITS_PER_WORD;
	      gp_reg_size += 2 * UNITS_PER_WORD;
	      mask |= 3 << regno;
	      regno++;
	    }
	  else
	    {
	      gp_reg_size += UNITS_PER_WORD;
	      mask |= 1 << regno;
	    }
	}
    }
  /* Add extra word in case we have to align the space to a double word
     boundary.  */
  if (gp_reg_size != 0)
    gp_reg_size += UNITS_PER_WORD;

  /* Calculate space needed for fp registers.  */
  for (regno = 32; regno <= 63; regno++)
    {
      if (regs_ever_live[regno] && !call_used_regs[regno])
	{
	  fp_reg_size += UNITS_PER_WORD;
	  fmask |= 1 << (regno - 32);
	}
    }

  total_size += gp_reg_size + fp_reg_size;

  if (total_size == extra_size)
    total_size = extra_size = 0;

  total_size = SPARC_STACK_ALIGN (total_size);

  /* Save other computed information.  */
  current_frame_info.total_size  = total_size;
  current_frame_info.var_size    = var_size;
  current_frame_info.args_size   = args_size;
  current_frame_info.extra_size  = extra_size;
  current_frame_info.gp_reg_size = gp_reg_size;
  current_frame_info.fp_reg_size = fp_reg_size;
  current_frame_info.mask	 = mask;
  current_frame_info.fmask	 = fmask;
  current_frame_info.initialized = reload_completed;

  if (mask)
    {
      unsigned long offset = args_size;
      if (extra_size)
	offset += FIRST_PARM_OFFSET(0);
      current_frame_info.gp_sp_offset = offset;
    }

  if (fmask)
    {
      unsigned long offset = args_size + gp_reg_size;
      if (extra_size)
	offset += FIRST_PARM_OFFSET(0);
      current_frame_info.fp_sp_offset = offset;
    }

  /* Ok, we're done.  */
  return total_size;
}

/* Common code to save/restore registers.  */

void
sparc_frw_save_restore (file, word_op, doubleword_op)
     FILE *file;		/* Stream to write to.  */
     char *word_op;		/* Operation to do for one word.  */
     char *doubleword_op;	/* Operation to do for doubleword.  */
{
  int regno;
  unsigned long mask	  = current_frame_info.mask;
  unsigned long fmask	  = current_frame_info.fmask;
  unsigned long gp_offset;
  unsigned long fp_offset;
  unsigned long max_offset;
  char *base_reg;

  if (mask == 0 && fmask == 0)
    return;

  base_reg   = reg_names[STACK_POINTER_REGNUM];
  gp_offset  = current_frame_info.gp_sp_offset;
  fp_offset  = current_frame_info.fp_sp_offset;
  max_offset = (gp_offset > fp_offset) ? gp_offset : fp_offset;

  /* Deal with calling functions with a large structure.  */
  if (max_offset >= 4096)
    {
      char *temp = "%g2";
      fprintf (file, "\tset %ld,%s\n", max_offset, temp);
      fprintf (file, "\tadd %s,%s,%s\n", temp, base_reg, temp);
      base_reg = temp;
      gp_offset = max_offset - gp_offset;
      fp_offset = max_offset - fp_offset;
    }

  /* Save registers starting from high to low.  The debuggers prefer
     at least the return register be stored at func+4, and also it
     allows us not to need a nop in the epilog if at least one
     register is reloaded in addition to return address.  */

  if (mask || frame_pointer_needed)
    {
      for (regno = 1; regno <= 31; regno++)
	{
	  if ((mask & (1L << regno)) != 0
	      || (regno == FRAME_POINTER_REGNUM && frame_pointer_needed))
	    {
	      if ((regno & 0x1) == 0 && ((mask & (1L << regno+1)) != 0))
		{
		  if (gp_offset % 8 != 0)
		    gp_offset += UNITS_PER_WORD;
		  
		  if (word_op[0] == 's')
		    fprintf (file, "\t%s %s,[%s+%d]\n",
			     doubleword_op, reg_names[regno],
			     base_reg, gp_offset);
		  else
		    fprintf (file, "\t%s [%s+%d],%s\n",
			     doubleword_op, base_reg, gp_offset,
			     reg_names[regno]);

		  gp_offset += 2 * UNITS_PER_WORD;
		  regno++;
		}
	      else
		{
		  if (word_op[0] == 's')
		    fprintf (file, "\t%s %s,[%s+%d]\n",
			     word_op, reg_names[regno],
			     base_reg, gp_offset);
		  else
		    fprintf (file, "\t%s [%s+%d],%s\n",
			     word_op, base_reg, gp_offset, reg_names[regno]);

		  gp_offset += UNITS_PER_WORD;
		}
	    }
	}
    }

  if (fmask)
    {
      for (regno = 32; regno <= 63; regno++)
	{
	  if ((fmask & (1L << (regno - 32))) != 0)
	    {
	      if (word_op[0] == 's')
		fprintf (file, "\t%s %s,[%s+%d]\n",
			 word_op, reg_names[regno],
			 base_reg, gp_offset);
	      else
		fprintf (file, "\t%s [%s+%d],%s\n",
			 word_op, base_reg, gp_offset, reg_names[regno]);

	      fp_offset += UNITS_PER_WORD;
	    }
	}
    }
}

/* Set up the stack and frame (if desired) for the function.  */

void
sparc_frw_output_function_prologue (file, size, ignored)
     FILE *file;
     int size;
{
  extern char call_used_regs[];
  int regno;
  int tsize;
  char *sp_str = reg_names[STACK_POINTER_REGNUM];
  frame_base_name
    = (!frame_pointer_needed) ? "%sp+80" : reg_names[FRAME_POINTER_REGNUM];

  fprintf (file, "\t!#PROLOGUE# 0\n");

  size = SPARC_STACK_ALIGN (size);
  tsize = (! current_frame_info.initialized
	   ? sparc_frw_compute_frame_size (size)
	   : current_frame_info.total_size);

  if (tsize > 0)
    {
      if (tsize <= 4095)
	fprintf (file,
		 "\tsub %s,%d,%s\t\t!# vars= %d, regs= %d/%d, args = %d, extra= %d\n",
		 sp_str, tsize, sp_str, current_frame_info.var_size,
		 current_frame_info.gp_reg_size / 4,
		 current_frame_info.fp_reg_size / 8,
		 current_function_outgoing_args_size,
		 current_frame_info.extra_size);
      else
	fprintf (file,
		 "\tset %d,%s\n\tsub\t%s,%s,%s\t\t!# vars= %d, regs= %d/%d, args = %d, sfo= %d\n",
		 tsize, "%g1", sp_str, "%g1",
		 sp_str, current_frame_info.var_size,
		 current_frame_info.gp_reg_size / 4,
		 current_frame_info.fp_reg_size / 8,
		 current_function_outgoing_args_size,
		 current_frame_info.extra_size);
    }

  sparc_frw_save_restore (file, "st", "std");

  if (frame_pointer_needed)
    {
      if (tsize <= 4095)
	fprintf (file, "\tadd %s,%d,%s\t!# set up frame pointer\n", sp_str,
		 tsize, frame_base_name);
      else
	fprintf (file, "\tadd %s,%s,%s\t!# set up frame pointer\n", sp_str,
		 "%g1", frame_base_name);
    }
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs. */

void
sparc_frw_output_function_epilogue (file, size, ignored1, ignored2)
     FILE *file;
     int size;
{
  extern FILE *asm_out_data_file, *asm_out_file;
  extern char call_used_regs[];
  extern int frame_pointer_needed;
  int tsize;
  char *sp_str = reg_names[STACK_POINTER_REGNUM];
  char *t1_str = "%g1";
  rtx epilogue_delay = current_function_epilogue_delay_list;
  int noepilogue = FALSE;
  int load_nop = FALSE;
  int load_only_r15;

  /* The epilogue does not depend on any registers, but the stack
     registers, so we assume that if we have 1 pending nop, it can be
     ignored, and 2 it must be filled (2 nops occur for integer
     multiply and divide).  */

  size = SPARC_STACK_ALIGN (size);
  tsize = (!current_frame_info.initialized
	   ? sparc_frw_compute_frame_size (size)
	   : current_frame_info.total_size);

  if (tsize == 0 && epilogue_delay == 0)
    {
      rtx insn = get_last_insn ();

      /* If the last insn was a BARRIER, we don't have to write any code
	 because a jump (aka return) was put there.  */
      if (GET_CODE (insn) == NOTE)
	insn = prev_nonnote_insn (insn);
      if (insn && GET_CODE (insn) == BARRIER)
	noepilogue = TRUE;
    }

  if (!noepilogue)
    {
      /* In the reload sequence, we don't need to fill the load delay
	 slots for most of the loads, also see if we can fill the final
	 delay slot if not otherwise filled by the reload sequence.  */

      if (tsize > 4095)
	fprintf (file, "\tset %d,%s\n", tsize, t1_str);

      if (frame_pointer_needed)
	{
	  char *fp_str = reg_names[FRAME_POINTER_REGNUM];
	  if (tsize > 4095)
	    fprintf (file,"\tsub %s,%s,%s\t\t!# sp not trusted  here\n",
		     fp_str, t1_str, sp_str);
	  else
	    fprintf (file,"\tsub %s,%d,%s\t\t!# sp not trusted  here\n",
		     fp_str, tsize, sp_str);
	}

      sparc_frw_save_restore (file, "ld", "ldd");

      load_only_r15 = (current_frame_info.mask == (1 << 15)
		       && current_frame_info.fmask == 0);

      if (current_function_returns_struct)
	fprintf (file, "\tjmp %%o7+12\n");
      else
	fprintf (file, "\tretl\n");

      /* If the only register saved is the return address, we need a
	 nop, unless we have an instruction to put into it.  Otherwise
	 we don't since reloading multiple registers doesn't reference
	 the register being loaded.  */

      if (epilogue_delay)
	{
	  if (tsize)
	    abort ();
	  final_scan_insn (XEXP (epilogue_delay, 0), file, 1, -2, 1);
	}

      else if (tsize > 4095)
	fprintf (file, "\tadd %s,%s,%s\n", sp_str, t1_str, sp_str);

      else if (tsize > 0)
	fprintf (file, "\tadd %s,%d,%s\n", sp_str, tsize, sp_str);

      else
	fprintf (file, "\tnop\n");
    }

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;
}

/* Define the number of delay slots needed for the function epilogue.

   On the sparc, we need a slot if either no stack has been allocated,
   or the only register saved is the return register.  */

int
sparc_frw_epilogue_delay_slots ()
{
  if (!current_frame_info.initialized)
    (void) sparc_frw_compute_frame_size (get_frame_size ());

  if (current_frame_info.total_size == 0)
    return 1;

  if (current_frame_info.mask == (1 << 15) && current_frame_info.fmask == 0)
    return 1;

  return 0;
}

/* Return true is TRIAL is a valid insn for the epilogue delay slot.
   Any single length instruction which doesn't reference the stack or frame
   pointer is OK.  */

int
sparc_frw_eligible_for_epilogue_delay (trial, slot)
     rtx trial;
     int slot;
{
  if (get_attr_length (trial) == 1
      && ! reg_mentioned_p (stack_pointer_rtx, PATTERN (trial))
      && ! reg_mentioned_p (frame_pointer_rtx, PATTERN (trial)))
    return 1;
  return 0;
}
