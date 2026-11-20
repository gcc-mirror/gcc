/* Subroutines used for instruction fusion for RISC-V.
   Copyright (C) 2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "regs.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "function.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "tm_p.h"
#include "riscv-protos.h"

/* Implement TARGET_SCHED_MACRO_FUSION_P.  Return true if target supports
   instruction fusion of some sort.  */

bool
riscv_macro_fusion_p (void)
{
  return riscv_get_fusible_ops () != RISCV_FUSE_NOTHING;
}

/* Return true iff the instruction fusion described by OP is enabled.  */

static bool
riscv_fusion_enabled_p (enum riscv_fusion_pairs op)
{
  return riscv_get_fusible_ops () & op;
}

/* Extract the single sets from PREV and CURR.  Reject conditional jumps,
   which cannot form a fusion pair.  */

static bool
riscv_fuse_sets_p (rtx_insn *prev, rtx_insn *curr,
		   rtx *prev_set_out = NULL,
		   rtx *curr_set_out = NULL)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (prev_set_out)
    *prev_set_out = prev_set;
  if (curr_set_out)
    *curr_set_out = curr_set;
  return prev_set && curr_set && !any_condjump_p (curr);
}

/* Extract a word-form binary operation with code CODE:
    (set (reg:DI rd)
	 (sign_extend:DI (op:SI operand1 operand2)))
   or
    (set (reg:DI rd)
	  (sign_extend:DI (truncate:SI (op:DI operand1 operand2))))
   or
    (set (reg:DI rd)
	  (sign_extend:DI (subreg:SI (op:DI operand1 operand2) 0)))
   Store the binary operation in *BINARY_SRC.  */

static bool
riscv_set_extract_word_binary_p (rtx set, rtx_code code, rtx *binary_src)
{
  if (!TARGET_64BIT)
    return false;

  rtx src = SET_SRC (set);
  *binary_src = NULL_RTX;

  if (GET_CODE (src) != SIGN_EXTEND
      || GET_MODE (src) != DImode)
    return false;

  src = XEXP (src, 0);
  if (GET_CODE (src) == code && GET_MODE (src) == SImode)
    {
      *binary_src = src;
      return true;
    }

  if (GET_MODE (src) != SImode)
    return false;

  if (GET_CODE (src) == TRUNCATE)
    src = XEXP (src, 0);
  else if (SUBREG_P (src) && subreg_lowpart_p (src))
    src = SUBREG_REG (src);
  else
    return false;

  if (GET_CODE (src) == code && GET_MODE (src) == DImode)
    {
      *binary_src = src;
      return true;
    }

  return false;
}

/* Return a comparable register number for X.  Use pseudo identity while
   pseudos may still be created and the hard-register number afterwards.
   Return INVALID_REGNUM if X is not a suitable register.  */

static unsigned int
riscv_regno (rtx x)
{
  /* Check pseudos first: true_regnum may already map one to a hard
     register.  */
  if (can_create_pseudo_p ())
    {
      rtx reg = SUBREG_P (x) ? SUBREG_REG (x) : x;
      if (REG_P (reg) && !HARD_REGISTER_P (reg))
	return REGNO (reg);
    }

  int regno = true_regnum (x);
  if (regno >= 0 && regno < FIRST_PSEUDO_REGISTER)
    return regno;

  return INVALID_REGNUM;
}

/* Return true if X and Y refer to the same comparable register part.
   Lowpart views of different modes can refer to the same part.  */

static bool
riscv_fuse_same_reg_p (rtx x, rtx y)
{
  unsigned int x_regno = riscv_regno (x);
  unsigned int y_regno = riscv_regno (y);

  if (x_regno == INVALID_REGNUM || x_regno != y_regno)
    return false;

  /* Different words of a multiword pseudo cannot share a hard register.
     Use bit positions to account for the modes and target endianness.  */
  if (x_regno >= FIRST_PSEUDO_REGISTER)
    {
      poly_uint64 x_lsb = SUBREG_P (x) ? subreg_lsb (x) : 0;
      poly_uint64 y_lsb = SUBREG_P (y) ? subreg_lsb (y) : 0;
      return known_eq (x_lsb, y_lsb);
    }

  return true;
}

/* Return true if PREV_SET and CURR_SET satisfy the same-destination
   constraint.  Before allocation, defer the destination-register comparison
   so that the scheduler can keep potential fusion pairs together.  If
   USED_IN_SRC_P, always require CURR_SET to use PREV_SET's destination as its
   first source operand.  After allocation, also require the hard-register
   destinations to match.  */

static bool
riscv_fuse_same_dest_p (rtx prev_set, rtx curr_set,
			bool used_in_src_p = false)
{
  unsigned int prev_dest_regno = riscv_regno (SET_DEST (prev_set));
  if (prev_dest_regno == INVALID_REGNUM)
    return false;

  unsigned int curr_dest_regno = riscv_regno (SET_DEST (curr_set));
  if (curr_dest_regno == INVALID_REGNUM)
    return false;

  if (used_in_src_p)
    {
      rtx src = SET_SRC (curr_set);
      rtx word_add_src;
      if (riscv_set_extract_word_binary_p (curr_set, PLUS,
					   &word_add_src))
	src = XEXP (word_add_src, 0);
      else
	{
	  while (GET_CODE (src) == SIGN_EXTEND
		 || GET_CODE (src) == ZERO_EXTEND)
	    src = XEXP (src, 0);

	  if (GET_CODE (src) == NOT
	      || BINARY_P (src)
	      || GET_CODE (src) == LO_SUM
	      || GET_CODE (src) == ZERO_EXTRACT)
	    src = XEXP (src, 0);

	  while (GET_CODE (src) == SIGN_EXTEND
		 || GET_CODE (src) == ZERO_EXTEND)
	    src = XEXP (src, 0);
	}

      if (!riscv_fuse_same_reg_p (src, SET_DEST (prev_set)))
	return false;
    }

  return can_create_pseudo_p () || prev_dest_regno == curr_dest_regno;
}

/* Matches an add:
   (set (reg rd) (plus (reg rs1) (reg rs2))) */

static bool
riscv_set_is_add_p (rtx set)
{
  return (GET_CODE (SET_SRC (set)) == PLUS
	  && REG_P (XEXP (SET_SRC (set), 0))
	  && REG_P (XEXP (SET_SRC (set), 1))
	  && REG_P (SET_DEST (set)));
}

/* Matches an addi:
   (set (reg rd) (plus (reg rs1) (const_int imm12))) */

static bool
riscv_set_is_addi_p (rtx set)
{
  return (GET_CODE (SET_SRC (set)) == PLUS
	  && REG_P (XEXP (SET_SRC (set), 0))
	  && CONST_INT_P (XEXP (SET_SRC (set), 1))
	  && REG_P (SET_DEST (set)));
}

/* Matches an add.uw:
  (set (reg:DI rd)
    (plus:DI (zero_extend:DI (reg:SI rs1)) (reg:DI rs2))) */

static bool
riscv_set_is_adduw_p (rtx set)
{
  return (GET_CODE (SET_SRC (set)) == PLUS
	  && GET_CODE (XEXP (SET_SRC (set), 0)) == ZERO_EXTEND
	  && REG_P (XEXP (XEXP (SET_SRC (set), 0), 0))
	  && REG_P (XEXP (SET_SRC (set), 1))
	  && REG_P (SET_DEST (set)));
}

/* Matches a shNadd:
   (set (reg rd)
	(plus (ashift (reg rs1) (const_int N)) (reg rs2))) */

static bool
riscv_set_is_shNadd_p (rtx set)
{
  return (GET_CODE (SET_SRC (set)) == PLUS
	  && GET_CODE (XEXP (SET_SRC (set), 0)) == ASHIFT
	  && REG_P (XEXP (XEXP (SET_SRC (set), 0), 0))
	  && CONST_INT_P (XEXP (XEXP (SET_SRC (set), 0), 1))
	  && (INTVAL (XEXP (XEXP (SET_SRC (set), 0), 1)) == 1
	      || INTVAL (XEXP (XEXP (SET_SRC (set), 0), 1)) == 2
	      || INTVAL (XEXP (XEXP (SET_SRC (set), 0), 1)) == 3)
	  && REG_P (SET_DEST (set)));
}

/* Matches a shNadd.uw:
  (set (reg:DI rd)
       (plus:DI (and:DI (ashift:DI (reg:DI rs1) (const_int N))
			(const_int mask))
		(reg:DI rs2))) */

static bool
riscv_set_is_shNadduw_p (rtx set)
{
  return (GET_CODE (SET_SRC (set)) == PLUS
	  && GET_CODE (XEXP (SET_SRC (set), 0)) == AND
	  && GET_CODE (XEXP (XEXP (SET_SRC (set), 0), 0)) == ASHIFT
	  && REG_P (XEXP (XEXP (XEXP (SET_SRC (set), 0), 0), 0))
	  && CONST_INT_P (XEXP (XEXP (XEXP (SET_SRC (set), 0), 0), 1))
	  && (INTVAL (XEXP (XEXP (XEXP (SET_SRC (set), 0), 0), 1)) == 1
	      || INTVAL (XEXP (XEXP (XEXP (SET_SRC (set), 0), 0), 1)) == 2
	      || INTVAL (XEXP (XEXP (XEXP (SET_SRC (set), 0), 0), 1)) == 3)
	  && REG_P (SET_DEST (set)));
}

/* Matches an addiw:
     (set (reg:DI rd)
	  (sign_extend:DI (plus:SI (reg:SI rs1) (const_int imm12))))
   or an equivalent word-add RTL form.  */

static bool
riscv_set_is_addiw_p (rtx set, rtx *src0 = NULL)
{
  if (!TARGET_64BIT)
    return false;

  rtx src;
  if (riscv_set_extract_word_binary_p (set, PLUS, &src)
      && REG_P (XEXP (src, 0))
      && CONST_INT_P (XEXP (src, 1))
      && REG_P (SET_DEST (set)))
    {
      if (src0)
	*src0 = XEXP (src, 0);
      return true;
    }

  return false;
}

/* Matches an addw:
     (set (reg:DI rd)
	  (sign_extend:DI (plus:SI (reg:SI rs1) (reg:SI rs2))))
   or an equivalent word-add RTL form.  */

static bool
riscv_set_is_addw_p (rtx set)
{
  if (!TARGET_64BIT)
    return false;

  rtx src;
  return (riscv_set_extract_word_binary_p (set, PLUS, &src)
	  && REG_P (XEXP (src, 0))
	  && REG_P (XEXP (src, 1))
	  && riscv_regno (SET_DEST (set)) != INVALID_REGNUM);
}

/* Matches an add-type instruction:
     (set (reg rd) (plus (reg rs1) (reg rs2)))
   or an accepted addw or add.uw RTL form.  Store whether the instruction is
   a word form in *WORD_P when requested.  */

static bool
riscv_insn_is_add_type_p (rtx_insn *insn, bool *word_p = NULL)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  enum attr_type type = get_attr_type (insn);
  bool is_word_p = false;
  if (type == TYPE_ARITH && riscv_set_is_add_p (set))
    is_word_p = TARGET_64BIT && GET_MODE (SET_SRC (set)) == SImode;
  else if (type == TYPE_ARITH && riscv_set_is_addw_p (set))
    is_word_p = true;
  else if (!TARGET_64BIT
	   || type != TYPE_BITMANIP
	   || !riscv_set_is_adduw_p (set))
    return false;

  if (word_p)
    *word_p = is_word_p;
  return true;
}

/* Matches an mv or li instruction:
     (set (reg rd) (reg rs1))
   or:
     (set (reg rd) (const_int imm12)).  */

static bool
riscv_insn_is_mv_li_p (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx dest = SET_DEST (set);
  rtx src = SET_SRC (set);

  /* Pseudos are valid GPR candidates before register allocation.  */
  if (get_attr_type (insn) != TYPE_MOVE
      || get_attr_length (insn) > 4
      || !REG_P (dest)
      || (HARD_REGISTER_P (dest) && !GP_REG_P (REGNO (dest))))
    return false;

  enum attr_move_type move_type = get_attr_move_type (insn);
  if (move_type == MOVE_TYPE_MOVE
      && REG_P (src)
      && (!HARD_REGISTER_P (src)
	  || GP_REG_P (REGNO (src))))
    return true;

  if (move_type == MOVE_TYPE_CONST
      && CONST_INT_P (src)
      /* Distinguish an ADDI-based LI from other single-insn constants.  */
      && SMALL_OPERAND (INTVAL (src)))
    return true;

  return false;
}

/* Matches an addi-type instruction:
     (set (reg rd) (plus (reg rs1) (const_int imm12)))
   or:
     (set (reg rd) (lo_sum (reg rs1) symbol))
   or an accepted addiw, mv, or li form.  ALLOW_WORD_P controls whether
   ADDIW is accepted.  On success, set *SRC0 to the register source when
   requested, or NULL_RTX for an immediate LI.  */

static bool
riscv_insn_is_addi_type_p (rtx_insn *insn, bool allow_word_p = true,
			   rtx *src0 = NULL)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx src = SET_SRC (set);
  if (riscv_insn_is_mv_li_p (insn))
    {
      if (src0)
	*src0 = REG_P (src) ? src : NULL_RTX;
      return true;
    }

  if (get_attr_type (insn) != TYPE_ARITH)
    return false;

  if (GET_CODE (src) == LO_SUM
      && GET_MODE (src) == Pmode
      && REG_P (XEXP (src, 0)))
    {
      if (src0)
	*src0 = XEXP (src, 0);
      return true;
    }

  if (riscv_set_is_addi_p (set))
    {
      if (!allow_word_p && GET_MODE (src) != Pmode)
	return false;
      if (src0)
	*src0 = XEXP (src, 0);
      return true;
    }

  if (allow_word_p
      && riscv_set_is_addiw_p (set, src0))
    return true;

  return false;
}

/* Matches an add- or addi-type instruction.  */

static bool
riscv_insn_is_add_addi_p (rtx_insn *insn)
{
  return (riscv_insn_is_add_type_p (insn)
	  || riscv_insn_is_addi_type_p (insn));
}

/* Matches a sub or subw:
     (set (reg rd) (minus (reg rs1) (reg rs2)))
   or:
     (set (reg:DI rd)
	  (sign_extend:DI (minus:SI (reg:SI rs1) (reg:SI rs2))))
   or an equivalent word-sub RTL form.  */

static bool
riscv_insn_is_sub_type_p (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set
      || get_attr_type (insn) != TYPE_ARITH
      || riscv_regno (SET_DEST (set)) == INVALID_REGNUM)
    return false;

  rtx src = SET_SRC (set);
  if (GET_CODE (src) == MINUS)
    return (REG_P (XEXP (src, 0))
	    && REG_P (XEXP (src, 1)));

  return (riscv_set_extract_word_binary_p (set, MINUS, &src)
	  && REG_P (XEXP (src, 0))
	  && REG_P (XEXP (src, 1)));
}

/* Matches an andi:
     (set (reg rd) (and (reg rs1) (const_int imm12)))
   or an equivalent zero-extend RTL form.  Store the normalized operands in
   *SRC0 and *SRC1 when requested.  */

static bool
riscv_insn_is_andi_type_p (rtx_insn *insn, rtx *src0 = NULL,
			   rtx *src1 = NULL)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx src = SET_SRC (set);
  if (get_attr_type (insn) == TYPE_LOGICAL
      && GET_CODE (src) == AND
      && REG_P (XEXP (src, 0))
      && CONST_INT_P (XEXP (src, 1))
      && riscv_regno (SET_DEST (set)) != INVALID_REGNUM)
    {
      if (src0)
	*src0 = XEXP (src, 0);
      if (src1)
	*src1 = XEXP (src, 1);
      return true;
    }

  if (get_attr_move_type (insn) == MOVE_TYPE_ANDI
      && GET_CODE (src) == ZERO_EXTEND
      && riscv_regno (XEXP (src, 0)) != INVALID_REGNUM
      && riscv_regno (SET_DEST (set)) != INVALID_REGNUM)
    {
      if (src0)
	*src0 = XEXP (src, 0);
      if (src1)
	*src1 = GEN_INT (0xff);
      return true;
    }

  return false;
}

/* Matches a logical instruction and stores its normalized operands in *SRC0
   and *SRC1 when requested.  */

static bool
riscv_insn_is_logical_type_p (rtx_insn *insn,
			      rtx *src0 = NULL,
			      rtx *src1 = NULL)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  /* Handle canonical ANDI and its equivalent zero-extend RTL.  */
  if (riscv_insn_is_andi_type_p (insn, src0, src1))
    return true;

  enum attr_type type = get_attr_type (insn);
  rtx src = SET_SRC (set);
  rtx_code code = GET_CODE (src);

  if (code == AND || code == IOR || code == XOR)
    {
      /* Handle ordinary register and immediate logical operations.  */
      if (type == TYPE_LOGICAL
	  && REG_P (XEXP (src, 0))
	  && (REG_P (XEXP (src, 1))
	      || CONST_INT_P (XEXP (src, 1))))
	{
	  if (src0)
	    *src0 = XEXP (src, 0);
	  if (src1)
	    *src1 = XEXP (src, 1);
	  return true;
	}

      /* ANDN and ORN have an inner NOT on the first RTL operand.  */
      if (type != TYPE_BITMANIP || !(TARGET_ZBB || TARGET_ZBKB))
	return false;

      rtx sub = XEXP (src, 0);
      if (GET_CODE (sub) != NOT
	  || !REG_P (XEXP (src, 1))
	  || !REG_P (XEXP (sub, 0)))
	return false;
      if (src0)
	*src0 = XEXP (src, 1);
      if (src1)
	*src1 = XEXP (sub, 0);
      return true;
    }

  if (code != NOT)
    return false;

  rtx sub = XEXP (src, 0);
  /* A NOT of a register is an ordinary unary logical operation.  */
  if (REG_P (sub))
    {
      if (type != TYPE_LOGICAL)
	return false;
      if (src0)
	*src0 = sub;
      return true;
    }

  /* XNOR is represented as an outer NOT of a register-register XOR.  */
  if (SUBREG_P (sub)
      || type != TYPE_BITMANIP
      || !(TARGET_ZBB || TARGET_ZBKB)
      || GET_CODE (sub) != XOR
      || !REG_P (XEXP (sub, 0))
      || !REG_P (XEXP (sub, 1)))
    return false;

  if (src0)
    *src0 = XEXP (sub, 0);
  if (src1)
    *src1 = XEXP (sub, 1);
  return true;
}

/* Return true if SET is a non-wrapped scalar shift of CODE.  */

static bool
riscv_set_is_shift_p (rtx set, rtx_code code)
{
  rtx src = SET_SRC (set);
  machine_mode mode = GET_MODE (src);

  if (GET_CODE (src) != code || !CONST_INT_P (XEXP (src, 1)))
    return false;

  if (((TARGET_64BIT && mode == DImode) || mode == SImode)
      && riscv_regno (XEXP (src, 0)) != INVALID_REGNUM
      && REG_P (SET_DEST (set)))
    return true;

  return false;
}

/* Matches an slli:
     (set (reg rd) (ashift (reg rs1) (const_int shamt))).  */

static bool
riscv_set_is_slli_p (rtx set)
{
  return riscv_set_is_shift_p (set, ASHIFT);
}

/* Matches an srli:
     (set (reg rd) (lshiftrt (reg rs1) (const_int shamt))).  */

static bool
riscv_set_is_srli_p (rtx set)
{
  return riscv_set_is_shift_p (set, LSHIFTRT);
}

/* Matches an srai:
     (set (reg rd) (ashiftrt (reg rs1) (const_int shamt))).  */

static bool
riscv_set_is_srai_p (rtx set)
{
  return riscv_set_is_shift_p (set, ASHIFTRT);
}

/* Match a scalar shift of CODE, including the equivalent RV64 word forms
   produced for SLLIW and SRLIW.  Store whether the instruction is a word
   form in *WORD_P and its effective shift amount in *SHIFT_AMOUNT when
   requested.  */

static bool
riscv_set_is_shift_type_p (rtx set, rtx_code code, bool *word_p,
			   HOST_WIDE_INT *shift_amount = NULL)
{
  if (riscv_set_is_shift_p (set, code))
    {
      rtx src = SET_SRC (set);
      *word_p = TARGET_64BIT && GET_MODE (src) == SImode;
      if (shift_amount)
	*shift_amount = INTVAL (XEXP (src, 1));
      return true;
    }

  if (!TARGET_64BIT
      || riscv_regno (SET_DEST (set)) == INVALID_REGNUM)
    return false;

  rtx src = SET_SRC (set);
  if (code == ASHIFT
      && GET_CODE (src) == SIGN_EXTEND
      && GET_MODE (src) == DImode)
    {
      src = XEXP (src, 0);
      if (GET_CODE (src) == ASHIFT
	  && GET_MODE (src) == SImode
	  && riscv_regno (XEXP (src, 0)) != INVALID_REGNUM
	  && CONST_INT_P (XEXP (src, 1)))
	{
	  *word_p = true;
	  if (shift_amount)
	    *shift_amount = INTVAL (XEXP (src, 1)) & 0x1f;
	  return true;
	}

      if (GET_CODE (src) == AND
	  && GET_MODE (src) == SImode
	  && GET_CODE (XEXP (src, 0)) == ROTATERT
	  && riscv_regno (XEXP (XEXP (src, 0), 0)) != INVALID_REGNUM
	  && CONST_INT_P (XEXP (XEXP (src, 0), 1))
	  && CONST_INT_P (XEXP (src, 1)))
	{
	  *word_p = true;
	  if (shift_amount)
	    *shift_amount
	      = (32 - (INTVAL (XEXP (XEXP (src, 0), 1)) & 0x1f)) & 0x1f;
	  return true;
	}
    }

  if (code != LSHIFTRT)
    return false;

  rtx_code src_code = GET_CODE (src);
  if ((src_code == ZERO_EXTEND || src_code == SIGN_EXTEND)
      && GET_MODE (src) == DImode)
    {
      rtx_code extend_code = src_code;
      src = XEXP (src, 0);
      if (GET_CODE (src) == LSHIFTRT
	  && GET_MODE (src) == SImode
	  && riscv_regno (XEXP (src, 0)) != INVALID_REGNUM
	  && CONST_INT_P (XEXP (src, 1))
	  && (extend_code == SIGN_EXTEND
	      || (INTVAL (XEXP (src, 1)) & 0x1f) != 0))
	{
	  *word_p = true;
	  if (shift_amount)
	    *shift_amount = INTVAL (XEXP (src, 1)) & 0x1f;
	  return true;
	}
      return false;
    }

  if (src_code == ZERO_EXTRACT
      && GET_MODE (src) == DImode
      && riscv_regno (XEXP (src, 0)) != INVALID_REGNUM
      && CONST_INT_P (XEXP (src, 1))
      && CONST_INT_P (XEXP (src, 2))
      && INTVAL (XEXP (src, 2)) > 0
      && INTVAL (XEXP (src, 1)) + INTVAL (XEXP (src, 2)) == 32)
    {
      *word_p = true;
      if (shift_amount)
	*shift_amount = INTVAL (XEXP (src, 2));
      return true;
    }

  if (src_code == LT
      && GET_MODE (src) == DImode
      && riscv_regno (XEXP (src, 0)) != INVALID_REGNUM
      && GET_MODE (XEXP (src, 0)) == SImode
      && XEXP (src, 1) == const0_rtx)
    {
      *word_p = true;
      if (shift_amount)
	*shift_amount = 31;
      return true;
    }

  return false;
}

/* Match a left-shift/right-shift fusion pair.  ALLOW_WORD_P accepts the
   equivalent RV64 word forms and requires both instructions to have the same
   wordness.  ALLOW_ARITHMETIC_P accepts an arithmetic right shift.  */

static bool
riscv_fuse_shift_pair_p (rtx_insn *prev, rtx_insn *curr,
			 bool allow_word_p, bool allow_arithmetic_p)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set)
      || get_attr_type (prev) != TYPE_SHIFT
      || get_attr_type (curr) != TYPE_SHIFT)
    return false;

  bool prev_word_p = false;
  bool curr_word_p = false;
  bool prev_match_p
    = (allow_word_p
	? riscv_set_is_shift_type_p (prev_set, ASHIFT, &prev_word_p)
	: riscv_set_is_slli_p (prev_set));
  bool curr_match_p
    = (allow_word_p
	? riscv_set_is_shift_type_p (curr_set, LSHIFTRT, &curr_word_p)
	: riscv_set_is_srli_p (curr_set));

  if (!curr_match_p && allow_arithmetic_p)
    curr_match_p = riscv_set_is_srai_p (curr_set);

  if (!prev_match_p
      || !curr_match_p
      || prev_word_p != curr_word_p
      || !riscv_fuse_same_dest_p (prev_set, curr_set, true))
    return false;

  return true;
}

/* Check the common RTL for ZEXTW, ZEXTWS and ZEXTH fusion.  */

static bool
riscv_fuse_zext_common (rtx_insn *prev, rtx_insn *curr,
			int shl_amount, bool zextws_p)
{
  if (!TARGET_64BIT)
    return false;

  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (!riscv_fuse_same_dest_p (prev_set, curr_set, true))
    return false;

  if (riscv_set_is_slli_p (prev_set)
      && riscv_set_is_srli_p (curr_set)
      && GET_MODE (SET_SRC (prev_set)) == DImode
      && GET_MODE (SET_SRC (curr_set)) == DImode
      && INTVAL (XEXP (SET_SRC (prev_set), 1)) == shl_amount
      && (zextws_p
	  ? INTVAL (XEXP (SET_SRC (curr_set), 1)) < shl_amount
	  : INTVAL (XEXP (SET_SRC (curr_set), 1)) == shl_amount))
    return true;

  return false;
}

/* Load/store classes used by fusion checks and rule masks.  */
enum sched_fusion_type
{
  SCHED_FUSION_LD_SIGN_EXTEND = 1 << 0,
  SCHED_FUSION_LD_ZERO_EXTEND = 1 << 1,
  SCHED_FUSION_LD = 1 << 2,
  SCHED_FUSION_ST = 1 << 3
};

/* Structural information about one scalar memory access.  Keep the memory
   mode separate from the register operand's mode for extended loads, and
   classify the register file from the actual instruction.  Hardware pair
   restrictions belong in riscv_fusion_mem_rules, not in the RTL parser.  */

struct riscv_fusion_mem_info
{
  enum sched_fusion_type type;
  struct riscv_address_info addr;
  /* The register operand, or const0_rtx for a zero store.  */
  rtx reg;
  machine_mode mode;
  bool fp_p;
};

/* Extract fusion-relevant information from scalar load/store address X.  */

static bool
riscv_fuse_extract_address (rtx x, struct riscv_address_info *addr)
{
  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
      addr->type = ADDRESS_REG;
      addr->reg = x;
      addr->offset = const0_rtx;
      break;

    case PLUS:
      if (!CONST_INT_P (XEXP (x, 1)))
	return false;

      addr->type = ADDRESS_REG;
      addr->reg = XEXP (x, 0);
      addr->offset = XEXP (x, 1);
      break;

    case LO_SUM:
      addr->type = ADDRESS_LO_SUM;
      addr->reg = XEXP (x, 0);
      addr->offset = XEXP (x, 1);
      break;

    default:
      return false;
    }

  return riscv_regno (addr->reg) != INVALID_REGNUM;
}

/* Extract a scalar integer or floating-point load/store into *INFO:
     (set (reg rd) (mem addr))
   or:
     (set (reg rd) (any_extend (mem addr)))
   or:
     (set (reg frd) (mem addr))
   or:
     (set (mem addr) (reg rs1))
   or:
     (set (mem addr) (reg frs1)).
   Store sources may also be mode-specific zero constants.  Classify
   FP-mode moves by the register file used by their instructions.  */

static bool
riscv_fuse_mem_p (rtx_insn *insn, struct riscv_fusion_mem_info *info)
{
  gcc_assert (INSN_P (insn));

  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx dest = SET_DEST (set);
  rtx src = SET_SRC (set);
  info->type = SCHED_FUSION_LD;

  if (GET_CODE (src) == SIGN_EXTEND
      || GET_CODE (src) == ZERO_EXTEND)
    {
      info->type = (GET_CODE (src) == SIGN_EXTEND
		    ? SCHED_FUSION_LD_SIGN_EXTEND
		    : SCHED_FUSION_LD_ZERO_EXTEND);
      src = XEXP (src, 0);
      if (!MEM_P (src))
	return false;
    }

  /* Exclude RVV loads and stores.  */
  if (riscv_vector_mode_p (GET_MODE (dest))
      || riscv_vector_mode_p (GET_MODE (src)))
    return false;

  if (!MEM_P (src) && !MEM_P (dest))
    return false;

  rtx mem = MEM_P (src) ? src : dest;
  poly_uint64 mode_size = GET_MODE_SIZE (GET_MODE (mem));
  /* Reject wide moves that split into multiple instructions, except for
     single 64-bit FPR or Zilsd moves on RV32.  */
  if (known_gt (mode_size, 0U + UNITS_PER_WORD)
      && get_attr_length (insn) > 4
      && (!known_eq (mode_size, 8U)
	  || riscv_split_64bit_move_p (dest, src)))
    return false;

  bool isload_p = MEM_P (src);
  if (isload_p)
    {
      if (riscv_regno (dest) == INVALID_REGNUM)
	return false;
    }
  else
    {
      /* A mode-specific zero is stored through the integer zero register.  */
      if (src == CONST0_RTX (GET_MODE (mem)))
	src = const0_rtx;
      if (src != const0_rtx && riscv_regno (src) == INVALID_REGNUM)
	return false;
      info->type = SCHED_FUSION_ST;
    }

  info->mode = GET_MODE (mem);
  if (!riscv_fuse_extract_address (XEXP (mem, 0), &info->addr))
    return false;

  /* Soft-float moves may have FP load/store types but use integer
     load/store instructions.  The move type distinguishes those cases.  */
  enum attr_type type = get_attr_type (insn);
  if (type == (isload_p ? TYPE_LOAD : TYPE_STORE))
    info->fp_p = false;
  else if (type == (isload_p ? TYPE_FPLOAD : TYPE_FPSTORE))
    info->fp_p = (get_attr_move_type (insn)
		  != (isload_p ? MOVE_TYPE_LOAD : MOVE_TYPE_STORE));
  else
    return false;

  /* Retain the register operand for pair dependency checks.  */
  info->reg = isload_p ? dest : src;
  return true;
}

/* Address orders supported by a memory-pair rule.  ANY permits either order;
   it does not establish that both orders are legal for a particular pair.  */
enum riscv_fusion_direction
{
  RISCV_FUSION_NONE,
  RISCV_FUSION_INC,
  RISCV_FUSION_DEC,
  RISCV_FUSION_ANY
};

/* Hardware constraints shared by candidate selection and final matching.
   Scheduling preferences and the legality of moving accesses are separate
   from these rules.  */

struct riscv_fusion_mem_rule
{
  enum riscv_fusion_pairs op;
  /* Bit N permits an N-byte access.  */
  unsigned HOST_WIDE_INT access_sizes;
  /* Mask of sched_fusion_type values.  */
  unsigned int types;
  /* Whether the access uses the floating-point register file.  */
  bool fp_p;
  /* Whether the memory mode must be a scalar integer mode.  */
  bool scalar_int_p;
  /* Supported address order, independent of scheduling preferences.  */
  enum riscv_fusion_direction direction;
  /* Positive multiple of the access size used to align the lower offset.  */
  unsigned int alignment_factor;
};

static const struct riscv_fusion_mem_rule riscv_fusion_mem_rules[] =
{
  { RISCV_FUSE_LDST_PAIR_INC, (1U << 4) | (1U << 8),
    SCHED_FUSION_LD | SCHED_FUSION_LD_SIGN_EXTEND | SCHED_FUSION_ST,
    false, false, RISCV_FUSION_INC, 1 },
  { RISCV_FUSE_LDST_PAIR_DEC, (1U << 4) | (1U << 8),
    SCHED_FUSION_LD | SCHED_FUSION_LD_SIGN_EXTEND | SCHED_FUSION_ST,
    false, false, RISCV_FUSION_DEC, 1 },
  { RISCV_FUSE_FLDFST_PAIR_INC, (1U << 4) | (1U << 8),
    SCHED_FUSION_LD | SCHED_FUSION_LD_SIGN_EXTEND
    | SCHED_FUSION_LD_ZERO_EXTEND | SCHED_FUSION_ST,
    true, false, RISCV_FUSION_INC, 1 },
  { RISCV_FUSE_FLDFST_PAIR_DEC, (1U << 4) | (1U << 8),
    SCHED_FUSION_LD | SCHED_FUSION_LD_SIGN_EXTEND
    | SCHED_FUSION_LD_ZERO_EXTEND | SCHED_FUSION_ST,
    true, false, RISCV_FUSION_DEC, 1 },
  /* Allow 1-, 2-, 4- and 8-byte stores.  */
  { RISCV_FUSE_ALIGNED_STD, (1U << 1) | (1U << 2) | (1U << 4) | (1U << 8),
    SCHED_FUSION_ST, false, true, RISCV_FUSION_ANY, 2 }
};

/* Return true if MEM has a form, mode and register file accepted by RULE.
   A pair search may rewrite the base and offset, so leave offset alignment
   and the relationship between accesses to the final pair check.  */

static bool
riscv_fuse_mem_candidate_p (const struct riscv_fusion_mem_info *mem,
			    const struct riscv_fusion_mem_rule &rule)
{
  /* Check the memory mode independently of the register file.  */
  if (mem->addr.type != ADDRESS_REG
      || !CONST_INT_P (mem->addr.offset)
      || mem->fp_p != rule.fp_p
      || !(rule.types & mem->type)
      || (rule.scalar_int_p && !SCALAR_INT_MODE_P (mem->mode)))
    return false;

  /* Bound the shift used to test the access-size mask.  */
  unsigned int access_size = GET_MODE_SIZE (mem->mode).to_constant ();
  return (access_size < HOST_BITS_PER_WIDE_INT
	  && (rule.access_sizes & (HOST_WIDE_INT_1U << access_size)));
}

/* Extract an add-type instruction followed by an integer load or store that
   uses the add result as an undisplaced address.  */

static bool
riscv_fuse_add_mem_p (rtx_insn *prev, rtx_insn *curr,
		      rtx *add_set_out,
		      struct riscv_fusion_mem_info *mem)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  rtx add_dest = SET_DEST (prev_set);
  if (!riscv_insn_is_add_type_p (prev)
      || !riscv_fuse_mem_p (curr, mem)
      || mem->fp_p
      || mem->addr.type != ADDRESS_REG
      || !CONST_INT_P (mem->addr.offset)
      || INTVAL (mem->addr.offset) != 0
      || !riscv_fuse_same_reg_p (add_dest, mem->addr.reg))
    return false;

  if (add_set_out)
    *add_set_out = prev_set;
  return true;
}

/* Match an in-place ADDI-type update and a scalar load or store with the
   same base register.  LOAD_P selects loads rather than stores, and
   PREINDEX_P selects whether the update precedes the memory instruction.
   The memory offset is independent of the update amount.  Loads may
   overwrite the base register.  */

static bool
riscv_fuse_indexed_mem_p (rtx_insn *prev, rtx_insn *curr,
			  bool load_p, bool preindex_p)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  rtx_insn *update_insn = preindex_p ? prev : curr;
  rtx_insn *mem_insn = preindex_p ? curr : prev;
  rtx update_set = preindex_p ? prev_set : curr_set;
  rtx update_dest = SET_DEST (update_set);
  rtx update_base = NULL_RTX;
  struct riscv_fusion_mem_info mem;

  if (!riscv_insn_is_addi_type_p (update_insn, false, &update_base)
      || update_base == NULL_RTX
      || !riscv_fuse_mem_p (mem_insn, &mem)
      || (load_p
	  ? mem.type == SCHED_FUSION_ST
	  : mem.type != SCHED_FUSION_ST)
      || !(mem.addr.type == ADDRESS_REG
	   || mem.addr.type == ADDRESS_LO_SUM)
      || riscv_regno (update_dest) == INVALID_REGNUM
      || !riscv_fuse_same_reg_p (update_base, update_dest)
      || !riscv_fuse_same_reg_p (mem.addr.reg, update_dest))
    return false;

  if (load_p)
    return true;

  /* A multi-register store must not overlap the updated base.  */
  return !reg_overlap_mentioned_p (update_dest, mem.reg);
}

/* Check two parsed accesses in their proposed execution order against RULE.
   The caller selects enabled rules and checks the scheduling phase.  Pair
   formation must separately prove that moving the accesses is safe.  */

static bool
riscv_fuse_mem_pair_p (const struct riscv_fusion_mem_info &prev_mem,
		       const struct riscv_fusion_mem_info &curr_mem,
		       const struct riscv_fusion_mem_rule &rule)
{
  if (!riscv_fuse_mem_candidate_p (&prev_mem, rule)
      || !riscv_fuse_mem_candidate_p (&curr_mem, rule))
    return false;

  /* Check the relationship between the two eligible accesses.  */
  bool prev_load_p = prev_mem.type != SCHED_FUSION_ST;
  bool curr_load_p = curr_mem.type != SCHED_FUSION_ST;
  if (prev_load_p != curr_load_p
      || prev_mem.fp_p != curr_mem.fp_p
      || prev_mem.mode != curr_mem.mode
      || !riscv_fuse_same_reg_p (prev_mem.addr.reg, curr_mem.addr.reg))
    return false;

  /* Loads need disjoint results, and the first must preserve the base.
     Account for all registers written by a Zilsd load on RV32.  */
  if (prev_load_p
      && (reg_overlap_mentioned_p (prev_mem.reg, curr_mem.reg)
	  || reg_overlap_mentioned_p (prev_mem.reg, prev_mem.addr.reg)))
    return false;

  HOST_WIDE_INT prev_offset = INTVAL (prev_mem.addr.offset);
  HOST_WIDE_INT curr_offset = INTVAL (curr_mem.addr.offset);
  bool inc_p = prev_offset < curr_offset;
  if (rule.direction != RISCV_FUSION_ANY
      && inc_p != (rule.direction == RISCV_FUSION_INC))
    return false;

  /* Use the lower offset for the pair alignment check.  */
  if (!inc_p)
    std::swap (prev_offset, curr_offset);

  /* Use unsigned subtraction to avoid overflow for opposite-signed offsets.  */
  unsigned HOST_WIDE_INT diff = ((unsigned HOST_WIDE_INT) curr_offset
				- (unsigned HOST_WIDE_INT) prev_offset);
  HOST_WIDE_INT access_size = GET_MODE_SIZE (prev_mem.mode).to_constant ();
  HOST_WIDE_INT alignment = rule.alignment_factor * access_size;
  return (diff == (unsigned HOST_WIDE_INT) access_size
	  && prev_offset % alignment == 0);
}

/* Check scalar load/store pair operation OP.  Match the caller's operation,
   regardless of other enabled rules, using the common parsed-access check.  */

static bool
riscv_fuse_mem_pair_p (rtx_insn *prev, rtx_insn *curr,
		       enum riscv_fusion_pairs op)
{
  struct riscv_fusion_mem_info prev_mem, curr_mem;
  if (!riscv_fuse_mem_p (prev, &prev_mem)
      || !riscv_fuse_mem_p (curr, &curr_mem))
    return false;

  for (const riscv_fusion_mem_rule &rule : riscv_fusion_mem_rules)
    if (rule.op == op)
      return riscv_fuse_mem_pair_p (prev_mem, curr_mem, rule);
  gcc_unreachable ();
}

/* Fusion recognizers.  */

/* Check for RISCV_FUSE_ZEXTW fusion.
   prev (slli) == (set (reg:DI rd1)
		       (ashift:DI (reg:DI rs1) (const_int 32)))
   curr (srli) == (set (reg:DI rd2)
		       (lshiftrt:DI (reg:DI rd1) (const_int 32)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_zextw (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_zext_common (prev, curr, 32, false);
}

/* Check for RISCV_FUSE_ZEXTWS fusion.
   prev (slli) == (set (reg:DI rd1)
		       (ashift:DI (reg:DI rs1) (const_int 32)))
   curr (srli) == (set (reg:DI rd2)
		       (lshiftrt:DI (reg:DI rd1) (const_int imm5)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_zextws (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_zext_common (prev, curr, 32, true);
}

/* Check for RISCV_FUSE_ZEXTH fusion.
   prev (slli) == (set (reg:DI rd1)
		       (ashift:DI (reg:DI rs1) (const_int 48)))
   curr (srli) == (set (reg:DI rd2)
		       (lshiftrt:DI (reg:DI rd1) (const_int 48)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_zexth (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_zext_common (prev, curr, 48, false);
}

/* Check for RISCV_FUSE_LDINDEXED fusion.
   prev (one of the following):
     (add) == (set (reg rd1) (plus (reg rs1) (reg rs2)))
     (addw) == (set (reg rd1) (sign_extend (plus:SI (reg rs1)
						    (reg rs2))))
     (add.uw) == (set (reg rd1) (plus (zero_extend (reg rs1))
				      (reg rs2)))
   curr (one of the following):
     (load) == (set (reg rd2) (mem (reg rd1)))
     (load) == (set (reg rd2)
		    (any_extend (mem (reg rd1))))

   Constraints:
     offset == 0.  */

static bool
riscv_fuse_ldindexed (rtx_insn *prev, rtx_insn *curr)
{
  struct riscv_fusion_mem_info mem;

  return (riscv_fuse_add_mem_p (prev, curr, NULL, &mem)
	  && mem.type != SCHED_FUSION_ST);
}

/* Check for RISCV_FUSE_ADD_ST fusion.
   prev (one of the following):
     (add) == (set (reg rd1) (plus (reg rs1) (reg rs2)))
     (addw) == (set (reg rd1) (sign_extend (plus:SI (reg rs1)
						    (reg rs2))))
     (add.uw) == (set (reg rd1) (plus (zero_extend (reg rs1))
				      (reg rs2)))
   curr (one of the following):
     (store) == (set (mem (rd1, offset)) (reg rs3))
     (store) == (set (mem (rd1, offset)) (const_int 0))

   Constraints:
     rd1 != rs3 for a register-source store
     offset == 0.  */

static bool
riscv_fuse_add_st (rtx_insn *prev, rtx_insn *curr)
{
  rtx add_set;
  struct riscv_fusion_mem_info mem;

  return (riscv_fuse_add_mem_p (prev, curr, &add_set, &mem)
	  && mem.type == SCHED_FUSION_ST
	  && !reg_overlap_mentioned_p (SET_DEST (add_set), mem.reg));
}

/* Check for RISCV_FUSE_EXPANDED_LD fusion.
   prev (one of the following):
     (add) == (set (reg rd1) (plus (reg rs1) (reg rs2)))
     (addi) == (set (reg rd1) (plus (reg rs1) (const_int imm12)))
     (shNadd) == (set (reg rd1) (plus (ashift (reg rs1) (const_int N))
				      (reg rs2)))
     (add.uw) == (set (reg rd1) (plus (zero_extend (reg rs1))
				      (reg rs2)))
     (shNadd.uw) == (set (reg rd1)
			 (plus (and (ashift (reg rs1)
					    (const_int N))
				    (const_int mask))
			       (reg rs2)))
   curr (one of the following):
     (load) == (set (reg rd2) (mem (rd1, offset)))
     (load) == (set (reg rd2) (any_extend (mem (rd1, offset))))

   Constraints:
     rd1 == rd2
     add pairs only with a displaced non-extended load
     addi and shNadd pair only with non-extended loads
     add.uw and shNadd.uw pair only with extended loads
     N is 1, 2, or 3 for shNadd and shNadd.uw
     mask >> N == 0xffffffff for shNadd.uw.  */

static bool
riscv_fuse_expanded_ld (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (!riscv_fuse_same_dest_p (prev_set, curr_set))
    return false;

  struct riscv_fusion_mem_info mem;
  if (!riscv_fuse_mem_p (curr, &mem)
      || mem.type == SCHED_FUSION_ST
      || mem.fp_p
      || mem.addr.type != ADDRESS_REG
      || !riscv_fuse_same_reg_p (mem.addr.reg, SET_DEST (prev_set)))
    return false;

  bool displaced_p = INTVAL (mem.addr.offset) != 0;
  if (mem.type == SCHED_FUSION_LD)
    return ((displaced_p && riscv_set_is_add_p (prev_set))
	    || riscv_set_is_addi_p (prev_set)
	    || riscv_set_is_shNadd_p (prev_set));

  return (riscv_set_is_adduw_p (prev_set)
	  || riscv_set_is_shNadduw_p (prev_set));
}

/* Check for RISCV_FUSE_LDPREINCREMENT fusion.
   prev (one of the following):
     (addi) == (set (reg rd1) (plus (reg rd1) (const_int imm12)))
     (self-mv) == (set (reg rd1) (reg rd1))
     (addi) == (set (reg rd1) (lo_sum (reg rd1) symbol1))
   curr (one of the following):
     (load) == (set (reg rd2) (mem addr))
     (load) == (set (reg rd2) (any_extend (mem addr)))
     (fpload) == (set (reg frd) (mem addr))
   addr (one of the following):
     (rd1, offset)
     (lo_sum (reg rd1) symbol2)

   Constraints:
     the ADDI-type instruction is not a word form.  */

static bool
riscv_fuse_ldpreincrement (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_indexed_mem_p (prev, curr, true, true);
}

/* Check for RISCV_FUSE_PREINDEX_ST fusion.
   prev (one of the following):
     (addi) == (set (reg rd1) (plus (reg rd1) (const_int imm12)))
     (self-mv) == (set (reg rd1) (reg rd1))
     (addi) == (set (reg rd1) (lo_sum (reg rd1) symbol1))
   curr (one of the following):
     (store) == (set (mem addr) (reg rs1))
     (store) == (set (mem addr) (const_int 0))
     (fpstore) == (set (mem addr) (reg frs1))
   addr (one of the following):
     (rd1, offset)
     (lo_sum (reg rd1) symbol2)

   Constraints:
     the ADDI-type instruction is not a word form
     for a register-source integer store, rd1 != rs1.  */

static bool
riscv_fuse_preindex_st (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_indexed_mem_p (prev, curr, false, true);
}

/* Check for RISCV_FUSE_POSTINDEX_LD fusion.
   prev (one of the following):
     (load) == (set (reg rd1) (mem addr))
     (load) == (set (reg rd1) (any_extend (mem addr)))
     (fpload) == (set (reg frd) (mem addr))
   addr (one of the following):
     (rd2, offset)
     (lo_sum (reg rd2) symbol1)
   curr (one of the following):
     (addi) == (set (reg rd2) (plus (reg rd2) (const_int imm12)))
     (self-mv) == (set (reg rd2) (reg rd2))
     (addi) == (set (reg rd2) (lo_sum (reg rd2) symbol2))

   Constraints:
     the ADDI-type instruction is not a word form.  */

static bool
riscv_fuse_postindex_ld (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_indexed_mem_p (prev, curr, true, false);
}

/* Check for RISCV_FUSE_POSTINDEX_ST fusion.
   prev (one of the following):
     (store) == (set (mem addr) (reg rs1))
     (store) == (set (mem addr) (const_int 0))
     (fpstore) == (set (mem addr) (reg frs1))
   addr (one of the following):
     (rd1, offset)
     (lo_sum (reg rd1) symbol1)
   curr (one of the following):
     (addi) == (set (reg rd1) (plus (reg rd1) (const_int imm12)))
     (self-mv) == (set (reg rd1) (reg rd1))
     (addi) == (set (reg rd1) (lo_sum (reg rd1) symbol2))

   Constraints:
     the ADDI-type instruction is not a word form
     for a register-source integer store, rd1 != rs1.  */

static bool
riscv_fuse_postindex_st (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_indexed_mem_p (prev, curr, false, false);
}

/* Check for RISCV_FUSE_LUI_ADDI fusion.
   prev (one of the following):
     (lui) == (set (reg rd1) (const_int imm20))
     (lui) == (set (reg rd1) (high symbol1))
   curr (one of the following):
     (addi/addiw) == (set (reg rd2)
			  (plus (reg rd1) (const_int imm12)))
     (addi) == (set (reg rd2)
		    (lo_sum (reg rd1) symbol2))
     (self-mv) == (set (reg rd2) (reg rd1))

   Constraints:
     rd1 == rd2
     imm20 != 0 for the constant form.  */

static bool
riscv_fuse_lui_addi (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (!riscv_fuse_same_dest_p (prev_set, curr_set, true))
    return false;

  if (riscv_insn_is_addi_type_p (curr)
      && (GET_CODE (SET_SRC (prev_set)) == HIGH
	  || (CONST_INT_P (SET_SRC (prev_set))
	      && (SET_SRC (prev_set)
		  != CONST0_RTX (GET_MODE (SET_DEST (prev_set))))
	      && LUI_OPERAND (INTVAL (SET_SRC (prev_set))))))
    return true;

  return false;
}

/* Check for RISCV_FUSE_AUIPC_ADDI fusion.
   prev (auipc) == (set (reg rd1) (unspec UNSPEC_AUIPC))
   curr (one of the following):
     (addi) == (set (reg rd2)
		    (plus (reg rd1) (const_int imm12)))
     (addi) == (set (reg rd2)
		    (lo_sum (reg rd1) symbol))
     (self-mv) == (set (reg rd2) (reg rd1))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_auipc_addi (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (!riscv_fuse_same_dest_p (prev_set, curr_set, true))
    return false;

  if (riscv_insn_is_addi_type_p (curr, false)
      && GET_CODE (SET_SRC (prev_set)) == UNSPEC
      && XINT (SET_SRC (prev_set), 1) == UNSPEC_AUIPC)
    return true;

  return false;
}

/* Check for RISCV_FUSE_LUI_LD fusion.
   prev (one of the following):
     (lui) == (set (reg rd1) (const_int imm20))
     (lui) == (set (reg rd1) (high symbol1))
   curr (one of the following):
     (load) == (set (reg rd2) (mem (rd1, offset)))
     (load) == (set (reg rd2) (mem (lo_sum (reg rd1) symbol2)))
     (load) == (set (reg rd2)
		    (any_extend (mem (lo_sum (reg rd1) symbol2))))

   Constraints:
     rd1 == rd2
     the constant form pairs only with a base-plus-offset load
     the symbolic form pairs only with a lo_sum load
     imm20 != 0 for the constant form.  */

static bool
riscv_fuse_lui_ld (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (!riscv_fuse_same_dest_p (prev_set, curr_set))
    return false;

  struct riscv_fusion_mem_info mem;
  if (!riscv_fuse_mem_p (curr, &mem)
      || mem.type == SCHED_FUSION_ST
      || mem.fp_p
      || !riscv_fuse_same_reg_p (mem.addr.reg, SET_DEST (prev_set)))
    return false;

  /* A LUI_OPERAND accepts (const_int 0), but we won't emit that as LUI.
     Reject that case explicitly.  */
  if (CONST_INT_P (SET_SRC (prev_set))
      && SET_SRC (prev_set) != CONST0_RTX (GET_MODE (SET_DEST (prev_set)))
      && LUI_OPERAND (INTVAL (SET_SRC (prev_set)))
      && mem.type == SCHED_FUSION_LD
      && mem.addr.type == ADDRESS_REG
      && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == PLUS)
    return true;

  if (GET_CODE (SET_SRC (prev_set)) == HIGH
      && mem.addr.type == ADDRESS_LO_SUM)
    return true;

  return false;
}

/* Check for RISCV_FUSE_AUIPC_LD fusion.
   prev (auipc) == (set (reg rd1) (unspec UNSPEC_AUIPC))
   curr (load)  == (set (reg rd2) (mem (rd1, offset)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_auipc_ld (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (!riscv_fuse_same_dest_p (prev_set, curr_set))
    return false;

  struct riscv_fusion_mem_info mem;

  if (GET_CODE (SET_SRC (prev_set)) == UNSPEC
      && XINT (SET_SRC (prev_set), 1) == UNSPEC_AUIPC
      && riscv_fuse_mem_p (curr, &mem)
      && mem.type == SCHED_FUSION_LD
      && !mem.fp_p
      && mem.addr.type == ADDRESS_REG
      && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == PLUS
      && riscv_fuse_same_reg_p (mem.addr.reg, SET_DEST (prev_set)))
    return true;

  return false;
}

/* Check for RISCV_FUSE_ALIGNED_STD fusion.
   prev (store) == (set (mem (rs1, offset1)) (reg rs2))
   curr (store) == (set (mem (rs1, offset2)) (reg rs3))

   Constraints:
     access size is 1, 2, 4 or 8 bytes
     both stores use the same scalar integer mode
     min (offset1, offset2) is aligned to twice the access size
     abs (offset1 - offset2) equals the access size.  */

static bool
riscv_fuse_aligned_std (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_mem_pair_p (prev, curr, RISCV_FUSE_ALIGNED_STD);
}

/* Check for RISCV_FUSE_LDST_PAIR_INC fusion.
   prev/curr (one of the following pairs):
     prev (lw/ld) == (set (reg rd1) (mem (rs1, offset1)))
     curr (lw/ld) == (set (reg rd2) (mem (rs1, offset2)))

     prev (sw/sd) == (set (mem (rs1, offset1)) (reg rs2))
     curr (sw/sd) == (set (mem (rs1, offset2)) (reg rs3))

   Constraints:
     access size is 4 or 8 bytes
     offset2 - offset1 equals the access size
     offsets are aligned to the access size
     loads are not zero-extending
     for loads, rd1 != rd2 and rd1 != rs1.  */

static bool
riscv_fuse_ldst_pair_inc (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_mem_pair_p (prev, curr, RISCV_FUSE_LDST_PAIR_INC);
}

/* Check for RISCV_FUSE_LDST_PAIR_DEC fusion.
   prev/curr (one of the following pairs):
     prev (lw/ld) == (set (reg rd1) (mem (rs1, offset1)))
     curr (lw/ld) == (set (reg rd2) (mem (rs1, offset2)))

     prev (sw/sd) == (set (mem (rs1, offset1)) (reg rs2))
     curr (sw/sd) == (set (mem (rs1, offset2)) (reg rs3))

   Constraints:
     access size is 4 or 8 bytes
     offset1 - offset2 equals the access size
     offsets are aligned to the access size
     loads are not zero-extending
     for loads, rd1 != rd2 and rd1 != rs1.  */

static bool
riscv_fuse_ldst_pair_dec (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_mem_pair_p (prev, curr, RISCV_FUSE_LDST_PAIR_DEC);
}

/* Check for RISCV_FUSE_FLDFST_PAIR_INC fusion.
   prev/curr (one of the following pairs):
     prev (flw/fld) == (set (reg frd1) (mem (rs1, offset1)))
     curr (flw/fld) == (set (reg frd2) (mem (rs1, offset2)))

     prev (fsw/fsd) == (set (mem (rs1, offset1)) (reg frs1))
     curr (fsw/fsd) == (set (mem (rs1, offset2)) (reg frs2))

   Constraints:
     access size is 4 or 8 bytes
     offset2 - offset1 equals the access size
     offsets are aligned to the access size
     for loads, frd1 != frd2.  */

static bool
riscv_fuse_fldfst_pair_inc (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_mem_pair_p (prev, curr, RISCV_FUSE_FLDFST_PAIR_INC);
}

/* Check for RISCV_FUSE_FLDFST_PAIR_DEC fusion.
   prev/curr (one of the following pairs):
     prev (flw/fld) == (set (reg frd1) (mem (rs1, offset1)))
     curr (flw/fld) == (set (reg frd2) (mem (rs1, offset2)))

     prev (fsw/fsd) == (set (mem (rs1, offset1)) (reg frs1))
     curr (fsw/fsd) == (set (mem (rs1, offset2)) (reg frs2))

   Constraints:
     access size is 4 or 8 bytes
     offset1 - offset2 equals the access size
     offsets are aligned to the access size
     for loads, frd1 != frd2.  */

static bool
riscv_fuse_fldfst_pair_dec (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_mem_pair_p (prev, curr, RISCV_FUSE_FLDFST_PAIR_DEC);
}

/* Check for RISCV_FUSE_BFEXT fusion.
   prev (slli) == (set (reg rd1)
		       (ashift (reg rs1) (const_int shamt1)))
   curr (one of the following):
     (srli) == (set (reg rd2)
		    (lshiftrt (reg rd1)
			      (const_int shamt2)))
     (srai) == (set (reg rd2)
		    (ashiftrt (reg rd1)
			      (const_int shamt2)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_bfext (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_shift_pair_p (prev, curr, false, true);
}

/* Check for RISCV_FUSE_SLLI_SRLI fusion.
   prev (slli/slliw) == (set (reg rd1) (ashift (reg rs1)
					      (const_int shamt1)))
   curr (srli/srliw) == (set (reg rd2) (lshiftrt (reg rd1)
						(const_int shamt2)))

   Constraints:
     rd1 == rd2
     both instructions are word forms or both are non-word forms.  */

static bool
riscv_fuse_slli_srli (rtx_insn *prev, rtx_insn *curr)
{
  return riscv_fuse_shift_pair_p (prev, curr, true, false);
}

/* Check for RISCV_FUSE_SRLI_ADD fusion.
   prev (srli/srliw) == (set (reg rd1) (lshiftrt (reg rs1)
						 (const_int shamt)))
   curr (one of the following):
     (add) == (set (reg rd2) (plus (reg rd1) (reg rs2)))
     (addw) == (set (reg rd2)
		  (sign_extend (plus (reg rd1) (reg rs2))))
     (add.uw) == (set (reg rd2) (plus (zero_extend (reg rd1))
				   (reg rs2)))

   Constraints:
     rd1 == rd2
     both instructions are word forms or both are non-word forms
     the effective right shift amount is in the range 0 to 3.

   SH1ADD, SH2ADD and SH3ADD instead add a value shifted left by 1, 2 or 3
   bits, scaling an index for 2-, 4- or 8-byte elements.  They correspond
   to SLLI + ADD, not this SRLI + ADD fusion.  */

static bool
riscv_fuse_srli_add (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set)
      || get_attr_type (prev) != TYPE_SHIFT)
    return false;

  bool shift_word_p, add_word_p;
  HOST_WIDE_INT shift_amount;
  if (!riscv_set_is_shift_type_p (prev_set, LSHIFTRT, &shift_word_p,
				  &shift_amount)
      || !riscv_insn_is_add_type_p (curr, &add_word_p)
      || shift_word_p != add_word_p
      || !riscv_fuse_same_dest_p (prev_set, curr_set, true))
    return false;

  unsigned int mask = shift_word_p ? 0x1f : (TARGET_64BIT ? 0x3f : 0x1f);
  return (shift_amount & mask) <= 3;
}

/* Check for RISCV_FUSE_B_ALUI fusion.
   prev/curr (one of the following pairs):
     prev (orc.b) == (set (reg rd1)
			  (unspec (reg rs1) UNSPEC_ORC_B))
     curr (not)   == (set (reg rd2) (not (reg rd1)))

     prev (ctz)  == (set (reg rd1) (ctz (reg rs1)))
     curr (andi) == (set (reg rd2)
			 (and (reg rd1) (const_int 63)))

     prev (sub)  == (set (reg rd1)
			 (minus (const_int 0) (reg rs1)))
     curr (smax) == (set (reg rd2)
		       (smax (reg rd1) (reg rs1)))

     prev (neg)  == (set (reg rd1) (neg (reg rs1)))
     curr (smax) == (set (reg rd2)
			 (smax (reg rd1) (reg rs1)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_b_alui (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (!riscv_fuse_same_dest_p (prev_set, curr_set, true))
    return false;

  /* orc.b + not.  */
  if (GET_CODE (SET_SRC (prev_set)) == UNSPEC
      && GET_CODE (SET_SRC (curr_set)) == NOT
      && XINT (SET_SRC (prev_set), 1) == UNSPEC_ORC_B)
    return true;

  /* ctz + andi.  */
  rtx andi_src1;
  if (GET_CODE (SET_SRC (prev_set)) == CTZ
      && riscv_insn_is_andi_type_p (curr, NULL, &andi_src1)
      && CONST_INT_P (andi_src1)
      && INTVAL (andi_src1) == 63)
    return true;

  /* sub + smax (abs pattern).  */
  if (GET_CODE (SET_SRC (prev_set)) == MINUS
      && (XEXP (SET_SRC (prev_set), 0)
	  == CONST0_RTX (GET_MODE (SET_SRC (prev_set))))
      && GET_CODE (SET_SRC (curr_set)) == SMAX
      && riscv_fuse_same_reg_p (XEXP (SET_SRC (prev_set), 1),
				XEXP (SET_SRC (curr_set), 1)))
    return true;

  /* neg + smax (abs pattern).  */
  if (GET_CODE (SET_SRC (prev_set)) == NEG
      && GET_CODE (SET_SRC (curr_set)) == SMAX
      && riscv_fuse_same_reg_p (XEXP (SET_SRC (prev_set), 0),
				XEXP (SET_SRC (curr_set), 1)))
    return true;

  return false;
}

/* Check for RISCV_FUSE_SUB_SEQZ fusion.
   prev (one of the following):
     (sub) == (set (reg rd1) (minus (reg rs1) (reg rs2)))
     (subw) == (set (reg rd1)
		    (sign_extend (minus:SI (reg:SI rs1) (reg:SI rs2))))
   curr (one of the following):
     (seqz) == (set (reg rd2) (eq (reg rd1) (const_int 0)))
     (snez) == (set (reg rd2) (ne (reg rd1) (const_int 0)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_sub_seqz (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  rtx curr_src = SET_SRC (curr_set);
  rtx_code curr_code = GET_CODE (curr_src);

  if (riscv_insn_is_sub_type_p (prev)
      && get_attr_type (curr) == TYPE_SLT
      && (curr_code == EQ || curr_code == NE)
      && riscv_fuse_same_dest_p (prev_set, curr_set, true)
      && XEXP (curr_src, 1) == const0_rtx)
    return true;

  return false;
}

/* Check for RISCV_FUSE_ADD_ANDI fusion.
   prev (one of the following):
     (add) == (set (reg rd1) (plus (reg rs1) (reg rs2)))
     (addi) == (set (reg rd1) (plus (reg rs1) (const_int imm12_1)))
     (addw) == (set (reg rd1)
		    (sign_extend (plus (reg rs1) (reg rs2))))
     (addiw) == (set (reg rd1)
		     (sign_extend (plus (reg rs1) (const_int imm12_1))))
     (add.uw) == (set (reg rd1) (plus (zero_extend (reg rs1))
				      (reg rs2)))
     (mv) == (set (reg rd1) (reg rs1))
     (li) == (set (reg rd1) (const_int imm12_1))
     (addi) == (set (reg rd1) (lo_sum (reg rs1) symbol))
   curr (one of the following):
     (andi) == (set (reg rd2) (and (reg rd1) (const_int imm12_2)))
     (andi) == (set (reg rd2) (zero_extend (reg rd1)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_add_andi (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (riscv_insn_is_add_addi_p (prev)
      && riscv_insn_is_andi_type_p (curr)
      && riscv_fuse_same_dest_p (prev_set, curr_set, true))
    return true;

  return false;
}

/* Check for RISCV_FUSE_ANDI_ADD fusion.
   prev (one of the following):
     (andi) == (set (reg rd1) (and (reg rs1) (const_int imm12_1)))
     (andi) == (set (reg rd1) (zero_extend (reg rs1)))
   curr (one of the following):
     (add) == (set (reg rd2) (plus (reg rd1) (reg rs2)))
     (addi) == (set (reg rd2) (plus (reg rd1) (const_int imm12_2)))
     (addw) == (set (reg rd2)
		    (sign_extend (plus (reg rd1) (reg rs2))))
     (addiw) == (set (reg rd2)
		     (sign_extend (plus (reg rd1) (const_int imm12_2))))
     (add.uw) == (set (reg rd2) (plus (zero_extend (reg rd1))
				      (reg rs2)))
     (mv) == (set (reg rd2) (reg rd1))
     (addi) == (set (reg rd2) (lo_sum (reg rd1) symbol))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_andi_add (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  if (riscv_insn_is_andi_type_p (prev)
      && riscv_insn_is_add_addi_p (curr)
      && riscv_fuse_same_dest_p (prev_set, curr_set, true))
    return true;

  return false;
}

/* Check for RISCV_FUSE_LOGIC_LOGIC fusion.
   prev (one of the following):
     (logic) == (set (reg rd1) (op1 (reg rs1) (reg rs2)))
     (logic) == (set (reg rd1) (op1 (reg rs1) (const_int imm12_1)))
     (logic) == (set (reg rd1) (op1 (not (reg rs2)) (reg rs1)))
     (xnor) == (set (reg rd1) (not (xor (reg rs1) (reg rs2))))
     (not) == (set (reg rd1) (not (reg rs1)))
     (andi) == (set (reg rd1) (zero_extend (reg rs1)))
   curr (one of the following):
     (logic) == (set (reg rd2) (op2 (reg rd1) (reg rs3)))
     (logic) == (set (reg rd2) (op2 (reg rd1) (const_int imm12_2)))
     (logic) == (set (reg rd2) (op2 (not (reg rs3)) (reg rd1)))
     (xnor) == (set (reg rd2) (not (xor (reg rd1) (reg rs3))))
     (not) == (set (reg rd2) (not (reg rd1)))
     (andi) == (set (reg rd2) (zero_extend (reg rd1)))

   Constraints:
     rd1 == rd2
     rd1 != rs3 when curr uses register source rs3
     rs2 and rs3 cannot both be register sources
     op1 and op2, when present, are and, ior, or xor
     andn/orn/xnor require ZBB or ZBKB.  */

static bool
riscv_fuse_logic_logic (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set, curr_set;
  if (!riscv_fuse_sets_p (prev, curr, &prev_set, &curr_set))
    return false;

  rtx prev_dest = SET_DEST (prev_set);

  rtx prev_src1 = NULL_RTX;
  rtx curr_src0 = NULL_RTX, curr_src1 = NULL_RTX;
  if (!riscv_insn_is_logical_type_p (prev, NULL, &prev_src1)
      || !riscv_insn_is_logical_type_p (curr, &curr_src0,
					&curr_src1))
    return false;

  if (riscv_fuse_same_dest_p (prev_set, curr_set)
      && riscv_fuse_same_reg_p (prev_dest, curr_src0)
      && ((curr_src1 == NULL_RTX)
	  || riscv_regno (curr_src1) == INVALID_REGNUM
	  || !riscv_fuse_same_reg_p (prev_dest, curr_src1))
      && !(prev_src1 != NULL_RTX
	   && curr_src1 != NULL_RTX
	   && riscv_regno (prev_src1) != INVALID_REGNUM
	   && riscv_regno (curr_src1) != INVALID_REGNUM))
    return true;

  return false;
}

/* Type for a fusion checker function.  Takes the two candidate insns
   and returns true if they should be fused.  */

typedef bool (*fusion_checker_fn) (rtx_insn *, rtx_insn *);

/* Descriptor for a single fusion rule.  */

struct riscv_fusion_entry
{
  /* The fusion operation to check enablement.  */
  enum riscv_fusion_pairs op;

  /* The checker function.  */
  fusion_checker_fn checker;

  /* The fusion type name used in dump output.  */
  const char *fusion_type;
};

/* Table of all fusion rules.  */

static const struct riscv_fusion_entry riscv_fusion_table[] =
{
  { RISCV_FUSE_ZEXTW,
    riscv_fuse_zextw, "RISCV_FUSE_ZEXTW" },
  { RISCV_FUSE_ZEXTWS,
    riscv_fuse_zextws, "RISCV_FUSE_ZEXTWS" },
  { RISCV_FUSE_ZEXTH,
    riscv_fuse_zexth, "RISCV_FUSE_ZEXTH" },
  { RISCV_FUSE_LDINDEXED,
    riscv_fuse_ldindexed, "RISCV_FUSE_LDINDEXED" },
  { RISCV_FUSE_ADD_ST,
    riscv_fuse_add_st, "RISCV_FUSE_ADD_ST" },
  { RISCV_FUSE_EXPANDED_LD,
    riscv_fuse_expanded_ld, "RISCV_FUSE_EXPANDED_LD" },
  { RISCV_FUSE_LDPREINCREMENT,
    riscv_fuse_ldpreincrement, "RISCV_FUSE_LDPREINCREMENT" },
  { RISCV_FUSE_PREINDEX_ST,
    riscv_fuse_preindex_st, "RISCV_FUSE_PREINDEX_ST" },
  { RISCV_FUSE_POSTINDEX_LD,
    riscv_fuse_postindex_ld, "RISCV_FUSE_POSTINDEX_LD" },
  { RISCV_FUSE_POSTINDEX_ST,
    riscv_fuse_postindex_st, "RISCV_FUSE_POSTINDEX_ST" },
  { RISCV_FUSE_LUI_ADDI,
    riscv_fuse_lui_addi, "RISCV_FUSE_LUI_ADDI" },
  { RISCV_FUSE_AUIPC_ADDI,
    riscv_fuse_auipc_addi, "RISCV_FUSE_AUIPC_ADDI" },
  { RISCV_FUSE_LUI_LD,
    riscv_fuse_lui_ld, "RISCV_FUSE_LUI_LD" },
  { RISCV_FUSE_AUIPC_LD,
    riscv_fuse_auipc_ld, "RISCV_FUSE_AUIPC_LD" },
  { RISCV_FUSE_ALIGNED_STD,
    riscv_fuse_aligned_std, "RISCV_FUSE_ALIGNED_STD" },
  { RISCV_FUSE_LDST_PAIR_INC,
    riscv_fuse_ldst_pair_inc, "RISCV_FUSE_LDST_PAIR_INC" },
  { RISCV_FUSE_LDST_PAIR_DEC,
    riscv_fuse_ldst_pair_dec, "RISCV_FUSE_LDST_PAIR_DEC" },
  { RISCV_FUSE_FLDFST_PAIR_INC,
    riscv_fuse_fldfst_pair_inc, "RISCV_FUSE_FLDFST_PAIR_INC" },
  { RISCV_FUSE_FLDFST_PAIR_DEC,
    riscv_fuse_fldfst_pair_dec, "RISCV_FUSE_FLDFST_PAIR_DEC" },
  { RISCV_FUSE_BFEXT,
    riscv_fuse_bfext, "RISCV_FUSE_BFEXT" },
  { RISCV_FUSE_SLLI_SRLI,
    riscv_fuse_slli_srli, "RISCV_FUSE_SLLI_SRLI" },
  { RISCV_FUSE_SRLI_ADD,
    riscv_fuse_srli_add, "RISCV_FUSE_SRLI_ADD" },
  { RISCV_FUSE_B_ALUI,
    riscv_fuse_b_alui, "RISCV_FUSE_B_ALUI" },
  { RISCV_FUSE_SUB_SEQZ,
    riscv_fuse_sub_seqz, "RISCV_FUSE_SUB_SEQZ" },
  { RISCV_FUSE_ADD_ANDI,
    riscv_fuse_add_andi, "RISCV_FUSE_ADD_ANDI" },
  { RISCV_FUSE_ANDI_ADD,
    riscv_fuse_andi_add, "RISCV_FUSE_ANDI_ADD" },
  { RISCV_FUSE_LOGIC_LOGIC,
    riscv_fuse_logic_logic, "RISCV_FUSE_LOGIC_LOGIC" },
};

/* Return the name of fusion operation OP.  */

static const char *
riscv_fusion_type_name (enum riscv_fusion_pairs op)
{
  for (size_t i = 0; i < ARRAY_SIZE (riscv_fusion_table); i++)
    if (riscv_fusion_table[i].op == op)
      return riscv_fusion_table[i].fusion_type;

  gcc_unreachable ();
}

/* Return the enabled fusion operation matched by PREV and CURR, or
   RISCV_FUSE_NOTHING if the instructions do not form a fusion pair.  */

enum riscv_fusion_pairs
riscv_get_fusion_pair_type (rtx_insn *prev, rtx_insn *curr)
{
  if (!riscv_macro_fusion_p ())
    return RISCV_FUSE_NOTHING;

  for (size_t i = 0; i < ARRAY_SIZE (riscv_fusion_table); i++)
    {
      const struct riscv_fusion_entry *entry = &riscv_fusion_table[i];

      if (!riscv_fusion_enabled_p (entry->op))
	continue;

      if (entry->checker (prev, curr))
	return entry->op;
    }

  return RISCV_FUSE_NOTHING;
}

/* Implement TARGET_SCHED_MACRO_FUSION_PAIR_P.  Return true if PREV and CURR
   should be kept together during scheduling.  */

bool
riscv_macro_fusion_pair_p (rtx_insn *prev, rtx_insn *curr)
{
  /* Do not extend an existing fusion group.  */
  if (SCHED_GROUP_P (prev))
    return false;

  enum riscv_fusion_pairs op = riscv_get_fusion_pair_type (prev, curr);
  if (op == RISCV_FUSE_NOTHING)
    return false;

  if (dump_file)
    fprintf (dump_file, ";; macro fusion: insn %d + insn %d -> %s\n",
	     INSN_UID (prev), INSN_UID (curr), riscv_fusion_type_name (op));
  return true;
}

/* Return a scheduling direction for MEM under the current tune.  Sorting
   existing accesses requires their current offsets to be size-aligned.
   This candidate query does not prove that a fusible partner exists.  */

static enum riscv_fusion_direction
riscv_fuse_mem_direction (const struct riscv_fusion_mem_info *mem)
{
  unsigned HOST_WIDE_INT fusible_ops = riscv_get_fusible_ops ();
  bool inc_p = false, dec_p = false, any_p = false;
  /* Collect directions from all enabled rules that accept this access.  */
  for (const riscv_fusion_mem_rule &rule : riscv_fusion_mem_rules)
    {
      if (!(fusible_ops & rule.op)
	  || !riscv_fuse_mem_candidate_p (mem, rule))
	continue;

      HOST_WIDE_INT access_size = GET_MODE_SIZE (mem->mode).to_constant ();
      if (INTVAL (mem->addr.offset) % access_size != 0)
	continue;

      inc_p |= rule.direction == RISCV_FUSION_INC;
      dec_p |= rule.direction == RISCV_FUSION_DEC;
      any_p |= rule.direction == RISCV_FUSION_ANY;
    }

  /* Prefer directional rules over bidirectional rules to preserve their
     scheduling order when both are eligible.  */
  if (inc_p && dec_p)
    return RISCV_FUSION_ANY;
  if (inc_p)
    return RISCV_FUSION_INC;
  if (dec_p)
    return RISCV_FUSION_DEC;
  return any_p ? RISCV_FUSION_ANY : RISCV_FUSION_NONE;
}

/* Implement TARGET_SCHED_FUSION_PRIORITY.  Group load/store pair candidates
   by register file, access kind, mode and base register, then by offset.  */

void
riscv_sched_fusion_priority (rtx_insn *insn, int max_pri,
			     int *fusion_pri, int *pri)
{
  struct riscv_fusion_mem_info mem;
  unsigned int base_regno;
  bool isload_p, fp_p, inc_p;
  int fusion_type, tmp;

  gcc_assert (INSN_P (insn));

  /* Keep default priorities unless this instruction is a pair candidate.  */
  tmp = max_pri - 1;
  *fusion_pri = tmp;
  *pri = tmp;

  if (!riscv_fuse_mem_p (insn, &mem))
    return;

  enum riscv_fusion_direction direction
    = riscv_fuse_mem_direction (&mem);
  if (direction == RISCV_FUSION_NONE)
    return;

  base_regno = riscv_regno (mem.addr.reg);
  if (base_regno >= FIRST_PSEUDO_REGISTER)
    return;

  isload_p = mem.type != SCHED_FUSION_ST;
  fp_p = mem.fp_p;
  /* Prefer increasing offsets when either direction is possible.  */
  inc_p = direction != RISCV_FUSION_DEC;

  /* Give each load/store class and base register a distinct priority below
     that of unrelated instructions.  */
  fusion_type = (fp_p ? 2 : 0) + (isload_p ? 0 : 1);
  fusion_type *= NUM_MACHINE_MODES;
  fusion_type += (int) mem.mode + 1;
  *fusion_pri -= (fusion_type * FIRST_PSEUDO_REGISTER
		  + (int) base_regno);

  tmp /= 2;
  HOST_WIDE_INT off_val = INTVAL (mem.addr.offset);
  /* Use unsigned arithmetic to handle the most negative offset.  */
  unsigned HOST_WIDE_INT magnitude = off_val < 0
				       ? -(unsigned HOST_WIDE_INT) off_val
				       : off_val;
  int offset_pri = magnitude & 0xfffff;

  /* Order offsets in the preferred pair direction.  */
  if (inc_p == (off_val >= 0))
    *pri = tmp - offset_pri;
  else
    *pri = tmp + offset_pri;
}
