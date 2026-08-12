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

/* Return true if PREV_SET and CURR_SET satisfy the same-dest constraint
   required by most fusion rules: when we are past register allocation
   (i.e. can_create_pseudo_p () is false), the destination registers of
   the two sets must be the same physical register.  */

static bool
riscv_fusion_same_dest_p (rtx prev_set, rtx curr_set)
{
  if (can_create_pseudo_p ())
    return true;

  unsigned int prev_dest_regno = (REG_P (SET_DEST (prev_set))
				  ? REGNO (SET_DEST (prev_set))
				  : FIRST_PSEUDO_REGISTER);
  unsigned int curr_dest_regno = (REG_P (SET_DEST (curr_set))
				  ? REGNO (SET_DEST (curr_set))
				  : FIRST_PSEUDO_REGISTER);

  return prev_dest_regno == curr_dest_regno;
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

/* Check the common RTL for ZEXTW, ZEXTWS and ZEXTH fusion.  */

static bool
riscv_fuse_zext_common (rtx_insn *prev, rtx_insn *curr,
			int shl_amount, bool zextws_p)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  if (GET_CODE (SET_SRC (prev_set)) == ASHIFT
      && GET_CODE (SET_SRC (curr_set)) == LSHIFTRT
      && REG_P (SET_DEST (prev_set))
      && REG_P (SET_DEST (curr_set))
      && REG_P (XEXP (SET_SRC (curr_set), 0))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (curr_set))
      && CONST_INT_P (XEXP (SET_SRC (prev_set), 1))
      && CONST_INT_P (XEXP (SET_SRC (curr_set), 1))
      && INTVAL (XEXP (SET_SRC (prev_set), 1)) == shl_amount
      && (zextws_p
	  ? INTVAL (XEXP (SET_SRC (curr_set), 1)) < shl_amount
	  : INTVAL (XEXP (SET_SRC (curr_set), 1)) == shl_amount))
    return true;

  return false;
}

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
   prev (add) == (set (reg rd1)
		      (plus (reg rs1) (reg rs2)))
   curr (one of the following):
     (load) == (set (reg rd2) (mem (reg rd1)))
     (load) == (set (reg rd2)
		    (any_extend (mem (reg rd1))))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_ldindexed (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  if (MEM_P (SET_SRC (curr_set))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && REG_P (XEXP (SET_SRC (curr_set), 0))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set))
      && GET_CODE (SET_SRC (prev_set)) == PLUS
      && REG_P (XEXP (SET_SRC (prev_set), 0))
      && REG_P (XEXP (SET_SRC (prev_set), 1)))
    return true;

  /* curr (lw) == (set (reg:DI rd2)
		       (any_extend:DI (mem:SUBX (reg:DI rd1)))).  */
  if ((GET_CODE (SET_SRC (curr_set)) == SIGN_EXTEND
       || (GET_CODE (SET_SRC (curr_set)) == ZERO_EXTEND))
      && MEM_P (XEXP (SET_SRC (curr_set), 0))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && REG_P (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
      && (REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
	  == REGNO (SET_DEST (prev_set)))
      && GET_CODE (SET_SRC (prev_set)) == PLUS
      && REG_P (XEXP (SET_SRC (prev_set), 0))
      && REG_P (XEXP (SET_SRC (prev_set), 1)))
    return true;

  return false;
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
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  /* Match a load with displacement:
     curr (load) == (set (reg rd2)
			 (mem (plus (reg rd1) (const_int offset))))  */
  if (MEM_P (SET_SRC (curr_set))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == PLUS
      && REG_P (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
      && (REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
	  == REGNO (SET_DEST (prev_set))))
    {
      if (riscv_set_is_add_p (prev_set)
	  || riscv_set_is_addi_p (prev_set)
	  || riscv_set_is_shNadd_p (prev_set))
	return true;
    }

  /* Match a load without displacement:
     curr (load) == (set (reg rd2) (mem (reg rd1))).  */
  if (MEM_P (SET_SRC (curr_set))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && REG_P (XEXP (SET_SRC (curr_set), 0))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set)))
    {
      if (riscv_set_is_addi_p (prev_set)
	  || riscv_set_is_shNadd_p (prev_set))
	return true;
    }

  /* Match lw with displacement.  */
  if ((GET_CODE (SET_SRC (curr_set)) == SIGN_EXTEND
       || (GET_CODE (SET_SRC (curr_set)) == ZERO_EXTEND))
      && MEM_P (XEXP (SET_SRC (curr_set), 0))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && GET_CODE (XEXP (XEXP (SET_SRC (curr_set), 0), 0)) == PLUS
      && REG_P (XEXP (XEXP (XEXP (SET_SRC (curr_set), 0), 0), 0))
      && (REGNO (XEXP (XEXP (XEXP (SET_SRC (curr_set), 0), 0), 0))
	  == REGNO (SET_DEST (prev_set))))
    {
      if (riscv_set_is_adduw_p (prev_set)
	  || riscv_set_is_shNadduw_p (prev_set))
	return true;
    }

  /* Match lw without displacement.  */
  if ((GET_CODE (SET_SRC (curr_set)) == SIGN_EXTEND
       || (GET_CODE (SET_SRC (curr_set)) == ZERO_EXTEND))
      && MEM_P (XEXP (SET_SRC (curr_set), 0))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && REG_P (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
      && (REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
	  == REGNO (SET_DEST (prev_set))))
    {
      if (riscv_set_is_adduw_p (prev_set)
	  || riscv_set_is_shNadduw_p (prev_set))
	return true;
    }

  return false;
}

/* Check for RISCV_FUSE_LDPREINCREMENT fusion.
   prev (addi) == (set (reg rd1)
		       (plus (reg rd1) (const_int offset)))
   curr (load) == (set (reg rd2) (mem (reg rd1)))

   Constraints:
     rd1 == rd2.  */

static bool
riscv_fuse_ldpreincrement (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  if (MEM_P (SET_SRC (curr_set))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && REG_P (XEXP (SET_SRC (curr_set), 0))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set))
      && GET_CODE (SET_SRC (prev_set)) == PLUS
      && REG_P (XEXP (SET_SRC (prev_set), 0))
      && CONST_INT_P (XEXP (SET_SRC (prev_set), 1)))
    return true;

  return false;
}

/* Check for RISCV_FUSE_LUI_ADDI fusion.
   prev (one of the following):
     (lui) == (set (reg rd1) (const_int imm20))
     (lui) == (set (reg rd1) (high symbol1))
   curr (one of the following):
     (addi) == (set (reg rd2)
		    (plus (reg rd1) (const_int imm12)))
     (addi) == (set (reg rd2)
		    (lo_sum (reg rd1) symbol2))

   Constraints:
     rd1 == rd2
     rd1 != x0
     imm20 != 0 for the constant form.  */

static bool
riscv_fuse_lui_addi (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  if ((GET_CODE (SET_SRC (curr_set)) == LO_SUM
       || (GET_CODE (SET_SRC (curr_set)) == PLUS
	   && CONST_INT_P (XEXP (SET_SRC (curr_set), 1))
	   && SMALL_OPERAND (INTVAL (XEXP (SET_SRC (curr_set), 1)))))
      && (GET_CODE (SET_SRC (prev_set)) == HIGH
	  || (CONST_INT_P (SET_SRC (prev_set))
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

   Constraints:
     rd1 == rd2
     rd1 != x0.  */

static bool
riscv_fuse_auipc_addi (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  if (GET_CODE (SET_SRC (prev_set)) == UNSPEC
      && XINT (SET_SRC (prev_set), 1) == UNSPEC_AUIPC
      && (GET_CODE (SET_SRC (curr_set)) == LO_SUM
	  || (GET_CODE (SET_SRC (curr_set)) == PLUS
	      && CONST_INT_P (XEXP (SET_SRC (curr_set), 1))
	      && SMALL_OPERAND (INTVAL (XEXP (SET_SRC (curr_set), 1))))))
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
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  /* A LUI_OPERAND accepts (const_int 0), but we won't emit that as LUI.
     Reject that case explicitly.  */
  if (CONST_INT_P (SET_SRC (prev_set))
      && SET_SRC (prev_set) != CONST0_RTX (GET_MODE (SET_DEST (prev_set)))
      && LUI_OPERAND (INTVAL (SET_SRC (prev_set)))
      && MEM_P (SET_SRC (curr_set))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == PLUS
      && REG_P (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
      && (REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
	  == REGNO (SET_DEST (prev_set))))
    return true;

  if (GET_CODE (SET_SRC (prev_set)) == HIGH
      && MEM_P (SET_SRC (curr_set))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == LO_SUM
      && REG_P (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
      && (REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
	  == REGNO (SET_DEST (prev_set))))
    return true;

  if (GET_CODE (SET_SRC (prev_set)) == HIGH
      && (GET_CODE (SET_SRC (curr_set)) == SIGN_EXTEND
	  || GET_CODE (SET_SRC (curr_set)) == ZERO_EXTEND)
      && MEM_P (XEXP (SET_SRC (curr_set), 0))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && (GET_CODE (XEXP (XEXP (SET_SRC (curr_set), 0), 0)) == LO_SUM
	  && REG_P (XEXP (XEXP (XEXP (SET_SRC (curr_set), 0), 0), 0))
	  && (REGNO (XEXP (XEXP (XEXP (SET_SRC (curr_set), 0), 0), 0))
	      == REGNO (SET_DEST (prev_set)))))
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
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  if (GET_CODE (SET_SRC (prev_set)) == UNSPEC
      && XINT (SET_SRC (prev_set), 1) == UNSPEC_AUIPC
      && MEM_P (SET_SRC (curr_set))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == PLUS)
    return true;

  return false;
}

/* Check for RISCV_FUSE_ALIGNED_STD fusion.
   prev (store) == (set (mem (rs1, offset1)) (reg rs2))
   curr (store) == (set (mem (rs1, offset2)) (reg rs3))

   Constraints:
     both stores use the same scalar integer mode
     min (offset1, offset2) is aligned to twice the access size
     abs (offset1 - offset2) equals the access size.  */

static bool
riscv_fuse_aligned_std (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (MEM_P (SET_DEST (prev_set))
      && SCALAR_INT_MODE_P (GET_MODE (SET_DEST (curr_set)))
      && MEM_P (SET_DEST (curr_set))
      /* Stores must have the same width.  */
      && GET_MODE (SET_DEST (curr_set)) == GET_MODE (SET_DEST (prev_set)))
    {
      rtx base_prev, base_curr, offset_prev, offset_curr;
      unsigned mode_size;

      extract_base_offset_in_addr (SET_DEST (prev_set),
				   &base_prev, &offset_prev);
      extract_base_offset_in_addr (SET_DEST (curr_set),
				   &base_curr, &offset_curr);

      /* Proceed only if we find both bases, both bases
	 are registers and bases are the same register.  */
      if (base_prev != NULL_RTX && base_curr != NULL_RTX
	  && REG_P (base_prev) && REG_P (base_curr)
	  && REGNO (base_prev) == REGNO (base_curr))
	{
	  machine_mode mode = GET_MODE (SET_DEST (curr_set));
	  mode_size = estimated_poly_value (GET_MODE_SIZE (mode));

	  HOST_WIDE_INT offset_prev_int = INTVAL (offset_prev);
	  HOST_WIDE_INT offset_curr_int = INTVAL (offset_curr);

	  /* Get the smaller offset into OFFSET_PREV_INT.  */
	  if (offset_prev_int > offset_curr_int)
	    std::swap (offset_prev_int, offset_curr_int);

	  /* We've normalized, so we need to check that the lower
	     address is aligned to 2X the size of the object.  The
	     higher address must be the lower address plus the
	     size of the object.  */
	  if (((offset_prev_int % (2 * mode_size)) == 0)
	      && offset_prev_int + mode_size == offset_curr_int)
	    return true;
	}
    }

  return false;
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
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  if (GET_CODE (SET_SRC (prev_set)) == ASHIFT
      && (GET_CODE (SET_SRC (curr_set)) == LSHIFTRT
	  || GET_CODE (SET_SRC (curr_set)) == ASHIFTRT)
      && REG_P (SET_DEST (prev_set))
      && REG_P (SET_DEST (curr_set))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set))
      && CONST_INT_P (XEXP (SET_SRC (prev_set), 1))
      && CONST_INT_P (XEXP (SET_SRC (curr_set), 1)))
    return true;

  return false;
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
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  if (!prev_set || !curr_set || any_condjump_p (curr))
    return false;

  if (!riscv_fusion_same_dest_p (prev_set, curr_set))
    return false;

  /* orc.b + not.  */
  if (GET_CODE (SET_SRC (prev_set)) == UNSPEC
      && GET_CODE (SET_SRC (curr_set)) == NOT
      && XINT (SET_SRC (prev_set), 1) == UNSPEC_ORC_B
      && REG_P (SET_DEST (prev_set))
      && REG_P (SET_DEST (curr_set))
      && REG_P (XEXP (SET_SRC (curr_set), 0))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set)))
    return true;

  /* ctz + andi.  */
  if (GET_CODE (SET_SRC (prev_set)) == CTZ
      && GET_CODE (SET_SRC (curr_set)) == AND
      && CONST_INT_P (XEXP (SET_SRC (curr_set), 1))
      && INTVAL (XEXP (SET_SRC (curr_set), 1)) == 63
      && REG_P (SET_DEST (prev_set))
      && REG_P (SET_DEST (curr_set))
      && REG_P (XEXP (SET_SRC (curr_set), 0))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set)))
    return true;

  /* sub + smax (abs pattern).  */
  if (GET_CODE (SET_SRC (prev_set)) == MINUS
      && (XEXP (SET_SRC (prev_set), 0)
	  == CONST0_RTX (GET_MODE (SET_SRC (prev_set))))
      && GET_CODE (SET_SRC (curr_set)) == SMAX
      && REG_P (SET_DEST (prev_set))
      && REG_P (SET_DEST (curr_set))
      && REG_P (XEXP (SET_SRC (curr_set), 0))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set))
      && REG_P (XEXP (SET_SRC (prev_set), 1))
      && REG_P (XEXP (SET_SRC (curr_set), 1))
      && (REGNO (XEXP (SET_SRC (prev_set), 1))
	  == REGNO (XEXP (SET_SRC (curr_set), 1))))
    return true;

  /* neg + smax (abs pattern).  */
  if (GET_CODE (SET_SRC (prev_set)) == NEG
      && GET_CODE (SET_SRC (curr_set)) == SMAX
      && REG_P (SET_DEST (prev_set))
      && REG_P (SET_DEST (curr_set))
      && REG_P (XEXP (SET_SRC (curr_set), 0))
      && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set))
      && REG_P (XEXP (SET_SRC (prev_set), 0))
      && REG_P (XEXP (SET_SRC (curr_set), 1))
      && (REGNO (XEXP (SET_SRC (prev_set), 0))
	  == REGNO (XEXP (SET_SRC (curr_set), 1))))
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
  { RISCV_FUSE_EXPANDED_LD,
    riscv_fuse_expanded_ld, "RISCV_FUSE_EXPANDED_LD" },
  { RISCV_FUSE_LDPREINCREMENT,
    riscv_fuse_ldpreincrement, "RISCV_FUSE_LDPREINCREMENT" },
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
  { RISCV_FUSE_BFEXT,
    riscv_fuse_bfext, "RISCV_FUSE_BFEXT" },
  { RISCV_FUSE_B_ALUI,
    riscv_fuse_b_alui, "RISCV_FUSE_B_ALUI" },
};

/* Implement TARGET_SCHED_MACRO_FUSION_PAIR_P.  Return true if PREV and CURR
   should be kept together during scheduling.  */

bool
riscv_macro_fusion_pair_p (rtx_insn *prev, rtx_insn *curr)
{
  /* If fusion is not enabled, then there's nothing to do.  */
  if (!riscv_macro_fusion_p ())
    return false;

  /* If PREV is already marked as fused, then we can't fuse CURR with PREV
     and if we were to fuse them we'd end up with a blob of insns that
     essentially are an atomic unit which is bad for scheduling.  */
  if (SCHED_GROUP_P (prev))
    return false;

  for (size_t i = 0; i < ARRAY_SIZE (riscv_fusion_table); i++)
    {
      const struct riscv_fusion_entry *entry = &riscv_fusion_table[i];

      /* Check if this fusion type is enabled.  */
      if (!riscv_fusion_enabled_p (entry->op))
	continue;

      if (entry->checker (prev, curr))
	{
	  if (dump_file)
	    fprintf (dump_file, ";; macro fusion: insn %d + insn %d -> %s\n",
		     INSN_UID (prev), INSN_UID (curr), entry->fusion_type);
	  return true;
	}
    }

  return false;
}
