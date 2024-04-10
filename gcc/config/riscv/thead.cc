/* Subroutines used for code generation for RISC-V.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by Christoph Müllner (christoph.muellner@vrull.eu).

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
#include "target.h"
#include "backend.h"
#include "tree.h"
#include "rtl.h"
#include "insn-attr.h"
#include "explow.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "optabs.h"
#include "poly-int.h"
#include "output.h"
#include "regs.h"
#include "riscv-protos.h"

/* If X is a PLUS of a CONST_INT, return the two terms in *BASE_PTR
   and *OFFSET_PTR.  Return X in *BASE_PTR and 0 in *OFFSET_PTR otherwise.  */

static void
split_plus (rtx x, rtx *base_ptr, HOST_WIDE_INT *offset_ptr)
{
  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
    {
      *base_ptr = XEXP (x, 0);
      *offset_ptr = INTVAL (XEXP (x, 1));
    }
  else
    {
      *base_ptr = x;
      *offset_ptr = 0;
    }
}

/* Output a mempair instruction with the provided OPERANDS.
   LOAD_P is true if a we have a pair of loads (stores otherwise).
   MODE is the access mode (DI or SI).
   CODE is the extension code (UNKNOWN, SIGN_EXTEND or ZERO_EXTEND).
   This instruction does not handle invalid inputs gracefully,
   but is full of assertions to ensure that only valid instructions
   are emitted.  */

const char *
th_mempair_output_move (rtx operands[4], bool load_p,
			machine_mode mode, RTX_CODE code)
{
  rtx reg1, reg2, mem1, mem2, base1, base2;
  HOST_WIDE_INT offset1, offset2;
  rtx output_operands[5];
  const char* format;

  gcc_assert (mode == SImode || mode == DImode);

  /* Paired 64-bit access instructions have a fixed shift amount of 4.
     Paired 32-bit access instructions have a fixed shift amount of 3.  */
  unsigned shamt = (mode == DImode) ? 4 : 3;

  if (load_p)
    {
	reg1 = copy_rtx (operands[0]);
	reg2 = copy_rtx (operands[2]);
	mem1 = copy_rtx (operands[1]);
	mem2 = copy_rtx (operands[3]);

	if (mode == SImode)
	  if (code == ZERO_EXTEND)
	    format = "th.lwud\t%0, %1, (%2), %3, %4";
	  else //SIGN_EXTEND or UNKNOWN
	    format = "th.lwd\t%0, %1, (%2), %3, %4";
	else
	  format = "th.ldd\t%0, %1, (%2), %3, %4";
    }
  else
    {
	reg1 = copy_rtx (operands[1]);
	reg2 = copy_rtx (operands[3]);
	mem1 = copy_rtx (operands[0]);
	mem2 = copy_rtx (operands[2]);

	if (mode == SImode)
	  format = "th.swd\t%z0, %z1, (%2), %3, %4";
	else
	  format = "th.sdd\t%z0, %z1, (%2), %3, %4";
    }

  split_plus (XEXP (mem1, 0), &base1, &offset1);
  split_plus (XEXP (mem2, 0), &base2, &offset2);
  gcc_assert (rtx_equal_p (base1, base2));
  auto size1 = MEM_SIZE (mem1);
  auto size2 = MEM_SIZE (mem2);
  gcc_assert (known_eq (size1, size2));
  gcc_assert (known_eq (offset1 + size1, offset2));

  HOST_WIDE_INT imm2 = offset1 >> shamt;

  /* Make sure all mempair instruction constraints are met.  */
  gcc_assert (imm2 >= 0 && imm2 < 4);
  gcc_assert ((imm2 << shamt) == offset1);
  gcc_assert (REG_P (reg1));
  gcc_assert (REG_P (reg2));
  gcc_assert (REG_P (base1));
  if (load_p)
    {
      gcc_assert (REGNO (reg1) != REGNO (reg2));
      gcc_assert (REGNO (reg1) != REGNO (base1));
      gcc_assert (REGNO (reg2) != REGNO (base1));
    }

  /* Output the mempair instruction.  */
  output_operands[0] = copy_rtx (reg1);
  output_operands[1] = copy_rtx (reg2);
  output_operands[2] = copy_rtx (base1);
  output_operands[3] = gen_rtx_CONST_INT (mode, imm2);
  output_operands[4] = gen_rtx_CONST_INT (mode, shamt);
  output_asm_insn (format, output_operands);

  return "";
}

/* Analyse if a pair of loads/stores MEM1 and MEM2 with given MODE
   are consecutive so they can be merged into a mempair instruction.
   RESERVED will be set to true, if a reversal of the accesses is
   required (false otherwise). Returns true if the accesses can be
   merged (even if reversing is necessary) and false if not.  */

static bool
th_mempair_check_consecutive_mems (machine_mode mode, rtx *mem1, rtx *mem2,
				   bool *reversed)
{
  rtx base1, base2, offset1, offset2;
  extract_base_offset_in_addr (*mem1, &base1, &offset1);
  extract_base_offset_in_addr (*mem2, &base2, &offset2);

  /* Make sure both mems are in base+offset form.  */
  if (!base1 || !base2)
    return false;

  /* If both mems use the same base register, just check the offsets.  */
  if (rtx_equal_p (base1, base2))
    {
      auto size = GET_MODE_SIZE (mode);

      if (known_eq (UINTVAL (offset1) + size, UINTVAL (offset2)))
	{
	  *reversed = false;
	  return true;
	}

      if (known_eq (UINTVAL (offset2) + size, UINTVAL (offset1)))
	{
	  *reversed = true;
	  return true;
	}

      return false;
    }

  return false;
}

/* Check if the given MEM can be used to define the address of a mempair
   instruction.  */

static bool
th_mempair_operand_p (rtx mem, machine_mode mode)
{
  if (!MEM_SIZE_KNOWN_P (mem))
    return false;

  /* Only DI or SI mempair instructions exist.  */
  gcc_assert (mode == SImode || mode == DImode);
  auto mem_sz = MEM_SIZE (mem);
  auto mode_sz = GET_MODE_SIZE (mode);
  if (!known_eq (mem_sz, mode_sz))
    return false;

  /* Paired 64-bit access instructions have a fixed shift amount of 4.
     Paired 32-bit access instructions have a fixed shift amount of 3.  */
  machine_mode mem_mode = GET_MODE (mem);
  unsigned shamt = (mem_mode == DImode) ? 4 : 3;

  rtx base;
  HOST_WIDE_INT offset;
  split_plus (XEXP (mem, 0), &base, &offset);
  HOST_WIDE_INT imm2 = offset >> shamt;

  if (imm2 < 0 || imm2 >= 4)
    return false;

  if ((imm2 << shamt) != offset)
    return false;

  return true;
}

static bool
th_mempair_load_overlap_p (rtx reg1, rtx reg2, rtx mem)
{
  if (REGNO (reg1) == REGNO (reg2))
    return true;

  if (reg_overlap_mentioned_p (reg1, mem))
    return true;

  rtx base;
  HOST_WIDE_INT offset;
  split_plus (XEXP (mem, 0), &base, &offset);

  if (!REG_P (base))
    return true;

  if (REG_P (base))
    {
      if (REGNO (base) == REGNO (reg1)
	  || REGNO (base) == REGNO (reg2))
	return true;
    }

  return false;
}

/* Given OPERANDS of consecutive load/store, check if we can merge
   them into load-pair or store-pair instructions.
   LOAD is true if they are load instructions.
   MODE is the mode of memory operation.  */

bool
th_mempair_operands_p (rtx operands[4], bool load_p,
		       machine_mode mode)
{
  rtx mem_1, mem_2, reg_1, reg_2;

  if (load_p)
    {
      reg_1 = operands[0];
      mem_1 = operands[1];
      reg_2 = operands[2];
      mem_2 = operands[3];
      if (!REG_P (reg_1) || !REG_P (reg_2))
	return false;
      if (th_mempair_load_overlap_p (reg_1, reg_2, mem_1))
	return false;
      if (th_mempair_load_overlap_p (reg_1, reg_2, mem_2))
	return false;
    }
  else
    {
      mem_1 = operands[0];
      reg_1 = operands[1];
      mem_2 = operands[2];
      reg_2 = operands[3];
    }

  /* Check if the registers are GP registers.  */
  if (!REG_P (reg_1) || !GP_REG_P (REGNO (reg_1))
      || !REG_P (reg_2) || !GP_REG_P (REGNO (reg_2)))
    return false;

  /* The mems cannot be volatile.  */
  if (!MEM_P (mem_1) || !MEM_P (mem_2))
    return false;
  if (MEM_VOLATILE_P (mem_1) || MEM_VOLATILE_P (mem_2))
    return false;

  /* If we have slow unaligned access, we only accept aligned memory.  */
  if (riscv_slow_unaligned_access_p
      && known_lt (MEM_ALIGN (mem_1), GET_MODE_SIZE (mode) * BITS_PER_UNIT))
    return false;

  /* Check if the addresses are in the form of [base+offset].  */
  bool reversed = false;
  if (!th_mempair_check_consecutive_mems (mode, &mem_1, &mem_2, &reversed))
    return false;

  /* The first memory accesses must be a mempair operand.  */
  if ((!reversed && !th_mempair_operand_p (mem_1, mode))
      || (reversed && !th_mempair_operand_p (mem_2, mode)))
    return false;

  /* The operands must be of the same size.  */
  gcc_assert (known_eq (GET_MODE_SIZE (GET_MODE (mem_1)),
			GET_MODE_SIZE (GET_MODE (mem_2))));

  return true;
}

/* Given OPERANDS of consecutive load/store that can be merged,
   swap them if they are not in ascending order.  */

void
th_mempair_order_operands (rtx operands[4], bool load_p, machine_mode mode)
{
  int mem_op = load_p ? 1 : 0;
  bool reversed = false;
  if (!th_mempair_check_consecutive_mems (mode,
					  operands + mem_op,
					  operands + mem_op + 2,
					  &reversed))
    gcc_unreachable ();

  if (reversed)
    {
      /* Irrespective of whether this is a load or a store,
	 we do the same swap.  */
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
}

/* Similar like riscv_save_reg, but saves two registers to memory
   and marks the resulting instruction as frame-related.  */

static void
th_mempair_save_regs (rtx operands[4])
{
  rtx set1 = gen_rtx_SET (operands[0], operands[1]);
  rtx set2 = gen_rtx_SET (operands[2], operands[3]);
  rtx dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (2));
  rtx insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set1, set2)));
  RTX_FRAME_RELATED_P (insn) = 1;

  XVECEXP (dwarf, 0, 0) = copy_rtx (set1);
  XVECEXP (dwarf, 0, 1) = copy_rtx (set2);
  RTX_FRAME_RELATED_P (XVECEXP (dwarf, 0, 0)) = 1;
  RTX_FRAME_RELATED_P (XVECEXP (dwarf, 0, 1)) = 1;
  add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
}

/* Similar like riscv_restore_reg, but restores two registers from memory
   and marks the instruction frame-related.  */

static void
th_mempair_restore_regs (rtx operands[4])
{
  rtx set1 = gen_rtx_SET (operands[0], operands[1]);
  rtx set2 = gen_rtx_SET (operands[2], operands[3]);
  rtx insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set1, set2)));
  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_CFA_RESTORE, operands[0]);
  add_reg_note (insn, REG_CFA_RESTORE, operands[2]);
}

/* Prepare the OPERANDS array to emit a mempair instruction using the
   provided information. No checks are performed, the resulting array
   should be validated using th_mempair_operands_p().  */

void
th_mempair_prepare_save_restore_operands (rtx operands[4],
					  bool load_p, machine_mode mode,
					  int regno, HOST_WIDE_INT offset,
					  int regno2, HOST_WIDE_INT offset2)
{
  int reg_op = load_p ? 0 : 1;
  int mem_op = load_p ? 1 : 0;

  rtx mem1 = plus_constant (mode, stack_pointer_rtx, offset);
  mem1 = gen_frame_mem (mode, mem1);
  rtx mem2 = plus_constant (mode, stack_pointer_rtx, offset2);
  mem2 = gen_frame_mem (mode, mem2);

  operands[reg_op] = gen_rtx_REG (mode, regno);
  operands[mem_op] = mem1;
  operands[2 + reg_op] = gen_rtx_REG (mode, regno2);
  operands[2 + mem_op] = mem2;
}

/* Emit a mempair instruction to save/restore two registers to/from stack.  */

void
th_mempair_save_restore_regs (rtx operands[4], bool load_p,
				 machine_mode mode)
{
  gcc_assert (th_mempair_operands_p (operands, load_p, mode));

  th_mempair_order_operands (operands, load_p, mode);

  if (load_p)
    th_mempair_restore_regs (operands);
  else
    th_mempair_save_regs (operands);
}

/* Return true if X can be represented as signed immediate of NBITS bits.
   The immediate is assumed to be shifted by LSHAMT bits left.  */

static bool
valid_signed_immediate (rtx x, unsigned nbits, unsigned lshamt)
{
  if (GET_CODE (x) != CONST_INT)
    return false;

  HOST_WIDE_INT v = INTVAL (x);

  HOST_WIDE_INT vunshifted = v >> lshamt;

  /* Make sure we did not shift out any bits.  */
  if (vunshifted << lshamt != v)
    return false;

  unsigned HOST_WIDE_INT imm_reach = 1LL << nbits;
  return ((unsigned HOST_WIDE_INT) vunshifted + imm_reach/2 < imm_reach);
}

/* Return the address RTX of a move to/from memory
   instruction.  */

static rtx
th_get_move_mem_addr (rtx dest, rtx src, bool load)
{
  rtx mem;

  if (load)
    mem = src;
  else
    mem = dest;

  gcc_assert (GET_CODE (mem) == MEM);
  return XEXP (mem, 0);
}

/* Return true if X is a valid address for T-Head's memory addressing modes
   with pre/post modification for machine mode MODE.
   If it is, fill in INFO appropriately (if non-NULL).
   If STRICT_P is true then REG_OK_STRICT is in effect.  */

static bool
th_memidx_classify_address_modify (struct riscv_address_info *info, rtx x,
				   machine_mode mode, bool strict_p)
{
  if (!TARGET_XTHEADMEMIDX)
    return false;

  if (!TARGET_64BIT && mode == DImode)
    return false;

  if (!(INTEGRAL_MODE_P (mode) && GET_MODE_SIZE (mode).to_constant () <= 8))
    return false;

  if (GET_CODE (x) != POST_MODIFY
      && GET_CODE (x) != PRE_MODIFY)
    return false;

  rtx reg = XEXP (x, 0);
  rtx exp = XEXP (x, 1);
  rtx expreg = XEXP (exp, 0);
  rtx expoff = XEXP (exp, 1);

  if (GET_CODE (exp) != PLUS
      || !rtx_equal_p (expreg, reg)
      || !CONST_INT_P (expoff)
      || !riscv_valid_base_register_p (reg, mode, strict_p))
    return false;

  /* The offset is calculated as (sign_extend(imm5) << imm2)  */
  const int shamt_bits = 2;
  for (int shamt = 0; shamt < (1 << shamt_bits); shamt++)
    {
      const int nbits = 5;
      if (valid_signed_immediate (expoff, nbits, shamt))
	{
	  if (info)
	    {
	      info->type = ADDRESS_REG_WB;
	      info->reg = reg;
	      info->offset = expoff;
	      info->shift = shamt;
	    }
	  return true;
	}
    }

  return false;
}

/* Return TRUE if X is a MEM with a legitimate modify address.  */

bool
th_memidx_legitimate_modify_p (rtx x)
{
  if (!MEM_P (x))
    return false;

  /* Get the mode from the MEM and unpack it.  */
  machine_mode mode = GET_MODE (x);
  x = XEXP (x, 0);

  return th_memidx_classify_address_modify (NULL, x, mode, reload_completed);
}

/* Return TRUE if X is a MEM with a legitimate modify address
   and the address is POST_MODIFY (if POST is true) or a PRE_MODIFY
   (otherwise).  */

bool
th_memidx_legitimate_modify_p (rtx x, bool post)
{
  if (!th_memidx_legitimate_modify_p (x))
    return false;

  /* Unpack the MEM and check the code.  */
  x = XEXP (x, 0);
  if (post)
    return GET_CODE (x) == POST_MODIFY;
  else
    return GET_CODE (x) == PRE_MODIFY;
}

/* Provide a buffer for a th.lXia/th.lXib/th.sXia/th.sXib instruction
   for the given MODE. If LOAD is true, a load instruction will be
   provided (otherwise, a store instruction). If X is not suitable
   return NULL.  */

static const char *
th_memidx_output_modify (rtx dest, rtx src, machine_mode mode, bool load)
{
  char format[24];
  rtx output_operands[2];
  rtx x = th_get_move_mem_addr (dest, src, load);

  /* Validate x.  */
  if (!th_memidx_classify_address_modify (NULL, x, mode, reload_completed))
    return NULL;

  int index = exact_log2 (GET_MODE_SIZE (mode).to_constant ());
  bool post = GET_CODE (x) == POST_MODIFY;

  const char *const insn[][4] = {
    {
      "th.sbi%s\t%%z1,%%0",
      "th.shi%s\t%%z1,%%0",
      "th.swi%s\t%%z1,%%0",
      "th.sdi%s\t%%z1,%%0"
    },
    {
      "th.lbui%s\t%%0,%%1",
      "th.lhui%s\t%%0,%%1",
      "th.lwi%s\t%%0,%%1",
      "th.ldi%s\t%%0,%%1"
    }
  };

  snprintf (format, sizeof (format), insn[load][index], post ? "a" : "b");
  output_operands[0] = dest;
  output_operands[1] = src;
  output_asm_insn (format, output_operands);
  return "";
}

static bool
is_memidx_mode (machine_mode mode)
{
  if (mode == QImode || mode == HImode || mode == SImode)
    return true;

  if (mode == DImode && TARGET_64BIT)
    return true;

  return false;
}

static bool
is_fmemidx_mode (machine_mode mode)
{
  if (mode == SFmode && TARGET_HARD_FLOAT)
    return true;

  if (mode == DFmode && TARGET_DOUBLE_FLOAT)
    return true;

  return false;
}

/* Return true if X is a valid address for T-Head's memory addressing modes
   with scaled register offsets for machine mode MODE.
   If it is, fill in INFO appropriately (if non-NULL).
   If STRICT_P is true then REG_OK_STRICT is in effect.  */

static bool
th_memidx_classify_address_index (struct riscv_address_info *info, rtx x,
				  machine_mode mode, bool strict_p)
{
  /* Ensure that the mode is supported.  */
  if (!(TARGET_XTHEADMEMIDX && is_memidx_mode (mode))
      && !(TARGET_XTHEADMEMIDX
	   && TARGET_XTHEADFMEMIDX && is_fmemidx_mode (mode)))
    return false;

  if (GET_CODE (x) != PLUS)
    return false;

  rtx reg = XEXP (x, 0);
  enum riscv_address_type type;
  rtx offset = XEXP (x, 1);
  int shift;

  if (!riscv_valid_base_register_p (reg, mode, strict_p))
    return false;

  /* (reg:X) */
  if (REG_P (offset)
      && GET_MODE (offset) == Xmode)
    {
      type = ADDRESS_REG_REG;
      shift = 0;
      offset = offset;
    }
  /* (zero_extend:DI (reg:SI)) */
  else if (GET_CODE (offset) == ZERO_EXTEND
	   && GET_MODE (offset) == DImode
	   && GET_MODE (XEXP (offset, 0)) == SImode)
    {
      type = ADDRESS_REG_UREG;
      shift = 0;
      offset = XEXP (offset, 0);
    }
  /* (ashift:X (reg:X) (const_int shift)) */
  else if (GET_CODE (offset) == ASHIFT
	   && GET_MODE (offset) == Xmode
	   && REG_P (XEXP (offset, 0))
	   && GET_MODE (XEXP (offset, 0)) == Xmode
	   && CONST_INT_P (XEXP (offset, 1))
	   && IN_RANGE (INTVAL (XEXP (offset, 1)), 0, 3))
    {
      type = ADDRESS_REG_REG;
      shift = INTVAL (XEXP (offset, 1));
      offset = XEXP (offset, 0);
    }
  /* (ashift:DI (zero_extend:DI (reg:SI)) (const_int shift)) */
  else if (GET_CODE (offset) == ASHIFT
	   && GET_MODE (offset) == DImode
	   && GET_CODE (XEXP (offset, 0)) == ZERO_EXTEND
	   && GET_MODE (XEXP (offset, 0)) == DImode
	   && GET_MODE (XEXP (XEXP (offset, 0), 0)) == SImode
	   && CONST_INT_P (XEXP (offset, 1))
	   && IN_RANGE(INTVAL (XEXP (offset, 1)), 0, 3))
    {
      type = ADDRESS_REG_UREG;
      shift = INTVAL (XEXP (offset, 1));
      offset = XEXP (XEXP (offset, 0), 0);
    }
  else
    return false;

  if (!strict_p && GET_CODE (offset) == SUBREG)
    offset = SUBREG_REG (offset);

  if (!REG_P (offset)
      || !riscv_regno_mode_ok_for_base_p (REGNO (offset), mode, strict_p))
    return false;

  if (info)
    {
      info->reg = reg;
      info->type = type;
      info->offset = offset;
      info->shift = shift;
    }
  return true;
}

/* Return TRUE if X is a MEM with a legitimate indexed address.  */

bool
th_memidx_legitimate_index_p (rtx x)
{
  if (!MEM_P (x))
    return false;

  /* Get the mode from the MEM and unpack it.  */
  machine_mode mode = GET_MODE (x);
  x = XEXP (x, 0);

  return th_memidx_classify_address_index (NULL, x, mode, reload_completed);
}

/* Return TRUE if X is a MEM with a legitimate indexed address
   and the offset register is zero-extended (if UINDEX is true)
   or sign-extended (otherwise).  */

bool
th_memidx_legitimate_index_p (rtx x, bool uindex)
{
  if (!MEM_P (x))
    return false;

  /* Get the mode from the MEM and unpack it.  */
  machine_mode mode = GET_MODE (x);
  x = XEXP (x, 0);

  struct riscv_address_info info;
  if (!th_memidx_classify_address_index (&info, x, mode, reload_completed))
    return false;

  if (uindex)
    return info.type == ADDRESS_REG_UREG;
  else
    return info.type == ADDRESS_REG_REG;
}

/* Provide a buffer for a th.lrX/th.lurX/th.srX/th.surX instruction
   for the given MODE. If LOAD is true, a load instruction will be
   provided (otherwise, a store instruction). If X is not suitable
   return NULL.  */

static const char *
th_memidx_output_index (rtx dest, rtx src, machine_mode mode, bool load)
{
  struct riscv_address_info info;
  char format[24];
  rtx output_operands[2];
  rtx x = th_get_move_mem_addr (dest, src, load);

  /* Validate x.  */
  if (!th_memidx_classify_address_index (&info, x, mode, reload_completed))
    return NULL;

  int index = exact_log2 (GET_MODE_SIZE (mode).to_constant ());
  bool uindex = info.type == ADDRESS_REG_UREG;

  const char *const insn[][4] = {
    {
      "th.s%srb\t%%z1,%%0",
      "th.s%srh\t%%z1,%%0",
      "th.s%srw\t%%z1,%%0",
      "th.s%srd\t%%z1,%%0"
    },
    {
      "th.l%srbu\t%%0,%%1",
      "th.l%srhu\t%%0,%%1",
      "th.l%srw\t%%0,%%1",
      "th.l%srd\t%%0,%%1"
    }
  };

  snprintf (format, sizeof (format), insn[load][index], uindex ? "u" : "");
  output_operands[0] = dest;
  output_operands[1] = src;
  output_asm_insn (format, output_operands);
  return "";
}

/* Provide a buffer for a th.flX/th.fluX/th.fsX/th.fsuX instruction
   for the given MODE. If LOAD is true, a load instruction will be
   provided (otherwise, a store instruction). If X is not suitable
   return NULL.  */

static const char *
th_fmemidx_output_index (rtx dest, rtx src, machine_mode mode, bool load)
{
  struct riscv_address_info info;
  char format[24];
  rtx output_operands[2];
  rtx x = th_get_move_mem_addr (dest, src, load);

  /* Validate x.  */
  if (!th_memidx_classify_address_index (&info, x, mode, false))
    return NULL;

  int index = exact_log2 (GET_MODE_SIZE (mode).to_constant ()) - 2;
  bool uindex = info.type == ADDRESS_REG_UREG;

  const char *const insn[][2] = {
    {
      "th.fs%srw\t%%z1,%%0",
      "th.fs%srd\t%%z1,%%0"
    },
    {
      "th.fl%srw\t%%0,%%1",
      "th.fl%srd\t%%0,%%1"
    }
  };

  snprintf (format, sizeof (format), insn[load][index], uindex ? "u" : "");
  output_operands[0] = dest;
  output_operands[1] = src;
  output_asm_insn (format, output_operands);
  return "";
}

/* Return true if X is a valid address for T-Head's memory addressing modes
   for machine mode MODE.  If it is, fill in INFO appropriately (if non-NULL).
   If STRICT_P is true then REG_OK_STRICT is in effect.  */

bool
th_classify_address (struct riscv_address_info *info, rtx x,
		     machine_mode mode, bool strict_p)
{
  switch (GET_CODE (x))
    {
    case PLUS:
      if (th_memidx_classify_address_index (info, x, mode, strict_p))
	return true;
      break;

    case POST_MODIFY:
    case PRE_MODIFY:
      if (th_memidx_classify_address_modify (info, x, mode, strict_p))
	return true;
      break;

    default:
      return false;
  }

    return false;
}

/* Provide a string containing a XTheadMemIdx instruction for the given
   MODE from the provided SRC to the provided DEST.
   A pointer to a NULL-terminated string containing the instruction will
   be returned if a suitable instruction is available. Otherwise, this
   function returns NULL.  */

const char *
th_output_move (rtx dest, rtx src)
{
  enum rtx_code dest_code, src_code;
  machine_mode mode;
  const char *insn = NULL;

  dest_code = GET_CODE (dest);
  src_code = GET_CODE (src);
  mode = GET_MODE (dest);

  if (!(mode == GET_MODE (src) || src == CONST0_RTX (mode)))
    return NULL;

  if (dest_code == REG && src_code == MEM)
    {
      if (GET_MODE_CLASS (mode) == MODE_INT
	  || (GET_MODE_CLASS (mode) == MODE_FLOAT && GP_REG_P (REGNO (dest))))
	{
	  if ((insn = th_memidx_output_index (dest, src, mode, true)))
	    return insn;
	  if ((insn = th_memidx_output_modify (dest, src, mode, true)))
	    return insn;
	}
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT && HARDFP_REG_P (REGNO (dest)))
	{
	  if ((insn = th_fmemidx_output_index (dest, src, mode, true)))
	    return insn;
	}
    }
  else if (dest_code == MEM && (src_code == REG || src == CONST0_RTX (mode)))
    {
      if (GET_MODE_CLASS (mode) == MODE_INT
	  || src == CONST0_RTX (mode)
	  || (GET_MODE_CLASS (mode) == MODE_FLOAT && GP_REG_P (REGNO (src))))
	{
	  if ((insn = th_memidx_output_index (dest, src, mode, false)))
	    return insn;
	  if ((insn = th_memidx_output_modify (dest, src, mode, false)))
	    return insn;
	}
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT && HARDFP_REG_P (REGNO (src)))
	{
	  if ((insn = th_fmemidx_output_index (dest, src, mode, false)))
	    return insn;
	}
    }
  return NULL;
}

/* Define ASM_OUTPUT_OPCODE to do anything special before
   emitting an opcode.  */
const char *
th_asm_output_opcode (FILE *asm_out_file, const char *p)
{
  /* We need to add th. prefix to all the xtheadvector
     instructions here.*/
  if (current_output_insn != NULL)
    {
      if (get_attr_type (current_output_insn) == TYPE_VSETVL)
	{
	  if (strstr (p, "zero"))
	    {
	      if (strstr (p, "zero,zero"))
		return "th.vsetvli\tzero,zero,e%0,%m1";
	      else
		return "th.vsetvli\tzero,%0,e%1,%m2";
	    }
	  else
	    {
	      return "th.vsetvli\t%0,%1,e%2,%m3";
	    }
	}

      if (get_attr_type (current_output_insn) == TYPE_VLDE ||
	  get_attr_type (current_output_insn) == TYPE_VSTE ||
	  get_attr_type (current_output_insn) == TYPE_VLDFF)
	{
	  if (strstr (p, "e8") || strstr (p, "e16") ||
	      strstr (p, "e32") || strstr (p, "e64"))
	    {
	      get_attr_type (current_output_insn) == TYPE_VSTE
				  ? fputs ("th.vse", asm_out_file)
				  : fputs ("th.vle", asm_out_file);
	      if (strstr (p, "e8"))
		return p+4;
	      else
		return p+5;
	    }
	}

      if (get_attr_type (current_output_insn) == TYPE_VLDS ||
	  get_attr_type (current_output_insn) == TYPE_VSTS)
	{
	  if (strstr (p, "vle8") || strstr (p, "vse8") ||
	      strstr (p, "vle16") || strstr (p, "vse16") ||
	      strstr (p, "vle32") || strstr (p, "vse32") ||
	      strstr (p, "vle64") || strstr (p, "vse64"))
	    {
	      get_attr_type (current_output_insn) == TYPE_VSTS
				  ? fputs ("th.vse", asm_out_file)
				  : fputs ("th.vle", asm_out_file);
	      if (strstr (p, "e8"))
		return p+4;
	      else
		return p+5;
	    }
	  else if (strstr (p, "vlse8") || strstr (p, "vsse8") ||
		   strstr (p, "vlse16") || strstr (p, "vsse16") ||
		   strstr (p, "vlse32") || strstr (p, "vsse32") ||
		   strstr (p, "vlse64") || strstr (p, "vsse64"))
	    {
	      get_attr_type (current_output_insn) == TYPE_VSTS
				  ? fputs ("th.vsse", asm_out_file)
				  : fputs ("th.vlse", asm_out_file);
	      if (strstr (p, "e8"))
		return p+5;
	      else
		return p+6;
	    }
	}

      if (get_attr_type (current_output_insn) == TYPE_VLDUX ||
	  get_attr_type (current_output_insn) == TYPE_VLDOX)
	{
	  if (strstr (p, "ei"))
	    {
	      fputs ("th.vlxe", asm_out_file);
	      if (strstr (p, "ei8"))
		return p+7;
	      else
		return p+8;
	    }
	}

      if (get_attr_type (current_output_insn) == TYPE_VSTUX ||
	  get_attr_type (current_output_insn) == TYPE_VSTOX)
	{
	  if (strstr (p, "ei"))
	    {
	      get_attr_type (current_output_insn) == TYPE_VSTUX
				? fputs ("th.vsuxe", asm_out_file)
				: fputs ("th.vsxe", asm_out_file);
	      if (strstr (p, "ei8"))
		return p+7;
	      else
		return p+8;
	    }
	}

      if (get_attr_type (current_output_insn) == TYPE_VLSEGDE ||
	  get_attr_type (current_output_insn) == TYPE_VSSEGTE ||
	  get_attr_type (current_output_insn) == TYPE_VLSEGDFF)
	{
	  get_attr_type (current_output_insn) == TYPE_VSSEGTE
				? fputs ("th.vsseg", asm_out_file)
				: fputs ("th.vlseg", asm_out_file);
	  asm_fprintf (asm_out_file, "%c", p[5]);
	  fputs ("e", asm_out_file);
	  if (strstr (p, "e8"))
	    return p+8;
	  else
	    return p+9;
	}

      if (get_attr_type (current_output_insn) == TYPE_VLSEGDS ||
	  get_attr_type (current_output_insn) == TYPE_VSSEGTS)
	{
	  get_attr_type (current_output_insn) == TYPE_VSSEGTS
				? fputs ("th.vssseg", asm_out_file)
				: fputs ("th.vlsseg", asm_out_file);
	  asm_fprintf (asm_out_file, "%c", p[6]);
	  fputs ("e", asm_out_file);
	  if (strstr (p, "e8"))
	    return p+9;
	  else
	    return p+10;
	}

      if (get_attr_type (current_output_insn) == TYPE_VLSEGDUX ||
	  get_attr_type (current_output_insn) == TYPE_VLSEGDOX)
	{
	  fputs ("th.vlxseg", asm_out_file);
	  asm_fprintf (asm_out_file, "%c", p[7]);
	  fputs ("e", asm_out_file);
	  if (strstr (p, "ei8"))
	    return p+11;
	  else
	    return p+12;
	}

      if (get_attr_type (current_output_insn) == TYPE_VSSEGTUX ||
	  get_attr_type (current_output_insn) == TYPE_VSSEGTOX)
	{
	  fputs ("th.vsxseg", asm_out_file);
	  asm_fprintf (asm_out_file, "%c", p[7]);
	  fputs ("e", asm_out_file);
	  if (strstr (p, "ei8"))
	    return p+11;
	  else
	    return p+12;
	}

      if (get_attr_type (current_output_insn) == TYPE_VNSHIFT)
	{
	  if (strstr (p, "vncvt"))
	    {
	      fputs ("th.vncvt.x.x.v", asm_out_file);
	      return p+11;
	    }

	  strstr (p, "vnsrl") ? fputs ("th.vnsrl.v", asm_out_file)
			      : fputs ("th.vnsra.v", asm_out_file);
	  return p+7;
	}

      if (get_attr_type (current_output_insn) == TYPE_VNCLIP)
	{
	  if (strstr (p, "vnclipu"))
	    {
	      fputs ("th.vnclipu.v", asm_out_file);
	      return p+9;
	    }
	  else
	    {
	      fputs ("th.vnclip.v", asm_out_file);
	      return p+8;
	    }
	}

      if (get_attr_type (current_output_insn) == TYPE_VMPOP)
	{
	  fputs ("th.vmpopc", asm_out_file);
	  return p+5;
	}

      if (get_attr_type (current_output_insn) == TYPE_VMFFS)
	{
	  fputs ("th.vmfirst", asm_out_file);
	  return p+6;
	}

      if (get_attr_type (current_output_insn) == TYPE_VFNCVTFTOI ||
	  get_attr_type (current_output_insn) == TYPE_VFNCVTITOF)
	{
	  if (strstr (p, "xu"))
	    {
	      get_attr_type (current_output_insn) == TYPE_VFNCVTFTOI
			   ? fputs ("th.vfncvt.xu.f.v", asm_out_file)
			   : fputs ("th.vfncvt.f.xu.v", asm_out_file);
	      return p+13;
	}
	  else
	    {
	      get_attr_type (current_output_insn) == TYPE_VFNCVTFTOI
			   ? fputs ("th.vfncvt.x.f.v", asm_out_file)
			   : fputs ("th.vfncvt.f.x.v", asm_out_file);
	      return p+12;
	    }
	}

      if (get_attr_type (current_output_insn) == TYPE_VFNCVTFTOF)
	{
	  fputs ("th.vfncvt.f.f.v", asm_out_file);
	  return p+12;
	}

      if (get_attr_type (current_output_insn) == TYPE_VFREDU
	  && strstr (p, "sum"))
	{
	  fputs ("th.vfredsum", asm_out_file);
	  return p+9;
	}

      if (get_attr_type (current_output_insn) == TYPE_VFWREDU
	  && strstr (p, "sum"))
	{
	  fputs ("th.vfwredsum", asm_out_file);
	  return p+10;
	}

      if (p[0] == 'v')
	fputs ("th.", asm_out_file);
    }

  return p;
}

/* Implement TARGET_PRINT_OPERAND_ADDRESS for XTheadMemIdx.  */

bool
th_print_operand_address (FILE *file, machine_mode mode, rtx x)
{
  struct riscv_address_info addr;

  if (!th_classify_address (&addr, x, mode, reload_completed))
    return false;

  switch (addr.type)
    {
    case ADDRESS_REG_REG:
    case ADDRESS_REG_UREG:
      fprintf (file, "%s,%s,%u", reg_names[REGNO (addr.reg)],
	       reg_names[REGNO (addr.offset)], addr.shift);
      return true;

    case ADDRESS_REG_WB:
      fprintf (file, "(%s)," HOST_WIDE_INT_PRINT_DEC ",%u",
	       reg_names[REGNO (addr.reg)],
	       INTVAL (addr.offset) >> addr.shift, addr.shift);
	return true;

    default:
      gcc_unreachable ();
    }

  gcc_unreachable ();
}

/* Number array of registers X1, X5-X7, X10-X17, X28-X31, to be
   operated on by instruction th.ipush/th.ipop in XTheadInt.  */

int th_int_regs[] ={
  RETURN_ADDR_REGNUM,
  T0_REGNUM, T1_REGNUM, T2_REGNUM,
  A0_REGNUM, A1_REGNUM, A2_REGNUM, A3_REGNUM,
  A4_REGNUM, A5_REGNUM, A6_REGNUM, A7_REGNUM,
  T3_REGNUM, T4_REGNUM, T5_REGNUM, T6_REGNUM,
};

/* If MASK contains registers X1, X5-X7, X10-X17, X28-X31, then
   return the mask composed of these registers, otherwise return
   zero.  */

unsigned int
th_int_get_mask (unsigned int mask)
{
  unsigned int xtheadint_mask = 0;

  if (!TARGET_XTHEADINT || TARGET_64BIT)
    return 0;

  for (unsigned int i = 0; i < ARRAY_SIZE (th_int_regs); i++)
    {
      if (!BITSET_P (mask, th_int_regs[i]))
	return 0;

      xtheadint_mask |= (1 << th_int_regs[i]);
    }

  return xtheadint_mask; /* Usually 0xf003fce2.  */
}

/* Returns the occupied frame needed to save registers X1, X5-X7,
   X10-X17, X28-X31.  */

unsigned int
th_int_get_save_adjustment (void)
{
  gcc_assert (TARGET_XTHEADINT && !TARGET_64BIT);
  return ARRAY_SIZE (th_int_regs) * UNITS_PER_WORD;
}

rtx
th_int_adjust_cfi_prologue (unsigned int mask)
{
  gcc_assert (TARGET_XTHEADINT && !TARGET_64BIT);

  rtx dwarf = NULL_RTX;
  rtx adjust_sp_rtx, reg, mem, insn;
  int saved_size = ARRAY_SIZE (th_int_regs) * UNITS_PER_WORD;
  int offset = saved_size;

  for (int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (mask, regno - GP_REG_FIRST))
      {
	offset -= UNITS_PER_WORD;
	reg = gen_rtx_REG (SImode, regno);
	mem = gen_frame_mem (SImode, plus_constant (Pmode,
						    stack_pointer_rtx,
						    offset));

	insn = gen_rtx_SET (mem, reg);
	dwarf = alloc_reg_note (REG_CFA_OFFSET, insn, dwarf);
      }

  /* Debug info for adjust sp.  */
  adjust_sp_rtx =
    gen_rtx_SET (stack_pointer_rtx,
		 gen_rtx_PLUS (GET_MODE (stack_pointer_rtx),
			       stack_pointer_rtx, GEN_INT (-saved_size)));
  dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, adjust_sp_rtx, dwarf);

  return dwarf;
}
