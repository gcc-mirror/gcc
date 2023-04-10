/* Subroutines used for code generation for RISC-V.
   Copyright (C) 2023 Free Software Foundation, Inc.
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
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "poly-int.h"
#include "output.h"

/* If MEM is in the form of "base+offset", extract the two parts
   of address and set to BASE and OFFSET, otherwise return false
   after clearing BASE and OFFSET.  */

static bool
extract_base_offset_in_addr (rtx mem, rtx *base, rtx *offset)
{
  rtx addr;

  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  if (REG_P (addr))
    {
      *base = addr;
      *offset = const0_rtx;
      return true;
    }

  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && CONST_INT_P (XEXP (addr, 1)))
    {
      *base = XEXP (addr, 0);
      *offset = XEXP (addr, 1);
      return true;
    }

  *base = NULL_RTX;
  *offset = NULL_RTX;

  return false;
}

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
   swap them if they are not in ascending order.
   Return true if swap was performed.  */
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
  rtx insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set1, set2)));
  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_CFA_OFFSET, copy_rtx (set1));
  add_reg_note (insn, REG_CFA_OFFSET, copy_rtx (set2));
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
