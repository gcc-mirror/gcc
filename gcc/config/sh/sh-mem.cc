/* Helper routines for memory move and comparison insns.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "function.h"
#include "basic-block.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "emit-rtl.h"
#include "explow.h"
#include "expr.h"

/* Like force_operand, but guarantees that VALUE ends up in TARGET.  */
static void
force_into (rtx value, rtx target)
{
  value = force_operand (value, target);
  if (! rtx_equal_p (value, target))
    emit_insn (gen_move_insn (target, value));
}

/* Emit code to perform a block move.  Choose the best method.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.
   OPERANDS[2] is the size.
   OPERANDS[3] is the alignment safe to use.  */
bool
expand_block_move (rtx *operands)
{
  int align = INTVAL (operands[3]);
  int constp = (CONST_INT_P (operands[2]));
  int bytes = (constp ? INTVAL (operands[2]) : 0);

  if (! constp)
    return false;

  /* If we could use mov.l to move words and dest is word-aligned, we
     can use movua.l for loads and still generate a relatively short
     and efficient sequence.  */
  if (TARGET_SH4A && align < 4
      && MEM_ALIGN (operands[0]) >= 32
      && can_move_by_pieces (bytes, 32))
    {
      rtx dest = copy_rtx (operands[0]);
      rtx src = copy_rtx (operands[1]);
      /* We could use different pseudos for each copied word, but
	 since movua can only load into r0, it's kind of
	 pointless.  */
      rtx temp = gen_reg_rtx (SImode);
      rtx src_addr = copy_addr_to_reg (XEXP (src, 0));
      int copied = 0;

      while (copied + 4 <= bytes)
	{
	  rtx to = adjust_address (dest, SImode, copied);
	  rtx from = adjust_automodify_address (src, BLKmode,
						src_addr, copied);

	  set_mem_size (from, 4);
	  emit_insn (gen_movua (temp, from));
	  emit_move_insn (src_addr, plus_constant (Pmode, src_addr, 4));
	  emit_move_insn (to, temp);
	  copied += 4;
	}

      if (copied < bytes)
	move_by_pieces (adjust_address (dest, BLKmode, copied),
			adjust_automodify_address (src, BLKmode,
						   src_addr, copied),
			bytes - copied, align, 0);

      return true;
    }

  /* If it isn't a constant number of bytes, or if it doesn't have 4 byte
     alignment, or if it isn't a multiple of 4 bytes, then fail.  */
  if (align < 4 || (bytes % 4 != 0))
    return false;

  if (TARGET_HARD_SH4)
    {
      if (bytes < 12)
	return false;
      else if (bytes == 12)
	{
	  rtx func_addr_rtx = gen_reg_rtx (Pmode);
	  rtx r4 = gen_rtx_REG (SImode, 4);
	  rtx r5 = gen_rtx_REG (SImode, 5);

	  rtx lab = function_symbol (func_addr_rtx, "__movmemSI12_i4",
				     SFUNC_STATIC).lab;
	  force_into (XEXP (operands[0], 0), r4);
	  force_into (XEXP (operands[1], 0), r5);
	  emit_insn (gen_block_move_real_i4 (func_addr_rtx, lab));
	  return true;
	}
      else if (! optimize_size)
	{
	  rtx func_addr_rtx = gen_reg_rtx (Pmode);
	  rtx r4 = gen_rtx_REG (SImode, 4);
	  rtx r5 = gen_rtx_REG (SImode, 5);
	  rtx r6 = gen_rtx_REG (SImode, 6);

	  rtx lab = function_symbol (func_addr_rtx, bytes & 4
						    ? "__movmem_i4_odd"
						    : "__movmem_i4_even",
				     SFUNC_STATIC).lab;
	  force_into (XEXP (operands[0], 0), r4);
	  force_into (XEXP (operands[1], 0), r5);

	  int dwords = bytes >> 3;
	  emit_insn (gen_move_insn (r6, GEN_INT (dwords - 1)));
	  emit_insn (gen_block_lump_real_i4 (func_addr_rtx, lab));
	  return true;
	}
      else
	return false;
    }
  if (bytes < 64)
    {
      char entry[30];
      rtx func_addr_rtx = gen_reg_rtx (Pmode);
      rtx r4 = gen_rtx_REG (SImode, 4);
      rtx r5 = gen_rtx_REG (SImode, 5);

      sprintf (entry, "__movmemSI%d", bytes);
      rtx lab = function_symbol (func_addr_rtx, entry, SFUNC_STATIC).lab;
      force_into (XEXP (operands[0], 0), r4);
      force_into (XEXP (operands[1], 0), r5);
      emit_insn (gen_block_move_real (func_addr_rtx, lab));
      return true;
    }

  /* This is the same number of bytes as a memcpy call, but to a different
     less common function name, so this will occasionally use more space.  */
  if (! optimize_size)
    {
      rtx func_addr_rtx = gen_reg_rtx (Pmode);
      int final_switch, while_loop;
      rtx r4 = gen_rtx_REG (SImode, 4);
      rtx r5 = gen_rtx_REG (SImode, 5);
      rtx r6 = gen_rtx_REG (SImode, 6);

      rtx lab = function_symbol (func_addr_rtx, "__movmem", SFUNC_STATIC).lab;
      force_into (XEXP (operands[0], 0), r4);
      force_into (XEXP (operands[1], 0), r5);

      /* r6 controls the size of the move.  16 is decremented from it
	 for each 64 bytes moved.  Then the negative bit left over is used
	 as an index into a list of move instructions.  e.g., a 72 byte move
	 would be set up with size(r6) = 14, for one iteration through the
	 big while loop, and a switch of -2 for the last part.  */

      final_switch = 16 - ((bytes / 4) % 16);
      while_loop = ((bytes / 4) / 16 - 1) * 16;
      emit_insn (gen_move_insn (r6, GEN_INT (while_loop + final_switch)));
      emit_insn (gen_block_lump_real (func_addr_rtx, lab));
      return true;
    }

  return false;
}

static const int prob_unlikely = REG_BR_PROB_BASE / 10;
static const int prob_likely = REG_BR_PROB_BASE / 4;

/* Emit code to perform a strcmp.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the first string.
   OPERANDS[2] is the second string.
   OPERANDS[3] is the known alignment.  */
bool
sh_expand_cmpstr (rtx *operands)
{
  rtx addr1 = operands[1];
  rtx addr2 = operands[2];
  rtx s1_addr = copy_addr_to_reg (XEXP (addr1, 0));
  rtx s2_addr = copy_addr_to_reg (XEXP (addr2, 0));
  rtx tmp0 = gen_reg_rtx (SImode);
  rtx tmp1 = gen_reg_rtx (SImode);
  rtx tmp2 = gen_reg_rtx (SImode);
  rtx tmp3 = gen_reg_rtx (SImode);

  rtx_insn *jump;
  rtx_code_label *L_return = gen_label_rtx ();
  rtx_code_label *L_loop_byte = gen_label_rtx ();
  rtx_code_label *L_end_loop_byte = gen_label_rtx ();
  rtx_code_label *L_loop_long = gen_label_rtx ();
  rtx_code_label *L_end_loop_long = gen_label_rtx ();

  const unsigned int addr1_alignment = MEM_ALIGN (operands[1]) / BITS_PER_UNIT;
  const unsigned int addr2_alignment = MEM_ALIGN (operands[2]) / BITS_PER_UNIT;

  if (addr1_alignment < 4 && addr2_alignment < 4)
    {
      emit_insn (gen_iorsi3 (tmp1, s1_addr, s2_addr));
      emit_insn (gen_tstsi_t (tmp1, GEN_INT (3)));
      jump = emit_jump_insn (gen_branch_false (L_loop_byte));
      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
    }
  else if (addr1_alignment < 4 && addr2_alignment >= 4)
    {
      emit_insn (gen_tstsi_t (s1_addr, GEN_INT (3)));
      jump = emit_jump_insn (gen_branch_false (L_loop_byte));
      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
    }
  else if (addr1_alignment >= 4 && addr2_alignment < 4)
    {
      emit_insn (gen_tstsi_t (s2_addr, GEN_INT (3)));
      jump = emit_jump_insn (gen_branch_false (L_loop_byte));
      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
    }

  addr1 = adjust_automodify_address (addr1, SImode, s1_addr, 0);
  addr2 = adjust_automodify_address (addr2, SImode, s2_addr, 0);

  /* tmp2 is aligned, OK to load.  */
  emit_move_insn (tmp3, addr2);
  emit_move_insn (s2_addr, plus_constant (Pmode, s2_addr, 4));

  /* start long loop.  */
  emit_label (L_loop_long);

  emit_move_insn (tmp2, tmp3);

  /* tmp1 is aligned, OK to load.  */
  emit_move_insn (tmp1, addr1);
  emit_move_insn (s1_addr, plus_constant (Pmode, s1_addr, 4));

  /* Is there a 0 byte ?  */
  emit_insn (gen_andsi3 (tmp3, tmp3, tmp1));

  emit_insn (gen_cmpstr_t (tmp0, tmp3));
  jump = emit_jump_insn (gen_branch_true (L_end_loop_long));
  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

  emit_insn (gen_cmpeqsi_t (tmp1, tmp2));

  /* tmp2 is aligned, OK to load.  */
  emit_move_insn (tmp3, addr2);
  emit_move_insn (s2_addr, plus_constant (Pmode, s2_addr, 4));

  jump = emit_jump_insn (gen_branch_true (L_loop_long));
  add_int_reg_note (jump, REG_BR_PROB, prob_likely);
  /* end loop.  */

  /* Fallthu, substract words.  */
  if (TARGET_LITTLE_ENDIAN)
    {
      rtx low_1 = gen_lowpart (HImode, tmp1);
      rtx low_2 = gen_lowpart (HImode, tmp2);

      emit_insn (gen_rotlhi3_8 (low_1, low_1));
      emit_insn (gen_rotlhi3_8 (low_2, low_2));
      emit_insn (gen_rotlsi3_16 (tmp1, tmp1));
      emit_insn (gen_rotlsi3_16 (tmp2, tmp2));
      emit_insn (gen_rotlhi3_8 (low_1, low_1));
      emit_insn (gen_rotlhi3_8 (low_2, low_2));
    }

  jump = emit_jump_insn (gen_jump_compact (L_return));
  emit_barrier_after (jump);

  emit_label (L_end_loop_long);

  emit_move_insn (s1_addr, plus_constant (Pmode, s1_addr, -4));
  emit_move_insn (s2_addr, plus_constant (Pmode, s2_addr, -4));

  /* start byte loop.  */
  addr1 = adjust_address (addr1, QImode, 0);
  addr2 = adjust_address (addr2, QImode, 0);

  emit_label (L_loop_byte);

  emit_insn (gen_extendqisi2 (tmp2, addr2));
  emit_move_insn (s2_addr, plus_constant (Pmode, s2_addr, 1));

  emit_insn (gen_extendqisi2 (tmp1, addr1));
  emit_move_insn (s1_addr, plus_constant (Pmode, s1_addr, 1));

  emit_insn (gen_cmpeqsi_t (tmp2, const0_rtx));
  jump = emit_jump_insn (gen_branch_true (L_end_loop_byte));
  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

  emit_insn (gen_cmpeqsi_t (tmp1, tmp2));
  if (flag_delayed_branch)
    emit_insn (gen_zero_extendqisi2 (tmp2, gen_lowpart (QImode, tmp2)));
  jump = emit_jump_insn (gen_branch_true (L_loop_byte));
  add_int_reg_note (jump, REG_BR_PROB, prob_likely);
  /* end loop.  */

  emit_label (L_end_loop_byte);

  if (! flag_delayed_branch)
    emit_insn (gen_zero_extendqisi2 (tmp2, gen_lowpart (QImode, tmp2)));
  emit_insn (gen_zero_extendqisi2 (tmp1, gen_lowpart (QImode, tmp1)));

  emit_label (L_return);

  emit_insn (gen_subsi3 (operands[0], tmp1, tmp2));

  return true;
}

/* Emit code to perform a strncmp.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the first string.
   OPERANDS[2] is the second string.
   OPERANDS[3] is the length.
   OPERANDS[4] is the known alignment.  */
bool
sh_expand_cmpnstr (rtx *operands)
{
  rtx addr1 = operands[1];
  rtx addr2 = operands[2];
  rtx s1_addr = copy_addr_to_reg (XEXP (addr1, 0));
  rtx s2_addr = copy_addr_to_reg (XEXP (addr2, 0));
  rtx tmp1 = gen_reg_rtx (SImode);
  rtx tmp2 = gen_reg_rtx (SImode);

  rtx_insn *jump;
  rtx_code_label *L_return = gen_label_rtx ();
  rtx_code_label *L_loop_byte = gen_label_rtx ();
  rtx_code_label *L_end_loop_byte = gen_label_rtx ();

  rtx len = copy_to_mode_reg (SImode, operands[3]);
  int constp = CONST_INT_P (operands[3]);
  HOST_WIDE_INT bytes = constp ? INTVAL (operands[3]) : 0;

  const unsigned int addr1_alignment = MEM_ALIGN (operands[1]) / BITS_PER_UNIT;
  const unsigned int addr2_alignment = MEM_ALIGN (operands[2]) / BITS_PER_UNIT;

  /* Loop on a register count.  */
  if (constp && bytes >= 0 && bytes < 32)
    {
      rtx tmp0 = gen_reg_rtx (SImode);
      rtx tmp3 = gen_reg_rtx (SImode);
      rtx lenw = gen_reg_rtx (SImode);

      rtx_code_label *L_loop_long = gen_label_rtx ();
      rtx_code_label *L_end_loop_long = gen_label_rtx ();

      int witers = bytes / 4;

      if (witers > 1)
	{
	  addr1 = adjust_automodify_address (addr1, SImode, s1_addr, 0);
	  addr2 = adjust_automodify_address (addr2, SImode, s2_addr, 0);

	  emit_move_insn (tmp0, const0_rtx);

	  if (addr1_alignment < 4 && addr2_alignment < 4)
	    {
	      emit_insn (gen_iorsi3 (tmp1, s1_addr, s2_addr));
	      emit_insn (gen_tstsi_t (tmp1, GEN_INT (3)));
	      jump = emit_jump_insn (gen_branch_false (L_loop_byte));
	      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
	    }
	  else if (addr1_alignment < 4 && addr2_alignment >= 4)
	    {
	      emit_insn (gen_tstsi_t (s1_addr, GEN_INT (3)));
	      jump = emit_jump_insn (gen_branch_false (L_loop_byte));
	      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
	    }
	  else if (addr1_alignment >= 4 && addr2_alignment < 4)
	    {
	      emit_insn (gen_tstsi_t (s2_addr, GEN_INT (3)));
	      jump = emit_jump_insn (gen_branch_false (L_loop_byte));
	      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
	    }

	  /* word count. Do we have iterations ?  */
	  emit_insn (gen_lshrsi3 (lenw, len, GEN_INT (2)));

	  /* start long loop.  */
	  emit_label (L_loop_long);

	  /* tmp2 is aligned, OK to load.  */
	  emit_move_insn (tmp2, addr2);
	  emit_move_insn (s2_addr, plus_constant (Pmode, s2_addr,
						  GET_MODE_SIZE (SImode)));

	  /* tmp1 is aligned, OK to load.  */
	  emit_move_insn (tmp1, addr1);
	  emit_move_insn (s1_addr, plus_constant (Pmode, s1_addr,
						  GET_MODE_SIZE (SImode)));

	  /* Is there a 0 byte ?  */
	  emit_insn (gen_andsi3 (tmp3, tmp2, tmp1));

	  emit_insn (gen_cmpstr_t (tmp0, tmp3));
	  jump = emit_jump_insn (gen_branch_true (L_end_loop_long));
	  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

	  emit_insn (gen_cmpeqsi_t (tmp1, tmp2));
	  jump = emit_jump_insn (gen_branch_false (L_end_loop_long));
	  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

	  if (TARGET_SH2)
	    emit_insn (gen_dect (lenw, lenw));
	  else
	    {
	      emit_insn (gen_addsi3 (lenw, lenw, GEN_INT (-1)));
	      emit_insn (gen_tstsi_t (lenw, lenw));
	    }

	  jump = emit_jump_insn (gen_branch_false (L_loop_long));
	  add_int_reg_note (jump, REG_BR_PROB, prob_likely);

	  int sbytes = bytes % 4;

	  /* end loop.  Reached max iterations.  */
	  if (sbytes == 0)
	    {
	      emit_insn (gen_subsi3 (operands[0], tmp1, tmp2));
	      jump = emit_jump_insn (gen_jump_compact (L_return));
	      emit_barrier_after (jump);
	    }
	  else
	    {
	      /* Remaining bytes to check.  */

	      addr1 = adjust_automodify_address (addr1, QImode, s1_addr, 0);
	      addr2 = adjust_automodify_address (addr2, QImode, s2_addr, 0);

	      while (sbytes--)
		{
		  emit_insn (gen_extendqisi2 (tmp1, addr1));
		  emit_insn (gen_extendqisi2 (tmp2, addr2));

		  emit_insn (gen_cmpeqsi_t (tmp2, const0_rtx));
		  jump = emit_jump_insn (gen_branch_true (L_end_loop_byte));
		  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

		  emit_insn (gen_cmpeqsi_t (tmp1, tmp2));
		  if (flag_delayed_branch)
		    emit_insn (gen_zero_extendqisi2 (tmp2,
						     gen_lowpart (QImode,
								  tmp2)));
		  jump = emit_jump_insn (gen_branch_false (L_end_loop_byte));
		  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

		  addr1 = adjust_address (addr1, QImode,
					  GET_MODE_SIZE (QImode));
		  addr2 = adjust_address (addr2, QImode,
					  GET_MODE_SIZE (QImode));
		}

	      jump = emit_jump_insn (gen_jump_compact( L_end_loop_byte));
	      emit_barrier_after (jump);
	    }

	  emit_label (L_end_loop_long);

	  /* Found last word.  Restart it byte per byte.  */

	  emit_move_insn (s1_addr, plus_constant (Pmode, s1_addr,
						  -GET_MODE_SIZE (SImode)));
	  emit_move_insn (s2_addr, plus_constant (Pmode, s2_addr,
						  -GET_MODE_SIZE (SImode)));

	  /* fall thru.  */
	}

      addr1 = adjust_automodify_address (addr1, QImode, s1_addr, 0);
      addr2 = adjust_automodify_address (addr2, QImode, s2_addr, 0);

      while (bytes--)
	{
	  emit_insn (gen_extendqisi2 (tmp1, addr1));
	  emit_insn (gen_extendqisi2 (tmp2, addr2));

	  emit_insn (gen_cmpeqsi_t (tmp2, const0_rtx));
	  jump = emit_jump_insn (gen_branch_true (L_end_loop_byte));
	  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

	  emit_insn (gen_cmpeqsi_t (tmp1, tmp2));
	  if (flag_delayed_branch)
	    emit_insn (gen_zero_extendqisi2 (tmp2,
					     gen_lowpart (QImode, tmp2)));
	  jump = emit_jump_insn (gen_branch_false (L_end_loop_byte));
	  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

	  addr1 = adjust_address (addr1, QImode, GET_MODE_SIZE (QImode));
	  addr2 = adjust_address (addr2, QImode, GET_MODE_SIZE (QImode));
	}

      jump = emit_jump_insn (gen_jump_compact( L_end_loop_byte));
      emit_barrier_after (jump);
    }
  else
    {
      emit_insn (gen_cmpeqsi_t (len, const0_rtx));
      emit_move_insn (operands[0], const0_rtx);
      jump = emit_jump_insn (gen_branch_true (L_return));
      add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);
    }

  addr1 = adjust_automodify_address (addr1, QImode, s1_addr, 0);
  addr2 = adjust_automodify_address (addr2, QImode, s2_addr, 0);

  emit_label (L_loop_byte);

  emit_insn (gen_extendqisi2 (tmp2, addr2));
  emit_move_insn (s2_addr, plus_constant (Pmode, s2_addr, 1));

  emit_insn (gen_extendqisi2 (tmp1, addr1));
  emit_move_insn (s1_addr, plus_constant (Pmode, s1_addr, 1));

  emit_insn (gen_cmpeqsi_t (tmp2, const0_rtx));
  jump = emit_jump_insn (gen_branch_true (L_end_loop_byte));
  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

  emit_insn (gen_cmpeqsi_t (tmp1, tmp2));
  if (flag_delayed_branch)
    emit_insn (gen_zero_extendqisi2 (tmp2, gen_lowpart (QImode, tmp2)));
  jump = emit_jump_insn (gen_branch_false (L_end_loop_byte));
  add_int_reg_note (jump, REG_BR_PROB, prob_unlikely);

  if (TARGET_SH2)
    emit_insn (gen_dect (len, len));
  else
    {
      emit_insn (gen_addsi3 (len, len, GEN_INT (-1)));
      emit_insn (gen_tstsi_t (len, len));
    }

  jump = emit_jump_insn (gen_branch_false (L_loop_byte));
  add_int_reg_note (jump, REG_BR_PROB, prob_likely);
  /* end byte loop.  */

  emit_label (L_end_loop_byte);

  if (! flag_delayed_branch)
    emit_insn (gen_zero_extendqisi2 (tmp2, gen_lowpart (QImode, tmp2)));
  emit_insn (gen_zero_extendqisi2 (tmp1, gen_lowpart (QImode, tmp1)));

  emit_insn (gen_subsi3 (operands[0], tmp1, tmp2));

  emit_label (L_return);

  return true;
}

/* Emit code to perform a strlen.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the string.
   OPERANDS[2] is the char to search.
   OPERANDS[3] is the alignment.  */
bool
sh_expand_strlen (rtx *operands)
{
  rtx addr1 = operands[1];
  rtx current_addr = copy_addr_to_reg (XEXP (addr1, 0));
  rtx start_addr = gen_reg_rtx (Pmode);
  rtx tmp0 = gen_reg_rtx (SImode);
  rtx tmp1 = gen_reg_rtx (SImode);
  rtx_code_label *L_return = gen_label_rtx ();
  rtx_code_label *L_loop_byte = gen_label_rtx ();

  rtx_insn *jump;
  rtx_code_label *L_loop_long = gen_label_rtx ();
  rtx_code_label *L_end_loop_long = gen_label_rtx ();

  int align = INTVAL (operands[3]);

  emit_move_insn (operands[0], GEN_INT (-1));

  /* remember start of string.  */
  emit_move_insn (start_addr, current_addr);

  if (align < 4)
    {
      emit_insn (gen_tstsi_t (current_addr, GEN_INT (3)));
      jump = emit_jump_insn (gen_branch_false (L_loop_byte));
      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
    }

  emit_move_insn (tmp0, operands[2]);

  addr1 = adjust_automodify_address (addr1, SImode, current_addr, 0);

  /* start long loop.  */
  emit_label (L_loop_long);

  /* tmp1 is aligned, OK to load.  */
  emit_move_insn (tmp1, addr1);
  emit_move_insn (current_addr, plus_constant (Pmode, current_addr, 4));

  /* Is there a 0 byte ?  */
  emit_insn (gen_cmpstr_t (tmp0, tmp1));

  jump = emit_jump_insn (gen_branch_false (L_loop_long));
  add_int_reg_note (jump, REG_BR_PROB, prob_likely);
  /* end loop.  */

  emit_label (L_end_loop_long);

  emit_move_insn (current_addr, plus_constant (Pmode, current_addr, -4));

  addr1 = adjust_address (addr1, QImode, 0);

  /* unroll remaining bytes.  */
  for (int i = 0; i < 4; ++i)
    {
      emit_insn (gen_extendqisi2 (tmp1, addr1));
      emit_move_insn (current_addr, plus_constant (Pmode, current_addr, 1));
      emit_insn (gen_cmpeqsi_t (tmp1, const0_rtx));
      jump = emit_jump_insn (gen_branch_true (L_return));
      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
    }

  emit_barrier_after (jump);

  /* start byte loop.  */
  emit_label (L_loop_byte);

  emit_insn (gen_extendqisi2 (tmp1, addr1));
  emit_move_insn (current_addr, plus_constant (Pmode, current_addr, 1));

  emit_insn (gen_cmpeqsi_t (tmp1, const0_rtx));
  jump = emit_jump_insn (gen_branch_false (L_loop_byte));
  add_int_reg_note (jump, REG_BR_PROB, prob_likely);

  /* end loop.  */

  emit_label (L_return);

  emit_insn (gen_addsi3 (start_addr, start_addr, GEN_INT (1)));
  emit_insn (gen_subsi3 (operands[0], current_addr, start_addr));

  return true;
}

/* Emit code to perform a memset.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the size;
   OPERANDS[2] is the char to search.
   OPERANDS[3] is the alignment.  */
void
sh_expand_setmem (rtx *operands)
{
  rtx_code_label *L_loop_byte = gen_label_rtx ();
  rtx_code_label *L_loop_word = gen_label_rtx ();
  rtx_code_label *L_return = gen_label_rtx ();
  rtx_insn *jump;
  rtx dest = copy_rtx (operands[0]);
  rtx dest_addr = copy_addr_to_reg (XEXP (dest, 0));
  rtx val = copy_to_mode_reg (SImode, operands[2]);
  int align = INTVAL (operands[3]);
  rtx len = copy_to_mode_reg (SImode, operands[1]);

  if (! CONST_INT_P (operands[1]))
    return;

  int count = INTVAL (operands[1]);

  if (CONST_INT_P (operands[2])
      && (INTVAL (operands[2]) == 0 || INTVAL (operands[2]) == -1) && count > 8)
    {
      rtx lenw = gen_reg_rtx (SImode);

      if (align < 4)
	{
	  emit_insn (gen_tstsi_t (dest_addr, GEN_INT (3)));
	  jump = emit_jump_insn (gen_branch_false (L_loop_byte));
	  add_int_reg_note (jump, REG_BR_PROB, prob_likely);
	}

      /* word count. Do we have iterations ?  */
      emit_insn (gen_lshrsi3 (lenw, len, GEN_INT (2)));

      dest = adjust_automodify_address (dest, SImode, dest_addr, 0);

      /* start loop.  */
      emit_label (L_loop_word);

      if (TARGET_SH2)
        emit_insn (gen_dect (lenw, lenw));
      else
	{
	  emit_insn (gen_addsi3 (lenw, lenw, GEN_INT (-1)));
	  emit_insn (gen_tstsi_t (lenw, lenw));
	}

      emit_move_insn (dest, val);
      emit_move_insn (dest_addr, plus_constant (Pmode, dest_addr,
						GET_MODE_SIZE (SImode)));


      jump = emit_jump_insn (gen_branch_false (L_loop_word));
      add_int_reg_note (jump, REG_BR_PROB, prob_likely);
      count = count % 4;

      dest = adjust_address (dest, QImode, 0);

      val = gen_lowpart (QImode, val);

      while (count--)
	{
	  emit_move_insn (dest, val);
	  emit_move_insn (dest_addr, plus_constant (Pmode, dest_addr,
						    GET_MODE_SIZE (QImode)));
	}

      jump = emit_jump_insn (gen_jump_compact (L_return));
      emit_barrier_after (jump);
    }

  dest = adjust_automodify_address (dest, QImode, dest_addr, 0);

  /* start loop.  */
  emit_label (L_loop_byte);

  if (TARGET_SH2)
    emit_insn (gen_dect (len, len));
  else
    {
      emit_insn (gen_addsi3 (len, len, GEN_INT (-1)));
      emit_insn (gen_tstsi_t (len, len));
    }

  val = gen_lowpart (QImode, val);
  emit_move_insn (dest, val);
  emit_move_insn (dest_addr, plus_constant (Pmode, dest_addr,
                                            GET_MODE_SIZE (QImode)));

  jump = emit_jump_insn (gen_branch_false (L_loop_byte));
  add_int_reg_note (jump, REG_BR_PROB, prob_likely);

  emit_label (L_return);
}
