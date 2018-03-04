/* Auxiliary functions for expand movmem, setmem, cmpmem, load_multiple
   and store_multiple pattern of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* ------------------------------------------------------------------------ */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "explow.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "nds32-protos.h"

/* ------------------------------------------------------------------------ */

/* Auxiliary static function definitions.  */

static void
nds32_emit_load_store (rtx reg, rtx mem,
		       enum machine_mode mode,
		       int offset, bool load_p)
{
  rtx new_mem;
  new_mem = adjust_address (mem, mode, offset);
  if (load_p)
    emit_move_insn (reg, new_mem);
  else
    emit_move_insn (new_mem, reg);
}

static void
nds32_emit_post_inc_load_store (rtx reg, rtx base_reg,
				enum machine_mode mode,
				bool load_p)
{
  gcc_assert (GET_MODE (reg) == mode);
  gcc_assert (GET_MODE (base_reg) == Pmode);

  /* Do not gen (set (reg) (mem (post_inc (reg)))) directly here since it may
     not recognize by gcc, so let gcc combine it at auto_inc_dec pass.  */
  if (load_p)
    emit_move_insn (reg,
		    gen_rtx_MEM (mode,
				 base_reg));
  else
    emit_move_insn (gen_rtx_MEM (mode,
				 base_reg),
		    reg);

  emit_move_insn (base_reg,
		  plus_constant(Pmode, base_reg, GET_MODE_SIZE (mode)));
}

static void
nds32_emit_mem_move (rtx src, rtx dst,
		     enum machine_mode mode,
		     int addr_offset)
{
  gcc_assert (MEM_P (src) && MEM_P (dst));
  rtx tmp_reg = gen_reg_rtx (mode);
  nds32_emit_load_store (tmp_reg, src, mode,
			 addr_offset, /* load_p */ true);
  nds32_emit_load_store (tmp_reg, dst, mode,
			 addr_offset, /* load_p */ false);
}

static void
nds32_emit_mem_move_block (int base_regno, int count,
			   rtx *dst_base_reg, rtx *dst_mem,
			   rtx *src_base_reg, rtx *src_mem,
			   bool update_base_reg_p)
{
  rtx new_base_reg;

  emit_insn (nds32_expand_load_multiple (base_regno, count,
					 *src_base_reg, *src_mem,
					 update_base_reg_p, &new_base_reg));
  if (update_base_reg_p)
    {
      *src_base_reg = new_base_reg;
      *src_mem = gen_rtx_MEM (SImode, *src_base_reg);
    }

  emit_insn (nds32_expand_store_multiple (base_regno, count,
					  *dst_base_reg, *dst_mem,
					  update_base_reg_p, &new_base_reg));

  if (update_base_reg_p)
    {
      *dst_base_reg = new_base_reg;
      *dst_mem = gen_rtx_MEM (SImode, *dst_base_reg);
    }
}

/* ------------------------------------------------------------------------ */

/* Auxiliary function for expand movmem pattern.  */

static bool
nds32_expand_movmemsi_loop_unknown_size (rtx dstmem, rtx srcmem,
					 rtx size,
					 rtx alignment)
{
  /* Emit loop version of movmem.

       andi    $size_least_3_bit, $size, #~7
       add     $dst_end, $dst, $size
       move    $dst_itr, $dst
       move    $src_itr, $src
       beqz    $size_least_3_bit, .Lbyte_mode_entry ! Not large enough.
       add     $double_word_end, $dst, $size_least_3_bit

     .Ldouble_word_mode_loop:
       lmw.bim $tmp-begin, [$src_itr], $tmp-end, #0 ! $src_itr' = $src_itr
       smw.bim $tmp-begin, [$dst_itr], $tmp-end, #0 ! $dst_itr' = $dst_itr
       ! move will delete after register allocation
       move    $src_itr, $src_itr'
       move    $dst_itr, $dst_itr'
       ! Not readch upper bound. Loop.
       bne     $double_word_end, $dst_itr, .Ldouble_word_mode_loop

     .Lbyte_mode_entry:
       beq     $dst_itr, $dst_end, .Lend_label
     .Lbyte_mode_loop:
       lbi.bi  $tmp, [$src_itr], #1
       sbi.bi  $tmp, [$dst_itr], #1
       ! Not readch upper bound. Loop.
       bne     $dst_itr, $dst_end, .Lbyte_mode_loop
     .Lend_label:
  */
  rtx dst_base_reg, src_base_reg;
  rtx dst_itr, src_itr;
  rtx dstmem_m, srcmem_m, dst_itr_m, src_itr_m;
  rtx dst_end;
  rtx size_least_3_bit;
  rtx double_word_end;
  rtx double_word_mode_loop, byte_mode_entry, byte_mode_loop, end_label;
  rtx tmp;
  rtx mask_least_3_bit;
  int start_regno;
  bool align_to_4_bytes = (INTVAL (alignment) & 3) == 0;

  if (TARGET_ISA_V3M && !align_to_4_bytes)
    return 0;

  if (TARGET_REDUCED_REGS)
    start_regno = 2;
  else
    start_regno = 16;

  dst_itr = gen_reg_rtx (Pmode);
  src_itr = gen_reg_rtx (Pmode);
  dst_end = gen_reg_rtx (Pmode);
  tmp = gen_reg_rtx (QImode);
  mask_least_3_bit = GEN_INT (~7);

  double_word_mode_loop = gen_label_rtx ();
  byte_mode_entry = gen_label_rtx ();
  byte_mode_loop = gen_label_rtx ();
  end_label = gen_label_rtx ();

  dst_base_reg = copy_to_mode_reg (Pmode, XEXP (dstmem, 0));
  src_base_reg = copy_to_mode_reg (Pmode, XEXP (srcmem, 0));
  /* andi   $size_least_3_bit, $size, #~7 */
  size_least_3_bit = expand_binop (SImode, and_optab, size, mask_least_3_bit,
				   NULL_RTX, 0, OPTAB_WIDEN);
  /* add     $dst_end, $dst, $size */
  dst_end = expand_binop (Pmode, add_optab, dst_base_reg, size,
			  NULL_RTX, 0, OPTAB_WIDEN);

  /* move    $dst_itr, $dst
     move    $src_itr, $src */
  emit_move_insn (dst_itr, dst_base_reg);
  emit_move_insn (src_itr, src_base_reg);

  /* beqz    $size_least_3_bit, .Lbyte_mode_entry ! Not large enough. */
  emit_cmp_and_jump_insns (size_least_3_bit, const0_rtx, EQ, NULL,
			   SImode, 1, byte_mode_entry);
  /* add     $double_word_end, $dst, $size_least_3_bit */
  double_word_end = expand_binop (Pmode, add_optab,
				  dst_base_reg, size_least_3_bit,
				  NULL_RTX, 0, OPTAB_WIDEN);

  /* .Ldouble_word_mode_loop: */
  emit_label (double_word_mode_loop);
  /* lmw.bim $tmp-begin, [$src_itr], $tmp-end, #0 ! $src_itr' = $src_itr
     smw.bim $tmp-begin, [$dst_itr], $tmp-end, #0 ! $dst_itr' = $dst_itr */
  src_itr_m = src_itr;
  dst_itr_m = dst_itr;
  srcmem_m = srcmem;
  dstmem_m = dstmem;
  nds32_emit_mem_move_block (start_regno, 2,
			     &dst_itr_m, &dstmem_m,
			     &src_itr_m, &srcmem_m,
			     true);
  /* move    $src_itr, $src_itr'
     move    $dst_itr, $dst_itr' */
  emit_move_insn (dst_itr, dst_itr_m);
  emit_move_insn (src_itr, src_itr_m);

  /* ! Not readch upper bound. Loop.
     bne     $double_word_end, $dst_itr, .Ldouble_word_mode_loop */
  emit_cmp_and_jump_insns (double_word_end, dst_itr, NE, NULL,
			   Pmode, 1, double_word_mode_loop);
  /* .Lbyte_mode_entry: */
  emit_label (byte_mode_entry);

  /* beq     $dst_itr, $dst_end, .Lend_label */
  emit_cmp_and_jump_insns (dst_itr, dst_end, EQ, NULL,
			   Pmode, 1, end_label);
  /* .Lbyte_mode_loop: */
  emit_label (byte_mode_loop);

  /* lbi.bi  $tmp, [$src_itr], #1 */
  nds32_emit_post_inc_load_store (tmp, src_itr, QImode, true);

  /* sbi.bi  $tmp, [$dst_itr], #1 */
  nds32_emit_post_inc_load_store (tmp, dst_itr, QImode, false);
  /* ! Not readch upper bound. Loop.
     bne     $dst_itr, $dst_end, .Lbyte_mode_loop */
  emit_cmp_and_jump_insns (dst_itr, dst_end, NE, NULL,
			   SImode, 1, byte_mode_loop);

  /* .Lend_label: */
  emit_label (end_label);

  return true;
}

static bool
nds32_expand_movmemsi_loop_known_size (rtx dstmem, rtx srcmem,
				       rtx size, rtx alignment)
{
  return nds32_expand_movmemsi_loop_unknown_size (dstmem, srcmem,
						  size, alignment);
}

static bool
nds32_expand_movmemsi_loop (rtx dstmem, rtx srcmem,
			    rtx size, rtx alignment)
{
  if (CONST_INT_P (size))
    return nds32_expand_movmemsi_loop_known_size (dstmem, srcmem,
						  size, alignment);
  else
    return nds32_expand_movmemsi_loop_unknown_size (dstmem, srcmem,
						    size, alignment);
}

static bool
nds32_expand_movmemsi_unroll (rtx dstmem, rtx srcmem,
			      rtx total_bytes, rtx alignment)
{
  rtx dst_base_reg, src_base_reg;
  rtx tmp_reg;
  int maximum_bytes;
  int maximum_bytes_per_inst;
  int maximum_regs;
  int start_regno;
  int i, inst_num;
  HOST_WIDE_INT remain_bytes, remain_words;
  bool align_to_4_bytes = (INTVAL (alignment) & 3) == 0;
  bool align_to_2_bytes = (INTVAL (alignment) & 1) == 0;

  /* Because reduced-set regsiters has few registers
     (r0~r5, r6~10, r15, r28~r31, where 'r15' and 'r28~r31'
      cannot be used for register allocation),
     using 8 registers (32 bytes) for moving memory block
     may easily consume all of them.
     It makes register allocation/spilling hard to work.
     So we only allow maximum=4 registers (16 bytes) for
     moving memory block under reduced-set registers.  */
  if (TARGET_REDUCED_REGS)
    {
      maximum_regs  = 4;
      maximum_bytes = 64;
      start_regno   = 2;
    }
  else
    {
      /* $r25 is $tp so we use up to 8 registers.  */
      maximum_regs  = 8;
      maximum_bytes = 160;
      start_regno   = 16;
    }
  maximum_bytes_per_inst = maximum_regs * UNITS_PER_WORD;

  /* 1. Total_bytes is integer for sure.
     2. Alignment is integer for sure.
     3. Maximum 4 or 10 registers and up to 4 instructions,
	4 * 4 * 4 = 64 bytes, 8 * 4 * 10 = 160 bytes.
     4. The dstmem cannot be volatile memory access.
     5. The srcmem cannot be volatile memory access.
     6. Known shared alignment not align to 4 byte in v3m since lmw/smw *NOT*
	support unalign access with v3m configure.  */
  if (GET_CODE (total_bytes) != CONST_INT
      || GET_CODE (alignment) != CONST_INT
      || INTVAL (total_bytes) > maximum_bytes
      || MEM_VOLATILE_P (dstmem)
      || MEM_VOLATILE_P (srcmem)
      || (TARGET_ISA_V3M && !align_to_4_bytes))
    return false;

  dst_base_reg = copy_to_mode_reg (SImode, XEXP (dstmem, 0));
  src_base_reg = copy_to_mode_reg (SImode, XEXP (srcmem, 0));
  remain_bytes = INTVAL (total_bytes);

  /* Do not update base address for last lmw/smw pair.  */
  inst_num = ((INTVAL (total_bytes) + (maximum_bytes_per_inst - 1))
	      / maximum_bytes_per_inst) - 1;

  for (i = 0; i < inst_num; i++)
    {
      nds32_emit_mem_move_block (start_regno, maximum_regs,
				 &dst_base_reg, &dstmem,
				 &src_base_reg, &srcmem,
				 true);
    }
  remain_bytes -= maximum_bytes_per_inst * inst_num;

  remain_words = remain_bytes / UNITS_PER_WORD;
  remain_bytes = remain_bytes - (remain_words * UNITS_PER_WORD);

  if (remain_words != 0)
    {
      if (remain_bytes != 0)
	nds32_emit_mem_move_block (start_regno, remain_words,
				   &dst_base_reg, &dstmem,
				   &src_base_reg, &srcmem,
				   true);
      else
	{
	  /* Do not update address if no further byte to move.  */
	  if (remain_words == 1)
	   {
	      /* emit move instruction if align to 4 byte and only 1
		 word to move.  */
	      if (align_to_4_bytes)
		nds32_emit_mem_move (srcmem, dstmem, SImode, 0);
	      else
		{
		  tmp_reg = gen_reg_rtx (SImode);
		  emit_insn (
		    gen_unaligned_load_w (tmp_reg,
					  gen_rtx_MEM (SImode, src_base_reg)));
		  emit_insn (
		    gen_unaligned_store_w (gen_rtx_MEM (SImode, dst_base_reg),
					   tmp_reg));
		}
	    }
	  else
	    nds32_emit_mem_move_block (start_regno, remain_words,
				       &dst_base_reg, &dstmem,
				       &src_base_reg, &srcmem,
				       false);
	}
    }

  switch (remain_bytes)
    {
    case 3:
    case 2:
      {
	if (align_to_2_bytes)
	  nds32_emit_mem_move (srcmem, dstmem, HImode, 0);
	else
	  {
	    nds32_emit_mem_move (srcmem, dstmem, QImode, 0);
	    nds32_emit_mem_move (srcmem, dstmem, QImode, 1);
	  }

	if (remain_bytes == 3)
	  nds32_emit_mem_move (srcmem, dstmem, QImode, 2);
	break;
      }
    case 1:
      nds32_emit_mem_move (srcmem, dstmem, QImode, 0);
      break;
    case 0:
      break;
    default:
      gcc_unreachable ();
    }

  /* Successfully create patterns, return true.  */
  return true;
}

/* Function to move block memory content by
   using load_multiple and store_multiple.
   This is auxiliary extern function to help create rtx template.
   Check nds32-multiple.md file for the patterns.  */
bool
nds32_expand_movmemsi (rtx dstmem, rtx srcmem, rtx total_bytes, rtx alignment)
{
  if (nds32_expand_movmemsi_unroll (dstmem, srcmem, total_bytes, alignment))
    return true;

  if (!optimize_size && optimize > 2)
    return nds32_expand_movmemsi_loop (dstmem, srcmem, total_bytes, alignment);

  return false;
}


/* Functions to expand load_multiple and store_multiple.
   They are auxiliary extern functions to help create rtx template.
   Check nds32-multiple.md file for the patterns.  */
rtx
nds32_expand_load_multiple (int base_regno, int count,
			    rtx base_addr, rtx basemem,
			    bool update_base_reg_p,
			    rtx *update_base_reg)
{
  int par_index;
  int offset;
  int start_idx;
  rtx result;
  rtx new_addr, mem, reg;

  /* Generate a unaligned load to prevent load instruction pull out from
     parallel, and then it will generate lwi, and lose unaligned acces */
  if (count == 1)
    {
      reg = gen_rtx_REG (SImode, base_regno);
      if (update_base_reg_p)
	{
	  *update_base_reg = gen_reg_rtx (SImode);
	  return gen_unaligned_load_update_base_w (*update_base_reg, reg, base_addr);
	}
      else
	return gen_unaligned_load_w (reg, gen_rtx_MEM (SImode, base_addr));
    }

  /* Create the pattern that is presented in nds32-multiple.md.  */
  if (update_base_reg_p)
    {
      result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count + 1));
      start_idx = 1;
    }
  else
    {
      result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));
      start_idx = 0;
    }

  if (update_base_reg_p)
    {
      offset           = count * 4;
      new_addr         = plus_constant (Pmode, base_addr, offset);
      *update_base_reg = gen_reg_rtx (SImode);

      XVECEXP (result, 0, 0) = gen_rtx_SET (*update_base_reg, new_addr);
    }

  for (par_index = 0; par_index < count; par_index++)
    {
      offset   = par_index * 4;
      /* 4-byte for loading data to each register.  */
      new_addr = plus_constant (Pmode, base_addr, offset);
      mem      = adjust_automodify_address_nv (basemem, SImode,
					       new_addr, offset);
      reg      = gen_rtx_REG (SImode, base_regno + par_index);

      XVECEXP (result, 0, (par_index + start_idx)) = gen_rtx_SET (reg, mem);
    }

  return result;
}

rtx
nds32_expand_store_multiple (int base_regno, int count,
			     rtx base_addr, rtx basemem,
			     bool update_base_reg_p,
			     rtx *update_base_reg)
{
  int par_index;
  int offset;
  int start_idx;
  rtx result;
  rtx new_addr, mem, reg;

  if (count == 1)
    {
      reg = gen_rtx_REG (SImode, base_regno);
      if (update_base_reg_p)
	{
	  *update_base_reg = gen_reg_rtx (SImode);
	  return gen_unaligned_store_update_base_w (*update_base_reg, base_addr, reg);
	}
      else
	return gen_unaligned_store_w (gen_rtx_MEM (SImode, base_addr), reg);
    }

  /* Create the pattern that is presented in nds32-multiple.md.  */

  if (update_base_reg_p)
    {
      result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count + 1));
      start_idx = 1;
    }
  else
    {
      result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));
      start_idx = 0;
    }

  if (update_base_reg_p)
    {
      offset           = count * 4;
      new_addr         = plus_constant (Pmode, base_addr, offset);
      *update_base_reg = gen_reg_rtx (SImode);

      XVECEXP (result, 0, 0) = gen_rtx_SET (*update_base_reg, new_addr);
    }

  for (par_index = 0; par_index < count; par_index++)
    {
      offset   = par_index * 4;
      /* 4-byte for storing data to memory.  */
      new_addr = plus_constant (Pmode, base_addr, offset);
      mem      = adjust_automodify_address_nv (basemem, SImode,
					       new_addr, offset);
      reg      = gen_rtx_REG (SImode, base_regno + par_index);

      XVECEXP (result, 0, par_index + start_idx) = gen_rtx_SET (mem, reg);
    }

  return result;
}

/* ------------------------------------------------------------------------ */
