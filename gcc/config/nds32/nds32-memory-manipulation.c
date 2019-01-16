/* Auxiliary functions for expand movmem, setmem, cmpmem, load_multiple
   and store_multiple pattern of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2019 Free Software Foundation, Inc.
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
  rtx dst_base_reg, src_base_reg;
  rtx dst_itr, src_itr;
  rtx dstmem_m, srcmem_m, dst_itr_m, src_itr_m;
  rtx dst_end;
  rtx double_word_mode_loop, byte_mode_loop;
  rtx tmp;
  int start_regno;
  bool align_to_4_bytes = (INTVAL (alignment) & 3) == 0;
  unsigned HOST_WIDE_INT total_bytes = UINTVAL (size);

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

  double_word_mode_loop = gen_label_rtx ();
  byte_mode_loop = gen_label_rtx ();

  dst_base_reg = copy_to_mode_reg (Pmode, XEXP (dstmem, 0));
  src_base_reg = copy_to_mode_reg (Pmode, XEXP (srcmem, 0));

  if (total_bytes < 8)
    {
      /* Emit total_bytes less than 8 loop version of movmem.
	add     $dst_end, $dst, $size
	move    $dst_itr, $dst
	.Lbyte_mode_loop:
	lbi.bi  $tmp, [$src_itr], #1
	sbi.bi  $tmp, [$dst_itr], #1
	! Not readch upper bound. Loop.
	bne     $dst_itr, $dst_end, .Lbyte_mode_loop */

      /* add     $dst_end, $dst, $size */
      dst_end = expand_binop (Pmode, add_optab, dst_base_reg, size,
			      NULL_RTX, 0, OPTAB_WIDEN);
      /* move    $dst_itr, $dst
	 move    $src_itr, $src */
      emit_move_insn (dst_itr, dst_base_reg);
      emit_move_insn (src_itr, src_base_reg);

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
      return true;
    }
  else if (total_bytes % 8 == 0)
    {
      /* Emit multiple of 8 loop version of movmem.

	 add     $dst_end, $dst, $size
	 move    $dst_itr, $dst
	 move    $src_itr, $src

	.Ldouble_word_mode_loop:
	lmw.bim $tmp-begin, [$src_itr], $tmp-end, #0 ! $src_itr' = $src_itr
	smw.bim $tmp-begin, [$dst_itr], $tmp-end, #0 ! $dst_itr' = $dst_itr
	! move will delete after register allocation
	move    $src_itr, $src_itr'
	move    $dst_itr, $dst_itr'
	! Not readch upper bound. Loop.
	bne     $double_word_end, $dst_itr, .Ldouble_word_mode_loop */

      /* add     $dst_end, $dst, $size */
      dst_end = expand_binop (Pmode, add_optab, dst_base_reg, size,
			      NULL_RTX, 0, OPTAB_WIDEN);

      /* move    $dst_itr, $dst
	 move    $src_itr, $src */
      emit_move_insn (dst_itr, dst_base_reg);
      emit_move_insn (src_itr, src_base_reg);

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
      emit_cmp_and_jump_insns (dst_end, dst_itr, NE, NULL,
			       Pmode, 1, double_word_mode_loop);
    }
  else
    {
      /* Handle size greater than 8, and not a multiple of 8.  */
      return nds32_expand_movmemsi_loop_unknown_size (dstmem, srcmem,
						      size, alignment);
    }

  return true;
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

/* ------------------------------------------------------------------------ */

/* Auxiliary function for expand setmem pattern.  */

static rtx
nds32_gen_dup_4_byte_to_word_value_aux (rtx value, rtx value4word)
{
  gcc_assert (GET_MODE (value) == QImode || CONST_INT_P (value));

  if (CONST_INT_P (value))
    {
      unsigned HOST_WIDE_INT val = UINTVAL (value) & GET_MODE_MASK(QImode);
      rtx new_val = gen_int_mode (val | (val << 8)
				  | (val << 16) | (val << 24), SImode);
      /* Just calculate at here if it's constant value.  */
      emit_move_insn (value4word, new_val);
    }
  else
    {
      if (NDS32_EXT_DSP_P ())
	{
	  /* ! prepare word
	     insb    $tmp, $value, 1         ! $tmp  <- 0x0000abab
	     pkbb16  $tmp6, $tmp2, $tmp2   ! $value4word  <- 0xabababab */
	  rtx tmp = gen_reg_rtx (SImode);

	  convert_move (tmp, value, true);

	  emit_insn (
	    gen_insvsi_internal (tmp, gen_int_mode (0x8, SImode), tmp));

	  emit_insn (gen_pkbbsi_1 (value4word, tmp, tmp));
	}
      else
	{
	  /* ! prepare word
	     andi    $tmp1, $value, 0xff       ! $tmp1  <- 0x000000ab
	     slli    $tmp2, $tmp1, 8           ! $tmp2  <- 0x0000ab00
	     or      $tmp3, $tmp1, $tmp2       ! $tmp3  <- 0x0000abab
	     slli    $tmp4, $tmp3, 16          ! $tmp4  <- 0xabab0000
	     or      $val4word, $tmp3, $tmp4   ! $value4word  <- 0xabababab  */

	  rtx tmp1, tmp2, tmp3, tmp4;
	  tmp1 = expand_binop (SImode, and_optab, value,
			       gen_int_mode (0xff, SImode),
			       NULL_RTX, 0, OPTAB_WIDEN);
	  tmp2 = expand_binop (SImode, ashl_optab, tmp1,
			       gen_int_mode (8, SImode),
			       NULL_RTX, 0, OPTAB_WIDEN);
	  tmp3 = expand_binop (SImode, ior_optab, tmp1, tmp2,
			       NULL_RTX, 0, OPTAB_WIDEN);
	  tmp4 = expand_binop (SImode, ashl_optab, tmp3,
			       gen_int_mode (16, SImode),
			       NULL_RTX, 0, OPTAB_WIDEN);

	  emit_insn (gen_iorsi3 (value4word, tmp3, tmp4));
	}
    }

  return value4word;
}

static rtx
nds32_gen_dup_4_byte_to_word_value (rtx value)
{
  rtx value4word = gen_reg_rtx (SImode);
  nds32_gen_dup_4_byte_to_word_value_aux (value, value4word);

  return value4word;
}

static rtx
nds32_gen_dup_8_byte_to_double_word_value (rtx value)
{
  rtx value4doubleword = gen_reg_rtx (DImode);

  nds32_gen_dup_4_byte_to_word_value_aux (
    value, nds32_di_low_part_subreg(value4doubleword));

  emit_move_insn (nds32_di_high_part_subreg(value4doubleword),
		  nds32_di_low_part_subreg(value4doubleword));
  return value4doubleword;
}


static rtx
emit_setmem_doubleword_loop (rtx itr, rtx size, rtx value)
{
  rtx word_mode_label = gen_label_rtx ();
  rtx word_mode_end_label = gen_label_rtx ();
  rtx byte_mode_size = gen_reg_rtx (SImode);
  rtx byte_mode_size_tmp = gen_reg_rtx (SImode);
  rtx word_mode_end = gen_reg_rtx (SImode);
  rtx size_for_word = gen_reg_rtx (SImode);

  /* and     $size_for_word, $size, #~0x7  */
  size_for_word = expand_binop (SImode, and_optab, size,
				gen_int_mode (~0x7, SImode),
				NULL_RTX, 0, OPTAB_WIDEN);

  emit_move_insn (byte_mode_size, size);

  /* beqz    $size_for_word, .Lbyte_mode_entry  */
  emit_cmp_and_jump_insns (size_for_word, const0_rtx, EQ, NULL,
			   SImode, 1, word_mode_end_label);
  /* add     $word_mode_end, $dst, $size_for_word  */
  word_mode_end = expand_binop (Pmode, add_optab, itr, size_for_word,
				NULL_RTX, 0, OPTAB_WIDEN);

  /* andi    $byte_mode_size, $size, 0x7  */
  byte_mode_size_tmp = expand_binop (SImode, and_optab, size, GEN_INT (0x7),
				     NULL_RTX, 0, OPTAB_WIDEN);

  emit_move_insn (byte_mode_size, byte_mode_size_tmp);

  /* .Lword_mode:  */
  emit_label (word_mode_label);
  /*   ! word-mode set loop
       smw.bim $value4word, [$dst_itr], $value4word, 0
       bne     $word_mode_end, $dst_itr, .Lword_mode  */
  emit_insn (gen_unaligned_store_update_base_dw (itr,
						 itr,
						 value));
  emit_cmp_and_jump_insns (word_mode_end, itr, NE, NULL,
			   Pmode, 1, word_mode_label);

  emit_label (word_mode_end_label);

  return byte_mode_size;
}

static rtx
emit_setmem_byte_loop (rtx itr, rtx size, rtx value, bool need_end)
{
  rtx end  = gen_reg_rtx (Pmode);
  rtx byte_mode_label = gen_label_rtx ();
  rtx end_label = gen_label_rtx ();

  value = force_reg (QImode, value);

  if (need_end)
    end = expand_binop (Pmode, add_optab, itr, size,
			NULL_RTX, 0, OPTAB_WIDEN);
  /*   beqz    $byte_mode_size, .Lend
       add     $byte_mode_end, $dst_itr, $byte_mode_size  */
  emit_cmp_and_jump_insns (size, const0_rtx, EQ, NULL,
			   SImode, 1, end_label);

  if (!need_end)
    end = expand_binop (Pmode, add_optab, itr, size,
			NULL_RTX, 0, OPTAB_WIDEN);

  /* .Lbyte_mode:  */
  emit_label (byte_mode_label);

  /*   ! byte-mode set loop
       sbi.bi  $value, [$dst_itr] ,1
       bne     $byte_mode_end, $dst_itr, .Lbyte_mode */
  nds32_emit_post_inc_load_store (value, itr, QImode, false);

  emit_cmp_and_jump_insns (end, itr, NE, NULL,
			   Pmode, 1, byte_mode_label);
  /* .Lend: */
  emit_label (end_label);

  if (need_end)
    return end;
  else
    return NULL_RTX;
}

static bool
nds32_expand_setmem_loop (rtx dstmem, rtx size, rtx value)
{
  rtx value4doubleword;
  rtx value4byte;
  rtx dst;
  rtx byte_mode_size;

  /* Emit loop version of setmem.
     memset:
       ! prepare word
       andi    $tmp1, $val, 0xff               ! $tmp1  <- 0x000000ab
       slli    $tmp2, $tmp1, 8                 ! $tmp2  <- 0x0000ab00
       or      $tmp3, $val, $tmp2              ! $tmp3  <- 0x0000abab
       slli    $tmp4, $tmp3, 16                ! $tmp4  <- 0xabab0000
       or      $val4word, $tmp3, $tmp4         ! $value4word  <- 0xabababab

       and     $size_for_word, $size, #-4
       beqz    $size_for_word, .Lword_mode_end

       add     $word_mode_end, $dst, $size_for_word
       andi    $byte_mode_size, $size, 3

     .Lword_mode:
       ! word-mode set loop
       smw.bim $value4word, [$dst], $value4word, 0
       bne     $word_mode_end, $dst, .Lword_mode

     .Lword_mode_end:
       beqz    $byte_mode_size, .Lend
       add     $byte_mode_end, $dst, $byte_mode_size

     .Lbyte_mode:
       ! byte-mode set loop
       sbi.bi  $value4word, [$dst] ,1
       bne     $byte_mode_end, $dst, .Lbyte_mode
     .Lend: */

  dst = copy_to_mode_reg (SImode, XEXP (dstmem, 0));

  /* ! prepare word
     andi    $tmp1, $value, 0xff             ! $tmp1  <- 0x000000ab
     slli    $tmp2, $tmp1, 8                 ! $tmp2  <- 0x0000ab00
     or      $tmp3, $tmp1, $tmp2             ! $tmp3  <- 0x0000abab
     slli    $tmp4, $tmp3, 16                ! $tmp4  <- 0xabab0000
     or      $val4word, $tmp3, $tmp4         ! $value4word  <- 0xabababab  */
  value4doubleword = nds32_gen_dup_8_byte_to_double_word_value (value);

  /*   and     $size_for_word, $size, #-4
       beqz    $size_for_word, .Lword_mode_end

       add     $word_mode_end, $dst, $size_for_word
       andi    $byte_mode_size, $size, 3

     .Lword_mode:
       ! word-mode set loop
       smw.bim $value4word, [$dst], $value4word, 0
       bne     $word_mode_end, $dst, .Lword_mode
     .Lword_mode_end:  */
  byte_mode_size = emit_setmem_doubleword_loop (dst, size, value4doubleword);

  /*   beqz    $byte_mode_size, .Lend
       add     $byte_mode_end, $dst, $byte_mode_size

     .Lbyte_mode:
       ! byte-mode set loop
       sbi.bi  $value, [$dst] ,1
       bne     $byte_mode_end, $dst, .Lbyte_mode
     .Lend: */

  value4byte = simplify_gen_subreg (QImode, value4doubleword, DImode,
				    subreg_lowpart_offset (QImode, DImode));

  emit_setmem_byte_loop (dst, byte_mode_size, value4byte, false);

  return true;
}

static bool
nds32_expand_setmem_loop_v3m (rtx dstmem, rtx size, rtx value)
{
  rtx base_reg = copy_to_mode_reg (Pmode, XEXP (dstmem, 0));
  rtx need_align_bytes = gen_reg_rtx (SImode);
  rtx last_2_bit = gen_reg_rtx (SImode);
  rtx byte_loop_base = gen_reg_rtx (SImode);
  rtx byte_loop_size = gen_reg_rtx (SImode);
  rtx remain_size = gen_reg_rtx (SImode);
  rtx new_base_reg;
  rtx value4byte, value4doubleword;
  rtx byte_mode_size;
  rtx last_byte_loop_label = gen_label_rtx ();

  size = force_reg (SImode, size);

  value4doubleword = nds32_gen_dup_8_byte_to_double_word_value (value);
  value4byte = simplify_gen_subreg (QImode, value4doubleword, DImode,
				    subreg_lowpart_offset (QImode, DImode));

  emit_move_insn (byte_loop_size, size);
  emit_move_insn (byte_loop_base, base_reg);

  /* Jump to last byte loop if size is less than 16.  */
  emit_cmp_and_jump_insns (size, gen_int_mode (16, SImode), LE, NULL,
			   SImode, 1, last_byte_loop_label);

  /* Make sure align to 4 byte first since v3m can't unalign access.  */
  emit_insn (gen_andsi3 (last_2_bit,
			 base_reg,
			 gen_int_mode (0x3, SImode)));

  emit_insn (gen_subsi3 (need_align_bytes,
			 gen_int_mode (4, SImode),
			 last_2_bit));

  /* Align to 4 byte. */
  new_base_reg = emit_setmem_byte_loop (base_reg,
					need_align_bytes,
					value4byte,
					true);

  /* Calculate remain size. */
  emit_insn (gen_subsi3 (remain_size, size, need_align_bytes));

  /* Set memory word by word. */
  byte_mode_size = emit_setmem_doubleword_loop (new_base_reg,
						remain_size,
						value4doubleword);

  emit_move_insn (byte_loop_base, new_base_reg);
  emit_move_insn (byte_loop_size, byte_mode_size);

  emit_label (last_byte_loop_label);

  /* And set memory for remain bytes. */
  emit_setmem_byte_loop (byte_loop_base, byte_loop_size, value4byte, false);
  return true;
}

static bool
nds32_expand_setmem_unroll (rtx dstmem, rtx size, rtx value,
			    rtx align ATTRIBUTE_UNUSED,
			    rtx expected_align ATTRIBUTE_UNUSED,
			    rtx expected_size ATTRIBUTE_UNUSED)
{
  unsigned maximum_regs, maximum_bytes, start_regno, regno;
  rtx value4word;
  rtx dst_base_reg, new_base_reg;
  unsigned HOST_WIDE_INT remain_bytes, remain_words, prepare_regs, fill_per_smw;
  unsigned HOST_WIDE_INT real_size;

  if (TARGET_REDUCED_REGS)
    {
      maximum_regs  = 4;
      maximum_bytes = 64;
      start_regno   = 2;
    }
  else
    {
      maximum_regs  = 8;
      maximum_bytes = 128;
      start_regno   = 16;
    }

  real_size = UINTVAL (size) & GET_MODE_MASK(SImode);

  if (!(CONST_INT_P (size) && real_size <= maximum_bytes))
    return false;

  remain_bytes = real_size;

  gcc_assert (GET_MODE (value) == QImode || CONST_INT_P (value));

  value4word = nds32_gen_dup_4_byte_to_word_value (value);

  prepare_regs = remain_bytes / UNITS_PER_WORD;

  dst_base_reg = copy_to_mode_reg (SImode, XEXP (dstmem, 0));

  if (prepare_regs > maximum_regs)
    prepare_regs = maximum_regs;

  fill_per_smw = prepare_regs * UNITS_PER_WORD;

  regno = start_regno;
  switch (prepare_regs)
    {
    case 2:
    default:
      {
	rtx reg0 = gen_rtx_REG (SImode, regno);
	rtx reg1 = gen_rtx_REG (SImode, regno+1);
	unsigned last_regno = start_regno + prepare_regs - 1;

	emit_move_insn (reg0, value4word);
	emit_move_insn (reg1, value4word);
	rtx regd = gen_rtx_REG (DImode, regno);
	regno += 2;

	/* Try to utilize movd44!  */
	while (regno <= last_regno)
	  {
	    if ((regno + 1) <=last_regno)
	      {
		rtx reg = gen_rtx_REG (DImode, regno);
		emit_move_insn (reg, regd);
		regno += 2;
	      }
	    else
	      {
		rtx reg = gen_rtx_REG (SImode, regno);
		emit_move_insn (reg, reg0);
		regno += 1;
	      }
	  }
	break;
      }
    case 1:
      {
	rtx reg = gen_rtx_REG (SImode, regno++);
	emit_move_insn (reg, value4word);
      }
      break;
    case 0:
      break;
    }

  if (fill_per_smw)
    for (;remain_bytes >= fill_per_smw;remain_bytes -= fill_per_smw)
      {
	emit_insn (nds32_expand_store_multiple (start_regno, prepare_regs,
						dst_base_reg, dstmem,
						true, &new_base_reg));
	dst_base_reg = new_base_reg;
	dstmem = gen_rtx_MEM (SImode, dst_base_reg);
      }

  remain_words = remain_bytes / UNITS_PER_WORD;

  if (remain_words)
    {
      emit_insn (nds32_expand_store_multiple (start_regno, remain_words,
					      dst_base_reg, dstmem,
					      true, &new_base_reg));
      dst_base_reg = new_base_reg;
      dstmem = gen_rtx_MEM (SImode, dst_base_reg);
    }

  remain_bytes = remain_bytes - (remain_words * UNITS_PER_WORD);

  if (remain_bytes)
    {
      value = simplify_gen_subreg (QImode, value4word, SImode,
				   subreg_lowpart_offset(QImode, SImode));
      int offset = 0;
      for (;remain_bytes;--remain_bytes, ++offset)
	{
	  nds32_emit_load_store (value, dstmem, QImode, offset, false);
	}
    }

  return true;
}

bool
nds32_expand_setmem (rtx dstmem, rtx size, rtx value, rtx align,
		     rtx expected_align,
		     rtx expected_size)
{
  bool align_to_4_bytes = (INTVAL (align) & 3) == 0;

  /* Only expand at O3 */
  if (optimize_size || optimize < 3)
    return false;

  if (TARGET_ISA_V3M && !align_to_4_bytes)
    return nds32_expand_setmem_loop_v3m (dstmem, size, value);

  if (nds32_expand_setmem_unroll (dstmem, size, value,
				  align, expected_align, expected_size))
    return true;

  return nds32_expand_setmem_loop (dstmem, size, value);
}

/* ------------------------------------------------------------------------ */

/* Auxiliary function for expand strlen pattern.  */

bool
nds32_expand_strlen (rtx result, rtx str,
		     rtx target_char, rtx align ATTRIBUTE_UNUSED)
{
  rtx base_reg, backup_base_reg;
  rtx ffb_result;
  rtx target_char_ptr, length;
  rtx loop_label, tmp;

  if (optimize_size || optimize < 3)
    return false;

  gcc_assert (MEM_P (str));
  gcc_assert (CONST_INT_P (target_char) || REG_P (target_char));

  base_reg = copy_to_mode_reg (SImode, XEXP (str, 0));
  loop_label = gen_label_rtx ();

  ffb_result = gen_reg_rtx (Pmode);
  tmp = gen_reg_rtx (SImode);
  backup_base_reg = gen_reg_rtx (SImode);

  /* Emit loop version of strlen.
       move  $backup_base, $base
     .Lloop:
       lmw.bim $tmp, [$base], $tmp, 0
       ffb   $ffb_result, $tmp, $target_char   ! is there $target_char?
       beqz  $ffb_result, .Lloop
       add   $last_char_ptr, $base, $ffb_result
       sub   $length, $last_char_ptr, $backup_base  */

  /* move  $backup_base, $base  */
  emit_move_insn (backup_base_reg, base_reg);

  /* .Lloop:  */
  emit_label (loop_label);
  /* lmw.bim $tmp, [$base], $tmp, 0  */
  emit_insn (gen_unaligned_load_update_base_w (base_reg, tmp, base_reg));

  /*  ffb   $ffb_result, $tmp, $target_char   ! is there $target_char?  */
  emit_insn (gen_unspec_ffb (ffb_result, tmp, target_char));

  /* beqz  $ffb_result, .Lloop  */
  emit_cmp_and_jump_insns (ffb_result, const0_rtx, EQ, NULL,
			   SImode, 1, loop_label);

  /* add   $target_char_ptr, $base, $ffb_result   */
  target_char_ptr = expand_binop (Pmode, add_optab, base_reg,
				ffb_result, NULL_RTX, 0, OPTAB_WIDEN);

  /* sub   $length, $target_char_ptr, $backup_base  */
  length = expand_binop (Pmode, sub_optab, target_char_ptr,
			 backup_base_reg, NULL_RTX, 0, OPTAB_WIDEN);

  emit_move_insn (result, length);

  return true;
}

/* ------------------------------------------------------------------------ */

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
