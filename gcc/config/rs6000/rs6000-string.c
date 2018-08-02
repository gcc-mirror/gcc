/* Subroutines used to expand string and block move, clear,
   compare and other operations for PowerPC.
   Copyright (C) 1991-2018 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "ira.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "target.h"

/* Expand a block clear operation, and return 1 if successful.  Return 0
   if we should let the compiler generate normal code.

   operands[0] is the destination
   operands[1] is the length
   operands[3] is the alignment */

int
expand_block_clear (rtx operands[])
{
  rtx orig_dest = operands[0];
  rtx bytes_rtx	= operands[1];
  rtx align_rtx = operands[3];
  bool constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  HOST_WIDE_INT align;
  HOST_WIDE_INT bytes;
  int offset;
  int clear_bytes;
  int clear_step;

  /* If this is not a fixed size move, just call memcpy */
  if (! constp)
    return 0;

  /* This must be a fixed size alignment  */
  gcc_assert (GET_CODE (align_rtx) == CONST_INT);
  align = INTVAL (align_rtx) * BITS_PER_UNIT;

  /* Anything to clear? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return 1;

  /* Use the builtin memset after a point, to avoid huge code bloat.
     When optimize_size, avoid any significant code bloat; calling
     memset is about 4 instructions, so allow for one instruction to
     load zero and three to do clearing.  */
  if (TARGET_ALTIVEC && (align >= 128 || TARGET_EFFICIENT_UNALIGNED_VSX))
    clear_step = 16;
  else if (TARGET_POWERPC64 && (align >= 64 || !STRICT_ALIGNMENT))
    clear_step = 8;
  else
    clear_step = 4;

  if (optimize_size && bytes > 3 * clear_step)
    return 0;
  if (! optimize_size && bytes > 8 * clear_step)
    return 0;

  for (offset = 0; bytes > 0; offset += clear_bytes, bytes -= clear_bytes)
    {
      machine_mode mode = BLKmode;
      rtx dest;

      if (TARGET_ALTIVEC
	  && ((bytes >= 16 && align >= 128)
	      || (bytes >= 32 && TARGET_EFFICIENT_UNALIGNED_VSX)))
	{
	  clear_bytes = 16;
	  mode = V4SImode;
	}
      else if (bytes >= 8 && TARGET_POWERPC64
	       && (align >= 64 || !STRICT_ALIGNMENT))
	{
	  clear_bytes = 8;
	  mode = DImode;
	  if (offset == 0 && align < 64)
	    {
	      rtx addr;

	      /* If the address form is reg+offset with offset not a
		 multiple of four, reload into reg indirect form here
		 rather than waiting for reload.  This way we get one
		 reload, not one per store.  */
	      addr = XEXP (orig_dest, 0);
	      if ((GET_CODE (addr) == PLUS || GET_CODE (addr) == LO_SUM)
		  && GET_CODE (XEXP (addr, 1)) == CONST_INT
		  && (INTVAL (XEXP (addr, 1)) & 3) != 0)
		{
		  addr = copy_addr_to_reg (addr);
		  orig_dest = replace_equiv_address (orig_dest, addr);
		}
	    }
	}
      else if (bytes >= 4 && (align >= 32 || !STRICT_ALIGNMENT))
	{			/* move 4 bytes */
	  clear_bytes = 4;
	  mode = SImode;
	}
      else if (bytes >= 2 && (align >= 16 || !STRICT_ALIGNMENT))
	{			/* move 2 bytes */
	  clear_bytes = 2;
	  mode = HImode;
	}
      else /* move 1 byte at a time */
	{
	  clear_bytes = 1;
	  mode = QImode;
	}

      dest = adjust_address (orig_dest, mode, offset);

      emit_move_insn (dest, CONST0_RTX (mode));
    }

  return 1;
}

/* Figure out the correct instructions to generate to load data for
   block compare.  MODE is used for the read from memory, and
   data is zero extended if REG is wider than MODE.  If LE code
   is being generated, bswap loads are used.

   REG is the destination register to move the data into.
   MEM is the memory block being read.
   MODE is the mode of memory to use for the read.  */
static void
do_load_for_compare (rtx reg, rtx mem, machine_mode mode)
{
  switch (GET_MODE (reg))
    {
    case E_DImode:
      switch (mode)
	{
	case E_QImode:
	  emit_insn (gen_zero_extendqidi2 (reg, mem));
	  break;
	case E_HImode:
	  {
	    rtx src = mem;
	    if (!BYTES_BIG_ENDIAN)
	      {
		src = gen_reg_rtx (HImode);
		emit_insn (gen_bswaphi2 (src, mem));
	      }
	    emit_insn (gen_zero_extendhidi2 (reg, src));
	    break;
	  }
	case E_SImode:
	  {
	    rtx src = mem;
	    if (!BYTES_BIG_ENDIAN)
	      {
		src = gen_reg_rtx (SImode);
		emit_insn (gen_bswapsi2 (src, mem));
	      }
	    emit_insn (gen_zero_extendsidi2 (reg, src));
	  }
	  break;
	case E_DImode:
	  if (!BYTES_BIG_ENDIAN)
	    emit_insn (gen_bswapdi2 (reg, mem));
	  else
	    emit_insn (gen_movdi (reg, mem));
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case E_SImode:
      switch (mode)
	{
	case E_QImode:
	  emit_insn (gen_zero_extendqisi2 (reg, mem));
	  break;
	case E_HImode:
	  {
	    rtx src = mem;
	    if (!BYTES_BIG_ENDIAN)
	      {
		src = gen_reg_rtx (HImode);
		emit_insn (gen_bswaphi2 (src, mem));
	      }
	    emit_insn (gen_zero_extendhisi2 (reg, src));
	    break;
	  }
	case E_SImode:
	  if (!BYTES_BIG_ENDIAN)
	    emit_insn (gen_bswapsi2 (reg, mem));
	  else
	    emit_insn (gen_movsi (reg, mem));
	  break;
	case E_DImode:
	  /* DImode is larger than the destination reg so is not expected.  */
	  gcc_unreachable ();
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    default:
      gcc_unreachable ();
      break;
    }
}

/* Select the mode to be used for reading the next chunk of bytes
   in the compare.

   OFFSET is the current read offset from the beginning of the block.
   BYTES is the number of bytes remaining to be read.
   ALIGN is the minimum alignment of the memory blocks being compared in bytes.
   WORD_MODE_OK indicates using WORD_MODE is allowed, else SImode is
   the largest allowable mode.  */
static machine_mode
select_block_compare_mode (unsigned HOST_WIDE_INT offset,
			   unsigned HOST_WIDE_INT bytes,
			   unsigned HOST_WIDE_INT align, bool word_mode_ok)
{
  /* First see if we can do a whole load unit
     as that will be more efficient than a larger load + shift.  */

  /* If big, use biggest chunk.
     If exactly chunk size, use that size.
     If remainder can be done in one piece with shifting, do that.
     Do largest chunk possible without violating alignment rules.  */

  /* The most we can read without potential page crossing.  */
  unsigned HOST_WIDE_INT maxread = ROUND_UP (bytes, align);

  if (word_mode_ok && bytes >= UNITS_PER_WORD)
    return word_mode;
  else if (bytes == GET_MODE_SIZE (SImode))
    return SImode;
  else if (bytes == GET_MODE_SIZE (HImode))
    return HImode;
  else if (bytes == GET_MODE_SIZE (QImode))
    return QImode;
  else if (bytes < GET_MODE_SIZE (SImode)
	   && TARGET_EFFICIENT_OVERLAPPING_UNALIGNED
	   && offset >= GET_MODE_SIZE (SImode) - bytes)
    /* This matches the case were we have SImode and 3 bytes
       and offset >= 1 and permits us to move back one and overlap
       with the previous read, thus avoiding having to shift
       unwanted bytes off of the input.  */
    return SImode;
  else if (word_mode_ok && bytes < UNITS_PER_WORD
	   && TARGET_EFFICIENT_OVERLAPPING_UNALIGNED
	   && offset >= UNITS_PER_WORD-bytes)
    /* Similarly, if we can use DImode it will get matched here and
       can do an overlapping read that ends at the end of the block.  */
    return word_mode;
  else if (word_mode_ok && maxread >= UNITS_PER_WORD)
    /* It is safe to do all remaining in one load of largest size,
       possibly with a shift to get rid of unwanted bytes.  */
    return word_mode;
  else if (maxread >= GET_MODE_SIZE (SImode))
    /* It is safe to do all remaining in one SImode load,
       possibly with a shift to get rid of unwanted bytes.  */
    return SImode;
  else if (bytes > GET_MODE_SIZE (SImode))
    return SImode;
  else if (bytes > GET_MODE_SIZE (HImode))
    return HImode;

  /* final fallback is do one byte */
  return QImode;
}

/* Compute the alignment of pointer+OFFSET where the original alignment
   of pointer was BASE_ALIGN.  */
static unsigned HOST_WIDE_INT
compute_current_alignment (unsigned HOST_WIDE_INT base_align,
			   unsigned HOST_WIDE_INT offset)
{
  if (offset == 0)
    return base_align;
  return MIN (base_align, offset & -offset);
}

/* Prepare address and then do a load.

   MODE is the mode to use for the load.
   DEST is the destination register for the data.
   ADDR is the address to be loaded.
   ORIG_ADDR is the original address expression.  */
static void
do_load_for_compare_from_addr (machine_mode mode, rtx dest, rtx addr,
			       rtx orig_addr)
{
  rtx mem = gen_rtx_MEM (mode, addr);
  MEM_COPY_ATTRIBUTES (mem, orig_addr);
  set_mem_size (mem, GET_MODE_SIZE (mode));
  do_load_for_compare (dest, mem, mode);
  return;
}

/* Do a branch for an if/else decision.

   CMPMODE is the mode to use for the comparison.
   COMPARISON is the rtx code for the compare needed.
   A is the first thing to be compared.
   B is the second thing to be compared.
   CR is the condition code reg input, or NULL_RTX.
   TRUE_LABEL is the label to branch to if the condition is true.

   The return value is the CR used for the comparison.
   If CR is null_rtx, then a new register of CMPMODE is generated.
   If A and B are both null_rtx, then CR must not be null, and the
   compare is not generated so you can use this with a dot form insn.  */

static void
do_ifelse (machine_mode cmpmode, rtx_code comparison,
	   rtx a, rtx b, rtx cr, rtx true_label)
{
  gcc_assert ((a == NULL_RTX && b == NULL_RTX && cr != NULL_RTX)
	      || (a != NULL_RTX && b != NULL_RTX));

  if (cr != NULL_RTX)
    gcc_assert (GET_MODE (cr) == cmpmode);
  else
    cr = gen_reg_rtx (cmpmode);

  rtx label_ref = gen_rtx_LABEL_REF (VOIDmode, true_label);

  if (a != NULL_RTX)
    emit_move_insn (cr, gen_rtx_COMPARE (cmpmode, a, b));

  rtx cmp_rtx = gen_rtx_fmt_ee (comparison, VOIDmode, cr, const0_rtx);

  rtx ifelse = gen_rtx_IF_THEN_ELSE (VOIDmode, cmp_rtx, label_ref, pc_rtx);
  rtx j = emit_jump_insn (gen_rtx_SET (pc_rtx, ifelse));
  JUMP_LABEL (j) = true_label;
  LABEL_NUSES (true_label) += 1;
}

/* Emit an isel of the proper mode for DEST.

   DEST is the isel destination register.
   SRC1 is the isel source if CR is true.
   SRC2 is the isel source if CR is false.
   CR is the condition for the isel.  */
static void
do_isel (rtx dest, rtx cmp, rtx src_t, rtx src_f, rtx cr)
{
  if (GET_MODE (dest) == DImode)
    emit_insn (gen_isel_signed_di (dest, cmp, src_t, src_f, cr));
  else
    emit_insn (gen_isel_signed_si (dest, cmp, src_t, src_f, cr));
}

/* Emit a subtract of the proper mode for DEST.

   DEST is the destination register for the subtract.
   SRC1 is the first subtract input.
   SRC2 is the second subtract input.

   Computes DEST = SRC1-SRC2.  */
static void
do_sub3 (rtx dest, rtx src1, rtx src2)
{
  if (GET_MODE (dest) == DImode)
    emit_insn (gen_subdi3 (dest, src1, src2));
  else
    emit_insn (gen_subsi3 (dest, src1, src2));
}

/* Emit an add of the proper mode for DEST.

   DEST is the destination register for the add.
   SRC1 is the first add input.
   SRC2 is the second add input.

   Computes DEST = SRC1+SRC2.  */
static void
do_add3 (rtx dest, rtx src1, rtx src2)
{
  if (GET_MODE (dest) == DImode)
    emit_insn (gen_adddi3 (dest, src1, src2));
  else
    emit_insn (gen_addsi3 (dest, src1, src2));
}

/* Emit an and of the proper mode for DEST.

   DEST is the destination register for the and.
   SRC1 is the first and input.
   SRC2 is the second and input.

   Computes DEST = SRC1&SRC2.  */
static void
do_and3 (rtx dest, rtx src1, rtx src2)
{
  if (GET_MODE (dest) == DImode)
    emit_insn (gen_anddi3 (dest, src1, src2));
  else
    emit_insn (gen_andsi3 (dest, src1, src2));
}

/* Emit an cmpb of the proper mode for DEST.

   DEST is the destination register for the cmpb.
   SRC1 is the first input.
   SRC2 is the second input.

   Computes cmpb of SRC1, SRC2.  */
static void
do_cmpb3 (rtx dest, rtx src1, rtx src2)
{
  if (GET_MODE (dest) == DImode)
    emit_insn (gen_cmpbdi3 (dest, src1, src2));
  else
    emit_insn (gen_cmpbsi3 (dest, src1, src2));
}

/* Emit a rotl of the proper mode for DEST.

   DEST is the destination register for the and.
   SRC1 is the first and input.
   SRC2 is the second and input.

   Computes DEST = SRC1 rotated left by SRC2.  */
static void
do_rotl3 (rtx dest, rtx src1, rtx src2)
{
  if (GET_MODE (dest) == DImode)
    emit_insn (gen_rotldi3 (dest, src1, src2));
  else
    emit_insn (gen_rotlsi3 (dest, src1, src2));
}

/* Generate rtl for a load, shift, and compare of less than a full word.

   LOAD_MODE is the machine mode for the loads.
   DIFF is the reg for the difference.
   CMP_REM is the reg containing the remaining bytes to compare.
   DCOND is the CCUNS reg for the compare if we are doing P9 code with setb.
   SRC1_ADDR is the first source address.
   SRC2_ADDR is the second source address.
   ORIG_SRC1 is the original first source block's address rtx.
   ORIG_SRC2 is the original second source block's address rtx.  */
static void
do_load_mask_compare (const machine_mode load_mode, rtx diff, rtx cmp_rem, rtx dcond,
		      rtx src1_addr, rtx src2_addr, rtx orig_src1, rtx orig_src2)
{
  HOST_WIDE_INT load_mode_size = GET_MODE_SIZE (load_mode);
  rtx shift_amount = gen_reg_rtx (word_mode);
  rtx d1 = gen_reg_rtx (word_mode);
  rtx d2 = gen_reg_rtx (word_mode);

  do_load_for_compare_from_addr (load_mode, d1, src1_addr, orig_src1);
  do_load_for_compare_from_addr (load_mode, d2, src2_addr, orig_src2);
  do_sub3 (shift_amount, GEN_INT (load_mode_size), cmp_rem);

  if (word_mode == DImode)
    {
      emit_insn (gen_ashldi3 (shift_amount, shift_amount,
			      GEN_INT (LOG2_BITS_PER_UNIT)));
      emit_insn (gen_lshrdi3 (d1, d1,
			      gen_lowpart (SImode, shift_amount)));
      emit_insn (gen_lshrdi3 (d2, d2,
			      gen_lowpart (SImode, shift_amount)));
    }
  else
    {
      emit_insn (gen_ashlsi3 (shift_amount, shift_amount,
			      GEN_INT (LOG2_BITS_PER_UNIT)));
      emit_insn (gen_lshrsi3 (d1, d1, shift_amount));
      emit_insn (gen_lshrsi3 (d2, d2, shift_amount));
    }

  if (TARGET_P9_MISC)
    {
      /* Generate a compare, and convert with a setb later.  */
      rtx cmp = gen_rtx_COMPARE (CCUNSmode, d1, d2);
      emit_insn (gen_rtx_SET (dcond, cmp));
    }
  else
    {
      if (word_mode == DImode)
	emit_insn (gen_subfdi3_carry (diff, d2, d1));
      else
	emit_insn (gen_subfsi3_carry (diff, d2, d1));
    }
}

/* Generate rtl for an overlapping load and compare of less than a
   full load_mode.  This assumes that the previous word is part of the
   block being compared so it's ok to back up part of a word so we can
   compare the last unaligned full word that ends at the end of the block.

   LOAD_MODE is the machine mode for the loads.
   ISCONST tells whether the remaining length is a constant or in a register.
   BYTES_REM is the remaining length if ISCONST is true.
   DIFF is the reg for the difference.
   CMP_REM is the reg containing the remaining bytes to compare if !ISCONST.
   DCOND is the CCUNS reg for the compare if we are doing P9 code with setb.
   SRC1_ADDR is the first source address.
   SRC2_ADDR is the second source address.
   ORIG_SRC1 is the original first source block's address rtx.
   ORIG_SRC2 is the original second source block's address rtx.  */
static void
do_overlap_load_compare (machine_mode load_mode, bool isConst,
			HOST_WIDE_INT bytes_rem, rtx diff,
			rtx cmp_rem, rtx dcond, rtx src1_addr, rtx src2_addr,
			rtx orig_src1, rtx orig_src2)
{
  HOST_WIDE_INT load_mode_size = GET_MODE_SIZE (load_mode);
  HOST_WIDE_INT addr_adj = load_mode_size - bytes_rem;
  rtx d1 = gen_reg_rtx (word_mode);
  rtx d2 = gen_reg_rtx (word_mode);

  rtx addr1, addr2;
  if (!isConst || addr_adj)
    {
      rtx adj_reg = gen_reg_rtx (word_mode);
      if (isConst)
	emit_move_insn (adj_reg, GEN_INT (-addr_adj));
      else
	{
	  rtx reg_lms = gen_reg_rtx (word_mode);
	  emit_move_insn (reg_lms, GEN_INT (load_mode_size));
	  do_sub3 (adj_reg, cmp_rem, reg_lms);
	}

      addr1 = gen_rtx_PLUS (word_mode, src1_addr, adj_reg);
      addr2 = gen_rtx_PLUS (word_mode, src2_addr, adj_reg);
    }
  else
    {
      addr1 = src1_addr;
      addr2 = src2_addr;
    }

  do_load_for_compare_from_addr (load_mode, d1, addr1, orig_src1);
  do_load_for_compare_from_addr (load_mode, d2, addr2, orig_src2);

  if (TARGET_P9_MISC)
    {
      /* Generate a compare, and convert with a setb later.  */
      rtx cmp = gen_rtx_COMPARE (CCUNSmode, d1, d2);
      emit_insn (gen_rtx_SET (dcond, cmp));
    }
  else
    {
      if (word_mode == DImode)
	emit_insn (gen_subfdi3_carry (diff, d2, d1));
      else
	emit_insn (gen_subfsi3_carry (diff, d2, d1));
    }
}

/* Expand a block compare operation using loop code, and return true
   if successful.  Return false if we should let the compiler generate
   normal code, probably a memcmp call.

   OPERANDS[0] is the target (result).
   OPERANDS[1] is the first source.
   OPERANDS[2] is the second source.
   OPERANDS[3] is the length.
   OPERANDS[4] is the alignment.  */
bool
expand_compare_loop (rtx operands[])
{
  rtx target = operands[0];
  rtx orig_src1 = operands[1];
  rtx orig_src2 = operands[2];
  rtx bytes_rtx = operands[3];
  rtx align_rtx = operands[4];

  /* This case is complicated to handle because the subtract
     with carry instructions do not generate the 64-bit
     carry and so we must emit code to calculate it ourselves.
     We choose not to implement this yet.  */
  if (TARGET_32BIT && TARGET_POWERPC64)
    return false;

  /* Allow non-const length.  */
  int bytes_is_const = CONST_INT_P (bytes_rtx);

  /* This must be a fixed size alignment.  */
  if (!CONST_INT_P (align_rtx))
    return false;

  HOST_WIDE_INT align1 = MEM_ALIGN (orig_src1) / BITS_PER_UNIT;
  HOST_WIDE_INT align2 = MEM_ALIGN (orig_src2) / BITS_PER_UNIT;
  HOST_WIDE_INT minalign = MIN (align1, align2);

  bool isP7 = (rs6000_tune == PROCESSOR_POWER7);

  gcc_assert (GET_MODE (target) == SImode);

  /* Anything to move?	*/
  HOST_WIDE_INT bytes = 0;
  if (bytes_is_const)
    bytes = INTVAL (bytes_rtx);

  if (bytes_is_const && bytes == 0)
    return true;

  /* Limit the amount we compare, if known statically.  */
  HOST_WIDE_INT max_bytes;
  switch (rs6000_tune)
    {
    case PROCESSOR_POWER7:
      if (!bytes_is_const)
	if (minalign < 8)
	  max_bytes = 0;
	else
	  max_bytes = 128;
      else
	if (minalign < 8)
	  max_bytes = 32;
	else
	  max_bytes = 128;
      break;
    case PROCESSOR_POWER8:
      if (!bytes_is_const)
	max_bytes = 0;
      else
	if (minalign < 8)
	  max_bytes = 128;
	else
	  max_bytes = 64;
      break;
    case PROCESSOR_POWER9:
      if (bytes_is_const)
	max_bytes = 191;
      else
	max_bytes = 0;
      break;
    default:
      max_bytes = 128;
    }

  /* Allow the option to override the default.  */
  if (rs6000_block_compare_inline_loop_limit >= 0)
    max_bytes = (unsigned HOST_WIDE_INT) rs6000_block_compare_inline_loop_limit;

  if (max_bytes == 0)
    return false;

  rtx cmp_rem = gen_reg_rtx (word_mode);  /* Remainder for library call.  */
  rtx loop_cmp = gen_reg_rtx (word_mode); /* Actual amount compared by loop.  */
  HOST_WIDE_INT niter;
  rtx iter = gen_reg_rtx (word_mode);
  rtx iv1 = gen_reg_rtx (word_mode);
  rtx iv2 = gen_reg_rtx (word_mode);
  rtx d1_1 = gen_reg_rtx (word_mode);  /* Addr expression src1+iv1 */
  rtx d1_2 = gen_reg_rtx (word_mode);  /* Addr expression src1+iv2 */
  rtx d2_1 = gen_reg_rtx (word_mode);  /* Addr expression src2+iv1 */
  rtx d2_2 = gen_reg_rtx (word_mode);  /* Addr expression src2+iv2 */

  /* Strip unneeded subreg from length if there is one.  */
  if (SUBREG_P (bytes_rtx) && subreg_lowpart_p (bytes_rtx))
    bytes_rtx = SUBREG_REG (bytes_rtx);
  /* Extend bytes_rtx to word_mode if needed.  But, we expect only to
   maybe have to deal with the case were bytes_rtx is SImode and
   word_mode is DImode.  */
  if (!bytes_is_const)
    {
      if (GET_MODE_SIZE (GET_MODE (bytes_rtx)) > GET_MODE_SIZE (word_mode))
	/* Do not expect length longer than word_mode.  */
	return false; 
      else if (GET_MODE_SIZE (GET_MODE (bytes_rtx)) < GET_MODE_SIZE (word_mode))
	{
	  bytes_rtx = force_reg (GET_MODE (bytes_rtx), bytes_rtx);
	  bytes_rtx = force_reg (word_mode,
				 gen_rtx_fmt_e (ZERO_EXTEND, word_mode,
						bytes_rtx));
	}
      else
	/* Make sure it's in a register before we get started.  */
	bytes_rtx = force_reg (GET_MODE (bytes_rtx), bytes_rtx);
    }

  machine_mode load_mode = word_mode;
  HOST_WIDE_INT load_mode_size = GET_MODE_SIZE (load_mode);

  /* Number of bytes per iteration of the unrolled loop.  */
  HOST_WIDE_INT loop_bytes = 2 * load_mode_size;
  /* max iters and bytes compared in the loop.  */
  HOST_WIDE_INT max_loop_iter = max_bytes / loop_bytes;
  HOST_WIDE_INT max_loop_bytes = max_loop_iter * loop_bytes;
  int l2lb = floor_log2 (loop_bytes);

  if (bytes_is_const && (max_bytes < load_mode_size
			 || !IN_RANGE (bytes, load_mode_size, max_bytes)))
    return false;

  bool no_remainder_code = false;
  rtx final_label = gen_label_rtx ();
  rtx final_ref = gen_rtx_LABEL_REF (VOIDmode, final_label);
  rtx diff_label = gen_label_rtx ();
  rtx library_call_label = NULL;
  rtx cleanup_label = gen_label_rtx ();

  rtx cr;

  rtx src1_addr = copy_addr_to_reg (XEXP (orig_src1, 0));
  rtx src2_addr = copy_addr_to_reg (XEXP (orig_src2, 0));

  /* Difference found is stored here before jump to diff_label.  */
  rtx diff = gen_reg_rtx (word_mode);
  rtx j;

  /* Example of generated code for 35 bytes aligned 1 byte.
     
	     mtctr 8
	     li 6,0
	     li 5,8
     .L13:
	     ldbrx 7,3,6
	     ldbrx 9,10,6
	     ldbrx 0,3,5
	     ldbrx 4,10,5
	     addi 6,6,16
	     addi 5,5,16
	     subfc. 9,9,7
	     bne 0,.L10
	     subfc. 9,4,0
	     bdnzt 2,.L13
	     bne 0,.L10
	     add 3,3,6
	     add 10,10,6
	     addi 9,3,-5
	     ldbrx 7,0,9
	     addi 9,10,-5
	     ldbrx 9,0,9
	     subfc 9,9,7
	     .p2align 4,,15
     .L10:
	     popcntd 9,9
	     subfe 10,10,10
	     or 9,9,10
     
     Compiled with -fno-reorder-blocks for clarity.  */

  /* Structure of what we're going to do:
     Two separate lengths: what we will compare before bailing to library
	call (max_bytes), and the total length to be checked.
     if length <= 16, branch to linear cleanup code starting with
	remainder length check (length not known at compile time)
     set up 2 iv's and load count reg, compute remainder length
     unrollx2 compare loop
     if loop exit due to a difference, branch to difference handling code
     if remainder length < 8, branch to final cleanup compare
     load and compare 8B
     final cleanup comparison (depends on alignment and length)
	load 8B, shift off bytes past length, compare
	load 8B ending at last byte and compare
	load/compare 1 byte at a time (short block abutting 4k boundary)
     difference handling, 64->32 conversion
     final result
     branch around memcmp call
     memcmp library call
  */

  /* If bytes is not const, compare length and branch directly
     to the cleanup code that can handle 0-16 bytes if length
     is >= 16.  Stash away bytes-max_bytes for the library call.  */
  if (bytes_is_const)
    {
      /* These need to be set for some of the places we may jump to.  */
      if (bytes > max_bytes)
	{
	  no_remainder_code = true;
	  niter = max_loop_iter;
	  library_call_label = gen_label_rtx ();
	}
      else
	{
	  niter = bytes / loop_bytes;
	}
      emit_move_insn (iter, GEN_INT (niter));
      emit_move_insn (loop_cmp, GEN_INT (niter * loop_bytes));
      emit_move_insn (cmp_rem, GEN_INT (bytes - niter * loop_bytes));
    }
  else
    {
      library_call_label = gen_label_rtx ();

      /* If we go to the cleanup code, it expects length to be in cmp_rem.  */
      emit_move_insn (cmp_rem, bytes_rtx);

      /* Check for > max_bytes bytes.  We want to bail out as quickly as
	 possible if we have to go over to memcmp.  */
      do_ifelse (CCmode, GT, bytes_rtx, GEN_INT (max_bytes),
		 NULL_RTX, library_call_label);

      /* Check for < loop_bytes bytes.  */
      do_ifelse (CCmode, LT, bytes_rtx, GEN_INT (loop_bytes),
		 NULL_RTX, cleanup_label);

      /* Loop compare bytes and iterations if bytes>max_bytes.  */
      rtx mb_reg = gen_reg_rtx (word_mode);
      emit_move_insn (mb_reg, GEN_INT (max_loop_bytes));
      rtx mi_reg = gen_reg_rtx (word_mode);
      emit_move_insn (mi_reg, GEN_INT (max_loop_iter));

      /* Compute number of loop iterations if bytes <= max_bytes.  */
      if (word_mode == DImode)
	emit_insn (gen_lshrdi3 (iter, bytes_rtx, GEN_INT (l2lb)));
      else
	emit_insn (gen_lshrsi3 (iter, bytes_rtx, GEN_INT (l2lb)));

      /* Compute bytes to compare in loop if bytes <= max_bytes.  */
      rtx mask = GEN_INT (HOST_WIDE_INT_M1U << l2lb);
      if (word_mode == DImode)
	{
	  emit_insn (gen_anddi3 (loop_cmp, bytes_rtx, mask));
	}
      else
	{
	  emit_insn (gen_andsi3 (loop_cmp, bytes_rtx, mask));
	}

      /* Check for bytes <= max_bytes.  */
      if (TARGET_ISEL)
	{
	  /* P9 has fast isel so we use one compare and two isel.  */
	  cr = gen_reg_rtx (CCmode);
	  rtx compare_rtx = gen_rtx_COMPARE (CCmode, bytes_rtx,
					     GEN_INT (max_bytes));
	  emit_move_insn (cr, compare_rtx);
	  rtx cmp_rtx = gen_rtx_LE (VOIDmode, cr, const0_rtx);
	  do_isel (loop_cmp, cmp_rtx, loop_cmp, mb_reg, cr);
	  do_isel (iter, cmp_rtx, iter, mi_reg, cr);
	}
      else
	{
	  rtx lab_after = gen_label_rtx ();
	  do_ifelse (CCmode, LE, bytes_rtx, GEN_INT (max_bytes),
		     NULL_RTX, lab_after);
	  emit_move_insn (loop_cmp, mb_reg);
	  emit_move_insn (iter, mi_reg);
	  emit_label (lab_after);
	}

      /* Now compute remainder bytes which isn't used until after the loop.  */
      do_sub3 (cmp_rem, bytes_rtx, loop_cmp);
    }

  rtx dcond = NULL_RTX; /* Used for when we jump to diff_label.  */
  /* For p9 we need to have just one of these as multiple places define
     it and it gets used by the setb at the end.  */
  if (TARGET_P9_MISC)
    dcond = gen_reg_rtx (CCUNSmode);

  if (!bytes_is_const || bytes >= loop_bytes)
    {
      /* It should not be possible to come here if remaining bytes is
	 < 16 in the runtime case either.  Compute number of loop
	 iterations.  We compare 2*word_mode per iteration so 16B for
	 64-bit code and 8B for 32-bit.  Set up two induction
	 variables and load count register.  */

      /* HACK ALERT: create hard reg for CTR here.  If we just use a
	 pseudo, cse will get rid of it and then the allocator will
	 see it used in the lshr above and won't give us ctr.  */
      rtx ctr = gen_rtx_REG (Pmode, CTR_REGNO);
      emit_move_insn (ctr, iter);
      emit_move_insn (diff, GEN_INT (0));
      emit_move_insn (iv1, GEN_INT (0));
      emit_move_insn (iv2, GEN_INT (load_mode_size));

      /* inner loop to compare 2*word_mode */
      rtx loop_top_label = gen_label_rtx ();
      emit_label (loop_top_label);

      rtx src1_ix1 = gen_rtx_PLUS (word_mode, src1_addr, iv1);
      rtx src2_ix1 = gen_rtx_PLUS (word_mode, src2_addr, iv1);

      do_load_for_compare_from_addr (load_mode, d1_1,
				     src1_ix1, orig_src1);
      do_load_for_compare_from_addr (load_mode, d2_1,
				     src2_ix1, orig_src2);
      do_add3 (iv1, iv1, GEN_INT (loop_bytes));

      rtx src1_ix2 = gen_rtx_PLUS (word_mode, src1_addr, iv2);
      rtx src2_ix2 = gen_rtx_PLUS (word_mode, src2_addr, iv2);

      do_load_for_compare_from_addr (load_mode, d1_2,
				     src1_ix2, orig_src1);
      do_load_for_compare_from_addr (load_mode, d2_2,
				     src2_ix2, orig_src2);
      do_add3 (iv2, iv2, GEN_INT (loop_bytes));

      if (TARGET_P9_MISC)
	{
	  /* Generate a compare, and convert with a setb later.  */
	  rtx cmp = gen_rtx_COMPARE (CCUNSmode, d1_1, d2_1);
	  emit_insn (gen_rtx_SET (dcond, cmp));
	}
      else
	{
	  dcond = gen_reg_rtx (CCmode);
	  if (word_mode == DImode)
	    emit_insn (gen_subfdi3_carry_dot2 (diff, d2_1, d1_1, dcond));
	  else
	    emit_insn (gen_subfsi3_carry_dot2 (diff, d2_1, d1_1, dcond));
	}

      do_ifelse (GET_MODE (dcond), NE, NULL_RTX, NULL_RTX,
		 dcond, diff_label);

      if (TARGET_P9_MISC)
	{
	  /* Generate a compare, and convert with a setb later.  */
	  rtx cmp = gen_rtx_COMPARE (CCUNSmode, d1_2, d2_2);
	  emit_insn (gen_rtx_SET (dcond, cmp));
	}
      else
	{
	  dcond = gen_reg_rtx (CCmode);
	  if (word_mode == DImode)
	    emit_insn (gen_subfdi3_carry_dot2 (diff, d2_2, d1_2, dcond));
	  else
	    emit_insn (gen_subfsi3_carry_dot2 (diff, d2_2, d1_2, dcond));
	}

      rtx eqrtx = gen_rtx_EQ (VOIDmode, d1_2, d2_2);
      if (TARGET_64BIT)
	j = emit_jump_insn (gen_bdnztf_di (loop_top_label, ctr, ctr,
					   eqrtx, dcond));
      else
	j = emit_jump_insn (gen_bdnztf_si (loop_top_label, ctr, ctr,
					   eqrtx, dcond));
      JUMP_LABEL (j) = loop_top_label;
      LABEL_NUSES (loop_top_label) += 1;
    }

  HOST_WIDE_INT bytes_remaining = 0;
  if (bytes_is_const)
    bytes_remaining = (bytes % loop_bytes);

  /* If diff is nonzero, branch to difference handling
     code.  If we exit here with a nonzero diff, it is
     because the second word differed.  */
  if (TARGET_P9_MISC)
    do_ifelse (CCUNSmode, NE, NULL_RTX, NULL_RTX, dcond, diff_label);
  else
    do_ifelse (CCmode, NE, diff, const0_rtx, NULL_RTX, diff_label);

  if (library_call_label != NULL && bytes_is_const && bytes > max_bytes)
    {
      /* If the length is known at compile time, then we will always
	 have a remainder to go to the library call with.  */
      rtx library_call_ref = gen_rtx_LABEL_REF (VOIDmode, library_call_label);
      j = emit_jump_insn (gen_rtx_SET (pc_rtx, library_call_ref));
      JUMP_LABEL (j) = library_call_label;
      LABEL_NUSES (library_call_label) += 1;
      emit_barrier ();
    }

  if (bytes_is_const && bytes_remaining == 0)
    {
      /* No remainder and if we are here then diff is 0 so just return 0 */
      if (TARGET_64BIT)
	emit_insn (gen_movsi (target, gen_lowpart (SImode, diff)));
      else
	emit_move_insn (target, diff);
      j = emit_jump_insn (gen_rtx_SET (pc_rtx, final_ref));
      JUMP_LABEL (j) = final_label;
      LABEL_NUSES (final_label) += 1;
      emit_barrier ();
    }
  else if (!no_remainder_code)
    {
      /* Update addresses to point to the next word to examine.  */
      do_add3 (src1_addr, src1_addr, iv1);
      do_add3 (src2_addr, src2_addr, iv1);

      emit_label (cleanup_label);

      if (!bytes_is_const)
	{
	  /* If we're dealing with runtime length, we have to check if
	     it's zero after the loop. When length is known at compile
	     time the no-remainder condition is dealt with above.  By
	     doing this after cleanup_label, we also deal with the
	     case where length is 0 at the start and we bypass the
	     loop with a branch to cleanup_label.  */
	  emit_move_insn (target, const0_rtx);
	  do_ifelse (CCmode, EQ, cmp_rem, const0_rtx,
		     NULL_RTX, final_label);
	}

      rtx final_cleanup = gen_label_rtx ();
      rtx cmp_rem_before = gen_reg_rtx (word_mode);
      /* Compare one more word_mode chunk if needed.  */
      if (!bytes_is_const || bytes_remaining >= load_mode_size)
	{
	  /* If remainder length < word length, branch to final
	     cleanup compare.  */
	  if (!bytes_is_const)
	    do_ifelse (CCmode, LT, cmp_rem, GEN_INT (load_mode_size),
		       NULL_RTX, final_cleanup);

	  /* load and compare 8B */
	  do_load_for_compare_from_addr (load_mode, d1_1,
					 src1_addr, orig_src1);
	  do_load_for_compare_from_addr (load_mode, d2_1,
					 src2_addr, orig_src2);

	  /* Compare the word, see if we need to do the last partial.  */
	  if (TARGET_P9_MISC)
	    {
	      /* Generate a compare, and convert with a setb later.  */
	      rtx cmp = gen_rtx_COMPARE (CCUNSmode, d1_1, d2_1);
	      emit_insn (gen_rtx_SET (dcond, cmp));
	    }
	  else
	    {
	      dcond = gen_reg_rtx (CCmode);
	      if (word_mode == DImode)
		emit_insn (gen_subfdi3_carry_dot2 (diff, d2_1, d1_1, dcond));
	      else
		emit_insn (gen_subfsi3_carry_dot2 (diff, d2_1, d1_1, dcond));
	    }

	  do_ifelse (GET_MODE (dcond), NE, NULL_RTX, NULL_RTX,
		     dcond, diff_label);

	  do_add3 (src1_addr, src1_addr, GEN_INT (load_mode_size));
	  do_add3 (src2_addr, src2_addr, GEN_INT (load_mode_size));
	  emit_move_insn (cmp_rem_before, cmp_rem);
	  do_add3 (cmp_rem, cmp_rem, GEN_INT (-load_mode_size));
	  if (bytes_is_const)
	    bytes_remaining -= load_mode_size;
	  else
	    /* See if remaining length is now zero.  We previously set
	       target to 0 so we can just jump to the end.  */
	    do_ifelse (CCmode, EQ, cmp_rem, const0_rtx,
		       NULL_RTX, final_label);

	}

      /* Cases:
	 bytes_is_const
	   We can always shift back to do an overlapping compare
	   of the last chunk because we know length >= 8.

	 !bytes_is_const
	   align>=load_mode_size
	     Read word_mode and mask
	   align<load_mode_size
	     avoid stepping past end

	  Three strategies:
	  * decrement address and do overlapping compare
	  * read word_mode and mask
	  * carefully avoid crossing 4k boundary
       */

      if ((!bytes_is_const || (bytes_is_const && bytes_remaining && isP7))
	  && align1 >= load_mode_size && align2 >= load_mode_size)
	{
	  /* Alignment is larger than word_mode so we do not need to be
	     concerned with extra page crossings.  But, we do not know
	     that the length is larger than load_mode_size so we might
	     end up compareing against data before the block if we try
	     an overlapping compare.  Also we use this on P7 for fixed length
	     remainder because P7 doesn't like overlapping unaligned.
	     Strategy: load 8B, shift off bytes past length, and compare.  */
	  emit_label (final_cleanup);
	  do_load_mask_compare (load_mode, diff, cmp_rem, dcond,
				src1_addr, src2_addr, orig_src1, orig_src2);
	}
      else if (bytes_remaining && bytes_is_const)
	{
	  /* We do not do loop expand if length < 32 so we know at the
	     end we can do an overlapping compare.
	     Strategy: shift address back and do word_mode load that
	     ends at the end of the block.  */
	  emit_label (final_cleanup);
	  do_overlap_load_compare (load_mode, true, bytes_remaining, diff,
				   cmp_rem, dcond, src1_addr, src2_addr,
				   orig_src1, orig_src2);
	}
      else if (!bytes_is_const)
	{
	  rtx handle4k_label = gen_label_rtx ();
	  rtx nonconst_overlap = gen_label_rtx ();
	  emit_label (nonconst_overlap);

	  /* Here we have to handle the case where whe have runtime
	     length which may be too short for overlap compare, and
	     alignment is not at least load_mode_size so we have to
	     tread carefully to avoid stepping across 4k boundaries.  */

	  /* If the length after the loop was larger than word_mode
	     size, we can just do an overlapping compare and we're
	     done.  We fall through to this code from the word_mode
	     compare that preceeds this.  */
	  do_overlap_load_compare (load_mode, false, 0, diff,
				   cmp_rem, dcond, src1_addr, src2_addr,
				   orig_src1, orig_src2);

	  rtx diff_ref = gen_rtx_LABEL_REF (VOIDmode, diff_label);
	  j = emit_jump_insn (gen_rtx_SET (pc_rtx, diff_ref));
	  JUMP_LABEL (j) = diff_label;
	  LABEL_NUSES (diff_label) += 1;
	  emit_barrier ();

	  /* If we couldn't do the overlap compare we have to be more
	     careful of the 4k boundary.  Test to see if either
	     address is less than word_mode_size away from a 4k
	     boundary.  If not, then we can do a load/shift/compare
	     and we are done.  We come to this code if length was less
	     than word_mode_size.  */

	  emit_label (final_cleanup);

	  /* We can still avoid the slow case if the length was larger
	     than one loop iteration, in which case go do the overlap
	     load compare path.  */
	  do_ifelse (CCmode, GT, bytes_rtx, GEN_INT (loop_bytes),
		     NULL_RTX, nonconst_overlap);

	  rtx rem4k = gen_reg_rtx (word_mode);
	  rtx dist1 = gen_reg_rtx (word_mode);
	  rtx dist2 = gen_reg_rtx (word_mode);
	  do_sub3 (rem4k, GEN_INT (4096), cmp_rem);
	  if (word_mode == SImode)
	    emit_insn (gen_andsi3 (dist1, src1_addr, GEN_INT (0xfff)));
	  else
	    emit_insn (gen_anddi3 (dist1, src1_addr, GEN_INT (0xfff)));
	  do_ifelse (CCmode, LE, dist1, rem4k, NULL_RTX, handle4k_label);
	  if (word_mode == SImode)
	    emit_insn (gen_andsi3 (dist2, src2_addr, GEN_INT (0xfff)));
	  else
	    emit_insn (gen_anddi3 (dist2, src2_addr, GEN_INT (0xfff)));
	  do_ifelse (CCmode, LE, dist2, rem4k, NULL_RTX, handle4k_label);

	  /* We don't have a 4k boundary to deal with, so do
	     a load/shift/compare and jump to diff.  */

	  do_load_mask_compare (load_mode, diff, cmp_rem, dcond,
				src1_addr, src2_addr, orig_src1, orig_src2);

	  j = emit_jump_insn (gen_rtx_SET (pc_rtx, diff_ref));
	  JUMP_LABEL (j) = diff_label;
	  LABEL_NUSES (diff_label) += 1;
	  emit_barrier ();

	  /* Finally in the unlikely case we are inching up to a
	     4k boundary we use a compact lbzx/compare loop to do
	     it a byte at a time.  */

	  emit_label (handle4k_label);

	  rtx ctr = gen_rtx_REG (Pmode, CTR_REGNO);
	  emit_move_insn (ctr, cmp_rem);
	  rtx ixreg = gen_reg_rtx (Pmode);
	  emit_move_insn (ixreg, const0_rtx);

	  rtx src1_ix = gen_rtx_PLUS (word_mode, src1_addr, ixreg);
	  rtx src2_ix = gen_rtx_PLUS (word_mode, src2_addr, ixreg);
	  rtx d1 = gen_reg_rtx (word_mode);
	  rtx d2 = gen_reg_rtx (word_mode);

	  rtx fc_loop = gen_label_rtx ();
	  emit_label (fc_loop);

	  do_load_for_compare_from_addr (QImode, d1, src1_ix, orig_src1);
	  do_load_for_compare_from_addr (QImode, d2, src2_ix, orig_src2);

	  do_add3 (ixreg, ixreg, const1_rtx);

	  rtx cond = gen_reg_rtx (CCmode);
	  rtx subexpr = gen_rtx_MINUS (word_mode, d1, d2);
	  rs6000_emit_dot_insn (diff, subexpr, 2, cond);

	  rtx eqrtx = gen_rtx_EQ (VOIDmode, d1, d2);
	  if (TARGET_64BIT)
	    j = emit_jump_insn (gen_bdnztf_di (fc_loop, ctr, ctr,
					       eqrtx, cond));
	  else
	    j = emit_jump_insn (gen_bdnztf_si (fc_loop, ctr, ctr,
					       eqrtx, cond));
	  JUMP_LABEL (j) = fc_loop;
	  LABEL_NUSES (fc_loop) += 1;

	  if (TARGET_64BIT)
	    emit_insn (gen_movsi (target, gen_lowpart (SImode, diff)));
	  else
	    emit_move_insn (target, diff);

	  /* Since we are comparing bytes, the difference can be used
	     as the final result and we are done here.  */
	  j = emit_jump_insn (gen_rtx_SET (pc_rtx, final_ref));
	  JUMP_LABEL (j) = final_label;
	  LABEL_NUSES (final_label) += 1;
	  emit_barrier ();
	}
    }

  emit_label (diff_label);
  /* difference handling, 64->32 conversion */

  /* We need to produce DI result from sub, then convert to target SI
     while maintaining <0 / ==0 / >0 properties.  This sequence works:
     subfc L,A,B
     subfe H,H,H
     popcntd L,L
     rldimi L,H,6,0

     This is an alternate one Segher cooked up if somebody
     wants to expand this for something that doesn't have popcntd:
     subfc L,a,b
     subfe H,x,x
     addic t,L,-1
     subfe v,t,L
     or z,v,H

     And finally, p9 can just do this:
     cmpld A,B
     setb r */

  if (TARGET_P9_MISC)
    emit_insn (gen_setb_unsigned (target, dcond));
  else
    {
      if (TARGET_64BIT)
	{
	  rtx tmp_reg_ca = gen_reg_rtx (DImode);
	  emit_insn (gen_subfdi3_carry_in_xx (tmp_reg_ca));
	  emit_insn (gen_popcntddi2 (diff, diff));
	  emit_insn (gen_iordi3 (diff, diff, tmp_reg_ca));
	  emit_insn (gen_movsi (target, gen_lowpart (SImode, diff)));
	}
      else
	{
	  rtx tmp_reg_ca = gen_reg_rtx (SImode);
	  emit_insn (gen_subfsi3_carry_in_xx (tmp_reg_ca));
	  emit_insn (gen_popcntdsi2 (diff, diff));
	  emit_insn (gen_iorsi3 (target, diff, tmp_reg_ca));
	}
    }

  if (library_call_label != NULL)
    {
      /* Branch around memcmp call.  */
      j = emit_jump_insn (gen_rtx_SET (pc_rtx, final_ref));
      JUMP_LABEL (j) = final_label;
      LABEL_NUSES (final_label) += 1;
      emit_barrier ();

      /* Make memcmp library call.  cmp_rem is the remaining bytes that
	 were compared and cmp_rem is the expected amount to be compared
	 by memcmp.  If we don't find a difference in the loop compare, do
	 the library call directly instead of doing a small compare just
	 to get to an arbitrary boundary before calling it anyway.
	 Also, update addresses to point to the next word to examine.  */
      emit_label (library_call_label);

      rtx len_rtx = gen_reg_rtx (word_mode);
      if (bytes_is_const)
	{
	  emit_move_insn (len_rtx, cmp_rem);
	  do_add3 (src1_addr, src1_addr, iv1);
	  do_add3 (src2_addr, src2_addr, iv1);
	}
      else
	emit_move_insn (len_rtx, bytes_rtx);

      tree fun = builtin_decl_explicit (BUILT_IN_MEMCMP);
      emit_library_call_value (XEXP (DECL_RTL (fun), 0),
			       target, LCT_NORMAL, GET_MODE (target),
			       src1_addr, Pmode,
			       src2_addr, Pmode,
			       len_rtx, GET_MODE (len_rtx));
    }

  /* emit final_label */
  emit_label (final_label);
  return true;
}

/* Expand a block compare operation, and return true if successful.
   Return false if we should let the compiler generate normal code,
   probably a memcmp call.

   OPERANDS[0] is the target (result).
   OPERANDS[1] is the first source.
   OPERANDS[2] is the second source.
   OPERANDS[3] is the length.
   OPERANDS[4] is the alignment.  */
bool
expand_block_compare (rtx operands[])
{
  rtx target = operands[0];
  rtx orig_src1 = operands[1];
  rtx orig_src2 = operands[2];
  rtx bytes_rtx = operands[3];
  rtx align_rtx = operands[4];
  HOST_WIDE_INT cmp_bytes = 0;
  rtx src1 = orig_src1;
  rtx src2 = orig_src2;

  /* This case is complicated to handle because the subtract
     with carry instructions do not generate the 64-bit
     carry and so we must emit code to calculate it ourselves.
     We choose not to implement this yet.  */
  if (TARGET_32BIT && TARGET_POWERPC64)
    return false;

  bool isP7 = (rs6000_tune == PROCESSOR_POWER7);

  /* Allow this param to shut off all expansion.  */
  if (rs6000_block_compare_inline_limit == 0)
    return false;

  /* targetm.slow_unaligned_access -- don't do unaligned stuff.
     However slow_unaligned_access returns true on P7 even though the
     performance of this code is good there.  */
  if (!isP7
      && (targetm.slow_unaligned_access (word_mode, MEM_ALIGN (orig_src1))
	  || targetm.slow_unaligned_access (word_mode, MEM_ALIGN (orig_src2))))
    return false;

  /* Unaligned l*brx traps on P7 so don't do this.  However this should
     not affect much because LE isn't really supported on P7 anyway.  */
  if (isP7 && !BYTES_BIG_ENDIAN)
    return false;

  /* If this is not a fixed size compare, try generating loop code and
     if that fails just call memcmp.  */
  if (!CONST_INT_P (bytes_rtx))
    return expand_compare_loop (operands);

  /* This must be a fixed size alignment.  */
  if (!CONST_INT_P (align_rtx))
    return false;

  unsigned int base_align = UINTVAL (align_rtx) / BITS_PER_UNIT;

  gcc_assert (GET_MODE (target) == SImode);

  /* Anything to move?  */
  unsigned HOST_WIDE_INT bytes = UINTVAL (bytes_rtx);
  if (bytes == 0)
    return true;

  rtx tmp_reg_src1 = gen_reg_rtx (word_mode);
  rtx tmp_reg_src2 = gen_reg_rtx (word_mode);
  /* P7/P8 code uses cond for subfc. but P9 uses
     it for cmpld which needs CCUNSmode. */
  rtx cond;
  if (TARGET_P9_MISC)
    cond = gen_reg_rtx (CCUNSmode);
  else
    cond = gen_reg_rtx (CCmode);

  /* If we have an LE target without ldbrx and word_mode is DImode,
     then we must avoid using word_mode.  */
  int word_mode_ok = !(!BYTES_BIG_ENDIAN && !TARGET_LDBRX
		       && word_mode == DImode);

  /* Strategy phase.  How many ops will this take and should we expand it?  */

  unsigned HOST_WIDE_INT offset = 0;
  machine_mode load_mode =
    select_block_compare_mode (offset, bytes, base_align, word_mode_ok);
  unsigned int load_mode_size = GET_MODE_SIZE (load_mode);

  /* We don't want to generate too much code.  The loop code can take
     over for lengths greater than 31 bytes.  */
  unsigned HOST_WIDE_INT max_bytes = rs6000_block_compare_inline_limit;
  if (!IN_RANGE (bytes, 1, max_bytes))
    return expand_compare_loop (operands);

  /* The code generated for p7 and older is not faster than glibc
     memcmp if alignment is small and length is not short, so bail
     out to avoid those conditions.  */
  if (!TARGET_EFFICIENT_OVERLAPPING_UNALIGNED
      && ((base_align == 1 && bytes > 16)
	  || (base_align == 2 && bytes > 32)))
    return false;

  bool generate_6432_conversion = false;
  rtx convert_label = NULL;
  rtx final_label = NULL;

  /* Example of generated code for 18 bytes aligned 1 byte.
     Compiled with -fno-reorder-blocks for clarity.
             ldbrx 10,31,8
             ldbrx 9,7,8
             subfc. 9,9,10
             bne 0,.L6487
             addi 9,12,8
             addi 5,11,8
             ldbrx 10,0,9
             ldbrx 9,0,5
             subfc. 9,9,10
             bne 0,.L6487
             addi 9,12,16
             lhbrx 10,0,9
             addi 9,11,16
             lhbrx 9,0,9
             subf 9,9,10
             b .L6488
             .p2align 4,,15
     .L6487: #convert_label
             popcntd 9,9
             subfe 10,10,10
             or 9,9,10
     .L6488: #final_label
             extsw 10,9

     We start off with DImode for two blocks that jump to the DI->SI conversion
     if the difference is found there, then a final block of HImode that skips
     the DI->SI conversion.  */

  while (bytes > 0)
    {
      unsigned int align = compute_current_alignment (base_align, offset);
      load_mode = select_block_compare_mode (offset, bytes,
					     align, word_mode_ok);
      load_mode_size = GET_MODE_SIZE (load_mode);
      if (bytes >= load_mode_size)
	cmp_bytes = load_mode_size;
      else if (TARGET_EFFICIENT_OVERLAPPING_UNALIGNED)
	{
	  /* Move this load back so it doesn't go past the end.
	     P8/P9 can do this efficiently.  */
	  unsigned int extra_bytes = load_mode_size - bytes;
	  cmp_bytes = bytes;
	  if (extra_bytes < offset)
	    {
	      offset -= extra_bytes;
	      cmp_bytes = load_mode_size;
	      bytes = cmp_bytes;
	    }
	}
      else
	/* P7 and earlier can't do the overlapping load trick fast,
	   so this forces a non-overlapping load and a shift to get
	   rid of the extra bytes.  */
	cmp_bytes = bytes;

      src1 = adjust_address (orig_src1, load_mode, offset);
      src2 = adjust_address (orig_src2, load_mode, offset);

      if (!REG_P (XEXP (src1, 0)))
	{
	  rtx src1_reg = copy_addr_to_reg (XEXP (src1, 0));
	  src1 = replace_equiv_address (src1, src1_reg);
	}
      set_mem_size (src1, load_mode_size);

      if (!REG_P (XEXP (src2, 0)))
	{
	  rtx src2_reg = copy_addr_to_reg (XEXP (src2, 0));
	  src2 = replace_equiv_address (src2, src2_reg);
	}
      set_mem_size (src2, load_mode_size);

      do_load_for_compare (tmp_reg_src1, src1, load_mode);
      do_load_for_compare (tmp_reg_src2, src2, load_mode);

      if (cmp_bytes < load_mode_size)
	{
	  /* Shift unneeded bytes off.  */
	  rtx sh = GEN_INT (BITS_PER_UNIT * (load_mode_size - cmp_bytes));
	  if (word_mode == DImode)
	    {
	      emit_insn (gen_lshrdi3 (tmp_reg_src1, tmp_reg_src1, sh));
	      emit_insn (gen_lshrdi3 (tmp_reg_src2, tmp_reg_src2, sh));
	    }
	  else
	    {
	      emit_insn (gen_lshrsi3 (tmp_reg_src1, tmp_reg_src1, sh));
	      emit_insn (gen_lshrsi3 (tmp_reg_src2, tmp_reg_src2, sh));
	    }
	}

      int remain = bytes - cmp_bytes;
      if (GET_MODE_SIZE (GET_MODE (target)) > GET_MODE_SIZE (load_mode))
	{
	  /* Target is larger than load size so we don't need to
	     reduce result size.  */

	  /* We previously did a block that need 64->32 conversion but
	     the current block does not, so a label is needed to jump
	     to the end.  */
	  if (generate_6432_conversion && !final_label)
	    final_label = gen_label_rtx ();

	  if (remain > 0)
	    {
	      /* This is not the last block, branch to the end if the result
		 of this subtract is not zero.  */
	      if (!final_label)
		final_label = gen_label_rtx ();
	      rtx fin_ref = gen_rtx_LABEL_REF (VOIDmode, final_label);
	      rtx tmp = gen_rtx_MINUS (word_mode, tmp_reg_src1, tmp_reg_src2);
	      rtx cr = gen_reg_rtx (CCmode);
	      rs6000_emit_dot_insn (tmp_reg_src2, tmp, 2, cr);
	      emit_insn (gen_movsi (target,
				    gen_lowpart (SImode, tmp_reg_src2)));
	      rtx ne_rtx = gen_rtx_NE (VOIDmode, cr, const0_rtx);
	      rtx ifelse = gen_rtx_IF_THEN_ELSE (VOIDmode, ne_rtx,
						 fin_ref, pc_rtx);
	      rtx j = emit_jump_insn (gen_rtx_SET (pc_rtx, ifelse));
	      JUMP_LABEL (j) = final_label;
	      LABEL_NUSES (final_label) += 1;
	    }
	  else
	    {
	      if (word_mode == DImode)
		{
		  emit_insn (gen_subdi3 (tmp_reg_src2, tmp_reg_src1,
					 tmp_reg_src2));
		  emit_insn (gen_movsi (target,
					gen_lowpart (SImode, tmp_reg_src2)));
		}
	      else
		emit_insn (gen_subsi3 (target, tmp_reg_src1, tmp_reg_src2));

	      if (final_label)
		{
		  rtx fin_ref = gen_rtx_LABEL_REF (VOIDmode, final_label);
		  rtx j = emit_jump_insn (gen_rtx_SET (pc_rtx, fin_ref));
		  JUMP_LABEL (j) = final_label;
		  LABEL_NUSES (final_label) += 1;
		  emit_barrier ();
		}
	    }
	}
      else
	{
	  /* Do we need a 64->32 conversion block? We need the 64->32
	     conversion even if target size == load_mode size because
	     the subtract generates one extra bit.  */
	  generate_6432_conversion = true;

	  if (remain > 0)
	    {
	      if (!convert_label)
		convert_label = gen_label_rtx ();

	      /* Compare to zero and branch to convert_label if not zero.  */
	      rtx cvt_ref = gen_rtx_LABEL_REF (VOIDmode, convert_label);
	      if (TARGET_P9_MISC)
		{
		/* Generate a compare, and convert with a setb later.  */
		  rtx cmp = gen_rtx_COMPARE (CCUNSmode, tmp_reg_src1,
					     tmp_reg_src2);
		  emit_insn (gen_rtx_SET (cond, cmp));
		}
	      else
		/* Generate a subfc. and use the longer
		   sequence for conversion.  */
		if (TARGET_64BIT)
		  emit_insn (gen_subfdi3_carry_dot2 (tmp_reg_src2, tmp_reg_src2,
						     tmp_reg_src1, cond));
		else
		  emit_insn (gen_subfsi3_carry_dot2 (tmp_reg_src2, tmp_reg_src2,
						     tmp_reg_src1, cond));
	      rtx ne_rtx = gen_rtx_NE (VOIDmode, cond, const0_rtx);
	      rtx ifelse = gen_rtx_IF_THEN_ELSE (VOIDmode, ne_rtx,
						 cvt_ref, pc_rtx);
	      rtx j = emit_jump_insn (gen_rtx_SET (pc_rtx, ifelse));
	      JUMP_LABEL (j) = convert_label;
	      LABEL_NUSES (convert_label) += 1;
	    }
	  else
	    {
	      /* Just do the subtract/compare.  Since this is the last block
		 the convert code will be generated immediately following.  */
	      if (TARGET_P9_MISC)
		{
		  rtx cmp = gen_rtx_COMPARE (CCUNSmode, tmp_reg_src1,
					     tmp_reg_src2);
		  emit_insn (gen_rtx_SET (cond, cmp));
		}
	      else
		if (TARGET_64BIT)
		  emit_insn (gen_subfdi3_carry (tmp_reg_src2, tmp_reg_src2,
						tmp_reg_src1));
		else
		  emit_insn (gen_subfsi3_carry (tmp_reg_src2, tmp_reg_src2,
						tmp_reg_src1));
	    }
	}

      offset += cmp_bytes;
      bytes -= cmp_bytes;
    }

  if (generate_6432_conversion)
    {
      if (convert_label)
	emit_label (convert_label);

      /* We need to produce DI result from sub, then convert to target SI
	 while maintaining <0 / ==0 / >0 properties. This sequence works:
	 subfc L,A,B
	 subfe H,H,H
	 popcntd L,L
	 rldimi L,H,6,0

	 This is an alternate one Segher cooked up if somebody
	 wants to expand this for something that doesn't have popcntd:
	 subfc L,a,b
	 subfe H,x,x
	 addic t,L,-1
	 subfe v,t,L
	 or z,v,H

	 And finally, p9 can just do this:
	 cmpld A,B
	 setb r */

      if (TARGET_P9_MISC)
	{
	  emit_insn (gen_setb_unsigned (target, cond));
	}
      else
	{
	  if (TARGET_64BIT)
	    {
	      rtx tmp_reg_ca = gen_reg_rtx (DImode);
	      emit_insn (gen_subfdi3_carry_in_xx (tmp_reg_ca));
	      emit_insn (gen_popcntddi2 (tmp_reg_src2, tmp_reg_src2));
	      emit_insn (gen_iordi3 (tmp_reg_src2, tmp_reg_src2, tmp_reg_ca));
	      emit_insn (gen_movsi (target, gen_lowpart (SImode, tmp_reg_src2)));
	    }
	  else
	    {
	      rtx tmp_reg_ca = gen_reg_rtx (SImode);
	      emit_insn (gen_subfsi3_carry_in_xx (tmp_reg_ca));
	      emit_insn (gen_popcntdsi2 (tmp_reg_src2, tmp_reg_src2));
	      emit_insn (gen_iorsi3 (target, tmp_reg_src2, tmp_reg_ca));
	    }
	}
    }

  if (final_label)
    emit_label (final_label);

  gcc_assert (bytes == 0);
  return true;
}

/* Generate page crossing check and branch code to set up for
   strncmp when we don't have DI alignment.
   STRNCMP_LABEL is the label to branch if there is a page crossing.
   SRC_ADDR is the string address to be examined.
   BYTES is the max number of bytes to compare.  */
static void
expand_strncmp_align_check (rtx strncmp_label, rtx src_addr, HOST_WIDE_INT bytes)
{
  rtx lab_ref = gen_rtx_LABEL_REF (VOIDmode, strncmp_label);
  rtx src_pgoff = gen_reg_rtx (GET_MODE (src_addr));
  do_and3 (src_pgoff, src_addr, GEN_INT (0xfff));
  rtx cond = gen_reg_rtx (CCmode);
  emit_move_insn (cond, gen_rtx_COMPARE (CCmode, src_pgoff,
					 GEN_INT (4096 - bytes)));

  rtx cmp_rtx = gen_rtx_GE (VOIDmode, cond, const0_rtx);

  rtx ifelse = gen_rtx_IF_THEN_ELSE (VOIDmode, cmp_rtx,
				     lab_ref, pc_rtx);
  rtx j = emit_jump_insn (gen_rtx_SET (pc_rtx, ifelse));
  JUMP_LABEL (j) = strncmp_label;
  LABEL_NUSES (strncmp_label) += 1;
}

/* Generate the final sequence that identifies the differing
   byte and generates the final result, taking into account
   zero bytes:
   
   cmpb              cmpb_result1, src1, src2
   cmpb              cmpb_result2, src1, zero
   orc               cmpb_result1, cmp_result1, cmpb_result2
   cntlzd            get bit of first zero/diff byte
   addi              convert for rldcl use
   rldcl rldcl       extract diff/zero byte
   subf              subtract for final result

   STR1 is the reg rtx for data from string 1.
   STR2 is the reg rtx for data from string 2.
   RESULT is the reg rtx for the comparison result.  */

static void
emit_final_str_compare_gpr (rtx str1, rtx str2, rtx result)
{
  machine_mode m = GET_MODE (str1);
  rtx cmpb_diff = gen_reg_rtx (m);
  rtx cmpb_zero = gen_reg_rtx (m);
  rtx rot_amt = gen_reg_rtx (m);
  rtx zero_reg = gen_reg_rtx (m);

  rtx rot1_1 = gen_reg_rtx (m);
  rtx rot1_2 = gen_reg_rtx (m);
  rtx rot2_1 = gen_reg_rtx (m);
  rtx rot2_2 = gen_reg_rtx (m);

  if (m == SImode)
    {
      emit_insn (gen_cmpbsi3 (cmpb_diff, str1, str2));
      emit_insn (gen_movsi (zero_reg, GEN_INT (0)));
      emit_insn (gen_cmpbsi3 (cmpb_zero, str1, zero_reg));
      emit_insn (gen_one_cmplsi2 (cmpb_diff,cmpb_diff));
      emit_insn (gen_iorsi3 (cmpb_diff, cmpb_diff, cmpb_zero));
      emit_insn (gen_clzsi2 (rot_amt, cmpb_diff));
      emit_insn (gen_addsi3 (rot_amt, rot_amt, GEN_INT (8)));
      emit_insn (gen_rotlsi3 (rot1_1, str1,
			      gen_lowpart (SImode, rot_amt)));
      emit_insn (gen_andsi3_mask (rot1_2, rot1_1, GEN_INT (0xff)));
      emit_insn (gen_rotlsi3 (rot2_1, str2,
			      gen_lowpart (SImode, rot_amt)));
      emit_insn (gen_andsi3_mask (rot2_2, rot2_1, GEN_INT (0xff)));
      emit_insn (gen_subsi3 (result, rot1_2, rot2_2));
    }
  else if (m == DImode)
    {
      emit_insn (gen_cmpbdi3 (cmpb_diff, str1, str2));
      emit_insn (gen_movdi (zero_reg, GEN_INT (0)));
      emit_insn (gen_cmpbdi3 (cmpb_zero, str1, zero_reg));
      emit_insn (gen_one_cmpldi2 (cmpb_diff,cmpb_diff));
      emit_insn (gen_iordi3 (cmpb_diff, cmpb_diff, cmpb_zero));
      emit_insn (gen_clzdi2 (rot_amt, cmpb_diff));
      emit_insn (gen_adddi3 (rot_amt, rot_amt, GEN_INT (8)));
      emit_insn (gen_rotldi3 (rot1_1, str1,
			      gen_lowpart (SImode, rot_amt)));
      emit_insn (gen_anddi3_mask (rot1_2, rot1_1, GEN_INT (0xff)));
      emit_insn (gen_rotldi3 (rot2_1, str2,
			      gen_lowpart (SImode, rot_amt)));
      emit_insn (gen_anddi3_mask (rot2_2, rot2_1, GEN_INT (0xff)));
      emit_insn (gen_subdi3 (result, rot1_2, rot2_2));
    }
  else
    gcc_unreachable ();
    
  return;
}

/* Expand a string compare operation with length, and return
   true if successful. Return false if we should let the
   compiler generate normal code, probably a strncmp call.

   OPERANDS[0] is the target (result).
   OPERANDS[1] is the first source.
   OPERANDS[2] is the second source.
   If NO_LENGTH is zero, then:
   OPERANDS[3] is the length.
   OPERANDS[4] is the alignment in bytes.
   If NO_LENGTH is nonzero, then:
   OPERANDS[3] is the alignment in bytes.  */
bool
expand_strn_compare (rtx operands[], int no_length)
{
  rtx target = operands[0];
  rtx orig_src1 = operands[1];
  rtx orig_src2 = operands[2];
  rtx bytes_rtx, align_rtx;
  if (no_length)
    {
      bytes_rtx = NULL;
      align_rtx = operands[3];
    }
  else
    {
      bytes_rtx = operands[3];
      align_rtx = operands[4];
    }
  unsigned HOST_WIDE_INT cmp_bytes = 0;
  rtx src1_addr = force_reg (Pmode, XEXP (orig_src1, 0));
  rtx src2_addr = force_reg (Pmode, XEXP (orig_src2, 0));

  /* If we have a length, it must be constant. This simplifies things
     a bit as we don't have to generate code to check if we've exceeded
     the length. Later this could be expanded to handle this case.  */
  if (!no_length && !CONST_INT_P (bytes_rtx))
    return false;

  /* This must be a fixed size alignment.  */
  if (!CONST_INT_P (align_rtx))
    return false;

  unsigned int base_align = UINTVAL (align_rtx);
  unsigned int align1 = MEM_ALIGN (orig_src1) / BITS_PER_UNIT;
  unsigned int align2 = MEM_ALIGN (orig_src2) / BITS_PER_UNIT;

  /* targetm.slow_unaligned_access -- don't do unaligned stuff.  */
  if (targetm.slow_unaligned_access (word_mode, align1)
      || targetm.slow_unaligned_access (word_mode, align2))
    return false;

  gcc_assert (GET_MODE (target) == SImode);

  /* If we have an LE target without ldbrx and word_mode is DImode,
     then we must avoid using word_mode.  */
  int word_mode_ok = !(!BYTES_BIG_ENDIAN && !TARGET_LDBRX
		       && word_mode == DImode);

  unsigned int word_mode_size = GET_MODE_SIZE (word_mode);

  unsigned HOST_WIDE_INT offset = 0;
  unsigned HOST_WIDE_INT bytes; /* N from the strncmp args if available.  */
  unsigned HOST_WIDE_INT compare_length; /* How much to compare inline.  */
  if (no_length)
    /* Use this as a standin to determine the mode to use.  */
    bytes = rs6000_string_compare_inline_limit * word_mode_size;
  else
    bytes = UINTVAL (bytes_rtx);

  machine_mode load_mode =
    select_block_compare_mode (offset, bytes, base_align, word_mode_ok);
  unsigned int load_mode_size = GET_MODE_SIZE (load_mode);
  compare_length = rs6000_string_compare_inline_limit * load_mode_size;

  /* If we have equality at the end of the last compare and we have not
     found the end of the string, we need to call strcmp/strncmp to
     compare the remainder.  */
  bool equality_compare_rest = false;

  if (no_length)
    {
      bytes = compare_length;
      equality_compare_rest = true;
    }
  else
    {
      if (bytes <= compare_length)
	compare_length = bytes;
      else
	equality_compare_rest = true;
    }

  rtx result_reg = gen_reg_rtx (word_mode);
  rtx final_move_label = gen_label_rtx ();
  rtx final_label = gen_label_rtx ();
  rtx begin_compare_label = NULL;
  unsigned int required_align = 8;

  if (base_align < required_align)
    {
      /* Generate code that checks distance to 4k boundary for this case.  */
      begin_compare_label = gen_label_rtx ();
      rtx strncmp_label = gen_label_rtx ();
      rtx jmp;

      /* Strncmp for power8 in glibc does this:
	 rldicl r8,r3,0,52
	 cmpldi cr7,r8,4096-16
	 bgt    cr7,L(pagecross) */

      /* Make sure that the length we use for the alignment test and
         the subsequent code generation are in agreement so we do not
         go past the length we tested for a 4k boundary crossing.  */
      unsigned HOST_WIDE_INT align_test = compare_length;
      if (align_test < 8)
        {
          align_test = HOST_WIDE_INT_1U << ceil_log2 (align_test);
          base_align = align_test;
        }
      else
        {
          align_test = ROUND_UP (align_test, required_align);
          base_align = required_align;
        }

      if (align1 < required_align)
        expand_strncmp_align_check (strncmp_label, src1_addr, align_test);
      if (align2 < required_align)
        expand_strncmp_align_check (strncmp_label, src2_addr, align_test);

      /* Now generate the following sequence:
	 - branch to begin_compare
	 - strncmp_label
	 - call to strncmp
	 - branch to final_label
	 - begin_compare_label */

      rtx cmp_ref = gen_rtx_LABEL_REF (VOIDmode, begin_compare_label);
      jmp = emit_jump_insn (gen_rtx_SET (pc_rtx, cmp_ref));
      JUMP_LABEL (jmp) = begin_compare_label;
      LABEL_NUSES (begin_compare_label) += 1;
      emit_barrier ();

      emit_label (strncmp_label);

      if (no_length)
	{
	  tree fun = builtin_decl_explicit (BUILT_IN_STRCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   force_reg (Pmode, src1_addr), Pmode,
				   force_reg (Pmode, src2_addr), Pmode);
	}
      else
	{
	  /* -m32 -mpowerpc64 results in word_mode being DImode even
	     though otherwise it is 32-bit. The length arg to strncmp
	     is a size_t which will be the same size as pointers.  */
	  rtx len_rtx = gen_reg_rtx (Pmode);
	  emit_move_insn (len_rtx, gen_int_mode (bytes, Pmode));

	  tree fun = builtin_decl_explicit (BUILT_IN_STRNCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   force_reg (Pmode, src1_addr), Pmode,
				   force_reg (Pmode, src2_addr), Pmode,
				   len_rtx, Pmode);
	}

      rtx fin_ref = gen_rtx_LABEL_REF (VOIDmode, final_label);
      jmp = emit_jump_insn (gen_rtx_SET (pc_rtx, fin_ref));
      JUMP_LABEL (jmp) = final_label;
      LABEL_NUSES (final_label) += 1;
      emit_barrier ();
      emit_label (begin_compare_label);
    }

  rtx cleanup_label = NULL;
  rtx tmp_reg_src1 = gen_reg_rtx (word_mode);
  rtx tmp_reg_src2 = gen_reg_rtx (word_mode);

  /* Generate a sequence of GPR or VEC/VSX instructions to compare out
     to the length specified.  */
  unsigned HOST_WIDE_INT bytes_to_compare = compare_length;
  while (bytes_to_compare > 0)
    {
      /* GPR compare sequence:
         check each 8B with: ld/ld cmpd bne
	 If equal, use rldicr/cmpb to check for zero byte.
         cleanup code at end:
         cmpb          get byte that differs
         cmpb          look for zero byte
         orc           combine
         cntlzd        get bit of first zero/diff byte
         subfic        convert for rldcl use
         rldcl rldcl   extract diff/zero byte
         subf          subtract for final result

         The last compare can branch around the cleanup code if the
         result is zero because the strings are exactly equal.  */
      
      unsigned int align = compute_current_alignment (base_align, offset);
      load_mode = select_block_compare_mode (offset, bytes_to_compare,
					     align, word_mode_ok);
      load_mode_size = GET_MODE_SIZE (load_mode);
      if (bytes_to_compare >= load_mode_size)
	cmp_bytes = load_mode_size;
      else if (TARGET_EFFICIENT_OVERLAPPING_UNALIGNED)
	{
	  /* Move this load back so it doesn't go past the end.
	     P8/P9 can do this efficiently.  */
	  unsigned int extra_bytes = load_mode_size - bytes_to_compare;
	  cmp_bytes = bytes_to_compare;
	  if (extra_bytes < offset)
	    {
	      offset -= extra_bytes;
	      cmp_bytes = load_mode_size;
	      bytes_to_compare = cmp_bytes;
	    }
	}
      else
	/* P7 and earlier can't do the overlapping load trick fast,
	   so this forces a non-overlapping load and a shift to get
	   rid of the extra bytes.  */
	cmp_bytes = bytes_to_compare;

      rtx addr1 = gen_rtx_PLUS (Pmode, src1_addr, GEN_INT (offset));
      do_load_for_compare_from_addr (load_mode, tmp_reg_src1, addr1, orig_src1);
      rtx addr2 = gen_rtx_PLUS (Pmode, src2_addr, GEN_INT (offset));
      do_load_for_compare_from_addr (load_mode, tmp_reg_src2, addr2, orig_src2);

      /* We must always left-align the data we read, and
	 clear any bytes to the right that are beyond the string.
	 Otherwise the cmpb sequence won't produce the correct
	 results.  The beginning of the compare will be done
	 with word_mode so will not have any extra shifts or
	 clear rights.  */

      if (load_mode_size < word_mode_size)
	{
	  /* Rotate left first. */
	  rtx sh = GEN_INT (BITS_PER_UNIT * (word_mode_size - load_mode_size));
	  do_rotl3 (tmp_reg_src1, tmp_reg_src1, sh);
	  do_rotl3 (tmp_reg_src2, tmp_reg_src2, sh);
	}

      if (cmp_bytes < word_mode_size)
	{
	  /* Now clear right.  This plus the rotate can be
	     turned into a rldicr instruction. */
	  HOST_WIDE_INT mb = BITS_PER_UNIT * (word_mode_size - cmp_bytes);
	  rtx mask = GEN_INT (HOST_WIDE_INT_M1U << mb);
	  do_and3 (tmp_reg_src1, tmp_reg_src1, mask);
	  do_and3 (tmp_reg_src2, tmp_reg_src2, mask);
	}

      /* Cases to handle.  A and B are chunks of the two strings.
	 1: Not end of comparison:
	 A != B: branch to cleanup code to compute result.
	 A == B: check for 0 byte, next block if not found.
	 2: End of the inline comparison:
	 A != B: branch to cleanup code to compute result.
	 A == B: check for 0 byte, call strcmp/strncmp
	 3: compared requested N bytes:
	 A == B: branch to result 0.
	 A != B: cleanup code to compute result.  */

      unsigned HOST_WIDE_INT remain = bytes_to_compare - cmp_bytes;

      rtx dst_label;
      if (remain > 0 || equality_compare_rest)
	{
	  /* Branch to cleanup code, otherwise fall through to do
	     more compares.  */
	  if (!cleanup_label)
	    cleanup_label = gen_label_rtx ();
	  dst_label = cleanup_label;
	}
      else
	/* Branch to end and produce result of 0.  */
	dst_label = final_move_label;

      rtx lab_ref = gen_rtx_LABEL_REF (VOIDmode, dst_label);
      rtx cond = gen_reg_rtx (CCmode);

      /* Always produce the 0 result, it is needed if
	 cmpb finds a 0 byte in this chunk.  */
      rtx tmp = gen_rtx_MINUS (word_mode, tmp_reg_src1, tmp_reg_src2);
      rs6000_emit_dot_insn (result_reg, tmp, 1, cond);

      rtx cmp_rtx;
      if (remain == 0 && !equality_compare_rest)
	cmp_rtx = gen_rtx_EQ (VOIDmode, cond, const0_rtx);
      else
	cmp_rtx = gen_rtx_NE (VOIDmode, cond, const0_rtx);

      rtx ifelse = gen_rtx_IF_THEN_ELSE (VOIDmode, cmp_rtx,
					 lab_ref, pc_rtx);
      rtx j = emit_jump_insn (gen_rtx_SET (pc_rtx, ifelse));
      JUMP_LABEL (j) = dst_label;
      LABEL_NUSES (dst_label) += 1;

      if (remain > 0 || equality_compare_rest)
	{
	  /* Generate a cmpb to test for a 0 byte and branch
	     to final result if found.  */
	  rtx cmpb_zero = gen_reg_rtx (word_mode);
	  rtx lab_ref_fin = gen_rtx_LABEL_REF (VOIDmode, final_move_label);
	  rtx condz = gen_reg_rtx (CCmode);
	  rtx zero_reg = gen_reg_rtx (word_mode);
	  emit_move_insn (zero_reg, GEN_INT (0));
	  do_cmpb3 (cmpb_zero, tmp_reg_src1, zero_reg);

	  if (cmp_bytes < word_mode_size)
	    {
	      /* Don't want to look at zero bytes past end.  */
	      HOST_WIDE_INT mb =
		BITS_PER_UNIT * (word_mode_size - cmp_bytes);
	      rtx mask = GEN_INT (HOST_WIDE_INT_M1U << mb);
	      do_and3 (cmpb_zero, cmpb_zero, mask);
	    }

	  emit_move_insn (condz, gen_rtx_COMPARE (CCmode, cmpb_zero, zero_reg));
	  rtx cmpnz_rtx = gen_rtx_NE (VOIDmode, condz, const0_rtx);
	  rtx ifelse = gen_rtx_IF_THEN_ELSE (VOIDmode, cmpnz_rtx,
					     lab_ref_fin, pc_rtx);
	  rtx j2 = emit_jump_insn (gen_rtx_SET (pc_rtx, ifelse));
	  JUMP_LABEL (j2) = final_move_label;
	  LABEL_NUSES (final_move_label) += 1;

	}

      offset += cmp_bytes;
      bytes_to_compare -= cmp_bytes;
    }

  if (equality_compare_rest)
    {
      /* Update pointers past what has been compared already.  */
      rtx src1 = force_reg (Pmode,
			    gen_rtx_PLUS (Pmode, src1_addr, GEN_INT (offset)));
      rtx src2 = force_reg (Pmode,
			    gen_rtx_PLUS (Pmode, src2_addr, GEN_INT (offset)));

      /* Construct call to strcmp/strncmp to compare the rest of the string.  */
      if (no_length)
	{
	  tree fun = builtin_decl_explicit (BUILT_IN_STRCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   src1, Pmode, src2, Pmode);
	}
      else
	{
	  rtx len_rtx = gen_reg_rtx (Pmode);
	  emit_move_insn (len_rtx, gen_int_mode (bytes - compare_length, Pmode));
	  tree fun = builtin_decl_explicit (BUILT_IN_STRNCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   src1, Pmode, src2, Pmode, len_rtx, Pmode);
	}

      rtx fin_ref = gen_rtx_LABEL_REF (VOIDmode, final_label);
      rtx jmp = emit_jump_insn (gen_rtx_SET (pc_rtx, fin_ref));
      JUMP_LABEL (jmp) = final_label;
      LABEL_NUSES (final_label) += 1;
      emit_barrier ();
    }

  if (cleanup_label)
    emit_label (cleanup_label);

  emit_final_str_compare_gpr (tmp_reg_src1, tmp_reg_src2, result_reg);

  emit_label (final_move_label);
  emit_insn (gen_movsi (target,
			gen_lowpart (SImode, result_reg)));
  emit_label (final_label);
  return true;
}

/* Expand a block move operation, and return 1 if successful.  Return 0
   if we should let the compiler generate normal code.

   operands[0] is the destination
   operands[1] is the source
   operands[2] is the length
   operands[3] is the alignment */

#define MAX_MOVE_REG 4

int
expand_block_move (rtx operands[])
{
  rtx orig_dest = operands[0];
  rtx orig_src	= operands[1];
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int align;
  int bytes;
  int offset;
  int move_bytes;
  rtx stores[MAX_MOVE_REG];
  int num_reg = 0;

  /* If this is not a fixed size move, just call memcpy */
  if (! constp)
    return 0;

  /* This must be a fixed size alignment */
  gcc_assert (GET_CODE (align_rtx) == CONST_INT);
  align = INTVAL (align_rtx) * BITS_PER_UNIT;

  /* Anything to move? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return 1;

  if (bytes > rs6000_block_move_inline_limit)
    return 0;

  for (offset = 0; bytes > 0; offset += move_bytes, bytes -= move_bytes)
    {
      union {
	rtx (*movmemsi) (rtx, rtx, rtx, rtx);
	rtx (*mov) (rtx, rtx);
      } gen_func;
      machine_mode mode = BLKmode;
      rtx src, dest;

      /* Altivec first, since it will be faster than a string move
	 when it applies, and usually not significantly larger.  */
      if (TARGET_ALTIVEC && bytes >= 16 && (TARGET_EFFICIENT_UNALIGNED_VSX || align >= 128))
	{
	  move_bytes = 16;
	  mode = V4SImode;
	  gen_func.mov = gen_movv4si;
	}
      else if (bytes >= 8 && TARGET_POWERPC64
	       && (align >= 64 || !STRICT_ALIGNMENT))
	{
	  move_bytes = 8;
	  mode = DImode;
	  gen_func.mov = gen_movdi;
	  if (offset == 0 && align < 64)
	    {
	      rtx addr;

	      /* If the address form is reg+offset with offset not a
		 multiple of four, reload into reg indirect form here
		 rather than waiting for reload.  This way we get one
		 reload, not one per load and/or store.  */
	      addr = XEXP (orig_dest, 0);
	      if ((GET_CODE (addr) == PLUS || GET_CODE (addr) == LO_SUM)
		  && GET_CODE (XEXP (addr, 1)) == CONST_INT
		  && (INTVAL (XEXP (addr, 1)) & 3) != 0)
		{
		  addr = copy_addr_to_reg (addr);
		  orig_dest = replace_equiv_address (orig_dest, addr);
		}
	      addr = XEXP (orig_src, 0);
	      if ((GET_CODE (addr) == PLUS || GET_CODE (addr) == LO_SUM)
		  && GET_CODE (XEXP (addr, 1)) == CONST_INT
		  && (INTVAL (XEXP (addr, 1)) & 3) != 0)
		{
		  addr = copy_addr_to_reg (addr);
		  orig_src = replace_equiv_address (orig_src, addr);
		}
	    }
	}
      else if (bytes >= 4 && (align >= 32 || !STRICT_ALIGNMENT))
	{			/* move 4 bytes */
	  move_bytes = 4;
	  mode = SImode;
	  gen_func.mov = gen_movsi;
	}
      else if (bytes >= 2 && (align >= 16 || !STRICT_ALIGNMENT))
	{			/* move 2 bytes */
	  move_bytes = 2;
	  mode = HImode;
	  gen_func.mov = gen_movhi;
	}
      else /* move 1 byte at a time */
	{
	  move_bytes = 1;
	  mode = QImode;
	  gen_func.mov = gen_movqi;
	}

      src = adjust_address (orig_src, mode, offset);
      dest = adjust_address (orig_dest, mode, offset);

      if (mode != BLKmode)
	{
	  rtx tmp_reg = gen_reg_rtx (mode);

	  emit_insn ((*gen_func.mov) (tmp_reg, src));
	  stores[num_reg++] = (*gen_func.mov) (dest, tmp_reg);
	}

      if (mode == BLKmode || num_reg >= MAX_MOVE_REG || bytes == move_bytes)
	{
	  int i;
	  for (i = 0; i < num_reg; i++)
	    emit_insn (stores[i]);
	  num_reg = 0;
	}

      if (mode == BLKmode)
	{
	  /* Move the address into scratch registers.  The movmemsi
	     patterns require zero offset.  */
	  if (!REG_P (XEXP (src, 0)))
	    {
	      rtx src_reg = copy_addr_to_reg (XEXP (src, 0));
	      src = replace_equiv_address (src, src_reg);
	    }
	  set_mem_size (src, move_bytes);

	  if (!REG_P (XEXP (dest, 0)))
	    {
	      rtx dest_reg = copy_addr_to_reg (XEXP (dest, 0));
	      dest = replace_equiv_address (dest, dest_reg);
	    }
	  set_mem_size (dest, move_bytes);

	  emit_insn ((*gen_func.movmemsi) (dest, src,
					   GEN_INT (move_bytes & 31),
					   align_rtx));
	}
    }

  return 1;
}
