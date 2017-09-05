/* Subroutines used to expand string and block move, clear,
   compare and other operations for PowerPC.
   Copyright (C) 1991-2017 Free Software Foundation, Inc.

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
  if (TARGET_ALTIVEC && align >= 128)
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

      if (bytes >= 16 && TARGET_ALTIVEC && align >= 128)
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
	   && offset >= GET_MODE_SIZE (SImode) - bytes)
    /* This matches the case were we have SImode and 3 bytes
       and offset >= 1 and permits us to move back one and overlap
       with the previous read, thus avoiding having to shift
       unwanted bytes off of the input.  */
    return SImode;
  else if (word_mode_ok && bytes < UNITS_PER_WORD
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

  /* If this is not a fixed size compare, just call memcmp.  */
  if (!CONST_INT_P (bytes_rtx))
    return false;

  /* This must be a fixed size alignment.  */
  if (!CONST_INT_P (align_rtx))
    return false;

  unsigned int base_align = UINTVAL (align_rtx) / BITS_PER_UNIT;

  /* SLOW_UNALIGNED_ACCESS -- don't do unaligned stuff.  */
  if (SLOW_UNALIGNED_ACCESS (word_mode, MEM_ALIGN (orig_src1))
      || SLOW_UNALIGNED_ACCESS (word_mode, MEM_ALIGN (orig_src2)))
    return false;

  gcc_assert (GET_MODE (target) == SImode);

  /* Anything to move?  */
  unsigned HOST_WIDE_INT bytes = UINTVAL (bytes_rtx);
  if (bytes == 0)
    return true;

  /* The code generated for p7 and older is not faster than glibc
     memcmp if alignment is small and length is not short, so bail
     out to avoid those conditions.  */
  if (!TARGET_EFFICIENT_OVERLAPPING_UNALIGNED
      && ((base_align == 1 && bytes > 16)
	  || (base_align == 2 && bytes > 32)))
    return false;

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

  /* We don't want to generate too much code.  */
  unsigned HOST_WIDE_INT max_bytes =
    load_mode_size * (unsigned HOST_WIDE_INT) rs6000_block_compare_inline_limit;
  if (!IN_RANGE (bytes, 1, max_bytes))
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
      if (TARGET_EFFICIENT_OVERLAPPING_UNALIGNED)
	load_mode = select_block_compare_mode (offset, bytes, align,
					       word_mode_ok);
      else
	load_mode = select_block_compare_mode (0, bytes, align, word_mode_ok);
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
      set_mem_size (src1, cmp_bytes);

      if (!REG_P (XEXP (src2, 0)))
	{
	  rtx src2_reg = copy_addr_to_reg (XEXP (src2, 0));
	  src2 = replace_equiv_address (src2, src2_reg);
	}
      set_mem_size (src2, cmp_bytes);

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
		  JUMP_LABEL(j) = final_label;
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
	      JUMP_LABEL(j) = convert_label;
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

/* Generate alignment check and branch code to set up for
   strncmp when we don't have DI alignment.
   STRNCMP_LABEL is the label to branch if there is a page crossing.
   SRC is the string pointer to be examined.
   BYTES is the max number of bytes to compare.  */
static void
expand_strncmp_align_check (rtx strncmp_label, rtx src, HOST_WIDE_INT bytes)
{
  rtx lab_ref = gen_rtx_LABEL_REF (VOIDmode, strncmp_label);
  rtx src_check = copy_addr_to_reg (XEXP (src, 0));
  if (GET_MODE (src_check) == SImode)
    emit_insn (gen_andsi3 (src_check, src_check, GEN_INT (0xfff)));
  else
    emit_insn (gen_anddi3 (src_check, src_check, GEN_INT (0xfff)));
  rtx cond = gen_reg_rtx (CCmode);
  emit_move_insn (cond, gen_rtx_COMPARE (CCmode, src_check,
					 GEN_INT (4096 - bytes)));

  rtx cmp_rtx = gen_rtx_LT (VOIDmode, cond, const0_rtx);

  rtx ifelse = gen_rtx_IF_THEN_ELSE (VOIDmode, cmp_rtx,
				     pc_rtx, lab_ref);
  rtx j = emit_jump_insn (gen_rtx_SET (pc_rtx, ifelse));
  JUMP_LABEL (j) = strncmp_label;
  LABEL_NUSES (strncmp_label) += 1;
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
  rtx src1 = orig_src1;
  rtx src2 = orig_src2;

  /* If we have a length, it must be constant. This simplifies things
     a bit as we don't have to generate code to check if we've exceeded
     the length. Later this could be expanded to handle this case.  */
  if (!no_length && !CONST_INT_P (bytes_rtx))
    return false;

  /* This must be a fixed size alignment.  */
  if (!CONST_INT_P (align_rtx))
    return false;

  unsigned int base_align = UINTVAL (align_rtx);
  int align1 = MEM_ALIGN (orig_src1) / BITS_PER_UNIT;
  int align2 = MEM_ALIGN (orig_src2) / BITS_PER_UNIT;

  /* SLOW_UNALIGNED_ACCESS -- don't do unaligned stuff.  */
  if (SLOW_UNALIGNED_ACCESS (word_mode, align1)
      || SLOW_UNALIGNED_ACCESS (word_mode, align2))
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

  if (base_align < 8)
    {
      /* Generate code that checks distance to 4k boundary for this case.  */
      begin_compare_label = gen_label_rtx ();
      rtx strncmp_label = gen_label_rtx ();
      rtx jmp;

      /* Strncmp for power8 in glibc does this:
	 rldicl	r8,r3,0,52
	 cmpldi	cr7,r8,4096-16
	 bgt	cr7,L(pagecross) */

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
          align_test = ROUND_UP (align_test, 8);
          base_align = 8;
        }

      if (align1 < 8)
        expand_strncmp_align_check (strncmp_label, src1, align_test);
      if (align2 < 8)
        expand_strncmp_align_check (strncmp_label, src2, align_test);

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

      if (!REG_P (XEXP (src1, 0)))
	{
	  rtx src1_reg = copy_addr_to_reg (XEXP (src1, 0));
	  src1 = replace_equiv_address (src1, src1_reg);
	}

      if (!REG_P (XEXP (src2, 0)))
	{
	  rtx src2_reg = copy_addr_to_reg (XEXP (src2, 0));
	  src2 = replace_equiv_address (src2, src2_reg);
	}

      if (no_length)
	{
	  tree fun = builtin_decl_explicit (BUILT_IN_STRCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   force_reg (Pmode, XEXP (src1, 0)), Pmode,
				   force_reg (Pmode, XEXP (src2, 0)), Pmode);
	}
      else
	{
	  /* -m32 -mpowerpc64 results in word_mode being DImode even
	     though otherwise it is 32-bit. The length arg to strncmp
	     is a size_t which will be the same size as pointers.  */
	  rtx len_rtx;
	  if (TARGET_64BIT)
	    len_rtx = gen_reg_rtx (DImode);
	  else
	    len_rtx = gen_reg_rtx (SImode);

	  emit_move_insn (len_rtx, bytes_rtx);

	  tree fun = builtin_decl_explicit (BUILT_IN_STRNCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   force_reg (Pmode, XEXP (src1, 0)), Pmode,
				   force_reg (Pmode, XEXP (src2, 0)), Pmode,
				   len_rtx, GET_MODE (len_rtx));
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

  /* Generate sequence of ld/ldbrx, cmpb to compare out
     to the length specified.  */
  unsigned HOST_WIDE_INT bytes_to_compare = compare_length;
  while (bytes_to_compare > 0)
    {
      /* Compare sequence:
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
      if (TARGET_EFFICIENT_OVERLAPPING_UNALIGNED)
	load_mode = select_block_compare_mode (offset, bytes_to_compare, align,
					       word_mode_ok);
      else
	load_mode = select_block_compare_mode (0, bytes_to_compare, align,
					       word_mode_ok);
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

      src1 = adjust_address (orig_src1, load_mode, offset);
      src2 = adjust_address (orig_src2, load_mode, offset);

      if (!REG_P (XEXP (src1, 0)))
	{
	  rtx src1_reg = copy_addr_to_reg (XEXP (src1, 0));
	  src1 = replace_equiv_address (src1, src1_reg);
	}
      set_mem_size (src1, cmp_bytes);

      if (!REG_P (XEXP (src2, 0)))
	{
	  rtx src2_reg = copy_addr_to_reg (XEXP (src2, 0));
	  src2 = replace_equiv_address (src2, src2_reg);
	}
      set_mem_size (src2, cmp_bytes);

      do_load_for_compare (tmp_reg_src1, src1, load_mode);
      do_load_for_compare (tmp_reg_src2, src2, load_mode);

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
	  if (word_mode == DImode)
	    {
	      emit_insn (gen_rotldi3 (tmp_reg_src1, tmp_reg_src1, sh));
	      emit_insn (gen_rotldi3 (tmp_reg_src2, tmp_reg_src2, sh));
	    }
	  else
	    {
	      emit_insn (gen_rotlsi3 (tmp_reg_src1, tmp_reg_src1, sh));
	      emit_insn (gen_rotlsi3 (tmp_reg_src2, tmp_reg_src2, sh));
	    }
	}

      if (cmp_bytes < word_mode_size)
	{
	  /* Now clear right.  This plus the rotate can be
	     turned into a rldicr instruction. */
	  HOST_WIDE_INT mb = BITS_PER_UNIT * (word_mode_size - cmp_bytes);
	  rtx mask = GEN_INT (HOST_WIDE_INT_M1U << mb);
	  if (word_mode == DImode)
	    {
	      emit_insn (gen_anddi3_mask (tmp_reg_src1, tmp_reg_src1, mask));
	      emit_insn (gen_anddi3_mask (tmp_reg_src2, tmp_reg_src2, mask));
	    }
	  else
	    {
	      emit_insn (gen_andsi3_mask (tmp_reg_src1, tmp_reg_src1, mask));
	      emit_insn (gen_andsi3_mask (tmp_reg_src2, tmp_reg_src2, mask));
	    }
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
	  if (word_mode == SImode)
	    {
	      emit_insn (gen_movsi (zero_reg, GEN_INT (0)));
	      emit_insn (gen_cmpbsi3 (cmpb_zero, tmp_reg_src1, zero_reg));
	      if (cmp_bytes < word_mode_size)
		{
		  /* Don't want to look at zero bytes past end.  */
		  HOST_WIDE_INT mb =
		    BITS_PER_UNIT * (word_mode_size - cmp_bytes);
		  rtx mask = GEN_INT (HOST_WIDE_INT_M1U << mb);
		  emit_insn (gen_andsi3_mask (cmpb_zero, cmpb_zero, mask));
		}
	    }
	  else
	    {
	      emit_insn (gen_movdi (zero_reg, GEN_INT (0)));
	      emit_insn (gen_cmpbdi3 (cmpb_zero, tmp_reg_src1, zero_reg));
	      if (cmp_bytes < word_mode_size)
		{
		  /* Don't want to look at zero bytes past end.  */
		  HOST_WIDE_INT mb =
		    BITS_PER_UNIT * (word_mode_size - cmp_bytes);
		  rtx mask = GEN_INT (HOST_WIDE_INT_M1U << mb);
		  emit_insn (gen_anddi3_mask (cmpb_zero, cmpb_zero, mask));
		}
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
      src1 = adjust_address (orig_src1, load_mode, offset);
      src2 = adjust_address (orig_src2, load_mode, offset);

      if (!REG_P (XEXP (src1, 0)))
	{
	  rtx src1_reg = copy_addr_to_reg (XEXP (src1, 0));
	  src1 = replace_equiv_address (src1, src1_reg);
	}
      set_mem_size (src1, cmp_bytes);

      if (!REG_P (XEXP (src2, 0)))
	{
	  rtx src2_reg = copy_addr_to_reg (XEXP (src2, 0));
	  src2 = replace_equiv_address (src2, src2_reg);
	}
      set_mem_size (src2, cmp_bytes);

      /* Construct call to strcmp/strncmp to compare the rest of the string.  */
      if (no_length)
	{
	  tree fun = builtin_decl_explicit (BUILT_IN_STRCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   force_reg (Pmode, XEXP (src1, 0)), Pmode,
				   force_reg (Pmode, XEXP (src2, 0)), Pmode);
	}
      else
	{
	  rtx len_rtx;
	  if (TARGET_64BIT)
	    len_rtx = gen_reg_rtx (DImode);
	  else
	    len_rtx = gen_reg_rtx (SImode);

	  emit_move_insn (len_rtx, GEN_INT (bytes - compare_length));
	  tree fun = builtin_decl_explicit (BUILT_IN_STRNCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   force_reg (Pmode, XEXP (src1, 0)), Pmode,
				   force_reg (Pmode, XEXP (src2, 0)), Pmode,
				   len_rtx, GET_MODE (len_rtx));
	}

      rtx fin_ref = gen_rtx_LABEL_REF (VOIDmode, final_label);
      rtx jmp = emit_jump_insn (gen_rtx_SET (pc_rtx, fin_ref));
      JUMP_LABEL (jmp) = final_label;
      LABEL_NUSES (final_label) += 1;
      emit_barrier ();
    }

  if (cleanup_label)
    emit_label (cleanup_label);

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
  */

  rtx cmpb_diff = gen_reg_rtx (word_mode);
  rtx cmpb_zero = gen_reg_rtx (word_mode);
  rtx rot_amt = gen_reg_rtx (word_mode);
  rtx zero_reg = gen_reg_rtx (word_mode);

  rtx rot1_1 = gen_reg_rtx (word_mode);
  rtx rot1_2 = gen_reg_rtx (word_mode);
  rtx rot2_1 = gen_reg_rtx (word_mode);
  rtx rot2_2 = gen_reg_rtx (word_mode);

  if (word_mode == SImode)
    {
      emit_insn (gen_cmpbsi3 (cmpb_diff, tmp_reg_src1, tmp_reg_src2));
      emit_insn (gen_movsi (zero_reg, GEN_INT (0)));
      emit_insn (gen_cmpbsi3 (cmpb_zero, tmp_reg_src1, zero_reg));
      emit_insn (gen_one_cmplsi2 (cmpb_diff,cmpb_diff));
      emit_insn (gen_iorsi3 (cmpb_diff, cmpb_diff, cmpb_zero));
      emit_insn (gen_clzsi2 (rot_amt, cmpb_diff));
      emit_insn (gen_addsi3 (rot_amt, rot_amt, GEN_INT (8)));
      emit_insn (gen_rotlsi3 (rot1_1, tmp_reg_src1,
			      gen_lowpart (SImode, rot_amt)));
      emit_insn (gen_andsi3_mask (rot1_2, rot1_1, GEN_INT (0xff)));
      emit_insn (gen_rotlsi3 (rot2_1, tmp_reg_src2,
			      gen_lowpart (SImode, rot_amt)));
      emit_insn (gen_andsi3_mask (rot2_2, rot2_1, GEN_INT (0xff)));
      emit_insn (gen_subsi3 (result_reg, rot1_2, rot2_2));
    }
  else
    {
      emit_insn (gen_cmpbdi3 (cmpb_diff, tmp_reg_src1, tmp_reg_src2));
      emit_insn (gen_movdi (zero_reg, GEN_INT (0)));
      emit_insn (gen_cmpbdi3 (cmpb_zero, tmp_reg_src1, zero_reg));
      emit_insn (gen_one_cmpldi2 (cmpb_diff,cmpb_diff));
      emit_insn (gen_iordi3 (cmpb_diff, cmpb_diff, cmpb_zero));
      emit_insn (gen_clzdi2 (rot_amt, cmpb_diff));
      emit_insn (gen_adddi3 (rot_amt, rot_amt, GEN_INT (8)));
      emit_insn (gen_rotldi3 (rot1_1, tmp_reg_src1,
			      gen_lowpart (SImode, rot_amt)));
      emit_insn (gen_anddi3_mask (rot1_2, rot1_1, GEN_INT (0xff)));
      emit_insn (gen_rotldi3 (rot2_1, tmp_reg_src2,
			      gen_lowpart (SImode, rot_amt)));
      emit_insn (gen_anddi3_mask (rot2_2, rot2_1, GEN_INT (0xff)));
      emit_insn (gen_subdi3 (result_reg, rot1_2, rot2_2));
    }

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
      if (TARGET_ALTIVEC && bytes >= 16 && align >= 128)
	{
	  move_bytes = 16;
	  mode = V4SImode;
	  gen_func.mov = gen_movv4si;
	}
      else if (TARGET_STRING
	  && bytes > 24		/* move up to 32 bytes at a time */
	  && ! fixed_regs[5]
	  && ! fixed_regs[6]
	  && ! fixed_regs[7]
	  && ! fixed_regs[8]
	  && ! fixed_regs[9]
	  && ! fixed_regs[10]
	  && ! fixed_regs[11]
	  && ! fixed_regs[12])
	{
	  move_bytes = (bytes > 32) ? 32 : bytes;
	  gen_func.movmemsi = gen_movmemsi_8reg;
	}
      else if (TARGET_STRING
	       && bytes > 16	/* move up to 24 bytes at a time */
	       && ! fixed_regs[5]
	       && ! fixed_regs[6]
	       && ! fixed_regs[7]
	       && ! fixed_regs[8]
	       && ! fixed_regs[9]
	       && ! fixed_regs[10])
	{
	  move_bytes = (bytes > 24) ? 24 : bytes;
	  gen_func.movmemsi = gen_movmemsi_6reg;
	}
      else if (TARGET_STRING
	       && bytes > 8	/* move up to 16 bytes at a time */
	       && ! fixed_regs[5]
	       && ! fixed_regs[6]
	       && ! fixed_regs[7]
	       && ! fixed_regs[8])
	{
	  move_bytes = (bytes > 16) ? 16 : bytes;
	  gen_func.movmemsi = gen_movmemsi_4reg;
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
      else if (TARGET_STRING && bytes > 4 && !TARGET_POWERPC64)
	{			/* move up to 8 bytes at a time */
	  move_bytes = (bytes > 8) ? 8 : bytes;
	  gen_func.movmemsi = gen_movmemsi_2reg;
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
      else if (TARGET_STRING && bytes > 1)
	{			/* move up to 4 bytes at a time */
	  move_bytes = (bytes > 4) ? 4 : bytes;
	  gen_func.movmemsi = gen_movmemsi_1reg;
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


/* Return a string to perform a load_multiple operation.
   operands[0] is the vector.
   operands[1] is the source address.
   operands[2] is the first destination register.  */

const char *
rs6000_output_load_multiple (rtx operands[3])
{
  /* We have to handle the case where the pseudo used to contain the address
     is assigned to one of the output registers.  */
  int i, j;
  int words = XVECLEN (operands[0], 0);
  rtx xop[10];

  if (XVECLEN (operands[0], 0) == 1)
    return "lwz %2,0(%1)";

  for (i = 0; i < words; i++)
    if (refers_to_regno_p (REGNO (operands[2]) + i, operands[1]))
      {
	if (i == words-1)
	  {
	    xop[0] = GEN_INT (4 * (words-1));
	    xop[1] = operands[1];
	    xop[2] = operands[2];
	    output_asm_insn ("lswi %2,%1,%0\n\tlwz %1,%0(%1)", xop);
	    return "";
	  }
	else if (i == 0)
	  {
	    xop[0] = GEN_INT (4 * (words-1));
	    xop[1] = operands[1];
	    xop[2] = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);
	    output_asm_insn ("addi %1,%1,4\n\tlswi %2,%1,%0\n\tlwz %1,-4(%1)", xop);
	    return "";
	  }
	else
	  {
	    for (j = 0; j < words; j++)
	      if (j != i)
		{
		  xop[0] = GEN_INT (j * 4);
		  xop[1] = operands[1];
		  xop[2] = gen_rtx_REG (SImode, REGNO (operands[2]) + j);
		  output_asm_insn ("lwz %2,%0(%1)", xop);
		}
	    xop[0] = GEN_INT (i * 4);
	    xop[1] = operands[1];
	    output_asm_insn ("lwz %1,%0(%1)", xop);
	    return "";
	  }
      }

  return "lswi %2,%1,%N0";
}

