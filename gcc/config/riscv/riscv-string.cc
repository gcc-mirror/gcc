/* Subroutines used to expand string operations for RISC-V.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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
#include "predict.h"
#include "optabs.h"
#include "riscv-protos.h"
#include "recog.h"
#include "tm-constrs.h"

/* Emit proper instruction depending on mode of dest.  */

#define GEN_EMIT_HELPER2(name)				\
static rtx_insn *					\
do_## name ## 2(rtx dest, rtx src)			\
{							\
  rtx_insn *insn;					\
  if (GET_MODE (dest) == DImode)			\
    insn = emit_insn (gen_ ## name ## di2 (dest, src));	\
  else							\
    insn = emit_insn (gen_ ## name ## si2 (dest, src));	\
  return insn;						\
}

/* Emit proper instruction depending on mode of dest.  */

#define GEN_EMIT_HELPER3(name)					\
static rtx_insn *						\
do_## name ## 3(rtx dest, rtx src1, rtx src2)			\
{								\
  rtx_insn *insn;						\
  if (GET_MODE (dest) == DImode)				\
    insn = emit_insn (gen_ ## name ## di3 (dest, src1, src2));	\
  else								\
    insn = emit_insn (gen_ ## name ## si3 (dest, src1, src2));	\
  return insn;							\
}

GEN_EMIT_HELPER3(add) /* do_add3  */
GEN_EMIT_HELPER3(and) /* do_and3  */
GEN_EMIT_HELPER3(ashl) /* do_ashl3  */
GEN_EMIT_HELPER2(bswap) /* do_bswap2  */
GEN_EMIT_HELPER2(clz) /* do_clz2  */
GEN_EMIT_HELPER2(ctz) /* do_ctz2  */
GEN_EMIT_HELPER3(ior) /* do_ior3  */
GEN_EMIT_HELPER3(ior_not) /* do_ior_not3  */
GEN_EMIT_HELPER3(lshr) /* do_lshr3  */
GEN_EMIT_HELPER2(neg) /* do_neg2  */
GEN_EMIT_HELPER2(orcb) /* do_orcb2  */
GEN_EMIT_HELPER2(one_cmpl) /* do_one_cmpl2  */
GEN_EMIT_HELPER3(rotr) /* do_rotr3  */
GEN_EMIT_HELPER3(sub) /* do_sub3  */
GEN_EMIT_HELPER2(th_rev) /* do_th_rev2  */
GEN_EMIT_HELPER2(th_tstnbz) /* do_th_tstnbz2  */
GEN_EMIT_HELPER3(xor) /* do_xor3  */
GEN_EMIT_HELPER2(zero_extendqi) /* do_zero_extendqi2  */

#undef GEN_EMIT_HELPER2
#undef GEN_EMIT_HELPER3

/* Helper function to load a byte or a Pmode register.

   MODE is the mode to use for the load (QImode or Pmode).
   DEST is the destination register for the data.
   ADDR_REG is the register that holds the address.
   ADDR is the address expression to load from.

   This function returns an rtx containing the register,
   where the ADDR is stored.  */

static rtx
do_load_from_addr (machine_mode mode, rtx dest, rtx addr_reg, rtx addr)
{
  rtx mem = gen_rtx_MEM (mode, addr_reg);
  MEM_COPY_ATTRIBUTES (mem, addr);
  set_mem_size (mem, GET_MODE_SIZE (mode));

  if (mode == QImode)
    do_zero_extendqi2 (dest, mem);
  else if (mode == Xmode)
    emit_move_insn (dest, mem);
  else
    gcc_unreachable ();

  return addr_reg;
}

/* Generate a sequence to compare single characters in data1 and data2.

   RESULT is the register where the return value of str(n)cmp will be stored.
   DATA1 is a register which contains character1.
   DATA2 is a register which contains character2.
   FINAL_LABEL is the location after the calculation of the return value.  */

static void
emit_strcmp_scalar_compare_byte (rtx result, rtx data1, rtx data2,
				 rtx final_label)
{
  rtx tmp = gen_reg_rtx (Xmode);
  do_sub3 (tmp, data1, data2);
  emit_insn (gen_movsi (result, gen_lowpart (SImode, tmp)));
  emit_jump_insn (gen_jump (final_label));
  emit_barrier (); /* No fall-through.  */
}

/* Generate a sequence to compare two strings in data1 and data2.

   DATA1 is a register which contains string1.
   DATA2 is a register which contains string2.
   ORC1 is a register where orc.b(data1) will be stored.
   CMP_BYTES is the length of the strings.
   END_LABEL is the location of the code that calculates the return value.  */

static void
emit_strcmp_scalar_compare_subword (rtx data1, rtx data2, rtx orc1,
				    unsigned HOST_WIDE_INT cmp_bytes,
				    rtx end_label)
{
  /* Set a NUL-byte after the relevant data (behind the string).  */
  long long im = -256ll;
  rtx imask = gen_rtx_CONST_INT (Xmode, im);
  rtx m_reg = gen_reg_rtx (Xmode);
  emit_insn (gen_rtx_SET (m_reg, imask));
  do_rotr3 (m_reg, m_reg, GEN_INT (64 - cmp_bytes * BITS_PER_UNIT));
  do_and3 (data1, m_reg, data1);
  do_and3 (data2, m_reg, data2);
  if (TARGET_ZBB)
    do_orcb2 (orc1, data1);
  else
    do_th_tstnbz2 (orc1, data1);
  emit_jump_insn (gen_jump (end_label));
  emit_barrier (); /* No fall-through.  */
}

/* Generate a sequence to compare two strings in data1 and data2.

   DATA1 is a register which contains string1.
   DATA2 is a register which contains string2.
   ORC1 is a register where orc.b(data1) will be stored.
   TESTVAL is the value to test ORC1 against.
   END_LABEL is the location of the code that calculates the return value.
   NONUL_END_LABEL is the location of the code that calculates the return value
   in case the first string does not contain a NULL-byte.  */

static void
emit_strcmp_scalar_compare_word (rtx data1, rtx data2, rtx orc1, rtx testval,
				 rtx end_label, rtx nonul_end_label)
{
  /* Check if data1 contains a NUL character.  */
  if (TARGET_ZBB)
    do_orcb2 (orc1, data1);
  else
    do_th_tstnbz2 (orc1, data1);
  rtx cond1 = gen_rtx_NE (VOIDmode, orc1, testval);
  emit_unlikely_jump_insn (gen_cbranch4 (Pmode, cond1, orc1, testval,
					  end_label));
  /* Break out if u1 != u2 */
  rtx cond2 = gen_rtx_NE (VOIDmode, data1, data2);
  emit_unlikely_jump_insn (gen_cbranch4 (Pmode, cond2, data1,
					 data2, nonul_end_label));
  /* Fall-through on equality.  */
}

/* Generate the sequence of compares for strcmp/strncmp using zbb instructions.

   RESULT is the register where the return value of str(n)cmp will be stored.
   The strings are referenced by SRC1 and SRC2.
   The number of bytes to compare is defined by NBYTES.
   DATA1 is a register where string1 will be stored.
   DATA2 is a register where string2 will be stored.
   ORC1 is a register where orc.b(data1) will be stored.
   END_LABEL is the location of the code that calculates the return value.
   NONUL_END_LABEL is the location of the code that calculates the return value
   in case the first string does not contain a NULL-byte.
   FINAL_LABEL is the location of the code that comes after the calculation
   of the return value.  */

static void
emit_strcmp_scalar_load_and_compare (rtx result, rtx src1, rtx src2,
				     unsigned HOST_WIDE_INT nbytes,
				     rtx data1, rtx data2, rtx orc1,
				     rtx end_label, rtx nonul_end_label,
				     rtx final_label)
{
  const unsigned HOST_WIDE_INT xlen = GET_MODE_SIZE (Xmode);
  rtx src1_addr = force_reg (Pmode, XEXP (src1, 0));
  rtx src2_addr = force_reg (Pmode, XEXP (src2, 0));
  unsigned HOST_WIDE_INT offset = 0;

  rtx testval = gen_reg_rtx (Xmode);
  if (TARGET_ZBB)
    emit_insn (gen_rtx_SET (testval, constm1_rtx));
  else
    emit_insn (gen_rtx_SET (testval, const0_rtx));

  while (nbytes > 0)
    {
      unsigned HOST_WIDE_INT cmp_bytes = xlen < nbytes ? xlen : nbytes;
      machine_mode load_mode;
      if (cmp_bytes == 1)
	load_mode = QImode;
      else
	load_mode = Xmode;

      rtx addr1 = gen_rtx_PLUS (Pmode, src1_addr, GEN_INT (offset));
      do_load_from_addr (load_mode, data1, addr1, src1);
      rtx addr2 = gen_rtx_PLUS (Pmode, src2_addr, GEN_INT (offset));
      do_load_from_addr (load_mode, data2, addr2, src2);

      if (cmp_bytes == 1)
	{
	  emit_strcmp_scalar_compare_byte (result, data1, data2, final_label);
	  return;
	}
      else if (cmp_bytes < xlen)
	{
	  emit_strcmp_scalar_compare_subword (data1, data2, orc1,
					      cmp_bytes, end_label);
	  return;
	}
      else
	emit_strcmp_scalar_compare_word (data1, data2, orc1, testval,
					 end_label, nonul_end_label);

      offset += cmp_bytes;
      nbytes -= cmp_bytes;
    }
}

/* Fixup pointers and generate a call to strcmp.

   RESULT is the register where the return value of str(n)cmp will be stored.
   The strings are referenced by SRC1 and SRC2.
   The number of already compared bytes is defined by NBYTES.  */

static void
emit_strcmp_scalar_call_to_libc (rtx result, rtx src1, rtx src2,
				 unsigned HOST_WIDE_INT nbytes)
{
  /* Update pointers past what has been compared already.  */
  rtx src1_addr = force_reg (Pmode, XEXP (src1, 0));
  rtx src2_addr = force_reg (Pmode, XEXP (src2, 0));
  rtx src1_new = force_reg (Pmode,
			    gen_rtx_PLUS (Pmode, src1_addr, GEN_INT (nbytes)));
  rtx src2_new = force_reg (Pmode,
			    gen_rtx_PLUS (Pmode, src2_addr, GEN_INT (nbytes)));

  /* Construct call to strcmp to compare the rest of the string.  */
  tree fun = builtin_decl_explicit (BUILT_IN_STRCMP);
  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
			   result, LCT_NORMAL, GET_MODE (result),
			   src1_new, Pmode, src2_new, Pmode);
}

/* Fast strcmp-result calculation if no NULL-byte in string1.

   RESULT is the register where the return value of str(n)cmp will be stored.
   The mismatching strings are stored in DATA1 and DATA2.  */

static void
emit_strcmp_scalar_result_calculation_nonul (rtx result, rtx data1, rtx data2)
{
  /* Words don't match, and no NUL byte in one word.
     Get bytes in big-endian order and compare as words.  */
  do_bswap2 (data1, data1);
  do_bswap2 (data2, data2);
  /* Synthesize (data1 >= data2) ? 1 : -1 in a branchless sequence.  */
  rtx tmp = gen_reg_rtx (Xmode);
  emit_insn (gen_slt_3 (LTU, Xmode, Xmode, tmp, data1, data2));
  do_neg2 (tmp, tmp);
  do_ior3 (tmp, tmp, const1_rtx);
  emit_insn (gen_movsi (result, gen_lowpart (SImode, tmp)));
}

/* strcmp-result calculation.

   RESULT is the register where the return value of str(n)cmp will be stored.
   The strings are stored in DATA1 and DATA2.
   ORC1 contains orc.b(DATA1).  */

static void
emit_strcmp_scalar_result_calculation (rtx result, rtx data1, rtx data2,
				       rtx orc1)
{
  const unsigned HOST_WIDE_INT xlen = GET_MODE_SIZE (Xmode);

  /* Convert non-equal bytes into non-NUL bytes.  */
  rtx diff = gen_reg_rtx (Xmode);
  do_xor3 (diff, data1, data2);
  rtx shift = gen_reg_rtx (Xmode);

  if (TARGET_ZBB)
    {
      /* Convert non-equal or NUL-bytes into non-NUL bytes.  */
      rtx syndrome = gen_reg_rtx (Xmode);
      do_orcb2 (diff, diff);
      do_ior_not3 (syndrome, orc1, diff);
      /* Count the number of equal bits from the beginning of the word.  */
      do_ctz2 (shift, syndrome);
    }
  else
    {
      /* Convert non-equal or NUL-bytes into non-NUL bytes.  */
      rtx syndrome = gen_reg_rtx (Xmode);
      do_th_tstnbz2 (diff, diff);
      do_one_cmpl2 (diff, diff);
      do_ior3 (syndrome, orc1, diff);
      /* Count the number of equal bits from the beginning of the word.  */
      do_th_rev2 (syndrome, syndrome);
      do_clz2 (shift, syndrome);
    }

  do_bswap2 (data1, data1);
  do_bswap2 (data2, data2);

  /* The most-significant-non-zero bit of the syndrome marks either the
     first bit that is different, or the top bit of the first zero byte.
     Shifting left now will bring the critical information into the
     top bits.  */
  do_ashl3 (data1, data1, gen_lowpart (QImode, shift));
  do_ashl3 (data2, data2, gen_lowpart (QImode, shift));

  /* But we need to zero-extend (char is unsigned) the value and then
     perform a signed 32-bit subtraction.  */
  unsigned int shiftr = (xlen - 1) * BITS_PER_UNIT;
  do_lshr3 (data1, data1, GEN_INT (shiftr));
  do_lshr3 (data2, data2, GEN_INT (shiftr));
  rtx tmp = gen_reg_rtx (Xmode);
  do_sub3 (tmp, data1, data2);
  emit_insn (gen_movsi (result, gen_lowpart (SImode, tmp)));
}

/* Expand str(n)cmp using Zbb/TheadBb instructions.

   The result will be stored in RESULT.
   The strings are referenced by SRC1 and SRC2.
   The number of bytes to compare is defined by NBYTES.
   The alignment is defined by ALIGNMENT.
   If NCOMPARE is false then libc's strcmp() will be called if comparing
   NBYTES of both strings did not find differences or NULL-bytes.

   Return true if expansion was successful, or false otherwise.  */

static bool
riscv_expand_strcmp_scalar (rtx result, rtx src1, rtx src2,
			    unsigned HOST_WIDE_INT nbytes,
			    unsigned HOST_WIDE_INT alignment,
			    bool ncompare)
{
  const unsigned HOST_WIDE_INT xlen = GET_MODE_SIZE (Xmode);

  gcc_assert (TARGET_ZBB || TARGET_XTHEADBB);
  gcc_assert (nbytes > 0);
  gcc_assert ((int)nbytes <= riscv_strcmp_inline_limit);
  gcc_assert (ncompare || (nbytes & (xlen - 1)) == 0);

  /* Limit to 12-bits (maximum load-offset).  */
  if (nbytes > IMM_REACH)
    nbytes = IMM_REACH;

  /* We don't support big endian.  */
  if (BYTES_BIG_ENDIAN)
    return false;

  /* We need xlen-aligned strings.  */
  if (alignment < xlen)
    return false;

  /* Overall structure of emitted code:
       Load-and-compare:
	 - Load data1 and data2
	 - Set orc1 := orc.b (data1) (or th.tstnbz)
	 - Compare strings and either:
	   - Fall-through on equality
	   - Jump to nonul_end_label if data1 !or end_label
	   - Calculate result value and jump to final_label
       // Fall-through
       Call-to-libc or set result to 0 (depending on ncompare)
       Jump to final_label
     nonul_end_label: // words don't match, and no null byte in first word.
       Calculate result value with the use of data1, data2 and orc1
       Jump to final_label
     end_label:
       Calculate result value with the use of data1, data2 and orc1
       Jump to final_label
     final_label:
       // Nothing.  */

  rtx data1 = gen_reg_rtx (Xmode);
  rtx data2 = gen_reg_rtx (Xmode);
  rtx orc1 = gen_reg_rtx (Xmode);
  rtx nonul_end_label = gen_label_rtx ();
  rtx end_label = gen_label_rtx ();
  rtx final_label = gen_label_rtx ();

  /* Generate a sequence of zbb instructions to compare out
     to the length specified.  */
  emit_strcmp_scalar_load_and_compare (result, src1, src2, nbytes,
				       data1, data2, orc1,
				       end_label, nonul_end_label, final_label);

  /* All compared and everything was equal.  */
  if (ncompare)
    {
      emit_insn (gen_rtx_SET (result, gen_rtx_CONST_INT (SImode, 0)));
      emit_jump_insn (gen_jump (final_label));
      emit_barrier (); /* No fall-through.  */
    }
  else
    {
      emit_strcmp_scalar_call_to_libc (result, src1, src2, nbytes);
      emit_jump_insn (gen_jump (final_label));
      emit_barrier (); /* No fall-through.  */
    }


  emit_label (nonul_end_label);
  emit_strcmp_scalar_result_calculation_nonul (result, data1, data2);
  emit_jump_insn (gen_jump (final_label));
  emit_barrier (); /* No fall-through.  */

  emit_label (end_label);
  emit_strcmp_scalar_result_calculation (result, data1, data2, orc1);
  emit_jump_insn (gen_jump (final_label));
  emit_barrier (); /* No fall-through.  */

  emit_label (final_label);
  return true;
}

/* Expand a string compare operation.

   The result will be stored in RESULT.
   The strings are referenced by SRC1 and SRC2.
   The argument BYTES_RTX either holds the number of characters to
   compare, or is NULL_RTX. The argument ALIGN_RTX holds the alignment.

   Return true if expansion was successful, or false otherwise.  */

bool
riscv_expand_strcmp (rtx result, rtx src1, rtx src2,
		     rtx bytes_rtx, rtx align_rtx)
{
  unsigned HOST_WIDE_INT compare_max;
  unsigned HOST_WIDE_INT nbytes;
  unsigned HOST_WIDE_INT alignment;
  bool ncompare = bytes_rtx != NULL_RTX;
  const unsigned HOST_WIDE_INT xlen = GET_MODE_SIZE (Xmode);

  if (riscv_strcmp_inline_limit == 0)
    return false;

  /* Round down the comparision limit to a multiple of xlen.  */
  compare_max = riscv_strcmp_inline_limit & ~(xlen - 1);

  /* Decide how many bytes to compare inline.  */
  if (bytes_rtx == NULL_RTX)
    {
      nbytes = compare_max;
    }
  else
    {
      /* If we have a length, it must be constant.  */
      if (!CONST_INT_P (bytes_rtx))
	return false;
      nbytes = UINTVAL (bytes_rtx);

      /* We don't emit parts of a strncmp() call.  */
      if (nbytes > compare_max)
	return false;
    }

  /* Guarantees:
     - nbytes > 0
     - nbytes <= riscv_strcmp_inline_limit
     - nbytes is a multiple of xlen if !ncompare  */

  if (!CONST_INT_P (align_rtx))
    return false;
  alignment = UINTVAL (align_rtx);

  if (TARGET_VECTOR && stringop_strategy & STRATEGY_VECTOR)
    {
      bool ok = riscv_vector::expand_strcmp (result, src1, src2,
					     bytes_rtx, alignment,
					     ncompare);
      if (ok)
	return true;
    }

  if ((TARGET_ZBB || TARGET_XTHEADBB) && stringop_strategy & STRATEGY_SCALAR)
    return riscv_expand_strcmp_scalar (result, src1, src2, nbytes, alignment,
				       ncompare);

  return false;
}

/* If the provided string is aligned, then read XLEN bytes
   in a loop and use orc.b to find NUL-bytes.  */

static bool
riscv_expand_strlen_scalar (rtx result, rtx src, rtx align)
{
  rtx testval, addr, addr_plus_regsz, word, zeros;
  rtx loop_label, cond;

  gcc_assert (TARGET_ZBB || TARGET_XTHEADBB);

  /* The alignment needs to be known and big enough.  */
  if (!CONST_INT_P (align) || UINTVAL (align) < GET_MODE_SIZE (Xmode))
    return false;

  testval = gen_reg_rtx (Xmode);
  addr = copy_addr_to_reg (XEXP (src, 0));
  addr_plus_regsz = gen_reg_rtx (Pmode);
  word = gen_reg_rtx (Xmode);
  zeros = gen_reg_rtx (Xmode);

  if (TARGET_ZBB)
    emit_insn (gen_rtx_SET (testval, constm1_rtx));
  else
    emit_insn (gen_rtx_SET (testval, const0_rtx));

  do_add3 (addr_plus_regsz, addr, GEN_INT (UNITS_PER_WORD));

  loop_label = gen_label_rtx ();
  emit_label (loop_label);

  /* Load a word and use orc.b/th.tstnbz to find a zero-byte.  */
  do_load_from_addr (Xmode, word, addr, src);
  do_add3 (addr, addr, GEN_INT (UNITS_PER_WORD));
  if (TARGET_ZBB)
    do_orcb2 (word, word);
  else
    do_th_tstnbz2 (word, word);
  cond = gen_rtx_EQ (VOIDmode, word, testval);
  emit_unlikely_jump_insn (gen_cbranch4 (Xmode, cond, word, testval, loop_label));

  /* Calculate the return value by counting zero-bits.  */
  if (TARGET_ZBB)
    do_one_cmpl2 (word, word);
  if (TARGET_BIG_ENDIAN)
    do_clz2 (zeros, word);
  else if (TARGET_ZBB)
    do_ctz2 (zeros, word);
  else
    {
      do_th_rev2 (word, word);
      do_clz2 (zeros, word);
    }

  do_lshr3 (zeros, zeros, GEN_INT (exact_log2 (BITS_PER_UNIT)));
  do_add3 (addr, addr, zeros);
  do_sub3 (result, addr, addr_plus_regsz);

  return true;
}

/* Expand a strlen operation and return true if successful.
   Return false if we should let the compiler generate normal
   code, probably a strlen call.  */

bool
riscv_expand_strlen (rtx result, rtx src, rtx search_char, rtx align)
{
  if (TARGET_VECTOR && stringop_strategy & STRATEGY_VECTOR)
    {
      riscv_vector::expand_rawmemchr (E_QImode, result, src, search_char,
				      /* strlen */ true);
      return true;
    }

  gcc_assert (search_char == const0_rtx);

  if ((TARGET_ZBB || TARGET_XTHEADBB) && stringop_strategy & STRATEGY_SCALAR)
    return riscv_expand_strlen_scalar (result, src, align);

  return false;
}

/* Emit straight-line code to move LENGTH bytes from SRC to DEST.
   Assume that the areas do not overlap.  */

static void
riscv_block_move_straight (rtx dest, rtx src, unsigned HOST_WIDE_INT length)
{
  unsigned HOST_WIDE_INT offset, delta;
  unsigned HOST_WIDE_INT bits;
  int i;
  enum machine_mode mode;
  rtx *regs;

  bits = MAX (BITS_PER_UNIT,
	      MIN (BITS_PER_WORD, MIN (MEM_ALIGN (src), MEM_ALIGN (dest))));

  mode = mode_for_size (bits, MODE_INT, 0).require ();
  delta = bits / BITS_PER_UNIT;

  /* Allocate a buffer for the temporary registers.  */
  regs = XALLOCAVEC (rtx, length / delta);

  /* Load as many BITS-sized chunks as possible.  Use a normal load if
     the source has enough alignment, otherwise use left/right pairs.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    {
      regs[i] = gen_reg_rtx (mode);
      riscv_emit_move (regs[i], adjust_address (src, mode, offset));
    }

  /* Copy the chunks to the destination.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    riscv_emit_move (adjust_address (dest, mode, offset), regs[i]);

  /* Mop up any left-over bytes.  */
  if (offset < length)
    {
      src = adjust_address (src, BLKmode, offset);
      dest = adjust_address (dest, BLKmode, offset);
      move_by_pieces (dest, src, length - offset,
		      MIN (MEM_ALIGN (src), MEM_ALIGN (dest)), RETURN_BEGIN);
    }
}

/* Helper function for doing a loop-based block operation on memory
   reference MEM.  Each iteration of the loop will operate on LENGTH
   bytes of MEM.

   Create a new base register for use within the loop and point it to
   the start of MEM.  Create a new memory reference that uses this
   register.  Store them in *LOOP_REG and *LOOP_MEM respectively.  */

static void
riscv_adjust_block_mem (rtx mem, unsigned HOST_WIDE_INT length,
			rtx *loop_reg, rtx *loop_mem)
{
  *loop_reg = copy_addr_to_reg (XEXP (mem, 0));

  /* Although the new mem does not refer to a known location,
     it does keep up to LENGTH bytes of alignment.  */
  *loop_mem = change_address (mem, BLKmode, *loop_reg);
  set_mem_align (*loop_mem, MIN (MEM_ALIGN (mem), length * BITS_PER_UNIT));
}

/* Move LENGTH bytes from SRC to DEST using a loop that moves BYTES_PER_ITER
   bytes at a time.  LENGTH must be at least BYTES_PER_ITER.  Assume that
   the memory regions do not overlap.  */

static void
riscv_block_move_loop (rtx dest, rtx src, unsigned HOST_WIDE_INT length,
		       unsigned HOST_WIDE_INT bytes_per_iter)
{
  rtx label, src_reg, dest_reg, final_src, test;
  unsigned HOST_WIDE_INT leftover;

  leftover = length % bytes_per_iter;
  length -= leftover;

  /* Create registers and memory references for use within the loop.  */
  riscv_adjust_block_mem (src, bytes_per_iter, &src_reg, &src);
  riscv_adjust_block_mem (dest, bytes_per_iter, &dest_reg, &dest);

  /* Calculate the value that SRC_REG should have after the last iteration
     of the loop.  */
  final_src = expand_simple_binop (Pmode, PLUS, src_reg, GEN_INT (length),
				   0, 0, OPTAB_WIDEN);

  /* Emit the start of the loop.  */
  label = gen_label_rtx ();
  emit_label (label);

  /* Emit the loop body.  */
  riscv_block_move_straight (dest, src, bytes_per_iter);

  /* Move on to the next block.  */
  riscv_emit_move (src_reg, plus_constant (Pmode, src_reg, bytes_per_iter));
  riscv_emit_move (dest_reg, plus_constant (Pmode, dest_reg, bytes_per_iter));

  /* Emit the loop condition.  */
  test = gen_rtx_NE (VOIDmode, src_reg, final_src);
  emit_jump_insn (gen_cbranch4 (Pmode, test, src_reg, final_src, label));

  /* Mop up any left-over bytes.  */
  if (leftover)
    riscv_block_move_straight (dest, src, leftover);
  else
    emit_insn(gen_nop ());
}

/* Expand a cpymemsi instruction, which copies LENGTH bytes from
   memory reference SRC to memory reference DEST.  */

static bool
riscv_expand_block_move_scalar (rtx dest, rtx src, rtx length)
{
  if (!CONST_INT_P (length))
    return false;

  unsigned HOST_WIDE_INT hwi_length = UINTVAL (length);
  unsigned HOST_WIDE_INT factor, align;

  align = MIN (MIN (MEM_ALIGN (src), MEM_ALIGN (dest)), BITS_PER_WORD);
  factor = BITS_PER_WORD / align;

  if (optimize_function_for_size_p (cfun)
      && hwi_length * factor * UNITS_PER_WORD > MOVE_RATIO (false))
    return false;

  if (hwi_length <= (RISCV_MAX_MOVE_BYTES_STRAIGHT / factor))
    {
      riscv_block_move_straight (dest, src, INTVAL (length));
      return true;
    }
  else if (optimize && align >= BITS_PER_WORD)
    {
      unsigned min_iter_words
	= RISCV_MAX_MOVE_BYTES_PER_LOOP_ITER / UNITS_PER_WORD;
      unsigned iter_words = min_iter_words;
      unsigned HOST_WIDE_INT bytes = hwi_length;
      unsigned HOST_WIDE_INT words = bytes / UNITS_PER_WORD;

      /* Lengthen the loop body if it shortens the tail.  */
      for (unsigned i = min_iter_words; i < min_iter_words * 2 - 1; i++)
	{
	  unsigned cur_cost = iter_words + words % iter_words;
	  unsigned new_cost = i + words % i;
	  if (new_cost <= cur_cost)
	    iter_words = i;
	}

      riscv_block_move_loop (dest, src, bytes, iter_words * UNITS_PER_WORD);
      return true;
    }

  return false;
}

/* This function delegates block-move expansion to either the vector
   implementation or the scalar one.  Return TRUE if successful or FALSE
   otherwise.  */

bool
riscv_expand_block_move (rtx dest, rtx src, rtx length)
{
  if ((TARGET_VECTOR && !TARGET_XTHEADVECTOR)
      && stringop_strategy & STRATEGY_VECTOR)
    {
      bool ok = riscv_vector::expand_block_move (dest, src, length);
      if (ok)
	return true;
    }

  if (stringop_strategy & STRATEGY_SCALAR)
    return riscv_expand_block_move_scalar (dest, src, length);

  return false;
}

/* --- Vector expanders --- */

namespace riscv_vector {

/* Used by cpymemsi in riscv.md .  */

bool
expand_block_move (rtx dst_in, rtx src_in, rtx length_in)
{
  /*
    memcpy:
	mv a3, a0                       # Copy destination
    loop:
	vsetvli t0, a2, e8, m8, ta, ma  # Vectors of 8b
	vle8.v v0, (a1)                 # Load bytes
	add a1, a1, t0                  # Bump pointer
	sub a2, a2, t0                  # Decrement count
	vse8.v v0, (a3)                 # Store bytes
	add a3, a3, t0                  # Bump pointer
	bnez a2, loop                   # Any more?
	ret                             # Return
  */
  gcc_assert (TARGET_VECTOR);

  HOST_WIDE_INT potential_ew
    = (MIN (MIN (MEM_ALIGN (src_in), MEM_ALIGN (dst_in)), BITS_PER_WORD)
       / BITS_PER_UNIT);
  machine_mode vmode = VOIDmode;
  bool need_loop = true;
  bool size_p = optimize_function_for_size_p (cfun);
  rtx src, dst;
  rtx end = gen_reg_rtx (Pmode);
  rtx vec;
  rtx length_rtx = length_in;

  if (CONST_INT_P (length_in))
    {
      HOST_WIDE_INT length = INTVAL (length_in);

    /* By using LMUL=8, we can copy as many bytes in one go as there
       are bits in a vector register.  If the entire block thus fits,
       we don't need a loop.  */
    if (length <= TARGET_MIN_VLEN)
      {
	need_loop = false;

	/* If a single scalar load / store pair can do the job, leave it
	   to the scalar code to do that.  */
	/* ??? If fast unaligned access is supported, the scalar code could
	   use suitably sized scalars irrespective of alignemnt.  If that
	   gets fixed, we have to adjust the test here.  */

	if (pow2p_hwi (length) && length <= potential_ew)
	  return false;
      }

      /* Find the vector mode to use.  Using the largest possible element
	 size is likely to give smaller constants, and thus potentially
	 reducing code size.  However, if we need a loop, we need to update
	 the pointers, and that is more complicated with a larger element
	 size, unless we use an immediate, which prevents us from dynamically
	 using the targets transfer size that the hart supports.  And then,
	 unless we know the *exact* vector size of the hart, we'd need
	 multiple vsetvli / branch statements, so it's not even a size win.
	 If, in the future, we find an RISCV-V implementation that is slower
	 for small element widths, we might allow larger element widths for
	 loops too.  */
      if (need_loop)
	potential_ew = 1;
      for (; potential_ew; potential_ew >>= 1)
	{
	  scalar_int_mode elem_mode;
	  unsigned HOST_WIDE_INT bits = potential_ew * BITS_PER_UNIT;
	  unsigned HOST_WIDE_INT per_iter;
	  HOST_WIDE_INT nunits;

	  if (need_loop)
	    per_iter = TARGET_MIN_VLEN;
	  else
	    per_iter = length;
	  nunits = per_iter / potential_ew;

	  /* Unless we get an implementation that's slow for small element
	     size / non-word-aligned accesses, we assume that the hardware
	     handles this well, and we don't want to complicate the code
	     with shifting word contents around or handling extra bytes at
	     the start and/or end.  So we want the total transfer size and
	     alignment to fit with the element size.  */
	  if (length % potential_ew != 0
	      || !int_mode_for_size (bits, 0).exists (&elem_mode))
	    continue;
	  /* Find the mode to use for the copy inside the loop - or the
	     sole copy, if there is no loop.  */
	  if (!need_loop)
	    {
	      /* Try if we have an exact mode for the copy.  */
	      if (riscv_vector::get_vector_mode (elem_mode,
						 nunits).exists (&vmode))
		break;
	      /* Since we don't have a mode that exactlty matches the transfer
		 size, we'll need to use pred_store, which is not available
		 for all vector modes, but only iE_RVV_M* modes, hence trying
		 to find a vector mode for a merely rounded-up size is
		 pointless.
		 Still, by choosing a lower LMUL factor that still allows
		 an entire transfer, we can reduce register pressure.  */
	      for (unsigned lmul = 1; lmul <= 4; lmul <<= 1)
		if (TARGET_MIN_VLEN * lmul <= nunits * BITS_PER_UNIT
		    /* Avoid loosing the option of using vsetivli .  */
		    && (nunits <= 31 * lmul || nunits > 31 * 8)
		    && multiple_p (BYTES_PER_RISCV_VECTOR * lmul, potential_ew)
		    && (riscv_vector::get_vector_mode
			 (elem_mode, exact_div (BYTES_PER_RISCV_VECTOR * lmul,
				     potential_ew)).exists (&vmode)))
		  break;
	    }

	  /* The RVVM8?I modes are notionally 8 * BYTES_PER_RISCV_VECTOR bytes
	     wide.  BYTES_PER_RISCV_VECTOR can't be eavenly divided by
	     the sizes of larger element types; the LMUL factor of 8 can at
	     the moment be divided by the SEW, with SEW of up to 8 bytes,
	     but there are reserved encodings so there might be larger
	     SEW in the future.  */
	  if (riscv_vector::get_vector_mode
	      (elem_mode, exact_div (BYTES_PER_RISCV_VECTOR * 8,
				     potential_ew)).exists (&vmode))
	    break;

	  /* We may get here if we tried an element size that's larger than
	     the hardware supports, but we should at least find a suitable
	     byte vector mode.  */
	  gcc_assert (potential_ew > 1);
	}
      if (potential_ew > 1)
	length_rtx = GEN_INT (length / potential_ew);
    }
  else
    {
      vmode = E_RVVM8QImode;
    }

  /* A memcpy libcall in the worst case takes 3 instructions to prepare the
     arguments + 1 for the call.  When RVV should take 7 instructions and
     we're optimizing for size a libcall may be preferable.  */
  if (size_p && need_loop)
    return false;

  /* length_rtx holds the (remaining) length of the required copy.
     cnt holds the length we copy with the current load/store pair.  */
  rtx cnt = length_rtx;
  rtx label = NULL_RTX;
  rtx dst_addr = copy_addr_to_reg (XEXP (dst_in, 0));
  rtx src_addr = copy_addr_to_reg (XEXP (src_in, 0));

  if (need_loop)
    {
      length_rtx = copy_to_mode_reg (Pmode, length_rtx);
      cnt = gen_reg_rtx (Pmode);
      label = gen_label_rtx ();

      emit_label (label);
      emit_insn (riscv_vector::gen_no_side_effects_vsetvl_rtx (vmode, cnt,
							       length_rtx));
    }

  vec = gen_reg_rtx (vmode);
  src = change_address (src_in, vmode, src_addr);
  dst = change_address (dst_in, vmode, dst_addr);

  /* If we don't need a loop and have a suitable mode to describe the size,
     just do a load / store pair and leave it up to the later lazy code
     motion pass to insert the appropriate vsetvli.  */
  if (!need_loop && known_eq (GET_MODE_SIZE (vmode), INTVAL (length_in)))
    {
      emit_move_insn (vec, src);
      emit_move_insn (dst, vec);
    }
  else
    {
      machine_mode mask_mode = riscv_vector::get_vector_mode
	(BImode, GET_MODE_NUNITS (vmode)).require ();
      rtx mask =  CONSTM1_RTX (mask_mode);
      if (!satisfies_constraint_K (cnt))
	cnt= force_reg (Pmode, cnt);
      rtx m_ops[] = {vec, mask, src};
      emit_nonvlmax_insn (code_for_pred_mov (vmode),
			  riscv_vector::UNARY_OP_TAMA, m_ops, cnt);
      emit_insn (gen_pred_store (vmode, dst, mask, vec, cnt,
				 get_avl_type_rtx (riscv_vector::NONVLMAX)));
    }

  if (need_loop)
    {
      emit_insn (gen_rtx_SET (src_addr, gen_rtx_PLUS (Pmode, src_addr, cnt)));
      emit_insn (gen_rtx_SET (dst_addr, gen_rtx_PLUS (Pmode, dst_addr, cnt)));
      emit_insn (gen_rtx_SET (length_rtx, gen_rtx_MINUS (Pmode, length_rtx, cnt)));

      /* Emit the loop condition.  */
      rtx test = gen_rtx_NE (VOIDmode, end, const0_rtx);
      emit_jump_insn (gen_cbranch4 (Pmode, test, length_rtx, const0_rtx, label));
      emit_insn (gen_nop ());
    }

  return true;
}


/* Implement rawmemchr<mode> and strlen using vector instructions.
   It can be assumed that the needle is in the haystack, otherwise the
   behavior is undefined.  */

void
expand_rawmemchr (machine_mode mode, rtx dst, rtx haystack, rtx needle,
		  bool strlen)
{
  /*
    rawmemchr:
    loop:
       vsetvli a1, zero, e[8,16,32,64], m1, ta, ma
       vle[8,16,32,64]ff.v v8, (a0)  # Load.
       csrr a1, vl		     # Get number of bytes read.
       vmseq.vx v0, v8, pat	     # v0 = (v8 == {pat, pat, ...})
       vfirst.m a2, v0		     # Find first hit.
       add a0, a0, a1		     # Bump pointer.
       bltz a2, loop		     # Not found?

       sub a0, a0, a1		     # Go back by a1.
       shll a2, a2, [0,1,2,3]	     # Shift to get byte offset.
       add a0, a0, a2		     # Add the offset.

       ret
  */
  gcc_assert (TARGET_VECTOR);

  if (strlen)
    gcc_assert (mode == E_QImode);

  unsigned int isize = GET_MODE_SIZE (mode).to_constant ();
  int lmul = TARGET_MAX_LMUL;
  poly_int64 nunits = exact_div (BYTES_PER_RISCV_VECTOR * lmul, isize);

  machine_mode vmode;
  if (!riscv_vector::get_vector_mode (GET_MODE_INNER (mode),
				      nunits).exists (&vmode))
    gcc_unreachable ();

  machine_mode mask_mode = riscv_vector::get_mask_mode (vmode);

  rtx cnt = gen_reg_rtx (Pmode);
  emit_move_insn (cnt, CONST0_RTX (Pmode));

  rtx end = gen_reg_rtx (Pmode);
  rtx vec = gen_reg_rtx (vmode);
  rtx mask = gen_reg_rtx (mask_mode);

  /* After finding the first vector element matching the needle, we
     need to multiply by the vector element width (SEW) in order to
     return a pointer to the matching byte.  */
  unsigned int shift = exact_log2 (GET_MODE_SIZE (mode).to_constant ());

  rtx src_addr = copy_addr_to_reg (XEXP (haystack, 0));
  rtx start_addr = copy_addr_to_reg (XEXP (haystack, 0));

  rtx loop = gen_label_rtx ();
  emit_label (loop);

  rtx vsrc = change_address (haystack, vmode, src_addr);

  /* Bump the pointer.  */
  rtx step = gen_reg_rtx (Pmode);
  emit_insn (gen_rtx_SET (step, gen_rtx_ASHIFT (Pmode, cnt, GEN_INT (shift))));
  emit_insn (gen_rtx_SET (src_addr, gen_rtx_PLUS (Pmode, src_addr, step)));

  /* Emit a first-fault load.  */
  rtx vlops[] = {vec, vsrc};
  emit_vlmax_insn (code_for_pred_fault_load (vmode),
		   riscv_vector::UNARY_OP, vlops);

  /* Read how far we read.  */
  if (Pmode == SImode)
    emit_insn (gen_read_vlsi (cnt));
  else
    emit_insn (gen_read_vldi_zero_extend (cnt));

  /* Compare needle with haystack and store in a mask.  */
  rtx eq = gen_rtx_EQ (mask_mode, gen_const_vec_duplicate (vmode, needle), vec);
  rtx vmsops[] = {mask, eq, vec, needle};
  emit_nonvlmax_insn (code_for_pred_eqne_scalar (vmode),
		      riscv_vector::COMPARE_OP, vmsops, cnt);

  /* Find the first bit in the mask.  */
  rtx vfops[] = {end, mask};
  emit_nonvlmax_insn (code_for_pred_ffs (mask_mode, Pmode),
		      riscv_vector::CPOP_OP, vfops, cnt);

  /* Emit the loop condition.  */
  rtx test = gen_rtx_LT (VOIDmode, end, const0_rtx);
  emit_jump_insn (gen_cbranch4 (Pmode, test, end, const0_rtx, loop));

  if (strlen)
    {
      /* For strlen, return the length.  */
      emit_insn (gen_rtx_SET (dst, gen_rtx_PLUS (Pmode, src_addr, end)));
      emit_insn (gen_rtx_SET (dst, gen_rtx_MINUS (Pmode, dst, start_addr)));
    }
  else
    {
      /*  For rawmemchr, return the position at SRC + END * [1,2,4,8].  */
      emit_insn (gen_rtx_SET (end, gen_rtx_ASHIFT (Pmode, end, GEN_INT (shift))));
      emit_insn (gen_rtx_SET (dst, gen_rtx_PLUS (Pmode, src_addr, end)));
    }
}

/* Implement cmpstr<mode> using vector instructions.  The ALIGNMENT and
   NCOMPARE parameters are unused for now.  */

bool
expand_strcmp (rtx result, rtx src1, rtx src2, rtx nbytes,
	       unsigned HOST_WIDE_INT, bool)
{
  gcc_assert (TARGET_VECTOR);

  /* We don't support big endian.  */
  if (BYTES_BIG_ENDIAN)
    return false;

  bool with_length = nbytes != NULL_RTX;

  if (with_length
      && (!REG_P (nbytes) && !SUBREG_P (nbytes) && !CONST_INT_P (nbytes)))
    return false;

  if (with_length && CONST_INT_P (nbytes))
    nbytes = force_reg (Pmode, nbytes);

  machine_mode mode = E_QImode;
  unsigned int isize = GET_MODE_SIZE (mode).to_constant ();
  int lmul = TARGET_MAX_LMUL;
  poly_int64 nunits = exact_div (BYTES_PER_RISCV_VECTOR * lmul, isize);

  machine_mode vmode;
  if (!riscv_vector::get_vector_mode (GET_MODE_INNER (mode), nunits)
	 .exists (&vmode))
    gcc_unreachable ();

  machine_mode mask_mode = riscv_vector::get_mask_mode (vmode);

  /* Prepare addresses.  */
  rtx src_addr1 = copy_addr_to_reg (XEXP (src1, 0));
  rtx vsrc1 = change_address (src1, vmode, src_addr1);

  rtx src_addr2 = copy_addr_to_reg (XEXP (src2, 0));
  rtx vsrc2 = change_address (src2, vmode, src_addr2);

  /* Set initial pointer bump to 0.  */
  rtx cnt = gen_reg_rtx (Pmode);
  emit_move_insn (cnt, CONST0_RTX (Pmode));

  rtx sub = gen_reg_rtx (Pmode);
  emit_move_insn (sub, CONST0_RTX (Pmode));

  /* Create source vectors.  */
  rtx vec1 = gen_reg_rtx (vmode);
  rtx vec2 = gen_reg_rtx (vmode);

  rtx done = gen_label_rtx ();
  rtx loop = gen_label_rtx ();
  emit_label (loop);

  /* Bump the pointers.  */
  emit_insn (gen_rtx_SET (src_addr1, gen_rtx_PLUS (Pmode, src_addr1, cnt)));
  emit_insn (gen_rtx_SET (src_addr2, gen_rtx_PLUS (Pmode, src_addr2, cnt)));

  rtx vlops1[] = {vec1, vsrc1};
  rtx vlops2[] = {vec2, vsrc2};

  if (!with_length)
    {
      emit_vlmax_insn (code_for_pred_fault_load (vmode),
		       riscv_vector::UNARY_OP, vlops1);

      emit_vlmax_insn (code_for_pred_fault_load (vmode),
		       riscv_vector::UNARY_OP, vlops2);
    }
  else
    {
      nbytes = gen_lowpart (Pmode, nbytes);
      emit_nonvlmax_insn (code_for_pred_fault_load (vmode),
			  riscv_vector::UNARY_OP, vlops1, nbytes);

      emit_nonvlmax_insn (code_for_pred_fault_load (vmode),
			  riscv_vector::UNARY_OP, vlops2, nbytes);
    }

  /* Read the vl for the next pointer bump.  */
  if (Pmode == SImode)
    emit_insn (gen_read_vlsi (cnt));
  else
    emit_insn (gen_read_vldi_zero_extend (cnt));

  if (with_length)
    {
      rtx test_done = gen_rtx_EQ (VOIDmode, cnt, const0_rtx);
      emit_jump_insn (gen_cbranch4 (Pmode, test_done, cnt, const0_rtx, done));
      emit_insn (gen_rtx_SET (nbytes, gen_rtx_MINUS (Pmode, nbytes, cnt)));
    }

  /* Look for a \0 in the first string.  */
  rtx mask0 = gen_reg_rtx (mask_mode);
  rtx eq0
    = gen_rtx_EQ (mask_mode, gen_const_vec_duplicate (vmode, CONST0_RTX (mode)),
		  vec1);
  rtx vmsops1[] = {mask0, eq0, vec1, CONST0_RTX (mode)};
  emit_nonvlmax_insn (code_for_pred_eqne_scalar (vmode),
		      riscv_vector::COMPARE_OP, vmsops1, cnt);

  /* Look for vec1 != vec2 (includes vec2[i] == 0).  */
  rtx maskne = gen_reg_rtx (mask_mode);
  rtx ne = gen_rtx_NE (mask_mode, vec1, vec2);
  rtx vmsops[] = {maskne, ne, vec1, vec2};
  emit_nonvlmax_insn (code_for_pred_cmp (vmode), riscv_vector::COMPARE_OP,
		      vmsops, cnt);

  /* Combine both masks into one.  */
  rtx mask = gen_reg_rtx (mask_mode);
  rtx vmorops[] = {mask, mask0, maskne};
  emit_nonvlmax_insn (code_for_pred (IOR, mask_mode),
		      riscv_vector::BINARY_MASK_OP, vmorops, cnt);

  /* Find the first bit in the mask (the first unequal element).  */
  rtx found_at = gen_reg_rtx (Pmode);
  rtx vfops[] = {found_at, mask};
  emit_nonvlmax_insn (code_for_pred_ffs (mask_mode, Pmode),
		      riscv_vector::CPOP_OP, vfops, cnt);

  /* Emit the loop condition.  */
  rtx test = gen_rtx_LT (VOIDmode, found_at, const0_rtx);
  emit_jump_insn (gen_cbranch4 (Pmode, test, found_at, const0_rtx, loop));

  /* Walk up to the difference point.  */
  emit_insn (
    gen_rtx_SET (src_addr1, gen_rtx_PLUS (Pmode, src_addr1, found_at)));
  emit_insn (
    gen_rtx_SET (src_addr2, gen_rtx_PLUS (Pmode, src_addr2, found_at)));

  /* Load the respective byte and compute the difference.  */
  rtx c1 = gen_reg_rtx (Pmode);
  rtx c2 = gen_reg_rtx (Pmode);

  do_load_from_addr (mode, c1, src_addr1, src1);
  do_load_from_addr (mode, c2, src_addr2, src2);

  do_sub3 (sub, c1, c2);

  if (with_length)
    emit_label (done);

  emit_insn (gen_movsi (result, gen_lowpart (SImode, sub)));
  return true;
}

}
