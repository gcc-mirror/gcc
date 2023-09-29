/* Subroutines used to expand string operations for RISC-V.
   Copyright (C) 2023 Free Software Foundation, Inc.

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

  if (TARGET_ZBB || TARGET_XTHEADBB)
    {
      return riscv_expand_strcmp_scalar (result, src1, src2, nbytes, alignment,
					 ncompare);
    }

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
  gcc_assert (search_char == const0_rtx);

  if (TARGET_ZBB || TARGET_XTHEADBB)
    return riscv_expand_strlen_scalar (result, src, align);

  return false;
}
