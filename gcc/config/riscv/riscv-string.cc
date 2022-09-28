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
GEN_EMIT_HELPER2(clz) /* do_clz2  */
GEN_EMIT_HELPER2(ctz) /* do_ctz2  */
GEN_EMIT_HELPER3(lshr) /* do_lshr3  */
GEN_EMIT_HELPER2(orcb) /* do_orcb2  */
GEN_EMIT_HELPER2(one_cmpl) /* do_one_cmpl2  */
GEN_EMIT_HELPER3(sub) /* do_sub3  */
GEN_EMIT_HELPER2(th_rev) /* do_th_rev2  */
GEN_EMIT_HELPER2(th_tstnbz) /* do_th_tstnbz2  */
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
