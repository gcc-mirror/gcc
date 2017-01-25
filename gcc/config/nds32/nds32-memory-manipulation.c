/* Auxiliary functions for expand movmem, setmem, cmpmem, load_multiple
   and store_multiple pattern of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "explow.h"

/* ------------------------------------------------------------------------ */

/* Functions to expand load_multiple and store_multiple.
   They are auxiliary extern functions to help create rtx template.
   Check nds32-multiple.md file for the patterns.  */
rtx
nds32_expand_load_multiple (int base_regno, int count,
			    rtx base_addr, rtx basemem)
{
  int par_index;
  int offset;
  rtx result;
  rtx new_addr, mem, reg;

  /* Create the pattern that is presented in nds32-multiple.md.  */

  result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  for (par_index = 0; par_index < count; par_index++)
    {
      offset   = par_index * 4;
      /* 4-byte for loading data to each register.  */
      new_addr = plus_constant (Pmode, base_addr, offset);
      mem      = adjust_automodify_address_nv (basemem, SImode,
					       new_addr, offset);
      reg      = gen_rtx_REG (SImode, base_regno + par_index);

      XVECEXP (result, 0, par_index) = gen_rtx_SET (reg, mem);
    }

  return result;
}

rtx
nds32_expand_store_multiple (int base_regno, int count,
			     rtx base_addr, rtx basemem)
{
  int par_index;
  int offset;
  rtx result;
  rtx new_addr, mem, reg;

  /* Create the pattern that is presented in nds32-multiple.md.  */

  result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  for (par_index = 0; par_index < count; par_index++)
    {
      offset   = par_index * 4;
      /* 4-byte for storing data to memory.  */
      new_addr = plus_constant (Pmode, base_addr, offset);
      mem      = adjust_automodify_address_nv (basemem, SImode,
					       new_addr, offset);
      reg      = gen_rtx_REG (SImode, base_regno + par_index);

      XVECEXP (result, 0, par_index) = gen_rtx_SET (mem, reg);
    }

  return result;
}

/* Function to move block memory content by
   using load_multiple and store_multiple.
   This is auxiliary extern function to help create rtx template.
   Check nds32-multiple.md file for the patterns.  */
int
nds32_expand_movmemqi (rtx dstmem, rtx srcmem, rtx total_bytes, rtx alignment)
{
  HOST_WIDE_INT in_words, out_words;
  rtx dst_base_reg, src_base_reg;
  int maximum_bytes;

  /* Because reduced-set regsiters has few registers
     (r0~r5, r6~10, r15, r28~r31, where 'r15' and 'r28~r31'
      cannot be used for register allocation),
     using 8 registers (32 bytes) for moving memory block
     may easily consume all of them.
     It makes register allocation/spilling hard to work.
     So we only allow maximum=4 registers (16 bytes) for
     moving memory block under reduced-set registers.  */
  if (TARGET_REDUCED_REGS)
    maximum_bytes = 16;
  else
    maximum_bytes = 32;

  /* 1. Total_bytes is integer for sure.
     2. Alignment is integer for sure.
     3. Maximum 4 or 8 registers, 4 * 4 = 16 bytes, 8 * 4 = 32 bytes.
     4. Requires (n * 4) block size.
     5. Requires 4-byte alignment.  */
  if (GET_CODE (total_bytes) != CONST_INT
      || GET_CODE (alignment) != CONST_INT
      || INTVAL (total_bytes) > maximum_bytes
      || INTVAL (total_bytes) & 3
      || INTVAL (alignment) & 3)
    return 0;

  dst_base_reg = copy_to_mode_reg (SImode, XEXP (dstmem, 0));
  src_base_reg = copy_to_mode_reg (SImode, XEXP (srcmem, 0));

  out_words = in_words = INTVAL (total_bytes) / UNITS_PER_WORD;

  emit_insn (nds32_expand_load_multiple (0, in_words, src_base_reg, srcmem));
  emit_insn (nds32_expand_store_multiple (0, out_words, dst_base_reg, dstmem));

  /* Successfully create patterns, return 1.  */
  return 1;
}

/* ------------------------------------------------------------------------ */
