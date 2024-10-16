/* Avoid store forwarding optimization pass.
   Copyright (C) 2024 Free Software Foundation, Inc.
   Contributed by VRULL GmbH.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AVOID_STORE_FORWARDING_H
#define GCC_AVOID_STORE_FORWARDING_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"

struct store_fwd_info
{
  /* The store instruction that is a store forwarding candidate.  */
  rtx_insn *store_insn;
  /* SET_DEST (single_set (store_insn)).  */
  rtx store_mem;
  /* The temporary that will hold the stored value at the original store
     position.  */
  rtx mov_reg;
  /* The instruction sequence that inserts the stored value's bits at the
     appropriate position in the loaded value.  */
  rtx_insn *bits_insert_insns;
  /* An instruction that saves the store's value in a register temporarily,
     (set (reg X) (SET_SRC (store_insn))).  */
  rtx_insn *save_store_value_insn;
  /* An instruction that stores the saved value back to memory,
     (set (SET_DEST (store_insn)) (reg X)).  */
  rtx_insn *store_saved_value_insn;
  /* The byte offset for the store's position within the load.  */
  HOST_WIDE_INT offset;

  unsigned int insn_cnt;
  bool remove;
  bool forwarded;
};

#endif  /* GCC_AVOID_STORE_FORWARDING_H  */
