/* Define control and data flow tables, and regsets.
   Copyright (C) 1987, 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "bitmap.h"

typedef bitmap regset;		/* Head of register set linked list.  */

/* Clear a register set by freeing up the linked list.  */
#define CLEAR_REG_SET(HEAD) bitmap_clear (HEAD)

/* Copy a register set to another register set.  */
#define COPY_REG_SET(TO, FROM) bitmap_copy (TO, FROM)

/* `and' a register set with a second register set.  */
#define AND_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_AND)

/* `and' the complement of a register set with a register set.  */
#define AND_COMPL_REG_SET(TO, FROM) \
  bitmap_operation (TO, TO, FROM, BITMAP_AND_COMPL)

/* Inclusive or a register set with a second register set.  */
#define IOR_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_IOR)

/* Or into TO the register set FROM1 `and'ed with the complement of FROM2.  */
#define IOR_AND_COMPL_REG_SET(TO, FROM1, FROM2) \
  bitmap_ior_and_compl (TO, FROM1, FROM2)

/* Clear a single register in a register set.  */
#define CLEAR_REGNO_REG_SET(HEAD, REG) bitmap_clear_bit (HEAD, REG)

/* Set a single register in a register set.  */
#define SET_REGNO_REG_SET(HEAD, REG) bitmap_set_bit (HEAD, REG)

/* Return true if a register is set in a register set.  */
#define REGNO_REG_SET_P(TO, REG) bitmap_bit_p (TO, REG)

/* Copy the hard registers in a register set to the hard register set.  */
#define REG_SET_TO_HARD_REG_SET(TO, FROM)				\
do {									\
  int i_;								\
  CLEAR_HARD_REG_SET (TO);						\
  for (i_ = 0; i_ < FIRST_PSEUDO_REGISTER; i_++)			\
    if (REGNO_REG_SET_P (FROM, i_))					\
      SET_HARD_REG_BIT (TO, i_);					\
} while (0)

/* Loop over all registers in REGSET, starting with MIN, setting REGNUM to the
   register number and executing CODE for all registers that are set. */
#define EXECUTE_IF_SET_IN_REG_SET(REGSET, MIN, REGNUM, CODE)		\
  EXECUTE_IF_SET_IN_BITMAP (REGSET, MIN, REGNUM, CODE)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in the first regset and not set in the second. */
#define EXECUTE_IF_AND_COMPL_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, CODE) \
  EXECUTE_IF_AND_COMPL_IN_BITMAP (REGSET1, REGSET2, MIN, REGNUM, CODE)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in both regsets. */
#define EXECUTE_IF_AND_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, CODE) \
  EXECUTE_IF_AND_IN_BITMAP (REGSET1, REGSET2, MIN, REGNUM, CODE)

/* Allocate a register set with oballoc.  */
#define OBSTACK_ALLOC_REG_SET(OBSTACK) BITMAP_OBSTACK_ALLOC (OBSTACK)

/* Allocate a register set with alloca.  */
#define ALLOCA_REG_SET() BITMAP_ALLOCA ()

/* Do any cleanup needed on a regset when it is no longer used.  */
#define FREE_REG_SET(REGSET) BITMAP_FREE(REGSET)

/* Do any one-time initializations needed for regsets.  */
#define INIT_ONCE_REG_SET() BITMAP_INIT_ONCE ()

/* Grow any tables needed when the number of registers is calculated
   or extended.  For the linked list allocation, nothing needs to
   be done, other than zero the statistics on the first allocation.  */
#define MAX_REGNO_REG_SET(NUM_REGS, NEW_P, RENUMBER_P)

/* Number of basic blocks in the current function.  */

extern int n_basic_blocks;

/* Index by basic block number, get first insn in the block.  */

extern rtx *basic_block_head;

/* Index by basic block number, get last insn in the block.  */

extern rtx *basic_block_end;

/* Index by basic block number, get address of regset
   describing the registers live at the start of that block.  */

extern regset *basic_block_live_at_start;

/* What registers are live at the setjmp call.  */

extern regset regs_live_at_setjmp;

/* Indexed by n, gives number of basic block that  (REG n) is used in.
   If the value is REG_BLOCK_GLOBAL (-2),
   it means (REG n) is used in more than one basic block.
   REG_BLOCK_UNKNOWN (-1) means it hasn't been seen yet so we don't know.
   This information remains valid for the rest of the compilation
   of the current function; it is used to control register allocation.  */

#define REG_BLOCK_UNKNOWN -1
#define REG_BLOCK_GLOBAL -2

#define REG_BASIC_BLOCK(N) (reg_n_info[(N)].basic_block)
