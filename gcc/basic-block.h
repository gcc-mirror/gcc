/* Define control and data flow tables, and regsets.
   Copyright (C) 1987 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Number of bits in each actual element of a regset.  */

#define REGSET_ELT_BITS HOST_BITS_PER_WIDE_INT

/* Type to use for a regset element.  Note that lots of code assumes
   that the initial part of a regset that contains information on the
   hard registers is the same format as a HARD_REG_SET.  */

#define REGSET_ELT_TYPE unsigned HOST_WIDE_INT

/* Define the type for a pointer to a set with a bit for each
   (hard or pseudo) register.  */

typedef REGSET_ELT_TYPE *regset;

/* Size of a regset for the current function,
   in (1) bytes and (2) elements.  */

extern int regset_bytes;
extern int regset_size;

/* Number of basic blocks in the current function.  */

extern int n_basic_blocks;

/* Index by basic block number, get first insn in the block.  */

extern rtx *basic_block_head;

/* Index by basic block number, get last insn in the block.  */

extern rtx *basic_block_end;

/* Index by basic block number, get address of regset
   describing the registers live at the start of that block.  */

extern regset *basic_block_live_at_start;

/* Indexed by n, gives number of basic block that  (REG n) is used in.
   If the value is REG_BLOCK_GLOBAL (-2),
   it means (REG n) is used in more than one basic block.
   REG_BLOCK_UNKNOWN (-1) means it hasn't been seen yet so we don't know.
   This information remains valid for the rest of the compilation
   of the current function; it is used to control register allocation.  */

#define REG_BLOCK_UNKNOWN -1
#define REG_BLOCK_GLOBAL -2
extern int *reg_basic_block;
