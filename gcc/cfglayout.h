/* Basic block reordering routines for the GNU compiler.
   Copyright (C) 2000, 2003 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* Structure to hold information about the blocks during reordering.  */
typedef struct reorder_block_def
{
  rtx header;
  rtx footer;
  basic_block next;
  basic_block original;
  /* Used by loop copying.  */
  basic_block copy;
  int duplicated;

  /* These fields are used by bb-reorder pass.  */
  int visited;
} *reorder_block_def;

extern rtx cfg_layout_function_footer;

extern void cfg_layout_initialize (unsigned int);
extern void cfg_layout_finalize (void);
extern bool cfg_layout_can_duplicate_bb_p (basic_block);
extern basic_block cfg_layout_duplicate_bb (basic_block, edge);
extern void insn_locators_initialize (void);
extern void reemit_insn_block_notes (void);
extern bool can_copy_bbs_p (basic_block *, unsigned);
extern void copy_bbs (basic_block *, unsigned, basic_block *,
		      edge *, unsigned, edge *, struct loop *);
extern void cfg_layout_initialize_rbi (basic_block);
