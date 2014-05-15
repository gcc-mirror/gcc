/* Shrink-wrapping related functions.
   Copyright (C) 1989-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_SHRINK_WRAP_H
#define GCC_SHRINK_WRAP_H

#include "hashtab.h"
#include "vec.h"
#include "machmode.h"

#ifdef HAVE_simple_return
/* In function.c.  */
extern void emit_return_into_block (bool simple_p, basic_block bb);
extern bool active_insn_between (rtx head, rtx tail);
extern vec<edge> convert_jumps_to_returns (basic_block last_bb, bool simple_p,
					   vec<edge> unconverted);
extern basic_block emit_return_for_exit (edge exit_fallthru_edge,
					 bool simple_p);

/* In shrink-wrap.c.  */
extern bool requires_stack_frame_p (rtx, HARD_REG_SET, HARD_REG_SET);
extern void prepare_shrink_wrap (basic_block entry_block);
extern void dup_block_and_redirect (basic_block bb, basic_block copy_bb,
				    rtx before,	bitmap_head *need_prologue);
extern void try_shrink_wrapping (edge *entry_edge, edge orig_entry_edge,
				 bitmap_head *bb_flags, rtx prologue_seq);
extern edge get_unconverted_simple_return (edge, bitmap_head,
					   vec<edge> *, rtx *);
extern void convert_to_simple_return (edge entry_edge, edge orig_entry_edge,
				      bitmap_head bb_flags, rtx returnjump,
				      vec<edge> unconverted_simple_returns);
#endif

#endif  /* GCC_SHRINK_WRAP_H  */


