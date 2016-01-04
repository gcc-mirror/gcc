/* Shrink-wrapping related functions.
   Copyright (C) 1989-2016 Free Software Foundation, Inc.

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

#include "function.h"

/* In shrink-wrap.c.  */
extern bool requires_stack_frame_p (rtx_insn *, HARD_REG_SET, HARD_REG_SET);
extern void try_shrink_wrapping (edge *entry_edge, bitmap_head *bb_flags,
				 rtx_insn *prologue_seq);
extern edge get_unconverted_simple_return (edge, bitmap_head,
					   vec<edge> *, rtx_insn **);
extern void convert_to_simple_return (edge entry_edge, edge orig_entry_edge,
				      bitmap_head bb_flags,
				      rtx_insn *returnjump,
				      vec<edge> unconverted_simple_returns);
#define SHRINK_WRAPPING_ENABLED \
  (flag_shrink_wrap && targetm.have_simple_return ())

#endif  /* GCC_SHRINK_WRAP_H  */


