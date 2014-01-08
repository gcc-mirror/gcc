/* Declarations and data structures for stmt.c.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.

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

#ifndef GCC_STMT_H
#define GCC_STMT_H

extern void expand_label (tree);
extern bool parse_output_constraint (const char **, int, int, int,
				     bool *, bool *, bool *);
extern bool parse_input_constraint (const char **, int, int, int, int,
				    const char * const *, bool *, bool *);
extern tree resolve_asm_operand_names (tree, tree, tree, tree);
#ifdef HARD_CONST
/* Silly ifdef to avoid having all includers depend on hard-reg-set.h.  */
extern tree tree_overlaps_hard_reg_set (tree, HARD_REG_SET *);
#endif

#endif  // GCC_STMT_H
