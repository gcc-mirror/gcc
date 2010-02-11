/* Translation of CLAST (CLooG AST) to Gimple.
   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_GRAPHITE_CLAST_TO_GIMPLE_H
#define GCC_GRAPHITE_CLAST_TO_GIMPLE_H

/* Data structure for CLooG program representation.  */

typedef struct cloog_prog_clast {
  CloogProgram *prog;
  struct clast_stmt *stmt;
} cloog_prog_clast;

/* Stores BB's related PBB.  */

typedef struct bb_pbb_def
{
  basic_block bb;
  poly_bb_p pbb;
}bb_pbb_def;

extern bool gloog (scop_p, VEC (scop_p, heap) *, htab_t);
extern cloog_prog_clast scop_to_clast (scop_p);
extern void debug_clast_stmt (struct clast_stmt *);
extern void print_clast_stmt (FILE *, struct clast_stmt *);
extern void debug_clast_name_indexes (htab_t);

/* Hash function for data base element BB_PBB.  */

static inline hashval_t
bb_pbb_map_hash (const void *bb_pbb)
{
  return (hashval_t)(((const bb_pbb_def *)bb_pbb)->bb->index);
}

/* Compare data base element BB_PBB1 and BB_PBB2.  */

static inline int
eq_bb_pbb_map (const void *bb_pbb1, const void *bb_pbb2)
{
  const bb_pbb_def *bp1 = (const bb_pbb_def *) bb_pbb1;
  const bb_pbb_def *bp2 = (const bb_pbb_def *) bb_pbb2;
  return (bp1->bb->index == bp2->bb->index);
}

/* Returns the scattering dimension for STMTFOR.

   The relationship between dimension in scattering matrix
   and the DEPTH of the loop is:
   DIMENSION = 2*DEPTH - 1
*/

static inline int get_scattering_level (int depth)
{
  return 2 * depth - 1;
}

#endif
