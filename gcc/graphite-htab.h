/* Translation of CLAST (CLooG AST) to Gimple.
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

#ifndef GCC_GRAPHITE_HTAB_H
#define GCC_GRAPHITE_HTAB_H

#include "hash-table.h"
#include "graphite-clast-to-gimple.h"

/* Hashtable helpers.  */

struct bb_pbb_hasher : typed_free_remove <bb_pbb_def>
{
  typedef bb_pbb_def value_type;
  typedef bb_pbb_def compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Hash function for data base element BB_PBB.  */

inline hashval_t
bb_pbb_hasher::hash (const value_type *bb_pbb)
{
  return (hashval_t)(bb_pbb->bb->index);
}

/* Compare data base element PB1 and PB2.  */

inline bool
bb_pbb_hasher::equal (const value_type *bp1, const compare_type *bp2)
{
  return (bp1->bb->index == bp2->bb->index);
}

typedef hash_table <bb_pbb_hasher> bb_pbb_htab_type;

extern bool gloog (scop_p, bb_pbb_htab_type);
poly_bb_p find_pbb_via_hash (bb_pbb_htab_type, basic_block);
bool loop_is_parallel_p (loop_p, bb_pbb_htab_type, int);
scop_p get_loop_body_pbbs (loop_p, bb_pbb_htab_type, vec<poly_bb_p> *);

#endif
