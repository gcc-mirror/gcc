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

#include "hash-map.h"

/* Hashtable helpers.  */

struct bb_pbb_hasher : default_hashmap_traits
{
  static inline hashval_t hash (const basic_block);
  static inline bool equal_keys (const basic_block, const basic_block);
};

/* Hash function.  */

inline hashval_t
bb_pbb_hasher::hash (const basic_block bb)
{
  return (hashval_t)(bb->index);
}

/* Compare data base element PB1 and PB2.  */

inline bool
bb_pbb_hasher::equal_keys (const basic_block a, const basic_block b)
{
  return (a->index == b->index);
}

typedef hash_map<basic_block, poly_bb_p, bb_pbb_hasher> bb_pbb_htab_type;

poly_bb_p find_pbb_via_hash (bb_pbb_htab_type *, basic_block);
bool loop_is_parallel_p (loop_p, bb_pbb_htab_type *, int);
scop_p get_loop_body_pbbs (loop_p, bb_pbb_htab_type *, vec<poly_bb_p> *);

#endif
