/* Header file for routines that straddle the border between GIMPLE and
   SSA in gimple.
   Copyright (C) 2009-2013 Free Software Foundation, Inc.

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

#ifndef GCC_GIMPLE_SSA_H
#define GCC_GIMPLE_SSA_H

/* Return the set of VUSE operand for statement G.  */

static inline use_operand_p
gimple_vuse_op (const_gimple g)
{
  struct use_optype_d *ops;
  if (!gimple_has_mem_ops (g))
    return NULL_USE_OPERAND_P;
  ops = g->gsops.opbase.use_ops;
  if (ops
      && USE_OP_PTR (ops)->use == &g->gsmembase.vuse)
    return USE_OP_PTR (ops);
  return NULL_USE_OPERAND_P;
}

/* Return the set of VDEF operand for statement G.  */

static inline def_operand_p
gimple_vdef_op (gimple g)
{
  if (!gimple_has_mem_ops (g))
    return NULL_DEF_OPERAND_P;
  if (g->gsmembase.vdef)
    return &g->gsmembase.vdef;
  return NULL_DEF_OPERAND_P;
}

/* Mark statement S as modified, and update it.  */

static inline void
update_stmt (gimple s)
{
  if (gimple_has_ops (s))
    {
      gimple_set_modified (s, true);
      update_stmt_operands (s);
    }
}

/* Update statement S if it has been optimized.  */

static inline void
update_stmt_if_modified (gimple s)
{
  if (gimple_modified_p (s))
    update_stmt_operands (s);
}


#endif /* GCC_GIMPLE_SSA_H */
