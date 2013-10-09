/* Inline functions for tree-flow.h
   Copyright (C) 2001-2013 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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

#ifndef _TREE_FLOW_INLINE_H
#define _TREE_FLOW_INLINE_H 1

/* Inline functions for manipulating various data structures defined in
   tree-flow.h.  See tree-flow.h for documentation.  */

/* Return true when gimple SSA form was built.
   gimple_in_ssa_p is queried by gimplifier in various early stages before SSA
   infrastructure is initialized.  Check for presence of the datastructures
   at first place.  */
static inline bool
gimple_in_ssa_p (const struct function *fun)
{
  return fun && fun->gimple_df && fun->gimple_df->in_ssa_p;
}

/* Artificial variable used for the virtual operand FUD chain.  */
static inline tree
gimple_vop (const struct function *fun)
{
  gcc_checking_assert (fun && fun->gimple_df);
  return fun->gimple_df->vop;
}

/* Get the number of the next statement uid to be allocated.  */
static inline unsigned int
gimple_stmt_max_uid (struct function *fn)
{
  return fn->last_stmt_uid;
}

/* Set the number of the next statement uid to be allocated.  */
static inline void
set_gimple_stmt_max_uid (struct function *fn, unsigned int maxid)
{
  fn->last_stmt_uid = maxid;
}

/* Set the number of the next statement uid to be allocated.  */
static inline unsigned int
inc_gimple_stmt_max_uid (struct function *fn)
{
  return fn->last_stmt_uid++;
}

/* Return the line number for EXPR, or return -1 if we have no line
   number information for it.  */
static inline int
get_lineno (const_gimple stmt)
{
  location_t loc;

  if (!stmt)
    return -1;

  loc = gimple_location (stmt);
  if (loc == UNKNOWN_LOCATION)
    return -1;

  return LOCATION_LINE (loc);
}


/* Return true if T (assumed to be a DECL) is a global variable.
   A variable is considered global if its storage is not automatic.  */

static inline bool
is_global_var (const_tree t)
{
  return (TREE_STATIC (t) || DECL_EXTERNAL (t));
}


/* Return true if VAR may be aliased.  A variable is considered as
   maybe aliased if it has its address taken by the local TU
   or possibly by another TU and might be modified through a pointer.  */

static inline bool
may_be_aliased (const_tree var)
{
  return (TREE_CODE (var) != CONST_DECL
	  && !((TREE_STATIC (var) || TREE_PUBLIC (var) || DECL_EXTERNAL (var))
	       && TREE_READONLY (var)
	       && !TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (var)))
	  && (TREE_PUBLIC (var)
	      || DECL_EXTERNAL (var)
	      || TREE_ADDRESSABLE (var)));
}

/* Return true if VAR cannot be modified by the program.  */

static inline bool
unmodifiable_var_p (const_tree var)
{
  if (TREE_CODE (var) == SSA_NAME)
    var = SSA_NAME_VAR (var);

  return TREE_READONLY (var) && (TREE_STATIC (var) || DECL_EXTERNAL (var));
}

/* Return true if REF, a handled component reference, has an ARRAY_REF
   somewhere in it.  */

static inline bool
ref_contains_array_ref (const_tree ref)
{
  gcc_checking_assert (handled_component_p (ref));

  do {
    if (TREE_CODE (ref) == ARRAY_REF)
      return true;
    ref = TREE_OPERAND (ref, 0);
  } while (handled_component_p (ref));

  return false;
}

/* Return true if REF has an VIEW_CONVERT_EXPR somewhere in it.  */

static inline bool
contains_view_convert_expr_p (const_tree ref)
{
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == VIEW_CONVERT_EXPR)
	return true;
      ref = TREE_OPERAND (ref, 0);
    }

  return false;
}

/* Return true, if the two ranges [POS1, SIZE1] and [POS2, SIZE2]
   overlap.  SIZE1 and/or SIZE2 can be (unsigned)-1 in which case the
   range is open-ended.  Otherwise return false.  */

static inline bool
ranges_overlap_p (unsigned HOST_WIDE_INT pos1,
		  unsigned HOST_WIDE_INT size1,
		  unsigned HOST_WIDE_INT pos2,
		  unsigned HOST_WIDE_INT size2)
{
  if (pos1 >= pos2
      && (size2 == (unsigned HOST_WIDE_INT)-1
	  || pos1 < (pos2 + size2)))
    return true;
  if (pos2 >= pos1
      && (size1 == (unsigned HOST_WIDE_INT)-1
	  || pos2 < (pos1 + size1)))
    return true;

  return false;
}

/* Accessor to tree-ssa-operands.c caches.  */
static inline struct ssa_operands *
gimple_ssa_operands (const struct function *fun)
{
  return &fun->gimple_df->ssa_operands;
}

#endif /* _TREE_FLOW_INLINE_H  */
