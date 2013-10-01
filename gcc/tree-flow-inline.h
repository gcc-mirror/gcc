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

/* Initialize the hashtable iterator HTI to point to hashtable TABLE */

static inline void *
first_htab_element (htab_iterator *hti, htab_t table)
{
  hti->htab = table;
  hti->slot = table->entries;
  hti->limit = hti->slot + htab_size (table);
  do
    {
      PTR x = *(hti->slot);
      if (x != HTAB_EMPTY_ENTRY && x != HTAB_DELETED_ENTRY)
	break;
    } while (++(hti->slot) < hti->limit);

  if (hti->slot < hti->limit)
    return *(hti->slot);
  return NULL;
}

/* Return current non-empty/deleted slot of the hashtable pointed to by HTI,
   or NULL if we have  reached the end.  */

static inline bool
end_htab_p (const htab_iterator *hti)
{
  if (hti->slot >= hti->limit)
    return true;
  return false;
}

/* Advance the hashtable iterator pointed to by HTI to the next element of the
   hashtable.  */

static inline void *
next_htab_element (htab_iterator *hti)
{
  while (++(hti->slot) < hti->limit)
    {
      PTR x = *(hti->slot);
      if (x != HTAB_EMPTY_ENTRY && x != HTAB_DELETED_ENTRY)
	return x;
    };
  return NULL;
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


/* Returns the loop of the statement STMT.  */

static inline struct loop *
loop_containing_stmt (gimple stmt)
{
  basic_block bb = gimple_bb (stmt);
  if (!bb)
    return NULL;

  return bb->loop_father;
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


/* Returns the base object and a constant BITS_PER_UNIT offset in *POFFSET that
   denotes the starting address of the memory access EXP.
   Returns NULL_TREE if the offset is not constant or any component
   is not BITS_PER_UNIT-aligned.
   VALUEIZE if non-NULL is used to valueize SSA names.  It should return
   its argument or a constant if the argument is known to be constant.  */
/* ??? This is a static inline here to avoid the overhead of the indirect calls
   to VALUEIZE.  But is this overhead really that significant?  And should we
   perhaps just rely on WHOPR to specialize the function?  */

static inline tree
get_addr_base_and_unit_offset_1 (tree exp, HOST_WIDE_INT *poffset,
				 tree (*valueize) (tree))
{
  HOST_WIDE_INT byte_offset = 0;

  /* Compute cumulative byte-offset for nested component-refs and array-refs,
     and find the ultimate containing object.  */
  while (1)
    {
      switch (TREE_CODE (exp))
	{
	case BIT_FIELD_REF:
	  {
	    HOST_WIDE_INT this_off = TREE_INT_CST_LOW (TREE_OPERAND (exp, 2));
	    if (this_off % BITS_PER_UNIT)
	      return NULL_TREE;
	    byte_offset += this_off / BITS_PER_UNIT;
	  }
	  break;

	case COMPONENT_REF:
	  {
	    tree field = TREE_OPERAND (exp, 1);
	    tree this_offset = component_ref_field_offset (exp);
	    HOST_WIDE_INT hthis_offset;

	    if (!this_offset
		|| TREE_CODE (this_offset) != INTEGER_CST
		|| (TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (field))
		    % BITS_PER_UNIT))
	      return NULL_TREE;

	    hthis_offset = TREE_INT_CST_LOW (this_offset);
	    hthis_offset += (TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (field))
			     / BITS_PER_UNIT);
	    byte_offset += hthis_offset;
	  }
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  {
	    tree index = TREE_OPERAND (exp, 1);
	    tree low_bound, unit_size;

	    if (valueize
		&& TREE_CODE (index) == SSA_NAME)
	      index = (*valueize) (index);

	    /* If the resulting bit-offset is constant, track it.  */
	    if (TREE_CODE (index) == INTEGER_CST
		&& (low_bound = array_ref_low_bound (exp),
		    TREE_CODE (low_bound) == INTEGER_CST)
		&& (unit_size = array_ref_element_size (exp),
		    TREE_CODE (unit_size) == INTEGER_CST))
	      {
		HOST_WIDE_INT hindex = TREE_INT_CST_LOW (index);

		hindex -= TREE_INT_CST_LOW (low_bound);
		hindex *= TREE_INT_CST_LOW (unit_size);
		byte_offset += hindex;
	      }
	    else
	      return NULL_TREE;
	  }
	  break;

	case REALPART_EXPR:
	  break;

	case IMAGPART_EXPR:
	  byte_offset += TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (exp)));
	  break;

	case VIEW_CONVERT_EXPR:
	  break;

	case MEM_REF:
	  {
	    tree base = TREE_OPERAND (exp, 0);
	    if (valueize
		&& TREE_CODE (base) == SSA_NAME)
	      base = (*valueize) (base);

	    /* Hand back the decl for MEM[&decl, off].  */
	    if (TREE_CODE (base) == ADDR_EXPR)
	      {
		if (!integer_zerop (TREE_OPERAND (exp, 1)))
		  {
		    double_int off = mem_ref_offset (exp);
		    gcc_assert (off.high == -1 || off.high == 0);
		    byte_offset += off.to_shwi ();
		  }
		exp = TREE_OPERAND (base, 0);
	      }
	    goto done;
	  }

	case TARGET_MEM_REF:
	  {
	    tree base = TREE_OPERAND (exp, 0);
	    if (valueize
		&& TREE_CODE (base) == SSA_NAME)
	      base = (*valueize) (base);

	    /* Hand back the decl for MEM[&decl, off].  */
	    if (TREE_CODE (base) == ADDR_EXPR)
	      {
		if (TMR_INDEX (exp) || TMR_INDEX2 (exp))
		  return NULL_TREE;
		if (!integer_zerop (TMR_OFFSET (exp)))
		  {
		    double_int off = mem_ref_offset (exp);
		    gcc_assert (off.high == -1 || off.high == 0);
		    byte_offset += off.to_shwi ();
		  }
		exp = TREE_OPERAND (base, 0);
	      }
	    goto done;
	  }

	default:
	  goto done;
	}

      exp = TREE_OPERAND (exp, 0);
    }
done:

  *poffset = byte_offset;
  return exp;
}

#endif /* _TREE_FLOW_INLINE_H  */
