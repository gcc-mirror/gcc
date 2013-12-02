/* Header file for tree data flow functions.
   Copyright (C) 2013 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_DFA_H
#define GCC_TREE_DFA_H

extern void renumber_gimple_stmt_uids (void);
extern void renumber_gimple_stmt_uids_in_blocks (basic_block *, int);
extern void dump_variable (FILE *, tree);
extern void debug_variable (tree);
extern void dump_dfa_stats (FILE *);
extern void debug_dfa_stats (void);
extern tree ssa_default_def (struct function *, tree);
extern void set_ssa_default_def (struct function *, tree, tree);
extern tree get_or_create_ssa_default_def (struct function *, tree);
extern tree get_ref_base_and_extent (tree, HOST_WIDE_INT *,
				     HOST_WIDE_INT *, HOST_WIDE_INT *);
extern tree get_addr_base_and_unit_offset (tree, HOST_WIDE_INT *);
extern bool stmt_references_abnormal_ssa_name (gimple);
extern void dump_enumerated_decls (FILE *, int);

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
		double_int doffset
		  = (TREE_INT_CST (index) - TREE_INT_CST (low_bound))
		    .sext (TYPE_PRECISION (TREE_TYPE (index)));
		doffset *= tree_to_double_int (unit_size);
		byte_offset += doffset.to_shwi ();
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



#endif /* GCC_TREE_DFA_H */
