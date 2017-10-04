/* Data flow functions for trees.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "langhooks.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-dfa.h"

/* Build and maintain data flow information for trees.  */

/* Counters used to display DFA and SSA statistics.  */
struct dfa_stats_d
{
  long num_defs;
  long num_uses;
  long num_phis;
  long num_phi_args;
  size_t max_num_phi_args;
  long num_vdefs;
  long num_vuses;
};


/* Local functions.  */
static void collect_dfa_stats (struct dfa_stats_d *);


/*---------------------------------------------------------------------------
			Dataflow analysis (DFA) routines
---------------------------------------------------------------------------*/

/* Renumber all of the gimple stmt uids.  */

void
renumber_gimple_stmt_uids (void)
{
  basic_block bb;

  set_gimple_stmt_max_uid (cfun, 0);
  FOR_ALL_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator bsi;
      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
    }
}

/* Like renumber_gimple_stmt_uids, but only do work on the basic blocks
   in BLOCKS, of which there are N_BLOCKS.  Also renumbers PHIs.  */

void
renumber_gimple_stmt_uids_in_blocks (basic_block *blocks, int n_blocks)
{
  int i;

  set_gimple_stmt_max_uid (cfun, 0);
  for (i = 0; i < n_blocks; i++)
    {
      basic_block bb = blocks[i];
      gimple_stmt_iterator bsi;
      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
    }
}



/*---------------------------------------------------------------------------
			      Debugging functions
---------------------------------------------------------------------------*/

/* Dump variable VAR and its may-aliases to FILE.  */

void
dump_variable (FILE *file, tree var)
{
  if (TREE_CODE (var) == SSA_NAME)
    {
      if (POINTER_TYPE_P (TREE_TYPE (var)))
	dump_points_to_info_for (file, var);
      var = SSA_NAME_VAR (var);
    }

  if (var == NULL_TREE)
    {
      fprintf (file, "<nil>");
      return;
    }

  print_generic_expr (file, var, dump_flags);

  fprintf (file, ", UID D.%u", (unsigned) DECL_UID (var));
  if (DECL_PT_UID (var) != DECL_UID (var))
    fprintf (file, ", PT-UID D.%u", (unsigned) DECL_PT_UID (var));

  fprintf (file, ", ");
  print_generic_expr (file, TREE_TYPE (var), dump_flags);

  if (TREE_ADDRESSABLE (var))
    fprintf (file, ", is addressable");

  if (is_global_var (var))
    fprintf (file, ", is global");

  if (TREE_THIS_VOLATILE (var))
    fprintf (file, ", is volatile");

  if (cfun && ssa_default_def (cfun, var))
    {
      fprintf (file, ", default def: ");
      print_generic_expr (file, ssa_default_def (cfun, var), dump_flags);
    }

  if (DECL_INITIAL (var))
    {
      fprintf (file, ", initial: ");
      print_generic_expr (file, DECL_INITIAL (var), dump_flags);
    }

  fprintf (file, "\n");
}


/* Dump variable VAR and its may-aliases to stderr.  */

DEBUG_FUNCTION void
debug_variable (tree var)
{
  dump_variable (stderr, var);
}


/* Dump various DFA statistics to FILE.  */

void
dump_dfa_stats (FILE *file)
{
  struct dfa_stats_d dfa_stats;

  unsigned long size, total = 0;
  const char * const fmt_str   = "%-30s%-13s%12s\n";
  const char * const fmt_str_1 = "%-30s%13lu%11lu%c\n";
  const char * const fmt_str_3 = "%-43s%11lu%c\n";
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);

  collect_dfa_stats (&dfa_stats);

  fprintf (file, "\nDFA Statistics for %s\n\n", funcname);

  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, fmt_str, "", "  Number of  ", "Memory");
  fprintf (file, fmt_str, "", "  instances  ", "used ");
  fprintf (file, "---------------------------------------------------------\n");

  size = dfa_stats.num_uses * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "USE operands", dfa_stats.num_uses,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_defs * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "DEF operands", dfa_stats.num_defs,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_vuses * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "VUSE operands", dfa_stats.num_vuses,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_vdefs * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "VDEF operands", dfa_stats.num_vdefs,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_phis * sizeof (struct gphi);
  total += size;
  fprintf (file, fmt_str_1, "PHI nodes", dfa_stats.num_phis,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_phi_args * sizeof (struct phi_arg_d);
  total += size;
  fprintf (file, fmt_str_1, "PHI arguments", dfa_stats.num_phi_args,
 	   SCALE (size), LABEL (size));

  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, fmt_str_3, "Total memory used by DFA/SSA data", SCALE (total),
	   LABEL (total));
  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, "\n");

  if (dfa_stats.num_phis)
    fprintf (file, "Average number of arguments per PHI node: %.1f (max: %ld)\n",
	     (float) dfa_stats.num_phi_args / (float) dfa_stats.num_phis,
	     (long) dfa_stats.max_num_phi_args);

  fprintf (file, "\n");
}


/* Dump DFA statistics on stderr.  */

DEBUG_FUNCTION void
debug_dfa_stats (void)
{
  dump_dfa_stats (stderr);
}


/* Collect DFA statistics and store them in the structure pointed to by
   DFA_STATS_P.  */

static void
collect_dfa_stats (struct dfa_stats_d *dfa_stats_p ATTRIBUTE_UNUSED)
{
  basic_block bb;

  gcc_assert (dfa_stats_p);

  memset ((void *)dfa_stats_p, 0, sizeof (struct dfa_stats_d));

  /* Walk all the statements in the function counting references.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gphi *phi = si.phi ();
	  dfa_stats_p->num_phis++;
	  dfa_stats_p->num_phi_args += gimple_phi_num_args (phi);
	  if (gimple_phi_num_args (phi) > dfa_stats_p->max_num_phi_args)
	    dfa_stats_p->max_num_phi_args = gimple_phi_num_args (phi);
	}

      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  dfa_stats_p->num_defs += NUM_SSA_OPERANDS (stmt, SSA_OP_DEF);
	  dfa_stats_p->num_uses += NUM_SSA_OPERANDS (stmt, SSA_OP_USE);
	  dfa_stats_p->num_vdefs += gimple_vdef (stmt) ? 1 : 0;
	  dfa_stats_p->num_vuses += gimple_vuse (stmt) ? 1 : 0;
	}
    }
}


/*---------------------------------------------------------------------------
			     Miscellaneous helpers
---------------------------------------------------------------------------*/

/* Lookup VAR UID in the default_defs hashtable and return the associated
   variable.  */

tree
ssa_default_def (struct function *fn, tree var)
{
  struct tree_decl_minimal ind;
  struct tree_ssa_name in;
  gcc_assert (VAR_P (var)
	      || TREE_CODE (var) == PARM_DECL
	      || TREE_CODE (var) == RESULT_DECL);

  /* Always NULL_TREE for rtl function dumps.  */
  if (!fn->gimple_df)
    return NULL_TREE;

  in.var = (tree)&ind;
  ind.uid = DECL_UID (var);
  return DEFAULT_DEFS (fn)->find_with_hash ((tree)&in, DECL_UID (var));
}

/* Insert the pair VAR's UID, DEF into the default_defs hashtable
   of function FN.  */

void
set_ssa_default_def (struct function *fn, tree var, tree def)
{
  struct tree_decl_minimal ind;
  struct tree_ssa_name in;

  gcc_assert (VAR_P (var)
	      || TREE_CODE (var) == PARM_DECL
	      || TREE_CODE (var) == RESULT_DECL);
  in.var = (tree)&ind;
  ind.uid = DECL_UID (var);
  if (!def)
    {
      tree *loc = DEFAULT_DEFS (fn)->find_slot_with_hash ((tree)&in,
							  DECL_UID (var),
							  NO_INSERT);
      if (loc)
	{
	  SSA_NAME_IS_DEFAULT_DEF (*(tree *)loc) = false;
	  DEFAULT_DEFS (fn)->clear_slot (loc);
	}
      return;
    }
  gcc_assert (TREE_CODE (def) == SSA_NAME && SSA_NAME_VAR (def) == var);
  tree *loc = DEFAULT_DEFS (fn)->find_slot_with_hash ((tree)&in,
						      DECL_UID (var), INSERT);

  /* Default definition might be changed by tail call optimization.  */
  if (*loc)
    SSA_NAME_IS_DEFAULT_DEF (*loc) = false;

   /* Mark DEF as the default definition for VAR.  */
  *loc = def;
  SSA_NAME_IS_DEFAULT_DEF (def) = true;
}

/* Retrieve or create a default definition for VAR.  */

tree
get_or_create_ssa_default_def (struct function *fn, tree var)
{
  tree ddef = ssa_default_def (fn, var);
  if (ddef == NULL_TREE)
    {
      ddef = make_ssa_name_fn (fn, var, gimple_build_nop ());
      set_ssa_default_def (fn, var, ddef);
    }
  return ddef;
}


/* If EXP is a handled component reference for a structure, return the
   base variable.  The access range is delimited by bit positions *POFFSET and
   *POFFSET + *PMAX_SIZE.  The access size is *PSIZE bits.  If either
   *PSIZE or *PMAX_SIZE is -1, they could not be determined.  If *PSIZE
   and *PMAX_SIZE are equal, the access is non-variable.  If *PREVERSE is
   true, the storage order of the reference is reversed.  */

tree
get_ref_base_and_extent (tree exp, HOST_WIDE_INT *poffset,
			 HOST_WIDE_INT *psize,
			 HOST_WIDE_INT *pmax_size,
			 bool *preverse)
{
  offset_int bitsize = -1;
  offset_int maxsize;
  tree size_tree = NULL_TREE;
  offset_int bit_offset = 0;
  bool seen_variable_array_ref = false;

  /* First get the final access size and the storage order from just the
     outermost expression.  */
  if (TREE_CODE (exp) == COMPONENT_REF)
    size_tree = DECL_SIZE (TREE_OPERAND (exp, 1));
  else if (TREE_CODE (exp) == BIT_FIELD_REF)
    size_tree = TREE_OPERAND (exp, 1);
  else if (!VOID_TYPE_P (TREE_TYPE (exp)))
    {
      machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
      if (mode == BLKmode)
	size_tree = TYPE_SIZE (TREE_TYPE (exp));
      else
	bitsize = int (GET_MODE_BITSIZE (mode));
    }
  if (size_tree != NULL_TREE
      && TREE_CODE (size_tree) == INTEGER_CST)
    bitsize = wi::to_offset (size_tree);

  *preverse = reverse_storage_order_for_component_p (exp);

  /* Initially, maxsize is the same as the accessed element size.
     In the following it will only grow (or become -1).  */
  maxsize = bitsize;

  /* Compute cumulative bit-offset for nested component-refs and array-refs,
     and find the ultimate containing object.  */
  while (1)
    {
      switch (TREE_CODE (exp))
	{
	case BIT_FIELD_REF:
	  bit_offset += wi::to_offset (TREE_OPERAND (exp, 2));
	  break;

	case COMPONENT_REF:
	  {
	    tree field = TREE_OPERAND (exp, 1);
	    tree this_offset = component_ref_field_offset (exp);

	    if (this_offset && TREE_CODE (this_offset) == INTEGER_CST)
	      {
		offset_int woffset = (wi::to_offset (this_offset)
				      << LOG2_BITS_PER_UNIT);
		woffset += wi::to_offset (DECL_FIELD_BIT_OFFSET (field));
		bit_offset += woffset;

		/* If we had seen a variable array ref already and we just
		   referenced the last field of a struct or a union member
		   then we have to adjust maxsize by the padding at the end
		   of our field.  */
		if (seen_variable_array_ref && maxsize != -1)
		  {
		    tree stype = TREE_TYPE (TREE_OPERAND (exp, 0));
		    tree next = DECL_CHAIN (field);
		    while (next && TREE_CODE (next) != FIELD_DECL)
		      next = DECL_CHAIN (next);
		    if (!next
			|| TREE_CODE (stype) != RECORD_TYPE)
		      {
			tree fsize = DECL_SIZE_UNIT (field);
			tree ssize = TYPE_SIZE_UNIT (stype);
			if (fsize == NULL
			    || TREE_CODE (fsize) != INTEGER_CST
			    || ssize == NULL
			    || TREE_CODE (ssize) != INTEGER_CST)
			  maxsize = -1;
			else
			  {
			    offset_int tem = (wi::to_offset (ssize)
					      - wi::to_offset (fsize));
			    tem <<= LOG2_BITS_PER_UNIT;
			    tem -= woffset;
			    maxsize += tem;
			  }
		      }
		  }
	      }
	    else
	      {
		tree csize = TYPE_SIZE (TREE_TYPE (TREE_OPERAND (exp, 0)));
		/* We need to adjust maxsize to the whole structure bitsize.
		   But we can subtract any constant offset seen so far,
		   because that would get us out of the structure otherwise.  */
		if (maxsize != -1
		    && csize
		    && TREE_CODE (csize) == INTEGER_CST)
		  maxsize = wi::to_offset (csize) - bit_offset;
		else
		  maxsize = -1;
	      }
	  }
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  {
	    tree index = TREE_OPERAND (exp, 1);
	    tree low_bound, unit_size;

	    /* If the resulting bit-offset is constant, track it.  */
	    if (TREE_CODE (index) == INTEGER_CST
		&& (low_bound = array_ref_low_bound (exp),
 		    TREE_CODE (low_bound) == INTEGER_CST)
		&& (unit_size = array_ref_element_size (exp),
		    TREE_CODE (unit_size) == INTEGER_CST))
	      {
		offset_int woffset
		  = wi::sext (wi::to_offset (index) - wi::to_offset (low_bound),
			      TYPE_PRECISION (TREE_TYPE (index)));
		woffset *= wi::to_offset (unit_size);
		woffset <<= LOG2_BITS_PER_UNIT;
		bit_offset += woffset;

		/* An array ref with a constant index up in the structure
		   hierarchy will constrain the size of any variable array ref
		   lower in the access hierarchy.  */
		seen_variable_array_ref = false;
	      }
	    else
	      {
		tree asize = TYPE_SIZE (TREE_TYPE (TREE_OPERAND (exp, 0)));
		/* We need to adjust maxsize to the whole array bitsize.
		   But we can subtract any constant offset seen so far,
		   because that would get us outside of the array otherwise.  */
		if (maxsize != -1
		    && asize
		    && TREE_CODE (asize) == INTEGER_CST)
		  maxsize = wi::to_offset (asize) - bit_offset;
		else
		  maxsize = -1;

		/* Remember that we have seen an array ref with a variable
		   index.  */
		seen_variable_array_ref = true;
	      }
	  }
	  break;

	case REALPART_EXPR:
	  break;

	case IMAGPART_EXPR:
	  bit_offset += bitsize;
	  break;

	case VIEW_CONVERT_EXPR:
	  break;

	case TARGET_MEM_REF:
	  /* Via the variable index or index2 we can reach the
	     whole object.  Still hand back the decl here.  */
	  if (TREE_CODE (TMR_BASE (exp)) == ADDR_EXPR
	      && (TMR_INDEX (exp) || TMR_INDEX2 (exp)))
	    {
	      exp = TREE_OPERAND (TMR_BASE (exp), 0);
	      bit_offset = 0;
	      maxsize = -1;
	      goto done;
	    }
	  /* Fallthru.  */
	case MEM_REF:
	  /* We need to deal with variable arrays ending structures such as
	     struct { int length; int a[1]; } x;           x.a[d]
	     struct { struct { int a; int b; } a[1]; } x;  x.a[d].a
	     struct { struct { int a[1]; } a[1]; } x;      x.a[0][d], x.a[d][0]
	     struct { int len; union { int a[1]; struct X x; } u; } x; x.u.a[d]
	     where we do not know maxsize for variable index accesses to
	     the array.  The simplest way to conservatively deal with this
	     is to punt in the case that offset + maxsize reaches the
	     base type boundary.  This needs to include possible trailing
	     padding that is there for alignment purposes.  */
	  if (seen_variable_array_ref
	      && maxsize != -1
	      && (TYPE_SIZE (TREE_TYPE (exp)) == NULL_TREE
		  || TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) != INTEGER_CST
		  || (bit_offset + maxsize
		      == wi::to_offset (TYPE_SIZE (TREE_TYPE (exp))))))
	    maxsize = -1;

	  /* Hand back the decl for MEM[&decl, off].  */
	  if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR)
	    {
	      if (integer_zerop (TREE_OPERAND (exp, 1)))
		exp = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
	      else
		{
		  offset_int off = mem_ref_offset (exp);
		  off <<= LOG2_BITS_PER_UNIT;
		  off += bit_offset;
		  if (wi::fits_shwi_p (off))
		    {
		      bit_offset = off;
		      exp = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
		    }
		}
	    }
	  goto done;

	default:
	  goto done;
	}

      exp = TREE_OPERAND (exp, 0);
    }

 done:
  if (!wi::fits_shwi_p (bitsize) || wi::neg_p (bitsize))
    {
      *poffset = 0;
      *psize = -1;
      *pmax_size = -1;

      return exp;
    }

  *psize = bitsize.to_shwi ();

  if (!wi::fits_shwi_p (bit_offset))
    {
      *poffset = 0;
      *pmax_size = -1;

      return exp;
    }

  /* In case of a decl or constant base object we can do better.  */

  if (DECL_P (exp))
    {
      if (flag_unconstrained_commons && VAR_P (exp) && DECL_COMMON (exp))
	{
	  tree sz_tree = TYPE_SIZE (TREE_TYPE (exp));
	  /* If size is unknown, or we have read to the end, assume there
	     may be more to the structure than we are told.  */
	  if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE
	      || (seen_variable_array_ref
		  && (sz_tree == NULL_TREE
		      || TREE_CODE (sz_tree) != INTEGER_CST
		      || (bit_offset + maxsize == wi::to_offset (sz_tree)))))
	    maxsize = -1;
	}
      /* If maxsize is unknown adjust it according to the size of the
         base decl.  */
      else if (maxsize == -1
	  && DECL_SIZE (exp)
	  && TREE_CODE (DECL_SIZE (exp)) == INTEGER_CST)
	maxsize = wi::to_offset (DECL_SIZE (exp)) - bit_offset;
    }
  else if (CONSTANT_CLASS_P (exp))
    {
      /* If maxsize is unknown adjust it according to the size of the
         base type constant.  */
      if (maxsize == -1
	  && TYPE_SIZE (TREE_TYPE (exp))
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) == INTEGER_CST)
	maxsize = (wi::to_offset (TYPE_SIZE (TREE_TYPE (exp)))
		   - bit_offset);
    }

  /* ???  Due to negative offsets in ARRAY_REF we can end up with
     negative bit_offset here.  We might want to store a zero offset
     in this case.  */
  *poffset = bit_offset.to_shwi ();
  if (!wi::fits_shwi_p (maxsize) || wi::neg_p (maxsize))
    *pmax_size = -1;
  else
    {
      *pmax_size = maxsize.to_shwi ();
      if (*poffset > HOST_WIDE_INT_MAX - *pmax_size)
	*pmax_size = -1;
    }

  /* Punt if *POFFSET + *PSIZE overflows in HOST_WIDE_INT, the callers don't
     check for such overflows individually and assume it works.  */
  if (*psize != -1 && *poffset > HOST_WIDE_INT_MAX - *psize)
    {
      *poffset = 0;
      *psize = -1;
      *pmax_size = -1;

      return exp;
    }

  return exp;
}

/* Returns the base object and a constant BITS_PER_UNIT offset in *POFFSET that
   denotes the starting address of the memory access EXP.
   Returns NULL_TREE if the offset is not constant or any component
   is not BITS_PER_UNIT-aligned.
   VALUEIZE if non-NULL is used to valueize SSA names.  It should return
   its argument or a constant if the argument is known to be constant.  */

tree
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
		offset_int woffset
		  = wi::sext (wi::to_offset (index) - wi::to_offset (low_bound),
			      TYPE_PRECISION (TREE_TYPE (index)));
		woffset *= wi::to_offset (unit_size);
		byte_offset += woffset.to_shwi ();
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
		    offset_int off = mem_ref_offset (exp);
		    byte_offset += off.to_short_addr ();
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
		    offset_int off = mem_ref_offset (exp);
		    byte_offset += off.to_short_addr ();
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

/* Returns the base object and a constant BITS_PER_UNIT offset in *POFFSET that
   denotes the starting address of the memory access EXP.
   Returns NULL_TREE if the offset is not constant or any component
   is not BITS_PER_UNIT-aligned.  */

tree
get_addr_base_and_unit_offset (tree exp, HOST_WIDE_INT *poffset)
{
  return get_addr_base_and_unit_offset_1 (exp, poffset, NULL);
}

/* Returns true if STMT references an SSA_NAME that has
   SSA_NAME_OCCURS_IN_ABNORMAL_PHI set, otherwise false.  */

bool
stmt_references_abnormal_ssa_name (gimple *stmt)
{
  ssa_op_iter oi;
  use_operand_p use_p;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, oi, SSA_OP_USE)
    {
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (USE_FROM_PTR (use_p)))
	return true;
    }

  return false;
}

/* If STMT takes any abnormal PHI values as input, replace them with
   local copies.  */

void
replace_abnormal_ssa_names (gimple *stmt)
{
  ssa_op_iter oi;
  use_operand_p use_p;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, oi, SSA_OP_USE)
    {
      tree op = USE_FROM_PTR (use_p);
      if (TREE_CODE (op) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  tree new_name = make_ssa_name (TREE_TYPE (op));
	  gassign *assign = gimple_build_assign (new_name, op);
	  gsi_insert_before (&gsi, assign, GSI_SAME_STMT);
	  SET_USE (use_p, new_name);
	}
    }
}

/* Pair of tree and a sorting index, for dump_enumerated_decls.  */
struct GTY(()) numbered_tree
{
  tree t;
  int num;
};


/* Compare two declarations references by their DECL_UID / sequence number.
   Called via qsort.  */

static int
compare_decls_by_uid (const void *pa, const void *pb)
{
  const numbered_tree *nt_a = ((const numbered_tree *)pa);
  const numbered_tree *nt_b = ((const numbered_tree *)pb);

  if (DECL_UID (nt_a->t) != DECL_UID (nt_b->t))
    return  DECL_UID (nt_a->t) - DECL_UID (nt_b->t);
  return nt_a->num - nt_b->num;
}

/* Called via walk_gimple_stmt / walk_gimple_op by dump_enumerated_decls.  */
static tree
dump_enumerated_decls_push (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  vec<numbered_tree> *list = (vec<numbered_tree> *) wi->info;
  numbered_tree nt;

  if (!DECL_P (*tp))
    return NULL_TREE;
  nt.t = *tp;
  nt.num = list->length ();
  list->safe_push (nt);
  *walk_subtrees = 0;
  return NULL_TREE;
}

/* Find all the declarations used by the current function, sort them by uid,
   and emit the sorted list.  Each declaration is tagged with a sequence
   number indicating when it was found during statement / tree walking,
   so that TDF_NOUID comparisons of anonymous declarations are still
   meaningful.  Where a declaration was encountered more than once, we
   emit only the sequence number of the first encounter.
   FILE is the dump file where to output the list and FLAGS is as in
   print_generic_expr.  */
void
dump_enumerated_decls (FILE *file, dump_flags_t flags)
{
  basic_block bb;
  struct walk_stmt_info wi;
  auto_vec<numbered_tree, 40> decl_list;

  memset (&wi, '\0', sizeof (wi));
  wi.info = (void *) &decl_list;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	if (!is_gimple_debug (gsi_stmt (gsi)))
	  walk_gimple_stmt (&gsi, NULL, dump_enumerated_decls_push, &wi);
    }
  decl_list.qsort (compare_decls_by_uid);
  if (decl_list.length ())
    {
      unsigned ix;
      numbered_tree *ntp;
      tree last = NULL_TREE;

      fprintf (file, "Declarations used by %s, sorted by DECL_UID:\n",
	       current_function_name ());
      FOR_EACH_VEC_ELT (decl_list, ix, ntp)
	{
	  if (ntp->t == last)
	    continue;
	  fprintf (file, "%d: ", ntp->num);
	  print_generic_decl (file, ntp->t, flags);
	  fprintf (file, "\n");
	  last = ntp->t;
	}
    }
}
