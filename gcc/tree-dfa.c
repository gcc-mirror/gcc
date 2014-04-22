/* Data flow functions for trees.
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
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
#include "tm.h"
#include "hashtab.h"
#include "tree.h"
#include "stor-layout.h"
#include "tm_p.h"
#include "basic-block.h"
#include "langhooks.h"
#include "flags.h"
#include "function.h"
#include "tree-pretty-print.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "params.h"

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
	  gimple stmt = gsi_stmt (bsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
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
	  gimple stmt = gsi_stmt (bsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
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

  size = dfa_stats.num_phis * sizeof (struct gimple_statement_phi);
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
      gimple_stmt_iterator si;

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple phi = gsi_stmt (si);
	  dfa_stats_p->num_phis++;
	  dfa_stats_p->num_phi_args += gimple_phi_num_args (phi);
	  if (gimple_phi_num_args (phi) > dfa_stats_p->max_num_phi_args)
	    dfa_stats_p->max_num_phi_args = gimple_phi_num_args (phi);
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple stmt = gsi_stmt (si);
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
  gcc_assert (TREE_CODE (var) == VAR_DECL
	      || TREE_CODE (var) == PARM_DECL
	      || TREE_CODE (var) == RESULT_DECL);
  in.var = (tree)&ind;
  ind.uid = DECL_UID (var);
  return (tree) htab_find_with_hash (DEFAULT_DEFS (fn), &in, DECL_UID (var));
}

/* Insert the pair VAR's UID, DEF into the default_defs hashtable
   of function FN.  */

void
set_ssa_default_def (struct function *fn, tree var, tree def)
{
  struct tree_decl_minimal ind;
  struct tree_ssa_name in;
  void **loc;

  gcc_assert (TREE_CODE (var) == VAR_DECL
	      || TREE_CODE (var) == PARM_DECL
	      || TREE_CODE (var) == RESULT_DECL);
  in.var = (tree)&ind;
  ind.uid = DECL_UID (var);
  if (!def)
    {
      loc = htab_find_slot_with_hash (DEFAULT_DEFS (fn), &in,
				      DECL_UID (var), NO_INSERT);
      if (loc)
	{
	  SSA_NAME_IS_DEFAULT_DEF (*(tree *)loc) = false;
	  htab_clear_slot (DEFAULT_DEFS (fn), loc);
	}
      return;
    }
  gcc_assert (TREE_CODE (def) == SSA_NAME && SSA_NAME_VAR (def) == var);
  loc = htab_find_slot_with_hash (DEFAULT_DEFS (fn), &in,
                                  DECL_UID (var), INSERT);

  /* Default definition might be changed by tail call optimization.  */
  if (*loc)
    SSA_NAME_IS_DEFAULT_DEF (*(tree *) loc) = false;

   /* Mark DEF as the default definition for VAR.  */
  *(tree *) loc = def;
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
   and *PMAX_SIZE are equal, the access is non-variable.  */

tree
get_ref_base_and_extent (tree exp, HOST_WIDE_INT *poffset,
			 HOST_WIDE_INT *psize,
			 HOST_WIDE_INT *pmax_size)
{
  double_int bitsize = double_int_minus_one;
  double_int maxsize;
  tree size_tree = NULL_TREE;
  double_int bit_offset = double_int_zero;
  bool seen_variable_array_ref = false;

  /* First get the final access size from just the outermost expression.  */
  if (TREE_CODE (exp) == COMPONENT_REF)
    size_tree = DECL_SIZE (TREE_OPERAND (exp, 1));
  else if (TREE_CODE (exp) == BIT_FIELD_REF)
    size_tree = TREE_OPERAND (exp, 1);
  else if (!VOID_TYPE_P (TREE_TYPE (exp)))
    {
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
      if (mode == BLKmode)
	size_tree = TYPE_SIZE (TREE_TYPE (exp));
      else
	bitsize = double_int::from_uhwi (GET_MODE_BITSIZE (mode));
    }
  if (size_tree != NULL_TREE
      && TREE_CODE (size_tree) == INTEGER_CST)
    bitsize = tree_to_double_int (size_tree);

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
	  bit_offset += tree_to_double_int (TREE_OPERAND (exp, 2));
	  break;

	case COMPONENT_REF:
	  {
	    tree field = TREE_OPERAND (exp, 1);
	    tree this_offset = component_ref_field_offset (exp);

	    if (this_offset && TREE_CODE (this_offset) == INTEGER_CST)
	      {
		double_int doffset = tree_to_double_int (this_offset);
		doffset = doffset.lshift (BITS_PER_UNIT == 8
					  ? 3 : exact_log2 (BITS_PER_UNIT));
		doffset += tree_to_double_int (DECL_FIELD_BIT_OFFSET (field));
		bit_offset = bit_offset + doffset;

		/* If we had seen a variable array ref already and we just
		   referenced the last field of a struct or a union member
		   then we have to adjust maxsize by the padding at the end
		   of our field.  */
		if (seen_variable_array_ref && !maxsize.is_minus_one ())
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
			  maxsize = double_int_minus_one;
			else
			  {
			    double_int tem = tree_to_double_int (ssize)
					     - tree_to_double_int (fsize);
			    if (BITS_PER_UNIT == 8)
			      tem = tem.lshift (3);
			    else
			      tem *= double_int::from_uhwi (BITS_PER_UNIT);
			    tem -= doffset;
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
		if (!maxsize.is_minus_one ()
		    && csize
		    && TREE_CODE (csize) == INTEGER_CST)
		  maxsize = tree_to_double_int (csize) - bit_offset;
		else
		  maxsize = double_int_minus_one;
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
		double_int doffset
		  = (TREE_INT_CST (index) - TREE_INT_CST (low_bound))
		    .sext (TYPE_PRECISION (TREE_TYPE (index)));
		doffset *= tree_to_double_int (unit_size);
		doffset = doffset.lshift (BITS_PER_UNIT == 8
					  ? 3 : exact_log2 (BITS_PER_UNIT));
		bit_offset = bit_offset + doffset;

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
		if (!maxsize.is_minus_one ()
		    && asize
		    && TREE_CODE (asize) == INTEGER_CST)
		  maxsize = tree_to_double_int (asize) - bit_offset;
		else
		  maxsize = double_int_minus_one;

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
	      bit_offset = double_int_zero;
	      maxsize = double_int_minus_one;
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
	      && !maxsize.is_minus_one ()
	      && (TYPE_SIZE (TREE_TYPE (exp)) == NULL_TREE
		  || TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) != INTEGER_CST
		  || (bit_offset + maxsize
		      == tree_to_double_int (TYPE_SIZE (TREE_TYPE (exp))))))
	    maxsize = double_int_minus_one;

	  /* Hand back the decl for MEM[&decl, off].  */
	  if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR)
	    {
	      if (integer_zerop (TREE_OPERAND (exp, 1)))
		exp = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
	      else
		{
		  double_int off = mem_ref_offset (exp);
		  off = off.lshift (BITS_PER_UNIT == 8
				    ? 3 : exact_log2 (BITS_PER_UNIT));
		  off += bit_offset;
		  if (off.fits_shwi ())
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

  /* We need to deal with variable arrays ending structures.  */
  if (seen_variable_array_ref
      && !maxsize.is_minus_one ()
      && (TYPE_SIZE (TREE_TYPE (exp)) == NULL_TREE
	  || TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) != INTEGER_CST
	  || (bit_offset + maxsize
	      == tree_to_double_int (TYPE_SIZE (TREE_TYPE (exp))))))
    maxsize = double_int_minus_one;

 done:
  if (!bitsize.fits_shwi () || bitsize.is_negative ())
    {
      *poffset = 0;
      *psize = -1;
      *pmax_size = -1;

      return exp;
    }

  *psize = bitsize.to_shwi ();

  if (!bit_offset.fits_shwi ())
    {
      *poffset = 0;
      *pmax_size = -1;

      return exp;
    }

  /* In case of a decl or constant base object we can do better.  */

  if (DECL_P (exp))
    {
      /* If maxsize is unknown adjust it according to the size of the
         base decl.  */
      if (maxsize.is_minus_one ()
	  && DECL_SIZE (exp)
	  && TREE_CODE (DECL_SIZE (exp)) == INTEGER_CST)
	maxsize = tree_to_double_int (DECL_SIZE (exp)) - bit_offset;
    }
  else if (CONSTANT_CLASS_P (exp))
    {
      /* If maxsize is unknown adjust it according to the size of the
         base type constant.  */
      if (maxsize.is_minus_one ()
	  && TYPE_SIZE (TREE_TYPE (exp))
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) == INTEGER_CST)
	maxsize = tree_to_double_int (TYPE_SIZE (TREE_TYPE (exp)))
		  - bit_offset;
    }

  /* ???  Due to negative offsets in ARRAY_REF we can end up with
     negative bit_offset here.  We might want to store a zero offset
     in this case.  */
  *poffset = bit_offset.to_shwi ();
  if (!maxsize.fits_shwi () || maxsize.is_negative ())
    *pmax_size = -1;
  else
    *pmax_size = maxsize.to_shwi ();

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
stmt_references_abnormal_ssa_name (gimple stmt)
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

/* Pair of tree and a sorting index, for dump_enumerated_decls.  */
struct GTY(()) numbered_tree_d
{
  tree t;
  int num;
};
typedef struct numbered_tree_d numbered_tree;


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
dump_enumerated_decls (FILE *file, int flags)
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
