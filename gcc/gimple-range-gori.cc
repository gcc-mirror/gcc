/* Gimple range GORI functions.
   Copyright (C) 2017-2025 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-range.h"

// Return TRUE if GS is a logical && or || expression.

static inline bool
is_gimple_logical_p (const gimple *gs)
{
  // Look for boolean and/or condition.
  if (is_gimple_assign (gs))
    switch (gimple_expr_code (gs))
      {
	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	  return true;

	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	  // Bitwise operations on single bits are logical too.
	  if (types_compatible_p (TREE_TYPE (gimple_assign_rhs1 (gs)),
				  boolean_type_node))
	    return true;
	  break;

	default:
	  break;
      }
  return false;
}

/* RANGE_DEF_CHAIN is used to determine which SSA names in a block can
   have range information calculated for them, and what the
   dependencies on each other are.

   Information for a basic block is calculated once and stored.  It is
   only calculated the first time a query is made, so if no queries
   are made, there is little overhead.

   The def_chain bitmap is indexed by SSA_NAME_VERSION.  Bits are set
   within this bitmap to indicate SSA names that are defined in the
   SAME block and used to calculate this SSA name.


    <bb 2> :
      _1 = x_4(D) + -2;
      _2 = _1 * 4;
      j_7 = foo ();
      q_5 = _2 + 3;
      if (q_5 <= 13)

    _1  : x_4(D)
    _2  : 1  x_4(D)
    q_5  : _1  _2  x_4(D)

    This dump indicates the bits set in the def_chain vector.
    as well as demonstrates the def_chain bits for the related ssa_names.

    Checking the chain for _2 indicates that _1 and x_4 are used in
    its evaluation.

    Def chains also only include statements which are valid gimple
    so a def chain will only span statements for which the range
    engine implements operations for.  */


// Construct a range_def_chain.

range_def_chain::range_def_chain ()
{
  bitmap_obstack_initialize (&m_bitmaps);
  m_def_chain.create (0);
  m_def_chain.safe_grow_cleared (num_ssa_names);
  m_logical_depth = 0;
}

// Destruct a range_def_chain.

range_def_chain::~range_def_chain ()
{
  m_def_chain.release ();
  bitmap_obstack_release (&m_bitmaps);
}

// Return true if NAME is in the def chain of DEF.  If BB is provided,
// only return true if the defining statement of DEF is in BB.

bool
range_def_chain::in_chain_p (tree name, tree def)
{
  gcc_checking_assert (gimple_range_ssa_p (def));
  gcc_checking_assert (gimple_range_ssa_p (name));

  // Get the definition chain for DEF.
  bitmap chain = get_def_chain (def);

  if (chain == NULL)
    return false;
  return bitmap_bit_p (chain, SSA_NAME_VERSION (name));
}

// Add either IMP or the import list B to the import set of DATA.

void
range_def_chain::set_import (struct rdc &data, tree imp, bitmap b)
{
  // If there are no imports, just return
  if (imp == NULL_TREE && !b)
    return;
  if (!data.m_import)
    data.m_import = BITMAP_ALLOC (&m_bitmaps);
  if (imp != NULL_TREE)
    bitmap_set_bit (data.m_import, SSA_NAME_VERSION (imp));
  else
    bitmap_ior_into (data.m_import, b);
}

// Return the import list for NAME.

bitmap
range_def_chain::get_imports (tree name)
{
  if (!has_def_chain (name))
    get_def_chain (name);
  bitmap i = m_def_chain[SSA_NAME_VERSION (name)].m_import;
  return i;
}

// Return true if IMPORT is an import to NAMEs def chain.

bool
range_def_chain::chain_import_p (tree name, tree import)
{
  bitmap b = get_imports (name);
  if (b)
    return bitmap_bit_p (b, SSA_NAME_VERSION (import));
  return false;
}

// Build def_chains for NAME if it is in BB.  Copy the def chain into RESULT.

void
range_def_chain::register_dependency (tree name, tree dep, basic_block bb)
{
  if (!gimple_range_ssa_p (dep))
    return;

  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_def_chain.length ())
    m_def_chain.safe_grow_cleared (num_ssa_names + 1);
  struct rdc &src = m_def_chain[v];
  gimple *def_stmt = SSA_NAME_DEF_STMT (dep);
  unsigned dep_v = SSA_NAME_VERSION (dep);
  bitmap b;

  // Set the direct dependency cache entries.
  if (!src.ssa1)
    src.ssa1 = SSA_NAME_VERSION (dep);
  else if (!src.ssa2 && src.ssa1 != SSA_NAME_VERSION (dep))
    src.ssa2 = SSA_NAME_VERSION (dep);

  // Don't calculate imports or export/dep chains if BB is not provided.
  // This is usually the case for when the temporal cache wants the direct
  // dependencies of a stmt.
  if (!bb)
    return;

  if (!src.bm)
    src.bm = BITMAP_ALLOC (&m_bitmaps);

  // Add this operand into the result.
  bitmap_set_bit (src.bm, dep_v);

  if (gimple_bb (def_stmt) == bb && !is_a<gphi *>(def_stmt))
    {
      // Get the def chain for the operand.
      b = get_def_chain (dep);
      // If there was one, copy it into result.  Access def_chain directly
      // as the get_def_chain request above could reallocate the vector.
      if (b)
	bitmap_ior_into (m_def_chain[v].bm, b);
      // And copy the import list.
      set_import (m_def_chain[v], NULL_TREE, get_imports (dep));
    }
  else
    // Originated outside the block, so it is an import.
    set_import (src, dep, NULL);
}

bool
range_def_chain::def_chain_in_bitmap_p (tree name, bitmap b)
{
  bitmap a = get_def_chain (name);
  if (a && b)
    return bitmap_intersect_p (a, b);
  return false;
}

void
range_def_chain::add_def_chain_to_bitmap (bitmap b, tree name)
{
  bitmap r = get_def_chain (name);
  if (r)
    bitmap_ior_into (b, r);
}


// Return TRUE if NAME has been processed for a def_chain.

inline bool
range_def_chain::has_def_chain (tree name)
{
  // Ensure there is an entry in the internal vector.
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_def_chain.length ())
    m_def_chain.safe_grow_cleared (num_ssa_names + 1);
  return (m_def_chain[v].ssa1 != 0);
}



// Calculate the def chain for NAME and all of its dependent
// operands. Only using names in the same BB.  Return the bitmap of
// all names in the m_def_chain.  This only works for supported range
// statements.

bitmap
range_def_chain::get_def_chain (tree name)
{
  tree ssa[3];
  unsigned v = SSA_NAME_VERSION (name);

  // If it has already been processed, just return the cached value.
  if (has_def_chain (name) && m_def_chain[v].bm)
    return m_def_chain[v].bm;

  // No definition chain for default defs.
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    {
      // A Default def is always an import.
      set_import (m_def_chain[v], name, NULL);
      return NULL;
    }

  gimple *stmt = SSA_NAME_DEF_STMT (name);
  unsigned count = gimple_range_ssa_names (ssa, 3, stmt);
  if (count == 0)
    {
      // Stmts not understood or with no operands are always imports.
      set_import (m_def_chain[v], name, NULL);
      return NULL;
    }

  // Terminate the def chains if we see too many cascading stmts.
  if (m_logical_depth == param_ranger_logical_depth)
    return NULL;

  // Increase the depth if we have a pair of ssa-names.
  if (count > 1)
    m_logical_depth++;

  for (unsigned x = 0; x < count; x++)
    register_dependency (name, ssa[x], gimple_bb (stmt));

  if (count > 1)
    m_logical_depth--;

  return m_def_chain[v].bm;
}

// Dump what we know for basic block BB to file F.

void
range_def_chain::dump (FILE *f, basic_block bb, const char *prefix)
{
  unsigned x, y;
  bitmap_iterator bi;

  // Dump the def chain for each SSA_NAME defined in BB.
  for (x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (!name)
	continue;
      gimple *stmt = SSA_NAME_DEF_STMT (name);
      if (!stmt || (bb && gimple_bb (stmt) != bb))
	continue;
      bitmap chain = (has_def_chain (name) ? get_def_chain (name) : NULL);
      if (chain && !bitmap_empty_p (chain))
	{
	  fprintf (f, prefix);
	  print_generic_expr (f, name, TDF_SLIM);
	  fprintf (f, " : ");

	  bitmap imports = get_imports (name);
	  EXECUTE_IF_SET_IN_BITMAP (chain, 0, y, bi)
	    {
	      print_generic_expr (f, ssa_name (y), TDF_SLIM);
	      if (imports && bitmap_bit_p (imports, y))
		fprintf (f, "(I)");
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }
}


// -------------------------------------------------------------------

/* GORI_MAP is used to accumulate what SSA names in a block can
   generate range information, and provides tools for the block ranger
   to enable it to efficiently calculate these ranges.

   GORI stands for "Generates Outgoing Range Information."

   It utilizes the range_def_chain class to construct def_chains.
   Information for a basic block is calculated once and stored.  It is
   only calculated the first time a query is made.  If no queries are
   made, there is little overhead.

   one bitmap is maintained for each basic block:
   m_outgoing  : a set bit indicates a range can be generated for a name.

   Generally speaking, the m_outgoing vector is the union of the
   entire def_chain of all SSA names used in the last statement of the
   block which generate ranges.  */


// Initialize a gori-map structure.

gori_map::gori_map ()
{
  m_outgoing.create (0);
  m_outgoing.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_incoming.create (0);
  m_incoming.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_maybe_variant = BITMAP_ALLOC (&m_bitmaps);
}

// Free any memory the GORI map allocated.

gori_map::~gori_map ()
{
  m_incoming.release ();
  m_outgoing.release ();
}

// Return the bitmap vector of all export from BB.  Calculate if necessary.

bitmap
gori_map::exports (basic_block bb)
{
  if (bb->index >= (signed int)m_outgoing.length () || !m_outgoing[bb->index])
    calculate_gori (bb);
  return m_outgoing[bb->index];
}

// Return the bitmap vector of all imports to BB.  Calculate if necessary.

bitmap
gori_map::imports (basic_block bb)
{
  if (bb->index >= (signed int)m_outgoing.length () || !m_outgoing[bb->index])
    calculate_gori (bb);
  return m_incoming[bb->index];
}

// Return true if NAME is can have ranges generated for it from basic
// block BB.

bool
gori_map::is_export_p (tree name, basic_block bb)
{
  // If no BB is specified, test if it is exported anywhere in the IL.
  if (!bb)
    return bitmap_bit_p (m_maybe_variant, SSA_NAME_VERSION (name));
  return bitmap_bit_p (exports (bb), SSA_NAME_VERSION (name));
}

// Set or clear the m_maybe_variant bit to determine if ranges will be tracked
// for NAME.  A clear bit means they will NOT be tracked.

void
gori_map::set_range_invariant (tree name, bool invariant)
{
  if (invariant)
    bitmap_clear_bit (m_maybe_variant, SSA_NAME_VERSION (name));
  else
    bitmap_set_bit (m_maybe_variant, SSA_NAME_VERSION (name));
}

// Return true if NAME is an import to block BB.

bool
gori_map::is_import_p (tree name, basic_block bb)
{
  // If no BB is specified, test if it is exported anywhere in the IL.
  return bitmap_bit_p (imports (bb), SSA_NAME_VERSION (name));
}

// If NAME is non-NULL and defined in block BB, calculate the def
// chain and add it to m_outgoing.

void
gori_map::maybe_add_gori (tree name, basic_block bb)
{
  if (name)
    {
      // Check if there is a def chain, regardless of the block.
      add_def_chain_to_bitmap (m_outgoing[bb->index], name);
      // Check for any imports.
      bitmap imp = get_imports (name);
      // If there were imports, add them so we can recompute
      if (imp)
	bitmap_ior_into (m_incoming[bb->index], imp);
      // This name is always an import.
      if (gimple_bb (SSA_NAME_DEF_STMT (name)) != bb)
	bitmap_set_bit (m_incoming[bb->index], SSA_NAME_VERSION (name));

      // Def chain doesn't include itself, and even if there isn't a
      // def chain, this name should be added to exports.
      bitmap_set_bit (m_outgoing[bb->index], SSA_NAME_VERSION (name));
    }
}

// Calculate all the required information for BB.

void
gori_map::calculate_gori (basic_block bb)
{
  tree name;
  if (bb->index >= (signed int)m_outgoing.length ())
    {
      m_outgoing.safe_grow_cleared (last_basic_block_for_fn (cfun));
      m_incoming.safe_grow_cleared (last_basic_block_for_fn (cfun));
    }
  gcc_checking_assert (m_outgoing[bb->index] == NULL);
  m_outgoing[bb->index] = BITMAP_ALLOC (&m_bitmaps);
  m_incoming[bb->index] = BITMAP_ALLOC (&m_bitmaps);

  if (single_succ_p (bb))
    return;

  // If this block's last statement may generate range information, go
  // calculate it.
  gimple *stmt = gimple_outgoing_range_stmt_p (bb);
  if (!stmt)
    return;
  if (is_a<gcond *> (stmt))
    {
      gcond *gc = as_a<gcond *>(stmt);
      name = gimple_range_ssa_p (gimple_cond_lhs (gc));
      maybe_add_gori (name, gimple_bb (stmt));

      name = gimple_range_ssa_p (gimple_cond_rhs (gc));
      maybe_add_gori (name, gimple_bb (stmt));
    }
  else
    {
      // Do not process switches if they are too large.
      if (EDGE_COUNT (bb->succs) > (unsigned)param_vrp_switch_limit)
	return;
      gswitch *gs = as_a<gswitch *>(stmt);
      name = gimple_range_ssa_p (gimple_switch_index (gs));
      maybe_add_gori (name, gimple_bb (stmt));
    }
  // Add this bitmap to the aggregate list of all outgoing names.
  bitmap_ior_into (m_maybe_variant, m_outgoing[bb->index]);
}

// Dump the table information for BB to file F.

void
gori_map::dump (FILE *f, basic_block bb, bool verbose)
{
  // BB was not processed.
  if (!m_outgoing[bb->index] || bitmap_empty_p (m_outgoing[bb->index]))
    return;

  tree name;

  bitmap imp = imports (bb);
  if (!bitmap_empty_p (imp))
    {
      if (verbose)
	fprintf (f, "bb<%u> Imports: ",bb->index);
      else
	fprintf (f, "Imports: ");
      FOR_EACH_GORI_IMPORT_NAME (this, bb, name)
	{
	  print_generic_expr (f, name, TDF_SLIM);
	  fprintf (f, "  ");
	}
      fputc ('\n', f);
    }

  if (verbose)
    fprintf (f, "bb<%u> Exports: ",bb->index);
  else
    fprintf (f, "Exports: ");
  // Dump the export vector.
  FOR_EACH_GORI_EXPORT_NAME (this, bb, name)
    {
      print_generic_expr (f, name, TDF_SLIM);
      fprintf (f, "  ");
    }
  fputc ('\n', f);

  range_def_chain::dump (f, bb, "         ");
}

// Dump the entire GORI map structure to file F.

void
gori_map::dump (FILE *f)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    dump (f, bb);
}

DEBUG_FUNCTION void
debug (gori_map &g)
{
  g.dump (stderr);
}

// -------------------------------------------------------------------

// Construct a gori_compute object.

gori_compute::gori_compute (gori_map &map, int not_executable_flag,
			    int sw_max_edges)
  : gimple_outgoing_range (sw_max_edges), m_map (map), tracer ("GORI ")
{
  m_not_executable_flag = not_executable_flag;
  // Create a boolean_type true and false range.
  m_bool_zero = range_false ();
  m_bool_one = range_true ();
  if (dump_file && (param_ranger_debug & RANGER_DEBUG_GORI))
    tracer.enable_trace ();

  // Reduce maximum recompute depth based on the size of the CFG to avoid
  // excessive compuations in large CFGs.
  m_recompute_depth = (int) param_ranger_recompute_depth
		      - (int) last_basic_block_for_fn (cfun) / 4096;
  if (m_recompute_depth < 1)
    m_recompute_depth = 1;
}

gori_compute::~gori_compute ()
{
}

// Given the switch S, return an evaluation in R for NAME when the lhs
// evaluates to LHS.  Returning false means the name being looked for
// was not resolvable.

bool
gori_compute::compute_operand_range_switch (vrange &r, gswitch *s,
					    const vrange &lhs,
					    tree name, fur_source &src)
{
  tree op1 = gimple_switch_index (s);

  // If name matches, the range is simply the range from the edge.
  // Empty ranges are viral as they are on a path which isn't
  // executable.
  if (op1 == name || lhs.undefined_p ())
    {
      r = lhs;
      return true;
    }

  // If op1 is in the definition chain, pass lhs back.
  if (gimple_range_ssa_p (op1) && m_map.in_chain_p (name, op1))
    return compute_operand_range (r, SSA_NAME_DEF_STMT (op1), lhs, name, src);

  return false;
}


// Return an evaluation for NAME as it would appear in STMT when the
// statement's lhs evaluates to LHS.  If successful, return TRUE and
// store the evaluation in R, otherwise return FALSE.

bool
gori_compute::compute_operand_range (vrange &r, gimple *stmt,
				     const vrange &lhs, tree name,
				     fur_source &src, value_relation *rel)
{
  value_relation vrel;
  value_relation *vrel_ptr = rel;
  // Empty ranges are viral as they are on an unexecutable path.
  if (lhs.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  if (is_a<gswitch *> (stmt))
    return compute_operand_range_switch (r, as_a<gswitch *> (stmt), lhs, name,
					 src);
  gimple_range_op_handler handler (stmt);
  if (!handler)
    return false;

  tree op1 = gimple_range_ssa_p (handler.operand1 ());
  tree op2 = gimple_range_ssa_p (handler.operand2 ());

  // If there is a relation betwen op1 and op2, use it instead as it is
  // likely to be more applicable.
  if (op1 && op2)
    {
      value_range r1, r2;
      r1.set_varying (TREE_TYPE (op1));
      r2.set_varying (TREE_TYPE (op2));
      relation_kind k = handler.op1_op2_relation (lhs, r1, r2);
      if (k != VREL_VARYING)
	{
	  vrel.set_relation (k, op1, op2);
	  vrel_ptr = &vrel;
	}
    }

  // Handle end of lookup first.
  if (op1 == name)
    return compute_operand1_range (r, handler, lhs, src, vrel_ptr);
  if (op2 == name)
    return compute_operand2_range (r, handler, lhs, src, vrel_ptr);

  // NAME is not in this stmt, but one of the names in it ought to be
  // derived from it.
  bool op1_in_chain = op1 && m_map.in_chain_p (name, op1);
  bool op2_in_chain = op2 && m_map.in_chain_p (name, op2);

  // If neither operand is derived, then this stmt tells us nothing.
  if (!op1_in_chain && !op2_in_chain)
    return false;

  // If either operand is in the def chain of the other (or they are equal), it
  // will be evaluated twice and can result in an exponential time calculation.
  // Instead just evaluate the one operand.
  if (op1_in_chain && op2_in_chain)
    {
      if (m_map.in_chain_p (op1, op2) || op1 == op2)
	op1_in_chain = false;
      else if (m_map.in_chain_p (op2, op1))
	op2_in_chain = false;
    }

  bool res = false;
  // If the lhs doesn't tell us anything only a relation can possibly enhance
  // the result.
  if (lhs.varying_p ())
    {
      if (!vrel_ptr)
	return false;
      // If there is a relation (ie: x != y) , it can only be relevant if
      // a) both elements are in the defchain
      //    c = x > y   // (x and y are in c's defchain)
      if (op1_in_chain)
	res = m_map.in_chain_p (vrel_ptr->op1 (), op1)
	      && m_map.in_chain_p (vrel_ptr->op2 (), op1);
      if (!res && op2_in_chain)
	res = m_map.in_chain_p (vrel_ptr->op1 (), op2)
	      || m_map.in_chain_p (vrel_ptr->op2 (), op2);
      if (!res)
	{
	  // or b) one relation element is in the defchain of the other and the
	  //       other is the LHS of this stmt.
	  //  x = y + 2
	  if (vrel_ptr->op1 () == handler.lhs ()
	      && (vrel_ptr->op2 () == op1 || vrel_ptr->op2 () == op2))
	    res = true;
	  else if (vrel_ptr->op2 () == handler.lhs ()
		   && (vrel_ptr->op1 () == op1 || vrel_ptr->op1 () == op2))
	    res = true;
	}
      if (!res)
	return false;
    }

  // Process logicals as they have special handling.
  if (is_gimple_logical_p (stmt))
    {
      // If the lhs doesn't tell us anything, neither will combining operands.
      if (lhs.varying_p ())
	return false;

      unsigned idx;
      if ((idx = tracer.header ("compute_operand ")))
	{
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, " with LHS = ");
	  lhs.dump (dump_file);
	  fprintf (dump_file, " at stmt ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}

      tree type = TREE_TYPE (name);
      value_range op1_trange (type), op1_frange (type);
      value_range op2_trange (type), op2_frange (type);
      compute_logical_operands (op1_trange, op1_frange, handler,
				as_a <irange> (lhs),
				name, src, op1, op1_in_chain);
      compute_logical_operands (op2_trange, op2_frange, handler,
				as_a <irange> (lhs),
				name, src, op2, op2_in_chain);
      res = logical_combine (r,
			     gimple_expr_code (stmt),
			     as_a <irange> (lhs),
			     op1_trange, op1_frange, op2_trange, op2_frange);
      if (idx)
	tracer.trailer (idx, "compute_operand", res, name, r);
      return res;
    }
  // Follow the appropriate operands now.
  if (op1_in_chain && op2_in_chain)
    return compute_operand1_and_operand2_range (r, handler, lhs, name, src,
						vrel_ptr);
  value_range vr;
  gimple *src_stmt;
  if (op1_in_chain)
    {
      vr.set_type (TREE_TYPE (op1));
      if (!compute_operand1_range (vr, handler, lhs, src, vrel_ptr))
	return false;
      src_stmt = SSA_NAME_DEF_STMT (op1);
    }
  else
    {
      gcc_checking_assert (op2_in_chain);
      vr.set_type (TREE_TYPE (op2));
      if (!compute_operand2_range (vr, handler, lhs, src, vrel_ptr))
	return false;
      src_stmt = SSA_NAME_DEF_STMT (op2);
    }

  gcc_checking_assert (src_stmt);
  // Then feed this range back as the LHS of the defining statement.
  return compute_operand_range (r, src_stmt, vr, name, src, vrel_ptr);
  // If neither operand is derived, this statement tells us nothing.
}


// Return TRUE if range R is either a true or false compatible range.

static bool
range_is_either_true_or_false (const irange &r)
{
  if (r.undefined_p ())
    return false;

  // This is complicated by the fact that Ada has multi-bit booleans,
  // so true can be ~[0, 0] (i.e. [1,MAX]).
  tree type = r.type ();
  gcc_checking_assert (range_compatible_p (type, boolean_type_node));
  return (r.singleton_p ()
	  || !r.contains_p (wi::zero (TYPE_PRECISION (type))));
}

// Evaluate a binary logical expression by combining the true and
// false ranges for each of the operands based on the result value in
// the LHS.

bool
gori_compute::logical_combine (vrange &r, enum tree_code code,
			       const irange &lhs,
			       const vrange &op1_true, const vrange &op1_false,
			       const vrange &op2_true, const vrange &op2_false)
{
  if (op1_true.varying_p () && op1_false.varying_p ()
      && op2_true.varying_p () && op2_false.varying_p ())
    return false;

  unsigned idx;
  if ((idx = tracer.header ("logical_combine")))
    {
      switch (code)
        {
	  case TRUTH_OR_EXPR:
	  case BIT_IOR_EXPR:
	    fprintf (dump_file, " || ");
	    break;
	  case TRUTH_AND_EXPR:
	  case BIT_AND_EXPR:
	    fprintf (dump_file, " && ");
	    break;
	  default:
	    break;
	}
      fprintf (dump_file, " with LHS = ");
      lhs.dump (dump_file);
      fputc ('\n', dump_file);

      tracer.print (idx, "op1_true = ");
      op1_true.dump (dump_file);
      fprintf (dump_file, "  op1_false = ");
      op1_false.dump (dump_file);
      fputc ('\n', dump_file);
      tracer.print (idx, "op2_true = ");
      op2_true.dump (dump_file);
      fprintf (dump_file, "  op2_false = ");
      op2_false.dump (dump_file);
      fputc ('\n', dump_file);
    }

  // This is not a simple fold of a logical expression, rather it
  // determines ranges which flow through the logical expression.
  //
  // Assuming x_8 is an unsigned char, and relational statements:
  //	      b_1 = x_8 < 20
  //	      b_2 = x_8 > 5
  // consider the logical expression and branch:
  //          c_2 = b_1 && b_2
  //          if (c_2)
  //
  // To determine the range of x_8 on either edge of the branch, one
  // must first determine what the range of x_8 is when the boolean
  // values of b_1 and b_2 are both true and false.
  //    b_1 TRUE      x_8 = [0, 19]
  //    b_1 FALSE     x_8 = [20, 255]
  //    b_2 TRUE      x_8 = [6, 255]
  //    b_2 FALSE     x_8 = [0,5].
  //
  // These ranges are then combined based on the expected outcome of
  // the branch.  The range on the TRUE side of the branch must satisfy
  //     b_1 == true && b_2 == true
  //
  // In terms of x_8, that means both x_8 == [0, 19] and x_8 = [6, 255]
  // must be true.  The range of x_8 on the true side must be the
  // intersection of both ranges since both must be true.  Thus the
  // range of x_8 on the true side is [6, 19].
  //
  // To determine the ranges on the FALSE side, all 3 combinations of
  // failing ranges must be considered, and combined as any of them
  // can cause the false result.
  //
  // If the LHS can be TRUE or FALSE, then evaluate both a TRUE and
  // FALSE results and combine them.  If we fell back to VARYING any
  // range restrictions that have been discovered up to this point
  // would be lost.
  if (!range_is_either_true_or_false (lhs))
    {
      bool res;
      value_range r1 (r);
      if (logical_combine (r1, code, m_bool_zero, op1_true, op1_false,
			   op2_true, op2_false)
	  && logical_combine (r, code, m_bool_one, op1_true, op1_false,
			      op2_true, op2_false))
	{
	  r.union_ (r1);
	  res = true;
	}
      else
	res = false;
      if (idx && res)
	{
	  tracer.print (idx, "logical_combine produced ");
	  r.dump (dump_file);
	  fputc ('\n', dump_file);
	}
      return res;
    }

  switch (code)
    {
      //  A logical AND combines ranges from 2 boolean conditions.
      //       c_2 = b_1 && b_2
      case TRUTH_AND_EXPR:
      case BIT_AND_EXPR:
        if (!lhs.zero_p ())
	  {
	    // The TRUE side is the intersection of the 2 true ranges.
	    r = op1_true;
	    r.intersect (op2_true);
	  }
	else
	  {
	    // The FALSE side is the union of the other 3 cases.
	    value_range ff (op1_false);
	    ff.intersect (op2_false);
	    value_range tf (op1_true);
	    tf.intersect (op2_false);
	    value_range ft (op1_false);
	    ft.intersect (op2_true);
	    r = ff;
	    r.union_ (tf);
	    r.union_ (ft);
	  }
        break;
      //  A logical OR combines ranges from 2 boolean conditions.
      // 	c_2 = b_1 || b_2
      case TRUTH_OR_EXPR:
      case BIT_IOR_EXPR:
        if (lhs.zero_p ())
	  {
	    // An OR operation will only take the FALSE path if both
	    // operands are false simultaneously, which means they should
	    // be intersected.  !(x || y) == !x && !y
	    r = op1_false;
	    r.intersect (op2_false);
	  }
	else
	  {
	    // The TRUE side of an OR operation will be the union of
	    // the other three combinations.
	    value_range tt (op1_true);
	    tt.intersect (op2_true);
	    value_range tf (op1_true);
	    tf.intersect (op2_false);
	    value_range ft (op1_false);
	    ft.intersect (op2_true);
	    r = tt;
	    r.union_ (tf);
	    r.union_ (ft);
	  }
	break;
      default:
        gcc_unreachable ();
    }

  if (idx)
    tracer.trailer (idx, "logical_combine", true, NULL_TREE, r);
  return true;
}


// Given a logical STMT, calculate true and false ranges for each
// potential path of NAME, assuming NAME came through the OP chain if
// OP_IN_CHAIN is true.

void
gori_compute::compute_logical_operands (vrange &true_range, vrange &false_range,
					gimple_range_op_handler &handler,
					const irange &lhs,
					tree name, fur_source &src,
					tree op, bool op_in_chain)
{
  gimple *stmt = handler.stmt ();
  gimple *src_stmt = gimple_range_ssa_p (op) ? SSA_NAME_DEF_STMT (op) : NULL;
  if (!op_in_chain || !src_stmt || m_map.chain_import_p (handler.lhs (), op))
    {
      // If op is not in the def chain, or defined in this block,
      // use its known value on entry to the block.
      src.get_operand (true_range, name);
      false_range = true_range;
      unsigned idx;
      if ((idx = tracer.header ("logical_operand")))
	{
	  print_generic_expr (dump_file, op, TDF_SLIM);
	  fprintf (dump_file, " not in computation chain. Queried.\n");
	  tracer.trailer (idx, "logical_operand", true, NULL_TREE, true_range);
        }
      return;
    }

  enum tree_code code = gimple_expr_code (stmt);
  // Optimize [0 = x | y], since neither operand can ever be non-zero.
  if ((code == BIT_IOR_EXPR || code == TRUTH_OR_EXPR) && lhs.zero_p ())
    {
      if (!compute_operand_range (false_range, src_stmt, m_bool_zero, name,
				  src))
	src.get_operand (false_range, name);
      true_range = false_range;
      return;
    }

  // Optimize [1 = x & y], since neither operand can ever be zero.
  if ((code == BIT_AND_EXPR || code == TRUTH_AND_EXPR) && lhs == m_bool_one)
    {
      if (!compute_operand_range (true_range, src_stmt, m_bool_one, name, src))
	src.get_operand (true_range, name);
      false_range = true_range;
      return;
    }

  // Calculate ranges for true and false on both sides, since the false
  // path is not always a simple inversion of the true side.
  if (!compute_operand_range (true_range, src_stmt, m_bool_one, name, src))
    src.get_operand (true_range, name);
  if (!compute_operand_range (false_range, src_stmt, m_bool_zero, name, src))
    src.get_operand (false_range, name);
}


// This routine will try to refine the ranges of OP1 and OP2 given a relation
// K between them.  In order to perform this refinement, one of the operands
// must be in the definition chain of the other.  The use is refined using
// op1/op2_range on the statement, and the definition is then recalculated
// using the relation.

bool
gori_compute::refine_using_relation (tree op1, vrange &op1_range,
			       tree op2, vrange &op2_range,
			       fur_source &src, relation_kind k)
{
  gcc_checking_assert (TREE_CODE (op1) == SSA_NAME);
  gcc_checking_assert (TREE_CODE (op2) == SSA_NAME);

  if (k == VREL_VARYING || k == VREL_EQ || k == VREL_UNDEFINED)
    return false;

  bool change = false;
  bool op1_def_p = m_map.in_chain_p (op2, op1);
  if (!op1_def_p)
    if (!m_map.in_chain_p (op1, op2))
      return false;

  tree def_op = op1_def_p ? op1 : op2;
  tree use_op = op1_def_p ? op2 : op1;

  if (!op1_def_p)
    k = relation_swap (k);

  // op1_def is true if we want to look up op1, otherwise we want op2.
  // if neither is the case, we returned in the above check.

  gimple *def_stmt = SSA_NAME_DEF_STMT (def_op);
  gimple_range_op_handler op_handler (def_stmt);
  if (!op_handler)
    return false;
  tree def_op1 = op_handler.operand1 ();
  tree def_op2 = op_handler.operand2 ();
  // if the def isn't binary, the relation will not be useful.
  if (!def_op2)
    return false;

  // Determine if op2 is directly referenced as an operand.
  if (def_op1 == use_op)
    {
      // def_stmt has op1 in the 1st operand position.
      value_range other_op (TREE_TYPE (def_op2));
      src.get_operand (other_op, def_op2);

      // Using op1_range as the LHS, and relation REL, evaluate op2.
      tree type = TREE_TYPE (def_op1);
      value_range new_result (type);
      if (!op_handler.op1_range (new_result, type,
				 op1_def_p ? op1_range : op2_range,
				 other_op, relation_trio::lhs_op1 (k)))
	return false;
      if (op1_def_p)
	{
	  change |= op2_range.intersect (new_result);
	  // Recalculate op2.
	  if (op_handler.fold_range (new_result, type, op2_range, other_op))
	    {
	      change |= op1_range.intersect (new_result);
	    }
	}
      else
	{
	  change |= op1_range.intersect (new_result);
	  // Recalculate op1.
	  if (op_handler.fold_range (new_result, type, op1_range, other_op))
	    {
	      change |= op2_range.intersect (new_result);
	    }
	}
    }
  else if (def_op2 == use_op)
    {
      // def_stmt has op1 in the 1st operand position.
      value_range other_op (TREE_TYPE (def_op1));
      src.get_operand (other_op, def_op1);

      // Using op1_range as the LHS, and relation REL, evaluate op2.
      tree type = TREE_TYPE (def_op2);
      value_range new_result (type);
      if (!op_handler.op2_range (new_result, type,
				 op1_def_p ? op1_range : op2_range,
				 other_op, relation_trio::lhs_op2 (k)))
	return false;
      if (op1_def_p)
	{
	  change |= op2_range.intersect (new_result);
	  // Recalculate op1.
	  if (op_handler.fold_range (new_result, type, other_op, op2_range))
	    {
	      change |= op1_range.intersect (new_result);
	    }
	}
      else
	{
	  change |= op1_range.intersect (new_result);
	  // Recalculate op2.
	  if (op_handler.fold_range (new_result, type, other_op, op1_range))
	    {
	      change |= op2_range.intersect (new_result);
	    }
	}
    }
  return change;
}

// Calculate a range for NAME from the operand 1 position of STMT
// assuming the result of the statement is LHS.  Return the range in
// R, or false if no range could be calculated.

bool
gori_compute::compute_operand1_range (vrange &r,
				      gimple_range_op_handler &handler,
				      const vrange &lhs,
				      fur_source &src, value_relation *rel)
{
  gimple *stmt = handler.stmt ();
  tree op1 = handler.operand1 ();
  tree op2 = handler.operand2 ();
  tree lhs_name = gimple_get_lhs (stmt);

  relation_trio trio;
  if (rel)
    trio = rel->create_trio (lhs_name, op1, op2);

  value_range op1_range (TREE_TYPE (op1));
  value_range op2_range (op2 ? TREE_TYPE (op2) : TREE_TYPE (op1));

  // Fetch the known range for op1 in this block.
  src.get_operand (op1_range, op1);

  // Now range-op calculate and put that result in r.
  if (op2)
    {
      src.get_operand (op2_range, op2);

      relation_kind op_op = trio.op1_op2 ();
      if (op_op != VREL_VARYING)
	refine_using_relation (op1, op1_range, op2, op2_range, src, op_op);

      // If op1 == op2, create a new trio for just this call.
      if (op1 == op2 && gimple_range_ssa_p (op1))
	trio = relation_trio (trio.lhs_op1 (), trio.lhs_op2 (), VREL_EQ);
      if (!handler.calc_op1 (r, lhs, op2_range, trio))
	return false;
    }
  else
    {
      // We pass op1_range to the unary operation.  Normally it's a
      // hidden range_for_type parameter, but sometimes having the
      // actual range can result in better information.
      if (!handler.calc_op1 (r, lhs, op1_range, trio))
	return false;
    }

  unsigned idx;
  if ((idx = tracer.header ("compute op 1 (")))
    {
      print_generic_expr (dump_file, op1, TDF_SLIM);
      fprintf (dump_file, ") at ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      tracer.print (idx, "LHS =");
      lhs.dump (dump_file);
      if (op2 && TREE_CODE (op2) == SSA_NAME)
	{
	  fprintf (dump_file, ", ");
	  print_generic_expr (dump_file, op2, TDF_SLIM);
	  fprintf (dump_file, " = ");
	  op2_range.dump (dump_file);
	}
      fprintf (dump_file, "\n");
      tracer.print (idx, "Computes ");
      print_generic_expr (dump_file, op1, TDF_SLIM);
      fprintf (dump_file, " = ");
      r.dump (dump_file);
      fprintf (dump_file, " intersect Known range : ");
      op1_range.dump (dump_file);
      fputc ('\n', dump_file);
    }

  r.intersect (op1_range);
  if (idx)
    tracer.trailer (idx, "produces ", true, op1, r);
  return true;
}


// Calculate a range for NAME from the operand 2 position of S
// assuming the result of the statement is LHS.  Return the range in
// R, or false if no range could be calculated.

bool
gori_compute::compute_operand2_range (vrange &r,
				      gimple_range_op_handler &handler,
				      const vrange &lhs,
				      fur_source &src, value_relation *rel)
{
  gimple *stmt = handler.stmt ();
  tree op1 = handler.operand1 ();
  tree op2 = handler.operand2 ();
  tree lhs_name = gimple_get_lhs (stmt);

  value_range op1_range (TREE_TYPE (op1));
  value_range op2_range (TREE_TYPE (op2));

  src.get_operand (op1_range, op1);
  src.get_operand (op2_range, op2);

  relation_trio trio;
  if (rel)
    trio = rel->create_trio (lhs_name, op1, op2);
  relation_kind op_op = trio.op1_op2 ();

  if (op_op != VREL_VARYING)
    refine_using_relation (op1, op1_range, op2, op2_range, src, op_op);

  // If op1 == op2, create a new trio for this stmt.
  if (op1 == op2 && gimple_range_ssa_p (op1))
    trio = relation_trio (trio.lhs_op1 (), trio.lhs_op2 (), VREL_EQ);
  // Intersect with range for op2 based on lhs and op1.
  if (!handler.calc_op2 (r, lhs, op1_range, trio))
    return false;

  unsigned idx;
  if ((idx = tracer.header ("compute op 2 (")))
    {
      print_generic_expr (dump_file, op2, TDF_SLIM);
      fprintf (dump_file, ") at ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      tracer.print (idx, "LHS = ");
      lhs.dump (dump_file);
      if (TREE_CODE (op1) == SSA_NAME)
	{
	  fprintf (dump_file, ", ");
	  print_generic_expr (dump_file, op1, TDF_SLIM);
	  fprintf (dump_file, " = ");
	  op1_range.dump (dump_file);
	}
      fprintf (dump_file, "\n");
      tracer.print (idx, "Computes ");
      print_generic_expr (dump_file, op2, TDF_SLIM);
      fprintf (dump_file, " = ");
      r.dump (dump_file);
      fprintf (dump_file, " intersect Known range : ");
      op2_range.dump (dump_file);
      fputc ('\n', dump_file);
    }
  // Intersect the calculated result with the known result and return if done.
  r.intersect (op2_range);
  if (idx)
    tracer.trailer (idx, " produces ", true, op2, r);
  return true;
}

// Calculate a range for NAME from both operand positions of S
// assuming the result of the statement is LHS.  Return the range in
// R, or false if no range could be calculated.

bool
gori_compute::compute_operand1_and_operand2_range (vrange &r,
						   gimple_range_op_handler
								     &handler,
						   const vrange &lhs,
						   tree name,
						   fur_source &src,
						   value_relation *rel)
{
  value_range op_range (TREE_TYPE (name));

  value_range vr (TREE_TYPE (handler.operand2 ()));
  // Calculate a good a range through op2.
  if (!compute_operand2_range (vr, handler, lhs, src, rel))
    return false;
  gimple *src_stmt = SSA_NAME_DEF_STMT (handler.operand2 ());
  gcc_checking_assert (src_stmt);
  // Then feed this range back as the LHS of the defining statement.
  if (!compute_operand_range (r, src_stmt, vr, name, src, rel))
    return false;

  // Now get the range thru op1.
  vr.set_type (TREE_TYPE (handler.operand1 ()));
  if (!compute_operand1_range (vr, handler, lhs, src, rel))
    return false;
  src_stmt = SSA_NAME_DEF_STMT (handler.operand1 ());
  gcc_checking_assert (src_stmt);
  // Then feed this range back as the LHS of the defining statement.
  if (!compute_operand_range (op_range, src_stmt, vr, name, src, rel))
    return false;

  // Both operands have to be simultaneously true, so perform an intersection.
  r.intersect (op_range);
  return true;
}

// Return TRUE if NAME can be recomputed on any edge exiting BB.  If any
// direct dependent is exported, it may also change the computed value of NAME.

bool
gori_compute::may_recompute_p (tree name, basic_block bb, int depth)
{
  tree dep1 = m_map.depend1 (name);
  tree dep2 = m_map.depend2 (name);

  // If the first dependency is not set, there is no recomputation.
  // Dependencies reflect original IL, not current state.   Check if the
  // SSA_NAME is still valid as well.
  if (!dep1)
    return false;

  // Don't recalculate PHIs or statements with side_effects.
  gimple *s = SSA_NAME_DEF_STMT (name);
  if (is_a<gphi *> (s) || gimple_has_side_effects (s))
    return false;

  if (!dep2)
    {
      // -1 indicates a default param, convert it to the real default.
      if (depth == -1)
	depth = m_recompute_depth;

      bool res = m_map.is_export_p (dep1, bb);
      if (res || depth <= 1)
	return res;
      // Check another level of recomputation.
      return may_recompute_p (dep1, bb, --depth);
    }
  // Two dependencies terminate the depth of the search.
  return m_map.is_export_p (dep1, bb) || m_map.is_export_p (dep2, bb);
}

// Return TRUE if NAME can be recomputed on edge E.  If any direct dependent
// is exported on edge E, it may change the computed value of NAME.

bool
gori_compute::may_recompute_p (tree name, edge e, int depth)
{
  gcc_checking_assert (e);
  return may_recompute_p (name, e->src, depth);
}


// Return TRUE if a range can be calculated or recomputed for NAME on any
// edge exiting BB.

bool
gori_compute::has_edge_range_p (tree name, basic_block bb)
{
  // Check if NAME is an export or can be recomputed.
  if (bb)
    return m_map.is_export_p (name, bb) || may_recompute_p (name, bb);

  // If no block is specified, check for anywhere in the IL.
  return m_map.is_export_p (name) || may_recompute_p (name);
}

// Return TRUE if a range can be calculated or recomputed for NAME on edge E.

bool
gori_compute::has_edge_range_p (tree name, edge e)
{
  gcc_checking_assert (e);
  return has_edge_range_p (name, e->src);
}

// Calculate a range on edge E and return it in R.  Try to evaluate a
// range for NAME on this edge.  Return FALSE if this is either not a
// control edge or NAME is not defined by this edge.

bool
gori_compute::edge_range_p (vrange &r, edge e, tree name, range_query &q)
{
  unsigned idx;

  if ((e->flags & m_not_executable_flag))
    {
      r.set_undefined ();
      if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Outgoing edge %d->%d unexecutable.\n",
		   e->src->index, e->dest->index);
      return true;
    }

  gcc_checking_assert (gimple_range_ssa_p (name));
  int_range_max lhs;
  // Determine if there is an outgoing edge.
  gimple *stmt = gimple_outgoing_range::edge_range_p (lhs, e);
  if (!stmt)
    return false;

  fur_stmt src (stmt, &q);
  // If NAME can be calculated on the edge, use that.
  if (m_map.is_export_p (name, e->src))
    {
      bool res;
      if ((idx = tracer.header ("outgoing_edge")))
	{
	  fprintf (dump_file, " for ");
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, " on edge %d->%d\n",
		   e->src->index, e->dest->index);
	}
      if ((res = compute_operand_range (r, stmt, lhs, name, src)))
	{
	  // Sometimes compatible types get interchanged. See PR97360.
	  // Make sure we are returning the type of the thing we asked for.
	  if (!r.undefined_p () && r.type () != TREE_TYPE (name))
	    {
	      gcc_checking_assert (range_compatible_p (r.type (),
						       TREE_TYPE (name)));
	      range_cast (r, TREE_TYPE (name));
	    }
	}
      if (idx)
	tracer.trailer (idx, "outgoing_edge", res, name, r);
      return res;
    }
  // If NAME isn't exported, check if it can be recomputed.
  else if (may_recompute_p (name, e))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);

      if ((idx = tracer.header ("recomputation")))
	{
	  fprintf (dump_file, " attempt on edge %d->%d for ",
		   e->src->index, e->dest->index);
	  print_gimple_stmt (dump_file, def_stmt, 0, TDF_SLIM);
	}
      // Simply calculate DEF_STMT on edge E using the range query Q.
      fold_range (r, def_stmt, e, &q);
      if (idx)
	tracer.trailer (idx, "recomputation", true, name, r);
      return true;
    }
  return false;
}

// Dump what is known to GORI computes to listing file F.

void
gori_compute::dump (FILE *f)
{
  m_map.gori_map::dump (f);
}

// ------------------------------------------------------------------------
//  GORI iterator.  Although we have bitmap iterators, don't expose that it
//  is currently a bitmap.  Use an export iterator to hide future changes.

// Construct a basic iterator over an export bitmap.

gori_export_iterator::gori_export_iterator (bitmap b)
{
  bm = b;
  if (b)
    bmp_iter_set_init (&bi, b, 1, &y);
}


// Move to the next export bitmap spot.

void
gori_export_iterator::next ()
{
  bmp_iter_next (&bi, &y);
}


// Fetch the name of the next export in the export list.  Return NULL if
// iteration is done.

tree
gori_export_iterator::get_name ()
{
  if (!bm)
    return NULL_TREE;

  while (bmp_iter_set (&bi, &y))
    {
      tree t = ssa_name (y);
      if (t)
	return t;
      next ();
    }
  return NULL_TREE;
}

// This is a helper class to set up STMT with a known LHS for further GORI
// processing.

class gori_stmt_info : public gimple_range_op_handler
{
public:
  gori_stmt_info (vrange &lhs, gimple *stmt, range_query *q);
  value_range op1_range;
  value_range op2_range;
  tree ssa1;
  tree ssa2;
};


// Uses query Q to get the known ranges on STMT with a LHS range
// for op1_range and op2_range and set ssa1 and ssa2 if either or both of
// those operands are SSA_NAMES.

gori_stmt_info::gori_stmt_info (vrange &lhs, gimple *stmt, range_query *q)
  : gimple_range_op_handler (stmt)
{
  ssa1 = NULL;
  ssa2 = NULL;
  // Don't handle switches as yet for vector processing.
  if (is_a<gswitch *> (stmt))
    return;

  // No frther processing for VARYING or undefined.
  if (lhs.undefined_p () || lhs.varying_p ())
    return;

  // If there is no range-op handler, we are also done.
  if (!*this)
    return;

  // Only evaluate logical cases if both operands must be the same as the LHS.
  // Otherwise its becomes exponential in time, as well as more complicated.
  if (is_gimple_logical_p (stmt))
    {
      gcc_checking_assert (range_compatible_p (lhs.type (), boolean_type_node));
      enum tree_code code = gimple_expr_code (stmt);
      if (code == TRUTH_OR_EXPR ||  code == BIT_IOR_EXPR)
	{
	  // [0, 0] = x || y  means both x and y must be zero.
	  if (!lhs.singleton_p () || !lhs.zero_p ())
	    return;
	}
      else if (code == TRUTH_AND_EXPR ||  code == BIT_AND_EXPR)
	{
	  // [1, 1] = x && y  means both x and y must be one.
	  if (!lhs.singleton_p () || lhs.zero_p ())
	    return;
	}
    }

  tree op1 = operand1 ();
  tree op2 = operand2 ();
  ssa1 = gimple_range_ssa_p (op1);
  ssa2 = gimple_range_ssa_p (op2);
  // If both operands are the same, only process one of them.
  if (ssa1 && ssa1 == ssa2)
    ssa2 = NULL_TREE;

  // Extract current ranges for the operands.
  fur_stmt src (stmt, q);
  if (op1)
    {
      op1_range.set_type (TREE_TYPE (op1));
      src.get_operand (op1_range, op1);
    }

  // And satisfy the second operand for single op satements.
  if (op2)
    {
      op2_range.set_type (TREE_TYPE (op2));
      src.get_operand (op2_range, op2);
    }
  else if (op1)
    op2_range = op1_range;
  return;
}


// Process STMT using LHS as the range of the LHS. Invoke GORI processing
// to resolve ranges for all SSA_NAMES feeding STMT which may be altered
// based on LHS.  Fill R with the results, and resolve all incoming
// ranges using range-query Q.

static void
gori_calc_operands (vrange &lhs, gimple *stmt, ssa_cache &r, range_query *q)
{
  struct gori_stmt_info si(lhs, stmt, q);
  if (!si)
    return;

  value_range tmp;
  // Now evaluate operand ranges, and set them in the edge cache.
  // If there was already a range, leave it and do no further evaluation.
  if (si.ssa1 && !r.has_range (si.ssa1))
    {
      tmp.set_type (TREE_TYPE (si.ssa1));
      if (si.calc_op1 (tmp, lhs, si.op2_range))
	si.op1_range.intersect (tmp);
      if (!si.op1_range.varying_p ())
	{
	  r.set_range (si.ssa1, si.op1_range);
	  gimple *src = SSA_NAME_DEF_STMT (si.ssa1);
	  // If defintion is in the same basic lock, evaluate it.
	  if (src && gimple_bb (src) == gimple_bb (stmt))
	    gori_calc_operands (si.op1_range, src, r, q);
	}
    }

  if (si.ssa2 && !r.has_range (si.ssa2))
    {
      tmp.set_type (TREE_TYPE (si.ssa2));
      if (si.calc_op2 (tmp, lhs, si.op1_range))
	si.op2_range.intersect (tmp);
      if (!si.op2_range.varying_p ())
	{
	  r.set_range (si.ssa2, si.op2_range);
	  gimple *src = SSA_NAME_DEF_STMT (si.ssa2);
	  if (src && gimple_bb (src) == gimple_bb (stmt))
	    gori_calc_operands (si.op2_range, src, r, q);
	}
    }
}

// Use ssa_cache R as a repository for all outgoing ranges on edge E that
// can be calculated.  Use Q to establish starting edge ranges anbd to resolve
// operand values.  If Q is NULL use the current range
// query available to the system.

bool
gori_on_edge (ssa_cache &r, edge e, range_query *q)
{
  if (!q)
    q = get_range_query (cfun);
  // Start with an empty vector
  r.clear ();
  int_range_max lhs;
  // Determine if there is an outgoing edge.
  gimple *stmt = q->gori ().edge_range_p (lhs, e);
  if (!stmt)
    return false;
  gori_calc_operands (lhs, stmt, r, q);
  return true;
}

// Helper for GORI_NAME_ON_EDGE which uses query Q to determine if STMT
// provides a range for NAME, and returns it in R if so. If it does not,
// continue processing feeding statments until we run out of statements
// or fine a range for NAME.

bool
gori_name_helper (vrange &r, tree name, vrange &lhs, gimple *stmt,
		  range_query *q)
{
  struct gori_stmt_info si(lhs, stmt, q);
  if (!si)
    return false;

  if (si.ssa1 == name)
    return si.calc_op1 (r, lhs, si.op2_range);
  if (si.ssa2 == name)
    return si.calc_op2 (r, lhs, si.op1_range);

  value_range tmp;
  // Now evaluate operand ranges, and set them in the edge cache.
  // If there was already a range, leave it and do no further evaluation.
  if (si.ssa1)
    {
      tmp.set_type (TREE_TYPE (si.ssa1));
      if (si.calc_op1 (tmp, lhs, si.op2_range))
	si.op1_range.intersect (tmp);
      gimple *src = SSA_NAME_DEF_STMT (si.ssa1);
      // If defintion is in the same basic lock, evaluate it.
      if (src && gimple_bb (src) == gimple_bb (stmt))
	if (gori_name_helper (r, name, si.op1_range, src, q))
	  return true;
    }

  if (si.ssa2)
    {
      tmp.set_type (TREE_TYPE (si.ssa2));
      if (si.calc_op2 (tmp, lhs, si.op1_range))
	si.op2_range.intersect (tmp);
      gimple *src = SSA_NAME_DEF_STMT (si.ssa2);
      if (src && gimple_bb (src) == gimple_bb (stmt))
	if (gori_name_helper (r, name, si.op2_range, src, q))
	  return true;
    }
  return false;
}

// Check if NAME has an outgoing range on edge E.  Use query Q to evaluate
// the operands.  Return TRUE and the range in R if there is an outgoing range.
// This is like gori_on_edge except it only looks for the single name and
// does not require an ssa_cache.

bool
gori_name_on_edge (vrange &r, tree name, edge e, range_query *q)
{
  int_range_max lhs;
  gimple *stmt = gimple_outgoing_range_stmt_p (e->src);
  if (!stmt || !is_a<gcond *> (stmt))
    return false;
  gcond_edge_range (lhs, e);
  return gori_name_helper (r, name, lhs, stmt, q);
}
