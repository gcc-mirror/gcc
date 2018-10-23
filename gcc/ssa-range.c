/* On-demand ssa range generator.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "domwalk.h"
#include "ssa-range.h"
#include "ssa-range-global.h"

// Class used to track non-null references of an ssa-name
// A vector of bitmaps indexed by ssa-name is maintained. When indexed by 
// Basic Block, an on-bit indicates there is a non-null dereference for
// that ssa_name in that basic block.
//
// There is a single API: no_null_deref_p(name, bb) which takes care of making
// sure its a pointer type and allocated the bitmap and populating it if needed.
//
// In order to populate the bitmap, a quick run of all the immediate uses
// are made and the statement checkd to see if a non-null dereference is made
// on that statement.

class non_null_ref
{
public:
  non_null_ref ();
  ~non_null_ref ();
  bool non_null_deref_p (tree name, basic_block bb);
private:
  vec <bitmap> m_nn;
  void process_name (tree name);
};

// During contructor, Allocate the vector of ssa_names.
non_null_ref::non_null_ref ()
{
  m_nn.create (0);
  m_nn.safe_grow_cleared (num_ssa_names);
}

// Free any bitmaps which were allocated,a swell as the vector itself.
non_null_ref::~non_null_ref ()
{
  unsigned x;
  for (x = 0; x< m_nn.length (); x++)
    if (m_nn[x])
      BITMAP_FREE (m_nn[x]);
}

// Allocate an populate the bitmap for NAME.
void
non_null_ref::process_name (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  use_operand_p use_p;
  imm_use_iterator iter;
  bitmap b;

  // Only tracked for pointers.
  if (!POINTER_TYPE_P (TREE_TYPE (name)))
    return;

  // Already processed if a bitmap has been allocated.
  if (m_nn[v])
    return;

  b = BITMAP_ALLOC (NULL);

  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      gimple *s = USE_STMT (use_p);
      unsigned index = gimple_bb (s)->index;
      tree value;
      enum tree_code comp_code;

      // If bit is already set for this block, dont bother looking again.
      if (bitmap_bit_p (b, index))
        continue;

      // If we can infer a != 0 range, then set the bit for this BB
      if (infer_value_range (s, name, &comp_code, &value))
        {
	  if (comp_code == NE_EXPR && integer_zerop (value))
	    bitmap_set_bit (b, index);
	}
    }

  m_nn[v] = b;
}

// Return true if NAME has a non-null dereference in block bb.  If this is the
// first query for NAME, calculate the summary first.
bool
non_null_ref::non_null_deref_p (tree name, basic_block bb)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!POINTER_TYPE_P (TREE_TYPE (name)))
    return false;

  if (!m_nn[v])
    process_name (name);

  return bitmap_bit_p (m_nn[v], bb->index);
}


// This class implements a cache of ranges indexed by basic block.
// It represents all that is known about an SSA_NAME on entry to each block
// It caches a range-for-type ranger so it doesnt need to be reformed all
// the time.  If a range is ever associated with a type, we can use it
// instead,.
// Whenever range_for_type is being set for a block, the cache simply points
// to this cached one rather than create a new one each time.
class ssa_block_ranges
{
public:
  ssa_block_ranges (tree t);
  ~ssa_block_ranges ();

  void set_bb_range (const basic_block bb, const irange &r);
  void set_bb_range_for_type (const basic_block bb);
  bool get_bb_range (irange& r, const basic_block bb);
  bool bb_range_p (const basic_block bb);

  void dump(FILE *f);
private:
  vec<irange_storage *> m_tab;
  irange_storage *m_type_range;
  tree m_type;
};

// Initialize a block cache for an ssa_name of type T
ssa_block_ranges::ssa_block_ranges (tree t)
{
  irange tr;
  gcc_assert (TYPE_P (t));
  m_type = t;

  m_tab.create (0);
  m_tab.safe_grow_cleared (last_basic_block_for_fn (cfun));

  // Create the cached type range.
  tr.set_varying (t);
  m_type_range = irange_storage::ggc_alloc_init (tr);

  m_tab[ENTRY_BLOCK_PTR_FOR_FN (cfun)->index] = m_type_range;
}

// Destruct block range.
ssa_block_ranges::~ssa_block_ranges ()
{
  m_tab.release ();
}

// Set the range for block BB to be R.
void
ssa_block_ranges::set_bb_range (const basic_block bb, const irange& r)
{
  irange_storage *m = m_tab[bb->index];

  // If there is already range memory for this block, reuse it.
  if (m && m != m_type_range)
    m->set_irange (r);
  else
    m = irange_storage::ggc_alloc_init (r);

  m_tab[bb->index] = m;
}

// Set the range for block BB to the range for the type.
void
ssa_block_ranges::set_bb_range_for_type (const basic_block bb)
{
  m_tab[bb->index] = m_type_range;
}

// Return the range associated with block BB in R. Return false if there is no
// range,
bool
ssa_block_ranges::get_bb_range (irange& r, const basic_block bb)
{
  irange_storage *m = m_tab[bb->index];
  if (m)
    {
      r = irange (m_type, m);
      return true;
    }
  return false;
}


// Returns true if a range is present
bool
ssa_block_ranges::bb_range_p (const basic_block bb)
{
  return m_tab[bb->index] != NULL;
}

// Print the list of known ranges for file F in a nice format.
void
ssa_block_ranges::dump (FILE *f)
{
  basic_block bb;
  irange r;

  FOR_EACH_BB_FN (bb, cfun)
    if (get_bb_range (r, bb))
      {
	fprintf (f, "BB%d  -> ", bb->index);
	r.dump (f);
      }
}

// -------------------------------------------------------------------------

// This class manages a vector of pointers to ssa_black ranges.
// THis provides the basis for the "range on entry" cache for
// all ssa-names.
class block_range_cache
{
public:
  block_range_cache ();
  ~block_range_cache ();

  // Hide the details of the block cache with these wrappers
  void set_bb_range (tree name, const basic_block bb, const irange &r);
  void set_bb_range_for_type (tree name, const basic_block bb);
  bool get_bb_range (irange& r, tree name, const basic_block bb);
  bool bb_range_p (tree name, const basic_block bb);

  void dump (FILE *f);
private:
  vec<ssa_block_ranges *> m_ssa_ranges;
  ssa_block_ranges& get_block_ranges (tree name);
};

// Initialize the block cache.
block_range_cache::block_range_cache ()
{
  m_ssa_ranges.create (0);
  m_ssa_ranges.safe_grow_cleared (num_ssa_names);
}

// Remove any m_block_caches which have been created.
block_range_cache::~block_range_cache ()
{
  unsigned x;
  for (x = 0; x < m_ssa_ranges.length (); ++x)
    {
      if (m_ssa_ranges[x])
	delete m_ssa_ranges[x];
    }
  // Release the vector itself.
  m_ssa_ranges.release ();
}

// Return a reference to the m_block_cache for NAME. If it has not been accessed
// yet, allocate it.
ssa_block_ranges&
block_range_cache::get_block_ranges (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!m_ssa_ranges[v])
    m_ssa_ranges[v] = new ssa_block_ranges (TREE_TYPE (name));

  return *(m_ssa_ranges[v]);
}

void
block_range_cache::set_bb_range (tree name, const basic_block bb,
				 const irange &r)
{
  return get_block_ranges (name).set_bb_range (bb, r);
}

void
block_range_cache::set_bb_range_for_type (tree name, const basic_block bb)
{
  return get_block_ranges (name).set_bb_range_for_type (bb);
}

bool 
block_range_cache::get_bb_range (irange& r, tree name, const basic_block bb)
{
  return get_block_ranges (name).get_bb_range (r, bb);
}

bool 
block_range_cache::bb_range_p (tree name, const basic_block bb)
{
  return get_block_ranges (name).bb_range_p (bb);
}


// Print all known block caches to file F.
void
block_range_cache::dump (FILE *f)
{
  unsigned x;
  for (x = 0; x < num_ssa_names; ++x)
    {
      if (m_ssa_ranges[x])
        {
	  fprintf (f, " Ranges for ");
	  print_generic_expr (f, ssa_name (x), TDF_NONE);
	  fprintf (f, ":\n");
	  m_ssa_ranges[x]->dump (f);
	}
    }
  
}
// -------------------------------------------------------------------------

// Initialize a path_ranger.
path_ranger::path_ranger () 
{
  m_block_cache = new block_range_cache ();
  m_globals = new ssa_global_cache ();
  m_non_null = new non_null_ref ();
}

// Deallocate path_ranger members.
path_ranger::~path_ranger () 
{
  delete m_block_cache;
  delete m_globals;
  delete m_non_null;
}

// Print everything known about the global cache to file F.
inline void
path_ranger::dump_global_ssa_range (FILE *f)
{
  m_globals->dump (f);
}

// Return the global range for NAME in R, if it has one. Otherwise return false.
bool
path_ranger::has_global_ssa_range (irange& r, tree name)
{
  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);
  if (!supports_ssa_p (name))
    return false;

  if (m_globals->get_global_range (r, name))
    return true;
  return false;
}


// Return the global range for NAME in R. IF it does not have a range set,
// invoke the ranger and attempt to find one.  If none can be found, simply 
// set the global range to the default range.
bool
path_ranger::get_global_ssa_range (irange& r, tree name)
{
  gimple *s;
  gcc_checking_assert (valid_ssa_p (name));

  if (m_globals->get_global_range (r, name))
    return true;

  // No range set so try to evaluate the definition.
  s = SSA_NAME_DEF_STMT (name);
  if (s && path_range_stmt (r, s))
    {
      m_globals->set_global_range (name, r);
      return true;
    }

  // No good range determined, use default value.
  r = range_from_ssa (name);
  m_globals->set_global_range (name, r);
  return true;
}

// Set global range of NAME to R.
inline void
path_ranger::set_global_ssa_range (tree name, const irange&r)
{
  m_globals->set_global_range (name, r);
}

// clear any global range for NAME.
void
path_ranger::clear_global_ssa_range (tree name) 
{
  m_globals->clear_global_range (name);
}

// Return true if there is a non-null dereference of name in BB somewhere and
// return the non-null range in R.
inline bool
path_ranger::non_null_deref_in_block (irange &r, tree name, basic_block bb)
{
  tree type = TREE_TYPE (name);
  if (!POINTER_TYPE_P (type))
    return false;
  if (m_non_null->non_null_deref_p (name, bb))
    {
      range_non_zero (&r, type);
      return true;
    }
  return false;
}

// Make sure that the range-on-entry cache for NAME is set for block BB.
// Work back thourgh the CFG to DEF_BB ensuring the range is calculated 
// on the block/edges leading back to that point.
void
path_ranger::determine_block (tree name, basic_block bb, basic_block def_bb)
{
  edge_iterator ei;
  edge e;
  irange er;
  irange block_result;

  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
      || bb == def_bb)
    return;

  // If the block cache is set, then we've already visited this block.
  if (m_block_cache->bb_range_p (name, bb))
    return;

  // Avoid infinite recursion by marking this block as calculated now.
  m_block_cache->set_bb_range_for_type (name, bb);

  // Visit each predecessor to resolve them.
  FOR_EACH_EDGE (e, ei, bb->preds)
    determine_block (name, e->src, def_bb);

  // Start with an empty range.
  block_result.set_undefined (TREE_TYPE (name));
  // And union all the ranges on the incoming edges.
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      irange pred_range;
      basic_block src = e->src;
      // If this is the defintion BB, force evaluation of the intial value.
      if (src == def_bb)
        get_global_ssa_range (pred_range, name);
      else
        {
	  bool res;
	  // We know this has been evaluated, just get the range on entry to 
	  // the predecessor block.
	  res = m_block_cache->get_bb_range (pred_range, name, src);
	  gcc_assert (res);
	}

      // If there is no range yet, and the previous block has a pointer
      // dereference, adjust the range.
      if (pred_range.varying_p ())
        non_null_deref_in_block (pred_range, name, src);

      // If the predecessor block refines the range on the incoming edge,
      // apply it to the range on entry value from that block.
      if (range_on_edge_p (er, e, name))
        {
	  pred_range.intersect (er);
	  er.intersect (pred_range);
	}

      // Union the range on that edge with our accumulating range on all edges.
      block_result.union_ (pred_range);

      // If we get to range_for_type, we can stop looking.
      if (block_result.varying_p ())
        break;
    }

  if (block_result.varying_p ())
    m_block_cache->set_bb_range_for_type (name, bb);
  else
    m_block_cache->set_bb_range (name, bb, block_result);
}



// Determine a range on entry of NAME to block BB. DEF_BB is the block that
// name is defined in.  Return the range in R.  A rnge is always returned,
// even if it is range_for_type.
void
path_ranger::range_for_bb (irange &r, tree name, basic_block bb,
			   basic_block def_bb)
{
  bool res;
  determine_block (name, bb, def_bb);
  res = m_block_cache->get_bb_range (r, name, bb);
  gcc_assert (res);
}


// External API.  Return the range of NAME on entry to block BB in R.
// Return false if there is no range other than range_for_type.
bool
path_ranger::path_range_entry (irange& r, tree name, basic_block bb)
{
  gimple *def_stmt;
  basic_block def_bb = NULL;

  if (!supports_ssa_p (name))
    return false;

  // Determine an origination block for the defining statement.
  def_stmt = SSA_NAME_DEF_STMT (name);
  if (def_stmt)
    def_bb = gimple_bb (def_stmt);;

  if (!def_bb)
    def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  // Start with any known range. 
  get_global_ssa_range (r, name);

  // If its defined in this basic block, then there is no range on entry,
  // otherwise, go figure out what is known in predecessor blocks. 
  if (def_bb != bb)
    {
      irange block_range;
      range_for_bb (block_range, name, bb, def_bb);
      r.intersect (block_range);
    }

  return true;
}


// Return the range of NAME on edge E back through to NAME's def point. Return
// the range in R.  Return false if no range other than range_for_type is found.
bool
path_ranger::path_range_edge (irange& r, tree name, edge e)
{
  basic_block bb = e->src;

  if (!supports_ssa_p (name))
    return false;

  // Get an initial range for NAME. 
  gimple *stmt = SSA_NAME_DEF_STMT (name);

  // If it is in this block, evaluate it, otherwise pick it up from entry.
  if (stmt && gimple_bb (stmt) == e->src)
    {
      if (!path_range_stmt (r, stmt))
	r.set_varying (TREE_TYPE (name));
    }
  else
    if (!path_range_entry (r, name, bb))
      r.set_varying (TREE_TYPE (name));

  // If result is range for type, see if there is a non-null reference
  // somewhere in this block. 
  if (r.varying_p ())
    non_null_deref_in_block (r, name, bb);

  // Now intersect the initial range with what NAME evaluates to on this edge.
  irange edge_range;
  if (range_on_edge_p (edge_range, e, name))
    r.intersect (edge_range);
  return true;
}

// Return a range for the phi node PHI in R.
bool
path_ranger::process_phi (irange &r, gphi *phi)
{
  tree phi_def = gimple_phi_result (phi);
  irange phi_range;
  unsigned x;

  // Global cache has already been initialized in path_range_stmt
  if (!has_global_ssa_range (phi_range, phi_def))
    internal_error ("Must set global range cache before calling process_phi.");
 
  // And start with an empty range, unioning in each argument's range.
  r.set_undefined (TREE_TYPE (phi_def));
  for (x = 0; x < gimple_phi_num_args (phi); x++)
    {
      irange arg_range;
      tree arg = gimple_phi_arg_def (phi, x);
      edge e = gimple_phi_arg_edge (phi, x);
      // Try to find a calulated range, if that fails, just get the operands
      // range. if that fails, return false.
      if ((TREE_CODE (arg) != SSA_NAME) || !path_range_edge (arg_range, arg, e))
	if (!range_of_expr (arg_range, arg))
	  {
	    clear_global_ssa_range (phi_def);
	    return false;
	  }

      r.union_ (arg_range);
      // Once its range_for_type, stop looking.
      if (r.varying_p ())
	break;
    }

  // Intersect any global info we have about PHI_DEF with what was calculated.
  r.intersect (phi_range);
  // This now becomes the global range for PHI_DEF.
  set_global_ssa_range (phi_def, r);
  return true;
}


// External API. Evaluate statement G, and return the result in R.
// Return false if it cannot be evaluated, or would return range_for_type.
bool
path_ranger::path_range_stmt (irange& r, gimple *g)
{
  tree name;
  bool res;

  name = is_a<gphi *> (g) ? gimple_phi_result (g) : gimple_get_lhs (g);

  // Not all statements have a LHS.  */
  if (name)
    {
      if (!supports_ssa_p (name))
        return false;

      // If this STMT has already been processed, return that value. 
      if (has_global_ssa_range (r, name))
	return true;
     
      // avoid infinite recursion by initializing global cache
      r = range_from_ssa (name);
      set_global_ssa_range (name, r);
    }

  if (is_a<gcall *> (g))
    return range_of_stmt (r, as_a<gcall *>(g));
  if (is_a<gphi *> (g))
    return process_phi (r, as_a<gphi *>(g));
  
  grange_op *rn = dyn_cast<grange_op *>(g);
  if (!rn)
    return false;
  irange range1, range2;

  // Evaluate operand 1.
  if (!range_of_expr (range1, rn->operand1 (), g))
    return false;
    
  // If this is a unary operation, call fold now.  
  if (!rn->operand2 ())
    res = rn->fold (r, range1);
  else
    {
      // Evaluate the second operand.
      if (!range_of_expr (range2, rn->operand2 (), g))
	return false;
      res = rn->fold (r, range1, range2);
    }

  if (name)
    {
      // If there is a LHS and a result, set the global cache.
      // Clear it if there was no range.
      if (res)
	set_global_ssa_range (name, r);
      else
	clear_global_ssa_range (name);
    }
  return res;
}

// Determine a range for OP on stmt S, returning the result in R.
// If OP is not defined in BB, find the range on entry to this block.
bool
path_ranger::range_of_expr (irange&r, tree op, gimple *s)
{
  if (!supports_p (op))
     return false;

  // If there is a statement, and a valid ssa_name, try to find a range.
  if (s && valid_ssa_p (op))
    {
      basic_block bb = gimple_bb (s);
      gimple *def_stmt = SSA_NAME_DEF_STMT (op);
      // if name is defined in this block, try to get an range from S.
      if (def_stmt && gimple_bb (def_stmt) == bb)
	{
	  if (!path_range_stmt (r, def_stmt))
	    r.set_varying (TREE_TYPE (op));
	}
      else
	// Otherwise OP comes from outside this block, try range on entry.
	if (!path_range_entry (r, op, bb))
	  r.set_varying (TREE_TYPE (op));

      // No range yet, see if there is a dereference in the block.
      // We don't care if it's between the def and a use within a block
      // because the entire block must be executed anyway.
      // FIXME:?? For non-call exceptions we could have a statement throw
      // which causes an early block exit. 
      // in which case we may need to walk from S back to the def/top of block
      // to make sure the deref happens between S and there before claiming 
      // there is a deref.   
      if (r.varying_p ())
	non_null_deref_in_block (r, op, bb);
      return true;
    }

  // Fall back to the default.
  return gimple_range::range_of_expr (r, op);
}


// Print the known table values ro file F.
void
path_ranger::dump(FILE *f)
{
  block_ranger::dump (f);
  dump_global_ssa_range (f);
}


// Exercise the ranger by visiting every block and asking for the range of 
// every ssa_name on each outgoing edge, and optionally providing a dump
// of everything than can be calculated by the ranger to file OUTPUT.
void
path_ranger::exercise (FILE *output)
{

  basic_block bb;
  irange range;

  FOR_EACH_BB_FN (bb, cfun)
    {
      unsigned x;
      edge_iterator ei;
      edge e;
      bool printed = false;

      // This dramatically slows down builds, so only when printing. 
      if (output)
        {
	  fprintf (output, "========= BB%d =========\n", bb->index);
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = ssa_name (x);
	      if (name && path_range_entry (range, name, bb))
		{
		  if (output && !range.varying_p ())
		    {
		      // avoid printing global values that are just global
		      irange gl;
		      if (has_global_ssa_range (gl, name) && gl == range)
		        continue;
			  
		      if (!printed)
			fprintf (output,"   Ranges on Entry :\n");
		      printed = true;
		      fprintf (output, "     ");
		      print_generic_expr (output, name, TDF_NONE);
		      fprintf (output, " : ");
		      range.dump (output);
		    }
		}
	    }
	  if (printed)
	    fprintf (output, "\n");
	  dump_bb (output, bb, 2, TDF_NONE);
	  printed = false;
	}

      // Run through the block once to visit each value.
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = ssa_name (x);
	      if (name && range_p (bb, name))
		  path_range_edge (range, name, e);
	    }
	}
      // Now calculate any globals in this block if we asked for details
      if (dump_file && (dump_flags & TDF_DETAILS))
	for (x = 1; x < num_ssa_names; x++)
	  {
	    tree name = ssa_name (x);
	    if (name && TREE_CODE (name) == SSA_NAME 
		&& !SSA_NAME_IS_VIRTUAL_OPERAND (name)
		&& SSA_NAME_DEF_STMT (name) &&
		gimple_bb (SSA_NAME_DEF_STMT (name)) == bb &&
		!get_global_ssa_range (range, name))
	      path_range_stmt (range, SSA_NAME_DEF_STMT (name));
	  }

      if (output)
        {
	  // Dump any globals defined.
	  printed = false;
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      if (ssa_name(x) && has_global_ssa_range (range, ssa_name (x))
		  && !range.varying_p ())
	        {
		  gimple *s = SSA_NAME_DEF_STMT (ssa_name (x));
		  if (s && gimple_bb (s) == bb)
		    {
		      if (!printed)
			fprintf (output, "     -- Ranges Defined -- :\n");
		      fprintf (output, "     ");
		      print_generic_expr (output, ssa_name (x), TDF_NONE);
		      fprintf (output, "  : ");
		      range.dump (output);
		      printed = true;
		    }
		}
	      else
		// Check for a pointer and it is dereferences in this block
		// and it had no range coming in.
		if (ssa_name (x)
		    && m_non_null->non_null_deref_p (ssa_name(x), bb)
		    && !path_range_entry (range, ssa_name (x), bb))
		  {
		    if (!printed)
		      fprintf (output, "     -- Ranges Defined -- :\n");
		    fprintf (output, "     ");
		    print_generic_expr (output, ssa_name (x), TDF_NONE);
		    fprintf (output, "  : non-null due to deref in block\n");
		    printed = true;
		  }
	    }
	  if (printed)
	    fprintf (output, "\n");

	  // Now print any edge values.
          printed = false;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      for (x = 1; x < num_ssa_names; x++)
		{
		  tree name = ssa_name (x);
		  if (name && range_p (bb, name))
		    {
		      if (path_range_edge (range, name, e))
			{
			  if (!range.varying_p ())
			    {
			      printed = true;
			      fprintf (output, "     %d->%d ", e->src->index,
				       e->dest->index);
			      if (e->flags & EDGE_TRUE_VALUE)
				fprintf (output, " (T) ");
			      else if (e->flags & EDGE_FALSE_VALUE)
				fprintf (output, " (F) ");
			      else
				fprintf (output, "     ");
			      print_generic_expr (output, name, TDF_SLIM);
			      fprintf(output, "  \t");
			      range.dump(output);
			    }
			}
		    }
		}
	    }
	  if (printed)
	    fprintf (output, "\n");
	}
    }

  if (output && (dump_flags & TDF_DETAILS))
    dump (output);

}



