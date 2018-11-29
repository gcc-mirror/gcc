/* SSA range cache related functionalilty.
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
#include "ssa-range.h"
#include "ssa-range-gori.h"
#include "fold-const.h"

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

// Return true if NAME has a non-null dereference in block bb.  If this is the
// first query for NAME, calculate the summary first.

bool
non_null_ref::non_null_deref_p (tree name, basic_block bb)
{
  if (!POINTER_TYPE_P (TREE_TYPE (name)))
    return false;

  unsigned v = SSA_NAME_VERSION (name);
  if (!m_nn[v])
    process_name (name);

  return bitmap_bit_p (m_nn[v], bb->index);
}

// Allocate an populate the bitmap for NAME.  An ON bit for a block index
// indicates there is a non-null reference in that block.
// In order to populate the bitmap, a quick run of all the immediate uses
// are made and the statement checked to see if a non-null dereference is made
// on that statement.

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

  // Loop over each immediate use and see if it implies a non-null value.
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

// This class implements a cache of ranges indexed by basic block.
// It represents all that is known about an SSA_NAME on entry to each block
// It caches a range-for-type varying range so it doesnt need to be reformed all
// the time.  If a range is ever always associated with a type, we can use that
// instead,.
// Whenever varying is being set for a block, the cache simply points
// to this cached one rather than create a new one each time.

class ssa_block_ranges
{
public:
  ssa_block_ranges (tree t);
  ~ssa_block_ranges ();

  void set_bb_range (const basic_block bb, const irange &r);
  void set_bb_varying (const basic_block bb);
  bool get_bb_range (irange &r, const basic_block bb);
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
ssa_block_ranges::set_bb_range (const basic_block bb, const irange &r)
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
ssa_block_ranges::set_bb_varying (const basic_block bb)
{
  m_tab[bb->index] = m_type_range;
}

// Return the range associated with block BB in R. Return false if there is no
// range,

bool
ssa_block_ranges::get_bb_range (irange &r, const basic_block bb)
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

// Return a reference to the m_block_cache for NAME. If it has not been
// accessed yet, allocate it.

ssa_block_ranges&
block_range_cache::get_block_ranges (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!m_ssa_ranges[v])
    m_ssa_ranges[v] = new ssa_block_ranges (TREE_TYPE (name));

  return *(m_ssa_ranges[v]);
}

// Set the range for NAME on entry to block BB to R.

void
block_range_cache::set_bb_range (tree name, const basic_block bb,
				 const irange &r)
{
  return get_block_ranges (name).set_bb_range (bb, r);
}

// Set the range for NAME on entry to block BB to varying..

void
block_range_cache::set_bb_varying (tree name, const basic_block bb)
{
  return get_block_ranges (name).set_bb_varying (bb);
}

// Return the range for NAME on entry to BB in R.  return true if here is one.

bool 
block_range_cache::get_bb_range (irange &r, tree name, const basic_block bb)
{
  return get_block_ranges (name).get_bb_range (r, bb);
}

// Return true if NAME has a range set in block BB.

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

// Print all known ranges on entry to blobk BB to file F.
void
block_range_cache::dump (FILE *f, basic_block bb, bool print_varying)
{
  unsigned x;
  irange r;
  bool summarize_varying = false;
  for (x = 1; x < num_ssa_names; ++x)
    {
      if (!ssa_ranger::valid_ssa_p (ssa_name (x)))
        continue;
      if (m_ssa_ranges[x] && m_ssa_ranges[x]->get_bb_range (r, bb))
        {
	  if (!print_varying && r.varying_p ())
	    {
	      summarize_varying = true;
	      continue;
	    }
	  print_generic_expr (f, ssa_name (x), TDF_NONE);
	  fprintf (f, "\t");
	  r.dump(f);
	}
    }
  // If there were any varying entries, lump them all together.
  if (summarize_varying)
    {
      fprintf (f, "VARYING_P on entry : ");
      for (x = 1; x < num_ssa_names; ++x)
	{
	  if (!ssa_ranger::valid_ssa_p (ssa_name (x)))
	    continue;
	  if (m_ssa_ranges[x] && m_ssa_ranges[x]->get_bb_range (r, bb))
	    {
	      if (r.varying_p ())
		{
		  print_generic_expr (f, ssa_name (x), TDF_NONE);
		  fprintf (f, "  ");
	        }
	    }
	}
      fprintf (f, "\n");
    }
}
// -------------------------------------------------------------------------

// Initialize a global cache.

ssa_global_cache::ssa_global_cache ()
{
  m_tab.create (0);
  m_tab.safe_grow_cleared (num_ssa_names);
}

// Deconstruct a global cache.

ssa_global_cache::~ssa_global_cache ()
{
  m_tab.release ();
}

// Retrieve the global range of NAME from cache memory if it exists. 
// Return the value in R.

bool
ssa_global_cache::get_global_range (irange &r, tree name) const
{
  irange_storage *stow = m_tab[SSA_NAME_VERSION (name)];
  if (!stow)
    return false;
  r = irange (TREE_TYPE (name), stow);
  return true;
}

// Set the range for NAME to R in the glonbal cache.

void
ssa_global_cache::set_global_range (tree name, const irange& r)
{
  irange_storage *m = m_tab[SSA_NAME_VERSION (name)];

  if (m)
    m->set_irange (r);
  else
    {
      m = irange_storage::ggc_alloc_init (r);
      m_tab[SSA_NAME_VERSION (name)] = m;
    }
}

// Set the range for NAME to R in the glonbal cache.

void
ssa_global_cache::clear_global_range (tree name)
{
  m_tab[SSA_NAME_VERSION (name)] = NULL;
}

// Clear the global cache.

void
ssa_global_cache::clear ()
{
  memset (m_tab.address(), 0, m_tab.length () * sizeof (irange_storage *));
}

// Dump the contents of the global cache to F. 

void
ssa_global_cache::dump (FILE *f)
{
  unsigned x;
  irange r;
  fprintf (f, "Non-varying global ranges:\n");
  fprintf (f, "=========================:\n");
  for ( x = 1; x < num_ssa_names; x++)
    if (ssa_ranger::valid_ssa_p (ssa_name (x)) &&
	get_global_range (r, ssa_name (x))  && !r.varying_p ())
      {
        print_generic_expr (f, ssa_name (x), TDF_NONE);
	fprintf (f, "  : ");
        r.dump (f);
      }
  fputc ('\n', dump_file);
}


// COnstruct a gori_cache object.

gori_cache::gori_cache ()
{
  m_workback.create (0);
  m_workback.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.create (0);
  m_update_list.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.truncate (0);
}

// Destruct a gori_cache object.

gori_cache::~gori_cache ()
{
  m_workback.release ();
  m_update_list.release ();
}

// Return true if NAME has a non-null dereference in block BB.

bool
gori_cache::non_null_deref_p (tree name, basic_block bb)
{
  return m_non_null.non_null_deref_p (name, bb);
}

void
gori_cache::dump_block (FILE *f, basic_block bb)
{
  m_on_entry.dump (f, bb);
}


// Return a static range for NAMe on entry to basic block BB in R.
// If calc is true, Fill any cache entries required between BB and the Def 
// block for NAME.  Otherwise, return false if the cache is empty

bool
gori_cache::block_range (irange &r, basic_block bb, tree name, bool calc)
{

  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = NULL;

  gcc_checking_assert (ssa_ranger::valid_ssa_p (name));

  if (calc)
    {
      if (def_stmt)
	def_bb = gimple_bb (def_stmt);;
      if (!def_bb)
	def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
      
      // There is no range on entry for the defintion block.
      if (def_bb == bb)
	return false;

      // Otherwise, go figure out what is known in predecessor blocks.
      fill_block_cache (name, bb, def_bb);
      gcc_checking_assert (m_on_entry.bb_range_p (name, bb));
    }
  return m_on_entry.get_bb_range (r, name, bb);
}

// Check if the range for NAME on outgoing edge E may change the incoming 
// range for NAME at the destination.  IF it does, update that range and
// push the destination block onto the update list.

bool
gori_cache::maybe_propagate_on_edge (tree name, edge e)
{
  basic_block pred = e->src;
  basic_block succ = e->dest;
  irange block_range;
  irange edge_range;
  irange succ_range_on_entry;

  // Look for entry block, DEF block or a block will a filled cache.
  if (pred == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    get_tree_range (block_range, name);
  else if (pred == gimple_bb (SSA_NAME_DEF_STMT (name)))
    get_tree_range (block_range, name);
  else if (!m_on_entry.get_bb_range (block_range, name, pred))
    return false;

  // Otherwise pick up the edge range and intersect it with block.
  if (outgoing_edge_range_p (edge_range, e, name, &block_range))
    block_range = edge_range;

  // Check if pointers have any non-null dereferences.
  // Non-call exceptions mean we could throw in the middle of he block,
  // so just punt for now on those.
  if (block_range.varying_p () && !cfun->can_throw_non_call_exceptions &&
      m_non_null.non_null_deref_p (name, pred))
    range_non_zero (&block_range, TREE_TYPE (name));

  // Now pick up the current range on entry to the successor and see if
  // union with the edge range causes a change.
  gcc_assert (m_on_entry.get_bb_range (succ_range_on_entry, name, succ));
  irange new_range = range_union (succ_range_on_entry, block_range);

  if (new_range != succ_range_on_entry)
    {
      // If ranges are different, Set new range and mark succ to be visited.
      m_on_entry.set_bb_range (name, succ, new_range);
      if (!m_update_list.contains (succ))
	m_update_list.quick_push (succ);
    }
  return true;
}

// See if the cache for NAME may need updating on any outgoing edges of BB.

inline void
gori_cache::maybe_propagate_block (tree name, basic_block bb)
{
  edge_iterator ei;
  edge e;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      basic_block dest = e->dest;

      // Only look at edges where the succ has been visited.
      if (!m_on_entry.bb_range_p (name, dest))
	continue;

      // Anything in this list ought to have range_on_entry set and
      // thus return a TRUE.
      gcc_assert (maybe_propagate_on_edge (name, e));
    }
}

// If there is anything in the iterative update_list, continue processing NAME
// until the list of blocks is empty.  Note calls to maybe_propagate_block may
// add to the current list.

void
gori_cache::iterative_cache_update (tree name)
{
  // Now the update_list queue has all the blocks that have had their entry
  // ranges "updated" from undefined. Start propagating until nothing
  // changes.

  while (m_update_list.length () > 0)
    {
      basic_block bb = m_update_list.pop ();
// fprintf (stderr, "FWD visiting block %d\n", bb->index);
      maybe_propagate_block (name, bb);
    }
// fprintf (stderr, "DONE visiting blocks \n\n");
}

// Make sure that the range-on-entry cache for NAME is set for block BB.
// Work back thourgh the CFG to DEF_BB ensuring the range is calculated 
// on the block/edges leading back to that point.

void
gori_cache::fill_block_cache (tree name, basic_block bb, basic_block def_bb)
{
  tree type = TREE_TYPE (name);
  edge_iterator ei;
  edge e;
  irange block_result;
  irange undefined;
  undefined.set_undefined (type);

  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
      || bb == def_bb)
    return;

  // If the block cache is set, then we've already visited this block.
  if (m_on_entry.bb_range_p (name, bb))
    return;

  // Visit each block back to the DEF.  Initialize each one to UNDEFINED.
  // m_visited at the end will contain all the blocks that we needed to set
  // the range_on_entry cache for.
  m_workback.truncate (0);
  m_workback.quick_push (bb);
  m_on_entry.set_bb_range (name, bb, undefined);
  gcc_checking_assert (m_update_list.length () == 0);

// fprintf (stderr, "\n");
// print_generic_expr (stderr, name, TDF_SLIM);
// fprintf (stderr, " : ");

  while (m_workback.length () > 0)
    {
      basic_block node = m_workback.pop ();
// fprintf (stderr, "BACK visiting block %d\n", node->index);

      FOR_EACH_EDGE (e, ei, node->preds)
        {
	  // If this is a propagation edge, dont go to the pred.
	  if (maybe_propagate_on_edge (name, e))
	    continue;

	  // If the pred hasn't been visited, add it to the list.
	  if (!m_on_entry.bb_range_p (name, e->src))
	    {
	      m_on_entry.set_bb_range (name, e->src, undefined);
	      m_workback.quick_push (e->src);
	    }
	  }
      }

  iterative_cache_update (name);
}
