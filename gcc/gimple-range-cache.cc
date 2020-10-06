/* Gimple ranger SSA cache implementation.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.
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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-range.h"

// During contructor, allocate the vector of ssa_names.

non_null_ref::non_null_ref ()
{
  m_nn.create (0);
  m_nn.safe_grow_cleared (num_ssa_names);
  bitmap_obstack_initialize (&m_bitmaps);
}

// Free any bitmaps which were allocated,a swell as the vector itself.

non_null_ref::~non_null_ref ()
{
  bitmap_obstack_release (&m_bitmaps);
  m_nn.release ();
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

// Allocate an populate the bitmap for NAME.  An ON bit for a block
// index indicates there is a non-null reference in that block.  In
// order to populate the bitmap, a quick run of all the immediate uses
// are made and the statement checked to see if a non-null dereference
// is made on that statement.

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

  b = BITMAP_ALLOC (&m_bitmaps);

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

// -------------------------------------------------------------------------

// This class implements a cache of ranges indexed by basic block.  It
// represents all that is known about an SSA_NAME on entry to each
// block.  It caches a range-for-type varying range so it doesn't need
// to be reformed all the time.  If a range is ever always associated
// with a type, we can use that instead.  Whenever varying is being
// set for a block, the cache simply points to this cached one rather
// than create a new one each time.

class ssa_block_ranges
{
public:
  ssa_block_ranges (tree t, irange_allocator *allocator);
  ~ssa_block_ranges ();

  void set_bb_range (const basic_block bb, const irange &r);
  void set_bb_varying (const basic_block bb);
  bool get_bb_range (irange &r, const basic_block bb);
  bool bb_range_p (const basic_block bb);

  void dump(FILE *f);
private:
  vec<irange *> m_tab;
  irange *m_type_range;
  tree m_type;
  irange_allocator *m_irange_allocator;
};


// Initialize a block cache for an ssa_name of type T.

ssa_block_ranges::ssa_block_ranges (tree t, irange_allocator *allocator)
{
  gcc_checking_assert (TYPE_P (t));
  m_type = t;
  m_irange_allocator = allocator;

  m_tab.create (0);
  m_tab.safe_grow_cleared (last_basic_block_for_fn (cfun));

  // Create the cached type range.
  m_type_range = m_irange_allocator->allocate (2);
  m_type_range->set_varying (t);

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
  irange *m = m_irange_allocator->allocate (r);
  m_tab[bb->index] = m;
}

// Set the range for block BB to the range for the type.

void
ssa_block_ranges::set_bb_varying (const basic_block bb)
{
  m_tab[bb->index] = m_type_range;
}

// Return the range associated with block BB in R.  Return false if
// there is no range.

bool
ssa_block_ranges::get_bb_range (irange &r, const basic_block bb)
{
  irange *m = m_tab[bb->index];
  if (m)
    {
      r = *m;
      return true;
    }
  return false;
}

// Return true if a range is present.

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
  int_range_max r;

  FOR_EACH_BB_FN (bb, cfun)
    if (get_bb_range (r, bb))
      {
	fprintf (f, "BB%d  -> ", bb->index);
	r.dump (f);
	fprintf (f, "\n");
      }
}

// -------------------------------------------------------------------------

// Initialize the block cache.

block_range_cache::block_range_cache ()
{
  m_ssa_ranges.create (0);
  m_ssa_ranges.safe_grow_cleared (num_ssa_names);
  m_irange_allocator = new irange_allocator;
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
  delete m_irange_allocator;
  // Release the vector itself.
  m_ssa_ranges.release ();
}

// Return a reference to the m_block_cache for NAME.  If it has not been
// accessed yet, allocate it.

ssa_block_ranges &
block_range_cache::get_block_ranges (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_ssa_ranges.length ())
    m_ssa_ranges.safe_grow_cleared (num_ssa_names + 1);

  if (!m_ssa_ranges[v])
    m_ssa_ranges[v] = new ssa_block_ranges (TREE_TYPE (name), m_irange_allocator);

  return *(m_ssa_ranges[v]);
}

// Set the range for NAME on entry to block BB to R.

void
block_range_cache::set_bb_range (tree name, const basic_block bb,
				 const irange &r)
{
  return get_block_ranges (name).set_bb_range (bb, r);
}

// Set the range for NAME on entry to block BB to varying.

void
block_range_cache::set_bb_varying (tree name, const basic_block bb)
{
  return get_block_ranges (name).set_bb_varying (bb);
}

// Return the range for NAME on entry to BB in R.  Return true if there
// is one.

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
  for (x = 0; x < m_ssa_ranges.length (); ++x)
    {
      if (m_ssa_ranges[x])
	{
	  fprintf (f, " Ranges for ");
	  print_generic_expr (f, ssa_name (x), TDF_NONE);
	  fprintf (f, ":\n");
	  m_ssa_ranges[x]->dump (f);
	  fprintf (f, "\n");
	}
    }
}

// Print all known ranges on entry to blobk BB to file F.

void
block_range_cache::dump (FILE *f, basic_block bb, bool print_varying)
{
  unsigned x;
  int_range_max r;
  bool summarize_varying = false;
  for (x = 1; x < m_ssa_ranges.length (); ++x)
    {
      if (!gimple_range_ssa_p (ssa_name (x)))
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
	  fprintf (f, "\n");
	}
    }
  // If there were any varying entries, lump them all together.
  if (summarize_varying)
    {
      fprintf (f, "VARYING_P on entry : ");
      for (x = 1; x < num_ssa_names; ++x)
	{
	  if (!gimple_range_ssa_p (ssa_name (x)))
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
  m_irange_allocator = new irange_allocator;
}

// Deconstruct a global cache.

ssa_global_cache::~ssa_global_cache ()
{
  m_tab.release ();
  delete m_irange_allocator;
}

// Retrieve the global range of NAME from cache memory if it exists. 
// Return the value in R.

bool
ssa_global_cache::get_global_range (irange &r, tree name) const
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    return false;

  irange *stow = m_tab[v];
  if (!stow)
    return false;
  r = *stow;
  return true;
}

// Set the range for NAME to R in the global cache.

void
ssa_global_cache::set_global_range (tree name, const irange &r)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    m_tab.safe_grow_cleared (num_ssa_names + 1);

  irange *m = m_tab[v];
  if (m && m->fits_p (r))
    *m = r;
  else
    m_tab[v] = m_irange_allocator->allocate (r);
}

// Set the range for NAME to R in the glonbal cache.

void
ssa_global_cache::clear_global_range (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    m_tab.safe_grow_cleared (num_ssa_names + 1);
  m_tab[v] = NULL;
}

// Clear the global cache.

void
ssa_global_cache::clear ()
{
  memset (m_tab.address(), 0, m_tab.length () * sizeof (irange *));
}

// Dump the contents of the global cache to F.

void
ssa_global_cache::dump (FILE *f)
{
  unsigned x;
  int_range_max r;
  fprintf (f, "Non-varying global ranges:\n");
  fprintf (f, "=========================:\n");
  for ( x = 1; x < num_ssa_names; x++)
    if (gimple_range_ssa_p (ssa_name (x)) &&
	get_global_range (r, ssa_name (x))  && !r.varying_p ())
      {
	print_generic_expr (f, ssa_name (x), TDF_NONE);
	fprintf (f, "  : ");
	r.dump (f);
	fprintf (f, "\n");
      }
  fputc ('\n', f);
}

// --------------------------------------------------------------------------

ranger_cache::ranger_cache (range_query &q) : query (q)
{
  m_workback.create (0);
  m_workback.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.create (0);
  m_update_list.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.truncate (0);
  m_poor_value_list.create (0);
  m_poor_value_list.safe_grow_cleared (20);
  m_poor_value_list.truncate (0);
}

ranger_cache::~ranger_cache ()
{
  m_poor_value_list.release ();
  m_workback.release ();
  m_update_list.release ();
}

// Push a request for a new lookup in block BB of name.  Return true if
// the request is actually made (ie, isn't a duplicate).

bool
ranger_cache::push_poor_value (basic_block bb, tree name)
{
  if (m_poor_value_list.length ())
    {
      // Don't push anything else to the same block.  If there are multiple 
      // things required, another request will come during a later evaluation
      // and this prevents oscillation building uneccessary depth.
      if ((m_poor_value_list.last ()).bb == bb)
	return false;
    }

  struct update_record rec;
  rec.bb = bb;
  rec.calc = name;
  m_poor_value_list.safe_push (rec);
  return true;
}

//  Provide lookup for the gori-computes class to access the best known range
//  of an ssa_name in any given basic block.  Note, this does no additonal
//  lookups, just accesses the data that is already known.

void
ranger_cache::ssa_range_in_bb (irange &r, tree name, basic_block bb)
{
  gimple *s = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = ((s && gimple_bb (s)) ? gimple_bb (s) :
					       ENTRY_BLOCK_PTR_FOR_FN (cfun));
  if (bb == def_bb)
    {
      // NAME is defined in this block, so request its current value
      if (!m_globals.get_global_range (r, name))
	{
	  // If it doesn't have a value calculated, it means it's a
	  // "poor" value being used in some calculation.  Queue it up
	  // as a poor value to be improved later.
	  r = gimple_range_global (name);
	  if (push_poor_value (bb, name))
	    {
	      if (DEBUG_RANGE_CACHE)
		{
		  fprintf (dump_file,
			   "*CACHE* no global def in bb %d for ", bb->index);
		  print_generic_expr (dump_file, name, TDF_SLIM);
		  fprintf (dump_file, " depth : %d\n",
			   m_poor_value_list.length ());
		}
	    }
	 }
    }
  // Look for the on-entry value of name in BB from the cache.
  else if (!m_on_entry.get_bb_range (r, name, bb))
    {
      // If it has no entry then mark this as a poor value.
      if (push_poor_value (bb, name))
	{
	  if (DEBUG_RANGE_CACHE)
	    {
	      fprintf (dump_file,
		       "*CACHE* no on entry range in bb %d for ", bb->index);
	      print_generic_expr (dump_file, name, TDF_SLIM);
	      fprintf (dump_file, " depth : %d\n", m_poor_value_list.length ());
	    }
	}
      // Try to pick up any known global value as a best guess for now.
      if (!m_globals.get_global_range (r, name))
	r = gimple_range_global (name);
    }

  // Check if pointers have any non-null dereferences.  Non-call
  // exceptions mean we could throw in the middle of the block, so just
  // punt for now on those.
  if (r.varying_p () && m_non_null.non_null_deref_p (name, bb) &&
      !cfun->can_throw_non_call_exceptions)
    r = range_nonzero (TREE_TYPE (name));
}

// Return a static range for NAME on entry to basic block BB in R.  If
// calc is true, fill any cache entries required between BB and the
// def block for NAME.  Otherwise, return false if the cache is empty.

bool
ranger_cache::block_range (irange &r, basic_block bb, tree name, bool calc)
{
  gcc_checking_assert (gimple_range_ssa_p (name));

  if (calc)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      basic_block def_bb = NULL;
      if (def_stmt)
	def_bb = gimple_bb (def_stmt);;
      if (!def_bb)
	{
	  // If we get to the entry block, this better be a default def
	  // or range_on_entry was called for a block not dominated by
	  // the def.  
	  gcc_checking_assert (SSA_NAME_IS_DEFAULT_DEF (name));
	  def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
	}

      // There is no range on entry for the definition block.
      if (def_bb == bb)
	return false;

      // Otherwise, go figure out what is known in predecessor blocks.
      fill_block_cache (name, bb, def_bb);
      gcc_checking_assert (m_on_entry.bb_range_p (name, bb));
    }
  return m_on_entry.get_bb_range (r, name, bb);
}

// Add BB to the list of blocks to update, unless it's already in the list.

void
ranger_cache::add_to_update (basic_block bb)
{
  if (!m_update_list.contains (bb))
    m_update_list.quick_push (bb);
}

// If there is anything in the iterative update_list, continue
// processing NAME until the list of blocks is empty.

void
ranger_cache::iterative_cache_update (tree name)
{
  basic_block bb;
  edge_iterator ei;
  edge e;
  int_range_max new_range;
  int_range_max current_range;
  int_range_max e_range;

  // Process each block by seeing if its calculated range on entry is
  // the same as its cached value. If there is a difference, update
  // the cache to reflect the new value, and check to see if any
  // successors have cache entries which may need to be checked for
  // updates.

  while (m_update_list.length () > 0)
    {
      bb = m_update_list.pop ();
      gcc_checking_assert (m_on_entry.bb_range_p (name, bb));
      m_on_entry.get_bb_range (current_range, name, bb);

      // Calculate the "new" range on entry by unioning the pred edges.
      new_range.set_undefined ();
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "   edge %d->%d :", e->src->index, bb->index);
	  // Get whatever range we can for this edge.
	  if (!outgoing_edge_range_p (e_range, e, name))
	    {
	      ssa_range_in_bb (e_range, name, e->src);
	      if (DEBUG_RANGE_CACHE)
		{
		  fprintf (dump_file, "No outgoing edge range, picked up ");
		  e_range.dump(dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
	  else
	    {
	      if (DEBUG_RANGE_CACHE)
		{
		  fprintf (dump_file, "outgoing range :");
		  e_range.dump(dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
	  new_range.union_ (e_range);
	  if (new_range.varying_p ())
	    break;
	}

      if (DEBUG_RANGE_CACHE)
	{
	  fprintf (dump_file, "FWD visiting block %d for ", bb->index);
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, "  starting range : ");
	  current_range.dump (dump_file);
	  fprintf (dump_file, "\n");
	}

      // If the range on entry has changed, update it.
      if (new_range != current_range)
	{
	  if (DEBUG_RANGE_CACHE) 
	    {
	      fprintf (dump_file, "      Updating range to ");
	      new_range.dump (dump_file);
	      fprintf (dump_file, "\n      Updating blocks :");
	    }
	  m_on_entry.set_bb_range (name, bb, new_range);
	  // Mark each successor that has a range to re-check its range
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (m_on_entry.bb_range_p (name, e->dest))
	      {
		if (DEBUG_RANGE_CACHE) 
		  fprintf (dump_file, " bb%d",e->dest->index);
		add_to_update (e->dest);
	      }
	  if (DEBUG_RANGE_CACHE) 
	    fprintf (dump_file, "\n");
	}
    }
    if (DEBUG_RANGE_CACHE)
      {
	fprintf (dump_file, "DONE visiting blocks for ");
	print_generic_expr (dump_file, name, TDF_SLIM);
	fprintf (dump_file, "\n");
      }
}

// Make sure that the range-on-entry cache for NAME is set for block BB.
// Work back through the CFG to DEF_BB ensuring the range is calculated
// on the block/edges leading back to that point.

void
ranger_cache::fill_block_cache (tree name, basic_block bb, basic_block def_bb)
{
  edge_iterator ei;
  edge e;
  int_range_max block_result;
  int_range_max undefined;
  unsigned poor_list_start = m_poor_value_list.length ();  

  // At this point we shouldn't be looking at the def, entry or exit block.
  gcc_checking_assert (bb != def_bb && bb != ENTRY_BLOCK_PTR_FOR_FN (cfun) &&
		       bb != EXIT_BLOCK_PTR_FOR_FN (cfun));

  // If the block cache is set, then we've already visited this block.
  if (m_on_entry.bb_range_p (name, bb))
    return;

  // Visit each block back to the DEF.  Initialize each one to UNDEFINED.
  // m_visited at the end will contain all the blocks that we needed to set
  // the range_on_entry cache for.
  m_workback.truncate (0);
  m_workback.quick_push (bb);
  undefined.set_undefined ();
  m_on_entry.set_bb_range (name, bb, undefined);
  gcc_checking_assert (m_update_list.length () == 0);

  if (DEBUG_RANGE_CACHE)
    {
      fprintf (dump_file, "\n");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, " : ");
    }

  while (m_workback.length () > 0)
    {
      basic_block node = m_workback.pop ();
      if (DEBUG_RANGE_CACHE)
	{
	  fprintf (dump_file, "BACK visiting block %d for ", node->index);
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}

      FOR_EACH_EDGE (e, ei, node->preds)
	{
	  basic_block pred = e->src;
	  int_range_max r;

	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "  %d->%d ",e->src->index, e->dest->index);

	  // If the pred block is the def block add this BB to update list.
	  if (pred == def_bb)
	    {
	      add_to_update (node);
	      continue;
	    }

	  // If the pred is entry but NOT def, then it is used before
	  // defined, it'll get set to [] and no need to update it.
	  if (pred == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    {
	      if (DEBUG_RANGE_CACHE)
		fprintf (dump_file, "entry: bail.");
	      continue;
	    }

	  // Regardless of whether we have visited pred or not, if the
	  // pred has a non-null reference, revisit this block.
	  if (m_non_null.non_null_deref_p (name, pred))
	    {
	      if (DEBUG_RANGE_CACHE)
		fprintf (dump_file, "nonnull: update ");
	      add_to_update (node);
	    }

	  // If the pred block already has a range, or if it can contribute
	  // something new. Ie, the edge generates a range of some sort.
	  if (m_on_entry.get_bb_range (r, name, pred))
	    {
	      if (DEBUG_RANGE_CACHE)
		fprintf (dump_file, "has cache, ");
	      if (!r.undefined_p () || has_edge_range_p (e, name))
		{
		  add_to_update (node);
		  if (DEBUG_RANGE_CACHE)
		    fprintf (dump_file, "update. ");
		}
	      continue;
	    }

	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "pushing undefined pred block. ");
	  // If the pred hasn't been visited (has no range), add it to
	  // the list.
	  gcc_checking_assert (!m_on_entry.bb_range_p (name, pred));
	  m_on_entry.set_bb_range (name, pred, undefined);
	  m_workback.quick_push (pred);
	}
    }

  if (DEBUG_RANGE_CACHE)
    fprintf (dump_file, "\n");

  // Now fill in the marked blocks with values.
  iterative_cache_update (name);
  if (DEBUG_RANGE_CACHE)
    fprintf (dump_file, "  iterative update done.\n");

  // Now that the cache has been updated, check to see if there were any 
  // SSA_NAMES used in filling the cache which were "poor values".
  // We can evaluate them, and inject any new values into the iteration 
  // list, and see if it improves any on-entry values.
  if (poor_list_start !=  m_poor_value_list.length ())
    {
      gcc_checking_assert (poor_list_start < m_poor_value_list.length ());
      while (poor_list_start < m_poor_value_list.length ())
	{
	  // Find a range for this unresolved value.   
	  // Note, this may spawn new cache filling cycles, but by the time it
	  // is finished, the work vectors will all be back to the same state
	  // as before the call.  The update record vector will always be
	  // returned to the current state upon return.
	  struct update_record rec = m_poor_value_list.pop ();
	  basic_block calc_bb = rec.bb;
	  int_range_max tmp;

	  // The update work list should be empty at this point.
	  gcc_checking_assert (m_update_list.length () == 0);

	  if (DEBUG_RANGE_CACHE)
	    {
	      fprintf (dump_file, "(%d:%d)Calculating ",
		       m_poor_value_list.length () + 1, poor_list_start);
	      print_generic_expr (dump_file, name, TDF_SLIM);
	      fprintf (dump_file, " used poor value for ");
	      print_generic_expr (dump_file, rec.calc, TDF_SLIM);
	      fprintf (dump_file, " in bb%d, trying to improve:\n",
		       calc_bb->index);
	    }

	  // It must have at least one edge, pick edge 0.  we just want to
	  // calculate a range at the exit from the block so the caches feeding
	  // this block will be filled up. 
	  gcc_checking_assert (EDGE_SUCC (calc_bb, 0));
	  query.range_on_edge (tmp, EDGE_SUCC (calc_bb, 0), rec.calc);
	  
	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "    Checking successors of bb%d :",
		     calc_bb->index);

	  // Try recalculating any successor blocks with the new value.
	  // Note that even if this value is refined from the initial value,
	  // it may not affect the calculation, but the iterative update
	  // will resolve that efficently.
	  FOR_EACH_EDGE (e, ei, calc_bb->succs)
	    {
	      if (DEBUG_RANGE_CACHE)
		fprintf (dump_file, "bb%d: ", e->dest->index);
	      // Only update active cache entries.
	      if (m_on_entry.bb_range_p (name, e->dest))
		{
		  if (DEBUG_RANGE_CACHE)
		    fprintf (dump_file, "update ");
		  add_to_update (e->dest);
		}
	    }
	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "\n");
	  // Now see if there is a new value.
	  iterative_cache_update (name);
	}
    }
 
}
