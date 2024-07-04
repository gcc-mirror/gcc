/* Gimple range inference implementation.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
#include "value-range-storage.h"
#include "tree-cfg.h"
#include "target.h"
#include "attribs.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "cfganal.h"
#include "tree-dfa.h"

// Create the global oracle.

infer_range_oracle infer_oracle;

// This class is merely an accessor which is granted internals to
// gimple_infer_range such that non_null_loadstore as a static callback can
// call the protected add_nonzero ().
// Static functions ccannot be friends, so we do it through a class wrapper.

class non_null_wrapper
{
public:
  inline non_null_wrapper (gimple_infer_range *infer) : m_infer (infer) { }
  inline void add_nonzero (tree name) { m_infer->add_nonzero (name); }
  inline void add_range (tree t, vrange &r) { m_infer->add_range (t, r); }
private:
  gimple_infer_range *m_infer;
};

// Adapted from infer_nonnull_range_by_dereference and check_loadstore
// to process nonnull ssa_name OP in S.  DATA contains a pointer to a
// stmt range inference instance.

static bool
non_null_loadstore (gimple *, tree op, tree, void *data)
{
  if (TREE_CODE (op) == MEM_REF || TREE_CODE (op) == TARGET_MEM_REF)
    {
      /* Some address spaces may legitimately dereference zero.  */
      addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (op));
      if (!targetm.addr_space.zero_address_valid (as))
	{
	  non_null_wrapper wrapper ((gimple_infer_range *)data);
	  wrapper.add_nonzero (TREE_OPERAND (op, 0));
	}
    }
  return false;
}

// Process an ASSUME call to see if there are any inferred ranges available.

void
gimple_infer_range::check_assume_func (gcall *call)
{
  tree arg;
  unsigned i;
  tree assume_id = TREE_OPERAND (gimple_call_arg (call, 0), 0);
  if (!assume_id)
    return;
  struct function *fun = DECL_STRUCT_FUNCTION (assume_id);
  if (!fun)
    return;
  // Loop over arguments, matching them to the assume parameters.
  for (arg = DECL_ARGUMENTS (assume_id), i = 1;
       arg && i < gimple_call_num_args (call);
       i++, arg = DECL_CHAIN (arg))
    {
      tree op = gimple_call_arg (call, i);
      tree type = TREE_TYPE (op);
      if (gimple_range_ssa_p (op) && value_range::supports_type_p (type))
	{
	  tree default_def = ssa_default_def (fun, arg);
	  if (!default_def || type != TREE_TYPE (default_def))
	    continue;
	  // Query the global range of the default def in the assume function.
	  value_range assume_range (type);
	  gimple_range_global (assume_range, default_def, fun);
	  // If there is a non-varying result, add it as an inferred range.
	  if (!assume_range.varying_p ())
	    {
	      add_range (op, assume_range);
	      if (dump_file)
		{
		  print_generic_expr (dump_file, assume_id, TDF_SLIM);
		  fprintf (dump_file, " assume inferred range of ");
		  print_generic_expr (dump_file, op, TDF_SLIM);
		  fprintf (dump_file, " (param ");
		  print_generic_expr (dump_file, arg, TDF_SLIM);
		  fprintf (dump_file, ") = ");
		  assume_range.dump (dump_file);
		  fputc ('\n', dump_file);
		}
	    }
	}
    }
}

// Add NAME and RANGE to the range inference summary.

void
gimple_infer_range::add_range (tree name, vrange &range)
{
  // Do not add an inferred range if it is VARYING.
  if (range.varying_p ())
    return;
  m_names[num_args] = name;
  m_ranges[num_args] = range;
  if (num_args < size_limit - 1)
    num_args++;
}

// Add a nonzero range for NAME to the range inference summary.

void
gimple_infer_range::add_nonzero (tree name)
{
  if (!gimple_range_ssa_p (name))
    return;
  prange nz;
  nz.set_nonzero (TREE_TYPE (name));
  add_range (name, nz);
}

// Process S for range inference and fill in the summary list.
// This is the routine where new inferred ranges should be added.
// If USE_RANGEOPS is true, invoke range-ops on stmts with a single
// ssa-name aa constant to reflect an inferred range. ie
// x_2 = y_3 + 1 will provide an inferred range for y_3 of [-INF, +INF - 1].
// This defaults to FALSE as it can be expensive.,

gimple_infer_range::gimple_infer_range (gimple *s, bool use_rangeops)
{
  num_args = 0;

  if (is_a<gphi *> (s))
    return;

  if (is_a<gcall *> (s) && flag_delete_null_pointer_checks)
    {
      tree fntype = gimple_call_fntype (s);
      bitmap nonnullargs = get_nonnull_args (fntype);
      // Process any non-null arguments
      if (nonnullargs)
	{
	  for (unsigned i = 0; i < gimple_call_num_args (s); i++)
	    {
	      if (bitmap_empty_p (nonnullargs)
		  || bitmap_bit_p (nonnullargs, i))
		{
		  tree op = gimple_call_arg (s, i);
		  if (POINTER_TYPE_P (TREE_TYPE (op)))
		    add_nonzero (op);
		}
	    }
	  BITMAP_FREE (nonnullargs);
	}
      // Fallthru and walk load/store ops now.
    }

  // Check for inferred ranges from ASSUME calls.
  if (is_a<gcall *> (s) && gimple_call_internal_p (s)
      && gimple_call_internal_fn (s) == IFN_ASSUME)
    check_assume_func (as_a<gcall *> (s));

  // Look for possible non-null values.
  if (flag_delete_null_pointer_checks && gimple_code (s) != GIMPLE_ASM
      && !gimple_clobber_p (s))
    walk_stmt_load_store_ops (s, (void *)this, non_null_loadstore,
			      non_null_loadstore);

  // Gated by flag.
  if (!use_rangeops)
    return;

  // Check if there are any inferred ranges from range-ops.
  gimple_range_op_handler handler (s);
  if (!handler)
    return;

  // Only proceed if ONE operand is an SSA_NAME,  This may provide an
  // inferred range for 'y + 3' , but will bypass expressions like
  // 'y + z' as it depends on symbolic values.
  tree ssa1 = gimple_range_ssa_p (handler.operand1 ());
  tree ssa2 = gimple_range_ssa_p (handler.operand2 ());
  if ((ssa1 != NULL) == (ssa2 != NULL))
    return;

  // The other operand should be a constant, so just use the global range
  // query to pick up any other values.
  if (ssa1)
    {
      value_range op1 (TREE_TYPE (ssa1));
      if (op1_range (op1, s, get_global_range_query ()) && !op1.varying_p ())
	add_range (ssa1, op1);
    }
  else
    {
      gcc_checking_assert (ssa2);
      value_range op2 (TREE_TYPE (ssa2));
      if (op2_range (op2, s, get_global_range_query ()) && !op2.varying_p ())
	add_range (ssa2, op2);
    }
}

// Create an single inferred range for NAMe using range R.

gimple_infer_range::gimple_infer_range (tree name, vrange &r)
{
  num_args = 0;
  add_range (name, r);
}

// -------------------------------------------------------------------------

// This class is an element in the list of inferred ranges.

class exit_range
{
public:
  tree name;
  gimple *stmt;
  vrange_storage *range;
  exit_range *next;
};


// If there is an element which matches SSA, return a pointer to the element.
// Otherwise return NULL.

exit_range *
infer_range_manager::exit_range_head::find_ptr (tree ssa)
{
  // Return NULL if SSA is not in this list.
  if (!m_names || !bitmap_bit_p (m_names, SSA_NAME_VERSION (ssa)))
    return NULL;
  for (exit_range *ptr = head; ptr != NULL; ptr = ptr->next)
    if (ptr->name == ssa)
      return ptr;
  // Should be unreachable.
  gcc_unreachable ();
  return NULL;
}

// Construct a range infer manager.  DO_SEARCH indicates whether an immediate
// use scan should be made the first time a name is processed.  This is for
// on-demand clients who may not visit every statement and may miss uses.

infer_range_manager::infer_range_manager (bool do_search)
{
  bitmap_obstack_initialize (&m_bitmaps);
  m_on_exit.create (0);
  m_on_exit.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
  // m_seen == NULL indicates no scanning.  Otherwise the bit indicates a
  // scan has been performed on NAME.
  if (do_search)
    m_seen = BITMAP_ALLOC (&m_bitmaps);
  else
    m_seen = NULL;
  obstack_init (&m_list_obstack);
  // Non-zero elements are very common, so cache them for each ssa-name.
  m_nonzero.create (0);
  m_nonzero.safe_grow_cleared (num_ssa_names + 1);
  m_range_allocator = new vrange_allocator;
}

// Destruct a range infer manager.

infer_range_manager::~infer_range_manager ()
{
  m_nonzero.release ();
  obstack_free (&m_list_obstack, NULL);
  m_on_exit.release ();
  bitmap_obstack_release (&m_bitmaps);
  delete m_range_allocator;
}

// Return a non-zero range value of the appropriate type for NAME from
// the cache, creating it if necessary.

const vrange&
infer_range_manager::get_nonzero (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_nonzero.length ())
    m_nonzero.safe_grow_cleared (num_ssa_names + 20);
  if (!m_nonzero[v])
    {
      m_nonzero[v]
	= (irange *) m_range_allocator->alloc (sizeof (int_range <2>));
      m_nonzero[v]->set_nonzero (TREE_TYPE (name));
    }
  return *(m_nonzero[v]);
}

// Return TRUE if NAME has a range inference in block BB.  If NAME is NULL,
// return TRUE if there are any name sin BB.

bool
infer_range_manager::has_range_p (basic_block bb, tree name)
{
  // Check if this is an immediate use search model.
  if (name && m_seen && !bitmap_bit_p (m_seen, SSA_NAME_VERSION (name)))
    register_all_uses (name);

  if (bb->index >= (int)m_on_exit.length ())
    return false;

  bitmap b = m_on_exit[bb->index].m_names;
  if (!b)
    return false;

  if (name)
    return bitmap_bit_p (m_on_exit[bb->index].m_names, SSA_NAME_VERSION (name));
  return !bitmap_empty_p (b);
}

// Return TRUE if NAME has a range inference in block BB, and adjust range R
// to include it.

bool
infer_range_manager::maybe_adjust_range (vrange &r, tree name, basic_block bb)
{
  if (!has_range_p (bb, name))
    return false;
  exit_range *ptr = m_on_exit[bb->index].find_ptr (name);
  gcc_checking_assert (ptr);
  // Return true if this exit range changes R, otherwise false.
  tree type = TREE_TYPE (name);
  value_range tmp (type);
  ptr->range->get_vrange (tmp, type);
  return r.intersect (tmp);
}

// Add all inferred ranges in INFER at stmt S.

void
infer_range_manager::add_ranges (gimple *s, gimple_infer_range &infer)
{
  for (unsigned x = 0; x < infer.num (); x++)
    add_range (infer.name (x), s, infer.range (x));
}

// Add range R as an inferred range for NAME on stmt S.

void
infer_range_manager::add_range (tree name, gimple *s, const vrange &r)
{
  basic_block bb = gimple_bb (s);
  if (!bb)
    return;
  if (bb->index >= (int)m_on_exit.length ())
    m_on_exit.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);

  // Create the summary list bitmap if it doesn't exist.
  if (!m_on_exit[bb->index].m_names)
      m_on_exit[bb->index].m_names = BITMAP_ALLOC (&m_bitmaps);

  if (dump_file && (dump_flags & TDF_DETAILS))
   {
     fprintf (dump_file, "   on-exit update ");
     print_generic_expr (dump_file, name, TDF_SLIM);
     fprintf (dump_file, " in BB%d : ",bb->index);
     r.dump (dump_file);
     fprintf (dump_file, "\n");
   }

  // If NAME already has a range, intersect them and done.
  exit_range *ptr = m_on_exit[bb->index].find_ptr (name);
  if (ptr)
    {
      tree type = TREE_TYPE (name);
      value_range cur (r), name_range (type);
      ptr->range->get_vrange (name_range, type);
      // If no new info is added, just return.
      if (!cur.intersect (name_range))
	return;
      if (ptr->range->fits_p (cur))
	ptr->range->set_vrange (cur);
      else
	ptr->range = m_range_allocator->clone (cur);
      ptr->stmt = s;
      return;
    }

  // Otherwise create a record.
  bitmap_set_bit (m_on_exit[bb->index].m_names, SSA_NAME_VERSION (name));
  ptr = (exit_range *)obstack_alloc (&m_list_obstack, sizeof (exit_range));
  ptr->range = m_range_allocator->clone (r);
  ptr->name = name;
  ptr->stmt = s;
  ptr->next = m_on_exit[bb->index].head;
  m_on_exit[bb->index].head = ptr;
}

// Add a non-zero inferred range for NAME at stmt S.

void
infer_range_manager::add_nonzero (tree name, gimple *s)
{
  add_range (name, s, get_nonzero (name));
}

// Follow immediate use chains and find all inferred ranges for NAME.

void
infer_range_manager::register_all_uses (tree name)
{
  gcc_checking_assert (m_seen);

  // Check if we've already processed this name.
  unsigned v = SSA_NAME_VERSION (name);
  if (bitmap_bit_p (m_seen, v))
     return;
  bitmap_set_bit (m_seen, v);

  use_operand_p use_p;
  imm_use_iterator iter;

  // Loop over each immediate use and see if it has an inferred range.
  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      gimple *s = USE_STMT (use_p);
      gimple_infer_range infer (s);
      for (unsigned x = 0; x < infer.num (); x++)
	{
	  if (name == infer.name (x))
	    add_range (name, s, infer.range (x));
	}
    }
}
