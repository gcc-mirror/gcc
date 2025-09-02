/* Avoid store forwarding optimization pass.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by VRULL GmbH.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "avoid-store-forwarding.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "alias.h"
#include "rtlanal.h"
#include "cfgrtl.h"
#include "tree-pass.h"
#include "cselib.h"
#include "predict.h"
#include "insn-config.h"
#include "expmed.h"
#include "recog.h"
#include "regset.h"
#include "df.h"
#include "expr.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "vec.h"

/* This pass tries to detect and avoid cases of store forwarding.
   On many processors there is a large penalty when smaller stores are
   forwarded to larger loads.  The idea used to avoid the stall is to move
   the store after the load and in addition emit a bit insert sequence so
   the load register has the correct value.  For example the following:

     strb    w2, [x1, 1]
     ldr     x0, [x1]

   Will be transformed to:

     ldr     x0, [x1]
     strb    w2, [x1]
     bfi     x0, x2, 0, 8
*/

namespace {

const pass_data pass_data_avoid_store_forwarding =
{
  RTL_PASS, /* type.  */
  "avoid_store_forwarding", /* name.  */
  OPTGROUP_NONE, /* optinfo_flags.  */
  TV_AVOID_STORE_FORWARDING, /* tv_id.  */
  0, /* properties_required.  */
  0, /* properties_provided.  */
  0, /* properties_destroyed.  */
  0, /* todo_flags_start.  */
  TODO_df_finish /* todo_flags_finish.  */
};

class pass_rtl_avoid_store_forwarding : public rtl_opt_pass
{
public:
  pass_rtl_avoid_store_forwarding (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_avoid_store_forwarding, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) final override
    {
      return flag_avoid_store_forwarding && optimize >= 1;
    }

  virtual unsigned int execute (function *) final override;
}; // class pass_rtl_avoid_store_forwarding

/* Handler for finding and avoiding store forwardings.  */

class store_forwarding_analyzer
{
public:
  unsigned int stats_sf_detected = 0;
  unsigned int stats_sf_avoided = 0;

  bool is_store_forwarding (rtx store_mem, rtx load_mem,
			    HOST_WIDE_INT *off_val);
  bool process_store_forwarding (vec<store_fwd_info> &, rtx_insn *load_insn,
				 rtx load_mem);
  void avoid_store_forwarding (basic_block);
  void update_stats (function *);
};

/* Return a bit insertion sequence that would make DEST have the correct value
   if the store represented by STORE_INFO were to be moved after DEST.  */

static rtx_insn *
generate_bit_insert_sequence (store_fwd_info *store_info, rtx dest)
{
  /* Memory size should be a constant at this stage.  */
  unsigned HOST_WIDE_INT store_size
    = MEM_SIZE (store_info->store_mem).to_constant ();

  start_sequence ();

  unsigned HOST_WIDE_INT bitsize = store_size * BITS_PER_UNIT;
  unsigned HOST_WIDE_INT start = store_info->offset * BITS_PER_UNIT;

  rtx mov_reg = store_info->mov_reg;
  store_bit_field (dest, bitsize, start, 0, 0, GET_MODE (mov_reg), mov_reg,
		   false, false);

  rtx_insn *insns = get_insns ();
  unshare_all_rtl_in_chain (insns);
  end_sequence ();

  for (rtx_insn *insn = insns; insn; insn = NEXT_INSN (insn))
    if (contains_mem_rtx_p (PATTERN (insn))
	|| recog_memoized (insn) < 0)
      return NULL;

  return insns;
}

/* Return true iff a store to STORE_MEM would write to a sub-region of bytes
   from what LOAD_MEM would read.  If true also store the relative byte offset
   of the store within the load to OFF_VAL.  */

bool store_forwarding_analyzer::
is_store_forwarding (rtx store_mem, rtx load_mem, HOST_WIDE_INT *off_val)
{
  poly_int64 load_offset, store_offset;
  rtx load_base = strip_offset (XEXP (load_mem, 0), &load_offset);
  rtx store_base = strip_offset (XEXP (store_mem, 0), &store_offset);
  poly_int64 off_diff = store_offset - load_offset;

  HOST_WIDE_INT off_val_tmp = 0;
  bool is_off_diff_constant = off_diff.is_constant (&off_val_tmp);
  if (off_val)
    *off_val = off_val_tmp;

  return (MEM_SIZE (load_mem).is_constant ()
	  && rtx_equal_p (load_base, store_base)
	  && known_subrange_p (store_offset, MEM_SIZE (store_mem),
			       load_offset, MEM_SIZE (load_mem))
	  && is_off_diff_constant);
}

/* Given a list of small stores that are forwarded to LOAD_INSN, try to
   rearrange them so that a store-forwarding penalty doesn't occur.
   The stores must be given in reverse program order, starting from the
   one closer to LOAD_INSN.  */

bool store_forwarding_analyzer::
process_store_forwarding (vec<store_fwd_info> &stores, rtx_insn *load_insn,
			  rtx load_mem)
{
  machine_mode load_mem_mode = GET_MODE (load_mem);
  /* Memory sizes should be constants at this stage.  */
  HOST_WIDE_INT load_size = MEM_SIZE (load_mem).to_constant ();

  /* If the stores cover all the bytes of the load, then we can eliminate
     the load entirely and use the computed value instead.
     We can also eliminate stores on addresses that are overwritten
     by later stores.  */

  sbitmap forwarded_bytes = sbitmap_alloc (load_size);
  bitmap_clear (forwarded_bytes);

  unsigned int i;
  store_fwd_info* it;
  auto_vec<store_fwd_info> redundant_stores;
  auto_vec<int> store_ind_to_remove;
  FOR_EACH_VEC_ELT (stores, i, it)
    {
      HOST_WIDE_INT store_size = MEM_SIZE (it->store_mem).to_constant ();
      if (bitmap_all_bits_in_range_p (forwarded_bytes, it->offset,
				      it->offset + store_size - 1))
	{
	  redundant_stores.safe_push (*it);
	  store_ind_to_remove.safe_push (i);
	  continue;
	}
      bitmap_set_range (forwarded_bytes, it->offset, store_size);
    }

  bitmap_not (forwarded_bytes, forwarded_bytes);
  bool load_elim = bitmap_empty_p (forwarded_bytes);

  stats_sf_detected++;

  if (dump_file)
    {
      fprintf (dump_file, "Store forwarding detected:\n");

      FOR_EACH_VEC_ELT (stores, i, it)
	{
	  fprintf (dump_file, "From: ");
	  print_rtl_single (dump_file, it->store_insn);
	}

      fprintf (dump_file, "To: ");
      print_rtl_single (dump_file, load_insn);

      if (load_elim)
	fprintf (dump_file, "(Load elimination candidate)\n");
    }

  /* Remove redundant stores from the vector.  Although this is quadratic,
     there doesn't seem to be much point optimizing it.  The number of
     redundant stores is expected to be low and the length of the list is
     limited by a --param.  The dependence checking that we did earlier is
     also quadratic in the size of this list.  */
  store_ind_to_remove.reverse ();
  for (int i : store_ind_to_remove)
    stores.ordered_remove (i);

  rtx load = single_set (load_insn);
  rtx dest;

  if (load_elim)
    dest = gen_reg_rtx (load_mem_mode);
  else
    dest = SET_DEST (load);

  int move_to_front = -1;
  int total_cost = 0;
  int base_offset_index = -1;

  /* Find the last store that has the same offset the load, in the case that
     we're eliminating the load.  We will try to use it as a base register
     to avoid bit inserts (see second loop below).  We want the last one, as
     it will be wider and we don't want to overwrite the base register if
     there are many of them.  */
  if (load_elim)
    {
      FOR_EACH_VEC_ELT_REVERSE (stores, i, it)
	{
	  const bool has_base_offset
	    = known_eq (poly_uint64 (it->offset),
			subreg_size_lowpart_offset (MEM_SIZE (it->store_mem),
						    load_size));
	  if (has_base_offset)
	    {
	      base_offset_index = i;
	      break;
	    }
	}
    }

  /* Check if we can emit bit insert instructions for all forwarded stores.  */
  FOR_EACH_VEC_ELT (stores, i, it)
    {
      it->mov_reg = gen_reg_rtx (GET_MODE (it->store_mem));
      rtx_insn *insns = NULL;

      /* Check if this is a store with base offset, if we're eliminating the
	 load, and use it as the base register to avoid a bit insert if
	 possible.  Load elimination is implied by base_offset_index != -1.  */
      if (i == (unsigned) base_offset_index)
	{
	  start_sequence ();

	  rtx base_reg = lowpart_subreg (GET_MODE (dest), it->mov_reg,
					 GET_MODE (it->mov_reg));

	  if (base_reg)
	    {
	      rtx_insn *move0 = emit_move_insn (dest, base_reg);
	      if (recog_memoized (move0) >= 0)
		{
		  insns = get_insns ();
		  move_to_front = (int) i;
		}
	    }

	  end_sequence ();
	}

      if (!insns)
	insns = generate_bit_insert_sequence (&(*it), dest);

      if (!insns)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Failed due to: ");
	      print_rtl_single (dump_file, it->store_insn);
	    }
	  return false;
	}

      total_cost += seq_cost (insns, true);
      it->bits_insert_insns = insns;

      rtx store_set = single_set (it->store_insn);

      /* Create a register move at the store's original position to save the
	 stored value.  */
      start_sequence ();
      rtx_insn *insn1
	= emit_insn (gen_rtx_SET (it->mov_reg, SET_SRC (store_set)));
      end_sequence ();

      if (recog_memoized (insn1) < 0)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Failed due to unrecognizable insn: ");
	      print_rtl_single (dump_file, insn1);
	    }
	  return false;
	}

      it->save_store_value_insn = insn1;

      /* Create a new store after the load with the saved original value.
	 This avoids the forwarding stall.  */
      start_sequence ();
      rtx_insn *insn2
	= emit_insn (gen_rtx_SET (SET_DEST (store_set), it->mov_reg));
      end_sequence ();

      if (recog_memoized (insn2) < 0)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Failed due to unrecognizable insn: ");
	      print_rtl_single (dump_file, insn2);
	    }
	  return false;
	}

      it->store_saved_value_insn = insn2;
    }

  if (load_elim)
    total_cost -= insn_cost (load_insn, true);

  /* Let the target decide if transforming this store forwarding instance is
     profitable.  */
  if (!targetm.avoid_store_forwarding_p (stores, load_mem, total_cost,
					 load_elim))
    {
      if (dump_file)
	fprintf (dump_file, "Not transformed due to target decision.\n");

      return false;
    }

  /* If we have a move instead of bit insert, it needs to be emitted first in
     the resulting sequence.  */
  if (move_to_front != -1)
    {
      store_fwd_info copy = stores[move_to_front];
      stores.safe_push (copy);
      stores.ordered_remove (move_to_front);
    }

  if (load_elim)
    {
      machine_mode outer_mode = GET_MODE (SET_DEST (load));
      rtx load_move;
      rtx load_value = dest;
      if (outer_mode != load_mem_mode)
	{
	  load_value = simplify_gen_unary (GET_CODE (SET_SRC (load)),
					   outer_mode, dest, load_mem_mode);
	}
      load_move = gen_rtx_SET (SET_DEST (load), load_value);

      start_sequence ();
      rtx_insn *insn = emit_insn (load_move);
      rtx_insn *seq = end_sequence ();

      if (recog_memoized (insn) < 0)
	return false;

      emit_insn_after (seq, load_insn);
    }

  if (dump_file)
    {
      fprintf (dump_file, "Store forwarding avoided with bit inserts:\n");

      FOR_EACH_VEC_ELT (stores, i, it)
	{
	  if (stores.length () > 1)
	    {
	      fprintf (dump_file, "For: ");
	      print_rtl_single (dump_file, it->store_insn);
	    }

	  fprintf (dump_file, "With sequence:\n");

	  for (rtx_insn *insn = it->bits_insert_insns; insn;
	       insn = NEXT_INSN (insn))
	    {
	      fprintf (dump_file, "  ");
	      print_rtl_single (dump_file, insn);
	    }
	}

      if (redundant_stores.length () > 0)
	{
	  fprintf (dump_file, "\nRedundant stores that have been removed:\n");
	  FOR_EACH_VEC_ELT (redundant_stores, i, it)
	    {
	      fprintf (dump_file, "  ");
	      print_rtl_single (dump_file, it->store_insn);
	    }
	}
    }

  stats_sf_avoided++;

  /* Done, emit all the generated instructions and delete the stores.
     Note that STORES are in reverse program order.  */

  FOR_EACH_VEC_ELT (stores, i, it)
    {
      emit_insn_after (it->bits_insert_insns, load_insn);
      emit_insn_after (it->store_saved_value_insn, load_insn);
    }

  FOR_EACH_VEC_ELT (stores, i, it)
    {
      emit_insn_before (it->save_store_value_insn, it->store_insn);
      delete_insn (it->store_insn);
    }

  /* Delete redundant stores.  */
  FOR_EACH_VEC_ELT (redundant_stores, i, it)
    delete_insn (it->store_insn);

  df_insn_rescan (load_insn);

  if (load_elim)
    delete_insn (load_insn);

  return true;
}

/* Try to modify BB so that expensive store forwarding cases are avoided.  */

void
store_forwarding_analyzer::avoid_store_forwarding (basic_block bb)
{
  if (!optimize_bb_for_speed_p (bb))
    return;

  auto_vec<store_fwd_info, 8> store_exprs;
  auto_vec<rtx> store_exprs_del;
  rtx_insn *insn;
  unsigned int insn_cnt = 0;

  /* We are iterating over the basic block's instructions detecting store
     instructions.  Upon reaching a load instruction, we check if any of the
     previously detected stores could result in store forwarding.  In that
     case, we try to reorder the load and store instructions.
     We skip this transformation when we encounter complex memory operations,
     instructions that might throw an exception, instruction dependencies,
     etc.  This is done by clearing the vector of detected stores, while
     keeping the removed stores in another vector.  By doing so, we can check
     if any of the removed stores operated on the load's address range, when
     reaching a subsequent store that operates on the same address range,
     as this would lead to incorrect values on the register that keeps the
     loaded value.  */
  FOR_BB_INSNS (bb, insn)
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      vec_rtx_properties properties;
      properties.add_insn (insn, false);

      rtx set = single_set (insn);

      if (!set || insn_could_throw_p (insn))
	{
	  unsigned int i;
	  store_fwd_info *it;
	  FOR_EACH_VEC_ELT (store_exprs, i, it)
	    store_exprs_del.safe_push (it->store_mem);
	  store_exprs.truncate (0);
	  continue;
	}

      /* The inner mem RTX if INSN is a load, NULL_RTX otherwise.  */
      rtx load_mem = SET_SRC (set);

      if (GET_CODE (load_mem) == ZERO_EXTEND
	  || GET_CODE (load_mem) == SIGN_EXTEND)
	load_mem = XEXP (load_mem, 0);

      if (!MEM_P (load_mem))
	load_mem = NULL_RTX;

      /* The mem RTX if INSN is a store, NULL_RTX otherwise.  */
      rtx store_mem = MEM_P (SET_DEST (set)) ? SET_DEST (set) : NULL_RTX;

      /* We cannot analyze memory RTXs that have unknown size.	*/
      if ((store_mem && (!MEM_SIZE_KNOWN_P (store_mem)
			 || !MEM_SIZE (store_mem).is_constant ()))
	  || (load_mem && (!MEM_SIZE_KNOWN_P (load_mem)
			   || !MEM_SIZE (load_mem).is_constant ())))
	{
	  unsigned int i;
	  store_fwd_info *it;
	  FOR_EACH_VEC_ELT (store_exprs, i, it)
	    store_exprs_del.safe_push (it->store_mem);
	  store_exprs.truncate (0);
	  continue;
	}

      bool is_simple = !properties.has_asm
		       && !properties.has_side_effects ();
      bool is_simple_store = is_simple
			     && store_mem
			     && !contains_mem_rtx_p (SET_SRC (set));
      bool is_simple_load = is_simple
			    && load_mem
			    && !contains_mem_rtx_p (SET_DEST (set));

      int removed_count = 0;

      if (is_simple_store)
	{
	  /* Record store forwarding candidate.	 */
	  store_fwd_info info;
	  info.store_insn = insn;
	  info.store_mem = store_mem;
	  info.insn_cnt = insn_cnt;
	  info.remove = false;
	  info.forwarded = false;
	  store_exprs.safe_push (info);
	}

      bool reads_mem = false;
      bool writes_mem = false;
      for (auto ref : properties.refs ())
	if (ref.is_mem ())
	  {
	    reads_mem |= ref.is_read ();
	    writes_mem |= ref.is_write ();
	  }
	else if (ref.is_write ())
	  {
	    /* Drop store forwarding candidates when the address register is
	       overwritten.  */
	    bool remove_rest = false;
	    unsigned int i;
	    store_fwd_info *it;
	    FOR_EACH_VEC_ELT_REVERSE (store_exprs, i, it)
	      {
		if (remove_rest
		    || reg_overlap_mentioned_p (regno_reg_rtx[ref.regno],
						it->store_mem))
		  {
		    it->remove = true;
		    removed_count++;
		    remove_rest = true;
		    store_exprs_del.safe_push (it->store_mem);
		  }
	      }
	  }

      if (is_simple_load)
	{
	  /* Process load for possible store forwarding cases.
	     Possible newly created/moved stores, resulted from a successful
	     forwarding, will be processed in subsequent iterations.  */
	  auto_vec<store_fwd_info> forwardings;
	  bool partial_forwarding = false;
	  bool remove_rest = false;

	  bool vector_load = VECTOR_MODE_P (GET_MODE (load_mem));

	  unsigned int i;
	  store_fwd_info *it;
	  FOR_EACH_VEC_ELT_REVERSE (store_exprs, i, it)
	    {
	      rtx store_mem = it->store_mem;
	      HOST_WIDE_INT off_val;

	      bool vector_store = VECTOR_MODE_P (GET_MODE (store_mem));

	      if (remove_rest)
		{
		  it->remove = true;
		  removed_count++;
		}
	      else if (vector_load ^ vector_store)
		{
		  /* Vector stores followed by a non-vector load or the
		     opposite, cause store_bit_field to generate non-canonical
		     expressions, like (subreg:V4SI (reg:DI ...) 0)).
		     Cases like that should be handled using vec_duplicate,
		     so we reject the transformation in those cases.  */
		  it->remove = true;
		  removed_count++;
		  remove_rest = true;
		  forwardings.truncate (0);
		}
	      else if (is_store_forwarding (store_mem, load_mem, &off_val))
		{
		  unsigned int j;
		  rtx *del_it;
		  bool same_range_as_removed = false;

		  /* Check if another store in the load's address range has
		     been deleted due to a constraint violation.  In this case
		     we can't forward any other stores that operate in this
		     range, as it would lead to partial update of the register
		     that holds the loaded value.  */
		  FOR_EACH_VEC_ELT (store_exprs_del, j, del_it)
		    {
		      rtx del_store_mem = *del_it;
		      same_range_as_removed
			= is_store_forwarding (del_store_mem, load_mem, NULL);
		      if (same_range_as_removed)
			break;
		    }

		  /* Check if moving this store after the load is legal.  */
		  bool write_dep = false;
		  if (!same_range_as_removed)
		    {
		      unsigned int j = store_exprs.length () - 1;
		      for (; j != i; j--)
			{
			  if (!store_exprs[j].forwarded
			      && output_dependence (store_mem,
						    store_exprs[j].store_mem))
			    {
			      write_dep = true;
			      break;
			    }
			}
		    }

		  if (!same_range_as_removed && !write_dep)
		    {
		      it->forwarded = true;
		      it->offset = off_val;
		      forwardings.safe_push (*it);
		    }
		  else
		    partial_forwarding = true;

		  it->remove = true;
		  removed_count++;
		}
	      else if (true_dependence (store_mem, GET_MODE (store_mem),
					load_mem))
		{
		  /* We cannot keep a store forwarding candidate if it possibly
		     interferes with this load.  */
		  it->remove = true;
		  removed_count++;
		  remove_rest = true;
		  forwardings.truncate (0);
		}
	    }

	  if (!forwardings.is_empty () && !partial_forwarding)
	    process_store_forwarding (forwardings, insn, load_mem);
	}

	/* Abort in case that we encounter a memory read/write that is not a
	   simple store/load, as we can't make safe assumptions about the
	   side-effects of this.  */
	if ((writes_mem && !is_simple_store)
	     || (reads_mem && !is_simple_load))
	  return;

	if (removed_count)
	{
	  unsigned int i, j;
	  store_fwd_info *it;
	  VEC_ORDERED_REMOVE_IF (store_exprs, i, j, it, it->remove);
	}

	/* Don't consider store forwarding if the RTL instruction distance is
	   more than PARAM_STORE_FORWARDING_MAX_DISTANCE and the cost checks
	   are not disabled.  */
	const bool unlimited_cost = (param_store_forwarding_max_distance == 0);
	if (!unlimited_cost && !store_exprs.is_empty ()
	    && (store_exprs[0].insn_cnt
		+ param_store_forwarding_max_distance <= insn_cnt))
	  store_exprs.ordered_remove (0);

	insn_cnt++;
    }
}

/* Update pass statistics.  */

void
store_forwarding_analyzer::update_stats (function *fn)
{
  statistics_counter_event (fn, "Cases of store forwarding detected: ",
			    stats_sf_detected);
  statistics_counter_event (fn, "Cases of store forwarding avoided: ",
			    stats_sf_detected);
}

unsigned int
pass_rtl_avoid_store_forwarding::execute (function *fn)
{
  df_set_flags (DF_DEFER_INSN_RESCAN);

  init_alias_analysis ();

  store_forwarding_analyzer analyzer;

  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    analyzer.avoid_store_forwarding (bb);

  end_alias_analysis ();

  analyzer.update_stats (fn);

  return 0;
}

} // anon namespace.

rtl_opt_pass *
make_pass_rtl_avoid_store_forwarding (gcc::context *ctxt)
{
  return new pass_rtl_avoid_store_forwarding (ctxt);
}
