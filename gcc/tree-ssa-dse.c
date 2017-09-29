/* Dead store elimination
   Copyright (C) 2004-2017 Free Software Foundation, Inc.

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
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "domwalk.h"
#include "tree-cfgcleanup.h"
#include "params.h"
#include "alias.h"

/* This file implements dead store elimination.

   A dead store is a store into a memory location which will later be
   overwritten by another store without any intervening loads.  In this
   case the earlier store can be deleted.

   In our SSA + virtual operand world we use immediate uses of virtual
   operands to detect dead stores.  If a store's virtual definition
   is used precisely once by a later store to the same location which
   post dominates the first store, then the first store is dead.

   The single use of the store's virtual definition ensures that
   there are no intervening aliased loads and the requirement that
   the second load post dominate the first ensures that if the earlier
   store executes, then the later stores will execute before the function
   exits.

   It may help to think of this as first moving the earlier store to
   the point immediately before the later store.  Again, the single
   use of the virtual definition and the post-dominance relationship
   ensure that such movement would be safe.  Clearly if there are
   back to back stores, then the second is redundant.

   Reviewing section 10.7.2 in Morgan's "Building an Optimizing Compiler"
   may also help in understanding this code since it discusses the
   relationship between dead store and redundant load elimination.  In
   fact, they are the same transformation applied to different views of
   the CFG.  */


/* Bitmap of blocks that have had EH statements cleaned.  We should
   remove their dead edges eventually.  */
static bitmap need_eh_cleanup;

/* Return value from dse_classify_store */
enum dse_store_status
{
  DSE_STORE_LIVE,
  DSE_STORE_MAYBE_PARTIAL_DEAD,
  DSE_STORE_DEAD
};

/* STMT is a statement that may write into memory.  Analyze it and
   initialize WRITE to describe how STMT affects memory.

   Return TRUE if the the statement was analyzed, FALSE otherwise.

   It is always safe to return FALSE.  But typically better optimziation
   can be achieved by analyzing more statements.  */

static bool
initialize_ao_ref_for_dse (gimple *stmt, ao_ref *write)
{
  /* It's advantageous to handle certain mem* functions.  */
  if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      switch (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt)))
	{
	  case BUILT_IN_MEMCPY:
	  case BUILT_IN_MEMMOVE:
	  case BUILT_IN_MEMSET:
	    {
	      tree size = NULL_TREE;
	      if (gimple_call_num_args (stmt) == 3)
		size = gimple_call_arg (stmt, 2);
	      tree ptr = gimple_call_arg (stmt, 0);
	      ao_ref_init_from_ptr_and_size (write, ptr, size);
	      return true;
	    }
	  default:
	    break;
	}
    }
  else if (is_gimple_assign (stmt))
    {
      ao_ref_init (write, gimple_assign_lhs (stmt));
      return true;
    }
  return false;
}

/* Given REF from the the alias oracle, return TRUE if it is a valid
   memory reference for dead store elimination, false otherwise.

   In particular, the reference must have a known base, known maximum
   size, start at a byte offset and have a size that is one or more
   bytes.  */

static bool
valid_ao_ref_for_dse (ao_ref *ref)
{
  return (ao_ref_base (ref)
	  && ref->max_size != -1
	  && ref->size != 0
	  && ref->max_size == ref->size
	  && (ref->offset % BITS_PER_UNIT) == 0
	  && (ref->size % BITS_PER_UNIT) == 0
	  && (ref->size != -1));
}

/* Normalize COPY (an ao_ref) relative to REF.  Essentially when we are
   done COPY will only refer bytes found within REF.

   We have already verified that COPY intersects at least one
   byte with REF.  */

static void
normalize_ref (ao_ref *copy, ao_ref *ref)
{
  /* If COPY starts before REF, then reset the beginning of
     COPY to match REF and decrease the size of COPY by the
     number of bytes removed from COPY.  */
  if (copy->offset < ref->offset)
    {
      copy->size -= (ref->offset - copy->offset);
      copy->offset = ref->offset;
    }

  /* If COPY extends beyond REF, chop off its size appropriately.  */
  if (copy->offset + copy->size > ref->offset + ref->size)
    copy->size -= (copy->offset + copy->size - (ref->offset + ref->size));
}

/* Clear any bytes written by STMT from the bitmap LIVE_BYTES.  The base
   address written by STMT must match the one found in REF, which must
   have its base address previously initialized.

   This routine must be conservative.  If we don't know the offset or
   actual size written, assume nothing was written.  */

static void
clear_bytes_written_by (sbitmap live_bytes, gimple *stmt, ao_ref *ref)
{
  ao_ref write;
  if (!initialize_ao_ref_for_dse (stmt, &write))
    return;

  /* Verify we have the same base memory address, the write
     has a known size and overlaps with REF.  */
  if (valid_ao_ref_for_dse (&write)
      && operand_equal_p (write.base, ref->base, OEP_ADDRESS_OF)
      && write.size == write.max_size
      && ((write.offset < ref->offset
	   && write.offset + write.size > ref->offset)
	  || (write.offset >= ref->offset
	      && write.offset < ref->offset + ref->size)))
    {
      normalize_ref (&write, ref);
      bitmap_clear_range (live_bytes,
			  (write.offset - ref->offset) / BITS_PER_UNIT,
			  write.size / BITS_PER_UNIT);
    }
}

/* REF is a memory write.  Extract relevant information from it and
   initialize the LIVE_BYTES bitmap.  If successful, return TRUE.
   Otherwise return FALSE.  */

static bool
setup_live_bytes_from_ref (ao_ref *ref, sbitmap live_bytes)
{
  if (valid_ao_ref_for_dse (ref)
      && (ref->size / BITS_PER_UNIT
	  <= PARAM_VALUE (PARAM_DSE_MAX_OBJECT_SIZE)))
    {
      bitmap_clear (live_bytes);
      bitmap_set_range (live_bytes, 0, ref->size / BITS_PER_UNIT);
      return true;
    }
  return false;
}

/* Compute the number of elements that we can trim from the head and
   tail of ORIG resulting in a bitmap that is a superset of LIVE.

   Store the number of elements trimmed from the head and tail in
   TRIM_HEAD and TRIM_TAIL.

   STMT is the statement being trimmed and is used for debugging dump
   output only.  */

static void
compute_trims (ao_ref *ref, sbitmap live, int *trim_head, int *trim_tail,
	       gimple *stmt)
{
  /* We use sbitmaps biased such that ref->offset is bit zero and the bitmap
     extends through ref->size.  So we know that in the original bitmap
     bits 0..ref->size were true.  We don't actually need the bitmap, just
     the REF to compute the trims.  */

  /* Now identify how much, if any of the tail we can chop off.  */
  int last_orig = (ref->size / BITS_PER_UNIT) - 1;
  int last_live = bitmap_last_set_bit (live);
  *trim_tail = (last_orig - last_live) & ~0x1;

  /* Identify how much, if any of the head we can chop off.  */
  int first_orig = 0;
  int first_live = bitmap_first_set_bit (live);
  *trim_head = (first_live - first_orig) & ~0x1;

  if ((*trim_head || *trim_tail)
      && dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  Trimming statement (head = %d, tail = %d): ",
	       *trim_head, *trim_tail);
      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
      fprintf (dump_file, "\n");
    }
}

/* STMT initializes an object from COMPLEX_CST where one or more of the
   bytes written may be dead stores.  REF is a representation of the
   memory written.  LIVE is the bitmap of stores that are actually live.

   Attempt to rewrite STMT so that only the real or imaginary part of
   the object is actually stored.  */

static void
maybe_trim_complex_store (ao_ref *ref, sbitmap live, gimple *stmt)
{
  int trim_head, trim_tail;
  compute_trims (ref, live, &trim_head, &trim_tail, stmt);

  /* The amount of data trimmed from the head or tail must be at
     least half the size of the object to ensure we're trimming
     the entire real or imaginary half.  By writing things this
     way we avoid more O(n) bitmap operations.  */
  if (trim_tail * 2 >= ref->size / BITS_PER_UNIT)
    {
      /* TREE_REALPART is live */
      tree x = TREE_REALPART (gimple_assign_rhs1 (stmt));
      tree y = gimple_assign_lhs (stmt);
      y = build1 (REALPART_EXPR, TREE_TYPE (x), y);
      gimple_assign_set_lhs (stmt, y);
      gimple_assign_set_rhs1 (stmt, x);
    }
  else if (trim_head * 2 >= ref->size / BITS_PER_UNIT)
    {
      /* TREE_IMAGPART is live */
      tree x = TREE_IMAGPART (gimple_assign_rhs1 (stmt));
      tree y = gimple_assign_lhs (stmt);
      y = build1 (IMAGPART_EXPR, TREE_TYPE (x), y);
      gimple_assign_set_lhs (stmt, y);
      gimple_assign_set_rhs1 (stmt, x);
    }

  /* Other cases indicate parts of both the real and imag subobjects
     are live.  We do not try to optimize those cases.  */
}

/* STMT initializes an object using a CONSTRUCTOR where one or more of the
   bytes written are dead stores.  ORIG is the bitmap of bytes stored by
   STMT.  LIVE is the bitmap of stores that are actually live.

   Attempt to rewrite STMT so that only the real or imaginary part of
   the object is actually stored.

   The most common case for getting here is a CONSTRUCTOR with no elements
   being used to zero initialize an object.  We do not try to handle other
   cases as those would force us to fully cover the object with the
   CONSTRUCTOR node except for the components that are dead.  */

static void
maybe_trim_constructor_store (ao_ref *ref, sbitmap live, gimple *stmt)
{
  tree ctor = gimple_assign_rhs1 (stmt);

  /* This is the only case we currently handle.  It actually seems to
     catch most cases of actual interest.  */
  gcc_assert (CONSTRUCTOR_NELTS (ctor) == 0);

  int head_trim = 0;
  int tail_trim = 0;
  compute_trims (ref, live, &head_trim, &tail_trim, stmt);

  /* Now we want to replace the constructor initializer
     with memset (object + head_trim, 0, size - head_trim - tail_trim).  */
  if (head_trim || tail_trim)
    {
      /* We want &lhs for the MEM_REF expression.  */
      tree lhs_addr = build_fold_addr_expr (gimple_assign_lhs (stmt));

      if (! is_gimple_min_invariant (lhs_addr))
	return;

      /* The number of bytes for the new constructor.  */
      int count = (ref->size / BITS_PER_UNIT) - head_trim - tail_trim;

      /* And the new type for the CONSTRUCTOR.  Essentially it's just
	 a char array large enough to cover the non-trimmed parts of
	 the original CONSTRUCTOR.  Note we want explicit bounds here
	 so that we know how many bytes to clear when expanding the
	 CONSTRUCTOR.  */
      tree type = build_array_type_nelts (char_type_node, count);

      /* Build a suitable alias type rather than using alias set zero
	 to avoid pessimizing.  */
      tree alias_type = reference_alias_ptr_type (gimple_assign_lhs (stmt));

      /* Build a MEM_REF representing the whole accessed area, starting
	 at the first byte not trimmed.  */
      tree exp = fold_build2 (MEM_REF, type, lhs_addr,
			      build_int_cst (alias_type, head_trim));

      /* Now update STMT with a new RHS and LHS.  */
      gimple_assign_set_lhs (stmt, exp);
      gimple_assign_set_rhs1 (stmt, build_constructor (type, NULL));
    }
}

/* STMT is a memcpy, memmove or memset.  Decrement the number of bytes
   copied/set by DECREMENT.  */
static void
decrement_count (gimple *stmt, int decrement)
{
  tree *countp = gimple_call_arg_ptr (stmt, 2);
  gcc_assert (TREE_CODE (*countp) == INTEGER_CST);
  *countp = wide_int_to_tree (TREE_TYPE (*countp), (TREE_INT_CST_LOW (*countp)
						    - decrement));

}

static void
increment_start_addr (gimple *stmt, tree *where, int increment)
{
  if (TREE_CODE (*where) == SSA_NAME)
    {
      tree tem = make_ssa_name (TREE_TYPE (*where));
      gassign *newop
        = gimple_build_assign (tem, POINTER_PLUS_EXPR, *where,
			       build_int_cst (sizetype, increment));
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gsi_insert_before (&gsi, newop, GSI_SAME_STMT);
      *where = tem;
      update_stmt (gsi_stmt (gsi));
      return;
    }

  *where = build_fold_addr_expr (fold_build2 (MEM_REF, char_type_node,
                                             *where,
                                             build_int_cst (ptr_type_node,
                                                            increment)));
}

/* STMT is builtin call that writes bytes in bitmap ORIG, some bytes are dead
   (ORIG & ~NEW) and need not be stored.  Try to rewrite STMT to reduce
   the amount of data it actually writes.

   Right now we only support trimming from the head or the tail of the
   memory region.  In theory we could split the mem* call, but it's
   likely of marginal value.  */

static void
maybe_trim_memstar_call (ao_ref *ref, sbitmap live, gimple *stmt)
{
  switch (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt)))
    {
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMMOVE:
      {
	int head_trim, tail_trim;
	compute_trims (ref, live, &head_trim, &tail_trim, stmt);

	/* Tail trimming is easy, we can just reduce the count.  */
        if (tail_trim)
	  decrement_count (stmt, tail_trim);

	/* Head trimming requires adjusting all the arguments.  */
        if (head_trim)
          {
	    tree *dst = gimple_call_arg_ptr (stmt, 0);
	    increment_start_addr (stmt, dst, head_trim);
	    tree *src = gimple_call_arg_ptr (stmt, 1);
	    increment_start_addr (stmt, src, head_trim);
	    decrement_count (stmt, head_trim);
	  }
        break;
      }

    case BUILT_IN_MEMSET:
      {
	int head_trim, tail_trim;
	compute_trims (ref, live, &head_trim, &tail_trim, stmt);

	/* Tail trimming is easy, we can just reduce the count.  */
        if (tail_trim)
	  decrement_count (stmt, tail_trim);

	/* Head trimming requires adjusting all the arguments.  */
        if (head_trim)
          {
	    tree *dst = gimple_call_arg_ptr (stmt, 0);
	    increment_start_addr (stmt, dst, head_trim);
	    decrement_count (stmt, head_trim);
	  }
	break;
      }

      default:
	break;
    }
}

/* STMT is a memory write where one or more bytes written are dead
   stores.  ORIG is the bitmap of bytes stored by STMT.  LIVE is the
   bitmap of stores that are actually live.

   Attempt to rewrite STMT so that it writes fewer memory locations.  Right
   now we only support trimming at the start or end of the memory region.
   It's not clear how much there is to be gained by trimming from the middle
   of the region.  */

static void
maybe_trim_partially_dead_store (ao_ref *ref, sbitmap live, gimple *stmt)
{
  if (is_gimple_assign (stmt)
      && TREE_CODE (gimple_assign_lhs (stmt)) != TARGET_MEM_REF)
    {
      switch (gimple_assign_rhs_code (stmt))
	{
	case CONSTRUCTOR:
	  maybe_trim_constructor_store (ref, live, stmt);
	  break;
	case COMPLEX_CST:
	  maybe_trim_complex_store (ref, live, stmt);
	  break;
	default:
	  break;
	}
    }
}

/* Return TRUE if USE_REF reads bytes from LIVE where live is
   derived from REF, a write reference.

   While this routine may modify USE_REF, it's passed by value, not
   location.  So callers do not see those modifications.  */

static bool
live_bytes_read (ao_ref use_ref, ao_ref *ref, sbitmap live)
{
  /* We have already verified that USE_REF and REF hit the same object.
     Now verify that there's actually an overlap between USE_REF and REF.  */
  if (ranges_overlap_p (use_ref.offset, use_ref.size, ref->offset, ref->size))
    {
      normalize_ref (&use_ref, ref);

      /* If USE_REF covers all of REF, then it will hit one or more
	 live bytes.   This avoids useless iteration over the bitmap
	 below.  */
      if (use_ref.offset <= ref->offset
	  && use_ref.offset + use_ref.size >= ref->offset + ref->size)
	return true;

      /* Now check if any of the remaining bits in use_ref are set in LIVE.  */
      unsigned int start = (use_ref.offset - ref->offset) / BITS_PER_UNIT;
      unsigned int end  = (use_ref.offset + use_ref.size) / BITS_PER_UNIT;
      return bitmap_bit_in_range_p (live, start, end);
    }
  return true;
}

/* A helper of dse_optimize_stmt.
   Given a GIMPLE_ASSIGN in STMT that writes to REF, find a candidate
   statement *USE_STMT that may prove STMT to be dead.
   Return TRUE if the above conditions are met, otherwise FALSE.  */

static dse_store_status
dse_classify_store (ao_ref *ref, gimple *stmt, gimple **use_stmt,
		    bool byte_tracking_enabled, sbitmap live_bytes)
{
  gimple *temp;
  unsigned cnt = 0;

  *use_stmt = NULL;

  /* Find the first dominated statement that clobbers (part of) the
     memory stmt stores to with no intermediate statement that may use
     part of the memory stmt stores.  That is, find a store that may
     prove stmt to be a dead store.  */
  temp = stmt;
  do
    {
      gimple *use_stmt, *defvar_def;
      imm_use_iterator ui;
      bool fail = false;
      tree defvar;

      /* Limit stmt walking to be linear in the number of possibly
         dead stores.  */
      if (++cnt > 256)
	return DSE_STORE_LIVE;

      if (gimple_code (temp) == GIMPLE_PHI)
	defvar = PHI_RESULT (temp);
      else
	defvar = gimple_vdef (temp);
      defvar_def = temp;
      temp = NULL;
      FOR_EACH_IMM_USE_STMT (use_stmt, ui, defvar)
	{
	  cnt++;

	  /* If we ever reach our DSE candidate stmt again fail.  We
	     cannot handle dead stores in loops.  */
	  if (use_stmt == stmt)
	    {
	      fail = true;
	      BREAK_FROM_IMM_USE_STMT (ui);
	    }
	  /* In simple cases we can look through PHI nodes, but we
	     have to be careful with loops and with memory references
	     containing operands that are also operands of PHI nodes.
	     See gcc.c-torture/execute/20051110-*.c.  */
	  else if (gimple_code (use_stmt) == GIMPLE_PHI)
	    {
	      if (temp
		  /* Make sure we are not in a loop latch block.  */
		  || gimple_bb (stmt) == gimple_bb (use_stmt)
		  || dominated_by_p (CDI_DOMINATORS,
				     gimple_bb (stmt), gimple_bb (use_stmt))
		  /* We can look through PHIs to regions post-dominating
		     the DSE candidate stmt.  */
		  || !dominated_by_p (CDI_POST_DOMINATORS,
				      gimple_bb (stmt), gimple_bb (use_stmt)))
		{
		  fail = true;
		  BREAK_FROM_IMM_USE_STMT (ui);
		}
	      /* Do not consider the PHI as use if it dominates the
	         stmt defining the virtual operand we are processing,
		 we have processed it already in this case.  */
	      if (gimple_bb (defvar_def) != gimple_bb (use_stmt)
		  && !dominated_by_p (CDI_DOMINATORS,
				      gimple_bb (defvar_def),
				      gimple_bb (use_stmt)))
		temp = use_stmt;
	    }
	  /* If the statement is a use the store is not dead.  */
	  else if (ref_maybe_used_by_stmt_p (use_stmt, ref))
	    {
	      /* Handle common cases where we can easily build a ao_ref
		 structure for USE_STMT and in doing so we find that the
		 references hit non-live bytes and thus can be ignored.  */
	      if (live_bytes && (!gimple_vdef (use_stmt) || !temp))
		{
		  if (is_gimple_assign (use_stmt))
		    {
		      /* Other cases were noted as non-aliasing by
			 the call to ref_maybe_used_by_stmt_p.  */
		      ao_ref use_ref;
		      ao_ref_init (&use_ref, gimple_assign_rhs1 (use_stmt));
		      if (valid_ao_ref_for_dse (&use_ref)
			  && use_ref.base == ref->base
			  && use_ref.size == use_ref.max_size
			  && !live_bytes_read (use_ref, ref, live_bytes))
			{
			  /* If this statement has a VDEF, then it is the
			     first store we have seen, so walk through it.  */
			  if (gimple_vdef (use_stmt))
			    temp = use_stmt;
			  continue;
			}
		    }
		}

	      fail = true;
	      BREAK_FROM_IMM_USE_STMT (ui);
	    }
	  /* If this is a store, remember it or bail out if we have
	     multiple ones (the will be in different CFG parts then).  */
	  else if (gimple_vdef (use_stmt))
	    {
	      if (temp)
		{
		  fail = true;
		  BREAK_FROM_IMM_USE_STMT (ui);
		}
	      temp = use_stmt;
	    }
	}

      if (fail)
	{
	  /* STMT might be partially dead and we may be able to reduce
	     how many memory locations it stores into.  */
	  if (byte_tracking_enabled && !gimple_clobber_p (stmt))
	    return DSE_STORE_MAYBE_PARTIAL_DEAD;
	  return DSE_STORE_LIVE;
	}

      /* If we didn't find any definition this means the store is dead
         if it isn't a store to global reachable memory.  In this case
	 just pretend the stmt makes itself dead.  Otherwise fail.  */
      if (!temp)
	{
	  if (ref_may_alias_global_p (ref))
	    return DSE_STORE_LIVE;

	  temp = stmt;
	  break;
	}

      if (byte_tracking_enabled && temp)
	clear_bytes_written_by (live_bytes, temp, ref);
    }
  /* Continue walking until we reach a full kill as a single statement
     or there are no more live bytes.  */
  while (!stmt_kills_ref_p (temp, ref)
	 && !(byte_tracking_enabled && bitmap_empty_p (live_bytes)));

  *use_stmt = temp;
  return DSE_STORE_DEAD;
}


class dse_dom_walker : public dom_walker
{
public:
  dse_dom_walker (cdi_direction direction)
    : dom_walker (direction),
    m_live_bytes (PARAM_VALUE (PARAM_DSE_MAX_OBJECT_SIZE)),
    m_byte_tracking_enabled (false) {}

  virtual edge before_dom_children (basic_block);

private:
  auto_sbitmap m_live_bytes;
  bool m_byte_tracking_enabled;
  void dse_optimize_stmt (gimple_stmt_iterator *);
};

/* Delete a dead call at GSI, which is mem* call of some kind.  */
static void
delete_dead_call (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  Deleted dead call: ");
      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
      fprintf (dump_file, "\n");
    }

  tree lhs = gimple_call_lhs (stmt);
  if (lhs)
    {
      tree ptr = gimple_call_arg (stmt, 0);
      gimple *new_stmt = gimple_build_assign (lhs, ptr);
      unlink_stmt_vdef (stmt);
      if (gsi_replace (gsi, new_stmt, true))
        bitmap_set_bit (need_eh_cleanup, gimple_bb (stmt)->index);
    }
  else
    {
      /* Then we need to fix the operand of the consuming stmt.  */
      unlink_stmt_vdef (stmt);

      /* Remove the dead store.  */
      if (gsi_remove (gsi, true))
	bitmap_set_bit (need_eh_cleanup, gimple_bb (stmt)->index);
      release_defs (stmt);
    }
}

/* Delete a dead store at GSI, which is a gimple assignment. */

static void
delete_dead_assignment (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  Deleted dead store: ");
      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
      fprintf (dump_file, "\n");
    }

  /* Then we need to fix the operand of the consuming stmt.  */
  unlink_stmt_vdef (stmt);

  /* Remove the dead store.  */
  basic_block bb = gimple_bb (stmt);
  if (gsi_remove (gsi, true))
    bitmap_set_bit (need_eh_cleanup, bb->index);

  /* And release any SSA_NAMEs set in this statement back to the
     SSA_NAME manager.  */
  release_defs (stmt);
}

/* Attempt to eliminate dead stores in the statement referenced by BSI.

   A dead store is a store into a memory location which will later be
   overwritten by another store without any intervening loads.  In this
   case the earlier store can be deleted.

   In our SSA + virtual operand world we use immediate uses of virtual
   operands to detect dead stores.  If a store's virtual definition
   is used precisely once by a later store to the same location which
   post dominates the first store, then the first store is dead.  */

void
dse_dom_walker::dse_optimize_stmt (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);

  /* If this statement has no virtual defs, then there is nothing
     to do.  */
  if (!gimple_vdef (stmt))
    return;

  /* Don't return early on *this_2(D) ={v} {CLOBBER}.  */
  if (gimple_has_volatile_ops (stmt)
      && (!gimple_clobber_p (stmt)
	  || TREE_CODE (gimple_assign_lhs (stmt)) != MEM_REF))
    return;

  ao_ref ref;
  if (!initialize_ao_ref_for_dse (stmt, &ref))
    return;

  /* We know we have virtual definitions.  We can handle assignments and
     some builtin calls.  */
  if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      switch (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt)))
	{
	  case BUILT_IN_MEMCPY:
	  case BUILT_IN_MEMMOVE:
	  case BUILT_IN_MEMSET:
	    {
	      /* Occasionally calls with an explicit length of zero
		 show up in the IL.  It's pointless to do analysis
		 on them, they're trivially dead.  */
	      tree size = gimple_call_arg (stmt, 2);
	      if (integer_zerop (size))
		{
		  delete_dead_call (gsi);
		  return;
		}

	      gimple *use_stmt;
	      enum dse_store_status store_status;
	      m_byte_tracking_enabled
		= setup_live_bytes_from_ref (&ref, m_live_bytes);
	      store_status = dse_classify_store (&ref, stmt, &use_stmt,
						 m_byte_tracking_enabled,
						 m_live_bytes);
	      if (store_status == DSE_STORE_LIVE)
		return;

	      if (store_status == DSE_STORE_MAYBE_PARTIAL_DEAD)
		{
		  maybe_trim_memstar_call (&ref, m_live_bytes, stmt);
		  return;
		}

	      if (store_status == DSE_STORE_DEAD)
		delete_dead_call (gsi);
	      return;
	    }

	  default:
	    return;
	}
    }

  if (is_gimple_assign (stmt))
    {
      gimple *use_stmt;

      /* Self-assignments are zombies.  */
      if (operand_equal_p (gimple_assign_rhs1 (stmt),
			   gimple_assign_lhs (stmt), 0))
	use_stmt = stmt;
      else
	{
	  m_byte_tracking_enabled
	    = setup_live_bytes_from_ref (&ref, m_live_bytes);
	  enum dse_store_status store_status;
	  store_status = dse_classify_store (&ref, stmt, &use_stmt,
					     m_byte_tracking_enabled,
					     m_live_bytes);
	  if (store_status == DSE_STORE_LIVE)
	    return;

	  if (store_status == DSE_STORE_MAYBE_PARTIAL_DEAD)
	    {
	      maybe_trim_partially_dead_store (&ref, m_live_bytes, stmt);
	      return;
	    }
	}

      /* Now we know that use_stmt kills the LHS of stmt.  */

      /* But only remove *this_2(D) ={v} {CLOBBER} if killed by
	 another clobber stmt.  */
      if (gimple_clobber_p (stmt)
	  && !gimple_clobber_p (use_stmt))
	return;

      delete_dead_assignment (gsi);
    }
}

edge
dse_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi);)
    {
      dse_optimize_stmt (&gsi);
      if (gsi_end_p (gsi))
	gsi = gsi_last_bb (bb);
      else
	gsi_prev (&gsi);
    }
  return NULL;
}

namespace {

const pass_data pass_data_dse =
{
  GIMPLE_PASS, /* type */
  "dse", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_DSE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_dse : public gimple_opt_pass
{
public:
  pass_dse (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_dse, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_dse (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_dse != 0; }
  virtual unsigned int execute (function *);

}; // class pass_dse

unsigned int
pass_dse::execute (function *fun)
{
  need_eh_cleanup = BITMAP_ALLOC (NULL);

  renumber_gimple_stmt_uids ();

  /* We might consider making this a property of each pass so that it
     can be [re]computed on an as-needed basis.  Particularly since
     this pass could be seen as an extension of DCE which needs post
     dominators.  */
  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);

  /* Dead store elimination is fundamentally a walk of the post-dominator
     tree and a backwards walk of statements within each block.  */
  dse_dom_walker (CDI_POST_DOMINATORS).walk (fun->cfg->x_exit_block_ptr);

  /* Removal of stores may make some EH edges dead.  Purge such edges from
     the CFG as needed.  */
  if (!bitmap_empty_p (need_eh_cleanup))
    {
      gimple_purge_all_dead_eh_edges (need_eh_cleanup);
      cleanup_tree_cfg ();
    }

  BITMAP_FREE (need_eh_cleanup);

  /* For now, just wipe the post-dominator information.  */
  free_dominance_info (CDI_POST_DOMINATORS);
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_dse (gcc::context *ctxt)
{
  return new pass_dse (ctxt);
}
