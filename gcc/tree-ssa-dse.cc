/* Dead and redundant store elimination
   Copyright (C) 2004-2022 Free Software Foundation, Inc.

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
#define INCLUDE_MEMORY
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
#include "tree-cfgcleanup.h"
#include "alias.h"
#include "tree-ssa-loop.h"
#include "tree-ssa-dse.h"
#include "builtins.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "tree-eh.h"
#include "cfganal.h"
#include "cgraph.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"
#include "target.h"
#include "tree-ssa-loop-niter.h"
#include "cfgloop.h"
#include "tree-data-ref.h"

/* This file implements dead store elimination.

   A dead store is a store into a memory location which will later be
   overwritten by another store without any intervening loads.  In this
   case the earlier store can be deleted or trimmed if the store
   was partially dead.

   A redundant store is a store into a memory location which stores
   the exact same value as a prior store to the same memory location.
   While this can often be handled by dead store elimination, removing
   the redundant store is often better than removing or trimming the
   dead store.

   In our SSA + virtual operand world we use immediate uses of virtual
   operands to detect these cases.  If a store's virtual definition
   is used precisely once by a later store to the same location which
   post dominates the first store, then the first store is dead.  If
   the data stored is the same, then the second store is redundant.

   The single use of the store's virtual definition ensures that
   there are no intervening aliased loads and the requirement that
   the second load post dominate the first ensures that if the earlier
   store executes, then the later stores will execute before the function
   exits.

   It may help to think of this as first moving the earlier store to
   the point immediately before the later store.  Again, the single
   use of the virtual definition and the post-dominance relationship
   ensure that such movement would be safe.  Clearly if there are
   back to back stores, then the second is makes the first dead.  If
   the second store stores the same value, then the second store is
   redundant.

   Reviewing section 10.7.2 in Morgan's "Building an Optimizing Compiler"
   may also help in understanding this code since it discusses the
   relationship between dead store and redundant load elimination.  In
   fact, they are the same transformation applied to different views of
   the CFG.  */

static void delete_dead_or_redundant_call (gimple_stmt_iterator *, const char *);

/* Bitmap of blocks that have had EH statements cleaned.  We should
   remove their dead edges eventually.  */
static bitmap need_eh_cleanup;
static bitmap need_ab_cleanup;

/* STMT is a statement that may write into memory.  Analyze it and
   initialize WRITE to describe how STMT affects memory.  When
   MAY_DEF_OK is true then the function initializes WRITE to what
   the stmt may define.

   Return TRUE if the statement was analyzed, FALSE otherwise.

   It is always safe to return FALSE.  But typically better optimziation
   can be achieved by analyzing more statements.  */

static bool
initialize_ao_ref_for_dse (gimple *stmt, ao_ref *write, bool may_def_ok = false)
{
  /* It's advantageous to handle certain mem* functions.  */
  if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      switch (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt)))
	{
	case BUILT_IN_MEMCPY:
	case BUILT_IN_MEMMOVE:
	case BUILT_IN_MEMSET:
	case BUILT_IN_MEMCPY_CHK:
	case BUILT_IN_MEMMOVE_CHK:
	case BUILT_IN_MEMSET_CHK:
	case BUILT_IN_STRNCPY:
	case BUILT_IN_STRNCPY_CHK:
	  {
	    tree size = gimple_call_arg (stmt, 2);
	    tree ptr = gimple_call_arg (stmt, 0);
	    ao_ref_init_from_ptr_and_size (write, ptr, size);
	    return true;
	  }

	/* A calloc call can never be dead, but it can make
	   subsequent stores redundant if they store 0 into
	   the same memory locations.  */
	case BUILT_IN_CALLOC:
	  {
	    tree nelem = gimple_call_arg (stmt, 0);
	    tree selem = gimple_call_arg (stmt, 1);
	    tree lhs;
	    if (TREE_CODE (nelem) == INTEGER_CST
		&& TREE_CODE (selem) == INTEGER_CST
		&& (lhs = gimple_call_lhs (stmt)) != NULL_TREE)
	      {
		tree size = fold_build2 (MULT_EXPR, TREE_TYPE (nelem),
					 nelem, selem);
		ao_ref_init_from_ptr_and_size (write, lhs, size);
		return true;
	      }
	  }

	default:
	  break;
	}
    }
  else if (is_gimple_call (stmt)
	   && gimple_call_internal_p (stmt))
    {
      switch (gimple_call_internal_fn (stmt))
	{
	case IFN_LEN_STORE:
	  ao_ref_init_from_ptr_and_size
	      (write, gimple_call_arg (stmt, 0),
	       int_const_binop (MINUS_EXPR,
				gimple_call_arg (stmt, 2),
				gimple_call_arg (stmt, 4)));
	  return true;
	case IFN_MASK_STORE:
	  /* We cannot initialize a must-def ao_ref (in all cases) but we
	     can provide a may-def variant.  */
	  if (may_def_ok)
	    {
	      ao_ref_init_from_ptr_and_size
		  (write, gimple_call_arg (stmt, 0),
		   TYPE_SIZE_UNIT (TREE_TYPE (gimple_call_arg (stmt, 3))));
	      return true;
	    }
	  break;
	default:;
	}
    }
  else if (tree lhs = gimple_get_lhs (stmt))
    {
      if (TREE_CODE (lhs) != SSA_NAME)
	{
	  ao_ref_init (write, lhs);
	  return true;
	}
    }
  return false;
}

/* Given REF from the alias oracle, return TRUE if it is a valid
   kill memory reference for dead store elimination, false otherwise.

   In particular, the reference must have a known base, known maximum
   size, start at a byte offset and have a size that is one or more
   bytes.  */

static bool
valid_ao_ref_kill_for_dse (ao_ref *ref)
{
  return (ao_ref_base (ref)
	  && known_size_p (ref->max_size)
	  && maybe_ne (ref->size, 0)
	  && known_eq (ref->max_size, ref->size)
	  && known_ge (ref->offset, 0));
}

/* Given REF from the alias oracle, return TRUE if it is a valid
   load or store memory reference for dead store elimination, false otherwise.

   Unlike for valid_ao_ref_kill_for_dse we can accept writes where max_size
   is not same as size since we can handle conservatively the larger range.  */

static bool
valid_ao_ref_for_dse (ao_ref *ref)
{
  return (ao_ref_base (ref)
	  && known_size_p (ref->max_size)
	  && known_ge (ref->offset, 0));
}

/* Initialize OFFSET and SIZE to a range known to contain REF
   where the boundaries are divisible by BITS_PER_UNIT (bit still in bits).
   Return false if this is impossible.  */

static bool
get_byte_aligned_range_containing_ref (ao_ref *ref, poly_int64 *offset,
				       HOST_WIDE_INT *size)
{
  if (!known_size_p (ref->max_size))
    return false;
  *offset = aligned_lower_bound (ref->offset, BITS_PER_UNIT);
  poly_int64 end = aligned_upper_bound (ref->offset + ref->max_size,
					BITS_PER_UNIT);
  return (end - *offset).is_constant (size);
}

/* Initialize OFFSET and SIZE to a range known to be contained REF
   where the boundaries are divisible by BITS_PER_UNIT (but still in bits).
   Return false if this is impossible.  */

static bool
get_byte_aligned_range_contained_in_ref (ao_ref *ref, poly_int64 *offset,
					 HOST_WIDE_INT *size)
{
  if (!known_size_p (ref->size)
      || !known_eq (ref->size, ref->max_size))
    return false;
  *offset = aligned_upper_bound (ref->offset, BITS_PER_UNIT);
  poly_int64 end = aligned_lower_bound (ref->offset + ref->max_size,
					BITS_PER_UNIT);
  /* For bit accesses we can get -1 here, but also 0 sized kill is not
     useful.  */
  if (!known_gt (end, *offset))
    return false;
  return (end - *offset).is_constant (size);
}

/* Compute byte range (returned iN REF_OFFSET and RET_SIZE) for access COPY
   inside REF.  If KILL is true, then COPY represent a kill and the byte range
   needs to be fully contained in bit range given by COPY.  If KILL is false
   then the byte range returned must contain the range of COPY.  */

static bool
get_byte_range (ao_ref *copy, ao_ref *ref, bool kill,
		HOST_WIDE_INT *ret_offset, HOST_WIDE_INT *ret_size)
{
  HOST_WIDE_INT copy_size, ref_size;
  poly_int64 copy_offset, ref_offset;
  HOST_WIDE_INT diff;

  /* First translate from bits to bytes, rounding to bigger or smaller ranges
     as needed.  Kills needs to be always rounded to smaller ranges while
     uses and stores to larger ranges.  */
  if (kill)
    {
      if (!get_byte_aligned_range_contained_in_ref (copy, &copy_offset,
						    &copy_size))
	return false;
    }
  else
    {
      if (!get_byte_aligned_range_containing_ref (copy, &copy_offset,
						  &copy_size))
	return false;
    }

  if (!get_byte_aligned_range_containing_ref (ref, &ref_offset, &ref_size)
      || !ordered_p (copy_offset, ref_offset))
    return false;

  /* Switch sizes from bits to bytes so we do not need to care about
     overflows.  Offset calculation needs to stay in bits until we compute
     the difference and can switch to HOST_WIDE_INT.  */
  copy_size /= BITS_PER_UNIT;
  ref_size /= BITS_PER_UNIT;

  /* If COPY starts before REF, then reset the beginning of
     COPY to match REF and decrease the size of COPY by the
     number of bytes removed from COPY.  */
  if (maybe_lt (copy_offset, ref_offset))
    {
      if (!(ref_offset - copy_offset).is_constant (&diff)
	  || copy_size < diff / BITS_PER_UNIT)
	return false;
      copy_size -= diff / BITS_PER_UNIT;
      copy_offset = ref_offset;
    }

  if (!(copy_offset - ref_offset).is_constant (&diff)
      || ref_size <= diff / BITS_PER_UNIT)
    return false;

  /* If COPY extends beyond REF, chop off its size appropriately.  */
  HOST_WIDE_INT limit = ref_size - diff / BITS_PER_UNIT;

  if (copy_size > limit)
    copy_size = limit;
  *ret_size = copy_size;
  if (!(copy_offset - ref_offset).is_constant (ret_offset))
    return false;
  *ret_offset /= BITS_PER_UNIT;
  return true;
}

/* Update LIVE_BYTES tracking REF for write to WRITE:
   Verify we have the same base memory address, the write
   has a known size and overlaps with REF.  */
static void
clear_live_bytes_for_ref (sbitmap live_bytes, ao_ref *ref, ao_ref *write)
{
  HOST_WIDE_INT start, size;

  if (valid_ao_ref_kill_for_dse (write)
      && operand_equal_p (write->base, ref->base, OEP_ADDRESS_OF)
      && get_byte_range (write, ref, true, &start, &size))
    bitmap_clear_range (live_bytes, start, size);
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

  if (gcall *call = dyn_cast <gcall *> (stmt))
    {
      bool interposed;
      modref_summary *summary = get_modref_function_summary (call, &interposed);

      if (summary && !interposed)
	for (auto kill : summary->kills)
	  if (kill.get_ao_ref (as_a <gcall *> (stmt), &write))
	    clear_live_bytes_for_ref (live_bytes, ref, &write);
    }
  if (!initialize_ao_ref_for_dse (stmt, &write))
    return;

  clear_live_bytes_for_ref (live_bytes, ref, &write);
}

/* REF is a memory write.  Extract relevant information from it and
   initialize the LIVE_BYTES bitmap.  If successful, return TRUE.
   Otherwise return FALSE.  */

static bool
setup_live_bytes_from_ref (ao_ref *ref, sbitmap live_bytes)
{
  HOST_WIDE_INT const_size;
  if (valid_ao_ref_for_dse (ref)
      && ((aligned_upper_bound (ref->offset + ref->max_size, BITS_PER_UNIT)
	   - aligned_lower_bound (ref->offset,
				  BITS_PER_UNIT)).is_constant (&const_size))
      && (const_size / BITS_PER_UNIT <= param_dse_max_object_size)
      && const_size > 1)
    {
      bitmap_clear (live_bytes);
      bitmap_set_range (live_bytes, 0, const_size / BITS_PER_UNIT);
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
  HOST_WIDE_INT const_size;
  int last_live = bitmap_last_set_bit (live);
  if (ref->size.is_constant (&const_size))
    {
      int last_orig = (const_size / BITS_PER_UNIT) - 1;
      /* We can leave inconvenient amounts on the tail as
	 residual handling in mem* and str* functions is usually
	 reasonably efficient.  */
      *trim_tail = last_orig - last_live;

      /* But don't trim away out of bounds accesses, as this defeats
	 proper warnings.

	 We could have a type with no TYPE_SIZE_UNIT or we could have a VLA
	 where TYPE_SIZE_UNIT is not a constant.  */
      if (*trim_tail
	  && TYPE_SIZE_UNIT (TREE_TYPE (ref->base))
	  && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (ref->base))) == INTEGER_CST
	  && compare_tree_int (TYPE_SIZE_UNIT (TREE_TYPE (ref->base)),
			       last_orig) <= 0)
	*trim_tail = 0;
    }
  else
    *trim_tail = 0;

  /* Identify how much, if any of the head we can chop off.  */
  int first_orig = 0;
  int first_live = bitmap_first_set_bit (live);
  *trim_head = first_live - first_orig;

  /* If REF is aligned, try to maintain this alignment if it reduces
     the number of (power-of-two sized aligned) writes to memory.  */
  unsigned int align_bits;
  unsigned HOST_WIDE_INT bitpos;
  if ((*trim_head || *trim_tail)
      && last_live - first_live >= 2
      && ao_ref_alignment (ref, &align_bits, &bitpos)
      && align_bits >= 32
      && bitpos == 0
      && align_bits % BITS_PER_UNIT == 0)
    {
      unsigned int align_units = align_bits / BITS_PER_UNIT;
      if (align_units > 16)
	align_units = 16;
      while ((first_live | (align_units - 1)) > (unsigned int)last_live)
	align_units >>= 1;

      if (*trim_head)
	{
	  unsigned int pos = first_live & (align_units - 1);
	  for (unsigned int i = 1; i <= align_units; i <<= 1)
	    {
	      unsigned int mask = ~(i - 1);
	      unsigned int bytes = align_units - (pos & mask);
	      if (wi::popcount (bytes) <= 1)
		{
		  *trim_head &= mask;
		  break;
		}
	    }
	}

      if (*trim_tail)
	{
	  unsigned int pos = last_live & (align_units - 1);
	  for (unsigned int i = 1; i <= align_units; i <<= 1)
	    {
	      int mask = i - 1;
	      unsigned int bytes = (pos | mask) + 1;
	      if ((last_live | mask) > (last_live + *trim_tail))
		break;
	      if (wi::popcount (bytes) <= 1)
		{
		  unsigned int extra = (last_live | mask) - last_live;
		  *trim_tail -= extra;
		  break;
		}
	    }
	}
    }

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
  if (known_ge (trim_tail * 2 * BITS_PER_UNIT, ref->size))
    {
      /* TREE_REALPART is live */
      tree x = TREE_REALPART (gimple_assign_rhs1 (stmt));
      tree y = gimple_assign_lhs (stmt);
      y = build1 (REALPART_EXPR, TREE_TYPE (x), y);
      gimple_assign_set_lhs (stmt, y);
      gimple_assign_set_rhs1 (stmt, x);
    }
  else if (known_ge (trim_head * 2 * BITS_PER_UNIT, ref->size))
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
      poly_int64 ref_bytes = exact_div (ref->size, BITS_PER_UNIT);
      poly_int64 count = ref_bytes - head_trim - tail_trim;

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
  if (tree lhs = gimple_call_lhs (stmt))
    if (where == gimple_call_arg_ptr (stmt, 0))
      {
	gassign *newop = gimple_build_assign (lhs, unshare_expr (*where));
	gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	gsi_insert_after (&gsi, newop, GSI_SAME_STMT);
	gimple_call_set_lhs (stmt, NULL_TREE);
	update_stmt (stmt);
      }

  if (TREE_CODE (*where) == SSA_NAME)
    {
      tree tem = make_ssa_name (TREE_TYPE (*where));
      gassign *newop
	= gimple_build_assign (tem, POINTER_PLUS_EXPR, *where,
			       build_int_cst (sizetype, increment));
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gsi_insert_before (&gsi, newop, GSI_SAME_STMT);
      *where = tem;
      update_stmt (stmt);
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
  int head_trim, tail_trim;
  switch (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt)))
    {
    case BUILT_IN_STRNCPY:
    case BUILT_IN_STRNCPY_CHK:
      compute_trims (ref, live, &head_trim, &tail_trim, stmt);
      if (head_trim)
	{
	  /* Head trimming of strncpy is only possible if we can
	     prove all bytes we would trim are non-zero (or we could
	     turn the strncpy into memset if there must be zero
	     among the head trimmed bytes).  If we don't know anything
	     about those bytes, the presence or absence of '\0' bytes
	     in there will affect whether it acts for the non-trimmed
	     bytes as memset or memcpy/strncpy.  */
	  c_strlen_data lendata = { };
	  int orig_head_trim = head_trim;
	  tree srcstr = gimple_call_arg (stmt, 1);
	  if (!get_range_strlen (srcstr, &lendata, /*eltsize=*/1)
	      || !tree_fits_uhwi_p (lendata.minlen))
	    head_trim = 0;
	  else if (tree_to_uhwi (lendata.minlen) < (unsigned) head_trim)
	    {
	      head_trim = tree_to_uhwi (lendata.minlen);
	      if ((orig_head_trim & (UNITS_PER_WORD - 1)) == 0)
		head_trim &= ~(UNITS_PER_WORD - 1);
	    }
	  if (orig_head_trim != head_trim
	      && dump_file
	      && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "  Adjusting strncpy trimming to (head = %d,"
		     " tail = %d)\n", head_trim, tail_trim);
	}
      goto do_memcpy;

    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMMOVE:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
      compute_trims (ref, live, &head_trim, &tail_trim, stmt);

    do_memcpy:
      /* Tail trimming is easy, we can just reduce the count.  */
      if (tail_trim)
	decrement_count (stmt, tail_trim);

      /* Head trimming requires adjusting all the arguments.  */
      if (head_trim)
	{
	  /* For __*_chk need to adjust also the last argument.  */
	  if (gimple_call_num_args (stmt) == 4)
	    {
	      tree size = gimple_call_arg (stmt, 3);
	      if (!tree_fits_uhwi_p (size))
		break;
	      if (!integer_all_onesp (size))
		{
		  unsigned HOST_WIDE_INT sz = tree_to_uhwi (size);
		  if (sz < (unsigned) head_trim)
		    break;
		  tree arg = wide_int_to_tree (TREE_TYPE (size),
					       sz - head_trim);
		  gimple_call_set_arg (stmt, 3, arg);
		}
	    }
	  tree *dst = gimple_call_arg_ptr (stmt, 0);
	  increment_start_addr (stmt, dst, head_trim);
	  tree *src = gimple_call_arg_ptr (stmt, 1);
	  increment_start_addr (stmt, src, head_trim);
	  decrement_count (stmt, head_trim);
	}
      break;

    case BUILT_IN_MEMSET:
    case BUILT_IN_MEMSET_CHK:
      compute_trims (ref, live, &head_trim, &tail_trim, stmt);

      /* Tail trimming is easy, we can just reduce the count.  */
      if (tail_trim)
	decrement_count (stmt, tail_trim);

      /* Head trimming requires adjusting all the arguments.  */
      if (head_trim)
	{
	  /* For __*_chk need to adjust also the last argument.  */
	  if (gimple_call_num_args (stmt) == 4)
	    {
	      tree size = gimple_call_arg (stmt, 3);
	      if (!tree_fits_uhwi_p (size))
		break;
	      if (!integer_all_onesp (size))
		{
		  unsigned HOST_WIDE_INT sz = tree_to_uhwi (size);
		  if (sz < (unsigned) head_trim)
		    break;
		  tree arg = wide_int_to_tree (TREE_TYPE (size),
					       sz - head_trim);
		  gimple_call_set_arg (stmt, 3, arg);
		}
	    }
	  tree *dst = gimple_call_arg_ptr (stmt, 0);
	  increment_start_addr (stmt, dst, head_trim);
	  decrement_count (stmt, head_trim);
	}
      break;

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
live_bytes_read (ao_ref *use_ref, ao_ref *ref, sbitmap live)
{
  /* We have already verified that USE_REF and REF hit the same object.
     Now verify that there's actually an overlap between USE_REF and REF.  */
  HOST_WIDE_INT start, size;
  if (get_byte_range (use_ref, ref, false, &start, &size))
    {
      /* If USE_REF covers all of REF, then it will hit one or more
	 live bytes.   This avoids useless iteration over the bitmap
	 below.  */
      if (start == 0 && known_eq (size * 8, ref->size))
	return true;

      /* Now check if any of the remaining bits in use_ref are set in LIVE.  */
      return bitmap_bit_in_range_p (live, start, (start + size - 1));
    }
  return true;
}

/* Callback for dse_classify_store calling for_each_index.  Verify that
   indices are invariant in the loop with backedge PHI in basic-block DATA.  */

static bool
check_name (tree, tree *idx, void *data)
{
  basic_block phi_bb = (basic_block) data;
  if (TREE_CODE (*idx) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (*idx)
      && dominated_by_p (CDI_DOMINATORS, gimple_bb (SSA_NAME_DEF_STMT (*idx)),
			 phi_bb))
    return false;
  return true;
}

/* STMT stores the value 0 into one or more memory locations
   (via memset, empty constructor, calloc call, etc).

   See if there is a subsequent store of the value 0 to one
   or more of the same memory location(s).  If so, the subsequent
   store is redundant and can be removed.

   The subsequent stores could be via memset, empty constructors,
   simple MEM stores, etc.  */

static void
dse_optimize_redundant_stores (gimple *stmt)
{
  int cnt = 0;

  /* TBAA state of STMT, if it is a call it is effectively alias-set zero.  */
  alias_set_type earlier_set = 0;
  alias_set_type earlier_base_set = 0;
  if (is_gimple_assign (stmt))
    {
      ao_ref lhs_ref;
      ao_ref_init (&lhs_ref, gimple_assign_lhs (stmt));
      earlier_set = ao_ref_alias_set (&lhs_ref);
      earlier_base_set = ao_ref_base_alias_set (&lhs_ref);
    }

  /* We could do something fairly complex and look through PHIs
     like DSE_CLASSIFY_STORE, but it doesn't seem to be worth
     the effort.

     Look at all the immediate uses of the VDEF (which are obviously
     dominated by STMT).   See if one or more stores 0 into the same
     memory locations a STMT, if so remove the immediate use statements.  */
  tree defvar = gimple_vdef (stmt);
  imm_use_iterator ui;
  gimple *use_stmt;
  FOR_EACH_IMM_USE_STMT (use_stmt, ui, defvar)
    {
      /* Limit stmt walking.  */
      if (++cnt > param_dse_max_alias_queries_per_store)
	break;

      /* If USE_STMT stores 0 into one or more of the same locations
	 as STMT and STMT would kill USE_STMT, then we can just remove
	 USE_STMT.  */
      tree fndecl;
      if ((is_gimple_assign (use_stmt)
	   && gimple_vdef (use_stmt)
	   && (gimple_assign_single_p (use_stmt)
	       && initializer_zerop (gimple_assign_rhs1 (use_stmt))))
	  || (gimple_call_builtin_p (use_stmt, BUILT_IN_NORMAL)
	      && (fndecl = gimple_call_fndecl (use_stmt)) != NULL
	      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMSET
		  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMSET_CHK)
	      && integer_zerop (gimple_call_arg (use_stmt, 1))))
	{
	  ao_ref write;

	  if (!initialize_ao_ref_for_dse (use_stmt, &write))
	    break;

	  if (valid_ao_ref_for_dse (&write)
	      && stmt_kills_ref_p (stmt, &write))
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	      if (is_gimple_assign (use_stmt))
		{
		  ao_ref lhs_ref;
		  ao_ref_init (&lhs_ref, gimple_assign_lhs (use_stmt));
		  if ((earlier_set == ao_ref_alias_set (&lhs_ref)
		       || alias_set_subset_of (ao_ref_alias_set (&lhs_ref),
					       earlier_set))
		      && (earlier_base_set == ao_ref_base_alias_set (&lhs_ref)
			  || alias_set_subset_of
			       (ao_ref_base_alias_set (&lhs_ref),
						  earlier_base_set)))
		    delete_dead_or_redundant_assignment (&gsi, "redundant",
							 need_eh_cleanup,
							 need_ab_cleanup);
		}
	      else if (is_gimple_call (use_stmt))
		{
		  if ((earlier_set == 0
		       || alias_set_subset_of (0, earlier_set))
		      && (earlier_base_set == 0
			  || alias_set_subset_of (0, earlier_base_set)))
		  delete_dead_or_redundant_call (&gsi, "redundant");
		}
	      else
		gcc_unreachable ();
	    }
	}
    }
}

/* Return whether PHI contains ARG as an argument.  */

static bool
contains_phi_arg (gphi *phi, tree arg)
{
  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
    if (gimple_phi_arg_def (phi, i) == arg)
      return true;
  return false;
}

/* Hash map of the memory use in a GIMPLE assignment to its
   data reference.  If NULL data-ref analysis isn't used.  */
static hash_map<gimple *, data_reference_p> *dse_stmt_to_dr_map;

/* A helper of dse_optimize_stmt.
   Given a GIMPLE_ASSIGN in STMT that writes to REF, classify it
   according to downstream uses and defs.  Sets *BY_CLOBBER_P to true
   if only clobber statements influenced the classification result.
   Returns the classification.  */

dse_store_status
dse_classify_store (ao_ref *ref, gimple *stmt,
		    bool byte_tracking_enabled, sbitmap live_bytes,
		    bool *by_clobber_p, tree stop_at_vuse)
{
  gimple *temp;
  int cnt = 0;
  auto_bitmap visited;
  std::unique_ptr<data_reference, void(*)(data_reference_p)>
    dra (nullptr, free_data_ref);

  if (by_clobber_p)
    *by_clobber_p = true;

  /* Find the first dominated statement that clobbers (part of) the
     memory stmt stores to with no intermediate statement that may use
     part of the memory stmt stores.  That is, find a store that may
     prove stmt to be a dead store.  */
  temp = stmt;
  do
    {
      gimple *use_stmt;
      imm_use_iterator ui;
      bool fail = false;
      tree defvar;

      if (gimple_code (temp) == GIMPLE_PHI)
	{
	  defvar = PHI_RESULT (temp);
	  bitmap_set_bit (visited, SSA_NAME_VERSION (defvar));
	}
      else
	defvar = gimple_vdef (temp);

      /* If we're instructed to stop walking at region boundary, do so.  */
      if (defvar == stop_at_vuse)
	return DSE_STORE_LIVE;

      auto_vec<gimple *, 10> defs;
      gphi *first_phi_def = NULL;
      gphi *last_phi_def = NULL;
      FOR_EACH_IMM_USE_STMT (use_stmt, ui, defvar)
	{
	  /* Limit stmt walking.  */
	  if (++cnt > param_dse_max_alias_queries_per_store)
	    {
	      fail = true;
	      break;
	    }

	  /* In simple cases we can look through PHI nodes, but we
	     have to be careful with loops and with memory references
	     containing operands that are also operands of PHI nodes.
	     See gcc.c-torture/execute/20051110-*.c.  */
	  if (gimple_code (use_stmt) == GIMPLE_PHI)
	    {
	      /* If we already visited this PHI ignore it for further
		 processing.  */
	      if (!bitmap_bit_p (visited,
				 SSA_NAME_VERSION (PHI_RESULT (use_stmt))))
		{
		  /* If we visit this PHI by following a backedge then we have
		     to make sure ref->ref only refers to SSA names that are
		     invariant with respect to the loop represented by this
		     PHI node.  */
		  if (dominated_by_p (CDI_DOMINATORS, gimple_bb (stmt),
				      gimple_bb (use_stmt))
		      && !for_each_index (ref->ref ? &ref->ref : &ref->base,
					  check_name, gimple_bb (use_stmt)))
		    return DSE_STORE_LIVE;
		  defs.safe_push (use_stmt);
		  if (!first_phi_def)
		    first_phi_def = as_a <gphi *> (use_stmt);
		  last_phi_def = as_a <gphi *> (use_stmt);
		}
	    }
	  /* If the statement is a use the store is not dead.  */
	  else if (ref_maybe_used_by_stmt_p (use_stmt, ref))
	    {
	      if (dse_stmt_to_dr_map
		  && ref->ref
		  && is_gimple_assign (use_stmt))
		{
		  if (!dra)
		    dra.reset (create_data_ref (NULL, NULL, ref->ref, stmt,
						false, false));
		  bool existed_p;
		  data_reference_p &drb
		    = dse_stmt_to_dr_map->get_or_insert (use_stmt, &existed_p);
		  if (!existed_p)
		    drb = create_data_ref (NULL, NULL,
					   gimple_assign_rhs1 (use_stmt),
					   use_stmt, false, false);
		  if (!dr_may_alias_p (dra.get (), drb, NULL))
		    {
		      if (gimple_vdef (use_stmt))
			defs.safe_push (use_stmt);
		      continue;
		    }
		}

	      /* Handle common cases where we can easily build an ao_ref
		 structure for USE_STMT and in doing so we find that the
		 references hit non-live bytes and thus can be ignored.

		 TODO: We can also use modref summary to handle calls.  */
	      if (byte_tracking_enabled
		  && is_gimple_assign (use_stmt))
		{
		  ao_ref use_ref;
		  ao_ref_init (&use_ref, gimple_assign_rhs1 (use_stmt));
		  if (valid_ao_ref_for_dse (&use_ref)
		      && operand_equal_p (use_ref.base, ref->base,
					  OEP_ADDRESS_OF)
		      && !live_bytes_read (&use_ref, ref, live_bytes))
		    {
		      /* If this is a store, remember it as we possibly
			 need to walk the defs uses.  */
		      if (gimple_vdef (use_stmt))
			defs.safe_push (use_stmt);
		      continue;
		    }
		}

	      fail = true;
	      break;
	    }
	  /* We have visited ourselves already so ignore STMT for the
	     purpose of chaining.  */
	  else if (use_stmt == stmt)
	    ;
	  /* If this is a store, remember it as we possibly need to walk the
	     defs uses.  */
	  else if (gimple_vdef (use_stmt))
	    defs.safe_push (use_stmt);
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
      if (defs.is_empty ())
	{
	  if (ref_may_alias_global_p (ref, false))
	    return DSE_STORE_LIVE;

	  if (by_clobber_p)
	    *by_clobber_p = false;
	  return DSE_STORE_DEAD;
	}

      /* Process defs and remove those we need not process further.  */
      for (unsigned i = 0; i < defs.length ();)
	{
	  gimple *def = defs[i];
	  gimple *use_stmt;
	  use_operand_p use_p;
	  tree vdef = (gimple_code (def) == GIMPLE_PHI
		       ? gimple_phi_result (def) : gimple_vdef (def));
	  gphi *phi_def;
	  /* If the path to check starts with a kill we do not need to
	     process it further.
	     ???  With byte tracking we need only kill the bytes currently
	     live.  */
	  if (stmt_kills_ref_p (def, ref))
	    {
	      if (by_clobber_p && !gimple_clobber_p (def))
		*by_clobber_p = false;
	      defs.unordered_remove (i);
	    }
	  /* If the path ends here we do not need to process it further.
	     This for example happens with calls to noreturn functions.  */
	  else if (has_zero_uses (vdef))
	    {
	      /* But if the store is to global memory it is definitely
		 not dead.  */
	      if (ref_may_alias_global_p (ref, false))
		return DSE_STORE_LIVE;
	      defs.unordered_remove (i);
	    }
	  /* In addition to kills we can remove defs whose only use
	     is another def in defs.  That can only ever be PHIs of which
	     we track two for simplicity reasons, the first and last in
	     {first,last}_phi_def (we fail for multiple PHIs anyways).
	     We can also ignore defs that feed only into
	     already visited PHIs.  */
	  else if (single_imm_use (vdef, &use_p, &use_stmt)
		   && (use_stmt == first_phi_def
		       || use_stmt == last_phi_def
		       || (gimple_code (use_stmt) == GIMPLE_PHI
			   && bitmap_bit_p (visited,
					    SSA_NAME_VERSION
					      (PHI_RESULT (use_stmt))))))
	    {
	      defs.unordered_remove (i);
	      if (def == first_phi_def)
		first_phi_def = NULL;
	      else if (def == last_phi_def)
		last_phi_def = NULL;
	    }
	  /* If def is a PHI and one of its arguments is another PHI node still
	     in consideration we can defer processing it.  */
	  else if ((phi_def = dyn_cast <gphi *> (def))
		   && ((last_phi_def
			&& phi_def != last_phi_def
			&& contains_phi_arg (phi_def,
					     gimple_phi_result (last_phi_def)))
		       || (first_phi_def
			   && phi_def != first_phi_def
			   && contains_phi_arg
				(phi_def, gimple_phi_result (first_phi_def)))))
	    {
	      defs.unordered_remove (i);
	      if (phi_def == first_phi_def)
		first_phi_def = NULL;
	      else if (phi_def == last_phi_def)
		last_phi_def = NULL;
	    }
	  else
	    ++i;
	}

      /* If all defs kill the ref we are done.  */
      if (defs.is_empty ())
	return DSE_STORE_DEAD;
      /* If more than one def survives fail.  */
      if (defs.length () > 1)
	{
	  /* STMT might be partially dead and we may be able to reduce
	     how many memory locations it stores into.  */
	  if (byte_tracking_enabled && !gimple_clobber_p (stmt))
	    return DSE_STORE_MAYBE_PARTIAL_DEAD;
	  return DSE_STORE_LIVE;
	}
      temp = defs[0];

      /* Track partial kills.  */
      if (byte_tracking_enabled)
	{
	  clear_bytes_written_by (live_bytes, temp, ref);
	  if (bitmap_empty_p (live_bytes))
	    {
	      if (by_clobber_p && !gimple_clobber_p (temp))
		*by_clobber_p = false;
	      return DSE_STORE_DEAD;
	    }
	}
    }
  /* Continue walking until there are no more live bytes.  */
  while (1);
}


/* Delete a dead call at GSI, which is mem* call of some kind.  */
static void
delete_dead_or_redundant_call (gimple_stmt_iterator *gsi, const char *type)
{
  gimple *stmt = gsi_stmt (*gsi);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  Deleted %s call: ", type);
      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
      fprintf (dump_file, "\n");
    }

  basic_block bb = gimple_bb (stmt);
  tree lhs = gimple_call_lhs (stmt);
  if (lhs)
    {
      tree ptr = gimple_call_arg (stmt, 0);
      gimple *new_stmt = gimple_build_assign (lhs, ptr);
      unlink_stmt_vdef (stmt);
      if (gsi_replace (gsi, new_stmt, true))
	bitmap_set_bit (need_eh_cleanup, bb->index);
    }
  else
    {
      /* Then we need to fix the operand of the consuming stmt.  */
      unlink_stmt_vdef (stmt);

      /* Remove the dead store.  */
      if (gsi_remove (gsi, true))
	bitmap_set_bit (need_eh_cleanup, bb->index);
      release_defs (stmt);
    }
}

/* Delete a dead store at GSI, which is a gimple assignment. */

void
delete_dead_or_redundant_assignment (gimple_stmt_iterator *gsi,
				     const char *type,
				     bitmap need_eh_cleanup,
				     bitmap need_ab_cleanup)
{
  gimple *stmt = gsi_stmt (*gsi);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  Deleted %s store: ", type);
      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
      fprintf (dump_file, "\n");
    }

  /* Then we need to fix the operand of the consuming stmt.  */
  unlink_stmt_vdef (stmt);

  /* Remove the dead store.  */
  basic_block bb = gimple_bb (stmt);
  if (need_ab_cleanup && stmt_can_make_abnormal_goto (stmt))
    bitmap_set_bit (need_ab_cleanup, bb->index);
  if (gsi_remove (gsi, true) && need_eh_cleanup)
    bitmap_set_bit (need_eh_cleanup, bb->index);

  /* And release any SSA_NAMEs set in this statement back to the
     SSA_NAME manager.  */
  release_defs (stmt);
}

/* Try to prove, using modref summary, that all memory written to by a call is
   dead and remove it.  Assume that if return value is written to memory
   it is already proved to be dead.  */

static bool
dse_optimize_call (gimple_stmt_iterator *gsi, sbitmap live_bytes)
{
  gcall *stmt = dyn_cast <gcall *> (gsi_stmt (*gsi));

  if (!stmt)
    return false;

  tree callee = gimple_call_fndecl (stmt);

  if (!callee)
    return false;

  /* Pure/const functions are optimized by normal DCE
     or handled as store above.  */
  int flags = gimple_call_flags (stmt);
  if ((flags & (ECF_PURE|ECF_CONST|ECF_NOVOPS))
      && !(flags & (ECF_LOOPING_CONST_OR_PURE)))
    return false;

  cgraph_node *node = cgraph_node::get (callee);
  if (!node)
    return false;

  if (stmt_could_throw_p (cfun, stmt)
      && !cfun->can_delete_dead_exceptions)
    return false;

  /* If return value is used the call is not dead.  */
  tree lhs = gimple_call_lhs (stmt);
  if (lhs && TREE_CODE (lhs) == SSA_NAME)
    {
      imm_use_iterator ui;
      gimple *use_stmt;
      FOR_EACH_IMM_USE_STMT (use_stmt, ui, lhs)
	if (!is_gimple_debug (use_stmt))
	  return false;
    }

  /* Verify that there are no side-effects except for return value
     and memory writes tracked by modref.  */
  modref_summary *summary = get_modref_function_summary (node);
  if (!summary || !summary->try_dse)
    return false;

  bool by_clobber_p = false;

  /* Walk all memory writes and verify that they are dead.  */
  for (auto base_node : summary->stores->bases)
    for (auto ref_node : base_node->refs)
      for (auto access_node : ref_node->accesses)
	{
	  tree arg = access_node.get_call_arg (stmt);

	  if (!arg || !POINTER_TYPE_P (TREE_TYPE (arg)))
	    return false;

	  if (integer_zerop (arg)
	      && !targetm.addr_space.zero_address_valid
		    (TYPE_ADDR_SPACE (TREE_TYPE (arg))))
	    continue;

	  ao_ref ref;

	  if (!access_node.get_ao_ref (stmt, &ref))
	    return false;
	  ref.ref_alias_set = ref_node->ref;
	  ref.base_alias_set = base_node->base;

	  bool byte_tracking_enabled
	      = setup_live_bytes_from_ref (&ref, live_bytes);
	  enum dse_store_status store_status;

	  store_status = dse_classify_store (&ref, stmt,
					     byte_tracking_enabled,
					     live_bytes, &by_clobber_p);
	  if (store_status != DSE_STORE_DEAD)
	    return false;
	}
  delete_dead_or_redundant_assignment (gsi, "dead", need_eh_cleanup,
				       need_ab_cleanup);
  return true;
}

/* Attempt to eliminate dead stores in the statement referenced by BSI.

   A dead store is a store into a memory location which will later be
   overwritten by another store without any intervening loads.  In this
   case the earlier store can be deleted.

   In our SSA + virtual operand world we use immediate uses of virtual
   operands to detect dead stores.  If a store's virtual definition
   is used precisely once by a later store to the same location which
   post dominates the first store, then the first store is dead.  */

static void
dse_optimize_stmt (function *fun, gimple_stmt_iterator *gsi, sbitmap live_bytes)
{
  gimple *stmt = gsi_stmt (*gsi);

  /* Don't return early on *this_2(D) ={v} {CLOBBER}.  */
  if (gimple_has_volatile_ops (stmt)
      && (!gimple_clobber_p (stmt)
	  || TREE_CODE (gimple_assign_lhs (stmt)) != MEM_REF))
    return;

  ao_ref ref;
  /* If this is not a store we can still remove dead call using
     modref summary.  Note we specifically allow ref to be initialized
     to a conservative may-def since we are looking for followup stores
     to kill all of it.  */
  if (!initialize_ao_ref_for_dse (stmt, &ref, true))
    {
      dse_optimize_call (gsi, live_bytes);
      return;
    }

  /* We know we have virtual definitions.  We can handle assignments and
     some builtin calls.  */
  if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      tree fndecl = gimple_call_fndecl (stmt);
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_MEMCPY:
	case BUILT_IN_MEMMOVE:
	case BUILT_IN_STRNCPY:
	case BUILT_IN_MEMSET:
	case BUILT_IN_MEMCPY_CHK:
	case BUILT_IN_MEMMOVE_CHK:
	case BUILT_IN_STRNCPY_CHK:
	case BUILT_IN_MEMSET_CHK:
	  {
	    /* Occasionally calls with an explicit length of zero
	       show up in the IL.  It's pointless to do analysis
	       on them, they're trivially dead.  */
	    tree size = gimple_call_arg (stmt, 2);
	    if (integer_zerop (size))
	      {
		delete_dead_or_redundant_call (gsi, "dead");
		return;
	      }

	    /* If this is a memset call that initializes an object
	       to zero, it may be redundant with an earlier memset
	       or empty CONSTRUCTOR of a larger object.  */
	    if ((DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMSET
		 || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMSET_CHK)
		&& integer_zerop (gimple_call_arg (stmt, 1)))
	      dse_optimize_redundant_stores (stmt);

	    enum dse_store_status store_status;
	    bool byte_tracking_enabled
	      = setup_live_bytes_from_ref (&ref, live_bytes);
	    store_status = dse_classify_store (&ref, stmt,
					       byte_tracking_enabled,
					       live_bytes);
	    if (store_status == DSE_STORE_LIVE)
	      return;

	    if (store_status == DSE_STORE_MAYBE_PARTIAL_DEAD)
	      {
		maybe_trim_memstar_call (&ref, live_bytes, stmt);
		return;
	      }

	    if (store_status == DSE_STORE_DEAD)
	      delete_dead_or_redundant_call (gsi, "dead");
	    return;
	  }

	case BUILT_IN_CALLOC:
	  /* We already know the arguments are integer constants.  */
	  dse_optimize_redundant_stores (stmt);
	  return;

	default:
	  return;
	}
    }
  else if (is_gimple_call (stmt)
	   && gimple_call_internal_p (stmt))
    {
      switch (gimple_call_internal_fn (stmt))
	{
	case IFN_LEN_STORE:
	case IFN_MASK_STORE:
	  {
	    enum dse_store_status store_status;
	    store_status = dse_classify_store (&ref, stmt, false, live_bytes);
	    if (store_status == DSE_STORE_DEAD)
	      delete_dead_or_redundant_call (gsi, "dead");
	    return;
	  }
	default:;
	}
    }

  bool by_clobber_p = false;

  /* Check if this statement stores zero to a memory location,
     and if there is a subsequent store of zero to the same
     memory location.  If so, remove the subsequent store.  */
  if (gimple_assign_single_p (stmt)
      && initializer_zerop (gimple_assign_rhs1 (stmt)))
    dse_optimize_redundant_stores (stmt);

  /* Self-assignments are zombies.  */
  if (is_gimple_assign (stmt)
      && operand_equal_p (gimple_assign_rhs1 (stmt),
			  gimple_assign_lhs (stmt), 0))
    ;
  else
    {
      bool byte_tracking_enabled
	  = setup_live_bytes_from_ref (&ref, live_bytes);
      enum dse_store_status store_status;
      store_status = dse_classify_store (&ref, stmt,
					 byte_tracking_enabled,
					 live_bytes, &by_clobber_p);
      if (store_status == DSE_STORE_LIVE)
	return;

      if (store_status == DSE_STORE_MAYBE_PARTIAL_DEAD)
	{
	  maybe_trim_partially_dead_store (&ref, live_bytes, stmt);
	  return;
	}
    }

  /* Now we know that use_stmt kills the LHS of stmt.  */

  /* But only remove *this_2(D) ={v} {CLOBBER} if killed by
     another clobber stmt.  */
  if (gimple_clobber_p (stmt)
      && !by_clobber_p)
    return;

  if (is_gimple_call (stmt)
      && (gimple_has_side_effects (stmt)
	  || (stmt_could_throw_p (fun, stmt)
	      && !fun->can_delete_dead_exceptions)))
    {
      /* See if we can remove complete call.  */
      if (dse_optimize_call (gsi, live_bytes))
	return;
      /* Make sure we do not remove a return slot we cannot reconstruct
	 later.  */
      if (gimple_call_return_slot_opt_p (as_a <gcall *>(stmt))
	  && (TREE_ADDRESSABLE (TREE_TYPE (gimple_call_fntype (stmt)))
	      || !poly_int_tree_p
		    (TYPE_SIZE (TREE_TYPE (gimple_call_fntype (stmt))))))
	return;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Deleted dead store in call LHS: ");
	  print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	  fprintf (dump_file, "\n");
	}
      gimple_call_set_lhs (stmt, NULL_TREE);
      update_stmt (stmt);
    }
  else if (!stmt_could_throw_p (fun, stmt)
	   || fun->can_delete_dead_exceptions)
    delete_dead_or_redundant_assignment (gsi, "dead", need_eh_cleanup,
					 need_ab_cleanup);
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
    : gimple_opt_pass (pass_data_dse, ctxt), use_dr_analysis_p (false)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_dse (m_ctxt); }
  void set_pass_param (unsigned n, bool param) final override
    {
      gcc_assert (n == 0);
      use_dr_analysis_p = param;
    }
  bool gate (function *) final override { return flag_tree_dse != 0; }
  unsigned int execute (function *) final override;

private:
  bool use_dr_analysis_p;
}; // class pass_dse

unsigned int
pass_dse::execute (function *fun)
{
  unsigned todo = 0;
  bool released_def = false;

  need_eh_cleanup = BITMAP_ALLOC (NULL);
  need_ab_cleanup = BITMAP_ALLOC (NULL);
  auto_sbitmap live_bytes (param_dse_max_object_size);
  if (flag_expensive_optimizations && use_dr_analysis_p)
    dse_stmt_to_dr_map = new hash_map<gimple *, data_reference_p>;

  renumber_gimple_stmt_uids (fun);

  calculate_dominance_info (CDI_DOMINATORS);

  /* Dead store elimination is fundamentally a reverse program order walk.  */
  int *rpo = XNEWVEC (int, n_basic_blocks_for_fn (fun) - NUM_FIXED_BLOCKS);
  int n = pre_and_rev_post_order_compute_fn (fun, NULL, rpo, false);
  for (int i = n; i != 0; --i)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (fun, rpo[i-1]);
      gimple_stmt_iterator gsi;

      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);

	  if (gimple_vdef (stmt))
	    dse_optimize_stmt (fun, &gsi, live_bytes);
	  else if (def_operand_p
		     def_p = single_ssa_def_operand (stmt, SSA_OP_DEF))
	    {
	      /* When we remove dead stores make sure to also delete trivially
		 dead SSA defs.  */
	      if (has_zero_uses (DEF_FROM_PTR (def_p))
		  && !gimple_has_side_effects (stmt)
		  && !is_ctrl_altering_stmt (stmt)
		  && (!stmt_could_throw_p (fun, stmt)
		      || fun->can_delete_dead_exceptions))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "  Deleted trivially dead stmt: ");
		      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
		      fprintf (dump_file, "\n");
		    }
		  if (gsi_remove (&gsi, true) && need_eh_cleanup)
		    bitmap_set_bit (need_eh_cleanup, bb->index);
		  release_defs (stmt);
		  released_def = true;
		}
	    }
	  if (gsi_end_p (gsi))
	    gsi = gsi_last_bb (bb);
	  else
	    gsi_prev (&gsi);
	}
      bool removed_phi = false;
      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);)
	{
	  gphi *phi = si.phi ();
	  if (has_zero_uses (gimple_phi_result (phi)))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "  Deleted trivially dead PHI: ");
		  print_gimple_stmt (dump_file, phi, 0, dump_flags);
		  fprintf (dump_file, "\n");
		}
	      remove_phi_node (&si, true);
	      removed_phi = true;
	      released_def = true;
	    }
	  else
	    gsi_next (&si);
	}
      if (removed_phi && gimple_seq_empty_p (phi_nodes (bb)))
	todo |= TODO_cleanup_cfg;
    }
  free (rpo);

  /* Removal of stores may make some EH edges dead.  Purge such edges from
     the CFG as needed.  */
  if (!bitmap_empty_p (need_eh_cleanup))
    {
      gimple_purge_all_dead_eh_edges (need_eh_cleanup);
      todo |= TODO_cleanup_cfg;
    }
  if (!bitmap_empty_p (need_ab_cleanup))
    {
      gimple_purge_all_dead_abnormal_call_edges (need_ab_cleanup);
      todo |= TODO_cleanup_cfg;
    }

  BITMAP_FREE (need_eh_cleanup);
  BITMAP_FREE (need_ab_cleanup);

  if (released_def)
    free_numbers_of_iterations_estimates (fun);

  if (flag_expensive_optimizations && use_dr_analysis_p)
    {
      for (auto i = dse_stmt_to_dr_map->begin ();
	   i != dse_stmt_to_dr_map->end (); ++i)
	free_data_ref ((*i).second);
      delete dse_stmt_to_dr_map;
      dse_stmt_to_dr_map = NULL;
    }

  return todo;
}

} // anon namespace

gimple_opt_pass *
make_pass_dse (gcc::context *ctxt)
{
  return new pass_dse (ctxt);
}
