/* Array prefetching.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "optabs-query.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "ssa.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "langhooks.h"
#include "tree-inline.h"
#include "tree-data-ref.h"
#include "diagnostic-core.h"
#include "dbgcnt.h"

/* This pass inserts prefetch instructions to optimize cache usage during
   accesses to arrays in loops.  It processes loops sequentially and:

   1) Gathers all memory references in the single loop.
   2) For each of the references it decides when it is profitable to prefetch
      it.  To do it, we evaluate the reuse among the accesses, and determines
      two values: PREFETCH_BEFORE (meaning that it only makes sense to do
      prefetching in the first PREFETCH_BEFORE iterations of the loop) and
      PREFETCH_MOD (meaning that it only makes sense to prefetch in the
      iterations of the loop that are zero modulo PREFETCH_MOD).  For example
      (assuming cache line size is 64 bytes, char has size 1 byte and there
      is no hardware sequential prefetch):

      char *a;
      for (i = 0; i < max; i++)
	{
	  a[255] = ...;		(0)
	  a[i] = ...;		(1)
	  a[i + 64] = ...;	(2)
	  a[16*i] = ...;	(3)
	  a[187*i] = ...;	(4)
	  a[187*i + 50] = ...;	(5)
	}

       (0) obviously has PREFETCH_BEFORE 1
       (1) has PREFETCH_BEFORE 64, since (2) accesses the same memory
           location 64 iterations before it, and PREFETCH_MOD 64 (since
	   it hits the same cache line otherwise).
       (2) has PREFETCH_MOD 64
       (3) has PREFETCH_MOD 4
       (4) has PREFETCH_MOD 1.  We do not set PREFETCH_BEFORE here, since
           the cache line accessed by (5) is the same with probability only
	   7/32.
       (5) has PREFETCH_MOD 1 as well.

      Additionally, we use data dependence analysis to determine for each
      reference the distance till the first reuse; this information is used
      to determine the temporality of the issued prefetch instruction.

   3) We determine how much ahead we need to prefetch.  The number of
      iterations needed is time to fetch / time spent in one iteration of
      the loop.  The problem is that we do not know either of these values,
      so we just make a heuristic guess based on a magic (possibly)
      target-specific constant and size of the loop.

   4) Determine which of the references we prefetch.  We take into account
      that there is a maximum number of simultaneous prefetches (provided
      by machine description).  We prefetch as many prefetches as possible
      while still within this bound (starting with those with lowest
      prefetch_mod, since they are responsible for most of the cache
      misses).

   5) We unroll and peel loops so that we are able to satisfy PREFETCH_MOD
      and PREFETCH_BEFORE requirements (within some bounds), and to avoid
      prefetching nonaccessed memory.
      TODO -- actually implement peeling.

   6) We actually emit the prefetch instructions.  ??? Perhaps emit the
      prefetch instructions with guards in cases where 5) was not sufficient
      to satisfy the constraints?

   A cost model is implemented to determine whether or not prefetching is
   profitable for a given loop.  The cost model has three heuristics:

   1. Function trip_count_to_ahead_ratio_too_small_p implements a
      heuristic that determines whether or not the loop has too few
      iterations (compared to ahead).  Prefetching is not likely to be
      beneficial if the trip count to ahead ratio is below a certain
      minimum.

   2. Function mem_ref_count_reasonable_p implements a heuristic that
      determines whether the given loop has enough CPU ops that can be
      overlapped with cache missing memory ops.  If not, the loop
      won't benefit from prefetching.  In the implementation,
      prefetching is not considered beneficial if the ratio between
      the instruction count and the mem ref count is below a certain
      minimum.

   3. Function insn_to_prefetch_ratio_too_small_p implements a
      heuristic that disables prefetching in a loop if the prefetching
      cost is above a certain limit.  The relative prefetching cost is
      estimated by taking the ratio between the prefetch count and the
      total intruction count (this models the I-cache cost).

   The limits used in these heuristics are defined as parameters with
   reasonable default values. Machine-specific default values will be
   added later.

   Some other TODO:
      -- write and use more general reuse analysis (that could be also used
	 in other cache aimed loop optimizations)
      -- make it behave sanely together with the prefetches given by user
	 (now we just ignore them; at the very least we should avoid
	 optimizing loops in that user put his own prefetches)
      -- we assume cache line size alignment of arrays; this could be
	 improved.  */

/* Magic constants follow.  These should be replaced by machine specific
   numbers.  */

/* True if write can be prefetched by a read prefetch.  */

#ifndef WRITE_CAN_USE_READ_PREFETCH
#define WRITE_CAN_USE_READ_PREFETCH 1
#endif

/* True if read can be prefetched by a write prefetch. */

#ifndef READ_CAN_USE_WRITE_PREFETCH
#define READ_CAN_USE_WRITE_PREFETCH 0
#endif

/* The size of the block loaded by a single prefetch.  Usually, this is
   the same as cache line size (at the moment, we only consider one level
   of cache hierarchy).  */

#ifndef PREFETCH_BLOCK
#define PREFETCH_BLOCK param_l1_cache_line_size
#endif

/* Do we have a forward hardware sequential prefetching?  */

#ifndef HAVE_FORWARD_PREFETCH
#define HAVE_FORWARD_PREFETCH 0
#endif

/* Do we have a backward hardware sequential prefetching?  */

#ifndef HAVE_BACKWARD_PREFETCH
#define HAVE_BACKWARD_PREFETCH 0
#endif

/* In some cases we are only able to determine that there is a certain
   probability that the two accesses hit the same cache line.  In this
   case, we issue the prefetches for both of them if this probability
   is less then (1000 - ACCEPTABLE_MISS_RATE) per thousand.  */

#ifndef ACCEPTABLE_MISS_RATE
#define ACCEPTABLE_MISS_RATE 50
#endif

#define L1_CACHE_SIZE_BYTES ((unsigned) (param_l1_cache_size * 1024))
#define L2_CACHE_SIZE_BYTES ((unsigned) (param_l2_cache_size * 1024))

/* We consider a memory access nontemporal if it is not reused sooner than
   after L2_CACHE_SIZE_BYTES of memory are accessed.  However, we ignore
   accesses closer than L1_CACHE_SIZE_BYTES / NONTEMPORAL_FRACTION,
   so that we use nontemporal prefetches e.g. if single memory location
   is accessed several times in a single iteration of the loop.  */
#define NONTEMPORAL_FRACTION 16

/* In case we have to emit a memory fence instruction after the loop that
   uses nontemporal stores, this defines the builtin to use.  */

#ifndef FENCE_FOLLOWING_MOVNT
#define FENCE_FOLLOWING_MOVNT NULL_TREE
#endif

/* It is not profitable to prefetch when the trip count is not at
   least TRIP_COUNT_TO_AHEAD_RATIO times the prefetch ahead distance.
   For example, in a loop with a prefetch ahead distance of 10,
   supposing that TRIP_COUNT_TO_AHEAD_RATIO is equal to 4, it is
   profitable to prefetch when the trip count is greater or equal to
   40.  In that case, 30 out of the 40 iterations will benefit from
   prefetching.  */

#ifndef TRIP_COUNT_TO_AHEAD_RATIO
#define TRIP_COUNT_TO_AHEAD_RATIO 4
#endif

/* The group of references between that reuse may occur.  */

struct mem_ref_group
{
  tree base;			/* Base of the reference.  */
  tree step;			/* Step of the reference.  */
  struct mem_ref *refs;		/* References in the group.  */
  struct mem_ref_group *next;	/* Next group of references.  */
  unsigned int uid;		/* Group UID, used only for debugging.  */
};

/* Assigned to PREFETCH_BEFORE when all iterations are to be prefetched.  */

#define PREFETCH_ALL		HOST_WIDE_INT_M1U

/* Do not generate a prefetch if the unroll factor is significantly less
   than what is required by the prefetch.  This is to avoid redundant
   prefetches.  For example, when prefetch_mod is 16 and unroll_factor is
   2, prefetching requires unrolling the loop 16 times, but
   the loop is actually unrolled twice.  In this case (ratio = 8),
   prefetching is not likely to be beneficial.  */

#ifndef PREFETCH_MOD_TO_UNROLL_FACTOR_RATIO
#define PREFETCH_MOD_TO_UNROLL_FACTOR_RATIO 4
#endif

/* Some of the prefetch computations have quadratic complexity.  We want to
   avoid huge compile times and, therefore, want to limit the amount of
   memory references per loop where we consider prefetching.  */

#ifndef PREFETCH_MAX_MEM_REFS_PER_LOOP
#define PREFETCH_MAX_MEM_REFS_PER_LOOP 200
#endif

/* The memory reference.  */

struct mem_ref
{
  gimple *stmt;			/* Statement in that the reference appears.  */
  tree mem;			/* The reference.  */
  HOST_WIDE_INT delta;		/* Constant offset of the reference.  */
  struct mem_ref_group *group;	/* The group of references it belongs to.  */
  unsigned HOST_WIDE_INT prefetch_mod;
				/* Prefetch only each PREFETCH_MOD-th
				   iteration.  */
  unsigned HOST_WIDE_INT prefetch_before;
				/* Prefetch only first PREFETCH_BEFORE
				   iterations.  */
  unsigned reuse_distance;	/* The amount of data accessed before the first
				   reuse of this value.  */
  struct mem_ref *next;		/* The next reference in the group.  */
  unsigned int uid;		/* Ref UID, used only for debugging.  */
  unsigned write_p : 1;		/* Is it a write?  */
  unsigned independent_p : 1;	/* True if the reference is independent on
				   all other references inside the loop.  */
  unsigned issue_prefetch_p : 1;	/* Should we really issue the prefetch?  */
  unsigned storent_p : 1;	/* True if we changed the store to a
				   nontemporal one.  */
};

/* Dumps information about memory reference */
static void
dump_mem_details (FILE *file, tree base, tree step,
	    HOST_WIDE_INT delta, bool write_p)
{
  fprintf (file, "(base ");
  print_generic_expr (file, base, TDF_SLIM);
  fprintf (file, ", step ");
  if (cst_and_fits_in_hwi (step))
    fprintf (file, HOST_WIDE_INT_PRINT_DEC, int_cst_value (step));
  else
    print_generic_expr (file, step, TDF_SLIM);
  fprintf (file, ")\n");
  fprintf (file, "  delta " HOST_WIDE_INT_PRINT_DEC "\n", delta);
  fprintf (file, "  %s\n\n", write_p ? "write" : "read");
}

/* Dumps information about reference REF to FILE.  */

static void
dump_mem_ref (FILE *file, struct mem_ref *ref)
{
  fprintf (file, "reference %u:%u (", ref->group->uid, ref->uid);
  print_generic_expr (file, ref->mem, TDF_SLIM);
  fprintf (file, ")\n");
}

/* Finds a group with BASE and STEP in GROUPS, or creates one if it does not
   exist.  */

static struct mem_ref_group *
find_or_create_group (struct mem_ref_group **groups, tree base, tree step)
{
  /* Global count for setting struct mem_ref_group->uid.  */
  static unsigned int last_mem_ref_group_uid = 0;

  struct mem_ref_group *group;

  for (; *groups; groups = &(*groups)->next)
    {
      if (operand_equal_p ((*groups)->step, step, 0)
	  && operand_equal_p ((*groups)->base, base, 0))
	return *groups;

      /* If step is an integer constant, keep the list of groups sorted
         by decreasing step.  */
      if (cst_and_fits_in_hwi ((*groups)->step) && cst_and_fits_in_hwi (step)
	  && int_cst_value ((*groups)->step) < int_cst_value (step))
	break;
    }

  group = XNEW (struct mem_ref_group);
  group->base = base;
  group->step = step;
  group->refs = NULL;
  group->uid = ++last_mem_ref_group_uid;
  group->next = *groups;
  *groups = group;

  return group;
}

/* Records a memory reference MEM in GROUP with offset DELTA and write status
   WRITE_P.  The reference occurs in statement STMT.  */

static void
record_ref (struct mem_ref_group *group, gimple *stmt, tree mem,
	    HOST_WIDE_INT delta, bool write_p)
{
  unsigned int last_mem_ref_uid = 0;
  struct mem_ref **aref;

  /* Do not record the same address twice.  */
  for (aref = &group->refs; *aref; aref = &(*aref)->next)
    {
      last_mem_ref_uid = (*aref)->uid;

      /* It does not have to be possible for write reference to reuse the read
	 prefetch, or vice versa.  */
      if (!WRITE_CAN_USE_READ_PREFETCH
	  && write_p
	  && !(*aref)->write_p)
	continue;
      if (!READ_CAN_USE_WRITE_PREFETCH
	  && !write_p
	  && (*aref)->write_p)
	continue;

      if ((*aref)->delta == delta)
	return;
    }

  (*aref) = XNEW (struct mem_ref);
  (*aref)->stmt = stmt;
  (*aref)->mem = mem;
  (*aref)->delta = delta;
  (*aref)->write_p = write_p;
  (*aref)->prefetch_before = PREFETCH_ALL;
  (*aref)->prefetch_mod = 1;
  (*aref)->reuse_distance = 0;
  (*aref)->issue_prefetch_p = false;
  (*aref)->group = group;
  (*aref)->next = NULL;
  (*aref)->independent_p = false;
  (*aref)->storent_p = false;
  (*aref)->uid = last_mem_ref_uid + 1;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_mem_ref (dump_file, *aref);

      fprintf (dump_file, "  group %u ", group->uid);
      dump_mem_details (dump_file, group->base, group->step, delta,
			write_p);
    }
}

/* Release memory references in GROUPS.  */

static void
release_mem_refs (struct mem_ref_group *groups)
{
  struct mem_ref_group *next_g;
  struct mem_ref *ref, *next_r;

  for (; groups; groups = next_g)
    {
      next_g = groups->next;
      for (ref = groups->refs; ref; ref = next_r)
	{
	  next_r = ref->next;
	  free (ref);
	}
      free (groups);
    }
}

/* A structure used to pass arguments to idx_analyze_ref.  */

struct ar_data
{
  class loop *loop;			/* Loop of the reference.  */
  gimple *stmt;				/* Statement of the reference.  */
  tree *step;				/* Step of the memory reference.  */
  HOST_WIDE_INT *delta;			/* Offset of the memory reference.  */
};

/* Analyzes a single INDEX of a memory reference to obtain information
   described at analyze_ref.  Callback for for_each_index.  */

static bool
idx_analyze_ref (tree base, tree *index, void *data)
{
  struct ar_data *ar_data = (struct ar_data *) data;
  tree ibase, step, stepsize;
  HOST_WIDE_INT idelta = 0, imult = 1;
  affine_iv iv;

  if (!simple_iv (ar_data->loop, loop_containing_stmt (ar_data->stmt),
		  *index, &iv, true))
    return false;
  ibase = iv.base;
  step = iv.step;

  if (TREE_CODE (ibase) == POINTER_PLUS_EXPR
      && cst_and_fits_in_hwi (TREE_OPERAND (ibase, 1)))
    {
      idelta = int_cst_value (TREE_OPERAND (ibase, 1));
      ibase = TREE_OPERAND (ibase, 0);
    }
  if (cst_and_fits_in_hwi (ibase))
    {
      idelta += int_cst_value (ibase);
      ibase = build_int_cst (TREE_TYPE (ibase), 0);
    }

  if (TREE_CODE (base) == ARRAY_REF)
    {
      stepsize = array_ref_element_size (base);
      if (!cst_and_fits_in_hwi (stepsize))
	return false;
      imult = int_cst_value (stepsize);
      step = fold_build2 (MULT_EXPR, sizetype,
			  fold_convert (sizetype, step),
			  fold_convert (sizetype, stepsize));
      idelta *= imult;
    }

  if (*ar_data->step == NULL_TREE)
    *ar_data->step = step;
  else
    *ar_data->step = fold_build2 (PLUS_EXPR, sizetype,
				  fold_convert (sizetype, *ar_data->step),
				  fold_convert (sizetype, step));
  *ar_data->delta += idelta;
  *index = ibase;

  return true;
}

/* Tries to express REF_P in shape &BASE + STEP * iter + DELTA, where DELTA and
   STEP are integer constants and iter is number of iterations of LOOP.  The
   reference occurs in statement STMT.  Strips nonaddressable component
   references from REF_P.  */

static bool
analyze_ref (class loop *loop, tree *ref_p, tree *base,
	     tree *step, HOST_WIDE_INT *delta,
	     gimple *stmt)
{
  struct ar_data ar_data;
  tree off;
  HOST_WIDE_INT bit_offset;
  tree ref = *ref_p;

  *step = NULL_TREE;
  *delta = 0;

  /* First strip off the component references.  Ignore bitfields.
     Also strip off the real and imagine parts of a complex, so that
     they can have the same base.  */
  if (TREE_CODE (ref) == REALPART_EXPR
      || TREE_CODE (ref) == IMAGPART_EXPR
      || (TREE_CODE (ref) == COMPONENT_REF
          && DECL_NONADDRESSABLE_P (TREE_OPERAND (ref, 1))))
    {
      if (TREE_CODE (ref) == IMAGPART_EXPR)
        *delta += int_size_in_bytes (TREE_TYPE (ref));
      ref = TREE_OPERAND (ref, 0);
    }

  *ref_p = ref;

  for (; TREE_CODE (ref) == COMPONENT_REF; ref = TREE_OPERAND (ref, 0))
    {
      off = DECL_FIELD_BIT_OFFSET (TREE_OPERAND (ref, 1));
      bit_offset = TREE_INT_CST_LOW (off);
      gcc_assert (bit_offset % BITS_PER_UNIT == 0);

      *delta += bit_offset / BITS_PER_UNIT;
    }

  *base = unshare_expr (ref);
  ar_data.loop = loop;
  ar_data.stmt = stmt;
  ar_data.step = step;
  ar_data.delta = delta;
  return for_each_index (base, idx_analyze_ref, &ar_data);
}

/* Record a memory reference REF to the list REFS.  The reference occurs in
   LOOP in statement STMT and it is write if WRITE_P.  Returns true if the
   reference was recorded, false otherwise.  */

static bool
gather_memory_references_ref (class loop *loop, struct mem_ref_group **refs,
			      tree ref, bool write_p, gimple *stmt)
{
  tree base, step;
  HOST_WIDE_INT delta;
  struct mem_ref_group *agrp;

  if (get_base_address (ref) == NULL)
    return false;

  if (!analyze_ref (loop, &ref, &base, &step, &delta, stmt))
    return false;
  /* If analyze_ref fails the default is a NULL_TREE.  We can stop here.  */
  if (step == NULL_TREE)
    return false;

  /* Stop if the address of BASE could not be taken.  */
  if (may_be_nonaddressable_p (base))
    return false;

  /* Limit non-constant step prefetching only to the innermost loops and
     only when the step is loop invariant in the entire loop nest. */
  if (!cst_and_fits_in_hwi (step))
    {
      if (loop->inner != NULL)
        {
          if (dump_file && (dump_flags & TDF_DETAILS))
            {
              fprintf (dump_file, "Memory expression %p\n",(void *) ref );
	      print_generic_expr (dump_file, ref, TDF_SLIM);
	      fprintf (dump_file,":");
              dump_mem_details (dump_file, base, step, delta, write_p);
              fprintf (dump_file,
                       "Ignoring %p, non-constant step prefetching is "
                       "limited to inner most loops \n",
                       (void *) ref);
            }
            return false;
         }
      else
        {
          if (!expr_invariant_in_loop_p (loop_outermost (loop), step))
          {
            if (dump_file && (dump_flags & TDF_DETAILS))
              {
                fprintf (dump_file, "Memory expression %p\n",(void *) ref );
		print_generic_expr (dump_file, ref, TDF_SLIM);
                fprintf (dump_file,":");
                dump_mem_details (dump_file, base, step, delta, write_p);
                fprintf (dump_file,
                         "Not prefetching, ignoring %p due to "
                         "loop variant step\n",
                         (void *) ref);
              }
              return false;
            }
        }
    }

  /* Now we know that REF = &BASE + STEP * iter + DELTA, where DELTA and STEP
     are integer constants.  */
  agrp = find_or_create_group (refs, base, step);
  record_ref (agrp, stmt, ref, delta, write_p);

  return true;
}

/* Record the suitable memory references in LOOP.  NO_OTHER_REFS is set to
   true if there are no other memory references inside the loop.  */

static struct mem_ref_group *
gather_memory_references (class loop *loop, bool *no_other_refs, unsigned *ref_count)
{
  basic_block *body = get_loop_body_in_dom_order (loop);
  basic_block bb;
  unsigned i;
  gimple_stmt_iterator bsi;
  gimple *stmt;
  tree lhs, rhs;
  struct mem_ref_group *refs = NULL;

  *no_other_refs = true;
  *ref_count = 0;

  /* Scan the loop body in order, so that the former references precede the
     later ones.  */
  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = body[i];
      if (bb->loop_father != loop)
	continue;

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  stmt = gsi_stmt (bsi);

	  if (gimple_code (stmt) != GIMPLE_ASSIGN)
	    {
	      if (gimple_vuse (stmt)
		  || (is_gimple_call (stmt)
		      && !(gimple_call_flags (stmt) & ECF_CONST)))
		*no_other_refs = false;
	      continue;
	    }

	  if (! gimple_vuse (stmt))
	    continue;

	  lhs = gimple_assign_lhs (stmt);
	  rhs = gimple_assign_rhs1 (stmt);

	  if (REFERENCE_CLASS_P (rhs))
	    {
	    *no_other_refs &= gather_memory_references_ref (loop, &refs,
							    rhs, false, stmt);
	    *ref_count += 1;
	    }
	  if (REFERENCE_CLASS_P (lhs))
	    {
	    *no_other_refs &= gather_memory_references_ref (loop, &refs,
							    lhs, true, stmt);
	    *ref_count += 1;
	    }
	}
    }
  free (body);

  return refs;
}

/* Prune the prefetch candidate REF using the self-reuse.  */

static void
prune_ref_by_self_reuse (struct mem_ref *ref)
{
  HOST_WIDE_INT step;
  bool backward;

  /* If the step size is non constant, we cannot calculate prefetch_mod.  */
  if (!cst_and_fits_in_hwi (ref->group->step))
    return;

  step = int_cst_value (ref->group->step);

  backward = step < 0;

  if (step == 0)
    {
      /* Prefetch references to invariant address just once.  */
      ref->prefetch_before = 1;
      return;
    }

  if (backward)
    step = -step;

  if (step > PREFETCH_BLOCK)
    return;

  if ((backward && HAVE_BACKWARD_PREFETCH)
      || (!backward && HAVE_FORWARD_PREFETCH))
    {
      ref->prefetch_before = 1;
      return;
    }

  ref->prefetch_mod = PREFETCH_BLOCK / step;
}

/* Divides X by BY, rounding down.  */

static HOST_WIDE_INT
ddown (HOST_WIDE_INT x, unsigned HOST_WIDE_INT by)
{
  gcc_assert (by > 0);

  if (x >= 0)
    return x / (HOST_WIDE_INT) by;
  else
    return (x + (HOST_WIDE_INT) by - 1) / (HOST_WIDE_INT) by;
}

/* Given a CACHE_LINE_SIZE and two inductive memory references
   with a common STEP greater than CACHE_LINE_SIZE and an address
   difference DELTA, compute the probability that they will fall
   in different cache lines.  Return true if the computed miss rate
   is not greater than the ACCEPTABLE_MISS_RATE.  DISTINCT_ITERS is the
   number of distinct iterations after which the pattern repeats itself.
   ALIGN_UNIT is the unit of alignment in bytes.  */

static bool
is_miss_rate_acceptable (unsigned HOST_WIDE_INT cache_line_size,
		   HOST_WIDE_INT step, HOST_WIDE_INT delta,
		   unsigned HOST_WIDE_INT distinct_iters,
		   int align_unit)
{
  unsigned align, iter;
  int total_positions, miss_positions, max_allowed_miss_positions;
  int address1, address2, cache_line1, cache_line2;

  /* It always misses if delta is greater than or equal to the cache
     line size.  */
  if (delta >= (HOST_WIDE_INT) cache_line_size)
    return false;

  gcc_assert (align_unit > 0);

  miss_positions = 0;
  total_positions = (cache_line_size / align_unit) * distinct_iters;
  max_allowed_miss_positions = (ACCEPTABLE_MISS_RATE * total_positions) / 1000;

  /* Iterate through all possible alignments of the first
     memory reference within its cache line.  */
  for (align = 0; align < cache_line_size; align += align_unit)

    /* Iterate through all distinct iterations.  */
    for (iter = 0; iter < distinct_iters; iter++)
      {
	address1 = align + step * iter;
	address2 = address1 + delta;
	cache_line1 = address1 / cache_line_size;
	cache_line2 = address2 / cache_line_size;
	if (cache_line1 != cache_line2)
	  {
	    miss_positions += 1;
            if (miss_positions > max_allowed_miss_positions)
	      return false;
          }
      }
  return true;
}

/* Prune the prefetch candidate REF using the reuse with BY.
   If BY_IS_BEFORE is true, BY is before REF in the loop.  */

static void
prune_ref_by_group_reuse (struct mem_ref *ref, struct mem_ref *by,
			  bool by_is_before)
{
  HOST_WIDE_INT step;
  bool backward;
  HOST_WIDE_INT delta_r = ref->delta, delta_b = by->delta;
  HOST_WIDE_INT delta = delta_b - delta_r;
  HOST_WIDE_INT hit_from;
  unsigned HOST_WIDE_INT prefetch_before, prefetch_block;
  HOST_WIDE_INT reduced_step;
  unsigned HOST_WIDE_INT reduced_prefetch_block;
  tree ref_type;
  int align_unit;

  /* If the step is non constant we cannot calculate prefetch_before.  */
  if (!cst_and_fits_in_hwi (ref->group->step)) {
    return;
  }

  step = int_cst_value (ref->group->step);

  backward = step < 0;


  if (delta == 0)
    {
      /* If the references has the same address, only prefetch the
	 former.  */
      if (by_is_before)
	ref->prefetch_before = 0;

      return;
    }

  if (!step)
    {
      /* If the reference addresses are invariant and fall into the
	 same cache line, prefetch just the first one.  */
      if (!by_is_before)
	return;

      if (ddown (ref->delta, PREFETCH_BLOCK)
	  != ddown (by->delta, PREFETCH_BLOCK))
	return;

      ref->prefetch_before = 0;
      return;
    }

  /* Only prune the reference that is behind in the array.  */
  if (backward)
    {
      if (delta > 0)
	return;

      /* Transform the data so that we may assume that the accesses
	 are forward.  */
      delta = - delta;
      step = -step;
      delta_r = PREFETCH_BLOCK - 1 - delta_r;
      delta_b = PREFETCH_BLOCK - 1 - delta_b;
    }
  else
    {
      if (delta < 0)
	return;
    }

  /* Check whether the two references are likely to hit the same cache
     line, and how distant the iterations in that it occurs are from
     each other.  */

  if (step <= PREFETCH_BLOCK)
    {
      /* The accesses are sure to meet.  Let us check when.  */
      hit_from = ddown (delta_b, PREFETCH_BLOCK) * PREFETCH_BLOCK;
      prefetch_before = (hit_from - delta_r + step - 1) / step;

      /* Do not reduce prefetch_before if we meet beyond cache size.  */
      if (prefetch_before > absu_hwi (L2_CACHE_SIZE_BYTES / step))
        prefetch_before = PREFETCH_ALL;
      if (prefetch_before < ref->prefetch_before)
	ref->prefetch_before = prefetch_before;

      return;
    }

  /* A more complicated case with step > prefetch_block.  First reduce
     the ratio between the step and the cache line size to its simplest
     terms.  The resulting denominator will then represent the number of
     distinct iterations after which each address will go back to its
     initial location within the cache line.  This computation assumes
     that PREFETCH_BLOCK is a power of two.  */
  prefetch_block = PREFETCH_BLOCK;
  reduced_prefetch_block = prefetch_block;
  reduced_step = step;
  while ((reduced_step & 1) == 0
	 && reduced_prefetch_block > 1)
    {
      reduced_step >>= 1;
      reduced_prefetch_block >>= 1;
    }

  prefetch_before = delta / step;
  delta %= step;
  ref_type = TREE_TYPE (ref->mem);
  align_unit = TYPE_ALIGN (ref_type) / 8;
  if (is_miss_rate_acceptable (prefetch_block, step, delta,
			       reduced_prefetch_block, align_unit))
    {
      /* Do not reduce prefetch_before if we meet beyond cache size.  */
      if (prefetch_before > L2_CACHE_SIZE_BYTES / PREFETCH_BLOCK)
        prefetch_before = PREFETCH_ALL;
      if (prefetch_before < ref->prefetch_before)
	ref->prefetch_before = prefetch_before;

      return;
    }

  /* Try also the following iteration.  */
  prefetch_before++;
  delta = step - delta;
  if (is_miss_rate_acceptable (prefetch_block, step, delta,
			       reduced_prefetch_block, align_unit))
    {
      if (prefetch_before < ref->prefetch_before)
	ref->prefetch_before = prefetch_before;

      return;
    }

  /* The ref probably does not reuse by.  */
  return;
}

/* Prune the prefetch candidate REF using the reuses with other references
   in REFS.  */

static void
prune_ref_by_reuse (struct mem_ref *ref, struct mem_ref *refs)
{
  struct mem_ref *prune_by;
  bool before = true;

  prune_ref_by_self_reuse (ref);

  for (prune_by = refs; prune_by; prune_by = prune_by->next)
    {
      if (prune_by == ref)
	{
	  before = false;
	  continue;
	}

      if (!WRITE_CAN_USE_READ_PREFETCH
	  && ref->write_p
	  && !prune_by->write_p)
	continue;
      if (!READ_CAN_USE_WRITE_PREFETCH
	  && !ref->write_p
	  && prune_by->write_p)
	continue;

      prune_ref_by_group_reuse (ref, prune_by, before);
    }
}

/* Prune the prefetch candidates in GROUP using the reuse analysis.  */

static void
prune_group_by_reuse (struct mem_ref_group *group)
{
  struct mem_ref *ref_pruned;

  for (ref_pruned = group->refs; ref_pruned; ref_pruned = ref_pruned->next)
    {
      prune_ref_by_reuse (ref_pruned, group->refs);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  dump_mem_ref (dump_file, ref_pruned);

	  if (ref_pruned->prefetch_before == PREFETCH_ALL
	      && ref_pruned->prefetch_mod == 1)
	    fprintf (dump_file, " no restrictions");
	  else if (ref_pruned->prefetch_before == 0)
	    fprintf (dump_file, " do not prefetch");
	  else if (ref_pruned->prefetch_before <= ref_pruned->prefetch_mod)
	    fprintf (dump_file, " prefetch once");
	  else
	    {
	      if (ref_pruned->prefetch_before != PREFETCH_ALL)
		{
		  fprintf (dump_file, " prefetch before ");
		  fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC,
			   ref_pruned->prefetch_before);
		}
	      if (ref_pruned->prefetch_mod != 1)
		{
		  fprintf (dump_file, " prefetch mod ");
		  fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC,
			   ref_pruned->prefetch_mod);
		}
	    }
	  fprintf (dump_file, "\n");
	}
    }
}

/* Prune the list of prefetch candidates GROUPS using the reuse analysis.  */

static void
prune_by_reuse (struct mem_ref_group *groups)
{
  for (; groups; groups = groups->next)
    prune_group_by_reuse (groups);
}

/* Returns true if we should issue prefetch for REF.  */

static bool
should_issue_prefetch_p (struct mem_ref *ref)
{
  /* Do we want to issue prefetches for non-constant strides?  */
  if (!cst_and_fits_in_hwi (ref->group->step)
      && param_prefetch_dynamic_strides == 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Skipping non-constant step for reference %u:%u\n",
		 ref->group->uid, ref->uid);
      return false;
    }

  /* Some processors may have a hardware prefetcher that may conflict with
     prefetch hints for a range of strides.  Make sure we don't issue
     prefetches for such cases if the stride is within this particular
     range.  */
  if (cst_and_fits_in_hwi (ref->group->step)
      && abs_hwi (int_cst_value (ref->group->step))
	  < (HOST_WIDE_INT) param_prefetch_minimum_stride)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Step for reference %u:%u (" HOST_WIDE_INT_PRINT_DEC
		 ") is less than the mininum required stride of %d\n",
		 ref->group->uid, ref->uid, int_cst_value (ref->group->step),
		 param_prefetch_minimum_stride);
      return false;
    }

  /* For now do not issue prefetches for only first few of the
     iterations.  */
  if (ref->prefetch_before != PREFETCH_ALL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "Ignoring reference %u:%u due to prefetch_before\n",
		 ref->group->uid, ref->uid);
      return false;
    }

  /* Do not prefetch nontemporal stores.  */
  if (ref->storent_p)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "Ignoring nontemporal store reference %u:%u\n", ref->group->uid, ref->uid);
      return false;
    }

  return true;
}

/* Decide which of the prefetch candidates in GROUPS to prefetch.
   AHEAD is the number of iterations to prefetch ahead (which corresponds
   to the number of simultaneous instances of one prefetch running at a
   time).  UNROLL_FACTOR is the factor by that the loop is going to be
   unrolled.  Returns true if there is anything to prefetch.  */

static bool
schedule_prefetches (struct mem_ref_group *groups, unsigned unroll_factor,
		     unsigned ahead)
{
  unsigned remaining_prefetch_slots, n_prefetches, prefetch_slots;
  unsigned slots_per_prefetch;
  struct mem_ref *ref;
  bool any = false;

  /* At most param_simultaneous_prefetches should be running
     at the same time.  */
  remaining_prefetch_slots = param_simultaneous_prefetches;

  /* The prefetch will run for AHEAD iterations of the original loop, i.e.,
     AHEAD / UNROLL_FACTOR iterations of the unrolled loop.  In each iteration,
     it will need a prefetch slot.  */
  slots_per_prefetch = (ahead + unroll_factor / 2) / unroll_factor;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Each prefetch instruction takes %u prefetch slots.\n",
	     slots_per_prefetch);

  /* For now we just take memory references one by one and issue
     prefetches for as many as possible.  The groups are sorted
     starting with the largest step, since the references with
     large step are more likely to cause many cache misses.  */

  for (; groups; groups = groups->next)
    for (ref = groups->refs; ref; ref = ref->next)
      {
	if (!should_issue_prefetch_p (ref))
	  continue;

        /* The loop is far from being sufficiently unrolled for this
           prefetch.  Do not generate prefetch to avoid many redudant
           prefetches.  */
        if (ref->prefetch_mod / unroll_factor > PREFETCH_MOD_TO_UNROLL_FACTOR_RATIO)
          continue;

	/* If we need to prefetch the reference each PREFETCH_MOD iterations,
	   and we unroll the loop UNROLL_FACTOR times, we need to insert
	   ceil (UNROLL_FACTOR / PREFETCH_MOD) instructions in each
	   iteration.  */
	n_prefetches = ((unroll_factor + ref->prefetch_mod - 1)
			/ ref->prefetch_mod);
	prefetch_slots = n_prefetches * slots_per_prefetch;

	/* If more than half of the prefetches would be lost anyway, do not
	   issue the prefetch.  */
	if (2 * remaining_prefetch_slots < prefetch_slots)
	  continue;

	/* Stop prefetching if debug counter is activated.  */
	if (!dbg_cnt (prefetch))
	  continue;

	ref->issue_prefetch_p = true;
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Decided to issue prefetch for reference %u:%u\n",
		   ref->group->uid, ref->uid);

	if (remaining_prefetch_slots <= prefetch_slots)
	  return true;
	remaining_prefetch_slots -= prefetch_slots;
	any = true;
      }

  return any;
}

/* Return TRUE if no prefetch is going to be generated in the given
   GROUPS.  */

static bool
nothing_to_prefetch_p (struct mem_ref_group *groups)
{
  struct mem_ref *ref;

  for (; groups; groups = groups->next)
    for (ref = groups->refs; ref; ref = ref->next)
      if (should_issue_prefetch_p (ref))
	return false;

  return true;
}

/* Estimate the number of prefetches in the given GROUPS.
   UNROLL_FACTOR is the factor by which LOOP was unrolled.  */

static int
estimate_prefetch_count (struct mem_ref_group *groups, unsigned unroll_factor)
{
  struct mem_ref *ref;
  unsigned n_prefetches;
  int prefetch_count = 0;

  for (; groups; groups = groups->next)
    for (ref = groups->refs; ref; ref = ref->next)
      if (should_issue_prefetch_p (ref))
	{
	  n_prefetches = ((unroll_factor + ref->prefetch_mod - 1)
			  / ref->prefetch_mod);
	  prefetch_count += n_prefetches;
	}

  return prefetch_count;
}

/* Issue prefetches for the reference REF into loop as decided before.
   HEAD is the number of iterations to prefetch ahead.  UNROLL_FACTOR
   is the factor by which LOOP was unrolled.  */

static void
issue_prefetch_ref (struct mem_ref *ref, unsigned unroll_factor, unsigned ahead)
{
  HOST_WIDE_INT delta;
  tree addr, addr_base, write_p, local, forward;
  gcall *prefetch;
  gimple_stmt_iterator bsi;
  unsigned n_prefetches, ap;
  bool nontemporal = ref->reuse_distance >= L2_CACHE_SIZE_BYTES;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Issued%s prefetch for reference %u:%u.\n",
	     nontemporal ? " nontemporal" : "",
	     ref->group->uid, ref->uid);

  bsi = gsi_for_stmt (ref->stmt);

  n_prefetches = ((unroll_factor + ref->prefetch_mod - 1)
		  / ref->prefetch_mod);
  addr_base = build_fold_addr_expr_with_type (ref->mem, ptr_type_node);
  addr_base = force_gimple_operand_gsi (&bsi, unshare_expr (addr_base),
					true, NULL, true, GSI_SAME_STMT);
  write_p = ref->write_p ? integer_one_node : integer_zero_node;
  local = nontemporal ? integer_zero_node : build_int_cst (integer_type_node, 3);

  for (ap = 0; ap < n_prefetches; ap++)
    {
      if (cst_and_fits_in_hwi (ref->group->step))
        {
          /* Determine the address to prefetch.  */
          delta = (ahead + ap * ref->prefetch_mod) *
		   int_cst_value (ref->group->step);
          addr = fold_build_pointer_plus_hwi (addr_base, delta);
          addr = force_gimple_operand_gsi (&bsi, unshare_expr (addr), true,
					   NULL, true, GSI_SAME_STMT);
        }
      else
        {
          /* The step size is non-constant but loop-invariant.  We use the
             heuristic to simply prefetch ahead iterations ahead.  */
          forward = fold_build2 (MULT_EXPR, sizetype,
                                 fold_convert (sizetype, ref->group->step),
                                 fold_convert (sizetype, size_int (ahead)));
          addr = fold_build_pointer_plus (addr_base, forward);
          addr = force_gimple_operand_gsi (&bsi, unshare_expr (addr), true,
					   NULL, true, GSI_SAME_STMT);
      }

      if (addr_base != addr
	  && TREE_CODE (addr_base) == SSA_NAME
	  && TREE_CODE (addr) == SSA_NAME)
	{
	  duplicate_ssa_name_ptr_info (addr, SSA_NAME_PTR_INFO (addr_base));
	  /* As this isn't a plain copy we have to reset alignment
	     information.  */
	  if (SSA_NAME_PTR_INFO (addr))
	    mark_ptr_info_alignment_unknown (SSA_NAME_PTR_INFO (addr));
	}

      /* Create the prefetch instruction.  */
      prefetch = gimple_build_call (builtin_decl_explicit (BUILT_IN_PREFETCH),
				    3, addr, write_p, local);
      gsi_insert_before (&bsi, prefetch, GSI_SAME_STMT);
    }
}

/* Issue prefetches for the references in GROUPS into loop as decided before.
   HEAD is the number of iterations to prefetch ahead.  UNROLL_FACTOR is the
   factor by that LOOP was unrolled.  */

static void
issue_prefetches (struct mem_ref_group *groups,
		  unsigned unroll_factor, unsigned ahead)
{
  struct mem_ref *ref;

  for (; groups; groups = groups->next)
    for (ref = groups->refs; ref; ref = ref->next)
      if (ref->issue_prefetch_p)
	issue_prefetch_ref (ref, unroll_factor, ahead);
}

/* Returns true if REF is a memory write for that a nontemporal store insn
   can be used.  */

static bool
nontemporal_store_p (struct mem_ref *ref)
{
  machine_mode mode;
  enum insn_code code;

  /* REF must be a write that is not reused.  We require it to be independent
     on all other memory references in the loop, as the nontemporal stores may
     be reordered with respect to other memory references.  */
  if (!ref->write_p
      || !ref->independent_p
      || ref->reuse_distance < L2_CACHE_SIZE_BYTES)
    return false;

  /* Check that we have the storent instruction for the mode.  */
  mode = TYPE_MODE (TREE_TYPE (ref->mem));
  if (mode == BLKmode)
    return false;

  code = optab_handler (storent_optab, mode);
  return code != CODE_FOR_nothing;
}

/* If REF is a nontemporal store, we mark the corresponding modify statement
   and return true.  Otherwise, we return false.  */

static bool
mark_nontemporal_store (struct mem_ref *ref)
{
  if (!nontemporal_store_p (ref))
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Marked reference %u:%u as a nontemporal store.\n",
	     ref->group->uid, ref->uid);

  gimple_assign_set_nontemporal_move (ref->stmt, true);
  ref->storent_p = true;

  return true;
}

/* Issue a memory fence instruction after LOOP.  */

static void
emit_mfence_after_loop (class loop *loop)
{
  auto_vec<edge> exits = get_loop_exit_edges (loop);
  edge exit;
  gcall *call;
  gimple_stmt_iterator bsi;
  unsigned i;

  FOR_EACH_VEC_ELT (exits, i, exit)
    {
      call = gimple_build_call (FENCE_FOLLOWING_MOVNT, 0);

      if (!single_pred_p (exit->dest)
	  /* If possible, we prefer not to insert the fence on other paths
	     in cfg.  */
	  && !(exit->flags & EDGE_ABNORMAL))
	split_loop_exit_edge (exit);
      bsi = gsi_after_labels (exit->dest);

      gsi_insert_before (&bsi, call, GSI_NEW_STMT);
    }
}

/* Returns true if we can use storent in loop, false otherwise.  */

static bool
may_use_storent_in_loop_p (class loop *loop)
{
  bool ret = true;

  if (loop->inner != NULL)
    return false;

  /* If we must issue a mfence insn after using storent, check that there
     is a suitable place for it at each of the loop exits.  */
  if (FENCE_FOLLOWING_MOVNT != NULL_TREE)
    {
      auto_vec<edge> exits = get_loop_exit_edges (loop);
      unsigned i;
      edge exit;

      FOR_EACH_VEC_ELT (exits, i, exit)
	if ((exit->flags & EDGE_ABNORMAL)
	    && exit->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	  ret = false;
    }

  return ret;
}

/* Marks nontemporal stores in LOOP.  GROUPS contains the description of memory
   references in the loop.  Returns whether we inserted any mfence call.  */

static bool
mark_nontemporal_stores (class loop *loop, struct mem_ref_group *groups)
{
  struct mem_ref *ref;
  bool any = false;

  if (!may_use_storent_in_loop_p (loop))
    return false;

  for (; groups; groups = groups->next)
    for (ref = groups->refs; ref; ref = ref->next)
      any |= mark_nontemporal_store (ref);

  if (any && FENCE_FOLLOWING_MOVNT != NULL_TREE)
    {
      emit_mfence_after_loop (loop);
      return true;
    }
  return false;
}

/* Determines whether we can profitably unroll LOOP FACTOR times, and if
   this is the case, fill in DESC by the description of number of
   iterations.  */

static bool
should_unroll_loop_p (class loop *loop, class tree_niter_desc *desc,
		      unsigned factor)
{
  if (!can_unroll_loop_p (loop, factor, desc))
    return false;

  /* We only consider loops without control flow for unrolling.  This is not
     a hard restriction -- tree_unroll_loop works with arbitrary loops
     as well; but the unrolling/prefetching is usually more profitable for
     loops consisting of a single basic block, and we want to limit the
     code growth.  */
  if (loop->num_nodes > 2)
    return false;

  return true;
}

/* Determine the coefficient by that unroll LOOP, from the information
   contained in the list of memory references REFS.  Description of
   number of iterations of LOOP is stored to DESC.  NINSNS is the number of
   insns of the LOOP.  EST_NITER is the estimated number of iterations of
   the loop, or -1 if no estimate is available.  */

static unsigned
determine_unroll_factor (class loop *loop, struct mem_ref_group *refs,
			 unsigned ninsns, class tree_niter_desc *desc,
			 HOST_WIDE_INT est_niter)
{
  unsigned upper_bound;
  unsigned nfactor, factor, mod_constraint;
  struct mem_ref_group *agp;
  struct mem_ref *ref;

  /* Bail out early in case we must not unroll loops.  */
  if (!flag_unroll_loops)
    return 1;

  /* First check whether the loop is not too large to unroll.  We ignore
     PARAM_MAX_UNROLL_TIMES, because for small loops, it prevented us
     from unrolling them enough to make exactly one cache line covered by each
     iteration.  Also, the goal of PARAM_MAX_UNROLL_TIMES is to prevent
     us from unrolling the loops too many times in cases where we only expect
     gains from better scheduling and decreasing loop overhead, which is not
     the case here.  */
  upper_bound = param_max_unrolled_insns / ninsns;

  /* If we unrolled the loop more times than it iterates, the unrolled version
     of the loop would be never entered.  */
  if (est_niter >= 0 && est_niter < (HOST_WIDE_INT) upper_bound)
    upper_bound = est_niter;

  if (upper_bound <= 1)
    return 1;

  /* Choose the factor so that we may prefetch each cache just once,
     but bound the unrolling by UPPER_BOUND.  */
  factor = 1;
  for (agp = refs; agp; agp = agp->next)
    for (ref = agp->refs; ref; ref = ref->next)
      if (should_issue_prefetch_p (ref))
	{
	  mod_constraint = ref->prefetch_mod;
	  nfactor = least_common_multiple (mod_constraint, factor);
	  if (nfactor <= upper_bound)
	    factor = nfactor;
	}

  if (!should_unroll_loop_p (loop, desc, factor))
    return 1;

  return factor;
}

/* Returns the total volume of the memory references REFS, taking into account
   reuses in the innermost loop and cache line size.  TODO -- we should also
   take into account reuses across the iterations of the loops in the loop
   nest.  */

static unsigned
volume_of_references (struct mem_ref_group *refs)
{
  unsigned volume = 0;
  struct mem_ref_group *gr;
  struct mem_ref *ref;

  for (gr = refs; gr; gr = gr->next)
    for (ref = gr->refs; ref; ref = ref->next)
      {
	/* Almost always reuses another value?  */
	if (ref->prefetch_before != PREFETCH_ALL)
	  continue;

	/* If several iterations access the same cache line, use the size of
	   the line divided by this number.  Otherwise, a cache line is
	   accessed in each iteration.  TODO -- in the latter case, we should
	   take the size of the reference into account, rounding it up on cache
	   line size multiple.  */
	volume += param_l1_cache_line_size / ref->prefetch_mod;
      }
  return volume;
}

/* Returns the volume of memory references accessed across VEC iterations of
   loops, whose sizes are described in the LOOP_SIZES array.  N is the number
   of the loops in the nest (length of VEC and LOOP_SIZES vectors).  */

static unsigned
volume_of_dist_vector (lambda_vector vec, unsigned *loop_sizes, unsigned n)
{
  unsigned i;

  for (i = 0; i < n; i++)
    if (vec[i] != 0)
      break;

  if (i == n)
    return 0;

  gcc_assert (vec[i] > 0);

  /* We ignore the parts of the distance vector in subloops, since usually
     the numbers of iterations are much smaller.  */
  return loop_sizes[i] * vec[i];
}

/* Add the steps of ACCESS_FN multiplied by STRIDE to the array STRIDE
   at the position corresponding to the loop of the step.  N is the depth
   of the considered loop nest, and, LOOP is its innermost loop.  */

static void
add_subscript_strides (tree access_fn, unsigned stride,
		       HOST_WIDE_INT *strides, unsigned n, class loop *loop)
{
  class loop *aloop;
  tree step;
  HOST_WIDE_INT astep;
  unsigned min_depth = loop_depth (loop) - n;

  while (TREE_CODE (access_fn) == POLYNOMIAL_CHREC)
    {
      aloop = get_chrec_loop (access_fn);
      step = CHREC_RIGHT (access_fn);
      access_fn = CHREC_LEFT (access_fn);

      if ((unsigned) loop_depth (aloop) <= min_depth)
	continue;

      if (tree_fits_shwi_p (step))
	astep = tree_to_shwi (step);
      else
	astep = param_l1_cache_line_size;

      strides[n - 1 - loop_depth (loop) + loop_depth (aloop)] += astep * stride;

    }
}

/* Returns the volume of memory references accessed between two consecutive
   self-reuses of the reference DR.  We consider the subscripts of DR in N
   loops, and LOOP_SIZES contains the volumes of accesses in each of the
   loops.  LOOP is the innermost loop of the current loop nest.  */

static unsigned
self_reuse_distance (data_reference_p dr, unsigned *loop_sizes, unsigned n,
		     class loop *loop)
{
  tree stride, access_fn;
  HOST_WIDE_INT *strides, astride;
  vec<tree> access_fns;
  tree ref = DR_REF (dr);
  unsigned i, ret = ~0u;

  /* In the following example:

     for (i = 0; i < N; i++)
       for (j = 0; j < N; j++)
         use (a[j][i]);
     the same cache line is accessed each N steps (except if the change from
     i to i + 1 crosses the boundary of the cache line).  Thus, for self-reuse,
     we cannot rely purely on the results of the data dependence analysis.

     Instead, we compute the stride of the reference in each loop, and consider
     the innermost loop in that the stride is less than cache size.  */

  strides = XCNEWVEC (HOST_WIDE_INT, n);
  access_fns = DR_ACCESS_FNS (dr);

  FOR_EACH_VEC_ELT (access_fns, i, access_fn)
    {
      /* Keep track of the reference corresponding to the subscript, so that we
	 know its stride.  */
      while (handled_component_p (ref) && TREE_CODE (ref) != ARRAY_REF)
	ref = TREE_OPERAND (ref, 0);

      if (TREE_CODE (ref) == ARRAY_REF)
	{
	  stride = TYPE_SIZE_UNIT (TREE_TYPE (ref));
	  if (tree_fits_uhwi_p (stride))
	    astride = tree_to_uhwi (stride);
	  else
	    astride = param_l1_cache_line_size;

	  ref = TREE_OPERAND (ref, 0);
	}
      else
	astride = 1;

      add_subscript_strides (access_fn, astride, strides, n, loop);
    }

  for (i = n; i-- > 0; )
    {
      unsigned HOST_WIDE_INT s;

      s = strides[i] < 0 ?  -strides[i] : strides[i];

      if (s < (unsigned) param_l1_cache_line_size
	  && (loop_sizes[i]
	      > (unsigned) (L1_CACHE_SIZE_BYTES / NONTEMPORAL_FRACTION)))
	{
	  ret = loop_sizes[i];
	  break;
	}
    }

  free (strides);
  return ret;
}

/* Determines the distance till the first reuse of each reference in REFS
   in the loop nest of LOOP.  NO_OTHER_REFS is true if there are no other
   memory references in the loop.  Return false if the analysis fails.  */

static bool
determine_loop_nest_reuse (class loop *loop, struct mem_ref_group *refs,
			   bool no_other_refs)
{
  class loop *nest, *aloop;
  vec<data_reference_p> datarefs = vNULL;
  vec<ddr_p> dependences = vNULL;
  struct mem_ref_group *gr;
  struct mem_ref *ref, *refb;
  auto_vec<loop_p> vloops;
  unsigned *loop_data_size;
  unsigned i, j, n;
  unsigned volume, dist, adist;
  HOST_WIDE_INT vol;
  data_reference_p dr;
  ddr_p dep;

  if (loop->inner)
    return true;

  /* Find the outermost loop of the loop nest of loop (we require that
     there are no sibling loops inside the nest).  */
  nest = loop;
  while (1)
    {
      aloop = loop_outer (nest);

      if (aloop == current_loops->tree_root
	  || aloop->inner->next)
	break;

      nest = aloop;
    }

  /* For each loop, determine the amount of data accessed in each iteration.
     We use this to estimate whether the reference is evicted from the
     cache before its reuse.  */
  find_loop_nest (nest, &vloops);
  n = vloops.length ();
  loop_data_size = XNEWVEC (unsigned, n);
  volume = volume_of_references (refs);
  i = n;
  while (i-- != 0)
    {
      loop_data_size[i] = volume;
      /* Bound the volume by the L2 cache size, since above this bound,
	 all dependence distances are equivalent.  */
      if (volume > L2_CACHE_SIZE_BYTES)
	continue;

      aloop = vloops[i];
      vol = estimated_stmt_executions_int (aloop);
      if (vol == -1)
	vol = expected_loop_iterations (aloop);
      volume *= vol;
    }

  /* Prepare the references in the form suitable for data dependence
     analysis.  We ignore unanalyzable data references (the results
     are used just as a heuristics to estimate temporality of the
     references, hence we do not need to worry about correctness).  */
  for (gr = refs; gr; gr = gr->next)
    for (ref = gr->refs; ref; ref = ref->next)
      {
	dr = create_data_ref (loop_preheader_edge (nest),
			      loop_containing_stmt (ref->stmt),
			      ref->mem, ref->stmt, !ref->write_p, false);

	if (dr)
	  {
	    ref->reuse_distance = volume;
	    dr->aux = ref;
	    datarefs.safe_push (dr);
	  }
	else
	  no_other_refs = false;
      }

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      dist = self_reuse_distance (dr, loop_data_size, n, loop);
      ref = (struct mem_ref *) dr->aux;
      if (ref->reuse_distance > dist)
	ref->reuse_distance = dist;

      if (no_other_refs)
	ref->independent_p = true;
    }

  if (!compute_all_dependences (datarefs, &dependences, vloops, true))
    return false;

  FOR_EACH_VEC_ELT (dependences, i, dep)
    {
      if (DDR_ARE_DEPENDENT (dep) == chrec_known)
	continue;

      ref = (struct mem_ref *) DDR_A (dep)->aux;
      refb = (struct mem_ref *) DDR_B (dep)->aux;

      if (DDR_ARE_DEPENDENT (dep) == chrec_dont_know
	  || DDR_COULD_BE_INDEPENDENT_P (dep)
	  || DDR_NUM_DIST_VECTS (dep) == 0)
	{
	  /* If the dependence cannot be analyzed, assume that there might be
	     a reuse.  */
	  dist = 0;

	  ref->independent_p = false;
	  refb->independent_p = false;
	}
      else
	{
	  /* The distance vectors are normalized to be always lexicographically
	     positive, hence we cannot tell just from them whether DDR_A comes
	     before DDR_B or vice versa.  However, it is not important,
	     anyway -- if DDR_A is close to DDR_B, then it is either reused in
	     DDR_B (and it is not nontemporal), or it reuses the value of DDR_B
	     in cache (and marking it as nontemporal would not affect
	     anything).  */

	  dist = volume;
	  for (j = 0; j < DDR_NUM_DIST_VECTS (dep); j++)
	    {
	      adist = volume_of_dist_vector (DDR_DIST_VECT (dep, j),
					     loop_data_size, n);

	      /* If this is a dependence in the innermost loop (i.e., the
		 distances in all superloops are zero) and it is not
		 the trivial self-dependence with distance zero, record that
		 the references are not completely independent.  */
	      if (lambda_vector_zerop (DDR_DIST_VECT (dep, j), n - 1)
		  && (ref != refb
		      || DDR_DIST_VECT (dep, j)[n-1] != 0))
		{
		  ref->independent_p = false;
		  refb->independent_p = false;
		}

	      /* Ignore accesses closer than
		 L1_CACHE_SIZE_BYTES / NONTEMPORAL_FRACTION,
	      	 so that we use nontemporal prefetches e.g. if single memory
		 location is accessed several times in a single iteration of
		 the loop.  */
	      if (adist < L1_CACHE_SIZE_BYTES / NONTEMPORAL_FRACTION)
		continue;

	      if (adist < dist)
		dist = adist;
	    }
	}

      if (ref->reuse_distance > dist)
	ref->reuse_distance = dist;
      if (refb->reuse_distance > dist)
	refb->reuse_distance = dist;
    }

  free_dependence_relations (dependences);
  free_data_refs (datarefs);
  free (loop_data_size);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Reuse distances:\n");
      for (gr = refs; gr; gr = gr->next)
	for (ref = gr->refs; ref; ref = ref->next)
	  fprintf (dump_file, " reference %u:%u distance %u\n",
		   ref->group->uid, ref->uid, ref->reuse_distance);
    }

  return true;
}

/* Determine whether or not the trip count to ahead ratio is too small based
   on prefitablility consideration.
   AHEAD: the iteration ahead distance,
   EST_NITER: the estimated trip count.  */

static bool
trip_count_to_ahead_ratio_too_small_p (unsigned ahead, HOST_WIDE_INT est_niter)
{
  /* Assume trip count to ahead ratio is big enough if the trip count could not
     be estimated at compile time.  */
  if (est_niter < 0)
    return false;

  if (est_niter < (HOST_WIDE_INT) (TRIP_COUNT_TO_AHEAD_RATIO * ahead))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Not prefetching -- loop estimated to roll only %d times\n",
		 (int) est_niter);
      return true;
    }

  return false;
}

/* Determine whether or not the number of memory references in the loop is
   reasonable based on the profitablity and compilation time considerations.
   NINSNS: estimated number of instructions in the loop,
   MEM_REF_COUNT: total number of memory references in the loop.  */

static bool
mem_ref_count_reasonable_p (unsigned ninsns, unsigned mem_ref_count)
{
  int insn_to_mem_ratio;

  if (mem_ref_count == 0)
    return false;

  /* Miss rate computation (is_miss_rate_acceptable) and dependence analysis
     (compute_all_dependences) have high costs based on quadratic complexity.
     To avoid huge compilation time, we give up prefetching if mem_ref_count
     is too large.  */
  if (mem_ref_count > PREFETCH_MAX_MEM_REFS_PER_LOOP)
    return false;

  /* Prefetching improves performance by overlapping cache missing
     memory accesses with CPU operations.  If the loop does not have
     enough CPU operations to overlap with memory operations, prefetching
     won't give a significant benefit.  One approximate way of checking
     this is to require the ratio of instructions to memory references to
     be above a certain limit.  This approximation works well in practice.
     TODO: Implement a more precise computation by estimating the time
     for each CPU or memory op in the loop. Time estimates for memory ops
     should account for cache misses.  */
  insn_to_mem_ratio = ninsns / mem_ref_count;

  if (insn_to_mem_ratio < param_prefetch_min_insn_to_mem_ratio)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file,
		 "Not prefetching -- instruction to memory reference ratio (%d) too small\n",
		 insn_to_mem_ratio);
      return false;
    }

  return true;
}

/* Determine whether or not the instruction to prefetch ratio in the loop is
   too small based on the profitablity consideration.
   NINSNS: estimated number of instructions in the loop,
   PREFETCH_COUNT: an estimate of the number of prefetches,
   UNROLL_FACTOR:  the factor to unroll the loop if prefetching.  */

static bool
insn_to_prefetch_ratio_too_small_p (unsigned ninsns, unsigned prefetch_count,
                                     unsigned unroll_factor)
{
  int insn_to_prefetch_ratio;

  /* Prefetching most likely causes performance degradation when the instruction
     to prefetch ratio is too small.  Too many prefetch instructions in a loop
     may reduce the I-cache performance.
     (unroll_factor * ninsns) is used to estimate the number of instructions in
     the unrolled loop.  This implementation is a bit simplistic -- the number
     of issued prefetch instructions is also affected by unrolling.  So,
     prefetch_mod and the unroll factor should be taken into account when
     determining prefetch_count.  Also, the number of insns of the unrolled
     loop will usually be significantly smaller than the number of insns of the
     original loop * unroll_factor (at least the induction variable increases
     and the exit branches will get eliminated), so it might be better to use
     tree_estimate_loop_size + estimated_unrolled_size.  */
  insn_to_prefetch_ratio = (unroll_factor * ninsns) / prefetch_count;
  if (insn_to_prefetch_ratio < param_min_insn_to_prefetch_ratio)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file,
		 "Not prefetching -- instruction to prefetch ratio (%d) too small\n",
		 insn_to_prefetch_ratio);
      return true;
    }

  return false;
}


/* Issue prefetch instructions for array references in LOOP.  Returns
   true if the LOOP was unrolled and updates NEED_LC_SSA_UPDATE if we need
   to update SSA for virtual operands and LC SSA for a split edge.  */

static bool
loop_prefetch_arrays (class loop *loop, bool &need_lc_ssa_update)
{
  struct mem_ref_group *refs;
  unsigned ahead, ninsns, time, unroll_factor;
  HOST_WIDE_INT est_niter;
  class tree_niter_desc desc;
  bool unrolled = false, no_other_refs;
  unsigned prefetch_count;
  unsigned mem_ref_count;

  if (optimize_loop_nest_for_size_p (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  ignored (cold area)\n");
      return false;
    }

  /* FIXME: the time should be weighted by the probabilities of the blocks in
     the loop body.  */
  time = tree_num_loop_insns (loop, &eni_time_weights);
  if (time == 0)
    return false;

  ahead = (param_prefetch_latency + time - 1) / time;
  est_niter = estimated_stmt_executions_int (loop);
  if (est_niter == -1)
    est_niter = likely_max_stmt_executions_int (loop);

  /* Prefetching is not likely to be profitable if the trip count to ahead
     ratio is too small.  */
  if (trip_count_to_ahead_ratio_too_small_p (ahead, est_niter))
    return false;

  ninsns = tree_num_loop_insns (loop, &eni_size_weights);

  /* Step 1: gather the memory references.  */
  refs = gather_memory_references (loop, &no_other_refs, &mem_ref_count);

  /* Give up prefetching if the number of memory references in the
     loop is not reasonable based on profitablity and compilation time
     considerations.  */
  if (!mem_ref_count_reasonable_p (ninsns, mem_ref_count))
    goto fail;

  /* Step 2: estimate the reuse effects.  */
  prune_by_reuse (refs);

  if (nothing_to_prefetch_p (refs))
    goto fail;

  if (!determine_loop_nest_reuse (loop, refs, no_other_refs))
    goto fail;

  /* Step 3: determine unroll factor.  */
  unroll_factor = determine_unroll_factor (loop, refs, ninsns, &desc,
					   est_niter);

  /* Estimate prefetch count for the unrolled loop.  */
  prefetch_count = estimate_prefetch_count (refs, unroll_factor);
  if (prefetch_count == 0)
    goto fail;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Ahead %d, unroll factor %d, trip count "
	     HOST_WIDE_INT_PRINT_DEC "\n"
	     "insn count %d, mem ref count %d, prefetch count %d\n",
	     ahead, unroll_factor, est_niter,
	     ninsns, mem_ref_count, prefetch_count);

  /* Prefetching is not likely to be profitable if the instruction to prefetch
     ratio is too small.  */
  if (insn_to_prefetch_ratio_too_small_p (ninsns, prefetch_count,
					  unroll_factor))
    goto fail;

  need_lc_ssa_update |= mark_nontemporal_stores (loop, refs);

  /* Step 4: what to prefetch?  */
  if (!schedule_prefetches (refs, unroll_factor, ahead))
    goto fail;

  /* Step 5: unroll the loop.  TODO -- peeling of first and last few
     iterations so that we do not issue superfluous prefetches.  */
  if (unroll_factor != 1)
    {
      tree_unroll_loop (loop, unroll_factor, &desc);
      unrolled = true;
    }

  /* Step 6: issue the prefetches.  */
  issue_prefetches (refs, unroll_factor, ahead);

fail:
  release_mem_refs (refs);
  return unrolled;
}

/* Issue prefetch instructions for array references in loops.  */

unsigned int
tree_ssa_prefetch_arrays (void)
{
  bool unrolled = false;
  bool need_lc_ssa_update = false;
  int todo_flags = 0;

  if (!targetm.have_prefetch ()
      /* It is possible to ask compiler for say -mtune=i486 -march=pentium4.
	 -mtune=i486 causes us having PREFETCH_BLOCK 0, since this is part
	 of processor costs and i486 does not have prefetch, but
	 -march=pentium4 causes targetm.have_prefetch to be true.  Ugh.  */
      || PREFETCH_BLOCK == 0)
    return 0;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Prefetching parameters:\n");
      fprintf (dump_file, "    simultaneous prefetches: %d\n",
	       param_simultaneous_prefetches);
      fprintf (dump_file, "    prefetch latency: %d\n", param_prefetch_latency);
      fprintf (dump_file, "    prefetch block size: %d\n", PREFETCH_BLOCK);
      fprintf (dump_file, "    L1 cache size: %d lines, %d kB\n",
	       L1_CACHE_SIZE_BYTES / param_l1_cache_line_size,
	       param_l1_cache_size);
      fprintf (dump_file, "    L1 cache line size: %d\n",
	       param_l1_cache_line_size);
      fprintf (dump_file, "    L2 cache size: %d kB\n", param_l2_cache_size);
      fprintf (dump_file, "    min insn-to-prefetch ratio: %d \n",
	       param_min_insn_to_prefetch_ratio);
      fprintf (dump_file, "    min insn-to-mem ratio: %d \n",
	       param_prefetch_min_insn_to_mem_ratio);
      fprintf (dump_file, "\n");
    }

  initialize_original_copy_tables ();

  if (!builtin_decl_explicit_p (BUILT_IN_PREFETCH))
    {
      tree type = build_function_type_list (void_type_node,
					    const_ptr_type_node, NULL_TREE);
      tree decl = add_builtin_function ("__builtin_prefetch", type,
					BUILT_IN_PREFETCH, BUILT_IN_NORMAL,
					NULL, NULL_TREE);
      DECL_IS_NOVOPS (decl) = true;
      set_builtin_decl (BUILT_IN_PREFETCH, decl, false);
    }

  for (auto loop : loops_list (cfun, LI_FROM_INNERMOST))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Processing loop %d:\n", loop->num);

      unrolled |= loop_prefetch_arrays (loop, need_lc_ssa_update);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\n\n");
    }

  if (need_lc_ssa_update)
    rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa_only_virtuals);

  if (unrolled)
    {
      scev_reset ();
      todo_flags |= TODO_cleanup_cfg;
    }

  free_original_copy_tables ();
  return todo_flags;
}

/* Prefetching.  */

namespace {

const pass_data pass_data_loop_prefetch =
{
  GIMPLE_PASS, /* type */
  "aprefetch", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_PREFETCH, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop_prefetch : public gimple_opt_pass
{
public:
  pass_loop_prefetch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_loop_prefetch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
  {
    return flag_prefetch_loop_arrays > 0;
  }
  unsigned int execute (function *) final override;

}; // class pass_loop_prefetch

unsigned int
pass_loop_prefetch::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  if ((PREFETCH_BLOCK & (PREFETCH_BLOCK - 1)) != 0)
    {
      static bool warned = false;

      if (!warned)
	{
	  warning (OPT_Wdisabled_optimization,
		   "%<l1-cache-size%> parameter is not a power of two: %d",
		   PREFETCH_BLOCK);
	  warned = true;
	}
      return 0;
    }

  return tree_ssa_prefetch_arrays ();
}

} // anon namespace

gimple_opt_pass *
make_pass_loop_prefetch (gcc::context *ctxt)
{
  return new pass_loop_prefetch (ctxt);
}


