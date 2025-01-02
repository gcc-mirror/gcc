/* Loop versioning pass.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.

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
#include "tree.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-pass.h"
#include "gimplify-me.h"
#include "cfgloop.h"
#include "tree-ssa-loop.h"
#include "ssa.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop-ivopts.h"
#include "fold-const.h"
#include "tree-ssa-propagate.h"
#include "tree-inline.h"
#include "domwalk.h"
#include "tree-vectorizer.h"
#include "omp-general.h"
#include "predict.h"
#include "tree-into-ssa.h"
#include "gimple-range.h"
#include "tree-cfg.h"

namespace {

/* This pass looks for loops that could be simplified if certain loop
   invariant conditions were true.  It is effectively a form of loop
   splitting in which the pass produces the split conditions itself,
   instead of using ones that are already present in the IL.

   Versioning for when strides are 1
   ---------------------------------

   At the moment the only thing the pass looks for are memory references
   like:

     for (auto i : ...)
       ...x[i * stride]...

   It considers changing such loops to:

     if (stride == 1)
       for (auto i : ...)    [A]
	 ...x[i]...
     else
       for (auto i : ...)    [B]
	 ...x[i * stride]...

   This can have several benefits:

   (1) [A] is often easier or cheaper to vectorize than [B].

   (2) The scalar code in [A] is simpler than the scalar code in [B]
       (if the loops cannot be vectorized or need an epilogue loop).

   (3) We might recognize [A] as a pattern, such as a memcpy or memset.

   (4) [A] has simpler address evolutions, which can help other passes
       like loop interchange.

   The optimization is particularly useful for assumed-shape arrays in
   Fortran, where the stride of the innermost dimension depends on the
   array descriptor but is often equal to 1 in practice.  For example:

     subroutine f1(x)
       real :: x(:)
       x(:) = 100
     end subroutine f1

   generates the equivalent of:

     raw_stride = *x.dim[0].stride;
     stride = raw_stride != 0 ? raw_stride : 1;
     x_base = *x.data;
     ...
     tmp1 = stride * S;
     tmp2 = tmp1 - stride;
     *x_base[tmp2] = 1.0e+2;

   but in the common case that stride == 1, the last three statements
   simplify to:

     tmp3 = S + -1;
     *x_base[tmp3] = 1.0e+2;

   The optimization is in principle very simple.  The difficult parts are:

   (a) deciding which parts of a general address calculation correspond
       to the inner dimension of an array, since this usually isn't explicit
       in the IL, and for C often isn't even explicit in the source code

   (b) estimating when the transformation is worthwhile

   Structure
   ---------

   The pass has four phases:

   (1) Walk through the statements looking for and recording potential
       versioning opportunities.  Stop if there are none.

   (2) Use context-sensitive range information to see whether any versioning
       conditions are impossible in practice.  Remove them if so, and stop
       if no opportunities remain.

       (We do this only after (1) to keep compile time down when no
       versioning opportunities exist.)

   (3) Apply the cost model.  Decide which versioning opportunities are
       worthwhile and at which nesting level they should be applied.

   (4) Attempt to version all the loops selected by (3), so that:

	 for (...)
	   ...

       becomes:

	 if (!cond)
	   for (...) // Original loop
	     ...
	 else
	   for (...) // New loop
	     ...

       Use the version condition COND to simplify the new loop.  */

/* Enumerates the likelihood that a particular value indexes the inner
   dimension of an array.  */
enum inner_likelihood {
  INNER_UNLIKELY,
  INNER_DONT_KNOW,
  INNER_LIKELY
};

/* Information about one term of an address_info.  */
struct address_term_info
{
  /* The value of the term is EXPR * MULTIPLIER.  */
  tree expr;
  unsigned HOST_WIDE_INT multiplier;

  /* The stride applied by EXPR in each iteration of some unrecorded loop,
     or null if no stride has been identified.  */
  tree stride;

  /* Enumerates the likelihood that EXPR indexes the inner dimension
     of an array.  */
  enum inner_likelihood inner_likelihood;

  /* True if STRIDE == 1 is a versioning opportunity when considered
     in isolation.  */
  bool versioning_opportunity_p;
};

/* Information about an address calculation, and the range of constant
   offsets applied to it.  */
class address_info
{
public:
  static const unsigned int MAX_TERMS = 8;

  /* One statement that calculates the address.  If multiple statements
     share the same address, we only record the first.  */
  gimple *stmt;

  /* The loop containing STMT (cached for convenience).  If multiple
     statements share the same address, they all belong to this loop.  */
  class loop *loop;

  /* A decomposition of the calculation into a sum of terms plus an
     optional base.  When BASE is provided, it is never an SSA name.
     Once initialization is complete, all members of TERMs are SSA names.  */
  tree base;
  auto_vec<address_term_info, MAX_TERMS> terms;

  /* All bytes accessed from the address fall in the offset range
     [MIN_OFFSET, MAX_OFFSET).  */
  HOST_WIDE_INT min_offset, max_offset;
};

/* Stores addresses based on their base and terms (ignoring the offsets).  */
struct address_info_hasher : nofree_ptr_hash <address_info>
{
  static hashval_t hash (const address_info *);
  static bool equal (const address_info *, const address_info *);
};

/* Information about the versioning we'd like to apply to a loop.  */
class loop_info
{
public:
  bool worth_versioning_p () const;

  /* True if we've decided not to version this loop.  The remaining
     fields are meaningless if so.  */
  bool rejected_p;

  /* True if at least one subloop of this loop benefits from versioning.  */
  bool subloops_benefit_p;

  /* An estimate of the total number of instructions in the loop,
     excluding those in subloops that benefit from versioning.  */
  unsigned int num_insns;

  /* The outermost loop that can handle all the version checks
     described below.  */
  class loop *outermost;

  /* The first entry in the list of blocks that belong to this loop
     (and not to subloops).  m_next_block_in_loop provides the chain
     pointers for the list.  */
  basic_block block_list;

  /* We'd like to version the loop for the case in which these SSA names
     (keyed off their SSA_NAME_VERSION) are all equal to 1 at runtime.  */
  bitmap_head unity_names;

  /* If versioning succeeds, this points the version of the loop that
     assumes the version conditions holds.  */
  class loop *optimized_loop;
};

/* The main pass structure.  */
class loop_versioning
{
public:
  loop_versioning (function *);
  ~loop_versioning ();
  unsigned int run ();

private:
  /* Used to walk the dominator tree to find loop versioning conditions
     that are always false.  */
  class lv_dom_walker : public dom_walker
  {
  public:
    lv_dom_walker (loop_versioning &);

    edge before_dom_children (basic_block) final override;

  private:
    /* The parent pass.  */
    loop_versioning &m_lv;
  };

  /* Used to simplify statements based on conditions that are established
     by the version checks.  */
  class name_prop : public substitute_and_fold_engine
  {
  public:
    name_prop (loop_info &li) : m_li (li) {}
    tree value_of_expr (tree name, gimple *) final override;

  private:
    /* Information about the versioning we've performed on the loop.  */
    loop_info &m_li;
  };

  loop_info &get_loop_info (class loop *loop) { return m_loops[loop->num]; }

  unsigned int max_insns_for_loop (class loop *);
  bool expensive_stmt_p (gimple *);

  void version_for_unity (gimple *, tree);
  bool acceptable_multiplier_p (tree, unsigned HOST_WIDE_INT,
				unsigned HOST_WIDE_INT * = 0);
  bool acceptable_type_p (tree, unsigned HOST_WIDE_INT *);
  bool multiply_term_by (address_term_info &, tree);
  inner_likelihood get_inner_likelihood (tree, unsigned HOST_WIDE_INT);
  void dump_inner_likelihood (address_info &, address_term_info &);
  void analyze_stride (address_info &, address_term_info &,
		       tree, class loop *);
  bool find_per_loop_multiplication (address_info &, address_term_info &);
  bool analyze_term_using_scevs (address_info &, address_term_info &);
  void analyze_arbitrary_term (address_info &, address_term_info &);
  void analyze_address_fragment (address_info &);
  void record_address_fragment (gimple *, unsigned HOST_WIDE_INT,
				tree, unsigned HOST_WIDE_INT, HOST_WIDE_INT);
  void analyze_expr (gimple *, tree);
  bool analyze_block (basic_block);
  bool analyze_blocks ();

  void prune_loop_conditions (class loop *);
  bool prune_conditions ();

  void merge_loop_info (class loop *, class loop *);
  void add_loop_to_queue (class loop *);
  bool decide_whether_loop_is_versionable (class loop *);
  bool make_versioning_decisions ();

  bool version_loop (class loop *);
  void implement_versioning_decisions ();

  /* The function we're optimizing.  */
  function *m_fn;

  /* The obstack to use for all pass-specific bitmaps.  */
  bitmap_obstack m_bitmap_obstack;

  /* An obstack to use for general allocation.  */
  obstack m_obstack;

  /* The total number of loop version conditions we've found.  */
  unsigned int m_num_conditions;

  /* Assume that an address fragment of the form i * stride * scale
     (for variable stride and constant scale) will not benefit from
     versioning for stride == 1 when scale is greater than this value.  */
  unsigned HOST_WIDE_INT m_maximum_scale;

  /* Information about each loop.  */
  auto_vec<loop_info> m_loops;

  /* Used to form a linked list of blocks that belong to a loop,
     started by loop_info::block_list.  */
  auto_vec<basic_block> m_next_block_in_loop;

  /* The list of loops that we've decided to version.  */
  auto_vec<class loop *> m_loops_to_version;

  /* A table of addresses in the current loop, keyed off their values
     but not their offsets.  */
  hash_table <address_info_hasher> m_address_table;

  /* A list of all addresses in M_ADDRESS_TABLE, in a predictable order.  */
  auto_vec <address_info *, 32> m_address_list;
};

/* If EXPR is an SSA name and not a default definition, return the
   defining statement, otherwise return null.  */

static gimple *
maybe_get_stmt (tree expr)
{
  if (TREE_CODE (expr) == SSA_NAME && !SSA_NAME_IS_DEFAULT_DEF (expr))
    return SSA_NAME_DEF_STMT (expr);
  return NULL;
}

/* Like maybe_get_stmt, but also return null if the defining
   statement isn't an assignment.  */

static gassign *
maybe_get_assign (tree expr)
{
  return safe_dyn_cast <gassign *> (maybe_get_stmt (expr));
}

/* Return true if this pass should look through a cast of expression FROM
   to type TYPE when analyzing pieces of an address.  */

static bool
look_through_cast_p (tree type, tree from)
{
  return (INTEGRAL_TYPE_P (TREE_TYPE (from)) == INTEGRAL_TYPE_P (type)
	  && POINTER_TYPE_P (TREE_TYPE (from)) == POINTER_TYPE_P (type));
}

/* Strip all conversions of integers or pointers from EXPR, regardless
   of whether the conversions are nops.  This is useful in the context
   of this pass because we're not trying to fold or simulate the
   expression; we just want to see how it's structured.  */

static tree
strip_casts (tree expr)
{
  const unsigned int MAX_NITERS = 4;

  tree type = TREE_TYPE (expr);
  while (CONVERT_EXPR_P (expr)
	 && look_through_cast_p (type, TREE_OPERAND (expr, 0)))
    expr = TREE_OPERAND (expr, 0);

  for (unsigned int niters = 0; niters < MAX_NITERS; ++niters)
    {
      gassign *assign = maybe_get_assign (expr);
      if (assign
	  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (assign))
	  && look_through_cast_p (type, gimple_assign_rhs1 (assign)))
	expr = gimple_assign_rhs1 (assign);
      else
	break;
    }
  return expr;
}

/* Compare two address_term_infos in the same address_info.  */

static int
compare_address_terms (const void *a_uncast, const void *b_uncast)
{
  const address_term_info *a = (const address_term_info *) a_uncast;
  const address_term_info *b = (const address_term_info *) b_uncast;

  if (a->expr != b->expr)
    return SSA_NAME_VERSION (a->expr) < SSA_NAME_VERSION (b->expr) ? -1 : 1;

  if (a->multiplier != b->multiplier)
    return a->multiplier < b->multiplier ? -1 : 1;

  return 0;
}

/* Dump ADDRESS using flags FLAGS.  */

static void
dump_address_info (dump_flags_t flags, address_info &address)
{
  if (address.base)
    dump_printf (flags, "%T + ", address.base);
  for (unsigned int i = 0; i < address.terms.length (); ++i)
    {
      if (i != 0)
	dump_printf (flags, " + ");
      dump_printf (flags, "%T", address.terms[i].expr);
      if (address.terms[i].multiplier != 1)
	dump_printf (flags, " * %wd", address.terms[i].multiplier);
    }
  dump_printf (flags, " + [%wd, %wd]",
	       address.min_offset, address.max_offset - 1);
}

/* Hash an address_info based on its base and terms.  */

hashval_t
address_info_hasher::hash (const address_info *info)
{
  inchash::hash hash;
  hash.add_int (info->base ? TREE_CODE (info->base) : 0);
  hash.add_int (info->terms.length ());
  for (unsigned int i = 0; i < info->terms.length (); ++i)
    {
      hash.add_int (SSA_NAME_VERSION (info->terms[i].expr));
      hash.add_hwi (info->terms[i].multiplier);
    }
  return hash.end ();
}

/* Return true if two address_infos have equal bases and terms.  Other
   properties might be different (such as the statement or constant
   offset range).  */

bool
address_info_hasher::equal (const address_info *a, const address_info *b)
{
  if (a->base != b->base
      && (!a->base || !b->base || !operand_equal_p (a->base, b->base, 0)))
    return false;

  if (a->terms.length () != b->terms.length ())
    return false;

  for (unsigned int i = 0; i < a->terms.length (); ++i)
    if (a->terms[i].expr != b->terms[i].expr
	|| a->terms[i].multiplier != b->terms[i].multiplier)
      return false;

  return true;
}

/* Return true if we want to version the loop, i.e. if we have a
   specific reason for doing so and no specific reason not to.  */

bool
loop_info::worth_versioning_p () const
{
  return (!rejected_p
	  && (!bitmap_empty_p (&unity_names) || subloops_benefit_p));
}

loop_versioning::lv_dom_walker::lv_dom_walker (loop_versioning &lv)
  : dom_walker (CDI_DOMINATORS), m_lv (lv)
{
}

/* Process BB before processing the blocks it dominates.  */

edge
loop_versioning::lv_dom_walker::before_dom_children (basic_block bb)
{
  if (bb == bb->loop_father->header)
    m_lv.prune_loop_conditions (bb->loop_father);

  return NULL;
}

/* Decide whether to replace VAL with a new value in a versioned loop.
   Return the new value if so, otherwise return null.  */

tree
loop_versioning::name_prop::value_of_expr (tree val, gimple *)
{
  if (TREE_CODE (val) == SSA_NAME
      && bitmap_bit_p (&m_li.unity_names, SSA_NAME_VERSION (val)))
    return build_one_cst (TREE_TYPE (val));
  return NULL_TREE;
}

/* Initialize the structure to optimize FN.  */

loop_versioning::loop_versioning (function *fn)
  : m_fn (fn),
    m_num_conditions (0),
    m_address_table (31)
{
  unsigned m_nloops = number_of_loops (fn);
  bitmap_obstack_initialize (&m_bitmap_obstack);
  gcc_obstack_init (&m_obstack);

  /* Initialize the loop information.  */
  m_loops.safe_grow_cleared (m_nloops, true);
  for (unsigned int i = 0; i < m_nloops; ++i)
    {
      m_loops[i].outermost = get_loop (m_fn, 0);
      bitmap_initialize (&m_loops[i].unity_names, &m_bitmap_obstack);
    }

  /* Initialize the list of blocks that belong to each loop.  */
  unsigned int nbbs = last_basic_block_for_fn (fn);
  m_next_block_in_loop.safe_grow (nbbs, true);
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      loop_info &li = get_loop_info (bb->loop_father);
      m_next_block_in_loop[bb->index] = li.block_list;
      li.block_list = bb;
    }

  /* MAX_FIXED_MODE_SIZE should be a reasonable maximum scale for
     unvectorizable code, since it is the largest size that can be
     handled efficiently by scalar code.  omp_max_vf calculates the
     maximum number of bytes in a vector, when such a value is relevant
     to loop optimization.  */
  m_maximum_scale = estimated_poly_value (omp_max_vf (false));
  m_maximum_scale = MAX (m_maximum_scale, MAX_FIXED_MODE_SIZE);
}

loop_versioning::~loop_versioning ()
{
  bitmap_obstack_release (&m_bitmap_obstack);
  obstack_free (&m_obstack, NULL);
}

/* Return the maximum number of instructions allowed in LOOP before
   it becomes too big for versioning.

   There are separate limits for inner and outer loops.  The limit for
   inner loops applies only to loops that benefit directly from versioning.
   The limit for outer loops applies to all code in the outer loop and
   its subloops that *doesn't* benefit directly from versioning; such code
   would be "taken along for the ride".  The idea is that if the cost of
   the latter is small, it is better to version outer loops rather than
   inner loops, both to reduce the number of repeated checks and to enable
   more of the loop nest to be optimized as a natural nest (e.g. by loop
   interchange or outer-loop vectorization).  */

unsigned int
loop_versioning::max_insns_for_loop (class loop *loop)
{
  return (loop->inner
	  ? param_loop_versioning_max_outer_insns
	  : param_loop_versioning_max_inner_insns);
}

/* Return true if for cost reasons we should avoid versioning any loop
   that contains STMT.

   Note that we don't need to check whether versioning is invalid for
   correctness reasons, since the versioning process does that for us.
   The conditions involved are too rare to be worth duplicating here.  */

bool
loop_versioning::expensive_stmt_p (gimple *stmt)
{
  if (gcall *call = dyn_cast <gcall *> (stmt))
    /* Assume for now that the time spent in an "expensive" call would
       overwhelm any saving from versioning.  */
    return !gimple_inexpensive_call_p (call);
  return false;
}

/* Record that we want to version the loop that contains STMT for the
   case in which SSA name NAME is equal to 1.  We already know that NAME
   is invariant in the loop.  */

void
loop_versioning::version_for_unity (gimple *stmt, tree name)
{
  class loop *loop = loop_containing_stmt (stmt);
  loop_info &li = get_loop_info (loop);

  if (bitmap_set_bit (&li.unity_names, SSA_NAME_VERSION (name)))
    {
      /* This is the first time we've wanted to version LOOP for NAME.
	 Keep track of the outermost loop that can handle all versioning
	 checks in LI.  */
      class loop *outermost
	= outermost_invariant_loop_for_expr (loop, name);
      if (loop_depth (li.outermost) < loop_depth (outermost))
	li.outermost = outermost;

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, stmt, "want to version containing loop"
			   " for when %T == 1", name);
	  if (outermost == loop)
	    dump_printf (MSG_NOTE, "; cannot hoist check further");
	  else
	    {
	      dump_printf (MSG_NOTE, "; could implement the check at loop"
			   " depth %d", loop_depth (outermost));
	      if (loop_depth (li.outermost) > loop_depth (outermost))
		dump_printf (MSG_NOTE, ", but other checks only allow"
			     " a depth of %d", loop_depth (li.outermost));
	    }
	  dump_printf (MSG_NOTE, "\n");
	}

      m_num_conditions += 1;
    }
  else
    {
      /* This is a duplicate request.  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, stmt, "already asked to version containing"
			 " loop for when %T == 1\n", name);
    }
}

/* Return true if OP1_TREE is constant and if in principle it is worth
   versioning an address fragment of the form:

     i * OP1_TREE * OP2 * stride

   for the case in which stride == 1.  This in practice means testing
   whether:

     OP1_TREE * OP2 <= M_MAXIMUM_SCALE.

   If RESULT is nonnull, store OP1_TREE * OP2 there when returning true.  */

bool
loop_versioning::acceptable_multiplier_p (tree op1_tree,
					  unsigned HOST_WIDE_INT op2,
					  unsigned HOST_WIDE_INT *result)
{
  if (tree_fits_uhwi_p (op1_tree))
    {
      unsigned HOST_WIDE_INT op1 = tree_to_uhwi (op1_tree);
      /* The first part checks for overflow.  */
      if (op1 * op2 >= op2 && op1 * op2 <= m_maximum_scale)
	{
	  if (result)
	    *result = op1 * op2;
	  return true;
	}
    }
  return false;
}

/* Return true if it is worth using loop versioning on a memory access
   of type TYPE.  Store the size of the access in *SIZE if so.  */

bool
loop_versioning::acceptable_type_p (tree type, unsigned HOST_WIDE_INT *size)
{
  return (TYPE_SIZE_UNIT (type)
	  && acceptable_multiplier_p (TYPE_SIZE_UNIT (type), 1, size));
}

/* See whether OP is constant and whether we can multiply TERM by that
   constant without exceeding M_MAXIMUM_SCALE.  Return true and update
   TERM if so.  */

bool
loop_versioning::multiply_term_by (address_term_info &term, tree op)
{
  return acceptable_multiplier_p (op, term.multiplier, &term.multiplier);
}

/* Decide whether an address fragment of the form STRIDE * MULTIPLIER
   is likely to be indexing an innermost dimension, returning the result
   as an INNER_* probability.  */

inner_likelihood
loop_versioning::get_inner_likelihood (tree stride,
				       unsigned HOST_WIDE_INT multiplier)
{
  const unsigned int MAX_NITERS = 8;

  /* Iterate over possible values of STRIDE.  Return INNER_LIKELY if at
     least one of those values is likely to be for the innermost dimension.
     Record in UNLIKELY_P if at least one of those values is unlikely to be
     for the innermost dimension.

     E.g. for:

       stride = cond ? a * b : 1

     we should treat STRIDE as being a likely inner dimension, since
     we know that it is 1 under at least some circumstances.  (See the
     Fortran example below.)  However:

       stride = a * b

     on its own is unlikely to be for the innermost dimension, since
     that would require both a and b to be 1 at runtime.  */
  bool unlikely_p = false;
  tree worklist[MAX_NITERS];
  unsigned int length = 0;
  worklist[length++] = stride;
  for (unsigned int i = 0; i < length; ++i)
    {
      tree expr = worklist[i];

      if (CONSTANT_CLASS_P (expr))
	{
	  /* See if EXPR * MULTIPLIER would be consistent with an individual
	     access or a small grouped access.  */
	  if (acceptable_multiplier_p (expr, multiplier))
	    return INNER_LIKELY;
	  else
	    unlikely_p = true;
	}
      else if (gimple *stmt = maybe_get_stmt (expr))
	{
	  /* If EXPR is set by a PHI node, queue its arguments in case
	     we find one that is consistent with an inner dimension.

	     An important instance of this is the Fortran handling of array
	     descriptors, which calculates the stride of the inner dimension
	     using a PHI equivalent of:

		raw_stride = a.dim[0].stride;
		stride = raw_stride != 0 ? raw_stride : 1;

	     (Strides for outer dimensions do not treat 0 specially.)  */
	  if (gphi *phi = dyn_cast <gphi *> (stmt))
	    {
	      unsigned int nargs = gimple_phi_num_args (phi);
	      for (unsigned int j = 0; j < nargs && length < MAX_NITERS; ++j)
		worklist[length++] = strip_casts (gimple_phi_arg_def (phi, j));
	    }
	  /* If the value is set by an assignment, expect it to be read
	     from memory (such as an array descriptor) rather than be
	     calculated.  */
	  else if (gassign *assign = dyn_cast <gassign *> (stmt))
	    {
	      if (!gimple_assign_load_p (assign))
		unlikely_p = true;
	    }
	  /* Things like calls don't really tell us anything.  */
	}
    }

  /* We didn't find any possible values of STRIDE that were likely to be
     for the innermost dimension.  If we found one that was actively
     unlikely to be for the innermost dimension, assume that that applies
     to STRIDE too.  */
  return unlikely_p ? INNER_UNLIKELY : INNER_DONT_KNOW;
}

/* Dump the likelihood that TERM's stride is for the innermost dimension.
   ADDRESS is the address that contains TERM.  */

void
loop_versioning::dump_inner_likelihood (address_info &address,
					address_term_info &term)
{
  if (term.inner_likelihood == INNER_LIKELY)
    dump_printf_loc (MSG_NOTE, address.stmt, "%T is likely to be the"
		     " innermost dimension\n", term.stride);
  else if (term.inner_likelihood == INNER_UNLIKELY)
    dump_printf_loc (MSG_NOTE, address.stmt, "%T is probably not the"
		     " innermost dimension\n", term.stride);
  else
    dump_printf_loc (MSG_NOTE, address.stmt, "cannot tell whether %T"
		     " is the innermost dimension\n", term.stride);
}

/* The caller has identified that STRIDE is the stride of interest
   in TERM, and that the stride is applied in OP_LOOP.  Record this
   information in TERM, deciding whether STRIDE is likely to be for
   the innermost dimension of an array and whether it represents a
   versioning opportunity.  ADDRESS is the address that contains TERM.  */

void
loop_versioning::analyze_stride (address_info &address,
				 address_term_info &term,
				 tree stride, class loop *op_loop)
{
  term.stride = stride;

  term.inner_likelihood = get_inner_likelihood (stride, term.multiplier);
  if (dump_enabled_p ())
    dump_inner_likelihood (address, term);

  /* To be a versioning opportunity we require:

     - The multiplier applied by TERM is equal to the access size,
       so that when STRIDE is 1, the accesses in successive loop
       iterations are consecutive.

       This is deliberately conservative.  We could relax it to handle
       other cases (such as those with gaps between iterations) if we
       find any real testcases for which it's useful.

     - the stride is applied in the same loop as STMT rather than
       in an outer loop.  Although versioning for strides applied in
       outer loops could help in some cases -- such as enabling
       more loop interchange -- the savings are much lower than for
       inner loops.

     - the stride is an SSA name that is invariant in STMT's loop,
       since otherwise versioning isn't possible.  */
  unsigned HOST_WIDE_INT access_size = address.max_offset - address.min_offset;
  if (term.multiplier == access_size
      && address.loop == op_loop
      && TREE_CODE (stride) == SSA_NAME
      && expr_invariant_in_loop_p (address.loop, stride))
    {
      term.versioning_opportunity_p = true;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, address.stmt, "%T == 1 is a versioning"
			 " opportunity\n", stride);
    }
}

/* See whether address term TERM (which belongs to ADDRESS) is the result
   of multiplying a varying SSA name by a loop-invariant SSA name.
   Return true and update TERM if so.

   This handles both cases that SCEV might handle, such as:

     for (int i = 0; i < n; ++i)
       res += a[i * stride];

   and ones in which the term varies arbitrarily between iterations, such as:

     for (int i = 0; i < n; ++i)
       res += a[index[i] * stride];  */

bool
loop_versioning::find_per_loop_multiplication (address_info &address,
					       address_term_info &term)
{
  gassign *mult = maybe_get_assign (term.expr);
  if (!mult || gimple_assign_rhs_code (mult) != MULT_EXPR)
    return false;

  class loop *mult_loop = loop_containing_stmt (mult);
  if (!loop_outer (mult_loop))
    return false;

  tree op1 = strip_casts (gimple_assign_rhs1 (mult));
  tree op2 = strip_casts (gimple_assign_rhs2 (mult));
  if (TREE_CODE (op1) != SSA_NAME || TREE_CODE (op2) != SSA_NAME)
    return false;

  bool invariant1_p = expr_invariant_in_loop_p (mult_loop, op1);
  bool invariant2_p = expr_invariant_in_loop_p (mult_loop, op2);
  if (invariant1_p == invariant2_p)
    return false;

  /* Make sure that the loop invariant is OP2 rather than OP1.  */
  if (invariant1_p)
    std::swap (op1, op2);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, address.stmt, "address term %T = varying %T"
		     " * loop-invariant %T\n", term.expr, op1, op2);
  analyze_stride (address, term, op2, mult_loop);
  return true;
}

/* Try to use scalar evolutions to find an address stride for TERM,
   which belongs to ADDRESS.  Return true and update TERM if so.

   Here we are interested in any evolution information we can find,
   not just evolutions wrt ADDRESS->LOOP.  For example, if we find that
   an outer loop obviously iterates over the inner dimension of an array,
   that information can help us eliminate worthless versioning opportunities
   in inner loops.  */

bool
loop_versioning::analyze_term_using_scevs (address_info &address,
					   address_term_info &term)
{
  gimple *setter = maybe_get_stmt (term.expr);
  if (!setter)
    return false;

  class loop *wrt_loop = loop_containing_stmt (setter);
  if (!loop_outer (wrt_loop))
    return false;

  tree chrec = strip_casts (analyze_scalar_evolution (wrt_loop, term.expr));
  if (TREE_CODE (chrec) == POLYNOMIAL_CHREC)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, address.stmt,
			 "address term %T = %T\n", term.expr, chrec);

      /* Peel casts and accumulate constant multiplications, up to the
	 limit allowed by M_MAXIMUM_SCALE.  */
      tree stride = strip_casts (CHREC_RIGHT (chrec));
      while (TREE_CODE (stride) == MULT_EXPR
	     && multiply_term_by (term, TREE_OPERAND (stride, 1)))
	stride = strip_casts (TREE_OPERAND (stride, 0));

      gassign *assign;
      while ((assign = maybe_get_assign (stride))
	     && gimple_assign_rhs_code (assign) == MULT_EXPR
	     && multiply_term_by (term, gimple_assign_rhs2 (assign)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, address.stmt,
			     "looking through %G", (gimple *) assign);
	  stride = strip_casts (gimple_assign_rhs1 (assign));
	}

      analyze_stride (address, term, stride, get_chrec_loop (chrec));
      return true;
    }

  return false;
}

/* Address term TERM is an arbitrary term that provides no versioning
   opportunities.  Analyze it to see whether it contains any likely
   inner strides, so that we don't mistakenly version for other
   (less likely) candidates.

   This copes with invariant innermost indices such as:

     x(i, :) = 100

   where the "i" component of the address is invariant in the loop
   but provides the real inner stride.

   ADDRESS is the address that contains TERM.  */

void
loop_versioning::analyze_arbitrary_term (address_info &address,
					 address_term_info &term)

{
  /* A multiplication offers two potential strides.  Pick the one that
     is most likely to be an innermost stride.  */
  tree expr = term.expr, alt = NULL_TREE;
  gassign *mult = maybe_get_assign (expr);
  if (mult && gimple_assign_rhs_code (mult) == MULT_EXPR)
    {
      expr = strip_casts (gimple_assign_rhs1 (mult));
      alt = strip_casts (gimple_assign_rhs2 (mult));
    }
  term.stride = expr;
  term.inner_likelihood = get_inner_likelihood (expr, term.multiplier);
  if (alt)
    {
      inner_likelihood alt_l = get_inner_likelihood (alt, term.multiplier);
      if (alt_l > term.inner_likelihood)
	{
	  term.stride = alt;
	  term.inner_likelihood = alt_l;
	}
    }
  if (dump_enabled_p ())
    dump_inner_likelihood (address, term);
}

/* Try to identify loop strides in ADDRESS and try to choose realistic
   versioning opportunities based on these strides.

   The main difficulty here isn't finding strides that could be used
   in a version check (that's pretty easy).  The problem instead is to
   avoid versioning for some stride S that is unlikely ever to be 1 at
   runtime.  Versioning for S == 1 on its own would lead to unnecessary
   code bloat, while adding S == 1 to more realistic version conditions
   would lose the optimisation opportunity offered by those other conditions.

   For example, versioning for a stride of 1 in the Fortran code:

     integer :: a(:,:)
     a(1,:) = 1

   is not usually a good idea, since the assignment is iterating over
   an outer dimension and is relatively unlikely to have a stride of 1.
   (It isn't impossible, since the inner dimension might be 1, or the
   array might be transposed.)  Similarly, in:

     integer :: a(:,:), b(:,:)
     b(:,1) = a(1,:)

   b(:,1) is relatively likely to have a stride of 1 while a(1,:) isn't.
   Versioning for when both strides are 1 would lose most of the benefit
   of versioning for b's access.

   The approach we take is as follows:

   - Analyze each term to see whether it has an identifiable stride,
     regardless of which loop applies the stride.

   - Evaluate the likelihood that each such stride is for the innermost
     dimension of an array, on the scale "likely", "don't know" or "unlikely".

   - If there is a single "likely" innermost stride, and that stride is
     applied in the loop that contains STMT, version the loop for when the
     stride is 1.  This deals with the cases in which we're fairly
     confident of doing the right thing, such as the b(:,1) reference above.

   - If there are no "likely" innermost strides, and the loop that contains
     STMT uses a stride that we rated as "don't know", version for when
     that stride is 1.  This is principally used for C code such as:

       for (int i = 0; i < n; ++i)
	 a[i * x] = ...;

     and:

       for (int j = 0; j < n; ++j)
	 for (int i = 0; i < n; ++i)
	   a[i * x + j * y] = ...;

     where nothing in the way "x" and "y" are set gives a hint as to
     whether "i" iterates over the innermost dimension of the array.
     In these situations it seems reasonable to assume the
     programmer has nested the loops appropriately (although of course
     there are examples like GEMM in which this assumption doesn't hold
     for all accesses in the loop).

     This case is also useful for the Fortran equivalent of the
     above C code.  */

void
loop_versioning::analyze_address_fragment (address_info &address)
{
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, address.stmt, "analyzing address fragment ");
      dump_address_info (MSG_NOTE, address);
      dump_printf (MSG_NOTE, "\n");
    }

  /* Analyze each component of the sum to see whether it involves an
     apparent stride.

     There is an overlap between the addresses that
     find_per_loop_multiplication and analyze_term_using_scevs can handle,
     but the former is much cheaper than SCEV analysis, so try it first.  */
  for (unsigned int i = 0; i < address.terms.length (); ++i)
    if (!find_per_loop_multiplication (address, address.terms[i])
	&& !analyze_term_using_scevs (address, address.terms[i])
	&& !POINTER_TYPE_P (TREE_TYPE (address.terms[i].expr)))
      analyze_arbitrary_term (address, address.terms[i]);

  /* Check for strides that are likely to be for the innermost dimension.

     1. If there is a single likely inner stride, if it is an SSA name,
	and if it is worth versioning the loop for when the SSA name
	equals 1, record that we want to do so.

     2. Otherwise, if there any likely inner strides, bail out.  This means
	one of:

	(a) There are multiple likely inner strides.  This suggests we're
	    confused and be can't be confident of doing the right thing.

	(b) There is a single likely inner stride and it is a constant
	    rather than an SSA name.  This can mean either that the access
	    is a natural one without any variable strides, such as:

	      for (int i = 0; i < n; ++i)
		a[i] += 1;

	    or that a variable stride is applied to an outer dimension,
	    such as:

	      for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
		  a[j * stride][i] += 1;

	(c) There is a single likely inner stride, and it is an SSA name,
	    but it isn't a worthwhile versioning opportunity.  This usually
	    means that the variable stride is applied by an outer loop,
	    such as:

	      for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
		  a[j][i * stride] += 1;

	    or (using an example with a more natural loop nesting):

	      for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
		  a[i][j] += b[i * stride];

	    in cases where b[i * stride] cannot (yet) be hoisted for
	    aliasing reasons.

     3. If there are no likely inner strides, fall through to the next
	set of checks.

     Pointer equality is enough to check for uniqueness in (1), since we
     only care about SSA names.  */
  tree chosen_stride = NULL_TREE;
  tree version_stride = NULL_TREE;
  for (unsigned int i = 0; i < address.terms.length (); ++i)
    if (chosen_stride != address.terms[i].stride
	&& address.terms[i].inner_likelihood == INNER_LIKELY)
      {
	if (chosen_stride)
	  return;
	chosen_stride = address.terms[i].stride;
	if (address.terms[i].versioning_opportunity_p)
	  version_stride = chosen_stride;
      }

  /* If there are no likely inner strides, see if there is a single
     versioning opportunity for a stride that was rated as INNER_DONT_KNOW.
     See the comment above the function for the cases that this code
     handles.  */
  if (!chosen_stride)
    for (unsigned int i = 0; i < address.terms.length (); ++i)
      if (version_stride != address.terms[i].stride
	  && address.terms[i].inner_likelihood == INNER_DONT_KNOW
	  && address.terms[i].versioning_opportunity_p)
	{
	  if (version_stride)
	    return;
	  version_stride = address.terms[i].stride;
	}

  if (version_stride)
    version_for_unity (address.stmt, version_stride);
}

/* Treat EXPR * MULTIPLIER + OFFSET as a fragment of an address that addresses
   TYPE_SIZE bytes and record this address fragment for later processing.
   STMT is the statement that contains the address.  */

void
loop_versioning::record_address_fragment (gimple *stmt,
					  unsigned HOST_WIDE_INT type_size,
					  tree expr,
					  unsigned HOST_WIDE_INT multiplier,
					  HOST_WIDE_INT offset)
{
  /* We're only interested in computed values.  */
  if (TREE_CODE (expr) != SSA_NAME)
    return;

  /* Quick exit if no part of the address is calculated in STMT's loop,
     since such addresses have no versioning opportunities.  */
  class loop *loop = loop_containing_stmt (stmt);
  if (expr_invariant_in_loop_p (loop, expr))
    return;

  /* Set up an address_info for EXPR * MULTIPLIER.  */
  address_info *address = XOBNEW (&m_obstack, address_info);
  new (address) address_info;
  address->stmt = stmt;
  address->loop = loop;
  address->base = NULL_TREE;
  address->terms.quick_grow (1);
  address->terms[0].expr = expr;
  address->terms[0].multiplier = multiplier;
  address->terms[0].stride = NULL_TREE;
  address->terms[0].inner_likelihood = INNER_UNLIKELY;
  address->terms[0].versioning_opportunity_p = false;
  address->min_offset = offset;

  /* Peel apart the expression into a sum of address_terms, where each
     term is multiplied by a constant.  Treat a + b and a - b the same,
     since it doesn't matter for our purposes whether an address is
     increasing or decreasing.  Distribute (a + b) * constant into
     a * constant + b * constant.

     We don't care which loop each term belongs to, since we want to
     examine as many candidate strides as possible when determining
     which is likely to be for the innermost dimension.  We therefore
     don't limit the search to statements in STMT's loop.  */
  for (unsigned int i = 0; i < address->terms.length (); )
    {
      if (gassign *assign = maybe_get_assign (address->terms[i].expr))
	{
	  tree_code code = gimple_assign_rhs_code (assign);
	  if (code == PLUS_EXPR
	      || code == POINTER_PLUS_EXPR
	      || code == MINUS_EXPR)
	    {
	      tree op1 = gimple_assign_rhs1 (assign);
	      tree op2 = gimple_assign_rhs2 (assign);
	      if (TREE_CODE (op2) == INTEGER_CST)
		{
		  address->terms[i].expr = strip_casts (op1);
		  /* This is heuristic only, so don't worry about truncation
		     or overflow.  */
		  address->min_offset += (TREE_INT_CST_LOW (op2)
					  * address->terms[i].multiplier);
		  continue;
		}
	      else if (address->terms.length () < address_info::MAX_TERMS)
		{
		  unsigned int j = address->terms.length ();
		  address->terms.quick_push (address->terms[i]);
		  address->terms[i].expr = strip_casts (op1);
		  address->terms[j].expr = strip_casts (op2);
		  continue;
		}
	    }
	  if (code == MULT_EXPR)
	    {
	      tree op1 = gimple_assign_rhs1 (assign);
	      tree op2 = gimple_assign_rhs2 (assign);
	      if (multiply_term_by (address->terms[i], op2))
		{
		  address->terms[i].expr = strip_casts (op1);
		  continue;
		}
	    }
	  if (CONVERT_EXPR_CODE_P (code))
	    {
	      tree op1 = gimple_assign_rhs1 (assign);
	      address->terms[i].expr = strip_casts (op1);
	      continue;
	    }
	}
      i += 1;
    }

  /* Peel off any symbolic pointer.  */
  if (TREE_CODE (address->terms[0].expr) != SSA_NAME
      && address->terms[0].multiplier == 1)
    {
      if (address->terms.length () == 1)
	{
	  obstack_free (&m_obstack, address);
	  return;
	}
      address->base = address->terms[0].expr;
      address->terms.ordered_remove (0);
    }

  /* Require all remaining terms to be SSA names.  (This could be false
     for unfolded statements, but they aren't worth dealing with.)  */
  for (unsigned int i = 0; i < address->terms.length (); ++i)
    if (TREE_CODE (address->terms[i].expr) != SSA_NAME)
      {
	obstack_free (&m_obstack, address);
	return;
      }

  /* The loop above set MIN_OFFSET based on the first byte of the
     referenced data.  Calculate the end + 1.  */
  address->max_offset = address->min_offset + type_size;

  /* Put the terms into a canonical order for the hash table lookup below.  */
  address->terms.qsort (compare_address_terms);

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, stmt, "recording address fragment %T", expr);
      if (multiplier != 1)
	dump_printf (MSG_NOTE, " * %wd", multiplier);
      dump_printf (MSG_NOTE, " = ");
      dump_address_info (MSG_NOTE, *address);
      dump_printf (MSG_NOTE, "\n");
    }

  /* Pool address information with the same terms (but potentially
     different offsets).  */
  address_info **slot = m_address_table.find_slot (address, INSERT);
  if (address_info *old_address = *slot)
    {
      /* We've already seen an address with the same terms.  Extend the
	 offset range to account for this access.  Doing this can paper
	 over gaps, such as in:

	   a[i * stride * 4] + a[i * stride * 4 + 3];

	 where nothing references "+ 1" or "+ 2".  However, the vectorizer
	 handles such gapped accesses without problems, so it's not worth
	 trying to exclude them.  */
      if (old_address->min_offset > address->min_offset)
	old_address->min_offset = address->min_offset;
      if (old_address->max_offset < address->max_offset)
	old_address->max_offset = address->max_offset;
      obstack_free (&m_obstack, address);
    }
  else
    {
      /* This is the first time we've seen an address with these terms.  */
      *slot = address;
      m_address_list.safe_push (address);
    }
}

/* Analyze expression EXPR, which occurs in STMT.  */

void
loop_versioning::analyze_expr (gimple *stmt, tree expr)
{
  unsigned HOST_WIDE_INT type_size;

  while (handled_component_p (expr))
    {
      /* See whether we can use versioning to avoid a multiplication
	 in an array index.  */
      if (TREE_CODE (expr) == ARRAY_REF
	  && acceptable_type_p (TREE_TYPE (expr), &type_size))
	record_address_fragment (stmt, type_size,
				 TREE_OPERAND (expr, 1), type_size, 0);
      expr = TREE_OPERAND (expr, 0);
    }

  /* See whether we can use versioning to avoid a multiplication
     in the pointer calculation of a MEM_REF.  */
  if (TREE_CODE (expr) == MEM_REF
      && acceptable_type_p (TREE_TYPE (expr), &type_size))
    record_address_fragment (stmt, type_size, TREE_OPERAND (expr, 0), 1,
			     /* This is heuristic only, so don't worry
				about truncation or overflow.  */
			     TREE_INT_CST_LOW (TREE_OPERAND (expr, 1)));

  /* These would be easy to handle if they existed at this stage.  */
  gcc_checking_assert (TREE_CODE (expr) != TARGET_MEM_REF);
}

/* Analyze all the statements in BB looking for useful version checks.
   Return true on success, false if something prevents the block from
   being versioned.  */

bool
loop_versioning::analyze_block (basic_block bb)
{
  class loop *loop = bb->loop_father;
  loop_info &li = get_loop_info (loop);
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (is_gimple_debug (stmt))
	continue;

      if (expensive_stmt_p (stmt))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, stmt, "expensive statement"
			     " prevents versioning: %G", stmt);
	  return false;
	}

      /* Only look for direct versioning opportunities in inner loops
	 since the benefit tends to be much smaller for outer loops.  */
      if (!loop->inner)
	{
	  unsigned int nops = gimple_num_ops (stmt);
	  for (unsigned int i = 0; i < nops; ++i)
	    if (tree op = gimple_op (stmt, i))
	      analyze_expr (stmt, op);
	}

      /* The point of the instruction limit is to prevent excessive
	 code growth, so this is a size-based estimate even though
	 the optimization is aimed at speed.  */
      li.num_insns += estimate_num_insns (stmt, &eni_size_weights);
    }

  return true;
}

/* Analyze all the blocks in the function, looking for useful version checks.
   Return true if we found one.  */

bool
loop_versioning::analyze_blocks ()
{
  AUTO_DUMP_SCOPE ("analyze_blocks",
		   dump_user_location_t::from_function_decl (m_fn->decl));

  /* For now we don't try to version the whole function, although
     versioning at that level could be useful in some cases.  */
  get_loop_info (get_loop (m_fn, 0)).rejected_p = true;

  for (auto loop : loops_list (cfun, LI_FROM_INNERMOST))
    {
      loop_info &linfo = get_loop_info (loop);

      /* Ignore cold loops.  */
      if (!optimize_loop_for_speed_p (loop))
	linfo.rejected_p = true;

      /* See whether an inner loop prevents versioning of this loop.  */
      if (!linfo.rejected_p)
	for (class loop *inner = loop->inner; inner; inner = inner->next)
	  if (get_loop_info (inner).rejected_p)
	    {
	      linfo.rejected_p = true;
	      break;
	    }

      /* If versioning the loop is still a possibility, examine the
	 statements in the loop to look for versioning opportunities.  */
      if (!linfo.rejected_p)
	{
	  void *start_point = obstack_alloc (&m_obstack, 0);

	  for (basic_block bb = linfo.block_list; bb;
	       bb = m_next_block_in_loop[bb->index])
	    if (!analyze_block (bb))
	      {
		linfo.rejected_p = true;
		break;
	      }

	  if (!linfo.rejected_p)
	    {
	      /* Process any queued address fragments, now that we have
		 complete grouping information.  */
	      address_info *address;
	      unsigned int i;
	      FOR_EACH_VEC_ELT (m_address_list, i, address)
		analyze_address_fragment (*address);
	    }

	  m_address_table.empty ();
	  m_address_list.truncate (0);
	  obstack_free (&m_obstack, start_point);
	}
    }

  return m_num_conditions != 0;
}

/* Use the ranges in VRS to remove impossible versioning conditions from
   LOOP.  */

void
loop_versioning::prune_loop_conditions (class loop *loop)
{
  loop_info &li = get_loop_info (loop);

  int to_remove = -1;
  bitmap_iterator bi;
  unsigned int i;
  int_range_max r;
  EXECUTE_IF_SET_IN_BITMAP (&li.unity_names, 0, i, bi)
    {
      tree name = ssa_name (i);
      gimple *stmt = first_stmt (loop->header);

      if (get_range_query (cfun)->range_of_expr (r, name, stmt)
	  && !r.contains_p (wi::one (TYPE_PRECISION (TREE_TYPE (name)))))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, find_loop_location (loop),
			     "%T can never be 1 in this loop\n", name);

	  if (to_remove >= 0)
	    bitmap_clear_bit (&li.unity_names, to_remove);
	  to_remove = i;
	  m_num_conditions -= 1;
	}
    }
  if (to_remove >= 0)
    bitmap_clear_bit (&li.unity_names, to_remove);
}

/* Remove any scheduled loop version conditions that will never be true.
   Return true if any remain.  */

bool
loop_versioning::prune_conditions ()
{
  AUTO_DUMP_SCOPE ("prune_loop_conditions",
		   dump_user_location_t::from_function_decl (m_fn->decl));

  calculate_dominance_info (CDI_DOMINATORS);
  lv_dom_walker dom_walker (*this);
  dom_walker.walk (ENTRY_BLOCK_PTR_FOR_FN (m_fn));
  return m_num_conditions != 0;
}

/* Merge the version checks for INNER into immediately-enclosing loop
   OUTER.  */

void
loop_versioning::merge_loop_info (class loop *outer, class loop *inner)
{
  loop_info &inner_li = get_loop_info (inner);
  loop_info &outer_li = get_loop_info (outer);

  if (dump_enabled_p ())
    {
      bitmap_iterator bi;
      unsigned int i;
      EXECUTE_IF_SET_IN_BITMAP (&inner_li.unity_names, 0, i, bi)
	if (!bitmap_bit_p (&outer_li.unity_names, i))
	  dump_printf_loc (MSG_NOTE, find_loop_location (inner),
			   "hoisting check that %T == 1 to outer loop\n",
			   ssa_name (i));
    }

  bitmap_ior_into (&outer_li.unity_names, &inner_li.unity_names);
  if (loop_depth (outer_li.outermost) < loop_depth (inner_li.outermost))
    outer_li.outermost = inner_li.outermost;
}

/* Add LOOP to the queue of loops to version.  */

void
loop_versioning::add_loop_to_queue (class loop *loop)
{
  loop_info &li = get_loop_info (loop);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, find_loop_location (loop),
		     "queuing this loop for versioning\n");
  m_loops_to_version.safe_push (loop);

  /* Don't try to version superloops.  */
  li.rejected_p = true;
}

/* Decide whether the cost model would allow us to version LOOP,
   either directly or as part of a parent loop, and return true if so.
   This does not imply that the loop is actually worth versioning in its
   own right, just that it would be valid to version it if something
   benefited.

   We have already made this decision for all inner loops of LOOP.  */

bool
loop_versioning::decide_whether_loop_is_versionable (class loop *loop)
{
  loop_info &li = get_loop_info (loop);

  if (li.rejected_p)
    return false;

  /* Examine the decisions made for inner loops.  */
  for (class loop *inner = loop->inner; inner; inner = inner->next)
    {
      loop_info &inner_li = get_loop_info (inner);
      if (inner_li.rejected_p)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, find_loop_location (loop),
			     "not versioning this loop because one of its"
			     " inner loops should not be versioned\n");
	  return false;
	}

      if (inner_li.worth_versioning_p ())
	li.subloops_benefit_p = true;

      /* Accumulate the number of instructions from subloops that are not
	 the innermost, or that don't benefit from versioning.  Only the
	 instructions from innermost loops that benefit from versioning
	 should be weighed against loop-versioning-max-inner-insns;
	 everything else should be weighed against
	 loop-versioning-max-outer-insns.  */
      if (!inner_li.worth_versioning_p () || inner->inner)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, find_loop_location (loop),
			     "counting %d instructions from this loop"
			     " against its parent loop\n", inner_li.num_insns);
	  li.num_insns += inner_li.num_insns;
	}
    }

  /* Enforce the size limits.  */
  if (li.worth_versioning_p ())
    {
      unsigned int max_num_insns = max_insns_for_loop (loop);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, find_loop_location (loop),
			 "this loop has %d instructions, against"
			 " a versioning limit of %d\n",
			 li.num_insns, max_num_insns);
      if (li.num_insns > max_num_insns)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION
			     | MSG_PRIORITY_USER_FACING,
			     find_loop_location (loop),
			     "this loop is too big to version");
	  return false;
	}
    }

  /* Hoist all version checks from subloops to this loop.  */
  for (class loop *subloop = loop->inner; subloop; subloop = subloop->next)
    merge_loop_info (loop, subloop);

  return true;
}

/* Decide which loops to version and add them to the versioning queue.
   Return true if there are any loops to version.  */

bool
loop_versioning::make_versioning_decisions ()
{
  AUTO_DUMP_SCOPE ("make_versioning_decisions",
		   dump_user_location_t::from_function_decl (m_fn->decl));

  for (auto loop : loops_list (cfun, LI_FROM_INNERMOST))
    {
      loop_info &linfo = get_loop_info (loop);
      if (decide_whether_loop_is_versionable (loop))
	{
	  /* Commit to versioning LOOP directly if we can't hoist the
	     version checks any further.  */
	  if (linfo.worth_versioning_p ()
	      && (loop_depth (loop) == 1 || linfo.outermost == loop))
	    add_loop_to_queue (loop);
	}
      else
	{
	  /* We can't version this loop, so individually version any
	     subloops that would benefit and haven't been versioned yet.  */
	  linfo.rejected_p = true;
	  for (class loop *subloop = loop->inner; subloop;
	       subloop = subloop->next)
	    if (get_loop_info (subloop).worth_versioning_p ())
	      add_loop_to_queue (subloop);
	}
    }

  return !m_loops_to_version.is_empty ();
}

/* Attempt to implement loop versioning for LOOP, using the information
   cached in the associated loop_info.  Return true on success.  */

bool
loop_versioning::version_loop (class loop *loop)
{
  loop_info &li = get_loop_info (loop);

  /* Build up a condition that selects the original loop instead of
     the simplified loop.  */
  tree cond = boolean_false_node;
  bitmap_iterator bi;
  unsigned int i;
  EXECUTE_IF_SET_IN_BITMAP (&li.unity_names, 0, i, bi)
    {
      tree name = ssa_name (i);
      tree ne_one = fold_build2 (NE_EXPR, boolean_type_node, name,
				 build_one_cst (TREE_TYPE (name)));
      cond = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, cond, ne_one);
    }

  /* Convert the condition into a suitable gcond.  */
  gimple_seq stmts = NULL;
  cond = force_gimple_operand_1 (cond, &stmts, is_gimple_condexpr_for_cond,
				 NULL_TREE);

  /* Version the loop.  */
  initialize_original_copy_tables ();
  basic_block cond_bb;
  li.optimized_loop = loop_version (loop, cond, &cond_bb,
				    profile_probability::unlikely (),
				    profile_probability::likely (),
				    profile_probability::unlikely (),
				    profile_probability::likely (), true);
  free_original_copy_tables ();
  if (!li.optimized_loop)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, find_loop_location (loop),
			 "tried but failed to version this loop for when"
			 " certain strides are 1\n");
      return false;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, find_loop_location (loop),
		     "versioned this loop for when certain strides are 1\n");

  /* Insert the statements that feed COND.  */
  if (stmts)
    {
      gimple_stmt_iterator gsi = gsi_last_bb (cond_bb);
      gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
    }

  return true;
}

/* Attempt to version all loops in the versioning queue.  */

void
loop_versioning::implement_versioning_decisions ()
{
  /* No AUTO_DUMP_SCOPE here since all messages are top-level and
     user-facing at this point.  */

  bool any_succeeded_p = false;
  class loop *loop;
  unsigned int i;
  FOR_EACH_VEC_ELT (m_loops_to_version, i, loop)
    if (version_loop (loop))
      any_succeeded_p = true;
  if (!any_succeeded_p)
    return;

  update_ssa (TODO_update_ssa);

  /* Simplify the new loop, which is used when COND is false.  */
  FOR_EACH_VEC_ELT (m_loops_to_version, i, loop)
    {
      loop_info &linfo = get_loop_info (loop);
      if (linfo.optimized_loop)
	name_prop (linfo).substitute_and_fold (linfo.optimized_loop->header);
    }
}

/* Run the pass and return a set of TODO_* flags.  */

unsigned int
loop_versioning::run ()
{
  gcc_assert (scev_initialized_p ());

  if (analyze_blocks ()
      && prune_conditions ()
      && make_versioning_decisions ())
    implement_versioning_decisions ();

  return 0;
}

/* Loop versioning pass.  */

const pass_data pass_data_loop_versioning =
{
  GIMPLE_PASS, /* type */
  "lversion", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP_VERSIONING, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop_versioning : public gimple_opt_pass
{
public:
  pass_loop_versioning (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_loop_versioning, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
  {
    return flag_version_loops_for_strides;
  }
  unsigned int execute (function *) final override;
};

unsigned int
pass_loop_versioning::execute (function *fn)
{
  if (number_of_loops (fn) <= 1)
    return 0;

  enable_ranger (fn);
  unsigned int ret = loop_versioning (fn).run ();
  disable_ranger (fn);
  return ret;
}

} // anon namespace

gimple_opt_pass *
make_pass_loop_versioning (gcc::context *ctxt)
{
  return new pass_loop_versioning (ctxt);
}
