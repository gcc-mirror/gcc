/* Straight-line strength reduction.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by Bill Schmidt, IBM <wschmidt@linux.ibm.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* There are many algorithms for performing strength reduction on
   loops.  This is not one of them.  IVOPTS handles strength reduction
   of induction variables just fine.  This pass is intended to pick
   up the crumbs it leaves behind, by considering opportunities for
   strength reduction along dominator paths.

   Strength reduction addresses explicit multiplies, and certain
   multiplies implicit in addressing expressions.  It would also be
   possible to apply strength reduction to divisions and modulos,
   but such opportunities are relatively uncommon.

   Strength reduction is also currently restricted to integer operations.
   If desired, it could be extended to floating-point operations under
   control of something like -funsafe-math-optimizations.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "basic-block.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "gimple-pretty-print.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssanames.h"
#include "domwalk.h"
#include "pointer-set.h"
#include "expmed.h"
#include "params.h"
#include "hash-table.h"
#include "tree-ssa-address.h"

/* Information about a strength reduction candidate.  Each statement
   in the candidate table represents an expression of one of the
   following forms (the special case of CAND_REF will be described
   later):

   (CAND_MULT)  S1:  X = (B + i) * S
   (CAND_ADD)   S1:  X = B + (i * S)

   Here X and B are SSA names, i is an integer constant, and S is
   either an SSA name or a constant.  We call B the "base," i the
   "index", and S the "stride."

   Any statement S0 that dominates S1 and is of the form:

   (CAND_MULT)  S0:  Y = (B + i') * S
   (CAND_ADD)   S0:  Y = B + (i' * S)

   is called a "basis" for S1.  In both cases, S1 may be replaced by
   
                S1':  X = Y + (i - i') * S,

   where (i - i') * S is folded to the extent possible.

   All gimple statements are visited in dominator order, and each
   statement that may contribute to one of the forms of S1 above is
   given at least one entry in the candidate table.  Such statements
   include addition, pointer addition, subtraction, multiplication,
   negation, copies, and nontrivial type casts.  If a statement may
   represent more than one expression of the forms of S1 above, 
   multiple "interpretations" are stored in the table and chained
   together.  Examples:

   * An add of two SSA names may treat either operand as the base.
   * A multiply of two SSA names, likewise.
   * A copy or cast may be thought of as either a CAND_MULT with
     i = 0 and S = 1, or as a CAND_ADD with i = 0 or S = 0.

   Candidate records are allocated from an obstack.  They are addressed
   both from a hash table keyed on S1, and from a vector of candidate
   pointers arranged in predominator order.

   Opportunity note
   ----------------
   Currently we don't recognize:

     S0: Y = (S * i') - B
     S1: X = (S * i) - B

   as a strength reduction opportunity, even though this S1 would
   also be replaceable by the S1' above.  This can be added if it
   comes up in practice.

   Strength reduction in addressing
   --------------------------------
   There is another kind of candidate known as CAND_REF.  A CAND_REF
   describes a statement containing a memory reference having 
   complex addressing that might benefit from strength reduction.
   Specifically, we are interested in references for which 
   get_inner_reference returns a base address, offset, and bitpos as
   follows:

     base:    MEM_REF (T1, C1)
     offset:  MULT_EXPR (PLUS_EXPR (T2, C2), C3)
     bitpos:  C4 * BITS_PER_UNIT

   Here T1 and T2 are arbitrary trees, and C1, C2, C3, C4 are 
   arbitrary integer constants.  Note that C2 may be zero, in which
   case the offset will be MULT_EXPR (T2, C3).

   When this pattern is recognized, the original memory reference
   can be replaced with:

     MEM_REF (POINTER_PLUS_EXPR (T1, MULT_EXPR (T2, C3)),
              C1 + (C2 * C3) + C4)

   which distributes the multiply to allow constant folding.  When
   two or more addressing expressions can be represented by MEM_REFs
   of this form, differing only in the constants C1, C2, and C4,
   making this substitution produces more efficient addressing during
   the RTL phases.  When there are not at least two expressions with
   the same values of T1, T2, and C3, there is nothing to be gained
   by the replacement.

   Strength reduction of CAND_REFs uses the same infrastructure as
   that used by CAND_MULTs and CAND_ADDs.  We record T1 in the base (B)
   field, MULT_EXPR (T2, C3) in the stride (S) field, and 
   C1 + (C2 * C3) + C4 in the index (i) field.  A basis for a CAND_REF
   is thus another CAND_REF with the same B and S values.  When at 
   least two CAND_REFs are chained together using the basis relation,
   each of them is replaced as above, resulting in improved code
   generation for addressing.

   Conditional candidates
   ======================

   Conditional candidates are best illustrated with an example.
   Consider the code sequence:

   (1)  x_0 = ...;
   (2)  a_0 = x_0 * 5;          MULT (B: x_0; i: 0; S: 5)
        if (...)
   (3)    x_1 = x_0 + 1;        ADD  (B: x_0, i: 1; S: 1)
   (4)  x_2 = PHI <x_0, x_1>;   PHI  (B: x_0, i: 0, S: 1)
   (5)  x_3 = x_2 + 1;          ADD  (B: x_2, i: 1, S: 1)
   (6)  a_1 = x_3 * 5;          MULT (B: x_2, i: 1; S: 5)

   Here strength reduction is complicated by the uncertain value of x_2.
   A legitimate transformation is:

   (1)  x_0 = ...;
   (2)  a_0 = x_0 * 5;
        if (...)
	  {
   (3)      [x_1 = x_0 + 1;]
   (3a)     t_1 = a_0 + 5;
          }
   (4)  [x_2 = PHI <x_0, x_1>;]
   (4a) t_2 = PHI <a_0, t_1>;
   (5)  [x_3 = x_2 + 1;]
   (6r) a_1 = t_2 + 5;

   where the bracketed instructions may go dead.

   To recognize this opportunity, we have to observe that statement (6)
   has a "hidden basis" (2).  The hidden basis is unlike a normal basis
   in that the statement and the hidden basis have different base SSA
   names (x_2 and x_0, respectively).  The relationship is established
   when a statement's base name (x_2) is defined by a phi statement (4),
   each argument of which (x_0, x_1) has an identical "derived base name."
   If the argument is defined by a candidate (as x_1 is by (3)) that is a
   CAND_ADD having a stride of 1, the derived base name of the argument is
   the base name of the candidate (x_0).  Otherwise, the argument itself
   is its derived base name (as is the case with argument x_0).

   The hidden basis for statement (6) is the nearest dominating candidate
   whose base name is the derived base name (x_0) of the feeding phi (4), 
   and whose stride is identical to that of the statement.  We can then
   create the new "phi basis" (4a) and feeding adds along incoming arcs (3a),
   allowing the final replacement of (6) by the strength-reduced (6r).

   To facilitate this, a new kind of candidate (CAND_PHI) is introduced.
   A CAND_PHI is not a candidate for replacement, but is maintained in the
   candidate table to ease discovery of hidden bases.  Any phi statement
   whose arguments share a common derived base name is entered into the
   table with the derived base name, an (arbitrary) index of zero, and a
   stride of 1.  A statement with a hidden basis can then be detected by
   simply looking up its feeding phi definition in the candidate table,
   extracting the derived base name, and searching for a basis in the
   usual manner after substituting the derived base name.

   Note that the transformation is only valid when the original phi and 
   the statements that define the phi's arguments are all at the same
   position in the loop hierarchy.  */


/* Index into the candidate vector, offset by 1.  VECs are zero-based,
   while cand_idx's are one-based, with zero indicating null.  */
typedef unsigned cand_idx;

/* The kind of candidate.  */
enum cand_kind
{
  CAND_MULT,
  CAND_ADD,
  CAND_REF,
  CAND_PHI
};

struct slsr_cand_d
{
  /* The candidate statement S1.  */
  gimple cand_stmt;

  /* The base expression B:  often an SSA name, but not always.  */
  tree base_expr;

  /* The stride S.  */
  tree stride;

  /* The index constant i.  */
  double_int index;

  /* The type of the candidate.  This is normally the type of base_expr,
     but casts may have occurred when combining feeding instructions.
     A candidate can only be a basis for candidates of the same final type.
     (For CAND_REFs, this is the type to be used for operand 1 of the
     replacement MEM_REF.)  */
  tree cand_type;

  /* The kind of candidate (CAND_MULT, etc.).  */
  enum cand_kind kind;

  /* Index of this candidate in the candidate vector.  */
  cand_idx cand_num;

  /* Index of the next candidate record for the same statement.
     A statement may be useful in more than one way (e.g., due to
     commutativity).  So we can have multiple "interpretations"
     of a statement.  */
  cand_idx next_interp;

  /* Index of the basis statement S0, if any, in the candidate vector.  */
  cand_idx basis;

  /* First candidate for which this candidate is a basis, if one exists.  */
  cand_idx dependent;

  /* Next candidate having the same basis as this one.  */
  cand_idx sibling;

  /* If this is a conditional candidate, the CAND_PHI candidate
     that defines the base SSA name B.  */
  cand_idx def_phi;

  /* Savings that can be expected from eliminating dead code if this
     candidate is replaced.  */
  int dead_savings;
};

typedef struct slsr_cand_d slsr_cand, *slsr_cand_t;
typedef const struct slsr_cand_d *const_slsr_cand_t;

/* Pointers to candidates are chained together as part of a mapping
   from base expressions to the candidates that use them.  */

struct cand_chain_d
{
  /* Base expression for the chain of candidates:  often, but not
     always, an SSA name.  */
  tree base_expr;

  /* Pointer to a candidate.  */
  slsr_cand_t cand;

  /* Chain pointer.  */
  struct cand_chain_d *next;

};

typedef struct cand_chain_d cand_chain, *cand_chain_t;
typedef const struct cand_chain_d *const_cand_chain_t;

/* Information about a unique "increment" associated with candidates
   having an SSA name for a stride.  An increment is the difference
   between the index of the candidate and the index of its basis,
   i.e., (i - i') as discussed in the module commentary.

   When we are not going to generate address arithmetic we treat
   increments that differ only in sign as the same, allowing sharing
   of the cost of initializers.  The absolute value of the increment
   is stored in the incr_info.  */

struct incr_info_d
{
  /* The increment that relates a candidate to its basis.  */
  double_int incr;

  /* How many times the increment occurs in the candidate tree.  */
  unsigned count;

  /* Cost of replacing candidates using this increment.  Negative and
     zero costs indicate replacement should be performed.  */
  int cost;

  /* If this increment is profitable but is not -1, 0, or 1, it requires
     an initializer T_0 = stride * incr to be found or introduced in the
     nearest common dominator of all candidates.  This field holds T_0
     for subsequent use.  */
  tree initializer;

  /* If the initializer was found to already exist, this is the block
     where it was found.  */
  basic_block init_bb;
};

typedef struct incr_info_d incr_info, *incr_info_t;

/* Candidates are maintained in a vector.  If candidate X dominates
   candidate Y, then X appears before Y in the vector; but the
   converse does not necessarily hold.  */
static vec<slsr_cand_t> cand_vec;

enum cost_consts
{
  COST_NEUTRAL = 0,
  COST_INFINITE = 1000
};

enum stride_status
{
  UNKNOWN_STRIDE = 0,
  KNOWN_STRIDE = 1
};

enum phi_adjust_status
{
  NOT_PHI_ADJUST = 0,
  PHI_ADJUST = 1
};

enum count_phis_status
{
  DONT_COUNT_PHIS = 0,
  COUNT_PHIS = 1
};
 
/* Pointer map embodying a mapping from statements to candidates.  */
static struct pointer_map_t *stmt_cand_map;

/* Obstack for candidates.  */
static struct obstack cand_obstack;

/* Obstack for candidate chains.  */
static struct obstack chain_obstack;

/* An array INCR_VEC of incr_infos is used during analysis of related
   candidates having an SSA name for a stride.  INCR_VEC_LEN describes
   its current length.  MAX_INCR_VEC_LEN is used to avoid costly
   pathological cases. */
static incr_info_t incr_vec;
static unsigned incr_vec_len;
const int MAX_INCR_VEC_LEN = 16;

/* For a chain of candidates with unknown stride, indicates whether or not
   we must generate pointer arithmetic when replacing statements.  */
static bool address_arithmetic_p;

/* Forward function declarations.  */
static slsr_cand_t base_cand_from_table (tree);
static tree introduce_cast_before_cand (slsr_cand_t, tree, tree);
static bool legal_cast_p_1 (tree, tree);

/* Produce a pointer to the IDX'th candidate in the candidate vector.  */

static slsr_cand_t
lookup_cand (cand_idx idx)
{
  return cand_vec[idx - 1];
}

/* Helper for hashing a candidate chain header.  */

struct cand_chain_hasher : typed_noop_remove <cand_chain>
{
  typedef cand_chain value_type;
  typedef cand_chain compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

inline hashval_t
cand_chain_hasher::hash (const value_type *p)
{
  tree base_expr = p->base_expr;
  return iterative_hash_expr (base_expr, 0);
}

inline bool
cand_chain_hasher::equal (const value_type *chain1, const compare_type *chain2)
{
  return operand_equal_p (chain1->base_expr, chain2->base_expr, 0);
}

/* Hash table embodying a mapping from base exprs to chains of candidates.  */
static hash_table <cand_chain_hasher> base_cand_map;

/* Look in the candidate table for a CAND_PHI that defines BASE and
   return it if found; otherwise return NULL.  */

static cand_idx
find_phi_def (tree base)
{
  slsr_cand_t c;

  if (TREE_CODE (base) != SSA_NAME)
    return 0;

  c = base_cand_from_table (base);

  if (!c || c->kind != CAND_PHI)
    return 0;

  return c->cand_num;
}

/* Helper routine for find_basis_for_candidate.  May be called twice:
   once for the candidate's base expr, and optionally again for the
   candidate's phi definition.  */

static slsr_cand_t
find_basis_for_base_expr (slsr_cand_t c, tree base_expr)
{
  cand_chain mapping_key;
  cand_chain_t chain;
  slsr_cand_t basis = NULL;

  // Limit potential of N^2 behavior for long candidate chains.
  int iters = 0;
  int max_iters = PARAM_VALUE (PARAM_MAX_SLSR_CANDIDATE_SCAN);

  mapping_key.base_expr = base_expr;
  chain = base_cand_map.find (&mapping_key);

  for (; chain && iters < max_iters; chain = chain->next, ++iters)
    {
      slsr_cand_t one_basis = chain->cand;

      if (one_basis->kind != c->kind
	  || one_basis->cand_stmt == c->cand_stmt
	  || !operand_equal_p (one_basis->stride, c->stride, 0)
	  || !types_compatible_p (one_basis->cand_type, c->cand_type)
	  || !dominated_by_p (CDI_DOMINATORS,
			      gimple_bb (c->cand_stmt),
			      gimple_bb (one_basis->cand_stmt)))
	continue;

      if (!basis || basis->cand_num < one_basis->cand_num)
	basis = one_basis;
    }

  return basis;
}

/* Use the base expr from candidate C to look for possible candidates
   that can serve as a basis for C.  Each potential basis must also
   appear in a block that dominates the candidate statement and have
   the same stride and type.  If more than one possible basis exists,
   the one with highest index in the vector is chosen; this will be
   the most immediately dominating basis.  */

static int
find_basis_for_candidate (slsr_cand_t c)
{
  slsr_cand_t basis = find_basis_for_base_expr (c, c->base_expr);

  /* If a candidate doesn't have a basis using its base expression,
     it may have a basis hidden by one or more intervening phis.  */
  if (!basis && c->def_phi)
    {
      basic_block basis_bb, phi_bb;
      slsr_cand_t phi_cand = lookup_cand (c->def_phi);
      basis = find_basis_for_base_expr (c, phi_cand->base_expr);

      if (basis)
	{
	  /* A hidden basis must dominate the phi-definition of the
	     candidate's base name.  */
	  phi_bb = gimple_bb (phi_cand->cand_stmt);
	  basis_bb = gimple_bb (basis->cand_stmt);

	  if (phi_bb == basis_bb
	      || !dominated_by_p (CDI_DOMINATORS, phi_bb, basis_bb))
	    {
	      basis = NULL;
	      c->basis = 0;
	    }

	  /* If we found a hidden basis, estimate additional dead-code
	     savings if the phi and its feeding statements can be removed.  */
	  if (basis && has_single_use (gimple_phi_result (phi_cand->cand_stmt)))
	    c->dead_savings += phi_cand->dead_savings;
	}
    }

  if (basis)
    {
      c->sibling = basis->dependent;
      basis->dependent = c->cand_num;
      return basis->cand_num;
    }

  return 0;
}

/* Record a mapping from the base expression of C to C itself, indicating that
   C may potentially serve as a basis using that base expression.  */

static void
record_potential_basis (slsr_cand_t c)
{
  cand_chain_t node;
  cand_chain **slot;

  node = (cand_chain_t) obstack_alloc (&chain_obstack, sizeof (cand_chain));
  node->base_expr = c->base_expr;
  node->cand = c;
  node->next = NULL;
  slot = base_cand_map.find_slot (node, INSERT);

  if (*slot)
    {
      cand_chain_t head = (cand_chain_t) (*slot);
      node->next = head->next;
      head->next = node;
    }
  else
    *slot = node;
}

/* Allocate storage for a new candidate and initialize its fields.
   Attempt to find a basis for the candidate.  */

static slsr_cand_t
alloc_cand_and_find_basis (enum cand_kind kind, gimple gs, tree base, 
			   double_int index, tree stride, tree ctype,
			   unsigned savings)
{
  slsr_cand_t c = (slsr_cand_t) obstack_alloc (&cand_obstack,
					       sizeof (slsr_cand));
  c->cand_stmt = gs;
  c->base_expr = base;
  c->stride = stride;
  c->index = index;
  c->cand_type = ctype;
  c->kind = kind;
  c->cand_num = cand_vec.length () + 1;
  c->next_interp = 0;
  c->dependent = 0;
  c->sibling = 0;
  c->def_phi = kind == CAND_MULT ? find_phi_def (base) : 0;
  c->dead_savings = savings;

  cand_vec.safe_push (c);

  if (kind == CAND_PHI)
    c->basis = 0;
  else
    c->basis = find_basis_for_candidate (c);

  record_potential_basis (c);

  return c;
}

/* Determine the target cost of statement GS when compiling according
   to SPEED.  */

static int
stmt_cost (gimple gs, bool speed)
{
  tree lhs, rhs1, rhs2;
  enum machine_mode lhs_mode;

  gcc_assert (is_gimple_assign (gs));
  lhs = gimple_assign_lhs (gs);
  rhs1 = gimple_assign_rhs1 (gs);
  lhs_mode = TYPE_MODE (TREE_TYPE (lhs));
  
  switch (gimple_assign_rhs_code (gs))
    {
    case MULT_EXPR:
      rhs2 = gimple_assign_rhs2 (gs);

      if (tree_fits_shwi_p (rhs2))
	return mult_by_coeff_cost (TREE_INT_CST_LOW (rhs2), lhs_mode, speed);

      gcc_assert (TREE_CODE (rhs1) != INTEGER_CST);
      return mul_cost (speed, lhs_mode);

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      return add_cost (speed, lhs_mode);

    case NEGATE_EXPR:
      return neg_cost (speed, lhs_mode);

    case NOP_EXPR:
      return convert_cost (lhs_mode, TYPE_MODE (TREE_TYPE (rhs1)), speed);

    /* Note that we don't assign costs to copies that in most cases
       will go away.  */
    default:
      ;
    }
  
  gcc_unreachable ();
  return 0;
}

/* Look up the defining statement for BASE_IN and return a pointer
   to its candidate in the candidate table, if any; otherwise NULL.
   Only CAND_ADD and CAND_MULT candidates are returned.  */

static slsr_cand_t
base_cand_from_table (tree base_in)
{
  slsr_cand_t *result;

  gimple def = SSA_NAME_DEF_STMT (base_in);
  if (!def)
    return (slsr_cand_t) NULL;

  result = (slsr_cand_t *) pointer_map_contains (stmt_cand_map, def);
  
  if (result && (*result)->kind != CAND_REF)
    return *result;

  return (slsr_cand_t) NULL;
}

/* Add an entry to the statement-to-candidate mapping.  */

static void
add_cand_for_stmt (gimple gs, slsr_cand_t c)
{
  void **slot = pointer_map_insert (stmt_cand_map, gs);
  gcc_assert (!*slot);
  *slot = c;
}

/* Given PHI which contains a phi statement, determine whether it
   satisfies all the requirements of a phi candidate.  If so, create
   a candidate.  Note that a CAND_PHI never has a basis itself, but
   is used to help find a basis for subsequent candidates.  */

static void
slsr_process_phi (gimple phi, bool speed)
{
  unsigned i;
  tree arg0_base = NULL_TREE, base_type;
  slsr_cand_t c;
  struct loop *cand_loop = gimple_bb (phi)->loop_father;
  unsigned savings = 0;

  /* A CAND_PHI requires each of its arguments to have the same
     derived base name.  (See the module header commentary for a
     definition of derived base names.)  Furthermore, all feeding
     definitions must be in the same position in the loop hierarchy
     as PHI.  */

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      slsr_cand_t arg_cand;
      tree arg = gimple_phi_arg_def (phi, i);
      tree derived_base_name = NULL_TREE;
      gimple arg_stmt = NULL;
      basic_block arg_bb = NULL;

      if (TREE_CODE (arg) != SSA_NAME)
	return;

      arg_cand = base_cand_from_table (arg);

      if (arg_cand)
	{
	  while (arg_cand->kind != CAND_ADD && arg_cand->kind != CAND_PHI)
	    {
	      if (!arg_cand->next_interp)
		return;

	      arg_cand = lookup_cand (arg_cand->next_interp);
	    }

	  if (!integer_onep (arg_cand->stride))
	    return;

	  derived_base_name = arg_cand->base_expr;
	  arg_stmt = arg_cand->cand_stmt;
	  arg_bb = gimple_bb (arg_stmt);

	  /* Gather potential dead code savings if the phi statement
	     can be removed later on.  */
	  if (has_single_use (arg))
	    {
	      if (gimple_code (arg_stmt) == GIMPLE_PHI)
		savings += arg_cand->dead_savings;
	      else
		savings += stmt_cost (arg_stmt, speed);
	    }
	}
      else
	{
	  derived_base_name = arg;

	  if (SSA_NAME_IS_DEFAULT_DEF (arg))
	    arg_bb = single_succ (ENTRY_BLOCK_PTR);
	  else
	    gimple_bb (SSA_NAME_DEF_STMT (arg));
	}

      if (!arg_bb || arg_bb->loop_father != cand_loop)
	return;

      if (i == 0)
	arg0_base = derived_base_name;
      else if (!operand_equal_p (derived_base_name, arg0_base, 0))
	return;
    }

  /* Create the candidate.  "alloc_cand_and_find_basis" is named
     misleadingly for this case, as no basis will be sought for a
     CAND_PHI.  */
  base_type = TREE_TYPE (arg0_base);

  c = alloc_cand_and_find_basis (CAND_PHI, phi, arg0_base, double_int_zero,
				 integer_one_node, base_type, savings);

  /* Add the candidate to the statement-candidate mapping.  */
  add_cand_for_stmt (phi, c);
}

/* Given PBASE which is a pointer to tree, look up the defining
   statement for it and check whether the candidate is in the
   form of:

     X = B + (1 * S), S is integer constant
     X = B + (i * S), S is integer one

   If so, set PBASE to the candidate's base_expr and return double
   int (i * S).
   Otherwise, just return double int zero.  */

static double_int
backtrace_base_for_ref (tree *pbase)
{
  tree base_in = *pbase;
  slsr_cand_t base_cand;

  STRIP_NOPS (base_in);

  /* Strip off widening conversion(s) to handle cases where
     e.g. 'B' is widened from an 'int' in order to calculate
     a 64-bit address.  */
  if (CONVERT_EXPR_P (base_in)
      && legal_cast_p_1 (base_in, TREE_OPERAND (base_in, 0)))
    base_in = get_unwidened (base_in, NULL_TREE);

  if (TREE_CODE (base_in) != SSA_NAME)
    return tree_to_double_int (integer_zero_node);

  base_cand = base_cand_from_table (base_in);

  while (base_cand && base_cand->kind != CAND_PHI)
    {
      if (base_cand->kind == CAND_ADD
	  && base_cand->index.is_one ()
	  && TREE_CODE (base_cand->stride) == INTEGER_CST)
	{
	  /* X = B + (1 * S), S is integer constant.  */
	  *pbase = base_cand->base_expr;
	  return tree_to_double_int (base_cand->stride);
	}
      else if (base_cand->kind == CAND_ADD
	       && TREE_CODE (base_cand->stride) == INTEGER_CST
	       && integer_onep (base_cand->stride))
	{
	  /* X = B + (i * S), S is integer one.  */
	  *pbase = base_cand->base_expr;
	  return base_cand->index;
	}

      if (base_cand->next_interp)
	base_cand = lookup_cand (base_cand->next_interp);
      else
	base_cand = NULL;
    }

  return tree_to_double_int (integer_zero_node);
}

/* Look for the following pattern:

    *PBASE:    MEM_REF (T1, C1)

    *POFFSET:  MULT_EXPR (T2, C3)        [C2 is zero]
                     or
               MULT_EXPR (PLUS_EXPR (T2, C2), C3)
                     or
               MULT_EXPR (MINUS_EXPR (T2, -C2), C3)

    *PINDEX:   C4 * BITS_PER_UNIT

   If not present, leave the input values unchanged and return FALSE.
   Otherwise, modify the input values as follows and return TRUE:

    *PBASE:    T1
    *POFFSET:  MULT_EXPR (T2, C3)
    *PINDEX:   C1 + (C2 * C3) + C4

   When T2 is recorded by a CAND_ADD in the form of (T2' + C5), it
   will be further restructured to:

    *PBASE:    T1
    *POFFSET:  MULT_EXPR (T2', C3)
    *PINDEX:   C1 + (C2 * C3) + C4 + (C5 * C3)  */

static bool
restructure_reference (tree *pbase, tree *poffset, double_int *pindex,
		       tree *ptype)
{
  tree base = *pbase, offset = *poffset;
  double_int index = *pindex;
  double_int bpu = double_int::from_uhwi (BITS_PER_UNIT);
  tree mult_op0, mult_op1, t1, t2, type;
  double_int c1, c2, c3, c4, c5;

  if (!base
      || !offset
      || TREE_CODE (base) != MEM_REF
      || TREE_CODE (offset) != MULT_EXPR
      || TREE_CODE (TREE_OPERAND (offset, 1)) != INTEGER_CST
      || !index.umod (bpu, FLOOR_MOD_EXPR).is_zero ())
    return false;

  t1 = TREE_OPERAND (base, 0);
  c1 = mem_ref_offset (base);
  type = TREE_TYPE (TREE_OPERAND (base, 1));

  mult_op0 = TREE_OPERAND (offset, 0);
  mult_op1 = TREE_OPERAND (offset, 1);

  c3 = tree_to_double_int (mult_op1);

  if (TREE_CODE (mult_op0) == PLUS_EXPR)

    if (TREE_CODE (TREE_OPERAND (mult_op0, 1)) == INTEGER_CST)
      {
	t2 = TREE_OPERAND (mult_op0, 0);
	c2 = tree_to_double_int (TREE_OPERAND (mult_op0, 1));
      }
    else
      return false;

  else if (TREE_CODE (mult_op0) == MINUS_EXPR)

    if (TREE_CODE (TREE_OPERAND (mult_op0, 1)) == INTEGER_CST)
      {
	t2 = TREE_OPERAND (mult_op0, 0);
	c2 = -tree_to_double_int (TREE_OPERAND (mult_op0, 1));
      }
    else
      return false;

  else
    {
      t2 = mult_op0;
      c2 = double_int_zero;
    }

  c4 = index.udiv (bpu, FLOOR_DIV_EXPR);
  c5 = backtrace_base_for_ref (&t2);

  *pbase = t1;
  *poffset = fold_build2 (MULT_EXPR, sizetype, fold_convert (sizetype, t2),
			  double_int_to_tree (sizetype, c3));
  *pindex = c1 + c2 * c3 + c4 + c5 * c3;
  *ptype = type;

  return true;
}

/* Given GS which contains a data reference, create a CAND_REF entry in
   the candidate table and attempt to find a basis.  */

static void
slsr_process_ref (gimple gs)
{
  tree ref_expr, base, offset, type;
  HOST_WIDE_INT bitsize, bitpos;
  enum machine_mode mode;
  int unsignedp, volatilep;
  double_int index;
  slsr_cand_t c;

  if (gimple_vdef (gs))
    ref_expr = gimple_assign_lhs (gs);
  else
    ref_expr = gimple_assign_rhs1 (gs);

  if (!handled_component_p (ref_expr)
      || TREE_CODE (ref_expr) == BIT_FIELD_REF
      || (TREE_CODE (ref_expr) == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (ref_expr, 1))))
    return;

  base = get_inner_reference (ref_expr, &bitsize, &bitpos, &offset, &mode,
			      &unsignedp, &volatilep, false);
  index = double_int::from_uhwi (bitpos);

  if (!restructure_reference (&base, &offset, &index, &type))
    return;

  c = alloc_cand_and_find_basis (CAND_REF, gs, base, index, offset,
				 type, 0);

  /* Add the candidate to the statement-candidate mapping.  */
  add_cand_for_stmt (gs, c);
}

/* Create a candidate entry for a statement GS, where GS multiplies
   two SSA names BASE_IN and STRIDE_IN.  Propagate any known information
   about the two SSA names into the new candidate.  Return the new
   candidate.  */

static slsr_cand_t
create_mul_ssa_cand (gimple gs, tree base_in, tree stride_in, bool speed)
{
  tree base = NULL_TREE, stride = NULL_TREE, ctype = NULL_TREE;
  double_int index;
  unsigned savings = 0;
  slsr_cand_t c;
  slsr_cand_t base_cand = base_cand_from_table (base_in);

  /* Look at all interpretations of the base candidate, if necessary,
     to find information to propagate into this candidate.  */
  while (base_cand && !base && base_cand->kind != CAND_PHI)
    {

      if (base_cand->kind == CAND_MULT && integer_onep (base_cand->stride))
	{
	  /* Y = (B + i') * 1
	     X = Y * Z
	     ================
	     X = (B + i') * Z  */
	  base = base_cand->base_expr;
	  index = base_cand->index;
	  stride = stride_in;
	  ctype = base_cand->cand_type;
	  if (has_single_use (base_in))
	    savings = (base_cand->dead_savings 
		       + stmt_cost (base_cand->cand_stmt, speed));
	}
      else if (base_cand->kind == CAND_ADD
	       && TREE_CODE (base_cand->stride) == INTEGER_CST)
	{
	  /* Y = B + (i' * S), S constant
	     X = Y * Z
	     ============================
	     X = B + ((i' * S) * Z)  */
	  base = base_cand->base_expr;
	  index = base_cand->index * tree_to_double_int (base_cand->stride);
	  stride = stride_in;
	  ctype = base_cand->cand_type;
	  if (has_single_use (base_in))
	    savings = (base_cand->dead_savings
		       + stmt_cost (base_cand->cand_stmt, speed));
	}

      if (base_cand->next_interp)
	base_cand = lookup_cand (base_cand->next_interp);
      else
	base_cand = NULL;
    }

  if (!base)
    {
      /* No interpretations had anything useful to propagate, so
	 produce X = (Y + 0) * Z.  */
      base = base_in;
      index = double_int_zero;
      stride = stride_in;
      ctype = TREE_TYPE (base_in);
    }

  c = alloc_cand_and_find_basis (CAND_MULT, gs, base, index, stride,
				 ctype, savings);
  return c;
}

/* Create a candidate entry for a statement GS, where GS multiplies
   SSA name BASE_IN by constant STRIDE_IN.  Propagate any known
   information about BASE_IN into the new candidate.  Return the new
   candidate.  */

static slsr_cand_t
create_mul_imm_cand (gimple gs, tree base_in, tree stride_in, bool speed)
{
  tree base = NULL_TREE, stride = NULL_TREE, ctype = NULL_TREE;
  double_int index, temp;
  unsigned savings = 0;
  slsr_cand_t c;
  slsr_cand_t base_cand = base_cand_from_table (base_in);

  /* Look at all interpretations of the base candidate, if necessary,
     to find information to propagate into this candidate.  */
  while (base_cand && !base && base_cand->kind != CAND_PHI)
    {
      if (base_cand->kind == CAND_MULT
	  && TREE_CODE (base_cand->stride) == INTEGER_CST)
	{
	  /* Y = (B + i') * S, S constant
	     X = Y * c
	     ============================
	     X = (B + i') * (S * c)  */
	  base = base_cand->base_expr;
	  index = base_cand->index;
	  temp = tree_to_double_int (base_cand->stride)
		 * tree_to_double_int (stride_in);
	  stride = double_int_to_tree (TREE_TYPE (stride_in), temp);
	  ctype = base_cand->cand_type;
	  if (has_single_use (base_in))
	    savings = (base_cand->dead_savings 
		       + stmt_cost (base_cand->cand_stmt, speed));
	}
      else if (base_cand->kind == CAND_ADD && integer_onep (base_cand->stride))
	{
	  /* Y = B + (i' * 1)
	     X = Y * c
	     ===========================
	     X = (B + i') * c  */
	  base = base_cand->base_expr;
	  index = base_cand->index;
	  stride = stride_in;
	  ctype = base_cand->cand_type;
	  if (has_single_use (base_in))
	    savings = (base_cand->dead_savings
		       + stmt_cost (base_cand->cand_stmt, speed));
	}
      else if (base_cand->kind == CAND_ADD
	       && base_cand->index.is_one ()
	       && TREE_CODE (base_cand->stride) == INTEGER_CST)
	{
	  /* Y = B + (1 * S), S constant
	     X = Y * c
	     ===========================
	     X = (B + S) * c  */
	  base = base_cand->base_expr;
	  index = tree_to_double_int (base_cand->stride);
	  stride = stride_in;
	  ctype = base_cand->cand_type;
	  if (has_single_use (base_in))
	    savings = (base_cand->dead_savings
		       + stmt_cost (base_cand->cand_stmt, speed));
	}

      if (base_cand->next_interp)
	base_cand = lookup_cand (base_cand->next_interp);
      else
	base_cand = NULL;
    }

  if (!base)
    {
      /* No interpretations had anything useful to propagate, so
	 produce X = (Y + 0) * c.  */
      base = base_in;
      index = double_int_zero;
      stride = stride_in;
      ctype = TREE_TYPE (base_in);
    }

  c = alloc_cand_and_find_basis (CAND_MULT, gs, base, index, stride,
				 ctype, savings);
  return c;
}

/* Given GS which is a multiply of scalar integers, make an appropriate
   entry in the candidate table.  If this is a multiply of two SSA names,
   create two CAND_MULT interpretations and attempt to find a basis for
   each of them.  Otherwise, create a single CAND_MULT and attempt to
   find a basis.  */

static void
slsr_process_mul (gimple gs, tree rhs1, tree rhs2, bool speed)
{
  slsr_cand_t c, c2;

  /* If this is a multiply of an SSA name with itself, it is highly
     unlikely that we will get a strength reduction opportunity, so
     don't record it as a candidate.  This simplifies the logic for
     finding a basis, so if this is removed that must be considered.  */
  if (rhs1 == rhs2)
    return;

  if (TREE_CODE (rhs2) == SSA_NAME)
    {
      /* Record an interpretation of this statement in the candidate table
	 assuming RHS1 is the base expression and RHS2 is the stride.  */
      c = create_mul_ssa_cand (gs, rhs1, rhs2, speed);

      /* Add the first interpretation to the statement-candidate mapping.  */
      add_cand_for_stmt (gs, c);

      /* Record another interpretation of this statement assuming RHS1
	 is the stride and RHS2 is the base expression.  */
      c2 = create_mul_ssa_cand (gs, rhs2, rhs1, speed);
      c->next_interp = c2->cand_num;
    }
  else
    {
      /* Record an interpretation for the multiply-immediate.  */
      c = create_mul_imm_cand (gs, rhs1, rhs2, speed);

      /* Add the interpretation to the statement-candidate mapping.  */
      add_cand_for_stmt (gs, c);
    }
}

/* Create a candidate entry for a statement GS, where GS adds two
   SSA names BASE_IN and ADDEND_IN if SUBTRACT_P is false, and
   subtracts ADDEND_IN from BASE_IN otherwise.  Propagate any known
   information about the two SSA names into the new candidate.
   Return the new candidate.  */

static slsr_cand_t
create_add_ssa_cand (gimple gs, tree base_in, tree addend_in,
		     bool subtract_p, bool speed)
{
  tree base = NULL_TREE, stride = NULL_TREE, ctype = NULL;
  double_int index;
  unsigned savings = 0;
  slsr_cand_t c;
  slsr_cand_t base_cand = base_cand_from_table (base_in);
  slsr_cand_t addend_cand = base_cand_from_table (addend_in);

  /* The most useful transformation is a multiply-immediate feeding
     an add or subtract.  Look for that first.  */
  while (addend_cand && !base && addend_cand->kind != CAND_PHI)
    {
      if (addend_cand->kind == CAND_MULT
	  && addend_cand->index.is_zero ()
	  && TREE_CODE (addend_cand->stride) == INTEGER_CST)
	{
	  /* Z = (B + 0) * S, S constant
	     X = Y +/- Z
	     ===========================
	     X = Y + ((+/-1 * S) * B)  */
	  base = base_in;
	  index = tree_to_double_int (addend_cand->stride);
	  if (subtract_p)
	    index = -index;
	  stride = addend_cand->base_expr;
	  ctype = TREE_TYPE (base_in);
	  if (has_single_use (addend_in))
	    savings = (addend_cand->dead_savings
		       + stmt_cost (addend_cand->cand_stmt, speed));
	}

      if (addend_cand->next_interp)
	addend_cand = lookup_cand (addend_cand->next_interp);
      else
	addend_cand = NULL;
    }

  while (base_cand && !base && base_cand->kind != CAND_PHI)
    {
      if (base_cand->kind == CAND_ADD
	  && (base_cand->index.is_zero ()
	      || operand_equal_p (base_cand->stride,
				  integer_zero_node, 0)))
	{
	  /* Y = B + (i' * S), i' * S = 0
	     X = Y +/- Z
	     ============================
	     X = B + (+/-1 * Z)  */
	  base = base_cand->base_expr;
	  index = subtract_p ? double_int_minus_one : double_int_one;
	  stride = addend_in;
	  ctype = base_cand->cand_type;
	  if (has_single_use (base_in))
	    savings = (base_cand->dead_savings
		       + stmt_cost (base_cand->cand_stmt, speed));
	}
      else if (subtract_p)
	{
	  slsr_cand_t subtrahend_cand = base_cand_from_table (addend_in);

	  while (subtrahend_cand && !base && subtrahend_cand->kind != CAND_PHI)
	    {
	      if (subtrahend_cand->kind == CAND_MULT
		  && subtrahend_cand->index.is_zero ()
		  && TREE_CODE (subtrahend_cand->stride) == INTEGER_CST)
		{
		  /* Z = (B + 0) * S, S constant
		     X = Y - Z
		     ===========================
		     Value:  X = Y + ((-1 * S) * B)  */
		  base = base_in;
		  index = tree_to_double_int (subtrahend_cand->stride);
		  index = -index;
		  stride = subtrahend_cand->base_expr;
		  ctype = TREE_TYPE (base_in);
		  if (has_single_use (addend_in))
		    savings = (subtrahend_cand->dead_savings 
			       + stmt_cost (subtrahend_cand->cand_stmt, speed));
		}
	      
	      if (subtrahend_cand->next_interp)
		subtrahend_cand = lookup_cand (subtrahend_cand->next_interp);
	      else
		subtrahend_cand = NULL;
	    }
	}
      
      if (base_cand->next_interp)
	base_cand = lookup_cand (base_cand->next_interp);
      else
	base_cand = NULL;
    }

  if (!base)
    {
      /* No interpretations had anything useful to propagate, so
	 produce X = Y + (1 * Z).  */
      base = base_in;
      index = subtract_p ? double_int_minus_one : double_int_one;
      stride = addend_in;
      ctype = TREE_TYPE (base_in);
    }

  c = alloc_cand_and_find_basis (CAND_ADD, gs, base, index, stride,
				 ctype, savings);
  return c;
}

/* Create a candidate entry for a statement GS, where GS adds SSA
   name BASE_IN to constant INDEX_IN.  Propagate any known information
   about BASE_IN into the new candidate.  Return the new candidate.  */

static slsr_cand_t
create_add_imm_cand (gimple gs, tree base_in, double_int index_in, bool speed)
{
  enum cand_kind kind = CAND_ADD;
  tree base = NULL_TREE, stride = NULL_TREE, ctype = NULL_TREE;
  double_int index, multiple;
  unsigned savings = 0;
  slsr_cand_t c;
  slsr_cand_t base_cand = base_cand_from_table (base_in);

  while (base_cand && !base && base_cand->kind != CAND_PHI)
    {
      bool unsigned_p = TYPE_UNSIGNED (TREE_TYPE (base_cand->stride));

      if (TREE_CODE (base_cand->stride) == INTEGER_CST
	  && index_in.multiple_of (tree_to_double_int (base_cand->stride),
				   unsigned_p, &multiple))
	{
	  /* Y = (B + i') * S, S constant, c = kS for some integer k
	     X = Y + c
	     ============================
	     X = (B + (i'+ k)) * S  
	  OR
	     Y = B + (i' * S), S constant, c = kS for some integer k
	     X = Y + c
	     ============================
	     X = (B + (i'+ k)) * S  */
	  kind = base_cand->kind;
	  base = base_cand->base_expr;
	  index = base_cand->index + multiple;
	  stride = base_cand->stride;
	  ctype = base_cand->cand_type;
	  if (has_single_use (base_in))
	    savings = (base_cand->dead_savings 
		       + stmt_cost (base_cand->cand_stmt, speed));
	}

      if (base_cand->next_interp)
	base_cand = lookup_cand (base_cand->next_interp);
      else
	base_cand = NULL;
    }

  if (!base)
    {
      /* No interpretations had anything useful to propagate, so
	 produce X = Y + (c * 1).  */
      kind = CAND_ADD;
      base = base_in;
      index = index_in;
      stride = integer_one_node;
      ctype = TREE_TYPE (base_in);
    }

  c = alloc_cand_and_find_basis (kind, gs, base, index, stride,
				 ctype, savings);
  return c;
}

/* Given GS which is an add or subtract of scalar integers or pointers,
   make at least one appropriate entry in the candidate table.  */

static void
slsr_process_add (gimple gs, tree rhs1, tree rhs2, bool speed)
{
  bool subtract_p = gimple_assign_rhs_code (gs) == MINUS_EXPR;
  slsr_cand_t c = NULL, c2;

  if (TREE_CODE (rhs2) == SSA_NAME)
    {
      /* First record an interpretation assuming RHS1 is the base expression
	 and RHS2 is the stride.  But it doesn't make sense for the
	 stride to be a pointer, so don't record a candidate in that case.  */
      if (!POINTER_TYPE_P (TREE_TYPE (rhs2)))
	{
	  c = create_add_ssa_cand (gs, rhs1, rhs2, subtract_p, speed);

	  /* Add the first interpretation to the statement-candidate
	     mapping.  */
	  add_cand_for_stmt (gs, c);
	}

      /* If the two RHS operands are identical, or this is a subtract,
	 we're done.  */
      if (operand_equal_p (rhs1, rhs2, 0) || subtract_p)
	return;

      /* Otherwise, record another interpretation assuming RHS2 is the
	 base expression and RHS1 is the stride, again provided that the
	 stride is not a pointer.  */
      if (!POINTER_TYPE_P (TREE_TYPE (rhs1)))
	{
	  c2 = create_add_ssa_cand (gs, rhs2, rhs1, false, speed);
	  if (c)
	    c->next_interp = c2->cand_num;
	  else
	    add_cand_for_stmt (gs, c2);
	}
    }
  else
    {
      double_int index;

      /* Record an interpretation for the add-immediate.  */
      index = tree_to_double_int (rhs2);
      if (subtract_p)
	index = -index;

      c = create_add_imm_cand (gs, rhs1, index, speed);

      /* Add the interpretation to the statement-candidate mapping.  */
      add_cand_for_stmt (gs, c);
    }
}

/* Given GS which is a negate of a scalar integer, make an appropriate
   entry in the candidate table.  A negate is equivalent to a multiply
   by -1.  */

static void
slsr_process_neg (gimple gs, tree rhs1, bool speed)
{
  /* Record a CAND_MULT interpretation for the multiply by -1.  */
  slsr_cand_t c = create_mul_imm_cand (gs, rhs1, integer_minus_one_node, speed);

  /* Add the interpretation to the statement-candidate mapping.  */
  add_cand_for_stmt (gs, c);
}

/* Help function for legal_cast_p, operating on two trees.  Checks
   whether it's allowable to cast from RHS to LHS.  See legal_cast_p
   for more details.  */

static bool
legal_cast_p_1 (tree lhs, tree rhs)
{
  tree lhs_type, rhs_type;
  unsigned lhs_size, rhs_size;
  bool lhs_wraps, rhs_wraps;

  lhs_type = TREE_TYPE (lhs);
  rhs_type = TREE_TYPE (rhs);
  lhs_size = TYPE_PRECISION (lhs_type);
  rhs_size = TYPE_PRECISION (rhs_type);
  lhs_wraps = TYPE_OVERFLOW_WRAPS (lhs_type);
  rhs_wraps = TYPE_OVERFLOW_WRAPS (rhs_type);

  if (lhs_size < rhs_size
      || (rhs_wraps && !lhs_wraps)
      || (rhs_wraps && lhs_wraps && rhs_size != lhs_size))
    return false;

  return true;
}

/* Return TRUE if GS is a statement that defines an SSA name from
   a conversion and is legal for us to combine with an add and multiply
   in the candidate table.  For example, suppose we have:

     A = B + i;
     C = (type) A;
     D = C * S;

   Without the type-cast, we would create a CAND_MULT for D with base B,
   index i, and stride S.  We want to record this candidate only if it
   is equivalent to apply the type cast following the multiply:

     A = B + i;
     E = A * S;
     D = (type) E;

   We will record the type with the candidate for D.  This allows us
   to use a similar previous candidate as a basis.  If we have earlier seen

     A' = B + i';
     C' = (type) A';
     D' = C' * S;

   we can replace D with

     D = D' + (i - i') * S;

   But if moving the type-cast would change semantics, we mustn't do this.

   This is legitimate for casts from a non-wrapping integral type to
   any integral type of the same or larger size.  It is not legitimate
   to convert a wrapping type to a non-wrapping type, or to a wrapping
   type of a different size.  I.e., with a wrapping type, we must
   assume that the addition B + i could wrap, in which case performing
   the multiply before or after one of the "illegal" type casts will
   have different semantics.  */

static bool
legal_cast_p (gimple gs, tree rhs)
{
  if (!is_gimple_assign (gs)
      || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (gs)))
    return false;

  return legal_cast_p_1 (gimple_assign_lhs (gs), rhs);
}

/* Given GS which is a cast to a scalar integer type, determine whether
   the cast is legal for strength reduction.  If so, make at least one
   appropriate entry in the candidate table.  */

static void
slsr_process_cast (gimple gs, tree rhs1, bool speed)
{
  tree lhs, ctype;
  slsr_cand_t base_cand, c, c2;
  unsigned savings = 0;

  if (!legal_cast_p (gs, rhs1))
    return;

  lhs = gimple_assign_lhs (gs);
  base_cand = base_cand_from_table (rhs1);
  ctype = TREE_TYPE (lhs);

  if (base_cand && base_cand->kind != CAND_PHI)
    {
      while (base_cand)
	{
	  /* Propagate all data from the base candidate except the type,
	     which comes from the cast, and the base candidate's cast,
	     which is no longer applicable.  */
	  if (has_single_use (rhs1))
	    savings = (base_cand->dead_savings 
		       + stmt_cost (base_cand->cand_stmt, speed));

	  c = alloc_cand_and_find_basis (base_cand->kind, gs,
					 base_cand->base_expr,
					 base_cand->index, base_cand->stride,
					 ctype, savings);
	  if (base_cand->next_interp)
	    base_cand = lookup_cand (base_cand->next_interp);
	  else
	    base_cand = NULL;
	}
    }
  else 
    {
      /* If nothing is known about the RHS, create fresh CAND_ADD and
	 CAND_MULT interpretations:

	 X = Y + (0 * 1)
	 X = (Y + 0) * 1

	 The first of these is somewhat arbitrary, but the choice of
	 1 for the stride simplifies the logic for propagating casts
	 into their uses.  */
      c = alloc_cand_and_find_basis (CAND_ADD, gs, rhs1, double_int_zero,
				     integer_one_node, ctype, 0);
      c2 = alloc_cand_and_find_basis (CAND_MULT, gs, rhs1, double_int_zero,
				      integer_one_node, ctype, 0);
      c->next_interp = c2->cand_num;
    }

  /* Add the first (or only) interpretation to the statement-candidate
     mapping.  */
  add_cand_for_stmt (gs, c);
}

/* Given GS which is a copy of a scalar integer type, make at least one
   appropriate entry in the candidate table.

   This interface is included for completeness, but is unnecessary
   if this pass immediately follows a pass that performs copy 
   propagation, such as DOM.  */

static void
slsr_process_copy (gimple gs, tree rhs1, bool speed)
{
  slsr_cand_t base_cand, c, c2;
  unsigned savings = 0;

  base_cand = base_cand_from_table (rhs1);

  if (base_cand && base_cand->kind != CAND_PHI)
    {
      while (base_cand)
	{
	  /* Propagate all data from the base candidate.  */
	  if (has_single_use (rhs1))
	    savings = (base_cand->dead_savings 
		       + stmt_cost (base_cand->cand_stmt, speed));

	  c = alloc_cand_and_find_basis (base_cand->kind, gs,
					 base_cand->base_expr,
					 base_cand->index, base_cand->stride,
					 base_cand->cand_type, savings);
	  if (base_cand->next_interp)
	    base_cand = lookup_cand (base_cand->next_interp);
	  else
	    base_cand = NULL;
	}
    }
  else 
    {
      /* If nothing is known about the RHS, create fresh CAND_ADD and
	 CAND_MULT interpretations:

	 X = Y + (0 * 1)
	 X = (Y + 0) * 1

	 The first of these is somewhat arbitrary, but the choice of
	 1 for the stride simplifies the logic for propagating casts
	 into their uses.  */
      c = alloc_cand_and_find_basis (CAND_ADD, gs, rhs1, double_int_zero,
				     integer_one_node, TREE_TYPE (rhs1), 0);
      c2 = alloc_cand_and_find_basis (CAND_MULT, gs, rhs1, double_int_zero,
				      integer_one_node, TREE_TYPE (rhs1), 0);
      c->next_interp = c2->cand_num;
    }

  /* Add the first (or only) interpretation to the statement-candidate
     mapping.  */
  add_cand_for_stmt (gs, c);
}

class find_candidates_dom_walker : public dom_walker
{
public:
  find_candidates_dom_walker (cdi_direction direction)
    : dom_walker (direction) {}
  virtual void before_dom_children (basic_block);
};

/* Find strength-reduction candidates in block BB.  */

void
find_candidates_dom_walker::before_dom_children (basic_block bb)
{
  bool speed = optimize_bb_for_speed_p (bb);
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    slsr_process_phi (gsi_stmt (gsi), speed);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple gs = gsi_stmt (gsi);

      if (gimple_vuse (gs) && gimple_assign_single_p (gs))
	slsr_process_ref (gs);

      else if (is_gimple_assign (gs)
	       && SCALAR_INT_MODE_P
	            (TYPE_MODE (TREE_TYPE (gimple_assign_lhs (gs)))))
	{
	  tree rhs1 = NULL_TREE, rhs2 = NULL_TREE;

	  switch (gimple_assign_rhs_code (gs))
	    {
	    case MULT_EXPR:
	    case PLUS_EXPR:
	      rhs1 = gimple_assign_rhs1 (gs);
	      rhs2 = gimple_assign_rhs2 (gs);
	      /* Should never happen, but currently some buggy situations
		 in earlier phases put constants in rhs1.  */
	      if (TREE_CODE (rhs1) != SSA_NAME)
		continue;
	      break;

	    /* Possible future opportunity: rhs1 of a ptr+ can be
	       an ADDR_EXPR.  */
	    case POINTER_PLUS_EXPR:
	    case MINUS_EXPR:
	      rhs2 = gimple_assign_rhs2 (gs);
	      /* Fall-through.  */

	    case NOP_EXPR:
	    case MODIFY_EXPR:
	    case NEGATE_EXPR:
	      rhs1 = gimple_assign_rhs1 (gs);
	      if (TREE_CODE (rhs1) != SSA_NAME)
		continue;
	      break;

	    default:
	      ;
	    }

	  switch (gimple_assign_rhs_code (gs))
	    {
	    case MULT_EXPR:
	      slsr_process_mul (gs, rhs1, rhs2, speed);
	      break;

	    case PLUS_EXPR:
	    case POINTER_PLUS_EXPR:
	    case MINUS_EXPR:
	      slsr_process_add (gs, rhs1, rhs2, speed);
	      break;

	    case NEGATE_EXPR:
	      slsr_process_neg (gs, rhs1, speed);
	      break;

	    case NOP_EXPR:
	      slsr_process_cast (gs, rhs1, speed);
	      break;

	    case MODIFY_EXPR:
	      slsr_process_copy (gs, rhs1, speed);
	      break;

	    default:
	      ;
	    }
	}
    }
}

/* Dump a candidate for debug.  */

static void
dump_candidate (slsr_cand_t c)
{
  fprintf (dump_file, "%3d  [%d] ", c->cand_num,
	   gimple_bb (c->cand_stmt)->index);
  print_gimple_stmt (dump_file, c->cand_stmt, 0, 0);
  switch (c->kind)
    {
    case CAND_MULT:
      fputs ("     MULT : (", dump_file);
      print_generic_expr (dump_file, c->base_expr, 0);
      fputs (" + ", dump_file);
      dump_double_int (dump_file, c->index, false);
      fputs (") * ", dump_file);
      print_generic_expr (dump_file, c->stride, 0);
      fputs (" : ", dump_file);
      break;
    case CAND_ADD:
      fputs ("     ADD  : ", dump_file);
      print_generic_expr (dump_file, c->base_expr, 0);
      fputs (" + (", dump_file);
      dump_double_int (dump_file, c->index, false);
      fputs (" * ", dump_file);
      print_generic_expr (dump_file, c->stride, 0);
      fputs (") : ", dump_file);
      break;
    case CAND_REF:
      fputs ("     REF  : ", dump_file);
      print_generic_expr (dump_file, c->base_expr, 0);
      fputs (" + (", dump_file);
      print_generic_expr (dump_file, c->stride, 0);
      fputs (") + ", dump_file);
      dump_double_int (dump_file, c->index, false);
      fputs (" : ", dump_file);
      break;
    case CAND_PHI:
      fputs ("     PHI  : ", dump_file);
      print_generic_expr (dump_file, c->base_expr, 0);
      fputs (" + (unknown * ", dump_file);
      print_generic_expr (dump_file, c->stride, 0);
      fputs (") : ", dump_file);
      break;
    default:
      gcc_unreachable ();
    }
  print_generic_expr (dump_file, c->cand_type, 0);
  fprintf (dump_file, "\n     basis: %d  dependent: %d  sibling: %d\n",
	   c->basis, c->dependent, c->sibling);
  fprintf (dump_file, "     next-interp: %d  dead-savings: %d\n",
	   c->next_interp, c->dead_savings);
  if (c->def_phi)
    fprintf (dump_file, "     phi:  %d\n", c->def_phi);
  fputs ("\n", dump_file);
}

/* Dump the candidate vector for debug.  */

static void
dump_cand_vec (void)
{
  unsigned i;
  slsr_cand_t c;

  fprintf (dump_file, "\nStrength reduction candidate vector:\n\n");
  
  FOR_EACH_VEC_ELT (cand_vec, i, c)
    dump_candidate (c);
}

/* Callback used to dump the candidate chains hash table.  */

int
ssa_base_cand_dump_callback (cand_chain **slot, void *ignored ATTRIBUTE_UNUSED)
{
  const_cand_chain_t chain = *slot;
  cand_chain_t p;

  print_generic_expr (dump_file, chain->base_expr, 0);
  fprintf (dump_file, " -> %d", chain->cand->cand_num);

  for (p = chain->next; p; p = p->next)
    fprintf (dump_file, " -> %d", p->cand->cand_num);

  fputs ("\n", dump_file);
  return 1;
}

/* Dump the candidate chains.  */

static void
dump_cand_chains (void)
{
  fprintf (dump_file, "\nStrength reduction candidate chains:\n\n");
  base_cand_map.traverse_noresize <void *, ssa_base_cand_dump_callback> (NULL);
  fputs ("\n", dump_file);
}

/* Dump the increment vector for debug.  */

static void
dump_incr_vec (void)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      unsigned i;

      fprintf (dump_file, "\nIncrement vector:\n\n");
  
      for (i = 0; i < incr_vec_len; i++)
	{
	  fprintf (dump_file, "%3d  increment:   ", i);
	  dump_double_int (dump_file, incr_vec[i].incr, false);
	  fprintf (dump_file, "\n     count:       %d", incr_vec[i].count);
	  fprintf (dump_file, "\n     cost:        %d", incr_vec[i].cost);
	  fputs ("\n     initializer: ", dump_file);
	  print_generic_expr (dump_file, incr_vec[i].initializer, 0);
	  fputs ("\n\n", dump_file);
	}
    }
}

/* Replace *EXPR in candidate C with an equivalent strength-reduced
   data reference.  */

static void
replace_ref (tree *expr, slsr_cand_t c)
{
  tree add_expr, mem_ref, acc_type = TREE_TYPE (*expr);
  unsigned HOST_WIDE_INT misalign;
  unsigned align;

  /* Ensure the memory reference carries the minimum alignment
     requirement for the data type.  See PR58041.  */
  get_object_alignment_1 (*expr, &align, &misalign);
  if (misalign != 0)
    align = (misalign & -misalign);
  if (align < TYPE_ALIGN (acc_type))
    acc_type = build_aligned_type (acc_type, align);

  add_expr = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (c->base_expr),
			  c->base_expr, c->stride);
  mem_ref = fold_build2 (MEM_REF, acc_type, add_expr,
			 double_int_to_tree (c->cand_type, c->index));

  /* Gimplify the base addressing expression for the new MEM_REF tree.  */
  gimple_stmt_iterator gsi = gsi_for_stmt (c->cand_stmt);
  TREE_OPERAND (mem_ref, 0)
    = force_gimple_operand_gsi (&gsi, TREE_OPERAND (mem_ref, 0),
				/*simple_p=*/true, NULL,
				/*before=*/true, GSI_SAME_STMT);
  copy_ref_info (mem_ref, *expr);
  *expr = mem_ref;
  update_stmt (c->cand_stmt);
}

/* Replace CAND_REF candidate C, each sibling of candidate C, and each
   dependent of candidate C with an equivalent strength-reduced data
   reference.  */

static void
replace_refs (slsr_cand_t c)
{
  if (gimple_vdef (c->cand_stmt))
    {
      tree *lhs = gimple_assign_lhs_ptr (c->cand_stmt);
      replace_ref (lhs, c);
    }
  else
    {
      tree *rhs = gimple_assign_rhs1_ptr (c->cand_stmt);
      replace_ref (rhs, c);
    }

  if (c->sibling)
    replace_refs (lookup_cand (c->sibling));

  if (c->dependent)
    replace_refs (lookup_cand (c->dependent));
}

/* Return TRUE if candidate C is dependent upon a PHI.  */

static bool
phi_dependent_cand_p (slsr_cand_t c)
{
  /* A candidate is not necessarily dependent upon a PHI just because
     it has a phi definition for its base name.  It may have a basis
     that relies upon the same phi definition, in which case the PHI
     is irrelevant to this candidate.  */
  return (c->def_phi
	  && c->basis
	  && lookup_cand (c->basis)->def_phi != c->def_phi);
}

/* Calculate the increment required for candidate C relative to 
   its basis.  */

static double_int
cand_increment (slsr_cand_t c)
{
  slsr_cand_t basis;

  /* If the candidate doesn't have a basis, just return its own
     index.  This is useful in record_increments to help us find
     an existing initializer.  Also, if the candidate's basis is
     hidden by a phi, then its own index will be the increment
     from the newly introduced phi basis.  */
  if (!c->basis || phi_dependent_cand_p (c))
    return c->index;

  basis = lookup_cand (c->basis);
  gcc_assert (operand_equal_p (c->base_expr, basis->base_expr, 0));
  return c->index - basis->index;
}

/* Calculate the increment required for candidate C relative to
   its basis.  If we aren't going to generate pointer arithmetic
   for this candidate, return the absolute value of that increment
   instead.  */

static inline double_int
cand_abs_increment (slsr_cand_t c)
{
  double_int increment = cand_increment (c);

  if (!address_arithmetic_p && increment.is_negative ())
    increment = -increment;

  return increment;
}

/* Return TRUE iff candidate C has already been replaced under
   another interpretation.  */

static inline bool
cand_already_replaced (slsr_cand_t c)
{
  return (gimple_bb (c->cand_stmt) == 0);
}

/* Common logic used by replace_unconditional_candidate and
   replace_conditional_candidate.  */

static void
replace_mult_candidate (slsr_cand_t c, tree basis_name, double_int bump)
{
  tree target_type = TREE_TYPE (gimple_assign_lhs (c->cand_stmt));
  enum tree_code cand_code = gimple_assign_rhs_code (c->cand_stmt);

  /* It is highly unlikely, but possible, that the resulting
     bump doesn't fit in a HWI.  Abandon the replacement
     in this case.  This does not affect siblings or dependents
     of C.  Restriction to signed HWI is conservative for unsigned
     types but allows for safe negation without twisted logic.  */
  if (bump.fits_shwi ()
      && bump.to_shwi () != HOST_WIDE_INT_MIN
      /* It is not useful to replace casts, copies, or adds of
	 an SSA name and a constant.  */
      && cand_code != MODIFY_EXPR
      && cand_code != NOP_EXPR
      && cand_code != PLUS_EXPR
      && cand_code != POINTER_PLUS_EXPR
      && cand_code != MINUS_EXPR)
    {
      enum tree_code code = PLUS_EXPR;
      tree bump_tree;
      gimple stmt_to_print = NULL;

      /* If the basis name and the candidate's LHS have incompatible
	 types, introduce a cast.  */
      if (!useless_type_conversion_p (target_type, TREE_TYPE (basis_name)))
	basis_name = introduce_cast_before_cand (c, target_type, basis_name);
      if (bump.is_negative ())
	{
	  code = MINUS_EXPR;
	  bump = -bump;
	}

      bump_tree = double_int_to_tree (target_type, bump);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fputs ("Replacing: ", dump_file);
	  print_gimple_stmt (dump_file, c->cand_stmt, 0, 0);
	}

      if (bump.is_zero ())
	{
	  tree lhs = gimple_assign_lhs (c->cand_stmt);
	  gimple copy_stmt = gimple_build_assign (lhs, basis_name);
	  gimple_stmt_iterator gsi = gsi_for_stmt (c->cand_stmt);
	  gimple_set_location (copy_stmt, gimple_location (c->cand_stmt));
	  gsi_replace (&gsi, copy_stmt, false);
	  c->cand_stmt = copy_stmt;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    stmt_to_print = copy_stmt;
	}
      else
	{
	  tree rhs1, rhs2;
	  if (cand_code != NEGATE_EXPR) {
	    rhs1 = gimple_assign_rhs1 (c->cand_stmt);
	    rhs2 = gimple_assign_rhs2 (c->cand_stmt);
	  }
	  if (cand_code != NEGATE_EXPR
	      && ((operand_equal_p (rhs1, basis_name, 0)
		   && operand_equal_p (rhs2, bump_tree, 0))
		  || (operand_equal_p (rhs1, bump_tree, 0)
		      && operand_equal_p (rhs2, basis_name, 0))))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fputs ("(duplicate, not actually replacing)", dump_file);
		  stmt_to_print = c->cand_stmt;
		}
	    }
	  else
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (c->cand_stmt);
	      gimple_assign_set_rhs_with_ops (&gsi, code,
					      basis_name, bump_tree);
	      update_stmt (gsi_stmt (gsi));
              c->cand_stmt = gsi_stmt (gsi);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		stmt_to_print = gsi_stmt (gsi);
	    }
	}
  
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fputs ("With: ", dump_file);
	  print_gimple_stmt (dump_file, stmt_to_print, 0, 0);
	  fputs ("\n", dump_file);
  	}
    }
}

/* Replace candidate C with an add or subtract.   Note that we only
   operate on CAND_MULTs with known strides, so we will never generate
   a POINTER_PLUS_EXPR.  Each candidate X = (B + i) * S is replaced by
   X = Y + ((i - i') * S), as described in the module commentary.  The
   folded value ((i - i') * S) is referred to here as the "bump."  */

static void
replace_unconditional_candidate (slsr_cand_t c)
{
  slsr_cand_t basis;
  double_int stride, bump;

  if (cand_already_replaced (c))
    return;

  basis = lookup_cand (c->basis);
  stride = tree_to_double_int (c->stride);
  bump = cand_increment (c) * stride;

  replace_mult_candidate (c, gimple_assign_lhs (basis->cand_stmt), bump);
}

/* Return the index in the increment vector of the given INCREMENT,
   or -1 if not found.  The latter can occur if more than
   MAX_INCR_VEC_LEN increments have been found.  */

static inline int
incr_vec_index (double_int increment)
{
  unsigned i;
  
  for (i = 0; i < incr_vec_len && increment != incr_vec[i].incr; i++)
    ;

  if (i < incr_vec_len)
    return i;
  else
    return -1;
}

/* Create a new statement along edge E to add BASIS_NAME to the product
   of INCREMENT and the stride of candidate C.  Create and return a new
   SSA name from *VAR to be used as the LHS of the new statement.
   KNOWN_STRIDE is true iff C's stride is a constant.  */

static tree
create_add_on_incoming_edge (slsr_cand_t c, tree basis_name,
			     double_int increment, edge e, location_t loc,
			     bool known_stride)
{
  basic_block insert_bb;
  gimple_stmt_iterator gsi;
  tree lhs, basis_type;
  gimple new_stmt;

  /* If the add candidate along this incoming edge has the same
     index as C's hidden basis, the hidden basis represents this
     edge correctly.  */
  if (increment.is_zero ())
    return basis_name;

  basis_type = TREE_TYPE (basis_name);
  lhs = make_temp_ssa_name (basis_type, NULL, "slsr");

  if (known_stride)
    {
      tree bump_tree;
      enum tree_code code = PLUS_EXPR;
      double_int bump = increment * tree_to_double_int (c->stride);
      if (bump.is_negative ())
	{
	  code = MINUS_EXPR;
	  bump = -bump;
	}

      bump_tree = double_int_to_tree (basis_type, bump);
      new_stmt = gimple_build_assign_with_ops (code, lhs, basis_name,
					       bump_tree);
    }
  else
    {
      int i;
      bool negate_incr = (!address_arithmetic_p && increment.is_negative ());
      i = incr_vec_index (negate_incr ? -increment : increment);
      gcc_assert (i >= 0);

      if (incr_vec[i].initializer)
	{
	  enum tree_code code = negate_incr ? MINUS_EXPR : PLUS_EXPR;
	  new_stmt = gimple_build_assign_with_ops (code, lhs, basis_name,
						   incr_vec[i].initializer);
	}
      else if (increment.is_one ())
	new_stmt = gimple_build_assign_with_ops (PLUS_EXPR, lhs, basis_name,
						 c->stride);
      else if (increment.is_minus_one ())
	new_stmt = gimple_build_assign_with_ops (MINUS_EXPR, lhs, basis_name,
						 c->stride);
      else
	gcc_unreachable ();
    }

  insert_bb = single_succ_p (e->src) ? e->src : split_edge (e);
  gsi = gsi_last_bb (insert_bb);

  if (!gsi_end_p (gsi) && is_ctrl_stmt (gsi_stmt (gsi)))
    gsi_insert_before (&gsi, new_stmt, GSI_NEW_STMT);
  else
    gsi_insert_after (&gsi, new_stmt, GSI_NEW_STMT);

  gimple_set_location (new_stmt, loc);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Inserting in block %d: ", insert_bb->index);
      print_gimple_stmt (dump_file, new_stmt, 0, 0);
    }

  return lhs;
}

/* Given a candidate C with BASIS_NAME being the LHS of C's basis which
   is hidden by the phi node FROM_PHI, create a new phi node in the same
   block as FROM_PHI.  The new phi is suitable for use as a basis by C,
   with its phi arguments representing conditional adjustments to the
   hidden basis along conditional incoming paths.  Those adjustments are
   made by creating add statements (and sometimes recursively creating
   phis) along those incoming paths.  LOC is the location to attach to
   the introduced statements.  KNOWN_STRIDE is true iff C's stride is a
   constant.  */

static tree
create_phi_basis (slsr_cand_t c, gimple from_phi, tree basis_name,
		  location_t loc, bool known_stride)
{
  int i;
  tree name, phi_arg;
  gimple phi;
  vec<tree> phi_args;
  slsr_cand_t basis = lookup_cand (c->basis);
  int nargs = gimple_phi_num_args (from_phi);
  basic_block phi_bb = gimple_bb (from_phi);
  slsr_cand_t phi_cand = base_cand_from_table (gimple_phi_result (from_phi));
  phi_args.create (nargs);

  /* Process each argument of the existing phi that represents
     conditionally-executed add candidates.  */
  for (i = 0; i < nargs; i++)
    {
      edge e = (*phi_bb->preds)[i];
      tree arg = gimple_phi_arg_def (from_phi, i);
      tree feeding_def;

      /* If the phi argument is the base name of the CAND_PHI, then
	 this incoming arc should use the hidden basis.  */
      if (operand_equal_p (arg, phi_cand->base_expr, 0))
	if (basis->index.is_zero ())
	  feeding_def = gimple_assign_lhs (basis->cand_stmt);
	else
	  {
	    double_int incr = -basis->index;
	    feeding_def = create_add_on_incoming_edge (c, basis_name, incr,
						       e, loc, known_stride);
	  }
      else
	{
	  gimple arg_def = SSA_NAME_DEF_STMT (arg);

	  /* If there is another phi along this incoming edge, we must
	     process it in the same fashion to ensure that all basis
	     adjustments are made along its incoming edges.  */
	  if (gimple_code (arg_def) == GIMPLE_PHI)
	    feeding_def = create_phi_basis (c, arg_def, basis_name,
					    loc, known_stride);
	  else
	    {
	      slsr_cand_t arg_cand = base_cand_from_table (arg);
	      double_int diff = arg_cand->index - basis->index;
	      feeding_def = create_add_on_incoming_edge (c, basis_name, diff,
							 e, loc, known_stride);
	    }
	}

      /* Because of recursion, we need to save the arguments in a vector
	 so we can create the PHI statement all at once.  Otherwise the
	 storage for the half-created PHI can be reclaimed.  */
      phi_args.safe_push (feeding_def);
    }

  /* Create the new phi basis.  */
  name = make_temp_ssa_name (TREE_TYPE (basis_name), NULL, "slsr");
  phi = create_phi_node (name, phi_bb);
  SSA_NAME_DEF_STMT (name) = phi;

  FOR_EACH_VEC_ELT (phi_args, i, phi_arg)
    {
      edge e = (*phi_bb->preds)[i];
      add_phi_arg (phi, phi_arg, e, loc);
    }

  update_stmt (phi);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fputs ("Introducing new phi basis: ", dump_file);
      print_gimple_stmt (dump_file, phi, 0, 0);
    }

  return name;
}

/* Given a candidate C whose basis is hidden by at least one intervening
   phi, introduce a matching number of new phis to represent its basis
   adjusted by conditional increments along possible incoming paths.  Then
   replace C as though it were an unconditional candidate, using the new
   basis.  */

static void
replace_conditional_candidate (slsr_cand_t c)
{
  tree basis_name, name;
  slsr_cand_t basis;
  location_t loc;
  double_int stride, bump;

  /* Look up the LHS SSA name from C's basis.  This will be the 
     RHS1 of the adds we will introduce to create new phi arguments.  */
  basis = lookup_cand (c->basis);
  basis_name = gimple_assign_lhs (basis->cand_stmt);

  /* Create a new phi statement which will represent C's true basis
     after the transformation is complete.  */
  loc = gimple_location (c->cand_stmt);
  name = create_phi_basis (c, lookup_cand (c->def_phi)->cand_stmt,
			   basis_name, loc, KNOWN_STRIDE);
  /* Replace C with an add of the new basis phi and a constant.  */
  stride = tree_to_double_int (c->stride);
  bump = c->index * stride;

  replace_mult_candidate (c, name, bump);
}

/* Compute the expected costs of inserting basis adjustments for
   candidate C with phi-definition PHI.  The cost of inserting 
   one adjustment is given by ONE_ADD_COST.  If PHI has arguments
   which are themselves phi results, recursively calculate costs
   for those phis as well.  */

static int
phi_add_costs (gimple phi, slsr_cand_t c, int one_add_cost)
{
  unsigned i;
  int cost = 0;
  slsr_cand_t phi_cand = base_cand_from_table (gimple_phi_result (phi));

  /* If we work our way back to a phi that isn't dominated by the hidden
     basis, this isn't a candidate for replacement.  Indicate this by
     returning an unreasonably high cost.  It's not easy to detect
     these situations when determining the basis, so we defer the
     decision until now.  */
  basic_block phi_bb = gimple_bb (phi);
  slsr_cand_t basis = lookup_cand (c->basis);
  basic_block basis_bb = gimple_bb (basis->cand_stmt);

  if (phi_bb == basis_bb || !dominated_by_p (CDI_DOMINATORS, phi_bb, basis_bb))
    return COST_INFINITE;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (arg != phi_cand->base_expr)
	{
	  gimple arg_def = SSA_NAME_DEF_STMT (arg);

	  if (gimple_code (arg_def) == GIMPLE_PHI)
	    cost += phi_add_costs (arg_def, c, one_add_cost);
	  else
	    {
	      slsr_cand_t arg_cand = base_cand_from_table (arg);

	      if (arg_cand->index != c->index)
		cost += one_add_cost;
	    }
	}
    }

  return cost;
}

/* For candidate C, each sibling of candidate C, and each dependent of
   candidate C, determine whether the candidate is dependent upon a 
   phi that hides its basis.  If not, replace the candidate unconditionally.
   Otherwise, determine whether the cost of introducing compensation code
   for the candidate is offset by the gains from strength reduction.  If
   so, replace the candidate and introduce the compensation code.  */

static void
replace_uncond_cands_and_profitable_phis (slsr_cand_t c)
{
  if (phi_dependent_cand_p (c))
    {
      if (c->kind == CAND_MULT)
	{
	  /* A candidate dependent upon a phi will replace a multiply by 
	     a constant with an add, and will insert at most one add for
	     each phi argument.  Add these costs with the potential dead-code
	     savings to determine profitability.  */
	  bool speed = optimize_bb_for_speed_p (gimple_bb (c->cand_stmt));
	  int mult_savings = stmt_cost (c->cand_stmt, speed);
	  gimple phi = lookup_cand (c->def_phi)->cand_stmt;
	  tree phi_result = gimple_phi_result (phi);
	  int one_add_cost = add_cost (speed, 
				       TYPE_MODE (TREE_TYPE (phi_result)));
	  int add_costs = one_add_cost + phi_add_costs (phi, c, one_add_cost);
	  int cost = add_costs - mult_savings - c->dead_savings;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  Conditional candidate %d:\n", c->cand_num);
	      fprintf (dump_file, "    add_costs = %d\n", add_costs);
	      fprintf (dump_file, "    mult_savings = %d\n", mult_savings);
	      fprintf (dump_file, "    dead_savings = %d\n", c->dead_savings);
	      fprintf (dump_file, "    cost = %d\n", cost);
	      if (cost <= COST_NEUTRAL)
		fputs ("  Replacing...\n", dump_file);
	      else
		fputs ("  Not replaced.\n", dump_file);
	    }

	  if (cost <= COST_NEUTRAL)
	    replace_conditional_candidate (c);
	}
    }
  else
    replace_unconditional_candidate (c);

  if (c->sibling)
    replace_uncond_cands_and_profitable_phis (lookup_cand (c->sibling));

  if (c->dependent)
    replace_uncond_cands_and_profitable_phis (lookup_cand (c->dependent));
}

/* Count the number of candidates in the tree rooted at C that have
   not already been replaced under other interpretations.  */

static int
count_candidates (slsr_cand_t c)
{
  unsigned count = cand_already_replaced (c) ? 0 : 1;

  if (c->sibling)
    count += count_candidates (lookup_cand (c->sibling));

  if (c->dependent)
    count += count_candidates (lookup_cand (c->dependent));

  return count;
}

/* Increase the count of INCREMENT by one in the increment vector.
   INCREMENT is associated with candidate C.  If INCREMENT is to be
   conditionally executed as part of a conditional candidate replacement,
   IS_PHI_ADJUST is true, otherwise false.  If an initializer
   T_0 = stride * I is provided by a candidate that dominates all
   candidates with the same increment, also record T_0 for subsequent use.  */

static void
record_increment (slsr_cand_t c, double_int increment, bool is_phi_adjust)
{
  bool found = false;
  unsigned i;

  /* Treat increments that differ only in sign as identical so as to
     share initializers, unless we are generating pointer arithmetic.  */
  if (!address_arithmetic_p && increment.is_negative ())
    increment = -increment;

  for (i = 0; i < incr_vec_len; i++)
    {
      if (incr_vec[i].incr == increment)
	{
	  incr_vec[i].count++;
	  found = true;

	  /* If we previously recorded an initializer that doesn't
	     dominate this candidate, it's not going to be useful to
	     us after all.  */
	  if (incr_vec[i].initializer
	      && !dominated_by_p (CDI_DOMINATORS,
				  gimple_bb (c->cand_stmt),
				  incr_vec[i].init_bb))
	    {
	      incr_vec[i].initializer = NULL_TREE;
	      incr_vec[i].init_bb = NULL;
	    }
	  
	  break;
	}
    }

  if (!found && incr_vec_len < MAX_INCR_VEC_LEN - 1)
    {
      /* The first time we see an increment, create the entry for it.
	 If this is the root candidate which doesn't have a basis, set
	 the count to zero.  We're only processing it so it can possibly
	 provide an initializer for other candidates.  */
      incr_vec[incr_vec_len].incr = increment;
      incr_vec[incr_vec_len].count = c->basis || is_phi_adjust ? 1 : 0;
      incr_vec[incr_vec_len].cost = COST_INFINITE;
      
      /* Optimistically record the first occurrence of this increment
	 as providing an initializer (if it does); we will revise this
	 opinion later if it doesn't dominate all other occurrences.
         Exception:  increments of -1, 0, 1 never need initializers;
	 and phi adjustments don't ever provide initializers.  */
      if (c->kind == CAND_ADD
	  && !is_phi_adjust
	  && c->index == increment
	  && (increment.sgt (double_int_one)
	      || increment.slt (double_int_minus_one))
	  && (gimple_assign_rhs_code (c->cand_stmt) == PLUS_EXPR
	      || gimple_assign_rhs_code (c->cand_stmt) == POINTER_PLUS_EXPR))
	{
	  tree t0 = NULL_TREE;
	  tree rhs1 = gimple_assign_rhs1 (c->cand_stmt);
	  tree rhs2 = gimple_assign_rhs2 (c->cand_stmt);
	  if (operand_equal_p (rhs1, c->base_expr, 0))
	    t0 = rhs2;
	  else if (operand_equal_p (rhs2, c->base_expr, 0))
	    t0 = rhs1;
	  if (t0
	      && SSA_NAME_DEF_STMT (t0)
	      && gimple_bb (SSA_NAME_DEF_STMT (t0)))
	    {
	      incr_vec[incr_vec_len].initializer = t0;
	      incr_vec[incr_vec_len++].init_bb
		= gimple_bb (SSA_NAME_DEF_STMT (t0));
	    }
	  else
	    {
	      incr_vec[incr_vec_len].initializer = NULL_TREE;
	      incr_vec[incr_vec_len++].init_bb = NULL;
	    }
	}
      else
	{
	  incr_vec[incr_vec_len].initializer = NULL_TREE;
	  incr_vec[incr_vec_len++].init_bb = NULL;
	}
    }
}

/* Given phi statement PHI that hides a candidate from its BASIS, find
   the increments along each incoming arc (recursively handling additional
   phis that may be present) and record them.  These increments are the
   difference in index between the index-adjusting statements and the
   index of the basis.  */

static void
record_phi_increments (slsr_cand_t basis, gimple phi)
{
  unsigned i;
  slsr_cand_t phi_cand = base_cand_from_table (gimple_phi_result (phi));
  
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (!operand_equal_p (arg, phi_cand->base_expr, 0))
	{
	  gimple arg_def = SSA_NAME_DEF_STMT (arg);

	  if (gimple_code (arg_def) == GIMPLE_PHI)
	    record_phi_increments (basis, arg_def);
	  else
	    {
	      slsr_cand_t arg_cand = base_cand_from_table (arg);
	      double_int diff = arg_cand->index - basis->index;
	      record_increment (arg_cand, diff, PHI_ADJUST);
	    }
	}
    }
}

/* Determine how many times each unique increment occurs in the set
   of candidates rooted at C's parent, recording the data in the
   increment vector.  For each unique increment I, if an initializer
   T_0 = stride * I is provided by a candidate that dominates all
   candidates with the same increment, also record T_0 for subsequent
   use.  */

static void
record_increments (slsr_cand_t c)
{
  if (!cand_already_replaced (c))
    {
      if (!phi_dependent_cand_p (c))
	record_increment (c, cand_increment (c), NOT_PHI_ADJUST);
      else
	{
	  /* A candidate with a basis hidden by a phi will have one
	     increment for its relationship to the index represented by
	     the phi, and potentially additional increments along each
	     incoming edge.  For the root of the dependency tree (which
	     has no basis), process just the initial index in case it has
	     an initializer that can be used by subsequent candidates.  */
	  record_increment (c, c->index, NOT_PHI_ADJUST);

	  if (c->basis)
	    record_phi_increments (lookup_cand (c->basis),
				   lookup_cand (c->def_phi)->cand_stmt);
	}
    }

  if (c->sibling)
    record_increments (lookup_cand (c->sibling));

  if (c->dependent)
    record_increments (lookup_cand (c->dependent));
}

/* Add up and return the costs of introducing add statements that
   require the increment INCR on behalf of candidate C and phi
   statement PHI.  Accumulate into *SAVINGS the potential savings
   from removing existing statements that feed PHI and have no other
   uses.  */

static int
phi_incr_cost (slsr_cand_t c, double_int incr, gimple phi, int *savings)
{
  unsigned i;
  int cost = 0;
  slsr_cand_t basis = lookup_cand (c->basis);
  slsr_cand_t phi_cand = base_cand_from_table (gimple_phi_result (phi));

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (!operand_equal_p (arg, phi_cand->base_expr, 0))
	{
	  gimple arg_def = SSA_NAME_DEF_STMT (arg);
      
	  if (gimple_code (arg_def) == GIMPLE_PHI)
	    {
	      int feeding_savings = 0;
	      cost += phi_incr_cost (c, incr, arg_def, &feeding_savings);
	      if (has_single_use (gimple_phi_result (arg_def)))
		*savings += feeding_savings;
	    }
	  else
	    {
	      slsr_cand_t arg_cand = base_cand_from_table (arg);
	      double_int diff = arg_cand->index - basis->index;

	      if (incr == diff)
		{
		  tree basis_lhs = gimple_assign_lhs (basis->cand_stmt);
		  tree lhs = gimple_assign_lhs (arg_cand->cand_stmt);
		  cost += add_cost (true, TYPE_MODE (TREE_TYPE (basis_lhs)));
		  if (has_single_use (lhs))
		    *savings += stmt_cost (arg_cand->cand_stmt, true);
		}
	    }
	}
    }

  return cost;
}

/* Return the first candidate in the tree rooted at C that has not
   already been replaced, favoring siblings over dependents.  */

static slsr_cand_t
unreplaced_cand_in_tree (slsr_cand_t c)
{
  if (!cand_already_replaced (c))
    return c;

  if (c->sibling)
    {
      slsr_cand_t sib = unreplaced_cand_in_tree (lookup_cand (c->sibling));
      if (sib)
	return sib;
    }

  if (c->dependent)
    {
      slsr_cand_t dep = unreplaced_cand_in_tree (lookup_cand (c->dependent));
      if (dep)
	return dep;
    }

  return NULL;
}

/* Return TRUE if the candidates in the tree rooted at C should be
   optimized for speed, else FALSE.  We estimate this based on the block
   containing the most dominant candidate in the tree that has not yet
   been replaced.  */

static bool
optimize_cands_for_speed_p (slsr_cand_t c)
{
  slsr_cand_t c2 = unreplaced_cand_in_tree (c);
  gcc_assert (c2);
  return optimize_bb_for_speed_p (gimple_bb (c2->cand_stmt));
}

/* Add COST_IN to the lowest cost of any dependent path starting at
   candidate C or any of its siblings, counting only candidates along
   such paths with increment INCR.  Assume that replacing a candidate
   reduces cost by REPL_SAVINGS.  Also account for savings from any
   statements that would go dead.  If COUNT_PHIS is true, include
   costs of introducing feeding statements for conditional candidates.  */

static int
lowest_cost_path (int cost_in, int repl_savings, slsr_cand_t c,
		  double_int incr, bool count_phis)
{
  int local_cost, sib_cost, savings = 0;
  double_int cand_incr = cand_abs_increment (c);

  if (cand_already_replaced (c))
    local_cost = cost_in;
  else if (incr == cand_incr)
    local_cost = cost_in - repl_savings - c->dead_savings;
  else
    local_cost = cost_in - c->dead_savings;

  if (count_phis
      && phi_dependent_cand_p (c)
      && !cand_already_replaced (c))
    {
      gimple phi = lookup_cand (c->def_phi)->cand_stmt;
      local_cost += phi_incr_cost (c, incr, phi, &savings);

      if (has_single_use (gimple_phi_result (phi)))
	local_cost -= savings;
    }

  if (c->dependent)
    local_cost = lowest_cost_path (local_cost, repl_savings, 
				   lookup_cand (c->dependent), incr,
				   count_phis);

  if (c->sibling)
    {
      sib_cost = lowest_cost_path (cost_in, repl_savings,
				   lookup_cand (c->sibling), incr,
				   count_phis);
      local_cost = MIN (local_cost, sib_cost);
    }

  return local_cost;
}

/* Compute the total savings that would accrue from all replacements
   in the candidate tree rooted at C, counting only candidates with
   increment INCR.  Assume that replacing a candidate reduces cost
   by REPL_SAVINGS.  Also account for savings from statements that
   would go dead.  */

static int
total_savings (int repl_savings, slsr_cand_t c, double_int incr,
	       bool count_phis)
{
  int savings = 0;
  double_int cand_incr = cand_abs_increment (c);

  if (incr == cand_incr && !cand_already_replaced (c))
    savings += repl_savings + c->dead_savings;

  if (count_phis
      && phi_dependent_cand_p (c)
      && !cand_already_replaced (c))
    {
      int phi_savings = 0;
      gimple phi = lookup_cand (c->def_phi)->cand_stmt;
      savings -= phi_incr_cost (c, incr, phi, &phi_savings);

      if (has_single_use (gimple_phi_result (phi)))
	savings += phi_savings;
    }

  if (c->dependent)
    savings += total_savings (repl_savings, lookup_cand (c->dependent), incr,
			      count_phis);

  if (c->sibling)
    savings += total_savings (repl_savings, lookup_cand (c->sibling), incr,
			      count_phis);

  return savings;
}

/* Use target-specific costs to determine and record which increments
   in the current candidate tree are profitable to replace, assuming
   MODE and SPEED.  FIRST_DEP is the first dependent of the root of
   the candidate tree.

   One slight limitation here is that we don't account for the possible
   introduction of casts in some cases.  See replace_one_candidate for
   the cases where these are introduced.  This should probably be cleaned
   up sometime.  */

static void
analyze_increments (slsr_cand_t first_dep, enum machine_mode mode, bool speed)
{
  unsigned i;

  for (i = 0; i < incr_vec_len; i++)
    {
      HOST_WIDE_INT incr = incr_vec[i].incr.to_shwi ();

      /* If somehow this increment is bigger than a HWI, we won't
	 be optimizing candidates that use it.  And if the increment
	 has a count of zero, nothing will be done with it.  */
      if (!incr_vec[i].incr.fits_shwi () || !incr_vec[i].count)
	incr_vec[i].cost = COST_INFINITE;

      /* Increments of 0, 1, and -1 are always profitable to replace,
	 because they always replace a multiply or add with an add or
	 copy, and may cause one or more existing instructions to go
	 dead.  Exception:  -1 can't be assumed to be profitable for
	 pointer addition.  */
      else if (incr == 0
	       || incr == 1
	       || (incr == -1
		   && (gimple_assign_rhs_code (first_dep->cand_stmt)
		       != POINTER_PLUS_EXPR)))
	incr_vec[i].cost = COST_NEUTRAL;
      
      /* FORNOW: If we need to add an initializer, give up if a cast from
	 the candidate's type to its stride's type can lose precision.
	 This could eventually be handled better by expressly retaining the
	 result of a cast to a wider type in the stride.  Example:

           short int _1;
	   _2 = (int) _1;
	   _3 = _2 * 10;
	   _4 = x + _3;    ADD: x + (10 * _1) : int
	   _5 = _2 * 15;
	   _6 = x + _3;    ADD: x + (15 * _1) : int

         Right now replacing _6 would cause insertion of an initializer
	 of the form "short int T = _1 * 5;" followed by a cast to 
	 int, which could overflow incorrectly.  Had we recorded _2 or
	 (int)_1 as the stride, this wouldn't happen.  However, doing
         this breaks other opportunities, so this will require some
	 care.  */
      else if (!incr_vec[i].initializer
	       && TREE_CODE (first_dep->stride) != INTEGER_CST
	       && !legal_cast_p_1 (first_dep->stride,
				   gimple_assign_lhs (first_dep->cand_stmt)))

	incr_vec[i].cost = COST_INFINITE;

      /* If we need to add an initializer, make sure we don't introduce
	 a multiply by a pointer type, which can happen in certain cast
	 scenarios.  FIXME: When cleaning up these cast issues, we can
         afford to introduce the multiply provided we cast out to an
         unsigned int of appropriate size.  */
      else if (!incr_vec[i].initializer
	       && TREE_CODE (first_dep->stride) != INTEGER_CST
	       && POINTER_TYPE_P (TREE_TYPE (first_dep->stride)))

	incr_vec[i].cost = COST_INFINITE;

      /* For any other increment, if this is a multiply candidate, we
	 must introduce a temporary T and initialize it with
	 T_0 = stride * increment.  When optimizing for speed, walk the
	 candidate tree to calculate the best cost reduction along any
	 path; if it offsets the fixed cost of inserting the initializer,
	 replacing the increment is profitable.  When optimizing for
         size, instead calculate the total cost reduction from replacing
	 all candidates with this increment.  */
      else if (first_dep->kind == CAND_MULT)
	{
	  int cost = mult_by_coeff_cost (incr, mode, speed);
	  int repl_savings = mul_cost (speed, mode) - add_cost (speed, mode);
	  if (speed)
	    cost = lowest_cost_path (cost, repl_savings, first_dep,
				     incr_vec[i].incr, COUNT_PHIS);
	  else
	    cost -= total_savings (repl_savings, first_dep, incr_vec[i].incr,
				   COUNT_PHIS);

	  incr_vec[i].cost = cost;
	}

      /* If this is an add candidate, the initializer may already
	 exist, so only calculate the cost of the initializer if it
	 doesn't.  We are replacing one add with another here, so the
	 known replacement savings is zero.  We will account for removal
	 of dead instructions in lowest_cost_path or total_savings.  */
      else
	{
	  int cost = 0;
	  if (!incr_vec[i].initializer)
	    cost = mult_by_coeff_cost (incr, mode, speed);

	  if (speed)
	    cost = lowest_cost_path (cost, 0, first_dep, incr_vec[i].incr,
				     DONT_COUNT_PHIS);
	  else
	    cost -= total_savings (0, first_dep, incr_vec[i].incr,
				   DONT_COUNT_PHIS);

	  incr_vec[i].cost = cost;
	}
    }
}

/* Return the nearest common dominator of BB1 and BB2.  If the blocks
   are identical, return the earlier of C1 and C2 in *WHERE.  Otherwise,
   if the NCD matches BB1, return C1 in *WHERE; if the NCD matches BB2,
   return C2 in *WHERE; and if the NCD matches neither, return NULL in
   *WHERE.  Note: It is possible for one of C1 and C2 to be NULL.  */

static basic_block
ncd_for_two_cands (basic_block bb1, basic_block bb2,
		   slsr_cand_t c1, slsr_cand_t c2, slsr_cand_t *where)
{
  basic_block ncd;

  if (!bb1)
    {
      *where = c2;
      return bb2;
    }

  if (!bb2)
    {
      *where = c1;
      return bb1;
    }

  ncd = nearest_common_dominator (CDI_DOMINATORS, bb1, bb2);
      
  /* If both candidates are in the same block, the earlier
     candidate wins.  */
  if (bb1 == ncd && bb2 == ncd)
    {
      if (!c1 || (c2 && c2->cand_num < c1->cand_num))
	*where = c2;
      else
	*where = c1;
    }

  /* Otherwise, if one of them produced a candidate in the
     dominator, that one wins.  */
  else if (bb1 == ncd)
    *where = c1;

  else if (bb2 == ncd)
    *where = c2;

  /* If neither matches the dominator, neither wins.  */
  else
    *where = NULL;

  return ncd;
}

/* Consider all candidates that feed PHI.  Find the nearest common
   dominator of those candidates requiring the given increment INCR.
   Further find and return the nearest common dominator of this result
   with block NCD.  If the returned block contains one or more of the
   candidates, return the earliest candidate in the block in *WHERE.  */

static basic_block
ncd_with_phi (slsr_cand_t c, double_int incr, gimple phi,
	      basic_block ncd, slsr_cand_t *where)
{
  unsigned i;
  slsr_cand_t basis = lookup_cand (c->basis);
  slsr_cand_t phi_cand = base_cand_from_table (gimple_phi_result (phi));

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (!operand_equal_p (arg, phi_cand->base_expr, 0))
	{
	  gimple arg_def = SSA_NAME_DEF_STMT (arg);

	  if (gimple_code (arg_def) == GIMPLE_PHI)
	    ncd = ncd_with_phi (c, incr, arg_def, ncd, where);
	  else 
	    {
	      slsr_cand_t arg_cand = base_cand_from_table (arg);
	      double_int diff = arg_cand->index - basis->index;

	      if ((incr == diff) || (!address_arithmetic_p && incr == -diff))
		ncd = ncd_for_two_cands (ncd, gimple_bb (arg_cand->cand_stmt),
					 *where, arg_cand, where);
	    }
	}
    }

  return ncd;
}

/* Consider the candidate C together with any candidates that feed
   C's phi dependence (if any).  Find and return the nearest common
   dominator of those candidates requiring the given increment INCR.
   If the returned block contains one or more of the candidates,
   return the earliest candidate in the block in *WHERE.  */

static basic_block
ncd_of_cand_and_phis (slsr_cand_t c, double_int incr, slsr_cand_t *where)
{
  basic_block ncd = NULL;

  if (cand_abs_increment (c) == incr)
    {
      ncd = gimple_bb (c->cand_stmt);
      *where = c;
    }
  
  if (phi_dependent_cand_p (c))
    ncd = ncd_with_phi (c, incr, lookup_cand (c->def_phi)->cand_stmt,
			ncd, where);

  return ncd;
}

/* Consider all candidates in the tree rooted at C for which INCR
   represents the required increment of C relative to its basis.
   Find and return the basic block that most nearly dominates all
   such candidates.  If the returned block contains one or more of
   the candidates, return the earliest candidate in the block in
   *WHERE.  */

static basic_block
nearest_common_dominator_for_cands (slsr_cand_t c, double_int incr,
				    slsr_cand_t *where)
{
  basic_block sib_ncd = NULL, dep_ncd = NULL, this_ncd = NULL, ncd;
  slsr_cand_t sib_where = NULL, dep_where = NULL, this_where = NULL, new_where;

  /* First find the NCD of all siblings and dependents.  */
  if (c->sibling)
    sib_ncd = nearest_common_dominator_for_cands (lookup_cand (c->sibling),
						  incr, &sib_where);
  if (c->dependent)
    dep_ncd = nearest_common_dominator_for_cands (lookup_cand (c->dependent),
						  incr, &dep_where);
  if (!sib_ncd && !dep_ncd)
    {
      new_where = NULL;
      ncd = NULL;
    }
  else if (sib_ncd && !dep_ncd)
    {
      new_where = sib_where;
      ncd = sib_ncd;
    }
  else if (dep_ncd && !sib_ncd)
    {
      new_where = dep_where;
      ncd = dep_ncd;
    }
  else
    ncd = ncd_for_two_cands (sib_ncd, dep_ncd, sib_where,
			     dep_where, &new_where);

  /* If the candidate's increment doesn't match the one we're interested
     in (and nor do any increments for feeding defs of a phi-dependence),
     then the result depends only on siblings and dependents.  */
  this_ncd = ncd_of_cand_and_phis (c, incr, &this_where);

  if (!this_ncd || cand_already_replaced (c))
    {
      *where = new_where;
      return ncd;
    }

  /* Otherwise, compare this candidate with the result from all siblings
     and dependents.  */
  ncd = ncd_for_two_cands (ncd, this_ncd, new_where, this_where, where);

  return ncd;
}

/* Return TRUE if the increment indexed by INDEX is profitable to replace.  */

static inline bool
profitable_increment_p (unsigned index)
{
  return (incr_vec[index].cost <= COST_NEUTRAL);
}

/* For each profitable increment in the increment vector not equal to
   0 or 1 (or -1, for non-pointer arithmetic), find the nearest common
   dominator of all statements in the candidate chain rooted at C
   that require that increment, and insert an initializer
   T_0 = stride * increment at that location.  Record T_0 with the
   increment record.  */

static void
insert_initializers (slsr_cand_t c)
{
  unsigned i;

  for (i = 0; i < incr_vec_len; i++)
    {
      basic_block bb;
      slsr_cand_t where = NULL;
      gimple init_stmt;
      tree stride_type, new_name, incr_tree;
      double_int incr = incr_vec[i].incr;

      if (!profitable_increment_p (i)
	  || incr.is_one ()
	  || (incr.is_minus_one ()
	      && gimple_assign_rhs_code (c->cand_stmt) != POINTER_PLUS_EXPR)
	  || incr.is_zero ())
	continue;

      /* We may have already identified an existing initializer that
	 will suffice.  */
      if (incr_vec[i].initializer)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fputs ("Using existing initializer: ", dump_file);
	      print_gimple_stmt (dump_file,
				 SSA_NAME_DEF_STMT (incr_vec[i].initializer),
				 0, 0);
	    }
	  continue;
	}

      /* Find the block that most closely dominates all candidates
	 with this increment.  If there is at least one candidate in
	 that block, the earliest one will be returned in WHERE.  */
      bb = nearest_common_dominator_for_cands (c, incr, &where);

      /* Create a new SSA name to hold the initializer's value.  */
      stride_type = TREE_TYPE (c->stride);
      new_name = make_temp_ssa_name (stride_type, NULL, "slsr");
      incr_vec[i].initializer = new_name;

      /* Create the initializer and insert it in the latest possible
	 dominating position.  */
      incr_tree = double_int_to_tree (stride_type, incr);
      init_stmt = gimple_build_assign_with_ops (MULT_EXPR, new_name,
						c->stride, incr_tree);
      if (where)
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (where->cand_stmt);
	  gsi_insert_before (&gsi, init_stmt, GSI_SAME_STMT);
	  gimple_set_location (init_stmt, gimple_location (where->cand_stmt));
	}
      else
	{
	  gimple_stmt_iterator gsi = gsi_last_bb (bb);
	  gimple basis_stmt = lookup_cand (c->basis)->cand_stmt;

	  if (!gsi_end_p (gsi) && is_ctrl_stmt (gsi_stmt (gsi)))
	    gsi_insert_before (&gsi, init_stmt, GSI_SAME_STMT);
	  else
	    gsi_insert_after (&gsi, init_stmt, GSI_SAME_STMT);

	  gimple_set_location (init_stmt, gimple_location (basis_stmt));
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fputs ("Inserting initializer: ", dump_file);
	  print_gimple_stmt (dump_file, init_stmt, 0, 0);
	}
    }
}

/* Return TRUE iff all required increments for candidates feeding PHI
   are profitable to replace on behalf of candidate C.  */

static bool
all_phi_incrs_profitable (slsr_cand_t c, gimple phi)
{
  unsigned i;
  slsr_cand_t basis = lookup_cand (c->basis);
  slsr_cand_t phi_cand = base_cand_from_table (gimple_phi_result (phi));

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (!operand_equal_p (arg, phi_cand->base_expr, 0))
	{
	  gimple arg_def = SSA_NAME_DEF_STMT (arg);

	  if (gimple_code (arg_def) == GIMPLE_PHI)
	    {
	      if (!all_phi_incrs_profitable (c, arg_def))
		return false;
	    }
	  else
	    {
	      int j;
	      slsr_cand_t arg_cand = base_cand_from_table (arg);
	      double_int increment = arg_cand->index - basis->index;

	      if (!address_arithmetic_p && increment.is_negative ())
		increment = -increment;

	      j = incr_vec_index (increment);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "  Conditional candidate %d, phi: ",
			   c->cand_num);
		  print_gimple_stmt (dump_file, phi, 0, 0);
		  fputs ("    increment: ", dump_file);
		  dump_double_int (dump_file, increment, false);
		  if (j < 0)
		    fprintf (dump_file,
			     "\n  Not replaced; incr_vec overflow.\n");
		  else {
		    fprintf (dump_file, "\n    cost: %d\n", incr_vec[j].cost);
		    if (profitable_increment_p (j))
		      fputs ("  Replacing...\n", dump_file);
		    else
		      fputs ("  Not replaced.\n", dump_file);
		  }
		}

	      if (j < 0 || !profitable_increment_p (j))
		return false;
	    }
	}
    }

  return true;
}
  
/* Create a NOP_EXPR that copies FROM_EXPR into a new SSA name of
   type TO_TYPE, and insert it in front of the statement represented
   by candidate C.  Use *NEW_VAR to create the new SSA name.  Return
   the new SSA name.  */

static tree
introduce_cast_before_cand (slsr_cand_t c, tree to_type, tree from_expr)
{
  tree cast_lhs;
  gimple cast_stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (c->cand_stmt);

  cast_lhs = make_temp_ssa_name (to_type, NULL, "slsr");
  cast_stmt = gimple_build_assign_with_ops (NOP_EXPR, cast_lhs,
					    from_expr, NULL_TREE);
  gimple_set_location (cast_stmt, gimple_location (c->cand_stmt));
  gsi_insert_before (&gsi, cast_stmt, GSI_SAME_STMT);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fputs ("  Inserting: ", dump_file);
      print_gimple_stmt (dump_file, cast_stmt, 0, 0);
    }

  return cast_lhs;
}

/* Replace the RHS of the statement represented by candidate C with 
   NEW_CODE, NEW_RHS1, and NEW_RHS2, provided that to do so doesn't
   leave C unchanged or just interchange its operands.  The original
   operation and operands are in OLD_CODE, OLD_RHS1, and OLD_RHS2.
   If the replacement was made and we are doing a details dump,
   return the revised statement, else NULL.  */

static gimple
replace_rhs_if_not_dup (enum tree_code new_code, tree new_rhs1, tree new_rhs2,
			enum tree_code old_code, tree old_rhs1, tree old_rhs2,
			slsr_cand_t c)
{
  if (new_code != old_code
      || ((!operand_equal_p (new_rhs1, old_rhs1, 0)
	   || !operand_equal_p (new_rhs2, old_rhs2, 0))
	  && (!operand_equal_p (new_rhs1, old_rhs2, 0)
	      || !operand_equal_p (new_rhs2, old_rhs1, 0))))
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (c->cand_stmt);
      gimple_assign_set_rhs_with_ops (&gsi, new_code, new_rhs1, new_rhs2);
      update_stmt (gsi_stmt (gsi));
      c->cand_stmt = gsi_stmt (gsi);

      if (dump_file && (dump_flags & TDF_DETAILS))
	return gsi_stmt (gsi);
    }

  else if (dump_file && (dump_flags & TDF_DETAILS))
    fputs ("  (duplicate, not actually replacing)\n", dump_file);

  return NULL;
}

/* Strength-reduce the statement represented by candidate C by replacing
   it with an equivalent addition or subtraction.  I is the index into
   the increment vector identifying C's increment.  NEW_VAR is used to
   create a new SSA name if a cast needs to be introduced.  BASIS_NAME
   is the rhs1 to use in creating the add/subtract.  */

static void
replace_one_candidate (slsr_cand_t c, unsigned i, tree basis_name)
{
  gimple stmt_to_print = NULL;
  tree orig_rhs1, orig_rhs2;
  tree rhs2;
  enum tree_code orig_code, repl_code;
  double_int cand_incr;

  orig_code = gimple_assign_rhs_code (c->cand_stmt);
  orig_rhs1 = gimple_assign_rhs1 (c->cand_stmt);
  orig_rhs2 = gimple_assign_rhs2 (c->cand_stmt);
  cand_incr = cand_increment (c);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fputs ("Replacing: ", dump_file);
      print_gimple_stmt (dump_file, c->cand_stmt, 0, 0);
      stmt_to_print = c->cand_stmt;
    }

  if (address_arithmetic_p)
    repl_code = POINTER_PLUS_EXPR;
  else
    repl_code = PLUS_EXPR;

  /* If the increment has an initializer T_0, replace the candidate
     statement with an add of the basis name and the initializer.  */
  if (incr_vec[i].initializer)
    {
      tree init_type = TREE_TYPE (incr_vec[i].initializer);
      tree orig_type = TREE_TYPE (orig_rhs2);

      if (types_compatible_p (orig_type, init_type))
	rhs2 = incr_vec[i].initializer;
      else
	rhs2 = introduce_cast_before_cand (c, orig_type,
					   incr_vec[i].initializer);

      if (incr_vec[i].incr != cand_incr)
	{
	  gcc_assert (repl_code == PLUS_EXPR);
	  repl_code = MINUS_EXPR;
	}

      stmt_to_print = replace_rhs_if_not_dup (repl_code, basis_name, rhs2,
					      orig_code, orig_rhs1, orig_rhs2,
					      c);
    }

  /* Otherwise, the increment is one of -1, 0, and 1.  Replace
     with a subtract of the stride from the basis name, a copy
     from the basis name, or an add of the stride to the basis
     name, respectively.  It may be necessary to introduce a
     cast (or reuse an existing cast).  */
  else if (cand_incr.is_one ())
    {
      tree stride_type = TREE_TYPE (c->stride);
      tree orig_type = TREE_TYPE (orig_rhs2);
      
      if (types_compatible_p (orig_type, stride_type))
	rhs2 = c->stride;
      else
	rhs2 = introduce_cast_before_cand (c, orig_type, c->stride);
      
      stmt_to_print = replace_rhs_if_not_dup (repl_code, basis_name, rhs2,
					      orig_code, orig_rhs1, orig_rhs2,
					      c);
    }

  else if (cand_incr.is_minus_one ())
    {
      tree stride_type = TREE_TYPE (c->stride);
      tree orig_type = TREE_TYPE (orig_rhs2);
      gcc_assert (repl_code != POINTER_PLUS_EXPR);
      
      if (types_compatible_p (orig_type, stride_type))
	rhs2 = c->stride;
      else
	rhs2 = introduce_cast_before_cand (c, orig_type, c->stride);
      
      if (orig_code != MINUS_EXPR
	  || !operand_equal_p (basis_name, orig_rhs1, 0)
	  || !operand_equal_p (rhs2, orig_rhs2, 0))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (c->cand_stmt);
	  gimple_assign_set_rhs_with_ops (&gsi, MINUS_EXPR, basis_name, rhs2);
	  update_stmt (gsi_stmt (gsi));
          c->cand_stmt = gsi_stmt (gsi);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    stmt_to_print = gsi_stmt (gsi);
	}
      else if (dump_file && (dump_flags & TDF_DETAILS))
	fputs ("  (duplicate, not actually replacing)\n", dump_file);
    }

  else if (cand_incr.is_zero ())
    {
      tree lhs = gimple_assign_lhs (c->cand_stmt);
      tree lhs_type = TREE_TYPE (lhs);
      tree basis_type = TREE_TYPE (basis_name);
      
      if (types_compatible_p (lhs_type, basis_type))
	{
	  gimple copy_stmt = gimple_build_assign (lhs, basis_name);
	  gimple_stmt_iterator gsi = gsi_for_stmt (c->cand_stmt);
	  gimple_set_location (copy_stmt, gimple_location (c->cand_stmt));
	  gsi_replace (&gsi, copy_stmt, false);
	  c->cand_stmt = copy_stmt;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    stmt_to_print = copy_stmt;
	}
      else
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (c->cand_stmt);
	  gimple cast_stmt = gimple_build_assign_with_ops (NOP_EXPR, lhs,
							   basis_name,
							   NULL_TREE);
	  gimple_set_location (cast_stmt, gimple_location (c->cand_stmt));
	  gsi_replace (&gsi, cast_stmt, false);
	  c->cand_stmt = cast_stmt;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    stmt_to_print = cast_stmt;
	}
    }
  else
    gcc_unreachable ();
  
  if (dump_file && (dump_flags & TDF_DETAILS) && stmt_to_print)
    {
      fputs ("With: ", dump_file);
      print_gimple_stmt (dump_file, stmt_to_print, 0, 0);
      fputs ("\n", dump_file);
    }
}

/* For each candidate in the tree rooted at C, replace it with
   an increment if such has been shown to be profitable.  */

static void
replace_profitable_candidates (slsr_cand_t c)
{
  if (!cand_already_replaced (c))
    {
      double_int increment = cand_abs_increment (c);
      enum tree_code orig_code = gimple_assign_rhs_code (c->cand_stmt);
      int i;

      i = incr_vec_index (increment);

      /* Only process profitable increments.  Nothing useful can be done
	 to a cast or copy.  */
      if (i >= 0
	  && profitable_increment_p (i) 
	  && orig_code != MODIFY_EXPR
	  && orig_code != NOP_EXPR)
	{
	  if (phi_dependent_cand_p (c))
	    {
	      gimple phi = lookup_cand (c->def_phi)->cand_stmt;

	      if (all_phi_incrs_profitable (c, phi))
		{
		  /* Look up the LHS SSA name from C's basis.  This will be 
		     the RHS1 of the adds we will introduce to create new
		     phi arguments.  */
		  slsr_cand_t basis = lookup_cand (c->basis);
		  tree basis_name = gimple_assign_lhs (basis->cand_stmt);

		  /* Create a new phi statement that will represent C's true
		     basis after the transformation is complete.  */
		  location_t loc = gimple_location (c->cand_stmt);
		  tree name = create_phi_basis (c, phi, basis_name,
						loc, UNKNOWN_STRIDE);

		  /* Replace C with an add of the new basis phi and the
		     increment.  */
		  replace_one_candidate (c, i, name);
		}
	    }
	  else
	    {
	      slsr_cand_t basis = lookup_cand (c->basis);
	      tree basis_name = gimple_assign_lhs (basis->cand_stmt);
	      replace_one_candidate (c, i, basis_name);
	    }
	}
    }

  if (c->sibling)
    replace_profitable_candidates (lookup_cand (c->sibling));

  if (c->dependent)
    replace_profitable_candidates (lookup_cand (c->dependent));
}

/* Analyze costs of related candidates in the candidate vector,
   and make beneficial replacements.  */

static void
analyze_candidates_and_replace (void)
{
  unsigned i;
  slsr_cand_t c;

  /* Each candidate that has a null basis and a non-null
     dependent is the root of a tree of related statements.
     Analyze each tree to determine a subset of those
     statements that can be replaced with maximum benefit.  */
  FOR_EACH_VEC_ELT (cand_vec, i, c)
    {
      slsr_cand_t first_dep;

      if (c->basis != 0 || c->dependent == 0)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\nProcessing dependency tree rooted at %d.\n",
		 c->cand_num);

      first_dep = lookup_cand (c->dependent);

      /* If this is a chain of CAND_REFs, unconditionally replace
	 each of them with a strength-reduced data reference.  */
      if (c->kind == CAND_REF)
	replace_refs (c);

      /* If the common stride of all related candidates is a known
	 constant, each candidate without a phi-dependence can be
	 profitably replaced.  Each replaces a multiply by a single
	 add, with the possibility that a feeding add also goes dead.
	 A candidate with a phi-dependence is replaced only if the
	 compensation code it requires is offset by the strength
	 reduction savings.  */
      else if (TREE_CODE (c->stride) == INTEGER_CST)
	replace_uncond_cands_and_profitable_phis (first_dep);

      /* When the stride is an SSA name, it may still be profitable
	 to replace some or all of the dependent candidates, depending
	 on whether the introduced increments can be reused, or are
	 less expensive to calculate than the replaced statements.  */
      else
	{
	  enum machine_mode mode;
	  bool speed;

	  /* Determine whether we'll be generating pointer arithmetic
	     when replacing candidates.  */
	  address_arithmetic_p = (c->kind == CAND_ADD
				  && POINTER_TYPE_P (c->cand_type));

	  /* If all candidates have already been replaced under other
	     interpretations, nothing remains to be done.  */
	  if (!count_candidates (c))
	    continue;

	  /* Construct an array of increments for this candidate chain.  */
	  incr_vec = XNEWVEC (incr_info, MAX_INCR_VEC_LEN);
	  incr_vec_len = 0;
	  record_increments (c);

	  /* Determine which increments are profitable to replace.  */
	  mode = TYPE_MODE (TREE_TYPE (gimple_assign_lhs (c->cand_stmt)));
	  speed = optimize_cands_for_speed_p (c);
	  analyze_increments (first_dep, mode, speed);

	  /* Insert initializers of the form T_0 = stride * increment
	     for use in profitable replacements.  */
	  insert_initializers (first_dep);
	  dump_incr_vec ();

	  /* Perform the replacements.  */
	  replace_profitable_candidates (first_dep);
	  free (incr_vec);
	}
    }
}

static unsigned
execute_strength_reduction (void)
{
  /* Create the obstack where candidates will reside.  */
  gcc_obstack_init (&cand_obstack);

  /* Allocate the candidate vector.  */
  cand_vec.create (128);

  /* Allocate the mapping from statements to candidate indices.  */
  stmt_cand_map = pointer_map_create ();

  /* Create the obstack where candidate chains will reside.  */
  gcc_obstack_init (&chain_obstack);

  /* Allocate the mapping from base expressions to candidate chains.  */
  base_cand_map.create (500);

  /* Initialize the loop optimizer.  We need to detect flow across
     back edges, and this gives us dominator information as well.  */
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  /* Walk the CFG in predominator order looking for strength reduction
     candidates.  */
  find_candidates_dom_walker (CDI_DOMINATORS)
    .walk (cfun->cfg->x_entry_block_ptr);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_cand_vec ();
      dump_cand_chains ();
    }

  /* Analyze costs and make appropriate replacements.  */
  analyze_candidates_and_replace ();

  loop_optimizer_finalize ();
  base_cand_map.dispose ();
  obstack_free (&chain_obstack, NULL);
  pointer_map_destroy (stmt_cand_map);
  cand_vec.release ();
  obstack_free (&cand_obstack, NULL);

  return 0;
}

static bool
gate_strength_reduction (void)
{
  return flag_tree_slsr;
}

namespace {

const pass_data pass_data_strength_reduction =
{
  GIMPLE_PASS, /* type */
  "slsr", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_GIMPLE_SLSR, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_ssa, /* todo_flags_finish */
};

class pass_strength_reduction : public gimple_opt_pass
{
public:
  pass_strength_reduction (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_strength_reduction, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_strength_reduction (); }
  unsigned int execute () { return execute_strength_reduction (); }

}; // class pass_strength_reduction

} // anon namespace

gimple_opt_pass *
make_pass_strength_reduction (gcc::context *ctxt)
{
  return new pass_strength_reduction (ctxt);
}
