/* Scalar evolution detector.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <s.pop@laposte.net>

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

/*
   Description:

   This pass analyzes the evolution of scalar variables in loop
   structures.  The algorithm is based on the SSA representation,
   and on the loop hierarchy tree.  This algorithm is not based on
   the notion of versions of a variable, as it was the case for the
   previous implementations of the scalar evolution algorithm, but
   it assumes that each defined name is unique.

   The notation used in this file is called "chains of recurrences",
   and has been proposed by Eugene Zima, Robert Van Engelen, and
   others for describing induction variables in programs.  For example
   "b -> {0, +, 2}_1" means that the scalar variable "b" is equal to 0
   when entering in the loop_1 and has a step 2 in this loop, in other
   words "for (b = 0; b < N; b+=2);".  Note that the coefficients of
   this chain of recurrence (or chrec [shrek]) can contain the name of
   other variables, in which case they are called parametric chrecs.
   For example, "b -> {a, +, 2}_1" means that the initial value of "b"
   is the value of "a".  In most of the cases these parametric chrecs
   are fully instantiated before their use because symbolic names can
   hide some difficult cases such as self-references described later
   (see the Fibonacci example).

   A short sketch of the algorithm is:

   Given a scalar variable to be analyzed, follow the SSA edge to
   its definition:

   - When the definition is a GIMPLE_ASSIGN: if the right hand side
   (RHS) of the definition cannot be statically analyzed, the answer
   of the analyzer is: "don't know".
   Otherwise, for all the variables that are not yet analyzed in the
   RHS, try to determine their evolution, and finally try to
   evaluate the operation of the RHS that gives the evolution
   function of the analyzed variable.

   - When the definition is a condition-phi-node: determine the
   evolution function for all the branches of the phi node, and
   finally merge these evolutions (see chrec_merge).

   - When the definition is a loop-phi-node: determine its initial
   condition, that is the SSA edge defined in an outer loop, and
   keep it symbolic.  Then determine the SSA edges that are defined
   in the body of the loop.  Follow the inner edges until ending on
   another loop-phi-node of the same analyzed loop.  If the reached
   loop-phi-node is not the starting loop-phi-node, then we keep
   this definition under a symbolic form.  If the reached
   loop-phi-node is the same as the starting one, then we compute a
   symbolic stride on the return path.  The result is then the
   symbolic chrec {initial_condition, +, symbolic_stride}_loop.

   Examples:

   Example 1: Illustration of the basic algorithm.

   | a = 3
   | loop_1
   |   b = phi (a, c)
   |   c = b + 1
   |   if (c > 10) exit_loop
   | endloop

   Suppose that we want to know the number of iterations of the
   loop_1.  The exit_loop is controlled by a COND_EXPR (c > 10).  We
   ask the scalar evolution analyzer two questions: what's the
   scalar evolution (scev) of "c", and what's the scev of "10".  For
   "10" the answer is "10" since it is a scalar constant.  For the
   scalar variable "c", it follows the SSA edge to its definition,
   "c = b + 1", and then asks again what's the scev of "b".
   Following the SSA edge, we end on a loop-phi-node "b = phi (a,
   c)", where the initial condition is "a", and the inner loop edge
   is "c".  The initial condition is kept under a symbolic form (it
   may be the case that the copy constant propagation has done its
   work and we end with the constant "3" as one of the edges of the
   loop-phi-node).  The update edge is followed to the end of the
   loop, and until reaching again the starting loop-phi-node: b -> c
   -> b.  At this point we have drawn a path from "b" to "b" from
   which we compute the stride in the loop: in this example it is
   "+1".  The resulting scev for "b" is "b -> {a, +, 1}_1".  Now
   that the scev for "b" is known, it is possible to compute the
   scev for "c", that is "c -> {a + 1, +, 1}_1".  In order to
   determine the number of iterations in the loop_1, we have to
   instantiate_parameters (loop_1, {a + 1, +, 1}_1), that gives after some
   more analysis the scev {4, +, 1}_1, or in other words, this is
   the function "f (x) = x + 4", where x is the iteration count of
   the loop_1.  Now we have to solve the inequality "x + 4 > 10",
   and take the smallest iteration number for which the loop is
   exited: x = 7.  This loop runs from x = 0 to x = 7, and in total
   there are 8 iterations.  In terms of loop normalization, we have
   created a variable that is implicitly defined, "x" or just "_1",
   and all the other analyzed scalars of the loop are defined in
   function of this variable:

   a -> 3
   b -> {3, +, 1}_1
   c -> {4, +, 1}_1

   or in terms of a C program:

   | a = 3
   | for (x = 0; x <= 7; x++)
   |   {
   |     b = x + 3
   |     c = x + 4
   |   }

   Example 2a: Illustration of the algorithm on nested loops.

   | loop_1
   |   a = phi (1, b)
   |   c = a + 2
   |   loop_2  10 times
   |     b = phi (c, d)
   |     d = b + 3
   |   endloop
   | endloop

   For analyzing the scalar evolution of "a", the algorithm follows
   the SSA edge into the loop's body: "a -> b".  "b" is an inner
   loop-phi-node, and its analysis as in Example 1, gives:

   b -> {c, +, 3}_2
   d -> {c + 3, +, 3}_2

   Following the SSA edge for the initial condition, we end on "c = a
   + 2", and then on the starting loop-phi-node "a".  From this point,
   the loop stride is computed: back on "c = a + 2" we get a "+2" in
   the loop_1, then on the loop-phi-node "b" we compute the overall
   effect of the inner loop that is "b = c + 30", and we get a "+30"
   in the loop_1.  That means that the overall stride in loop_1 is
   equal to "+32", and the result is:

   a -> {1, +, 32}_1
   c -> {3, +, 32}_1

   Example 2b: Multivariate chains of recurrences.

   | loop_1
   |   k = phi (0, k + 1)
   |   loop_2  4 times
   |     j = phi (0, j + 1)
   |     loop_3 4 times
   |       i = phi (0, i + 1)
   |       A[j + k] = ...
   |     endloop
   |   endloop
   | endloop

   Analyzing the access function of array A with
   instantiate_parameters (loop_1, "j + k"), we obtain the
   instantiation and the analysis of the scalar variables "j" and "k"
   in loop_1.  This leads to the scalar evolution {4, +, 1}_1: the end
   value of loop_2 for "j" is 4, and the evolution of "k" in loop_1 is
   {0, +, 1}_1.  To obtain the evolution function in loop_3 and
   instantiate the scalar variables up to loop_1, one has to use:
   instantiate_scev (block_before_loop (loop_1), loop_3, "j + k").
   The result of this call is {{0, +, 1}_1, +, 1}_2.

   Example 3: Higher degree polynomials.

   | loop_1
   |   a = phi (2, b)
   |   c = phi (5, d)
   |   b = a + 1
   |   d = c + a
   | endloop

   a -> {2, +, 1}_1
   b -> {3, +, 1}_1
   c -> {5, +, a}_1
   d -> {5 + a, +, a}_1

   instantiate_parameters (loop_1, {5, +, a}_1) -> {5, +, 2, +, 1}_1
   instantiate_parameters (loop_1, {5 + a, +, a}_1) -> {7, +, 3, +, 1}_1

   Example 4: Lucas, Fibonacci, or mixers in general.

   | loop_1
   |   a = phi (1, b)
   |   c = phi (3, d)
   |   b = c
   |   d = c + a
   | endloop

   a -> (1, c)_1
   c -> {3, +, a}_1

   The syntax "(1, c)_1" stands for a PEELED_CHREC that has the
   following semantics: during the first iteration of the loop_1, the
   variable contains the value 1, and then it contains the value "c".
   Note that this syntax is close to the syntax of the loop-phi-node:
   "a -> (1, c)_1" vs. "a = phi (1, c)".

   The symbolic chrec representation contains all the semantics of the
   original code.  What is more difficult is to use this information.

   Example 5: Flip-flops, or exchangers.

   | loop_1
   |   a = phi (1, b)
   |   c = phi (3, d)
   |   b = c
   |   d = a
   | endloop

   a -> (1, c)_1
   c -> (3, a)_1

   Based on these symbolic chrecs, it is possible to refine this
   information into the more precise PERIODIC_CHRECs:

   a -> |1, 3|_1
   c -> |3, 1|_1

   This transformation is not yet implemented.

   Further readings:

   You can find a more detailed description of the algorithm in:
   http://icps.u-strasbg.fr/~pop/DEA_03_Pop.pdf
   http://icps.u-strasbg.fr/~pop/DEA_03_Pop.ps.gz.  But note that
   this is a preliminary report and some of the details of the
   algorithm have changed.  I'm working on a research report that
   updates the description of the algorithms to reflect the design
   choices used in this implementation.

   A set of slides show a high level overview of the algorithm and run
   an example through the scalar evolution analyzer:
   http://cri.ensmp.fr/~pop/gcc/mar04/slides.pdf

   The slides that I have presented at the GCC Summit'04 are available
   at: http://cri.ensmp.fr/~pop/gcc/20040604/gccsummit-lno-spop.pdf
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "optabs-query.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-affine.h"
#include "tree-scalar-evolution.h"
#include "dumpfile.h"
#include "tree-ssa-propagate.h"
#include "gimple-fold.h"
#include "tree-into-ssa.h"
#include "builtins.h"
#include "case-cfn-macros.h"
#include "tree-eh.h"

static tree analyze_scalar_evolution_1 (class loop *, tree);
static tree analyze_scalar_evolution_for_address_of (class loop *loop,
						     tree var);

/* The cached information about an SSA name with version NAME_VERSION,
   claiming that below basic block with index INSTANTIATED_BELOW, the
   value of the SSA name can be expressed as CHREC.  */

struct GTY((for_user)) scev_info_str {
  unsigned int name_version;
  int instantiated_below;
  tree chrec;
};

/* Counters for the scev database.  */
static unsigned nb_set_scev = 0;
static unsigned nb_get_scev = 0;

struct scev_info_hasher : ggc_ptr_hash<scev_info_str>
{
  static hashval_t hash (scev_info_str *i);
  static bool equal (const scev_info_str *a, const scev_info_str *b);
};

static GTY (()) hash_table<scev_info_hasher> *scalar_evolution_info;


/* Constructs a new SCEV_INFO_STR structure for VAR and INSTANTIATED_BELOW.  */

static inline struct scev_info_str *
new_scev_info_str (basic_block instantiated_below, tree var)
{
  struct scev_info_str *res;

  res = ggc_alloc<scev_info_str> ();
  res->name_version = SSA_NAME_VERSION (var);
  res->chrec = chrec_not_analyzed_yet;
  res->instantiated_below = instantiated_below->index;

  return res;
}

/* Computes a hash function for database element ELT.  */

hashval_t
scev_info_hasher::hash (scev_info_str *elt)
{
  return elt->name_version ^ elt->instantiated_below;
}

/* Compares database elements E1 and E2.  */

bool
scev_info_hasher::equal (const scev_info_str *elt1, const scev_info_str *elt2)
{
  return (elt1->name_version == elt2->name_version
	  && elt1->instantiated_below == elt2->instantiated_below);
}

/* Get the scalar evolution of VAR for INSTANTIATED_BELOW basic block.
   A first query on VAR returns chrec_not_analyzed_yet.  */

static tree *
find_var_scev_info (basic_block instantiated_below, tree var)
{
  struct scev_info_str *res;
  struct scev_info_str tmp;

  tmp.name_version = SSA_NAME_VERSION (var);
  tmp.instantiated_below = instantiated_below->index;
  scev_info_str **slot = scalar_evolution_info->find_slot (&tmp, INSERT);

  if (!*slot)
    *slot = new_scev_info_str (instantiated_below, var);
  res = *slot;

  return &res->chrec;
}


/* Hashtable helpers for a temporary hash-table used when
   analyzing a scalar evolution, instantiating a CHREC or
   resolving mixers.  */

class instantiate_cache_type
{
public:
  htab_t map;
  vec<scev_info_str> entries;

  instantiate_cache_type () : map (NULL), entries (vNULL) {}
  ~instantiate_cache_type ();
  tree get (unsigned slot) { return entries[slot].chrec; }
  void set (unsigned slot, tree chrec) { entries[slot].chrec = chrec; }
};

instantiate_cache_type::~instantiate_cache_type ()
{
  if (map != NULL)
    {
      htab_delete (map);
      entries.release ();
    }
}

/* Cache to avoid infinite recursion when instantiating an SSA name.
   Live during the outermost analyze_scalar_evolution, instantiate_scev
   or resolve_mixers call.  */
static instantiate_cache_type *global_cache;


/* Return true when PHI is a loop-phi-node.  */

static bool
loop_phi_node_p (gimple *phi)
{
  /* The implementation of this function is based on the following
     property: "all the loop-phi-nodes of a loop are contained in the
     loop's header basic block".  */

  return loop_containing_stmt (phi)->header == gimple_bb (phi);
}

/* Compute the scalar evolution for EVOLUTION_FN after crossing LOOP.
   In general, in the case of multivariate evolutions we want to get
   the evolution in different loops.  LOOP specifies the level for
   which to get the evolution.

   Example:

   | for (j = 0; j < 100; j++)
   |   {
   |     for (k = 0; k < 100; k++)
   |       {
   |         i = k + j;   - Here the value of i is a function of j, k.
   |       }
   |      ... = i         - Here the value of i is a function of j.
   |   }
   | ... = i              - Here the value of i is a scalar.

   Example:

   | i_0 = ...
   | loop_1 10 times
   |   i_1 = phi (i_0, i_2)
   |   i_2 = i_1 + 2
   | endloop

   This loop has the same effect as:
   LOOP_1 has the same effect as:

   | i_1 = i_0 + 20

   The overall effect of the loop, "i_0 + 20" in the previous example,
   is obtained by passing in the parameters: LOOP = 1,
   EVOLUTION_FN = {i_0, +, 2}_1.
*/

tree
compute_overall_effect_of_inner_loop (class loop *loop, tree evolution_fn)
{
  bool val = false;

  if (evolution_fn == chrec_dont_know)
    return chrec_dont_know;

  else if (TREE_CODE (evolution_fn) == POLYNOMIAL_CHREC)
    {
      class loop *inner_loop = get_chrec_loop (evolution_fn);

      if (inner_loop == loop
	  || flow_loop_nested_p (loop, inner_loop))
	{
	  tree nb_iter = number_of_latch_executions (inner_loop);

	  if (nb_iter == chrec_dont_know)
	    return chrec_dont_know;
	  else
	    {
	      tree res;

	      /* evolution_fn is the evolution function in LOOP.  Get
		 its value in the nb_iter-th iteration.  */
	      res = chrec_apply (inner_loop->num, evolution_fn, nb_iter);

	      if (chrec_contains_symbols_defined_in_loop (res, loop->num))
		res = instantiate_parameters (loop, res);

	      /* Continue the computation until ending on a parent of LOOP.  */
	      return compute_overall_effect_of_inner_loop (loop, res);
	    }
	}
      else
	return evolution_fn;
     }

  /* If the evolution function is an invariant, there is nothing to do.  */
  else if (no_evolution_in_loop_p (evolution_fn, loop->num, &val) && val)
    return evolution_fn;

  else
    return chrec_dont_know;
}

/* Associate CHREC to SCALAR.  */

static void
set_scalar_evolution (basic_block instantiated_below, tree scalar, tree chrec)
{
  tree *scalar_info;

  if (TREE_CODE (scalar) != SSA_NAME)
    return;

  scalar_info = find_var_scev_info (instantiated_below, scalar);

  if (dump_file)
    {
      if (dump_flags & TDF_SCEV)
	{
	  fprintf (dump_file, "(set_scalar_evolution \n");
	  fprintf (dump_file, "  instantiated_below = %d \n",
		   instantiated_below->index);
	  fprintf (dump_file, "  (scalar = ");
	  print_generic_expr (dump_file, scalar);
	  fprintf (dump_file, ")\n  (scalar_evolution = ");
	  print_generic_expr (dump_file, chrec);
	  fprintf (dump_file, "))\n");
	}
      if (dump_flags & TDF_STATS)
	nb_set_scev++;
    }

  *scalar_info = chrec;
}

/* Retrieve the chrec associated to SCALAR instantiated below
   INSTANTIATED_BELOW block.  */

static tree
get_scalar_evolution (basic_block instantiated_below, tree scalar)
{
  tree res;

  if (dump_file)
    {
      if (dump_flags & TDF_SCEV)
	{
	  fprintf (dump_file, "(get_scalar_evolution \n");
	  fprintf (dump_file, "  (scalar = ");
	  print_generic_expr (dump_file, scalar);
	  fprintf (dump_file, ")\n");
	}
      if (dump_flags & TDF_STATS)
	nb_get_scev++;
    }

  if (VECTOR_TYPE_P (TREE_TYPE (scalar))
      || TREE_CODE (TREE_TYPE (scalar)) == COMPLEX_TYPE)
    /* For chrec_dont_know we keep the symbolic form.  */
    res = scalar;
  else
    switch (TREE_CODE (scalar))
      {
      case SSA_NAME:
        if (SSA_NAME_IS_DEFAULT_DEF (scalar))
	  res = scalar;
	else
	  res = *find_var_scev_info (instantiated_below, scalar);
	break;

      case REAL_CST:
      case FIXED_CST:
      case INTEGER_CST:
	res = scalar;
	break;

      default:
	res = chrec_not_analyzed_yet;
	break;
      }

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "  (scalar_evolution = ");
      print_generic_expr (dump_file, res);
      fprintf (dump_file, "))\n");
    }

  return res;
}


/* Depth first search algorithm.  */

enum t_bool {
  t_false,
  t_true,
  t_dont_know
};

class scev_dfs
{
public:
  scev_dfs (class loop *loop_, gphi *phi_, tree init_cond_)
      : loop (loop_), loop_phi_node (phi_), init_cond (init_cond_) {}
  t_bool get_ev (tree *, tree);

private:
  t_bool follow_ssa_edge_expr (gimple *, tree, tree *, int);
  t_bool follow_ssa_edge_binary (gimple *at_stmt,
				 tree type, tree rhs0, enum tree_code code,
				 tree rhs1, tree *evolution_of_loop, int limit);
  t_bool follow_ssa_edge_in_condition_phi_branch (int i,
						  gphi *condition_phi,
						  tree *evolution_of_branch,
						  tree init_cond, int limit);
  t_bool follow_ssa_edge_in_condition_phi (gphi *condition_phi,
					   tree *evolution_of_loop, int limit);
  t_bool follow_ssa_edge_inner_loop_phi (gphi *loop_phi_node,
					 tree *evolution_of_loop, int limit);
  tree add_to_evolution (tree chrec_before, enum tree_code code,
			 tree to_add, gimple *at_stmt);
  tree add_to_evolution_1 (tree chrec_before, tree to_add, gimple *at_stmt);

  class loop *loop;
  gphi *loop_phi_node;
  tree init_cond;
};

t_bool
scev_dfs::get_ev (tree *ev_fn, tree arg)
{
  *ev_fn = chrec_dont_know;
  return follow_ssa_edge_expr (loop_phi_node, arg, ev_fn, 0);
}

/* Helper function for add_to_evolution.  Returns the evolution
   function for an assignment of the form "a = b + c", where "a" and
   "b" are on the strongly connected component.  CHREC_BEFORE is the
   information that we already have collected up to this point.
   TO_ADD is the evolution of "c".

   When CHREC_BEFORE has an evolution part in LOOP_NB, add to this
   evolution the expression TO_ADD, otherwise construct an evolution
   part for this loop.  */

tree
scev_dfs::add_to_evolution_1 (tree chrec_before, tree to_add, gimple *at_stmt)
{
  tree type, left, right;
  unsigned loop_nb = loop->num;
  class loop *chloop;

  switch (TREE_CODE (chrec_before))
    {
    case POLYNOMIAL_CHREC:
      chloop = get_chrec_loop (chrec_before);
      if (chloop == loop
	  || flow_loop_nested_p (chloop, loop))
	{
	  unsigned var;

	  type = chrec_type (chrec_before);

	  /* When there is no evolution part in this loop, build it.  */
	  if (chloop != loop)
	    {
	      var = loop_nb;
	      left = chrec_before;
	      right = SCALAR_FLOAT_TYPE_P (type)
		? build_real (type, dconst0)
		: build_int_cst (type, 0);
	    }
	  else
	    {
	      var = CHREC_VARIABLE (chrec_before);
	      left = CHREC_LEFT (chrec_before);
	      right = CHREC_RIGHT (chrec_before);
	    }

	  to_add = chrec_convert (type, to_add, at_stmt);
	  right = chrec_convert_rhs (type, right, at_stmt);
	  right = chrec_fold_plus (chrec_type (right), right, to_add);
	  return build_polynomial_chrec (var, left, right);
	}
      else
	{
	  gcc_assert (flow_loop_nested_p (loop, chloop));

	  /* Search the evolution in LOOP_NB.  */
	  left = add_to_evolution_1 (CHREC_LEFT (chrec_before),
				     to_add, at_stmt);
	  right = CHREC_RIGHT (chrec_before);
	  right = chrec_convert_rhs (chrec_type (left), right, at_stmt);
	  return build_polynomial_chrec (CHREC_VARIABLE (chrec_before),
					 left, right);
	}

    default:
      /* These nodes do not depend on a loop.  */
      if (chrec_before == chrec_dont_know)
	return chrec_dont_know;

      left = chrec_before;
      right = chrec_convert_rhs (chrec_type (left), to_add, at_stmt);
      /* When we add the first evolution we need to replace the symbolic
	 evolution we've put in when the DFS reached the loop PHI node
	 with the initial value.  There's only a limited cases of
	 extra operations ontop of that symbol allowed, namely
	 sign-conversions we can look through.  For other cases we leave
	 the symbolic initial condition which causes build_polynomial_chrec
	 to return chrec_dont_know.  See PR42512, PR66375 and PR107176 for
	 cases we mishandled before.  */
      STRIP_NOPS (chrec_before);
      if (chrec_before == gimple_phi_result (loop_phi_node))
	left = fold_convert (TREE_TYPE (left), init_cond);
      return build_polynomial_chrec (loop_nb, left, right);
    }
}

/* Add TO_ADD to the evolution part of CHREC_BEFORE in the dimension
   of LOOP_NB.

   Description (provided for completeness, for those who read code in
   a plane, and for my poor 62 bytes brain that would have forgotten
   all this in the next two or three months):

   The algorithm of translation of programs from the SSA representation
   into the chrecs syntax is based on a pattern matching.  After having
   reconstructed the overall tree expression for a loop, there are only
   two cases that can arise:

   1. a = loop-phi (init, a + expr)
   2. a = loop-phi (init, expr)

   where EXPR is either a scalar constant with respect to the analyzed
   loop (this is a degree 0 polynomial), or an expression containing
   other loop-phi definitions (these are higher degree polynomials).

   Examples:

   1.
   | init = ...
   | loop_1
   |   a = phi (init, a + 5)
   | endloop

   2.
   | inita = ...
   | initb = ...
   | loop_1
   |   a = phi (inita, 2 * b + 3)
   |   b = phi (initb, b + 1)
   | endloop

   For the first case, the semantics of the SSA representation is:

   | a (x) = init + \sum_{j = 0}^{x - 1} expr (j)

   that is, there is a loop index "x" that determines the scalar value
   of the variable during the loop execution.  During the first
   iteration, the value is that of the initial condition INIT, while
   during the subsequent iterations, it is the sum of the initial
   condition with the sum of all the values of EXPR from the initial
   iteration to the before last considered iteration.

   For the second case, the semantics of the SSA program is:

   | a (x) = init, if x = 0;
   |         expr (x - 1), otherwise.

   The second case corresponds to the PEELED_CHREC, whose syntax is
   close to the syntax of a loop-phi-node:

   | phi (init, expr)  vs.  (init, expr)_x

   The proof of the translation algorithm for the first case is a
   proof by structural induction based on the degree of EXPR.

   Degree 0:
   When EXPR is a constant with respect to the analyzed loop, or in
   other words when EXPR is a polynomial of degree 0, the evolution of
   the variable A in the loop is an affine function with an initial
   condition INIT, and a step EXPR.  In order to show this, we start
   from the semantics of the SSA representation:

   f (x) = init + \sum_{j = 0}^{x - 1} expr (j)

   and since "expr (j)" is a constant with respect to "j",

   f (x) = init + x * expr

   Finally, based on the semantics of the pure sum chrecs, by
   identification we get the corresponding chrecs syntax:

   f (x) = init * \binom{x}{0} + expr * \binom{x}{1}
   f (x) -> {init, +, expr}_x

   Higher degree:
   Suppose that EXPR is a polynomial of degree N with respect to the
   analyzed loop_x for which we have already determined that it is
   written under the chrecs syntax:

   | expr (x)  ->  {b_0, +, b_1, +, ..., +, b_{n-1}} (x)

   We start from the semantics of the SSA program:

   | f (x) = init + \sum_{j = 0}^{x - 1} expr (j)
   |
   | f (x) = init + \sum_{j = 0}^{x - 1}
   |                (b_0 * \binom{j}{0} + ... + b_{n-1} * \binom{j}{n-1})
   |
   | f (x) = init + \sum_{j = 0}^{x - 1}
   |                \sum_{k = 0}^{n - 1} (b_k * \binom{j}{k})
   |
   | f (x) = init + \sum_{k = 0}^{n - 1}
   |                (b_k * \sum_{j = 0}^{x - 1} \binom{j}{k})
   |
   | f (x) = init + \sum_{k = 0}^{n - 1}
   |                (b_k * \binom{x}{k + 1})
   |
   | f (x) = init + b_0 * \binom{x}{1} + ...
   |              + b_{n-1} * \binom{x}{n}
   |
   | f (x) = init * \binom{x}{0} + b_0 * \binom{x}{1} + ...
   |                             + b_{n-1} * \binom{x}{n}
   |

   And finally from the definition of the chrecs syntax, we identify:
   | f (x)  ->  {init, +, b_0, +, ..., +, b_{n-1}}_x

   This shows the mechanism that stands behind the add_to_evolution
   function.  An important point is that the use of symbolic
   parameters avoids the need of an analysis schedule.

   Example:

   | inita = ...
   | initb = ...
   | loop_1
   |   a = phi (inita, a + 2 + b)
   |   b = phi (initb, b + 1)
   | endloop

   When analyzing "a", the algorithm keeps "b" symbolically:

   | a  ->  {inita, +, 2 + b}_1

   Then, after instantiation, the analyzer ends on the evolution:

   | a  ->  {inita, +, 2 + initb, +, 1}_1

*/

tree
scev_dfs::add_to_evolution (tree chrec_before, enum tree_code code,
			    tree to_add, gimple *at_stmt)
{
  tree type = chrec_type (to_add);
  tree res = NULL_TREE;

  if (to_add == NULL_TREE)
    return chrec_before;

  /* TO_ADD is either a scalar, or a parameter.  TO_ADD is not
     instantiated at this point.  */
  if (TREE_CODE (to_add) == POLYNOMIAL_CHREC)
    /* This should not happen.  */
    return chrec_dont_know;

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "(add_to_evolution \n");
      fprintf (dump_file, "  (loop_nb = %d)\n", loop->num);
      fprintf (dump_file, "  (chrec_before = ");
      print_generic_expr (dump_file, chrec_before);
      fprintf (dump_file, ")\n  (to_add = ");
      print_generic_expr (dump_file, to_add);
      fprintf (dump_file, ")\n");
    }

  if (code == MINUS_EXPR)
    to_add = chrec_fold_multiply (type, to_add, SCALAR_FLOAT_TYPE_P (type)
				  ? build_real (type, dconstm1)
				  : build_int_cst_type (type, -1));

  res = add_to_evolution_1 (chrec_before, to_add, at_stmt);

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "  (res = ");
      print_generic_expr (dump_file, res);
      fprintf (dump_file, "))\n");
    }

  return res;
}


/* Follow the ssa edge into the binary expression RHS0 CODE RHS1.
   Return true if the strongly connected component has been found.  */

t_bool
scev_dfs::follow_ssa_edge_binary (gimple *at_stmt, tree type, tree rhs0,
				  enum tree_code code, tree rhs1,
				  tree *evolution_of_loop, int limit)
{
  t_bool res = t_false;
  tree evol;

  switch (code)
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
      if (TREE_CODE (rhs0) == SSA_NAME)
	{
	  if (TREE_CODE (rhs1) == SSA_NAME)
	    {
	      /* Match an assignment under the form:
		 "a = b + c".  */

	      /* We want only assignments of form "name + name" contribute to
		 LIMIT, as the other cases do not necessarily contribute to
		 the complexity of the expression.  */
	      limit++;

	      evol = *evolution_of_loop;
	      res = follow_ssa_edge_expr (at_stmt, rhs0, &evol, limit);
	      if (res == t_true)
		*evolution_of_loop = add_to_evolution
		    (chrec_convert (type, evol, at_stmt), code, rhs1, at_stmt);
	      else if (res == t_false)
		{
		  res = follow_ssa_edge_expr
		    (at_stmt, rhs1, evolution_of_loop, limit);
		  if (res == t_true)
		    *evolution_of_loop = add_to_evolution
			(chrec_convert (type, *evolution_of_loop, at_stmt),
			 code, rhs0, at_stmt);
		}
	    }

	  else
	    gcc_unreachable ();  /* Handled in caller.  */
	}

      else if (TREE_CODE (rhs1) == SSA_NAME)
	{
	  /* Match an assignment under the form:
	     "a = ... + c".  */
	  res = follow_ssa_edge_expr (at_stmt, rhs1, evolution_of_loop, limit);
	  if (res == t_true)
	    *evolution_of_loop = add_to_evolution
		(chrec_convert (type, *evolution_of_loop, at_stmt),
		 code, rhs0, at_stmt);
	}

      else
	/* Otherwise, match an assignment under the form:
	   "a = ... + ...".  */
	/* And there is nothing to do.  */
	res = t_false;
      break;

    case MINUS_EXPR:
      /* This case is under the form "opnd0 = rhs0 - rhs1".  */
      if (TREE_CODE (rhs0) == SSA_NAME)
	gcc_unreachable (); /* Handled in caller.  */
      else
	/* Otherwise, match an assignment under the form:
	   "a = ... - ...".  */
	/* And there is nothing to do.  */
	res = t_false;
      break;

    default:
      res = t_false;
    }

  return res;
}

/* Checks whether the I-th argument of a PHI comes from a backedge.  */

static bool
backedge_phi_arg_p (gphi *phi, int i)
{
  const_edge e = gimple_phi_arg_edge (phi, i);

  /* We would in fact like to test EDGE_DFS_BACK here, but we do not care
     about updating it anywhere, and this should work as well most of the
     time.  */
  if (e->flags & EDGE_IRREDUCIBLE_LOOP)
    return true;

  return false;
}

/* Helper function for one branch of the condition-phi-node.  Return
   true if the strongly connected component has been found following
   this path.  */

t_bool
scev_dfs::follow_ssa_edge_in_condition_phi_branch (int i,
						   gphi *condition_phi,
						   tree *evolution_of_branch,
						   tree init_cond, int limit)
{
  tree branch = PHI_ARG_DEF (condition_phi, i);
  *evolution_of_branch = chrec_dont_know;

  /* Do not follow back edges (they must belong to an irreducible loop, which
     we really do not want to worry about).  */
  if (backedge_phi_arg_p (condition_phi, i))
    return t_false;

  if (TREE_CODE (branch) == SSA_NAME)
    {
      *evolution_of_branch = init_cond;
      return follow_ssa_edge_expr (condition_phi, branch,
				   evolution_of_branch, limit);
    }

  /* This case occurs when one of the condition branches sets
     the variable to a constant: i.e. a phi-node like
     "a_2 = PHI <a_7(5), 2(6)>;".

     FIXME:  This case have to be refined correctly:
     in some cases it is possible to say something better than
     chrec_dont_know, for example using a wrap-around notation.  */
  return t_false;
}

/* This function merges the branches of a condition-phi-node in a
   loop.  */

t_bool
scev_dfs::follow_ssa_edge_in_condition_phi (gphi *condition_phi,
					    tree *evolution_of_loop, int limit)
{
  int i, n;
  tree init = *evolution_of_loop;
  tree evolution_of_branch;
  t_bool res = follow_ssa_edge_in_condition_phi_branch (0, condition_phi,
							&evolution_of_branch,
							init, limit);
  if (res == t_false || res == t_dont_know)
    return res;

  *evolution_of_loop = evolution_of_branch;

  n = gimple_phi_num_args (condition_phi);
  for (i = 1; i < n; i++)
    {
      /* Quickly give up when the evolution of one of the branches is
	 not known.  */
      if (*evolution_of_loop == chrec_dont_know)
	return t_true;

      /* Increase the limit by the PHI argument number to avoid exponential
	 time and memory complexity.  */
      res = follow_ssa_edge_in_condition_phi_branch (i, condition_phi,
						     &evolution_of_branch,
						     init, limit + i);
      if (res == t_false || res == t_dont_know)
	return res;

      *evolution_of_loop = chrec_merge (*evolution_of_loop,
					evolution_of_branch);
    }

  return t_true;
}

/* Follow an SSA edge in an inner loop.  It computes the overall
   effect of the loop, and following the symbolic initial conditions,
   it follows the edges in the parent loop.  The inner loop is
   considered as a single statement.  */

t_bool
scev_dfs::follow_ssa_edge_inner_loop_phi (gphi *loop_phi_node,
					  tree *evolution_of_loop, int limit)
{
  class loop *loop = loop_containing_stmt (loop_phi_node);
  tree ev = analyze_scalar_evolution (loop, PHI_RESULT (loop_phi_node));

  /* Sometimes, the inner loop is too difficult to analyze, and the
     result of the analysis is a symbolic parameter.  */
  if (ev == PHI_RESULT (loop_phi_node))
    {
      t_bool res = t_false;
      int i, n = gimple_phi_num_args (loop_phi_node);

      for (i = 0; i < n; i++)
	{
	  tree arg = PHI_ARG_DEF (loop_phi_node, i);
	  basic_block bb;

	  /* Follow the edges that exit the inner loop.  */
	  bb = gimple_phi_arg_edge (loop_phi_node, i)->src;
	  if (!flow_bb_inside_loop_p (loop, bb))
	    res = follow_ssa_edge_expr (loop_phi_node,
					arg, evolution_of_loop, limit);
	  if (res == t_true)
	    break;
	}

      /* If the path crosses this loop-phi, give up.  */
      if (res == t_true)
	*evolution_of_loop = chrec_dont_know;

      return res;
    }

  /* Otherwise, compute the overall effect of the inner loop.  */
  ev = compute_overall_effect_of_inner_loop (loop, ev);
  return follow_ssa_edge_expr (loop_phi_node, ev, evolution_of_loop, limit);
}

/* Follow the ssa edge into the expression EXPR.
   Return true if the strongly connected component has been found.  */

t_bool
scev_dfs::follow_ssa_edge_expr (gimple *at_stmt, tree expr,
				tree *evolution_of_loop, int limit)
{
  gphi *halting_phi = loop_phi_node;
  enum tree_code code;
  tree type, rhs0, rhs1 = NULL_TREE;

  /* The EXPR is one of the following cases:
     - an SSA_NAME,
     - an INTEGER_CST,
     - a PLUS_EXPR,
     - a POINTER_PLUS_EXPR,
     - a MINUS_EXPR,
     - other cases are not yet handled.  */

  /* For SSA_NAME look at the definition statement, handling
     PHI nodes and otherwise expand appropriately for the expression
     handling below.  */
  if (TREE_CODE (expr) == SSA_NAME)
    {
      gimple *def = SSA_NAME_DEF_STMT (expr);

      if (gimple_nop_p (def))
	return t_false;

      /* Give up if the path is longer than the MAX that we allow.  */
      if (limit > param_scev_max_expr_complexity)
	{
	  *evolution_of_loop = chrec_dont_know;
	  return t_dont_know;
	}

      if (gphi *phi = dyn_cast <gphi *>(def))
	{
	  if (!loop_phi_node_p (phi))
	    /* DEF is a condition-phi-node.  Follow the branches, and
	       record their evolutions.  Finally, merge the collected
	       information and set the approximation to the main
	       variable.  */
	    return follow_ssa_edge_in_condition_phi (phi, evolution_of_loop,
						     limit);

	  /* When the analyzed phi is the halting_phi, the
	     depth-first search is over: we have found a path from
	     the halting_phi to itself in the loop.  */
	  if (phi == halting_phi)
	    {
	      *evolution_of_loop = expr;
	      return t_true;
	    }

	  /* Otherwise, the evolution of the HALTING_PHI depends
	     on the evolution of another loop-phi-node, i.e. the
	     evolution function is a higher degree polynomial.  */
	  class loop *def_loop = loop_containing_stmt (def);
	  if (def_loop == loop)
	    return t_false;

	  /* Inner loop.  */
	  if (flow_loop_nested_p (loop, def_loop))
	    return follow_ssa_edge_inner_loop_phi (phi, evolution_of_loop,
						   limit + 1);

	  /* Outer loop.  */
	  return t_false;
	}

      /* At this level of abstraction, the program is just a set
	 of GIMPLE_ASSIGNs and PHI_NODEs.  In principle there is no
	 other def to be handled.  */
      if (!is_gimple_assign (def))
	return t_false;

      code = gimple_assign_rhs_code (def);
      switch (get_gimple_rhs_class (code))
	{
	case GIMPLE_BINARY_RHS:
	  rhs0 = gimple_assign_rhs1 (def);
	  rhs1 = gimple_assign_rhs2 (def);
	  break;
	case GIMPLE_UNARY_RHS:
	case GIMPLE_SINGLE_RHS:
	  rhs0 = gimple_assign_rhs1 (def);
	  break;
	default:
	  return t_false;
	}
      type = TREE_TYPE (gimple_assign_lhs (def));
      at_stmt = def;
    }
  else
    {
      code = TREE_CODE (expr);
      type = TREE_TYPE (expr);
      /* Via follow_ssa_edge_inner_loop_phi we arrive here with the
	 GENERIC scalar evolution of the inner loop.  */
      switch (code)
	{
	CASE_CONVERT:
	  rhs0 = TREE_OPERAND (expr, 0);
	  break;
	case POINTER_PLUS_EXPR:
	case PLUS_EXPR:
	case MINUS_EXPR:
	  rhs0 = TREE_OPERAND (expr, 0);
	  rhs1 = TREE_OPERAND (expr, 1);
	  STRIP_USELESS_TYPE_CONVERSION (rhs0);
	  STRIP_USELESS_TYPE_CONVERSION (rhs1);
	  break;
	default:
	  rhs0 = expr;
	}
    }

  switch (code)
    {
    CASE_CONVERT:
      {
	/* This assignment is under the form "a_1 = (cast) rhs.  We cannot
	   validate any precision altering conversion during the SCC
	   analysis, so don't even try.  */
	if (!tree_nop_conversion_p (type, TREE_TYPE (rhs0)))
	  return t_false;
	t_bool res = follow_ssa_edge_expr (at_stmt, rhs0,
					   evolution_of_loop, limit);
	if (res == t_true)
	  *evolution_of_loop = chrec_convert (type, *evolution_of_loop,
					      at_stmt);
	return res;
      }

    case INTEGER_CST:
      /* This assignment is under the form "a_1 = 7".  */
      return t_false;

    case ADDR_EXPR:
      {
	/* Handle &MEM[ptr + CST] which is equivalent to POINTER_PLUS_EXPR.  */
	if (TREE_CODE (TREE_OPERAND (rhs0, 0)) != MEM_REF)
	  return t_false;
	tree mem = TREE_OPERAND (rhs0, 0);
	rhs0 = TREE_OPERAND (mem, 0);
	rhs1 = TREE_OPERAND (mem, 1);
	code = POINTER_PLUS_EXPR;
      }
      /* Fallthru.  */
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
      /* This case is under the form "rhs0 +- rhs1".  */
      if (TREE_CODE (rhs0) == SSA_NAME
	  && (TREE_CODE (rhs1) != SSA_NAME || code == MINUS_EXPR))
	{
	  /* Match an assignment under the form:
	     "a = b +- ...".  */
	  t_bool res = follow_ssa_edge_expr (at_stmt, rhs0,
					     evolution_of_loop, limit);
	  if (res == t_true)
	    *evolution_of_loop = add_to_evolution
		(chrec_convert (type, *evolution_of_loop, at_stmt),
		 code, rhs1, at_stmt);
	  return res;
	}
      /* Else search for the SCC in both rhs0 and rhs1.  */
      return follow_ssa_edge_binary (at_stmt, type, rhs0, code, rhs1,
				     evolution_of_loop, limit);

    default:
      return t_false;
    }
}


/* This section selects the loops that will be good candidates for the
   scalar evolution analysis.  For the moment, greedily select all the
   loop nests we could analyze.  */

/* For a loop with a single exit edge, return the COND_EXPR that
   guards the exit edge.  If the expression is too difficult to
   analyze, then give up.  */

gcond *
get_loop_exit_condition (const class loop *loop)
{
  return get_loop_exit_condition (single_exit (loop));
}

/* If the statement just before the EXIT_EDGE contains a condition then
   return the condition, otherwise NULL. */

gcond *
get_loop_exit_condition (const_edge exit_edge)
{
  gcond *res = NULL;

  if (dump_file && (dump_flags & TDF_SCEV))
    fprintf (dump_file, "(get_loop_exit_condition \n  ");

  if (exit_edge)
    res = safe_dyn_cast <gcond *> (*gsi_last_bb (exit_edge->src));

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      print_gimple_stmt (dump_file, res, 0);
      fprintf (dump_file, ")\n");
    }

  return res;
}


/* Simplify PEELED_CHREC represented by (init_cond, arg) in LOOP.
   Handle below case and return the corresponding POLYNOMIAL_CHREC:

   # i_17 = PHI <i_13(5), 0(3)>
   # _20 = PHI <_5(5), start_4(D)(3)>
   ...
   i_13 = i_17 + 1;
   _5 = start_4(D) + i_13;

   Though variable _20 appears as a PEELED_CHREC in the form of
   (start_4, _5)_LOOP, it's a POLYNOMIAL_CHREC like {start_4, 1}_LOOP.

   See PR41488.  */

static tree
simplify_peeled_chrec (class loop *loop, tree arg, tree init_cond)
{
  aff_tree aff1, aff2;
  tree ev, left, right, type, step_val;
  hash_map<tree, name_expansion *> *peeled_chrec_map = NULL;

  ev = instantiate_parameters (loop, analyze_scalar_evolution (loop, arg));
  if (ev == NULL_TREE || TREE_CODE (ev) != POLYNOMIAL_CHREC)
    return chrec_dont_know;

  left = CHREC_LEFT (ev);
  right = CHREC_RIGHT (ev);
  type = TREE_TYPE (left);
  step_val = chrec_fold_plus (type, init_cond, right);

  /* Transform (init, {left, right}_LOOP)_LOOP to {init, right}_LOOP
     if "left" equals to "init + right".  */
  if (operand_equal_p (left, step_val, 0))
    {
      if (dump_file && (dump_flags & TDF_SCEV))
	fprintf (dump_file, "Simplify PEELED_CHREC into POLYNOMIAL_CHREC.\n");

      return build_polynomial_chrec (loop->num, init_cond, right);
    }

  /* The affine code only deals with pointer and integer types.  */
  if (!POINTER_TYPE_P (type)
      && !INTEGRAL_TYPE_P (type))
    return chrec_dont_know;

  /* Try harder to check if they are equal.  */
  tree_to_aff_combination_expand (left, type, &aff1, &peeled_chrec_map);
  tree_to_aff_combination_expand (step_val, type, &aff2, &peeled_chrec_map);
  free_affine_expand_cache (&peeled_chrec_map);
  aff_combination_scale (&aff2, -1);
  aff_combination_add (&aff1, &aff2);

  /* Transform (init, {left, right}_LOOP)_LOOP to {init, right}_LOOP
     if "left" equals to "init + right".  */
  if (aff_combination_zero_p (&aff1))
    {
      if (dump_file && (dump_flags & TDF_SCEV))
	fprintf (dump_file, "Simplify PEELED_CHREC into POLYNOMIAL_CHREC.\n");

      return build_polynomial_chrec (loop->num, init_cond, right);
    }
  return chrec_dont_know;
}

/* Given a LOOP_PHI_NODE, this function determines the evolution
   function from LOOP_PHI_NODE to LOOP_PHI_NODE in the loop.  */

static tree
analyze_evolution_in_loop (gphi *loop_phi_node,
			   tree init_cond)
{
  int i, n = gimple_phi_num_args (loop_phi_node);
  tree evolution_function = chrec_not_analyzed_yet;
  class loop *loop = loop_containing_stmt (loop_phi_node);
  basic_block bb;
  static bool simplify_peeled_chrec_p = true;

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "(analyze_evolution_in_loop \n");
      fprintf (dump_file, "  (loop_phi_node = ");
      print_gimple_stmt (dump_file, loop_phi_node, 0);
      fprintf (dump_file, ")\n");
    }

  for (i = 0; i < n; i++)
    {
      tree arg = PHI_ARG_DEF (loop_phi_node, i);
      tree ev_fn = chrec_dont_know;
      t_bool res;

      /* Select the edges that enter the loop body.  */
      bb = gimple_phi_arg_edge (loop_phi_node, i)->src;
      if (!flow_bb_inside_loop_p (loop, bb))
	continue;

      if (TREE_CODE (arg) == SSA_NAME)
	{
	  bool val = false;

	  /* Pass in the initial condition to the follow edge function.  */
	  scev_dfs dfs (loop, loop_phi_node, init_cond);
	  res = dfs.get_ev (&ev_fn, arg);

	  /* If ev_fn has no evolution in the inner loop, and the
	     init_cond is not equal to ev_fn, then we have an
	     ambiguity between two possible values, as we cannot know
	     the number of iterations at this point.  */
	  if (TREE_CODE (ev_fn) != POLYNOMIAL_CHREC
	      && no_evolution_in_loop_p (ev_fn, loop->num, &val) && val
	      && !operand_equal_p (init_cond, ev_fn, 0))
	    ev_fn = chrec_dont_know;
	}
      else
	res = t_false;

      /* When it is impossible to go back on the same
	 loop_phi_node by following the ssa edges, the
	 evolution is represented by a peeled chrec, i.e. the
	 first iteration, EV_FN has the value INIT_COND, then
	 all the other iterations it has the value of ARG.
	 For the moment, PEELED_CHREC nodes are not built.  */
      if (res != t_true)
	{
	  ev_fn = chrec_dont_know;
	  /* Try to recognize POLYNOMIAL_CHREC which appears in
	     the form of PEELED_CHREC, but guard the process with
	     a bool variable to keep the analyzer from infinite
	     recurrence for real PEELED_RECs.  */
	  if (simplify_peeled_chrec_p && TREE_CODE (arg) == SSA_NAME)
	    {
	      simplify_peeled_chrec_p = false;
	      ev_fn = simplify_peeled_chrec (loop, arg, init_cond);
	      simplify_peeled_chrec_p = true;
	    }
	}

      /* When there are multiple back edges of the loop (which in fact never
	 happens currently, but nevertheless), merge their evolutions.  */
      evolution_function = chrec_merge (evolution_function, ev_fn);

      if (evolution_function == chrec_dont_know)
	break;
    }

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "  (evolution_function = ");
      print_generic_expr (dump_file, evolution_function);
      fprintf (dump_file, "))\n");
    }

  return evolution_function;
}

/* Looks to see if VAR is a copy of a constant (via straightforward assignments
   or degenerate phi's).  If so, returns the constant; else, returns VAR.  */

static tree
follow_copies_to_constant (tree var)
{
  tree res = var;
  while (TREE_CODE (res) == SSA_NAME
	 /* We face not updated SSA form in multiple places and this walk
	    may end up in sibling loops so we have to guard it.  */
	 && !name_registered_for_update_p (res))
    {
      gimple *def = SSA_NAME_DEF_STMT (res);
      if (gphi *phi = dyn_cast <gphi *> (def))
	{
	  if (tree rhs = degenerate_phi_result (phi))
	    res = rhs;
	  else
	    break;
	}
      else if (gimple_assign_single_p (def))
	/* Will exit loop if not an SSA_NAME.  */
	res = gimple_assign_rhs1 (def);
      else
	break;
    }
  if (CONSTANT_CLASS_P (res))
    return res;
  return var;
}

/* Given a loop-phi-node, return the initial conditions of the
   variable on entry of the loop.  When the CCP has propagated
   constants into the loop-phi-node, the initial condition is
   instantiated, otherwise the initial condition is kept symbolic.
   This analyzer does not analyze the evolution outside the current
   loop, and leaves this task to the on-demand tree reconstructor.  */

static tree
analyze_initial_condition (gphi *loop_phi_node)
{
  int i, n;
  tree init_cond = chrec_not_analyzed_yet;
  class loop *loop = loop_containing_stmt (loop_phi_node);

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "(analyze_initial_condition \n");
      fprintf (dump_file, "  (loop_phi_node = \n");
      print_gimple_stmt (dump_file, loop_phi_node, 0);
      fprintf (dump_file, ")\n");
    }

  n = gimple_phi_num_args (loop_phi_node);
  for (i = 0; i < n; i++)
    {
      tree branch = PHI_ARG_DEF (loop_phi_node, i);
      basic_block bb = gimple_phi_arg_edge (loop_phi_node, i)->src;

      /* When the branch is oriented to the loop's body, it does
     	 not contribute to the initial condition.  */
      if (flow_bb_inside_loop_p (loop, bb))
       	continue;

      if (init_cond == chrec_not_analyzed_yet)
	{
	  init_cond = branch;
	  continue;
	}

      if (TREE_CODE (branch) == SSA_NAME)
	{
	  init_cond = chrec_dont_know;
      	  break;
	}

      init_cond = chrec_merge (init_cond, branch);
    }

  /* Ooops -- a loop without an entry???  */
  if (init_cond == chrec_not_analyzed_yet)
    init_cond = chrec_dont_know;

  /* We may not have fully constant propagated IL.  Handle degenerate PHIs here
     to not miss important early loop unrollings.  */
  init_cond = follow_copies_to_constant (init_cond);

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "  (init_cond = ");
      print_generic_expr (dump_file, init_cond);
      fprintf (dump_file, "))\n");
    }

  return init_cond;
}

/* Analyze the scalar evolution for LOOP_PHI_NODE.  */

static tree
interpret_loop_phi (class loop *loop, gphi *loop_phi_node)
{
  class loop *phi_loop = loop_containing_stmt (loop_phi_node);
  tree init_cond;

  gcc_assert (phi_loop == loop);

  /* Otherwise really interpret the loop phi.  */
  init_cond = analyze_initial_condition (loop_phi_node);
  return analyze_evolution_in_loop (loop_phi_node, init_cond);
}

/* This function merges the branches of a condition-phi-node,
   contained in the outermost loop, and whose arguments are already
   analyzed.  */

static tree
interpret_condition_phi (class loop *loop, gphi *condition_phi)
{
  int i, n = gimple_phi_num_args (condition_phi);
  tree res = chrec_not_analyzed_yet;

  for (i = 0; i < n; i++)
    {
      tree branch_chrec;

      if (backedge_phi_arg_p (condition_phi, i))
	{
	  res = chrec_dont_know;
	  break;
	}

      branch_chrec = analyze_scalar_evolution
	(loop, PHI_ARG_DEF (condition_phi, i));

      res = chrec_merge (res, branch_chrec);
      if (res == chrec_dont_know)
	break;
    }

  return res;
}

/* Interpret the operation RHS1 OP RHS2.  If we didn't
   analyze this node before, follow the definitions until ending
   either on an analyzed GIMPLE_ASSIGN, or on a loop-phi-node.  On the
   return path, this function propagates evolutions (ala constant copy
   propagation).  OPND1 is not a GIMPLE expression because we could
   analyze the effect of an inner loop: see interpret_loop_phi.  */

static tree
interpret_rhs_expr (class loop *loop, gimple *at_stmt,
		    tree type, tree rhs1, enum tree_code code, tree rhs2)
{
  tree res, chrec1, chrec2, ctype;
  gimple *def;

  if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS)
    {
      if (is_gimple_min_invariant (rhs1))
	return chrec_convert (type, rhs1, at_stmt);

      if (code == SSA_NAME)
	return chrec_convert (type, analyze_scalar_evolution (loop, rhs1),
			      at_stmt);
    }

  switch (code)
    {
    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (rhs1, 0)) == MEM_REF
	  || handled_component_p (TREE_OPERAND (rhs1, 0)))
        {
	  machine_mode mode;
	  poly_int64 bitsize, bitpos;
	  int unsignedp, reversep;
	  int volatilep = 0;
	  tree base, offset;
	  tree chrec3;
	  tree unitpos;

	  base = get_inner_reference (TREE_OPERAND (rhs1, 0),
				      &bitsize, &bitpos, &offset, &mode,
				      &unsignedp, &reversep, &volatilep);

	  if (TREE_CODE (base) == MEM_REF)
	    {
	      rhs2 = TREE_OPERAND (base, 1);
	      rhs1 = TREE_OPERAND (base, 0);

	      chrec1 = analyze_scalar_evolution (loop, rhs1);
	      chrec2 = analyze_scalar_evolution (loop, rhs2);
	      chrec1 = chrec_convert (type, chrec1, at_stmt);
	      chrec2 = chrec_convert (TREE_TYPE (rhs2), chrec2, at_stmt);
	      chrec1 = instantiate_parameters (loop, chrec1);
	      chrec2 = instantiate_parameters (loop, chrec2);
	      res = chrec_fold_plus (type, chrec1, chrec2);
	    }
	  else
	    {
	      chrec1 = analyze_scalar_evolution_for_address_of (loop, base);
	      chrec1 = chrec_convert (type, chrec1, at_stmt);
	      res = chrec1;
	    }

	  if (offset != NULL_TREE)
	    {
	      chrec2 = analyze_scalar_evolution (loop, offset);
	      chrec2 = chrec_convert (TREE_TYPE (offset), chrec2, at_stmt);
	      chrec2 = instantiate_parameters (loop, chrec2);
	      res = chrec_fold_plus (type, res, chrec2);
	    }

	  if (maybe_ne (bitpos, 0))
	    {
	      unitpos = size_int (exact_div (bitpos, BITS_PER_UNIT));
	      chrec3 = analyze_scalar_evolution (loop, unitpos);
	      chrec3 = chrec_convert (TREE_TYPE (unitpos), chrec3, at_stmt);
	      chrec3 = instantiate_parameters (loop, chrec3);
	      res = chrec_fold_plus (type, res, chrec3);
	    }
        }
      else
	res = chrec_dont_know;
      break;

    case POINTER_PLUS_EXPR:
      chrec1 = analyze_scalar_evolution (loop, rhs1);
      chrec2 = analyze_scalar_evolution (loop, rhs2);
      chrec1 = chrec_convert (type, chrec1, at_stmt);
      chrec2 = chrec_convert (TREE_TYPE (rhs2), chrec2, at_stmt);
      chrec1 = instantiate_parameters (loop, chrec1);
      chrec2 = instantiate_parameters (loop, chrec2);
      res = chrec_fold_plus (type, chrec1, chrec2);
      break;

    case PLUS_EXPR:
      chrec1 = analyze_scalar_evolution (loop, rhs1);
      chrec2 = analyze_scalar_evolution (loop, rhs2);
      ctype = type;
      /* When the stmt is conditionally executed re-write the CHREC
         into a form that has well-defined behavior on overflow.  */
      if (at_stmt
	  && INTEGRAL_TYPE_P (type)
	  && ! TYPE_OVERFLOW_WRAPS (type)
	  && ! dominated_by_p (CDI_DOMINATORS, loop->latch,
			       gimple_bb (at_stmt)))
	ctype = unsigned_type_for (type);
      chrec1 = chrec_convert (ctype, chrec1, at_stmt);
      chrec2 = chrec_convert (ctype, chrec2, at_stmt);
      chrec1 = instantiate_parameters (loop, chrec1);
      chrec2 = instantiate_parameters (loop, chrec2);
      res = chrec_fold_plus (ctype, chrec1, chrec2);
      if (type != ctype)
	res = chrec_convert (type, res, at_stmt);
      break;

    case MINUS_EXPR:
      chrec1 = analyze_scalar_evolution (loop, rhs1);
      chrec2 = analyze_scalar_evolution (loop, rhs2);
      ctype = type;
      /* When the stmt is conditionally executed re-write the CHREC
         into a form that has well-defined behavior on overflow.  */
      if (at_stmt
	  && INTEGRAL_TYPE_P (type)
	  && ! TYPE_OVERFLOW_WRAPS (type)
	  && ! dominated_by_p (CDI_DOMINATORS,
			       loop->latch, gimple_bb (at_stmt)))
	ctype = unsigned_type_for (type);
      chrec1 = chrec_convert (ctype, chrec1, at_stmt);
      chrec2 = chrec_convert (ctype, chrec2, at_stmt);
      chrec1 = instantiate_parameters (loop, chrec1);
      chrec2 = instantiate_parameters (loop, chrec2);
      res = chrec_fold_minus (ctype, chrec1, chrec2);
      if (type != ctype)
	res = chrec_convert (type, res, at_stmt);
      break;

    case NEGATE_EXPR:
      chrec1 = analyze_scalar_evolution (loop, rhs1);
      ctype = type;
      /* When the stmt is conditionally executed re-write the CHREC
         into a form that has well-defined behavior on overflow.  */
      if (at_stmt
	  && INTEGRAL_TYPE_P (type)
	  && ! TYPE_OVERFLOW_WRAPS (type)
	  && ! dominated_by_p (CDI_DOMINATORS,
			       loop->latch, gimple_bb (at_stmt)))
	ctype = unsigned_type_for (type);
      chrec1 = chrec_convert (ctype, chrec1, at_stmt);
      /* TYPE may be integer, real or complex, so use fold_convert.  */
      chrec1 = instantiate_parameters (loop, chrec1);
      res = chrec_fold_multiply (ctype, chrec1,
				 fold_convert (ctype, integer_minus_one_node));
      if (type != ctype)
	res = chrec_convert (type, res, at_stmt);
      break;

    case BIT_NOT_EXPR:
      /* Handle ~X as -1 - X.  */
      chrec1 = analyze_scalar_evolution (loop, rhs1);
      chrec1 = chrec_convert (type, chrec1, at_stmt);
      chrec1 = instantiate_parameters (loop, chrec1);
      res = chrec_fold_minus (type,
			      fold_convert (type, integer_minus_one_node),
			      chrec1);
      break;

    case MULT_EXPR:
      chrec1 = analyze_scalar_evolution (loop, rhs1);
      chrec2 = analyze_scalar_evolution (loop, rhs2);
      ctype = type;
      /* When the stmt is conditionally executed re-write the CHREC
         into a form that has well-defined behavior on overflow.  */
      if (at_stmt
	  && INTEGRAL_TYPE_P (type)
	  && ! TYPE_OVERFLOW_WRAPS (type)
	  && ! dominated_by_p (CDI_DOMINATORS,
			       loop->latch, gimple_bb (at_stmt)))
	ctype = unsigned_type_for (type);
      chrec1 = chrec_convert (ctype, chrec1, at_stmt);
      chrec2 = chrec_convert (ctype, chrec2, at_stmt);
      chrec1 = instantiate_parameters (loop, chrec1);
      chrec2 = instantiate_parameters (loop, chrec2);
      res = chrec_fold_multiply (ctype, chrec1, chrec2);
      if (type != ctype)
	res = chrec_convert (type, res, at_stmt);
      break;

    case LSHIFT_EXPR:
      {
	/* Handle A<<B as A * (1<<B).  */
	tree uns = unsigned_type_for (type);
	chrec1 = analyze_scalar_evolution (loop, rhs1);
	chrec2 = analyze_scalar_evolution (loop, rhs2);
	chrec1 = chrec_convert (uns, chrec1, at_stmt);
	chrec1 = instantiate_parameters (loop, chrec1);
	chrec2 = instantiate_parameters (loop, chrec2);

	tree one = build_int_cst (uns, 1);
	chrec2 = fold_build2 (LSHIFT_EXPR, uns, one, chrec2);
	res = chrec_fold_multiply (uns, chrec1, chrec2);
	res = chrec_convert (type, res, at_stmt);
      }
      break;

    CASE_CONVERT:
      /* In case we have a truncation of a widened operation that in
         the truncated type has undefined overflow behavior analyze
	 the operation done in an unsigned type of the same precision
	 as the final truncation.  We cannot derive a scalar evolution
	 for the widened operation but for the truncated result.  */
      if (TREE_CODE (type) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (rhs1)) == INTEGER_TYPE
	  && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (rhs1))
	  && TYPE_OVERFLOW_UNDEFINED (type)
	  && TREE_CODE (rhs1) == SSA_NAME
	  && (def = SSA_NAME_DEF_STMT (rhs1))
	  && is_gimple_assign (def)
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (def)) == tcc_binary
	  && TREE_CODE (gimple_assign_rhs2 (def)) == INTEGER_CST)
	{
	  tree utype = unsigned_type_for (type);
	  chrec1 = interpret_rhs_expr (loop, at_stmt, utype,
				       gimple_assign_rhs1 (def),
				       gimple_assign_rhs_code (def),
				       gimple_assign_rhs2 (def));
	}
      else
	chrec1 = analyze_scalar_evolution (loop, rhs1);
      res = chrec_convert (type, chrec1, at_stmt, true, rhs1);
      break;

    case BIT_AND_EXPR:
      /* Given int variable A, handle A&0xffff as (int)(unsigned short)A.
	 If A is SCEV and its value is in the range of representable set
	 of type unsigned short, the result expression is a (no-overflow)
	 SCEV.  */
      res = chrec_dont_know;
      if (tree_fits_uhwi_p (rhs2))
	{
	  int precision;
	  unsigned HOST_WIDE_INT val = tree_to_uhwi (rhs2);

	  val ++;
	  /* Skip if value of rhs2 wraps in unsigned HOST_WIDE_INT or
	     it's not the maximum value of a smaller type than rhs1.  */
	  if (val != 0
	      && (precision = exact_log2 (val)) > 0
	      && (unsigned) precision < TYPE_PRECISION (TREE_TYPE (rhs1)))
	    {
	      tree utype = build_nonstandard_integer_type (precision, 1);

	      if (TYPE_PRECISION (utype) < TYPE_PRECISION (TREE_TYPE (rhs1)))
		{
		  chrec1 = analyze_scalar_evolution (loop, rhs1);
		  chrec1 = chrec_convert (utype, chrec1, at_stmt);
		  res = chrec_convert (TREE_TYPE (rhs1), chrec1, at_stmt);
		}
	    }
	}
      break;

    default:
      res = chrec_dont_know;
      break;
    }

  return res;
}

/* Interpret the expression EXPR.  */

static tree
interpret_expr (class loop *loop, gimple *at_stmt, tree expr)
{
  enum tree_code code;
  tree type = TREE_TYPE (expr), op0, op1;

  if (automatically_generated_chrec_p (expr))
    return expr;

  if (TREE_CODE (expr) == POLYNOMIAL_CHREC
      || TREE_CODE (expr) == CALL_EXPR
      || get_gimple_rhs_class (TREE_CODE (expr)) == GIMPLE_TERNARY_RHS)
    return chrec_dont_know;

  extract_ops_from_tree (expr, &code, &op0, &op1);

  return interpret_rhs_expr (loop, at_stmt, type,
			     op0, code, op1);
}

/* Interpret the rhs of the assignment STMT.  */

static tree
interpret_gimple_assign (class loop *loop, gimple *stmt)
{
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  enum tree_code code = gimple_assign_rhs_code (stmt);

  return interpret_rhs_expr (loop, stmt, type,
			     gimple_assign_rhs1 (stmt), code,
			     gimple_assign_rhs2 (stmt));
}



/* This section contains all the entry points:
   - number_of_iterations_in_loop,
   - analyze_scalar_evolution,
   - instantiate_parameters.
*/

/* Helper recursive function.  */

static tree
analyze_scalar_evolution_1 (class loop *loop, tree var)
{
  gimple *def;
  basic_block bb;
  class loop *def_loop;
  tree res;

  if (TREE_CODE (var) != SSA_NAME)
    return interpret_expr (loop, NULL, var);

  def = SSA_NAME_DEF_STMT (var);
  bb = gimple_bb (def);
  def_loop = bb->loop_father;

  if (!flow_bb_inside_loop_p (loop, bb))
    {
      /* Keep symbolic form, but look through obvious copies for constants.  */
      res = follow_copies_to_constant (var);
      goto set_and_end;
    }

  if (loop != def_loop)
    {
      res = analyze_scalar_evolution_1 (def_loop, var);
      class loop *loop_to_skip = superloop_at_depth (def_loop,
						      loop_depth (loop) + 1);
      res = compute_overall_effect_of_inner_loop (loop_to_skip, res);
      if (chrec_contains_symbols_defined_in_loop (res, loop->num))
	res = analyze_scalar_evolution_1 (loop, res);
      goto set_and_end;
    }

  switch (gimple_code (def))
    {
    case GIMPLE_ASSIGN:
      res = interpret_gimple_assign (loop, def);
      break;

    case GIMPLE_PHI:
      if (loop_phi_node_p (def))
	res = interpret_loop_phi (loop, as_a <gphi *> (def));
      else
	res = interpret_condition_phi (loop, as_a <gphi *> (def));
      break;

    default:
      res = chrec_dont_know;
      break;
    }

 set_and_end:

  /* Keep the symbolic form.  */
  if (res == chrec_dont_know)
    res = var;

  if (loop == def_loop)
    set_scalar_evolution (block_before_loop (loop), var, res);

  return res;
}

/* Analyzes and returns the scalar evolution of the ssa_name VAR in
   LOOP.  LOOP is the loop in which the variable is used.

   Example of use: having a pointer VAR to a SSA_NAME node, STMT a
   pointer to the statement that uses this variable, in order to
   determine the evolution function of the variable, use the following
   calls:

   loop_p loop = loop_containing_stmt (stmt);
   tree chrec_with_symbols = analyze_scalar_evolution (loop, var);
   tree chrec_instantiated = instantiate_parameters (loop, chrec_with_symbols);
*/

tree
analyze_scalar_evolution (class loop *loop, tree var)
{
  tree res;

  /* ???  Fix callers.  */
  if (! loop)
    return var;

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "(analyze_scalar_evolution \n");
      fprintf (dump_file, "  (loop_nb = %d)\n", loop->num);
      fprintf (dump_file, "  (scalar = ");
      print_generic_expr (dump_file, var);
      fprintf (dump_file, ")\n");
    }

  res = get_scalar_evolution (block_before_loop (loop), var);
  if (res == chrec_not_analyzed_yet)
    {
      /* We'll recurse into instantiate_scev, avoid tearing down the
         instantiate cache repeatedly and keep it live from here.  */
      bool destr = false;
      if (!global_cache)
	{
	  global_cache = new instantiate_cache_type;
	  destr = true;
	}
      res = analyze_scalar_evolution_1 (loop, var);
      if (destr)
	{
	  delete global_cache;
	  global_cache = NULL;
	}
    }

  if (dump_file && (dump_flags & TDF_SCEV))
    fprintf (dump_file, ")\n");

  return res;
}

/* If CHREC doesn't overflow, set the nonwrapping flag.  */

void record_nonwrapping_chrec (tree chrec)
{
  CHREC_NOWRAP(chrec) = 1;

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "(record_nonwrapping_chrec: ");
      print_generic_expr (dump_file, chrec);
      fprintf (dump_file, ")\n");
    }
}

/* Return true if CHREC's nonwrapping flag is set.  */

bool nonwrapping_chrec_p (tree chrec)
{
  if (!chrec || TREE_CODE(chrec) != POLYNOMIAL_CHREC)
    return false;

  return CHREC_NOWRAP(chrec);
}

/* Analyzes and returns the scalar evolution of VAR address in LOOP.  */

static tree
analyze_scalar_evolution_for_address_of (class loop *loop, tree var)
{
  return analyze_scalar_evolution (loop, build_fold_addr_expr (var));
}

/* Analyze scalar evolution of use of VERSION in USE_LOOP with respect to
   WRTO_LOOP (which should be a superloop of USE_LOOP)

   FOLDED_CASTS is set to true if resolve_mixers used
   chrec_convert_aggressive (TODO -- not really, we are way too conservative
   at the moment in order to keep things simple).

   To illustrate the meaning of USE_LOOP and WRTO_LOOP, consider the following
   example:

   for (i = 0; i < 100; i++)			-- loop 1
     {
       for (j = 0; j < 100; j++)		-- loop 2
         {
	   k1 = i;
	   k2 = j;

	   use2 (k1, k2);

	   for (t = 0; t < 100; t++)		-- loop 3
	     use3 (k1, k2);

	 }
       use1 (k1, k2);
     }

   Both k1 and k2 are invariants in loop3, thus
     analyze_scalar_evolution_in_loop (loop3, loop3, k1) = k1
     analyze_scalar_evolution_in_loop (loop3, loop3, k2) = k2

   As they are invariant, it does not matter whether we consider their
   usage in loop 3 or loop 2, hence
     analyze_scalar_evolution_in_loop (loop2, loop3, k1) =
       analyze_scalar_evolution_in_loop (loop2, loop2, k1) = i
     analyze_scalar_evolution_in_loop (loop2, loop3, k2) =
       analyze_scalar_evolution_in_loop (loop2, loop2, k2) = [0,+,1]_2

   Similarly for their evolutions with respect to loop 1.  The values of K2
   in the use in loop 2 vary independently on loop 1, thus we cannot express
   the evolution with respect to loop 1:
     analyze_scalar_evolution_in_loop (loop1, loop3, k1) =
       analyze_scalar_evolution_in_loop (loop1, loop2, k1) = [0,+,1]_1
     analyze_scalar_evolution_in_loop (loop1, loop3, k2) =
       analyze_scalar_evolution_in_loop (loop1, loop2, k2) = dont_know

   The value of k2 in the use in loop 1 is known, though:
     analyze_scalar_evolution_in_loop (loop1, loop1, k1) = [0,+,1]_1
     analyze_scalar_evolution_in_loop (loop1, loop1, k2) = 100
   */

static tree
analyze_scalar_evolution_in_loop (class loop *wrto_loop, class loop *use_loop,
				  tree version, bool *folded_casts)
{
  bool val = false;
  tree ev = version, tmp;

  /* We cannot just do

     tmp = analyze_scalar_evolution (use_loop, version);
     ev = resolve_mixers (wrto_loop, tmp, folded_casts);

     as resolve_mixers would query the scalar evolution with respect to
     wrto_loop.  For example, in the situation described in the function
     comment, suppose that wrto_loop = loop1, use_loop = loop3 and
     version = k2.  Then

     analyze_scalar_evolution (use_loop, version) = k2

     and resolve_mixers (loop1, k2, folded_casts) finds that the value of
     k2 in loop 1 is 100, which is a wrong result, since we are interested
     in the value in loop 3.

     Instead, we need to proceed from use_loop to wrto_loop loop by loop,
     each time checking that there is no evolution in the inner loop.  */

  if (folded_casts)
    *folded_casts = false;
  while (1)
    {
      tmp = analyze_scalar_evolution (use_loop, ev);
      ev = resolve_mixers (use_loop, tmp, folded_casts);

      if (use_loop == wrto_loop)
	return ev;

      /* If the value of the use changes in the inner loop, we cannot express
	 its value in the outer loop (we might try to return interval chrec,
	 but we do not have a user for it anyway)  */
      if (!no_evolution_in_loop_p (ev, use_loop->num, &val)
	  || !val)
	return chrec_dont_know;

      use_loop = loop_outer (use_loop);
    }
}


/* Computes a hash function for database element ELT.  */

static inline hashval_t
hash_idx_scev_info (const void *elt_)
{
  unsigned idx = ((size_t) elt_) - 2;
  return scev_info_hasher::hash (&global_cache->entries[idx]);
}

/* Compares database elements E1 and E2.  */

static inline int
eq_idx_scev_info (const void *e1, const void *e2)
{
  unsigned idx1 = ((size_t) e1) - 2;
  return scev_info_hasher::equal (&global_cache->entries[idx1],
				  (const scev_info_str *) e2);
}

/* Returns from CACHE the slot number of the cached chrec for NAME.  */

static unsigned
get_instantiated_value_entry (instantiate_cache_type &cache,
			      tree name, edge instantiate_below)
{
  if (!cache.map)
    {
      cache.map = htab_create (10, hash_idx_scev_info, eq_idx_scev_info, NULL);
      cache.entries.create (10);
    }

  scev_info_str e;
  e.name_version = SSA_NAME_VERSION (name);
  e.instantiated_below = instantiate_below->dest->index;
  void **slot = htab_find_slot_with_hash (cache.map, &e,
					  scev_info_hasher::hash (&e), INSERT);
  if (!*slot)
    {
      e.chrec = chrec_not_analyzed_yet;
      *slot = (void *)(size_t)(cache.entries.length () + 2);
      cache.entries.safe_push (e);
    }

  return ((size_t)*slot) - 2;
}


/* Return the closed_loop_phi node for VAR.  If there is none, return
   NULL_TREE.  */

static tree
loop_closed_phi_def (tree var)
{
  class loop *loop;
  edge exit;
  gphi *phi;
  gphi_iterator psi;

  if (var == NULL_TREE
      || TREE_CODE (var) != SSA_NAME)
    return NULL_TREE;

  loop = loop_containing_stmt (SSA_NAME_DEF_STMT (var));
  exit = single_exit (loop);
  if (!exit)
    return NULL_TREE;

  for (psi = gsi_start_phis (exit->dest); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = psi.phi ();
      if (PHI_ARG_DEF_FROM_EDGE (phi, exit) == var)
	return PHI_RESULT (phi);
    }

  return NULL_TREE;
}

static tree instantiate_scev_r (edge, class loop *, class loop *,
				tree, bool *, int);

/* Analyze all the parameters of the chrec, between INSTANTIATE_BELOW
   and EVOLUTION_LOOP, that were left under a symbolic form.

   CHREC is an SSA_NAME to be instantiated.

   CACHE is the cache of already instantiated values.

   Variable pointed by FOLD_CONVERSIONS is set to TRUE when the
   conversions that may wrap in signed/pointer type are folded, as long
   as the value of the chrec is preserved.  If FOLD_CONVERSIONS is NULL
   then we don't do such fold.

   SIZE_EXPR is used for computing the size of the expression to be
   instantiated, and to stop if it exceeds some limit.  */

static tree
instantiate_scev_name (edge instantiate_below,
		       class loop *evolution_loop, class loop *inner_loop,
		       tree chrec,
		       bool *fold_conversions,
		       int size_expr)
{
  tree res;
  class loop *def_loop;
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (chrec));

  /* A parameter, nothing to do.  */
  if (!def_bb
      || !dominated_by_p (CDI_DOMINATORS, def_bb, instantiate_below->dest))
    return chrec;

  /* We cache the value of instantiated variable to avoid exponential
     time complexity due to reevaluations.  We also store the convenient
     value in the cache in order to prevent infinite recursion -- we do
     not want to instantiate the SSA_NAME if it is in a mixer
     structure.  This is used for avoiding the instantiation of
     recursively defined functions, such as:

     | a_2 -> {0, +, 1, +, a_2}_1  */

  unsigned si = get_instantiated_value_entry (*global_cache,
					      chrec, instantiate_below);
  if (global_cache->get (si) != chrec_not_analyzed_yet)
    return global_cache->get (si);

  /* On recursion return chrec_dont_know.  */
  global_cache->set (si, chrec_dont_know);

  def_loop = find_common_loop (evolution_loop, def_bb->loop_father);

  if (! dominated_by_p (CDI_DOMINATORS,
			def_loop->header, instantiate_below->dest))
    {
      gimple *def = SSA_NAME_DEF_STMT (chrec);
      if (gassign *ass = dyn_cast <gassign *> (def))
	{
	  switch (gimple_assign_rhs_class (ass))
	    {
	    case GIMPLE_UNARY_RHS:
	      {
		tree op0 = instantiate_scev_r (instantiate_below, evolution_loop,
					       inner_loop, gimple_assign_rhs1 (ass),
					       fold_conversions, size_expr);
		if (op0 == chrec_dont_know)
		  return chrec_dont_know;
		res = fold_build1 (gimple_assign_rhs_code (ass),
				   TREE_TYPE (chrec), op0);
		break;
	      }
	    case GIMPLE_BINARY_RHS:
	      {
		tree op0 = instantiate_scev_r (instantiate_below, evolution_loop,
					       inner_loop, gimple_assign_rhs1 (ass),
					       fold_conversions, size_expr);
		if (op0 == chrec_dont_know)
		  return chrec_dont_know;
		tree op1 = instantiate_scev_r (instantiate_below, evolution_loop,
					       inner_loop, gimple_assign_rhs2 (ass),
					       fold_conversions, size_expr);
		if (op1 == chrec_dont_know)
		  return chrec_dont_know;
		res = fold_build2 (gimple_assign_rhs_code (ass),
				   TREE_TYPE (chrec), op0, op1);
		break;
	      }
	    default:
	      res = chrec_dont_know;
	    }
	}
      else
	res = chrec_dont_know;
      global_cache->set (si, res);
      return res;
    }

  /* If the analysis yields a parametric chrec, instantiate the
     result again.  */
  res = analyze_scalar_evolution (def_loop, chrec);

  /* Don't instantiate default definitions.  */
  if (TREE_CODE (res) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (res))
    ;

  /* Don't instantiate loop-closed-ssa phi nodes.  */
  else if (TREE_CODE (res) == SSA_NAME
	   && loop_depth (loop_containing_stmt (SSA_NAME_DEF_STMT (res)))
	   > loop_depth (def_loop))
    {
      if (res == chrec)
	res = loop_closed_phi_def (chrec);
      else
	res = chrec;

      /* When there is no loop_closed_phi_def, it means that the
	 variable is not used after the loop: try to still compute the
	 value of the variable when exiting the loop.  */
      if (res == NULL_TREE)
	{
	  loop_p loop = loop_containing_stmt (SSA_NAME_DEF_STMT (chrec));
	  res = analyze_scalar_evolution (loop, chrec);
	  res = compute_overall_effect_of_inner_loop (loop, res);
	  res = instantiate_scev_r (instantiate_below, evolution_loop,
				    inner_loop, res,
				    fold_conversions, size_expr);
	}
      else if (dominated_by_p (CDI_DOMINATORS,
				gimple_bb (SSA_NAME_DEF_STMT (res)),
				instantiate_below->dest))
	res = chrec_dont_know;
    }

  else if (res != chrec_dont_know)
    {
      if (inner_loop
	  && def_bb->loop_father != inner_loop
	  && !flow_loop_nested_p (def_bb->loop_father, inner_loop))
	/* ???  We could try to compute the overall effect of the loop here.  */
	res = chrec_dont_know;
      else
	res = instantiate_scev_r (instantiate_below, evolution_loop,
				  inner_loop, res,
				  fold_conversions, size_expr);
    }

  /* Store the correct value to the cache.  */
  global_cache->set (si, res);
  return res;
}

/* Analyze all the parameters of the chrec, between INSTANTIATE_BELOW
   and EVOLUTION_LOOP, that were left under a symbolic form.

   CHREC is a polynomial chain of recurrence to be instantiated.

   CACHE is the cache of already instantiated values.

   Variable pointed by FOLD_CONVERSIONS is set to TRUE when the
   conversions that may wrap in signed/pointer type are folded, as long
   as the value of the chrec is preserved.  If FOLD_CONVERSIONS is NULL
   then we don't do such fold.

   SIZE_EXPR is used for computing the size of the expression to be
   instantiated, and to stop if it exceeds some limit.  */

static tree
instantiate_scev_poly (edge instantiate_below,
		       class loop *evolution_loop, class loop *,
		       tree chrec, bool *fold_conversions, int size_expr)
{
  tree op1;
  tree op0 = instantiate_scev_r (instantiate_below, evolution_loop,
				 get_chrec_loop (chrec),
				 CHREC_LEFT (chrec), fold_conversions,
				 size_expr);
  if (op0 == chrec_dont_know)
    return chrec_dont_know;

  op1 = instantiate_scev_r (instantiate_below, evolution_loop,
			    get_chrec_loop (chrec),
			    CHREC_RIGHT (chrec), fold_conversions,
			    size_expr);
  if (op1 == chrec_dont_know)
    return chrec_dont_know;

  if (CHREC_LEFT (chrec) != op0
      || CHREC_RIGHT (chrec) != op1)
    {
      op1 = chrec_convert_rhs (chrec_type (op0), op1, NULL);
      chrec = build_polynomial_chrec (CHREC_VARIABLE (chrec), op0, op1);
    }

  return chrec;
}

/* Analyze all the parameters of the chrec, between INSTANTIATE_BELOW
   and EVOLUTION_LOOP, that were left under a symbolic form.

   "C0 CODE C1" is a binary expression of type TYPE to be instantiated.

   CACHE is the cache of already instantiated values.

   Variable pointed by FOLD_CONVERSIONS is set to TRUE when the
   conversions that may wrap in signed/pointer type are folded, as long
   as the value of the chrec is preserved.  If FOLD_CONVERSIONS is NULL
   then we don't do such fold.

   SIZE_EXPR is used for computing the size of the expression to be
   instantiated, and to stop if it exceeds some limit.  */

static tree
instantiate_scev_binary (edge instantiate_below,
			 class loop *evolution_loop, class loop *inner_loop,
			 tree chrec, enum tree_code code,
			 tree type, tree c0, tree c1,
			 bool *fold_conversions, int size_expr)
{
  tree op1;
  tree op0 = instantiate_scev_r (instantiate_below, evolution_loop, inner_loop,
				 c0, fold_conversions, size_expr);
  if (op0 == chrec_dont_know)
    return chrec_dont_know;

  /* While we eventually compute the same op1 if c0 == c1 the process
     of doing this is expensive so the following short-cut prevents
     exponential compile-time behavior.  */
  if (c0 != c1)
    {
      op1 = instantiate_scev_r (instantiate_below, evolution_loop, inner_loop,
				c1, fold_conversions, size_expr);
      if (op1 == chrec_dont_know)
	return chrec_dont_know;
    }
  else
    op1 = op0;

  if (c0 != op0
      || c1 != op1)
    {
      op0 = chrec_convert (type, op0, NULL);
      op1 = chrec_convert_rhs (type, op1, NULL);

      switch (code)
	{
	case POINTER_PLUS_EXPR:
	case PLUS_EXPR:
	  return chrec_fold_plus (type, op0, op1);

	case MINUS_EXPR:
	  return chrec_fold_minus (type, op0, op1);

	case MULT_EXPR:
	  return chrec_fold_multiply (type, op0, op1);

	default:
	  gcc_unreachable ();
	}
    }

  return chrec ? chrec : fold_build2 (code, type, c0, c1);
}

/* Analyze all the parameters of the chrec, between INSTANTIATE_BELOW
   and EVOLUTION_LOOP, that were left under a symbolic form.

   "CHREC" that stands for a convert expression "(TYPE) OP" is to be
   instantiated.

   CACHE is the cache of already instantiated values.

   Variable pointed by FOLD_CONVERSIONS is set to TRUE when the
   conversions that may wrap in signed/pointer type are folded, as long
   as the value of the chrec is preserved.  If FOLD_CONVERSIONS is NULL
   then we don't do such fold.

   SIZE_EXPR is used for computing the size of the expression to be
   instantiated, and to stop if it exceeds some limit.  */

static tree
instantiate_scev_convert (edge instantiate_below,
			  class loop *evolution_loop, class loop *inner_loop,
			  tree chrec, tree type, tree op,
			  bool *fold_conversions, int size_expr)
{
  tree op0 = instantiate_scev_r (instantiate_below, evolution_loop,
				 inner_loop, op,
				 fold_conversions, size_expr);

  if (op0 == chrec_dont_know)
    return chrec_dont_know;

  if (fold_conversions)
    {
      tree tmp = chrec_convert_aggressive (type, op0, fold_conversions);
      if (tmp)
	return tmp;

      /* If we used chrec_convert_aggressive, we can no longer assume that
	 signed chrecs do not overflow, as chrec_convert does, so avoid
	 calling it in that case.  */
      if (*fold_conversions)
	{
	  if (chrec && op0 == op)
	    return chrec;

	  return fold_convert (type, op0);
	}
    }

  return chrec_convert (type, op0, NULL);
}

/* Analyze all the parameters of the chrec, between INSTANTIATE_BELOW
   and EVOLUTION_LOOP, that were left under a symbolic form.

   CHREC is a BIT_NOT_EXPR or a NEGATE_EXPR expression to be instantiated.
   Handle ~X as -1 - X.
   Handle -X as -1 * X.

   CACHE is the cache of already instantiated values.

   Variable pointed by FOLD_CONVERSIONS is set to TRUE when the
   conversions that may wrap in signed/pointer type are folded, as long
   as the value of the chrec is preserved.  If FOLD_CONVERSIONS is NULL
   then we don't do such fold.

   SIZE_EXPR is used for computing the size of the expression to be
   instantiated, and to stop if it exceeds some limit.  */

static tree
instantiate_scev_not (edge instantiate_below,
		      class loop *evolution_loop, class loop *inner_loop,
		      tree chrec,
		      enum tree_code code, tree type, tree op,
		      bool *fold_conversions, int size_expr)
{
  tree op0 = instantiate_scev_r (instantiate_below, evolution_loop,
				 inner_loop, op,
				 fold_conversions, size_expr);

  if (op0 == chrec_dont_know)
    return chrec_dont_know;

  if (op != op0)
    {
      op0 = chrec_convert (type, op0, NULL);

      switch (code)
	{
	case BIT_NOT_EXPR:
	  return chrec_fold_minus
	    (type, fold_convert (type, integer_minus_one_node), op0);

	case NEGATE_EXPR:
	  return chrec_fold_multiply
	    (type, fold_convert (type, integer_minus_one_node), op0);

	default:
	  gcc_unreachable ();
	}
    }

  return chrec ? chrec : fold_build1 (code, type, op0);
}

/* Analyze all the parameters of the chrec, between INSTANTIATE_BELOW
   and EVOLUTION_LOOP, that were left under a symbolic form.

   CHREC is the scalar evolution to instantiate.

   CACHE is the cache of already instantiated values.

   Variable pointed by FOLD_CONVERSIONS is set to TRUE when the
   conversions that may wrap in signed/pointer type are folded, as long
   as the value of the chrec is preserved.  If FOLD_CONVERSIONS is NULL
   then we don't do such fold.

   SIZE_EXPR is used for computing the size of the expression to be
   instantiated, and to stop if it exceeds some limit.  */

static tree
instantiate_scev_r (edge instantiate_below,
		    class loop *evolution_loop, class loop *inner_loop,
		    tree chrec,
		    bool *fold_conversions, int size_expr)
{
  /* Give up if the expression is larger than the MAX that we allow.  */
  if (size_expr++ > param_scev_max_expr_size)
    return chrec_dont_know;

  if (chrec == NULL_TREE
      || automatically_generated_chrec_p (chrec)
      || is_gimple_min_invariant (chrec))
    return chrec;

  switch (TREE_CODE (chrec))
    {
    case SSA_NAME:
      return instantiate_scev_name (instantiate_below, evolution_loop,
				    inner_loop, chrec,
				    fold_conversions, size_expr);

    case POLYNOMIAL_CHREC:
      return instantiate_scev_poly (instantiate_below, evolution_loop,
				    inner_loop, chrec,
				    fold_conversions, size_expr);

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
      return instantiate_scev_binary (instantiate_below, evolution_loop,
				      inner_loop, chrec,
				      TREE_CODE (chrec), chrec_type (chrec),
				      TREE_OPERAND (chrec, 0),
				      TREE_OPERAND (chrec, 1),
				      fold_conversions, size_expr);

    CASE_CONVERT:
      return instantiate_scev_convert (instantiate_below, evolution_loop,
				       inner_loop, chrec,
				       TREE_TYPE (chrec), TREE_OPERAND (chrec, 0),
				       fold_conversions, size_expr);

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
      return instantiate_scev_not (instantiate_below, evolution_loop,
				   inner_loop, chrec,
				   TREE_CODE (chrec), TREE_TYPE (chrec),
				   TREE_OPERAND (chrec, 0),
				   fold_conversions, size_expr);

    case ADDR_EXPR:
      if (is_gimple_min_invariant (chrec))
	return chrec;
      /* Fallthru.  */
    case SCEV_NOT_KNOWN:
      return chrec_dont_know;

    case SCEV_KNOWN:
      return chrec_known;

    default:
      if (CONSTANT_CLASS_P (chrec))
	return chrec;
      return chrec_dont_know;
    }
}

/* Analyze all the parameters of the chrec that were left under a
   symbolic form.  INSTANTIATE_BELOW is the basic block that stops the
   recursive instantiation of parameters: a parameter is a variable
   that is defined in a basic block that dominates INSTANTIATE_BELOW or
   a function parameter.  */

tree
instantiate_scev (edge instantiate_below, class loop *evolution_loop,
		  tree chrec)
{
  tree res;

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "(instantiate_scev \n");
      fprintf (dump_file, "  (instantiate_below = %d -> %d)\n",
	       instantiate_below->src->index, instantiate_below->dest->index);
      if (evolution_loop)
	fprintf (dump_file, "  (evolution_loop = %d)\n", evolution_loop->num);
      fprintf (dump_file, "  (chrec = ");
      print_generic_expr (dump_file, chrec);
      fprintf (dump_file, ")\n");
    }

  bool destr = false;
  if (!global_cache)
    {
      global_cache = new instantiate_cache_type;
      destr = true;
    }

  res = instantiate_scev_r (instantiate_below, evolution_loop,
			    NULL, chrec, NULL, 0);

  if (destr)
    {
      delete global_cache;
      global_cache = NULL;
    }

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "  (res = ");
      print_generic_expr (dump_file, res);
      fprintf (dump_file, "))\n");
    }

  return res;
}

/* Similar to instantiate_parameters, but does not introduce the
   evolutions in outer loops for LOOP invariants in CHREC, and does not
   care about causing overflows, as long as they do not affect value
   of an expression.  */

tree
resolve_mixers (class loop *loop, tree chrec, bool *folded_casts)
{
  bool destr = false;
  bool fold_conversions = false;
  if (!global_cache)
    {
      global_cache = new instantiate_cache_type;
      destr = true;
    }

  tree ret = instantiate_scev_r (loop_preheader_edge (loop), loop, NULL,
				 chrec, &fold_conversions, 0);

  if (folded_casts && !*folded_casts)
    *folded_casts = fold_conversions;

  if (destr)
    {
      delete global_cache;
      global_cache = NULL;
    }

  return ret;
}

/* Entry point for the analysis of the number of iterations pass.
   This function tries to safely approximate the number of iterations
   the loop will run.  When this property is not decidable at compile
   time, the result is chrec_dont_know.  Otherwise the result is a
   scalar or a symbolic parameter.  When the number of iterations may
   be equal to zero and the property cannot be determined at compile
   time, the result is a COND_EXPR that represents in a symbolic form
   the conditions under which the number of iterations is not zero.

   Example of analysis: suppose that the loop has an exit condition:

   "if (b > 49) goto end_loop;"

   and that in a previous analysis we have determined that the
   variable 'b' has an evolution function:

   "EF = {23, +, 5}_2".

   When we evaluate the function at the point 5, i.e. the value of the
   variable 'b' after 5 iterations in the loop, we have EF (5) = 48,
   and EF (6) = 53.  In this case the value of 'b' on exit is '53' and
   the loop body has been executed 6 times.  */

tree
number_of_latch_executions (class loop *loop)
{
  edge exit;
  class tree_niter_desc niter_desc;
  tree may_be_zero;
  tree res;

  /* Determine whether the number of iterations in loop has already
     been computed.  */
  res = loop->nb_iterations;
  if (res)
    return res;

  may_be_zero = NULL_TREE;

  if (dump_file && (dump_flags & TDF_SCEV))
    fprintf (dump_file, "(number_of_iterations_in_loop = \n");

  res = chrec_dont_know;
  exit = single_exit (loop);

  if (exit && number_of_iterations_exit (loop, exit, &niter_desc, false))
    {
      may_be_zero = niter_desc.may_be_zero;
      res = niter_desc.niter;
    }

  if (res == chrec_dont_know
      || !may_be_zero
      || integer_zerop (may_be_zero))
    ;
  else if (integer_nonzerop (may_be_zero))
    res = build_int_cst (TREE_TYPE (res), 0);

  else if (COMPARISON_CLASS_P (may_be_zero))
    res = fold_build3 (COND_EXPR, TREE_TYPE (res), may_be_zero,
		       build_int_cst (TREE_TYPE (res), 0), res);
  else
    res = chrec_dont_know;

  if (dump_file && (dump_flags & TDF_SCEV))
    {
      fprintf (dump_file, "  (set_nb_iterations_in_loop = ");
      print_generic_expr (dump_file, res);
      fprintf (dump_file, "))\n");
    }

  loop->nb_iterations = res;
  return res;
}


/* Counters for the stats.  */

struct chrec_stats
{
  unsigned nb_chrecs;
  unsigned nb_affine;
  unsigned nb_affine_multivar;
  unsigned nb_higher_poly;
  unsigned nb_chrec_dont_know;
  unsigned nb_undetermined;
};

/* Reset the counters.  */

static inline void
reset_chrecs_counters (struct chrec_stats *stats)
{
  stats->nb_chrecs = 0;
  stats->nb_affine = 0;
  stats->nb_affine_multivar = 0;
  stats->nb_higher_poly = 0;
  stats->nb_chrec_dont_know = 0;
  stats->nb_undetermined = 0;
}

/* Dump the contents of a CHREC_STATS structure.  */

static void
dump_chrecs_stats (FILE *file, struct chrec_stats *stats)
{
  fprintf (file, "\n(\n");
  fprintf (file, "-----------------------------------------\n");
  fprintf (file, "%d\taffine univariate chrecs\n", stats->nb_affine);
  fprintf (file, "%d\taffine multivariate chrecs\n", stats->nb_affine_multivar);
  fprintf (file, "%d\tdegree greater than 2 polynomials\n",
	   stats->nb_higher_poly);
  fprintf (file, "%d\tchrec_dont_know chrecs\n", stats->nb_chrec_dont_know);
  fprintf (file, "-----------------------------------------\n");
  fprintf (file, "%d\ttotal chrecs\n", stats->nb_chrecs);
  fprintf (file, "%d\twith undetermined coefficients\n",
	   stats->nb_undetermined);
  fprintf (file, "-----------------------------------------\n");
  fprintf (file, "%d\tchrecs in the scev database\n",
	   (int) scalar_evolution_info->elements ());
  fprintf (file, "%d\tsets in the scev database\n", nb_set_scev);
  fprintf (file, "%d\tgets in the scev database\n", nb_get_scev);
  fprintf (file, "-----------------------------------------\n");
  fprintf (file, ")\n\n");
}

/* Gather statistics about CHREC.  */

static void
gather_chrec_stats (tree chrec, struct chrec_stats *stats)
{
  if (dump_file && (dump_flags & TDF_STATS))
    {
      fprintf (dump_file, "(classify_chrec ");
      print_generic_expr (dump_file, chrec);
      fprintf (dump_file, "\n");
    }

  stats->nb_chrecs++;

  if (chrec == NULL_TREE)
    {
      stats->nb_undetermined++;
      return;
    }

  switch (TREE_CODE (chrec))
    {
    case POLYNOMIAL_CHREC:
      if (evolution_function_is_affine_p (chrec))
	{
	  if (dump_file && (dump_flags & TDF_STATS))
	    fprintf (dump_file, "  affine_univariate\n");
	  stats->nb_affine++;
	}
      else if (evolution_function_is_affine_multivariate_p (chrec, 0))
	{
	  if (dump_file && (dump_flags & TDF_STATS))
	    fprintf (dump_file, "  affine_multivariate\n");
	  stats->nb_affine_multivar++;
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_STATS))
	    fprintf (dump_file, "  higher_degree_polynomial\n");
	  stats->nb_higher_poly++;
	}

      break;

    default:
      break;
    }

  if (chrec_contains_undetermined (chrec))
    {
      if (dump_file && (dump_flags & TDF_STATS))
	fprintf (dump_file, "  undetermined\n");
      stats->nb_undetermined++;
    }

  if (dump_file && (dump_flags & TDF_STATS))
    fprintf (dump_file, ")\n");
}

/* Classify the chrecs of the whole database.  */

void
gather_stats_on_scev_database (void)
{
  struct chrec_stats stats;

  if (!dump_file)
    return;

  reset_chrecs_counters (&stats);

  hash_table<scev_info_hasher>::iterator iter;
  scev_info_str *elt;
  FOR_EACH_HASH_TABLE_ELEMENT (*scalar_evolution_info, elt, scev_info_str *,
			       iter)
    gather_chrec_stats (elt->chrec, &stats);

  dump_chrecs_stats (dump_file, &stats);
}


/* Initialize the analysis of scalar evolutions for LOOPS.  */

void
scev_initialize (void)
{
  gcc_assert (! scev_initialized_p ()
	      && loops_state_satisfies_p (cfun, LOOPS_NORMAL));

  scalar_evolution_info = hash_table<scev_info_hasher>::create_ggc (100);

  for (auto loop : loops_list (cfun, 0))
    loop->nb_iterations = NULL_TREE;
}

/* Return true if SCEV is initialized.  */

bool
scev_initialized_p (void)
{
  return scalar_evolution_info != NULL;
}

/* Cleans up the information cached by the scalar evolutions analysis
   in the hash table.  */

void
scev_reset_htab (void)
{
  if (!scalar_evolution_info)
    return;

  scalar_evolution_info->empty ();
}

/* Cleans up the information cached by the scalar evolutions analysis
   in the hash table and in the loop->nb_iterations.  */

void
scev_reset (void)
{
  scev_reset_htab ();

  for (auto loop : loops_list (cfun, 0))
    loop->nb_iterations = NULL_TREE;
}

/* Return true if the IV calculation in TYPE can overflow based on the knowledge
   of the upper bound on the number of iterations of LOOP, the BASE and STEP
   of IV.

   We do not use information whether TYPE can overflow so it is safe to
   use this test even for derived IVs not computed every iteration or
   hypotetical IVs to be inserted into code.  */

bool
iv_can_overflow_p (class loop *loop, tree type, tree base, tree step)
{
  widest_int nit;
  wide_int base_min, base_max, step_min, step_max, type_min, type_max;
  signop sgn = TYPE_SIGN (type);
  int_range_max r;

  if (integer_zerop (step))
    return false;

  if (!INTEGRAL_TYPE_P (TREE_TYPE (base))
      || !get_range_query (cfun)->range_of_expr (r, base)
      || r.varying_p ()
      || r.undefined_p ())
    return true;

  base_min = r.lower_bound ();
  base_max = r.upper_bound ();

  if (!INTEGRAL_TYPE_P (TREE_TYPE (step))
      || !get_range_query (cfun)->range_of_expr (r, step)
      || r.varying_p ()
      || r.undefined_p ())
    return true;

  step_min = r.lower_bound ();
  step_max = r.upper_bound ();

  if (!get_max_loop_iterations (loop, &nit))
    return true;

  type_min = wi::min_value (type);
  type_max = wi::max_value (type);

  /* Just sanity check that we don't see values out of the range of the type.
     In this case the arithmetics bellow would overflow.  */
  gcc_checking_assert (wi::ge_p (base_min, type_min, sgn)
		       && wi::le_p (base_max, type_max, sgn));

  /* Account the possible increment in the last ieration.  */
  wi::overflow_type overflow = wi::OVF_NONE;
  nit = wi::add (nit, 1, SIGNED, &overflow);
  if (overflow)
    return true;

  /* NIT is typeless and can exceed the precision of the type.  In this case
     overflow is always possible, because we know STEP is non-zero.  */
  if (wi::min_precision (nit, UNSIGNED) > TYPE_PRECISION (type))
    return true;
  wide_int nit2 = wide_int::from (nit, TYPE_PRECISION (type), UNSIGNED);

  /* If step can be positive, check that nit*step <= type_max-base.
     This can be done by unsigned arithmetic and we only need to watch overflow
     in the multiplication. The right hand side can always be represented in
     the type.  */
  if (sgn == UNSIGNED || !wi::neg_p (step_max))
    {
      wi::overflow_type overflow = wi::OVF_NONE;
      if (wi::gtu_p (wi::mul (step_max, nit2, UNSIGNED, &overflow),
		     type_max - base_max)
	  || overflow)
	return true;
    }
  /* If step can be negative, check that nit*(-step) <= base_min-type_min.  */
  if (sgn == SIGNED && wi::neg_p (step_min))
    {
      wi::overflow_type overflow, overflow2;
      overflow = overflow2 = wi::OVF_NONE;
      if (wi::gtu_p (wi::mul (wi::neg (step_min, &overflow2),
		     nit2, UNSIGNED, &overflow),
		     base_min - type_min)
	  || overflow || overflow2)
        return true;
    }

  return false;
}

/* Given EV with form of "(type) {inner_base, inner_step}_loop", this
   function tries to derive condition under which it can be simplified
   into "{(type)inner_base, (type)inner_step}_loop".  The condition is
   the maximum number that inner iv can iterate.  */

static tree
derive_simple_iv_with_niters (tree ev, tree *niters)
{
  if (!CONVERT_EXPR_P (ev))
    return ev;

  tree inner_ev = TREE_OPERAND (ev, 0);
  if (TREE_CODE (inner_ev) != POLYNOMIAL_CHREC)
    return ev;

  tree init = CHREC_LEFT (inner_ev);
  tree step = CHREC_RIGHT (inner_ev);
  if (TREE_CODE (init) != INTEGER_CST
      || TREE_CODE (step) != INTEGER_CST || integer_zerop (step))
    return ev;

  tree type = TREE_TYPE (ev);
  tree inner_type = TREE_TYPE (inner_ev);
  if (TYPE_PRECISION (inner_type) >= TYPE_PRECISION (type))
    return ev;

  /* Type conversion in "(type) {inner_base, inner_step}_loop" can be
     folded only if inner iv won't overflow.  We compute the maximum
     number the inner iv can iterate before overflowing and return the
     simplified affine iv.  */
  tree delta;
  init = fold_convert (type, init);
  step = fold_convert (type, step);
  ev = build_polynomial_chrec (CHREC_VARIABLE (inner_ev), init, step);
  if (tree_int_cst_sign_bit (step))
    {
      tree bound = lower_bound_in_type (inner_type, inner_type);
      delta = fold_build2 (MINUS_EXPR, type, init, fold_convert (type, bound));
      step = fold_build1 (NEGATE_EXPR, type, step);
    }
  else
    {
      tree bound = upper_bound_in_type (inner_type, inner_type);
      delta = fold_build2 (MINUS_EXPR, type, fold_convert (type, bound), init);
    }
  *niters = fold_build2 (FLOOR_DIV_EXPR, type, delta, step);
  return ev;
}

/* Checks whether use of OP in USE_LOOP behaves as a simple affine iv with
   respect to WRTO_LOOP and returns its base and step in IV if possible
   (see analyze_scalar_evolution_in_loop for more details on USE_LOOP
   and WRTO_LOOP).  If ALLOW_NONCONSTANT_STEP is true, we want step to be
   invariant in LOOP.  Otherwise we require it to be an integer constant.

   IV->no_overflow is set to true if we are sure the iv cannot overflow (e.g.
   because it is computed in signed arithmetics).  Consequently, adding an
   induction variable

   for (i = IV->base; ; i += IV->step)

   is only safe if IV->no_overflow is false, or TYPE_OVERFLOW_UNDEFINED is
   false for the type of the induction variable, or you can prove that i does
   not wrap by some other argument.  Otherwise, this might introduce undefined
   behavior, and

   i = iv->base;
   for (; ; i = (type) ((unsigned type) i + (unsigned type) iv->step))

   must be used instead.

   When IV_NITERS is not NULL, this function also checks case in which OP
   is a conversion of an inner simple iv of below form:

     (outer_type){inner_base, inner_step}_loop.

   If type of inner iv has smaller precision than outer_type, it can't be
   folded into {(outer_type)inner_base, (outer_type)inner_step}_loop because
   the inner iv could overflow/wrap.  In this case, we derive a condition
   under which the inner iv won't overflow/wrap and do the simplification.
   The derived condition normally is the maximum number the inner iv can
   iterate, and will be stored in IV_NITERS.  This is useful in loop niter
   analysis, to derive break conditions when a loop must terminate, when is
   infinite.  */

bool
simple_iv_with_niters (class loop *wrto_loop, class loop *use_loop,
		       tree op, affine_iv *iv, tree *iv_niters,
		       bool allow_nonconstant_step)
{
  enum tree_code code;
  tree type, ev, base, e;
  wide_int extreme;
  bool folded_casts;

  iv->base = NULL_TREE;
  iv->step = NULL_TREE;
  iv->no_overflow = false;

  type = TREE_TYPE (op);
  if (!POINTER_TYPE_P (type)
      && !INTEGRAL_TYPE_P (type))
    return false;

  ev = analyze_scalar_evolution_in_loop (wrto_loop, use_loop, op,
					 &folded_casts);
  if (chrec_contains_undetermined (ev)
      || chrec_contains_symbols_defined_in_loop (ev, wrto_loop->num))
    return false;

  if (tree_does_not_contain_chrecs (ev))
    {
      iv->base = ev;
      tree ev_type = TREE_TYPE (ev);
      if (POINTER_TYPE_P (ev_type))
	ev_type = sizetype;

      iv->step = build_int_cst (ev_type, 0);
      iv->no_overflow = true;
      return true;
    }

  /* If we can derive valid scalar evolution with assumptions.  */
  if (iv_niters && TREE_CODE (ev) != POLYNOMIAL_CHREC)
    ev = derive_simple_iv_with_niters (ev, iv_niters);

  if (TREE_CODE (ev) != POLYNOMIAL_CHREC)
    return false;

  if (CHREC_VARIABLE (ev) != (unsigned) wrto_loop->num)
    return false;

  iv->step = CHREC_RIGHT (ev);
  if ((!allow_nonconstant_step && TREE_CODE (iv->step) != INTEGER_CST)
      || tree_contains_chrecs (iv->step, NULL))
    return false;

  iv->base = CHREC_LEFT (ev);
  if (tree_contains_chrecs (iv->base, NULL))
    return false;

  iv->no_overflow = !folded_casts && nowrap_type_p (type);

  if (!iv->no_overflow
      && !iv_can_overflow_p (wrto_loop, type, iv->base, iv->step))
    iv->no_overflow = true;

  /* Try to simplify iv base:

       (signed T) ((unsigned T)base + step) ;; TREE_TYPE (base) == signed T
	 == (signed T)(unsigned T)base + step
	 == base + step

     If we can prove operation (base + step) doesn't overflow or underflow.
     Specifically, we try to prove below conditions are satisfied:

	     base <= UPPER_BOUND (type) - step  ;;step > 0
	     base >= LOWER_BOUND (type) - step  ;;step < 0

     This is done by proving the reverse conditions are false using loop's
     initial conditions.

     The is necessary to make loop niter, or iv overflow analysis easier
     for below example:

       int foo (int *a, signed char s, signed char l)
	 {
	   signed char i;
	   for (i = s; i < l; i++)
	     a[i] = 0;
	   return 0;
	  }

     Note variable I is firstly converted to type unsigned char, incremented,
     then converted back to type signed char.  */

  if (wrto_loop->num != use_loop->num)
    return true;

  if (!CONVERT_EXPR_P (iv->base) || TREE_CODE (iv->step) != INTEGER_CST)
    return true;

  type = TREE_TYPE (iv->base);
  e = TREE_OPERAND (iv->base, 0);
  if (!tree_nop_conversion_p (type, TREE_TYPE (e))
      || TREE_CODE (e) != PLUS_EXPR
      || TREE_CODE (TREE_OPERAND (e, 1)) != INTEGER_CST
      || !tree_int_cst_equal (iv->step,
			      fold_convert (type, TREE_OPERAND (e, 1))))
    return true;
  e = TREE_OPERAND (e, 0);
  if (!CONVERT_EXPR_P (e))
    return true;
  base = TREE_OPERAND (e, 0);
  if (!useless_type_conversion_p (type, TREE_TYPE (base)))
    return true;

  if (tree_int_cst_sign_bit (iv->step))
    {
      code = LT_EXPR;
      extreme = wi::min_value (type);
    }
  else
    {
      code = GT_EXPR;
      extreme = wi::max_value (type);
    }
  wi::overflow_type overflow = wi::OVF_NONE;
  extreme = wi::sub (extreme, wi::to_wide (iv->step),
		     TYPE_SIGN (type), &overflow);
  if (overflow)
    return true;
  e = fold_build2 (code, boolean_type_node, base,
		   wide_int_to_tree (type, extreme));
  e = simplify_using_initial_conditions (use_loop, e);
  if (!integer_zerop (e))
    return true;

  if (POINTER_TYPE_P (TREE_TYPE (base)))
    code = POINTER_PLUS_EXPR;
  else
    code = PLUS_EXPR;

  iv->base = fold_build2 (code, TREE_TYPE (base), base, iv->step);
  return true;
}

/* Like simple_iv_with_niters, but return TRUE when OP behaves as a simple
   affine iv unconditionally.  */

bool
simple_iv (class loop *wrto_loop, class loop *use_loop, tree op,
	   affine_iv *iv, bool allow_nonconstant_step)
{
  return simple_iv_with_niters (wrto_loop, use_loop, op, iv,
				NULL, allow_nonconstant_step);
}

/* Finalize the scalar evolution analysis.  */

void
scev_finalize (void)
{
  if (!scalar_evolution_info)
    return;
  scalar_evolution_info->empty ();
  scalar_evolution_info = NULL;
  free_numbers_of_iterations_estimates (cfun);
}

/* Returns true if the expression EXPR is considered to be too expensive
   for scev_const_prop.  Sets *COND_OVERFLOW_P to true when the
   expression might contain a sub-expression that is subject to undefined
   overflow behavior and conditionally evaluated.  */

static bool
expression_expensive_p (tree expr, bool *cond_overflow_p,
			hash_map<tree, uint64_t> &cache, uint64_t &cost)
{
  enum tree_code code;

  if (is_gimple_val (expr))
    return false;

  code = TREE_CODE (expr);
  if (code == TRUNC_DIV_EXPR
      || code == CEIL_DIV_EXPR
      || code == FLOOR_DIV_EXPR
      || code == ROUND_DIV_EXPR
      || code == TRUNC_MOD_EXPR
      || code == CEIL_MOD_EXPR
      || code == FLOOR_MOD_EXPR
      || code == ROUND_MOD_EXPR
      || code == EXACT_DIV_EXPR)
    {
      /* Division by power of two is usually cheap, so we allow it.
	 Forbid anything else.  */
      if (!integer_pow2p (TREE_OPERAND (expr, 1)))
	return true;
    }

  bool visited_p;
  uint64_t &local_cost = cache.get_or_insert (expr, &visited_p);
  if (visited_p)
    {
      uint64_t tem = cost + local_cost;
      if (tem < cost)
	return true;
      cost = tem;
      return false;
    }
  local_cost = 1;

  uint64_t op_cost = 0;
  if (code == CALL_EXPR)
    {
      tree arg;
      call_expr_arg_iterator iter;
      /* Even though is_inexpensive_builtin might say true, we will get a
	 library call for popcount when backend does not have an instruction
	 to do so.  We consider this to be expensive and generate
	 __builtin_popcount only when backend defines it.  */
      optab optab;
      combined_fn cfn = get_call_combined_fn (expr);
      switch (cfn)
	{
	CASE_CFN_POPCOUNT:
	  optab = popcount_optab;
	  goto bitcount_call;
	CASE_CFN_CLZ:
	  optab = clz_optab;
	  goto bitcount_call;
	CASE_CFN_CTZ:
	  optab = ctz_optab;
bitcount_call:
	  /* Check if opcode for popcount is available in the mode required.  */
	  if (optab_handler (optab,
			     TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (expr, 0))))
	      == CODE_FOR_nothing)
	    {
	      machine_mode mode;
	      mode = TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (expr, 0)));
	      scalar_int_mode int_mode;

	      /* If the mode is of 2 * UNITS_PER_WORD size, we can handle
		 double-word popcount by emitting two single-word popcount
		 instructions.  */
	      if (is_a <scalar_int_mode> (mode, &int_mode)
		  && GET_MODE_SIZE (int_mode) == 2 * UNITS_PER_WORD
		  && (optab_handler (optab, word_mode)
		      != CODE_FOR_nothing))
		  break;
	      /* If popcount is available for a wider mode, we emulate the
		 operation for a narrow mode by first zero-extending the value
		 and then computing popcount in the wider mode.  Analogue for
		 ctz.  For clz we do the same except that we additionally have
		 to subtract the difference of the mode precisions from the
		 result.  */
	      if (is_a <scalar_int_mode> (mode, &int_mode))
		{
		  machine_mode wider_mode_iter;
		  FOR_EACH_WIDER_MODE (wider_mode_iter, mode)
		    if (optab_handler (optab, wider_mode_iter)
			!= CODE_FOR_nothing)
		      goto check_call_args;
		  /* Operation ctz may be emulated via clz in expand_ctz.  */
		  if (optab == ctz_optab)
		    {
		      FOR_EACH_WIDER_MODE_FROM (wider_mode_iter, mode)
			if (optab_handler (clz_optab, wider_mode_iter)
			    != CODE_FOR_nothing)
			  goto check_call_args;
		    }
		}
	      return true;
	    }
	  break;

	default:
	  if (cfn == CFN_LAST
	      || !is_inexpensive_builtin (get_callee_fndecl (expr)))
	    return true;
	  break;
	}

check_call_args:
      FOR_EACH_CALL_EXPR_ARG (arg, iter, expr)
	if (expression_expensive_p (arg, cond_overflow_p, cache, op_cost))
	  return true;
      *cache.get (expr) += op_cost;
      cost += op_cost + 1;
      return false;
    }

  if (code == COND_EXPR)
    {
      if (expression_expensive_p (TREE_OPERAND (expr, 0), cond_overflow_p,
				  cache, op_cost)
	  || (EXPR_P (TREE_OPERAND (expr, 1))
	      && EXPR_P (TREE_OPERAND (expr, 2)))
	  /* If either branch has side effects or could trap.  */
	  || TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
	  || generic_expr_could_trap_p (TREE_OPERAND (expr, 1))
	  || TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 0))
	  || generic_expr_could_trap_p (TREE_OPERAND (expr, 0))
	  || expression_expensive_p (TREE_OPERAND (expr, 1), cond_overflow_p,
				     cache, op_cost)
	  || expression_expensive_p (TREE_OPERAND (expr, 2), cond_overflow_p,
				     cache, op_cost))
	return true;
      /* Conservatively assume there's overflow for now.  */
      *cond_overflow_p = true;
      *cache.get (expr) += op_cost;
      cost += op_cost + 1;
      return false;
    }

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_binary:
    case tcc_comparison:
      if (expression_expensive_p (TREE_OPERAND (expr, 1), cond_overflow_p,
				  cache, op_cost))
	return true;

      /* Fallthru.  */
    case tcc_unary:
      if (expression_expensive_p (TREE_OPERAND (expr, 0), cond_overflow_p,
				  cache, op_cost))
	return true;
      *cache.get (expr) += op_cost;
      cost += op_cost + 1;
      return false;

    default:
      return true;
    }
}

bool
expression_expensive_p (tree expr, bool *cond_overflow_p)
{
  hash_map<tree, uint64_t> cache;
  uint64_t expanded_size = 0;
  *cond_overflow_p = false;
  return (expression_expensive_p (expr, cond_overflow_p, cache, expanded_size)
	  /* ???  Both the explicit unsharing and gimplification of expr will
	     expand shared trees to multiple copies.
	     Guard against exponential growth by counting the visits and
	     comparing againt the number of original nodes.  Allow a tiny
	     bit of duplication to catch some additional optimizations.  */
	  || expanded_size > (cache.elements () + 1));
}

/* Match.pd function to match bitwise inductive expression.
   .i.e.
   _2 = 1 << _1;
   _3 = ~_2;
   tmp_9 = _3 & tmp_12;  */
extern bool gimple_bitwise_induction_p (tree, tree *, tree (*)(tree));

/* Return the inductive expression of bitwise operation if possible,
   otherwise returns DEF.  */
static tree
analyze_and_compute_bitwise_induction_effect (class loop* loop,
					      tree phidef,
					      unsigned HOST_WIDE_INT niter)
{
  tree match_op[3],inv, bitwise_scev;
  tree type = TREE_TYPE (phidef);
  gphi* header_phi = NULL;

  /* Match things like op2(MATCH_OP[2]), op1(MATCH_OP[1]), phidef(PHIDEF)

     op2 = PHI <phidef, inv>
     _1 = (int) bit_17;
     _3 = 1 << _1;
     op1 = ~_3;
     phidef = op1 & op2;  */
  if (!gimple_bitwise_induction_p (phidef, &match_op[0], NULL)
      || TREE_CODE (match_op[2]) != SSA_NAME
      || !(header_phi = dyn_cast <gphi *> (SSA_NAME_DEF_STMT (match_op[2])))
      || gimple_bb (header_phi) != loop->header
      || gimple_phi_num_args (header_phi) != 2)
    return NULL_TREE;

  if (PHI_ARG_DEF_FROM_EDGE (header_phi, loop_latch_edge (loop)) != phidef)
    return NULL_TREE;

  bitwise_scev = analyze_scalar_evolution (loop, match_op[1]);
  bitwise_scev = instantiate_parameters (loop, bitwise_scev);

  /* Make sure bits is in range of type precision.  */
  if (TREE_CODE (bitwise_scev) != POLYNOMIAL_CHREC
      || !INTEGRAL_TYPE_P (TREE_TYPE (bitwise_scev))
      || !tree_fits_uhwi_p (CHREC_LEFT (bitwise_scev))
      || tree_to_uhwi (CHREC_LEFT (bitwise_scev)) >= TYPE_PRECISION (type)
      || !tree_fits_shwi_p (CHREC_RIGHT (bitwise_scev)))
    return NULL_TREE;

enum bit_op_kind
  {
   INDUCTION_BIT_CLEAR,
   INDUCTION_BIT_IOR,
   INDUCTION_BIT_XOR,
   INDUCTION_BIT_RESET,
   INDUCTION_ZERO,
   INDUCTION_ALL
  };

  enum bit_op_kind induction_kind;
  enum tree_code code1
    = gimple_assign_rhs_code (SSA_NAME_DEF_STMT (phidef));
  enum tree_code code2
    = gimple_assign_rhs_code (SSA_NAME_DEF_STMT (match_op[0]));

  /* BIT_CLEAR: A &= ~(1 << bit)
     BIT_RESET: A ^= (1 << bit).
     BIT_IOR: A |= (1 << bit)
     BIT_ZERO: A &= (1 << bit)
     BIT_ALL: A |= ~(1 << bit)
     BIT_XOR: A ^= ~(1 << bit).
     bit is induction variable.  */
  switch (code1)
    {
    case BIT_AND_EXPR:
      induction_kind = code2 == BIT_NOT_EXPR
	? INDUCTION_BIT_CLEAR
	: INDUCTION_ZERO;
      break;
    case BIT_IOR_EXPR:
      induction_kind = code2 == BIT_NOT_EXPR
	? INDUCTION_ALL
	: INDUCTION_BIT_IOR;
      break;
    case BIT_XOR_EXPR:
      induction_kind = code2 == BIT_NOT_EXPR
	? INDUCTION_BIT_XOR
	: INDUCTION_BIT_RESET;
      break;
      /* A ^ ~(1 << bit) is equal to ~(A ^ (1 << bit)).  */
    case BIT_NOT_EXPR:
      gcc_assert (code2 == BIT_XOR_EXPR);
      induction_kind = INDUCTION_BIT_XOR;
      break;
    default:
      gcc_unreachable ();
    }

  if (induction_kind == INDUCTION_ZERO)
    return build_zero_cst (type);
  if (induction_kind == INDUCTION_ALL)
    return build_all_ones_cst (type);

  wide_int bits = wi::zero (TYPE_PRECISION (type));
  HOST_WIDE_INT bit_start = tree_to_shwi (CHREC_LEFT (bitwise_scev));
  HOST_WIDE_INT step = tree_to_shwi (CHREC_RIGHT (bitwise_scev));
  HOST_WIDE_INT bit_final = bit_start + step * niter;

  /* bit_start, bit_final in range of [0,TYPE_PRECISION)
     implies all bits are set in range.  */
  if (bit_final >= TYPE_PRECISION (type)
      || bit_final < 0)
    return NULL_TREE;

  /* Loop tripcount should be niter + 1.  */
  for (unsigned i = 0; i != niter + 1; i++)
    {
      bits = wi::set_bit (bits, bit_start);
      bit_start += step;
    }

  bool inverted = false;
  switch (induction_kind)
    {
    case INDUCTION_BIT_CLEAR:
      code1 = BIT_AND_EXPR;
      inverted = true;
      break;
    case INDUCTION_BIT_IOR:
      code1 = BIT_IOR_EXPR;
      break;
    case INDUCTION_BIT_RESET:
      code1 = BIT_XOR_EXPR;
      break;
    /* A ^= ~(1 << bit) is special, when loop tripcount is even,
       it's equal to  A ^= bits, else A ^= ~bits.  */
    case INDUCTION_BIT_XOR:
      code1 = BIT_XOR_EXPR;
      if (niter % 2 == 0)
	inverted = true;
      break;
    default:
      gcc_unreachable ();
    }

  if (inverted)
    bits = wi::bit_not (bits);

  inv = PHI_ARG_DEF_FROM_EDGE (header_phi, loop_preheader_edge (loop));
  return fold_build2 (code1, type, inv, wide_int_to_tree (type, bits));
}

/* Match.pd function to match bitop with invariant expression
  .i.e.
  tmp_7 = _0 & _1; */
extern bool gimple_bitop_with_inv_p (tree, tree *, tree (*)(tree));

/* Return the inductive expression of bitop with invariant if possible,
   otherwise returns DEF.  */
static tree
analyze_and_compute_bitop_with_inv_effect (class loop* loop, tree phidef,
					   tree niter)
{
  tree match_op[2],inv;
  tree type = TREE_TYPE (phidef);
  gphi* header_phi = NULL;
  enum tree_code code;
  /* match thing like op0 (match[0]), op1 (match[1]), phidef (PHIDEF)

    op1 =  PHI <phidef, inv>
    phidef = op0 & op1
    if op0 is an invariant, it could change to
    phidef = op0 & inv.  */
  gimple *def;
  def = SSA_NAME_DEF_STMT (phidef);
  if (!(is_gimple_assign (def)
      && ((code = gimple_assign_rhs_code (def)), true)
      && (code == BIT_AND_EXPR || code == BIT_IOR_EXPR
	  || code == BIT_XOR_EXPR)))
    return NULL_TREE;

  match_op[0] = gimple_assign_rhs1 (def);
  match_op[1] = gimple_assign_rhs2 (def);

  if (expr_invariant_in_loop_p (loop, match_op[1]))
    std::swap (match_op[0], match_op[1]);

  if (TREE_CODE (match_op[1]) != SSA_NAME
      || !expr_invariant_in_loop_p (loop, match_op[0])
      || !(header_phi = dyn_cast <gphi *> (SSA_NAME_DEF_STMT (match_op[1])))
      || gimple_bb (header_phi) != loop->header
      || gimple_phi_num_args (header_phi) != 2)
    return NULL_TREE;

  if (PHI_ARG_DEF_FROM_EDGE (header_phi, loop_latch_edge (loop)) != phidef)
    return NULL_TREE;

  enum tree_code code1
    = gimple_assign_rhs_code (def);

  if (code1 == BIT_XOR_EXPR)
    {
       if (!tree_fits_uhwi_p (niter))
	return NULL_TREE;
       unsigned HOST_WIDE_INT niter_num;
       niter_num = tree_to_uhwi (niter);
       if (niter_num % 2 != 0)
	match_op[0] =  build_zero_cst (type);
    }

  inv = PHI_ARG_DEF_FROM_EDGE (header_phi, loop_preheader_edge (loop));
  return fold_build2 (code1, type, inv, match_op[0]);
}

/* Do final value replacement for LOOP, return true if we did anything.  */

bool
final_value_replacement_loop (class loop *loop)
{
  /* If we do not know exact number of iterations of the loop, we cannot
     replace the final value.  */
  edge exit = single_exit (loop);
  if (!exit)
    return false;

  tree niter = number_of_latch_executions (loop);
  if (niter == chrec_dont_know)
    return false;

  /* Ensure that it is possible to insert new statements somewhere.  */
  if (!single_pred_p (exit->dest))
    split_loop_exit_edge (exit);

  /* Set stmt insertion pointer.  All stmts are inserted before this point.  */

  class loop *ex_loop
    = superloop_at_depth (loop,
			  loop_depth (exit->dest->loop_father) + 1);

  bool any = false;
  gphi_iterator psi;
  for (psi = gsi_start_phis (exit->dest); !gsi_end_p (psi); )
    {
      gphi *phi = psi.phi ();
      tree rslt = PHI_RESULT (phi);
      tree phidef = PHI_ARG_DEF_FROM_EDGE (phi, exit);
      tree def = phidef;
      if (virtual_operand_p (def))
	{
	  gsi_next (&psi);
	  continue;
	}

      if (!POINTER_TYPE_P (TREE_TYPE (def))
	  && !INTEGRAL_TYPE_P (TREE_TYPE (def)))
	{
	  gsi_next (&psi);
	  continue;
	}

      bool folded_casts;
      def = analyze_scalar_evolution_in_loop (ex_loop, loop, def,
					      &folded_casts);

      tree bitinv_def, bit_def;
      unsigned HOST_WIDE_INT niter_num;

      if (def != chrec_dont_know)
	def = compute_overall_effect_of_inner_loop (ex_loop, def);

      /* Handle bitop with invariant induction expression.

	.i.e
	for (int i =0 ;i < 32; i++)
	  tmp &= bit2;
	if bit2 is an invariant in loop which could simple to
	tmp &= bit2.  */
      else if ((bitinv_def
		= analyze_and_compute_bitop_with_inv_effect (loop,
							     phidef, niter)))
	def = bitinv_def;

      /* Handle bitwise induction expression.

	 .i.e.
	 for (int i = 0; i != 64; i+=3)
	   res &= ~(1UL << i);

	 RES can't be analyzed out by SCEV because it is not polynomially
	 expressible, but in fact final value of RES can be replaced by
	 RES & CONSTANT where CONSTANT all ones with bit {0,3,6,9,... ,63}
	 being cleared, similar for BIT_IOR_EXPR/BIT_XOR_EXPR.  */
      else if (tree_fits_uhwi_p (niter)
	       && (niter_num = tree_to_uhwi (niter)) != 0
	       && niter_num < TYPE_PRECISION (TREE_TYPE (phidef))
	       && (bit_def
		   = analyze_and_compute_bitwise_induction_effect (loop,
								   phidef,
								   niter_num)))
	def = bit_def;

      bool cond_overflow_p;
      if (!tree_does_not_contain_chrecs (def)
	  || chrec_contains_symbols_defined_in_loop (def, ex_loop->num)
	  /* Moving the computation from the loop may prolong life range
	     of some ssa names, which may cause problems if they appear
	     on abnormal edges.  */
	  || contains_abnormal_ssa_name_p (def)
	  /* Do not emit expensive expressions.  The rationale is that
	     when someone writes a code like

	     while (n > 45) n -= 45;

	     he probably knows that n is not large, and does not want it
	     to be turned into n %= 45.  */
	  || expression_expensive_p (def, &cond_overflow_p))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "not replacing:\n  ");
	      print_gimple_stmt (dump_file, phi, 0);
	      fprintf (dump_file, "\n");
	    }
	  gsi_next (&psi);
	  continue;
	}

      /* Eliminate the PHI node and replace it by a computation outside
	 the loop.  */
      if (dump_file)
	{
	  fprintf (dump_file, "\nfinal value replacement:\n  ");
	  print_gimple_stmt (dump_file, phi, 0);
	  fprintf (dump_file, " with expr: ");
	  print_generic_expr (dump_file, def);
	  fprintf (dump_file, "\n");
	}
      any = true;
      /* ???  Here we'd like to have a unshare_expr that would assign
	 shared sub-trees to new temporary variables either gimplified
	 to a GIMPLE sequence or to a statement list (keeping this a
	 GENERIC interface).  */
      def = unshare_expr (def);
      auto loc = gimple_phi_arg_location (phi, exit->dest_idx);
      remove_phi_node (&psi, false);

      /* Propagate constants immediately, but leave an unused initialization
	 around to avoid invalidating the SCEV cache.  */
      if (CONSTANT_CLASS_P (def) && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rslt))
	replace_uses_by (rslt, def);

      /* Create the replacement statements.  */
      gimple_seq stmts;
      def = force_gimple_operand (def, &stmts, false, NULL_TREE);
      gassign *ass = gimple_build_assign (rslt, def);
      gimple_set_location (ass, loc);
      gimple_seq_add_stmt (&stmts, ass);

      /* If def's type has undefined overflow and there were folded
	 casts, rewrite all stmts added for def into arithmetics
	 with defined overflow behavior.  */
      if ((folded_casts
	   && ANY_INTEGRAL_TYPE_P (TREE_TYPE (def))
	   && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (def)))
	  || cond_overflow_p)
	{
	  gimple_stmt_iterator gsi2;
	  gsi2 = gsi_start (stmts);
	  while (!gsi_end_p (gsi2))
	    {
	      gimple *stmt = gsi_stmt (gsi2);
	      if (is_gimple_assign (stmt)
		  && arith_code_with_undefined_signed_overflow
		       (gimple_assign_rhs_code (stmt)))
		rewrite_to_defined_overflow (&gsi2);
	      gsi_next (&gsi2);
	    }
	}
      gimple_stmt_iterator gsi = gsi_after_labels (exit->dest);
      gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
      if (dump_file)
	{
	  fprintf (dump_file, " final stmt:\n  ");
	  print_gimple_stmt (dump_file, SSA_NAME_DEF_STMT (rslt), 0);
	  fprintf (dump_file, "\n");
	}

      /* Re-fold immediate uses of the replaced def, but avoid
	 CFG manipulations from this function.  For now only do
	 a single-level re-folding, not re-folding uses of
	 folded uses.  */
      if (! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rslt))
	{
	  gimple *use_stmt;
	  imm_use_iterator imm_iter;
	  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, rslt)
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	      if (!stmt_can_throw_internal (cfun, use_stmt)
		  && fold_stmt (&gsi, follow_all_ssa_edges))
		update_stmt (gsi_stmt (gsi));
	    }
	}
    }

  return any;
}

#include "gt-tree-scalar-evolution.h"
