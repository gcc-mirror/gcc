/* Scalar evolution detector.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <s.pop@laposte.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

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
     
   - When the definition is a MODIFY_EXPR: if the right hand side
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
   instantiate_parameters ({a + 1, +, 1}_1), that gives after some
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
     
   Example 2: Illustration of the algorithm on nested loops.
     
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
     
   instantiate_parameters ({5, +, a}_1) -> {5, +, 2, +, 1}_1
   instantiate_parameters ({5 + a, +, a}_1) -> {7, +, 3, +, 1}_1
     
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
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"

/* These RTL headers are needed for basic-block.h.  */
#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "flags.h"

static tree analyze_scalar_evolution_1 (struct loop *, tree, tree);
static tree resolve_mixers (struct loop *, tree);

/* The cached information about a ssa name VAR, claiming that inside LOOP,
   the value of VAR can be expressed as CHREC.  */

struct scev_info_str
{
  tree var;
  tree chrec;
};

/* Counters for the scev database.  */
static unsigned nb_set_scev = 0;
static unsigned nb_get_scev = 0;

/* The following trees are unique elements.  Thus the comparison of
   another element to these elements should be done on the pointer to
   these trees, and not on their value.  */

/* The SSA_NAMEs that are not yet analyzed are qualified with NULL_TREE.  */
tree chrec_not_analyzed_yet;

/* Reserved to the cases where the analyzer has detected an
   undecidable property at compile time.  */
tree chrec_dont_know;

/* When the analyzer has detected that a property will never
   happen, then it qualifies it with chrec_known.  */
tree chrec_known;

static bitmap already_instantiated;

static htab_t scalar_evolution_info;


/* Constructs a new SCEV_INFO_STR structure.  */

static inline struct scev_info_str *
new_scev_info_str (tree var)
{
  struct scev_info_str *res;
  
  res = xmalloc (sizeof (struct scev_info_str));
  res->var = var;
  res->chrec = chrec_not_analyzed_yet;
  
  return res;
}

/* Computes a hash function for database element ELT.  */

static hashval_t
hash_scev_info (const void *elt)
{
  return SSA_NAME_VERSION (((struct scev_info_str *) elt)->var);
}

/* Compares database elements E1 and E2.  */

static int
eq_scev_info (const void *e1, const void *e2)
{
  const struct scev_info_str *elt1 = e1;
  const struct scev_info_str *elt2 = e2;

  return elt1->var == elt2->var;
}

/* Deletes database element E.  */

static void
del_scev_info (void *e)
{
  free (e);
}

/* Get the index corresponding to VAR in the current LOOP.  If
   it's the first time we ask for this VAR, then we return
   chrec_not_analyzed_yet for this VAR and return its index.  */

static tree *
find_var_scev_info (tree var)
{
  struct scev_info_str *res;
  struct scev_info_str tmp;
  PTR *slot;

  tmp.var = var;
  slot = htab_find_slot (scalar_evolution_info, &tmp, INSERT);

  if (!*slot)
    *slot = new_scev_info_str (var);
  res = *slot;

  return &res->chrec;
}

/* Tries to express CHREC in wider type TYPE.  */

tree
count_ev_in_wider_type (tree type, tree chrec)
{
  tree base, step;
  struct loop *loop;

  if (!evolution_function_is_affine_p (chrec))
    return fold_convert (type, chrec);

  base = CHREC_LEFT (chrec);
  step = CHREC_RIGHT (chrec);
  loop = current_loops->parray[CHREC_VARIABLE (chrec)];

  /* TODO -- if we knew the statement at that the conversion occurs,
     we could pass it to can_count_iv_in_wider_type and get a better
     result.  */
  step = can_count_iv_in_wider_type (loop, type, base, step, NULL_TREE);
  if (!step)
    return fold_convert (type, chrec);
  base = chrec_convert (type, base);

  return build_polynomial_chrec (CHREC_VARIABLE (chrec),
				 base, step);
}

/* Return true when CHREC contains symbolic names defined in
   LOOP_NB.  */

bool 
chrec_contains_symbols_defined_in_loop (tree chrec, unsigned loop_nb)
{
  if (chrec == NULL_TREE)
    return false;

  if (TREE_INVARIANT (chrec))
    return false;

  if (TREE_CODE (chrec) == VAR_DECL
      || TREE_CODE (chrec) == PARM_DECL
      || TREE_CODE (chrec) == FUNCTION_DECL
      || TREE_CODE (chrec) == LABEL_DECL
      || TREE_CODE (chrec) == RESULT_DECL
      || TREE_CODE (chrec) == FIELD_DECL)
    return true;

  if (TREE_CODE (chrec) == SSA_NAME)
    {
      tree def = SSA_NAME_DEF_STMT (chrec);
      struct loop *def_loop = loop_containing_stmt (def);
      struct loop *loop = current_loops->parray[loop_nb];

      if (def_loop == NULL)
	return false;

      if (loop == def_loop || flow_loop_nested_p (loop, def_loop))
	return true;

      return false;
    }

  switch (TREE_CODE_LENGTH (TREE_CODE (chrec)))
    {
    case 3:
      if (chrec_contains_symbols_defined_in_loop (TREE_OPERAND (chrec, 2), 
						  loop_nb))
	return true;

    case 2:
      if (chrec_contains_symbols_defined_in_loop (TREE_OPERAND (chrec, 1), 
						  loop_nb))
	return true;

    case 1:
      if (chrec_contains_symbols_defined_in_loop (TREE_OPERAND (chrec, 0), 
						  loop_nb))
	return true;

    default:
      return false;
    }
}

/* Return true when PHI is a loop-phi-node.  */

static bool
loop_phi_node_p (tree phi)
{
  /* The implementation of this function is based on the following
     property: "all the loop-phi-nodes of a loop are contained in the
     loop's header basic block".  */

  return loop_containing_stmt (phi)->header == bb_for_stmt (phi);
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
 
static tree 
compute_overall_effect_of_inner_loop (struct loop *loop, tree evolution_fn)
{
  bool val = false;

  if (evolution_fn == chrec_dont_know)
    return chrec_dont_know;

  else if (TREE_CODE (evolution_fn) == POLYNOMIAL_CHREC)
    {
      if (CHREC_VARIABLE (evolution_fn) >= (unsigned) loop->num)
	{
	  struct loop *inner_loop = 
	    current_loops->parray[CHREC_VARIABLE (evolution_fn)];
	  tree nb_iter = number_of_iterations_in_loop (inner_loop);

	  if (nb_iter == chrec_dont_know)
	    return chrec_dont_know;
	  else
	    {
	      tree res;

	      /* Number of iterations is off by one (the ssa name we
		 analyze must be defined before the exit).  */
	      nb_iter = chrec_fold_minus (chrec_type (nb_iter),
				nb_iter,
				build_int_cst_type (chrec_type (nb_iter), 1));
	      
	      /* evolution_fn is the evolution function in LOOP.  Get
		 its value in the nb_iter-th iteration.  */
	      res = chrec_apply (inner_loop->num, evolution_fn, nb_iter);
	      
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

/* Determine whether the CHREC is always positive/negative.  If the expression
   cannot be statically analyzed, return false, otherwise set the answer into
   VALUE.  */

bool
chrec_is_positive (tree chrec, bool *value)
{
  bool value0, value1;
  bool value2;
  tree end_value;
  tree nb_iter;
  
  switch (TREE_CODE (chrec))
    {
    case POLYNOMIAL_CHREC:
      if (!chrec_is_positive (CHREC_LEFT (chrec), &value0)
	  || !chrec_is_positive (CHREC_RIGHT (chrec), &value1))
	return false;
     
      /* FIXME -- overflows.  */
      if (value0 == value1)
	{
	  *value = value0;
	  return true;
	}

      /* Otherwise the chrec is under the form: "{-197, +, 2}_1",
	 and the proof consists in showing that the sign never
	 changes during the execution of the loop, from 0 to
	 loop->nb_iterations.  */
      if (!evolution_function_is_affine_p (chrec))
	return false;

      nb_iter = number_of_iterations_in_loop
	(current_loops->parray[CHREC_VARIABLE (chrec)]);

      if (chrec_contains_undetermined (nb_iter))
	return false;

      nb_iter = chrec_fold_minus 
	(chrec_type (nb_iter), nb_iter,
	 build_int_cst (chrec_type (nb_iter), 1));

#if 0
      /* TODO -- If the test is after the exit, we may decrease the number of
	 iterations by one.  */
      if (after_exit)
	nb_iter = chrec_fold_minus 
		(chrec_type (nb_iter), nb_iter,
		 build_int_cst (chrec_type (nb_iter), 1));
#endif

      end_value = chrec_apply (CHREC_VARIABLE (chrec), chrec, nb_iter);
	      
      if (!chrec_is_positive (end_value, &value2))
	return false;
	
      *value = value0;
      return value0 == value1;
      
    case INTEGER_CST:
      *value = (tree_int_cst_sgn (chrec) == 1);
      return true;
      
    default:
      return false;
    }
}

/* Associate CHREC to SCALAR.  */

static void
set_scalar_evolution (tree scalar, tree chrec)
{
  tree *scalar_info;
 
  if (TREE_CODE (scalar) != SSA_NAME)
    return;

  scalar_info = find_var_scev_info (scalar);
  
  if (dump_file)
    {
      if (dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "(set_scalar_evolution \n");
	  fprintf (dump_file, "  (scalar = ");
	  print_generic_expr (dump_file, scalar, 0);
	  fprintf (dump_file, ")\n  (scalar_evolution = ");
	  print_generic_expr (dump_file, chrec, 0);
	  fprintf (dump_file, "))\n");
	}
      if (dump_flags & TDF_STATS)
	nb_set_scev++;
    }
  
  *scalar_info = chrec;
}

/* Retrieve the chrec associated to SCALAR in the LOOP.  */

static tree
get_scalar_evolution (tree scalar)
{
  tree res;
  
  if (dump_file)
    {
      if (dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "(get_scalar_evolution \n");
	  fprintf (dump_file, "  (scalar = ");
	  print_generic_expr (dump_file, scalar, 0);
	  fprintf (dump_file, ")\n");
	}
      if (dump_flags & TDF_STATS)
	nb_get_scev++;
    }
  
  switch (TREE_CODE (scalar))
    {
    case SSA_NAME:
      res = *find_var_scev_info (scalar);
      break;

    case REAL_CST:
    case INTEGER_CST:
      res = scalar;
      break;

    default:
      res = chrec_not_analyzed_yet;
      break;
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (scalar_evolution = ");
      print_generic_expr (dump_file, res, 0);
      fprintf (dump_file, "))\n");
    }
  
  return res;
}

/* Helper function for add_to_evolution.  Returns the evolution
   function for an assignment of the form "a = b + c", where "a" and
   "b" are on the strongly connected component.  CHREC_BEFORE is the
   information that we already have collected up to this point.
   TO_ADD is the evolution of "c".  
   
   When CHREC_BEFORE has an evolution part in LOOP_NB, add to this
   evolution the expression TO_ADD, otherwise construct an evolution
   part for this loop.  */

static tree
add_to_evolution_1 (unsigned loop_nb, 
		    tree chrec_before, 
		    tree to_add)
{
  switch (TREE_CODE (chrec_before))
    {
    case POLYNOMIAL_CHREC:
      if (CHREC_VARIABLE (chrec_before) <= loop_nb)
	{
	  unsigned var;
	  tree left, right;
	  tree type = chrec_type (chrec_before);
	  
	  /* When there is no evolution part in this loop, build it.  */
	  if (CHREC_VARIABLE (chrec_before) < loop_nb)
	    {
	      var = loop_nb;
	      left = chrec_before;
	      right = build_int_cst (type, 0);
	    }
	  else
	    {
	      var = CHREC_VARIABLE (chrec_before);
	      left = CHREC_LEFT (chrec_before);
	      right = CHREC_RIGHT (chrec_before);
	    }

	  return build_polynomial_chrec 
	    (var, left, chrec_fold_plus (type, right, to_add));
	}
      else
	/* Search the evolution in LOOP_NB.  */
	return build_polynomial_chrec 
	  (CHREC_VARIABLE (chrec_before),
	   add_to_evolution_1 (loop_nb, CHREC_LEFT (chrec_before), to_add),
	   CHREC_RIGHT (chrec_before));
      
    default:
      /* These nodes do not depend on a loop.  */
      if (chrec_before == chrec_dont_know)
	return chrec_dont_know;
      return build_polynomial_chrec (loop_nb, chrec_before, to_add);
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

static tree 
add_to_evolution (unsigned loop_nb, 
		  tree chrec_before,
		  enum tree_code code,
		  tree to_add)
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
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(add_to_evolution \n");
      fprintf (dump_file, "  (loop_nb = %d)\n", loop_nb);
      fprintf (dump_file, "  (chrec_before = ");
      print_generic_expr (dump_file, chrec_before, 0);
      fprintf (dump_file, ")\n  (to_add = ");
      print_generic_expr (dump_file, to_add, 0);
      fprintf (dump_file, ")\n");
    }

  if (code == MINUS_EXPR)
    to_add = chrec_fold_multiply (type, to_add, 
				  build_int_cst_type (type, -1));

  res = add_to_evolution_1 (loop_nb, chrec_before, to_add);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (res = ");
      print_generic_expr (dump_file, res, 0);
      fprintf (dump_file, "))\n");
    }

  return res;
}

/* Helper function.  */

static inline tree
set_nb_iterations_in_loop (struct loop *loop, 
			   tree res)
{
  res = chrec_fold_plus (chrec_type (res), res,
			 build_int_cst_type (chrec_type (res), 1));

  /* FIXME HWI: However we want to store one iteration less than the
     count of the loop in order to be compatible with the other
     nb_iter computations in loop-iv.  This also allows the
     representation of nb_iters that are equal to MAX_INT.  */
  if ((TREE_CODE (res) == INTEGER_CST && TREE_INT_CST_LOW (res) == 0)
      || TREE_OVERFLOW (res))
    res = chrec_dont_know;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (set_nb_iterations_in_loop = ");
      print_generic_expr (dump_file, res, 0);
      fprintf (dump_file, "))\n");
    }
  
  loop->nb_iterations = res;
  return res;
}



/* This section selects the loops that will be good candidates for the
   scalar evolution analysis.  For the moment, greedily select all the
   loop nests we could analyze.  */

/* Return true when it is possible to analyze the condition expression
   EXPR.  */

static bool
analyzable_condition (tree expr)
{
  tree condition;
  
  if (TREE_CODE (expr) != COND_EXPR)
    return false;
  
  condition = TREE_OPERAND (expr, 0);
  
  switch (TREE_CODE (condition))
    {
    case SSA_NAME:
      return true;
      
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      return true;
      
    default:
      return false;
    }
  
  return false;
}

/* For a loop with a single exit edge, return the COND_EXPR that
   guards the exit edge.  If the expression is too difficult to
   analyze, then give up.  */

tree 
get_loop_exit_condition (struct loop *loop)
{
  tree res = NULL_TREE;
  edge exit_edge = loop->single_exit;

  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(get_loop_exit_condition \n  ");
  
  if (exit_edge)
    {
      tree expr;
      
      expr = last_stmt (exit_edge->src);
      if (analyzable_condition (expr))
	res = expr;
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_generic_expr (dump_file, res, 0);
      fprintf (dump_file, ")\n");
    }
  
  return res;
}

/* Recursively determine and enqueue the exit conditions for a loop.  */

static void 
get_exit_conditions_rec (struct loop *loop, 
			 varray_type *exit_conditions)
{
  if (!loop)
    return;
  
  /* Recurse on the inner loops, then on the next (sibling) loops.  */
  get_exit_conditions_rec (loop->inner, exit_conditions);
  get_exit_conditions_rec (loop->next, exit_conditions);
  
  if (loop->single_exit)
    {
      tree loop_condition = get_loop_exit_condition (loop);
      
      if (loop_condition)
	VARRAY_PUSH_TREE (*exit_conditions, loop_condition);
    }
}

/* Select the candidate loop nests for the analysis.  This function
   initializes the EXIT_CONDITIONS array.  */

static void
select_loops_exit_conditions (struct loops *loops, 
			      varray_type *exit_conditions)
{
  struct loop *function_body = loops->parray[0];
  
  get_exit_conditions_rec (function_body->inner, exit_conditions);
}


/* Depth first search algorithm.  */

static bool follow_ssa_edge (struct loop *loop, tree, tree, tree *);

/* Follow the ssa edge into the right hand side RHS of an assignment.
   Return true if the strongly connected component has been found.  */

static bool
follow_ssa_edge_in_rhs (struct loop *loop,
			tree rhs, 
			tree halting_phi, 
			tree *evolution_of_loop)
{
  bool res = false;
  tree rhs0, rhs1;
  tree type_rhs = TREE_TYPE (rhs);
  
  /* The RHS is one of the following cases:
     - an SSA_NAME, 
     - an INTEGER_CST,
     - a PLUS_EXPR, 
     - a MINUS_EXPR,
     - other cases are not yet handled. 
  */
  switch (TREE_CODE (rhs))
    {
    case NOP_EXPR:
      /* This assignment is under the form "a_1 = (cast) rhs.  */
      res = follow_ssa_edge_in_rhs (loop, TREE_OPERAND (rhs, 0), halting_phi, 
				    evolution_of_loop);
      *evolution_of_loop = chrec_convert (TREE_TYPE (rhs), *evolution_of_loop);
      break;

    case INTEGER_CST:
      /* This assignment is under the form "a_1 = 7".  */
      res = false;
      break;
      
    case SSA_NAME:
      /* This assignment is under the form: "a_1 = b_2".  */
      res = follow_ssa_edge 
	(loop, SSA_NAME_DEF_STMT (rhs), halting_phi, evolution_of_loop);
      break;
      
    case PLUS_EXPR:
      /* This case is under the form "rhs0 + rhs1".  */
      rhs0 = TREE_OPERAND (rhs, 0);
      rhs1 = TREE_OPERAND (rhs, 1);
      STRIP_TYPE_NOPS (rhs0);
      STRIP_TYPE_NOPS (rhs1);

      if (TREE_CODE (rhs0) == SSA_NAME)
	{
	  if (TREE_CODE (rhs1) == SSA_NAME)
	    {
	      /* Match an assignment under the form: 
		 "a = b + c".  */
	      res = follow_ssa_edge 
		(loop, SSA_NAME_DEF_STMT (rhs0), halting_phi, 
		 evolution_of_loop);
	      
	      if (res)
		*evolution_of_loop = add_to_evolution 
		  (loop->num, 
		   chrec_convert (type_rhs, *evolution_of_loop), 
		   PLUS_EXPR, rhs1);
	      
	      else
		{
		  res = follow_ssa_edge 
		    (loop, SSA_NAME_DEF_STMT (rhs1), halting_phi, 
		     evolution_of_loop);
		  
		  if (res)
		    *evolution_of_loop = add_to_evolution 
		      (loop->num, 
		       chrec_convert (type_rhs, *evolution_of_loop), 
		       PLUS_EXPR, rhs0);
		}
	    }
	  
	  else
	    {
	      /* Match an assignment under the form: 
		 "a = b + ...".  */
	      res = follow_ssa_edge 
		(loop, SSA_NAME_DEF_STMT (rhs0), halting_phi, 
		 evolution_of_loop);
	      if (res)
		*evolution_of_loop = add_to_evolution 
		  (loop->num, chrec_convert (type_rhs, *evolution_of_loop), 
		   PLUS_EXPR, rhs1);
	    }
	}
      
      else if (TREE_CODE (rhs1) == SSA_NAME)
	{
	  /* Match an assignment under the form: 
	     "a = ... + c".  */
	  res = follow_ssa_edge 
	    (loop, SSA_NAME_DEF_STMT (rhs1), halting_phi, 
	     evolution_of_loop);
	  if (res)
	    *evolution_of_loop = add_to_evolution 
	      (loop->num, chrec_convert (type_rhs, *evolution_of_loop), 
	       PLUS_EXPR, rhs0);
	}

      else
	/* Otherwise, match an assignment under the form: 
	   "a = ... + ...".  */
	/* And there is nothing to do.  */
	res = false;
      
      break;
      
    case MINUS_EXPR:
      /* This case is under the form "opnd0 = rhs0 - rhs1".  */
      rhs0 = TREE_OPERAND (rhs, 0);
      rhs1 = TREE_OPERAND (rhs, 1);
      STRIP_TYPE_NOPS (rhs0);
      STRIP_TYPE_NOPS (rhs1);

      if (TREE_CODE (rhs0) == SSA_NAME)
	{
	  /* Match an assignment under the form: 
	     "a = b - ...".  */
	  res = follow_ssa_edge (loop, SSA_NAME_DEF_STMT (rhs0), halting_phi, 
				 evolution_of_loop);
	  if (res)
	    *evolution_of_loop = add_to_evolution 
		    (loop->num, chrec_convert (type_rhs, *evolution_of_loop), 
		     MINUS_EXPR, rhs1);
	}
      else
	/* Otherwise, match an assignment under the form: 
	   "a = ... - ...".  */
	/* And there is nothing to do.  */
	res = false;
      
      break;
    
    case MULT_EXPR:
      /* This case is under the form "opnd0 = rhs0 * rhs1".  */
      rhs0 = TREE_OPERAND (rhs, 0);
      rhs1 = TREE_OPERAND (rhs, 1);
      STRIP_TYPE_NOPS (rhs0);
      STRIP_TYPE_NOPS (rhs1);

      if (TREE_CODE (rhs0) == SSA_NAME)
	{
	  if (TREE_CODE (rhs1) == SSA_NAME)
	    {
	      /* Match an assignment under the form: 
		 "a = b * c".  */
	      res = follow_ssa_edge 
		(loop, SSA_NAME_DEF_STMT (rhs0), halting_phi, 
		 evolution_of_loop);
	      
	      if (res)
		*evolution_of_loop = chrec_dont_know;
	      
	      else
		{
		  res = follow_ssa_edge 
		    (loop, SSA_NAME_DEF_STMT (rhs1), halting_phi, 
		     evolution_of_loop);
		  
		  if (res)
		    *evolution_of_loop = chrec_dont_know;
		}
	    }
	  
	  else
	    {
	      /* Match an assignment under the form: 
		 "a = b * ...".  */
	      res = follow_ssa_edge 
		(loop, SSA_NAME_DEF_STMT (rhs0), halting_phi, 
		 evolution_of_loop);
	      if (res)
		*evolution_of_loop = chrec_dont_know;
	    }
	}
      
      else if (TREE_CODE (rhs1) == SSA_NAME)
	{
	  /* Match an assignment under the form: 
	     "a = ... * c".  */
	  res = follow_ssa_edge 
	    (loop, SSA_NAME_DEF_STMT (rhs1), halting_phi, 
	     evolution_of_loop);
	  if (res)
	    *evolution_of_loop = chrec_dont_know;
	}
      
      else
	/* Otherwise, match an assignment under the form: 
	   "a = ... * ...".  */
	/* And there is nothing to do.  */
	res = false;
      
      break;

    default:
      res = false;
      break;
    }
  
  return res;
}

/* Checks whether the I-th argument of a PHI comes from a backedge.  */

static bool
backedge_phi_arg_p (tree phi, int i)
{
  edge e = PHI_ARG_EDGE (phi, i);

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

static inline bool
follow_ssa_edge_in_condition_phi_branch (int i,
					 struct loop *loop, 
					 tree condition_phi, 
					 tree halting_phi,
					 tree *evolution_of_branch,
					 tree init_cond)
{
  tree branch = PHI_ARG_DEF (condition_phi, i);
  *evolution_of_branch = chrec_dont_know;

  /* Do not follow back edges (they must belong to an irreducible loop, which
     we really do not want to worry about).  */
  if (backedge_phi_arg_p (condition_phi, i))
    return false;

  if (TREE_CODE (branch) == SSA_NAME)
    {
      *evolution_of_branch = init_cond;
      return follow_ssa_edge (loop, SSA_NAME_DEF_STMT (branch), halting_phi, 
			      evolution_of_branch);
    }

  /* This case occurs when one of the condition branches sets 
     the variable to a constant: i.e. a phi-node like
     "a_2 = PHI <a_7(5), 2(6)>;".  
	 
     FIXME:  This case have to be refined correctly: 
     in some cases it is possible to say something better than
     chrec_dont_know, for example using a wrap-around notation.  */
  return false;
}

/* This function merges the branches of a condition-phi-node in a
   loop.  */

static bool
follow_ssa_edge_in_condition_phi (struct loop *loop,
				  tree condition_phi, 
				  tree halting_phi, 
				  tree *evolution_of_loop)
{
  int i;
  tree init = *evolution_of_loop;
  tree evolution_of_branch;

  if (!follow_ssa_edge_in_condition_phi_branch (0, loop, condition_phi,
						halting_phi,
						&evolution_of_branch,
						init))
    return false;
  *evolution_of_loop = evolution_of_branch;

  for (i = 1; i < PHI_NUM_ARGS (condition_phi); i++)
    {
      /* Quickly give up when the evolution of one of the branches is
	 not known.  */
      if (*evolution_of_loop == chrec_dont_know)
	return true;

      if (!follow_ssa_edge_in_condition_phi_branch (i, loop, condition_phi,
						    halting_phi,
						    &evolution_of_branch,
						    init))
	return false;

      *evolution_of_loop = chrec_merge (*evolution_of_loop,
					evolution_of_branch);
    }
  
  return true;
}

/* Follow an SSA edge in an inner loop.  It computes the overall
   effect of the loop, and following the symbolic initial conditions,
   it follows the edges in the parent loop.  The inner loop is
   considered as a single statement.  */

static bool
follow_ssa_edge_inner_loop_phi (struct loop *outer_loop,
				tree loop_phi_node, 
				tree halting_phi,
				tree *evolution_of_loop)
{
  struct loop *loop = loop_containing_stmt (loop_phi_node);
  tree ev = analyze_scalar_evolution (loop, PHI_RESULT (loop_phi_node));

  /* Sometimes, the inner loop is too difficult to analyze, and the
     result of the analysis is a symbolic parameter.  */
  if (ev == PHI_RESULT (loop_phi_node))
    {
      bool res = false;
      int i;

      for (i = 0; i < PHI_NUM_ARGS (loop_phi_node); i++)
	{
	  tree arg = PHI_ARG_DEF (loop_phi_node, i);
	  basic_block bb;

	  /* Follow the edges that exit the inner loop.  */
	  bb = PHI_ARG_EDGE (loop_phi_node, i)->src;
	  if (!flow_bb_inside_loop_p (loop, bb))
	    res = res || follow_ssa_edge_in_rhs (outer_loop, arg, halting_phi,
						 evolution_of_loop);
	}

      /* If the path crosses this loop-phi, give up.  */
      if (res == true)
	*evolution_of_loop = chrec_dont_know;

      return res;
    }

  /* Otherwise, compute the overall effect of the inner loop.  */
  ev = compute_overall_effect_of_inner_loop (loop, ev);
  return follow_ssa_edge_in_rhs (outer_loop, ev, halting_phi,
				 evolution_of_loop);
}

/* Follow an SSA edge from a loop-phi-node to itself, constructing a
   path that is analyzed on the return walk.  */

static bool
follow_ssa_edge (struct loop *loop, 
		 tree def, 
		 tree halting_phi,
		 tree *evolution_of_loop)
{
  struct loop *def_loop;
  
  if (TREE_CODE (def) == NOP_EXPR)
    return false;
  
  def_loop = loop_containing_stmt (def);
  
  switch (TREE_CODE (def))
    {
    case PHI_NODE:
      if (!loop_phi_node_p (def))
	/* DEF is a condition-phi-node.  Follow the branches, and
	   record their evolutions.  Finally, merge the collected
	   information and set the approximation to the main
	   variable.  */
	return follow_ssa_edge_in_condition_phi 
	  (loop, def, halting_phi, evolution_of_loop);

      /* When the analyzed phi is the halting_phi, the
	 depth-first search is over: we have found a path from
	 the halting_phi to itself in the loop.  */
      if (def == halting_phi)
	return true;
	  
      /* Otherwise, the evolution of the HALTING_PHI depends
	 on the evolution of another loop-phi-node, i.e. the
	 evolution function is a higher degree polynomial.  */
      if (def_loop == loop)
	return false;
	  
      /* Inner loop.  */
      if (flow_loop_nested_p (loop, def_loop))
	return follow_ssa_edge_inner_loop_phi 
	  (loop, def, halting_phi, evolution_of_loop);

      /* Outer loop.  */
      return false;

    case MODIFY_EXPR:
      return follow_ssa_edge_in_rhs (loop,
				     TREE_OPERAND (def, 1), 
				     halting_phi, 
				     evolution_of_loop);
      
    default:
      /* At this level of abstraction, the program is just a set
	 of MODIFY_EXPRs and PHI_NODEs.  In principle there is no
	 other node to be handled.  */
      return false;
    }
}



/* Given a LOOP_PHI_NODE, this function determines the evolution
   function from LOOP_PHI_NODE to LOOP_PHI_NODE in the loop.  */

static tree
analyze_evolution_in_loop (tree loop_phi_node, 
			   tree init_cond)
{
  int i;
  tree evolution_function = chrec_not_analyzed_yet;
  struct loop *loop = loop_containing_stmt (loop_phi_node);
  basic_block bb;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(analyze_evolution_in_loop \n");
      fprintf (dump_file, "  (loop_phi_node = ");
      print_generic_expr (dump_file, loop_phi_node, 0);
      fprintf (dump_file, ")\n");
    }
  
  for (i = 0; i < PHI_NUM_ARGS (loop_phi_node); i++)
    {
      tree arg = PHI_ARG_DEF (loop_phi_node, i);
      tree ssa_chain, ev_fn;
      bool res;

      /* Select the edges that enter the loop body.  */
      bb = PHI_ARG_EDGE (loop_phi_node, i)->src;
      if (!flow_bb_inside_loop_p (loop, bb))
	continue;
      
      if (TREE_CODE (arg) == SSA_NAME)
	{
	  ssa_chain = SSA_NAME_DEF_STMT (arg);

	  /* Pass in the initial condition to the follow edge function.  */
	  ev_fn = init_cond;
	  res = follow_ssa_edge (loop, ssa_chain, loop_phi_node, &ev_fn);
	}
      else
	res = false;
	      
      /* When it is impossible to go back on the same
	 loop_phi_node by following the ssa edges, the
	 evolution is represented by a peeled chrec, i.e. the
	 first iteration, EV_FN has the value INIT_COND, then
	 all the other iterations it has the value of ARG.  
	 For the moment, PEELED_CHREC nodes are not built.  */
      if (!res)
	ev_fn = chrec_dont_know;
      
      /* When there are multiple back edges of the loop (which in fact never
	 happens currently, but nevertheless), merge their evolutions.  */
      evolution_function = chrec_merge (evolution_function, ev_fn);
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (evolution_function = ");
      print_generic_expr (dump_file, evolution_function, 0);
      fprintf (dump_file, "))\n");
    }
  
  return evolution_function;
}

/* Given a loop-phi-node, return the initial conditions of the
   variable on entry of the loop.  When the CCP has propagated
   constants into the loop-phi-node, the initial condition is
   instantiated, otherwise the initial condition is kept symbolic.
   This analyzer does not analyze the evolution outside the current
   loop, and leaves this task to the on-demand tree reconstructor.  */

static tree 
analyze_initial_condition (tree loop_phi_node)
{
  int i;
  tree init_cond = chrec_not_analyzed_yet;
  struct loop *loop = bb_for_stmt (loop_phi_node)->loop_father;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(analyze_initial_condition \n");
      fprintf (dump_file, "  (loop_phi_node = \n");
      print_generic_expr (dump_file, loop_phi_node, 0);
      fprintf (dump_file, ")\n");
    }
  
  for (i = 0; i < PHI_NUM_ARGS (loop_phi_node); i++)
    {
      tree branch = PHI_ARG_DEF (loop_phi_node, i);
      basic_block bb = PHI_ARG_EDGE (loop_phi_node, i)->src;
      
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

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (init_cond = ");
      print_generic_expr (dump_file, init_cond, 0);
      fprintf (dump_file, "))\n");
    }
  
  return init_cond;
}

/* Analyze the scalar evolution for LOOP_PHI_NODE.  */

static tree 
interpret_loop_phi (struct loop *loop, tree loop_phi_node)
{
  tree res;
  struct loop *phi_loop = loop_containing_stmt (loop_phi_node);
  tree init_cond;
  
  if (phi_loop != loop)
    {
      struct loop *subloop;
      tree evolution_fn = analyze_scalar_evolution
	(phi_loop, PHI_RESULT (loop_phi_node));

      /* Dive one level deeper.  */
      subloop = superloop_at_depth (phi_loop, loop->depth + 1);

      /* Interpret the subloop.  */
      res = compute_overall_effect_of_inner_loop (subloop, evolution_fn);
      return res;
    }

  /* Otherwise really interpret the loop phi.  */
  init_cond = analyze_initial_condition (loop_phi_node);
  res = analyze_evolution_in_loop (loop_phi_node, init_cond);

  return res;
}

/* This function merges the branches of a condition-phi-node,
   contained in the outermost loop, and whose arguments are already
   analyzed.  */

static tree
interpret_condition_phi (struct loop *loop, tree condition_phi)
{
  int i;
  tree res = chrec_not_analyzed_yet;
  
  for (i = 0; i < PHI_NUM_ARGS (condition_phi); i++)
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
    }

  return res;
}

/* Interpret the right hand side of a modify_expr OPND1.  If we didn't
   analyzed this node before, follow the definitions until ending
   either on an analyzed modify_expr, or on a loop-phi-node.  On the
   return path, this function propagates evolutions (ala constant copy
   propagation).  OPND1 is not a GIMPLE expression because we could
   analyze the effect of an inner loop: see interpret_loop_phi.  */

static tree
interpret_rhs_modify_expr (struct loop *loop,
			   tree opnd1, tree type)
{
  tree res, opnd10, opnd11, chrec10, chrec11;
  
  if (is_gimple_min_invariant (opnd1))
    return chrec_convert (type, opnd1);
  
  switch (TREE_CODE (opnd1))
    {
    case PLUS_EXPR:
      opnd10 = TREE_OPERAND (opnd1, 0);
      opnd11 = TREE_OPERAND (opnd1, 1);
      chrec10 = analyze_scalar_evolution (loop, opnd10);
      chrec11 = analyze_scalar_evolution (loop, opnd11);
      chrec10 = chrec_convert (type, chrec10);
      chrec11 = chrec_convert (type, chrec11);
      res = chrec_fold_plus (type, chrec10, chrec11);
      break;
      
    case MINUS_EXPR:
      opnd10 = TREE_OPERAND (opnd1, 0);
      opnd11 = TREE_OPERAND (opnd1, 1);
      chrec10 = analyze_scalar_evolution (loop, opnd10);
      chrec11 = analyze_scalar_evolution (loop, opnd11);
      chrec10 = chrec_convert (type, chrec10);
      chrec11 = chrec_convert (type, chrec11);
      res = chrec_fold_minus (type, chrec10, chrec11);
      break;

    case NEGATE_EXPR:
      opnd10 = TREE_OPERAND (opnd1, 0);
      chrec10 = analyze_scalar_evolution (loop, opnd10);
      chrec10 = chrec_convert (type, chrec10);
      res = chrec_fold_minus (type, build_int_cst (type, 0), chrec10);
      break;

    case MULT_EXPR:
      opnd10 = TREE_OPERAND (opnd1, 0);
      opnd11 = TREE_OPERAND (opnd1, 1);
      chrec10 = analyze_scalar_evolution (loop, opnd10);
      chrec11 = analyze_scalar_evolution (loop, opnd11);
      chrec10 = chrec_convert (type, chrec10);
      chrec11 = chrec_convert (type, chrec11);
      res = chrec_fold_multiply (type, chrec10, chrec11);
      break;
      
    case SSA_NAME:
      res = chrec_convert (type, analyze_scalar_evolution (loop, opnd1));
      break;
      
    case NOP_EXPR:
    case CONVERT_EXPR:
      opnd10 = TREE_OPERAND (opnd1, 0);
      chrec10 = analyze_scalar_evolution (loop, opnd10);
      res = chrec_convert (type, chrec10);
      break;
      
    default:
      res = chrec_dont_know;
      break;
    }
  
  return res;
}



/* This section contains all the entry points: 
   - number_of_iterations_in_loop,
   - analyze_scalar_evolution,
   - instantiate_parameters.
*/

/* Compute and return the evolution function in WRTO_LOOP, the nearest
   common ancestor of DEF_LOOP and USE_LOOP.  */

static tree 
compute_scalar_evolution_in_loop (struct loop *wrto_loop, 
				  struct loop *def_loop, 
				  tree ev)
{
  tree res;
  if (def_loop == wrto_loop)
    return ev;

  def_loop = superloop_at_depth (def_loop, wrto_loop->depth + 1);
  res = compute_overall_effect_of_inner_loop (def_loop, ev);

  return analyze_scalar_evolution_1 (wrto_loop, res, chrec_not_analyzed_yet);
}

/* Helper recursive function.  */

static tree
analyze_scalar_evolution_1 (struct loop *loop, tree var, tree res)
{
  tree def, type = TREE_TYPE (var);
  basic_block bb;
  struct loop *def_loop;

  if (loop == NULL)
    return chrec_dont_know;

  if (TREE_CODE (var) != SSA_NAME)
    return interpret_rhs_modify_expr (loop, var, type);

  def = SSA_NAME_DEF_STMT (var);
  bb = bb_for_stmt (def);
  def_loop = bb ? bb->loop_father : NULL;

  if (bb == NULL
      || !flow_bb_inside_loop_p (loop, bb))
    {
      /* Keep the symbolic form.  */
      res = var;
      goto set_and_end;
    }

  if (res != chrec_not_analyzed_yet)
    {
      if (loop != bb->loop_father)
	res = compute_scalar_evolution_in_loop 
	    (find_common_loop (loop, bb->loop_father), bb->loop_father, res);

      goto set_and_end;
    }

  if (loop != def_loop)
    {
      res = analyze_scalar_evolution_1 (def_loop, var, chrec_not_analyzed_yet);
      res = compute_scalar_evolution_in_loop (loop, def_loop, res);

      goto set_and_end;
    }

  switch (TREE_CODE (def))
    {
    case MODIFY_EXPR:
      res = interpret_rhs_modify_expr (loop, TREE_OPERAND (def, 1), type);
      break;

    case PHI_NODE:
      if (loop_phi_node_p (def))
	res = interpret_loop_phi (loop, def);
      else
	res = interpret_condition_phi (loop, def);
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
    set_scalar_evolution (var, res);

  return res;
}

/* Entry point for the scalar evolution analyzer.
   Analyzes and returns the scalar evolution of the ssa_name VAR.
   LOOP_NB is the identifier number of the loop in which the variable
   is used.
   
   Example of use: having a pointer VAR to a SSA_NAME node, STMT a
   pointer to the statement that uses this variable, in order to
   determine the evolution function of the variable, use the following
   calls:
   
   unsigned loop_nb = loop_containing_stmt (stmt)->num;
   tree chrec_with_symbols = analyze_scalar_evolution (loop_nb, var);
   tree chrec_instantiated = instantiate_parameters 
   (loop_nb, chrec_with_symbols);
*/

tree 
analyze_scalar_evolution (struct loop *loop, tree var)
{
  tree res;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(analyze_scalar_evolution \n");
      fprintf (dump_file, "  (loop_nb = %d)\n", loop->num);
      fprintf (dump_file, "  (scalar = ");
      print_generic_expr (dump_file, var, 0);
      fprintf (dump_file, ")\n");
    }

  res = analyze_scalar_evolution_1 (loop, var, get_scalar_evolution (var));

  if (TREE_CODE (var) == SSA_NAME && res == chrec_dont_know)
    res = var;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");

  return res;
}

/* Analyze scalar evolution of use of VERSION in USE_LOOP with respect to
   WRTO_LOOP (which should be a superloop of both USE_LOOP and definition
   of VERSION).  */

static tree
analyze_scalar_evolution_in_loop (struct loop *wrto_loop, struct loop *use_loop,
				  tree version)
{
  bool val = false;
  tree ev = version;

  while (1)
    {
      ev = analyze_scalar_evolution (use_loop, ev);
      ev = resolve_mixers (use_loop, ev);

      if (use_loop == wrto_loop)
	return ev;

      /* If the value of the use changes in the inner loop, we cannot express
	 its value in the outer loop (we might try to return interval chrec,
	 but we do not have a user for it anyway)  */
      if (!no_evolution_in_loop_p (ev, use_loop->num, &val)
	  || !val)
	return chrec_dont_know;

      use_loop = use_loop->outer;
    }
}

/* Returns instantiated value for VERSION in CACHE.  */

static tree
get_instantiated_value (htab_t cache, tree version)
{
  struct scev_info_str *info, pattern;
  
  pattern.var = version;
  info = htab_find (cache, &pattern);

  if (info)
    return info->chrec;
  else
    return NULL_TREE;
}

/* Sets instantiated value for VERSION to VAL in CACHE.  */

static void
set_instantiated_value (htab_t cache, tree version, tree val)
{
  struct scev_info_str *info, pattern;
  PTR *slot;
  
  pattern.var = version;
  slot = htab_find_slot (cache, &pattern, INSERT);

  if (*slot)
    info = *slot;
  else
    info = *slot = new_scev_info_str (version);
  info->chrec = val;
}

/* Analyze all the parameters of the chrec that were left under a symbolic form,
   with respect to LOOP.  CHREC is the chrec to instantiate.  If
   ALLOW_SUPERLOOP_CHRECS is true, replacing loop invariants with
   outer loop chrecs is done.  CACHE is the cache of already instantiated
   values.  */

static tree
instantiate_parameters_1 (struct loop *loop, tree chrec,
			  bool allow_superloop_chrecs,
			  htab_t cache)
{
  tree res, op0, op1, op2;
  basic_block def_bb;
  struct loop *def_loop;
 
  if (chrec == NULL_TREE
      || automatically_generated_chrec_p (chrec))
    return chrec;
 
  if (is_gimple_min_invariant (chrec))
    return chrec;

  switch (TREE_CODE (chrec))
    {
    case SSA_NAME:
      def_bb = bb_for_stmt (SSA_NAME_DEF_STMT (chrec));

      /* A parameter (or loop invariant and we do not want to include
	 evolutions in outer loops), nothing to do.  */
      if (!def_bb
	  || (!allow_superloop_chrecs
	      && !flow_bb_inside_loop_p (loop, def_bb)))
	return chrec;

      /* We cache the value of instantiated variable to avoid exponential
	 time complexity due to reevaluations.  We also store the convenient
	 value in the cache in order to prevent infinite recursion -- we do
	 not want to instantiate the SSA_NAME if it is in a mixer
	 structure.  This is used for avoiding the instantiation of
	 recursively defined functions, such as: 

	 | a_2 -> {0, +, 1, +, a_2}_1  */

      res = get_instantiated_value (cache, chrec);
      if (res)
	return res;

      /* Store the convenient value for chrec in the structure.  If it
	 is defined outside of the loop, we may just leave it in symbolic
	 form, otherwise we need to admit that we do not know its behavior
	 inside the loop.  */
      res = !flow_bb_inside_loop_p (loop, def_bb) ? chrec : chrec_dont_know;
      set_instantiated_value (cache, chrec, res);

      /* To make things even more complicated, instantiate_parameters_1
	 calls analyze_scalar_evolution that may call # of iterations
	 analysis that may in turn call instantiate_parameters_1 again.
	 To prevent the infinite recursion, keep also the bitmap of
	 ssa names that are being instantiated globally.  */
      if (bitmap_bit_p (already_instantiated, SSA_NAME_VERSION (chrec)))
	return res;

      def_loop = find_common_loop (loop, def_bb->loop_father);

      /* If the analysis yields a parametric chrec, instantiate the
	 result again.  */
      bitmap_set_bit (already_instantiated, SSA_NAME_VERSION (chrec));
      res = analyze_scalar_evolution (def_loop, chrec);
      if (res != chrec_dont_know)
	res = instantiate_parameters_1 (loop, res, allow_superloop_chrecs,
					cache);
      bitmap_clear_bit (already_instantiated, SSA_NAME_VERSION (chrec));

      /* Store the correct value to the cache.  */
      set_instantiated_value (cache, chrec, res);
      return res;

    case POLYNOMIAL_CHREC:
      op0 = instantiate_parameters_1 (loop, CHREC_LEFT (chrec),
				      allow_superloop_chrecs, cache);
      if (op0 == chrec_dont_know)
	return chrec_dont_know;

      op1 = instantiate_parameters_1 (loop, CHREC_RIGHT (chrec),
				      allow_superloop_chrecs, cache);
      if (op1 == chrec_dont_know)
	return chrec_dont_know;

      if (CHREC_LEFT (chrec) != op0
	  || CHREC_RIGHT (chrec) != op1)
	chrec = build_polynomial_chrec (CHREC_VARIABLE (chrec), op0, op1);
      return chrec;

    case PLUS_EXPR:
      op0 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 0),
				      allow_superloop_chrecs, cache);
      if (op0 == chrec_dont_know)
	return chrec_dont_know;

      op1 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 1),
				      allow_superloop_chrecs, cache);
      if (op1 == chrec_dont_know)
	return chrec_dont_know;

      if (TREE_OPERAND (chrec, 0) != op0
	  || TREE_OPERAND (chrec, 1) != op1)
      	chrec = chrec_fold_plus (TREE_TYPE (chrec), op0, op1);
      return chrec;

    case MINUS_EXPR:
      op0 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 0),
				      allow_superloop_chrecs, cache);
      if (op0 == chrec_dont_know)
	return chrec_dont_know;

      op1 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 1),
				      allow_superloop_chrecs, cache);
      if (op1 == chrec_dont_know)
	return chrec_dont_know;

      if (TREE_OPERAND (chrec, 0) != op0
	  || TREE_OPERAND (chrec, 1) != op1)
        chrec = chrec_fold_minus (TREE_TYPE (chrec), op0, op1);
      return chrec;

    case MULT_EXPR:
      op0 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 0),
				      allow_superloop_chrecs, cache);
      if (op0 == chrec_dont_know)
	return chrec_dont_know;

      op1 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 1),
				      allow_superloop_chrecs, cache);
      if (op1 == chrec_dont_know)
	return chrec_dont_know;

      if (TREE_OPERAND (chrec, 0) != op0
	  || TREE_OPERAND (chrec, 1) != op1)
	chrec = chrec_fold_multiply (TREE_TYPE (chrec), op0, op1);
      return chrec;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      op0 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 0),
				      allow_superloop_chrecs, cache);
      if (op0 == chrec_dont_know)
        return chrec_dont_know;

      if (op0 == TREE_OPERAND (chrec, 0))
	return chrec;

      return chrec_convert (TREE_TYPE (chrec), op0);

    case SCEV_NOT_KNOWN:
      return chrec_dont_know;

    case SCEV_KNOWN:
      return chrec_known;
                                     
    default:
      break;
    }

  switch (TREE_CODE_LENGTH (TREE_CODE (chrec)))
    {
    case 3:
      op0 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 0),
				      allow_superloop_chrecs, cache);
      if (op0 == chrec_dont_know)
	return chrec_dont_know;

      op1 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 1),
				      allow_superloop_chrecs, cache);
      if (op1 == chrec_dont_know)
	return chrec_dont_know;

      op2 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 2),
				      allow_superloop_chrecs, cache);
      if (op2 == chrec_dont_know)
        return chrec_dont_know;

      if (op0 == TREE_OPERAND (chrec, 0)
	  && op1 == TREE_OPERAND (chrec, 1)
	  && op2 == TREE_OPERAND (chrec, 2))
	return chrec;

      return fold (build (TREE_CODE (chrec),
			  TREE_TYPE (chrec), op0, op1, op2));

    case 2:
      op0 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 0),
				      allow_superloop_chrecs, cache);
      if (op0 == chrec_dont_know)
	return chrec_dont_know;

      op1 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 1),
				      allow_superloop_chrecs, cache);
      if (op1 == chrec_dont_know)
        return chrec_dont_know;

      if (op0 == TREE_OPERAND (chrec, 0)
	  && op1 == TREE_OPERAND (chrec, 1))
	return chrec;
      return fold (build (TREE_CODE (chrec), TREE_TYPE (chrec), op0, op1));
	    
    case 1:
      op0 = instantiate_parameters_1 (loop, TREE_OPERAND (chrec, 0),
				      allow_superloop_chrecs, cache);
      if (op0 == chrec_dont_know)
        return chrec_dont_know;
      if (op0 == TREE_OPERAND (chrec, 0))
	return chrec;
      return fold (build1 (TREE_CODE (chrec), TREE_TYPE (chrec), op0));

    case 0:
      return chrec;

    default:
      break;
    }

  /* Too complicated to handle.  */
  return chrec_dont_know;
}

/* Analyze all the parameters of the chrec that were left under a
   symbolic form.  LOOP is the loop in which symbolic names have to
   be analyzed and instantiated.  */

tree
instantiate_parameters (struct loop *loop,
			tree chrec)
{
  tree res;
  htab_t cache = htab_create (10, hash_scev_info, eq_scev_info, del_scev_info);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(instantiate_parameters \n");
      fprintf (dump_file, "  (loop_nb = %d)\n", loop->num);
      fprintf (dump_file, "  (chrec = ");
      print_generic_expr (dump_file, chrec, 0);
      fprintf (dump_file, ")\n");
    }
 
  res = instantiate_parameters_1 (loop, chrec, true, cache);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (res = ");
      print_generic_expr (dump_file, res, 0);
      fprintf (dump_file, "))\n");
    }

  htab_delete (cache);
  
  return res;
}

/* Similar to instantiate_parameters, but does not introduce the
   evolutions in outer loops for LOOP invariants in CHREC.  */

static tree
resolve_mixers (struct loop *loop, tree chrec)
{
  htab_t cache = htab_create (10, hash_scev_info, eq_scev_info, del_scev_info);
  tree ret = instantiate_parameters_1 (loop, chrec, false, cache);
  htab_delete (cache);
  return ret;
}

/* Entry point for the analysis of the number of iterations pass.  
   This function tries to safely approximate the number of iterations
   the loop will run.  When this property is not decidable at compile
   time, the result is chrec_dont_know.  Otherwise the result is
   a scalar or a symbolic parameter.
   
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
number_of_iterations_in_loop (struct loop *loop)
{
  tree res, type;
  edge exit;
  struct tree_niter_desc niter_desc;

  /* Determine whether the number_of_iterations_in_loop has already
     been computed.  */
  res = loop->nb_iterations;
  if (res)
    return res;
  res = chrec_dont_know;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(number_of_iterations_in_loop\n");
  
  exit = loop->single_exit;
  if (!exit)
    goto end;

  if (!number_of_iterations_exit (loop, exit, &niter_desc))
    goto end;

  type = TREE_TYPE (niter_desc.niter);
  if (integer_nonzerop (niter_desc.may_be_zero))
    res = build_int_cst (type, 0);
  else if (integer_zerop (niter_desc.may_be_zero))
    res = niter_desc.niter;
  else
    res = chrec_dont_know;

end:
  return set_nb_iterations_in_loop (loop, res);
}

/* One of the drivers for testing the scalar evolutions analysis.
   This function computes the number of iterations for all the loops
   from the EXIT_CONDITIONS array.  */

static void 
number_of_iterations_for_all_loops (varray_type exit_conditions)
{
  unsigned int i;
  unsigned nb_chrec_dont_know_loops = 0;
  unsigned nb_static_loops = 0;
  
  for (i = 0; i < VARRAY_ACTIVE_SIZE (exit_conditions); i++)
    {
      tree res = number_of_iterations_in_loop 
	(loop_containing_stmt (VARRAY_TREE (exit_conditions, i)));
      if (chrec_contains_undetermined (res))
	nb_chrec_dont_know_loops++;
      else
	nb_static_loops++;
    }
  
  if (dump_file)
    {
      fprintf (dump_file, "\n(\n");
      fprintf (dump_file, "-----------------------------------------\n");
      fprintf (dump_file, "%d\tnb_chrec_dont_know_loops\n", nb_chrec_dont_know_loops);
      fprintf (dump_file, "%d\tnb_static_loops\n", nb_static_loops);
      fprintf (dump_file, "%d\tnb_total_loops\n", current_loops->num);
      fprintf (dump_file, "-----------------------------------------\n");
      fprintf (dump_file, ")\n\n");
      
      print_loop_ir (dump_file);
    }
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
	   (int) htab_elements (scalar_evolution_info));
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
      print_generic_expr (dump_file, chrec, 0);
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
      else if (evolution_function_is_affine_multivariate_p (chrec))
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

/* One of the drivers for testing the scalar evolutions analysis.
   This function analyzes the scalar evolution of all the scalars
   defined as loop phi nodes in one of the loops from the
   EXIT_CONDITIONS array.  
   
   TODO Optimization: A loop is in canonical form if it contains only
   a single scalar loop phi node.  All the other scalars that have an
   evolution in the loop are rewritten in function of this single
   index.  This allows the parallelization of the loop.  */

static void 
analyze_scalar_evolution_for_all_loop_phi_nodes (varray_type exit_conditions)
{
  unsigned int i;
  struct chrec_stats stats;
  
  reset_chrecs_counters (&stats);
  
  for (i = 0; i < VARRAY_ACTIVE_SIZE (exit_conditions); i++)
    {
      struct loop *loop;
      basic_block bb;
      tree phi, chrec;
      
      loop = loop_containing_stmt (VARRAY_TREE (exit_conditions, i));
      bb = loop->header;
      
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	if (is_gimple_reg (PHI_RESULT (phi)))
	  {
	    chrec = instantiate_parameters 
	      (loop, 
	       analyze_scalar_evolution (loop, PHI_RESULT (phi)));
	    
	    if (dump_file && (dump_flags & TDF_STATS))
	      gather_chrec_stats (chrec, &stats);
	  }
    }
  
  if (dump_file && (dump_flags & TDF_STATS))
    dump_chrecs_stats (dump_file, &stats);
}

/* Callback for htab_traverse, gathers information on chrecs in the
   hashtable.  */

static int
gather_stats_on_scev_database_1 (void **slot, void *stats)
{
  struct scev_info_str *entry = *slot;

  gather_chrec_stats (entry->chrec, stats);

  return 1;
}

/* Classify the chrecs of the whole database.  */

void 
gather_stats_on_scev_database (void)
{
  struct chrec_stats stats;
  
  if (!dump_file)
    return;
  
  reset_chrecs_counters (&stats);
 
  htab_traverse (scalar_evolution_info, gather_stats_on_scev_database_1,
		 &stats);

  dump_chrecs_stats (dump_file, &stats);
}



/* Initializer.  */

static void
initialize_scalar_evolutions_analyzer (void)
{
  /* The elements below are unique.  */
  if (chrec_dont_know == NULL_TREE)
    {
      chrec_not_analyzed_yet = NULL_TREE;
      chrec_dont_know = make_node (SCEV_NOT_KNOWN);
      chrec_known = make_node (SCEV_KNOWN);
      TREE_TYPE (chrec_dont_know) = NULL_TREE;
      TREE_TYPE (chrec_known) = NULL_TREE;
    }
}

/* Initialize the analysis of scalar evolutions for LOOPS.  */

void
scev_initialize (struct loops *loops)
{
  unsigned i;
  current_loops = loops;

  scalar_evolution_info = htab_create (100, hash_scev_info,
				       eq_scev_info, del_scev_info);
  already_instantiated = BITMAP_ALLOC (NULL);
  
  initialize_scalar_evolutions_analyzer ();

  for (i = 1; i < loops->num; i++)
    if (loops->parray[i])
      loops->parray[i]->nb_iterations = NULL_TREE;
}

/* Cleans up the information cached by the scalar evolutions analysis.  */

void
scev_reset (void)
{
  unsigned i;
  struct loop *loop;

  if (!scalar_evolution_info || !current_loops)
    return;

  htab_empty (scalar_evolution_info);
  for (i = 1; i < current_loops->num; i++)
    {
      loop = current_loops->parray[i];
      if (loop)
	loop->nb_iterations = NULL_TREE;
    }
}

/* Checks whether OP behaves as a simple affine iv of LOOP in STMT and returns
   its BASE and STEP if possible.  */

bool
simple_iv (struct loop *loop, tree stmt, tree op, tree *base, tree *step)
{
  basic_block bb = bb_for_stmt (stmt);
  tree type, ev;

  *base = NULL_TREE;
  *step = NULL_TREE;

  type = TREE_TYPE (op);
  if (TREE_CODE (type) != INTEGER_TYPE
      && TREE_CODE (type) != POINTER_TYPE)
    return false;

  ev = analyze_scalar_evolution_in_loop (loop, bb->loop_father, op);
  if (chrec_contains_undetermined (ev))
    return false;

  if (tree_does_not_contain_chrecs (ev)
      && !chrec_contains_symbols_defined_in_loop (ev, loop->num))
    {
      *base = ev;
      return true;
    }

  if (TREE_CODE (ev) != POLYNOMIAL_CHREC
      || CHREC_VARIABLE (ev) != (unsigned) loop->num)
    return false;

  *step = CHREC_RIGHT (ev);
  if (TREE_CODE (*step) != INTEGER_CST)
    return false;
  *base = CHREC_LEFT (ev);
  if (tree_contains_chrecs (*base, NULL)
      || chrec_contains_symbols_defined_in_loop (*base, loop->num))
    return false;

  return true;
}

/* Runs the analysis of scalar evolutions.  */

void
scev_analysis (void)
{
  varray_type exit_conditions;
  
  VARRAY_GENERIC_PTR_INIT (exit_conditions, 37, "exit_conditions");
  select_loops_exit_conditions (current_loops, &exit_conditions);

  if (dump_file && (dump_flags & TDF_STATS))
    analyze_scalar_evolution_for_all_loop_phi_nodes (exit_conditions);
  
  number_of_iterations_for_all_loops (exit_conditions);
  VARRAY_CLEAR (exit_conditions);
}

/* Finalize the scalar evolution analysis.  */

void
scev_finalize (void)
{
  htab_delete (scalar_evolution_info);
  BITMAP_FREE (already_instantiated);
}

