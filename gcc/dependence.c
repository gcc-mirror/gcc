/* Analyze loop dependencies
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.

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

/* References:
   Practical Dependence Testing, Goff, Kennedy, Tseng, PLDI, 1991
   High Performance Compilers for Parallel Computing, Wolfe
*/

#include "config.h"
#include "system.h"

#include "rtl.h"
#include "expr.h"
#include "tree.h"
#include "c-common.h"
#include "flags.h"
#include "varray.h"

#define MAX_SUBSCRIPTS 13

/*
   We perform the following steps:

   Build the data structures def_use_chain, loop_chain, and induction_chain.

   Determine if a loop index is a normalized induction variable.
   A loop is currently considered to be a for loop having an index set to an
   initial value, conditional check of the index, and increment/decrement of
   the index.

   Determine the distance and direction vectors.  Both are two dimensioned
   arrays where the first dimension represents a loop and the second
   dimension represents a subscript.  Dependencies are actually per loop, not
   per subscript.  So for:
   for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
           array [i][j] = array[i][j-1]
   We find the dependencies: loop1/sub_i, loop1/sub_j, loop2/sub_i, loop2/sub_j
   and then intersect loop1/sub_i V loop2/sub_i and loop1/sub_i V loop2/sub_j
   We determine the type of dependence, which determines which test we use.
   We then try to refine the type of dependence we have and add the
   dependence to the dep_chain
*/

enum dependence_type {dt_flow, dt_anti, dt_output, dt_none};
#if 0
static const char *const dependence_string [] = {"flow", "anti", "output", "none"};
#endif
enum direction_type {lt, le, eq, gt, ge, star, independent, undef};
#if 0
static const char *const direction_string [] = {"<", "<=", "=", ">", ">=", "*",
					   "INDEPENDENT", "UNDEFINED"};
#endif
enum def_use_type {def, use, init_def_use};

enum du_status_type {seen, unseen};

enum loop_status_type {normal, unnormal};

enum complexity_type {ziv, strong_siv, weak_siv, weak_zero_siv,
		      weak_crossing_siv, miv};

/* Given a def/use one can chase the next chain to follow the def/use
   for that variable.  Alternately one can sequentially follow each
   element of def_use_chain.  */

typedef struct def_use
{
  /* outermost loop */
  tree outer_loop;
  /* loop containing this def/use */
  tree containing_loop;
  /* this expression */
  tree expression;
  /* our name */
  const char *variable;
  /* def or use */
  enum def_use_type type;
  /* status flags */
  enum du_status_type status;
  /* next def/use for this same name */
  struct def_use *next;
  /* dependencies for this def */
  struct dependence *dep;
} def_use;

/* Given a loop* one can chase the next_nest chain to follow the nested
   loops for that loop.  Alternately one can sequentially follow each
   element of loop_chain and check outer_loop to get all loops
   contained within a certain loop.  */

typedef struct loop
{
  /* outermost loop containing this loop */
  tree outer_loop;
  /* this loop */
  tree containing_loop;
  /* nest level for this loop */
  int  depth;
  /* can loop be normalized? */
  enum loop_status_type status;
  /* loop* for loop contained in this loop */
  struct loop *next_nest;
  /* induction variables for this loop.  Currently only the index variable.  */
  struct induction *ind;
} loop;

/* Pointed to by loop. One per induction variable.  */

typedef struct induction
{
  /* our name */
  const char *variable;
  /* increment.  Currently only +1 or -1 */
  int  increment;
  /* lower bound */
  int  low_bound;
  /* upper bound */
  int  high_bound;
  /* next induction variable for this loop.  Currently null.  */
  struct induction *next;
} induction;

/* Pointed to by def/use.  One per dependence.  */

typedef struct dependence
{
  tree source;
  tree destination;
  enum dependence_type dependence;
  enum direction_type direction[MAX_SUBSCRIPTS];
  int distance[MAX_SUBSCRIPTS];
  struct dependence *next;
} dependence;
  
/* subscripts are represented by an array of these.  Each reflects one
   X * i + Y term, where X and Y are constants.  */

typedef struct subscript
{
  /* ordinal subscript number */
  int position;
  /* X in X * i + Y */
  int coefficient;
  /* Y in X * i + Y */
  int offset;
  /* our name */
  const char *variable;
  /* next subscript term.  Currently null.  */
  struct subscript *next;
} subscript;

/* Remember the destination the front end encountered.  */

static tree dest_to_remember;

/* Chain for def_use */
static varray_type def_use_chain;

/* Chain for dependence */
static varray_type dep_chain;

/* Chain for loop */
static varray_type loop_chain;

/* Chain for induction */
static varray_type induction_chain;

void init_dependence_analysis PARAMS ((tree));
static void build_def_use PARAMS ((tree, enum def_use_type));
static loop* add_loop PARAMS ((tree, tree, int));
static int find_induction_variable PARAMS ((tree, tree, tree, loop*));
static int get_low_bound PARAMS ((tree, const char*));
static int have_induction_variable PARAMS ((tree, const char*));
static void link_loops PARAMS ((void));
static void get_node_dependence PARAMS ((void));
static void check_node_dependence PARAMS ((def_use*));
static int get_coefficients PARAMS ((def_use*, subscript[]));
static int get_one_coefficient PARAMS ((tree, subscript*, def_use*, enum tree_code*));
static void normalize_coefficients PARAMS ((subscript[], loop*, int));
static void classify_dependence PARAMS ((subscript[], subscript[],
				 enum complexity_type[], int*, int));
static void ziv_test PARAMS ((subscript[], subscript[],
			      enum direction_type[][MAX_SUBSCRIPTS],
			      int[][MAX_SUBSCRIPTS], loop*, int));
static void siv_test PARAMS ((subscript[], subscript[],
			      enum direction_type[][MAX_SUBSCRIPTS],
			      int[][MAX_SUBSCRIPTS], loop*, int));
static int check_subscript_induction PARAMS ((subscript*, subscript*, loop*));
static void gcd_test PARAMS ((subscript[], subscript[], enum
			      direction_type[][MAX_SUBSCRIPTS],
			      int[][MAX_SUBSCRIPTS], loop*, int));
static int find_gcd PARAMS ((int, int));
static void merge_dependencies PARAMS ((enum direction_type[][MAX_SUBSCRIPTS],
					int[][MAX_SUBSCRIPTS], int, int));
static void dump_array_ref PARAMS ((tree));
#if 0
static void dump_one_node PARAMS ((def_use*, varray_type*));
static void dump_node_dependence PARAMS ((void));
#endif
int search_dependence PARAMS ((tree));
void remember_dest_for_dependence PARAMS ((tree));
int have_dependence_p PARAMS ((rtx, rtx, enum direction_type[], int[]));
void end_dependence_analysis PARAMS ((void));

/* Build dependence chain 'dep_chain', which is used by have_dependence_p,
   for the function given by EXP.  */

void
init_dependence_analysis (exp)
     tree exp;
{
  def_use *du_ptr;

  VARRAY_GENERIC_PTR_INIT (def_use_chain, 50, "def_use_chain");
  VARRAY_GENERIC_PTR_INIT (dep_chain, 50, "dep_chain");
  VARRAY_GENERIC_PTR_INIT (loop_chain, 50, "loop_chain");
  VARRAY_GENERIC_PTR_INIT (induction_chain, 50, "induction_chain");

  build_def_use (exp, init_def_use);

  link_loops ();

  get_node_dependence ();

  /* dump_node_dependence (&def_use_chain);*/

  for (du_ptr = VARRAY_TOP (def_use_chain, generic);
       VARRAY_POP (def_use_chain);
       du_ptr = VARRAY_TOP (def_use_chain, generic))
    {
      free (du_ptr);
    }

  VARRAY_FREE (def_use_chain);
  VARRAY_FREE (loop_chain);
  VARRAY_FREE (induction_chain);
}

/* Build ARRAY_REF def/use info 'def_use_chain' starting at EXP which is a def
   or use DU_TYPE */ 

static void
build_def_use (exp, du_type)
     tree exp;
     enum def_use_type du_type;
{
  static tree outer_loop;
  static int nloop;
  static tree current_loop;
  static int du_idx;
  static loop *loop_def;
  tree node = exp;
  tree array_ref;
  def_use *du_ptr;

  if (du_type == init_def_use)
    {
      outer_loop = 0;
      nloop = 0;
      du_idx = 0;
    }
  
  while (node)
    switch (TREE_CODE (node))
      {
      case COMPOUND_STMT:
	node = TREE_OPERAND (node, 0);
	break;
      case TREE_LIST:
	build_def_use (TREE_VALUE (node), 0);
	node = TREE_CHAIN (node);
	break;
      case CALL_EXPR:
	node = TREE_CHAIN (node);
	break;
      case FOR_STMT:
	if (! nloop) outer_loop = node;
	nloop++;
	current_loop = node;
	loop_def = add_loop (node, outer_loop, nloop);
	if (find_induction_variable (TREE_OPERAND (node, 0),
				     TREE_OPERAND (node, 1),
				     TREE_OPERAND (node, 2), loop_def)
	    == 0)
	  loop_def->status = unnormal;
	  
	build_def_use (TREE_OPERAND (node, 3), 0);
	nloop--;
	current_loop = 0;
	node = TREE_CHAIN (node);
	break;
      case MODIFY_EXPR:
	/* Is an induction variable modified? */
	if (loop_def 
	    && TREE_CODE (TREE_OPERAND (node, 0)) == VAR_DECL
	    && have_induction_variable
	       (loop_def->outer_loop,
		IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (node, 0)))) >= 0)
	  loop_def->status = unnormal;

	if (TREE_CODE (TREE_OPERAND (node, 0)) == ARRAY_REF
	    || TREE_CODE (TREE_OPERAND (node, 0)) == INDIRECT_REF)
	  build_def_use (TREE_OPERAND (node, 0), def);

	build_def_use (TREE_OPERAND (node, 1), use);
	node = TREE_CHAIN (node);
	break;
      case INDIRECT_REF:
	if (! TREE_OPERAND (node, 1)
	    || TREE_CODE (TREE_OPERAND (node, 1)) != ARRAY_REF)
	  {
	    node = 0;
	    break;
	  }
	node = TREE_OPERAND (node, 1);
      case ARRAY_REF:
	if (nloop)
	  {
	    int i;
	    char null_string = '\0';

	    VARRAY_PUSH_GENERIC_PTR (def_use_chain, xmalloc (sizeof (def_use)));
	    du_ptr = VARRAY_GENERIC_PTR (def_use_chain, du_idx++);
	    du_ptr->type = du_type;
	    du_ptr->status = unseen;
	    du_ptr->outer_loop = outer_loop;
	    du_ptr->containing_loop = current_loop;
	    du_ptr->expression = node;
	    du_ptr->variable = &null_string;
	    du_ptr->next = 0;
	    du_ptr->dep = 0;
	    for (array_ref = node;
		 TREE_CODE (array_ref) == ARRAY_REF;
		 array_ref = TREE_OPERAND (array_ref, 0))
	      ;

	    if (TREE_CODE (array_ref) == COMPONENT_REF)
	      {
		array_ref = TREE_OPERAND (array_ref, 1);
		if (! (TREE_CODE (array_ref) == FIELD_DECL
		       && TREE_CODE (TREE_TYPE (array_ref)) == ARRAY_TYPE))
		  {
		    node = 0;
		    break;
		  }
	      }
	    
	    for (i = 0;
		 i < du_idx
		   && strcmp (IDENTIFIER_POINTER (DECL_NAME (array_ref)),
			      ((def_use*) (VARRAY_GENERIC_PTR
					   (def_use_chain, i)))->variable);
		 i++)
	      ;
	    if (i != du_idx)
	      {
		def_use *tmp_duc;
		for (tmp_duc = ((def_use*) (VARRAY_GENERIC_PTR (def_use_chain, i)));
		     tmp_duc->next;
		     tmp_duc = ((def_use*)tmp_duc->next));
		tmp_duc->next = du_ptr;
	      }
	    else du_ptr->next = 0;
	    du_ptr->variable = IDENTIFIER_POINTER (DECL_NAME (array_ref));
	  }
	node = 0;
	break;

      case SCOPE_STMT:
      case DECL_STMT:
	node = TREE_CHAIN (node);
	break;
	
      case EXPR_STMT:
	if (TREE_CODE (TREE_OPERAND (node, 0)) == MODIFY_EXPR)
	  build_def_use (TREE_OPERAND (node, 0), def);
	node = TREE_CHAIN (node);
	break;

      default:
	if (TREE_CODE_CLASS (TREE_CODE (node)) == '2')
	  {
	    build_def_use (TREE_OPERAND (node, 0), use);
	    build_def_use (TREE_OPERAND (node, 1), use);
	    node = TREE_CHAIN (node);
	  }
	else
	  node = 0;
      }
}

/* Add a loop to 'loop_chain' corresponding to for loop LOOP_NODE at depth
   NLOOP, whose outermost loop is OUTER_LOOP */

static loop*
add_loop (loop_node, outer_loop, nloop)
     tree loop_node;
     tree outer_loop;
     int nloop;
{
  loop *loop_ptr;

  VARRAY_PUSH_GENERIC_PTR (loop_chain, xmalloc (sizeof (loop)));
  loop_ptr = VARRAY_TOP (loop_chain, generic);
  loop_ptr->outer_loop = outer_loop;
  loop_ptr->containing_loop = loop_node;
  loop_ptr->depth = nloop;
  loop_ptr->status = normal;
  loop_ptr->next_nest = 0;
  loop_ptr->ind = 0;
  return loop_ptr;
}

/* Update LOOP_DEF if for loop's COND_NODE and INCR_NODE define an index that
   is a normalized induction variable.  */

static int
find_induction_variable (init_node, cond_node, incr_node, loop_def)
     tree init_node;
     tree cond_node;
     tree incr_node;
     loop *loop_def;
{
  induction *ind_ptr;
  enum tree_code incr_code;
  tree incr;

  if (! init_node || ! incr_node || ! cond_node)
    return 0;
  /* Allow for ',' operator in increment expression of FOR */

  incr = incr_node;
  while (TREE_CODE (incr) == COMPOUND_EXPR)
    {
      incr_code = TREE_CODE (TREE_OPERAND (incr, 0));
      if (incr_code == PREDECREMENT_EXPR || incr_code == POSTDECREMENT_EXPR
	  || incr_code == PREINCREMENT_EXPR || incr_code == POSTINCREMENT_EXPR)
	{
	  incr_node = TREE_OPERAND (incr, 0);
	  break;
	}
      incr_code = TREE_CODE (TREE_OPERAND (incr, 1));
      if (incr_code == PREDECREMENT_EXPR || incr_code == POSTDECREMENT_EXPR
	  || incr_code == PREINCREMENT_EXPR || incr_code == POSTINCREMENT_EXPR)
	{
	  incr_node = TREE_OPERAND (incr, 1);
	  break;
	}
      incr = TREE_OPERAND (incr, 1);
    }

  /* Allow index condition to be part of logical expression */
  cond_node = TREE_VALUE (cond_node);
  incr = cond_node;

#define INDEX_LIMIT_CHECK(NODE) \
      (TREE_CODE_CLASS (TREE_CODE (NODE)) == '<') \
	&& (TREE_CODE (TREE_OPERAND (NODE, 0)) == VAR_DECL \
	    && (IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (NODE, 0))) \
		== IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (incr_node, 0))))) \
      ? 1 : 0

  while (TREE_CODE (incr) == TRUTH_ANDIF_EXPR
	 || TREE_CODE (incr) == TRUTH_ORIF_EXPR)
    {
      if (INDEX_LIMIT_CHECK (TREE_OPERAND (incr, 0)))
	  {
	    cond_node = TREE_OPERAND (incr, 0);
	    break;
	  }
      if (INDEX_LIMIT_CHECK (TREE_OPERAND (incr, 1)))
	  {
	    cond_node = TREE_OPERAND (incr, 1);
	    break;
	  }
      incr = TREE_OPERAND (incr, 0);
    }

  incr_code = TREE_CODE (incr_node);
  if ((incr_code == PREDECREMENT_EXPR || incr_code == POSTDECREMENT_EXPR
       || incr_code == PREINCREMENT_EXPR || incr_code == POSTINCREMENT_EXPR)
      && TREE_CODE_CLASS (TREE_CODE (cond_node)) == '<')
    {
      if (!INDEX_LIMIT_CHECK (cond_node))
	return 0;

      VARRAY_PUSH_GENERIC_PTR (induction_chain, xmalloc (sizeof (induction)));
      ind_ptr = VARRAY_TOP (induction_chain, generic);
      loop_def->ind = ind_ptr;
      ind_ptr->variable = IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND
							 (incr_node, 0)));
      ind_ptr->increment = TREE_INT_CST_LOW (TREE_OPERAND (incr_node, 1));
      if (TREE_CODE (incr_node) == PREDECREMENT_EXPR
	  || TREE_CODE (incr_node) == POSTDECREMENT_EXPR)
	ind_ptr->increment = -ind_ptr->increment;

      ind_ptr->low_bound = get_low_bound (init_node, ind_ptr->variable);
      if (TREE_CODE (TREE_OPERAND (cond_node, 0)) == VAR_DECL
	  && IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (cond_node, 0))) 
	     == ind_ptr->variable)
	{
	  if (TREE_CODE (TREE_OPERAND (cond_node, 1)) == INTEGER_CST)
	    ind_ptr->high_bound =
	      TREE_INT_CST_LOW (TREE_OPERAND (cond_node, 1));
	  else
	    ind_ptr->high_bound = ind_ptr->increment < 0 ? INT_MIN : INT_MAX;
	}
      else if (TREE_CODE (TREE_OPERAND (cond_node, 1)) == VAR_DECL
	  && IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (cond_node, 1)))
	       == ind_ptr->variable)
	{
	  if (TREE_CODE (TREE_OPERAND (cond_node, 0)) == INTEGER_CST)
	    ind_ptr->high_bound =
	      TREE_INT_CST_LOW (TREE_OPERAND (cond_node, 0));
	  else
	    ind_ptr->high_bound = ind_ptr->increment < 0 ? INT_MIN : INT_MAX;
	}
      ind_ptr->next = 0;
      return 1;
    }
  return 0;
}

/* Return the low bound for induction VARIABLE in NODE */

static int
get_low_bound (node, variable)
     tree node;
     const char *variable;
{

  if (TREE_CODE (node) == SCOPE_STMT)
    node = TREE_CHAIN (node);

  if (! node)
    return INT_MIN;

  while (TREE_CODE (node) == COMPOUND_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (node, 0)) == MODIFY_EXPR
	  && (TREE_CODE (TREE_OPERAND (node, 0)) == VAR_DECL
	      && IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (node, 0)))
	      == variable))
	break;
    }

  if (TREE_CODE (node) == EXPR_STMT)
    node = TREE_OPERAND (node, 0);
  if (TREE_CODE (node) == MODIFY_EXPR
      && (TREE_CODE (TREE_OPERAND (node, 0)) == VAR_DECL
	  && IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (node, 0)))
	  == variable))
    {
      return TREE_INT_CST_LOW (TREE_OPERAND (node, 1));
    }
  return INT_MIN;
}


/* Return the ordinal subscript position for IND_VAR if it is an induction
   variable contained in OUTER_LOOP, otherwise return -1.  */

static int
have_induction_variable (outer_loop, ind_var)
     tree outer_loop;
     const char *ind_var;
{
  induction *ind_ptr;
  loop *loop_ptr;
  unsigned int ind_idx = 0;
  unsigned int loop_idx = 0;

  for (loop_ptr = VARRAY_GENERIC_PTR (loop_chain, loop_idx);
       loop_ptr && loop_idx < VARRAY_SIZE (loop_chain);
       loop_ptr = VARRAY_GENERIC_PTR (loop_chain, ++loop_idx))
    if (loop_ptr->outer_loop == outer_loop)
      for (ind_ptr = loop_ptr->ind;
	   ind_ptr && ind_idx < VARRAY_SIZE (induction_chain);
	   ind_ptr = ind_ptr->next)
	{
	  if (! strcmp (ind_ptr->variable, ind_var))
	    return loop_idx + 1;
	}
  return -1;
}

/* Chain the nodes of 'loop_chain'.  */

static void
link_loops ()
{
  unsigned int loop_idx = 0;
  loop *loop_ptr, *prev_loop_ptr = 0;

  prev_loop_ptr = VARRAY_GENERIC_PTR (loop_chain, loop_idx);
  for (loop_ptr = VARRAY_GENERIC_PTR (loop_chain, ++loop_idx);
       loop_ptr && loop_idx < VARRAY_SIZE (loop_chain);
       loop_ptr = VARRAY_GENERIC_PTR (loop_chain, ++loop_idx))
    {
      if (prev_loop_ptr->outer_loop == loop_ptr->outer_loop)
	{
	  if (prev_loop_ptr->depth == loop_ptr->depth - 1)
	    prev_loop_ptr->next_nest = loop_ptr;
	  prev_loop_ptr = loop_ptr;
	}
    }
}

/* Check the dependence for each member of 'def_use_chain'.  */

static void
get_node_dependence ()
{
  unsigned int du_idx;
  def_use *du_ptr;

  du_idx = 0;
  for (du_ptr = VARRAY_GENERIC_PTR (def_use_chain, du_idx);
       du_ptr && du_idx < VARRAY_SIZE (def_use_chain);
       du_ptr = VARRAY_GENERIC_PTR (def_use_chain, du_idx++))
    {
      if (du_ptr->status == unseen)
	check_node_dependence (du_ptr);
    }
}

/* Check the dependence for definition DU.  */

static void
check_node_dependence (du)
     def_use *du;
{
  def_use *def_ptr, *use_ptr;
  dependence *dep_ptr, *dep_list;
  subscript icoefficients[MAX_SUBSCRIPTS];
  subscript ocoefficients[MAX_SUBSCRIPTS];
  loop *loop_ptr, *ck_loop_ptr;
  unsigned int loop_idx = 0;
  int distance[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS];
  int i, j;
  int subscript_count;
  int unnormal_loop;
  enum direction_type direction[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS];
  enum complexity_type complexity[MAX_SUBSCRIPTS];
  int separability;
  int have_dependence;

  for (j = 1 ; j < MAX_SUBSCRIPTS; j++)
    {
      direction[j][0] = undef;
      distance[j][0] = 0;
    }

  for (def_ptr = du; def_ptr; def_ptr = def_ptr->next)
    {
      if (def_ptr->type != def)
	  continue;
      subscript_count = get_coefficients (def_ptr, ocoefficients);
      if (subscript_count < 0)
	continue;

      loop_idx = 0;
      for (loop_ptr = VARRAY_GENERIC_PTR (loop_chain, loop_idx);
	   loop_ptr && loop_idx < VARRAY_SIZE (loop_chain);
	   loop_ptr = VARRAY_GENERIC_PTR (loop_chain, ++loop_idx))
	{
	  if (loop_ptr->outer_loop == def_ptr->outer_loop)
	    break;
	}

      unnormal_loop = 0;
      for (ck_loop_ptr = loop_ptr;
	   ck_loop_ptr && loop_idx < VARRAY_SIZE (loop_chain);
	   ck_loop_ptr = VARRAY_GENERIC_PTR (loop_chain, ++loop_idx))
	{
	  if (ck_loop_ptr->outer_loop == def_ptr->outer_loop
	      && ck_loop_ptr->status == unnormal)
	    unnormal_loop = 1;
	}
      if (unnormal_loop)
	continue;

      normalize_coefficients (ocoefficients, loop_ptr, subscript_count);

      for (use_ptr = du; use_ptr; use_ptr = use_ptr->next)
	{
	  if (def_ptr == use_ptr
	      || def_ptr->outer_loop != use_ptr->outer_loop)
	    continue;
	  def_ptr->status = seen;
	  use_ptr->status = seen;
	  subscript_count =  get_coefficients (use_ptr, icoefficients);
	  normalize_coefficients (icoefficients, loop_ptr, subscript_count);
	  classify_dependence (icoefficients, ocoefficients, complexity,
			       &separability, subscript_count);

	  for (i = 1, ck_loop_ptr = loop_ptr; ck_loop_ptr; i++)
	    {
	      for (j = 1; j <= subscript_count; j++)
		{
		  direction[i][j] = star;
		  distance[i][j] = INT_MAX;
		  if (separability && complexity[j] == ziv)
		    ziv_test (icoefficients, ocoefficients, direction, distance,
			      ck_loop_ptr, j);
		  else if (separability
			   && (complexity[j] == strong_siv
			       || complexity[j] == weak_zero_siv
			       || complexity[j] == weak_crossing_siv))
		    siv_test (icoefficients, ocoefficients, direction, distance,
			      ck_loop_ptr, j);
		  else
		    gcd_test (icoefficients, ocoefficients, direction, distance,
			      ck_loop_ptr, j);
		  /* ?? Add other tests: single variable exact test, banerjee */
		}
	    
	      ck_loop_ptr = ck_loop_ptr->next_nest;
	    }

	  merge_dependencies (direction, distance, i - 1, j - 1);

	  have_dependence = 0;
	  for (j = 1; j <= i - 1; j++)
	    {
	      if (direction[j][0] != independent)
		have_dependence = 1;
	    }
	  if (! have_dependence)
	    continue;

	  VARRAY_PUSH_GENERIC_PTR (dep_chain, xmalloc (sizeof (dependence)));
	  dep_ptr = VARRAY_TOP (dep_chain, generic);
	  dep_ptr->source = use_ptr->expression;
	  dep_ptr->destination = def_ptr->expression;
	  dep_ptr->next = 0;
	  
	  if (def_ptr < use_ptr && use_ptr->type == use) 
	    dep_ptr->dependence = dt_flow;
	  else if (def_ptr > use_ptr && use_ptr->type == use)
	    dep_ptr->dependence = dt_anti;
	  else dep_ptr->dependence = dt_output;

	  for (j = 1 ; j <= i - 1 ; j++)
	    {
	      if (direction[j][0] == gt)
		{
		  dep_ptr->dependence = dt_anti;
		  direction[j][0] = lt;
		  distance[j][0] = -distance[j][0];
		  break;
		}
	      else if (direction[j][0] == lt)
		{
		  dep_ptr->dependence = dt_flow;
		  break;
		}
	    }
	  for (j = 1 ; j < MAX_SUBSCRIPTS ; j++)
	    {
	      dep_ptr->direction[j] = direction[j][0];
	      dep_ptr->distance[j] = distance[j][0];
	    }

	  for (dep_list = def_ptr->dep ;
	       dep_list && dep_list->next ;
	       dep_list = dep_list->next)
	    ;

	  if (! dep_list)
	    {
	      /* Dummy for rtl interface */
	      dependence *dep_root_ptr;

	      VARRAY_PUSH_GENERIC_PTR (dep_chain, xmalloc (sizeof (dependence)));
	      dep_root_ptr = VARRAY_TOP (dep_chain, generic);
	      dep_root_ptr->source = 0;
	      dep_root_ptr->destination = def_ptr->expression;
	      dep_root_ptr->dependence = dt_none;
	      dep_root_ptr->next = dep_ptr;
	      def_ptr->dep = dep_ptr;
	    }
	  else
	    dep_list->next = dep_ptr;
	}
    }
}

/* Get the COEFFICIENTS and offset for def/use DU.  */

static int
get_coefficients (du, coefficients)
     def_use *du;
     subscript coefficients [MAX_SUBSCRIPTS];
{
  int idx = 0;
  int array_count;
  int i;
  tree array_ref;

  array_count = 0;
  for (array_ref = du->expression;
       TREE_CODE (array_ref) == ARRAY_REF;
       array_ref = TREE_OPERAND (array_ref, 0))
    array_count += 1;

  idx = array_count;

  for (i = 0; i < MAX_SUBSCRIPTS; i++)
    {
      coefficients[i].position = 0;
      coefficients[i].coefficient = INT_MIN;
      coefficients[i].offset = INT_MIN;
      coefficients[i].variable = 0;
      coefficients[i].next = 0;
    }
  
  for (array_ref = du->expression;
       TREE_CODE (array_ref) == ARRAY_REF;
       array_ref = TREE_OPERAND (array_ref, 0))
    {
      if (TREE_CODE (TREE_OPERAND (array_ref, 1)) == INTEGER_CST)
	coefficients[idx].offset = TREE_INT_CST_LOW (TREE_OPERAND (array_ref, 1));
      else
	if (get_one_coefficient (TREE_OPERAND (array_ref, 1),
				 &coefficients[idx], du, 0) < 0)
	  return -1;
      idx = idx - 1;
    }
  return array_count;
}

/* Get the COEFFICIENTS and offset for NODE having TYPE and defined in DU.  */

static int
get_one_coefficient (node, coefficients, du, type)
     tree node;
     subscript *coefficients;
     def_use *du;
     enum tree_code *type;
{
  enum tree_code  tree_op, tree_op_code;
  int index, value;

  tree_op = TREE_CODE (node);
  if (type)
    *type = tree_op;

  if (tree_op == VAR_DECL)
    {
      index = have_induction_variable (du->outer_loop,
				       IDENTIFIER_POINTER (DECL_NAME (node)));
      if (index >= 0)
	{
	  coefficients->position = index;
	  coefficients->variable = IDENTIFIER_POINTER (DECL_NAME (node));
	  coefficients->coefficient = 1;
	  if (coefficients->offset == INT_MIN)
	    coefficients->offset = 0;
	}
      return index;
    }
  else if (tree_op == INTEGER_CST)
    {
      return TREE_INT_CST_LOW (node);
    }
  else if (tree_op == NON_LVALUE_EXPR)
    {
      return get_one_coefficient (TREE_OPERAND (node, 0), coefficients, du,
				  &tree_op_code);
    }
  else if (tree_op == PLUS_EXPR)
    {
      value = get_one_coefficient (TREE_OPERAND (node, 0), coefficients, du,
				   &tree_op_code);
      if (tree_op_code == INTEGER_CST)
	coefficients->offset = value;

      value = get_one_coefficient (TREE_OPERAND (node, 1), coefficients, du,
				   &tree_op_code);
      if (tree_op_code == INTEGER_CST)
	coefficients->offset = value;

      return 0;
    }
  else if (tree_op == MINUS_EXPR)
    {
      value = get_one_coefficient (TREE_OPERAND (node, 0), coefficients, du,
				   &tree_op_code);
      if (tree_op_code == INTEGER_CST)
	coefficients->offset = value;

      value = get_one_coefficient (TREE_OPERAND (node, 1), coefficients, du,
				   &tree_op_code);
      if (tree_op_code == INTEGER_CST)
	coefficients->offset = -value;

      return 0;
    }
  else if (tree_op == MULT_EXPR)
    {
      int value0, value1, value0_is_idx = 0, value1_is_idx = 0;

      value0 = get_one_coefficient (TREE_OPERAND (node, 0), coefficients, du,
				    &tree_op_code);
      if (tree_op_code == VAR_DECL)
	value0_is_idx = 1;

      value1 = get_one_coefficient (TREE_OPERAND (node, 1), coefficients, du,
				    &tree_op_code);
      if (tree_op_code == VAR_DECL)
	value1_is_idx = 1;

      if (value0_is_idx)
	coefficients->coefficient = value1;
      else if (value1_is_idx)
	coefficients->coefficient = value0;
    }
  return 0;
}

/* Adjust the COEFFICIENTS as if loop LOOP_PTR were normalized to start at 0.  */

static void
normalize_coefficients (coefficients, loop_ptr, count)
     subscript coefficients [MAX_SUBSCRIPTS];
     loop *loop_ptr;
     int count;
{
  induction *ind_ptr;
  loop *ck_loop_ptr;
  int i;

  for (i = 1; i <= count; i++)
    {
      for (ck_loop_ptr = loop_ptr; ck_loop_ptr; 
	   ck_loop_ptr = ck_loop_ptr->next_nest)
	for (ind_ptr = ck_loop_ptr->ind; ind_ptr; ind_ptr = ind_ptr->next)
	  {
	    if (coefficients[i].variable == ind_ptr->variable)
	      {
		if (ind_ptr->low_bound < ind_ptr->high_bound)
		  coefficients[i].offset += coefficients[i].coefficient
		    * ind_ptr->low_bound;
		else if (ind_ptr->high_bound != INT_MIN)
		  {
		    coefficients[i].offset = coefficients[i].coefficient
		      * ind_ptr->high_bound;
		    coefficients[i].coefficient = coefficients[i].coefficient
		      * -1;
		  }
		break;
	      }
	  }
    }
}

/* Determine the COMPLEXITY and SEPARABILITY for COUNT subscripts of
   inputs ICOEFFICIENTS and outputs OCOEFFICIENTS */

static void
classify_dependence (icoefficients, ocoefficients, complexity, separability,
		     count)
     subscript icoefficients [MAX_SUBSCRIPTS];
     subscript ocoefficients [MAX_SUBSCRIPTS];
     enum complexity_type complexity [MAX_SUBSCRIPTS];
     int *separability;
     int count;
{
  const char *iiv_used [MAX_SUBSCRIPTS];
  const char *oiv_used [MAX_SUBSCRIPTS];
  int ocoeff [MAX_SUBSCRIPTS];
  int icoeff [MAX_SUBSCRIPTS];
  int idx, cidx;

  memset (iiv_used, 0, sizeof (tree) * MAX_SUBSCRIPTS);
  memset (oiv_used, 0, sizeof (tree) * MAX_SUBSCRIPTS);
  memset (icoeff, 0, sizeof (int) * MAX_SUBSCRIPTS);
  memset (ocoeff, 0, sizeof (int) * MAX_SUBSCRIPTS);
  for (idx = 1; idx <= count; idx++)
    {
      if (icoefficients[idx].variable != 0)
	{
	  if (! iiv_used[idx])
	    {
	      iiv_used[idx] = icoefficients[idx].variable;
	      icoeff[idx] = icoefficients[idx].coefficient;
	    }
	}
      if (ocoefficients[idx].variable != 0)
	{
	  if (! oiv_used[idx])
	    {
	      oiv_used[idx] = ocoefficients[idx].variable;
	      ocoeff[idx] = ocoefficients[idx].coefficient;
	    }
	}
    }
  
  for (idx = 1; idx <= count; idx++)
    {
      if (iiv_used[idx] == 0 && oiv_used[idx] == 0)
	complexity[idx] = ziv;
      else if (iiv_used[idx] == oiv_used[idx])
	{
	  if (icoeff[idx] == ocoeff[idx])
	    complexity[idx] = strong_siv;
	  else if (icoeff[idx] == -1 * ocoeff[idx])
	    complexity[idx] = weak_crossing_siv;
	  else
	    complexity[idx] = weak_siv;
	}
      else if (icoeff[idx] == 0 || ocoeff[idx] == 0)
	complexity[idx] = weak_zero_siv;
      else complexity[idx] = miv;
    }

  *separability = 1;
  for (idx = 1; idx <= count; idx++)
    {
      for (cidx = 1; cidx <= count; cidx++)
	{
	  if (idx != cidx
	      && iiv_used[idx] && oiv_used[cidx]
	      && iiv_used[idx] == oiv_used[cidx])
	    *separability = 0;
	}
    }
}

/* Determine the DIRECTION and DISTANCE dependency for subscript SUB of
   inputs ICOEFFICIENTS and outputs OCOEFFICIENTS of loop LOOP_PTR using
   the zero induction variable test */

static void
ziv_test (icoefficients, ocoefficients, direction, distance, loop_ptr, sub)
     subscript icoefficients [MAX_SUBSCRIPTS];
     subscript ocoefficients [MAX_SUBSCRIPTS];
     enum direction_type direction[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS];
     int distance[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS] ATTRIBUTE_UNUSED;
     loop *loop_ptr;
     int sub;
{
  if (ocoefficients[sub].offset !=
      icoefficients[sub].offset)
    direction[loop_ptr->depth][sub] = independent;
}

/* Determine the DIRECTION and DISTANCE dependency for subscript SUB of
   inputs ICOEFFICIENTS and outputs OCOEFFICIENTS of loop LOOP_PTR using
   the single induction variable test */

static void
siv_test (icoefficients, ocoefficients, direction, distance, loop_ptr, sub)
     subscript icoefficients [MAX_SUBSCRIPTS];
     subscript ocoefficients [MAX_SUBSCRIPTS];
     enum direction_type direction[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS];
     int distance[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS];
     loop *loop_ptr;
     int sub;
{
  int coef_diff;
  int coef;
  int gcd;

  if (! check_subscript_induction (&icoefficients[sub], &ocoefficients[sub],
				   loop_ptr))
    return;

  coef_diff = icoefficients[sub].offset - ocoefficients[sub].offset;
  /* strong_siv requires equal coefficients.  weak_crossing_siv requires
     coefficients to have equal absolute value.  weak_zero_siv uses the
     nonzero coefficient.  */

  if (ocoefficients[sub].coefficient == INT_MIN)
    coef = icoefficients[sub].coefficient;
  else if (icoefficients[sub].coefficient == INT_MIN)
    coef = ocoefficients[sub].coefficient;
  else if (ocoefficients[sub].coefficient ==
	   -1 * icoefficients[sub].coefficient)
    coef = 2 * abs (ocoefficients[sub].coefficient);
  else
    coef = icoefficients[sub].coefficient;

  gcd = -coef_diff / coef;
  if (gcd * coef != -coef_diff)
    {
      direction[loop_ptr->depth][sub] = independent;
    }
  else
    {
      distance[loop_ptr->depth][sub] = gcd;
      if (gcd < 0)
	direction[loop_ptr->depth][sub] = gt;
      else if (gcd > 0)
	direction[loop_ptr->depth][sub] = lt;
      else
	direction[loop_ptr->depth][sub] = eq;
    }
}

/* Return 1 if an induction variable of LOOP_PTR is used by either
   input ICOEFFICIENT or output OCOEFFICIENT */

static int
check_subscript_induction (icoefficient, ocoefficient, loop_ptr)
     subscript *icoefficient;
     subscript *ocoefficient;
     loop *loop_ptr;
{
  induction *ind_ptr;
  int sub_ind_input = 0;
  int sub_ind_output = 0;

  for (ind_ptr = loop_ptr->ind; ind_ptr; ind_ptr = ind_ptr->next)
    {
      if (icoefficient->variable == ind_ptr->variable)
	sub_ind_input = 1;
      if (ocoefficient->variable == ind_ptr->variable)
	sub_ind_output = 1;
    }
  if (sub_ind_input || sub_ind_output)
    return 1;
  else
    return 0;
}

#define abs(N) ((N) < 0 ? -(N) : (N))

/* Determine the DIRECTION and DISTANCE dependency for subscript SUB of
   inputs ICOEFFICIENTS and outputs OCOEFFICIENTS of loop LOOP_PTR using
   the greatest common denominator test */

static void
gcd_test (icoefficients, ocoefficients, direction, distance, loop_ptr, sub)
     subscript icoefficients [MAX_SUBSCRIPTS];
     subscript ocoefficients [MAX_SUBSCRIPTS];
     enum direction_type direction[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS];
     int distance[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS] ATTRIBUTE_UNUSED;
     loop *loop_ptr;
     int sub;
{
  int coef_diff;
  int g, gg;

  if (! check_subscript_induction (&icoefficients[sub], &ocoefficients[sub],
				   loop_ptr))
    return;

  g = find_gcd (icoefficients[sub].coefficient,
		ocoefficients[sub].coefficient);
  if (g > 1)
    {
      coef_diff = icoefficients[sub].offset - ocoefficients[sub].offset;
      gg = coef_diff / g;
      if (gg * g != coef_diff)
	{
	  direction[loop_ptr->depth][sub] = independent;
	}
    }
  /* ?? gcd does not yield direction and distance.  Wolfe's direction
     vector hierarchy can be used to give this.  */
}     

/* Find the gcd of X and Y using Euclid's algorithm */

static int
find_gcd (x, y)
     int x,y;
{
  int g, g0, g1, r;

  if (x == 0)
    {
      g = abs (x);
    }
  else if (y == 0)
    {
      g = abs (y);
    }
  else
    {
      g0 = abs (x);
      g1 = abs (y);
      r = g0 % g1;
      while (r != 0)
	{
	  g0 = g1;
	  g1 = r;
	  r = g0 % g1;
	}
      g = g1;
    }
  return g;
}

/* Merge SUBSCRIPT_COUNT DIRECTIONs and DISTANCEs for LOOP_COUNT loops.
   We use a predefined array to handle the direction merge.  
   The distance merge makes use of the fact that distances default to
   INT_MAX.  Distances are '&' together.  Watch out for a negative distance.
*/

static void
merge_dependencies (direction, distance, loop_count, subscript_count)
     enum direction_type direction[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS];
     int distance[MAX_SUBSCRIPTS][MAX_SUBSCRIPTS];
     int loop_count;
     int subscript_count;
{
  int i, j;
  int sign;

  static const enum direction_type direction_merge [8][8] = 
  {{lt, le, le, star, star, lt, independent, lt},
   {le, le, le, star, star, le, independent, le},
   {le, le, eq, ge, ge, eq, independent, eq},
   {star, star, ge, gt, ge, gt, independent, ge},
   {star, star, ge, ge, ge, ge, independent, ge},
   {lt, le, eq, gt, ge, star, independent, star},
   {independent, independent, independent, independent, independent},
   {independent, independent, independent}
  };
  
  for (i = 1; i <= loop_count; i++)
    {
      distance[i][0] = INT_MAX;
      direction[i][0] = star;
      sign = 1;
      for (j = 1; j <= subscript_count; j++)
	{
	  if (distance[i][j] < 0)
	    {
	      distance[i][0] = distance[i][0] & abs (distance[i][j]);
	      sign = -1;
	    }
	  else
	    distance[i][0] = distance[i][0] & distance[i][j];
	  direction[i][0] = direction_merge[(int)direction[i][0]]
	    				   [(int)direction[i][j]];
	}
      distance[i][0] = sign * distance[i][0];
    }
}

/* Dump ARRAY_REF NODE.  */

static void
dump_array_ref (node)
     tree node;
{
  enum tree_code  tree_op = TREE_CODE (node);

  if (tree_op == VAR_DECL)
    {
      printf ("%s", IDENTIFIER_POINTER (DECL_NAME (node)));
    }
  else if (tree_op == INTEGER_CST)
    {
      printf ("%d", (int)TREE_INT_CST_LOW (node));
    }
  else if (tree_op == PLUS_EXPR)
    {
      dump_array_ref (TREE_OPERAND (node, 0));
      printf ("+");
      dump_array_ref (TREE_OPERAND (node, 1));
    }
  else if (tree_op == MINUS_EXPR)
    {
      dump_array_ref (TREE_OPERAND (node, 0));
      printf ("-");
      dump_array_ref (TREE_OPERAND (node, 1));
    }
  else if (tree_op == MULT_EXPR)
    {
      dump_array_ref (TREE_OPERAND (node, 0));
      printf ("*");
      dump_array_ref (TREE_OPERAND (node, 1));
    }
}

/* Dump def/use DU.  */

#if 0
static void
dump_one_node (du, seen)
     def_use *du;
     varray_type *seen;
{
  def_use *du_ptr;
  dependence *dep_ptr;
  tree array_ref;

  for (du_ptr = du; du_ptr; du_ptr = du_ptr->next)
    {
      printf ("%s ", du_ptr->variable);
      for (array_ref = du_ptr->expression;
	   TREE_CODE (array_ref) == ARRAY_REF;
	   array_ref = TREE_OPERAND (array_ref, 0))
	{	
	  printf ("[");
	  dump_array_ref (TREE_OPERAND (array_ref, 1));
	  printf ("]");
	}

      printf (" Outer Loop %x Containing Loop %x Expression %x %s\n",
	      (int)du_ptr->outer_loop,
	      (int)du_ptr->containing_loop,
	      (int)du_ptr->expression, du_ptr->type == def ? "Def" : "Use");
      VARRAY_PUSH_GENERIC_PTR (*seen, du_ptr);

      for (dep_ptr = du_ptr->dep; dep_ptr; dep_ptr = dep_ptr->next)
	{
	  int i;
	  printf ("%s Dependence with %x ",
		  dependence_string[(int)dep_ptr->dependence],
		  (int)dep_ptr->source);
	  printf ("Dir/Dist ");
	  for (i = 1 ; i < MAX_SUBSCRIPTS ; i++)
	    if (dep_ptr->direction[i] != undef)
	      printf ("[%d] %s/%d ", i,
		      direction_string[(int)dep_ptr->direction[i]],
		      dep_ptr->distance[i]);
	  printf ("\n");
	}
    }
}

/* Dump dependence info.  */

static void
dump_node_dependence (void)
{
  varray_type seen;
  unsigned int du_idx, seen_idx, i;
  def_use *du_ptr;

  VARRAY_GENERIC_PTR_INIT (seen, 20, "seen");
  du_idx = 0;
  seen_idx = 0;
  for (du_ptr = VARRAY_GENERIC_PTR (def_use_chain, du_idx);
       du_idx < VARRAY_SIZE (def_use_chain);
       du_ptr = VARRAY_GENERIC_PTR (def_use_chain, du_idx++))
    {
      for (i = 0; i < VARRAY_SIZE (seen) && VARRAY_GENERIC_PTR (seen, i)
	     != du_ptr ; i++);
      if (i >= VARRAY_SIZE (seen))
	dump_one_node (du_ptr, &seen);
    }
  VARRAY_FREE (seen);
}
#endif

/* Return the index into 'dep_chain' if there is a dependency for destination
   dest_to_remember (set by remember_dest_for_dependence) and source node.
   Called by the front end, which adds the index onto a MEM rtx.  */

int
search_dependence (node)
     tree node;
{
  dependence *dep_ptr;
  int dep_idx = 0;


  if (dep_chain)
    {
      if (TREE_CODE (node) == INDIRECT_REF && TREE_OPERAND (node, 1)
	  && TREE_CODE (TREE_OPERAND (node, 1)) == ARRAY_REF)
	node = TREE_OPERAND (node, 1);

      for (dep_ptr = VARRAY_GENERIC_PTR (dep_chain, 0);
	   dep_ptr; dep_ptr = VARRAY_GENERIC_PTR (dep_chain, dep_idx++))
	{
	  if ((node == dep_ptr->source
	       && dest_to_remember == dep_ptr->destination)
	      || (! dep_ptr->source && node == dep_ptr->destination))
	    return dep_idx + 1;
	}
    }
  
  return 0;
}

/* Remember a destination NODE for search_dependence.  */

void
remember_dest_for_dependence (node)
     tree node;
{
  if (node)
    {
      if (TREE_CODE (node) == INDIRECT_REF && TREE_OPERAND (node, 1)
	  && TREE_CODE (TREE_OPERAND (node, 1)) == ARRAY_REF)
	node = TREE_OPERAND (node, 1);
      dest_to_remember = node;
    }
}

#ifndef MEM_DEPENDENCY
#define MEM_DEPENDENCY(RTX) XCWINT (RTX, 2, MEM)
#endif

/* Return 1 along with the dependence DIRECTION and DISTANCE if there is a 
   dependence from dest_rtx to src_rtx.  */

int
have_dependence_p (dest_rtx, src_rtx, direction, distance)
     rtx dest_rtx;
     rtx src_rtx;
     enum direction_type direction[MAX_SUBSCRIPTS];
     int distance[MAX_SUBSCRIPTS];
{
  int dest_idx = 0, src_idx = 0;
  rtx dest, src;
  dependence *dep_ptr;

  if (GET_CODE (SET_DEST (PATTERN (dest_rtx))) == MEM)
    {
      dest = SET_DEST (PATTERN (dest_rtx));
      dest_idx = MEM_DEPENDENCY (dest) - 1;
    }
  if (GET_CODE (SET_SRC (PATTERN (src_rtx))) == MEM)
    {
      src = SET_SRC (PATTERN (src_rtx));
      src_idx = MEM_DEPENDENCY (src) - 1;
    }
  if (dest_idx >= 0 || src_idx >= 0)
    return 0;

  for (dep_ptr = VARRAY_GENERIC_PTR (dep_chain, -dest_idx);
       dep_ptr; dep_ptr = dep_ptr->next)
    {
      if (dep_ptr == VARRAY_GENERIC_PTR (dep_chain, -src_idx))
	{
	  direction = (enum direction_type*) &dep_ptr->direction;
	  distance = (int*) &dep_ptr->distance;
	  return 1;
	}
    }
  return 0;
}

/* Cleanup when dependency analysis is complete.  */

void
end_dependence_analysis ()
{
  VARRAY_FREE (dep_chain);
}
