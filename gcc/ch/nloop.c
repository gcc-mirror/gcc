/* Implement looping actions for CHILL.
   Copyright (C) 1992, 93, 94, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <stdio.h>
#include <limits.h>
#include "config.h"
#include "tree.h"
#include "ch-tree.h"
#include "lex.h"
#include "flags.h"
#include "actions.h"
#include "input.h"
#include "obstack.h"
#include "assert.h"
#include "rtl.h"

/* if the user codes '-flocal-loop-counter' on the command line,
   ch-actions.c (lang_decode_option) will set this flag. */
int flag_local_loop_counter = 0;

extern tree chill_truthvalue_conversion PARAMS ((tree));
extern rtx  emit_line_note              PARAMS ((char *, int)); 
extern void error                       PARAMS ((char *, ...));
extern rtx  expand_assignment           PARAMS ((tree, tree, int, int));
extern void save_expr_under_name        PARAMS ((tree, tree));
extern void stamp_nesting_label         PARAMS ((tree));
extern int  int_fits_type_p             PARAMS ((tree, tree));
extern void warning                     PARAMS ((char *, ...));

/* forward declarations */
static int  classify_loop            PARAMS ((void));
static int  declare_temps            PARAMS ((void));
static int  initialize_iter_var      PARAMS ((void));
static int  maybe_skip_loop          PARAMS ((void));
static int  top_loop_end_check       PARAMS ((void));
static int  bottom_loop_end_check    PARAMS ((void));
static int  increment_temps          PARAMS ((void));
static tree build_temporary_variable PARAMS ((char *, tree));
static tree maybe_make_for_temp      PARAMS ((tree, char *, tree));
static tree chill_unsigned_type      PARAMS ((tree));

/* In terms of the parameters passed to build_loop_iterator,
 *   there are several types of loops.  They are encoded by
 *   the ITER_TYPE enumeration.
 *
 *   1) DO FOR EVER; ... OD
 *      indicated by a NULL_TREE start_exp, step_exp and end_exp,
 *      condition == NULL, in_flag = 0, and ever_flag == 1 in the
 *      first ITERATOR.
 *
 *   2) DO WHILE cond; ... OD
 *      indicated by NULL_TREE start_exp, step_exp and end_exp, 
 *      in_flag = 0, and condition != NULL.
 *
 *   3) DO; ... OD
 *      indicated by NULL_TREEs in start_exp, step_exp and end_exp,
 *      condition != NULL, in_flag == 0 and ever_flag == 0.  This
 *      is not really a loop, but a compound statement.
 *
 *   4) DO FOR user_var := start_exp 
 *         [DOWN] TO end_exp BY step_exp; ... DO
 *      indicated by non-NULL_TREE start_exp, step_exp and end_exp.
 *
 *   5) DO FOR user_var [DOWN] IN discrete_mode; ... OD
 *      indicated by in_flag == 1.  start_exp is a non-NULL_TREE 
 *      discrete mode, with an optional down_flag.
 *
 *   6) DO FOR user_var [DOWN] IN powerset_expr; ... OD
 *      indicated by in_flag == 1.  start_exp is a non-NULL_TREE 
 *      powerset mode, with an optional down_flag.
 *
 *   7) DO FOR user_var [DOWN] IN location; ... OD
 *      indicated by in_flag == 1.  start_exp is a non-NULL_TREE 
 *      location mode, with an optional down_flag.
 */
typedef enum 
{
   DO_UNUSED,
   DO_FOREVER,
   DO_WHILE,
   DO_OD,
   DO_STEP,
   DO_RANGE,
   DO_POWERSET,
   DO_LOC,
   DO_LOC_VARYING 
} ITER_TYPE;


typedef struct iterator 
{
/* These variables only have meaning in the first ITERATOR structure. */
  ITER_TYPE itype;                  /* type of this iterator */
  int  error_flag;                  /* TRUE if no loop was started due to 
				       user error */
  tree condition;                   /* WHILE condition expression */
  int  down_flag;                   /* TRUE if DOWN was coded */

/* These variables have meaning in every ITERATOR structure. */
  tree user_var;                    /* user's explicit iteration variable */
  tree start_exp;                   /* user's start expression
                                       or IN expression of a FOR .. IN*/
  tree step_exp;                    /* user's step expression */
  tree end_exp;                     /* user's end expression */
  tree start_temp;                  /* temp holding evaluated start_exp */
  tree end_temp;                    /* temp holding evaluated end_exp */
  tree step_temp;                   /* temp holding evaluated step_exp */
  tree powerset_temp;               /* temp holding user's initial powerset expression */
  tree loc_ptr_temp;                /* temp holding count for LOC enumeration ptr */
  tree iter_var;                    /* hidden variable for the loop */
  tree iter_type;                   /* hidden variable's type */
  tree base_type;                   /* LOC enumeration base type */
  struct iterator *next;            /* ptr to next iterator for this loop */
} ITERATOR;

/*
 * There's an entry like this for each nested DO loop.
 * The list is maintained by push_loop_block
 * and pop_loop_block.
 */
typedef struct loop {
  struct loop *nxt_level;   /* pointer to enclosing loop */
  ITERATOR    *iter_list;   /* iterators for the current loop */
} LOOP;

static LOOP *loop_stack = (LOOP *)0;

#if 0

Here is a CHILL DO FOR statement:

DO FOR user_var := start_exp BY step_exp [DOWN] TO end_exp 
   WHILE condition;

For this loop to be 'safe', like a Pascal FOR loop, the start,
end, and increment expressions are computed once, before the
assignment to the iteration variable and saved in temporaries,
before the first assignment of the iteration variable, so the
following works:

          FOR i := (i+1) TO (i+10) DO

To prevent changes to the start/end/step expressions from
effecting the loop''s termination, and to make the loop end-check
as simple as possible, we evaluate the step expression into
a temporary and compute a hidden iteration count before entering 
the loop''s body.  User code cannot effect the counter, and the
end-loop check simply decrements the counter and checks for zero.

The whole phrase FOR iter := ... TO end_exp can be repeated
multiple times, with different user-iteration variables.  This
is discussed later.

The loop counter calculations need careful design since a loop
from MININT TO MAXINT must work, in the precision of integers.

Here''s how it works, in C:

        0) The DO ... OD loop is simply a block with 
           its own scope.  

	1) The DO FOR EVER is simply implemented:

	   loop_top:
		.
		. body of loop
		.
		goto loop_top
	   end_loop:

	2) The DO WHILE is also simple:


	   loop_top:
		if (!condition) goto end_loop
		.
		. body of loop
		.
		goto loop_top
	   end_loop:


	3) The DO FOR [while condition] loop (no DOWN)

	push a new scope,
	decl iter_var

		step_temp = step_exp
                start_temp = start_exp
                end_temp = end_exp
		if (end_exp < start_exp) goto end_loop
                /* following line is all unsigned arithmetic */
		iter_var = (end_exp - start_exp + step_exp) / step_exp
                user_var = start_temp
	   loop_top:
		if (!condition) goto end_loop
		.
		. body of loop
		.
                iter_var--
		if (iter_var == 0) goto end_loop
                user_var += step_temp
		goto loop_top
	end_loop:
	pop scope

	4) The proposed CHILL for [while condition] loop (with DOWN)

	push a new scope,
        decl iter
		step_temp = step_exp
                start_temp = start_exp
                end_temp = end_exp
		if (end_exp > start_exp) goto end_loop
                /* following line is all unsigned arithmetic */
		iter_var = (start_exp - end_exp + step_exp) / step_exp
                user_var = start_temp
	   loop_top:
		if (!condition) goto end_loop
		.
		. body of loop
		.
                iter_var--
		if (iter_var == 0) goto end_loop
		user_var -= step_temp
		goto loop_top
	    end_loop:
	pop scope


        5) The range loop, which iterates over a mode''s possible
           values, works just like the above step loops, but with
           the start and end values taken from the mode''s lower
           and upper domain values.


	6) The FOR IN loop, where a location enumeration is
           specified (see spec on page 81 of Z.200, bottom
           of page 186):

	push a new scope,
        decl iter_var as an unsigned integer
             loc_ptr_temp as pointer to a composite base type
        
               if array is varying
                   iter_var = array''s length field
               else
                   iter_var = sizeof array / sizeof base_type
	       loc_ptr_temp = &of highest or lowest indexable entry
	   loop_top:
		if (!condition) goto end_loop
		.
		. body of loop
		.
                iter_var--
                if (iter_var == 0) goto end_loop               
		loc_ptr_temp +/-= sizeof array base_type
		goto loop_top
	   end_loop:
	pop scope

	7) The DO FOR (DOWN) IN powerset_exp

	push a new scope,
        decl powerset_temp
	decl iterator as basetype of powerset

	        powerset_temp := start_exp
	   loop_top:
	        /* if DOWN */
                if (__flsetclrpowerset () == 0) goto end_loop;
                /* not DOWN */
                if (__ffsetclrpowerset () == 0) goto end_loop;
		if (!condition) goto end_loop
		.
		. body of loop
		.
		goto loop_top
	   end_loop:
	pop scope


So, here''s the general DO FOR schema, as implemented here:

        classify_loop       -- what type of loop have we?
                            -- build_iterator does some of this, also
        expand_start_loop   -- start the loop''s control scope
        -- start scope for synthesized loop variables
        declare_temps       -- create, initialize temporary variables
        maybe_skip_loop     -- skip loop if end conditions unsatisfiable
        initialize_iter_var -- initialize the iteration counter
                            -- initialize user''s loop variable
        expand_start_loop   -- generate top-of-loop label
        top_loop_end_check  -- generate while code and/or
                               powerset find-a-bit function call
        .
        .
        .  user''s loop body code
        .
        .
        bottom_loop_end_check  -- exit if counter has become zero
        increment_temps     -- update temps for next iteration
        expand_end_loop     -- generate jump back to top of loop
        expand_end_cond     -- generate label for end of conditional
        -- end of scope for synthesized loop variables
        free_iterators      -- free up iterator space

When there are two or more iterator phrases, each of the
above loop steps must act upon all iterators.  For example,
the 'increment_temps' step must increment all temporaries
(associated with all iterators).

 NOTE: Z.200, section 10.1 says that a block is ...
       "the actions statement list in a do action, including any
       loop counter and while control".  This means that an exp-
       ression in a WHILE control can include references to the
       loop counters created for the loop''s exclusive use.  
       Example:

             DCL a (1:10) INT;
             DCL j INT;
             DO FOR j IN a WHILE j > 0;
             ...
             OD;
       The 'j' referenced in the while is the loc-identity 'j'
       created inside the loop''s scope, and NOT the 'j' declared
       before the loop.
#endif

/*
 * The following routines are called directly by the
 * CHILL parser.
 */
void
push_loop_block ()
{
  LOOP *temp = (LOOP *)xmalloc (sizeof (LOOP));

  /* push a new loop onto the stack */
  temp->nxt_level = loop_stack;
  temp->iter_list = (ITERATOR *)0;
  loop_stack = temp;
}

void
pop_loop_block ()
{
  LOOP *do_temp = loop_stack;
  ITERATOR  *ip;

  /* pop loop block off the list */
  loop_stack = do_temp->nxt_level;

  /* free the loop's iterator blocks */
  ip = do_temp->iter_list;
  while (ip != NULL)
    {
      ITERATOR *temp = ip->next;
      free (ip);
      ip = temp;
    }
  free (do_temp);
}

void
begin_loop_scope ()
{
  ITERATOR *firstp = loop_stack->iter_list;

  if (pass < 2)
    return;

  /*
   * We need to classify the loop and declare its temporaries
   * here, so as to define them before the WHILE condition
   * (if any) is parsed.  The WHILE expression may refer to
   * a temporary.
   */
  if (classify_loop ())
    return;

  if (firstp->itype != DO_OD)
    declare_temps ();
  
  clear_last_expr ();
  push_momentary ();
  expand_start_bindings (0);
}


void
end_loop_scope (opt_label)
     tree opt_label;
{
  if (opt_label)
    possibly_define_exit_label (opt_label);
  poplevel (0, 0, 0);

  if (pass < 2)
    return;

  expand_end_bindings (getdecls (), kept_level_p (), 0);
  pop_momentary ();
}

/* The iterator structure records all aspects of a 
 * 'FOR i := start [DOWN] TO end' clause or
 * 'FOR i IN modename' or 'FOR i IN powerset' clause.
 * It's saved on the iter_list of the current LOOP.
 */
void
build_loop_iterator (user_var, start_exp, step_exp, end_exp, 
		     down_flag, in_flag, ever_flag)
     tree user_var, start_exp, step_exp, end_exp;
     int  down_flag, in_flag, ever_flag;
{
  ITERATOR *ip = (ITERATOR *)xmalloc (sizeof (ITERATOR));

  /* chain this iterator onto the current loop */
  if (loop_stack->iter_list == NULL)
    loop_stack->iter_list = ip;
  else
    {
      ITERATOR *temp = loop_stack->iter_list;
      while (temp->next != NULL)
	temp = temp->next;
      temp->next = ip;
    }

  ip->itype         = DO_UNUSED;
  ip->user_var      = user_var;
  ip->start_exp     = start_exp;
  ip->step_exp      = step_exp;
  ip->end_exp       = end_exp;
  ip->condition     = NULL_TREE;
  ip->start_temp    = NULL_TREE;
  ip->end_temp      = NULL_TREE;
  ip->step_temp     = NULL_TREE;
  ip->down_flag     = down_flag;
  ip->powerset_temp = NULL_TREE;
  ip->iter_var      = NULL_TREE;
  ip->iter_type     = NULL_TREE;
  ip->loc_ptr_temp  = NULL_TREE;
  ip->error_flag    = 1;          /* assume error will be found */
  ip->next          = (ITERATOR *)0;

  if (ever_flag)
    ip->itype = DO_FOREVER;
  else if (in_flag && start_exp != NULL_TREE)
    {
      if (TREE_CODE (start_exp) == ERROR_MARK)
	return;
      if (TREE_CODE (TREE_TYPE (start_exp)) == SET_TYPE)
	ip->itype = DO_POWERSET;
      else if (discrete_type_p (TREE_TYPE (ip->start_exp)))
	ip->itype = DO_RANGE;
      else if (TREE_CODE (TREE_TYPE (ip->start_exp)) == ARRAY_TYPE)
	ip->itype = DO_LOC;
      else if (chill_varying_type_p (TREE_TYPE (ip->start_exp)))
	ip->itype = DO_LOC_VARYING;
      else
	{
	  error ("Loop's IN expression is not a composite object");
	  return;
	}
    }
  else if (start_exp == NULL_TREE && end_exp == NULL_TREE
	   && step_exp == NULL_TREE && !down_flag)
    ip->itype = DO_OD;
  else
    {
      /* FIXME: Move this to the lexer? */
#define CST_FITS_INT(NODE) (TREE_CODE(NODE) == INTEGER_CST &&\
            int_fits_type_p (NODE, integer_type_node))

      tree max_prec_type = integer_type_node;

      if (! discrete_type_p (TREE_TYPE (ip->start_exp)))
	{
	  error ("start expr must have discrete mode");
	  return;
	}
      if (TREE_CODE (TREE_TYPE (ip->start_exp)) == ENUMERAL_TYPE
	  && CH_ENUM_IS_NUMBERED (TREE_TYPE (ip->start_exp)))
	{
	  error ("DO FOR start expression is a numbered SET");
	  return;
	}
      if (TREE_CODE (TREE_TYPE (ip->end_exp)) == ENUMERAL_TYPE
	  && CH_ENUM_IS_NUMBERED (TREE_TYPE (ip->end_exp)))
	{
	  error ("TO expression is a numbered SET");
	  return;
	}
      /* Convert all three expressions to a common precision,
	 which is the largest precision they exhibit, but
         INTEGER_CST nodes are built in the lexer as
	 long_integer_type nodes.  We'll treat convert them to
	 integer_type_nodes if possible, for faster loop times. */

      if (TYPE_PRECISION (max_prec_type) <
	    TYPE_PRECISION (TREE_TYPE (ip->start_exp))
	  && !CST_FITS_INT (ip->start_exp))
	max_prec_type = TREE_TYPE (ip->start_exp);
      if (! discrete_type_p (TREE_TYPE (ip->end_exp)))
	{
	  error ("TO expr must have discrete mode");
	  return;
	}
      if (! CH_COMPATIBLE (ip->start_exp, 
			   TREE_TYPE (ip->end_exp)))
	{
	  error ("start expr and TO expr must be compatible");
	  return;
	}
      if (TYPE_PRECISION (max_prec_type) <
	    TYPE_PRECISION (TREE_TYPE (ip->end_exp))
	  && !CST_FITS_INT (ip->end_exp))
	max_prec_type = TREE_TYPE (ip->end_exp);
      if (ip->step_exp != NULL_TREE)
	{
	  /* assure that default 'BY 1' gets a useful type */
	  if (ip->step_exp == integer_one_node)
	    ip->step_exp = convert (TREE_TYPE (ip->start_exp),
				    ip->step_exp);
	  if (! discrete_type_p (TREE_TYPE (ip->step_exp)))
	    {
	      error ("BY expr must have discrete mode");
	      return;
	    }
	  if (! CH_COMPATIBLE (ip->start_exp,
		  TREE_TYPE (ip->step_exp)))
	    {
	      error ("start expr and BY expr must be compatible");
	      return;
	    }
	  if (TYPE_PRECISION (max_prec_type) <
		TYPE_PRECISION (TREE_TYPE (ip->step_exp))
	      && !CST_FITS_INT (ip->step_exp))
	    max_prec_type = TREE_TYPE (ip->step_exp);
	}
      if (TREE_CODE (ip->start_exp) == INTEGER_CST
	  && TREE_CODE (ip->end_exp) == INTEGER_CST
	  && compare_int_csts (ip->down_flag ? LT_EXPR : GT_EXPR,
			       ip->start_exp, ip->end_exp))
	warning ("body of DO FOR will never execute");

      ip->start_exp = 
	convert (max_prec_type, ip->start_exp);
      ip->end_exp   = 
	convert (max_prec_type, ip->end_exp);

      if (ip->step_exp != NULL_TREE)
	{
	  ip->step_exp =
	    convert (max_prec_type, ip->step_exp);

	  if (TREE_CODE (ip->step_exp) != INTEGER_CST)
	    {
	      /* generate runtime check for negative BY expr */
	      ip->step_exp = 
		check_range (ip->step_exp, ip->step_exp,
			     integer_zero_node, NULL_TREE);
	    }
	  else if (compare_int_csts (LE_EXPR, ip->step_exp, integer_zero_node))
	    {
	      error ("BY expression is negative or zero");
	      return;
	    }
	}
      ip->itype = DO_STEP;
    }

  ip->error_flag = 0;           /* no errors! */
}

void
build_loop_start (while_control, start_label)
     tree while_control, start_label;
{
  ITERATOR *firstp = loop_stack->iter_list;
  
  firstp->condition = while_control;

  if (firstp->error_flag)
    return;

  /* We didn't know at begin_loop_scope time about the condition;
     adjust iterator type now. */
  if (firstp->itype == DO_OD && firstp->condition)
    firstp->itype = DO_WHILE;

  if (initialize_iter_var ())
    return;
  
  if (maybe_skip_loop ())
    return;

  /* use the label as an 'exit' label, 
     'goto' needs another sort of label */
  expand_start_loop (start_label != NULL_TREE);
  
  if (top_loop_end_check ())
    return;
  emit_line_note (input_filename, lineno); 
}

/*
 * Called after the last action of the loop body
 * has been parsed.
 */
void
build_loop_end ()
{
  ITERATOR *ip = loop_stack->iter_list;

  emit_line_note (input_filename, lineno);

  if (ip->error_flag)
    return;

  if (bottom_loop_end_check ())
    return;

  if (increment_temps ())
    return;

  if (ip->itype != DO_OD)
    {
      expand_end_loop ();

      for (; ip != NULL; ip = ip->next)
	{
	  switch (ip->itype)
	    {
	    case DO_LOC_VARYING:
	    case DO_STEP:
	      expand_end_cond ();
	      break;
	    default:
	      break;
	    }
	}
    }
}

/*
 * The rest of the routines in this file are called from
 * the above three routines.
 */
static int
classify_loop ()
{
  ITERATOR *firstp = loop_stack->iter_list, *ip;

  firstp->error_flag = 0;
  if (firstp->itype == DO_UNUSED || firstp->itype == DO_OD)
    {
      /* if we have just DO .. OD, do nothing - this is just a 
         BEGIN .. END without creating a new scope, and no looping  */
      if (firstp->condition != NULL_TREE)
	firstp->itype = DO_WHILE;
      else
	firstp->itype = DO_OD;
    }
  
  /* Issue a warning if the any loop counter is mentioned more 
     than once in the iterator list. */
  for (ip = firstp; ip != NULL; ip = ip->next)
    {
      switch (ip->itype)
	{
	case DO_FOREVER:
	case DO_WHILE:
	  break;
	case DO_STEP:
	case DO_RANGE:
	case DO_POWERSET:
	case DO_LOC:
	case DO_LOC_VARYING:
	  /* FIXME: check for name uniqueness */
	  break;
	default:
	  ;
	}
    }
  return firstp->error_flag;
}

/*
 * Reserve space for any loop-control temporaries, initialize them
 */
static int
declare_temps ()
{
  ITERATOR *firstp = loop_stack->iter_list, *ip;
  tree start_ptr;

  for (ip = firstp; ip != NULL; ip = ip->next)
    {
      switch (ip->itype)
	{
	case DO_FOREVER:
	case DO_WHILE:
	  break;
	case DO_STEP:
	  ip->iter_type = chill_unsigned_type (TREE_TYPE (ip->start_exp));

	  /* create, initialize temporaries if expressions aren't constant */
	  ip->start_temp = maybe_make_for_temp (ip->start_exp, "for_start",
						ip->iter_type);
	  ip->end_temp = maybe_make_for_temp (ip->end_exp, "for_end",
					      ip->iter_type);
	  /* this is just the step-expression */
	  ip->step_temp    = maybe_make_for_temp (ip->step_exp, "for_step",
						  ip->iter_type);
	  goto do_step_range;
	  
	case DO_RANGE:
	  ip->iter_type = chill_unsigned_type_node;
	  
	  ip->start_temp =
	    (ip->down_flag ? build_chill_upper : build_chill_lower)(TREE_TYPE (ip->start_exp));
	  ip->end_temp =
	    (ip->down_flag ? build_chill_lower : build_chill_upper)(TREE_TYPE (ip->start_exp));
	  
	  ip->step_temp = integer_one_node;
	  
	do_step_range:
	  if (flag_local_loop_counter)
	    {
	      /* (re-)declare the user's iteration variable in the 
		 loop's scope. */
	      tree id_node = ip->user_var;
	      IDENTIFIER_LOCAL_VALUE (id_node) = ip->user_var = 
		decl_temp1 (id_node, ip->iter_type, 0, NULL_TREE,
			    0, 0);
	    }
	  else
	    {
	      /* in this case, it's a previously-declared 
		 VAR_DECL node, checked in build_loop_iterator. */
	      if (TREE_CODE (ip->user_var) == IDENTIFIER_NODE)
		ip->user_var = lookup_name (ip->user_var);
	      if (ip->user_var == NULL_TREE)
		{
		  error ("loop identifier undeclared");
		  ip->error_flag = 1;
		  return 1;
		}
	    }
	  ip->iter_var = 
	    decl_temp1 (get_unique_identifier ("iter_var"),
			ip->iter_type, 0, NULL_TREE, 0, 0);
	  break;

	case DO_POWERSET:
	  ip->iter_type = chill_unsigned_type (
			    TYPE_DOMAIN (TREE_TYPE (ip->start_exp)));
	  if (flag_local_loop_counter)
	    {
	      /* declare the user's iteration variable in the loop's scope. */
	      /* in this case, it's just an IDENTIFIER_NODE */
	      ip->user_var = 
		decl_temp1 (ip->user_var, ip->iter_type, 0, NULL_TREE, 0, 0);
	    }
	  else
	    {
	      /* in this case, it's a previously-declared VAR_DECL node */
	      ip->user_var = lookup_name (ip->user_var);
	    }
	  /* the user's powerset-expression, evaluated and saved in a temp */
	  ip->powerset_temp = maybe_make_for_temp (ip->start_exp, "for_set",
						 TREE_TYPE (ip->start_exp));
	  mark_addressable (ip->powerset_temp);
	  break;

	case DO_LOC:
	case DO_LOC_VARYING:
	  ip->iter_type = chill_unsigned_type_node;
	  /* create the counter temp */
	  ip->iter_var = 
	    build_temporary_variable ("iter_var", ip->iter_type);

	  if (!CH_LOCATION_P (ip->start_exp))
	    ip->start_exp
	      = decl_temp1 (get_unique_identifier ("iter_loc"),
			    TREE_TYPE (ip->start_exp), 0,
			    ip->start_exp, 0, 0);

	  if (ip->itype == DO_LOC)
	    {
	      tree array_type = TREE_TYPE (ip->start_exp);
	      tree ptr_type;
	      tree temp;
	      
	      if (TREE_CODE (TREE_TYPE (array_type)) == BOOLEAN_TYPE)
		{
		  error ("Can't iterate through array of BOOL");
		  ip->error_flag = 1;
		  return ip->error_flag;
		}
	      
	      /* FIXME: check for array type in ip->start_exp */

	      /* create pointer temporary */
	      ip->base_type = TREE_TYPE (array_type);
	      ptr_type = build_pointer_type (ip->base_type);
	      ip->loc_ptr_temp =
		build_temporary_variable ("loc_ptr_tmp", ptr_type);
	      
	      /* declare the user's iteration variable in 
		 the loop's scope, as an expression, to be
		 passed to build_component_ref later */
	      save_expr_under_name (ip->user_var, 
		build1 (INDIRECT_REF, ip->base_type, 
			ip->loc_ptr_temp));
	      
	      /* FIXME: see stor_layout */
	      ip->step_temp = size_in_bytes (ip->base_type);
	      
	      temp = TYPE_DOMAIN (array_type);

	      /* pointer to first array entry to look at */
	      start_ptr = build1 (ADDR_EXPR, ptr_type, ip->start_exp);
	      mark_addressable (ip->start_exp);
	      ip->start_temp = ip->down_flag ? 
		fold (build (PLUS_EXPR, ptr_type, 
			     start_ptr,
		  fold (build (MULT_EXPR, integer_type_node, ip->step_temp,
		    fold (build (MINUS_EXPR, integer_type_node,
				 TYPE_MAX_VALUE (temp),
				 TYPE_MIN_VALUE (temp)))))))
		  : start_ptr;
	    }
	  else
	    {
	      tree array_length =
		convert (integer_type_node,
		  build_component_ref (ip->start_exp, var_length_id));
	      tree array_type = TREE_TYPE (TREE_CHAIN (
			TYPE_FIELDS (TREE_TYPE (ip->start_exp))));
	      tree array_data_ptr = 
		build_component_ref (ip->start_exp, var_data_id);
	      tree ptr_type;
	      
	      if (TREE_CODE (TREE_TYPE (array_type)) == BOOLEAN_TYPE)
		{
		  error ("Can't iterate through array of BOOL");
		  firstp->error_flag = 1;
		  return firstp->error_flag;
		}
	      
	      /* create pointer temporary */
	      ip->base_type = TREE_TYPE (array_type);
	      ptr_type = build_pointer_type (ip->base_type);
	      ip->loc_ptr_temp = 
		build_temporary_variable ("loc_ptr_temp", ptr_type);
							   
	      
	      /* declare the user's iteration variable in 
		 the loop's scope, as an expression, to be
		 passed to build_component_ref later */
	      save_expr_under_name (ip->user_var, 
		build1 (INDIRECT_REF, ip->base_type, 
			ip->loc_ptr_temp));
	      
	      /* FIXME: see stor_layout */
	      ip->step_temp = size_in_bytes (ip->base_type);
	      
	      /* pointer to first array entry to look at */
	      start_ptr = build1 (ADDR_EXPR, ptr_type, array_data_ptr);
	      mark_addressable (array_data_ptr);
	      ip->start_temp = ip->down_flag ? 
		fold (build (PLUS_EXPR, ptr_type, 
                  start_ptr,
		    fold (build (MULT_EXPR, integer_type_node, ip->step_temp,
		      fold (build (MINUS_EXPR, integer_type_node,
				   array_length,
				   integer_one_node))))))
		  : start_ptr;
	    }
	default:
	  ;
	}
    }
  return firstp->error_flag;
}

/*
 * Initialize the hidden iteration-control variables,
 * and the user's explicit loop variable.
 */
static int
initialize_iter_var ()
{
  ITERATOR *firstp = loop_stack->iter_list, *ip;

  for (ip = firstp; ip != NULL; ip = ip->next)
    {
      switch (ip->itype)
	{
	case DO_FOREVER:
	case DO_WHILE:
	  break;
	case DO_STEP:
	case DO_RANGE:
	  {
	    tree count =
	      fold (build (PLUS_EXPR, ip->iter_type, integer_one_node,
		  fold (build (TRUNC_DIV_EXPR, ip->iter_type, 
		    convert (ip->iter_type,
	              fold (build (MINUS_EXPR, ip->iter_type,
			ip->down_flag ? ip->start_temp : ip->end_temp,
			ip->down_flag ? ip->end_temp   : ip->start_temp))),
			       ip->step_temp))));
	    /* initialize the loop's hidden counter variable */
	    expand_expr_stmt (
	      build_chill_modify_expr (ip->iter_var, count));

	    /* initialize user's variable */
	    expand_expr_stmt (
	      build_chill_modify_expr (ip->user_var, ip->start_temp));
	  }
	  break;
	case DO_POWERSET:
	  break;
	case DO_LOC:
	  {
	    tree array_type = TREE_TYPE (ip->start_exp);
	    tree array_length =
	      fold (build (TRUNC_DIV_EXPR, integer_type_node,
			   size_in_bytes (array_type),
			   size_in_bytes (TREE_TYPE (array_type))));

	    expand_expr_stmt (
	      build_chill_modify_expr (ip->iter_var, array_length));
	    goto do_loc_common;
	  }

	case DO_LOC_VARYING:
	  expand_expr_stmt (
	    build_chill_modify_expr (ip->iter_var,
	      convert (integer_type_node,
		build_component_ref (ip->start_exp, var_length_id))));

	do_loc_common:
	  expand_expr_stmt (
	    build_chill_modify_expr (ip->loc_ptr_temp, 
				     ip->start_temp));
	  break;

	default:
	  ;
	}
    }
  return firstp->error_flag;
}

/* Generate code to skip the whole loop, if start expression not
 * <= end expression (or >= for DOWN loops).  This comparison must
 * *NOT* be done in unsigned mode, or it will fail.
 *  Also, skip processing an empty VARYING array. 
 */
static int
maybe_skip_loop ()
{
  ITERATOR *firstp = loop_stack->iter_list, *ip;

  for (ip = firstp; ip != NULL; ip = ip->next)
    {
      switch (ip->itype)
	{
	case DO_STEP:
	  expand_start_cond (
	    build (ip->down_flag ? GE_EXPR : LE_EXPR, 
		   TREE_TYPE (ip->start_exp),
		   ip->start_exp, ip->end_exp), 0);
	  break;
    
	case DO_LOC_VARYING:
	  { tree array_length =
	      convert (integer_type_node,
	        build_component_ref (ip->start_exp, var_length_id));
	    expand_start_cond (
	      build (NE_EXPR, TREE_TYPE (array_length),
		     array_length, integer_zero_node), 0);
	    break;
	  }
	default:
	  break;
	}
    }
  return 0;
}  

/*
 * Check at the top of the loop for a termination
 */
static int
top_loop_end_check ()
{
  ITERATOR *firstp = loop_stack->iter_list, *ip;

  /* now, exit the loop if the condition isn't TRUE. */
  if (firstp->condition)
    {
      expand_exit_loop_if_false (0,
	chill_truthvalue_conversion (firstp->condition));
    }

  for (ip = firstp; ip != NULL; ip = ip->next)
    {
      switch (ip->itype)
	{
	case DO_FOREVER:
	case DO_WHILE:
	case DO_STEP:
	case DO_RANGE:
	  break;
	case DO_POWERSET:
	  {
	    tree temp1;
	    char *func_name;

	    if (ip->down_flag)
	      func_name = "__flsetclrpowerset";
	    else
	      func_name = "__ffsetclrpowerset";
	    
	    temp1 = TYPE_MIN_VALUE
	      (TYPE_DOMAIN (TREE_TYPE (ip->powerset_temp)));
	    expand_exit_loop_if_false (0,
	      build_chill_function_call (lookup_name (get_identifier (func_name)),
	        tree_cons (NULL_TREE, force_addr_of (ip->powerset_temp),
                  tree_cons (NULL_TREE, powersetlen (ip->powerset_temp),
	            tree_cons (NULL_TREE, force_addr_of (ip->user_var),
                      tree_cons (NULL_TREE, size_in_bytes (TREE_TYPE (ip->user_var)),
		        tree_cons (NULL_TREE,
				   convert (long_integer_type_node, temp1),
				   NULL_TREE)))))));
	  }
	  break;
	case DO_LOC:
	case DO_LOC_VARYING:
	  break;
	default:
	  ;
	}
    }
  return firstp->error_flag;
}

/*
 * Check generated temporaries for loop's end
 */
static int
bottom_loop_end_check ()
{
  ITERATOR *firstp = loop_stack->iter_list, *ip;

  emit_line_note (input_filename, lineno);

  /* now, generate code to check each loop counter for termination */
  for (ip = firstp; ip != NULL; ip = ip->next)
    {
      switch (ip->itype)
	{
	case DO_FOREVER:
	case DO_WHILE:
	  break;
	case DO_STEP:
	case DO_RANGE:
	case DO_LOC:
	case DO_LOC_VARYING:
	  /* decrement iteration counter by one */
	  chill_expand_assignment (ip->iter_var, MINUS_EXPR, integer_one_node);
	  /* exit if it's zero */
	  expand_exit_loop_if_false (0,
	    build (NE_EXPR, boolean_type_node, 
		   ip->iter_var,
		   integer_zero_node));
	  break;
	case DO_POWERSET:
	  break;
	default:
	  ;
	}
    }

  return firstp->error_flag;
}

/*
 * increment the loop-control variables.
 */
static int
increment_temps ()
{
  ITERATOR *firstp = loop_stack->iter_list, *ip;

  for (ip  = firstp; ip != NULL; ip = ip->next)
    {
      switch (ip->itype)
	{
	case DO_FOREVER:
	case DO_WHILE:
	  break;
	case DO_STEP:
	case DO_RANGE:
	  {
	    tree delta =
	      fold (build (ip->down_flag ? MINUS_EXPR : PLUS_EXPR,
			   TREE_TYPE (ip->user_var), ip->user_var,
			   ip->step_temp));
 	    expand_expr_stmt (
	      build_chill_modify_expr (ip->user_var, delta));
	  }
	  break;
	case DO_LOC:
	case DO_LOC_VARYING:
	  /* This statement uses the C semantics, so that 
	     the pointer is actually incremented by the 
	     length of the object pointed to. */
#if 1
	  expand_expr_stmt (
	    build_modify_expr (ip->loc_ptr_temp, 
			       ip->down_flag ? MINUS_EXPR : PLUS_EXPR,
			       integer_one_node));
#else
	  {
	    enum tree_code op = ip->down_flag ? MINUS_EXPR : PLUS_EXPR;
	    tree el_type = TREE_TYPE (TREE_TYPE (ip->loc_ptr_temp));
	    chill_expand_assignment (ip->loc_ptr_temp, NOP_EXPR,
				     build (op,
					    TREE_TYPE (ip->loc_ptr_temp),
					    ip->loc_ptr_temp,
					    size_in_bytes (el_type)));
	  }
#endif
	  break;
	case DO_POWERSET:
	  break;
	default:
	  ;
	}
    }
  return firstp->error_flag;
}

/*
 * Generate a (temporary) unique identifier_node of
 * the form "__tmp_%s_%d"
 */
tree
get_unique_identifier (lead)
     char *lead;
{
  char idbuf [256];
  static int idcount = 0;

  sprintf (idbuf, "__tmp_%s_%d", lead ? lead : "", idcount++);
  return get_identifier (idbuf);
}

/*
 * build a temporary variable, given its NAME and TYPE.
 * The name will have a number appended to assure uniqueness.
 * return its DECL node.
 */
static tree
build_temporary_variable (name, type)
     char *name;
     tree type;
{
  return decl_temp1 (get_unique_identifier (name), type, 0, NULL_TREE, 0, 0);
}


/*
 * If the given expression isn't a constant, build a temp for it
 * and evaluate the expression into the temp.  Return the tree
 * representing either the original constant expression or the
 * temp which now contains the expression's value. 
 */
static tree
maybe_make_for_temp (exp, temp_name, exp_type)
     tree exp;
     char *temp_name;
     tree exp_type;
{
  tree result = exp;

  if (exp != NULL_TREE)
    {
      /* if exp isn't constant, create a temporary for its value */
      if (TREE_CONSTANT (exp))
	{
          /* FIXME: assure that TREE_TYPE (result) == ip->exp_type */
	  result = convert (exp_type, exp);
	}
      else {
	/* build temp, assign the value */
	result = decl_temp1 (get_unique_identifier (temp_name), exp_type, 0,
			     exp, 0, 0);
      }
    }
  return result;
}


/*
 * Adapt the C unsigned_type function to CHILL - we need to
 * account for any CHILL-specific integer types here.  So far,
 * the 16-bit integer type is the only one.
 */
static tree
chill_unsigned_type (type)
     tree type;
{
  extern tree chill_unsigned_type_node;
  tree type1 = TYPE_MAIN_VARIANT (type);

  if (type1 == chill_integer_type_node)
    return chill_unsigned_type_node;
  else
    return unsigned_type (type);
}
