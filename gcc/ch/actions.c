/* Implement actions for CHILL.
   Copyright (C) 1992, 93, 94, 98, 99, 2000 Free Software Foundation, Inc.
   Authors: Per Bothner, Bill Cox, Michael Tiemann, Michael North

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

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "expr.h"
#include "ch-tree.h"
#include "lex.h"
#include "flags.h"
#include "actions.h"
#include "obstack.h"
#include "assert.h"
#include "toplev.h"

static int id_cmp PROTO ((tree *, tree *));
static void warn_unhandled PROTO ((const char *));
static tree adjust_return_value PROTO ((tree, const char *));
static tree update_else_range_for_int_const PROTO ((tree, tree));
static tree update_else_range_for_range PROTO ((tree, tree, tree));
static tree update_else_range_for_range_expr PROTO ((tree, tree));
static tree update_else_range_for_type PROTO ((tree, tree));
static tree compute_else_range PROTO ((tree, tree, int));
static tree check_case_value PROTO ((tree, tree));
static void chill_handle_case_label_range PROTO ((tree, tree, tree));
static tree chill_handle_multi_case_label_range PROTO ((tree, tree, tree));
static tree chill_handle_multi_case_else_label PROTO ((tree));
static tree chill_handle_multi_case_label PROTO ((tree, tree));
static tree chill_handle_multi_case_label_list PROTO ((tree, tree));
static void print_missing_cases PROTO ((tree, const unsigned char *, long));

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* reserved tag definitions */

#define TYPE_ID                 "id"
#define TAG_OBJECT              "chill_object"
#define TAG_CLASS               "chill_class"

extern int flag_short_enums;
extern int current_nesting_level;

extern struct obstack *expression_obstack, permanent_obstack;
extern struct obstack *current_obstack, *saveable_obstack;

/* This flag is checked throughout the non-CHILL-specific
   in the front end. */
tree chill_integer_type_node;
tree chill_unsigned_type_node;

/* Never used.  Referenced from c-typeck.c, which we use. */
int current_function_returns_value = 0;
int current_function_returns_null = 0;

/* data imported from toplev.c  */

extern char *dump_base_name;

/* set from command line parameter, to exit after 
   grant file written, generating no code. */
int grant_only_flag = 0;

const char *
lang_identify ()
{
  return "chill";
}


void
init_chill ()
{
}

void
print_lang_statistics ()
{
}


void
lang_finish ()
{
#if 0
    extern int errorcount, sorrycount;

    /* this should be the last action in compiling a module.
       If there are other actions to be performed at lang_finish
       please insert before this */

    /* FIXME: in case of a syntax error, this leaves the grant file incomplete */
    /* for the moment we print a warning in case of errors and 
       continue granting */
    if ((errorcount || sorrycount) && grant_count)
      {
	warning ("%d errors, %d sorries, do granting", errorcount, sorrycount);
	errorcount = sorrycount = 0;
      }
#endif
}

void
chill_check_decl (decl)
     tree decl;
{
  tree type = TREE_TYPE (decl);
  static int alreadyWarned = 0;

  if (TREE_CODE (type) == RECORD_TYPE) /* && TREE_STATIC_TEMPLATE (type)) */
    {
      if (!alreadyWarned)
        {
          error ("GNU compiler does not support statically allocated objects");          
          alreadyWarned = 1;
        }
      error_with_decl (decl, "`%s' cannot be statically allocated");
    }
}

/* Comparison function for sorting identifiers in RAISES lists.
   Note that because IDENTIFIER_NODEs are unique, we can sort
   them by address, saving an indirection.  */
static int
id_cmp (p1, p2)
     tree *p1, *p2;
{
  long diff = (long)TREE_VALUE (*p1) - (long)TREE_VALUE (*p2);

  return (diff < 0) ? -1 : (diff > 0);
}

/* Build the FUNCTION_TYPE or METHOD_TYPE which may raise exceptions
   listed in RAISES.  */
tree
build_exception_variant (type, raises)
     tree type, raises;
{
  int i;
  tree v = TYPE_MAIN_VARIANT (type);
  tree t, t2;
  int constp    = TYPE_READONLY (type);
  int volatilep = TYPE_VOLATILE (type);

  if (!raises)
    return build_type_variant (v, constp, volatilep);

  if (TREE_CHAIN (raises))
    { /* Sort the list */
      tree *a = (tree *)alloca ((list_length (raises)+1) * sizeof (tree));
      for (i = 0, t = raises; t; t = TREE_CHAIN (t), i++)
	a[i] = t;
      /* NULL terminator for list.  */
      a[i] = NULL_TREE;
      qsort (a, i, sizeof (tree),
	     (int (*) PROTO((const void*, const void*))) id_cmp);
      while (i--)
	TREE_CHAIN (a[i]) = a[i+1];
      raises = a[0];
    }

  for (v = TYPE_NEXT_VARIANT (v); v; v = TYPE_NEXT_VARIANT (v))
    {
      if (TYPE_READONLY (v) != constp
	  || TYPE_VOLATILE (v) != volatilep)
	continue;

      t = raises;
      t2 = TYPE_RAISES_EXCEPTIONS (v);
      while (t && t2)
	{
	  if (TREE_TYPE (t) == TREE_TYPE (t2))
	    {
	      t = TREE_CHAIN (t);
	      t2 = TREE_CHAIN (t2);
	    }
	  else break;
	}
      if (t || t2)
	continue;
      /* List of exceptions raised matches previously found list.

         @@ Nice to free up storage used in consing up the
	 @@ list of exceptions raised.  */
      return v;
    }

  /* Need to build a new variant.  */
  if (TREE_PERMANENT (type))
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
      v = copy_node (type);
      pop_obstacks ();
    }
  else
    v = copy_node (type);

  TYPE_NEXT_VARIANT (v) = TYPE_NEXT_VARIANT (type);
  TYPE_NEXT_VARIANT (type) = v;
  if (raises && ! TREE_PERMANENT (raises))
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
      raises = copy_list (raises);
      pop_obstacks ();
    }
  TYPE_RAISES_EXCEPTIONS (v) = raises;
  return v;
}
#if 0

tree
build_rts_call (name, type, args)
     const char *name;
     tree type, args;
{
  tree decl = lookup_name (get_identifier (name));
  tree converted_args = NULL_TREE;
  tree result, length = NULL_TREE;

  assert (decl != NULL_TREE);
  while (args)
    {
      tree arg = TREE_VALUE (args);
      if (TREE_CODE (TREE_TYPE (arg)) == SET_TYPE
	  || TREE_CODE (TREE_TYPE (arg)) == ARRAY_TYPE)
	{
	  length = size_in_bytes (TREE_TYPE (arg));
	  arg = build_chill_addr_expr (arg, (char *)0);
	}
      converted_args = tree_cons (NULL_TREE, arg, converted_args);
      args = TREE_CHAIN (args);
    }
  if (length != NULL_TREE)
    converted_args = tree_cons (NULL_TREE, length, converted_args);
  converted_args = nreverse (converted_args);
  result = build_chill_function_call (decl, converted_args);
  if (TREE_CODE (type) == SET_TYPE || TREE_CODE (type) == ARRAY_TYPE)
    result = build1 (INDIRECT_REF, type, result);
  else
    result = convert (type, result);
  return result;
}
#endif

/*
 * queue name of unhandled exception
 * to avoid multiple unhandled warnings
 * in one compilation module
 */

struct already_type
{
  struct already_type *next;
  char *name;
};

static struct already_type *already_warned = 0;

static void
warn_unhandled (ex)
     const char *ex;
{
  struct already_type *p = already_warned;

  while (p)
    {
      if (!strcmp (p->name, ex))
	return;
      p = p->next;
    }
  
  /* not yet warned */
  p = (struct already_type *)xmalloc (sizeof (struct already_type));
  p->next = already_warned;
  p->name = xstrdup (ex);
  already_warned = p;
  pedwarn ("causing unhandled exception `%s' (this is flaged only once)", ex);
}

/*
 * build a call to the following function:
 *   void   __cause_ex1 (char* ex, const char *file, 
 *                       const unsigned lineno);
 * if the exception is handled or
 *   void __unhandled_ex (char *ex, char *file, unsigned lineno)
 * if the exception is not handled.
 */
tree
build_cause_exception (exp_name, warn_if_unhandled)
     tree exp_name;
     int warn_if_unhandled;
{
  /* We don't use build_rts_call() here, because the string (array of char)
     would be followed by its length in the parameter list built by
     build_rts_call, and the runtime routine doesn't want a length parameter.*/
  tree exp_decl = build_chill_exception_decl (IDENTIFIER_POINTER (exp_name));
  tree function, fname, lineno, result;
  int handled = is_handled (exp_name);

  switch (handled)
    {
    case 0:
      /* no handler */
      if (warn_if_unhandled)
	warn_unhandled (IDENTIFIER_POINTER (exp_name));
      function = lookup_name (get_identifier ("__unhandled_ex"));
      fname = force_addr_of (get_chill_filename ());
      lineno = get_chill_linenumber ();
      break;
    case 1:
      /* local handler */
      function = lookup_name (get_identifier ("__cause_ex1"));
      fname = force_addr_of (get_chill_filename ());
      lineno = get_chill_linenumber ();
      break;
    case 2:
      /* function may propagate this exception */
      function = lookup_name (get_identifier ("__cause_ex1"));
      fname = lookup_name (get_identifier (CALLER_FILE));
      if (fname == NULL_TREE)
	fname = error_mark_node;
      lineno = lookup_name (get_identifier (CALLER_LINE));
      if (lineno == NULL_TREE)
	lineno = error_mark_node;
      break;
    default:
      abort();
    }
  result =
    build_chill_function_call (function,
      tree_cons (NULL_TREE, build_chill_addr_expr (exp_decl, (char *)0),
	tree_cons (NULL_TREE,  fname,
	  tree_cons (NULL_TREE, lineno, NULL_TREE))));
  return result;
}

void
expand_cause_exception (exp_name)
     tree exp_name;
{
  expand_expr_stmt (build_cause_exception (exp_name, 1));
}

/* If CONDITION is true, raise EXCEPTION (an IDENTIFIER_NODE);
   otherwise return EXPR. */

tree
check_expression (expr, condition, exception)
     tree expr, condition, exception;
{
  if (integer_zerop (condition))
    return expr;
  else
    return build (COMPOUND_EXPR, TREE_TYPE (expr),
		  fold (build (TRUTH_ANDIF_EXPR, boolean_type_node,
			       condition, build_cause_exception (exception, 0))),
		  expr);
}

/* Return an expression for VALUE < LO_LIMIT || VALUE > HI_LIMIT,
   somewhat optimized and with some warnings suppressed.
   If LO_LIMIT or HI_LIMIT is NULL_TREE, assume that (sub-)test passes.  */

tree
test_range (value, lo_limit, hi_limit)
     tree value, lo_limit, hi_limit;
{
  if (lo_limit || hi_limit)
    {
      int old_inhibit_warnings = inhibit_warnings;
      tree lo_check, hi_check, check;

      /* This is a hack so that `shorten_compare' doesn't warn the
	 user about useless range checks that are too much work to
	 optimize away here.  */
      inhibit_warnings = 1;

      lo_check = lo_limit ? 
	fold (build_compare_discrete_expr (LT_EXPR, value, lo_limit)) :
	  boolean_false_node;   /* fake passing the check */

      hi_check = hi_limit ? 
	fold (build_compare_discrete_expr (GT_EXPR, value, hi_limit)) :
	  boolean_false_node;   /* fake passing the check */

      if (lo_check == boolean_false_node)
	check = hi_check;
      else if (hi_check == boolean_false_node)
	check = lo_check;
      else
	check = fold (build (TRUTH_ORIF_EXPR, boolean_type_node,
			     lo_check, hi_check));

      inhibit_warnings = old_inhibit_warnings;
      return check;
    }
  else
    return boolean_false_node;
}

/* Return EXPR, except if range_checking is on, return an expression
   that also checks that value >= low_limit && value <= hi_limit.
   If LO_LIMIT or HI_LIMIT is NULL_TREE, assume that test passes.  */

tree
check_range (expr, value, lo_limit, hi_limit)
     tree expr, value, lo_limit, hi_limit;
{
  tree check = test_range (value, lo_limit, hi_limit);
  if (!integer_zerop (check))
    {
      if (current_function_decl == NULL_TREE)
	{
	  if (TREE_CODE (check) == INTEGER_CST)
	    error ("range failure (not inside function)");
	  else
	    warning ("possible range failure (not inside function)");
	}
      else
	{
	  if (TREE_CODE (check) == INTEGER_CST)
	    warning ("expression will always cause RANGEFAIL");
	  if (range_checking)
	    expr = check_expression (expr, check,
				     ridpointers[(int) RID_RANGEFAIL]);
	}
    }
  return expr;
}

/* Same as EXPR, except raise EMPTY if EXPR is NULL. */

tree
check_non_null (expr)
     tree expr;
{
  if (empty_checking)
    {
      expr = save_if_needed (expr);
      return check_expression (expr,
			       build_compare_expr (EQ_EXPR,
						   expr, null_pointer_node),
			       ridpointers[(int) RID_EMPTY]);
    }
  return expr;
}

/*
 * There are four conditions to generate a runtime check:
 *    1) assigning a longer INT to a shorter (signs irrelevant)
 *    2) assigning a signed to an unsigned
 *    3) assigning an unsigned to a signed of the same size.
 *    4) TYPE is a discrete subrange
 */
tree
chill_convert_for_assignment (type, expr, place)
     tree type, expr;
     const char *place; /* location description for error messages */
{
  tree ttype = type;
  tree etype = TREE_TYPE (expr);
  tree result;

  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;
  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return expr;
  if (TREE_CODE (expr) == TYPE_DECL)
    {
      error ("right hand side of assignment is a mode");
      return error_mark_node;
    }

  if (! CH_COMPATIBLE (expr, type))
    {
      error ("incompatible modes in %s", place);
      return error_mark_node;
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    ttype = TREE_TYPE (ttype);
  if (etype && TREE_CODE (etype) == REFERENCE_TYPE)
    etype = TREE_TYPE (etype);

  if (etype
      && (CH_STRING_TYPE_P (ttype)
	  || (chill_varying_type_p (ttype)
	      && CH_STRING_TYPE_P (CH_VARYING_ARRAY_TYPE (ttype))))
      && (CH_STRING_TYPE_P (etype)
	  || (chill_varying_type_p (etype)
	      && CH_STRING_TYPE_P (CH_VARYING_ARRAY_TYPE (etype)))))
    {
      tree cond;
      if (range_checking)
	expr = save_if_needed (expr);
      cond = string_assignment_condition (ttype, expr);
      if (TREE_CODE (cond) == INTEGER_CST)
	{
	  if (integer_zerop (cond))
	    {
	      error ("bad string length in %s", place);
	      return error_mark_node;
	    }
	  /* Otherwise, the condition is always true, so no runtime test. */
	}
      else if (range_checking)
	expr = check_expression (expr,
				 invert_truthvalue (cond),
				 ridpointers[(int) RID_RANGEFAIL]);
    }

  if (range_checking 
      && discrete_type_p (ttype) 
      && etype != NULL_TREE
      && discrete_type_p (etype))
    {
      int cond1 = tree_int_cst_lt (TYPE_SIZE (ttype),
				   TYPE_SIZE (etype));
      int cond2 = TREE_UNSIGNED (ttype) 
	          && (! TREE_UNSIGNED (etype));
      int cond3 = (! TREE_UNSIGNED (type))
	          && TREE_UNSIGNED (etype) 
		  && tree_int_cst_equal (TYPE_SIZE (ttype),
					 TYPE_SIZE (etype));
      int cond4 = TREE_TYPE (ttype) 
	          && discrete_type_p (TREE_TYPE (ttype));

      if (cond1 || cond2 || cond3 || cond4)
	{
	  tree type_min = TYPE_MIN_VALUE (ttype);
	  tree type_max = TYPE_MAX_VALUE (ttype);

	  expr = save_if_needed (expr);
	  if (expr && type_min && type_max)
	    expr = check_range (expr, expr, type_min, type_max);
	}
    }
  result = convert (type, expr);

  /* If the type is a array of PACK bits and the expression is an array constructor,
     then build a CONSTRUCTOR for a bitstring.  Bitstrings are zero based, so
     decrement the value of each CONSTRUCTOR element by the amount of the lower
     bound of the array.  */
  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_PACKED (type)
      && TREE_CODE (result) == CONSTRUCTOR)
    {
      tree domain_min = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
      tree new_list = NULL_TREE;
      long index;
      tree element;
      for (element = TREE_OPERAND (result, 1);
	   element != NULL_TREE;
	   element = TREE_CHAIN (element))
	{
	  if (!tree_int_cst_equal (TREE_VALUE (element), integer_zero_node))
	    {
	      tree purpose = TREE_PURPOSE (element);
	      switch (TREE_CODE (purpose))
		{
		case INTEGER_CST:
		  new_list = tree_cons (NULL_TREE,
					size_binop (MINUS_EXPR, purpose, domain_min),
					new_list);
		  break;
		case RANGE_EXPR:
		  for (index  = TREE_INT_CST_LOW (TREE_OPERAND (purpose, 0));
		       index <= TREE_INT_CST_LOW (TREE_OPERAND (purpose, 1));
		       index++)
		    new_list = tree_cons (NULL_TREE,
					  size_binop (MINUS_EXPR,
						      build_int_2 (index, 0),
						      domain_min),
					  new_list);
		  break;
		default:
		  abort ();
		}
	    }
	}
      result = copy_node (result);
      TREE_OPERAND (result, 1) = nreverse (new_list);
      TREE_TYPE (result) = build_bitstring_type (TYPE_SIZE (type));
    }

  return result;
}

/* Check that EXPR has valid type for a RETURN or RESULT expression,
   converting to the right type.  ACTION is "RESULT" or "RETURN". */

static tree
adjust_return_value (expr, action)
     tree expr;
     const char *action;
{
  tree type = TREE_TYPE (TREE_TYPE (current_function_decl));

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      if (CH_LOCATION_P (expr))
	{
	  if (! CH_READ_COMPATIBLE (TREE_TYPE (type), 
				    TREE_TYPE (expr)))
	    {
	      error ("mode mismatch in %s expression", action);
	      return error_mark_node;
	    }
	  return convert (type, expr);
	}
      else
	{
	  error ("%s expression must be referable", action);
	  return error_mark_node;
	}
    }
  else if (! CH_COMPATIBLE (expr, type))
    {
      error ("mode mismatch in %s expression", action);
      return error_mark_node;
    }
  return convert (type, expr);
}

void
chill_expand_result (expr, result_or_return)
     tree expr;
     int result_or_return;
{
  tree type;
  const char *action_name = result_or_return ? "RESULT" : "RETURN";
  
  if (pass == 1)
    return;

  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return;

  CH_FUNCTION_SETS_RESULT (current_function_decl) = 1;

  if (chill_at_module_level || global_bindings_p ())
    error ("%s not allowed outside a PROC", action_name);

  result_never_set = 0;

  if (chill_result_decl == NULL_TREE)
    {
      error ("%s action in PROC with no declared RESULTS", action_name);
      return;
    }
  type = TREE_TYPE (chill_result_decl);

  if (TREE_CODE (type) == ERROR_MARK)
    return;

  expr = adjust_return_value (expr, action_name);

  expand_expr_stmt (build_chill_modify_expr (chill_result_decl, expr));
}

/*
 * error if EXPR not NULL and procedure doesn't
 * have a return type; 
 * warning if EXPR NULL,
 * procedure *has* a return type, and a previous
 * RESULT actions hasn't saved a return value.
 */
void
chill_expand_return (expr, implicit)
     tree expr;
     int implicit; /* 1 if an implicit return at end of function. */
{
  tree valtype;

  if (expr != NULL_TREE && TREE_CODE (expr) == ERROR_MARK)
    return;
  if (chill_at_module_level || global_bindings_p ())
    {
      error ("RETURN not allowed outside PROC");
      return;
    }

  if (pass == 1)
    return;

  result_never_set = 0;

  valtype = TREE_TYPE (TREE_TYPE (current_function_decl));
  if (TREE_CODE (valtype) == VOID_TYPE)
    {
      if (expr != NULL_TREE)
	error ("RETURN with a value, in PROC returning void");
      expand_null_return ();
    }
  else if (TREE_CODE (valtype) != ERROR_MARK)
    {
      if (expr == NULL_TREE)
	{
	  if (!CH_FUNCTION_SETS_RESULT (current_function_decl)
	      && !implicit)
	    warning ("RETURN with no value and no RESULT action in procedure");
	  expr = chill_result_decl;
	}
      else
	expr = adjust_return_value (expr, "RETURN");
      expr = build (MODIFY_EXPR, valtype,
		    DECL_RESULT (current_function_decl),
		    expr);
      TREE_SIDE_EFFECTS (expr) = 1;
      expand_return (expr);
    }
}

void
lookup_and_expand_goto (name)
     tree name;
{
  if (name == NULL_TREE ||  TREE_CODE (name) == ERROR_MARK)
    return;
  if (!ignoring)
    {
      tree decl = lookup_name (name);
      if (decl == NULL || TREE_CODE (decl) != LABEL_DECL)
	error ("no label named `%s'", IDENTIFIER_POINTER (name));
      else if (DECL_CONTEXT (decl) != current_function_decl)
	error ("cannot GOTO label `%s' outside current function",
	       IDENTIFIER_POINTER (name));
      else
	{
	  TREE_USED (decl) = 1;
	  expand_goto_except_cleanup (DECL_ACTION_NESTING_LEVEL (decl));
	  expand_goto (decl);
	}
    }
}

void
lookup_and_handle_exit (name)
     tree name;
{
  if (name == NULL_TREE ||  TREE_CODE (name) == ERROR_MARK)
    return;
  if (!ignoring)
    {
      tree label = munge_exit_label (name);
      tree decl = lookup_name (label);
      if (decl == NULL || TREE_CODE (decl) != LABEL_DECL)
	error ("no EXITable label named `%s'", IDENTIFIER_POINTER (name));
      else if (DECL_CONTEXT (decl) != current_function_decl)
	error ("cannot EXIT label `%s' outside current function",
	       IDENTIFIER_POINTER (name));
      else
	{
	  TREE_USED (decl) = 1;
	  expand_goto_except_cleanup (DECL_ACTION_NESTING_LEVEL (decl));
	  expand_goto (decl);
	}
    }
}

/* ELSE-range handling: The else-range is a chain of trees which collectively
   represent the ranges to be tested for the (ELSE) case label. Each element in
   the chain represents a range to be tested. The boundaries of the range are
   represented by INTEGER_CST trees in the PURPOSE and VALUE fields. */

/* This function updates the else-range by removing the given integer constant. */
static tree
update_else_range_for_int_const (else_range, label)
     tree else_range, label;
{
  int  lowval = 0, highval = 0;
  int  label_value = TREE_INT_CST_LOW (label);
  tree this_range, prev_range, new_range;

  /* First, find the range element containing the integer, if it exists. */
  prev_range = NULL_TREE;
  for (this_range = else_range ;
       this_range != NULL_TREE;
       this_range = TREE_CHAIN (this_range))
    {
      lowval  = TREE_INT_CST_LOW (TREE_PURPOSE (this_range));
      highval = TREE_INT_CST_LOW (TREE_VALUE (this_range));
      if (label_value >= lowval && label_value <= highval)
	break;
      prev_range = this_range;
    }

  /* If a range element containing the integer was found, then update the range. */
  if (this_range != NULL_TREE)
    {
      tree next = TREE_CHAIN (this_range);
      if (label_value == lowval)
	{
	  /* The integer is the lower bound of the range element. If it is also the
	     upper bound, then remove this range element, otherwise update it. */
	  if (label_value == highval)
	    {
	      if (prev_range == NULL_TREE)
		else_range = next;
	      else
		TREE_CHAIN (prev_range) = next;
	    }
	  else
	    TREE_PURPOSE (this_range) = build_int_2 (label_value + 1, 0);
	}
      else if (label_value == highval)
	{
	  /* The integer is the upper bound of the range element, so ajust it. */
	  TREE_VALUE (this_range) = build_int_2 (label_value - 1, 0);
	}
      else
	{
	  /* The integer is in the middle of the range element, so split it. */
	  new_range = tree_cons (
            build_int_2 (label_value + 1, 0), TREE_VALUE (this_range), next);
	  TREE_VALUE (this_range) = build_int_2 (label_value - 1, 0);
	  TREE_CHAIN (this_range) = new_range;
	}
    }
  return else_range;
}

/* Update the else-range to remove a range of values/ */
static tree
update_else_range_for_range (else_range, low_target, high_target)
     tree else_range, low_target, high_target;
{
  tree this_range, prev_range, new_range, next_range;
  int  low_range_val = 0, high_range_val = 0;
  int  low_target_val  = TREE_INT_CST_LOW (low_target);
  int  high_target_val = TREE_INT_CST_LOW (high_target);

  /* find the first else-range element which overlaps the target range. */
  prev_range = NULL_TREE;
  for (this_range = else_range ;
       this_range != NULL_TREE;
       this_range = TREE_CHAIN (this_range))
    {
      low_range_val  = TREE_INT_CST_LOW (TREE_PURPOSE (this_range));
      high_range_val = TREE_INT_CST_LOW (TREE_VALUE (this_range));
      if ((low_target_val >= low_range_val && low_target_val <= high_range_val)
	  || (high_target_val >= low_range_val && high_target_val <= high_range_val))
	break;
      prev_range = this_range;
    }
  if (this_range == NULL_TREE)
    return else_range;

  /* This first else-range element might be truncated at the top or completely
     contain the target range. */
  if (low_range_val < low_target_val)
    {
      next_range = TREE_CHAIN (this_range);
      if (high_range_val > high_target_val)
	{
	  new_range = tree_cons (
            build_int_2 (high_target_val + 1, 0), TREE_VALUE (this_range), next_range);
	  TREE_VALUE (this_range) = build_int_2 (low_target_val - 1, 0);
	  TREE_CHAIN (this_range) = new_range;
	  return else_range;
	}

      TREE_VALUE (this_range) = build_int_2 (low_target_val - 1, 0);
      if (next_range == NULL_TREE)
	return else_range;

      prev_range = this_range;
      this_range = next_range;
      high_range_val = TREE_INT_CST_LOW (TREE_VALUE (this_range));
    }

  /* There may then follow zero or more else-range elements which are completely
     contained in the target range. */
  while (high_range_val <= high_target_val)
    {
      this_range = TREE_CHAIN (this_range);
      if (prev_range == NULL_TREE)
	else_range = this_range;
      else
	TREE_CHAIN (prev_range) = this_range;

      if (this_range == NULL_TREE)
	return else_range;
      high_range_val = TREE_INT_CST_LOW (TREE_VALUE (this_range));
    }

  /* Finally, there may be a else-range element which is truncated at the bottom. */
  low_range_val = TREE_INT_CST_LOW (TREE_PURPOSE (this_range));
  if (low_range_val <= high_target_val)
    TREE_PURPOSE (this_range) = build_int_2 (high_target_val + 1, 0);

  return else_range;
}

static tree
update_else_range_for_range_expr (else_range, label)
     tree else_range, label;
{
  if (TREE_OPERAND (label, 0) == NULL_TREE)
    {
      if (TREE_OPERAND (label, 1) == NULL_TREE)
	else_range = NULL_TREE; /* (*) -- matches everything */
    }
  else
    else_range = update_else_range_for_range (
      else_range, TREE_OPERAND (label, 0), TREE_OPERAND (label, 1));

  return else_range;
}

static tree
update_else_range_for_type (else_range, label)
     tree else_range, label;
{
  tree type = TREE_TYPE (label);
  else_range = update_else_range_for_range (
    else_range, TYPE_MIN_VALUE (type), TYPE_MAX_VALUE (type));
  return else_range;
}

static tree
compute_else_range (selector, alternatives, selector_no)
     tree selector, alternatives;
     int selector_no;
{
  /* Start with an else-range that spans the entire range of the selector type. */
  tree type = TREE_TYPE (TREE_VALUE (selector));
  tree range = tree_cons (TYPE_MIN_VALUE (type), TYPE_MAX_VALUE (type), NULL_TREE);

  /* Now remove the values represented by each case lebel specified for that
     selector. The remaining range is the else-range. */
  for ( ; alternatives != NULL_TREE; alternatives = TREE_CHAIN (alternatives))
    {
      tree label;
      tree label_list = TREE_PURPOSE (alternatives);
      int  this_selector;
      for (this_selector = 0; this_selector < selector_no ; ++this_selector)
	label_list = TREE_CHAIN (label_list);

      for (label = TREE_VALUE (label_list);
	   label != NULL_TREE;
	   label = TREE_CHAIN (label))
	{
	  tree label_value = TREE_VALUE (label);
	  if (TREE_CODE (label_value) == INTEGER_CST)
	    range = update_else_range_for_int_const (range, label_value);
	  else if (TREE_CODE (label_value) == RANGE_EXPR)
	    range = update_else_range_for_range_expr (range, label_value);
	  else if (TREE_CODE (label_value) == TYPE_DECL)
	    range = update_else_range_for_type (range, label_value);

	  if (range == NULL_TREE)
	    break;
	}
    }

  return range;
}

void
compute_else_ranges (selectors, alternatives)
     tree selectors, alternatives;
{
  tree selector;
  int selector_no = 0;

  for (selector = selectors; selector != NULL_TREE; selector = TREE_CHAIN (selector))
    {
      if (ELSE_LABEL_SPECIFIED (selector))
	TREE_PURPOSE (selector) =
	  compute_else_range (selector, alternatives, selector_no);
      selector_no++;
    }
}

static tree
check_case_value (label_value, selector)
     tree label_value, selector;
{
  if (TREE_CODE (label_value) == ERROR_MARK)
    return label_value;
  if (TREE_CODE (selector) == ERROR_MARK)
    return selector;    

  /* Z.200 (6.4 Case action) says:  "The class of any discrete expression
     in the case selector list must be compatible with the corresponding
     (by position) class of the resulting list of classes of the case label
     list occurrences ...".  We don't actually construct the resulting
     list of classes, but this test should be more-or-less equivalent.
     I think... */
  if (!CH_COMPATIBLE_CLASSES (selector, label_value))
    {
      error ("case selector not compatible with label");
      return error_mark_node;
    }

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (label_value);

  if (TREE_CODE (label_value) != INTEGER_CST)
    {
      error ("case label does not reduce to an integer constant");
      return error_mark_node;
    }

  constant_expression_warning (label_value);
  return label_value;
}

void
chill_handle_case_default ()
{
  tree duplicate;
  register tree label = build_decl (LABEL_DECL, NULL_TREE, 
				    NULL_TREE);
  int success = pushcase (NULL_TREE, 0, label, &duplicate);

  if (success == 1)
    error ("ELSE label not within a CASE statement");
#if 0
  else if (success == 2)
    {
      error ("multiple default labels found in a CASE statement"); 
      error_with_decl (duplicate, "this is the first ELSE label");
    }
#endif
}

/* Handle cases label such as (I:J):  or (modename): */

static void
chill_handle_case_label_range (min_value, max_value, selector)
     tree min_value, max_value, selector;
{
  register tree label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
  min_value = check_case_value (min_value, selector);
  max_value = check_case_value (max_value, selector);
  if (TREE_CODE (min_value) != ERROR_MARK
      && TREE_CODE (max_value) != ERROR_MARK)
    {
      tree duplicate;
      int success = pushcase_range (min_value, max_value,
				    convert, label, &duplicate);
      if (success == 1)
	error ("label found outside of CASE statement");
      else if (success == 2)
	{
	  error ("duplicate CASE value");
	  error_with_decl (duplicate, "this is the first entry for that value");
	}
      else if (success == 3)
	error ("CASE value out of range");
      else if (success == 4)
	error ("empty range");
      else if (success == 5)
	error ("label within scope of cleanup or variable array");
    }
}

void
chill_handle_case_label (label_value, selector)
     tree label_value, selector;
{
  if (label_value == NULL_TREE 
      || TREE_CODE (label_value) == ERROR_MARK)
    return;
  if (TREE_CODE (label_value) == RANGE_EXPR)
    {
      if (TREE_OPERAND (label_value, 0) == NULL_TREE)
	chill_handle_case_default ();  /* i.e. (ELSE): or (*): */
      else
	chill_handle_case_label_range (TREE_OPERAND (label_value, 0),
				       TREE_OPERAND (label_value, 1),
				       selector);
    }
  else if (TREE_CODE (label_value) == TYPE_DECL)
    {
      tree type = TREE_TYPE (label_value);
      if (! discrete_type_p (type))
	error ("mode in label is not discrete");
      else
	chill_handle_case_label_range (TYPE_MIN_VALUE (type),
				       TYPE_MAX_VALUE (type),
				       selector);
    }
  else
    {
      register tree label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

      label_value = check_case_value (label_value, selector);

      if (TREE_CODE (label_value) != ERROR_MARK)
	{
	  tree duplicate;
	  int success = pushcase (label_value, convert, label, &duplicate);
	  if (success == 1)
	    error ("label not within a CASE statement");
	  else if (success == 2)
	    {
	      error ("duplicate case value");
	      error_with_decl (duplicate, 
			       "this is the first entry for that value");
	    }
	  else if (success == 3)
	    error ("CASE value out of range");
	  else if (success == 4)
	    error ("empty range");
	  else if (success == 5)
	    error ("label within scope of cleanup or variable array");
        }
    }
}

int
chill_handle_single_dimension_case_label (
  selector, label_spec, expand_exit_needed, caseaction_flag
)
  tree selector, label_spec;
  int *expand_exit_needed, *caseaction_flag;
{
  tree labels, one_label;
  int  no_completeness_check = 0;

  if (*expand_exit_needed || *caseaction_flag == 1)
    {
      expand_exit_something ();
      *expand_exit_needed = 0;
    }

  for (labels = label_spec; labels != NULL_TREE; labels = TREE_CHAIN (labels))
    for (one_label = TREE_VALUE (labels); one_label != NULL_TREE;
         one_label = TREE_CHAIN (one_label))
      {
        if (TREE_VALUE (one_label) == case_else_node)
          no_completeness_check = 1;

        chill_handle_case_label (TREE_VALUE (one_label), selector);
      }

  *caseaction_flag = 1;

  return no_completeness_check;
}

static tree
chill_handle_multi_case_label_range (low, high, selector)
  tree low, high, selector;
{
  tree low_expr, high_expr, and_expr;
  tree selector_type;
  int  low_target_val, high_target_val;
  int  low_type_val, high_type_val;

  /* we can eliminate some tests is the low and/or high value in the given range
     are outside the range of the selector type. */
  low_target_val  = TREE_INT_CST_LOW (low);
  high_target_val = TREE_INT_CST_LOW (high);
  selector_type   = TREE_TYPE (selector);
  low_type_val    = TREE_INT_CST_LOW (TYPE_MIN_VALUE (selector_type));
  high_type_val   = TREE_INT_CST_LOW (TYPE_MAX_VALUE (selector_type));

  if (low_target_val > high_type_val || high_target_val < low_type_val)
    return boolean_false_node; /* selector never in range */

  if (low_type_val >= low_target_val)
    {
      if (high_type_val <= high_target_val)
	return boolean_true_node; /* always in the range */
      return build_compare_expr (LE_EXPR, selector, high);
    }

  if (high_type_val <= high_target_val)
    return build_compare_expr (GE_EXPR, selector, low);

  /* The target range in completely within the range of the selector, but we
     might be able to save a test if the upper bound is the same as the lower
     bound. */
  if (low_target_val == high_target_val)
    return build_compare_expr (EQ_EXPR, selector, low);

  /* No optimizations possible. Just generate tests against the upper and lower
     bound of the target */
  low_expr  = build_compare_expr (GE_EXPR, selector, low);
  high_expr = build_compare_expr (LE_EXPR, selector, high);
  and_expr  = build_chill_binary_op (TRUTH_ANDIF_EXPR, low_expr, high_expr);

  return and_expr;
}

static tree
chill_handle_multi_case_else_label (selector)
     tree selector;
{
  tree else_range, selector_value, selector_type;
  tree low, high, larg;

  else_range = TREE_PURPOSE (selector);
  if (else_range == NULL_TREE)
    return boolean_false_node; /* no values in ELSE range */

  /* Test each of the ranges in the else-range chain */
  selector_value = TREE_VALUE (selector);
  selector_type  = TREE_TYPE (selector_value);
  low  = convert (selector_type, TREE_PURPOSE (else_range));
  high = convert (selector_type, TREE_VALUE (else_range));
  larg = chill_handle_multi_case_label_range (low, high, selector_value);

  for (else_range = TREE_CHAIN (else_range);
       else_range != NULL_TREE;
       else_range = TREE_CHAIN (else_range))
    {
      tree rarg;
      low  = convert (selector_type, TREE_PURPOSE (else_range));
      high = convert (selector_type, TREE_VALUE (else_range));
      rarg = chill_handle_multi_case_label_range (low, high, selector_value);
      larg = build_chill_binary_op (TRUTH_ORIF_EXPR, larg, rarg);
    }

  return larg;
}

static tree
chill_handle_multi_case_label (selector, label)
  tree selector, label;
{
  tree expr = NULL_TREE;

  if (label == NULL_TREE || TREE_CODE (label) == ERROR_MARK)
    return NULL_TREE;

  if (TREE_CODE (label) == INTEGER_CST)
    {
      int  target_val = TREE_INT_CST_LOW (label);
      tree selector_type = TREE_TYPE (TREE_VALUE (selector));
      int  low_type_val  = TREE_INT_CST_LOW (TYPE_MIN_VALUE (selector_type));
      int  high_type_val = TREE_INT_CST_LOW (TYPE_MAX_VALUE (selector_type));
      if (target_val < low_type_val || target_val > high_type_val)
	expr = boolean_false_node;
      else
	expr = build_compare_expr (EQ_EXPR, TREE_VALUE (selector), label);
    }
  else if (TREE_CODE (label) == RANGE_EXPR)
    {
      if (TREE_OPERAND (label, 0) == NULL_TREE)
	{
	  if (TREE_OPERAND (label, 1) == NULL_TREE)
	    expr = boolean_true_node; /* (*) -- matches everything */
	  else
	    expr = chill_handle_multi_case_else_label (selector);
	}
      else
	{
	  tree low = TREE_OPERAND (label, 0);
	  tree high = TREE_OPERAND (label, 1);
	  if (TREE_CODE (low) != INTEGER_CST)
	    {
	      error ("Lower bound of range must be a discrete literal expression");
	      expr = error_mark_node;
	    }
	  if (TREE_CODE (high) != INTEGER_CST)
	    {
	      error ("Upper bound of range must be a discrete literal expression");
	      expr = error_mark_node;
	    }
	  if (expr != error_mark_node)
	    {
	      expr = chill_handle_multi_case_label_range (
                       low, high, TREE_VALUE (selector));
	    }
	}
    }
  else if (TREE_CODE (label) == TYPE_DECL)
    {
      tree type = TREE_TYPE (label);
      if (! discrete_type_p (type))
	{
	  error ("mode in label is not discrete");
	  expr = error_mark_node;
	}
      else
	expr = chill_handle_multi_case_label_range (
		 TYPE_MIN_VALUE (type), TYPE_MAX_VALUE (type), TREE_VALUE (selector));
    }
  else
    {
      error ("The CASE label is not valid");
      expr = error_mark_node;
    }

  return expr;
}

static tree
chill_handle_multi_case_label_list (selector, labels)
  tree selector, labels;
{
  tree one_label, larg, rarg;

  one_label = TREE_VALUE (labels);
  larg = chill_handle_multi_case_label (selector, TREE_VALUE (one_label));

  for (one_label = TREE_CHAIN (one_label);
       one_label != NULL_TREE;
       one_label = TREE_CHAIN (one_label))
    {
      rarg = chill_handle_multi_case_label (selector, TREE_VALUE (one_label));
      larg = build_chill_binary_op (TRUTH_ORIF_EXPR, larg, rarg);
    }

  return larg;
}

tree
build_multi_case_selector_expression (selector_list, label_spec)
  tree selector_list, label_spec;
{
  tree labels, selector, larg, rarg;

  labels   = label_spec;
  selector = selector_list;
  larg = chill_handle_multi_case_label_list(selector, labels);

  for (labels = TREE_CHAIN (labels), selector = TREE_CHAIN (selector);
       labels != NULL_TREE && selector != NULL_TREE;
       labels = TREE_CHAIN (labels), selector = TREE_CHAIN (selector))
    {
      rarg = chill_handle_multi_case_label_list(selector, labels);
      larg = build_chill_binary_op (TRUTH_ANDIF_EXPR, larg, rarg);
    }

  if (labels != NULL_TREE || selector != NULL_TREE)
    error ("The number of CASE selectors does not match the number of CASE label lists");

  return larg;
}

#define BITARRAY_TEST(ARRAY, INDEX) \
  ((ARRAY)[(unsigned)(INDEX) / HOST_BITS_PER_CHAR]\
			  & (1 << ((unsigned)(INDEX) % HOST_BITS_PER_CHAR)))
#define BITARRAY_SET(ARRAY, INDEX) \
  ((ARRAY)[(unsigned)(INDEX) / HOST_BITS_PER_CHAR]\
			  |= 1 << ((unsigned)(INDEX) % HOST_BITS_PER_CHAR))

/* CASES_SEEN is a set (bitarray) of length COUNT.
   For each element that is zero, print an error message,
   assume the element have the given TYPE. */

static void
print_missing_cases (type, cases_seen, count)
     tree type;
     const unsigned char *cases_seen;
     long count;
{
  long i;
  for (i = 0;  i < count; i++)
    {
      if (BITARRAY_TEST(cases_seen, i) == 0)
	{
	  char buf[20];
	  long x = i;
	  long j;
	  tree t = type;
	  const char *err_val_name = "???";
	  if (TYPE_MIN_VALUE (t)
	      && TREE_CODE (TYPE_MIN_VALUE (t)) == INTEGER_CST)
	    x += TREE_INT_CST_LOW (TYPE_MIN_VALUE (t));
	  while (TREE_TYPE (t) != NULL_TREE)
	    t = TREE_TYPE (t);
	  switch (TREE_CODE (t))
	    {
	      tree v;
	    case BOOLEAN_TYPE:
	      err_val_name = x ? "TRUE" : "FALSE";
	      break;
	    case CHAR_TYPE:
	      {
		char *bufptr;
		if ((x >= ' ' && x < 127) && x != '\'' && x != '^')
		  sprintf (buf, "'%c'", (char)x);
		else
		  sprintf (buf, "'^(%ld)'", x);
		bufptr = buf;
		j = i;
		while (j < count && !BITARRAY_TEST(cases_seen, j))
		  j++;
		if (j > i + 1)
		  {
		    long y = x+j-i-1;
		    bufptr += strlen (bufptr);
		    if ((y >= ' ' && y < 127) && y != '\'' && y != '^')
		      sprintf (bufptr, "%s:'%c'", buf, (char)y);
		    else
		      sprintf (bufptr, "%s:'^(%ld)'", buf, y);
		    i = j - 1;      
		  }
		err_val_name = bufptr;
	      }
	      break;
	    case ENUMERAL_TYPE:
	      for (v = TYPE_VALUES (t);  v && x;  v = TREE_CHAIN (v))
		x--;
	      if (v)
		err_val_name = IDENTIFIER_POINTER (TREE_PURPOSE (v));
	      break;
	    default:
	      j = i;
	      while (j < count && !BITARRAY_TEST(cases_seen, j))
		j++;
	      if (j == i + 1)
		sprintf (buf, "%ld", x);
	      else
		sprintf (buf, "%ld:%ld", x, x+j-i-1);
	      i = j - 1;      
	      err_val_name = buf;
	      break;
	    }
	  error ("incomplete CASE - %s not handled", err_val_name);
	}
    }
}

void
check_missing_cases (type)
     tree type;
{
  int is_sparse;
  /* For each possible selector value. a one iff it has been matched
     by a case value alternative. */
  unsigned char *cases_seen;
  /* The number of possible selector values. */
  HOST_WIDE_INT size = all_cases_count (type, &is_sparse);
  long bytes_needed = (size+HOST_BITS_PER_CHAR)/HOST_BITS_PER_CHAR;

  if (size == -1)
    warning ("CASE selector with variable range");
  else if (size < 0 || size > 600000
	   /* We deliberately use malloc here - not xmalloc. */
	   || (cases_seen = (char*) malloc (bytes_needed)) == NULL)
    warning ("too many cases to do CASE completeness testing");
  else
    {
      bzero (cases_seen, bytes_needed);
      mark_seen_cases (type, cases_seen, size, is_sparse);
      print_missing_cases (type, cases_seen, size);
      free (cases_seen);
    }
}

/*
 * We build an expression tree here because, in many contexts,
 * we don't know the type of result that's desired.  By the
 * time we get to expanding the tree, we do know.
 */
tree
build_chill_case_expr (exprlist, casealtlist_expr,
		       optelsecase_expr)
     tree exprlist, casealtlist_expr, optelsecase_expr;
{
  return build (CASE_EXPR, NULL_TREE, exprlist,
		optelsecase_expr ?
		  tree_cons (NULL_TREE,
			     optelsecase_expr,
			     casealtlist_expr) :
		  casealtlist_expr);
}

/* This function transforms the selector_list and alternatives into a COND_EXPR. */
tree
build_chill_multi_dimension_case_expr (selector_list, alternatives, else_expr)
  tree selector_list, alternatives, else_expr;
{
  tree expr;

  selector_list = check_case_selector_list (selector_list);

  if (alternatives == NULL_TREE)
    return NULL_TREE;

  alternatives = nreverse (alternatives);
  /* alternatives represents the CASE label specifications and resulting values in
     the reverse order in which they appeared.
     If there is an ELSE expression, then use it. If there is no
     ELSE expression, make the last alternative (which is the first in the list)
     into the ELSE expression. This is safe because, if the CASE is complete
     (as required), then the last condition need not be checked anyway. */
  if (else_expr != NULL_TREE)
    expr = else_expr;
  else
    {
      expr = TREE_VALUE (alternatives);
      alternatives = TREE_CHAIN (alternatives);
    }

  for ( ; alternatives != NULL_TREE; alternatives = TREE_CHAIN (alternatives))
    { 
      tree value  = TREE_VALUE (alternatives);
      tree labels = TREE_PURPOSE (alternatives);
      tree cond   = build_multi_case_selector_expression(selector_list, labels);
      expr = build_nt (COND_EXPR, cond, value, expr);
    }

  return expr;
}


/* This is called with the assumption that RHS has been stabilized.  
   It has one purpose:  to iterate through the CHILL list of LHS's */
void
expand_assignment_action (loclist, modifycode, rhs)
     tree loclist;
     enum chill_tree_code modifycode;
     tree rhs;
{
  if (loclist == NULL_TREE || TREE_CODE (loclist) == ERROR_MARK
      || rhs == NULL_TREE  || TREE_CODE (rhs) == ERROR_MARK)
    return;

  if (TREE_CHAIN (loclist) != NULL_TREE)
    { /* Multiple assignment */
      tree target;
      if (TREE_TYPE (rhs) != NULL_TREE)
	rhs = save_expr (rhs);
      else if (TREE_CODE (rhs) == CONSTRUCTOR)
	error ("type of tuple cannot be implicit in multiple assignent");
      else if (TREE_CODE (rhs) == CASE_EXPR || TREE_CODE (rhs) == COND_EXPR)
	error ("conditional expression cannot be used in multiple assignent");
      else
	error ("internal error - unknown type in multiple assignment");

      if (modifycode != NOP_EXPR)
	{
	  error ("no operator allowed in multiple assignment,");
	  modifycode = NOP_EXPR;
	}

      for (target = TREE_CHAIN (loclist); target; target = TREE_CHAIN (target))
	{
	  if (!CH_EQUIVALENT (TREE_TYPE (TREE_VALUE (target)),
			      TREE_TYPE (TREE_VALUE (loclist))))
	    {
	      error
		("location modes in multiple assignment are not equivalent");
	      break;
	    }
	}
    }
  for ( ; loclist != NULL_TREE; loclist = TREE_CHAIN (loclist))
    chill_expand_assignment (TREE_VALUE (loclist), modifycode, rhs);
}

void
chill_expand_assignment (lhs, modifycode, rhs)
     tree lhs;
     enum chill_tree_code modifycode;
     tree rhs;
{
  tree loc;

  while (TREE_CODE (lhs) == COMPOUND_EXPR)
    {
      expand_expr (TREE_OPERAND (lhs, 0), const0_rtx, VOIDmode, 0);
      emit_queue ();
      lhs = TREE_OPERAND (lhs, 1);
    }

  if (TREE_CODE (lhs) == ERROR_MARK)
    return;

  /* errors for assignment to BUFFER, EVENT locations.
     what about SIGNALs? FIXME: Need similar test in
     build_chill_function_call. */
  if (TREE_CODE (lhs) == IDENTIFIER_NODE)
    {
      tree decl = lookup_name (lhs);
      if (decl)
	{
	  tree type = TREE_TYPE (decl);
	  if (CH_IS_BUFFER_MODE (type) || CH_IS_EVENT_MODE (type))
	    {
	      error ("You may not assign a value to a BUFFER or EVENT location");
	      return;
	    }
	}
    }

  if (TYPE_READONLY_PROPERTY (TREE_TYPE (lhs)) || TREE_READONLY (lhs))
    {
      error ("can't assign value to READonly location");
      return;
    }
  if (CH_TYPE_NONVALUE_P (TREE_TYPE (lhs)))
    {
      error ("cannot assign to location with non-value property");
      return;
    }

  if (TREE_CODE (TREE_TYPE (lhs)) == REFERENCE_TYPE)
    lhs = convert_from_reference (lhs);

  /* check for lhs is a location */
  loc = lhs;
  while (1)
    {
      if (TREE_CODE (loc) == SLICE_EXPR)
	loc = TREE_OPERAND (loc, 0);
      else if (TREE_CODE (loc) == SET_IN_EXPR)
	loc = TREE_OPERAND (loc, 1);
      else
	break;
    }
  if (! CH_LOCATION_P (loc))
    {
      error ("lefthand side of assignment is not a location");
      return;
    }

  /* If a binary op has been requested, combine the old LHS value with
     the RHS producing the value we should actually store into the LHS. */

  if (modifycode != NOP_EXPR)
    {
      lhs = stabilize_reference (lhs);
      /* This is to handle border-line cases such
	 as: LHS OR := [I].  This seems to be permitted
	 by the letter of Z.200, though it violates
	 its spirit, since LHS:=LHS OR [I] is
	 *not* legal. */
      if (TREE_TYPE (rhs) == NULL_TREE)
	rhs = convert (TREE_TYPE (lhs), rhs);
      rhs = build_chill_binary_op (modifycode, lhs, rhs);
    }

  rhs = chill_convert_for_assignment (TREE_TYPE (lhs), rhs, "assignment");

  /* handle the LENGTH (vary_array) := expr action */
  loc = lhs;
  if (TREE_CODE (loc) == NOP_EXPR)
    loc = TREE_OPERAND (loc, 0);
  if (TREE_CODE (loc) == COMPONENT_REF
      && chill_varying_type_p (TREE_TYPE (TREE_OPERAND (loc, 0)))
      && DECL_NAME (TREE_OPERAND (loc, 1)) == var_length_id)
    {
      expand_varying_length_assignment (TREE_OPERAND (loc, 0), rhs);
    }
  else if (TREE_CODE (lhs) == SLICE_EXPR)
    {
      tree func = lookup_name (get_identifier ("__pscpy"));
      tree dst = TREE_OPERAND (lhs, 0);
      tree dst_offset = TREE_OPERAND (lhs, 1);
      tree length = TREE_OPERAND (lhs, 2);
      tree src, src_offset;
      if (TREE_CODE (rhs) == SLICE_EXPR)
	{
	  src = TREE_OPERAND (rhs, 0);
	  /* Should check that the TREE_OPERAND (src, 0) is
	     the same as length and powerserlen (src).  FIXME */
	  src_offset = TREE_OPERAND (rhs, 1);
	}
      else
	{
	  src = rhs;
	  src_offset = integer_zero_node;
	}
      expand_expr_stmt (build_chill_function_call (func,
	tree_cons (NULL_TREE, force_addr_of (dst),
	  tree_cons (NULL_TREE, powersetlen (dst),
	    tree_cons (NULL_TREE, convert (long_unsigned_type_node, dst_offset),
	      tree_cons (NULL_TREE, force_addr_of (src),
		tree_cons (NULL_TREE, powersetlen (src),
		  tree_cons (NULL_TREE, convert (long_unsigned_type_node, src_offset),
		    tree_cons (NULL_TREE, convert (long_unsigned_type_node, length),
		       NULL_TREE)))))))));
    }

  else if (TREE_CODE (lhs) == SET_IN_EXPR)
    {
      tree from_pos = save_expr (TREE_OPERAND (lhs, 0));
      tree set = TREE_OPERAND (lhs, 1);
      tree domain = TYPE_DOMAIN (TREE_TYPE (set));
      tree set_length = size_binop (PLUS_EXPR,
				    size_binop (MINUS_EXPR,
						TYPE_MAX_VALUE (domain),
						TYPE_MIN_VALUE (domain)),
				    integer_one_node);
      tree filename = force_addr_of (get_chill_filename());
      
      if (TREE_CODE (TREE_TYPE (lhs)) != BOOLEAN_TYPE)
	sorry("bitstring slice");
      expand_expr_stmt (
	build_chill_function_call (lookup_name (
	  get_identifier ("__setbitpowerset")),
	      tree_cons (NULL_TREE, build_chill_addr_expr (set, "powerset"),
		  tree_cons (NULL_TREE, set_length,
		    tree_cons (NULL_TREE, TYPE_MIN_VALUE (domain),
		      tree_cons (NULL_TREE, convert (long_integer_type_node, from_pos),
			tree_cons (NULL_TREE, rhs,
			  tree_cons (NULL_TREE, filename,
			    tree_cons (NULL_TREE, get_chill_linenumber(),
  			      NULL_TREE)))))))));
    }

  /* Handle arrays of packed bitfields. Currently, this is limited to bitfields
     which are 1 bit wide, so use the powerset runtime function. */
  else if (TREE_CODE (lhs) == PACKED_ARRAY_REF)
    {
      tree from_pos = save_expr (TREE_OPERAND (lhs, 1));
      tree array = TREE_OPERAND (lhs, 0);
      tree domain = TYPE_DOMAIN (TREE_TYPE (array));
      tree array_length = powersetlen (array);
      tree filename = force_addr_of (get_chill_filename());
      expand_expr_stmt (
	build_chill_function_call (lookup_name (
	  get_identifier ("__setbitpowerset")),
            tree_cons (NULL_TREE, build_chill_addr_expr (array, "packed bitfield array"),
		tree_cons (NULL_TREE, convert (long_unsigned_type_node, array_length),
		  tree_cons (NULL_TREE, convert (long_integer_type_node,
						 TYPE_MIN_VALUE (domain)),
		    tree_cons (NULL_TREE, convert (long_integer_type_node, from_pos),
		      tree_cons (NULL_TREE, build1 (CONVERT_EXPR, boolean_type_node, rhs),
			tree_cons (NULL_TREE, filename,
			  tree_cons (NULL_TREE, get_chill_linenumber(),
  			    NULL_TREE)))))))));
    }

  /* The following is probably superceded by the
     above code for SET_IN_EXPR. FIXME! */
  else if (TREE_CODE (lhs) == BIT_FIELD_REF)
    {
      tree set = TREE_OPERAND (lhs, 0);
      tree numbits = TREE_OPERAND (lhs, 1);
      tree from_pos = save_expr (TREE_OPERAND (lhs, 2));
      tree domain = TYPE_DOMAIN (TREE_TYPE (set));
      tree set_length = size_binop (PLUS_EXPR,
				    size_binop (MINUS_EXPR,
						TYPE_MAX_VALUE (domain),
						TYPE_MIN_VALUE (domain)),
				    integer_one_node);
      tree filename = force_addr_of (get_chill_filename());
      tree to_pos;
      switch (TREE_CODE (TREE_TYPE (rhs)))
	{
	case SET_TYPE:
	  to_pos = size_binop (MINUS_EXPR,
			       size_binop (PLUS_EXPR, from_pos, numbits),
			       integer_one_node);
	  break;
	case BOOLEAN_TYPE:
	  to_pos = from_pos;
	  break;
	default:
	  abort ();
	}
      
      if (TREE_CODE (TREE_TYPE (lhs)) != BOOLEAN_TYPE)
	sorry("bitstring slice");
      expand_expr_stmt (
	  build_chill_function_call( lookup_name (
	      get_identifier ("__setbitpowerset")),
		tree_cons (NULL_TREE, build_chill_addr_expr (set, "powerset"),
		  tree_cons (NULL_TREE, set_length,
		    tree_cons (NULL_TREE, TYPE_MIN_VALUE (domain),
		      tree_cons (NULL_TREE, from_pos,
			tree_cons (NULL_TREE, rhs,
			  tree_cons (NULL_TREE, filename,
			    tree_cons (NULL_TREE, get_chill_linenumber(),
	  		      NULL_TREE)))))))));
    }

  else
    expand_expr_stmt (build_chill_modify_expr (lhs, rhs));
}

/* Also assumes that rhs has been stabilized */
void
expand_varying_length_assignment (lhs, rhs)
     tree lhs, rhs;
{
  tree base_array, min_domain_val;

  pedwarn ("LENGTH on left-hand-side is non-portable");
      
  if (! CH_LOCATION_P (lhs))
    {
      error ("Can only set LENGTH of array location");
      return;
    }

  /* cause a RANGE exception if rhs would cause a 'hole' in the array. */
  rhs = valid_array_index_p (lhs, rhs, "new array length too large", 1);

  base_array     = CH_VARYING_ARRAY_TYPE (TREE_TYPE (lhs));
  min_domain_val = TYPE_MIN_VALUE (TYPE_DOMAIN (base_array));

  lhs = build_component_ref (lhs, var_length_id);
  rhs = size_binop (MINUS_EXPR, rhs, min_domain_val);

  expand_expr_stmt (build_chill_modify_expr (lhs, rhs));
}

void
push_action ()
{
  push_handler ();
  if (ignoring)
    return;
  emit_line_note (input_filename, lineno);
}
