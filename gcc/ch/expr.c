/* Convert language-specific tree expression to rtl instructions,
   for GNU CHILL compiler.
   Copyright (C) 1992, 93, 1994, 1998, 1999 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "expr.h"
#include "ch-tree.h"
#include "assert.h"
#include "lex.h"
#include "convert.h"
#include "toplev.h"

extern char **boolean_code_name;
extern int  flag_old_strings;
extern int  ignore_case;
extern int  special_UC;

/* definitions for duration built-ins */
#define MILLISECS_MULTIPLIER                                 1
#define SECS_MULTIPLIER            MILLISECS_MULTIPLIER * 1000
#define MINUTES_MULTIPLIER                SECS_MULTIPLIER * 60
#define HOURS_MULTIPLIER               MINUTES_MULTIPLIER * 60
#define DAYS_MULTIPLIER                  HOURS_MULTIPLIER * 24

/* the maximum value for each of the calls */
#define MILLISECS_MAX                               0xffffffff
#define SECS_MAX                                       4294967
#define MINUTES_MAX                                      71582
#define HOURS_MAX                                         1193
#define DAYS_MAX                                            49

/* forward declarations */
static rtx chill_expand_expr		PARAMS ((tree, rtx, enum machine_mode, 
						enum expand_modifier));
static tree chill_expand_case_expr	PARAMS ((tree));
static int check_arglist_length		PARAMS ((tree, int, int, tree));
static tree internal_build_compound_expr PARAMS ((tree, int));
static int is_really_instance		PARAMS ((tree));
static int invalid_operand		PARAMS ((enum chill_tree_code,
						tree, int));
static int invalid_right_operand	PARAMS ((enum chill_tree_code, tree));
static tree build_chill_abstime		PARAMS ((tree));
static tree build_allocate_memory_call	PARAMS ((tree, tree));
static tree build_allocate_global_memory_call PARAMS ((tree, tree));
static tree build_return_memory		PARAMS ((tree));
static tree build_chill_duration	PARAMS ((tree, unsigned long,
						tree, unsigned long));
static tree build_chill_floatcall	PARAMS ((tree, const char *,
						const char *));
static tree build_allocate_getstack	PARAMS ((tree, tree, const char *,
						const char *, tree, tree));
static tree build_chill_allocate	PARAMS ((tree, tree));
static tree build_chill_getstack	PARAMS ((tree, tree));
static tree build_chill_terminate	PARAMS ((tree));
static tree build_chill_inttime		PARAMS ((tree, tree));
static tree build_chill_lower_or_upper	PARAMS ((tree, int));
static tree build_max_min		PARAMS ((tree, int));
static tree build_chill_pred_or_succ	PARAMS ((tree, enum tree_code));
static tree expand_packed_set		PARAMS ((const char *, int, tree));
static tree fold_set_expr		PARAMS ((enum chill_tree_code,
						tree, tree));
static tree build_compare_set_expr	PARAMS ((enum tree_code, tree, tree));
static tree scalar_to_string		PARAMS ((tree));
static tree build_concat_expr		PARAMS ((tree, tree));
static tree build_compare_string_expr	PARAMS ((enum tree_code, tree, tree));
static tree compare_records		PARAMS ((tree, tree));
static tree string_char_rep		PARAMS ((int, tree));
static tree build_boring_bitstring	PARAMS ((long, int));

/* variable to hold the type the DESCR built-in returns */
static tree descr_type = NULL_TREE;


/* called from ch-lex.l */
void
init_chill_expand ()
{
  lang_expand_expr = chill_expand_expr;
}

/* Take the address of something that needs to be passed by reference. */
tree
force_addr_of (value)
     tree value;
{
  /* FIXME.  Move to memory, if needed. */
  if (TREE_CODE (value) == INDIRECT_REF)
    return convert_to_pointer (ptr_type_node, TREE_OPERAND (value, 0));
  mark_addressable (value);
  return build1 (ADDR_EXPR, ptr_type_node, value);
}

/* Check that EXP has a known type. */

tree
check_have_mode (exp, context)
     tree exp;
     const char *context;
{
  if (TREE_CODE (exp) != ERROR_MARK && TREE_TYPE (exp) == NULL_TREE)
    {
      if (TREE_CODE (exp) == CONSTRUCTOR)
	error ("tuple without specified mode not allowed in %s", context);
      else if (TREE_CODE (exp) == COND_EXPR || TREE_CODE (exp) == CASE_EXPR)
	error ("conditional expression not allowed in %s", context);
      else
	error ("internal error:  unknown expression mode in %s", context);

      return error_mark_node;
    }
  return exp;
}

/* Check that EXP is discrete.  Handle conversion if flag_old_strings. */

tree
check_case_selector (exp)
     tree exp;
{
  if (exp != NULL_TREE && TREE_TYPE (exp) != NULL_TREE)
    exp = convert_to_discrete (exp);
  if (exp)
    return exp;
  error ("CASE selector is not a discrete expression");
  return error_mark_node;
}

tree
check_case_selector_list (list)
     tree list;
{
  tree selector, exp, return_list = NULL_TREE;

  for (selector = list; selector != NULL_TREE; selector = TREE_CHAIN (selector))
    {
      exp = check_case_selector (TREE_VALUE (selector));
      if (exp == error_mark_node)
	{
	  return_list = error_mark_node;
	  break;
	}
      return_list = tree_cons (TREE_PURPOSE (selector), exp, return_list);
    }

  return nreverse(return_list);
}

static tree
chill_expand_case_expr (expr)
     tree expr;
{
  tree selector_list = TREE_OPERAND (expr, 0), selector;
  tree alternatives  = TREE_OPERAND (expr, 1);
  tree type = TREE_TYPE (expr);
  int  else_seen = 0;
  tree result;

  if (TREE_CODE (selector_list) != TREE_LIST
    || TREE_CODE (alternatives) != TREE_LIST)
    abort();
  if (TREE_CHAIN (selector_list) != NULL_TREE)
    abort ();

  /* make a temp for the case result */
  result = decl_temp1 (get_unique_identifier ("CASE_EXPR"),
		       type, 0, NULL_TREE, 0, 0);

  selector = check_case_selector (TREE_VALUE (selector_list));

  expand_start_case (1, selector, TREE_TYPE (selector), "CASE expression");

  alternatives = nreverse (alternatives);
  for ( ; alternatives != NULL_TREE; alternatives = TREE_CHAIN (alternatives))
    { 
      tree labels = TREE_PURPOSE (alternatives), t;
      
      if (labels == NULL_TREE)
	{
	  chill_handle_case_default ();
	  else_seen++;
        }
      else
	{
	  tree label;
	  if (labels != NULL_TREE)
	    {
	      for (label = TREE_VALUE (labels);
		   label != NULL_TREE; label = TREE_CHAIN (label))
		chill_handle_case_label (TREE_VALUE (label), selector);
	      labels = TREE_CHAIN (labels);
	      if (labels != NULL_TREE)
		error ("The number of CASE selectors does not match the number of CASE label lists");
		
	    }
        }

      t = build (MODIFY_EXPR, type, result,
		 convert (type, TREE_VALUE (alternatives)));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr_stmt (t);
      expand_exit_something ();
    }

  if (!else_seen)
    {
      chill_handle_case_default ();
      expand_exit_something ();
#if 0
      expand_raise ();
#endif

      check_missing_cases (TREE_TYPE (selector));
    }

  expand_end_case (selector);
  return result;
}

/* Hook used by expand_expr to expand CHILL-specific tree codes.  */

static rtx
chill_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);
  register enum machine_mode mode = TYPE_MODE (type);
  register enum tree_code code = TREE_CODE (exp);
  rtx original_target = target;
  rtx op0, op1;
  int ignore = target == const0_rtx;
  const char *lib_func;                   /* name of library routine */

  if (ignore)
    target = 0, original_target = 0;

  /* No sense saving up arithmetic to be done
     if it's all in the wrong mode to form part of an address.
     And force_operand won't know whether to sign-extend or zero-extend.  */

  if (mode != Pmode && modifier == EXPAND_SUM)
    modifier = EXPAND_NORMAL;

  switch (code)
    {
    case STRING_EQ_EXPR:
    case STRING_LT_EXPR:
      {
	rtx func = gen_rtx (SYMBOL_REF, Pmode,
			    code == STRING_EQ_EXPR ? "__eqstring"
			    : "__ltstring");
	tree exp0 = TREE_OPERAND (exp, 0);
	tree exp1 = TREE_OPERAND (exp, 1);
	tree size0, size1;
	rtx op0, op1, siz0, siz1;
	if (chill_varying_type_p (TREE_TYPE (exp0)))
	  {
	    exp0 = save_if_needed (exp0);
	    size0 = convert (integer_type_node,
			     build_component_ref (exp0, var_length_id));
	    exp0 = build_component_ref (exp0, var_data_id);
	  }
	else
	  size0 = size_in_bytes (TREE_TYPE (exp0));
	if (chill_varying_type_p (TREE_TYPE (exp1)))
	  {
	    exp1 = save_if_needed (exp1);
	    size1 = convert (integer_type_node,
			     build_component_ref (exp1, var_length_id));
	    exp1 = build_component_ref (exp1, var_data_id);
	  }
	else
	  size1 = size_in_bytes (TREE_TYPE (exp1));

	op0 = expand_expr (force_addr_of (exp0),
			   NULL_RTX, MEM, EXPAND_CONST_ADDRESS);
	op1 = expand_expr (force_addr_of (exp1),
			   NULL_RTX, MEM, EXPAND_CONST_ADDRESS);
	siz0 = expand_expr (size0, NULL_RTX, VOIDmode, 0);
	siz1 = expand_expr (size1, NULL_RTX, VOIDmode, 0);
	return emit_library_call_value (func, target,
					0, QImode, 4,
					op0, GET_MODE (op0),
					siz0, TYPE_MODE (sizetype),
					op1, GET_MODE (op1),
					siz1, TYPE_MODE (sizetype));
      }

    case CASE_EXPR:
      return expand_expr (chill_expand_case_expr (exp),
			  NULL_RTX, VOIDmode, 0);
      break;

    case SLICE_EXPR:
      {
	tree func_call;
	tree array = TREE_OPERAND (exp, 0);
	tree min_value = TREE_OPERAND (exp, 1);
	tree length = TREE_OPERAND (exp, 2);
	tree new_type = TREE_TYPE (exp);
	tree temp = decl_temp1 (get_unique_identifier ("BITSTRING"),
				new_type, 0, NULL_TREE, 0, 0);
	if (! CH_REFERABLE (array) && TYPE_MODE (TREE_TYPE (array)) != BLKmode)
	  array = decl_temp1 (get_unique_identifier ("BSTRINGVAL"),
				TREE_TYPE (array), 0, array, 0, 0);
	func_call = build_chill_function_call (
		    lookup_name (get_identifier ("__psslice")),
			   tree_cons (NULL_TREE, 
                             build_chill_addr_expr (temp, (char *)0),
			       tree_cons (NULL_TREE, length,
		                 tree_cons (NULL_TREE,
                                    force_addr_of (array),
			             tree_cons (NULL_TREE, powersetlen (array),
			               tree_cons (NULL_TREE, convert (integer_type_node, min_value),
				         tree_cons (NULL_TREE, length, NULL_TREE)))))));
	expand_expr (func_call, const0_rtx, VOIDmode, 0);
	emit_queue ();
	return expand_expr (temp, ignore ? const0_rtx : target,
			    VOIDmode, 0);
      }
      
    /* void __concatstring (char *out, char *left, unsigned left_len,
                            char *right, unsigned right_len) */
    case CONCAT_EXPR:
      {
	tree exp0 = TREE_OPERAND (exp, 0);
	tree exp1 = TREE_OPERAND (exp, 1);
	rtx size0 = NULL_RTX, size1 = NULL_RTX;
	rtx targetx;

	if (TREE_CODE (exp1) == UNDEFINED_EXPR)
	  {
	    if (TYPE_MODE (TREE_TYPE (exp0)) == BLKmode
		&& TYPE_MODE (TREE_TYPE (exp)) == BLKmode)
	      {
		rtx temp = expand_expr (exp0, target, tmode, modifier);
		if (temp == target || target == NULL_RTX)
		  return temp;
		emit_block_move (target, temp, expr_size (exp0),
				 TYPE_ALIGN (TREE_TYPE(exp0)) / BITS_PER_UNIT);
		return target;
	      }
	    else
	      {
		exp0 = force_addr_of (exp0);
		exp0 = convert (build_pointer_type (TREE_TYPE (exp)), exp0);
		exp0 = build1 (INDIRECT_REF, TREE_TYPE (exp), exp0);
		return expand_expr (exp0,
				    NULL_RTX, Pmode, EXPAND_CONST_ADDRESS);
	      }
	  }

	if (TREE_CODE (type) == ARRAY_TYPE)
	  {
	    /* No need to handle scalars or varying strings here, since that
	       was done in convert or build_concat_expr. */
	    size0 = expand_expr (size_in_bytes (TREE_TYPE (exp0)),
				 NULL_RTX, Pmode, EXPAND_CONST_ADDRESS);

	    size1 = expand_expr (size_in_bytes (TREE_TYPE (exp1)),
				   NULL_RTX, Pmode, EXPAND_CONST_ADDRESS);

	    /* build a temp for the result, target is its address */
	    if (target == NULL_RTX)
	      {
		tree type0 = TREE_TYPE (exp0);
		tree type1 = TREE_TYPE (exp1);
		int	len0 = int_size_in_bytes (type0);
		int	len1 = int_size_in_bytes (type1);

		if (len0 < 0 && TYPE_ARRAY_MAX_SIZE (type0)
		    && TREE_CODE (TYPE_ARRAY_MAX_SIZE (type0)) == INTEGER_CST)
		  len0 = TREE_INT_CST_LOW (TYPE_ARRAY_MAX_SIZE (type0));

		if (len1 < 0 && TYPE_ARRAY_MAX_SIZE (type1)
		    && TREE_CODE (TYPE_ARRAY_MAX_SIZE (type1)) == INTEGER_CST)
		  len1 = TREE_INT_CST_LOW (TYPE_ARRAY_MAX_SIZE (type1));

		if (len0 < 0 || len1 < 0)
		  fatal ("internal error - don't know how much space is needed for concatenation");
		target = assign_stack_temp (mode, len0 + len1, 0);
		preserve_temp_slots (target);
	      }
	  }
	else if (TREE_CODE (type) == SET_TYPE)
	  {
	    if (target == NULL_RTX)
	      {
		target = assign_stack_temp (mode, int_size_in_bytes (type), 0);
		preserve_temp_slots (target);
	      }
	  }
	else
	  abort ();

	if (GET_CODE (target) == MEM)
	  targetx = target;
	else
	  targetx = assign_stack_temp (mode, GET_MODE_SIZE (mode), 0);

	/* expand 1st operand to a pointer to the array */
	op0 = expand_expr (force_addr_of (exp0),
			   NULL_RTX, MEM, EXPAND_CONST_ADDRESS);

	/* expand 2nd operand to a pointer to the array */
	op1 = expand_expr (force_addr_of (exp1),
			   NULL_RTX, MEM, EXPAND_CONST_ADDRESS);

	if (TREE_CODE (type) == SET_TYPE)
	  {
	    size0 = expand_expr (powersetlen (exp0),
				 NULL_RTX, VOIDmode, 0);
	    size1 = expand_expr (powersetlen (exp1),
				 NULL_RTX, VOIDmode, 0);

	    emit_library_call (gen_rtx(SYMBOL_REF, Pmode, "__concatps"),
			       0, Pmode, 5, XEXP (targetx, 0), Pmode,
			       op0, GET_MODE (op0),
			       convert_to_mode (TYPE_MODE (sizetype),
						size0, TREE_UNSIGNED (sizetype)),
			       TYPE_MODE (sizetype),
			       op1, GET_MODE (op1),
			       convert_to_mode (TYPE_MODE (sizetype),
						size1, TREE_UNSIGNED (sizetype)),
			       TYPE_MODE (sizetype));
	  }
	else
	  {
	    /* copy left, then right array to target */
	    emit_library_call (gen_rtx(SYMBOL_REF, Pmode, "__concatstring"),
			       0, Pmode, 5, XEXP (targetx, 0), Pmode,
			       op0, GET_MODE (op0),
			       convert_to_mode (TYPE_MODE (sizetype),
						size0, TREE_UNSIGNED (sizetype)),
			       TYPE_MODE (sizetype),
			       op1, GET_MODE (op1),
			       convert_to_mode (TYPE_MODE (sizetype),
						size1, TREE_UNSIGNED (sizetype)),
			       TYPE_MODE (sizetype));
	  }
	if (targetx != target)
	  emit_move_insn (target, targetx);
	return target;
      }

      /* FIXME: the set_length computed below is a compile-time constant;
	 you'll need to re-write that part for VARYING bit arrays, and
	 possibly the set pointer will need to be adjusted to point past
	 the word containing its dynamic length. */

    /* void __notpowerset (char *out, char *src,
       unsigned long bitlength) */
    case SET_NOT_EXPR:
      {
	
	tree expr = TREE_OPERAND (exp, 0);
	tree tsize = powersetlen (expr);
	rtx targetx;

	if (TREE_CODE (TREE_TYPE (expr)) != SET_TYPE)
	  tsize = fold (build (MULT_EXPR, sizetype, tsize, 
			       size_int (BITS_PER_UNIT)));

	/* expand 1st operand to a pointer to the set */
	op0 = expand_expr (force_addr_of (expr),
			   NULL_RTX, MEM, EXPAND_CONST_ADDRESS);

	/* build a temp for the result, target is its address */
	if (target == NULL_RTX)
	  {
	    target = assign_stack_temp (TYPE_MODE (TREE_TYPE (exp)), 
					int_size_in_bytes (TREE_TYPE (exp)),
					0);
	    preserve_temp_slots (target);
	  }
	if (GET_CODE (target) == MEM)
	  targetx = target;
	else
	  targetx = assign_stack_temp (GET_MODE (target),
				       GET_MODE_SIZE (GET_MODE (target)),
				       0);
	emit_library_call (gen_rtx(SYMBOL_REF, Pmode, "__notpowerset"), 
			   0, VOIDmode, 3, XEXP (targetx, 0), Pmode,
			   op0, GET_MODE (op0),
			   expand_expr (tsize, NULL_RTX, MEM, 
					EXPAND_CONST_ADDRESS),
			   TYPE_MODE (long_unsigned_type_node));
	if (targetx != target)
	  emit_move_insn (target, targetx);
	return target;
      }

    case SET_DIFF_EXPR:
      lib_func = "__diffpowerset";
      goto format_2;

    case SET_IOR_EXPR:
      lib_func = "__orpowerset";
      goto format_2;

    case SET_XOR_EXPR:
      lib_func = "__xorpowerset";
      goto format_2;

    /* void __diffpowerset (char *out, char *left, char *right,
                            unsigned bitlength) */
    case SET_AND_EXPR:
      lib_func = "__andpowerset";
    format_2:
      {
	tree expr = TREE_OPERAND (exp, 0);
	tree tsize = powersetlen (expr);
	rtx targetx;

	if (TREE_CODE (TREE_TYPE (expr)) != SET_TYPE)
	  tsize = fold (build (MULT_EXPR, long_unsigned_type_node,
			       tsize, 
			       size_int (BITS_PER_UNIT)));

	/* expand 1st operand to a pointer to the set */
        op0 = expand_expr (force_addr_of (expr),
			   NULL_RTX, MEM, EXPAND_CONST_ADDRESS);

	/* expand 2nd operand to a pointer to the set */
        op1 = expand_expr (force_addr_of (TREE_OPERAND (exp, 1)),
			   NULL_RTX, MEM,
			   EXPAND_CONST_ADDRESS);

/* FIXME: re-examine this code - the unary operator code above has recently
   (93/03/12) been changed a lot.  Should this code also change? */
	/* build a temp for the result, target is its address */
	if (target == NULL_RTX)
	  {
	    target = assign_stack_temp (TYPE_MODE (TREE_TYPE (exp)), 
					int_size_in_bytes (TREE_TYPE (exp)),
					0);
	    preserve_temp_slots (target);
	  }
	if (GET_CODE (target) == MEM)
	  targetx = target;
	else
	  targetx = assign_stack_temp (GET_MODE (target),
				       GET_MODE_SIZE (GET_MODE (target)), 0);
	emit_library_call (gen_rtx(SYMBOL_REF, Pmode, lib_func),
			   0, VOIDmode, 4, XEXP (targetx, 0), Pmode,
			   op0, GET_MODE (op0), op1, GET_MODE (op1),
			   expand_expr (tsize, NULL_RTX, MEM, 
					EXPAND_CONST_ADDRESS),
			   TYPE_MODE (long_unsigned_type_node));
	if (target != targetx)
	  emit_move_insn (target, targetx);
	return target;
      }

    case SET_IN_EXPR:
      {
	tree set = TREE_OPERAND (exp, 1);
	tree pos = convert (long_unsigned_type_node, TREE_OPERAND (exp, 0));
	tree set_type = TREE_TYPE (set);
	tree set_length = discrete_count (TYPE_DOMAIN (set_type));
	tree min_val = convert (long_integer_type_node,
				TYPE_MIN_VALUE (TYPE_DOMAIN (set_type)));
	tree fcall;
	
	/* FIXME: Function-call not needed if pos and width are constant! */
	if (! mark_addressable (set))
	  {
	    error ("powerset is not addressable");
	    return const0_rtx;
	  }
	/* we use different functions for bitstrings and powersets */
	if (CH_BOOLS_TYPE_P (set_type))
	  fcall =
             build_chill_function_call (
               lookup_name (get_identifier ("__inbitstring")),
	         tree_cons (NULL_TREE, 
	           convert (long_unsigned_type_node, pos), 
		     tree_cons (NULL_TREE,
		       build1 (ADDR_EXPR, build_pointer_type (set_type), set),
		         tree_cons (NULL_TREE, 
		           convert (long_unsigned_type_node, set_length),
		             tree_cons (NULL_TREE, min_val,
                               tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                                 build_tree_list (NULL_TREE, get_chill_linenumber ())))))));
	else
	  fcall =
             build_chill_function_call (
               lookup_name (get_identifier ("__inpowerset")),
	         tree_cons (NULL_TREE, 
	           convert (long_unsigned_type_node, pos), 
		     tree_cons (NULL_TREE,
		       build1 (ADDR_EXPR, build_pointer_type (set_type), set),
		         tree_cons (NULL_TREE, 
		           convert (long_unsigned_type_node, set_length),
		             build_tree_list (NULL_TREE, min_val)))));
	return expand_expr (fcall, NULL_RTX, VOIDmode, 0);
      }

    case PACKED_ARRAY_REF:
      {
	tree array = TREE_OPERAND (exp, 0);
	tree pos = save_expr (TREE_OPERAND (exp, 1));
	tree array_type = TREE_TYPE (array);
	tree array_length = discrete_count (TYPE_DOMAIN (array_type));
	tree min_val = convert (long_integer_type_node,
				TYPE_MIN_VALUE (TYPE_DOMAIN (array_type)));
	tree fcall;
	
	/* FIXME: Function-call not needed if pos and width are constant! */
	/* TODO: make sure this makes sense. */
	if (! mark_addressable (array))
	  {
	    error ("array is not addressable");
	    return const0_rtx;
	  }
	fcall =
	  build_chill_function_call (
               lookup_name (get_identifier ("__inpowerset")),
	         tree_cons (NULL_TREE, 
	           convert (long_unsigned_type_node, pos), 
		     tree_cons (NULL_TREE,
		       build1 (ADDR_EXPR, build_pointer_type (array_type), array),
		         tree_cons (NULL_TREE, 
		           convert (long_unsigned_type_node, array_length),
		             build_tree_list (NULL_TREE, min_val)))));
	return expand_expr (fcall, NULL_RTX, VOIDmode, 0);
      }

    case UNDEFINED_EXPR:
      if (target == 0)
	{
	  target = assign_stack_temp (TYPE_MODE (TREE_TYPE (exp)), 
				      int_size_in_bytes (TREE_TYPE (exp)), 0);
	  preserve_temp_slots (target);
	}
      /* We don't actually need to *do* anything ... */
      return target;

    default:
      break;
    }

  /* NOTREACHED */
  return NULL;
}

/* Check that the argument list has a length in [min_length .. max_length].
   (max_length == -1 means "infinite".)
   If so return the actual length.
   Otherwise, return an error message and return -1. */

static int
check_arglist_length (args, min_length, max_length, name)
     tree args;
     int min_length;
     int max_length;
     tree name;
{
  int length = list_length (args);
  if (length < min_length)
    error ("Too few arguments in call to `%s'", IDENTIFIER_POINTER (name));
  else if (max_length != -1 && length > max_length)
    error ("Too many arguments in call to `%s'", IDENTIFIER_POINTER (name));
  else
    return length;
  return -1;
}

/*
 * This is the code from c-typeck.c, with the C-specific cruft
 * removed (possibly I just didn't understand it, but it was
 * apparently simply discarding part of my LIST).
 */
static tree
internal_build_compound_expr (list, first_p)
     tree list;
     int first_p ATTRIBUTE_UNUSED;
{
  register tree rest;

  if (TREE_CHAIN (list) == 0)
    return TREE_VALUE (list);

  rest = internal_build_compound_expr (TREE_CHAIN (list), FALSE);

  if (! TREE_SIDE_EFFECTS (TREE_VALUE (list)))
    return rest;

  return build (COMPOUND_EXPR, TREE_TYPE (rest), TREE_VALUE (list), rest);
}


/* Given a list of expressions, return a compound expression
   that performs them all and returns the value of the last of them.  */
/* FIXME: this should be merged with the C version */
tree
build_chill_compound_expr (list)
     tree list;
{
  return internal_build_compound_expr (list, TRUE);
}

/* Given an expression PTR for a pointer, return an expression
   for the value pointed to.
   do_empty_check is 0, don't perform a NULL pointer check,
   else do it. */

tree
build_chill_indirect_ref (ptr, mode, do_empty_check)
     tree ptr;
     tree mode;
     int do_empty_check;
{
  register tree type;

  if (ptr == NULL_TREE || TREE_CODE (ptr) == ERROR_MARK)
    return ptr;
  if (mode != NULL_TREE && TREE_CODE (mode) == ERROR_MARK)
    return error_mark_node;

  type = TREE_TYPE (ptr);

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      type = TREE_TYPE (type);
      ptr = convert (type, ptr);
    }

  /* check for ptr is really a POINTER */
  if (TREE_CODE (type) != POINTER_TYPE)
    {
      error ("cannot dereference, not a pointer.");
      return error_mark_node;
    }
  
  if (mode && TREE_CODE (mode) == IDENTIFIER_NODE)
    {
      tree decl = lookup_name (mode);
      if (decl == NULL_TREE || TREE_CODE (decl) != TYPE_DECL)
	{
	  if (pass == 2)
	    error ("missing '.' operator or undefined mode name `%s'.",
		   IDENTIFIER_POINTER (mode));
#if 0
	  error ("You have forgotten the '.' operator which must");
	  error (" precede a STRUCT field reference, or `%s' is an undefined mode", 
		 IDENTIFIER_POINTER (mode));
#endif
	  return error_mark_node;
	}
    }

  if (mode)
    {
      mode = get_type_of (mode);
      ptr = convert (build_pointer_type (mode), ptr);
    }
  else if (type == ptr_type_node)
    {
      error ("Can't dereference PTR value using unary `->'.");
      return error_mark_node;
    }

  if (do_empty_check)
    ptr = check_non_null (ptr);

  type = TREE_TYPE (ptr);

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (TREE_CODE (ptr) == ADDR_EXPR
	  && !flag_volatile
	  && (TREE_TYPE (TREE_OPERAND (ptr, 0))
	      == TREE_TYPE (type)))
	return TREE_OPERAND (ptr, 0);
      else
	{
	  tree t = TREE_TYPE (type);
	  register tree ref = build1 (INDIRECT_REF,
				      TYPE_MAIN_VARIANT (t), ptr);

	  if (TYPE_SIZE (t) == 0 && TREE_CODE (t) != ARRAY_TYPE)
	    {
	      error ("dereferencing pointer to incomplete type");
	      return error_mark_node;
	    }
	  if (TREE_CODE (t) == VOID_TYPE)
	    warning ("dereferencing `void *' pointer");

	  /* We *must* set TREE_READONLY when dereferencing a pointer to const,
	     so that we get the proper error message if the result is used
	     to assign to.  Also, &* is supposed to be a no-op.
	     And ANSI C seems to specify that the type of the result
	     should be the const type.  */
	  /* A de-reference of a pointer to const is not a const.  It is valid
	     to change it via some other pointer.  */
	  TREE_READONLY (ref) = TYPE_READONLY (t);
	  TREE_SIDE_EFFECTS (ref)
	    = TYPE_VOLATILE (t) || TREE_SIDE_EFFECTS (ptr) || flag_volatile;
	  TREE_THIS_VOLATILE (ref) = TYPE_VOLATILE (t) || flag_volatile;
	  return ref;
	}
    }
  else if (TREE_CODE (ptr) != ERROR_MARK)
    error ("invalid type argument of `->'");
  return error_mark_node;
}

/* NODE is a COMPONENT_REF whose mode is an IDENTIFIER,
   which is replaced by the proper FIELD_DECL.
   Also do the right thing for variant records. */

tree
resolve_component_ref (node)
     tree node;
{
  tree datum = TREE_OPERAND (node, 0);
  tree field_name = TREE_OPERAND (node, 1);
  tree type = TREE_TYPE (datum);
  tree field;
  if (TREE_CODE (datum) == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      type = TREE_TYPE (type);
      TREE_OPERAND (node, 0) = datum = convert (type, datum);
    }
  if (TREE_CODE (type) != RECORD_TYPE)
    {
      error ("operand of '.' is not a STRUCT");
      return error_mark_node;
    }

  TREE_READONLY (node) = TREE_READONLY (datum);
  TREE_SIDE_EFFECTS (node) = TREE_SIDE_EFFECTS (datum);

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (TREE_TYPE (field)) == UNION_TYPE)
	{
	  tree variant;
	  for (variant = TYPE_FIELDS (TREE_TYPE (field));
	       variant;  variant = TREE_CHAIN (variant))
	    {
	      tree vfield;
	      for (vfield = TYPE_FIELDS (TREE_TYPE (variant));
		   vfield; vfield = TREE_CHAIN (vfield))
		{
		  if (DECL_NAME (vfield) == field_name)
		    { /* Found a variant field */
		      datum = build (COMPONENT_REF, TREE_TYPE (field),
				     datum, field);
		      datum = build (COMPONENT_REF, TREE_TYPE (variant),
				     datum, variant);
		      TREE_OPERAND (node, 0) = datum;
		      TREE_OPERAND (node, 1) = vfield;
		      TREE_TYPE (node) = TREE_TYPE (vfield);
		      TREE_READONLY (node) |= TYPE_READONLY (TREE_TYPE (node));
#if 0
		      if (flag_testing_tags)
			{
			  tree tagtest = NOT IMPLEMENTED;
			  tree tagf = ridpointers[(int) RID_RANGEFAIL];
			  node = check_expression (node, tagtest,
						   tagf);
			}
#endif
		      return node;
		    }
		}
	    }
	}

      if (DECL_NAME (field) == field_name)
	{ /* Found a fixed field */
	  TREE_OPERAND (node, 1) = field;
	  TREE_TYPE (node) = TREE_TYPE (field);
	  TREE_READONLY (node) |= TYPE_READONLY (TREE_TYPE (node));
	  return fold (node);
	}
    }

  error ("No field named `%s'", IDENTIFIER_POINTER (field_name));
  return error_mark_node;
}

tree
build_component_ref (datum, field_name)
  tree datum, field_name;
{
  tree node = build_nt (COMPONENT_REF, datum, field_name);
  if (pass != 1)
    node = resolve_component_ref (node);
  return node;
}

/*
 function checks (for build_chill_component_ref) if a given
 type is really an instance type. CH_IS_INSTANCE_MODE is not
 strict enough in this case, i.e. SYNMODE foo = STRUCT (a, b UINT)
 is compatible to INSTANCE. */

static int
is_really_instance (type)
     tree type;
{
  tree decl = TYPE_NAME (type);

  if (decl == NULL_TREE)
    /* this is not an instance */
    return 0;

  if (DECL_NAME (decl) == ridpointers[(int)RID_INSTANCE])
    /* this is an instance */
    return 1;

  if (TYPE_FIELDS (type) == TYPE_FIELDS (instance_type_node))
    /* we have a NEWMODE'd instance */
    return 1;

  return 0;
}

/* This function is called by the parse.
   Here we check if the user tries to access a field in a type which is
   layouted as a structure but isn't like INSTANCE, BUFFER, EVENT, ASSOCIATION,
   ACCESS, TEXT, or VARYING array or character string.
   We don't do this in build_component_ref cause this function gets
   called from the compiler to access fields in one of the above mentioned
   modes. */
tree
build_chill_component_ref (datum, field_name)
     tree datum, field_name;
{
  tree type = TREE_TYPE (datum);
  if ((type != NULL_TREE && TREE_CODE (type) == RECORD_TYPE) &&
      ((CH_IS_INSTANCE_MODE (type) && is_really_instance (type)) ||
	CH_IS_BUFFER_MODE (type) ||
       CH_IS_EVENT_MODE (type) || CH_IS_ASSOCIATION_MODE (type) ||
       CH_IS_ACCESS_MODE (type) || CH_IS_TEXT_MODE (type) ||
       chill_varying_type_p (type)))
    {
      error ("operand of '.' is not a STRUCT");
      return error_mark_node;
    }
  return build_component_ref (datum, field_name);
}

/*
 * Check for invalid binary operands & unary operands
 * RIGHT is 1 if checking right operand or unary operand;
 * it is 0 if checking left operand.
 *
 * return 1 if the given operand is NOT compatible as the
 * operand of the given operator
 *
 * return 0 if they might be compatible
 */
static int
invalid_operand (code, type, right)
     enum chill_tree_code code;
     tree type;
     int right; /* 1 if right operand */
{
  switch ((int)code)
    {
    case ADDR_EXPR:
      break;
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_NOT_EXPR:
    case BIT_XOR_EXPR:
      goto relationals;
    case CASE_EXPR:
      break;
    case CEIL_MOD_EXPR:
      goto numerics;
    case CONCAT_EXPR:           /* must be static or varying char array */
      if (TREE_CODE (type) == CHAR_TYPE)
	return 0;
      if (TREE_CODE (type) == ARRAY_TYPE 
	   && TREE_CODE (TREE_TYPE (type)) == CHAR_TYPE)
	return 0;
      if (!chill_varying_type_p (type))
	  return 1;
      if (TREE_CODE (TREE_TYPE (CH_VARYING_ARRAY_TYPE (type)))
            == CHAR_TYPE)
        return 0;
      else
        return 1;
    /* note: CHILL conditional expressions (COND_EXPR) won't come
     *  through here; they're routed straight to C-specific code */
    case EQ_EXPR:
      return 0;                  /* ANYTHING can be compared equal */
    case FLOOR_MOD_EXPR:
      if (TREE_CODE (type) == REAL_TYPE)
	return 1;
      goto numerics;
    case GE_EXPR:
    case GT_EXPR:
      goto relatables;
    case SET_IN_EXPR:
      if (TREE_CODE (type) == SET_TYPE)
        return 0;
      else
        return 1;
    case PACKED_ARRAY_REF:
      if (TREE_CODE (type) == ARRAY_TYPE)
        return 0;
      else
        return 1;
    case LE_EXPR:
    case LT_EXPR:
    relatables:
      switch ((int)TREE_CODE(type))   /* right operand must be set/bitarray type */
	{
	case ARRAY_TYPE:
	  if (TREE_CODE (TREE_TYPE (type)) == CHAR_TYPE)
	    return 0;
	  else
	    return 1;
	case BOOLEAN_TYPE:
	case CHAR_TYPE:
	case COMPLEX_TYPE:
	case ENUMERAL_TYPE:
	case INTEGER_TYPE:
	case OFFSET_TYPE:
	case POINTER_TYPE:
	case REAL_TYPE:
	case SET_TYPE:
	  return 0;
	case FILE_TYPE:
	case FUNCTION_TYPE:
	case GRANT_TYPE:
	case LANG_TYPE:
	case METHOD_TYPE:
	  return 1;
	case RECORD_TYPE:
	  if (chill_varying_type_p (type)
	      && TREE_CODE (TREE_TYPE (CH_VARYING_ARRAY_TYPE (type))) == CHAR_TYPE)
	    return 0;
	  else
	    return 1;
	case REFERENCE_TYPE:
	case SEIZE_TYPE:
	case UNION_TYPE:
	case VOID_TYPE:
	  return 1;
	}
      break;
    case MINUS_EXPR:
    case MULT_EXPR:
      goto numerics;
    case NEGATE_EXPR:
      if (TREE_CODE (type) == BOOLEAN_TYPE)
        return 0;
      else
	goto numerics;
    case NE_EXPR:
      return 0;                  /* ANYTHING can be compared unequal */
    case NOP_EXPR:
      return 0;                  /* ANYTHING can be converted */
    case PLUS_EXPR:
    numerics:
      switch ((int)TREE_CODE(type))   /* left operand must be discrete type */
	{
	case ARRAY_TYPE:
	  if (right || TREE_CODE (TREE_TYPE (type)) != BOOLEAN_TYPE)
	    return 1;
	  else
	    return 0;
	case CHAR_TYPE:
	  return right;
	case BOOLEAN_TYPE:
	case COMPLEX_TYPE:
	case FILE_TYPE:
	case FUNCTION_TYPE:
	case GRANT_TYPE:
	case LANG_TYPE:
	case METHOD_TYPE:
	case RECORD_TYPE:
	case REFERENCE_TYPE:
	case SEIZE_TYPE:
	case UNION_TYPE:
	case VOID_TYPE:
	  return 1;
	case ENUMERAL_TYPE:
	case INTEGER_TYPE:
	case OFFSET_TYPE:
	case POINTER_TYPE:
	case REAL_TYPE:
	case SET_TYPE:
	  return 0;
	}
      break;
    case RANGE_EXPR:
      break;

    case REPLICATE_EXPR:
      switch ((int)TREE_CODE(type))   /* right operand must be set/bitarray type */
	{
	case COMPLEX_TYPE:
	case FILE_TYPE:
	case FUNCTION_TYPE:
	case GRANT_TYPE:
	case LANG_TYPE:
	case METHOD_TYPE:
	case OFFSET_TYPE:
	case POINTER_TYPE:
	case RECORD_TYPE:
	case REAL_TYPE:
	case SEIZE_TYPE:
	case UNION_TYPE:
	case VOID_TYPE:
	  return 1;
	case ARRAY_TYPE:
	case BOOLEAN_TYPE:
	case CHAR_TYPE:
	case ENUMERAL_TYPE:
	case INTEGER_TYPE:
	case REFERENCE_TYPE:
	case SET_TYPE:
	  return 0;
	}
      
    case TRUNC_DIV_EXPR:
      goto numerics;
    case TRUNC_MOD_EXPR:
      if (TREE_CODE (type) == REAL_TYPE)
	return 1;
      goto numerics;
    case TRUTH_ANDIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_NOT_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_OR_EXPR:
    relationals:
      switch ((int)TREE_CODE(type))   /* left operand must be discrete type */
	{
	case ARRAY_TYPE:
	case CHAR_TYPE:
	case COMPLEX_TYPE:
	case ENUMERAL_TYPE:
	case FILE_TYPE:
	case FUNCTION_TYPE:
	case GRANT_TYPE:
	case INTEGER_TYPE:
	case LANG_TYPE:
	case METHOD_TYPE:
	case OFFSET_TYPE:
	case POINTER_TYPE:
	case REAL_TYPE:
	case RECORD_TYPE:
	case REFERENCE_TYPE:
	case SEIZE_TYPE:
	case UNION_TYPE:
	case VOID_TYPE:
	  return 1;
	case BOOLEAN_TYPE:
	case SET_TYPE:
	  return 0;
	}
      break;

    default:
      return 1;       /* perhaps you forgot to add a new DEFTREECODE? */
    }
  return 1;
}


static int
invalid_right_operand (code, type)
     enum chill_tree_code code;
     tree type;
{
  return invalid_operand (code, type, 1);
}

tree
build_chill_abs (expr)
     tree expr;
{
  tree temp;

  if (TREE_CODE (TREE_TYPE (expr)) == REAL_TYPE
      || discrete_type_p (TREE_TYPE (expr)))
    temp = fold (build1 (ABS_EXPR, TREE_TYPE (expr), expr));
  else 
    {
      error("ABS argument must be discrete or real mode");
      return error_mark_node;
    }
  /* FIXME: should call
   * cond_type_range_exception (temp);
   */
  return temp;
}

static tree
build_chill_abstime (exprlist)
     tree exprlist;
{
  int  mask = 0, i, numargs;
  tree args = NULL_TREE;
  tree filename, lineno;
  int  had_errors = 0;
  tree tmp;

  if (exprlist != NULL_TREE && TREE_CODE (exprlist) == ERROR_MARK)
    return error_mark_node;

  /* check for integer expressions */
  i = 1;
  tmp = exprlist;
  while (tmp != NULL_TREE)
    {
      tree exp = TREE_VALUE (tmp);

      if (exp == NULL_TREE || TREE_CODE (exp) == ERROR_MARK)
	had_errors = 1;
      else if (TREE_CODE (TREE_TYPE (exp)) != INTEGER_TYPE)
	{
	  error ("argument %d to ABSTIME must be of integer type.", i);
	  had_errors = 1;
	}
      tmp = TREE_CHAIN (tmp);
      i++;
    }
  if (had_errors)
    return error_mark_node;

  numargs = list_length (exprlist);
  for (i = 0; i < numargs; i++)
    mask |= (1 << i);

  /* make it all arguments */
  for (i = numargs; i < 6; i++)
    exprlist = tree_cons (NULL_TREE, integer_zero_node, exprlist);

  args = tree_cons (NULL_TREE, build_int_2 (mask, 0), exprlist);

  filename = force_addr_of (get_chill_filename ());
  lineno = get_chill_linenumber ();
  args = chainon (args, tree_cons (NULL_TREE, filename,
			  tree_cons (NULL_TREE, lineno, NULL_TREE)));

  return build_chill_function_call (
    lookup_name (get_identifier ("_abstime")), args);
}


static tree
build_allocate_memory_call (ptr, size)
  tree ptr, size;
{
  int err = 0;
    
  /* check for ptr is referable */
  if (! CH_REFERABLE (ptr))
    {
      error ("parameter 1 must be referable.");
      err++;
    }
   /* check for pointer */
  else if (TREE_CODE (TREE_TYPE (ptr)) != POINTER_TYPE)
    {
      error ("mode mismatch in parameter 1.");
      err++;
    }

  /* check for size > 0 if it is a constant */
  if (TREE_CODE (size) == INTEGER_CST && TREE_INT_CST_LOW (size) <= 0)
    {
      error ("parameter 2 must be a positive integer.");
      err++;
    }
  if (err)
    return error_mark_node;

  if (TREE_TYPE (ptr) != ptr_type_node)
    ptr = build_chill_cast (ptr_type_node, ptr);

  return build_chill_function_call (
    lookup_name (get_identifier ("_allocate_memory")),
           tree_cons (NULL_TREE, ptr,
	     tree_cons (NULL_TREE, size,
	       tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
		 tree_cons (NULL_TREE, get_chill_linenumber (), 
			    NULL_TREE)))));
}


static tree
build_allocate_global_memory_call (ptr, size)
  tree ptr, size;
{
  int err = 0;
    
  /* check for ptr is referable */
  if (! CH_REFERABLE (ptr))
    {
      error ("parameter 1 must be referable.");
      err++;
    }
  /* check for pointer */
  else if (TREE_CODE (TREE_TYPE (ptr)) != POINTER_TYPE)
    {
      error ("mode mismatch in parameter 1.");
      err++;
    }

  /* check for size > 0 if it is a constant */
  if (TREE_CODE (size) == INTEGER_CST && TREE_INT_CST_LOW (size) <= 0)
    {
      error ("parameter 2 must be a positive integer.");
      err++;
    }
  if (err)
    return error_mark_node;
    
  if (TREE_TYPE (ptr) != ptr_type_node)
    ptr = build_chill_cast (ptr_type_node, ptr);

  return build_chill_function_call (
    lookup_name (get_identifier ("_allocate_global_memory")),
           tree_cons (NULL_TREE, ptr,
	     tree_cons (NULL_TREE, size,
	       tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
		 tree_cons (NULL_TREE, get_chill_linenumber (), 
			    NULL_TREE)))));
}


static tree
build_return_memory (ptr)
  tree ptr;
{
  /* check input */
  if (ptr == NULL_TREE || TREE_CODE (ptr) == ERROR_MARK)
      return error_mark_node;
  
  /* check for pointer */
  if (TREE_CODE (TREE_TYPE (ptr)) != POINTER_TYPE)
    {
      error ("mode mismatch in parameter 1.");
      return error_mark_node;
    }

  if (TREE_TYPE (ptr) != ptr_type_node)
    ptr = build_chill_cast (ptr_type_node, ptr);

  return build_chill_function_call (
    lookup_name (get_identifier ("_return_memory")),
      tree_cons (NULL_TREE, ptr,
	tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
	  tree_cons (NULL_TREE, get_chill_linenumber (), 
		     NULL_TREE))));
}


/* Compute the number of runtime members of the
 * given powerset.
 */
tree
build_chill_card (powerset)
     tree powerset;
{
  if (pass == 2)
    {
      tree temp;
      tree card_func = lookup_name (get_identifier ("__cardpowerset"));
      
      if (powerset == NULL_TREE || TREE_CODE (powerset) == ERROR_MARK)
	return error_mark_node;
      
      if (TREE_CODE (powerset) == IDENTIFIER_NODE)
	powerset = lookup_name (powerset);

      if (TREE_CODE (TREE_TYPE(powerset)) == SET_TYPE)
	{ int size;

	  /* Do constant folding, if possible. */
	  if (TREE_CODE (powerset) == CONSTRUCTOR
	      && TREE_CONSTANT (powerset)
	      && (size = int_size_in_bytes (TREE_TYPE (powerset))) >= 0)
	    {
	      int bit_size = size * BITS_PER_UNIT;
	      char* buffer = (char*) alloca (bit_size);
	      temp = get_set_constructor_bits (powerset, buffer, bit_size);
	      if (!temp)
		{ int i;
		  int count = 0;
		  for (i = 0; i < bit_size; i++)
		    if (buffer[i])
		      count++;
		  temp = build_int_2 (count, 0);
		  TREE_TYPE (temp) = TREE_TYPE (TREE_TYPE (card_func));
		  return temp;
		}
	    }
	  temp = build_chill_function_call (card_func,
		     tree_cons (NULL_TREE, force_addr_of (powerset),
		       tree_cons (NULL_TREE, powersetlen (powerset), NULL_TREE)));
	  /* FIXME: should call
	   * cond_type_range_exception (op0);
	   */
	  return temp;
	}
      error("CARD argument must be powerset mode");
      return error_mark_node;
    }
  return NULL_TREE;
}

/* function to build the type needed for the DESCR-built-in
 */

void build_chill_descr_type ()
{
  tree decl1, decl2;
  
  if (descr_type != NULL_TREE)
    /* already done */
    return;
  
  decl1 = build_decl (FIELD_DECL, get_identifier ("datap"), ptr_type_node);
  decl2 = build_decl (FIELD_DECL, get_identifier ("len"),
		      TREE_TYPE (lookup_name (
					      get_identifier ((ignore_case || ! special_UC) ? "ulong" : "ULONG"))));
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
  decl2 = build_chill_struct_type (decl1);
  descr_type = build_decl (TYPE_DECL, get_identifier ("__tmp_DESCR_type"), decl2);
  pushdecl (descr_type);
  DECL_SOURCE_LINE (descr_type) = 0;
  satisfy_decl (descr_type, 0);
}

/* build a pointer to a descriptor.
 * descriptor = STRUCT (datap PTR,
 *			len ULONG);
 * This descriptor is build in variable descr_type.
 */

tree
build_chill_descr (expr)
    tree expr;
{
  if (pass == 2)
    {
      tree tuple, decl, descr_var, datap, len, tmp;
      int is_static;

      if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
	return error_mark_node;
      
      /* check for expression is referable */
      if (! CH_REFERABLE (expr))
	{
	  error ("expression for DESCR-builtin must be referable.");
	  return error_mark_node;
	}
      
      mark_addressable (expr);
#if 0
      datap = build1 (ADDR_EXPR, build_chill_pointer_type (descr_type), expr);
#else
      datap = build_chill_arrow_expr (expr, 1);
#endif
      len = size_in_bytes (TREE_TYPE (expr));
      
      descr_var = get_unique_identifier ("DESCR");
      tuple = build_nt (CONSTRUCTOR, NULL_TREE,
			tree_cons (NULL_TREE, datap,
				   tree_cons (NULL_TREE, len, NULL_TREE)));

      is_static = (current_function_decl == global_function_decl) && TREE_STATIC (expr);
      decl = decl_temp1 (descr_var, TREE_TYPE (descr_type), is_static,
			 tuple, 0, 0);
#if 0
      tmp = force_addr_of (decl);
#else
      tmp = build_chill_arrow_expr (decl, 1);
#endif
      return tmp;
    }
  return NULL_TREE;
}

/* this function process the builtin's
   MILLISECS, SECS, MINUTES, HOURS and DAYS.
   The built duration value is in milliseconds. */

static tree
build_chill_duration (expr, multiplier, fnname, maxvalue)
     tree           expr;
     unsigned long  multiplier;
     tree           fnname;
     unsigned long  maxvalue;
{
  tree temp;

  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return error_mark_node;

  if (TREE_CODE (TREE_TYPE (expr)) != INTEGER_TYPE)
    {
      error ("argument to `%s' must be of integer type.", IDENTIFIER_POINTER (fnname));
      return error_mark_node;
    }

  temp = convert (duration_timing_type_node, expr);
  temp = fold (build (MULT_EXPR, duration_timing_type_node,
		      temp, build_int_2 (multiplier, 0)));

  if (range_checking)
    temp = check_range (temp, expr, integer_zero_node, build_int_2 (maxvalue, 0));

  return temp;
}

/* build function call to one of the floating point functions */
static tree
build_chill_floatcall (expr, chillname, funcname)
     tree expr;
     const char *chillname;
     const char *funcname;
{
  tree result;
  tree type;

  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return error_mark_node;

  /* look if expr is a REAL_TYPE */
  type = TREE_TYPE (expr);
  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (type) != REAL_TYPE)
    {
      error ("argument 1 to `%s' must be of floating point mode", chillname);
      return error_mark_node;
    }
  result = build_chill_function_call (
             lookup_name (get_identifier (funcname)),
               tree_cons (NULL_TREE, expr, NULL_TREE));
  return result;
}

/* common function for ALLOCATE and GETSTACK */
static tree
build_allocate_getstack (mode, value, chill_name, fnname, filename, linenumber)
     tree mode;
     tree value;
     const char *chill_name;
     const char *fnname;
     tree filename;
     tree linenumber;
{
  tree type, result;
  tree expr = NULL_TREE;
  tree args, tmpvar, fncall, ptr, outlist = NULL_TREE;

  if (mode == NULL_TREE || TREE_CODE (mode) == ERROR_MARK)
    return error_mark_node;

  if (TREE_CODE (mode) == TYPE_DECL)
    type = TREE_TYPE (mode);
  else
    type = mode;

  /* check if we have a mode */
  if (TREE_CODE_CLASS (TREE_CODE (type)) != 't')
    {
      error ("First argument to `%s' must be a mode", chill_name);
      return error_mark_node;
    }

  /* check if we have a value if type is READonly */
  if (TYPE_READONLY_PROPERTY (type) && value == NULL_TREE)
    {
      error ("READonly modes for %s must have a value", chill_name);
      return error_mark_node;
    }

  if (value != NULL_TREE)
    {
      if (TREE_CODE (value) == ERROR_MARK)
	return error_mark_node;
      expr = chill_convert_for_assignment (type, value, "assignment");
    }

  /* build function arguments */
  if (filename == NULL_TREE)
    args = tree_cons (NULL_TREE, size_in_bytes (type), NULL_TREE);
  else
    args = tree_cons (NULL_TREE, size_in_bytes (type),
             tree_cons (NULL_TREE, force_addr_of (filename),
               tree_cons (NULL_TREE, linenumber, NULL_TREE)));

  ptr = build_chill_pointer_type (type);
  tmpvar = decl_temp1 (get_unique_identifier (chill_name),
		       ptr, 0, NULL_TREE, 0, 0);
  fncall = build_chill_function_call (
             lookup_name (get_identifier (fnname)), args);
  outlist = tree_cons (NULL_TREE,
               build_chill_modify_expr (tmpvar, fncall), outlist);
  if (expr == NULL_TREE)
    {
      /* set allocated memory to 0 */
      fncall = build_chill_function_call (
                 lookup_name (get_identifier ("memset")),
                   tree_cons (NULL_TREE, convert (ptr_type_node, tmpvar),
                     tree_cons (NULL_TREE, integer_zero_node,
                       tree_cons (NULL_TREE, size_in_bytes (type), NULL_TREE))));
      outlist = tree_cons (NULL_TREE, fncall, outlist);
    }
  else
    {
      /* write the init value to allocated memory */
      outlist = tree_cons (NULL_TREE,
                  build_chill_modify_expr (build_chill_indirect_ref (tmpvar, NULL_TREE, 0),
					   expr), 
			   outlist);
    }
  outlist = tree_cons (NULL_TREE, tmpvar, outlist);
  result = build_chill_compound_expr (nreverse (outlist));
  return result;
}

/* process the ALLOCATE built-in */
static tree
build_chill_allocate (mode, value)
     tree mode;
     tree value;
{
  return build_allocate_getstack (mode, value, "ALLOCATE", "__allocate",
				  get_chill_filename (), get_chill_linenumber ());
}

/* process the GETSTACK built-in */
static tree
build_chill_getstack (mode, value)
     tree mode;
     tree value;
{
  return build_allocate_getstack (mode, value, "GETSTACK", "__builtin_alloca",
				  NULL_TREE, NULL_TREE);
}

/* process the TERMINATE built-in */
static tree
build_chill_terminate (ptr)
     tree ptr;
{
  tree result;
  tree type;

  if (ptr == NULL_TREE || TREE_CODE (ptr) == ERROR_MARK)
    return error_mark_node;

  type = TREE_TYPE (ptr);
  if (type == NULL_TREE || TREE_CODE (type) != POINTER_TYPE)
    {
      error ("argument to TERMINATE must be a reference primitive value");
      return error_mark_node;
    }
  result = build_chill_function_call (
	     lookup_name (get_identifier ("__terminate")),
	       tree_cons (NULL_TREE, convert (ptr_type_node, ptr),
                 tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                   tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  return result;
}

/* build the type passed to _inttime function */
void
build_chill_inttime_type ()
{
  tree idxlist;
  tree arrtype;
  tree decl;

  idxlist = build_tree_list (NULL_TREE,
               build_chill_range_type (NULL_TREE,
				       integer_zero_node,
				       build_int_2 (5, 0)));
  arrtype = build_chill_array_type (ptr_type_node, idxlist, 0, NULL_TREE);

  decl = build_decl (TYPE_DECL, get_identifier ("__tmp_INTTIME_type"), arrtype);
  pushdecl (decl);
  DECL_SOURCE_LINE (decl) = 0;
  satisfy_decl (decl, 0);
}

static tree
build_chill_inttime (t, loclist)
     tree t, loclist;
{
  int  had_errors = 0, cnt;
  tree tmp;
  tree init = NULL_TREE;
  int  numargs;
  tree tuple, var;

  if (t == NULL_TREE || TREE_CODE (t) == ERROR_MARK)
    return error_mark_node;
  if (loclist == NULL_TREE || TREE_CODE (loclist) == ERROR_MARK)
    return error_mark_node;

  /* check first argument to be NEWMODE TIME */
  if (TREE_TYPE (t) != abs_timing_type_node)
    {
      error ("argument 1 to INTTIME must be of mode TIME.");
      had_errors = 1;
    }

  cnt = 2;
  tmp = loclist;
  while (tmp != NULL_TREE)
    {
      tree loc = TREE_VALUE (tmp);
      char errmsg[200];
      char *p, *p1;
      int  write_error = 0;

      sprintf (errmsg, "argument %d to INTTIME must be ", cnt);
      p = errmsg + strlen (errmsg);
      p1 = p;
      
      if (loc == NULL_TREE || TREE_CODE (loc) == ERROR_MARK)
	had_errors = 1;
      else
	{
	  if (! CH_REFERABLE (loc))
	    {
	      strcpy (p, "referable");
	      p += strlen (p);
	      write_error = 1;
	      had_errors = 1;
	    }
	  if (TREE_CODE (TREE_TYPE (loc)) != INTEGER_TYPE)
	    {
	      if (p != p1)
		{
		  strcpy (p, " and ");
		  p += strlen (p);
		}
	      strcpy (p, "of integer type");
	      write_error = 1;
	      had_errors = 1;
	    }
	  /* FIXME: what's about ranges can't hold the result ?? */
	  if (write_error)
	    error ("%s.", errmsg);
	}
      /* next location */
      tmp = TREE_CHAIN (tmp);
      cnt++;
    }

  if (had_errors)
    return error_mark_node;

  /* make it always 6 arguments */
  numargs = list_length (loclist);
  for (cnt = numargs; cnt < 6; cnt++)
    init = tree_cons (NULL_TREE, null_pointer_node, init);

  /* append the given one's */
  tmp = loclist;
  while (tmp != NULL_TREE)
    {
      init = chainon (init,
		      build_tree_list (NULL_TREE,
				       build_chill_descr (TREE_VALUE (tmp))));
      tmp = TREE_CHAIN (tmp);
    }

  tuple = build_nt (CONSTRUCTOR, NULL_TREE, init);
  var = decl_temp1 (get_unique_identifier ("INTTIME"),
		    TREE_TYPE (lookup_name (get_identifier ("__tmp_INTTIME_type"))),
		    0, tuple, 0, 0);

  return build_chill_function_call (
    lookup_name (get_identifier ("_inttime")),
       tree_cons (NULL_TREE, t,
          tree_cons (NULL_TREE, force_addr_of (var),
		     NULL_TREE)));
}


/* Compute the runtime length of the given string variable
 * or expression.
 */
tree
build_chill_length (expr)
     tree expr;
{
  if (pass == 2)
    {
      tree type;
      
      if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
	return error_mark_node;
      
      if (TREE_CODE (expr) == IDENTIFIER_NODE)
	expr = lookup_name (expr);

      type = TREE_TYPE (expr);
      
      if (TREE_CODE(type) == ERROR_MARK)
	return type;
      if (chill_varying_type_p (type))
	{ 
	  tree temp = convert (integer_type_node,
			  build_component_ref (expr, var_length_id));
	  /* FIXME: should call
	   * cond_type_range_exception (temp);
	   */
	  return temp;
	}
      
      if ((TREE_CODE (type) == ARRAY_TYPE ||
	   /* should work for a bitstring too */
	   (TREE_CODE (type) == SET_TYPE && TREE_CODE (TREE_TYPE (type)) == BOOLEAN_TYPE)) &&
	  integer_zerop (TYPE_MIN_VALUE (TYPE_DOMAIN (type))))
	{
	  tree temp =  fold (build (PLUS_EXPR, chill_integer_type_node,
				    integer_one_node,
				    TYPE_MAX_VALUE (TYPE_DOMAIN (type))));
	  return convert (chill_integer_type_node, temp);
	}
      
      if (CH_IS_BUFFER_MODE (type) || CH_IS_EVENT_MODE (type))
        {
          tree len = max_queue_size (type);
          
          if (len == NULL_TREE)
            len = integer_minus_one_node;
          return len;
        }

      if (CH_IS_TEXT_MODE (type))
	{
	  if (TREE_CODE (expr) == TYPE_DECL)
	    {
	      /* text mode name */
	      return text_length (type);
	    }
	  else
	    {
	      /* text location */
	      tree temp = build_component_ref (
			    build_component_ref (expr, get_identifier ("tloc")),
                                var_length_id);
	      return convert (integer_type_node, temp);
	    }
	}
 
      error("LENGTH argument must be string, buffer, event mode, text location or mode");
      return error_mark_node;
    }
  return NULL_TREE;
}

/* Compute the declared minimum/maximum value of the variable,
 * expression or declared type
 */
static tree
build_chill_lower_or_upper (what, is_upper)
     tree what;
     int is_upper;  /* o -> LOWER; 1 -> UPPER */
{
  if (pass == 2)
    {
      tree type;
      struct ch_class class;

      if (what == NULL_TREE || TREE_CODE (what) == ERROR_MARK)
	return error_mark_node;
      
      if (TREE_CODE_CLASS (TREE_CODE (what)) == 't')
	type = what;
      else
	type = TREE_TYPE (what);
      if (type == NULL_TREE)
	{
	  if (is_upper)
	    error ("UPPER argument must have a mode, or be a mode");
	  else
	    error ("LOWER argument must have a mode, or be a mode");
	  return error_mark_node;
	}
      while (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);
      if (chill_varying_type_p (type))
	type = CH_VARYING_ARRAY_TYPE (type);
     
      if (discrete_type_p (type))
	{
	  tree val = is_upper ? TYPE_MAX_VALUE (type) : TYPE_MIN_VALUE (type);
	  class.kind = CH_VALUE_CLASS;
	  class.mode = type;
	  return convert_to_class (class, val);
	}
      else if (TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == SET_TYPE)
	{
	  if (TYPE_STRING_FLAG (type))
	    {
	      class.kind = CH_DERIVED_CLASS;
	      class.mode = integer_type_node;
	    }
	  else
	    {
	      class.kind = CH_VALUE_CLASS;
	      class.mode = TYPE_DOMAIN (type);
	    }
	  type = TYPE_DOMAIN (type);
	  return convert_to_class (class,
				   is_upper
				   ? TYPE_MAX_VALUE (type)
				   : TYPE_MIN_VALUE (type));
	}
      if (is_upper)
	error("UPPER argument must be string, array, mode or integer");
      else
	error("LOWER argument must be string, array, mode or integer");
      return error_mark_node;
    }
  return NULL_TREE;
}

tree
build_chill_lower (what)
     tree what;
{
  return build_chill_lower_or_upper (what, 0);
}

static tree
build_max_min (expr, max_min)
     tree expr;
     int max_min; /* 0: calculate MIN; 1: calculate MAX */
{
  if (pass == 2)
    {
      tree type, temp, setminval;
      tree set_base_type;
      int size_in_bytes;
      
      if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
	return error_mark_node;
      
      if (TREE_CODE (expr) == IDENTIFIER_NODE)
	expr = lookup_name (expr);

      type = TREE_TYPE (expr);
      set_base_type = TYPE_DOMAIN (type);
      setminval = TYPE_MIN_VALUE (set_base_type);
      
      if (TREE_CODE (type) != SET_TYPE)
	{
	  error("%s argument must be POWERSET mode",
		max_min ? "MAX" : "MIN");
	  return error_mark_node;
	}

      /* find max/min of constant powerset at compile time */
      if (TREE_CODE (expr) == CONSTRUCTOR && TREE_CONSTANT (expr)
	  && (size_in_bytes = int_size_in_bytes (type)) >= 0)
	{
	  HOST_WIDE_INT min_val = -1, max_val = -1;
	  HOST_WIDE_INT i, i_hi = 0;
	  HOST_WIDE_INT size_in_bits = size_in_bytes * BITS_PER_UNIT;
	  char *buffer = (char*) alloca (size_in_bits);
	  if (buffer == NULL
	      || get_set_constructor_bits (expr, buffer, size_in_bits))
	    abort ();
	  for (i = 0; i < size_in_bits; i++)
	    {
	      if (buffer[i])
		{
		  if (min_val < 0)
		    min_val = i;
		  max_val = i;
		}
	    }
	  if (min_val < 0)
	    error ("%s called for empty POWERSET", max_min ? "MAX" : "MIN");
	  i = max_min ? max_val : min_val;
	  temp = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (expr)));
	  add_double (i, i_hi,
		      TREE_INT_CST_LOW (temp), TREE_INT_CST_HIGH (temp),
		      &i, &i_hi);
	  temp = build_int_2 (i, i_hi);
	  TREE_TYPE (temp) = set_base_type;
	  return temp;
	}
      else
	{
	  tree parmlist, filename, lineno;
	  const char *funcname;
	  
	  /* set up to call appropriate runtime function */
	  if (max_min)
	    funcname = "__flsetpowerset";
	  else
	    funcname = "__ffsetpowerset";
	  
	  setminval = convert (long_integer_type_node, setminval);
	  filename = force_addr_of (get_chill_filename());
	  lineno = get_chill_linenumber();
	  parmlist = tree_cons (NULL_TREE, force_addr_of (expr),
		       tree_cons (NULL_TREE, powersetlen (expr),
			 tree_cons (NULL_TREE, setminval,
			   tree_cons (NULL_TREE, filename,
			     build_tree_list (NULL_TREE, lineno)))));
	  temp = lookup_name (get_identifier (funcname));
	  temp = build_chill_function_call (temp, parmlist);
	  TREE_TYPE (temp) = set_base_type;
	  return temp;
	}
    }
  return NULL_TREE;
}


/* Compute the current runtime maximum value of the powerset
 */
tree
build_chill_max (expr)
     tree expr;
{
  return build_max_min (expr, 1);
}


/* Compute the current runtime minimum value of the powerset
 */
tree
build_chill_min (expr)
     tree expr;
{
  return build_max_min (expr, 0);
}


/* Build a conversion from the given expression to an INT,
 * but only when the expression's type is the same size as
 * an INT.
 */
tree
build_chill_num (expr)
     tree expr;
{
  if (pass == 2)
    {
      tree temp;
      int need_unsigned;

      if (expr == NULL_TREE || TREE_CODE(expr) == ERROR_MARK)
	return error_mark_node;
      
      if (TREE_CODE (expr) == IDENTIFIER_NODE)
	expr = lookup_name (expr);

      expr = convert_to_discrete (expr);
      if (expr == NULL_TREE)
	{
	  error ("argument to NUM is not discrete");
	  return error_mark_node;
	}

      /* enumeral types and string slices of length 1 must be kept unsigned */
      need_unsigned = (TREE_CODE (TREE_TYPE (expr)) == ENUMERAL_TYPE)
	|| TREE_UNSIGNED (TREE_TYPE (expr));

      temp = type_for_size (TYPE_PRECISION (TREE_TYPE (expr)), 
			    need_unsigned);
      if (temp == NULL_TREE)
	{
	  error ("No integer mode which matches expression's mode");
	  return integer_zero_node;
	}
      temp = convert (temp, expr);

      if (TREE_CONSTANT (temp))
	{
	  if (tree_int_cst_lt (temp, 
			       TYPE_MIN_VALUE (TREE_TYPE (temp))))
	    error ("NUM's parameter is below its mode range");
	  if (tree_int_cst_lt (TYPE_MAX_VALUE (TREE_TYPE (temp)),
	                       temp))
	    error ("NUM's parameter is above its mode range");
	}
#if 0
      else
	{
	  if (range_checking)
	    cond_overflow_exception (temp, 
	      TYPE_MIN_VALUE (TREE_TYPE (temp)),
	      TYPE_MAX_VALUE (TREE_TYPE (temp)));
	}
#endif

      /* NUM delivers the INT derived class */
      CH_DERIVED_FLAG (temp) = 1;
      
      return temp;
    }
  return NULL_TREE;
}


static tree
build_chill_pred_or_succ (expr, op)
     tree expr;
     enum tree_code op; /* PLUS_EXPR for SUCC; MINUS_EXPR for PRED. */
{
  struct ch_class class;
  tree etype, cond;

  if (pass == 1)
    return NULL_TREE;

  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return error_mark_node;
  
  /* disallow numbered SETs */
  if (TREE_CODE (TREE_TYPE (expr)) == ENUMERAL_TYPE
      && CH_ENUM_IS_NUMBERED (TREE_TYPE (expr)))
    {
      error ("Cannot take SUCC or PRED of a numbered SET");
      return error_mark_node;
    }
  
  if (TREE_CODE (TREE_TYPE (expr)) == POINTER_TYPE)
    {
      if (TREE_TYPE (TREE_TYPE (expr)) == void_type_node)
	{
	  error ("SUCC or PRED must not be done on a PTR.");
	  return error_mark_node;
	}
      pedwarn ("SUCC or PRED for a reference type is not standard.");
      return fold (build (op, TREE_TYPE (expr),
			  expr,
			  size_in_bytes (TREE_TYPE (TREE_TYPE (expr)))));
    }

  expr = convert_to_discrete (expr);

  if (expr == NULL_TREE)
    {
      error ("SUCC or PRED argument must be a discrete mode");
      return error_mark_node;
    }

  class = chill_expr_class (expr);
  if (class.mode)
    class.mode = CH_ROOT_MODE (class.mode);
  etype = class.mode;
  expr = convert (etype, expr);

  /* Exception if expression is already at the
     min (PRED)/max(SUCC) valid value for its type. */
  cond = fold (build (op == PLUS_EXPR ? GE_EXPR : LE_EXPR,
		      boolean_type_node,
		      expr,
		      convert (etype,
			       op == PLUS_EXPR ? TYPE_MAX_VALUE (etype)
			       : TYPE_MIN_VALUE (etype))));
  if (TREE_CODE (cond) == INTEGER_CST
      && tree_int_cst_equal (cond, integer_one_node))
    {
      error ("Taking the %s of a value already at its %s value",
	     op == PLUS_EXPR ? "SUCC" : "PRED",
	     op == PLUS_EXPR ? "maximum" : "minimum");
      return error_mark_node;
    }

  if (range_checking)
    expr = check_expression (expr, cond,
			     ridpointers[(int) RID_OVERFLOW]);

  expr = fold (build (op, etype, expr, 
	   convert (etype, integer_one_node)));
  return convert_to_class (class, expr);
}

/* Compute the value of the CHILL `size' operator just
 * like the C 'sizeof' operator (code stolen from c-typeck.c)
 * TYPE may be a location or mode tree.  In pass 1, we build
 * a function-call syntax tree;  in pass 2, we evaluate it.
 */
tree
build_chill_sizeof (type)
     tree type;
{
  if (pass == 2)
    {
      tree temp;
      struct ch_class class;
      enum tree_code code;
      tree signame = NULL_TREE;

      if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
	return error_mark_node;

      if (TREE_CODE (type) == IDENTIFIER_NODE)
	type = lookup_name (type);

      code = TREE_CODE (type);
      if (code == ERROR_MARK)
	return error_mark_node;
      
      if (TREE_CODE_CLASS (TREE_CODE (type)) != 't')
	{
	  if (TREE_CODE (type) == TYPE_DECL && CH_DECL_SIGNAL (type))
	    signame = DECL_NAME (type);
	type = TREE_TYPE (type);
	}

      if (code == FUNCTION_TYPE)
	{
	  if (pedantic || warn_pointer_arith)
	    pedwarn ("size applied to a function mode");
	  return error_mark_node;
	}
      if (code == VOID_TYPE)
	{
	  if (pedantic || warn_pointer_arith)
	    pedwarn ("sizeof applied to a void mode");
	  return error_mark_node;
	}
      if (TYPE_SIZE (type) == 0)
	{
	  error ("sizeof applied to an incomplete mode");
	  return error_mark_node;
	}
      
      temp = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type),
			 size_int (TYPE_PRECISION (char_type_node)));
      if (signame != NULL_TREE)
        {
          /* we have a signal definition. This signal may have no
             data items specified. The definition however says that
             there are data, cause we cannot build a structure without
             fields. In this case return 0. */
          if (IDENTIFIER_SIGNAL_DATA (signame) == 0)
            temp = integer_zero_node;
        }
      
      /* FIXME: should call
       * cond_type_range_exception (temp);
       */
      class.kind = CH_DERIVED_CLASS;
      class.mode = integer_type_node;
      return convert_to_class (class, temp);
    }
  return NULL_TREE;
}

/* Compute the declared maximum value of the variable,
 * expression or declared type
 */
tree
build_chill_upper (what)
     tree what;
{
  return build_chill_lower_or_upper (what, 1);
}

/*
 * Here at the site of a function/procedure call..  We need to build
 * temps for the INOUT and OUT parameters, and copy the actual parameters
 * into the temps.  After the call, we 'copy back' the values from the
 * temps to the actual parameter variables.  This somewhat verbose pol-
 * icy meets the requirement that the actual parameters are undisturbed
 * if the function/procedure causes an exception.  They are updated only
 * upon a normal return from the function.
 *
 * Note: the expr_list, which collects all of the above assignments, etc,
 * is built in REVERSE execution order.  The list is corrected by nreverse
 * inside the build_chill_compound_expr call.
 */
tree
build_chill_function_call (function, expr)
     tree function, expr;
{
  register tree typetail, valtail, typelist;
  register tree temp, actual_args = NULL_TREE;
  tree name = NULL_TREE;
  tree function_call;
  tree fntype;
  int parmno = 1;            /* parameter number for error message */
  int callee_raise_exception = 0;

  /* list of assignments to run after the actual call,
     copying from the temps back to the user's variables. */
  tree copy_back = NULL_TREE;

  /* list of expressions to run before the call, copying from
     the user's variable to the temps that are passed to the function */
  tree expr_list = NULL_TREE;
 
  if (function == NULL_TREE || TREE_CODE (function) == ERROR_MARK)
    return error_mark_node;

  if (expr != NULL_TREE && TREE_CODE (expr) == ERROR_MARK)
    return error_mark_node;

  if (pass < 2)
    return error_mark_node;

  fntype = TREE_TYPE (function);
  if (TREE_CODE (function) == FUNCTION_DECL)
    {
      callee_raise_exception = TYPE_RAISES_EXCEPTIONS (fntype) != NULL_TREE;

      /* Differs from default_conversion by not setting TREE_ADDRESSABLE
	 (because calling an inline function does not mean the function
	 needs to be separately compiled).  */
      fntype = build_type_variant (fntype,
				   TREE_READONLY (function),
				   TREE_THIS_VOLATILE (function));
      name = DECL_NAME (function);

      /* check that function is not a PROCESS */
      if (CH_DECL_PROCESS (function))
	{
	  error ("cannot call a PROCESS, you START a PROCESS");
	  return error_mark_node;
	}

      function = build1 (ADDR_EXPR, build_pointer_type (fntype), function);
    }
  else if (TREE_CODE (fntype) == POINTER_TYPE)
    {
      fntype = TREE_TYPE (fntype);
      callee_raise_exception = TYPE_RAISES_EXCEPTIONS (fntype) != NULL_TREE;

      /* Z.200 6.7 Call Action:
	 "A procedure call causes the EMPTY exception if the
	 procedure primitive value delivers NULL. */
      if (TREE_CODE (function) != ADDR_EXPR
	  || TREE_CODE (TREE_OPERAND (function, 0)) != FUNCTION_DECL)
	function = check_non_null (function);
    }

  typelist = TYPE_ARG_TYPES (fntype);
  if (callee_raise_exception)
    {
      /* remove last two arguments from list for subsequent checking.
	  They will get added automatically after checking */
      int len = list_length (typelist);
      int i;
      tree newtypelist = NULL_TREE;
      tree wrk = typelist;
      
      for (i = 0; i < len - 3; i++)
	{
	    newtypelist = tree_cons (TREE_PURPOSE (wrk), TREE_VALUE (wrk), newtypelist);
	      wrk = TREE_CHAIN (wrk);
	  }
      /* add the void_type_node */
      newtypelist = tree_cons (NULL_TREE, void_type_node, newtypelist);
      typelist = nreverse (newtypelist);
    }

  /* Scan the given expressions and types, producing individual
     converted arguments and pushing them on ACTUAL_ARGS in 
     reverse order.  */
  for (valtail = expr, typetail = typelist;
       valtail != NULL_TREE && typetail != NULL_TREE;  parmno++,
       valtail = TREE_CHAIN (valtail), typetail = TREE_CHAIN (typetail))
    {
      register tree actual = TREE_VALUE (valtail);
      register tree attr   = TREE_PURPOSE (typetail)
	? TREE_PURPOSE (typetail) : ridpointers[(int) RID_IN];
      register tree type   = TREE_VALUE (typetail);
      char place[30];
      sprintf (place, "parameter %d", parmno);
 	  
      /* if we have reached void_type_node in typelist we are at the
	  end of formal parameters and then we have too many actual
	   parameters */
      if (type == void_type_node)
	 break;

      /* check if actual is a TYPE_DECL. FIXME: what else ? */
      if (TREE_CODE (actual) == TYPE_DECL)
	{
	  error ("invalid %s", place);
	  actual = error_mark_node;
	}
      /* INOUT or OUT param to handle? */
      else if (attr == ridpointers[(int) RID_OUT]
	  || attr == ridpointers[(int)RID_INOUT])
	{
	  char temp_name[20]; 
	  tree parmtmp;
	  tree in_actual = NULL_TREE, out_actual;

	  /* actual parameter must be a location so we can
	     build a reference to it */
	  if (!CH_LOCATION_P (actual))
	    {
	      error ("%s parameter %d must be a location", 
		     (attr == ridpointers[(int) RID_OUT]) ?
		     "OUT" : "INOUT", parmno);
	      continue;
	    }
	  if (TYPE_READONLY_PROPERTY (TREE_TYPE (actual))
	      || TREE_READONLY (actual))
	    {
	      error ("%s parameter %d is READ-only", 
		     (attr == ridpointers[(int) RID_OUT]) ?
		     "OUT" : "INOUT", parmno);
	      continue;
	    }

	  sprintf (temp_name, "PARM_%d_%s",  parmno,
		   (attr == ridpointers[(int)RID_OUT]) ?
		   "OUT" : "INOUT");
	  parmtmp = decl_temp1 (get_unique_identifier (temp_name),
				TREE_TYPE (type), 0, NULL_TREE, 0, 0);
	  /* this temp *must not* be optimized into a register */
	  mark_addressable (parmtmp);

	  if (attr == ridpointers[(int)RID_INOUT])
	    {
	      tree in_actual = chill_convert_for_assignment (TREE_TYPE (type),
							     actual, place);
	      tree tmp = build_chill_modify_expr (parmtmp, in_actual);
	      expr_list = tree_cons (NULL_TREE, tmp, expr_list);
	    }
	  if (in_actual != error_mark_node)
	    {
	      /* list of copy back assignments to perform, from the temp
		 back to the actual parameter */
	      out_actual = chill_convert_for_assignment (TREE_TYPE (actual),
							 parmtmp, place);
	      copy_back = tree_cons (NULL_TREE,
				     build_chill_modify_expr (actual,
							      out_actual),
				     copy_back);
	    }
	  /* we can do this because build_chill_function_type
	     turned these parameters into REFERENCE_TYPEs. */
	  actual = build1 (ADDR_EXPR, type, parmtmp);
	}
      else if (attr == ridpointers[(int) RID_LOC])
	{
	  int is_location = chill_location (actual);
	  if (is_location)
	    {
	      if (is_location == 1)
		{
		  error ("LOC actual parameter %d is a non-referable location",
			 parmno);
		  actual = error_mark_node;
		}
	      else if (! CH_READ_COMPATIBLE (type, TREE_TYPE (actual)))
		{
		  error ("mode mismatch in parameter %d", parmno);
		  actual = error_mark_node;
		}
	      else
		actual = convert (type, actual);
	    }
	  else
	    {
	      sprintf (place, "parameter_%d", parmno);
	      actual = decl_temp1 (get_identifier (place),
				   TREE_TYPE (type), 0, actual, 0, 0);
	      actual = convert (type, actual);
	    }
	  mark_addressable (actual);
	}
      else
	actual = chill_convert_for_assignment (type, actual, place);

      actual_args = tree_cons (NULL_TREE, actual, actual_args);
    }
 
  if (valtail != 0 && TREE_VALUE (valtail) != void_type_node)
    {
      if (name)
	error ("too many arguments to procedure `%s'",
	       IDENTIFIER_POINTER (name));
      else
	error ("too many arguments to procedure");
      return error_mark_node;
    }
  else if (typetail != 0 && TREE_VALUE (typetail) != void_type_node)
    {
      if (name)
	error ("too few arguments to procedure `%s'",
	       IDENTIFIER_POINTER (name));
      else
	error ("too few arguments to procedure");
      return error_mark_node;
    }
  
  if (callee_raise_exception)
    {
      /* add linenumber and filename of the caller as arguments */
      actual_args = tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
			              actual_args);
      actual_args = tree_cons (NULL_TREE, get_chill_linenumber (), actual_args);
    }
  
  function_call = build (CALL_EXPR, TREE_TYPE (fntype),
			  function, nreverse (actual_args), NULL_TREE);
  TREE_SIDE_EFFECTS (function_call) = 1;

  if (copy_back == NULL_TREE && expr_list == NULL_TREE)
    return function_call;        /* no copying to do, either way */
  else
    {
      tree result_type = TREE_TYPE (fntype);
      tree result_tmp = NULL_TREE;

      /* no result wanted from procedure call */
      if (result_type == NULL_TREE || result_type == void_type_node)
	expr_list = tree_cons (NULL_TREE, function_call, expr_list);
      else
	{
	  /* create a temp for the function's result. this is so that we can
	     evaluate this temp as the last expression in the list, which will
	     make the function's return value the value of the whole list of
	     expressions (by the C rules for compound expressions) */
	  result_tmp = decl_temp1 (get_unique_identifier ("FUNC_RESULT"),
				   result_type, 0, NULL_TREE, 0, 0);
	  expr_list = tree_cons (NULL_TREE, 
	         	build_chill_modify_expr (result_tmp, function_call),
				 expr_list);
	}

      expr_list = chainon (copy_back, expr_list);

      /* last, but not least, the function's result */
      if (result_tmp != NULL_TREE)
	expr_list = tree_cons (NULL_TREE, result_tmp, expr_list);
      temp = build_chill_compound_expr (nreverse (expr_list));
      return temp;
    }
}

/* We saw something that looks like a function call,
   but if it's pass 1, we're not sure. */

tree
build_generalized_call (func, args)
     tree func, args;
{
  tree type = TREE_TYPE (func);

  if (pass == 1)
    return build (CALL_EXPR, NULL_TREE, func, args, NULL_TREE);

  /* Handle string repetition */
  if (TREE_CODE (func) == INTEGER_CST)
    {
      if (args == NULL_TREE || TREE_CHAIN (args) != NULL_TREE)
	{
	  error ("syntax error (integer used as function)");
	  return error_mark_node;
	}
      if (TREE_CODE (args) == TREE_LIST)
	args = TREE_VALUE (args);
      return build_chill_repetition_op (func, args);
    }

  if (args != NULL_TREE)
    {
      if (TREE_CODE (args) == RANGE_EXPR)
	{
	  tree lo = TREE_OPERAND (args, 0), hi = TREE_OPERAND (args, 1);
	  if (TREE_CODE_CLASS (TREE_CODE (func)) == 't')
	    return build_chill_range_type (func, lo, hi);
	  else
	    return build_chill_slice_with_range (func, lo, hi);
	}
      else if (TREE_CODE (args) != TREE_LIST)
	{
	  error ("syntax error - missing operator, comma, or '('?");
	  return error_mark_node;
	}
    }

  if (TREE_CODE (func) == TYPE_DECL)
    {
      if (CH_DECL_SIGNAL (func))
	return build_signal_descriptor (func, args);
      func = TREE_TYPE (func);
    }

  if (TREE_CODE_CLASS (TREE_CODE (func)) == 't'
      && args != NULL_TREE && TREE_CHAIN (args) == NULL_TREE)
    return build_chill_cast (func, TREE_VALUE (args));

  if (TREE_CODE (type) == FUNCTION_TYPE
      || (TREE_CODE (type) == POINTER_TYPE
	  && TREE_TYPE (type) != NULL_TREE
	  && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE))
    {
      /* Check for a built-in Chill function.  */
      if (TREE_CODE (func) == FUNCTION_DECL
	  && DECL_BUILT_IN (func)
	  && DECL_FUNCTION_CODE (func) > END_BUILTINS)
	{
	  tree fnname = DECL_NAME (func);
	  switch ((enum chill_built_in_function)DECL_FUNCTION_CODE (func))
	    {
	    case BUILT_IN_CH_ABS:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_abs (TREE_VALUE (args));
	    case BUILT_IN_ABSTIME:
	      if (check_arglist_length (args, 0, 6, fnname) < 0)
		return error_mark_node;
	      return build_chill_abstime (args);
	    case BUILT_IN_ADDR:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
#if 0
	      return build_chill_addr_expr (TREE_VALUE (args), (char *)0);
#else
	      return build_chill_arrow_expr (TREE_VALUE (args), 0);
#endif
	    case BUILT_IN_ALLOCATE_GLOBAL_MEMORY:
	      if (check_arglist_length (args, 2, 2, fnname) < 0)
		return error_mark_node;
	      return build_allocate_global_memory_call
		(TREE_VALUE (args),
		 TREE_VALUE (TREE_CHAIN (args)));
	    case BUILT_IN_ALLOCATE:
	      if (check_arglist_length (args, 1, 2, fnname) < 0)
		return error_mark_node;
	      return build_chill_allocate (TREE_VALUE (args),
                       TREE_CHAIN (args) == NULL_TREE ? NULL_TREE : TREE_VALUE (TREE_CHAIN (args)));
	    case BUILT_IN_ALLOCATE_MEMORY:
	      if (check_arglist_length (args, 2, 2, fnname) < 0)
		return error_mark_node;
	      return build_allocate_memory_call
		(TREE_VALUE (args),
		 TREE_VALUE (TREE_CHAIN (args)));
	    case BUILT_IN_ASSOCIATE:
	      if (check_arglist_length (args, 2, 3, fnname) < 0)
		return error_mark_node;
	      return build_chill_associate
		(TREE_VALUE (args),
		 TREE_VALUE (TREE_CHAIN (args)),
		 TREE_CHAIN (TREE_CHAIN (args)));
	    case BUILT_IN_ARCCOS:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__acos");
	    case BUILT_IN_ARCSIN:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__asin");
	    case BUILT_IN_ARCTAN:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__atan");
	    case BUILT_IN_CARD:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_card (TREE_VALUE (args));
	    case BUILT_IN_CONNECT:
	      if (check_arglist_length (args, 3, 5, fnname) < 0)
		return error_mark_node;
	      return build_chill_connect 
		(TREE_VALUE (args),
		 TREE_VALUE (TREE_CHAIN (args)),
		 TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args))),
		 TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (args))));
	    case BUILT_IN_COPY_NUMBER:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_copy_number (TREE_VALUE (args));
	    case BUILT_IN_CH_COS:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__cos");
	    case BUILT_IN_CREATE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_create (TREE_VALUE (args));
	    case BUILT_IN_DAYS:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_duration (TREE_VALUE (args), DAYS_MULTIPLIER,
					   fnname, DAYS_MAX);
	    case BUILT_IN_CH_DELETE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_delete (TREE_VALUE (args));
	    case BUILT_IN_DESCR:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_descr (TREE_VALUE (args));
	    case BUILT_IN_DISCONNECT:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_disconnect (TREE_VALUE (args));
	    case BUILT_IN_DISSOCIATE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_dissociate (TREE_VALUE (args));
	    case BUILT_IN_EOLN:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_eoln (TREE_VALUE (args));
	    case BUILT_IN_EXISTING:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_existing (TREE_VALUE (args));
	    case BUILT_IN_EXP:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__exp");
	    case BUILT_IN_GEN_CODE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_gen_code (TREE_VALUE (args));
	    case BUILT_IN_GEN_INST:
	      if (check_arglist_length (args, 2, 2, fnname) < 0)
		return error_mark_node;
	      return build_gen_inst (TREE_VALUE (args),
		 TREE_VALUE (TREE_CHAIN (args)));
	    case BUILT_IN_GEN_PTYPE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_gen_ptype (TREE_VALUE (args));
	    case BUILT_IN_GETASSOCIATION:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_getassociation (TREE_VALUE (args));
	    case BUILT_IN_GETSTACK:
	      if (check_arglist_length (args, 1, 2, fnname) < 0)
		return error_mark_node;
	      return build_chill_getstack (TREE_VALUE (args),
		       TREE_CHAIN (args) == NULL_TREE ? NULL_TREE : TREE_VALUE (TREE_CHAIN (args)));
	    case BUILT_IN_GETTEXTACCESS:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_gettextaccess (TREE_VALUE (args));
	    case BUILT_IN_GETTEXTINDEX:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_gettextindex (TREE_VALUE (args));
	    case BUILT_IN_GETTEXTRECORD:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_gettextrecord (TREE_VALUE (args));
	    case BUILT_IN_GETUSAGE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_getusage (TREE_VALUE (args));
	    case BUILT_IN_HOURS:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_duration (TREE_VALUE (args), HOURS_MULTIPLIER,
					   fnname, HOURS_MAX);
	    case BUILT_IN_INDEXABLE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_indexable (TREE_VALUE (args));
	    case BUILT_IN_INTTIME:
	      if (check_arglist_length (args, 2, 7, fnname) < 0)
		return error_mark_node;
	      return build_chill_inttime (TREE_VALUE (args),
		 TREE_CHAIN (args));
	    case BUILT_IN_ISASSOCIATED:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_isassociated (TREE_VALUE (args));
	    case BUILT_IN_LENGTH:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_length (TREE_VALUE (args));
	    case BUILT_IN_LN:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__log");
	    case BUILT_IN_LOG:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__log10");
	    case BUILT_IN_LOWER:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_lower (TREE_VALUE (args));
	    case BUILT_IN_MAX:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_max (TREE_VALUE (args));
	    case BUILT_IN_MILLISECS:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_duration (TREE_VALUE (args), MILLISECS_MULTIPLIER,
					   fnname, MILLISECS_MAX);
	    case BUILT_IN_MIN:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_min (TREE_VALUE (args));
	    case BUILT_IN_MINUTES:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_duration (TREE_VALUE (args), MINUTES_MULTIPLIER,
					   fnname, MINUTES_MAX);
	    case BUILT_IN_MODIFY:
	      if (check_arglist_length (args, 1, -1, fnname) < 0)
		return error_mark_node;
	      return build_chill_modify (TREE_VALUE (args), TREE_CHAIN (args));
	    case BUILT_IN_NUM:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_num (TREE_VALUE (args));
	    case BUILT_IN_OUTOFFILE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_outoffile (TREE_VALUE (args));
	    case BUILT_IN_PRED:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_pred_or_succ (TREE_VALUE (args), MINUS_EXPR);
	    case BUILT_IN_PROC_TYPE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_proc_type (TREE_VALUE (args));
	    case BUILT_IN_QUEUE_LENGTH:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_queue_length (TREE_VALUE (args));
	    case BUILT_IN_READABLE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_readable (TREE_VALUE (args));
	    case BUILT_IN_READRECORD:
	      if (check_arglist_length (args, 1, 3, fnname) < 0)
		return error_mark_node;
	      return build_chill_readrecord (TREE_VALUE (args), TREE_CHAIN (args));
	    case BUILT_IN_READTEXT:
	      if (check_arglist_length (args, 2, -1, fnname) < 0)
		return error_mark_node;
	      return build_chill_readtext (TREE_VALUE (args),
					   TREE_CHAIN (args));
	    case BUILT_IN_RETURN_MEMORY:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_return_memory (TREE_VALUE (args));
	    case BUILT_IN_SECS:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_duration (TREE_VALUE (args), SECS_MULTIPLIER,
					   fnname, SECS_MAX);
	    case BUILT_IN_SEQUENCIBLE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_sequencible (TREE_VALUE (args));
	    case BUILT_IN_SETTEXTACCESS:
	      if (check_arglist_length (args, 2, 2, fnname) < 0)
		return error_mark_node;
	      return build_chill_settextaccess (TREE_VALUE (args),
						TREE_VALUE (TREE_CHAIN (args)));
	    case BUILT_IN_SETTEXTINDEX:
	      if (check_arglist_length (args, 2, 2, fnname) < 0)
		return error_mark_node;
	      return build_chill_settextindex (TREE_VALUE (args),
					       TREE_VALUE (TREE_CHAIN (args)));
	    case BUILT_IN_SETTEXTRECORD:
	      if (check_arglist_length (args, 2, 2, fnname) < 0)
		return error_mark_node;
	      return build_chill_settextrecord (TREE_VALUE (args),
						TREE_VALUE (TREE_CHAIN (args)));
	    case BUILT_IN_CH_SIN:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__sin");
	    case BUILT_IN_SIZE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_sizeof (TREE_VALUE (args));
	    case BUILT_IN_SQRT:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__sqrt");
	    case BUILT_IN_SUCC:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_pred_or_succ (TREE_VALUE (args), PLUS_EXPR);
	    case BUILT_IN_TAN:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_floatcall (TREE_VALUE (args),
					    IDENTIFIER_POINTER (fnname),
					    "__tan");
	    case BUILT_IN_TERMINATE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_terminate (TREE_VALUE (args));
	    case BUILT_IN_UPPER:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_upper (TREE_VALUE (args));
	    case BUILT_IN_VARIABLE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_variable (TREE_VALUE (args));
	    case BUILT_IN_WRITEABLE:
	      if (check_arglist_length (args, 1, 1, fnname) < 0)
		return error_mark_node;
	      return build_chill_writeable (TREE_VALUE (args));
	    case BUILT_IN_WRITERECORD:
	      if (check_arglist_length (args, 2, 3, fnname) < 0)
		return error_mark_node;
	      return build_chill_writerecord (TREE_VALUE (args), TREE_CHAIN (args));
	    case BUILT_IN_WRITETEXT:
	      if (check_arglist_length (args, 2, -1, fnname) < 0)
		return error_mark_node;
	      return build_chill_writetext (TREE_VALUE (args),
					    TREE_CHAIN (args));

	    case BUILT_IN_EXPIRED:
	    case BUILT_IN_WAIT:
	      sorry ("unimplemented builtin function `%s'",
		     IDENTIFIER_POINTER (fnname));
	      break;
	    default:
	      error ("internal error - bad builtin function `%s'",
		     IDENTIFIER_POINTER (fnname));
	    }
	}
      return build_chill_function_call (func, args);
    }

  if (chill_varying_type_p (TREE_TYPE (func)))
    type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type)));

  if (CH_STRING_TYPE_P (type))
    {
      if (args == NULL_TREE)
	{
	  error ("empty expression in string index");
	  return error_mark_node;
	}
      if (TREE_CHAIN (args) != NULL)
	{
	  error ("only one expression allowed in string index");
	  return error_mark_node;
	}
      if (flag_old_strings)
	return build_chill_slice_with_length (func,
					      TREE_VALUE (args),
					      integer_one_node);
      else if (CH_BOOLS_TYPE_P (type))
	return build_chill_bitref (func, args);
      else
	return build_chill_array_ref (func, args);
    }

  else if (TREE_CODE (type) == ARRAY_TYPE)
    return build_chill_array_ref (func, args);

  if (TREE_CODE (func) != ERROR_MARK)
    error ("invalid: primval ( untyped_exprlist )");
  return error_mark_node;
}

/* Given a set stored as one bit per char (in BUFFER[0 .. BIT_SIZE-1]),
   return a CONTRUCTOR, of type TYPE (a SET_TYPE). */
static tree
expand_packed_set (buffer, bit_size, type)
     const char *buffer;
     int   bit_size;
     tree type;
{
  /* The ordinal number corresponding to the first stored bit. */
  HOST_WIDE_INT first_bit_no =
    TREE_INT_CST_LOW (TYPE_MIN_VALUE (TYPE_DOMAIN (type)));
  tree list = NULL_TREE;
  int i;

  for (i = 0; i < bit_size; i++)
    if (buffer[i])
      {
	int next_0;
	for (next_0 = i + 1; 
	     next_0 < bit_size && buffer[next_0]; next_0++)
	  ;
	if (next_0 == i + 1)
	  list = tree_cons (NULL_TREE, 
		   build_int_2 (i + first_bit_no, 0), list);
	else
	  {
	    list = tree_cons (build_int_2 (i + first_bit_no, 0),
			      build_int_2 (next_0 - 1 + first_bit_no, 0), list);
	    /* advance i past the range of 1-bits */
	    i = next_0;
	  }
      }
  list = build (CONSTRUCTOR, type, NULL_TREE, nreverse (list));
  TREE_CONSTANT (list) = 1;
  return list;
}

/*
 * fold a set represented as a CONSTRUCTOR list.
 * An empty set has a NULL_TREE in its TREE_OPERAND (set, 1) slot.
 */
static tree
fold_set_expr (code, op0, op1)
     enum chill_tree_code code;
     tree op0, op1;
{
  tree temp;
  char *buffer0, *buffer1 = NULL, *bufferr;
  int i, size0, size1, first_unused_bit;

  if (! TREE_CONSTANT (op0) || TREE_CODE (op0) != CONSTRUCTOR)
      return NULL_TREE;

  if (op1 
      && (! TREE_CONSTANT (op1) || TREE_CODE (op1) != CONSTRUCTOR))
    return NULL_TREE;

  size0 = int_size_in_bytes (TREE_TYPE (op0)) * BITS_PER_UNIT;
  if (size0 < 0)
    {
      error ("operand is variable-size bitstring/power-set");
      return error_mark_node;
    }
  buffer0 = (char*) alloca (size0);

  temp = get_set_constructor_bits (op0, buffer0, size0);
  if (temp)
    return NULL_TREE;
  
  if (op0 && op1)
    {
      size1 = int_size_in_bytes (TREE_TYPE (op1)) * BITS_PER_UNIT;
      if (size1 < 0)
	{
	  error ("operand is variable-size bitstring/power-set");
	  return error_mark_node;
	}
      if (size0 != size1)
	return NULL_TREE;
      buffer1 = (char*) alloca (size1);
      temp = get_set_constructor_bits (op1, buffer1, size1);
      if (temp)
	return NULL_TREE;
    }

  bufferr = (char*) alloca (size0); /* result buffer */

  switch ((int)code)
    {
    case SET_NOT_EXPR:
    case BIT_NOT_EXPR:
      for (i = 0; i < size0; i++) 
	bufferr[i] = 1 & ~buffer0[i];
      goto build_result;
    case SET_AND_EXPR:
    case BIT_AND_EXPR:
      for (i = 0; i < size0; i++)
	bufferr[i] = buffer0[i] & buffer1[i];
      goto build_result;
    case SET_IOR_EXPR:
    case BIT_IOR_EXPR:
      for (i = 0; i < size0; i++)
	bufferr[i] = buffer0[i] | buffer1[i];
      goto build_result;
    case SET_XOR_EXPR:
    case BIT_XOR_EXPR:
      for (i = 0; i < size0; i++) 
	bufferr[i] = (buffer0[i] ^ buffer1[i]) & 1;      
      goto build_result;
    case SET_DIFF_EXPR:
    case MINUS_EXPR:
      for (i = 0; i < size0; i++)
	bufferr[i] = buffer0[i] & ~buffer1[i];
      goto build_result;
    build_result:
      /* mask out unused bits. Same as runtime library does. */
      first_unused_bit = TREE_INT_CST_LOW (TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (op0))))
	- TREE_INT_CST_LOW (TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (op0)))) + 1;
      for (i = first_unused_bit; i < size0 ; i++)
	bufferr[i] = 0;
      return expand_packed_set (bufferr, size0, TREE_TYPE (op0));
    case EQ_EXPR:
      for (i = 0; i < size0; i++)
	if (buffer0[i] != buffer1[i])
	  return boolean_false_node;
      return boolean_true_node;
      
    case NE_EXPR:
      for (i = 0; i < size0; i++)
	if (buffer0[i] != buffer1[i])
	  return boolean_true_node;
      return boolean_false_node;

    default:
      return NULL_TREE;
    }
}

/*
 * build a set or bit-array expression.  Type-checking is
 * done elsewhere.
 */
static tree
build_compare_set_expr (code, op0, op1)
     enum tree_code code;
     tree op0, op1;
{
  tree result_type = NULL_TREE;
  const char *fnname;
  tree x;

  /* These conversions are needed if -fold-strings. */
  if (TREE_CODE (TREE_TYPE (op0)) == BOOLEAN_TYPE)
    {
      if (CH_BOOLS_ONE_P (TREE_TYPE (op1)))
	return build_compare_discrete_expr (code,
					    op0,
					    convert (boolean_type_node, op1));
      else
	op0 = convert (bitstring_one_type_node, op0);
    }
  if (TREE_CODE (TREE_TYPE (op1)) == BOOLEAN_TYPE)
    {
      if (CH_BOOLS_ONE_P (TREE_TYPE (op0)))
	return build_compare_discrete_expr (code,
					    convert (boolean_type_node, op0),
					    op1);
      else
	op1 = convert (bitstring_one_type_node, op1);
    }

  switch ((int)code)
    {
    case EQ_EXPR:
      {
	tree temp = fold_set_expr (EQ_EXPR, op0, op1);
	if (temp) 
	  return temp;
	fnname = "__eqpowerset";
	goto compare_powerset;
      }
      break;

    case GE_EXPR:
      /* switch operands and fall thru */
      x = op0;
      op0 = op1;
      op1 = x;

    case LE_EXPR:
      fnname = "__lepowerset";
      goto compare_powerset;

    case GT_EXPR:
      /* switch operands and fall thru */
      x = op0;
      op0 = op1;
      op1 = x;

    case LT_EXPR:
      fnname = "__ltpowerset";
      goto compare_powerset;

    case NE_EXPR:
      return invert_truthvalue (build_compare_set_expr (EQ_EXPR, op0, op1));

    compare_powerset:
      {
	tree tsize = powersetlen (op0);
	
	if (TREE_CODE (TREE_TYPE (op0)) != SET_TYPE)
	  tsize = fold (build (MULT_EXPR, sizetype, tsize,
			       size_int (BITS_PER_UNIT)));

	return build_chill_function_call (lookup_name (get_identifier (fnname)),
	       tree_cons (NULL_TREE, force_addr_of (op0),
	         tree_cons (NULL_TREE, force_addr_of (op1),
		   tree_cons (NULL_TREE, tsize, NULL_TREE))));
      }
      break;

    default:
      if ((int) code >= (int)LAST_AND_UNUSED_TREE_CODE)
	{
	  error ("tree code `%s' unhandled in build_compare_set_expr",
		 tree_code_name[(int)code]);
	  return error_mark_node;
	}
      break;
    }

  return build ((enum tree_code)code, result_type, 
		op0, op1);
}

/* Convert a varying string (or array) to dynamic non-varying string:
   EXP becomes EXP.var_data(0 UP EXP.var_length). */

tree
varying_to_slice (exp)
     tree exp;
{
  if (!chill_varying_type_p (TREE_TYPE (exp)))
    return exp;
  else
    { tree size, data, data_domain, min;
      tree novelty = CH_NOVELTY (TREE_TYPE (exp));
      exp = save_if_needed (exp);
      size = build_component_ref (exp, var_length_id);
      data = build_component_ref (exp, var_data_id);
      TREE_TYPE (data) = copy_novelty (novelty, TREE_TYPE (data));
      data_domain = TYPE_DOMAIN (TREE_TYPE (data));
      if (data_domain != NULL_TREE
	  && TYPE_MIN_VALUE (data_domain) != NULL_TREE)
	min = TYPE_MIN_VALUE (data_domain);
      else
	min = integer_zero_node;
      return build_chill_slice (data, min, size);
    }
}

/* Convert a scalar argument to a string or array type.  This is a subroutine
   of `build_concat_expr'.  */

static tree
scalar_to_string (exp)
     tree exp;
{
  tree type = TREE_TYPE (exp);

  if (SCALAR_P (type))
    {
      int was_const = TREE_CONSTANT (exp);
      if (TREE_TYPE (exp) == char_type_node)
	exp = convert (string_one_type_node, exp);
      else if (TREE_TYPE (exp) == boolean_type_node)
	exp = convert (bitstring_one_type_node, exp);
      else
	exp = convert (build_array_type_for_scalar (type), exp);
      TREE_CONSTANT (exp) = was_const;
      return exp;
    }
  return varying_to_slice (exp);
}

/* FIXME:  Generalize this to general arrays (not just strings),
   at least for the compiler-generated case of padding fixed-length arrays. */

static tree
build_concat_expr (op0, op1)
     tree op0, op1;
{
  tree orig_op0 = op0, orig_op1 = op1;
  tree type0, type1, size0, size1, res;

  op0 = scalar_to_string (op0);
  type0 = TREE_TYPE (op0);
  op1 = scalar_to_string (op1);
  type1 = TREE_TYPE (op1);
  size1 = size_in_bytes (type1);

  /* try to fold constant string literals */
  if (TREE_CODE (op0) == STRING_CST
      && (TREE_CODE (op1) == STRING_CST 
	  || TREE_CODE (op1) == UNDEFINED_EXPR)
      && TREE_CODE (size1) == INTEGER_CST)
    {
      int len0 = TREE_STRING_LENGTH (op0);
      int len1 = TREE_INT_CST_LOW (size1);
      char *result = xmalloc (len0 + len1 + 1);
      memcpy (result, TREE_STRING_POINTER (op0), len0);
      if (TREE_CODE (op1) == UNDEFINED_EXPR)
	memset (&result[len0], '\0', len1);
      else
	memcpy (&result[len0], TREE_STRING_POINTER (op1), len1);
      return build_chill_string (len0 + len1, result);
    }
  else if (TREE_CODE (type0) == TREE_CODE (type1))
    {
      tree result_size;
      struct ch_class result_class;
      struct ch_class class0;
      struct ch_class class1;

      class0 = chill_expr_class (orig_op0);
      class1 = chill_expr_class (orig_op1);

      if (TREE_CODE (type0) == SET_TYPE)
	{
	  result_size = size_binop (PLUS_EXPR,
				    discrete_count (TYPE_DOMAIN (type0)),
				    discrete_count (TYPE_DOMAIN (type1)));
	  result_class.mode = build_bitstring_type (result_size);
	}
      else
	{
	  tree max0 = TYPE_MAX_VALUE (type0);
	  tree max1 = TYPE_MAX_VALUE (type1);

	  /* new array's dynamic size (in bytes). */
	  size0     = size_in_bytes (type0);
	  /* size1 was computed above.  */

	  result_size = size_binop (PLUS_EXPR, size0, size1);
	  /* new array's type. */
	  result_class.mode = build_string_type (char_type_node, result_size);

	  if (max0 || max1)
	    {
	      max0 = max0 == 0 ? size0 : convert (sizetype, max0);
	      max1 = max1 == 0 ? size1 : convert (sizetype, max1);
	      TYPE_MAX_VALUE (result_class.mode)
		= size_binop (PLUS_EXPR, max0, max1);
	    }
	}

      if (class0.kind == CH_VALUE_CLASS || class1.kind == CH_VALUE_CLASS)
	{
	  tree novelty0 = CH_NOVELTY (TREE_TYPE (orig_op0));
	  result_class.kind = CH_VALUE_CLASS;
	  if (class0.kind == CH_VALUE_CLASS && novelty0 != NULL_TREE)
	    SET_CH_NOVELTY_NONNIL (result_class.mode, novelty0);
	  else if (class1.kind == CH_VALUE_CLASS)
	    SET_CH_NOVELTY (result_class.mode,
			    CH_NOVELTY (TREE_TYPE (orig_op1)));
	}
      else
	result_class.kind = CH_DERIVED_CLASS;

      if (TREE_CODE (result_class.mode) == SET_TYPE
	  && TREE_CONSTANT (op0) && TREE_CONSTANT (op1)
	  && TREE_CODE (op0) == CONSTRUCTOR && TREE_CODE (op1) == CONSTRUCTOR)
	{
	  HOST_WIDE_INT size0, size1;  char *buffer;
	  size0 = TREE_INT_CST_LOW (TYPE_MAX_VALUE (TYPE_DOMAIN (type0))) + 1;
	  size1 = TREE_INT_CST_LOW (TYPE_MAX_VALUE (TYPE_DOMAIN (type1))) + 1;
	  buffer = (char*) alloca (size0 + size1);
	  if (size0 < 0 || size1 < 0
	      || get_set_constructor_bits (op0, buffer, size0)
	      || get_set_constructor_bits (op1, buffer + size0, size1))
	    abort ();
	  res = expand_packed_set (buffer, size0 + size1, result_class.mode);
	}
      else
	res = build (CONCAT_EXPR, result_class.mode, op0, op1);
      return convert_to_class (result_class, res);
    }
  else
    {
      error ("incompatible modes in concat expression");
      return error_mark_node;
    }
}

/*
 * handle varying and fixed array compare operations
 */
static tree
build_compare_string_expr (code, op0, op1)
     enum tree_code code;
     tree op0, op1;
{
  if (op0 == NULL_TREE || TREE_CODE (op0) == ERROR_MARK)
    return error_mark_node;
  if (op1 == NULL_TREE || TREE_CODE (op1) == ERROR_MARK)
    return error_mark_node;

  if (tree_int_cst_equal (TYPE_SIZE (TREE_TYPE (op0)),
			  TYPE_SIZE (TREE_TYPE (op1)))
      && ! chill_varying_type_p (TREE_TYPE (op0))
      && ! chill_varying_type_p (TREE_TYPE (op1)))
    {
      tree size = size_in_bytes (TREE_TYPE (op0));
      tree temp = lookup_name (get_identifier ("memcmp"));
      temp = build_chill_function_call (temp,
		 tree_cons (NULL_TREE, force_addr_of (op0),
		     tree_cons (NULL_TREE, force_addr_of (op1),
		       tree_cons (NULL_TREE, size, NULL_TREE))));
      return build_compare_discrete_expr (code, temp, integer_zero_node);
    }

  switch ((int)code)
    {
    case EQ_EXPR:
      code = STRING_EQ_EXPR;
      break;
    case GE_EXPR:
      return invert_truthvalue (build_compare_string_expr (LT_EXPR, op0, op1));
    case LE_EXPR:
      return invert_truthvalue (build_compare_string_expr (LT_EXPR, op1, op0));
    case GT_EXPR:
      return build_compare_string_expr (LT_EXPR, op1, op0);
    case LT_EXPR:
      code = STRING_LT_EXPR;
      break;
    case NE_EXPR:
      return invert_truthvalue (build_compare_string_expr (EQ_EXPR, op0, op1));
    default:
      error ("Invalid operation on array of chars");
      return error_mark_node;
    }

  return build (code, boolean_type_node, op0, op1);
}

static tree
compare_records (exp0, exp1)
     tree exp0, exp1;
{
  tree type = TREE_TYPE (exp0);
  tree field;
  int have_variants = 0;

  tree result = boolean_true_node;
  extern int maximum_field_alignment;

  if (TREE_CODE (type) != RECORD_TYPE)
    abort ();

  exp0 = save_if_needed (exp0);
  exp1 = save_if_needed (exp1);

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (DECL_NAME (field) == NULL_TREE)
	{
	  have_variants = 1;
	  break;
	}
    }

  /* in case of -fpack we always do a memcmp */
  if (maximum_field_alignment != 0)
    {
      tree memcmp_func = lookup_name (get_identifier ("memcmp"));
      tree arg1 = force_addr_of (exp0);
      tree arg2 = force_addr_of (exp1);
      tree arg3 = size_in_bytes (type);
      tree fcall = build_chill_function_call (memcmp_func,
                     tree_cons (NULL_TREE, arg1,
                       tree_cons (NULL_TREE, arg2,
                         tree_cons (NULL_TREE, arg3, NULL_TREE))));

      if (have_variants)
	warning ("comparison of variant structures is unsafe");
      result = build_chill_binary_op (EQ_EXPR, fcall, integer_zero_node);
      return result;
    }

  if (have_variants)
    {
      sorry ("compare with variant records");
      return error_mark_node;
    }

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      tree exp0fld = build_component_ref (exp0, DECL_NAME (field));
      tree exp1fld = build_component_ref (exp1, DECL_NAME (field));
      tree eq_flds = build_chill_binary_op (EQ_EXPR, exp0fld, exp1fld);
      result = build_chill_binary_op (TRUTH_AND_EXPR, result, eq_flds);
    }
  return result;
}

int
compare_int_csts (op, val1, val2)
     enum tree_code op;
     tree val1, val2;
{
  int result;
  tree tmp;
  tree type1 = TREE_TYPE (val1);
  tree type2 = TREE_TYPE (val2);
  switch (op)
    {
    case GT_EXPR:
    case GE_EXPR:
      tmp = val1;  val1 = val2;  val2 = tmp;
      tmp = type1;  type1 = type2; type2 = tmp;
      op = (op == GT_EXPR) ? LT_EXPR : LE_EXPR;
      /* ... fall through ... */
    case LT_EXPR:
    case LE_EXPR:
      if (!TREE_UNSIGNED (type1))
	{
	  if (!TREE_UNSIGNED (type2))
	    result = INT_CST_LT (val1, val2);
	  else if (TREE_INT_CST_HIGH (val1) < 0)
	    result = 1;
	  else
	    result = INT_CST_LT_UNSIGNED (val1, val2);
	}
      else
	{
	  if (!TREE_UNSIGNED (type2) && TREE_INT_CST_HIGH (val2) < 0)
	    result = 0;
	  else
	    result = INT_CST_LT_UNSIGNED (val1, val2);
	}
      if (op == LT_EXPR || result == 1)
	break;
      /* else fall through ... */
    case NE_EXPR:
    case EQ_EXPR:
      if (TREE_INT_CST_LOW (val1) == TREE_INT_CST_LOW (val2)
	  && TREE_INT_CST_HIGH (val1) == TREE_INT_CST_HIGH (val2)
	  /* They're bitwise equal.
	     Check for one being negative and the other unsigned. */
	  && (TREE_INT_CST_HIGH (val2) >= 0
	      || TREE_UNSIGNED (TREE_TYPE (val1))
	      == TREE_UNSIGNED (TREE_TYPE (val2))))
	result = 1;
      else
	result = 0;
      if (op == NE_EXPR)
	result = !result;
      break;
    default:
      abort();
    }
  return result;
}

/* Build an expression to compare discrete values VAL1 and VAL2.
   This does not check that they are discrete, nor that they are
   compatible;  if you need such checks use build_compare_expr. */

tree
build_compare_discrete_expr (op, val1, val2)
     enum tree_code op;
     tree val1, val2;
{
  tree type1 = TREE_TYPE (val1);
  tree type2 = TREE_TYPE (val2);
  tree tmp;

  if (TREE_CODE (val1) == INTEGER_CST && TREE_CODE (val2) == INTEGER_CST)
    {
      if (compare_int_csts (op, val1, val2))
	return boolean_true_node;
      else	
	return boolean_false_node;
    }

  if (TREE_UNSIGNED (type1) != TREE_UNSIGNED (type2))
    {
      switch (op)
	{
	case GT_EXPR:
	case GE_EXPR:
	  tmp = val1;  val1 = val2;  val2 = tmp;
	  tmp = type1;  type1 = type2; type2 = tmp;
	  op = (op == GT_EXPR) ? LT_EXPR : LE_EXPR;
	  /* ... fall through ... */
	case LT_EXPR:
	case LE_EXPR:
	  if (TREE_UNSIGNED (type2))
	    {
	      tmp = build_int_2_wide (0, 0);
	      TREE_TYPE (tmp) = type1;
	      val1 = save_expr (val1);
	      tmp = fold (build (LT_EXPR, boolean_type_node, val1, tmp));
	      if (TYPE_PRECISION (type2) < TYPE_PRECISION (type1))	
		{
		  type2 = unsigned_type (type1);
		  val2 = convert_to_integer (type2, val2);
		}
	      val1 = convert_to_integer (type2, val1);
	      return fold (build (TRUTH_OR_EXPR, boolean_type_node,
				  tmp,
				  fold (build (op, boolean_type_node,
					       val1, val2))));
	    }
	unsigned_vs_signed: /* val1 is unsigned, val2 is signed */
	  tmp = build_int_2_wide (0, 0);
	  TREE_TYPE (tmp) = type2;
	  val2 = save_expr (val2);
	  tmp = fold (build (GE_EXPR, boolean_type_node, val2, tmp));
	  if (TYPE_PRECISION (type1) < TYPE_PRECISION (type2))	
	    {
	      type1 = unsigned_type (type2);
	      val1 = convert_to_integer (type1, val1);
	    }
	  val2 = convert_to_integer (type1, val2);
	  return fold (build (TRUTH_AND_EXPR, boolean_type_node, tmp,
			      fold (build (op, boolean_type_node,
					   val1, val2))));
	case EQ_EXPR:
	  if (TREE_UNSIGNED (val2))
	    {
	      tmp = val1;  val1 = val2;  val2 = tmp;
	      tmp = type1;  type1 = type2; type2 = tmp;
	    }
	  goto unsigned_vs_signed;
	case NE_EXPR:
	  tmp = build_compare_expr (EQ_EXPR, val1, val2);
	  return build_chill_unary_op (TRUTH_NOT_EXPR, tmp);
	default:
	  abort();
	}
    }
  if (TYPE_PRECISION (type1) > TYPE_PRECISION (type2))
    val2 = convert (type1, val2);
  else if (TYPE_PRECISION (type1) < TYPE_PRECISION (type2))
    val1 = convert (type2, val1);
  return fold (build (op, boolean_type_node, val1, val2));
}

tree
build_compare_expr (op, val1, val2)
     enum tree_code op;
     tree val1, val2;
{
  tree tmp;
  tree type1, type2;
  val1 = check_have_mode (val1, "relational expression");
  val2 = check_have_mode (val2, "relational expression");
  if (val1 == NULL_TREE || TREE_CODE (val1) == ERROR_MARK)
    return error_mark_node;
  if (val2 == NULL_TREE || TREE_CODE (val2) == ERROR_MARK)
    return error_mark_node;

  if (pass == 1)
    return build (op, NULL_TREE, val1, val2);

  if (!CH_COMPATIBLE_CLASSES (val1, val2))
    {
      error ("incompatible operands to %s", boolean_code_name [op]);
      return error_mark_node;
    }

  tmp = CH_ROOT_MODE (TREE_TYPE (val1));
  if (tmp != TREE_TYPE (val1))
    val1 = convert (tmp, val1);
  tmp = CH_ROOT_MODE (TREE_TYPE (val2));
  if (tmp != TREE_TYPE (val2))
    val2 = convert (tmp, val2);

  type1 = TREE_TYPE (val1);
  type2 = TREE_TYPE (val2);

  if (TREE_CODE (type1) == SET_TYPE)
    tmp =  build_compare_set_expr (op, val1, val2);

  else if (discrete_type_p (type1))
    tmp = build_compare_discrete_expr (op, val1, val2);

  else if (chill_varying_type_p (type1) || chill_varying_type_p (type2)
      || (TREE_CODE (type1) == ARRAY_TYPE
	  && TREE_CODE (TREE_TYPE (type1)) == CHAR_TYPE)
      || (TREE_CODE (type2) == ARRAY_TYPE
	  && TREE_CODE (TREE_TYPE (type2)) == CHAR_TYPE) )
    tmp =  build_compare_string_expr (op, val1, val2);

  else if ((TREE_CODE (type1) == RECORD_TYPE
	    || TREE_CODE (type2) == RECORD_TYPE)
	   && (op == EQ_EXPR || op == NE_EXPR))
    {
      /* This is for handling INSTANCEs being compared against NULL. */
      if (val1 == null_pointer_node)
	val1 = convert (type2, val1);
      if (val2 == null_pointer_node)
	val2 = convert (type1, val2);

      tmp = compare_records (val1, val2);
      if (op == NE_EXPR)
	tmp = build_chill_unary_op (TRUTH_NOT_EXPR, tmp);
    }

  else if (TREE_CODE (type1) == REAL_TYPE || TREE_CODE (type2) == REAL_TYPE
	   || (op == EQ_EXPR || op == NE_EXPR))
    {
      tmp = build (op, boolean_type_node, val1, val2);
      CH_DERIVED_FLAG (tmp) = 1; /* Optimization to avoid copy_node. */
      tmp = fold (tmp);
    }

  else
    {
      error ("relational operator not allowed for this mode");
      return error_mark_node;
    }

  if (!CH_DERIVED_FLAG (tmp))
    {
      tmp = copy_node (tmp);
      CH_DERIVED_FLAG (tmp) = 1;
    }
  return tmp;
}

tree
finish_chill_binary_op (node)
     tree node;
{
  tree op0 = check_have_mode (TREE_OPERAND (node, 0), "binary expression");
  tree op1 = check_have_mode (TREE_OPERAND (node, 1), "binary expression");
  tree type0 = TREE_TYPE (op0);
  tree type1 = TREE_TYPE (op1);
  tree folded;

  if (TREE_CODE (op0) == ERROR_MARK || TREE_CODE (op1) == ERROR_MARK)
    return error_mark_node;

  if (UNSATISFIED (op0) || UNSATISFIED (op1))
    {
      UNSATISFIED_FLAG (node) = 1;
      return node;
    }
#if 0
  /* assure that both operands have a type */
  if (! type0 && type1)
    {
      op0 = convert (type1, op0);
      type0 = TREE_TYPE (op0);
    }
  if (! type1 && type0)
    {
      op1 = convert (type0, op1);
      type1 = TREE_TYPE (op1);
    }
#endif
  UNSATISFIED_FLAG (node) = 0;
#if 0

  { int op0f = TREE_CODE (op0) == FUNCTION_DECL;
    int op1f = TREE_CODE (op1) == FUNCTION_DECL;
    if (op0f)
      op0 = convert (build_pointer_type (TREE_TYPE (op0)), op0);
    if (op1f)
      op1 = convert (build_pointer_type (TREE_TYPE (op1)), op1);
    if ((op0f || op1f)
	&& code != EQ_EXPR && code != NE_EXPR)
      error ("Cannot use %s operator on PROC mode variable",
	     tree_code_name[(int)code]);
  }

  if (invalid_left_operand (type0, code))
    {
      error ("invalid left operand of %s", tree_code_name[(int)code]);
      return error_mark_node;
    }
  if (invalid_right_operand (code, type1))
    {
      error ("invalid right operand of %s", tree_code_name[(int)code]);
      return error_mark_node;
    }
#endif

  switch (TREE_CODE (node))
    {
    case CONCAT_EXPR:
      return build_concat_expr (op0, op1);

    case REPLICATE_EXPR:
      op0 = fold (op0);
      if (!TREE_CONSTANT (op0) || !TREE_CONSTANT (op1))
	{
	  error ("repetition expression must be constant");
	  return error_mark_node;
	}
      else
	return build_chill_repetition_op (op0, op1);

    case FLOOR_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      if (TREE_CODE (type0) != INTEGER_TYPE)
	{
	  error ("left argument to MOD/REM operator must be integral");
	  return error_mark_node;
	}
      if (TREE_CODE (type1) != INTEGER_TYPE)
	{
	  error ("right argument to MOD/REM operator must be integral");
	  return error_mark_node;
	}
      break;

    case MINUS_EXPR:
      if (TREE_CODE (type1) == SET_TYPE)
	{
	  tree temp = fold_set_expr (MINUS_EXPR, op0, op1);

	  if (temp)
	    return temp;
	  if (TYPE_MODE (type1) == BLKmode)
	    TREE_SET_CODE (node, SET_DIFF_EXPR);
	  else
	    {
	      op1 = build_chill_unary_op (BIT_NOT_EXPR, op1);
	      TREE_OPERAND (node, 1) = op1;
	      TREE_SET_CODE (node, BIT_AND_EXPR);
	    }
	}
      break;

    case TRUNC_DIV_EXPR:
      if (TREE_CODE (type0) == REAL_TYPE || TREE_CODE (type1) == REAL_TYPE)
	TREE_SET_CODE (node, RDIV_EXPR);
      break;

    case BIT_AND_EXPR:
      if (TYPE_MODE (type1) == BLKmode)
	TREE_SET_CODE (node, SET_AND_EXPR);
      goto fold_set_binop;
    case BIT_IOR_EXPR:
      if (TYPE_MODE (type1) == BLKmode)
	TREE_SET_CODE (node, SET_IOR_EXPR);
      goto fold_set_binop;
    case BIT_XOR_EXPR:
      if (TYPE_MODE (type1) == BLKmode)
	TREE_SET_CODE (node, SET_XOR_EXPR);
      goto fold_set_binop;
    case SET_AND_EXPR:
    case SET_IOR_EXPR:
    case SET_XOR_EXPR:
    case SET_DIFF_EXPR:
    fold_set_binop:
      if (TREE_CODE (type0) == SET_TYPE)
	{
	  tree temp = fold_set_expr (TREE_CODE (node), op0, op1);

	  if (temp)
	    return temp;
	}
      break;

    case SET_IN_EXPR:
      if (TREE_CODE (type1) != SET_TYPE || CH_BOOLS_TYPE_P (type1))
	{
	  error ("right operand of IN is not a powerset");
	  return error_mark_node;
	}
      if (!CH_COMPATIBLE (op0, TYPE_DOMAIN (type1)))
	{
	  error ("left operand of IN incompatible with right operand");
	  return error_mark_node;
	}
      type0 = CH_ROOT_MODE (type0);
      if (type0 != TREE_TYPE (op0))
	TREE_OPERAND (node, 0) = op0 = convert (type0, op0);
      TREE_TYPE (node) = boolean_type_node;
      CH_DERIVED_FLAG (node) = 1;
      node = fold (node);
      if (!CH_DERIVED_FLAG (node))
	{
	  node = copy_node (node);
	  CH_DERIVED_FLAG (node) = 1;
	}
      return node;
    case NE_EXPR:
    case EQ_EXPR:
    case GE_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case LT_EXPR:
      return build_compare_expr (TREE_CODE (node), op0, op1);
    default:
      ;
    }

  if (!CH_COMPATIBLE_CLASSES (op0, op1))
    {
      error ("incompatible operands to %s", tree_code_name[(int) TREE_CODE (node)]);
      return error_mark_node;
    }

  if (TREE_TYPE (node) == NULL_TREE)
    {
      struct ch_class class;
      class = CH_ROOT_RESULTING_CLASS (op0, op1);
      TREE_OPERAND (node, 0) = op0 = convert_to_class (class, op0);
      type0 = TREE_TYPE (op0);
      TREE_OPERAND (node, 1) = op1 = convert_to_class (class, op1);
      type1 = TREE_TYPE (op1);
      TREE_TYPE (node) = class.mode;
      folded = convert_to_class (class, fold (node));
    }
  else
    folded = fold (node);
#if 0
  if (folded == node)
    TREE_CONSTANT (folded) = TREE_CONSTANT (op0) & TREE_CONSTANT (op1);
#endif
  if (TREE_CODE (node) == TRUNC_DIV_EXPR)
    {
      if (TREE_CONSTANT (op1))
	{
	  if (tree_int_cst_equal (op1, integer_zero_node))
	    {
	      error ("division by zero");
	      return integer_zero_node;
	    }
	}
      else if (range_checking)
	{
#if 0
	  tree test =
	    build (EQ_EXPR, boolean_type_node, op1, integer_zero_node);
	  /* Should this be overflow? */
	  folded = check_expression (folded, test,
				     ridpointers[(int) RID_RANGEFAIL]);
#endif
	}
    }
  return folded;
}

/*
 * This implements the '->' operator, which, like the '&' in C,
 * returns a pointer to an object, which has the type of
 * pointer-to-that-object.
 *
 * FORCE is 0 when we're evaluating a user-level syntactic construct,
 * and 1 when we're calling from inside the compiler.
 */
tree
build_chill_arrow_expr (ref, force)
     tree ref;
     int force;
{
  tree addr_type;
  tree result;

  if (pass == 1)
    {
      error ("-> operator not allow in constant expression");
      return error_mark_node;
    }

  if (ref == NULL_TREE || TREE_CODE (ref) == ERROR_MARK)
    return ref;

  while (TREE_CODE (TREE_TYPE (ref)) == REFERENCE_TYPE)
    ref = convert (TREE_TYPE (TREE_TYPE (ref)), ref);

  if (!force && ! CH_LOCATION_P (ref))
    {
      if (TREE_CODE (ref) == STRING_CST)
	pedwarn ("taking the address of a string literal is non-standard");
      else if (TREE_CODE (TREE_TYPE (ref)) == FUNCTION_TYPE)
	pedwarn ("taking the address of a function is non-standard");
      else
	{
	  error ("ADDR requires a LOCATION argument");
	  return error_mark_node;
	}
      /* FIXME: Should we be sure that ref isn't a
	 function if we're being pedantic? */
    }

  addr_type = build_pointer_type (TREE_TYPE (ref));

#if 0
  /* This transformation makes chill_expr_class return CH_VALUE_CLASS
     when it should return CH_REFERENCE_CLASS.  That could be fixed,
     but we probably don't want this transformation anyway. */
  if (TREE_CODE (ref) == NOP_EXPR) /* RETYPE_EXPR */
    {
      tree addr;
      while (TREE_CODE (ref) == NOP_EXPR) /* RETYPE_EXPR */
	ref = TREE_OPERAND (ref, 0);
      mark_addressable (ref);
      addr = build1 (ADDR_EXPR, 
		     build_pointer_type (TREE_TYPE (ref)), ref);
      return build1 (NOP_EXPR, /* RETYPE_EXPR */
		      addr_type,
		      addr);
    } 
  else
#endif
    {
      if (! mark_addressable (ref))
	{
	  error ("-> expression is not addressable");
	  return error_mark_node;
	}
      result = build1 (ADDR_EXPR, addr_type, ref);
      if (staticp (ref)
	  && ! (TREE_CODE (ref) == FUNCTION_DECL
		&& DECL_CONTEXT (ref) != 0))
	TREE_CONSTANT (result) = 1;
      return result;
    }
}

/*
 * This implements the ADDR builtin function, which returns a 
 * free reference, analogous to the C 'void *'.
 */
tree
build_chill_addr_expr (ref, errormsg)
     tree ref;
     const char *errormsg;
{
  if (ref == error_mark_node)
    return ref;

  if (! CH_LOCATION_P (ref)
      && TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE)
    {
      error ("ADDR parameter must be a LOCATION");
      return error_mark_node;
    }
  ref = build_chill_arrow_expr (ref, 1);

  if (ref != NULL_TREE && TREE_CODE (ref) != ERROR_MARK)
    TREE_TYPE (ref) = ptr_type_node;
  else if (errormsg == NULL)
    {
      error ("possible internal error in build_chill_arrow_expr");
      return error_mark_node;
    }
  else
    {
      error ("%s is not addressable", errormsg);
      return error_mark_node;
    }
  return ref;
}

tree
build_chill_binary_op (code, op0, op1)
     enum chill_tree_code code;
     tree op0, op1;
{
  register tree result;

  if (op0 == NULL_TREE || TREE_CODE (op0) == ERROR_MARK)
    return error_mark_node;
  if (op1 == NULL_TREE || TREE_CODE (op1) == ERROR_MARK)
    return error_mark_node;

  result = build (code, NULL_TREE, op0, op1);

  if (pass != 1)
    result = finish_chill_binary_op (result);
  return result;
}

/*
 * process a string repetition phrase '(' COUNT ')' STRING
 */
static tree
string_char_rep (count, string)
     int count;
     tree string;
{
  int slen, charindx, repcnt;
  char ch;
  char *temp;
  const char *inp;
  char *outp;
  tree type;

  if (string == NULL_TREE || TREE_CODE (string) == ERROR_MARK)
    return error_mark_node;

  type = TREE_TYPE (string);
  slen = int_size_in_bytes (type);
  temp = xmalloc (slen * count);
  inp = &ch;
  outp = temp;
  if (TREE_CODE (string) == STRING_CST)  
    inp = TREE_STRING_POINTER (string);
  else                           /* single character */
    ch = (char)TREE_INT_CST_LOW (string);

  /* copy the string/char COUNT times into the output buffer */
  for (outp = temp, repcnt = 0; repcnt < count; repcnt++)
    for (charindx = 0; charindx < slen; charindx++)
      *outp++ = inp[charindx];
  return build_chill_string (slen * count, temp);
}

/* Build a bit-string constant containing with the given LENGTH
   containing all ones (if VALUE is true), or all zeros (if VALUE is false). */

static tree
build_boring_bitstring (length, value)
     long length;
     int value;
{
  tree result;
  tree list;  /* Value of CONSTRUCTOR_ELTS in the result. */
  if (value && length > 0)
    list = tree_cons (integer_zero_node, size_int (length - 1), NULL_TREE);
  else
    list = NULL_TREE;
		
  result = build (CONSTRUCTOR,
		  build_bitstring_type (size_int (length)),
		  NULL_TREE,
		  list);
  TREE_CONSTANT (result) = 1;
  CH_DERIVED_FLAG (result) = 1;
  return result;
}

/*
 * handle a string repetition, with the syntax:
 *        ( COUNT ) 'STRING'
 * COUNT is required to be constant, positive and folded.
 */
tree
build_chill_repetition_op (count_op, string)
     tree count_op;
     tree string;
{
  int count;
  tree type = TREE_TYPE (string);

  if (TREE_CODE (count_op) != INTEGER_CST)
    {
      error ("repetition count is not an integer constant");
      return error_mark_node;
    }

  count = TREE_INT_CST_LOW (count_op);

  if (count < 0)
    {
      error ("repetition count < 0");
      return error_mark_node;
    }
  if (! TREE_CONSTANT (string))
    {
      error ("repetition value not constant");
      return error_mark_node;
    }

  if (TREE_CODE (string) == STRING_CST)
    return string_char_rep (count, string);

  switch ((int)TREE_CODE (type))
    {
    case BOOLEAN_TYPE:
      if (TREE_CODE (string) == INTEGER_CST)
	return build_boring_bitstring (count, TREE_INT_CST_LOW (string));
      error ("bitstring repetition of non-constant boolean");
      return error_mark_node;

    case CHAR_TYPE:
      return string_char_rep (count, string);

    case SET_TYPE:
      { int i, tree_const = 1;
	tree new_list = NULL_TREE;
	tree vallist;
	tree result;
	tree domain = TYPE_DOMAIN (type);
	tree orig_length;
	HOST_WIDE_INT orig_len;

	if (!CH_BOOLS_TYPE_P (type)) /* cannot replicate a powerset */
	  break;

	orig_length = discrete_count (domain);

	if (TREE_CODE (string) != CONSTRUCTOR || !TREE_CONSTANT (string)
	    || TREE_CODE (orig_length) != INTEGER_CST)
	  {
	    error ("string repetition operand is non-constant bitstring");
	    return error_mark_node;
	  }

			       
	orig_len = TREE_INT_CST_LOW (orig_length);

	/* if the set is empty, this is NULL */
	vallist = TREE_OPERAND (string, 1);

	if (vallist == NULL_TREE) /* No bits are set. */
	  return build_boring_bitstring (count * orig_len, 0);
	else if (TREE_CHAIN (vallist) == NULL_TREE
		 && (TREE_PURPOSE (vallist) == NULL_TREE
		     ? (orig_len == 1
			&& tree_int_cst_equal (TYPE_MIN_VALUE (domain),
					       TREE_VALUE (vallist)))
		     : (tree_int_cst_equal (TYPE_MIN_VALUE (domain),
					    TREE_PURPOSE (vallist))
			&& tree_int_cst_equal (TYPE_MAX_VALUE (domain),
					       TREE_VALUE (vallist)))))
	  return build_boring_bitstring (count * orig_len, 1);

	for (i = 0; i < count; i++)
	  {
	    tree origin = build_int_2 (i * orig_len, 0);
	    tree temp;

	    /* scan down the given value list, building
	       new bit-positions */
	    for (temp = vallist; temp; temp = TREE_CHAIN (temp))
	      {
		tree new_value
		  = fold (size_binop (PLUS_EXPR, origin, TREE_VALUE (temp)));
		tree new_purpose = NULL_TREE;
		if (! TREE_CONSTANT (TREE_VALUE (temp)))
		  tree_const = 0;
		if (TREE_PURPOSE (temp))
		  {
		    new_purpose = fold (size_binop (PLUS_EXPR,
						    origin,
						    TREE_PURPOSE (temp)));
		    if (! TREE_CONSTANT (TREE_PURPOSE (temp)))
		      tree_const = 0;
		  }

		new_list = tree_cons (new_purpose,
					  new_value, new_list);
	      }
	  }
	result = build (CONSTRUCTOR,
			build_bitstring_type (size_int (count * orig_len)),
			NULL_TREE, nreverse (new_list));
	TREE_CONSTANT (result) = tree_const;
	CH_DERIVED_FLAG (result) = CH_DERIVED_FLAG (string);
	return result;
      }

    default:
      error ("non-char, non-bit string repetition");
      return error_mark_node;
  }
  return error_mark_node;
}

tree
finish_chill_unary_op (node)
     tree node;
{
  enum chill_tree_code code = TREE_CODE (node);
  tree op0 = check_have_mode (TREE_OPERAND (node, 0), "unary expression");
  tree type0 = TREE_TYPE (op0);
  struct ch_class class;

  if (TREE_CODE (op0) == ERROR_MARK)
    return error_mark_node;
  /* The expression codes of the data types of the arguments tell us
     whether the arguments are integers, floating, pointers, etc.  */

  if (TREE_CODE (type0) == REFERENCE_TYPE)
    {
      op0 = convert (TREE_TYPE (type0), op0);
      type0 = TREE_TYPE (op0);
    }

  if (invalid_right_operand (code, type0))
    {
      error ("invalid operand of %s", 
	     tree_code_name[(int)code]);
      return error_mark_node;
    }
  switch ((int)TREE_CODE (type0))
    {
    case ARRAY_TYPE:
      if (TREE_CODE ( TREE_TYPE (type0)) == BOOLEAN_TYPE)
	code = SET_NOT_EXPR;
      else
	{
	  error ("right operand of %s is not array of boolean",
		 tree_code_name[(int)code]);
	  return error_mark_node;
	}
      break;
    case BOOLEAN_TYPE:
      switch ((int)code)
	{
	case BIT_NOT_EXPR:
	case TRUTH_NOT_EXPR:
	  return invert_truthvalue (truthvalue_conversion (op0));

	default:
	  error ("%s operator applied to boolean variable",
		 tree_code_name[(int)code]);
	  return error_mark_node;
	}
      break;

    case SET_TYPE:
      switch ((int)code)
	{
	case BIT_NOT_EXPR:
	case NEGATE_EXPR:
	  {
	    tree temp = fold_set_expr (BIT_NOT_EXPR, op0, NULL_TREE);

	    if (temp) 
	      return temp;

	    code = SET_NOT_EXPR;
	  }
	  break;

	default:
	  error ("invalid right operand of %s", tree_code_name[(int)code]);
	  return error_mark_node;
	}

    }

  class = chill_expr_class (op0);
  if (class.mode)
    class.mode = CH_ROOT_MODE (class.mode);
  TREE_SET_CODE (node, code);
  TREE_OPERAND (node, 0) = op0 = convert_to_class (class, op0);
  TREE_TYPE (node) = TREE_TYPE (op0);

  node = convert_to_class (class, fold (node));

  /* FIXME: should call
   * cond_type_range_exception (op0);
   */
  return node;
}

/* op is TRUTH_NOT_EXPR, BIT_NOT_EXPR, or NEGATE_EXPR */

tree
build_chill_unary_op (code, op0)
     enum chill_tree_code code;
     tree op0;
{
  register tree result = NULL_TREE;

  if (op0 == NULL_TREE || TREE_CODE (op0) == ERROR_MARK)
    return error_mark_node;

  result = build1 (code, NULL_TREE, op0);

  if (pass != 1)
    result = finish_chill_unary_op (result);
  return result;
}

tree
truthvalue_conversion (expr)
     tree expr;
{
  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return error_mark_node;

#if 0        /* what about a LE_EXPR (integer_type, integer_type ) */
  if (TREE_CODE (TREE_TYPE (expr)) != BOOLEAN_TYPE)
    error ("non-boolean mode in conditional expression");
#endif

  switch ((int)TREE_CODE (expr))
    {
      /* It is simpler and generates better code to have only TRUTH_*_EXPR
	 or comparison expressions as truth values at this level.  */
#if 0
    case COMPONENT_REF:
      /* A one-bit unsigned bit-field is already acceptable.  */
      if (1 == TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (expr, 1)))
	  && TREE_UNSIGNED (TREE_OPERAND (expr, 1)))
	return expr;
      break;
#endif

    case EQ_EXPR:
      /* It is simpler and generates better code to have only TRUTH_*_EXPR
	 or comparison expressions as truth values at this level.  */
    case NE_EXPR: case LE_EXPR: case GE_EXPR: case LT_EXPR: case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? boolean_false_node : boolean_true_node;

    case REAL_CST:
      return real_zerop (expr) ? boolean_false_node : boolean_true_node;

    case ADDR_EXPR:
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 0)))
	return build (COMPOUND_EXPR, boolean_type_node,
		      TREE_OPERAND (expr, 0), boolean_true_node);
      else
	return boolean_true_node;

    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
    case FFS_EXPR:
      /* These don't change whether an object is non-zero or zero.  */
      return truthvalue_conversion (TREE_OPERAND (expr, 0));

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These don't change whether an object is zero or non-zero, but
	 we can't ignore them if their second arg has side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)))
	return build (COMPOUND_EXPR, boolean_type_node, TREE_OPERAND (expr, 1),
		      truthvalue_conversion (TREE_OPERAND (expr, 0)));
      else
	return truthvalue_conversion (TREE_OPERAND (expr, 0));
      
    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, boolean_type_node, TREE_OPERAND (expr, 0),
			  truthvalue_conversion (TREE_OPERAND (expr, 1)),
			  truthvalue_conversion (TREE_OPERAND (expr, 2))));

    case CONVERT_EXPR:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	 since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
	  || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
	break;
      /* fall through... */
    case NOP_EXPR:
      /* If this is widening the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
	  >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
	return truthvalue_conversion (TREE_OPERAND (expr, 0));
      break;

    case BIT_XOR_EXPR:
    case MINUS_EXPR:
      /* These can be changed into a comparison of the two objects.  */
      if (TREE_TYPE (TREE_OPERAND (expr, 0))
	  == TREE_TYPE (TREE_OPERAND (expr, 1)))
	return build_chill_binary_op (NE_EXPR, TREE_OPERAND (expr, 0),
				      TREE_OPERAND (expr, 1));
      return build_chill_binary_op (NE_EXPR, TREE_OPERAND (expr, 0),
				    fold (build1 (NOP_EXPR,
					    TREE_TYPE (TREE_OPERAND (expr, 0)),
					    TREE_OPERAND (expr, 1))));
    }

  return build_chill_binary_op (NE_EXPR, expr, boolean_false_node);
}


/*
 * return a folded tree for the powerset's length in bits.  If a
 * non-set is passed, we assume it's an array or boolean bytes.
 */
tree
powersetlen (powerset)
     tree powerset;
{
  if (powerset == NULL_TREE || TREE_CODE (powerset) == ERROR_MARK)
    return error_mark_node;

  return discrete_count (TYPE_DOMAIN (TREE_TYPE (powerset)));
}
