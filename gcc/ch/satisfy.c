/* Name-satisfaction for GNU Chill compiler.
   Copyright (C) 1993, 98, 99, 2000 Free Software Foundation, Inc.

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
#include "flags.h"
#include "ch-tree.h"
#include "lex.h"
#include "toplev.h"

#define SATISFY(ARG) ((ARG) = satisfy(ARG, chain))

struct decl_chain
{
  struct decl_chain *prev;
  /* DECL can be a decl, or a POINTER_TYPE or a REFERENCE_TYPE. */
  tree decl;
};

/* forward declarations */
static tree satisfy		PARAMS ((tree, struct decl_chain *));
static void cycle_error_print	PARAMS ((struct decl_chain *, tree));
static tree safe_satisfy_decl	PARAMS ((tree, struct decl_chain *));
static void satisfy_list	PARAMS ((tree, struct decl_chain *));
static void satisfy_list_values	PARAMS ((tree, struct decl_chain *));

static struct decl_chain dummy_chain;
#define LOOKUP_ONLY (chain==&dummy_chain)

/* Recursive helper routine to logically reverse the chain. */
static void
cycle_error_print (chain, decl)
     struct decl_chain *chain;
     tree decl;
{
  if (chain->decl != decl)
    {
      cycle_error_print (chain->prev, decl);
      if (TREE_CODE_CLASS (TREE_CODE (chain->decl)) == 'd')
	error_with_decl (chain->decl, "  `%s', which depends on ...");
    }
}

static tree
safe_satisfy_decl (decl, prev_chain)
     tree decl;
     struct decl_chain *prev_chain;
{
  struct decl_chain new_link;
  struct decl_chain *link;
  struct decl_chain *chain = prev_chain;
  char *save_filename = input_filename;
  int save_lineno = lineno;
  tree result = decl;
  
  if (decl == NULL_TREE)
    return decl;

  if (!LOOKUP_ONLY)
    {
      int pointer_type_breaks_cycle = 0;
      /* Look for a cycle.
	 We could do this test more efficiently by setting a flag.  FIXME */
      for (link = prev_chain; link != NULL; link = link->prev)
	{
	  if (TREE_CODE_CLASS (TREE_CODE (link->decl)) != 'd')
	    pointer_type_breaks_cycle = 1;
	  if (link->decl == decl)
	    {
	      if (!pointer_type_breaks_cycle)
		{
		  error_with_decl (decl, "Cycle: `%s' depends on ...");
		  cycle_error_print (prev_chain, decl);
		  error_with_decl (decl, "  `%s'");
		  return error_mark_node;
		}
	      /* There is a cycle, but it includes a pointer type,
		 so we're OK.  However, we still have to continue
		 the satisfy (for example in case this is a TYPE_DECL
		 that points to a LANG_DECL).  The cycle-check for
		 POINTER_TYPE/REFERENCE_TYPE should stop the recursion. */
	      break;
	    }
	}

      new_link.decl = decl;
      new_link.prev = prev_chain;
      chain = &new_link;
    }

  input_filename = DECL_SOURCE_FILE (decl);
  lineno = DECL_SOURCE_LINE (decl);

  switch ((enum chill_tree_code)TREE_CODE (decl))
    {
    case ALIAS_DECL:
      if (!LOOKUP_ONLY && !DECL_POSTFIX_ALL(decl))
	result = safe_satisfy_decl (DECL_ABSTRACT_ORIGIN (decl), chain);
      break;
    case BASED_DECL:
      SATISFY (TREE_TYPE (decl));
      SATISFY (DECL_ABSTRACT_ORIGIN (decl));
      break;
    case CONST_DECL:
      SATISFY (TREE_TYPE (decl));
      SATISFY (DECL_INITIAL (decl));
      if (!LOOKUP_ONLY)
	{
	  if (DECL_SIZE (decl) == 0)
	    {
	      tree init_expr = DECL_INITIAL (decl);
	      tree init_type;
	      tree specified_mode = TREE_TYPE (decl);

	      if (init_expr == NULL_TREE
		  || TREE_CODE (init_expr) == ERROR_MARK)
		goto bad_const;
	      init_type = TREE_TYPE (init_expr);
	      if (specified_mode == NULL_TREE)
		{
		  if (init_type == NULL_TREE)
		    {
		      check_have_mode (init_expr, "SYN without mode");
		      goto bad_const;
		    }
		  TREE_TYPE (decl) = init_type;
		  CH_DERIVED_FLAG (decl) = CH_DERIVED_FLAG (init_expr);
		}
	      else if (CH_IS_ASSOCIATION_MODE (specified_mode) ||
		       CH_IS_ACCESS_MODE (specified_mode) || CH_IS_TEXT_MODE (specified_mode) ||
		       CH_IS_BUFFER_MODE (specified_mode) || CH_IS_EVENT_MODE (specified_mode))
		{
		  error ("SYN of this mode not allowed");
		  goto bad_const;
		}
	      else if (!CH_COMPATIBLE (init_expr, specified_mode))
		{
		  error ("mode of SYN incompatible with value");
		  goto bad_const;
		} 
	      else if (discrete_type_p (specified_mode)
		       && TREE_CODE (init_expr) == INTEGER_CST
		       && (compare_int_csts (LT_EXPR, init_expr,
					     TYPE_MIN_VALUE (specified_mode))
			   || compare_int_csts (GT_EXPR, init_expr,
						TYPE_MAX_VALUE(specified_mode))
			   ))
		{
		  error ("SYN value outside range of its mode");
		  /* set an always-valid initial value to prevent 
		     other errors. */
		  DECL_INITIAL (decl) = TYPE_MIN_VALUE (specified_mode);
		}
	      else if (CH_STRING_TYPE_P (specified_mode) 
		       && (init_type && CH_STRING_TYPE_P (init_type))
		       && integer_zerop (string_assignment_condition (specified_mode, init_expr)))
		{
		  error ("INIT string too large for mode");
		  DECL_INITIAL (decl) = error_mark_node;
		}
	      else
		{
		  struct ch_class class;
		  class.mode = TREE_TYPE (decl);
		  class.kind = CH_VALUE_CLASS;
		  DECL_INITIAL (decl)
		    = convert_to_class (class, DECL_INITIAL (decl));
		}
	      /* DECL_SIZE is set to prevent re-doing this stuff. */
	      DECL_SIZE (decl) = TYPE_SIZE (TREE_TYPE (decl));
	      if (! TREE_CONSTANT (DECL_INITIAL (decl))
		  && TREE_CODE (DECL_INITIAL (decl)) != ERROR_MARK)
		{
		  error_with_decl (decl,
				   "value of %s is not a valid constant");
		  DECL_INITIAL (decl) = error_mark_node;
		}
	    }
	  result = DECL_INITIAL (decl);
	}
      break;
    bad_const:
      DECL_INITIAL (decl) = error_mark_node;
      TREE_TYPE (decl) = error_mark_node;
      return error_mark_node;
    case FUNCTION_DECL:
      SATISFY (TREE_TYPE (decl));
      if (CH_DECL_PROCESS (decl))
	safe_satisfy_decl ((tree) DECL_TASKING_CODE_DECL (decl), prev_chain);
      break;
    case PARM_DECL:
      SATISFY (TREE_TYPE (decl));
      break;
    /* RESULT_DECL doesn't need to be satisfied;  
       it's only built internally in pass 2 */
    case TYPE_DECL:
      SATISFY (TREE_TYPE (decl));
      if (CH_DECL_SIGNAL (decl))
	safe_satisfy_decl ((tree) DECL_TASKING_CODE_DECL (decl), prev_chain);
      if (!LOOKUP_ONLY)
	{
	  if (TYPE_NAME (TREE_TYPE (decl)) == NULL_TREE)
	    TYPE_NAME (TREE_TYPE (decl)) = decl;
	  layout_decl (decl, 0);
	  if (CH_DECL_SIGNAL (decl) && CH_TYPE_NONVALUE_P (TREE_TYPE (decl)))
	    error ("mode with non-value property in signal definition");
	  result = TREE_TYPE (decl);
	}
      break;
    case VAR_DECL:
      SATISFY (TREE_TYPE (decl));
      if (!LOOKUP_ONLY)
	{
	  layout_decl (decl, 0);
	  if (TREE_READONLY (TREE_TYPE (decl)))
	    TREE_READONLY (decl) = 1;
	}
      break;
    default:
      ;
    }

  /* Now set the DECL_RTL, if needed. */
  if (!LOOKUP_ONLY && DECL_RTL (decl) == 0
      && (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL
	  || TREE_CODE (decl) == CONST_DECL))
    {
      if (TREE_CODE (decl) == FUNCTION_DECL && decl_function_context (decl))
	make_function_rtl (decl);
      else if (!TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
	expand_decl (decl);
      else
	{ char * asm_name;
	  if (current_module == 0 || TREE_PUBLIC (decl)
	      || current_function_decl)
	    asm_name = NULL;
	  else
	    {
	      asm_name = (char*)
		alloca (IDENTIFIER_LENGTH (current_module->prefix_name)
			+ IDENTIFIER_LENGTH (DECL_NAME (decl)) + 3);
	      sprintf (asm_name, "%s__%s",
		       IDENTIFIER_POINTER (current_module->prefix_name),
		       IDENTIFIER_POINTER (DECL_NAME (decl)));
	    }
	  make_decl_rtl (decl, asm_name, TREE_PUBLIC (decl));
	}
    }

  input_filename = save_filename;
  lineno = save_lineno;

  return result;
}

tree
satisfy_decl (decl, lookup_only)
     tree decl;
     int lookup_only;
{
  return safe_satisfy_decl (decl, lookup_only ? &dummy_chain : NULL);
}

static void
satisfy_list (exp, chain)
     register tree exp;
     struct decl_chain *chain;
{
  for (; exp != NULL_TREE; exp = TREE_CHAIN (exp))
    {
      SATISFY (TREE_VALUE (exp));
      SATISFY (TREE_PURPOSE (exp));
    }
}

static void
satisfy_list_values (exp, chain)
     register tree exp;
     struct decl_chain *chain;
{
  for (; exp != NULL_TREE; exp = TREE_CHAIN (exp))
    {
      SATISFY (TREE_VALUE (exp));
    }
}

static tree
satisfy (exp, chain)
     tree exp;
     struct decl_chain *chain;
{
  int arg_length;
  int i;
  tree decl;

  if (exp == NULL_TREE)
    return NULL_TREE;

#if 0
  if (!UNSATISFIED (exp))
    return exp;
#endif

  switch (TREE_CODE_CLASS (TREE_CODE (exp)))
    {
    case 'd':
      if (!LOOKUP_ONLY)
	return safe_satisfy_decl (exp, chain);
      break;
    case 'r':
    case 's':
    case '<':
    case 'e':
      switch ((enum chill_tree_code)TREE_CODE (exp))
	{
	case REPLICATE_EXPR:
	  goto binary_op;
	case TRUTH_NOT_EXPR:
	  goto unary_op;
	case COMPONENT_REF:
	  SATISFY (TREE_OPERAND (exp, 0));
	  if (!LOOKUP_ONLY && TREE_TYPE (exp) == NULL_TREE)
	    return resolve_component_ref (exp);
	  return exp;
	case CALL_EXPR:
	  SATISFY (TREE_OPERAND (exp, 0));
	  SATISFY (TREE_OPERAND (exp, 1));
	  if (!LOOKUP_ONLY && TREE_TYPE (exp) == NULL_TREE)
	    return build_generalized_call (TREE_OPERAND (exp, 0),
					   TREE_OPERAND (exp, 1));
	  return exp;
	case CONSTRUCTOR:
	  { tree link = TREE_OPERAND (exp, 1);
	    int expand_needed = TREE_TYPE (exp)
	      && TREE_CODE_CLASS (TREE_CODE (TREE_TYPE (exp))) != 't';
	    for (; link != NULL_TREE; link = TREE_CHAIN (link))
	      {
		SATISFY (TREE_VALUE (link));
		if (!TUPLE_NAMED_FIELD (link))
		  SATISFY (TREE_PURPOSE (link));
	      }
	    SATISFY (TREE_TYPE (exp));
	    if (expand_needed && !LOOKUP_ONLY)
	      {
		tree type = TREE_TYPE (exp);
		TREE_TYPE (exp) = NULL_TREE; /* To force expansion. */
		return chill_expand_tuple (type, exp);
	      }
	    return exp;
	  }
	default:
	  ;
	}
      arg_length = tree_code_length[TREE_CODE (exp)];
      for (i = 0; i < arg_length; i++)
	SATISFY (TREE_OPERAND (exp, i));
      return exp;
    case '1':
    unary_op:
      SATISFY (TREE_OPERAND (exp, 0));
      if ((enum chill_tree_code)TREE_CODE (exp) == PAREN_EXPR)
	return TREE_OPERAND (exp, 0);
      if (!LOOKUP_ONLY)
	return finish_chill_unary_op (exp);
      break;
    case '2':
    binary_op:
      SATISFY (TREE_OPERAND (exp, 0));
      SATISFY (TREE_OPERAND (exp, 1));
      if (!LOOKUP_ONLY && TREE_CODE (exp) != RANGE_EXPR)
	return finish_chill_binary_op (exp);
      break;
    case 'x':
      switch ((enum chill_tree_code)TREE_CODE (exp))
	{
	case IDENTIFIER_NODE:
	  decl = lookup_name (exp);
	  if (decl == NULL)
	    {
	      if (LOOKUP_ONLY)
		return exp;
	      error ("undeclared identifier `%s'", IDENTIFIER_POINTER (exp));
	      return error_mark_node;
	    }
	  if (LOOKUP_ONLY)
	    return decl;
	  return safe_satisfy_decl (decl, chain);
	case TREE_LIST:
	  satisfy_list (exp, chain);
	  break;
	default:
	  ;
	}
      break;
    case 't':
      /* If TYPE_SIZE is non-NULL, exp and its subfields has already been
	 satified and laid out.  The exception is pointer and reference types,
	 which we layout before we lay out their TREE_TYPE. */
      if (TYPE_SIZE (exp) && TREE_CODE (exp) != POINTER_TYPE
	  && TREE_CODE (exp) != REFERENCE_TYPE)
	return exp;
      if (TYPE_MAIN_VARIANT (exp) != exp)
	SATISFY (TYPE_MAIN_VARIANT (exp));
      switch ((enum chill_tree_code)TREE_CODE (exp))
	{
	case LANG_TYPE:
	  {
	    tree d = TYPE_DOMAIN (exp);
	    tree t = satisfy (TREE_TYPE (exp), chain);
	    SATISFY (d);
	    /* It is possible that one of the above satisfy calls recursively
	       caused exp to be satisfied, in which case we're done. */
	    if (TREE_CODE (exp) != LANG_TYPE)
	      return exp;
	    TREE_TYPE (exp) = t;
	    TYPE_DOMAIN (exp) = d;
	    if (!LOOKUP_ONLY)
	      exp = smash_dummy_type (exp);
	  }
	  break;
	case ARRAY_TYPE:
	  SATISFY (TREE_TYPE (exp));
	  SATISFY (TYPE_DOMAIN (exp));
	  SATISFY (TYPE_ATTRIBUTES (exp));
	  if (!LOOKUP_ONLY)
	    CH_TYPE_NONVALUE_P (exp) = CH_TYPE_NONVALUE_P (TREE_TYPE (exp));
	  if (!TYPE_SIZE (exp)  && !LOOKUP_ONLY)
	    exp = layout_chill_array_type (exp);
	  break;
	case FUNCTION_TYPE:
	  SATISFY (TREE_TYPE (exp));
	  if (TREE_CODE_CLASS (TREE_CODE (TREE_TYPE (exp))) != 't'
	      && !LOOKUP_ONLY && TREE_CODE (TREE_TYPE (exp)) != ERROR_MARK)
	    {
	      error ("RETURNS spec with invalid mode");
	      TREE_TYPE (exp) = error_mark_node;
	    }
	  satisfy_list_values (TYPE_ARG_TYPES (exp), chain);
	  if (!TYPE_SIZE (exp)  && !LOOKUP_ONLY)
	    layout_type (exp);
	  break;
	case ENUMERAL_TYPE:
	  if (TYPE_SIZE (exp) == NULL_TREE && !LOOKUP_ONLY)
	    { tree pair;
	      /* FIXME:  Should this use satisfy_decl? */
	      for (pair = TYPE_VALUES (exp); pair; pair = TREE_CHAIN (pair))
		SATISFY (DECL_INITIAL (TREE_VALUE (pair)));
	      layout_enum (exp);
	    }
	  break;
	case INTEGER_TYPE:
	  SATISFY (TYPE_MIN_VALUE (exp));
	  SATISFY (TYPE_MAX_VALUE (exp));
	  if (TREE_TYPE (exp) != NULL_TREE)
	    { /* A range type */
	      if (TREE_TYPE (exp) != ridpointers[(int) RID_RANGE]
		  && TREE_TYPE (exp) != ridpointers[(int) RID_BIN]
		  && TREE_TYPE (exp) != string_index_type_dummy)
		SATISFY (TREE_TYPE (exp));
	      if (!TYPE_SIZE (exp)  && !LOOKUP_ONLY)
		exp = layout_chill_range_type (exp, 1);
	    }
	  break;
	case POINTER_TYPE:
	case REFERENCE_TYPE:
	  if (LOOKUP_ONLY)
	    SATISFY (TREE_TYPE (exp));
	  else
	    {
	      struct decl_chain *link;
	      int already_seen = 0;
	      for (link = chain; ; link = link->prev)
		{
		  if (link == NULL)
		    {	
		      struct decl_chain new_link;
		      new_link.decl = exp;
		      new_link.prev = chain;
		      TREE_TYPE (exp) = satisfy (TREE_TYPE (exp), &new_link);
		      break;
		    }
		  else if (link->decl == exp)
		    {
		      already_seen = 1;
		      break;
		    }
		}
	      if (!TYPE_SIZE (exp))
		{
		  layout_type (exp);
		  if (TREE_CODE (exp) == REFERENCE_TYPE)
		    CH_NOVELTY (exp) = CH_NOVELTY (TREE_TYPE (exp));
		  if (! already_seen)
		    {
		      tree valtype = TREE_TYPE (exp);
		      if (TREE_CODE_CLASS (TREE_CODE (valtype)) != 't')
			{
			  if (TREE_CODE (valtype) != ERROR_MARK)
			    error ("operand to REF is not a mode");
			  TREE_TYPE (exp) = error_mark_node;
			  return error_mark_node;
			}
		      else if (TREE_CODE (exp) == POINTER_TYPE
			       && TYPE_POINTER_TO (valtype) == NULL)
			TYPE_POINTER_TO (valtype) = exp;
		    }
		}
	    }
	  break;
	case RECORD_TYPE:
	  {
	    /* FIXME: detected errors in here will be printed as
	       often as this sequence runs. Find another way or
	       place to print the errors. */
	    /* if we have an ACCESS or TEXT mode we have to set
	       maximum_field_alignment to 0 to fit with runtime
	       system, even when we compile with -fpack. */
	    extern int maximum_field_alignment;
	    int save_maximum_field_alignment = maximum_field_alignment;

	    if (CH_IS_ACCESS_MODE (exp) || CH_IS_TEXT_MODE (exp))
	      maximum_field_alignment = 0;

	    for (decl = TYPE_FIELDS (exp); decl; decl = TREE_CHAIN (decl))
	      {
		SATISFY (TREE_TYPE (decl));
		if (!LOOKUP_ONLY)
		  {
		    /* if we have a UNION_TYPE here (variant structure), check for
		       non-value mode in it. This is not allowed (Z.200/pg. 33) */
		    if (TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE &&
			CH_TYPE_NONVALUE_P (TREE_TYPE (decl)))
		      {
			error ("field with non-value mode in variant structure not allowed");
			TREE_TYPE (decl) = error_mark_node;
		      }
		    /* RECORD_TYPE gets the non-value property if one of the
		       fields has the non-value property */
		    CH_TYPE_NONVALUE_P (exp) |= CH_TYPE_NONVALUE_P (TREE_TYPE (decl));
		  }
		if (TREE_CODE (decl) == CONST_DECL)
		  {
		    SATISFY (DECL_INITIAL (decl));
		    if (!LOOKUP_ONLY)
		      {
			if (CH_IS_BUFFER_MODE (exp) || CH_IS_EVENT_MODE (exp))
			  DECL_INITIAL (decl)
			    = check_queue_size (DECL_INITIAL (decl));
			else if (CH_IS_TEXT_MODE (exp) &&
				 DECL_NAME (decl) == get_identifier ("__textlength"))
			  DECL_INITIAL (decl)
			    = check_text_length (DECL_INITIAL (decl));
		      }
		  }
		else if (TREE_CODE (decl) == FIELD_DECL)
		  {
		    SATISFY (DECL_INITIAL (decl));
		  }
	      }
	    satisfy_list (TYPE_TAG_VALUES (exp), chain);
	    if (!TYPE_SIZE (exp)  && !LOOKUP_ONLY)
	      exp = layout_chill_struct_type (exp);
	    maximum_field_alignment = save_maximum_field_alignment;

	    /* perform some checks on nonvalue modes, they are record_mode's */
	    if (!LOOKUP_ONLY)
	      {
		if (CH_IS_BUFFER_MODE (exp))
		  {
		    tree elemmode = buffer_element_mode (exp);
		    if (elemmode != NULL_TREE && CH_TYPE_NONVALUE_P (elemmode))
		      {
			error ("buffer element mode must not have non-value property");
			invalidate_buffer_element_mode (exp);
		      }
		  }
		else if (CH_IS_ACCESS_MODE (exp))
		  {
		    tree recordmode = access_recordmode (exp);
		    if (recordmode != NULL_TREE && CH_TYPE_NONVALUE_P (recordmode))
		      {
			error ("recordmode must not have the non-value property");
			invalidate_access_recordmode (exp);
		      }
		  }
	      }
	  }
	  break;
	case SET_TYPE:
	  SATISFY (TYPE_DOMAIN (exp));
	  if (!TYPE_SIZE (exp)  && !LOOKUP_ONLY)
	    exp = layout_powerset_type (exp);
	  break;
	case UNION_TYPE:
	  for (decl = TYPE_FIELDS (exp); decl; decl = TREE_CHAIN (decl))
	    {
	      SATISFY (TREE_TYPE (decl));
	      if (!LOOKUP_ONLY)
		CH_TYPE_NONVALUE_P (exp) |= CH_TYPE_NONVALUE_P (TREE_TYPE (decl));
	    }
	  if (!TYPE_SIZE (exp)  && !LOOKUP_ONLY)
	    exp = layout_chill_variants (exp);
	  break;
	default:
	  ;
	}
    }
  return exp;
}
