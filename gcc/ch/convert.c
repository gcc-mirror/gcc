/* Language-level data type conversion for GNU CHILL.
   Copyright (C) 1992, 93, 1994, 1998 Free Software Foundation, Inc.

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


/* This file contains the functions for converting CHILL expressions
   to different data types.  The only entry point is `convert'.
   Every language front end must have a `convert' function
   but what kind of conversions it does will depend on the language.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "ch-tree.h"
#include "flags.h"
#include "convert.h"
#include "lex.h"
#include "toplev.h"
#include "output.h"

extern tree bit_one_node, bit_zero_node;
extern tree string_one_type_node;
extern tree bitstring_one_type_node;

static tree convert_to_reference	PARAMS ((tree, tree));
static tree convert_to_boolean		PARAMS ((tree, tree));
static tree convert_to_char		PARAMS ((tree, tree));
#if 0
static tree base_type_size_in_bytes	PARAMS ((tree));
#endif
static tree remove_tree_element		PARAMS ((tree, tree *));
static tree check_ps_range		PARAMS ((tree, tree, tree));
static tree digest_powerset_tuple	PARAMS ((tree, tree));
static tree digest_structure_tuple	PARAMS ((tree, tree));
static tree digest_array_tuple		PARAMS ((tree, tree, int));
static tree convert1			PARAMS ((tree, tree));

static tree
convert_to_reference (reftype, expr)
     tree reftype, expr;
{
  while (TREE_CODE (expr) == NOP_EXPR)  /* RETYPE_EXPR */
    expr = TREE_OPERAND (expr, 0);

  if (! CH_LOCATION_P (expr))
    error("internal error: trying to make loc-identity with non-location");
  else
    {
      mark_addressable (expr);
      return fold (build1 (ADDR_EXPR, reftype, expr));
    }

  return error_mark_node;
}

tree
convert_from_reference (expr)
     tree expr;
{
  tree e = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (expr)), expr);
  TREE_READONLY (e) = TREE_READONLY (expr);
  return e;
}

/* Convert EXPR to a boolean type.  */

static tree
convert_to_boolean (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  
  if (integer_zerop (expr))
    return boolean_false_node;
  if (integer_onep (expr))
    return boolean_true_node;

  /* Convert a singleton bitstring to a Boolean.
     Needed if flag_old_strings. */
  if (CH_BOOLS_ONE_P (intype))
    {
      if (TREE_CODE (expr) == CONSTRUCTOR)
	{
	  tree valuelist = TREE_OPERAND (expr, 1);
	  if (valuelist == NULL_TREE)
	    return boolean_false_node;
	  if (TREE_CHAIN (valuelist) == NULL_TREE
	      && TREE_PURPOSE (valuelist) == NULL_TREE
	      && integer_zerop (TREE_VALUE (valuelist)))
	    return boolean_true_node;
	}
      return build_chill_bitref (expr,
				 build_tree_list (NULL_TREE,
						  integer_zero_node));
    }

  if (INTEGRAL_TYPE_P (intype))
    return build1 (CONVERT_EXPR, type, expr);

  error ("cannot convert to a boolean mode");
  return boolean_false_node;
}

/* Convert EXPR to a char type.  */

static tree
convert_to_char (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum chill_tree_code form = TREE_CODE (intype);
  
  if (form == CHAR_TYPE)
    return build1 (NOP_EXPR, type, expr);

  /* Convert a singleton string to a char.
     Needed if flag_old_strings. */
  if (CH_CHARS_ONE_P (intype))
    {
      if (TREE_CODE (expr) == STRING_CST)
	{
	  expr = build_int_2 ((unsigned char)TREE_STRING_POINTER(expr)[0], 0);
	  TREE_TYPE (expr) = char_type_node;
	  return expr;
	}
      else
	return build (ARRAY_REF, char_type_node, expr, integer_zero_node);

    }

  /* For now, assume it will always fit */
  if (form == INTEGER_TYPE)
    return build1 (CONVERT_EXPR, type, expr);

  error ("cannot convert to a char mode");

  {
    register tree tem = build_int_2 (0, 0);
    TREE_TYPE (tem) = type;
    return tem;
  }
}

#if 0
static tree
base_type_size_in_bytes (type)
     tree type;
{
  if (type == NULL_TREE
      || TREE_CODE (type) == ERROR_MARK
      || TREE_CODE (type) != ARRAY_TYPE)
    return error_mark_node;
  return size_in_bytes (TREE_TYPE (type));
}
#endif

/*
 * build a singleton array type, of TYPE objects.
 */
tree
build_array_type_for_scalar (type)
     tree type;
{
  /* KLUDGE */
  if (type == char_type_node)
    return build_string_type (type, integer_one_node);

  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;

  return build_chill_array_type
    (type,
     tree_cons (NULL_TREE,
		build_chill_range_type (NULL_TREE,
					integer_zero_node, integer_zero_node),
		NULL_TREE),
     0, NULL_TREE);

}

#if 0
static tree
unreferenced_type_of (type)
     tree type;
{
  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;
  while (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);
  return type;
}
#endif


/* Remove from *LISTP the first TREE_LIST node whose TREE_PURPOSE == KEY.
   Return the TREE_LIST node, or NULL_TREE on failure. */

static tree
remove_tree_element (key, listp)
     tree *listp;
     tree key;
{
  tree node = *listp;
  for ( ; node; listp = &TREE_CHAIN (node), node = *listp)
    {
      if (TREE_PURPOSE (node) == key)
	{
	  *listp = TREE_CHAIN (node);
	  TREE_CHAIN (node) = NULL_TREE;
	  return node;
	}
    }
  return NULL_TREE;
}

/* This is quite the same as check_range in actions.c, but with
   different error message. */

static tree
check_ps_range (value, lo_limit, hi_limit)
     tree value;
     tree lo_limit;
     tree hi_limit;
{
  tree check = test_range (value, lo_limit, hi_limit);

  if (!integer_zerop (check))
    {
      if (TREE_CODE (check) == INTEGER_CST)
	{
	  error ("powerset tuple element out of range");
	  return error_mark_node;
	}
      else
	value = check_expression (value, check,
				  ridpointers[(int) RID_RANGEFAIL]);
    }
  return value;
}

static tree
digest_powerset_tuple (type, inits)
     tree type;
     tree inits;
{
  tree list;
  tree result;
  tree domain = TYPE_DOMAIN (type);
  int i = 0;
  int is_erroneous = 0, is_constant = 1, is_simple = 1;
  if (domain == NULL_TREE || TREE_CODE (domain) == ERROR_MARK)
    return error_mark_node;
  for (list = TREE_OPERAND (inits, 1);  list; list = TREE_CHAIN (list), i++)
    {
      tree val = TREE_VALUE (list);
      if (TREE_CODE (val) == ERROR_MARK)
	{
	  is_erroneous = 1;
	  continue;
	}
      if (!TREE_CONSTANT (val))
	is_constant = 0;
      else if (!initializer_constant_valid_p (val, TREE_TYPE (val)))
	is_simple = 0;
      if (! CH_COMPATIBLE (val, domain))
	{
	  error ("incompatible member of powerset tuple (at position #%d)", i);
	  is_erroneous = 1;
	  continue;
	}
      /* check range of value */
      val = check_ps_range (val, TYPE_MIN_VALUE (domain),
			    TYPE_MAX_VALUE (domain));
      if (TREE_CODE (val) == ERROR_MARK)
	{
	  is_erroneous = 1;
	  continue;
	}

      /* Updating the list in place is in principle questionable,
	 but I can't think how it could hurt. */
      TREE_VALUE (list) = convert (domain, val);

      val = TREE_PURPOSE (list);
      if (val == NULL_TREE)
	continue;

      if (TREE_CODE (val) == ERROR_MARK)
	{
	  is_erroneous = 1;
	  continue;
	}
      if (! CH_COMPATIBLE (val, domain))
	{
	  error ("incompatible member of powerset tuple (at position #%d)", i);
	  is_erroneous = 1;
	  continue;
	}
      val = check_ps_range (val, TYPE_MIN_VALUE (domain),
			    TYPE_MAX_VALUE (domain));
      if (TREE_CODE (val) == ERROR_MARK)
	{
	  is_erroneous = 1;
	  continue;
	}
      TREE_PURPOSE (list) = convert (domain, val);
      if (!TREE_CONSTANT (val))
	is_constant = 0;
      else if (!initializer_constant_valid_p (val, TREE_TYPE (val)))
	is_simple = 0;
    }
  result = build (CONSTRUCTOR, type, NULL_TREE, TREE_OPERAND (inits, 1));
  if (is_erroneous)
    return error_mark_node;
  if (is_constant)
    TREE_CONSTANT (result) = 1;
  if (is_constant && is_simple)
    TREE_STATIC (result) = 1;
  return result;
}

static tree
digest_structure_tuple (type, inits)
     tree type;
     tree inits;
{
  tree elements = CONSTRUCTOR_ELTS (inits);
  tree values = NULL_TREE;
  int is_constant = 1;
  int is_simple = 1;
  int is_erroneous = 0;
  tree field;
  int labelled_elements = 0;
  int unlabelled_elements = 0;
  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (TREE_TYPE (field)) != UNION_TYPE)
	{ /* Regular fixed field. */
	  tree value = remove_tree_element (DECL_NAME (field), &elements);

	  if (value)
	    labelled_elements++;
	  else if (elements && TREE_PURPOSE (elements) == NULL_TREE)
	    {
	      value = elements;
	      elements = TREE_CHAIN (elements);
	      unlabelled_elements++;
	    }

	  if (value)
	    {
	      tree val;
	      char msg[120];
	      sprintf (msg, "initializer for field `%.80s'",
		       IDENTIFIER_POINTER (DECL_NAME (field)));
	      val = chill_convert_for_assignment (TREE_TYPE (field),
						  TREE_VALUE (value), msg);
	      if (TREE_CODE (val) == ERROR_MARK)
		  is_erroneous = 1;
	      else
		{
		  TREE_VALUE (value) = val;
		  TREE_CHAIN (value) = values;
		  TREE_PURPOSE (value) = field;
		  values = value;	
		  if (TREE_CODE (val) == ERROR_MARK)
		    is_erroneous = 1;
		  else if (!TREE_CONSTANT (val))
		    is_constant = 0;
		  else if (!initializer_constant_valid_p (val,
							  TREE_TYPE (val)))
		    is_simple = 0;
		}
	    }
	  else
	    {
	      pedwarn ("no initializer value for fixed field `%s'",
		       IDENTIFIER_POINTER (DECL_NAME (field)));
	    }
	}
      else
	{
	  tree variant;
	  tree selected_variant = NULL_TREE;
	  tree variant_values = NULL_TREE;

	  /* In a tagged variant structure mode, try to figure out
	     (from the fixed fields), which is the selected variant. */
	  if (TYPE_TAGFIELDS (TREE_TYPE (field)))
	    {
	      for (variant = TYPE_FIELDS (TREE_TYPE (field));
		   variant; variant = TREE_CHAIN (variant))
		{
		  tree tag_labels = TYPE_TAG_VALUES (TREE_TYPE (variant));
		  tree tag_fields = TYPE_TAGFIELDS (TREE_TYPE (field));
		  if (DECL_NAME (variant) == ELSE_VARIANT_NAME)
		    {
		      selected_variant = variant;
		      break;
		    }
		  for (; tag_labels && tag_fields;
		       tag_labels = TREE_CHAIN (tag_labels),
		       tag_fields = TREE_CHAIN (tag_fields))
		    {
		      tree tag_value = values;
		      int found = 0;
		      tree tag_decl = TREE_VALUE (tag_fields);
		      tree tag_value_set = TREE_VALUE (tag_labels);
		      for ( ; tag_value; tag_value = TREE_CHAIN (tag_value))
			{
			  if (TREE_PURPOSE (tag_value) == tag_decl)
			    {
			      tag_value = TREE_VALUE (tag_value);
			      break;
			    }
			}
		      if (!tag_value || TREE_CODE (tag_value) != INTEGER_CST)
			{
			  pedwarn ("non-constant value for tag field `%s'",
				   IDENTIFIER_POINTER (DECL_NAME (tag_decl)));
			  goto get_values;
			}

		      /* Check if the value of the tag (as given in a
			 previous field) matches the case label list. */
		      for (; tag_value_set;
			   tag_value_set = TREE_CHAIN (tag_value_set))
			{
			  if (tree_int_cst_equal (TREE_VALUE (tag_value_set),
						  tag_value))
			    {
			      found = 1;
			      break;
			    }
			}
		      if (!found)
			break;
		    }
		  if (!tag_fields)
		    {
		      selected_variant = variant;
		      break;
		    }
		}
	    }
	get_values:
	  for (variant = TYPE_FIELDS (TREE_TYPE (field));
	       variant; variant = TREE_CHAIN (variant))
	    {
	      tree vfield0 = TYPE_FIELDS (TREE_TYPE (variant)); 
	      tree vfield;
	      for (vfield = vfield0; vfield;  vfield = TREE_CHAIN (vfield))
		{
		  tree value = remove_tree_element (DECL_NAME (vfield),
						    &elements);

		  if (value)
		    labelled_elements++;
		  else if (variant == selected_variant
			   && elements && TREE_PURPOSE (elements) == NULL_TREE)
		    {
		      value = elements;
		      elements = TREE_CHAIN (elements);
		      unlabelled_elements++;
		    }

		  if (value)
		    {
		      if (selected_variant && selected_variant != variant)
			{
			  error ("field `%s' in wrong variant",
				 IDENTIFIER_POINTER (DECL_NAME (vfield)));
			  is_erroneous = 1;
			}
		      else
			{
			  if (!selected_variant && vfield != vfield0)
			    pedwarn ("missing variant fields (at least `%s')",
				     IDENTIFIER_POINTER (DECL_NAME (vfield0)));
			  selected_variant = variant;
			  if (CH_COMPATIBLE (TREE_VALUE (value),
					     TREE_TYPE (vfield)))
			    {
			      tree val = convert (TREE_TYPE (vfield),
						  TREE_VALUE (value));
			      TREE_PURPOSE (value) = vfield;
			      TREE_VALUE (value) = val;
			      TREE_CHAIN (value) = variant_values;
			      variant_values = value;
			      if (TREE_CODE (val) == ERROR_MARK)
				is_erroneous = 1;
			      else if (!TREE_CONSTANT (val))
				is_constant = 0;
			      else if (!initializer_constant_valid_p
				       (val, TREE_TYPE (val)))
				is_simple = 0;
			    }
			  else
			    {
			      is_erroneous = 1;
			      error ("bad initializer for field `%s'",
				     IDENTIFIER_POINTER (DECL_NAME (vfield)));
			    }
			}
		    }
		  else if (variant == selected_variant)
		    {
		      pedwarn ("no initializer value for variant field `%s'",
			       IDENTIFIER_POINTER (DECL_NAME (field)));
		    }
		}
	    }
	  if (selected_variant == NULL_TREE)
	    pedwarn ("no selected variant");
	  else
	    {
	      variant_values = build (CONSTRUCTOR,
				      TREE_TYPE (selected_variant),
				      NULL_TREE, nreverse (variant_values));
	      variant_values
		= build (CONSTRUCTOR, TREE_TYPE (field), NULL_TREE,
			 build_tree_list (selected_variant, variant_values));
	      values = tree_cons (field, variant_values, values);
	    }
	}
    }

  if (labelled_elements && unlabelled_elements)
    pedwarn ("mixture of labelled and unlabelled tuple elements");

  /* Check for unused initializer elements. */
  unlabelled_elements = 0;
  for ( ; elements != NULL_TREE; elements = TREE_CHAIN (elements))
    {
      if (TREE_PURPOSE (elements) == NULL_TREE)
	unlabelled_elements++;
      else
	{
	  if (IDENTIFIER_POINTER (TREE_PURPOSE (elements)) == 0)
	    error ("probably not a structure tuple");
	  else
	    error ("excess initializer for field `%s'",
		   IDENTIFIER_POINTER (TREE_PURPOSE (elements)));
	  is_erroneous = 1;
	}
    }
  if (unlabelled_elements)
    {
      error ("excess unnamed initializers");
      is_erroneous = 1;
    }

  CONSTRUCTOR_ELTS (inits) = nreverse (values);
  TREE_TYPE (inits) = type;
  if (is_erroneous)
    return error_mark_node;
  if (is_constant)
    TREE_CONSTANT (inits) = 1;
  if (is_constant && is_simple)
    TREE_STATIC (inits) = 1;
  return inits;
}

/* Return a Chill representation of the INTEGER_CST VAL.
   The result may be in a static buffer, */

const char *
display_int_cst (val)
     tree val;
{
  static char buffer[50];
  HOST_WIDE_INT x;
  tree fields;
  if (TREE_CODE (val) != INTEGER_CST)
    return "<not a constant>";

  x = TREE_INT_CST_LOW (val);

  switch (TREE_CODE (TREE_TYPE (val)))
    {
    case BOOLEAN_TYPE:
      if (x == 0)
	return "FALSE";
      if (x == 1)
	return "TRUE";
      goto int_case;
    case CHAR_TYPE:
      if (x == '^')
	strcpy (buffer, "'^^'");
      else if (x == '\n')
	strcpy (buffer, "'^J'");
      else if (x < ' ' || x > '~')
	sprintf (buffer, "'^(%u)'", (unsigned int) x);
      else
	sprintf (buffer, "'%c'", (char) x);
      return buffer;
    case ENUMERAL_TYPE:
      for (fields = TYPE_VALUES (TREE_TYPE (val)); fields != NULL_TREE;
	   fields = TREE_CHAIN (fields))
	{
	  if (tree_int_cst_equal (TREE_VALUE (fields), val))
	    return IDENTIFIER_POINTER (TREE_PURPOSE (fields));
	}
      goto int_case;
    case POINTER_TYPE:
      if (x == 0)
	return "NULL";
      goto int_case;
    int_case:
    default:
      /* This code is derived from print-tree.c:print_code_brief. */
      if (TREE_INT_CST_HIGH (val) == 0)
	sprintf (buffer,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		 "%1u",
#else
		 "%1lu",
#endif
		 x);
      else if (TREE_INT_CST_HIGH (val) == -1 && TREE_INT_CST_LOW (val) != 0)
	sprintf (buffer,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		 "-%1u",
#else
		 "-%1lu",
#endif
		 -x);
      else
	sprintf (buffer,
#if HOST_BITS_PER_WIDE_INT == 64
#if HOST_BITS_PER_WIDE_INT != HOST_BITS_PER_INT
		 "H'%lx%016lx",
#else
		 "H'%x%016x",
#endif
#else
#if HOST_BITS_PER_WIDE_INT != HOST_BITS_PER_INT
		 "H'%lx%08lx",
#else
		 "H'%x%08x",
#endif
#endif
		 TREE_INT_CST_HIGH (val), TREE_INT_CST_LOW (val));
      return buffer;
    }
}

static tree
digest_array_tuple (type, init, allow_missing_elements)
     tree type;
     tree init;
     int allow_missing_elements;
{
  tree element = CONSTRUCTOR_ELTS (init);
  int is_constant = 1;
  int is_simple = 1;
  tree element_type = TREE_TYPE (type);
  tree default_value = NULL_TREE;
  tree element_list = NULL_TREE;
  tree domain_min;
  tree domain_max;
  tree *ptr = &element_list;
  int errors = 0;
  int labelled_elements = 0;
  int unlabelled_elements = 0;
  tree first, last = NULL_TREE;

  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;

  domain_min = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
  domain_max = TYPE_MAX_VALUE (TYPE_DOMAIN (type));

  if (domain_min == NULL || TREE_CODE (domain_min) != INTEGER_CST)
    {
      error ("non-constant start index for tuple");
      return error_mark_node;
    }
  if (TREE_CODE (domain_max) != INTEGER_CST)
    is_constant = 0;

  if (TREE_CODE (type) != ARRAY_TYPE)
    abort ();  

  for ( ; element != NULL_TREE; element = TREE_CHAIN (element))
    {
      tree purpose = TREE_PURPOSE (element);
      tree value   = TREE_VALUE (element);

      if (purpose == NULL_TREE)
	{
	  if (last == NULL_TREE)
	    first = domain_min;
	  else
	    {
	      HOST_WIDE_INT new_lo, new_hi;
	      add_double (TREE_INT_CST_LOW (last), TREE_INT_CST_HIGH (last),
			  1, 0,
			  &new_lo, &new_hi);
	      first = build_int_2 (new_lo, new_hi);
	      TREE_TYPE (first) = TYPE_DOMAIN (type);
	    }
	  last = first;
	  unlabelled_elements++;
	}
      else
	{
	  labelled_elements++;
	  if (TREE_CODE (purpose) == INTEGER_CST)
	    first = last = purpose;
	  else if (TREE_CODE (purpose) == TYPE_DECL
		   && discrete_type_p (TREE_TYPE (purpose)))
	    {
	      first = TYPE_MIN_VALUE (TREE_TYPE (purpose));
	      last = TYPE_MAX_VALUE (TREE_TYPE (purpose));
	    }
	  else if (TREE_CODE (purpose) != RANGE_EXPR)
	    {
	      error ("invalid array tuple label");
	      errors++;
	      continue;
	    }
	  else if (TREE_OPERAND (purpose, 0) == NULL_TREE)
	    first = last = NULL_TREE;  /* Default value. */
	  else
	    {
	      first = TREE_OPERAND (purpose, 0);
	      last = TREE_OPERAND (purpose, 1);
	    }
	  if ((first != NULL && TREE_CODE (first) != INTEGER_CST)
	      || (last != NULL && TREE_CODE (last) != INTEGER_CST))
	    {
	      error ("non-constant array tuple index range");
	      errors++;
	    }
	}

      if (! CH_COMPATIBLE (value, element_type))
	{
	  const char *err_val_name =
	    first ? display_int_cst (first) : "(default)";
	  error ("incompatible array tuple element %s", err_val_name);
	  value = error_mark_node;
	}
      else
	value = convert (element_type, value);
      if (TREE_CODE (value) == ERROR_MARK)
	errors++;
      else if (!TREE_CONSTANT (value))
	is_constant = 0;
      else if (!initializer_constant_valid_p (value, TREE_TYPE (value)))
	is_simple = 0;

      if (first == NULL_TREE)
	{
	  if (default_value != NULL)
	    {
	      error ("multiple (*) or (ELSE) array tuple labels");
	      errors++;
	    }
	  default_value = value;
	  continue;
	}

      if (first != last && tree_int_cst_lt (last, first))
	{
	  error ("empty range in array tuple");
	  errors++;
	  continue;
	}

      ptr = &element_list;

#define MAYBE_RANGE_OP(PURPOSE, OPNO) \
  (TREE_CODE (PURPOSE) == RANGE_EXPR ? TREE_OPERAND (PURPOSE, OPNO): PURPOSE)
#define CONSTRUCTOR_ELT_LO(ELT) MAYBE_RANGE_OP (TREE_PURPOSE (ELT), 0)
#define CONSTRUCTOR_ELT_HI(ELT) MAYBE_RANGE_OP (TREE_PURPOSE (ELT), 1)
      while (*ptr && tree_int_cst_lt (last,
				      CONSTRUCTOR_ELT_LO (*ptr)))
	ptr = &TREE_CHAIN (*ptr);
      if (*ptr && ! tree_int_cst_lt (CONSTRUCTOR_ELT_HI (*ptr), first))
	{
	  const char *err_val_name = display_int_cst (first);
	  error ("array tuple has duplicate index %s", err_val_name);
	  errors++;
	  continue;
	}
      if ((ptr == &element_list && tree_int_cst_lt (domain_max, last))
	|| (*ptr == NULL_TREE && tree_int_cst_lt (first, domain_min)))
	{
	  if (purpose)
	    error ("array tuple index out of range");
	  else if (errors == 0)
	    error ("too many array tuple values");
	  errors++;
	  continue;
	}
      if (! tree_int_cst_lt (first, last))
	purpose = first;
      else if (purpose == NULL_TREE || TREE_CODE (purpose) != RANGE_EXPR)
	purpose = build_nt (RANGE_EXPR, first, last);
      *ptr = tree_cons (purpose, value, *ptr);
    }

  element_list = nreverse (element_list);

  /* For each missing element, set it to the default value,
     if there is one.  Otherwise, emit an error.  */

  if (errors == 0
      && (!allow_missing_elements || default_value != NULL_TREE))
    {
      /* Iterate over each *gap* between specified elements/ranges. */
      tree prev_elt;
      if (element_list &&
	  tree_int_cst_equal (CONSTRUCTOR_ELT_LO (element_list), domain_min))
	{
	  ptr = &TREE_CHAIN (element_list);
	  prev_elt = element_list;
	}
      else
	{
	  prev_elt = NULL_TREE;
	  ptr = &element_list;
	}
      for (;;)
	{
	  tree first, last;
	  /* Calculate the first element of the gap. */
	  if (prev_elt == NULL_TREE)
	    first = domain_min;
	  else
	    {
	      first = CONSTRUCTOR_ELT_HI (prev_elt);
	      if (tree_int_cst_equal (first, domain_max))
		break; /* We're done.  Avoid overflow below. */
	      first = copy_node (first);
	      add_double (TREE_INT_CST_LOW (first), TREE_INT_CST_HIGH (first),
			  1, 0,
			  &TREE_INT_CST_LOW (first),
			  &TREE_INT_CST_HIGH (first));
	    }
	  /* Calculate the last element of the gap. */
	  if (*ptr)
	    {
	      /* Actually end up with correct type. */
	      last = size_binop (MINUS_EXPR,
				 CONSTRUCTOR_ELT_LO (*ptr),
				 integer_one_node);
	    }
	  else
	    last = domain_max;
	  if (TREE_CODE (last) == INTEGER_CST && tree_int_cst_lt (last, first))
	    ; /* Empty "gap" - no missing elements. */
	  else if (default_value)
	    {
	      tree purpose;
	      if (tree_int_cst_equal (first, last))
		purpose = first;
	      else
		purpose = build_nt (RANGE_EXPR, first, last);
	      *ptr = tree_cons (purpose, default_value, *ptr);
	    }
	  else
	    {
	      const char *err_val_name = display_int_cst (first);
	      if (TREE_CODE (last) != INTEGER_CST)
		error ("dynamic array tuple without (*) or (ELSE)");
	      else if (tree_int_cst_equal (first, last))
		error ("missing array tuple element %s", err_val_name);
	      else
		{
		  char *first_name = (char *)
		    xmalloc (strlen (err_val_name) + 1);
		  strcpy (first_name, err_val_name);
		  err_val_name = display_int_cst (last);
		  error ("missing array tuple elements %s : %s",
			 first_name, err_val_name);
		  free (first_name);
		}
	      errors++;
	    }
	  if (*ptr == NULL_TREE)
	    break;
	  prev_elt = *ptr;
	  ptr = &TREE_CHAIN (*ptr);
	}
    }
  if (errors)
    return error_mark_node;

  element = build (CONSTRUCTOR, type, NULL_TREE, element_list);
  TREE_CONSTANT (element) = is_constant;
  if (is_constant && is_simple)
    TREE_STATIC (element) = 1;
  if (labelled_elements && unlabelled_elements)
    pedwarn ("mixture of labelled and unlabelled tuple elements");
  return element;
}

/* This function is needed because no-op CHILL conversions are not fully
   understood by the initialization machinery.  This function should only
   be called when a conversion truly is a no-op.  */

static tree
convert1 (type, expr)
     tree type, expr;
{
  int was_constant = TREE_CONSTANT (expr);
  STRIP_NOPS (expr);
  was_constant |= TREE_CONSTANT (expr);
  expr = copy_node (expr);
  TREE_TYPE (expr) = type;
  if (TREE_CONSTANT (expr) != was_constant) abort ();
  TREE_CONSTANT (expr) = was_constant;
  return expr;
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.

   In CHILL, we assume that the type is Compatible with the
   Class of expr, and generally complain otherwise.
   However, convert is more general (e.g. allows enum<->int
   conversion), so there should probably be at least two routines.
   Maybe add something like convert_for_assignment.  FIXME. */

tree
convert (type, expr)
     tree type, expr;
{
  register tree e = expr;
  register enum chill_tree_code code;
  int type_varying;

  if (e == NULL_TREE || TREE_CODE (e) == ERROR_MARK)
    return error_mark_node;

  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;

  code = TREE_CODE (type);

  if (type == TREE_TYPE (e))
    return e;

  if (TREE_TYPE (e) != NULL_TREE
      && TREE_CODE (TREE_TYPE (e)) == REFERENCE_TYPE)
    e = convert_from_reference (e);

  /* Support for converting *to* a reference type is limited;
     it is only here as a convenience for loc-identity declarations,
     and loc parameters. */
  if (code == REFERENCE_TYPE)
    return convert_to_reference (type, e);

  /* if expression was untyped because of its context (an if_expr or case_expr
     in a tuple, perhaps) just apply the type */
  if (TREE_TYPE (e) && TREE_CODE (TREE_TYPE (e)) == ERROR_MARK)
    {
      TREE_TYPE (e) = type;
      return e;
    }

  /* Turn a NULL keyword into [0, 0] for an instance */
  if (CH_IS_INSTANCE_MODE (type) && expr == null_pointer_node)
    {
      tree field0 = TYPE_FIELDS (type);
      tree field1 = TREE_CHAIN (field0);
      e = build (CONSTRUCTOR, type, NULL_TREE,
		 tree_cons (field0, integer_zero_node,
			    tree_cons (field1, integer_zero_node,
				       NULL_TREE)));
      TREE_CONSTANT (e) = 1;
      TREE_STATIC (e) = 1;
      return e;
    }

  /* Turn a pointer into a function pointer for a procmode */
  if (TREE_CODE (type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE
      && expr == null_pointer_node)
    return convert1 (type, expr);

  /* turn function_decl expression into a pointer to 
     that function */
  if (TREE_CODE (expr) == FUNCTION_DECL
      && TREE_CODE (type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    {
      e = build1 (ADDR_EXPR, type, expr);
      TREE_CONSTANT (e) = 1;
      return e;
    }

  if (TREE_TYPE (e) && TREE_CODE (TREE_TYPE (e)) == RECORD_TYPE)
    e = varying_to_slice (e);
  type_varying   = chill_varying_type_p (type);

  /* Convert a char to a singleton string.
     Needed for compatibility with 1984 version of Z.200. */
  if (TREE_TYPE (e) && TREE_CODE (TREE_TYPE (e)) == CHAR_TYPE
      && (CH_CHARS_ONE_P (type) || type_varying))
    {
      if (TREE_CODE (e) == INTEGER_CST)
	{
	  char ch = TREE_INT_CST_LOW (e);
	  e = build_chill_string (1, &ch);
	}
      else
	e = build (CONSTRUCTOR, string_one_type_node, NULL_TREE,
		   tree_cons (NULL_TREE, e, NULL_TREE));
    }

  /* Convert a Boolean to a singleton bitstring.
     Needed for compatibility with 1984 version of Z.200. */
  if (TREE_TYPE (e) && TREE_CODE (TREE_TYPE (e)) == BOOLEAN_TYPE
      && (CH_BOOLS_ONE_P (type) || type_varying))
    {
      if (TREE_CODE (e) == INTEGER_CST)
	e = integer_zerop (e) ? bit_zero_node : bit_one_node;
      else
	e = build (COND_EXPR, bitstring_one_type_node,
		   e, bit_one_node, bit_zero_node);
    }

  if (type_varying)
    {
      tree nentries;
      tree field0 = TYPE_FIELDS (type);
      tree field1 = TREE_CHAIN (field0);
      tree orig_e = e;
      tree target_array_type = TREE_TYPE (field1);
      tree needed_padding;
      tree padding_max_size = 0;
      int orig_e_constant = TREE_CONSTANT (orig_e);
      if (TREE_TYPE (e) != NULL_TREE
	  && TREE_CODE (TREE_TYPE (e)) == ARRAY_TYPE)
	{
	  /* Note that array_type_nelts returns 1 less than the size. */
	  nentries = array_type_nelts (TREE_TYPE (e));
	  needed_padding = size_binop (MINUS_EXPR,
				       array_type_nelts (target_array_type),
				       nentries);
	  if (TREE_CODE (needed_padding) != INTEGER_CST)
	    {
	      padding_max_size = size_in_bytes (TREE_TYPE (e));
	      if (TREE_CODE (padding_max_size) != INTEGER_CST)
		padding_max_size = TYPE_ARRAY_MAX_SIZE (TREE_TYPE (e));
	    }
	  nentries = size_binop (PLUS_EXPR, nentries, integer_one_node);
	}
      else if (TREE_CODE (e) == CONSTRUCTOR)
	{
	  HOST_WIDE_INT init_cnt = 0;
	  tree chaser = CONSTRUCTOR_ELTS (e);
	  for ( ; chaser; chaser = TREE_CHAIN (chaser))
	    init_cnt++;               /* count initializer elements */
	  nentries = build_int_2 (init_cnt, 0);
	  needed_padding = integer_zero_node;
	  if (TREE_TYPE (e) == NULL_TREE)
	    e = digest_array_tuple (TREE_TYPE (field1), e, 1);
	  orig_e_constant = TREE_CONSTANT (e);
	}
      else
	{
	  error ("initializer is not an array or string mode");
	  return error_mark_node;
	}
#if 0
      FIXME check that nentries will fit in type;
#endif
      if (!integer_zerop (needed_padding))
	{
	  tree padding, padding_type, padding_range;
	  if (TREE_CODE (needed_padding) == INTEGER_CST
	      && (long)TREE_INT_CST_LOW (needed_padding) < 0)
	    {
	      error ("destination is too small");
	      return error_mark_node;
	    }
	  padding_range = build_chill_range_type (NULL_TREE, integer_one_node,
						  needed_padding);
	  padding_type
	    = build_simple_array_type (TREE_TYPE (target_array_type),
				       padding_range, NULL_TREE);
	  TYPE_ARRAY_MAX_SIZE (padding_type) = padding_max_size;
	  if (CH_CHARS_TYPE_P (target_array_type))
	    MARK_AS_STRING_TYPE (padding_type);
	  padding = build (UNDEFINED_EXPR, padding_type);
	  if (TREE_CONSTANT (e))
	    e = build_chill_binary_op (CONCAT_EXPR, e, padding);
	  else
	    e = build (CONCAT_EXPR, target_array_type, e, padding);
	}
      e = convert (TREE_TYPE (field1), e);
      /* We build this constructor by hand (rather than going through
	 digest_structure_tuple), to avoid some type-checking problem.
	 E.g. type may have non-null novelty, but its field1 will
	 have non-novelty. */
      e = build (CONSTRUCTOR, type, NULL_TREE,
		    tree_cons (field0, nentries,
			       build_tree_list (field1, e)));
      /* following was wrong, cause orig_e never will be TREE_CONSTANT. e
	 may become constant after digest_array_tuple. */
      if (TREE_CONSTANT (nentries) && orig_e_constant) /* TREE_CONSTANT (orig_e)) */
	{
	  TREE_CONSTANT (e) = 1;
	  if (TREE_STATIC (nentries) && TREE_STATIC (orig_e))
	    TREE_STATIC (e) = 1;
	}
    }
  if (TREE_TYPE (e) == NULL_TREE)
    {
      if (TREE_CODE (e) == CONSTRUCTOR)
	{
	  if (TREE_CODE (type) == SET_TYPE)
	    return digest_powerset_tuple (type, e);
	  if (TREE_CODE (type) == RECORD_TYPE)
	    return digest_structure_tuple (type, e);
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    return digest_array_tuple (type, e, 0);
	  fatal ("internal error - bad CONSTRUCTOR passed to convert");
	}
      else if (TREE_CODE (e) == COND_EXPR)
	e = build (COND_EXPR, type,
		   TREE_OPERAND (e, 0),
		   convert (type, TREE_OPERAND (e, 1)),
		   convert (type, TREE_OPERAND (e, 2)));
      else if (TREE_CODE (e) == CASE_EXPR)
	TREE_TYPE (e) = type;
      else
	{
	  error ("internal error:  unknown type of expression");
	  return error_mark_node;
	}
    }

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (e))
      || (CH_NOVELTY (type) != NULL_TREE
	  && CH_NOVELTY (type) == CH_NOVELTY (TREE_TYPE (e))))
    return convert1 (type, e);

  if (TREE_CODE (TREE_TYPE (e)) == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (code == VOID_TYPE)
    return build1 (CONVERT_EXPR, type, e);

  if (code == SET_TYPE)
    return convert1 (type, e);

  if (code == INTEGER_TYPE || code == ENUMERAL_TYPE)
    {
      if (flag_old_strings)
	{
	  if (CH_CHARS_ONE_P (TREE_TYPE (e)))
	    e = convert_to_char (char_type_node, e);
	  else if (CH_BOOLS_ONE_P (TREE_TYPE (e)))
	    e = convert_to_boolean (boolean_type_node, e);
	}
      return fold (convert_to_integer (type, e));
    }
  if (code == POINTER_TYPE)
    return fold (convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, e));
  if (code == BOOLEAN_TYPE)
    return fold (convert_to_boolean (type, e));
  if (code == CHAR_TYPE)
    return fold (convert_to_char (type, e));

  if (code == ARRAY_TYPE && TYPE_MODE (type) != TYPE_MODE (TREE_TYPE (e)))
    {
      /* The mode of the expression is different from that of the type.
	 Earlier checks should have tested against different lengths.
	 But even if the lengths are the same, it is possible that one
	 type is a static type (and hence could be say SImode), while the
	 other type is dynamic type (and hence is BLKmode).
	 This causes problems when emitting instructions.  */
      tree ee = build1 (INDIRECT_REF, type,
			build1 (NOP_EXPR, build_pointer_type (type),
				build1 (ADDR_EXPR,
					build_pointer_type (TREE_TYPE (e)),
					e)));
      TREE_READONLY (ee) = TYPE_READONLY (type);
      return ee;
    }

  /* The default! */
  return convert1 (type, e);
}

/* Return an expression whose value is EXPR, but whose class is CLASS. */

tree
convert_to_class (class, expr)
     struct ch_class class;
     tree expr;
{
  switch (class.kind)
    {
    case CH_NULL_CLASS:
    case CH_ALL_CLASS:
      return expr;
    case CH_DERIVED_CLASS:
      if (TREE_TYPE (expr) != class.mode)
	expr = convert (class.mode, expr);
      if (!CH_DERIVED_FLAG (expr))
	{
	  expr = copy_node (expr);
	  CH_DERIVED_FLAG (expr) = 1;
	}
      return expr;
    case CH_VALUE_CLASS:
    case CH_REFERENCE_CLASS:
      if (TREE_TYPE (expr) != class.mode)
	expr = convert (class.mode, expr);
      if (CH_DERIVED_FLAG (expr))
	{
	  expr = copy_node (expr);
	  CH_DERIVED_FLAG (expr) = 0;
	}
      return expr;
    }
  return expr;
}
