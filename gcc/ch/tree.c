/* Language-dependent node constructors for parse phase of GNU compiler.
   Copyright (C) 1992, 93, 94, 98, 99, 2000  Free Software Foundation, Inc.

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
#include "obstack.h"
#include "tree.h"
#include "ch-tree.h"
#include "toplev.h"

/* Here is how primitive or already-canonicalized types' 
   hash codes are made.  */
#define TYPE_HASH(TYPE) ((HOST_WIDE_INT) (TYPE) & 0777777)

extern struct obstack permanent_obstack;
/* This is special sentinel used to communicate from build_string_type
   to layout_chill_range_type for the index range of a string. */
tree string_index_type_dummy;

static tree make_powerset_type				PARAMS ((tree));

/* Build a chill string type.
   For a character string, ELT_TYPE==char_type_node; 
   for a bit-string, ELT_TYPE==boolean_type_node. */

tree
build_string_type (elt_type, length)
     tree elt_type;
     tree length;
{
  register tree t;

  if (TREE_CODE (elt_type) == ERROR_MARK || TREE_CODE (length) == ERROR_MARK)
    return error_mark_node;

  /* Allocate the array after the pointer type,
     in case we free it in type_hash_canon.  */

  if (pass > 0 && TREE_CODE (length) == INTEGER_CST
      && ! tree_int_cst_equal (length, integer_zero_node)
      && compare_int_csts (LT_EXPR, TYPE_MAX_VALUE (chill_unsigned_type_node),
			   length))
    {
      error ("string length > UPPER (UINT)");
      length = integer_one_node;
    }

  /* Subtract 1 from length to get max index value.
     Note we cannot use size_binop for pass 1 expressions. */
  if (TREE_CODE (length) == INTEGER_CST || pass != 1)
    length = size_binop (MINUS_EXPR, length, integer_one_node);
  else
    length = build (MINUS_EXPR, sizetype, length, integer_one_node);

  t = make_node (elt_type == boolean_type_node ? SET_TYPE : ARRAY_TYPE);
  TREE_TYPE (t) = elt_type;

  MARK_AS_STRING_TYPE (t);

  TYPE_DOMAIN (t) = build_chill_range_type (string_index_type_dummy,
					    integer_zero_node, length);
  if (pass == 1 && TREE_CODE (length) == INTEGER_CST)
    TYPE_DOMAIN (t) = layout_chill_range_type (TYPE_DOMAIN (t), 0);

  if (pass != 1
      || (TREE_CODE (length) == INTEGER_CST && TYPE_SIZE (elt_type)))
    {
      if (TREE_CODE (t) == SET_TYPE)
	t = layout_powerset_type (t);
      else
	t = layout_chill_array_type (t);
    }
  return t;
}

static tree
make_powerset_type (domain)
     tree domain;
{
  tree t = make_node (SET_TYPE);

  TREE_TYPE (t) = boolean_type_node;
  TYPE_DOMAIN (t) = domain;
  
  return t;
}

/* Used to layout both bitstring and powerset types. */

tree
layout_powerset_type (type)
     tree type;
{
  tree domain = TYPE_DOMAIN (type);

  if (! discrete_type_p (domain))
    {
      error ("Can only build a powerset from a discrete mode");
      return error_mark_node;
    }

  if (TREE_CODE (TYPE_MAX_VALUE (domain)) == ERROR_MARK ||
      TREE_CODE (TYPE_MIN_VALUE (domain)) == ERROR_MARK)
    return error_mark_node;

  if (TREE_CODE (TYPE_MAX_VALUE (domain)) != INTEGER_CST
      || TREE_CODE (TYPE_MIN_VALUE (domain)) != INTEGER_CST)
    {
      if (CH_BOOLS_TYPE_P (type))
	error ("non-constant bitstring size invalid");
      else
	error ("non-constant powerset size invalid");
      return error_mark_node;
    }

  if (TYPE_SIZE (type) == 0)
    layout_type (type);
  return type;
}

/* Build a SET_TYPE node whose elements are from the set of values
   in TYPE.  TYPE must be a discrete mode; we check for that here. */
tree
build_powerset_type (type)
     tree type;
{
  tree t = make_powerset_type (type);
  if (pass != 1)
    t = layout_powerset_type (t);
  return t;
}

tree
build_bitstring_type (size_in_bits)
     tree size_in_bits;
{
  return build_string_type (boolean_type_node, size_in_bits);
}

/* Return get_identifier (the concatenations of part1, part2, and part3). */

tree
get_identifier3 (part1, part2, part3)
     const char *part1, *part2, *part3;
{
  char *buf = (char*)
    alloca (strlen(part1) + strlen(part2) + strlen(part3) + 1);
  sprintf (buf, "%s%s%s", part1, part2, part3);
  return get_identifier (buf);
}

/* Build an ALIAS_DECL for the prefix renamed clause:
   (OLD_PREFIX -> NEW_PREFIX) ! POSTFIX. */

tree
build_alias_decl (old_prefix, new_prefix, postfix)
     tree old_prefix, new_prefix, postfix;
{
  tree decl = make_node (ALIAS_DECL);

  const char *postfix_pointer = IDENTIFIER_POINTER (postfix);
  int postfix_length = IDENTIFIER_LENGTH (postfix);
  int old_length = old_prefix ? IDENTIFIER_LENGTH(old_prefix) : 0;
  int new_length = new_prefix ? IDENTIFIER_LENGTH(new_prefix) : 0;

  char *buf = (char*) alloca (old_length + new_length + postfix_length + 3);

  /* Convert (OP->NP)!P!ALL to (OP!P->NP!P)!ALL */
  if (postfix_length > 1 && postfix_pointer[postfix_length-1] == '*')
    {
      int chopped_length = postfix_length - 2; /* Without final "!*" */
      if (old_prefix)
	sprintf (buf, "%s!%.*s", IDENTIFIER_POINTER (old_prefix),
		 chopped_length, postfix_pointer);
      else
	sprintf (buf, "%.*s", chopped_length, postfix_pointer);
      old_prefix = get_identifier (buf);
      if (new_prefix)
	sprintf (buf, "%s!%.*s", IDENTIFIER_POINTER (new_prefix),
		 chopped_length, postfix_pointer);
      else
	sprintf (buf, "%.*s", chopped_length, postfix_pointer);
      new_prefix = get_identifier (buf);
      postfix = ALL_POSTFIX;
    }

  DECL_OLD_PREFIX (decl) = old_prefix;
  DECL_NEW_PREFIX (decl) = new_prefix;
  DECL_POSTFIX (decl) = postfix;

  if (DECL_POSTFIX_ALL (decl))
    DECL_NAME (decl) = NULL_TREE;
  else if (new_prefix == NULL_TREE)
    DECL_NAME (decl) = postfix;
  else
    DECL_NAME (decl) = get_identifier3 (IDENTIFIER_POINTER (new_prefix),
					"!", IDENTIFIER_POINTER (postfix));

  return decl;
}

/* Return the "old name string" of an ALIAS_DECL. */

tree
decl_old_name (decl)
     tree decl;
{
  
  if (DECL_OLD_PREFIX (decl) == NULL_TREE)
    return DECL_POSTFIX (decl);
  return get_identifier3 (IDENTIFIER_POINTER (DECL_OLD_PREFIX (decl)),
			  "!", IDENTIFIER_POINTER (DECL_POSTFIX (decl)));
}

/* See if OLD_NAME (an identifier) matches the OLD_PREFIX!POSTFIX
   of ALIAS.  If so, return the corresponding NEW_NEW!POSTFIX. */

tree
decl_check_rename (alias, old_name)
     tree alias, old_name;
{
  const char *old_pointer = IDENTIFIER_POINTER (old_name);
  int old_len = IDENTIFIER_LENGTH (old_name);
  if (DECL_OLD_PREFIX (alias))
    {
      int old_prefix_len = IDENTIFIER_LENGTH (DECL_OLD_PREFIX (alias));
      if (old_prefix_len >= old_len
	  || old_pointer[old_prefix_len] != '!'
	  || strncmp (old_pointer, IDENTIFIER_POINTER (DECL_OLD_PREFIX (alias)), old_prefix_len) != 0)
	return NULL_TREE;

      /* Skip the old prefix. */
      old_pointer += old_prefix_len + 1; /* Also skip the '!', */
    }
  if (DECL_POSTFIX_ALL (alias)
      || strcmp (IDENTIFIER_POINTER (DECL_POSTFIX (alias)), old_pointer) == 0)
    {
      if (DECL_NEW_PREFIX (alias))
	return get_identifier3 (IDENTIFIER_POINTER (DECL_NEW_PREFIX (alias)),
				"!", old_pointer);
      else if (old_pointer == IDENTIFIER_POINTER (old_name))
	return old_name;
      else
	return get_identifier (old_pointer);
    }
  else
    return NULL_TREE;
}

/* 'EXIT foo' is treated like 'GOTO EXIT!foo'.
    This function converts LABEL into a labal name for EXIT. */

tree
munge_exit_label (label)
     tree label;
{
  return get_identifier3 ("EXIT", "!", IDENTIFIER_POINTER (label));
}

/* Make SAVE_EXPRs as needed, but don't turn a location into a non-location. */

tree
save_if_needed (exp)
tree exp;
{
  return CH_REFERABLE (exp) ? stabilize_reference (exp) : save_expr (exp);
}

/* Return the number of elements in T, which must be a discrete type. */
tree
discrete_count (t)
     tree t;
{
  tree hi = convert (sizetype, TYPE_MAX_VALUE (t));
  if (TYPE_MIN_VALUE (t))
    hi = size_binop (MINUS_EXPR, hi, convert (sizetype, TYPE_MIN_VALUE (t)));
  return size_binop (PLUS_EXPR, hi, integer_one_node);
}
