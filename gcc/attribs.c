/* Functions dealing with attribute handling, used by most front ends.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "toplev.h"
#include "output.h"
#include "rtl.h"
#include "ggc.h"
#include "tm_p.h"
#include "cpplib.h"
#include "target.h"
#include "langhooks.h"
#include "hashtab.h"

static void init_attributes (void);

/* Table of the tables of attributes (common, language, format, machine)
   searched.  */
static const struct attribute_spec *attribute_tables[4];

/* Hashtable mapping names (represented as substrings) to attribute specs. */
static htab_t attribute_hash;

/* Substring representation.  */

struct substring
{
  const char *str;
  int length;
};

static bool attributes_initialized = false;

/* Default empty table of attributes.  */

static const struct attribute_spec empty_attribute_table[] =
{
  { NULL, 0, 0, false, false, false, NULL }
};

/* Return base name of the attribute.  Ie '__attr__' is turned into 'attr'.
   To avoid need for copying, we simply return length of the string.  */

static void
extract_attribute_substring (struct substring *str)
{
  if (str->length > 4 && str->str[0] == '_' && str->str[1] == '_'
      && str->str[str->length - 1] == '_' && str->str[str->length - 2] == '_')
    {
      str->length -= 4;
      str->str += 2;
    }
}

/* Simple hash function to avoid need to scan whole string.  */

static inline hashval_t
substring_hash (const char *str, int l)
{
  return str[0] + str[l - 1] * 256 + l * 65536;
}

/* Used for attribute_hash.  */

static hashval_t
hash_attr (const void *p)
{
  const struct attribute_spec *const spec = (const struct attribute_spec *) p;
  const int l = strlen (spec->name);

  return substring_hash (spec->name, l);
}

/* Used for attribute_hash.  */

static int
eq_attr (const void *p, const void *q)
{
  const struct attribute_spec *const spec = (const struct attribute_spec *) p;
  const struct substring *const str = (const struct substring *) q;

  return (!strncmp (spec->name, str->str, str->length) && !spec->name[str->length]);
}

/* Initialize attribute tables, and make some sanity checks
   if --enable-checking.  */

static void
init_attributes (void)
{
  size_t i;
  int k;

  attribute_tables[0] = lang_hooks.common_attribute_table;
  attribute_tables[1] = lang_hooks.attribute_table;
  attribute_tables[2] = lang_hooks.format_attribute_table;
  attribute_tables[3] = targetm.attribute_table;

  /* Translate NULL pointers to pointers to the empty table.  */
  for (i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    if (attribute_tables[i] == NULL)
      attribute_tables[i] = empty_attribute_table;

#ifdef ENABLE_CHECKING
  /* Make some sanity checks on the attribute tables.  */
  for (i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    {
      int j;

      for (j = 0; attribute_tables[i][j].name != NULL; j++)
	{
	  /* The name must not begin and end with __.  */
	  const char *name = attribute_tables[i][j].name;
	  int len = strlen (name);

	  gcc_assert (!(name[0] == '_' && name[1] == '_'
			&& name[len - 1] == '_' && name[len - 2] == '_'));

	  /* The minimum and maximum lengths must be consistent.  */
	  gcc_assert (attribute_tables[i][j].min_length >= 0);

	  gcc_assert (attribute_tables[i][j].max_length == -1
		      || (attribute_tables[i][j].max_length
			  >= attribute_tables[i][j].min_length));

	  /* An attribute cannot require both a DECL and a TYPE.  */
	  gcc_assert (!attribute_tables[i][j].decl_required
		      || !attribute_tables[i][j].type_required);

	  /* If an attribute requires a function type, in particular
	     it requires a type.  */
	  gcc_assert (!attribute_tables[i][j].function_type_required
		      || attribute_tables[i][j].type_required);
	}
    }

  /* Check that each name occurs just once in each table.  */
  for (i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    {
      int j, k;
      for (j = 0; attribute_tables[i][j].name != NULL; j++)
	for (k = j + 1; attribute_tables[i][k].name != NULL; k++)
	  gcc_assert (strcmp (attribute_tables[i][j].name,
			      attribute_tables[i][k].name));
    }
  /* Check that no name occurs in more than one table.  */
  for (i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    {
      size_t j, k, l;

      for (j = i + 1; j < ARRAY_SIZE (attribute_tables); j++)
	for (k = 0; attribute_tables[i][k].name != NULL; k++)
	  for (l = 0; attribute_tables[j][l].name != NULL; l++)
	    gcc_assert (strcmp (attribute_tables[i][k].name,
				attribute_tables[j][l].name));
    }
#endif

  attribute_hash = htab_create (200, hash_attr, eq_attr, NULL);
  for (i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    for (k = 0; attribute_tables[i][k].name != NULL; k++)
      {
	struct substring str;
	const void **slot;

	str.str = attribute_tables[i][k].name;
	str.length = strlen (attribute_tables[i][k].name);
	slot = (const void **)htab_find_slot_with_hash (attribute_hash, &str,
					 substring_hash (str.str, str.length),
					 INSERT);
	gcc_assert (!*slot);
	*slot = &attribute_tables[i][k];
      }
  attributes_initialized = true;
}

/* Return the spec for the attribute named NAME.  */

const struct attribute_spec *
lookup_attribute_spec (tree name)
{
  struct substring attr;

  attr.str = IDENTIFIER_POINTER (name);
  attr.length = IDENTIFIER_LENGTH (name);
  extract_attribute_substring (&attr);
  return htab_find_with_hash (attribute_hash, &attr,
			      substring_hash (attr.str, attr.length));
}

/* Process the attributes listed in ATTRIBUTES and install them in *NODE,
   which is either a DECL (including a TYPE_DECL) or a TYPE.  If a DECL,
   it should be modified in place; if a TYPE, a copy should be created
   unless ATTR_FLAG_TYPE_IN_PLACE is set in FLAGS.  FLAGS gives further
   information, in the form of a bitwise OR of flags in enum attribute_flags
   from tree.h.  Depending on these flags, some attributes may be
   returned to be applied at a later stage (for example, to apply
   a decl attribute to the declaration rather than to its type).  */

tree
decl_attributes (tree *node, tree attributes, int flags)
{
  tree a;
  tree returned_attrs = NULL_TREE;

  if (TREE_TYPE (*node) == error_mark_node)
    return NULL_TREE;

  if (!attributes_initialized)
    init_attributes ();

  targetm.insert_attributes (*node, &attributes);

  for (a = attributes; a; a = TREE_CHAIN (a))
    {
      tree name = TREE_PURPOSE (a);
      tree args = TREE_VALUE (a);
      tree *anode = node;
      const struct attribute_spec *spec = lookup_attribute_spec (name);
      bool no_add_attrs = 0;
      tree fn_ptr_tmp = NULL_TREE;

      if (spec == NULL)
	{
	  warning (OPT_Wattributes, "%qs attribute directive ignored",
		   IDENTIFIER_POINTER (name));
	  continue;
	}
      else if (list_length (args) < spec->min_length
	       || (spec->max_length >= 0
		   && list_length (args) > spec->max_length))
	{
	  error ("wrong number of arguments specified for %qs attribute",
		 IDENTIFIER_POINTER (name));
	  continue;
	}
      gcc_assert (is_attribute_p (spec->name, name));

      if (spec->decl_required && !DECL_P (*anode))
	{
	  if (flags & ((int) ATTR_FLAG_DECL_NEXT
		       | (int) ATTR_FLAG_FUNCTION_NEXT
		       | (int) ATTR_FLAG_ARRAY_NEXT))
	    {
	      /* Pass on this attribute to be tried again.  */
	      returned_attrs = tree_cons (name, args, returned_attrs);
	      continue;
	    }
	  else
	    {
	      warning (OPT_Wattributes, "%qs attribute does not apply to types",
		       IDENTIFIER_POINTER (name));
	      continue;
	    }
	}

      /* If we require a type, but were passed a decl, set up to make a
	 new type and update the one in the decl.  ATTR_FLAG_TYPE_IN_PLACE
	 would have applied if we'd been passed a type, but we cannot modify
	 the decl's type in place here.  */
      if (spec->type_required && DECL_P (*anode))
	{
	  anode = &TREE_TYPE (*anode);
	  flags &= ~(int) ATTR_FLAG_TYPE_IN_PLACE;
	}

      if (spec->function_type_required && TREE_CODE (*anode) != FUNCTION_TYPE
	  && TREE_CODE (*anode) != METHOD_TYPE)
	{
	  if (TREE_CODE (*anode) == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (*anode)) == FUNCTION_TYPE
		  || TREE_CODE (TREE_TYPE (*anode)) == METHOD_TYPE))
	    {
	      /* OK, this is a bit convoluted.  We can't just make a copy
		 of the pointer type and modify its TREE_TYPE, because if
		 we change the attributes of the target type the pointer
		 type needs to have a different TYPE_MAIN_VARIANT.  So we
		 pull out the target type now, frob it as appropriate, and
		 rebuild the pointer type later.

		 This would all be simpler if attributes were part of the
		 declarator, grumble grumble.  */
	      fn_ptr_tmp = TREE_TYPE (*anode);
	      anode = &fn_ptr_tmp;
	      flags &= ~(int) ATTR_FLAG_TYPE_IN_PLACE;
	    }
	  else if (flags & (int) ATTR_FLAG_FUNCTION_NEXT)
	    {
	      /* Pass on this attribute to be tried again.  */
	      returned_attrs = tree_cons (name, args, returned_attrs);
	      continue;
	    }

	  if (TREE_CODE (*anode) != FUNCTION_TYPE
	      && TREE_CODE (*anode) != METHOD_TYPE)
	    {
	      warning (OPT_Wattributes,
		       "%qs attribute only applies to function types",
		       IDENTIFIER_POINTER (name));
	      continue;
	    }
	}

      if (TYPE_P (*anode)
	  && (flags & (int) ATTR_FLAG_TYPE_IN_PLACE)
	  && TYPE_SIZE (*anode) != NULL_TREE)
	{
	  warning (OPT_Wattributes, "type attributes ignored after type is already defined");
	  continue;
	}

      if (spec->handler != NULL)
	returned_attrs = chainon ((*spec->handler) (anode, name, args,
						    flags, &no_add_attrs),
				  returned_attrs);

      /* Layout the decl in case anything changed.  */
      if (spec->type_required && DECL_P (*node)
	  && (TREE_CODE (*node) == VAR_DECL
	      || TREE_CODE (*node) == PARM_DECL
	      || TREE_CODE (*node) == RESULT_DECL))
	relayout_decl (*node);

      if (!no_add_attrs)
	{
	  tree old_attrs;
	  tree a;

	  if (DECL_P (*anode))
	    old_attrs = DECL_ATTRIBUTES (*anode);
	  else
	    old_attrs = TYPE_ATTRIBUTES (*anode);

	  for (a = lookup_attribute (spec->name, old_attrs);
	       a != NULL_TREE;
	       a = lookup_attribute (spec->name, TREE_CHAIN (a)))
	    {
	      if (simple_cst_equal (TREE_VALUE (a), args) == 1)
		break;
	    }

	  if (a == NULL_TREE)
	    {
	      /* This attribute isn't already in the list.  */
	      if (DECL_P (*anode))
		DECL_ATTRIBUTES (*anode) = tree_cons (name, args, old_attrs);
	      else if (flags & (int) ATTR_FLAG_TYPE_IN_PLACE)
		{
		  TYPE_ATTRIBUTES (*anode) = tree_cons (name, args, old_attrs);
		  /* If this is the main variant, also push the attributes
		     out to the other variants.  */
		  if (*anode == TYPE_MAIN_VARIANT (*anode))
		    {
		      tree variant;
		      for (variant = *anode; variant;
			   variant = TYPE_NEXT_VARIANT (variant))
			{
			  if (TYPE_ATTRIBUTES (variant) == old_attrs)
			    TYPE_ATTRIBUTES (variant)
			      = TYPE_ATTRIBUTES (*anode);
			  else if (!lookup_attribute
				   (spec->name, TYPE_ATTRIBUTES (variant)))
			    TYPE_ATTRIBUTES (variant) = tree_cons
			      (name, args, TYPE_ATTRIBUTES (variant));
			}
		    }
		}
	      else
		*anode = build_type_attribute_variant (*anode,
						       tree_cons (name, args,
								  old_attrs));
	    }
	}

      if (fn_ptr_tmp)
	{
	  /* Rebuild the function pointer type and put it in the
	     appropriate place.  */
	  fn_ptr_tmp = build_pointer_type (fn_ptr_tmp);
	  if (DECL_P (*node))
	    TREE_TYPE (*node) = fn_ptr_tmp;
	  else
	    {
	      gcc_assert (TREE_CODE (*node) == POINTER_TYPE);
	      *node = fn_ptr_tmp;
	    }
	}
    }

  return returned_attrs;
}
