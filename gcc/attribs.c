/* Functions dealing with attribute handling, used by most front ends.
   Copyright (C) 1992-2016 Free Software Foundation, Inc.

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
#include "target.h"
#include "tree.h"
#include "stringpool.h"
#include "diagnostic-core.h"
#include "attribs.h"
#include "stor-layout.h"
#include "langhooks.h"
#include "plugin.h"

/* Table of the tables of attributes (common, language, format, machine)
   searched.  */
static const struct attribute_spec *attribute_tables[4];

/* Substring representation.  */

struct substring
{
  const char *str;
  int length;
};

/* Simple hash function to avoid need to scan whole string.  */

static inline hashval_t
substring_hash (const char *str, int l)
{
  return str[0] + str[l - 1] * 256 + l * 65536;
}

/* Used for attribute_hash.  */

struct attribute_hasher : nofree_ptr_hash <attribute_spec>
{
  typedef substring *compare_type;
  static inline hashval_t hash (const attribute_spec *);
  static inline bool equal (const attribute_spec *, const substring *);
};

inline hashval_t
attribute_hasher::hash (const attribute_spec *spec)
{
  const int l = strlen (spec->name);
  return substring_hash (spec->name, l);
}

inline bool
attribute_hasher::equal (const attribute_spec *spec, const substring *str)
{
  return (strncmp (spec->name, str->str, str->length) == 0
	  && !spec->name[str->length]);
}

/* Scoped attribute name representation.  */

struct scoped_attributes
{
  const char *ns;
  vec<attribute_spec> attributes;
  hash_table<attribute_hasher> *attribute_hash;
};

/* The table of scope attributes.  */
static vec<scoped_attributes> attributes_table;

static scoped_attributes* find_attribute_namespace (const char*);
static void register_scoped_attribute (const struct attribute_spec *,
				       scoped_attributes *);

static bool attributes_initialized = false;

/* Default empty table of attributes.  */

static const struct attribute_spec empty_attribute_table[] =
{
  { NULL, 0, 0, false, false, false, NULL, false }
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

/* Insert an array of attributes ATTRIBUTES into a namespace.  This
   array must be NULL terminated.  NS is the name of attribute
   namespace.  The function returns the namespace into which the
   attributes have been registered.  */

scoped_attributes*
register_scoped_attributes (const struct attribute_spec * attributes,
			    const char* ns)
{
  scoped_attributes *result = NULL;

  /* See if we already have attributes in the namespace NS.  */
  result = find_attribute_namespace (ns);

  if (result == NULL)
    {
      /* We don't have any namespace NS yet.  Create one.  */
      scoped_attributes sa;

      if (attributes_table.is_empty ())
	attributes_table.create (64);

      memset (&sa, 0, sizeof (sa));
      sa.ns = ns;
      sa.attributes.create (64);
      result = attributes_table.safe_push (sa);
      result->attribute_hash = new hash_table<attribute_hasher> (200);
    }

  /* Really add the attributes to their namespace now.  */
  for (unsigned i = 0; attributes[i].name != NULL; ++i)
    {
      result->attributes.safe_push (attributes[i]);
      register_scoped_attribute (&attributes[i], result);
    }

  gcc_assert (result != NULL);

  return result;
}

/* Return the namespace which name is NS, NULL if none exist.  */

static scoped_attributes*
find_attribute_namespace (const char* ns)
{
  unsigned ix;
  scoped_attributes *iter;

  FOR_EACH_VEC_ELT (attributes_table, ix, iter)
    if (ns == iter->ns
	|| (iter->ns != NULL
	    && ns != NULL
	    && !strcmp (iter->ns, ns)))
      return iter;
  return NULL;
}

/* Make some sanity checks on the attribute tables.  */

static void
check_attribute_tables (void)
{
  for (size_t i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    for (size_t j = 0; attribute_tables[i][j].name != NULL; j++)
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

  /* Check that each name occurs just once in each table.  */
  for (size_t i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    for (size_t j = 0; attribute_tables[i][j].name != NULL; j++)
      for (size_t k = j + 1; attribute_tables[i][k].name != NULL; k++)
	gcc_assert (strcmp (attribute_tables[i][j].name,
			    attribute_tables[i][k].name));

  /* Check that no name occurs in more than one table.  Names that
     begin with '*' are exempt, and may be overridden.  */
  for (size_t i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    for (size_t j = i + 1; j < ARRAY_SIZE (attribute_tables); j++)
      for (size_t k = 0; attribute_tables[i][k].name != NULL; k++)
	for (size_t l = 0; attribute_tables[j][l].name != NULL; l++)
	  gcc_assert (attribute_tables[i][k].name[0] == '*'
		      || strcmp (attribute_tables[i][k].name,
				 attribute_tables[j][l].name));
}

/* Initialize attribute tables, and make some sanity checks if checking is
   enabled.  */

void
init_attributes (void)
{
  size_t i;

  if (attributes_initialized)
    return;

  attribute_tables[0] = lang_hooks.common_attribute_table;
  attribute_tables[1] = lang_hooks.attribute_table;
  attribute_tables[2] = lang_hooks.format_attribute_table;
  attribute_tables[3] = targetm.attribute_table;

  /* Translate NULL pointers to pointers to the empty table.  */
  for (i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    if (attribute_tables[i] == NULL)
      attribute_tables[i] = empty_attribute_table;

  if (flag_checking)
    check_attribute_tables ();

  for (i = 0; i < ARRAY_SIZE (attribute_tables); ++i)
    /* Put all the GNU attributes into the "gnu" namespace.  */
    register_scoped_attributes (attribute_tables[i], "gnu");

  invoke_plugin_callbacks (PLUGIN_ATTRIBUTES, NULL);
  attributes_initialized = true;
}

/* Insert a single ATTR into the attribute table.  */

void
register_attribute (const struct attribute_spec *attr)
{
  register_scoped_attribute (attr, find_attribute_namespace ("gnu"));
}

/* Insert a single attribute ATTR into a namespace of attributes.  */

static void
register_scoped_attribute (const struct attribute_spec *attr,
			   scoped_attributes *name_space)
{
  struct substring str;
  attribute_spec **slot;

  gcc_assert (attr != NULL && name_space != NULL);

  gcc_assert (name_space->attribute_hash);

  str.str = attr->name;
  str.length = strlen (str.str);

  /* Attribute names in the table must be in the form 'text' and not
     in the form '__text__'.  */
  gcc_assert (str.length > 0 && str.str[0] != '_');

  slot = name_space->attribute_hash
	 ->find_slot_with_hash (&str, substring_hash (str.str, str.length),
				INSERT);
  gcc_assert (!*slot || attr->name[0] == '*');
  *slot = CONST_CAST (struct attribute_spec *, attr);
}

/* Return the spec for the scoped attribute with namespace NS and
   name NAME.   */

static const struct attribute_spec *
lookup_scoped_attribute_spec (const_tree ns, const_tree name)
{
  struct substring attr;
  scoped_attributes *attrs;

  const char *ns_str = (ns != NULL_TREE) ? IDENTIFIER_POINTER (ns): NULL;

  attrs = find_attribute_namespace (ns_str);

  if (attrs == NULL)
    return NULL;

  attr.str = IDENTIFIER_POINTER (name);
  attr.length = IDENTIFIER_LENGTH (name);
  extract_attribute_substring (&attr);
  return attrs->attribute_hash->find_with_hash (&attr,
						substring_hash (attr.str,
							       	attr.length));
}

/* Return the spec for the attribute named NAME.  If NAME is a TREE_LIST,
   it also specifies the attribute namespace.  */

const struct attribute_spec *
lookup_attribute_spec (const_tree name)
{
  tree ns;
  if (TREE_CODE (name) == TREE_LIST)
    {
      ns = TREE_PURPOSE (name);
      name = TREE_VALUE (name);
    }
  else
    ns = get_identifier ("gnu");
  return lookup_scoped_attribute_spec (ns, name);
}


/* Return the namespace of the attribute ATTR.  This accessor works on
   GNU and C++11 (scoped) attributes.  On GNU attributes,
   it returns an identifier tree for the string "gnu".

   Please read the comments of cxx11_attribute_p to understand the
   format of attributes.  */

static tree
get_attribute_namespace (const_tree attr)
{
  if (cxx11_attribute_p (attr))
    return TREE_PURPOSE (TREE_PURPOSE (attr));
  return get_identifier ("gnu");
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

  if (TREE_TYPE (*node) == error_mark_node || attributes == error_mark_node)
    return NULL_TREE;

  if (!attributes_initialized)
    init_attributes ();

  /* If this is a function and the user used #pragma GCC optimize, add the
     options to the attribute((optimize(...))) list.  */
  if (TREE_CODE (*node) == FUNCTION_DECL && current_optimize_pragma)
    {
      tree cur_attr = lookup_attribute ("optimize", attributes);
      tree opts = copy_list (current_optimize_pragma);

      if (! cur_attr)
	attributes
	  = tree_cons (get_identifier ("optimize"), opts, attributes);
      else
	TREE_VALUE (cur_attr) = chainon (opts, TREE_VALUE (cur_attr));
    }

  if (TREE_CODE (*node) == FUNCTION_DECL
      && optimization_current_node != optimization_default_node
      && !DECL_FUNCTION_SPECIFIC_OPTIMIZATION (*node))
    DECL_FUNCTION_SPECIFIC_OPTIMIZATION (*node) = optimization_current_node;

  /* If this is a function and the user used #pragma GCC target, add the
     options to the attribute((target(...))) list.  */
  if (TREE_CODE (*node) == FUNCTION_DECL
      && current_target_pragma
      && targetm.target_option.valid_attribute_p (*node, NULL_TREE,
						  current_target_pragma, 0))
    {
      tree cur_attr = lookup_attribute ("target", attributes);
      tree opts = copy_list (current_target_pragma);

      if (! cur_attr)
	attributes = tree_cons (get_identifier ("target"), opts, attributes);
      else
	TREE_VALUE (cur_attr) = chainon (opts, TREE_VALUE (cur_attr));
    }

  /* A "naked" function attribute implies "noinline" and "noclone" for
     those targets that support it.  */
  if (TREE_CODE (*node) == FUNCTION_DECL
      && attributes
      && lookup_attribute_spec (get_identifier ("naked"))
      && lookup_attribute ("naked", attributes) != NULL)
    {
      if (lookup_attribute ("noinline", attributes) == NULL)
	attributes = tree_cons (get_identifier ("noinline"), NULL, attributes);

      if (lookup_attribute ("noclone", attributes) == NULL)
	attributes = tree_cons (get_identifier ("noclone"),  NULL, attributes);
    }

  targetm.insert_attributes (*node, &attributes);

  for (a = attributes; a; a = TREE_CHAIN (a))
    {
      tree ns = get_attribute_namespace (a);
      tree name = get_attribute_name (a);
      tree args = TREE_VALUE (a);
      tree *anode = node;
      const struct attribute_spec *spec =
	lookup_scoped_attribute_spec (ns, name);
      bool no_add_attrs = 0;
      int fn_ptr_quals = 0;
      tree fn_ptr_tmp = NULL_TREE;

      if (spec == NULL)
	{
	  if (!(flags & (int) ATTR_FLAG_BUILT_IN))
	    {
	      if (ns == NULL_TREE || !cxx11_attribute_p (a))
		warning (OPT_Wattributes, "%qE attribute directive ignored",
			 name);
	      else
		warning (OPT_Wattributes,
			 "%<%E::%E%> scoped attribute directive ignored",
			 ns, name);
	    }
	  continue;
	}
      else if (list_length (args) < spec->min_length
	       || (spec->max_length >= 0
		   && list_length (args) > spec->max_length))
	{
	  error ("wrong number of arguments specified for %qE attribute",
		 name);
	  continue;
	}
      gcc_assert (is_attribute_p (spec->name, name));

      if (TYPE_P (*node)
	  && cxx11_attribute_p (a)
	  && !(flags & ATTR_FLAG_TYPE_IN_PLACE))
	{
	  /* This is a c++11 attribute that appertains to a
	     type-specifier, outside of the definition of, a class
	     type.  Ignore it.  */
	  if (warning (OPT_Wattributes, "attribute ignored"))
	    inform (input_location,
		    "an attribute that appertains to a type-specifier "
		    "is ignored");
	  continue;
	}

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
	      warning (OPT_Wattributes, "%qE attribute does not apply to types",
		       name);
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
	      fn_ptr_quals = TYPE_QUALS (*anode);
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
		       "%qE attribute only applies to function types",
		       name);
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
	{
	  int cxx11_flag =
	    cxx11_attribute_p (a) ? ATTR_FLAG_CXX11 : 0;

	  returned_attrs = chainon ((*spec->handler) (anode, name, args,
						      flags|cxx11_flag,
						      &no_add_attrs),
				    returned_attrs);
	}

      /* Layout the decl in case anything changed.  */
      if (spec->type_required && DECL_P (*node)
	  && (VAR_P (*node)
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
	  if (fn_ptr_quals)
	    fn_ptr_tmp = build_qualified_type (fn_ptr_tmp, fn_ptr_quals);
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

/* Return TRUE iff ATTR has been parsed by the front-end as a C++-11
   attribute.

   When G++ parses a C++11 attribute, it is represented as
   a TREE_LIST which TREE_PURPOSE is itself a TREE_LIST.  TREE_PURPOSE
   (TREE_PURPOSE (ATTR)) is the namespace of the attribute, and the
   TREE_VALUE (TREE_PURPOSE (ATTR)) is its non-qualified name.  Please
   use get_attribute_namespace and get_attribute_name to retrieve the
   namespace and name of the attribute, as these accessors work with
   GNU attributes as well.  */

bool
cxx11_attribute_p (const_tree attr)
{
  if (attr == NULL_TREE
      || TREE_CODE (attr) != TREE_LIST)
    return false;

  return (TREE_CODE (TREE_PURPOSE (attr)) == TREE_LIST);
}

/* Return the name of the attribute ATTR.  This accessor works on GNU
   and C++11 (scoped) attributes.

   Please read the comments of cxx11_attribute_p to understand the
   format of attributes.  */

tree
get_attribute_name (const_tree attr)
{
  if (cxx11_attribute_p (attr))
    return TREE_VALUE (TREE_PURPOSE (attr));
  return TREE_PURPOSE (attr);
}

/* Subroutine of set_method_tm_attributes.  Apply TM attribute ATTR
   to the method FNDECL.  */

void
apply_tm_attr (tree fndecl, tree attr)
{
  decl_attributes (&TREE_TYPE (fndecl), tree_cons (attr, NULL, NULL), 0);
}

/* Makes a function attribute of the form NAME(ARG_NAME) and chains
   it to CHAIN.  */

tree
make_attribute (const char *name, const char *arg_name, tree chain)
{
  tree attr_name;
  tree attr_arg_name;
  tree attr_args;
  tree attr;

  attr_name = get_identifier (name);
  attr_arg_name = build_string (strlen (arg_name), arg_name);
  attr_args = tree_cons (NULL_TREE, attr_arg_name, NULL_TREE);
  attr = tree_cons (attr_name, attr_args, chain);
  return attr;
}
