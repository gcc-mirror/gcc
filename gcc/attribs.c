/* Functions dealing with attribute handling, used by most front ends.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#include "toplev.h"
#include "output.h"
#include "rtl.h"
#include "ggc.h"
#include "expr.h"
#include "tm_p.h"
#include "obstack.h"
#include "cpplib.h"
#include "target.h"

static void init_attributes		PARAMS ((void));

/* Table of the tables of attributes (common, format, language, machine)
   searched.  */
static const struct attribute_spec *attribute_tables[4];

static bool attributes_initialized = false;

static tree handle_packed_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_nocommon_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_common_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_noreturn_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_unused_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_const_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_transparent_union_attribute PARAMS ((tree *, tree, tree,
							int, bool *));
static tree handle_constructor_attribute PARAMS ((tree *, tree, tree, int,
						  bool *));
static tree handle_destructor_attribute PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_mode_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_section_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_aligned_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_weak_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_alias_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_no_instrument_function_attribute PARAMS ((tree *, tree,
							     tree, int,
							     bool *));
static tree handle_no_check_memory_usage_attribute PARAMS ((tree *, tree, tree,
							    int, bool *));
static tree handle_malloc_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_no_limit_stack_attribute PARAMS ((tree *, tree, tree, int,
						     bool *));
static tree handle_pure_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));

/* Table of machine-independent attributes common to all C-like languages.  */
static const struct attribute_spec c_common_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "packed",                 0, 0, false, false, false,
      			      handle_packed_attribute },
  { "nocommon",               0, 0, true,  false, false,
			      handle_nocommon_attribute },
  { "common",                 0, 0, true,  false, false,
			      handle_common_attribute },
  /* FIXME: logically, noreturn attributes should be listed as
     "false, true, true" and apply to function types.  But implementing this
     would require all the places in the compiler that use TREE_THIS_VOLATILE
     on a decl to identify non-returning functions to be located and fixed
     to check the function type instead.  */
  { "noreturn",               0, 0, true,  false, false,
			      handle_noreturn_attribute },
  { "volatile",               0, 0, true,  false, false,
			      handle_noreturn_attribute },
  { "unused",                 0, 0, false, false, false,
			      handle_unused_attribute },
  /* The same comments as for noreturn attributes apply to const ones.  */
  { "const",                  0, 0, true,  false, false,
			      handle_const_attribute },
  { "transparent_union",      0, 0, false, false, false,
			      handle_transparent_union_attribute },
  { "constructor",            0, 0, true,  false, false,
			      handle_constructor_attribute },
  { "destructor",             0, 0, true,  false, false,
			      handle_destructor_attribute },
  { "mode",                   1, 1, true,  false, false,
			      handle_mode_attribute },
  { "section",                1, 1, true,  false, false,
			      handle_section_attribute },
  { "aligned",                0, 1, false, false, false,
			      handle_aligned_attribute },
  { "weak",                   0, 0, true,  false, false,
			      handle_weak_attribute },
  { "alias",                  1, 1, true,  false, false,
			      handle_alias_attribute },
  { "no_instrument_function", 0, 0, true,  false, false,
			      handle_no_instrument_function_attribute },
  { "no_check_memory_usage",  0, 0, true,  false, false,
			      handle_no_check_memory_usage_attribute },
  { "malloc",                 0, 0, true,  false, false,
			      handle_malloc_attribute },
  { "no_stack_limit",         0, 0, true,  false, false,
			      handle_no_limit_stack_attribute },
  { "pure",                   0, 0, true,  false, false,
			      handle_pure_attribute },
  { NULL,                     0, 0, false, false, false, NULL }
};

/* Default empty table of attributes.  */
static const struct attribute_spec empty_attribute_table[] =
{
  { NULL, 0, 0, false, false, false, NULL }
};

/* Table of machine-independent attributes for checking formats, if used.  */
const struct attribute_spec *format_attribute_table = empty_attribute_table;

/* Table of machine-independent attributes for a particular language.  */
const struct attribute_spec *lang_attribute_table = empty_attribute_table;

/* Flag saying whether common language attributes are to be supported.  */
int lang_attribute_common = 1;

/* Initialize attribute tables, and make some sanity checks
   if --enable-checking.  */

static void
init_attributes ()
{
#ifdef ENABLE_CHECKING
  int i;
#endif

  attribute_tables[0]
    = lang_attribute_common ? c_common_attribute_table : empty_attribute_table;
  attribute_tables[1] = lang_attribute_table;
  attribute_tables[2] = format_attribute_table;
  attribute_tables[3] = targetm.attribute_table;

#ifdef ENABLE_CHECKING
  /* Make some sanity checks on the attribute tables.  */
  for (i = 0;
       i < (int) (sizeof (attribute_tables) / sizeof (attribute_tables[0]));
       i++)
    {
      int j;

      for (j = 0; attribute_tables[i][j].name != NULL; j++)
	{
	  /* The name must not begin and end with __.  */
	  const char *name = attribute_tables[i][j].name;
	  int len = strlen (name);
	  if (name[0] == '_' && name[1] == '_'
	      && name[len - 1] == '_' && name[len - 2] == '_')
	    abort ();
	  /* The minimum and maximum lengths must be consistent.  */
	  if (attribute_tables[i][j].min_length < 0)
	    abort ();
	  if (attribute_tables[i][j].max_length != -1
	      && (attribute_tables[i][j].max_length
		  < attribute_tables[i][j].min_length))
	    abort ();
	  /* An attribute cannot require both a DECL and a TYPE.  */
	  if (attribute_tables[i][j].decl_required
	      && attribute_tables[i][j].type_required)
	    abort ();
	  /* If an attribute requires a function type, in particular
	     it requires a type.  */
	  if (attribute_tables[i][j].function_type_required
	      && !attribute_tables[i][j].type_required)
	    abort ();
	}
    }

  /* Check that each name occurs just once in each table.  */
  for (i = 0;
       i < (int) (sizeof (attribute_tables) / sizeof (attribute_tables[0]));
       i++)
    {
      int j, k;
      for (j = 0; attribute_tables[i][j].name != NULL; j++)
	for (k = j + 1; attribute_tables[i][k].name != NULL; k++)
	  if (!strcmp (attribute_tables[i][j].name,
		       attribute_tables[i][k].name))
	    abort ();
    }
  /* Check that no name occurs in more than one table.  */
  for (i = 0;
       i < (int) (sizeof (attribute_tables) / sizeof (attribute_tables[0]));
       i++)
    {
      int j, k, l;

      for (j = i + 1;
	   j < ((int) (sizeof (attribute_tables)
		       / sizeof (attribute_tables[0])));
	   j++)
	for (k = 0; attribute_tables[i][k].name != NULL; k++)
	  for (l = 0; attribute_tables[j][l].name != NULL; l++)
	    if (!strcmp (attribute_tables[i][k].name,
			 attribute_tables[j][l].name))
	      abort ();
    }
#endif

  attributes_initialized = true;
}

/* Process the attributes listed in ATTRIBUTES and install them in *NODE,
   which is either a DECL (including a TYPE_DECL) or a TYPE.  If a DECL,
   it should be modified in place; if a TYPE, a copy should be created
   unless ATTR_FLAG_TYPE_IN_PLACE is set in FLAGS.  FLAGS gives further
   information, in the form of a bitwise OR of flags in enum attribute_flags
   from tree.h.  Depending on these flags, some attributes may be
   returned to be applied at a later stage (for example, to apply
   a decl attribute to the declaration rather than to its type).  If
   ATTR_FLAG_BUILT_IN is not set and *NODE is a DECL, then also consider
   whether there might be some default attributes to apply to this DECL;
   if so, decl_attributes will be called recusrively with those attributes
   and ATTR_FLAG_BUILT_IN set.  */

tree
decl_attributes (node, attributes, flags)
     tree *node, attributes;
     int flags;
{
  tree a;
  tree returned_attrs = NULL_TREE;

  if (!attributes_initialized)
    init_attributes ();

  (*targetm.insert_attributes) (*node, &attributes);

  if (DECL_P (*node) && TREE_CODE (*node) == FUNCTION_DECL
      && !(flags & (int) ATTR_FLAG_BUILT_IN))
    insert_default_attributes (*node);

  for (a = attributes; a; a = TREE_CHAIN (a))
    {
      tree name = TREE_PURPOSE (a);
      tree args = TREE_VALUE (a);
      tree *anode = node;
      const struct attribute_spec *spec = NULL;
      bool no_add_attrs = 0;
      int i;

      for (i = 0;
	   i < ((int) (sizeof (attribute_tables)
		       / sizeof (attribute_tables[0])));
	   i++)
	{
	  int j;

	  for (j = 0; attribute_tables[i][j].name != NULL; j++)
	    {
	      if (is_attribute_p (attribute_tables[i][j].name, name))
		{
		  spec = &attribute_tables[i][j];
		  break;
		}
	    }
	  if (spec != NULL)
	    break;
	}

      if (spec == NULL)
	{
	  warning ("`%s' attribute directive ignored",
		   IDENTIFIER_POINTER (name));
	  continue;
	}
      else if (list_length (args) < spec->min_length
	       || (spec->max_length >= 0
		   && list_length (args) > spec->max_length))
	{
	  error ("wrong number of arguments specified for `%s' attribute",
		 IDENTIFIER_POINTER (name));
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
	      warning ("`%s' attribute does not apply to types",
		       IDENTIFIER_POINTER (name));
	      continue;
	    }
	}

      if (spec->type_required && DECL_P (*anode))
	anode = &TREE_TYPE (*anode);

      if (spec->function_type_required && TREE_CODE (*anode) != FUNCTION_TYPE
	  && TREE_CODE (*anode) != METHOD_TYPE)
	{
	  if (TREE_CODE (*anode) == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (*anode)) == FUNCTION_TYPE
		  || TREE_CODE (TREE_TYPE (*anode)) == METHOD_TYPE))
	    {
	      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
		*anode = build_type_copy (*anode);
	      anode = &TREE_TYPE (*anode);
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
	      warning ("`%s' attribute only applies to function types",
		       IDENTIFIER_POINTER (name));
	      continue;
	    }
	}

      if (spec->handler != NULL)
	returned_attrs = chainon ((*spec->handler) (anode, name, args,
						    flags, &no_add_attrs),
				  returned_attrs);
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
		TYPE_ATTRIBUTES (*anode) = tree_cons (name, args, old_attrs);
	      else
		*anode = build_type_attribute_variant (*anode,
						       tree_cons (name, args,
								  old_attrs));
	    }
	}
    }

  return returned_attrs;
}

/* Handle a "packed" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_packed_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags;
     bool *no_add_attrs;
{
  tree *type = NULL;
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
	type = &TREE_TYPE (*node);
    }
  else
    type = node;

  if (type)
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*type = build_type_copy (*type);
      TYPE_PACKED (*type) = 1;
    }
  else if (TREE_CODE (*node) == FIELD_DECL)
    DECL_PACKED (*node) = 1;
  /* We can't set DECL_PACKED for a VAR_DECL, because the bit is
     used for DECL_REGISTER.  It wouldn't mean anything anyway.  */
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "nocommon" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nocommon_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == VAR_DECL)
    DECL_COMMON (*node) = 0;
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "common" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_common_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == VAR_DECL)
    DECL_COMMON (*node) = 1;
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noreturn" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noreturn_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree type = TREE_TYPE (*node);

  /* See FIXME comment in c_common_attribute_table.  */
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_THIS_VOLATILE (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = build_pointer_type
	(build_type_variant (TREE_TYPE (type),
			     TREE_READONLY (TREE_TYPE (type)), 1));
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "unused" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_unused_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags;
     bool *no_add_attrs;
{
  if (DECL_P (*node))
    {
      tree decl = *node;

      if (TREE_CODE (decl) == PARM_DECL
	  || TREE_CODE (decl) == VAR_DECL
	  || TREE_CODE (decl) == FUNCTION_DECL
	  || TREE_CODE (decl) == LABEL_DECL
	  || TREE_CODE (decl) == TYPE_DECL)
	TREE_USED (decl) = 1;
      else
	{
	  warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
    }
  else
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*node = build_type_copy (*node);
      TREE_USED (*node) = 1;
    }

  return NULL_TREE;
}

/* Handle a "const" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_const_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree type = TREE_TYPE (*node);

  /* See FIXME comment on noreturn in c_common_attribute_table.  */
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_READONLY (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = build_pointer_type
	(build_type_variant (TREE_TYPE (type), 1,
			     TREE_THIS_VOLATILE (TREE_TYPE (type))));
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "transparent_union" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_transparent_union_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags;
     bool *no_add_attrs;
{
  tree decl = NULL_TREE;
  tree *type = NULL;
  int is_type = 0;

  if (DECL_P (*node))
    {
      decl = *node;
      type = &TREE_TYPE (decl);
      is_type = TREE_CODE (*node) == TYPE_DECL;
    }
  else if (TYPE_P (*node))
    type = node, is_type = 1;

  if (is_type
      && TREE_CODE (*type) == UNION_TYPE
      && (decl == 0
	  || (TYPE_FIELDS (*type) != 0
	      && TYPE_MODE (*type) == DECL_MODE (TYPE_FIELDS (*type)))))
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*type = build_type_copy (*type);
      TYPE_TRANSPARENT_UNION (*type) = 1;
    }
  else if (decl != 0 && TREE_CODE (decl) == PARM_DECL
	   && TREE_CODE (*type) == UNION_TYPE
	   && TYPE_MODE (*type) == DECL_MODE (TYPE_FIELDS (*type)))
    DECL_TRANSPARENT_UNION (decl) = 1;
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "constructor" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_constructor_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TREE_CODE (type) == FUNCTION_TYPE
      && decl_function_context (decl) == 0)
    {
      DECL_STATIC_CONSTRUCTOR (decl) = 1;
      TREE_USED (decl) = 1;
    }
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "destructor" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_destructor_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TREE_CODE (type) == FUNCTION_TYPE
      && decl_function_context (decl) == 0)
    {
      DECL_STATIC_DESTRUCTOR (decl) = 1;
      TREE_USED (decl) = 1;
    }
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "mode" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_mode_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  *no_add_attrs = true;

  if (TREE_CODE (TREE_VALUE (args)) != IDENTIFIER_NODE)
    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
  else
    {
      int j;
      const char *p = IDENTIFIER_POINTER (TREE_VALUE (args));
      int len = strlen (p);
      enum machine_mode mode = VOIDmode;
      tree typefm;

      if (len > 4 && p[0] == '_' && p[1] == '_'
	  && p[len - 1] == '_' && p[len - 2] == '_')
	{
	  char *newp = (char *) alloca (len - 1);

	  strcpy (newp, &p[2]);
	  newp[len - 4] = '\0';
	  p = newp;
	}

      /* Give this decl a type with the specified mode.
	 First check for the special modes.  */
      if (! strcmp (p, "byte"))
	mode = byte_mode;
      else if (!strcmp (p, "word"))
	mode = word_mode;
      else if (! strcmp (p, "pointer"))
	mode = ptr_mode;
      else
	for (j = 0; j < NUM_MACHINE_MODES; j++)
	  if (!strcmp (p, GET_MODE_NAME (j)))
	    mode = (enum machine_mode) j;

      if (mode == VOIDmode)
	error ("unknown machine mode `%s'", p);
      else if (0 == (typefm = type_for_mode (mode,
					     TREE_UNSIGNED (type))))
	error ("no data type for mode `%s'", p);
      else
	{
	  TREE_TYPE (decl) = type = typefm;
	  DECL_SIZE (decl) = DECL_SIZE_UNIT (decl) = 0;
	  if (TREE_CODE (decl) != FIELD_DECL)
	    layout_decl (decl, 0);
	}
    }

  return NULL_TREE;
}

/* Handle a "section" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_section_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (targetm.have_named_sections)
    {
      if ((TREE_CODE (decl) == FUNCTION_DECL
	   || TREE_CODE (decl) == VAR_DECL)
	  && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
	{
	  if (TREE_CODE (decl) == VAR_DECL
	      && current_function_decl != NULL_TREE
	      && ! TREE_STATIC (decl))
	    {
	      error_with_decl (decl,
			       "section attribute cannot be specified for local variables");
	      *no_add_attrs = true;
	    }

	  /* The decl may have already been given a section attribute
	     from a previous declaration.  Ensure they match.  */
	  else if (DECL_SECTION_NAME (decl) != NULL_TREE
		   && strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
			      TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
	    {
	      error_with_decl (*node,
			       "section of `%s' conflicts with previous declaration");
	      *no_add_attrs = true;
	    }
	  else
	    DECL_SECTION_NAME (decl) = TREE_VALUE (args);
	}
      else
	{
	  error_with_decl (*node,
			   "section attribute not allowed for `%s'");
	  *no_add_attrs = true;
	}
    }
  else
    {
      error_with_decl (*node,
		       "section attributes are not supported for this target");
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "aligned" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_aligned_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args;
     int flags;
     bool *no_add_attrs;
{
  tree decl = NULL_TREE;
  tree *type = NULL;
  int is_type = 0;
  tree align_expr = (args ? TREE_VALUE (args)
		     : size_int (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
  int i;

  if (DECL_P (*node))
    {
      decl = *node;
      type = &TREE_TYPE (decl);
      is_type = TREE_CODE (*node) == TYPE_DECL;
    }
  else if (TYPE_P (*node))
    type = node, is_type = 1;

  /* Strip any NOPs of any kind.  */
  while (TREE_CODE (align_expr) == NOP_EXPR
	 || TREE_CODE (align_expr) == CONVERT_EXPR
	 || TREE_CODE (align_expr) == NON_LVALUE_EXPR)
    align_expr = TREE_OPERAND (align_expr, 0);

  if (TREE_CODE (align_expr) != INTEGER_CST)
    {
      error ("requested alignment is not a constant");
      *no_add_attrs = true;
    }
  else if ((i = tree_log2 (align_expr)) == -1)
    {
      error ("requested alignment is not a power of 2");
      *no_add_attrs = true;
    }
  else if (i > HOST_BITS_PER_INT - 2)
    {
      error ("requested alignment is too large");
      *no_add_attrs = true;
    }
  else if (is_type)
    {
      /* If we have a TYPE_DECL, then copy the type, so that we
	 don't accidentally modify a builtin type.  See pushdecl.  */
      if (decl && TREE_TYPE (decl) != error_mark_node
	  && DECL_ORIGINAL_TYPE (decl) == NULL_TREE)
	{
	  tree tt = TREE_TYPE (decl);
	  *type = build_type_copy (*type);
	  DECL_ORIGINAL_TYPE (decl) = tt;
	  TYPE_NAME (*type) = decl;
	  TREE_USED (*type) = TREE_USED (decl);
	  TREE_TYPE (decl) = *type;
	}
      else if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*type = build_type_copy (*type);

      TYPE_ALIGN (*type) = (1 << i) * BITS_PER_UNIT;
      TYPE_USER_ALIGN (*type) = 1;
    }
  else if (TREE_CODE (decl) != VAR_DECL
	   && TREE_CODE (decl) != FIELD_DECL)
    {
      error_with_decl (decl,
		       "alignment may not be specified for `%s'");
      *no_add_attrs = true;
    }
  else
    {
      DECL_ALIGN (decl) = (1 << i) * BITS_PER_UNIT;
      DECL_USER_ALIGN (decl) = 1;
    }

  return NULL_TREE;
}

/* Handle a "weak" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_weak_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs ATTRIBUTE_UNUSED;
{
  declare_weak (*node);

  return NULL_TREE;
}

/* Handle an "alias" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_alias_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if ((TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl))
      || (TREE_CODE (decl) != FUNCTION_DECL && ! DECL_EXTERNAL (decl)))
    {
      error_with_decl (decl,
		       "`%s' defined both normally and as an alias");
      *no_add_attrs = true;
    }
  else if (decl_function_context (decl) == 0)
    {
      tree id;

      id = TREE_VALUE (args);
      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("alias arg not a string");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      id = get_identifier (TREE_STRING_POINTER (id));
      /* This counts as a use of the object pointed to.  */
      TREE_USED (id) = 1;

      if (TREE_CODE (decl) == FUNCTION_DECL)
	DECL_INITIAL (decl) = error_mark_node;
      else
	DECL_EXTERNAL (decl) = 0;
      assemble_alias (decl, id);
    }
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no_instrument_function" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_instrument_function_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_with_decl (decl,
		       "`%s' attribute applies only to functions",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_with_decl (decl,
		       "can't set `%s' attribute after definition",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else
    DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;

  return NULL_TREE;
}

/* Handle a "no_check_memory_usage" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_check_memory_usage_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_with_decl (decl,
		       "`%s' attribute applies only to functions",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_with_decl (decl,
		       "can't set `%s' attribute after definition",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else
    DECL_NO_CHECK_MEMORY_USAGE (decl) = 1;

  return NULL_TREE;
}

/* Handle a "malloc" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_malloc_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_IS_MALLOC (*node) = 1;
  /* ??? TODO: Support types.  */
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no_limit_stack" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_limit_stack_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_with_decl (decl,
		       "`%s' attribute applies only to functions",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_with_decl (decl,
		       "can't set `%s' attribute after definition",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else
    DECL_NO_LIMIT_STACK (decl) = 1;

  return NULL_TREE;
}

/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_pure_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_IS_PURE (*node) = 1;
  /* ??? TODO: Support types.  */
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Split SPECS_ATTRS, a list of declspecs and prefix attributes, into two
   lists.  SPECS_ATTRS may also be just a typespec (eg: RECORD_TYPE).

   The head of the declspec list is stored in DECLSPECS.
   The head of the attribute list is stored in PREFIX_ATTRIBUTES.

   Note that attributes in SPECS_ATTRS are stored in the TREE_PURPOSE of
   the list elements.  We drop the containing TREE_LIST nodes and link the
   resulting attributes together the way decl_attributes expects them.  */

void
split_specs_attrs (specs_attrs, declspecs, prefix_attributes)
     tree specs_attrs;
     tree *declspecs, *prefix_attributes;
{
  tree t, s, a, next, specs, attrs;

  /* This can happen after an __extension__ in pedantic mode.  */
  if (specs_attrs != NULL_TREE 
      && TREE_CODE (specs_attrs) == INTEGER_CST)
    {
      *declspecs = NULL_TREE;
      *prefix_attributes = NULL_TREE;
      return;
    }

  /* This can happen in c++ (eg: decl: typespec initdecls ';').  */
  if (specs_attrs != NULL_TREE
      && TREE_CODE (specs_attrs) != TREE_LIST)
    {
      *declspecs = specs_attrs;
      *prefix_attributes = NULL_TREE;
      return;
    }

  /* Remember to keep the lists in the same order, element-wise.  */

  specs = s = NULL_TREE;
  attrs = a = NULL_TREE;
  for (t = specs_attrs; t; t = next)
    {
      next = TREE_CHAIN (t);
      /* Declspecs have a non-NULL TREE_VALUE.  */
      if (TREE_VALUE (t) != NULL_TREE)
	{
	  if (specs == NULL_TREE)
	    specs = s = t;
	  else
	    {
	      TREE_CHAIN (s) = t;
	      s = t;
	    }
	}
      /* The TREE_PURPOSE may also be empty in the case of
	 __attribute__(()).  */
      else if (TREE_PURPOSE (t) != NULL_TREE)
	{
	  if (attrs == NULL_TREE)
	    attrs = a = TREE_PURPOSE (t);
	  else
	    {
	      TREE_CHAIN (a) = TREE_PURPOSE (t);
	      a = TREE_PURPOSE (t);
	    }
	  /* More attrs can be linked here, move A to the end.  */
	  while (TREE_CHAIN (a) != NULL_TREE)
	    a = TREE_CHAIN (a);
	}
    }

  /* Terminate the lists.  */
  if (s != NULL_TREE)
    TREE_CHAIN (s) = NULL_TREE;
  if (a != NULL_TREE)
    TREE_CHAIN (a) = NULL_TREE;

  /* All done.  */
  *declspecs = specs;
  *prefix_attributes = attrs;
}

/* Strip attributes from SPECS_ATTRS, a list of declspecs and attributes.
   This function is used by the parser when a rule will accept attributes
   in a particular position, but we don't want to support that just yet.

   A warning is issued for every ignored attribute.  */

tree
strip_attrs (specs_attrs)
     tree specs_attrs;
{
  tree specs, attrs;

  split_specs_attrs (specs_attrs, &specs, &attrs);

  while (attrs)
    {
      warning ("`%s' attribute ignored",
	       IDENTIFIER_POINTER (TREE_PURPOSE (attrs)));
      attrs = TREE_CHAIN (attrs);
    }

  return specs;
}
