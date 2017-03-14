/* Pass for parsing functions with multiple target attributes.

   Contributed by Evgeny Stupachenko <evstupac@gmail.com>

   Copyright (C) 2015-2017 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "stringpool.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "target.h"
#include "attribs.h"
#include "pretty-print.h"

/* If the call in NODE has multiple target attribute with multiple fields,
   replace it with dispatcher call and create dispatcher (once).  */

static void
create_dispatcher_calls (struct cgraph_node *node)
{
  cgraph_edge *e;
  cgraph_edge *e_next = NULL;

  /* We need to remember NEXT_CALLER as it could be modified in the loop.  */
  for (e = node->callers; e ;e = (e == NULL) ? e_next : e->next_caller)
    {
      tree resolver_decl;
      tree idecl;
      tree decl;
      gimple *call = e->call_stmt;
      struct cgraph_node *inode;

      /* Checking if call of function is call of versioned function.
	 Versioned function are not inlined, so there is no need to
	 check for inline.  */
      if (!call
	  || !(decl = gimple_call_fndecl (call))
	  || !DECL_FUNCTION_VERSIONED (decl))
	continue;

      if (!targetm.has_ifunc_p ())
	{
	  error_at (gimple_location (call),
		    "the call requires ifunc, which is not"
		    " supported by this target");
	  break;
	}
      else if (!targetm.get_function_versions_dispatcher)
	{
	  error_at (gimple_location (call),
		    "target does not support function version dispatcher");
	  break;
	}

      e_next = e->next_caller;
      idecl = targetm.get_function_versions_dispatcher (decl);
      if (!idecl)
	{
	  error_at (gimple_location (call),
		    "default target_clones attribute was not set");
	  break;
	}
      inode = cgraph_node::get (idecl);
      gcc_assert (inode);
      resolver_decl = targetm.generate_version_dispatcher_body (inode);

      /* Update aliases.  */
      inode->alias = true;
      inode->alias_target = resolver_decl;
      if (!inode->analyzed)
	inode->resolve_alias (cgraph_node::get (resolver_decl));

      e->redirect_callee (inode);
      e->redirect_call_stmt_to_callee ();
      /*  Since REDIRECT_CALLEE modifies NEXT_CALLER field we move to
	  previously set NEXT_CALLER.  */
      e = NULL;
    }
}

/* Return length of attribute names string,
   if arglist chain > 1, -1 otherwise.  */

static int
get_attr_len (tree arglist)
{
  tree arg;
  int str_len_sum = 0;
  int argnum = 0;

  for (arg = arglist; arg; arg = TREE_CHAIN (arg))
    {
      const char *str = TREE_STRING_POINTER (TREE_VALUE (arg));
      size_t len = strlen (str);
      str_len_sum += len + 1;
      for (const char *p = strchr (str, ','); p; p = strchr (p + 1, ','))
	argnum++;
      argnum++;
    }
  if (argnum <= 1)
    return -1;
  return str_len_sum;
}

/* Create string with attributes separated by comma.
   Return number of attributes.  */

static int
get_attr_str (tree arglist, char *attr_str)
{
  tree arg;
  size_t str_len_sum = 0;
  int argnum = 0;

  for (arg = arglist; arg; arg = TREE_CHAIN (arg))
    {
      const char *str = TREE_STRING_POINTER (TREE_VALUE (arg));
      size_t len = strlen (str);
      for (const char *p = strchr (str, ','); p; p = strchr (p + 1, ','))
	argnum++;
      memcpy (attr_str + str_len_sum, str, len);
      attr_str[str_len_sum + len] = TREE_CHAIN (arg) ? ',' : '\0';
      str_len_sum += len + 1;
      argnum++;
    }
  return argnum;
}

/* Return number of attributes separated by comma and put them into ARGS.
   If there is no DEFAULT attribute return -1.  */

static int
separate_attrs (char *attr_str, char **attrs)
{
  int i = 0;
  bool has_default = false;

  for (char *attr = strtok (attr_str, ",");
       attr != NULL; attr = strtok (NULL, ","))
    {
      if (strcmp (attr, "default") == 0)
	{
	  has_default = true;
	  continue;
	}
      attrs[i++] = attr;
    }
  if (!has_default)
    return -1;
  return i;
}

/*  Return true if symbol is valid in assembler name.  */

static bool
is_valid_asm_symbol (char c)
{
  if ('a' <= c && c <= 'z')
    return true;
  if ('A' <= c && c <= 'Z')
    return true;
  if ('0' <= c && c <= '9')
    return true;
  if (c == '_')
    return true;
  return false;
}

/*  Replace all not valid assembler symbols with '_'.  */

static void
create_new_asm_name (char *old_asm_name, char *new_asm_name)
{
  int i;
  int old_name_len = strlen (old_asm_name);

  /* Replace all not valid assembler symbols with '_'.  */
  for (i = 0; i < old_name_len; i++)
    if (!is_valid_asm_symbol (old_asm_name[i]))
      new_asm_name[i] = '_';
    else
      new_asm_name[i] = old_asm_name[i];
  new_asm_name[old_name_len] = '\0';
}

/*  Creates target clone of NODE.  */

static cgraph_node *
create_target_clone (cgraph_node *node, bool definition, char *name)
{
  cgraph_node *new_node;

  if (definition)
    {
      new_node = node->create_version_clone_with_body (vNULL, NULL,
    						       NULL, false,
						       NULL, NULL,
						       name);
      new_node->force_output = true;
    }
  else
    {
      tree new_decl = copy_node (node->decl);
      new_node = cgraph_node::get_create (new_decl);
      /* Generate a new name for the new version.  */
      symtab->change_decl_assembler_name (new_node->decl,
					  clone_function_name (node->decl,
							       name));
    }
  return new_node;
}

/* If the function in NODE has multiple target attributes
   create the appropriate clone for each valid target attribute.  */

static bool
expand_target_clones (struct cgraph_node *node, bool definition)
{
  int i;
  /* Parsing target attributes separated by comma.  */
  tree attr_target = lookup_attribute ("target_clones",
				       DECL_ATTRIBUTES (node->decl));
  /* No targets specified.  */
  if (!attr_target)
    return false;

  tree arglist = TREE_VALUE (attr_target);
  int attr_len = get_attr_len (arglist);

  /* No need to clone for 1 target attribute.  */
  if (attr_len == -1)
    {
      warning_at (DECL_SOURCE_LOCATION (node->decl),
		  0,
		  "single target_clones attribute is ignored");
      return false;
    }

  char *attr_str = XNEWVEC (char, attr_len);
  int attrnum = get_attr_str (arglist, attr_str);
  char **attrs = XNEWVEC (char *, attrnum);

  attrnum = separate_attrs (attr_str, attrs);
  if (attrnum == -1)
    {
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"default target was not set");
      XDELETEVEC (attrs);
      XDELETEVEC (attr_str);
      return false;
    }

  cgraph_function_version_info *decl1_v = NULL;
  cgraph_function_version_info *decl2_v = NULL;
  cgraph_function_version_info *before = NULL;
  cgraph_function_version_info *after = NULL;
  decl1_v = node->function_version ();
  if (decl1_v == NULL)
    decl1_v = node->insert_new_function_version ();
  before = decl1_v;
  DECL_FUNCTION_VERSIONED (node->decl) = 1;

  for (i = 0; i < attrnum; i++)
    {
      char *attr = attrs[i];
      char *suffix = XNEWVEC (char, strlen (attr) + 1);

      create_new_asm_name (attr, suffix);
      /* Create new target clone.  */
      cgraph_node *new_node = create_target_clone (node, definition, suffix);
      new_node->local.local = false;
      XDELETEVEC (suffix);

      /* Set new attribute for the clone.  */
      tree attributes = make_attribute ("target", attr,
					DECL_ATTRIBUTES (new_node->decl));
      DECL_ATTRIBUTES (new_node->decl) = attributes;
      location_t saved_loc = input_location;
      input_location = DECL_SOURCE_LOCATION (node->decl);
      if (!targetm.target_option.valid_attribute_p (new_node->decl, NULL,
						    TREE_VALUE (attributes),
						    0))
	return false;

      input_location = saved_loc;
      decl2_v = new_node->function_version ();
      if (decl2_v != NULL)
        continue;
      decl2_v = new_node->insert_new_function_version ();

      /* Chain decl2_v and decl1_v.  All semantically identical versions
	 will be chained together.  */
      after = decl2_v;
      while (before->next != NULL)
	before = before->next;
      while (after->prev != NULL)
	after = after->prev;

      before->next = after;
      after->prev = before;
      DECL_FUNCTION_VERSIONED (new_node->decl) = 1;
    }

  XDELETEVEC (attrs);
  XDELETEVEC (attr_str);

  /* Setting new attribute to initial function.  */
  tree attributes = make_attribute ("target", "default",
				    DECL_ATTRIBUTES (node->decl));
  DECL_ATTRIBUTES (node->decl) = attributes;
  node->local.local = false;
  location_t saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (node->decl);
  bool ret
    = targetm.target_option.valid_attribute_p (node->decl, NULL,
					       TREE_VALUE (attributes), 0);
  input_location = saved_loc;
  return ret;
}

static unsigned int
ipa_target_clone (void)
{
  struct cgraph_node *node;

  bool target_clone_pass = false;
  FOR_EACH_FUNCTION (node)
    target_clone_pass |= expand_target_clones (node, node->definition);

  if (target_clone_pass)
    FOR_EACH_FUNCTION (node)
      create_dispatcher_calls (node);

  return 0;
}

namespace {

const pass_data pass_data_target_clone =
{
  SIMPLE_IPA_PASS,		/* type */
  "targetclone",		/* name */
  OPTGROUP_NONE,		/* optinfo_flags */
  TV_NONE,			/* tv_id */
  ( PROP_ssa | PROP_cfg ),	/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_update_ssa		/* todo_flags_finish */
};

class pass_target_clone : public simple_ipa_opt_pass
{
public:
  pass_target_clone (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_target_clone, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *) { return ipa_target_clone (); }
};

bool
pass_target_clone::gate (function *)
{
  return true;
}

} // anon namespace

simple_ipa_opt_pass *
make_pass_target_clone (gcc::context *ctxt)
{
  return new pass_target_clone (ctxt);
}
