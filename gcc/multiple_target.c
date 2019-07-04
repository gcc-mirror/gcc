/* Pass for parsing functions with multiple target attributes.

   Contributed by Evgeny Stupachenko <evstupac@gmail.com>

   Copyright (C) 2015-2019 Free Software Foundation, Inc.

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
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-inline.h"
#include "intl.h"

/* Walker callback that replaces all FUNCTION_DECL of a function that's
   going to be versioned.  */

static tree
replace_function_decl (tree *op, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  cgraph_function_version_info *info = (cgraph_function_version_info *)wi->info;

  if (TREE_CODE (*op) == FUNCTION_DECL
      && info->this_node->decl == *op)
    {
      *op = info->dispatcher_resolver;
      *walk_subtrees = 0;
    }

  return NULL;
}

/* If the call in NODE has multiple target attribute with multiple fields,
   replace it with dispatcher call and create dispatcher (once).  */

static void
create_dispatcher_calls (struct cgraph_node *node)
{
  ipa_ref *ref;

  if (!DECL_FUNCTION_VERSIONED (node->decl)
      || !is_function_default_version (node->decl))
    return;

  if (!targetm.has_ifunc_p ())
    {
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"the call requires %<ifunc%>, which is not"
		" supported by this target");
      return;
    }
  else if (!targetm.get_function_versions_dispatcher)
    {
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"target does not support function version dispatcher");
      return;
    }

  tree idecl = targetm.get_function_versions_dispatcher (node->decl);
  if (!idecl)
    {
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"default %<target_clones%> attribute was not set");
      return;
    }

  cgraph_node *inode = cgraph_node::get (idecl);
  gcc_assert (inode);
  tree resolver_decl = targetm.generate_version_dispatcher_body (inode);

  /* Update aliases.  */
  inode->alias = true;
  inode->alias_target = resolver_decl;
  if (!inode->analyzed)
    inode->resolve_alias (cgraph_node::get (resolver_decl));

  auto_vec<cgraph_edge *> edges_to_redirect;
  /* We need to capture the references by value rather than just pointers to them
     and remove them right away, as removing them later would invalidate what
     some other reference pointers point to.  */
  auto_vec<ipa_ref> references_to_redirect;

  while (node->iterate_referring (0, ref))
    {
      references_to_redirect.safe_push (*ref);
      ref->remove_reference ();
    }

  /* We need to remember NEXT_CALLER as it could be modified in the loop.  */
  for (cgraph_edge *e = node->callers; e ; e = e->next_caller)
    edges_to_redirect.safe_push (e);

  if (!edges_to_redirect.is_empty () || !references_to_redirect.is_empty ())
    {
      /* Redirect edges.  */
      unsigned i;
      cgraph_edge *e;
      FOR_EACH_VEC_ELT (edges_to_redirect, i, e)
	{
	  e->redirect_callee (inode);
	  e->redirect_call_stmt_to_callee ();
	}

      /* Redirect references.  */
      FOR_EACH_VEC_ELT (references_to_redirect, i, ref)
	{
	  if (ref->use == IPA_REF_ADDR)
	    {
	      struct walk_stmt_info wi;
	      memset (&wi, 0, sizeof (wi));
	      wi.info = (void *)node->function_version ();

	      if (dyn_cast<varpool_node *> (ref->referring))
		{
		  hash_set<tree> visited_nodes;
		  walk_tree (&DECL_INITIAL (ref->referring->decl),
			     replace_function_decl, &wi, &visited_nodes);
		}
	      else
		{
		  gimple_stmt_iterator it = gsi_for_stmt (ref->stmt);
		  if (ref->referring->decl != resolver_decl)
		    walk_gimple_stmt (&it, NULL, replace_function_decl, &wi);
		}

	      symtab_node *source = ref->referring;
	      source->create_reference (inode, IPA_REF_ADDR);
	    }
	  else if (ref->use == IPA_REF_ALIAS)
	    {
	      symtab_node *source = ref->referring;
	      source->create_reference (inode, IPA_REF_ALIAS);
	      if (inode->get_comdat_group ())
		source->add_to_same_comdat_group (inode);
	    }
	  else
	    gcc_unreachable ();
	}
    }

  symtab->change_decl_assembler_name (node->decl,
				      clone_function_name_numbered (
					  node->decl, "default"));

  /* FIXME: copy of cgraph_node::make_local that should be cleaned up
	    in next stage1.  */
  node->make_decl_local ();
  node->set_section (NULL);
  node->set_comdat_group (NULL);
  node->externally_visible = false;
  node->forced_by_abi = false;
  node->set_section (NULL);
  node->unique_name = ((node->resolution == LDPR_PREVAILING_DEF_IRONLY
			|| node->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
		       && !flag_incremental_link);
  node->resolution = LDPR_PREVAILING_DEF_IRONLY;

  DECL_ARTIFICIAL (node->decl) = 1;
  node->force_output = true;
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
   If there is no DEFAULT attribute return -1.
   If there is an empty string in attribute return -2.
   If there are multiple DEFAULT attributes return -3.
   */

static int
separate_attrs (char *attr_str, char **attrs, int attrnum)
{
  int i = 0;
  int default_count = 0;

  for (char *attr = strtok (attr_str, ",");
       attr != NULL; attr = strtok (NULL, ","))
    {
      if (strcmp (attr, "default") == 0)
	{
	  default_count++;
	  continue;
	}
      attrs[i++] = attr;
    }
  if (default_count == 0)
    return -1;
  else if (default_count > 1)
    return -3;
  else if (i + default_count < attrnum)
    return -2;

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
create_target_clone (cgraph_node *node, bool definition, char *name,
		     tree attributes)
{
  cgraph_node *new_node;

  if (definition)
    {
      new_node = node->create_version_clone_with_body (vNULL, NULL,
    						       NULL, false,
						       NULL, NULL,
						       name, attributes);
      if (new_node == NULL)
	return NULL;
      new_node->force_output = true;
    }
  else
    {
      tree new_decl = copy_node (node->decl);
      new_node = cgraph_node::get_create (new_decl);
      DECL_ATTRIBUTES (new_decl) = attributes;
      /* Generate a new name for the new version.  */
      symtab->change_decl_assembler_name (new_node->decl,
					  clone_function_name_numbered (
					      node->decl, name));
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
		  0, "single %<target_clones%> attribute is ignored");
      return false;
    }

  if (node->definition
      && !tree_versionable_function_p (node->decl))
    {
      auto_diagnostic_group d;
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"clones for %<target_clones%> attribute cannot be created");
      const char *reason = NULL;
      if (lookup_attribute ("noclone", DECL_ATTRIBUTES (node->decl)))
	reason = G_("function %q+F can never be copied "
		    "because it has %<noclone%> attribute");
      else
	reason = copy_forbidden (DECL_STRUCT_FUNCTION (node->decl));
      if (reason)
	inform (DECL_SOURCE_LOCATION (node->decl), reason, node->decl);
      return false;
    }

  char *attr_str = XNEWVEC (char, attr_len);
  int attrnum = get_attr_str (arglist, attr_str);
  char **attrs = XNEWVEC (char *, attrnum);

  attrnum = separate_attrs (attr_str, attrs, attrnum);
  switch (attrnum)
    {
    case -1:
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"%<default%> target was not set");
      break;
    case -2:
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"an empty string cannot be in %<target_clones%> attribute");
      break;
    case -3:
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"multiple %<default%> targets were set");
      break;
    default:
      break;
    }

  if (attrnum < 0)
    {
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
      tree attributes = make_attribute ("target", attr,
					DECL_ATTRIBUTES (node->decl));

      cgraph_node *new_node = create_target_clone (node, definition, suffix,
						   attributes);
      if (new_node == NULL)
	return false;
      new_node->local.local = false;
      XDELETEVEC (suffix);

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
  return true;
}

/* When NODE is a target clone, consider all callees and redirect
   to a clone with equal target attributes.  That prevents multiple
   multi-versioning dispatches and a call-chain can be optimized.  */

static void
redirect_to_specific_clone (cgraph_node *node)
{
  cgraph_function_version_info *fv = node->function_version ();
  if (fv == NULL)
    return;

  tree attr_target = lookup_attribute ("target", DECL_ATTRIBUTES (node->decl));
  if (attr_target == NULL_TREE)
    return;

  /* We need to remember NEXT_CALLER as it could be modified in the loop.  */
  for (cgraph_edge *e = node->callees; e ; e = e->next_callee)
    {
      cgraph_function_version_info *fv2 = e->callee->function_version ();
      if (!fv2)
	continue;

      tree attr_target2 = lookup_attribute ("target",
					    DECL_ATTRIBUTES (e->callee->decl));

      /* Function is not calling proper target clone.  */
      if (!attribute_list_equal (attr_target, attr_target2))
	{
	  while (fv2->prev != NULL)
	    fv2 = fv2->prev;

	  /* Try to find a clone with equal target attribute.  */
	  for (; fv2 != NULL; fv2 = fv2->next)
	    {
	      cgraph_node *callee = fv2->this_node;
	      attr_target2 = lookup_attribute ("target",
					       DECL_ATTRIBUTES (callee->decl));
	      if (attribute_list_equal (attr_target, attr_target2))
		{
		  e->redirect_callee (callee);
		  e->redirect_call_stmt_to_callee ();
		  break;
		}
	    }
	}
    }
}

static unsigned int
ipa_target_clone (void)
{
  struct cgraph_node *node;
  auto_vec<cgraph_node *> to_dispatch;

  FOR_EACH_FUNCTION (node)
    if (expand_target_clones (node, node->definition))
      to_dispatch.safe_push (node);

  for (unsigned i = 0; i < to_dispatch.length (); i++)
    create_dispatcher_calls (to_dispatch[i]);

  FOR_EACH_FUNCTION (node)
    redirect_to_specific_clone (node);

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
