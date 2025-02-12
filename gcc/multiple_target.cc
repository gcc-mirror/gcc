/* Pass for parsing functions with multiple target attributes.

   Contributed by Evgeny Stupachenko <evstupac@gmail.com>

   Copyright (C) 2015-2025 Free Software Foundation, Inc.

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
	  cgraph_edge::redirect_call_stmt_to_callee (e);
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
		{
		  if (source->same_comdat_group)
		    source->remove_from_same_comdat_group ();
		  source->add_to_same_comdat_group (inode);
		}
	    }
	  else
	    gcc_unreachable ();
	}
    }

  if (node->definition)
    {
      /* FIXME: copy of cgraph_node::make_local that should be cleaned up
		in next stage1.  */
      node->make_decl_local ();
      node->set_section (NULL);
      node->set_comdat_group (NULL);
      node->externally_visible = false;
      node->forced_by_abi = false;

      DECL_ARTIFICIAL (node->decl) = 1;
      node->force_output = true;
    }
}

/*  Creates target clone of NODE.  */

static cgraph_node *
create_target_clone (cgraph_node *node, bool definition, char *name,
		     tree attributes)
{
  cgraph_node *new_node;

  if (definition)
    {
      new_node
	= node->create_version_clone_with_body (vNULL, NULL, NULL, NULL, NULL,
						name, attributes, false);
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
      tree fname = clone_function_name (node->decl, name);
      symtab->change_decl_assembler_name (new_node->decl, fname);
    }
  return new_node;
}

/* If the function in NODE has multiple target attributes
   create the appropriate clone for each valid target attribute.  */

static bool
expand_target_clones (struct cgraph_node *node, bool definition)
{
  /* Parsing target attributes separated by TARGET_CLONES_ATTR_SEPARATOR.  */
  tree attr_target = lookup_attribute ("target_clones",
				       DECL_ATTRIBUTES (node->decl));
  /* No targets specified.  */
  if (!attr_target)
    return false;

  int num_defaults = 0;
  auto_vec<string_slice> attr_list = get_clone_versions (node->decl,
							 &num_defaults);

  /* No need to clone for 1 target attribute.  */
  if (attr_list.length () == 1)
    {
      warning_at (DECL_SOURCE_LOCATION (node->decl),
		  0, "single %<target_clones%> attribute is ignored");
      return false;
    }

  if (node->definition
      && (node->alias || !tree_versionable_function_p (node->decl)))
    {
      auto_diagnostic_group d;
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"clones for %<target_clones%> attribute cannot be created");
      const char *reason = NULL;
      if (lookup_attribute ("noclone", DECL_ATTRIBUTES (node->decl)))
	reason = G_("function %q+F can never be copied "
		    "because it has %<noclone%> attribute");
      else if (node->alias)
	reason
	  = "%<target_clones%> cannot be combined with %<alias%> attribute";
      else
	reason = copy_forbidden (DECL_STRUCT_FUNCTION (node->decl));
      if (reason)
	inform (DECL_SOURCE_LOCATION (node->decl), reason, node->decl);
      return false;
    }

  /* Disallow multiple defaults.  */
  if (num_defaults > 1)
    {
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"multiple %<default%> targets were set");
      return false;
    }
  /* Disallow target clones with no defaults.  */
  if (num_defaults == 0)
    {
      error_at (DECL_SOURCE_LOCATION (node->decl),
		"%<default%> target was not set");
      return false;
    }

  /* Disallow any empty values in the clone attr.  */
  for (string_slice attr : attr_list)
    if (attr.empty () || !attr.is_valid ())
      {
	error_at (DECL_SOURCE_LOCATION (node->decl),
		  "an empty string cannot be in %<target_clones%> attribute");
	return false;
      }

  string_slice new_attr_name = TARGET_HAS_FMV_TARGET_ATTRIBUTE
			       ? "target"
			       : "target_version";

  cgraph_function_version_info *node_v = node->function_version ();

  if (!node_v)
    node_v = node->insert_new_function_version ();

  /* If this target_clones contains a default, then convert this node to the
     default.  If this node does not contain default (this is only possible
     in target_version semantics) then remove the node.  This is safe at this
     point as only target_clones declarations containing default version is
     resolvable so this decl will have no calls/references.  */

  tree attrs = remove_attribute ("target_clones",
				  DECL_ATTRIBUTES (node->decl));
  tree assembler_name = node_v->assembler_name;

  /* Change the current node into the default node.  */
  gcc_assert (num_defaults == 1);

  /* Setting new attribute to initial function.  */
  tree attributes = make_attribute (new_attr_name, "default", attrs);
  DECL_ATTRIBUTES (node->decl) = attributes;
  DECL_FUNCTION_VERSIONED (node->decl) = true;

  node->is_target_clone = true;
  node->local = false;

  /* Remangle base node after new target version string set.  */
  tree id = targetm.mangle_decl_assembler_name (node->decl, assembler_name);
  symtab->change_decl_assembler_name (node->decl, id);

  for (string_slice attr : attr_list)
    {
      /* Skip default nodes.  */
      if (attr == "default")
	continue;

      /* Create new target clone.  */
      tree attributes = make_attribute (new_attr_name, attr, attrs);

      cgraph_node *new_node
	= create_target_clone (node, definition, NULL, attributes);
      if (new_node == NULL)
	return false;
      new_node->local = false;

      DECL_FUNCTION_VERSIONED (new_node->decl) = true;
      if (!node_v)
	node_v = new_node->insert_new_function_version ();
      else
	cgraph_node::add_function_version (node_v, new_node->decl);

      /* Use the base node's assembler name for all created nodes.  */
      new_node->function_version ()->assembler_name = assembler_name;
      new_node->is_target_clone = true;

      /* Mangle all new nodes.  */
      tree id = targetm.mangle_decl_assembler_name
	(new_node->decl, new_node->function_version ()->assembler_name);
      symtab->change_decl_assembler_name (new_node->decl, id);
    }


  return true;
}

/* When NODE is a target clone, consider all callees and redirect
   to a clone with equal target attributes.  That prevents multiple
   multi-versioning dispatches and a call-chain can be optimized.

   This optimisation might pick the wrong version in some cases, since knowing
   that we meet the target requirements for a matching callee version does not
   tell us that we won't also meet the target requirements for a higher
   priority callee version at runtime.  Since this is longstanding behaviour
   for x86 and powerpc, we preserve it for those targets, but skip the optimisation
   for targets that use the "target_version" attribute for multi-versioning.  */

static void
redirect_to_specific_clone (cgraph_node *node)
{
  cgraph_function_version_info *fv = node->function_version ();
  if (fv == NULL)
    return;

  gcc_assert (TARGET_HAS_FMV_TARGET_ATTRIBUTE);
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
      if (attr_target2 == NULL_TREE
	  || !attribute_value_equal (attr_target, attr_target2))
	{
	  while (fv2->prev != NULL)
	    fv2 = fv2->prev;

	  /* Try to find a clone with equal target attribute.  */
	  for (; fv2 != NULL; fv2 = fv2->next)
	    {
	      cgraph_node *callee = fv2->this_node;
	      attr_target2 = lookup_attribute ("target",
					       DECL_ATTRIBUTES (callee->decl));
	      if (attr_target2 != NULL_TREE
		  && attribute_value_equal (attr_target, attr_target2))
		{
		  e->redirect_callee (callee);
		  cgraph_edge::redirect_call_stmt_to_callee (e);
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

  if (TARGET_HAS_FMV_TARGET_ATTRIBUTE)
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
  bool gate (function *) final override;
  unsigned int execute (function *) final override
  {
    return ipa_target_clone ();
  }
};

bool
pass_target_clone::gate (function *)
{
  /* If there were any errors avoid pass property verification errors.  */
  return !seen_error ();
}

} // anon namespace

simple_ipa_opt_pass *
make_pass_target_clone (gcc::context *ctxt)
{
  return new pass_target_clone (ctxt);
}
