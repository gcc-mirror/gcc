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

/* In target FMV attributes, if the call in NODE has multiple target attribute
   with multiple fields, replace it with calls to the dispatched symbol and
   create the dispatcher body (once).

   In target_version semantics, if it is a lone annotated default, then
   the dispatched symbol is changed to be an alias and no resolver is
   required.  Otherwise, redirect all calls and references to the dispatched
   symbol, but only create the resolver body if the default version is
   implemented.  */

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
  cgraph_function_version_info *inode_info = inode->function_version ();
  gcc_assert (inode_info);

  tree resolver_decl = NULL;

  /* For target_version semantics, if there is a lone default declaration
     it needs to be mangled, with an alias from the dispatched symbol to the
     default version.  */
  if (!TARGET_HAS_FMV_TARGET_ATTRIBUTE
      && TREE_STATIC (node->decl)
      && inode_info->next
      && !inode_info->next->next)
    {
      inode->alias = true;
      inode->alias_target = inode_info->next->this_node->decl;
      inode->externally_visible = true;
      if (!inode->analyzed)
	inode->resolve_alias
	  (cgraph_node::get (inode_info->next->this_node->decl));

      DECL_ATTRIBUTES (idecl)
	= make_attribute ("alias",
			  IDENTIFIER_POINTER
			    (DECL_ASSEMBLER_NAME
			       (inode_info->next->this_node->decl)),
			  DECL_ATTRIBUTES (node->decl));
      TREE_USED (idecl) = true;
      DECL_EXTERNAL (idecl) = false;
      TREE_STATIC (idecl) = true;
      return;
    }
  /* In target_version semantics, only create the resolver if the
     default node is implemented.  */
  else if (TARGET_HAS_FMV_TARGET_ATTRIBUTE || TREE_STATIC (node->decl))
    {
      resolver_decl = targetm.generate_version_dispatcher_body (inode);
      /* Update aliases.  */
      inode->alias = true;
      inode->alias_target = resolver_decl;
      if (!inode->analyzed)
	inode->resolve_alias (cgraph_node::get (resolver_decl));
    }

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

  /* If the target clones list is empty after filtering, remove this node.  */
  if (!TARGET_HAS_FMV_TARGET_ATTRIBUTE && attr_list.is_empty ())
    {
      node->remove ();
      return false;
    }

  /* No need to clone for 1 target attribute.  */
  if (attr_list.length () == 1 && TARGET_HAS_FMV_TARGET_ATTRIBUTE)
    {
      warning_at (DECL_SOURCE_LOCATION (node->decl),
		  0, "single %<target_clones%> attribute is ignored");
      return false;
    }

  /* For target_version semantics, a target clone with just a default version
     is the same as an unannotated decl, so can ignore.  */
  if (!TARGET_HAS_FMV_TARGET_ATTRIBUTE
      && attr_list.length () == 1
      && num_defaults == 1)
    return false;

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

  /* For target FMV semantics, where target and target_clone mixing
     is not supported, disallow target clones with no defaults.  */
  if (TARGET_HAS_FMV_TARGET_ATTRIBUTE && num_defaults == 0)
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
  if (num_defaults == 1)
    {
      /* Setting new attribute to initial function.  */
      tree attributes = make_attribute (new_attr_name, "default", attrs);
      DECL_ATTRIBUTES (node->decl) = attributes;
      DECL_FUNCTION_VERSIONED (node->decl) = true;

      node->is_target_clone = true;
      node->local = false;

      /* Remangle base node after new target version string set.  */
      tree id = targetm.mangle_decl_assembler_name (node->decl, assembler_name);
      symtab->change_decl_assembler_name (node->decl, id);
    }
  else
    {
      /* Target clones without a default are only allowed for target_version
	 semantics where we can have target_clones/target_version mixing.  */
      gcc_assert (!TARGET_HAS_FMV_TARGET_ATTRIBUTE);

      /* If there isn't a default version, can safely remove this version.
	 The node itself gets removed after the other versions are created.  */
      cgraph_function_version_info *temp = node_v;
      node_v = node_v->next ? node_v->next : node_v->prev;
      cgraph_node::delete_function_version (temp);
    }

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

  /* If there are no default versions in the target_clones, this node is not
     reused, so can delete this node.  */
  if (num_defaults == 0)
    node->remove ();

  return true;
}

/* When NODE is part of an FMV function set, consider all callees and check if
   any can provably always resolve a certain version and then call that version
   directly.  */

static void
redirect_to_specific_clone (cgraph_node *node)
{
  if (!targetm.compare_version_priority || !optimize)
    return;

  /* We need to remember NEXT_CALLER as it could be modified in the loop.  */
  for (cgraph_edge *e = node->callees; e ; e = e->next_callee)
    {
      /* Only if this is a call to a dispatched symbol.  */
      if (!e->callee->dispatcher_function)
	continue;

      cgraph_function_version_info *callee_v
	= e->callee->function_version ();
      cgraph_function_version_info *caller_v
	= e->caller->function_version ();

      gcc_assert (callee_v);

      /* Find the default nodes for both callee and caller (if present).  */
      cgraph_function_version_info *callee_default_v = callee_v->next;
      cgraph_function_version_info *caller_default_v = caller_v;
      if (caller_v)
	{
	  while (caller_default_v->prev)
	    caller_default_v = caller_default_v->prev;
	  if (!is_function_default_version (caller_default_v->this_node->decl))
	    caller_default_v = NULL;
	}

      /* If this is not the TU that contains the definition of the default
	 version we are not guaranteed to have visibility of all versions
	 so cannot reason about them.  */
      if (!callee_default_v
	  || !callee_default_v->this_node->binds_to_current_def_p ())
	continue;

      cgraph_function_version_info *highest_callable_fn = NULL;
      for (cgraph_function_version_info *ver = callee_v->next;
	   ver;
	   ver = ver->next)
	if (targetm.target_option.functions_b_resolvable_from_a
	      (node->decl, ver->this_node->decl, node->decl))
	  highest_callable_fn = ver;

      if (!highest_callable_fn)
	continue;

      bool inlinable = true;

      /* If there are higher priority versions of callee and caller has no
	 more version information, then not callable.  */
      if (highest_callable_fn->next)
	{
	  /* If this is not the TU where the callee default is defined then
	     cannot reason about the caller versions.  */
	  if (!caller_default_v
	      || !caller_default_v->this_node->binds_to_current_def_p ())
	    continue;

	  /* If every higher priority version would imply a higher priority
	     version of caller would have been selected, then this is
	     callable.  */
	  for (cgraph_function_version_info *callee_ver
	       = highest_callable_fn->next;
	       callee_ver; callee_ver = callee_ver->next)
	    {
	      bool is_possible = true;
	      for (cgraph_function_version_info *caller_ver = caller_v->next;
		   caller_ver; caller_ver = caller_ver->next)
		if (targetm.target_option.functions_b_resolvable_from_a
		      (callee_ver->this_node->decl, caller_ver->this_node->decl,
		       node->decl))
		  {
		    is_possible = false;
		    break;
		  }
	      if (is_possible)
		{
		  inlinable = false;
		  break;
		}
	    }
	}
      if (inlinable)
	{
	  e->redirect_callee (highest_callable_fn->this_node);
	  cgraph_edge::redirect_call_stmt_to_callee (e);
	}
    }
}

/* Checks if NODE is in the 'simple' target_clones case, which is where NODE
   is a declaration annotated with target_clones containing the default, and it
   is the sole function declaration in the FMV function set.  */

static bool
is_simple_target_clones_case (cgraph_node *node)
{
  /* target attribute semantics doesnt support the complex case,
     so this is always true.  */
  if (TARGET_HAS_FMV_TARGET_ATTRIBUTE)
    return true;

  int num_defaults = 0;
  auto versions = get_clone_versions (node->decl, &num_defaults);
  if (versions.is_empty () || num_defaults != 1)
    return false;

  cgraph_function_version_info *fv = node->function_version ();

  if (fv && (fv->next || fv->prev))
    return false;

  return true;
}

static unsigned int
ipa_target_clone (bool early)
{
  struct cgraph_node *node;
  auto_vec<cgraph_node *> to_dispatch;

  /* Don't need to do anything early for target attribute semantics.  */
  if (early && TARGET_HAS_FMV_TARGET_ATTRIBUTE)
    return 0;

  /* For target attribute semantics, this pass skips the early phase, and in
     the later stage is only responsible for expanding and dispatching
     target_clone declarations, as target annotated functions are dispatched
     in the front end.

     The expanding and dispatching can be done at the late stage as the
     target_clone functions aren't allowed to be part of a larger FMV set, so
     all versions will all have the same body, so early optimisations are safe
     to treat a call to a target_clones set as a call to one function.

     For target_version semantics, this pass is responsible for expanding
     target_clones and dispatching all FMV function sets, including ones only
     made up of target_version declarations.

     Cases where there is more than one declaration must be expanded and
     dispatched at the early stage, as the declarations may have different
     bodies, and so the early optimisation passes would not be valid.

     The late stage is only used for the expansion and dispatching of the simple
     case where the FMV set is defined by a single target_clone attribute.  */

  FOR_EACH_FUNCTION_REMOVABLE (node)
    {
      /* In the early stage, we need to expand any target clone that is not
	 the simple case.  Simple cases are dispatched in the later stage.  */

      if (early == !is_simple_target_clones_case (node))
	if (expand_target_clones (node, node->definition)
	    && TARGET_HAS_FMV_TARGET_ATTRIBUTE)
	  /* In non target_version semantics, dispatch all target clones.  */
	  to_dispatch.safe_push (node);
    }

  /* In target_version semantics dispatch all FMV function sets with a default
     implementation in the early stage.
     Also dispatch any default versions generated by expanding target_clones
     in the late stage.  */

  if (!TARGET_HAS_FMV_TARGET_ATTRIBUTE)
    FOR_EACH_FUNCTION (node)
      if (is_function_default_version (node->decl)
	  && DECL_FUNCTION_VERSIONED (node->decl)
	  /* Don't dispatch target clones, as they haven't been expanded so
	     are simple.  */
	  && !lookup_attribute ("target_clones", DECL_ATTRIBUTES (node->decl)))
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
    : simple_ipa_opt_pass (pass_data_target_clone, ctxt), early_p (false)
  {}
  bool early_p;

  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      early_p = param;
    }
  /* opt_pass methods: */
  bool gate (function *) final override;
  opt_pass * clone () final override { return new pass_target_clone (m_ctxt); }
  unsigned int execute (function *) final override
  {
    return ipa_target_clone (early_p);
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
