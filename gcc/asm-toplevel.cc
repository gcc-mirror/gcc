/* Toplevel assembly.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Michal Jires <mjires@suse.cz>

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
#include "tree-pass.h"
#include "cgraph.h"

/* This symbol must be available and cannot be renamed.
   Marks the symbol and symbols that reference it.  */
static void
mark_fragile_ref_by_asm (symtab_node* node, bool maybe_local = false)
{
  node->ref_by_asm = true;
  /* Local symbols must remain in the same partition with their callers.  */
  if (!TREE_PUBLIC (node->decl) || maybe_local)
    {
      unsigned j;
      ipa_ref *ref;
      node->must_remain_in_tu_name = true;
      for (j = 0; node->iterate_referring (j, ref); j++)
	ref->referring->must_remain_in_tu_body = true;

      if (cgraph_node* cnode = dyn_cast <cgraph_node *> (node))
	for (cgraph_edge *e = cnode->callers; e ; e = e->next_caller)
	  e->caller->must_remain_in_tu_body = true;
    }
}

/* Helper struct for walk_through_constraints.  */
struct constraint_data {
  asm_node *node;
  unsigned asm_definition : 1;
};

/* Mark symbols in constraints.  */
static tree
walk_through_constraints (tree* t, int*, void* dat)
{
  constraint_data* data = (constraint_data*) dat;
  asm_node* anode = data->node;

  if (VAR_OR_FUNCTION_DECL_P (*t))
    {
      symtab_node* node;
      if (!flag_wpa && !flag_ltrans)
	{
	  node = symtab_node::get_create (*t);
	  node->ref_by_asm = true;

	  /* Disable implicit definition on static variables defined in asm.  */
	  if (data->asm_definition && is_a<varpool_node*> (node)
	      && !TREE_PUBLIC (node->decl))
	    DECL_EXTERNAL (node->decl) = true;

	  if (data->asm_definition && !TREE_PUBLIC (node->decl) && flag_lto)
	    node->must_remain_in_tu_name = true;
	}
      else
	{
	  node = symtab_node::get (*t);
	  gcc_assert (node);

	  /* Local symbols defined in asm cannot be renamed.
	     LGEN pass is too early to use node->callers.
	     So we do it in WPA.  */
	  if (data->asm_definition && flag_wpa)
	    mark_fragile_ref_by_asm (node);
	}
      anode->symbols_referenced.safe_push (node);
    }
  return NULL;
}

/* Analyze constraints of toplevel extended assembly.  */
void
analyze_toplevel_extended_asm ()
{
  asm_node *anode;
  for (anode = symtab->first_asm_symbol (); anode;
       anode = safe_as_a<asm_node*> (anode->next))
    {
      if (TREE_CODE (anode->asm_str) != ASM_EXPR)
	continue;
      struct constraint_data data {anode, false};

      for (tree l = ASM_INPUTS (anode->asm_str); l; l = TREE_CHAIN (l))
	{
	  tree constraint = TREE_VALUE (TREE_PURPOSE (l));
	  const char* c = TREE_STRING_POINTER (constraint);
	  data.asm_definition = c[0] == ':' && c[1] == 0;
	  walk_tree (&l, walk_through_constraints, (void*) &data, NULL);
	}
      data.asm_definition = false;
      for (tree l = ASM_OUTPUTS (anode->asm_str); l; l = TREE_CHAIN (l))
	walk_tree (&l, walk_through_constraints, (void*) &data, NULL);
    }
}


/* Checks all toplevel assembly contents and compares them with known symbols.
   Marks those symbols with relevant flags.
   Heuristics: Detects anything in assembly that looks like an identifer.

   This pass must be in WPA, otherwise we will not see all possibly referenced
   symbols - if a symbol is only declared, it will not be in the callgraph if
   it is only referenced from toplevel assembly.
   However in WPA there may be multiple symbols with the same identifier.
   The chosen solution is to handle local symbols in LGEN pass first.  */

void
ipa_asm_heuristics ()
{
  hash_map<nofree_string_hash, symtab_node *> map;
  asm_node *anode = symtab->first_asm_symbol ();
  if (!anode)
    return;

  symtab_node* snode;
  if (flag_wpa)
    {
      FOR_EACH_SYMBOL (snode)
	if (TREE_PUBLIC (snode->decl))
	    map.put (snode->asm_name (), snode);
    }
  else
    {
      FOR_EACH_SYMBOL (snode)
	map.put (snode->asm_name (), snode);
    }

  auto_vec<char> ident;

  for (; anode; anode = safe_as_a<asm_node*> (anode->next))
    {
      if (TREE_CODE (anode->asm_str) != STRING_CST)
	continue;

      const char *asm_str = TREE_STRING_POINTER (anode->asm_str);
      int asm_len = TREE_STRING_LENGTH (anode->asm_str);

      if (dump_file)
	fprintf (dump_file, "Searching for symbols in toplevel asm:\n%s\n---\n",
		 asm_str);

      for (int i = 0; i < asm_len + 1; ++i)
	{
	  char c = 0;
	  if (i < asm_len)
	    c = asm_str[i];

	  if ('0' <= c && c <= '9')
	    {
	      if (ident.length ())
		ident.safe_push (c);
	    }
	  else if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_')
	    ident.safe_push (c);
	  else if (ident.length ())
	    {
	      ident.safe_push ('\0');
	      symtab_node **n_ = map.get (ident.begin ());

	      if (n_)
		{
		  bool is_label = c == ':';

		  if (dump_file)
		    fprintf (dump_file, "Found symbol '%s' (is_label: %d)\n",
			     ident.begin (), is_label);

		  mark_fragile_ref_by_asm (*n_, is_label);
		}

	      ident.truncate (0);
	    }
	}
    }
}

namespace {

const pass_data pass_data_ipa_asm =
{
  IPA_PASS, /* type */
  "ipa-asm", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_LTO_ASM, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_asm : public ipa_opt_pass_d
{
public:
  pass_ipa_asm (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_asm, ctxt,
		      NULL, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */

  bool gate (function *) final override
    {
      return (flag_lto || flag_wpa) && flag_toplevel_asm_heuristics;
    }

  unsigned int execute (function *) final override
    {
      ipa_asm_heuristics ();
      return 0;
    }

};

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_asm_lgen (gcc::context *ctxt)
{
  return new pass_ipa_asm (ctxt);
}

ipa_opt_pass_d *
make_pass_ipa_asm_wpa (gcc::context *ctxt)
{
  return new pass_ipa_asm (ctxt);
}
