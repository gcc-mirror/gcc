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
mark_fragile_ref_by_asm (symtab_node* node)
{
  node->ref_by_asm = true;
  /* Local symbols must remain in the same partition with their callers.  */
  if (!TREE_PUBLIC (node->decl))
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
