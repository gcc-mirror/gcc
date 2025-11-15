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

/* Mark symbols in constraints.  */
static tree
walk_through_constraints (tree* t, int*, void* data)
{
  asm_node* anode = (asm_node*) data;
  if (VAR_OR_FUNCTION_DECL_P (*t))
    {
      symtab_node* node;
      if (!flag_wpa && !flag_ltrans)
	{
	  node = symtab_node::get_create (*t);
	  node->ref_by_asm = true;
	}
      else
	{
	  node = symtab_node::get (*t);
	  gcc_assert (node);
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

      for (tree l = ASM_INPUTS (anode->asm_str); l; l = TREE_CHAIN (l))
	walk_tree (&l, walk_through_constraints, (void*) anode, NULL);
      for (tree l = ASM_OUTPUTS (anode->asm_str); l; l = TREE_CHAIN (l))
	walk_tree (&l, walk_through_constraints, (void*) anode, NULL);
    }
}
