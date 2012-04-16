/* Symbol table.
   Copyright (C) 2012 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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
#include "tree-inline.h"
#include "hashtab.h"
#include "cgraph.h"

/* Linked list of symbol table nodes.  */
symtab_node symtab_nodes;

/* The order index of the next symtab node to be created.  This is
   used so that we can sort the cgraph nodes in order by when we saw
   them, to support -fno-toplevel-reorder.  */
int symtab_order;

/* Add node into symbol table.  This function is not used directly, but via
   cgraph/varpool node creation routines.  */

void
symtab_register_node (symtab_node node)
{
  node->symbol.next = symtab_nodes;
  node->symbol.previous = NULL;
  if (symtab_nodes)
    symtab_nodes->symbol.previous = node;
  symtab_nodes = node;

  node->symbol.order = symtab_order++;

  ipa_empty_ref_list (&node->symbol.ref_list);
}

/* Remove node from symbol table.  This function is not used directly, but via
   cgraph/varpool node removal routines.  */

void
symtab_unregister_node (symtab_node node)
{
  ipa_remove_all_references (&node->symbol.ref_list);
  ipa_remove_all_refering (&node->symbol.ref_list);

  if (node->symbol.same_comdat_group)
    {
      symtab_node prev;
      for (prev = node->symbol.same_comdat_group;
	   prev->symbol.same_comdat_group != node;
	   prev = prev->symbol.same_comdat_group)
	;
      if (node->symbol.same_comdat_group == prev)
	prev->symbol.same_comdat_group = NULL;
      else
	prev->symbol.same_comdat_group = node->symbol.same_comdat_group;
      node->symbol.same_comdat_group = NULL;
    }

  if (node->symbol.previous)
    node->symbol.previous->symbol.next = node->symbol.next;
  else
    symtab_nodes = node->symbol.next;
  if (node->symbol.next)
    node->symbol.next->symbol.previous = node->symbol.previous;
  node->symbol.next = NULL;
  node->symbol.previous = NULL;
}

/* Remove symtab NODE from the symbol table.  */

void
symtab_remove_node (symtab_node node)
{
  if (symtab_function_p (node))
    cgraph_remove_node (cgraph (node));
  else if (symtab_variable_p (node))
    varpool_remove_node (varpool (node));
}
