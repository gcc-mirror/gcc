/* Callgraph handling code.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "hashtab.h"
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"

/* Hash table used to convert declarations into nodes.  */
static htab_t cgraph_hash = 0;

/* The linked list of cgraph nodes.  */
struct cgraph_node *cgraph_nodes;

/* Number of nodes in existence.  */
int cgraph_n_nodes;

/* Set when whole unit has been analyzed so we can access global info.  */
bool cgraph_global_info_ready = false;

static struct cgraph_edge *create_edge PARAMS ((struct cgraph_node *,
						struct cgraph_node *));
static void remove_edge PARAMS ((struct cgraph_node *, struct cgraph_node *));
static hashval_t hash_node PARAMS ((const PTR));
static int eq_node PARAMS ((const PTR, const PTR));

/* Returns a hash code for P.  */

static hashval_t
hash_node (p)
     const PTR p;
{
  return (hashval_t)
    htab_hash_pointer (DECL_ASSEMBLER_NAME
		       (((struct cgraph_node *) p)->decl));
}

/* Returns non-zero if P1 and P2 are equal.  */

static int
eq_node (p1, p2)
     const PTR p1;
     const PTR p2;
{
  return ((DECL_ASSEMBLER_NAME (((struct cgraph_node *) p1)->decl)) ==
	  DECL_ASSEMBLER_NAME ((tree) p2));
}

/* Return cgraph node assigned to DECL.  Create new one when needed.  */
struct cgraph_node *
cgraph_node (decl)
     tree decl;
{
  struct cgraph_node *node;
  struct cgraph_node **slot;

  if (!cgraph_hash)
    cgraph_hash = htab_create (10, hash_node, eq_node, NULL);

  slot =
    (struct cgraph_node **) htab_find_slot_with_hash (cgraph_hash, decl,
						      htab_hash_pointer
						      (DECL_ASSEMBLER_NAME
						       (decl)), 1);
  if (*slot)
    return *slot;
  node = xcalloc (sizeof (*node), 1);
  node->decl = decl;
  node->next = cgraph_nodes;
  cgraph_nodes = node;
  cgraph_n_nodes++;
  *slot = node;
  if (DECL_CONTEXT (decl))
    {
      node->origin = cgraph_node (DECL_CONTEXT (decl));
      node->next_nested = node->origin->nested;
      node->origin->nested = node;
    }
  return node;
}

/* Create edge from CALLER to CALLEE in the cgraph.  */

static struct cgraph_edge *
create_edge (caller, callee)
     struct cgraph_node *caller, *callee;
{
  struct cgraph_edge *edge = xmalloc (sizeof (struct cgraph_edge));

  edge->caller = caller;
  edge->callee = callee;
  edge->next_caller = callee->callers;
  edge->next_callee = caller->callees;
  caller->callees = edge;
  callee->callers = edge;
  return edge;
}

/* Remove the edge from CALLER to CALLEE in the cgraph.  */

static void
remove_edge (caller, callee)
     struct cgraph_node *caller, *callee;
{
  struct cgraph_edge **edge, **edge2;

  for (edge = &callee->callers; *edge && (*edge)->caller != caller;
       edge = &((*edge)->next_caller))
    continue;
  if (!*edge)
    abort ();
  *edge = (*edge)->next_caller;
  for (edge2 = &caller->callees; *edge2 && (*edge2)->callee != callee;
       edge2 = &(*edge2)->next_callee)
    continue;
  if (!*edge2)
    abort ();
  *edge2 = (*edge2)->next_callee;
}

/* Record call from CALLER to CALLEE  */

struct cgraph_edge *
cgraph_record_call (caller, callee)
     tree caller, callee;
{
  return create_edge (cgraph_node (caller), cgraph_node (callee));
}

void
cgraph_remove_call (caller, callee)
     tree caller, callee;
{
  remove_edge (cgraph_node (caller), cgraph_node (callee));
}

/* Return true when CALLER_DECL calls CALLEE_DECL.  */

bool
cgraph_calls_p (caller_decl, callee_decl)
     tree caller_decl, callee_decl;
{
  struct cgraph_node *caller = cgraph_node (caller_decl);
  struct cgraph_node *callee = cgraph_node (callee_decl);
  struct cgraph_edge *edge;

  for (edge = callee->callers; edge && (edge)->caller != caller;
       edge = (edge->next_caller))
    continue;
  return edge != NULL;
}

/* Return local info for the compiled function.  */

struct cgraph_local_info *
cgraph_local_info (decl)
     tree decl;
{
  struct cgraph_node *node;
  if (TREE_CODE (decl) != FUNCTION_DECL)
    abort ();
  node = cgraph_node (decl);
  return &node->local;
}

/* Return local info for the compiled function.  */

struct cgraph_global_info *
cgraph_global_info (decl)
     tree decl;
{
  struct cgraph_node *node;
  if (TREE_CODE (decl) != FUNCTION_DECL || !cgraph_global_info_ready)
    abort ();
  node = cgraph_node (decl);
  return &node->global;
}


/* Dump the callgraph.  */

void
dump_cgraph (f)
     FILE *f;
{
  struct cgraph_node *node;

  fprintf (f, "\nCallgraph:\n\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct cgraph_edge *edge;
      fprintf (f, "%s", IDENTIFIER_POINTER (DECL_NAME (node->decl)));
      if (node->origin)
	fprintf (f, " nested in: %s",
		 IDENTIFIER_POINTER (DECL_NAME (node->origin->decl)));
      if (node->needed)
	fprintf (f, " needed");
      else if (node->reachable)
	fprintf (f, " reachable");
      if (DECL_SAVED_TREE (node->decl))
	fprintf (f, " tree");

      fprintf (f, "\n  called by :");
      for (edge = node->callers; edge; edge = edge->next_caller)
	fprintf (f, "%s ",
		 IDENTIFIER_POINTER (DECL_NAME (edge->caller->decl)));

      fprintf (f, "\n  calls: ");
      for (edge = node->callees; edge; edge = edge->next_callee)
	fprintf (f, "%s ",
		 IDENTIFIER_POINTER (DECL_NAME (edge->callee->decl)));
      fprintf (f, "\n");
    }
}
