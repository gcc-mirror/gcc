/* Localize comdats.
   Copyright (C) 2014-2017 Free Software Foundation, Inc.

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

/* This is very simple pass that looks for static symbols that are used
   exlusively by symbol within one comdat group.  In this case it makes
   sense to bring the symbol itself into the group to avoid dead code
   that would arrise when the comdat group from current unit is replaced
   by a different copy.  Consider for example:

    static int q(void)
    {
      ....
    }
    inline int t(void)
    {
      return q();
    }

   if Q is used only by T, it makes sense to put Q into T's comdat group.

   The pass solve simple dataflow across the callgraph trying to prove what
   symbols are used exclusively from a given comdat group.

   The implementation maintains a queue linked by AUX pointer terminated by
   pointer value 1. Lattice values are NULL for TOP, actual comdat group, or
   ERROR_MARK_NODE for bottom.

   TODO: When symbol is used only by comdat symbols, but from different groups,
   it would make sense to produce a new comdat group for it with anonymous name.

   TODO2: We can't mix variables and functions within one group.  Currently
   we just give up on references of symbols of different types.  We also should
   handle this by anonymous comdat group section.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-pass.h"
#include "cgraph.h"

/* Main dataflow loop propagating comdat groups across
   the symbol table.  All references to SYMBOL are examined
   and NEWGROUP is updated accordingly. MAP holds current lattice
   values for individual symbols.  */

tree
propagate_comdat_group (struct symtab_node *symbol,
			tree newgroup, hash_map<symtab_node *, tree> &map)
{
  int i;
  struct ipa_ref *ref;

  /* Walk all references to SYMBOL, recursively dive into aliases.  */

  for (i = 0;
       symbol->iterate_referring (i, ref)
       && newgroup != error_mark_node; i++)
    {
      struct symtab_node *symbol2 = ref->referring;

      if (ref->use == IPA_REF_ALIAS)
	{
	  newgroup = propagate_comdat_group (symbol2, newgroup, map);
	  continue;
	}

      /* One COMDAT group can not hold both variables and functions at
	 a same time.  For now we just go to BOTTOM, in future we may
	 invent special comdat groups for this case.  */

      if (symbol->type != symbol2->type)
	{
	  newgroup = error_mark_node;
	  break;
	}

      /* If we see inline clone, its comdat group actually
	 corresponds to the comdat group of the function it is inlined
	 to.  */

      if (cgraph_node * cn = dyn_cast <cgraph_node *> (symbol2))
	{
	  if (cn->global.inlined_to)
	    symbol2 = cn->global.inlined_to;
	}

      /* The actual merge operation.  */

      tree *val2 = map.get (symbol2);

      if (val2 && *val2 != newgroup)
	{
	  if (!newgroup)
	    newgroup = *val2;
	  else
	    newgroup = error_mark_node;
	}
    }

  /* If we analyze function, walk also callers.  */

  cgraph_node *cnode = dyn_cast <cgraph_node *> (symbol);

  if (cnode)
    for (struct cgraph_edge * edge = cnode->callers;
	 edge && newgroup != error_mark_node; edge = edge->next_caller)
      {
	struct symtab_node *symbol2 = edge->caller;

	if (cgraph_node * cn = dyn_cast <cgraph_node *> (symbol2))
	  {
	    /* Thunks can not call across section boundary.  */
	    if (cn->thunk.thunk_p)
	      newgroup = propagate_comdat_group (symbol2, newgroup, map);
	    /* If we see inline clone, its comdat group actually
	       corresponds to the comdat group of the function it
	       is inlined to.  */
	    if (cn->global.inlined_to)
	      symbol2 = cn->global.inlined_to;
	  }

        /* The actual merge operation.  */

	tree *val2 = map.get (symbol2);

	if (val2 && *val2 != newgroup)
	  {
	    if (!newgroup)
	      newgroup = *val2;
	    else
	      newgroup = error_mark_node;
	  }
      }
  return newgroup;
}


/* Add all references of SYMBOL that are defined into queue started by FIRST
   and linked by AUX pointer (unless they are already enqueued).
   Walk recursively inlined functions.  */

void
enqueue_references (symtab_node **first,
		    symtab_node *symbol)
{
  int i;
  struct ipa_ref *ref = NULL;

  for (i = 0; symbol->iterate_reference (i, ref); i++)
    {
      symtab_node *node = ref->referred->ultimate_alias_target ();

      /* Always keep thunks in same sections as target function.  */
      if (is_a <cgraph_node *>(node))
	node = dyn_cast <cgraph_node *> (node)->function_symbol ();
      if (!node->aux && node->definition)
	{
	   node->aux = *first;
	   *first = node;
	}
    }

  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (symbol))
    {
      struct cgraph_edge *edge;

      for (edge = cnode->callees; edge; edge = edge->next_callee)
	if (!edge->inline_failed)
	  enqueue_references (first, edge->callee);
	else
	  {
	    symtab_node *node = edge->callee->ultimate_alias_target ();

	    /* Always keep thunks in same sections as target function.  */
	    if (is_a <cgraph_node *>(node))
	      node = dyn_cast <cgraph_node *> (node)->function_symbol ();
	    if (!node->aux && node->definition)
	      {
		 node->aux = *first;
		 *first = node;
	      }
	  }
    }
}

/* Set comdat group of SYMBOL to GROUP.
   Callback for for_node_and_aliases.  */

bool
set_comdat_group (symtab_node *symbol,
	          void *head_p)
{
  symtab_node *head = (symtab_node *)head_p;

  gcc_assert (!symbol->get_comdat_group ());
  symbol->set_comdat_group (head->get_comdat_group ());
  symbol->add_to_same_comdat_group (head);
  return false;
}

/* Set comdat group of SYMBOL to GROUP.
   Callback for for_node_thunks_and_aliases.  */

bool
set_comdat_group_1 (cgraph_node *symbol,
		    void *head_p)
{
  return set_comdat_group (symbol, head_p);
}

/* The actual pass with the main dataflow loop.  */

static unsigned int
ipa_comdats (void)
{
  hash_map<symtab_node *, tree> map (251);
  hash_map<tree, symtab_node *> comdat_head_map (251);
  symtab_node *symbol;
  bool comdat_group_seen = false;
  symtab_node *first = (symtab_node *) (void *) 1;
  tree group;

  /* Start the dataflow by assigning comdat group to symbols that are in comdat
     groups already.  All other externally visible symbols must stay, we use
     ERROR_MARK_NODE as bottom for the propagation.  */

  FOR_EACH_DEFINED_SYMBOL (symbol)
    if (!symbol->real_symbol_p ())
      ;
    else if ((group = symbol->get_comdat_group ()) != NULL)
      {
        map.put (symbol, group);
        comdat_head_map.put (group, symbol);
	comdat_group_seen = true;

	/* Mark the symbol so we won't waste time visiting it for dataflow.  */
	symbol->aux = (symtab_node *) (void *) 1;
      }
    /* See symbols that can not be privatized to comdats; that is externally
       visible symbols or otherwise used ones.  We also do not want to mangle
       user section names.  */
    else if (symbol->externally_visible
	     || symbol->force_output
	     || symbol->used_from_other_partition
	     || TREE_THIS_VOLATILE (symbol->decl)
	     || symbol->get_section ()
	     || (TREE_CODE (symbol->decl) == FUNCTION_DECL
		 && (DECL_STATIC_CONSTRUCTOR (symbol->decl)
		     || DECL_STATIC_DESTRUCTOR (symbol->decl))))
      {
	symtab_node *target = symbol->ultimate_alias_target ();

	/* Always keep thunks in same sections as target function.  */
	if (is_a <cgraph_node *>(target))
	  target = dyn_cast <cgraph_node *> (target)->function_symbol ();
	map.put (target, error_mark_node);

	/* Mark the symbol so we won't waste time visiting it for dataflow.  */
	symbol->aux = (symtab_node *) (void *) 1;
      }
    else
      {
	/* Enqueue symbol for dataflow.  */
        symbol->aux = first;
	first = symbol;
      }

  if (!comdat_group_seen)
    {
      FOR_EACH_DEFINED_SYMBOL (symbol)
        symbol->aux = NULL;
      return 0;
    }

  /* The actual dataflow.  */

  while (first != (void *) 1)
    {
      tree group = NULL;
      tree newgroup, *val;

      symbol = first;
      first = (symtab_node *)first->aux;

      /* Get current lattice value of SYMBOL.  */
      val = map.get (symbol);
      if (val)
	group = *val;

      /* If it is bottom, there is nothing to do; do not clear AUX
	 so we won't re-queue the symbol.  */
      if (group == error_mark_node)
	continue;

      newgroup = propagate_comdat_group (symbol, group, map);

      /* If nothing changed, proceed to next symbol.  */
      if (newgroup == group)
	{
	  symbol->aux = NULL;
	  continue;
	}

      /* Update lattice value and enqueue all references for re-visiting.  */
      gcc_assert (newgroup);
      if (val)
	*val = newgroup;
      else
	map.put (symbol, newgroup);
      enqueue_references (&first, symbol);

      /* We may need to revisit the symbol unless it is BOTTOM.  */
      if (newgroup != error_mark_node)
        symbol->aux = NULL;
    }

  /* Finally assign symbols to the sections.  */

  FOR_EACH_DEFINED_SYMBOL (symbol)
    {
      struct cgraph_node *fun;
      symbol->aux = NULL; 
      if (!symbol->get_comdat_group ()
	  && !symbol->alias
	  && (!(fun = dyn_cast <cgraph_node *> (symbol))
	      || !fun->thunk.thunk_p)
	  && symbol->real_symbol_p ())
	{
	  tree *val = map.get (symbol);

	  /* A NULL here means that SYMBOL is unreachable in the definition
	     of ipa-comdats. Either ipa-comdats is wrong about this or someone
	     forgot to cleanup and remove unreachable functions earlier.  */
	  gcc_assert (val);

	  tree group = *val;

	  if (group == error_mark_node)
	    continue;
	  if (dump_file)
	    {
	      fprintf (dump_file, "Localizing symbol\n");
	      symbol->dump (dump_file);
	      fprintf (dump_file, "To group: %s\n", IDENTIFIER_POINTER (group));
	    }
	  if (is_a <cgraph_node *> (symbol))
	   dyn_cast <cgraph_node *>(symbol)->call_for_symbol_thunks_and_aliases
		  (set_comdat_group_1,
		   *comdat_head_map.get (group),
		   true);
	  else
	   symbol->call_for_symbol_and_aliases
		  (set_comdat_group,
		   *comdat_head_map.get (group),
		   true);
	}
    }
  return 0;
}

namespace {

const pass_data pass_data_ipa_comdats =
{
  IPA_PASS, /* type */
  "comdats", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_COMDATS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_comdats : public ipa_opt_pass_d
{
public:
  pass_ipa_comdats (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_comdats, ctxt,
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
  virtual bool gate (function *);
  virtual unsigned int execute (function *) { return ipa_comdats (); }

}; // class pass_ipa_comdats

bool
pass_ipa_comdats::gate (function *)
{
  return HAVE_COMDAT_GROUP;
}

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_comdats (gcc::context *ctxt)
{
  return new pass_ipa_comdats (ctxt);
}
