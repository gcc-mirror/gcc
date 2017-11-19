/* SSA definition Chains.
   Copyright (C) 2017 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "ssa-def-chain.h"
#include "ssa-range-gen.h"


ssa_define_chain::ssa_define_chain ()
{
  def_chain.create (0);
  def_chain.safe_grow_cleared (num_ssa_names);
  terminal.create (0);
  terminal.safe_grow_cleared (num_ssa_names);
  anchors.create (0);
  anchors.safe_grow_cleared (last_basic_block_for_fn (cfun));
}

ssa_define_chain::~ssa_define_chain ()
{
  anchors.release ();
  terminal.release ();
  def_chain.release ();
}

bitmap
ssa_define_chain::operator[] (tree name)
{
  unsigned index = SSA_NAME_VERSION (name);

  /* Dynamically calculate defintion chain if it hasnt been done yet.  */
  if (!def_chain[index])
    generate_def_chain (name);
  return def_chain[index];
}

bitmap
ssa_define_chain::operator[] (unsigned index)
{
  gcc_assert (index < num_ssa_names);
  return operator[] (ssa_name (index));
}


tree 
ssa_define_chain::terminal_name (unsigned index)
{
  /* Ensure the terminal name has been set.  */
  operator[] (index);
  return terminal[index];
}

tree
ssa_define_chain::terminal_name (tree name)
{
  return terminal_name (SSA_NAME_VERSION (name));
}

bool
ssa_define_chain::in_chain_p (tree def, tree name)
{
  if (TREE_CODE (def) != SSA_NAME || TREE_CODE (name) != SSA_NAME)
    return false;
  unsigned v = SSA_NAME_VERSION (name);
  bitmap b = operator[](def);
  return (b && bitmap_bit_p (b, v));
}


/* Generate def info for OPERAND if needed, and update the def map for VERSION
   to incorporate it.  */
tree
ssa_define_chain::process_op (tree operand, unsigned version, basic_block bb)
{
  bitmap ret;
  tree term = operand;
  unsigned op_index = SSA_NAME_VERSION (operand);
  
  if (!SSA_NAME_IS_DEFAULT_DEF (operand))
    {
      /* Make sure we dont look outside the require BB if approriate.  */
      if (gimple_bb (SSA_NAME_DEF_STMT (operand)) != bb)
	return operand;

      /* Make sure defintion chain exists for operand. */
      term = generate_def_chain (operand);

      /* If it has a chain, add them to this one. */
      ret = def_chain [op_index];
      if (!bitmap_empty_p (ret))
	bitmap_ior_into (def_chain[version], ret);
    }

  /* Finally add operand itself to the chain.  */
  bitmap_set_bit (def_chain[version], op_index);
  return term;
}

void
ssa_define_chain::add_anchor (basic_block bb, tree name)
{
  unsigned bbindex = bb->index;
  if (anchors[bbindex] == NULL)
    {
      range_stmt stmt (last_stmt (bb));
      anchors[bbindex] = BITMAP_ALLOC (NULL);
      if (stmt.valid () && stmt.logical_transition_p ())
        {
	  if (stmt.ssa_operand1 ())
	    add_anchor (bb, stmt.ssa_operand1 ());
	  if (stmt.ssa_operand2 ())
	    add_anchor (bb, stmt.ssa_operand2 ());
	}
    }
  if (name != NULL_TREE)
    bitmap_set_bit (anchors[bbindex], SSA_NAME_VERSION (name));
}

bool
ssa_define_chain::is_anchor_p (tree name, basic_block bb)
{
  unsigned index = bb->index;
  if (!anchors[index])
    return false;
  
  return bitmap_bit_p (anchors[index], SSA_NAME_VERSION (name));
}
tree 
ssa_define_chain::anchor_of (tree name, basic_block bb)
{
  unsigned index = bb->index;
  bitmap_iterator bi;
  unsigned x;

  if (!anchors[index])
    return NULL_TREE;
  
  EXECUTE_IF_SET_IN_BITMAP (anchors[index], 0, x, bi)
    {
      tree t = ssa_name (x);
      if (in_chain_p (t, name))
        return t;
    }

  return NULL_TREE;
}

tree
ssa_define_chain::generate_def_chain (tree name)
{
  gimple *stmt;
  range_stmt rn;
  tree ssa1, ssa2, tmp = NULL_TREE;
  unsigned index = SSA_NAME_VERSION (name);
  tree ret = name;

  /* If bitmap has been allocated, version has already been processed.  */
  if (def_chain[index])
    return terminal[index];

  def_chain[index] = BITMAP_ALLOC (NULL);

  stmt = SSA_NAME_DEF_STMT (name);
  rn = stmt;

  /* If a valid range stmt and there are ssa names, process args.  */
  if (rn.valid ())
    {
      basic_block bb = gimple_bb (stmt);
      ssa1 = rn.ssa_operand1 ();
      ssa2 = rn.ssa_operand2 ();

      if (ssa1)
	ret = process_op (ssa1, index, bb);
      if (ssa2)
	tmp = process_op (ssa2, index, bb);
      /* If there are 2 different terminal results, this is the terminal. */
      if (ret && tmp && ret != tmp)
	ret = name;
      else
	if (!ret)
	  ret = tmp;
      if (rn.logical_transition_p ())
        {
	  if (ssa1)
	    add_anchor (bb, ssa1);
	  if (ssa2)
	    add_anchor (bb, ssa2);
	}
    }
  terminal[index] = ret;
  return ret;
}

void
ssa_define_chain::dump (FILE *f)
{
  unsigned x, y;
  bitmap_iterator bi;
  basic_block bb;
  for (x = 1; x< num_ssa_names; x++)
    {
      if (def_chain[x] && !bitmap_empty_p (def_chain[x]))
        {
	  print_generic_expr (f, ssa_name (x), TDF_SLIM);
	  fprintf (f, "  : (terminal term: ");
	  if (terminal_name (x))
	    print_generic_expr (f, terminal_name(x), TDF_SLIM);
	  else
	    fprintf (f, "none");
	  fprintf (f, ")  :");
	  EXECUTE_IF_SET_IN_BITMAP (def_chain[x], 0, y, bi)
	    {
	      print_generic_expr (f, ssa_name (y), TDF_SLIM);
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }
  fprintf (f, "----  anchors  ----\n");
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (anchors[bb->index])
        {
	  fprintf (f, "BB%d : ",bb->index);
	  EXECUTE_IF_SET_IN_BITMAP (anchors[bb->index], 0, y, bi)
	    {
	      print_generic_expr (f, ssa_name (y), TDF_SLIM);
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }
}
