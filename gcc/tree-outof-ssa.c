/* Convert a program in SSA form into Normal form.
   Copyright (C) 2004-2021 Free Software Foundation, Inc.
   Contributed by Andrew Macleod <amacleod@redhat.com>

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
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "ssa.h"
#include "tree-ssa.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "tree-dfa.h"
#include "stor-layout.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "dumpfile.h"
#include "tree-ssa-live.h"
#include "tree-ssa-ter.h"
#include "tree-ssa-coalesce.h"
#include "tree-outof-ssa.h"
#include "dojump.h"

/* FIXME: A lot of code here deals with expanding to RTL.  All that code
   should be in cfgexpand.c.  */
#include "explow.h"
#include "expr.h"

/* Return TRUE if expression STMT is suitable for replacement.  */

bool
ssa_is_replaceable_p (gimple *stmt)
{
  use_operand_p use_p;
  tree def;
  gimple *use_stmt;

  /* Only consider modify stmts.  */
  if (!is_gimple_assign (stmt))
    return false;

  /* If the statement may throw an exception, it cannot be replaced.  */
  if (stmt_could_throw_p (cfun, stmt))
    return false;

  /* Punt if there is more than 1 def.  */
  def = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_DEF);
  if (!def)
    return false;

  /* Only consider definitions which have a single use.  */
  if (!single_imm_use (def, &use_p, &use_stmt))
    return false;

  /* Used in this block, but at the TOP of the block, not the end.  */
  if (gimple_code (use_stmt) == GIMPLE_PHI)
    return false;

  /* There must be no VDEFs.  */
  if (gimple_vdef (stmt))
    return false;

  /* Float expressions must go through memory if float-store is on.  */
  if (flag_float_store
      && FLOAT_TYPE_P (TREE_TYPE (def)))
    return false;

  /* An assignment with a register variable on the RHS is not
     replaceable.  */
  if (gimple_assign_rhs_code (stmt) == VAR_DECL
      && DECL_HARD_REGISTER (gimple_assign_rhs1 (stmt)))
    return false;

  /* No function calls can be replaced.  */
  if (is_gimple_call (stmt))
    return false;

  /* Leave any stmt with volatile operands alone as well.  */
  if (gimple_has_volatile_ops (stmt))
    return false;

  return true;
}


/* Used to hold all the components required to do SSA PHI elimination.
   The node and pred/succ list is a simple linear list of nodes and
   edges represented as pairs of nodes.

   The predecessor and successor list:  Nodes are entered in pairs, where
   [0] ->PRED, [1]->SUCC.  All the even indexes in the array represent
   predecessors, all the odd elements are successors.

   Rationale:
   When implemented as bitmaps, very large programs SSA->Normal times were
   being dominated by clearing the interference graph.

   Typically this list of edges is extremely small since it only includes
   PHI results and uses from a single edge which have not coalesced with
   each other.  This means that no virtual PHI nodes are included, and
   empirical evidence suggests that the number of edges rarely exceed
   3, and in a bootstrap of GCC, the maximum size encountered was 7.
   This also limits the number of possible nodes that are involved to
   rarely more than 6, and in the bootstrap of gcc, the maximum number
   of nodes encountered was 12.  */

class elim_graph
{
public:
  elim_graph (var_map map);

  /* Size of the elimination vectors.  */
  int size;

  /* List of nodes in the elimination graph.  */
  auto_vec<int> nodes;

  /*  The predecessor and successor edge list.  */
  auto_vec<int> edge_list;

  /* Source locus on each edge */
  auto_vec<location_t> edge_locus;

  /* Visited vector.  */
  auto_sbitmap visited;

  /* Stack for visited nodes.  */
  auto_vec<int> stack;

  /* The variable partition map.  */
  var_map map;

  /* Edge being eliminated by this graph.  */
  edge e;

  /* List of constant copies to emit.  These are pushed on in pairs.  */
  auto_vec<int> const_dests;
  auto_vec<tree> const_copies;

  /* Source locations for any constant copies.  */
  auto_vec<location_t> copy_locus;
};


/* For an edge E find out a good source location to associate with
   instructions inserted on edge E.  If E has an implicit goto set,
   use its location.  Otherwise search instructions in predecessors
   of E for a location, and use that one.  That makes sense because
   we insert on edges for PHI nodes, and effects of PHIs happen on
   the end of the predecessor conceptually.  An exception is made
   for EH edges because we don't want to drag the source location
   of unrelated statements at the beginning of handlers; they would
   be further reused for various EH constructs, which would damage
   the coverage information.  */

static void
set_location_for_edge (edge e)
{
  if (e->goto_locus)
    set_curr_insn_location (e->goto_locus);
  else if (e->flags & EDGE_EH)
    {
      basic_block bb = e->dest;
      gimple_stmt_iterator gsi;

      do
	{
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (is_gimple_debug (stmt))
		continue;
	      if (gimple_has_location (stmt) || gimple_block (stmt))
		{
		  set_curr_insn_location (gimple_location (stmt));
		  return;
		}
	    }
	  /* Nothing found in this basic block.  Make a half-assed attempt
	     to continue with another block.  */
	  if (single_succ_p (bb))
	    bb = single_succ (bb);
	  else
	    bb = e->dest;
	}
      while (bb != e->dest);
    }
  else
    {
      basic_block bb = e->src;
      gimple_stmt_iterator gsi;

      do
	{
	  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (is_gimple_debug (stmt))
		continue;
	      if (gimple_has_location (stmt) || gimple_block (stmt))
		{
		  set_curr_insn_location (gimple_location (stmt));
		  return;
		}
	    }
	  /* Nothing found in this basic block.  Make a half-assed attempt
	     to continue with another block.  */
	  if (single_pred_p (bb))
	    bb = single_pred (bb);
	  else
	    bb = e->src;
	}
      while (bb != e->src);
    }
}

/* Emit insns to copy SRC into DEST converting SRC if necessary.  As
   SRC/DEST might be BLKmode memory locations SIZEEXP is a tree from
   which we deduce the size to copy in that case.  */

static inline rtx_insn *
emit_partition_copy (rtx dest, rtx src, int unsignedsrcp, tree sizeexp)
{
  start_sequence ();

  if (GET_MODE (src) != VOIDmode && GET_MODE (src) != GET_MODE (dest))
    src = convert_to_mode (GET_MODE (dest), src, unsignedsrcp);
  if (GET_MODE (src) == BLKmode)
    {
      gcc_assert (GET_MODE (dest) == BLKmode);
      emit_block_move (dest, src, expr_size (sizeexp), BLOCK_OP_NORMAL);
    }
  else
    emit_move_insn (dest, src);
  do_pending_stack_adjust ();

  rtx_insn *seq = get_insns ();
  end_sequence ();

  return seq;
}

/* Insert a copy instruction from partition SRC to DEST onto edge E.  */

static void
insert_partition_copy_on_edge (edge e, int dest, int src, location_t locus)
{
  tree var;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Inserting a partition copy on edge BB%d->BB%d : "
	       "PART.%d = PART.%d",
	       e->src->index,
	       e->dest->index, dest, src);
      fprintf (dump_file, "\n");
    }

  gcc_assert (SA.partition_to_pseudo[dest]);
  gcc_assert (SA.partition_to_pseudo[src]);

  set_location_for_edge (e);
  /* If a locus is provided, override the default.  */
  if (locus)
    set_curr_insn_location (locus);

  var = partition_to_var (SA.map, src);
  rtx_insn *seq = emit_partition_copy (copy_rtx (SA.partition_to_pseudo[dest]),
				       copy_rtx (SA.partition_to_pseudo[src]),
				       TYPE_UNSIGNED (TREE_TYPE (var)),
				       var);

  insert_insn_on_edge (seq, e);
}

/* Insert a copy instruction from expression SRC to partition DEST
   onto edge E.  */

static void
insert_value_copy_on_edge (edge e, int dest, tree src, location_t locus)
{
  rtx dest_rtx, seq, x;
  machine_mode dest_mode, src_mode;
  int unsignedp;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Inserting a value copy on edge BB%d->BB%d : PART.%d = ",
	       e->src->index,
	       e->dest->index, dest);
      print_generic_expr (dump_file, src, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  dest_rtx = copy_rtx (SA.partition_to_pseudo[dest]);
  gcc_assert (dest_rtx);

  set_location_for_edge (e);
  /* If a locus is provided, override the default.  */
  if (locus)
    set_curr_insn_location (locus);

  start_sequence ();

  tree name = partition_to_var (SA.map, dest);
  src_mode = TYPE_MODE (TREE_TYPE (src));
  dest_mode = GET_MODE (dest_rtx);
  gcc_assert (src_mode == TYPE_MODE (TREE_TYPE (name)));
  gcc_assert (!REG_P (dest_rtx)
	      || dest_mode == promote_ssa_mode (name, &unsignedp));

  if (src_mode != dest_mode)
    {
      x = expand_expr (src, NULL, src_mode, EXPAND_NORMAL);
      x = convert_modes (dest_mode, src_mode, x, unsignedp);
    }
  else if (src_mode == BLKmode)
    {
      x = dest_rtx;
      store_expr (src, x, 0, false, false);
    }
  else
    x = expand_expr (src, dest_rtx, dest_mode, EXPAND_NORMAL);

  if (x != dest_rtx)
    emit_move_insn (dest_rtx, x);
  do_pending_stack_adjust ();

  seq = get_insns ();
  end_sequence ();

  insert_insn_on_edge (seq, e);
}

/* Insert a copy instruction from RTL expression SRC to partition DEST
   onto edge E.  */

static void
insert_rtx_to_part_on_edge (edge e, int dest, rtx src, int unsignedsrcp,
			    location_t locus)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Inserting a temp copy on edge BB%d->BB%d : PART.%d = ",
	       e->src->index,
	       e->dest->index, dest);
      print_simple_rtl (dump_file, src);
      fprintf (dump_file, "\n");
    }

  gcc_assert (SA.partition_to_pseudo[dest]);

  set_location_for_edge (e);
  /* If a locus is provided, override the default.  */
  if (locus)
    set_curr_insn_location (locus);

  /* We give the destination as sizeexp in case src/dest are BLKmode
     mems.  Usually we give the source.  As we result from SSA names
     the left and right size should be the same (and no WITH_SIZE_EXPR
     involved), so it doesn't matter.  */
  rtx_insn *seq = emit_partition_copy (copy_rtx (SA.partition_to_pseudo[dest]),
				       src, unsignedsrcp,
				       partition_to_var (SA.map, dest));

  insert_insn_on_edge (seq, e);
}

/* Insert a copy instruction from partition SRC to RTL lvalue DEST
   onto edge E.  */

static void
insert_part_to_rtx_on_edge (edge e, rtx dest, int src, location_t locus)
{
  tree var;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Inserting a temp copy on edge BB%d->BB%d : ",
	       e->src->index,
	       e->dest->index);
      print_simple_rtl (dump_file, dest);
      fprintf (dump_file, "= PART.%d\n", src);
    }

  gcc_assert (SA.partition_to_pseudo[src]);

  set_location_for_edge (e);
  /* If a locus is provided, override the default.  */
  if (locus)
    set_curr_insn_location (locus);

  var = partition_to_var (SA.map, src);
  rtx_insn *seq = emit_partition_copy (dest,
				       copy_rtx (SA.partition_to_pseudo[src]),
				       TYPE_UNSIGNED (TREE_TYPE (var)),
				       var);

  insert_insn_on_edge (seq, e);
}


/* Create an elimination graph for map.  */

elim_graph::elim_graph (var_map map) :
  nodes (30), edge_list (20), edge_locus (10), visited (map->num_partitions),
  stack (30), map (map), const_dests (20), const_copies (20), copy_locus (10)
{
}


/* Empty elimination graph G.  */

static inline void
clear_elim_graph (elim_graph *g)
{
  g->nodes.truncate (0);
  g->edge_list.truncate (0);
  g->edge_locus.truncate (0);
}


/* Return the number of nodes in graph G.  */

static inline int
elim_graph_size (elim_graph *g)
{
  return g->nodes.length ();
}


/* Add NODE to graph G, if it doesn't exist already.  */

static inline void
elim_graph_add_node (elim_graph *g, int node)
{
  int x;
  int t;

  FOR_EACH_VEC_ELT (g->nodes, x, t)
    if (t == node)
      return;
  g->nodes.safe_push (node);
}


/* Add the edge PRED->SUCC to graph G.  */

static inline void
elim_graph_add_edge (elim_graph *g, int pred, int succ, location_t locus)
{
  g->edge_list.safe_push (pred);
  g->edge_list.safe_push (succ);
  g->edge_locus.safe_push (locus);
}


/* Remove an edge from graph G for which NODE is the predecessor, and
   return the successor node.  -1 is returned if there is no such edge.  */

static inline int
elim_graph_remove_succ_edge (elim_graph *g, int node, location_t *locus)
{
  int y;
  unsigned x;
  for (x = 0; x < g->edge_list.length (); x += 2)
    if (g->edge_list[x] == node)
      {
        g->edge_list[x] = -1;
	y = g->edge_list[x + 1];
	g->edge_list[x + 1] = -1;
	*locus = g->edge_locus[x / 2];
	g->edge_locus[x / 2] = UNKNOWN_LOCATION;
	return y;
      }
  *locus = UNKNOWN_LOCATION;
  return -1;
}


/* Find all the nodes in GRAPH which are successors to NODE in the
   edge list.  VAR will hold the partition number found.  CODE is the
   code fragment executed for every node found.  */

#define FOR_EACH_ELIM_GRAPH_SUCC(GRAPH, NODE, VAR, LOCUS, CODE)		\
do {									\
  unsigned x_;								\
  int y_;								\
  for (x_ = 0; x_ < (GRAPH)->edge_list.length (); x_ += 2)	\
    {									\
      y_ = (GRAPH)->edge_list[x_];					\
      if (y_ != (NODE))							\
        continue;							\
      (void) ((VAR) = (GRAPH)->edge_list[x_ + 1]);			\
      (void) ((LOCUS) = (GRAPH)->edge_locus[x_ / 2]);			\
      CODE;								\
    }									\
} while (0)


/* Find all the nodes which are predecessors of NODE in the edge list for
   GRAPH.  VAR will hold the partition number found.  CODE is the
   code fragment executed for every node found.  */

#define FOR_EACH_ELIM_GRAPH_PRED(GRAPH, NODE, VAR, LOCUS, CODE)		\
do {									\
  unsigned x_;								\
  int y_;								\
  for (x_ = 0; x_ < (GRAPH)->edge_list.length (); x_ += 2)	\
    {									\
      y_ = (GRAPH)->edge_list[x_ + 1];					\
      if (y_ != (NODE))							\
        continue;							\
      (void) ((VAR) = (GRAPH)->edge_list[x_]);				\
      (void) ((LOCUS) = (GRAPH)->edge_locus[x_ / 2]);			\
      CODE;								\
    }									\
} while (0)


/* Add T to elimination graph G.  */

static inline void
eliminate_name (elim_graph *g, int T)
{
  elim_graph_add_node (g, T);
}

/* Return true if this phi argument T should have a copy queued when using
   var_map MAP.  PHI nodes should contain only ssa_names and invariants.  A
   test for ssa_name is definitely simpler, but don't let invalid contents
   slip through in the meantime.  */

static inline bool
queue_phi_copy_p (var_map map, tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    { 
      if (var_to_partition (map, t) == NO_PARTITION)
        return true;
      return false;
    }
  gcc_checking_assert (is_gimple_min_invariant (t));
  return true;
}

/* Build elimination graph G for basic block BB on incoming PHI edge
   G->e.  */

static void
eliminate_build (elim_graph *g)
{
  tree Ti;
  int p0, pi;
  gphi_iterator gsi;

  clear_elim_graph (g);

  for (gsi = gsi_start_phis (g->e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      location_t locus;

      p0 = var_to_partition (g->map, gimple_phi_result (phi));
      /* Ignore results which are not in partitions.  */
      if (p0 == NO_PARTITION)
	continue;

      Ti = PHI_ARG_DEF (phi, g->e->dest_idx);
      /* See set_location_for_edge for the rationale.  */
      if (g->e->flags & EDGE_EH)
	locus = UNKNOWN_LOCATION;
      else
	locus = gimple_phi_arg_location_from_edge (phi, g->e);

      /* If this argument is a constant, or a SSA_NAME which is being
	 left in SSA form, just queue a copy to be emitted on this
	 edge.  */
      if (queue_phi_copy_p (g->map, Ti))
        {
	  /* Save constant copies until all other copies have been emitted
	     on this edge.  */
	  g->const_dests.safe_push (p0);
	  g->const_copies.safe_push (Ti);
	  g->copy_locus.safe_push (locus);
	}
      else
        {
	  pi = var_to_partition (g->map, Ti);
	  if (p0 != pi)
	    {
	      eliminate_name (g, p0);
	      eliminate_name (g, pi);
	      elim_graph_add_edge (g, p0, pi, locus);
	    }
	}
    }
}


/* Push successors of T onto the elimination stack for G.  */

static void
elim_forward (elim_graph *g, int T)
{
  int S;
  location_t locus;

  bitmap_set_bit (g->visited, T);
  FOR_EACH_ELIM_GRAPH_SUCC (g, T, S, locus,
    {
      if (!bitmap_bit_p (g->visited, S))
        elim_forward (g, S);
    });
  g->stack.safe_push (T);
}


/* Return 1 if there unvisited predecessors of T in graph G.  */

static int
elim_unvisited_predecessor (elim_graph *g, int T)
{
  int P;
  location_t locus;

  FOR_EACH_ELIM_GRAPH_PRED (g, T, P, locus,
    {
      if (!bitmap_bit_p (g->visited, P))
        return 1;
    });
  return 0;
}

/* Process predecessors first, and insert a copy.  */

static void
elim_backward (elim_graph *g, int T)
{
  int P;
  location_t locus;

  bitmap_set_bit (g->visited, T);
  FOR_EACH_ELIM_GRAPH_PRED (g, T, P, locus,
    {
      if (!bitmap_bit_p (g->visited, P))
        {
	  elim_backward (g, P);
	  insert_partition_copy_on_edge (g->e, P, T, locus);
	}
    });
}

/* Allocate a new pseudo register usable for storing values sitting
   in NAME (a decl or SSA name), i.e. with matching mode and attributes.  */

static rtx
get_temp_reg (tree name)
{
  tree type = TREE_TYPE (name);
  int unsignedp;
  machine_mode reg_mode = promote_ssa_mode (name, &unsignedp);
  if (reg_mode == BLKmode)
    return assign_temp (type, 0, 0);
  rtx x = gen_reg_rtx (reg_mode);
  if (POINTER_TYPE_P (type))
    mark_reg_pointer (x, TYPE_ALIGN (TREE_TYPE (type)));
  return x;
}

/* Insert required copies for T in graph G.  Check for a strongly connected
   region, and create a temporary to break the cycle if one is found.  */

static void
elim_create (elim_graph *g, int T)
{
  int P, S;
  location_t locus;

  if (elim_unvisited_predecessor (g, T))
    {
      tree var = partition_to_var (g->map, T);
      rtx U = get_temp_reg (var);
      int unsignedsrcp = TYPE_UNSIGNED (TREE_TYPE (var));

      insert_part_to_rtx_on_edge (g->e, U, T, UNKNOWN_LOCATION);
      FOR_EACH_ELIM_GRAPH_PRED (g, T, P, locus,
	{
	  if (!bitmap_bit_p (g->visited, P))
	    {
	      elim_backward (g, P);
	      insert_rtx_to_part_on_edge (g->e, P, U, unsignedsrcp, locus);
	    }
	});
    }
  else
    {
      S = elim_graph_remove_succ_edge (g, T, &locus);
      if (S != -1)
	{
	  bitmap_set_bit (g->visited, T);
	  insert_partition_copy_on_edge (g->e, T, S, locus);
	}
    }
}


/* Eliminate all the phi nodes on edge E in graph G.  */

static void
eliminate_phi (edge e, elim_graph *g)
{
  int x;

  gcc_assert (g->const_copies.length () == 0);
  gcc_assert (g->copy_locus.length () == 0);

  /* Abnormal edges already have everything coalesced.  */
  if (e->flags & EDGE_ABNORMAL)
    return;

  g->e = e;

  eliminate_build (g);

  if (elim_graph_size (g) != 0)
    {
      int part;

      bitmap_clear (g->visited);
      g->stack.truncate (0);

      FOR_EACH_VEC_ELT (g->nodes, x, part)
        {
	  if (!bitmap_bit_p (g->visited, part))
	    elim_forward (g, part);
	}

      bitmap_clear (g->visited);
      while (g->stack.length () > 0)
	{
	  x = g->stack.pop ();
	  if (!bitmap_bit_p (g->visited, x))
	    elim_create (g, x);
	}
    }

  /* If there are any pending constant copies, issue them now.  */
  while (g->const_copies.length () > 0)
    {
      int dest;
      tree src;
      location_t locus;

      src = g->const_copies.pop ();
      dest = g->const_dests.pop ();
      locus = g->copy_locus.pop ();
      insert_value_copy_on_edge (e, dest, src, locus);
    }
}


/* Remove each argument from PHI.  If an arg was the last use of an SSA_NAME,
   check to see if this allows another PHI node to be removed.  */

static void
remove_gimple_phi_args (gphi *phi)
{
  use_operand_p arg_p;
  ssa_op_iter iter;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Removing Dead PHI definition: ");
      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
    }

  FOR_EACH_PHI_ARG (arg_p, phi, iter, SSA_OP_USE)
    {
      tree arg = USE_FROM_PTR (arg_p);
      if (TREE_CODE (arg) == SSA_NAME)
        {
	  /* Remove the reference to the existing argument.  */
	  SET_USE (arg_p, NULL_TREE);
	  if (has_zero_uses (arg))
	    {
	      gimple *stmt;
	      gimple_stmt_iterator gsi;

	      stmt = SSA_NAME_DEF_STMT (arg);

	      /* Also remove the def if it is a PHI node.  */
	      if (gimple_code (stmt) == GIMPLE_PHI)
		{
		  remove_gimple_phi_args (as_a <gphi *> (stmt));
		  gsi = gsi_for_stmt (stmt);
		  remove_phi_node (&gsi, true);
		}

	    }
	}
    }
}

/* Remove any PHI node which is a virtual PHI, or a PHI with no uses.  */

static void
eliminate_useless_phis (void)
{
  basic_block bb;
  gphi_iterator gsi;
  tree result;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); )
        {
	  gphi *phi = gsi.phi ();
	  result = gimple_phi_result (phi);
	  if (virtual_operand_p (result))
	    remove_phi_node (&gsi, true);
          else
	    {
	      /* Also remove real PHIs with no uses.  */
	      if (has_zero_uses (result))
	        {
		  remove_gimple_phi_args (phi);
		  remove_phi_node (&gsi, true);
		}
	      else
		gsi_next (&gsi);
	    }
	}
    }
}


/* This function will rewrite the current program using the variable mapping
   found in MAP.  If the replacement vector VALUES is provided, any
   occurrences of partitions with non-null entries in the vector will be
   replaced with the expression in the vector instead of its mapped
   variable.  */

static void
rewrite_trees (var_map map)
{
  if (!flag_checking)
    return;

  basic_block bb;
  /* Search for PHIs where the destination has no partition, but one
     or more arguments has a partition.  This should not happen and can
     create incorrect code.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gphi_iterator gsi;
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  tree T0 = var_to_partition_to_var (map, gimple_phi_result (phi));
	  if (T0 == NULL_TREE)
	    {
	      size_t i;
	      for (i = 0; i < gimple_phi_num_args (phi); i++)
		{
		  tree arg = PHI_ARG_DEF (phi, i);

		  if (TREE_CODE (arg) == SSA_NAME
		      && var_to_partition (map, arg) != NO_PARTITION)
		    {
		      fprintf (stderr, "Argument of PHI is in a partition :(");
		      print_generic_expr (stderr, arg, TDF_SLIM);
		      fprintf (stderr, "), but the result is not :");
		      print_gimple_stmt (stderr, phi, 0, TDF_SLIM);
		      internal_error ("SSA corruption");
		    }
		}
	    }
	}
    }
}

/* Create a default def for VAR.  */

static void
create_default_def (tree var, void *arg ATTRIBUTE_UNUSED)
{
  if (!is_gimple_reg (var))
    return;

  tree ssa = get_or_create_ssa_default_def (cfun, var);
  gcc_assert (ssa);
}

/* Call CALLBACK for all PARM_DECLs and RESULT_DECLs for which
   assign_parms may ask for a default partition.  */

static void
for_all_parms (void (*callback)(tree var, void *arg), void *arg)
{
  for (tree var = DECL_ARGUMENTS (current_function_decl); var;
       var = DECL_CHAIN (var))
    callback (var, arg);
  if (!VOID_TYPE_P (TREE_TYPE (DECL_RESULT (current_function_decl))))
    callback (DECL_RESULT (current_function_decl), arg);
  if (cfun->static_chain_decl)
    callback (cfun->static_chain_decl, arg);
}

/* We need to pass two arguments to set_parm_default_def_partition,
   but for_all_parms only supports one.  Use a pair.  */

typedef std::pair<var_map, bitmap> parm_default_def_partition_arg;

/* Set in ARG's PARTS bitmap the bit corresponding to the partition in
   ARG's MAP containing VAR's default def.  */

static void
set_parm_default_def_partition (tree var, void *arg_)
{
  parm_default_def_partition_arg *arg = (parm_default_def_partition_arg *)arg_;
  var_map map = arg->first;
  bitmap parts = arg->second;

  if (!is_gimple_reg (var))
    return;

  tree ssa = ssa_default_def (cfun, var);
  gcc_assert (ssa);

  int version = var_to_partition (map, ssa);
  gcc_assert (version != NO_PARTITION);

  bool changed = bitmap_set_bit (parts, version);
  gcc_assert (changed);
}

/* Allocate and return a bitmap that has a bit set for each partition
   that contains a default def for a parameter.  */

static bitmap
get_parm_default_def_partitions (var_map map)
{
  bitmap parm_default_def_parts = BITMAP_ALLOC (NULL);

  parm_default_def_partition_arg
    arg = std::make_pair (map, parm_default_def_parts);

  for_all_parms (set_parm_default_def_partition, &arg);

  return parm_default_def_parts;
}

/* Allocate and return a bitmap that has a bit set for each partition
   that contains an undefined value.  */

static bitmap
get_undefined_value_partitions (var_map map)
{
  bitmap undefined_value_parts = BITMAP_ALLOC (NULL);

  for (unsigned int i = 1; i < num_ssa_names; i++)
    {
      tree var = ssa_name (i);
      if (var
	  && !virtual_operand_p (var)
	  && !has_zero_uses (var)
	  && ssa_undefined_value_p (var))
	{
	  const int p = var_to_partition (map, var);
	  if (p != NO_PARTITION)
	    bitmap_set_bit (undefined_value_parts, p);
	}
    }

  return undefined_value_parts;
}

/* Given the out-of-ssa info object SA (with prepared partitions)
   eliminate all phi nodes in all basic blocks.  Afterwards no
   basic block will have phi nodes anymore and there are possibly
   some RTL instructions inserted on edges.  */

void
expand_phi_nodes (struct ssaexpand *sa)
{
  basic_block bb;
  elim_graph g (sa->map);

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb,
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    if (!gimple_seq_empty_p (phi_nodes (bb)))
      {
	edge e;
	edge_iterator ei;
	FOR_EACH_EDGE (e, ei, bb->preds)
	  eliminate_phi (e, &g);
	set_phi_nodes (bb, NULL);
	/* We can't redirect EH edges in RTL land, so we need to do this
	   here.  Redirection happens only when splitting is necessary,
	   which it is only for critical edges, normally.  For EH edges
	   it might also be necessary when the successor has more than
	   one predecessor.  In that case the edge is either required to
	   be fallthru (which EH edges aren't), or the predecessor needs
	   to end with a jump (which again, isn't the case with EH edges).
	   Hence, split all EH edges on which we inserted instructions
	   and whose successor has multiple predecessors.  */
	for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
	  {
	    if (e->insns.r && (e->flags & EDGE_EH)
		&& !single_pred_p (e->dest))
	      {
		rtx_insn *insns = e->insns.r;
		basic_block bb;
		e->insns.r = NULL;
		bb = split_edge (e);
		single_pred_edge (bb)->insns.r = insns;
	      }
	    else
	      ei_next (&ei);
	  }
      }
}


/* Remove the ssa-names in the current function and translate them into normal
   compiler variables.  PERFORM_TER is true if Temporary Expression Replacement
   should also be used.  */

static void
remove_ssa_form (bool perform_ter, struct ssaexpand *sa)
{
  bitmap values = NULL;
  var_map map;

  for_all_parms (create_default_def, NULL);
  map = init_var_map (num_ssa_names);
  coalesce_ssa_name (map);

  /* Return to viewing the variable list as just all reference variables after
     coalescing has been performed.  */
  partition_view_normal (map);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "After Coalescing:\n");
      dump_var_map (dump_file, map);
    }

  if (perform_ter)
    {
      values = find_replaceable_exprs (map);
      if (values && dump_file && (dump_flags & TDF_DETAILS))
	dump_replaceable_exprs (dump_file, values);
    }

  rewrite_trees (map);

  sa->map = map;
  sa->values = values;
  sa->partitions_for_parm_default_defs = get_parm_default_def_partitions (map);
  sa->partitions_for_undefined_values = get_undefined_value_partitions (map);
}


/* If not already done so for basic block BB, assign increasing uids
   to each of its instructions.  */

static void
maybe_renumber_stmts_bb (basic_block bb)
{
  unsigned i = 0;
  gimple_stmt_iterator gsi;

  if (!bb->aux)
    return;
  bb->aux = NULL;
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      gimple_set_uid (stmt, i);
      i++;
    }
}


/* Return true if we can determine that the SSA_NAMEs RESULT (a result
   of a PHI node) and ARG (one of its arguments) conflict.  Return false
   otherwise, also when we simply aren't sure.  */

static bool
trivially_conflicts_p (basic_block bb, tree result, tree arg)
{
  use_operand_p use;
  imm_use_iterator imm_iter;
  gimple *defa = SSA_NAME_DEF_STMT (arg);

  /* If ARG isn't defined in the same block it's too complicated for
     our little mind.  */
  if (gimple_bb (defa) != bb)
    return false;

  FOR_EACH_IMM_USE_FAST (use, imm_iter, result)
    {
      gimple *use_stmt = USE_STMT (use);
      if (is_gimple_debug (use_stmt))
	continue;
      /* Now, if there's a use of RESULT that lies outside this basic block,
	 then there surely is a conflict with ARG.  */
      if (gimple_bb (use_stmt) != bb)
	return true;
      if (gimple_code (use_stmt) == GIMPLE_PHI)
	continue;
      /* The use now is in a real stmt of BB, so if ARG was defined
         in a PHI node (like RESULT) both conflict.  */
      if (gimple_code (defa) == GIMPLE_PHI)
	return true;
      maybe_renumber_stmts_bb (bb);
      /* If the use of RESULT occurs after the definition of ARG,
         the two conflict too.  */
      if (gimple_uid (defa) < gimple_uid (use_stmt))
	return true;
    }

  return false;
}


/* Search every PHI node for arguments associated with backedges which
   we can trivially determine will need a copy (the argument is either
   not an SSA_NAME or the argument has a different underlying variable
   than the PHI result).

   Insert a copy from the PHI argument to a new destination at the
   end of the block with the backedge to the top of the loop.  Update
   the PHI argument to reference this new destination.  */

static void
insert_backedge_copies (void)
{
  basic_block bb;
  gphi_iterator gsi;

  mark_dfs_back_edges ();

  FOR_EACH_BB_FN (bb, cfun)
    {
      /* Mark block as possibly needing calculation of UIDs.  */
      bb->aux = &bb->aux;

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  tree result = gimple_phi_result (phi);
	  size_t i;

	  if (virtual_operand_p (result))
	    continue;

	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      edge e = gimple_phi_arg_edge (phi, i);
	      /* We are only interested in copies emitted on critical
                 backedges.  */
	      if (!(e->flags & EDGE_DFS_BACK)
		  || !EDGE_CRITICAL_P (e))
		continue;

	      /* If the argument is not an SSA_NAME, then we will need a
		 constant initialization.  If the argument is an SSA_NAME then
		 a copy statement may be needed.  First handle the case
		 where we cannot insert before the argument definition.  */
	      if (TREE_CODE (arg) != SSA_NAME
		  || (gimple_code (SSA_NAME_DEF_STMT (arg)) == GIMPLE_PHI
		      && trivially_conflicts_p (bb, result, arg)))
		{
		  tree name;
		  gassign *stmt;
		  gimple *last = NULL;
		  gimple_stmt_iterator gsi2;

		  gsi2 = gsi_last_bb (gimple_phi_arg_edge (phi, i)->src);
		  if (!gsi_end_p (gsi2))
		    last = gsi_stmt (gsi2);

		  /* In theory the only way we ought to get back to the
		     start of a loop should be with a COND_EXPR or GOTO_EXPR.
		     However, better safe than sorry.
		     If the block ends with a control statement or
		     something that might throw, then we have to
		     insert this assignment before the last
		     statement.  Else insert it after the last statement.  */
		  if (last && stmt_ends_bb_p (last))
		    {
		      /* If the last statement in the block is the definition
			 site of the PHI argument, then we can't insert
			 anything after it.  */
		      if (TREE_CODE (arg) == SSA_NAME
			  && SSA_NAME_DEF_STMT (arg) == last)
			continue;
		    }

		  /* Create a new instance of the underlying variable of the
		     PHI result.  */
		  name = copy_ssa_name (result);
		  stmt = gimple_build_assign (name,
					      gimple_phi_arg_def (phi, i));

		  /* copy location if present.  */
		  if (gimple_phi_arg_has_location (phi, i))
		    gimple_set_location (stmt,
					 gimple_phi_arg_location (phi, i));

		  /* Insert the new statement into the block and update
		     the PHI node.  */
		  if (last && stmt_ends_bb_p (last))
		    gsi_insert_before (&gsi2, stmt, GSI_NEW_STMT);
		  else
		    gsi_insert_after (&gsi2, stmt, GSI_NEW_STMT);
		  SET_PHI_ARG_DEF (phi, i, name);
		}
	      /* Insert a copy before the definition of the backedge value
		 and adjust all conflicting uses.  */
	      else if (trivially_conflicts_p (bb, result, arg))
		{
		  gimple *def = SSA_NAME_DEF_STMT (arg);
		  if (gimple_nop_p (def)
		      || gimple_code (def) == GIMPLE_PHI)
		    continue;
		  tree name = copy_ssa_name (result);
		  gimple *stmt = gimple_build_assign (name, result);
		  imm_use_iterator imm_iter;
		  gimple *use_stmt;
		  /* The following matches trivially_conflicts_p.  */
		  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, result)
		    {
		      if (gimple_bb (use_stmt) != bb
			  || (gimple_code (use_stmt) != GIMPLE_PHI
			      && (maybe_renumber_stmts_bb (bb), true)
			      && gimple_uid (use_stmt) > gimple_uid (def)))
			{
			  use_operand_p use;
			  FOR_EACH_IMM_USE_ON_STMT (use, imm_iter)
			    SET_USE (use, name);
			}
		    }
		  gimple_stmt_iterator gsi = gsi_for_stmt (def);
		  gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
		}
	    }
	}

      /* Unmark this block again.  */
      bb->aux = NULL;
    }
}

/* Free all memory associated with going out of SSA form.  SA is
   the outof-SSA info object.  */

void
finish_out_of_ssa (struct ssaexpand *sa)
{
  free (sa->partition_to_pseudo);
  if (sa->values)
    BITMAP_FREE (sa->values);
  delete_var_map (sa->map);
  BITMAP_FREE (sa->partitions_for_parm_default_defs);
  BITMAP_FREE (sa->partitions_for_undefined_values);
  memset (sa, 0, sizeof *sa);
}

/* Take the current function out of SSA form, translating PHIs as described in
   R. Morgan, ``Building an Optimizing Compiler'',
   Butterworth-Heinemann, Boston, MA, 1998. pp 176-186.  */

unsigned int
rewrite_out_of_ssa (struct ssaexpand *sa)
{
  /* If elimination of a PHI requires inserting a copy on a backedge,
     then we will have to split the backedge which has numerous
     undesirable performance effects.

     A significant number of such cases can be handled here by inserting
     copies into the loop itself.  */
  insert_backedge_copies ();


  /* Eliminate PHIs which are of no use, such as virtual or dead phis.  */
  eliminate_useless_phis ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    gimple_dump_cfg (dump_file, dump_flags & ~TDF_DETAILS);

  remove_ssa_form (flag_tree_ter, sa);

  if (dump_file && (dump_flags & TDF_DETAILS))
    gimple_dump_cfg (dump_file, dump_flags & ~TDF_DETAILS);

  return 0;
}
