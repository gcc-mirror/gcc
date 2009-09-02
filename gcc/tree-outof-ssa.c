/* Convert a program in SSA form into Normal form.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "ggc.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "bitmap.h"
#include "tree-flow.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "toplev.h"
#include "expr.h"
#include "ssaexpand.h"


DEF_VEC_I(source_location);
DEF_VEC_ALLOC_I(source_location,heap);

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
 
typedef struct _elim_graph {
  /* Size of the elimination vectors.  */
  int size;

  /* List of nodes in the elimination graph.  */
  VEC(int,heap) *nodes;

  /*  The predecessor and successor edge list.  */
  VEC(int,heap) *edge_list;

  /* Source locus on each edge */
  VEC(source_location,heap) *edge_locus;

  /* Visited vector.  */
  sbitmap visited;

  /* Stack for visited nodes.  */
  VEC(int,heap) *stack;
  
  /* The variable partition map.  */
  var_map map;

  /* Edge being eliminated by this graph.  */
  edge e;

  /* List of constant copies to emit.  These are pushed on in pairs.  */
  VEC(int,heap) *const_dests;
  VEC(tree,heap) *const_copies;

  /* Source locations for any constant copies.  */
  VEC(source_location,heap) *copy_locus;
} *elim_graph;


/* For an edge E find out a good source location to associate with
   instructions inserted on edge E.  If E has an implicit goto set,
   use its location.  Otherwise search instructions in predecessors
   of E for a location, and use that one.  That makes sense because
   we insert on edges for PHI nodes, and effects of PHIs happen on
   the end of the predecessor conceptually.  */

static void
set_location_for_edge (edge e)
{
  if (e->goto_locus)
    {
      set_curr_insn_source_location (e->goto_locus);
      set_curr_insn_block (e->goto_block);
    }
  else
    {
      basic_block bb = e->src;
      gimple_stmt_iterator gsi;

      do
	{
	  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);
	      if (is_gimple_debug (stmt))
		continue;
	      if (gimple_has_location (stmt) || gimple_block (stmt))
		{
		  set_curr_insn_source_location (gimple_location (stmt));
		  set_curr_insn_block (gimple_block (stmt));
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

/* Emit insns to copy SRC into DEST converting SRC if necessary.  */

static inline rtx
emit_partition_copy (rtx dest, rtx src, int unsignedsrcp)
{
  rtx seq;

  start_sequence ();

  if (GET_MODE (src) != VOIDmode && GET_MODE (src) != GET_MODE (dest))
    src = convert_to_mode (GET_MODE (dest), src, unsignedsrcp);
  emit_move_insn (dest, src);

  seq = get_insns ();
  end_sequence ();

  return seq;
}

/* Insert a copy instruction from partition SRC to DEST onto edge E.  */

static void
insert_partition_copy_on_edge (edge e, int dest, int src, source_location locus)
{
  rtx seq;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Inserting a partition copy on edge BB%d->BB%d :"
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
    set_curr_insn_source_location (locus);

  seq = emit_partition_copy (SA.partition_to_pseudo[dest],
			     SA.partition_to_pseudo[src],
			     TYPE_UNSIGNED (TREE_TYPE (
			       partition_to_var (SA.map, src))));

  insert_insn_on_edge (seq, e);
}

/* Insert a copy instruction from expression SRC to partition DEST
   onto edge E.  */

static void
insert_value_copy_on_edge (edge e, int dest, tree src, source_location locus)
{
  rtx seq, x;
  enum machine_mode dest_mode, src_mode;
  int unsignedp;
  tree var;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Inserting a value copy on edge BB%d->BB%d : PART.%d = ",
	       e->src->index,
	       e->dest->index, dest);
      print_generic_expr (dump_file, src, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  gcc_assert (SA.partition_to_pseudo[dest]);

  set_location_for_edge (e);
  /* If a locus is provided, override the default.  */
  if (locus)
    set_curr_insn_source_location (locus);

  start_sequence ();

  var = SSA_NAME_VAR (partition_to_var (SA.map, dest));
  src_mode = TYPE_MODE (TREE_TYPE (src));
  dest_mode = promote_decl_mode (var, &unsignedp);
  gcc_assert (src_mode == TYPE_MODE (TREE_TYPE (var)));
  gcc_assert (dest_mode == GET_MODE (SA.partition_to_pseudo[dest]));

  if (src_mode != dest_mode)
    {
      x = expand_expr (src, NULL, src_mode, EXPAND_NORMAL);
      x = convert_modes (dest_mode, src_mode, x, unsignedp);
    }
  else
    x = expand_expr (src, SA.partition_to_pseudo[dest],
		     dest_mode, EXPAND_NORMAL);

  if (x != SA.partition_to_pseudo[dest])
    emit_move_insn (SA.partition_to_pseudo[dest], x);
  seq = get_insns ();
  end_sequence ();

  insert_insn_on_edge (seq, e);
}

/* Insert a copy instruction from RTL expression SRC to partition DEST
   onto edge E.  */

static void
insert_rtx_to_part_on_edge (edge e, int dest, rtx src, int unsignedsrcp,
			    source_location locus)
{
  rtx seq;
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
    set_curr_insn_source_location (locus);

  seq = emit_partition_copy (SA.partition_to_pseudo[dest],
			     src,
			     unsignedsrcp);

  insert_insn_on_edge (seq, e);
}

/* Insert a copy instruction from partition SRC to RTL lvalue DEST
   onto edge E.  */

static void
insert_part_to_rtx_on_edge (edge e, rtx dest, int src, source_location locus)
{
  rtx seq;
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
    set_curr_insn_source_location (locus);

  seq = emit_partition_copy (dest,
			     SA.partition_to_pseudo[src],
			     TYPE_UNSIGNED (TREE_TYPE (
			       partition_to_var (SA.map, src))));

  insert_insn_on_edge (seq, e);
}


/* Create an elimination graph with SIZE nodes and associated data
   structures.  */

static elim_graph
new_elim_graph (int size)
{
  elim_graph g = (elim_graph) xmalloc (sizeof (struct _elim_graph));

  g->nodes = VEC_alloc (int, heap, 30);
  g->const_dests = VEC_alloc (int, heap, 20);
  g->const_copies = VEC_alloc (tree, heap, 20);
  g->copy_locus = VEC_alloc (source_location, heap, 10);
  g->edge_list = VEC_alloc (int, heap, 20);
  g->edge_locus = VEC_alloc (source_location, heap, 10);
  g->stack = VEC_alloc (int, heap, 30);
  
  g->visited = sbitmap_alloc (size);

  return g;
}


/* Empty elimination graph G.  */

static inline void
clear_elim_graph (elim_graph g)
{
  VEC_truncate (int, g->nodes, 0);
  VEC_truncate (int, g->edge_list, 0);
  VEC_truncate (source_location, g->edge_locus, 0);
}


/* Delete elimination graph G.  */

static inline void
delete_elim_graph (elim_graph g)
{
  sbitmap_free (g->visited);
  VEC_free (int, heap, g->stack);
  VEC_free (int, heap, g->edge_list);
  VEC_free (tree, heap, g->const_copies);
  VEC_free (int, heap, g->const_dests);
  VEC_free (int, heap, g->nodes);
  VEC_free (source_location, heap, g->copy_locus);
  VEC_free (source_location, heap, g->edge_locus);

  free (g);
}


/* Return the number of nodes in graph G.  */

static inline int
elim_graph_size (elim_graph g)
{
  return VEC_length (int, g->nodes);
}


/* Add NODE to graph G, if it doesn't exist already.  */

static inline void 
elim_graph_add_node (elim_graph g, int node)
{
  int x;
  int t;

  for (x = 0; VEC_iterate (int, g->nodes, x, t); x++)
    if (t == node)
      return;
  VEC_safe_push (int, heap, g->nodes, node);
}


/* Add the edge PRED->SUCC to graph G.  */

static inline void
elim_graph_add_edge (elim_graph g, int pred, int succ, source_location locus)
{
  VEC_safe_push (int, heap, g->edge_list, pred);
  VEC_safe_push (int, heap, g->edge_list, succ);
  VEC_safe_push (source_location, heap, g->edge_locus, locus);
}


/* Remove an edge from graph G for which NODE is the predecessor, and
   return the successor node.  -1 is returned if there is no such edge.  */

static inline int
elim_graph_remove_succ_edge (elim_graph g, int node, source_location *locus)
{
  int y;
  unsigned x;
  for (x = 0; x < VEC_length (int, g->edge_list); x += 2)
    if (VEC_index (int, g->edge_list, x) == node)
      {
        VEC_replace (int, g->edge_list, x, -1);
	y = VEC_index (int, g->edge_list, x + 1);
	VEC_replace (int, g->edge_list, x + 1, -1);
	*locus = VEC_index (source_location, g->edge_locus, x / 2);
	VEC_replace (source_location, g->edge_locus, x / 2, UNKNOWN_LOCATION);
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
  for (x_ = 0; x_ < VEC_length (int, (GRAPH)->edge_list); x_ += 2)	\
    {									\
      y_ = VEC_index (int, (GRAPH)->edge_list, x_);			\
      if (y_ != (NODE))							\
        continue;							\
      (VAR) = VEC_index (int, (GRAPH)->edge_list, x_ + 1);		\
      (LOCUS) = VEC_index (source_location, (GRAPH)->edge_locus, x_ / 2); \
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
  for (x_ = 0; x_ < VEC_length (int, (GRAPH)->edge_list); x_ += 2)	\
    {									\
      y_ = VEC_index (int, (GRAPH)->edge_list, x_ + 1);			\
      if (y_ != (NODE))							\
        continue;							\
      (VAR) = VEC_index (int, (GRAPH)->edge_list, x_);			\
      (LOCUS) = VEC_index (source_location, (GRAPH)->edge_locus, x_ / 2); \
      CODE;								\
    }									\
} while (0)


/* Add T to elimination graph G.  */

static inline void
eliminate_name (elim_graph g, int T)
{
  elim_graph_add_node (g, T);
}


/* Build elimination graph G for basic block BB on incoming PHI edge
   G->e.  */

static void
eliminate_build (elim_graph g)
{
  tree Ti;
  int p0, pi;
  gimple_stmt_iterator gsi;

  clear_elim_graph (g);
  
  for (gsi = gsi_start_phis (g->e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      source_location locus;

      p0 = var_to_partition (g->map, gimple_phi_result (phi));
      /* Ignore results which are not in partitions.  */
      if (p0 == NO_PARTITION)
	continue;

      Ti = PHI_ARG_DEF (phi, g->e->dest_idx);
      locus = gimple_phi_arg_location_from_edge (phi, g->e);

      /* If this argument is a constant, or a SSA_NAME which is being
	 left in SSA form, just queue a copy to be emitted on this
	 edge.  */
      if (!phi_ssa_name_p (Ti)
	  || (TREE_CODE (Ti) == SSA_NAME
	      && var_to_partition (g->map, Ti) == NO_PARTITION))
        {
	  /* Save constant copies until all other copies have been emitted
	     on this edge.  */
	  VEC_safe_push (int, heap, g->const_dests, p0);
	  VEC_safe_push (tree, heap, g->const_copies, Ti);
	  VEC_safe_push (source_location, heap, g->copy_locus, locus);
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
elim_forward (elim_graph g, int T)
{
  int S;
  source_location locus;

  SET_BIT (g->visited, T);
  FOR_EACH_ELIM_GRAPH_SUCC (g, T, S, locus,
    {
      if (!TEST_BIT (g->visited, S))
        elim_forward (g, S);
    });
  VEC_safe_push (int, heap, g->stack, T);
}


/* Return 1 if there unvisited predecessors of T in graph G.  */

static int
elim_unvisited_predecessor (elim_graph g, int T)
{
  int P;
  source_location locus;

  FOR_EACH_ELIM_GRAPH_PRED (g, T, P, locus,
    {
      if (!TEST_BIT (g->visited, P))
        return 1;
    });
  return 0;
}

/* Process predecessors first, and insert a copy.  */

static void
elim_backward (elim_graph g, int T)
{
  int P;
  source_location locus;

  SET_BIT (g->visited, T);
  FOR_EACH_ELIM_GRAPH_PRED (g, T, P, locus,
    {
      if (!TEST_BIT (g->visited, P))
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
  tree var = TREE_CODE (name) == SSA_NAME ? SSA_NAME_VAR (name) : name;
  tree type = TREE_TYPE (var);
  int unsignedp;
  enum machine_mode reg_mode = promote_decl_mode (var, &unsignedp);
  rtx x = gen_reg_rtx (reg_mode);
  if (POINTER_TYPE_P (type))
    mark_reg_pointer (x, TYPE_ALIGN (TREE_TYPE (TREE_TYPE (var))));
  return x;
}

/* Insert required copies for T in graph G.  Check for a strongly connected 
   region, and create a temporary to break the cycle if one is found.  */

static void 
elim_create (elim_graph g, int T)
{
  int P, S;
  source_location locus;

  if (elim_unvisited_predecessor (g, T))
    {
      tree var = partition_to_var (g->map, T);
      rtx U = get_temp_reg (var);
      int unsignedsrcp = TYPE_UNSIGNED (TREE_TYPE (var));

      insert_part_to_rtx_on_edge (g->e, U, T, UNKNOWN_LOCATION);
      FOR_EACH_ELIM_GRAPH_PRED (g, T, P, locus,
	{
	  if (!TEST_BIT (g->visited, P))
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
	  SET_BIT (g->visited, T);
	  insert_partition_copy_on_edge (g->e, T, S, locus);
	}
    }
}


/* Eliminate all the phi nodes on edge E in graph G.  */

static void
eliminate_phi (edge e, elim_graph g)
{
  int x;

  gcc_assert (VEC_length (tree, g->const_copies) == 0);
  gcc_assert (VEC_length (source_location, g->copy_locus) == 0);

  /* Abnormal edges already have everything coalesced.  */
  if (e->flags & EDGE_ABNORMAL)
    return;

  g->e = e;

  eliminate_build (g);

  if (elim_graph_size (g) != 0)
    {
      int part;

      sbitmap_zero (g->visited);
      VEC_truncate (int, g->stack, 0);

      for (x = 0; VEC_iterate (int, g->nodes, x, part); x++)
        {
	  if (!TEST_BIT (g->visited, part))
	    elim_forward (g, part);
	}
       
      sbitmap_zero (g->visited);
      while (VEC_length (int, g->stack) > 0)
	{
	  x = VEC_pop (int, g->stack);
	  if (!TEST_BIT (g->visited, x))
	    elim_create (g, x);
	}
    }

  /* If there are any pending constant copies, issue them now.  */
  while (VEC_length (tree, g->const_copies) > 0)
    {
      int dest;
      tree src;
      source_location locus;

      src = VEC_pop (tree, g->const_copies);
      dest = VEC_pop (int, g->const_dests);
      locus = VEC_pop (source_location, g->copy_locus);
      insert_value_copy_on_edge (e, dest, src, locus);
    }
}


/* Remove each argument from PHI.  If an arg was the last use of an SSA_NAME, 
   check to see if this allows another PHI node to be removed.  */

static void
remove_gimple_phi_args (gimple phi)
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
	      gimple stmt;
	      gimple_stmt_iterator gsi;

	      stmt = SSA_NAME_DEF_STMT (arg);

	      /* Also remove the def if it is a PHI node.  */
	      if (gimple_code (stmt) == GIMPLE_PHI)
		{
		  remove_gimple_phi_args (stmt);
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
  gimple_stmt_iterator gsi;
  tree result;

  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); )
        {
	  gimple phi = gsi_stmt (gsi);
	  result = gimple_phi_result (phi);
	  if (!is_gimple_reg (SSA_NAME_VAR (result)))
	    {
#ifdef ENABLE_CHECKING
	      size_t i;
	      /* There should be no arguments which are not virtual, or the
	         results will be incorrect.  */
	      for (i = 0; i < gimple_phi_num_args (phi); i++)
	        {
		  tree arg = PHI_ARG_DEF (phi, i);
		  if (TREE_CODE (arg) == SSA_NAME 
		      && is_gimple_reg (SSA_NAME_VAR (arg)))
		    {
		      fprintf (stderr, "Argument of PHI is not virtual (");
		      print_generic_expr (stderr, arg, TDF_SLIM);
		      fprintf (stderr, "), but the result is :");
		      print_gimple_stmt (stderr, phi, 0, TDF_SLIM);
		      internal_error ("SSA corruption");
		    }
		}
#endif
	      remove_phi_node (&gsi, true);
	    }
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
rewrite_trees (var_map map ATTRIBUTE_UNUSED)
{
#ifdef ENABLE_CHECKING
  basic_block bb;
  /* Search for PHIs where the destination has no partition, but one
     or more arguments has a partition.  This should not happen and can
     create incorrect code.  */
  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
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
#endif
}

/* Given the out-of-ssa info object SA (with prepared partitions)
   eliminate all phi nodes in all basic blocks.  Afterwards no
   basic block will have phi nodes anymore and there are possibly
   some RTL instructions inserted on edges.  */

void
expand_phi_nodes (struct ssaexpand *sa)
{
  basic_block bb;
  elim_graph g = new_elim_graph (sa->map->num_partitions);
  g->map = sa->map;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR->next_bb, EXIT_BLOCK_PTR, next_bb)
    if (!gimple_seq_empty_p (phi_nodes (bb)))
      {
	edge e;
	edge_iterator ei;
	FOR_EACH_EDGE (e, ei, bb->preds)
	  eliminate_phi (e, g);
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
		rtx insns = e->insns.r;
		basic_block bb;
		e->insns.r = NULL_RTX;
		bb = split_edge (e);
		single_pred_edge (bb)->insns.r = insns;
	      }
	    else
	      ei_next (&ei);
	  }
      }

  delete_elim_graph (g);
}


/* Remove the ssa-names in the current function and translate them into normal
   compiler variables.  PERFORM_TER is true if Temporary Expression Replacement
   should also be used.  */

static void
remove_ssa_form (bool perform_ter, struct ssaexpand *sa)
{
  bitmap values = NULL;
  var_map map;
  unsigned i;

  map = coalesce_ssa_name ();

  /* Return to viewing the variable list as just all reference variables after
     coalescing has been performed.  */
  partition_view_normal (map, false);

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
  sa->partition_has_default_def = BITMAP_ALLOC (NULL);
  for (i = 1; i < num_ssa_names; i++)
    {
      tree t = ssa_name (i);
      if (t && SSA_NAME_IS_DEFAULT_DEF (t))
	{
	  int p = var_to_partition (map, t);
	  if (p != NO_PARTITION)
	    bitmap_set_bit (sa->partition_has_default_def, p);
	}
    }
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
      gimple stmt = gsi_stmt (gsi);
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
  gimple defa = SSA_NAME_DEF_STMT (arg);

  /* If ARG isn't defined in the same block it's too complicated for
     our little mind.  */
  if (gimple_bb (defa) != bb)
    return false;

  FOR_EACH_IMM_USE_FAST (use, imm_iter, result)
    {
      gimple use_stmt = USE_STMT (use);
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
  gimple_stmt_iterator gsi;

  FOR_EACH_BB (bb)
    {
      /* Mark block as possibly needing calculation of UIDs.  */
      bb->aux = &bb->aux;

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
	  tree result = gimple_phi_result (phi);
	  tree result_var;
	  size_t i;

	  if (!is_gimple_reg (result))
	    continue;

	  result_var = SSA_NAME_VAR (result);
	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      edge e = gimple_phi_arg_edge (phi, i);

	      /* If the argument is not an SSA_NAME, then we will need a 
		 constant initialization.  If the argument is an SSA_NAME with
		 a different underlying variable then a copy statement will be 
		 needed.  */
	      if ((e->flags & EDGE_DFS_BACK)
		  && (TREE_CODE (arg) != SSA_NAME
		      || SSA_NAME_VAR (arg) != result_var
		      || trivially_conflicts_p (bb, result, arg)))
		{
		  tree name;
		  gimple stmt, last = NULL;
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
		  stmt = gimple_build_assign (result_var,
					      gimple_phi_arg_def (phi, i));
		  name = make_ssa_name (result_var, stmt);
		  gimple_assign_set_lhs (stmt, name);

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
  BITMAP_FREE (sa->partition_has_default_def);
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
