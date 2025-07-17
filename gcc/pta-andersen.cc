/* Andersen-style solver for tree based points-to analysis
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

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

#include "tree-ssa-structalias.h"
#include "pta-andersen.h"

/* During variable substitution and the offline version of indirect
   cycle finding, we create nodes to represent dereferences and
   address taken constraints.  These represent where these start and
   end.  */
#define FIRST_REF_NODE (varmap).length ()
#define LAST_REF_NODE (FIRST_REF_NODE + (FIRST_REF_NODE - 1))

#define EXECUTE_IF_IN_NONNULL_BITMAP(a, b, c, d)	\
  if (a)						\
    EXECUTE_IF_SET_IN_BITMAP (a, b, c, d)

using namespace pointer_analysis;

/* Used for predecessor bitmaps.  */
static bitmap_obstack predbitmap_obstack;

/* Used for per-solver-iteration bitmaps.  */
static bitmap_obstack iteration_obstack;

typedef struct constraint_graph *constraint_graph_t;

/* The constraint graph is represented as an array of bitmaps
   containing successor nodes.  */

struct constraint_graph
{
  /* Size of this graph, which may be different than the number of
     nodes in the variable map.  */
  unsigned int size;

  /* Explicit successors of each node.  */
  bitmap *succs;

  /* Implicit predecessors of each node (Used for variable
     substitution). */
  bitmap *implicit_preds;

  /* Explicit predecessors of each node (Used for variable substitution).  */
  bitmap *preds;

  /* Indirect cycle representatives, or -1 if the node has no indirect
     cycles.  */
  int *indirect_cycles;

  /* Representative node for a node.  rep[a] == a unless the node has
     been unified.  */
  unsigned int *rep;

  /* Equivalence class representative for a label.  This is used for
     variable substitution.  */
  int *eq_rep;

  /* Pointer equivalence label for a node.  All nodes with the same
     pointer equivalence label can be unified together at some point
     (either during constraint optimization or after the constraint
     graph is built).  */
  unsigned int *pe;

  /* Pointer equivalence representative for a label.  This is used to
     handle nodes that are pointer equivalent but not location
     equivalent.  We can unite these once the addressof constraints
     are transformed into initial points-to sets.  */
  int *pe_rep;

  /* Pointer equivalence label for each node, used during variable
     substitution.  */
  unsigned int *pointer_label;

  /* Location equivalence label for each node, used during location
     equivalence finding.  */
  unsigned int *loc_label;

  /* Pointed-by set for each node, used during location equivalence
     finding.  This is pointed-by rather than pointed-to, because it
     is constructed using the predecessor graph.  */
  bitmap *pointed_by;

  /* Points to sets for pointer equivalence.  This is *not* the actual
     points-to sets for nodes.  */
  bitmap *points_to;

  /* Bitmap of nodes where the bit is set if the node is a direct
     node.  Used for variable substitution.  */
  sbitmap direct_nodes;

  /* Bitmap of nodes where the bit is set if the node is address
     taken.  Used for variable substitution.  */
  bitmap address_taken;

  /* Vector of complex constraints for each graph node.  Complex
     constraints are those involving dereferences or offsets that are
     not 0.  */
  vec<constraint_t> *complex;
};

static constraint_graph_t graph;

static void unify_nodes (constraint_graph_t, unsigned int, unsigned int, bool);


/* Return the representative node for NODE, if NODE has been unioned
   with another NODE.
   This function performs path compression along the way to finding
   the representative.  */

static unsigned int
find (unsigned int node)
{
  gcc_checking_assert (node < graph->size);
  if (graph->rep[node] != node)
    return graph->rep[node] = find (graph->rep[node]);
  return node;
}

/* Union the TO and FROM nodes to the TO nodes.
   Note that at some point in the future, we may want to do
   union-by-rank, in which case we are going to have to return the
   node we unified to.  */

static bool
unite (unsigned int to, unsigned int from)
{
  gcc_checking_assert (to < graph->size && from < graph->size);
  if (to != from && graph->rep[from] != to)
    {
      graph->rep[from] = to;
      return true;
    }
  return false;
}

/* Perform path compression for all nodes in the node representatives
   union-find structure.  */

static void
union_find_compress_all (void)
{
  unsigned int i;
  for (i = 0; i < graph->size; i++)
    find (i);
}

/* Print the constraint graph in dot format.  */

static void
dump_constraint_graph (FILE *file)
{
  unsigned int i;

  /* Only print the graph if it has already been initialized:  */
  if (!graph)
    return;

  /* Prints the header of the dot file:  */
  fprintf (file, "strict digraph {\n");
  fprintf (file, "  node [\n    shape = box\n  ]\n");
  fprintf (file, "  edge [\n    fontsize = \"12\"\n  ]\n");
  fprintf (file, "\n  // List of nodes and complex constraints in "
	   "the constraint graph:\n");

  /* The next lines print the nodes in the graph together with the
     complex constraints attached to them.  */
  for (i = 1; i < graph->size; i++)
    {
      if (i == FIRST_REF_NODE)
	continue;
      if (find (i) != i)
	continue;
      if (i < FIRST_REF_NODE)
	fprintf (file, "\"%s\"", get_varinfo (i)->name);
      else
	fprintf (file, "\"*%s\"", get_varinfo (i - FIRST_REF_NODE)->name);
      if (graph->complex[i].exists ())
	{
	  unsigned j;
	  constraint_t c;
	  fprintf (file, " [label=\"\\N\\n");
	  for (j = 0; graph->complex[i].iterate (j, &c); ++j)
	    {
	      dump_constraint (file, c);
	      fprintf (file, "\\l");
	    }
	  fprintf (file, "\"]");
	}
      fprintf (file, ";\n");
    }

  /* Go over the edges.  */
  fprintf (file, "\n  // Edges in the constraint graph:\n");
  for (i = 1; i < graph->size; i++)
    {
      unsigned j;
      bitmap_iterator bi;
      if (find (i) != i)
	continue;
      EXECUTE_IF_IN_NONNULL_BITMAP (graph->succs[i], 0, j, bi)
	{
	  unsigned to = find (j);
	  if (i == to)
	    continue;
	  if (i < FIRST_REF_NODE)
	    fprintf (file, "\"%s\"", get_varinfo (i)->name);
	  else
	    fprintf (file, "\"*%s\"", get_varinfo (i - FIRST_REF_NODE)->name);
	  fprintf (file, " -> ");
	  if (to < FIRST_REF_NODE)
	    fprintf (file, "\"%s\"", get_varinfo (to)->name);
	  else
	    fprintf (file, "\"*%s\"", get_varinfo (to - FIRST_REF_NODE)->name);
	  fprintf (file, ";\n");
	}
    }

  /* Prints the tail of the dot file.  */
  fprintf (file, "}\n");
}

/* Print out the constraint graph to stderr.  */

DEBUG_FUNCTION void
debug_constraint_graph (void)
{
  dump_constraint_graph (stderr);
}


/* SOLVER FUNCTIONS

   The solver is a simple worklist solver, that works on the following
   algorithm:

   sbitmap changed_nodes = all zeroes;
   changed_count = 0;
   For each node that is not already collapsed:
       changed_count++;
       set bit in changed nodes

   while (changed_count > 0)
   {
     compute topological ordering for constraint graph

     find and collapse cycles in the constraint graph (updating
     changed if necessary)

     for each node (n) in the graph in topological order:
       changed_count--;

       Process each complex constraint associated with the node,
       updating changed if necessary.

       For each outgoing edge from n, propagate the solution from n to
       the destination of the edge, updating changed as necessary.

   }  */

/* Return true if two constraint expressions A and B are equal.  */

static bool
constraint_expr_equal (struct constraint_expr a, struct constraint_expr b)
{
  return a.type == b.type && a.var == b.var && a.offset == b.offset;
}

/* Return true if constraint expression A is less than constraint expression
   B.  This is just arbitrary, but consistent, in order to give them an
   ordering.  */

static bool
constraint_expr_less (struct constraint_expr a, struct constraint_expr b)
{
  if (a.type == b.type)
    {
      if (a.var == b.var)
	return a.offset < b.offset;
      else
	return a.var < b.var;
    }
  else
    return a.type < b.type;
}

/* Return true if constraint A is less than constraint B.  This is just
   arbitrary, but consistent, in order to give them an ordering.  */

static bool
constraint_less (const constraint_t &a, const constraint_t &b)
{
  if (constraint_expr_less (a->lhs, b->lhs))
    return true;
  else if (constraint_expr_less (b->lhs, a->lhs))
    return false;
  else
    return constraint_expr_less (a->rhs, b->rhs);
}

/* Return true if two constraints A and B are equal.  */

static bool
constraint_equal (const constraint &a, const constraint &b)
{
  return constraint_expr_equal (a.lhs, b.lhs)
    && constraint_expr_equal (a.rhs, b.rhs);
}

/* Find a constraint LOOKFOR in the sorted constraint vector VEC.  */

static constraint_t
constraint_vec_find (vec<constraint_t> vec,
		     constraint &lookfor)
{
  unsigned int place;
  constraint_t found;

  if (!vec.exists ())
    return NULL;

  place = vec.lower_bound (&lookfor, constraint_less);
  if (place >= vec.length ())
    return NULL;
  found = vec[place];
  if (!constraint_equal (*found, lookfor))
    return NULL;
  return found;
}

/* Union two constraint vectors, TO and FROM.  Put the result in TO.
   Returns true of TO set is changed.  */

static bool
constraint_set_union (vec<constraint_t> *to,
		      vec<constraint_t> *from)
{
  int i;
  constraint_t c;
  bool any_change = false;

  FOR_EACH_VEC_ELT (*from, i, c)
    {
      if (constraint_vec_find (*to, *c) == NULL)
	{
	  unsigned int place = to->lower_bound (c, constraint_less);
	  to->safe_insert (place, c);
	  any_change = true;
	}
    }
  return any_change;
}

/* Expands the solution in SET to all sub-fields of variables included.  */

static bitmap
solution_set_expand (bitmap set, bitmap *expanded)
{
  bitmap_iterator bi;
  unsigned j;

  if (*expanded)
    return *expanded;

  *expanded = BITMAP_ALLOC (&iteration_obstack);

  /* In a first pass expand variables, once for each head to avoid
     quadratic behavior, to include all sub-fields.  */
  unsigned prev_head = 0;
  EXECUTE_IF_SET_IN_BITMAP (set, 0, j, bi)
    {
      varinfo_t v = get_varinfo (j);
      if (v->is_artificial_var
	  || v->is_full_var)
	continue;
      if (v->head != prev_head)
	{
	  varinfo_t head = get_varinfo (v->head);
	  unsigned num = 1;
	  for (varinfo_t n = vi_next (head); n != NULL; n = vi_next (n))
	    {
	      if (n->id != head->id + num)
		{
		  /* Usually sub variables are adjacent but since we
		     create pointed-to restrict representatives there
		     can be gaps as well.  */
		  bitmap_set_range (*expanded, head->id, num);
		  head = n;
		  num = 1;
		}
	      else
		num++;
	    }

	  bitmap_set_range (*expanded, head->id, num);
	  prev_head = v->head;
	}
    }

  /* And finally set the rest of the bits from SET in an efficient way.  */
  bitmap_ior_into (*expanded, set);

  return *expanded;
}

/* Union solution sets TO and DELTA, and add INC to each member of DELTA in the
   process.  */

static bool
set_union_with_increment (bitmap to, bitmap delta, HOST_WIDE_INT inc,
			  bitmap *expanded_delta)
{
  bool changed = false;
  bitmap_iterator bi;
  unsigned int i;

  /* If the solution of DELTA contains anything it is good enough to transfer
     this to TO.  */
  if (bitmap_bit_p (delta, anything_id))
    return bitmap_set_bit (to, anything_id);

  /* If the offset is unknown we have to expand the solution to
     all subfields.  */
  if (inc == UNKNOWN_OFFSET)
    {
      delta = solution_set_expand (delta, expanded_delta);
      changed |= bitmap_ior_into (to, delta);
      return changed;
    }

  /* For non-zero offset union the offsetted solution into the destination.  */
  EXECUTE_IF_SET_IN_BITMAP (delta, 0, i, bi)
    {
      varinfo_t vi = get_varinfo (i);

      /* If this is a variable with just one field just set its bit
	 in the result.  */
      if (vi->is_artificial_var
	  || vi->is_unknown_size_var
	  || vi->is_full_var)
	changed |= bitmap_set_bit (to, i);
      else
	{
	  HOST_WIDE_INT fieldoffset = vi->offset + inc;
	  unsigned HOST_WIDE_INT size = vi->size;

	  /* If the offset makes the pointer point to before the
	     variable use offset zero for the field lookup.  */
	  if (fieldoffset < 0)
	    vi = get_varinfo (vi->head);
	  else
	    vi = first_or_preceding_vi_for_offset (vi, fieldoffset);

	  do
	    {
	      changed |= bitmap_set_bit (to, vi->id);
	      if (vi->is_full_var
		  || vi->next == 0)
		break;

	      /* We have to include all fields that overlap the current field
		 shifted by inc.  */
	      vi = vi_next (vi);
	    }
	  while (vi->offset < fieldoffset + size);
	}
    }

  return changed;
}

/* Insert constraint C into the list of complex constraints for graph
   node VAR.  */

static void
insert_into_complex (constraint_graph_t graph,
		     unsigned int var, constraint_t c)
{
  vec<constraint_t> complex = graph->complex[var];
  unsigned int place = complex.lower_bound (c, constraint_less);

  /* Only insert constraints that do not already exist.  */
  if (place >= complex.length ()
      || !constraint_equal (*c, *complex[place]))
    graph->complex[var].safe_insert (place, c);
}


/* Condense two variable nodes into a single variable node, by moving
   all associated info from FROM to TO.  Returns true if TO node's
   constraint set changes after the merge.  */

static bool
merge_node_constraints (constraint_graph_t graph, unsigned int to,
			unsigned int from)
{
  unsigned int i;
  constraint_t c;
  bool any_change = false;

  gcc_checking_assert (find (from) == to);

  /* Move all complex constraints from src node into to node.  */
  FOR_EACH_VEC_ELT (graph->complex[from], i, c)
    {
      /* In complex constraints for node FROM, we may have either
	 a = *FROM, and *FROM = a, or an offseted constraint which are
	 always added to the rhs node's constraints.  */

      if (c->rhs.type == DEREF)
	c->rhs.var = to;
      else if (c->lhs.type == DEREF)
	c->lhs.var = to;
      else
	c->rhs.var = to;

    }
  any_change = constraint_set_union (&graph->complex[to],
				     &graph->complex[from]);
  graph->complex[from].release ();
  return any_change;
}

/* Remove edges involving NODE from GRAPH.  */

static void
clear_edges_for_node (constraint_graph_t graph, unsigned int node)
{
  if (graph->succs[node])
    BITMAP_FREE (graph->succs[node]);
}

/* Merge GRAPH nodes FROM and TO into node TO.  */

static void
merge_graph_nodes (constraint_graph_t graph, unsigned int to,
		   unsigned int from)
{
  if (graph->indirect_cycles[from] != -1)
    {
      /* If we have indirect cycles with the from node, and we have
	 none on the to node, the to node has indirect cycles from the
	 from node now that they are unified.
	 If indirect cycles exist on both, unify the nodes that they
	 are in a cycle with, since we know they are in a cycle with
	 each other.  */
      if (graph->indirect_cycles[to] == -1)
	graph->indirect_cycles[to] = graph->indirect_cycles[from];
    }

  /* Merge all the successor edges.  */
  if (graph->succs[from])
    {
      if (!graph->succs[to])
	graph->succs[to] = BITMAP_ALLOC (&pta_obstack);
      bitmap_ior_into (graph->succs[to],
		       graph->succs[from]);
    }

  clear_edges_for_node (graph, from);
}


/* Add an indirect graph edge to GRAPH, going from TO to FROM if
   it doesn't exist in the graph already.  */

static void
add_implicit_graph_edge (constraint_graph_t graph, unsigned int to,
			 unsigned int from)
{
  if (to == from)
    return;

  if (!graph->implicit_preds[to])
    graph->implicit_preds[to] = BITMAP_ALLOC (&predbitmap_obstack);

  if (bitmap_set_bit (graph->implicit_preds[to], from))
    stats.num_implicit_edges++;
}

/* Add a predecessor graph edge to GRAPH, going from TO to FROM if
   it doesn't exist in the graph already.
   Return false if the edge already existed, true otherwise.  */

static void
add_pred_graph_edge (constraint_graph_t graph, unsigned int to,
		     unsigned int from)
{
  if (!graph->preds[to])
    graph->preds[to] = BITMAP_ALLOC (&predbitmap_obstack);
  bitmap_set_bit (graph->preds[to], from);
}

/* Add a graph edge to GRAPH, going from FROM to TO if
   it doesn't exist in the graph already.
   Return false if the edge already existed, true otherwise.  */

static bool
add_graph_edge (constraint_graph_t graph, unsigned int to,
		unsigned int from)
{
  if (to == from)
    {
      return false;
    }
  else
    {
      bool r = false;

      if (!graph->succs[from])
	graph->succs[from] = BITMAP_ALLOC (&pta_obstack);

      /* The graph solving process does not avoid "triangles", thus
	 there can be multiple paths from a node to another involving
	 intermediate other nodes.  That causes extra copying which is
	 most difficult to avoid when the intermediate node is ESCAPED
	 because there are no edges added from ESCAPED.  Avoid
	 adding the direct edge FROM -> TO when we have FROM -> ESCAPED
	 and TO contains ESCAPED.
	 ???  Note this is only a heuristic, it does not prevent the
	 situation from occuring.  The heuristic helps PR38474 and
	 PR99912 significantly.  */
      if (to < FIRST_REF_NODE
	  && bitmap_bit_p (graph->succs[from], find (escaped_id))
	  && bitmap_bit_p (get_varinfo (find (to))->solution, escaped_id))
	{
	  stats.num_avoided_edges++;
	  return false;
	}

      if (bitmap_set_bit (graph->succs[from], to))
	{
	  r = true;
	  if (to < FIRST_REF_NODE && from < FIRST_REF_NODE)
	    stats.num_edges++;
	}
      return r;
    }
}

/* Initialize the constraint graph structure to contain SIZE nodes.  */

static void
init_graph (unsigned int size)
{
  unsigned int j;

  bitmap_obstack_initialize (&predbitmap_obstack);

  graph = XCNEW (struct constraint_graph);
  graph->size = size;
  graph->succs = XCNEWVEC (bitmap, graph->size);
  graph->indirect_cycles = XNEWVEC (int, graph->size);
  graph->rep = XNEWVEC (unsigned int, graph->size);
  /* ??? Macros do not support template types with multiple arguments,
     so we use a typedef to work around it.  */
  typedef vec<constraint_t> vec_constraint_t_heap;
  graph->complex = XCNEWVEC (vec_constraint_t_heap, size);
  graph->pe = XCNEWVEC (unsigned int, graph->size);
  graph->pe_rep = XNEWVEC (int, graph->size);

  for (j = 0; j < graph->size; j++)
    {
      graph->rep[j] = j;
      graph->pe_rep[j] = -1;
      graph->indirect_cycles[j] = -1;
    }
}

/* Build the constraint graph, adding only predecessor edges right now.  */

static void
build_pred_graph (void)
{
  int i;
  constraint_t c;
  unsigned int j;

  graph->implicit_preds = XCNEWVEC (bitmap, graph->size);
  graph->preds = XCNEWVEC (bitmap, graph->size);
  graph->pointer_label = XCNEWVEC (unsigned int, graph->size);
  graph->loc_label = XCNEWVEC (unsigned int, graph->size);
  graph->pointed_by = XCNEWVEC (bitmap, graph->size);
  graph->points_to = XCNEWVEC (bitmap, graph->size);
  graph->eq_rep = XNEWVEC (int, graph->size);
  graph->direct_nodes = sbitmap_alloc (graph->size);
  graph->address_taken = BITMAP_ALLOC (&predbitmap_obstack);
  bitmap_clear (graph->direct_nodes);

  for (j = 1; j < FIRST_REF_NODE; j++)
    {
      if (!get_varinfo (j)->is_special_var)
	bitmap_set_bit (graph->direct_nodes, j);
    }

  for (j = 0; j < graph->size; j++)
    graph->eq_rep[j] = -1;

  for (j = 0; j < varmap.length (); j++)
    graph->indirect_cycles[j] = -1;

  FOR_EACH_VEC_ELT (constraints, i, c)
    {
      struct constraint_expr lhs = c->lhs;
      struct constraint_expr rhs = c->rhs;
      unsigned int lhsvar = lhs.var;
      unsigned int rhsvar = rhs.var;

      if (lhs.type == DEREF)
	{
	  /* *x = y.  */
	  if (rhs.offset == 0 && lhs.offset == 0 && rhs.type == SCALAR)
	    {
	      if (lhs.var == anything_id)
		add_pred_graph_edge (graph, storedanything_id, rhsvar);
	      else
		add_pred_graph_edge (graph, FIRST_REF_NODE + lhsvar, rhsvar);
	    }
	}
      else if (rhs.type == DEREF)
	{
	  /* x = *y */
	  if (rhs.offset == 0 && lhs.offset == 0 && lhs.type == SCALAR)
	    add_pred_graph_edge (graph, lhsvar, FIRST_REF_NODE + rhsvar);
	  else
	    bitmap_clear_bit (graph->direct_nodes, lhsvar);
	}
      else if (rhs.type == ADDRESSOF)
	{
	  varinfo_t v;

	  /* x = &y */
	  if (graph->points_to[lhsvar] == NULL)
	    graph->points_to[lhsvar] = BITMAP_ALLOC (&predbitmap_obstack);
	  bitmap_set_bit (graph->points_to[lhsvar], rhsvar);

	  if (graph->pointed_by[rhsvar] == NULL)
	    graph->pointed_by[rhsvar] = BITMAP_ALLOC (&predbitmap_obstack);
	  bitmap_set_bit (graph->pointed_by[rhsvar], lhsvar);

	  /* Implicitly, *x = y */
	  add_implicit_graph_edge (graph, FIRST_REF_NODE + lhsvar, rhsvar);

	  /* All related variables are no longer direct nodes.  */
	  bitmap_clear_bit (graph->direct_nodes, rhsvar);
	  v = get_varinfo (rhsvar);
	  if (!v->is_full_var)
	    {
	      v = get_varinfo (v->head);
	      do
		{
		  bitmap_clear_bit (graph->direct_nodes, v->id);
		  v = vi_next (v);
		}
	      while (v != NULL);
	    }
	  bitmap_set_bit (graph->address_taken, rhsvar);
	}
      else if (lhsvar > anything_id
	       && lhsvar != rhsvar && lhs.offset == 0 && rhs.offset == 0)
	{
	  /* x = y */
	  add_pred_graph_edge (graph, lhsvar, rhsvar);
	  /* Implicitly, *x = *y */
	  add_implicit_graph_edge (graph, FIRST_REF_NODE + lhsvar,
				   FIRST_REF_NODE + rhsvar);
	}
      else if (lhs.offset != 0 || rhs.offset != 0)
	{
	  if (rhs.offset != 0)
	    bitmap_clear_bit (graph->direct_nodes, lhs.var);
	  else if (lhs.offset != 0)
	    bitmap_clear_bit (graph->direct_nodes, rhs.var);
	}
    }
}

/* Build the constraint graph, adding successor edges.  */

static void
build_succ_graph (void)
{
  unsigned i, t;
  constraint_t c;

  FOR_EACH_VEC_ELT (constraints, i, c)
    {
      struct constraint_expr lhs;
      struct constraint_expr rhs;
      unsigned int lhsvar;
      unsigned int rhsvar;

      if (!c)
	continue;

      lhs = c->lhs;
      rhs = c->rhs;
      lhsvar = find (lhs.var);
      rhsvar = find (rhs.var);

      if (lhs.type == DEREF)
	{
	  if (rhs.offset == 0 && lhs.offset == 0 && rhs.type == SCALAR)
	    {
	      if (lhs.var == anything_id)
		add_graph_edge (graph, storedanything_id, rhsvar);
	      else
		add_graph_edge (graph, FIRST_REF_NODE + lhsvar, rhsvar);
	    }
	}
      else if (rhs.type == DEREF)
	{
	  if (rhs.offset == 0 && lhs.offset == 0 && lhs.type == SCALAR)
	    add_graph_edge (graph, lhsvar, FIRST_REF_NODE + rhsvar);
	}
      else if (rhs.type == ADDRESSOF)
	{
	  /* x = &y */
	  gcc_checking_assert (find (rhs.var) == rhs.var);
	  bitmap_set_bit (get_varinfo (lhsvar)->solution, rhsvar);
	}
      else if (lhsvar > anything_id
	       && lhsvar != rhsvar && lhs.offset == 0 && rhs.offset == 0)
	{
	  add_graph_edge (graph, lhsvar, rhsvar);
	}
    }

  /* Add edges from STOREDANYTHING to all nodes that can receive pointers.  */
  t = find (storedanything_id);
  for (i = integer_id + 1; i < FIRST_REF_NODE; ++i)
    {
      if (get_varinfo (i)->may_have_pointers)
	add_graph_edge (graph, find (i), t);
    }

  /* Everything stored to ANYTHING also potentially escapes.  */
  add_graph_edge (graph, find (escaped_id), t);
}


/* Changed variables on the last iteration.  */
static bitmap changed;

/* Strongly Connected Component visitation info.  */

class scc_info
{
public:
  scc_info (size_t size);
  ~scc_info ();

  auto_sbitmap visited;
  auto_sbitmap deleted;
  unsigned int *dfs;
  unsigned int *node_mapping;
  int current_index;
  auto_vec<unsigned> scc_stack;
};


/* Recursive routine to find strongly connected components in GRAPH.
   SI is the SCC info to store the information in, and N is the id of current
   graph node we are processing.

   This is Tarjan's strongly connected component finding algorithm, as
   modified by Nuutila to keep only non-root nodes on the stack.
   The algorithm can be found in "On finding the strongly connected
   connected components in a directed graph" by Esko Nuutila and Eljas
   Soisalon-Soininen, in Information Processing Letters volume 49,
   number 1, pages 9-14.  */

static void
scc_visit (constraint_graph_t graph, class scc_info *si, unsigned int n)
{
  unsigned int i;
  bitmap_iterator bi;
  unsigned int my_dfs;

  bitmap_set_bit (si->visited, n);
  si->dfs[n] = si->current_index ++;
  my_dfs = si->dfs[n];

  /* Visit all the successors.  */
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->succs[n], 0, i, bi)
    {
      unsigned int w;

      if (i > LAST_REF_NODE)
	break;

      w = find (i);
      if (bitmap_bit_p (si->deleted, w))
	continue;

      if (!bitmap_bit_p (si->visited, w))
	scc_visit (graph, si, w);

      unsigned int t = find (w);
      gcc_checking_assert (find (n) == n);
      if (si->dfs[t] < si->dfs[n])
	si->dfs[n] = si->dfs[t];
    }

  /* See if any components have been identified.  */
  if (si->dfs[n] == my_dfs)
    {
      if (si->scc_stack.length () > 0
	  && si->dfs[si->scc_stack.last ()] >= my_dfs)
	{
	  bitmap scc = BITMAP_ALLOC (NULL);
	  unsigned int lowest_node;
	  bitmap_iterator bi;

	  bitmap_set_bit (scc, n);

	  while (si->scc_stack.length () != 0
		 && si->dfs[si->scc_stack.last ()] >= my_dfs)
	    {
	      unsigned int w = si->scc_stack.pop ();

	      bitmap_set_bit (scc, w);
	    }

	  lowest_node = bitmap_first_set_bit (scc);
	  gcc_assert (lowest_node < FIRST_REF_NODE);

	  /* Collapse the SCC nodes into a single node, and mark the
	     indirect cycles.  */
	  EXECUTE_IF_SET_IN_BITMAP (scc, 0, i, bi)
	    {
	      if (i < FIRST_REF_NODE)
		{
		  if (unite (lowest_node, i))
		    unify_nodes (graph, lowest_node, i, false);
		}
	      else
		{
		  unite (lowest_node, i);
		  graph->indirect_cycles[i - FIRST_REF_NODE] = lowest_node;
		}
	    }
	  bitmap_set_bit (si->deleted, lowest_node);
	}
      else
	bitmap_set_bit (si->deleted, n);
    }
  else
    si->scc_stack.safe_push (n);
}

/* Unify node FROM into node TO, updating the changed count if
   necessary when UPDATE_CHANGED is true.  */

static void
unify_nodes (constraint_graph_t graph, unsigned int to, unsigned int from,
	     bool update_changed)
{
  gcc_checking_assert (to != from && find (to) == to);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Unifying %s to %s\n",
	     get_varinfo (from)->name,
	     get_varinfo (to)->name);

  if (update_changed)
    stats.unified_vars_dynamic++;
  else
    stats.unified_vars_static++;

  merge_graph_nodes (graph, to, from);
  if (merge_node_constraints (graph, to, from))
    {
      if (update_changed)
	bitmap_set_bit (changed, to);
    }

  /* Mark TO as changed if FROM was changed.  If TO was already marked
     as changed, decrease the changed count.  */

  if (update_changed
      && bitmap_clear_bit (changed, from))
    bitmap_set_bit (changed, to);
  varinfo_t fromvi = get_varinfo (from);
  if (fromvi->solution)
    {
      /* If the solution changes because of the merging, we need to mark
	 the variable as changed.  */
      varinfo_t tovi = get_varinfo (to);
      if (bitmap_ior_into (tovi->solution, fromvi->solution))
	{
	  if (update_changed)
	    bitmap_set_bit (changed, to);
	}

      BITMAP_FREE (fromvi->solution);
      if (fromvi->oldsolution)
	BITMAP_FREE (fromvi->oldsolution);

      if (stats.iterations > 0
	  && tovi->oldsolution)
	BITMAP_FREE (tovi->oldsolution);
    }
  if (graph->succs[to])
    bitmap_clear_bit (graph->succs[to], to);
}

/* Add a copy edge FROM -> TO, optimizing special cases.  Returns TRUE
   if the solution of TO changed.  */

static bool
solve_add_graph_edge (constraint_graph_t graph, unsigned int to,
		      unsigned int from)
{
  /* Adding edges from the special vars is pointless.
     They don't have sets that can change.  */
  if (get_varinfo (from)->is_special_var)
    return bitmap_ior_into (get_varinfo (to)->solution,
			    get_varinfo (from)->solution);
  /* Merging the solution from ESCAPED needlessly increases
     the set.  Use ESCAPED as representative instead.  */
  else if (from == find (escaped_id))
    return bitmap_set_bit (get_varinfo (to)->solution, escaped_id);
  else if (get_varinfo (from)->may_have_pointers
	   && add_graph_edge (graph, to, from))
    return bitmap_ior_into (get_varinfo (to)->solution,
			    get_varinfo (from)->solution);
  return false;
}

/* Process a constraint C that represents x = *(y + off), using DELTA as the
   starting solution for y.  */

static void
do_sd_constraint (constraint_graph_t graph, constraint_t c,
		  bitmap delta, bitmap *expanded_delta)
{
  unsigned int lhs = c->lhs.var;
  bool flag = false;
  bitmap sol = get_varinfo (lhs)->solution;
  unsigned int j;
  bitmap_iterator bi;
  HOST_WIDE_INT roffset = c->rhs.offset;

  /* Our IL does not allow this.  */
  gcc_checking_assert (c->lhs.offset == 0);

  /* If the solution of Y contains anything it is good enough to transfer
     this to the LHS.  */
  if (bitmap_bit_p (delta, anything_id))
    {
      flag |= bitmap_set_bit (sol, anything_id);
      goto done;
    }

  /* If we do not know at with offset the rhs is dereferenced compute
     the reachability set of DELTA, conservatively assuming it is
     dereferenced at all valid offsets.  */
  if (roffset == UNKNOWN_OFFSET)
    {
      delta = solution_set_expand (delta, expanded_delta);
      /* No further offset processing is necessary.  */
      roffset = 0;
    }

  /* For each variable j in delta (Sol(y)), add
     an edge in the graph from j to x, and union Sol(j) into Sol(x).  */
  EXECUTE_IF_SET_IN_BITMAP (delta, 0, j, bi)
    {
      varinfo_t v = get_varinfo (j);
      HOST_WIDE_INT fieldoffset = v->offset + roffset;
      unsigned HOST_WIDE_INT size = v->size;
      unsigned int t;

      if (v->is_full_var)
	;
      else if (roffset != 0)
	{
	  if (fieldoffset < 0)
	    v = get_varinfo (v->head);
	  else
	    v = first_or_preceding_vi_for_offset (v, fieldoffset);
	}

      /* We have to include all fields that overlap the current field
	 shifted by roffset.  */
      do
	{
	  t = find (v->id);

	  flag |= solve_add_graph_edge (graph, lhs, t);

	  if (v->is_full_var
	      || v->next == 0)
	    break;

	  v = vi_next (v);
	}
      while (v->offset < fieldoffset + size);
    }

done:
  /* If the LHS solution changed, mark the var as changed.  */
  if (flag)
    bitmap_set_bit (changed, lhs);
}

/* Process a constraint C that represents *(x + off) = y using DELTA
   as the starting solution for x.  */

static void
do_ds_constraint (constraint_t c, bitmap delta, bitmap *expanded_delta)
{
  unsigned int rhs = c->rhs.var;
  bitmap sol = get_varinfo (rhs)->solution;
  unsigned int j;
  bitmap_iterator bi;
  HOST_WIDE_INT loff = c->lhs.offset;
  bool escaped_p = false;

  /* Our IL does not allow this.  */
  gcc_checking_assert (c->rhs.offset == 0);

  /* If the solution of y contains ANYTHING simply use the ANYTHING
     solution.  This avoids needlessly increasing the points-to sets.  */
  if (bitmap_bit_p (sol, anything_id))
    sol = get_varinfo (find (anything_id))->solution;

  /* If the solution for x contains ANYTHING we have to merge the
     solution of y into all pointer variables which we do via
     STOREDANYTHING.  */
  if (bitmap_bit_p (delta, anything_id))
    {
      unsigned t = find (storedanything_id);
      if (solve_add_graph_edge (graph, t, rhs))
	bitmap_set_bit (changed, t);
      return;
    }

  /* If we do not know at with offset the rhs is dereferenced compute
     the reachability set of DELTA, conservatively assuming it is
     dereferenced at all valid offsets.  */
  if (loff == UNKNOWN_OFFSET)
    {
      delta = solution_set_expand (delta, expanded_delta);
      loff = 0;
    }

  /* For each member j of delta (Sol(x)), add an edge from y to j and
     union Sol(y) into Sol(j) */
  EXECUTE_IF_SET_IN_BITMAP (delta, 0, j, bi)
    {
      varinfo_t v = get_varinfo (j);
      unsigned int t;
      HOST_WIDE_INT fieldoffset = v->offset + loff;
      unsigned HOST_WIDE_INT size = v->size;

      if (v->is_full_var)
	;
      else if (loff != 0)
	{
	  if (fieldoffset < 0)
	    v = get_varinfo (v->head);
	  else
	    v = first_or_preceding_vi_for_offset (v, fieldoffset);
	}

      /* We have to include all fields that overlap the current field
	 shifted by loff.  */
      do
	{
	  if (v->may_have_pointers)
	    {
	      /* If v is a global variable then this is an escape point.  */
	      if (v->is_global_var
		  && !escaped_p)
		{
		  t = find (escaped_id);
		  if (add_graph_edge (graph, t, rhs)
		      && bitmap_ior_into (get_varinfo (t)->solution, sol))
		    bitmap_set_bit (changed, t);
		  /* Enough to let rhs escape once.  */
		  escaped_p = true;
		}

	      if (v->is_special_var)
		break;

	      t = find (v->id);

	      if (solve_add_graph_edge (graph, t, rhs))
		bitmap_set_bit (changed, t);
	    }

	  if (v->is_full_var
	      || v->next == 0)
	    break;

	  v = vi_next (v);
	}
      while (v->offset < fieldoffset + size);
    }
}

/* Handle a non-simple (simple meaning requires no iteration),
   constraint (IE *x = &y, x = *y, *x = y, and x = y with offsets involved).  */

static void
do_complex_constraint (constraint_graph_t graph, constraint_t c, bitmap delta,
		       bitmap *expanded_delta)
{
  if (c->lhs.type == DEREF)
    {
      if (c->rhs.type == ADDRESSOF)
	{
	  gcc_unreachable ();
	}
      else
	{
	  /* *x = y */
	  do_ds_constraint (c, delta, expanded_delta);
	}
    }
  else if (c->rhs.type == DEREF)
    {
      /* x = *y */
      if (!(get_varinfo (c->lhs.var)->is_special_var))
	do_sd_constraint (graph, c, delta, expanded_delta);
    }
  else
    {
      bitmap tmp;
      bool flag = false;

      gcc_checking_assert (c->rhs.type == SCALAR && c->lhs.type == SCALAR
			   && c->rhs.offset != 0 && c->lhs.offset == 0);
      tmp = get_varinfo (c->lhs.var)->solution;

      flag = set_union_with_increment (tmp, delta, c->rhs.offset,
				       expanded_delta);

      if (flag)
	bitmap_set_bit (changed, c->lhs.var);
    }
}

/* Initialize and return a new SCC info structure.  */

scc_info::scc_info (size_t size) :
  visited (size), deleted (size), current_index (0), scc_stack (1)
{
  bitmap_clear (visited);
  bitmap_clear (deleted);
  node_mapping = XNEWVEC (unsigned int, size);
  dfs = XCNEWVEC (unsigned int, size);

  for (size_t i = 0; i < size; i++)
    node_mapping[i] = i;
}

/* Free an SCC info structure pointed to by SI.  */

scc_info::~scc_info ()
{
  free (node_mapping);
  free (dfs);
}


/* Find indirect cycles in GRAPH that occur, using strongly connected
   components, and note them in the indirect cycles map.

   This technique comes from Ben Hardekopf and Calvin Lin,
   "It Pays to be Lazy: Fast and Accurate Pointer Analysis for Millions of
   Lines of Code", submitted to PLDI 2007.  */

static void
find_indirect_cycles (constraint_graph_t graph)
{
  unsigned int i;
  unsigned int size = graph->size;
  scc_info si (size);

  for (i = 0; i < MIN (LAST_REF_NODE, size); i++)
    if (!bitmap_bit_p (si.visited, i) && find (i) == i)
      scc_visit (graph, &si, i);
}

/* Visit the graph in topological order starting at node N, and store the
   order in TOPO_ORDER using VISITED to indicate visited nodes.  */

static void
topo_visit (constraint_graph_t graph, vec<unsigned> &topo_order,
	    sbitmap visited, unsigned int n)
{
  bitmap_iterator bi;
  unsigned int j;

  bitmap_set_bit (visited, n);

  if (graph->succs[n])
    EXECUTE_IF_SET_IN_BITMAP (graph->succs[n], 0, j, bi)
      {
	unsigned k = find (j);
	if (!bitmap_bit_p (visited, k))
	  topo_visit (graph, topo_order, visited, k);
      }

  /* Also consider copy with offset complex constraints as implicit edges.  */
  for (auto c : graph->complex[n])
    {
      /* Constraints are ordered so that SCALAR = SCALAR appear first.  */
      if (c->lhs.type != SCALAR || c->rhs.type != SCALAR)
	break;
      gcc_checking_assert (c->rhs.var == n);
      unsigned k = find (c->lhs.var);
      if (!bitmap_bit_p (visited, k))
	topo_visit (graph, topo_order, visited, k);
    }

  topo_order.quick_push (n);
}

/* Compute a topological ordering for GRAPH, and return the result.  */

static auto_vec<unsigned>
compute_topo_order (constraint_graph_t graph)
{
  unsigned int i;
  unsigned int size = graph->size;

  auto_sbitmap visited (size);
  bitmap_clear (visited);

  /* For the heuristic in add_graph_edge to work optimally make sure to
     first visit the connected component of the graph containing
     ESCAPED.  Do this by extracting the connected component
     with ESCAPED and append that to all other components as solve_graph
     pops from the order.  */
  auto_vec<unsigned> tail (size);
  topo_visit (graph, tail, visited, find (escaped_id));

  auto_vec<unsigned> topo_order (size);

  for (i = 0; i != size; ++i)
    if (!bitmap_bit_p (visited, i) && find (i) == i)
      topo_visit (graph, topo_order, visited, i);

  topo_order.splice (tail);
  return topo_order;
}

/* Structure used to for hash value numbering of pointer equivalence
   classes.  */

typedef struct equiv_class_label
{
  hashval_t hashcode;
  unsigned int equivalence_class;
  bitmap labels;
} *equiv_class_label_t;
typedef const struct equiv_class_label *const_equiv_class_label_t;

/* Equiv_class_label hashtable helpers.  */

struct equiv_class_hasher : nofree_ptr_hash <equiv_class_label>
{
  static inline hashval_t hash (const equiv_class_label *);
  static inline bool equal (const equiv_class_label *,
			    const equiv_class_label *);
};

/* A hashtable for mapping a bitmap of labels->pointer equivalence
   classes.  */
static hash_table<equiv_class_hasher> *pointer_equiv_class_table;

/* A hashtable for mapping a bitmap of labels->location equivalence
   classes.  */
static hash_table<equiv_class_hasher> *location_equiv_class_table;

/* Hash function for a equiv_class_label_t.  */

inline hashval_t
equiv_class_hasher::hash (const equiv_class_label *ecl)
{
  return ecl->hashcode;
}

/* Equality function for two equiv_class_label_t's.  */

inline bool
equiv_class_hasher::equal (const equiv_class_label *eql1,
			   const equiv_class_label *eql2)
{
  return (eql1->hashcode == eql2->hashcode
	  && bitmap_equal_p (eql1->labels, eql2->labels));
}

struct obstack equiv_class_obstack;

/* Lookup a equivalence class in TABLE by the bitmap of LABELS with
   hash HAS it contains.  Sets *REF_LABELS to the bitmap LABELS
   is equivalent to.  */

static equiv_class_label *
equiv_class_lookup_or_add (hash_table<equiv_class_hasher> *table,
			   bitmap labels)
{
  equiv_class_label **slot;
  equiv_class_label ecl;

  ecl.labels = labels;
  ecl.hashcode = bitmap_hash (labels);
  slot = table->find_slot (&ecl, INSERT);
  if (!*slot)
    {
      *slot = XOBNEW (&equiv_class_obstack, struct equiv_class_label);
      (*slot)->labels = labels;
      (*slot)->hashcode = ecl.hashcode;
      (*slot)->equivalence_class = 0;
    }

  return *slot;
}


/* Perform offline variable substitution.

   This is a worst case quadratic time way of identifying variables
   that must have equivalent points-to sets, including those caused by
   static cycles, and single entry subgraphs, in the constraint graph.

   The technique is described in "Exploiting Pointer and Location
   Equivalence to Optimize Pointer Analysis.  In the 14th International
   Static Analysis Symposium (SAS), August 2007."  It is known as the
   "HU" algorithm, and is equivalent to value numbering the collapsed
   constraint graph including evaluating unions.

   The general method of finding equivalence classes is as follows:
   Add fake nodes (REF nodes) and edges for *a = b and a = *b constraints.
   Initialize all non-REF nodes to be direct nodes.
   For each constraint a = a U {b}, we set pts(a) = pts(a) u {fresh
   variable}
   For each constraint containing the dereference, we also do the same
   thing.

   We then compute SCC's in the graph and unify nodes in the same SCC,
   including pts sets.

   For each non-collapsed node x:
    Visit all unvisited explicit incoming edges.
    Ignoring all non-pointers, set pts(x) = Union of pts(a) for y
    where y->x.
    Lookup the equivalence class for pts(x).
     If we found one, equivalence_class(x) = found class.
     Otherwise, equivalence_class(x) = new class, and new_class is
    added to the lookup table.

   All direct nodes with the same equivalence class can be replaced
   with a single representative node.
   All unlabeled nodes (label == 0) are not pointers and all edges
   involving them can be eliminated.
   We perform these optimizations during rewrite_constraints

   In addition to pointer equivalence class finding, we also perform
   location equivalence class finding.  This is the set of variables
   that always appear together in points-to sets.  We use this to
   compress the size of the points-to sets.  */

/* Current maximum pointer equivalence class id.  */
static int pointer_equiv_class;

/* Current maximum location equivalence class id.  */
static int location_equiv_class;

/* Recursive routine to find strongly connected components in GRAPH,
   and label it's nodes with DFS numbers.  */

static void
condense_visit (constraint_graph_t graph, class scc_info *si, unsigned int n)
{
  unsigned int i;
  bitmap_iterator bi;
  unsigned int my_dfs;

  gcc_checking_assert (si->node_mapping[n] == n);
  bitmap_set_bit (si->visited, n);
  si->dfs[n] = si->current_index ++;
  my_dfs = si->dfs[n];

  /* Visit all the successors.  */
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->preds[n], 0, i, bi)
    {
      unsigned int w = si->node_mapping[i];

      if (bitmap_bit_p (si->deleted, w))
	continue;

      if (!bitmap_bit_p (si->visited, w))
	condense_visit (graph, si, w);

      unsigned int t = si->node_mapping[w];
      gcc_checking_assert (si->node_mapping[n] == n);
      if (si->dfs[t] < si->dfs[n])
	si->dfs[n] = si->dfs[t];
    }

  /* Visit all the implicit predecessors.  */
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->implicit_preds[n], 0, i, bi)
    {
      unsigned int w = si->node_mapping[i];

      if (bitmap_bit_p (si->deleted, w))
	continue;

      if (!bitmap_bit_p (si->visited, w))
	condense_visit (graph, si, w);

      unsigned int t = si->node_mapping[w];
      gcc_assert (si->node_mapping[n] == n);
      if (si->dfs[t] < si->dfs[n])
	si->dfs[n] = si->dfs[t];
    }

  /* See if any components have been identified.  */
  if (si->dfs[n] == my_dfs)
    {
      if (si->scc_stack.length () != 0
	  && si->dfs[si->scc_stack.last ()] >= my_dfs)
	{
	  /* Find the first node of the SCC and do non-bitmap work.  */
	  bool direct_p = true;
	  unsigned first = si->scc_stack.length ();
	  do
	    {
	      --first;
	      unsigned int w = si->scc_stack[first];
	      si->node_mapping[w] = n;
	      if (!bitmap_bit_p (graph->direct_nodes, w))
		direct_p = false;
	    }
	  while (first > 0
		 && si->dfs[si->scc_stack[first - 1]] >= my_dfs);
	  if (!direct_p)
	    bitmap_clear_bit (graph->direct_nodes, n);

	  /* Want to reduce to node n, push that first.  */
	  si->scc_stack.reserve (1);
	  si->scc_stack.quick_push (si->scc_stack[first]);
	  si->scc_stack[first] = n;

	  unsigned scc_size = si->scc_stack.length () - first;
	  unsigned split = scc_size / 2;
	  unsigned carry = scc_size - split * 2;
	  while (split > 0)
	    {
	      for (unsigned i = 0; i < split; ++i)
		{
		  unsigned a = si->scc_stack[first + i];
		  unsigned b = si->scc_stack[first + split + carry + i];

		  /* Unify our nodes.  */
		  if (graph->preds[b])
		    {
		      if (!graph->preds[a])
			std::swap (graph->preds[a], graph->preds[b]);
		      else
			bitmap_ior_into_and_free (graph->preds[a],
						  &graph->preds[b]);
		    }
		  if (graph->implicit_preds[b])
		    {
		      if (!graph->implicit_preds[a])
			std::swap (graph->implicit_preds[a],
				   graph->implicit_preds[b]);
		      else
			bitmap_ior_into_and_free (graph->implicit_preds[a],
						  &graph->implicit_preds[b]);
		    }
		  if (graph->points_to[b])
		    {
		      if (!graph->points_to[a])
			std::swap (graph->points_to[a], graph->points_to[b]);
		      else
			bitmap_ior_into_and_free (graph->points_to[a],
						  &graph->points_to[b]);
		    }
		}
	      unsigned remain = split + carry;
	      split = remain / 2;
	      carry = remain - split * 2;
	    }
	  /* Actually pop the SCC.  */
	  si->scc_stack.truncate (first);
	}
      bitmap_set_bit (si->deleted, n);
    }
  else
    si->scc_stack.safe_push (n);
}

/* Label pointer equivalences.

   This performs a value numbering of the constraint graph to
   discover which variables will always have the same points-to sets
   under the current set of constraints.

   The way it value numbers is to store the set of points-to bits
   generated by the constraints and graph edges.  This is just used as a
   hash and equality comparison.  The *actual set of points-to bits* is
   completely irrelevant, in that we don't care about being able to
   extract them later.

   The equality values (currently bitmaps) just have to satisfy a few
   constraints, the main ones being:
   1. The combining operation must be order independent.
   2. The end result of a given set of operations must be unique iff the
      combination of input values is unique
   3. Hashable.  */

static void
label_visit (constraint_graph_t graph, class scc_info *si, unsigned int n)
{
  unsigned int i, first_pred;
  bitmap_iterator bi;

  bitmap_set_bit (si->visited, n);

  /* Label and union our incoming edges's points to sets.  */
  first_pred = -1U;
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->preds[n], 0, i, bi)
    {
      unsigned int w = si->node_mapping[i];
      if (!bitmap_bit_p (si->visited, w))
	label_visit (graph, si, w);

      /* Skip unused edges.  */
      if (w == n || graph->pointer_label[w] == 0)
	continue;

      if (graph->points_to[w])
	{
	  if (!graph->points_to[n])
	    {
	      if (first_pred == -1U)
		first_pred = w;
	      else
		{
		  graph->points_to[n] = BITMAP_ALLOC (&predbitmap_obstack);
		  bitmap_ior (graph->points_to[n],
			      graph->points_to[first_pred],
			      graph->points_to[w]);
		}
	    }
	  else
	    bitmap_ior_into (graph->points_to[n], graph->points_to[w]);
	}
    }

  /* Indirect nodes get fresh variables and a new pointer equiv class.  */
  if (!bitmap_bit_p (graph->direct_nodes, n))
    {
      if (!graph->points_to[n])
	{
	  graph->points_to[n] = BITMAP_ALLOC (&predbitmap_obstack);
	  if (first_pred != -1U)
	    bitmap_copy (graph->points_to[n], graph->points_to[first_pred]);
	}
      bitmap_set_bit (graph->points_to[n], FIRST_REF_NODE + n);
      graph->pointer_label[n] = pointer_equiv_class++;
      equiv_class_label_t ecl;
      ecl = equiv_class_lookup_or_add (pointer_equiv_class_table,
				       graph->points_to[n]);
      ecl->equivalence_class = graph->pointer_label[n];
      return;
    }

  /* If there was only a single non-empty predecessor the pointer equiv
     class is the same.  */
  if (!graph->points_to[n])
    {
      if (first_pred != -1U)
	{
	  graph->pointer_label[n] = graph->pointer_label[first_pred];
	  graph->points_to[n] = graph->points_to[first_pred];
	}
      return;
    }

  if (!bitmap_empty_p (graph->points_to[n]))
    {
      equiv_class_label_t ecl;
      ecl = equiv_class_lookup_or_add (pointer_equiv_class_table,
				       graph->points_to[n]);
      if (ecl->equivalence_class == 0)
	ecl->equivalence_class = pointer_equiv_class++;
      else
	{
	  BITMAP_FREE (graph->points_to[n]);
	  graph->points_to[n] = ecl->labels;
	}
      graph->pointer_label[n] = ecl->equivalence_class;
    }
}

/* Print the pred graph in dot format.  */

static void
dump_pred_graph (class scc_info *si, FILE *file)
{
  unsigned int i;

  /* Only print the graph if it has already been initialized:  */
  if (!graph)
    return;

  /* Prints the header of the dot file:  */
  fprintf (file, "strict digraph {\n");
  fprintf (file, "  node [\n    shape = box\n  ]\n");
  fprintf (file, "  edge [\n    fontsize = \"12\"\n  ]\n");
  fprintf (file, "\n  // List of nodes and complex constraints in "
	   "the constraint graph:\n");

  /* The next lines print the nodes in the graph together with the
     complex constraints attached to them.  */
  for (i = 1; i < graph->size; i++)
    {
      if (i == FIRST_REF_NODE)
	continue;
      if (si->node_mapping[i] != i)
	continue;
      if (i < FIRST_REF_NODE)
	fprintf (file, "\"%s\"", get_varinfo (i)->name);
      else
	fprintf (file, "\"*%s\"", get_varinfo (i - FIRST_REF_NODE)->name);
      if (graph->points_to[i]
	  && !bitmap_empty_p (graph->points_to[i]))
	{
	  if (i < FIRST_REF_NODE)
	    fprintf (file, "[label=\"%s = {", get_varinfo (i)->name);
	  else
	    fprintf (file, "[label=\"*%s = {",
		     get_varinfo (i - FIRST_REF_NODE)->name);
	  unsigned j;
	  bitmap_iterator bi;
	  EXECUTE_IF_SET_IN_BITMAP (graph->points_to[i], 0, j, bi)
	    fprintf (file, " %d", j);
	  fprintf (file, " }\"]");
	}
      fprintf (file, ";\n");
    }

  /* Go over the edges.  */
  fprintf (file, "\n  // Edges in the constraint graph:\n");
  for (i = 1; i < graph->size; i++)
    {
      unsigned j;
      bitmap_iterator bi;
      if (si->node_mapping[i] != i)
	continue;
      EXECUTE_IF_IN_NONNULL_BITMAP (graph->preds[i], 0, j, bi)
	{
	  unsigned from = si->node_mapping[j];
	  if (from < FIRST_REF_NODE)
	    fprintf (file, "\"%s\"", get_varinfo (from)->name);
	  else
	    fprintf (file, "\"*%s\"",
		     get_varinfo (from - FIRST_REF_NODE)->name);
	  fprintf (file, " -> ");
	  if (i < FIRST_REF_NODE)
	    fprintf (file, "\"%s\"", get_varinfo (i)->name);
	  else
	    fprintf (file, "\"*%s\"", get_varinfo (i - FIRST_REF_NODE)->name);
	  fprintf (file, ";\n");
	}
    }

  /* Prints the tail of the dot file.  */
  fprintf (file, "}\n");
}

/* Perform offline variable substitution, discovering equivalence
   classes, and eliminating non-pointer variables.  */

static class scc_info *
perform_var_substitution (constraint_graph_t graph)
{
  unsigned int i;
  unsigned int size = graph->size;
  scc_info *si = new scc_info (size);

  bitmap_obstack_initialize (&iteration_obstack);
  gcc_obstack_init (&equiv_class_obstack);
  pointer_equiv_class_table = new hash_table<equiv_class_hasher> (511);
  location_equiv_class_table
    = new hash_table<equiv_class_hasher> (511);
  pointer_equiv_class = 1;
  location_equiv_class = 1;

  /* Condense the nodes, which means to find SCC's, count incoming
     predecessors, and unite nodes in SCC's.  */
  for (i = 1; i < FIRST_REF_NODE; i++)
    if (!bitmap_bit_p (si->visited, si->node_mapping[i]))
      condense_visit (graph, si, si->node_mapping[i]);

  if (dump_file && (dump_flags & TDF_GRAPH))
    {
      fprintf (dump_file, "\n\n// The constraint graph before var-substitution "
	       "in dot format:\n");
      dump_pred_graph (si, dump_file);
      fprintf (dump_file, "\n\n");
    }

  bitmap_clear (si->visited);
  /* Actually the label the nodes for pointer equivalences.  */
  for (i = 1; i < FIRST_REF_NODE; i++)
    if (!bitmap_bit_p (si->visited, si->node_mapping[i]))
      label_visit (graph, si, si->node_mapping[i]);

  /* Calculate location equivalence labels.  */
  for (i = 1; i < FIRST_REF_NODE; i++)
    {
      bitmap pointed_by;
      bitmap_iterator bi;
      unsigned int j;

      if (!graph->pointed_by[i])
	continue;
      pointed_by = BITMAP_ALLOC (&iteration_obstack);

      /* Translate the pointed-by mapping for pointer equivalence
	 labels.  */
      EXECUTE_IF_SET_IN_BITMAP (graph->pointed_by[i], 0, j, bi)
	{
	  bitmap_set_bit (pointed_by,
			  graph->pointer_label[si->node_mapping[j]]);
	}
      /* The original pointed_by is now dead.  */
      BITMAP_FREE (graph->pointed_by[i]);

      /* Look up the location equivalence label if one exists, or make
	 one otherwise.  */
      equiv_class_label_t ecl;
      ecl = equiv_class_lookup_or_add (location_equiv_class_table, pointed_by);
      if (ecl->equivalence_class == 0)
	ecl->equivalence_class = location_equiv_class++;
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Found location equivalence for node %s\n",
		     get_varinfo (i)->name);
	  BITMAP_FREE (pointed_by);
	}
      graph->loc_label[i] = ecl->equivalence_class;

    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    for (i = 1; i < FIRST_REF_NODE; i++)
      {
	unsigned j = si->node_mapping[i];
	if (j != i)
	  {
	    fprintf (dump_file, "%s node id %d ",
		     bitmap_bit_p (graph->direct_nodes, i)
		     ? "Direct" : "Indirect", i);
	    if (i < FIRST_REF_NODE)
	      fprintf (dump_file, "\"%s\"", get_varinfo (i)->name);
	    else
	      fprintf (dump_file, "\"*%s\"",
		       get_varinfo (i - FIRST_REF_NODE)->name);
	    fprintf (dump_file, " mapped to SCC leader node id %d ", j);
	    if (j < FIRST_REF_NODE)
	      fprintf (dump_file, "\"%s\"\n", get_varinfo (j)->name);
	    else
	      fprintf (dump_file, "\"*%s\"\n",
		       get_varinfo (j - FIRST_REF_NODE)->name);
	  }
	else
	  {
	    fprintf (dump_file,
		     "Equivalence classes for %s node id %d ",
		     bitmap_bit_p (graph->direct_nodes, i)
		     ? "direct" : "indirect", i);
	    if (i < FIRST_REF_NODE)
	      fprintf (dump_file, "\"%s\"", get_varinfo (i)->name);
	    else
	      fprintf (dump_file, "\"*%s\"",
		       get_varinfo (i - FIRST_REF_NODE)->name);
	    fprintf (dump_file,
		     ": pointer %d, location %d\n",
		     graph->pointer_label[i], graph->loc_label[i]);
	  }
      }

  /* Quickly eliminate our non-pointer variables.  */

  for (i = 1; i < FIRST_REF_NODE; i++)
    {
      unsigned int node = si->node_mapping[i];

      if (graph->pointer_label[node] == 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "%s is a non-pointer variable, eliminating edges.\n",
		     get_varinfo (node)->name);
	  stats.nonpointer_vars++;
	  clear_edges_for_node (graph, node);
	}
    }

  return si;
}

/* Free information that was only necessary for variable
   substitution.  */

static void
free_var_substitution_info (class scc_info *si)
{
  delete si;
  free (graph->pointer_label);
  free (graph->loc_label);
  free (graph->pointed_by);
  free (graph->points_to);
  free (graph->eq_rep);
  sbitmap_free (graph->direct_nodes);
  delete pointer_equiv_class_table;
  pointer_equiv_class_table = NULL;
  delete location_equiv_class_table;
  location_equiv_class_table = NULL;
  obstack_free (&equiv_class_obstack, NULL);
  bitmap_obstack_release (&iteration_obstack);
}

/* Return an existing node that is equivalent to NODE, which has
   equivalence class LABEL, if one exists.  Return NODE otherwise.  */

static unsigned int
find_equivalent_node (constraint_graph_t graph,
		      unsigned int node, unsigned int label)
{
  /* If the address version of this variable is unused, we can
     substitute it for anything else with the same label.
     Otherwise, we know the pointers are equivalent, but not the
     locations, and we can unite them later.  */

  if (!bitmap_bit_p (graph->address_taken, node))
    {
      gcc_checking_assert (label < graph->size);

      if (graph->eq_rep[label] != -1)
	{
	  /* Unify the two variables since we know they are equivalent.  */
	  if (unite (graph->eq_rep[label], node))
	    unify_nodes (graph, graph->eq_rep[label], node, false);
	  return graph->eq_rep[label];
	}
      else
	{
	  graph->eq_rep[label] = node;
	  graph->pe_rep[label] = node;
	}
    }
  else
    {
      gcc_checking_assert (label < graph->size);
      graph->pe[node] = label;
      if (graph->pe_rep[label] == -1)
	graph->pe_rep[label] = node;
    }

  return node;
}

/* Unite pointer equivalent but not location equivalent nodes in
   GRAPH.  This may only be performed once variable substitution is
   finished.  */

static void
unite_pointer_equivalences (constraint_graph_t graph)
{
  unsigned int i;

  /* Go through the pointer equivalences and unite them to their
     representative, if they aren't already.  */
  for (i = 1; i < FIRST_REF_NODE; i++)
    {
      unsigned int label = graph->pe[i];
      if (label)
	{
	  int label_rep = graph->pe_rep[label];

	  if (label_rep == -1)
	    continue;

	  label_rep = find (label_rep);
	  if (label_rep >= 0 && unite (label_rep, find (i)))
	    unify_nodes (graph, label_rep, i, false);
	}
    }
}

/* Move complex constraints to the GRAPH nodes they belong to.  */

static void
move_complex_constraints (constraint_graph_t graph)
{
  int i;
  constraint_t c;

  FOR_EACH_VEC_ELT (constraints, i, c)
    {
      if (c)
	{
	  struct constraint_expr lhs = c->lhs;
	  struct constraint_expr rhs = c->rhs;

	  if (lhs.type == DEREF)
	    {
	      insert_into_complex (graph, lhs.var, c);
	    }
	  else if (rhs.type == DEREF)
	    {
	      if (!(get_varinfo (lhs.var)->is_special_var))
		insert_into_complex (graph, rhs.var, c);
	    }
	  else if (rhs.type != ADDRESSOF && lhs.var > anything_id
		   && (lhs.offset != 0 || rhs.offset != 0))
	    {
	      insert_into_complex (graph, rhs.var, c);
	    }
	}
    }
}

/* Optimize and rewrite complex constraints while performing
   collapsing of equivalent nodes.  SI is the SCC_INFO that is the
   result of perform_variable_substitution.  */

static void
rewrite_constraints (constraint_graph_t graph,
		     class scc_info *si)
{
  int i;
  constraint_t c;

  if (flag_checking)
    {
      for (unsigned int j = 0; j < graph->size; j++)
	gcc_assert (find (j) == j);
    }

  FOR_EACH_VEC_ELT (constraints, i, c)
    {
      struct constraint_expr lhs = c->lhs;
      struct constraint_expr rhs = c->rhs;
      unsigned int lhsvar = find (lhs.var);
      unsigned int rhsvar = find (rhs.var);
      unsigned int lhsnode, rhsnode;
      unsigned int lhslabel, rhslabel;

      lhsnode = si->node_mapping[lhsvar];
      rhsnode = si->node_mapping[rhsvar];
      lhslabel = graph->pointer_label[lhsnode];
      rhslabel = graph->pointer_label[rhsnode];

      /* See if it is really a non-pointer variable, and if so, ignore
	 the constraint.  */
      if (lhslabel == 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {

	      fprintf (dump_file, "%s is a non-pointer variable, "
		       "ignoring constraint:",
		       get_varinfo (lhs.var)->name);
	      dump_constraint (dump_file, c);
	      fprintf (dump_file, "\n");
	    }
	  constraints[i] = NULL;
	  continue;
	}

      if (rhslabel == 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {

	      fprintf (dump_file, "%s is a non-pointer variable, "
		       "ignoring constraint:",
		       get_varinfo (rhs.var)->name);
	      dump_constraint (dump_file, c);
	      fprintf (dump_file, "\n");
	    }
	  constraints[i] = NULL;
	  continue;
	}

      lhsvar = find_equivalent_node (graph, lhsvar, lhslabel);
      rhsvar = find_equivalent_node (graph, rhsvar, rhslabel);
      c->lhs.var = lhsvar;
      c->rhs.var = rhsvar;
    }
}

/* Eliminate indirect cycles involving NODE.  Return true if NODE was
   part of an SCC, false otherwise.  */

static bool
eliminate_indirect_cycles (unsigned int node)
{
  if (graph->indirect_cycles[node] != -1
      && !bitmap_empty_p (get_varinfo (node)->solution))
    {
      unsigned int i;
      auto_vec<unsigned> queue;
      int queuepos;
      unsigned int to = find (graph->indirect_cycles[node]);
      bitmap_iterator bi;

      /* We can't touch the solution set and call unify_nodes
	 at the same time, because unify_nodes is going to do
	 bitmap unions into it.  */

      EXECUTE_IF_SET_IN_BITMAP (get_varinfo (node)->solution, 0, i, bi)
	{
	  if (find (i) == i && i != to)
	    {
	      if (unite (to, i))
		queue.safe_push (i);
	    }
	}

      for (queuepos = 0;
	   queue.iterate (queuepos, &i);
	   queuepos++)
	{
	  unify_nodes (graph, to, i, true);
	}
      return true;
    }
  return false;
}

/* Solve the constraint graph GRAPH using our worklist solver.
   This is based on the PW* family of solvers from the "Efficient Field
   Sensitive Pointer Analysis for C" paper.
   It works by iterating over all the graph nodes, processing the complex
   constraints and propagating the copy constraints, until everything stops
   changed.  This corresponds to steps 6-8 in the solving list given above.  */

static void
solve_graph (constraint_graph_t graph)
{
  unsigned int size = graph->size;
  unsigned int i;
  bitmap pts;

  changed = BITMAP_ALLOC (NULL);

  /* Mark all initial non-collapsed nodes as changed.  */
  for (i = 1; i < size; i++)
    {
      varinfo_t ivi = get_varinfo (i);
      if (find (i) == i && !bitmap_empty_p (ivi->solution)
	  && ((graph->succs[i] && !bitmap_empty_p (graph->succs[i]))
	      || graph->complex[i].length () > 0))
	bitmap_set_bit (changed, i);
    }

  /* Allocate a bitmap to be used to store the changed bits.  */
  pts = BITMAP_ALLOC (&pta_obstack);

  while (!bitmap_empty_p (changed))
    {
      unsigned int i;
      stats.iterations++;

      bitmap_obstack_initialize (&iteration_obstack);

      auto_vec<unsigned> topo_order = compute_topo_order (graph);
      while (topo_order.length () != 0)
	{
	  i = topo_order.pop ();

	  /* If this variable is not a representative, skip it.  */
	  if (find (i) != i)
	    continue;

	  /* In certain indirect cycle cases, we may merge this
	     variable to another.  */
	  if (eliminate_indirect_cycles (i) && find (i) != i)
	    continue;

	  /* If the node has changed, we need to process the
	     complex constraints and outgoing edges again.  For complex
	     constraints that modify i itself, like the common group of
	       callarg = callarg + UNKNOWN;
	       callarg = *callarg + UNKNOWN;
	       *callarg = callescape;
	     make sure to iterate immediately because that maximizes
	     cache reuse and expands the graph quickest, leading to
	     better visitation order in the next iteration.  */
	  while (bitmap_clear_bit (changed, i))
	    {
	      bitmap solution;
	      vec<constraint_t> &complex = graph->complex[i];
	      varinfo_t vi = get_varinfo (i);
	      bool solution_empty;

	      /* Compute the changed set of solution bits.  If anything
		 is in the solution just propagate that.  */
	      if (bitmap_bit_p (vi->solution, anything_id))
		{
		  /* If anything is also in the old solution there is
		     nothing to do.
		     ???  But we shouldn't ended up with "changed" set ...  */
		  if (vi->oldsolution
		      && bitmap_bit_p (vi->oldsolution, anything_id))
		    break;
		  bitmap_copy (pts, get_varinfo (find (anything_id))->solution);
		}
	      else if (vi->oldsolution)
		bitmap_and_compl (pts, vi->solution, vi->oldsolution);
	      else
		bitmap_copy (pts, vi->solution);

	      if (bitmap_empty_p (pts))
		break;

	      if (vi->oldsolution)
		bitmap_ior_into (vi->oldsolution, pts);
	      else
		{
		  vi->oldsolution = BITMAP_ALLOC (&oldpta_obstack);
		  bitmap_copy (vi->oldsolution, pts);
		}

	      solution = vi->solution;
	      solution_empty = bitmap_empty_p (solution);

	      /* Process the complex constraints.  */
	      hash_set<constraint_t> *cvisited = nullptr;
	      if (flag_checking)
		cvisited = new hash_set<constraint_t>;
	      bitmap expanded_pts = NULL;
	      for (unsigned j = 0; j < complex.length (); ++j)
		{
		  constraint_t c = complex[j];
		  /* At unification time only the directly involved nodes
		     will get their complex constraints updated.  Update
		     our complex constraints now but keep the constraint
		     vector sorted and clear of duplicates.  Also make
		     sure to evaluate each prevailing constraint only once.  */
		  unsigned int new_lhs = find (c->lhs.var);
		  unsigned int new_rhs = find (c->rhs.var);
		  if (c->lhs.var != new_lhs || c->rhs.var != new_rhs)
		    {
		      constraint tem = *c;
		      tem.lhs.var = new_lhs;
		      tem.rhs.var = new_rhs;
		      unsigned int place
			= complex.lower_bound (&tem, constraint_less);
		      c->lhs.var = new_lhs;
		      c->rhs.var = new_rhs;
		      if (place != j)
			{
			  complex.ordered_remove (j);
			  if (j < place)
			    --place;
			  if (place < complex.length ())
			    {
			      if (constraint_equal (*complex[place], *c))
				{
				  j--;
				  continue;
				}
			      else
				complex.safe_insert (place, c);
			    }
			  else
			    complex.quick_push (c);
			  if (place > j)
			    {
			      j--;
			      continue;
			    }
			}
		    }

		  /* The only complex constraint that can change our
		     solution to non-empty, given an empty solution,
		     is a constraint where the lhs side is receiving
		     some set from elsewhere.  */
		  if (cvisited && cvisited->add (c))
		    gcc_unreachable ();
		  if (!solution_empty || c->lhs.type != DEREF)
		    do_complex_constraint (graph, c, pts, &expanded_pts);
		}
	      if (cvisited)
		{
		  /* When checking, verify the order of constraints is
		     maintained and each constraint is evaluated exactly
		     once.  */
		  for (unsigned j = 1; j < complex.length (); ++j)
		    gcc_assert (constraint_less (complex[j-1], complex[j]));
		  gcc_assert (cvisited->elements () == complex.length ());
		  delete cvisited;
		}
	      BITMAP_FREE (expanded_pts);

	      solution_empty = bitmap_empty_p (solution);

	      if (!solution_empty)
		{
		  bitmap_iterator bi;
		  unsigned eff_escaped_id = find (escaped_id);
		  unsigned j;

		  /* Propagate solution to all successors.  */
		  unsigned to_remove = ~0U;
		  EXECUTE_IF_IN_NONNULL_BITMAP (graph->succs[i],
						0, j, bi)
		    {
		      if (to_remove != ~0U)
			{
			  bitmap_clear_bit (graph->succs[i], to_remove);
			  to_remove = ~0U;
			}
		      unsigned int to = find (j);
		      if (to != j)
			{
			  /* Update the succ graph, avoiding duplicate
			     work.  */
			  to_remove = j;
			  if (! bitmap_set_bit (graph->succs[i], to))
			    continue;
			  /* We eventually end up processing 'to' twice
			     as it is undefined whether bitmap iteration
			     iterates over bits set during iteration.
			     Play safe instead of doing tricks.  */
			}
		      /* Don't try to propagate to ourselves.  */
		      if (to == i)
			{
			  to_remove = j;
			  continue;
			}
		      /* Early node unification can lead to edges from
			 escaped - remove them.  */
		      if (i == eff_escaped_id)
			{
			  to_remove = j;
			  if (bitmap_set_bit (get_varinfo (to)->solution,
					      escaped_id))
			    bitmap_set_bit (changed, to);
			  continue;
			}

		      if (bitmap_ior_into (get_varinfo (to)->solution, pts))
			bitmap_set_bit (changed, to);
		    }
		  if (to_remove != ~0U)
		    bitmap_clear_bit (graph->succs[i], to_remove);
		}
	    }
	}
      bitmap_obstack_release (&iteration_obstack);
    }

  BITMAP_FREE (pts);
  BITMAP_FREE (changed);
  bitmap_obstack_release (&oldpta_obstack);
}

void
delete_graph (void)
{
  unsigned int i;
  for (i = 0; i < graph->size; i++)
    graph->complex[i].release ();
  free (graph->complex);

  free (graph->succs);
  free (graph->pe);
  free (graph->pe_rep);
  free (graph->indirect_cycles);
  /* We are not doing free (graph->rep) since the representatives mapping is
     needed outside of the solver too.  */
  free (graph);
}

/* Remove the REF and ADDRESS edges from GRAPH, as well as all the
   predecessor edges.  */

static void
remove_preds_and_fake_succs (constraint_graph_t graph)
{
  unsigned int i;

  /* Clear the implicit ref and address nodes from the successor
     lists.  */
  for (i = 1; i < FIRST_REF_NODE; i++)
    {
      if (graph->succs[i])
	bitmap_clear_range (graph->succs[i], FIRST_REF_NODE,
			    FIRST_REF_NODE * 2);
    }

  /* Free the successor list for the non-ref nodes.  */
  for (i = FIRST_REF_NODE + 1; i < graph->size; i++)
    {
      if (graph->succs[i])
	BITMAP_FREE (graph->succs[i]);
    }

  /* Now reallocate the size of the successor list as, and blow away
     the predecessor bitmaps.  */
  graph->size = varmap.length ();
  graph->succs = XRESIZEVEC (bitmap, graph->succs, graph->size);

  free (graph->implicit_preds);
  graph->implicit_preds = NULL;
  free (graph->preds);
  graph->preds = NULL;
  bitmap_obstack_release (&predbitmap_obstack);
}

namespace pointer_analysis {

/* Solve the constraint set.  The entry function of the solver.  */

void
solve_constraints (void)
{
  class scc_info *si;

  /* Sort varinfos so that ones that cannot be pointed to are last.
     This makes bitmaps more efficient.  */
  unsigned int *map = XNEWVEC (unsigned int, varmap.length ());
  for (unsigned i = 0; i < integer_id + 1; ++i)
    map[i] = i;
  /* Start with address-taken vars, followed by not address-taken vars
     to move vars never appearing in the points-to solution bitmaps last.  */
  unsigned j = integer_id + 1;
  for (unsigned i = integer_id + 1; i < varmap.length (); ++i)
    if (varmap[varmap[i]->head]->address_taken)
      map[i] = j++;
  for (unsigned i = integer_id + 1; i < varmap.length (); ++i)
    if (! varmap[varmap[i]->head]->address_taken)
      map[i] = j++;
  /* Shuffle varmap according to map.  */
  for (unsigned i = integer_id + 1; i < varmap.length (); ++i)
    {
      while (map[varmap[i]->id] != i)
	std::swap (varmap[i], varmap[map[varmap[i]->id]]);
      gcc_assert (bitmap_empty_p (varmap[i]->solution));
      varmap[i]->id = i;
      varmap[i]->next = map[varmap[i]->next];
      varmap[i]->head = map[varmap[i]->head];
    }
  /* Finally rewrite constraints.  */
  for (unsigned i = 0; i < constraints.length (); ++i)
    {
      constraints[i]->lhs.var = map[constraints[i]->lhs.var];
      constraints[i]->rhs.var = map[constraints[i]->rhs.var];
    }
  free (map);

  if (dump_file)
    fprintf (dump_file,
	     "\nCollapsing static cycles and doing variable "
	     "substitution\n");

  init_graph (varmap.length () * 2);

  if (dump_file)
    fprintf (dump_file, "Building predecessor graph\n");
  build_pred_graph ();

  if (dump_file)
    fprintf (dump_file, "Detecting pointer and location "
	     "equivalences\n");
  si = perform_var_substitution (graph);

  if (dump_file)
    fprintf (dump_file, "Rewriting constraints and unifying "
	     "variables\n");
  rewrite_constraints (graph, si);

  build_succ_graph ();

  free_var_substitution_info (si);

  /* Attach complex constraints to graph nodes.  */
  move_complex_constraints (graph);

  if (dump_file)
    fprintf (dump_file, "Uniting pointer but not location equivalent "
	     "variables\n");
  unite_pointer_equivalences (graph);

  if (dump_file)
    fprintf (dump_file, "Finding indirect cycles\n");
  find_indirect_cycles (graph);

  /* Implicit nodes and predecessors are no longer necessary at this
     point.  */
  remove_preds_and_fake_succs (graph);

  if (dump_file && (dump_flags & TDF_GRAPH))
    {
      fprintf (dump_file, "\n\n// The constraint graph before solve-graph "
	       "in dot format:\n");
      dump_constraint_graph (dump_file);
      fprintf (dump_file, "\n\n");
    }

  if (dump_file)
    fprintf (dump_file, "Solving graph\n");

  solve_graph (graph);

  if (dump_file && (dump_flags & TDF_GRAPH))
    {
      fprintf (dump_file, "\n\n// The constraint graph after solve-graph "
	       "in dot format:\n");
      dump_constraint_graph (dump_file);
      fprintf (dump_file, "\n\n");
    }

  /* The mapping node -> representative is one of the outputs of the
     solver.  */
  union_find_compress_all ();
  var_rep = graph->rep;

  delete_graph ();
}

} // namespace pointer_analysis
