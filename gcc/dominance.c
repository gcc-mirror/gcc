/* Calculate (post)dominators in slightly super-linear time.
   Copyright (C) 2000-2016 Free Software Foundation, Inc.
   Contributed by Michael Matz (matz@ifh.de).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This file implements the well known algorithm from Lengauer and Tarjan
   to compute the dominators in a control flow graph.  A basic block D is said
   to dominate another block X, when all paths from the entry node of the CFG
   to X go also over D.  The dominance relation is a transitive reflexive
   relation and its minimal transitive reduction is a tree, called the
   dominator tree.  So for each block X besides the entry block exists a
   block I(X), called the immediate dominator of X, which is the parent of X
   in the dominator tree.

   The algorithm computes this dominator tree implicitly by computing for
   each block its immediate dominator.  We use tree balancing and path
   compression, so it's the O(e*a(e,v)) variant, where a(e,v) is the very
   slowly growing functional inverse of the Ackerman function.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "timevar.h"
#include "diagnostic-core.h"
#include "cfganal.h"
#include "et-forest.h"
#include "graphds.h"

/* We name our nodes with integers, beginning with 1.  Zero is reserved for
   'undefined' or 'end of list'.  The name of each node is given by the dfs
   number of the corresponding basic block.  Please note, that we include the
   artificial ENTRY_BLOCK (or EXIT_BLOCK in the post-dom case) in our lists to
   support multiple entry points.  Its dfs number is of course 1.  */

/* Type of Basic Block aka. TBB */
typedef unsigned int TBB;

namespace {

/* This class holds various arrays reflecting the (sub)structure of the
   flowgraph.  Most of them are of type TBB and are also indexed by TBB.  */

class dom_info
{
public:
  dom_info (function *, cdi_direction);
  ~dom_info ();
  void calc_dfs_tree ();
  void calc_idoms ();

  inline basic_block get_idom (basic_block);
private:
  void calc_dfs_tree_nonrec (basic_block);
  void compress (TBB);
  TBB eval (TBB);
  void link_roots (TBB, TBB);

  /* The parent of a node in the DFS tree.  */
  TBB *m_dfs_parent;
  /* For a node x m_key[x] is roughly the node nearest to the root from which
     exists a way to x only over nodes behind x.  Such a node is also called
     semidominator.  */
  TBB *m_key;
  /* The value in m_path_min[x] is the node y on the path from x to the root of
     the tree x is in with the smallest m_key[y].  */
  TBB *m_path_min;
  /* m_bucket[x] points to the first node of the set of nodes having x as
     key.  */
  TBB *m_bucket;
  /* And m_next_bucket[x] points to the next node.  */
  TBB *m_next_bucket;
  /* After the algorithm is done, m_dom[x] contains the immediate dominator
     of x.  */
  TBB *m_dom;

  /* The following few fields implement the structures needed for disjoint
     sets.  */
  /* m_set_chain[x] is the next node on the path from x to the representative
     of the set containing x.  If m_set_chain[x]==0 then x is a root.  */
  TBB *m_set_chain;
  /* m_set_size[x] is the number of elements in the set named by x.  */
  unsigned int *m_set_size;
  /* m_set_child[x] is used for balancing the tree representing a set.  It can
     be understood as the next sibling of x.  */
  TBB *m_set_child;

  /* If b is the number of a basic block (BB->index), m_dfs_order[b] is the
     number of that node in DFS order counted from 1.  This is an index
     into most of the other arrays in this structure.  */
  TBB *m_dfs_order;
  /* Points to last element in m_dfs_order array.  */
  TBB *m_dfs_last;
  /* If x is the DFS-index of a node which corresponds with a basic block,
     m_dfs_to_bb[x] is that basic block.  Note, that in our structure there are
     more nodes that basic blocks, so only
     m_dfs_to_bb[m_dfs_order[bb->index]]==bb is true for every basic block bb,
     but not the opposite.  */
  basic_block *m_dfs_to_bb;

  /* This is the next free DFS number when creating the DFS tree.  */
  unsigned int m_dfsnum;
  /* The number of nodes in the DFS tree (==m_dfsnum-1).  */
  unsigned int m_nodes;

  /* Blocks with bits set here have a fake edge to EXIT.  These are used
     to turn a DFS forest into a proper tree.  */
  bitmap m_fake_exit_edge;

  /* Number of basic blocks in the function being compiled.  */
  size_t m_n_basic_blocks;

  /* True, if we are computing postdominators (rather than dominators).  */
  bool m_reverse;

  /* Start block (the entry block for forward problem, exit block for backward
     problem).  */
  basic_block m_start_block;
  /* Ending block.  */
  basic_block m_end_block;
};

} // anonymous namespace

void debug_dominance_info (cdi_direction);
void debug_dominance_tree (cdi_direction, basic_block);

/* Allocate and zero-initialize NUM elements of type T (T must be a
   POD-type).  Note: after transition to C++11 or later,
   `x = new_zero_array <T> (num);' can be replaced with
   `x = new T[num] {};'.  */

template<typename T>
inline T *new_zero_array (size_t num)
{
  T *result = new T[num];
  memset (result, 0, sizeof (T) * num);
  return result;
}

/* Allocate all needed memory in a pessimistic fashion (so we round up).  */

dom_info::dom_info (function *fn, cdi_direction dir)
{
  /* We need memory for n_basic_blocks nodes.  */
  size_t num = m_n_basic_blocks = n_basic_blocks_for_fn (fn);
  m_dfs_parent = new_zero_array <TBB> (num);
  m_dom = new_zero_array <TBB> (num);

  m_path_min = new TBB[num];
  m_key = new TBB[num];
  m_set_size = new unsigned int[num];
  for (size_t i = 0; i < num; i++)
    {
      m_path_min[i] = m_key[i] = i;
      m_set_size[i] = 1;
    }

  m_bucket = new_zero_array <TBB> (num);
  m_next_bucket = new_zero_array <TBB> (num);

  m_set_chain = new_zero_array <TBB> (num);
  m_set_child = new_zero_array <TBB> (num);

  unsigned last_bb_index = last_basic_block_for_fn (fn);
  m_dfs_order = new_zero_array <TBB> (last_bb_index + 1);
  m_dfs_last = &m_dfs_order[last_bb_index];
  m_dfs_to_bb = new_zero_array <basic_block> (num);

  m_dfsnum = 1;
  m_nodes = 0;

  switch (dir)
    {
      case CDI_DOMINATORS:
	m_reverse = false;
	m_fake_exit_edge = NULL;
	m_start_block = ENTRY_BLOCK_PTR_FOR_FN (fn);
	m_end_block = EXIT_BLOCK_PTR_FOR_FN (fn);
	break;
      case CDI_POST_DOMINATORS:
	m_reverse = true;
	m_fake_exit_edge = BITMAP_ALLOC (NULL);
	m_start_block = EXIT_BLOCK_PTR_FOR_FN (fn);
	m_end_block = ENTRY_BLOCK_PTR_FOR_FN (fn);
	break;
      default:
	gcc_unreachable ();
    }
}

inline basic_block
dom_info::get_idom (basic_block bb)
{
  TBB d = m_dom[m_dfs_order[bb->index]];
  return m_dfs_to_bb[d];
}

/* Map dominance calculation type to array index used for various
   dominance information arrays.  This version is simple -- it will need
   to be modified, obviously, if additional values are added to
   cdi_direction.  */

static inline unsigned int
dom_convert_dir_to_idx (cdi_direction dir)
{
  gcc_checking_assert (dir == CDI_DOMINATORS || dir == CDI_POST_DOMINATORS);
  return dir - 1;
}

/* Free all allocated memory in dom_info.  */

dom_info::~dom_info ()
{
  delete[] m_dfs_parent;
  delete[] m_path_min;
  delete[] m_key;
  delete[] m_dom;
  delete[] m_bucket;
  delete[] m_next_bucket;
  delete[] m_set_chain;
  delete[] m_set_size;
  delete[] m_set_child;
  delete[] m_dfs_order;
  delete[] m_dfs_to_bb;
  BITMAP_FREE (m_fake_exit_edge);
}

/* The nonrecursive variant of creating a DFS tree.  BB is the starting basic
   block for this tree and m_reverse is true, if predecessors should be visited
   instead of successors of a node.  After this is done all nodes reachable
   from BB were visited, have assigned their dfs number and are linked together
   to form a tree.  */

void
dom_info::calc_dfs_tree_nonrec (basic_block bb)
{
  edge_iterator *stack = new edge_iterator[m_n_basic_blocks + 1];
  int sp = 0;

  /* Initialize the first edge.  */
  edge_iterator ei = m_reverse ? ei_start (bb->preds)
			       : ei_start (bb->succs);

  /* When the stack is empty we break out of this loop.  */
  while (1)
    {
      basic_block bn;
      edge_iterator einext;

      /* This loop traverses edges e in depth first manner, and fills the
         stack.  */
      while (!ei_end_p (ei))
	{
	  edge e = ei_edge (ei);

	  /* Deduce from E the current and the next block (BB and BN), and the
	     next edge.  */
	  if (m_reverse)
	    {
	      bn = e->src;

	      /* If the next node BN is either already visited or a border
	         block the current edge is useless, and simply overwritten
	         with the next edge out of the current node.  */
	      if (bn == m_end_block || m_dfs_order[bn->index])
		{
		  ei_next (&ei);
		  continue;
		}
	      bb = e->dest;
	      einext = ei_start (bn->preds);
	    }
	  else
	    {
	      bn = e->dest;
	      if (bn == m_end_block || m_dfs_order[bn->index])
		{
		  ei_next (&ei);
		  continue;
		}
	      bb = e->src;
	      einext = ei_start (bn->succs);
	    }

	  gcc_assert (bn != m_start_block);

	  /* Fill the DFS tree info calculatable _before_ recursing.  */
	  TBB my_i;
	  if (bb != m_start_block)
	    my_i = m_dfs_order[bb->index];
	  else
	    my_i = *m_dfs_last;
	  TBB child_i = m_dfs_order[bn->index] = m_dfsnum++;
	  m_dfs_to_bb[child_i] = bn;
	  m_dfs_parent[child_i] = my_i;

	  /* Save the current point in the CFG on the stack, and recurse.  */
	  stack[sp++] = ei;
	  ei = einext;
	}

      if (!sp)
	break;
      ei = stack[--sp];

      /* OK.  The edge-list was exhausted, meaning normally we would
         end the recursion.  After returning from the recursive call,
         there were (may be) other statements which were run after a
         child node was completely considered by DFS.  Here is the
         point to do it in the non-recursive variant.
         E.g. The block just completed is in e->dest for forward DFS,
         the block not yet completed (the parent of the one above)
         in e->src.  This could be used e.g. for computing the number of
         descendants or the tree depth.  */
      ei_next (&ei);
    }
  delete[] stack;
}

/* The main entry for calculating the DFS tree or forest.  m_reverse is true,
   if we are interested in the reverse flow graph.  In that case the result is
   not necessarily a tree but a forest, because there may be nodes from which
   the EXIT_BLOCK is unreachable.  */

void
dom_info::calc_dfs_tree ()
{
  *m_dfs_last = m_dfsnum;
  m_dfs_to_bb[m_dfsnum] = m_start_block;
  m_dfsnum++;

  calc_dfs_tree_nonrec (m_start_block);

  if (m_reverse)
    {
      /* In the post-dom case we may have nodes without a path to EXIT_BLOCK.
         They are reverse-unreachable.  In the dom-case we disallow such
         nodes, but in post-dom we have to deal with them.

	 There are two situations in which this occurs.  First, noreturn
	 functions.  Second, infinite loops.  In the first case we need to
	 pretend that there is an edge to the exit block.  In the second
	 case, we wind up with a forest.  We need to process all noreturn
	 blocks before we know if we've got any infinite loops.  */

      basic_block b;
      bool saw_unconnected = false;

      FOR_BB_BETWEEN (b, m_start_block->prev_bb, m_end_block, prev_bb)
	{
	  if (EDGE_COUNT (b->succs) > 0)
	    {
	      if (m_dfs_order[b->index] == 0)
		saw_unconnected = true;
	      continue;
	    }
	  bitmap_set_bit (m_fake_exit_edge, b->index);
	  m_dfs_order[b->index] = m_dfsnum;
	  m_dfs_to_bb[m_dfsnum] = b;
	  m_dfs_parent[m_dfsnum] = *m_dfs_last;
	  m_dfsnum++;
	  calc_dfs_tree_nonrec (b);
	}

      if (saw_unconnected)
	{
	  FOR_BB_BETWEEN (b, m_start_block->prev_bb, m_end_block, prev_bb)
	    {
	      if (m_dfs_order[b->index])
		continue;
	      basic_block b2 = dfs_find_deadend (b);
	      gcc_checking_assert (m_dfs_order[b2->index] == 0);
	      bitmap_set_bit (m_fake_exit_edge, b2->index);
	      m_dfs_order[b2->index] = m_dfsnum;
	      m_dfs_to_bb[m_dfsnum] = b2;
	      m_dfs_parent[m_dfsnum] = *m_dfs_last;
	      m_dfsnum++;
	      calc_dfs_tree_nonrec (b2);
	      gcc_checking_assert (m_dfs_order[b->index]);
	    }
	}
    }

  m_nodes = m_dfsnum - 1;

  /* This aborts e.g. when there is _no_ path from ENTRY to EXIT at all.  */
  gcc_assert (m_nodes == (unsigned int) m_n_basic_blocks - 1);
}

/* Compress the path from V to the root of its set and update path_min at the
   same time.  After compress(di, V) set_chain[V] is the root of the set V is
   in and path_min[V] is the node with the smallest key[] value on the path
   from V to that root.  */

void
dom_info::compress (TBB v)
{
  /* Btw. It's not worth to unrecurse compress() as the depth is usually not
     greater than 5 even for huge graphs (I've not seen call depth > 4).
     Also performance wise compress() ranges _far_ behind eval().  */
  TBB parent = m_set_chain[v];
  if (m_set_chain[parent])
    {
      compress (parent);
      if (m_key[m_path_min[parent]] < m_key[m_path_min[v]])
	m_path_min[v] = m_path_min[parent];
      m_set_chain[v] = m_set_chain[parent];
    }
}

/* Compress the path from V to the set root of V if needed (when the root has
   changed since the last call).  Returns the node with the smallest key[]
   value on the path from V to the root.  */

inline TBB
dom_info::eval (TBB v)
{
  /* The representative of the set V is in, also called root (as the set
     representation is a tree).  */
  TBB rep = m_set_chain[v];

  /* V itself is the root.  */
  if (!rep)
    return m_path_min[v];

  /* Compress only if necessary.  */
  if (m_set_chain[rep])
    {
      compress (v);
      rep = m_set_chain[v];
    }

  if (m_key[m_path_min[rep]] >= m_key[m_path_min[v]])
    return m_path_min[v];
  else
    return m_path_min[rep];
}

/* This essentially merges the two sets of V and W, giving a single set with
   the new root V.  The internal representation of these disjoint sets is a
   balanced tree.  Currently link(V,W) is only used with V being the parent
   of W.  */

void
dom_info::link_roots (TBB v, TBB w)
{
  TBB s = w;

  /* Rebalance the tree.  */
  while (m_key[m_path_min[w]] < m_key[m_path_min[m_set_child[s]]])
    {
      if (m_set_size[s] + m_set_size[m_set_child[m_set_child[s]]]
	  >= 2 * m_set_size[m_set_child[s]])
	{
	  m_set_chain[m_set_child[s]] = s;
	  m_set_child[s] = m_set_child[m_set_child[s]];
	}
      else
	{
	  m_set_size[m_set_child[s]] = m_set_size[s];
	  s = m_set_chain[s] = m_set_child[s];
	}
    }

  m_path_min[s] = m_path_min[w];
  m_set_size[v] += m_set_size[w];
  if (m_set_size[v] < 2 * m_set_size[w])
    std::swap (m_set_child[v], s);

  /* Merge all subtrees.  */
  while (s)
    {
      m_set_chain[s] = v;
      s = m_set_child[s];
    }
}

/* This calculates the immediate dominators (or post-dominators). THIS is our
   working structure and should hold the DFS forest.
   On return the immediate dominator to node V is in m_dom[V].  */

void
dom_info::calc_idoms ()
{
  /* Go backwards in DFS order, to first look at the leafs.  */
  for (TBB v = m_nodes; v > 1; v--)
    {
      basic_block bb = m_dfs_to_bb[v];
      edge e;

      TBB par = m_dfs_parent[v];
      TBB k = v;

      edge_iterator ei = m_reverse ? ei_start (bb->succs)
				   : ei_start (bb->preds);
      edge_iterator einext;

      if (m_reverse)
	{
	  /* If this block has a fake edge to exit, process that first.  */
	  if (bitmap_bit_p (m_fake_exit_edge, bb->index))
	    {
	      einext = ei;
	      einext.index = 0;
	      goto do_fake_exit_edge;
	    }
	}

      /* Search all direct predecessors for the smallest node with a path
         to them.  That way we have the smallest node with also a path to
         us only over nodes behind us.  In effect we search for our
         semidominator.  */
      while (!ei_end_p (ei))
	{
	  basic_block b;
	  TBB k1;

	  e = ei_edge (ei);
	  b = m_reverse ? e->dest : e->src;
	  einext = ei;
	  ei_next (&einext);

	  if (b == m_start_block)
	    {
	    do_fake_exit_edge:
	      k1 = *m_dfs_last;
	    }
	  else
	    k1 = m_dfs_order[b->index];

	  /* Call eval() only if really needed.  If k1 is above V in DFS tree,
	     then we know, that eval(k1) == k1 and key[k1] == k1.  */
	  if (k1 > v)
	    k1 = m_key[eval (k1)];
	  if (k1 < k)
	    k = k1;

	  ei = einext;
	}

      m_key[v] = k;
      link_roots (par, v);
      m_next_bucket[v] = m_bucket[k];
      m_bucket[k] = v;

      /* Transform semidominators into dominators.  */
      for (TBB w = m_bucket[par]; w; w = m_next_bucket[w])
	{
	  k = eval (w);
	  if (m_key[k] < m_key[w])
	    m_dom[w] = k;
	  else
	    m_dom[w] = par;
	}
      /* We don't need to cleanup next_bucket[].  */
      m_bucket[par] = 0;
    }

  /* Explicitly define the dominators.  */
  m_dom[1] = 0;
  for (TBB v = 2; v <= m_nodes; v++)
    if (m_dom[v] != m_key[v])
      m_dom[v] = m_dom[m_dom[v]];
}

/* Assign dfs numbers starting from NUM to NODE and its sons.  */

static void
assign_dfs_numbers (struct et_node *node, int *num)
{
  struct et_node *son;

  node->dfs_num_in = (*num)++;

  if (node->son)
    {
      assign_dfs_numbers (node->son, num);
      for (son = node->son->right; son != node->son; son = son->right)
	assign_dfs_numbers (son, num);
    }

  node->dfs_num_out = (*num)++;
}

/* Compute the data necessary for fast resolving of dominator queries in a
   static dominator tree.  */

static void
compute_dom_fast_query (enum cdi_direction dir)
{
  int num = 0;
  basic_block bb;
  unsigned int dir_index = dom_convert_dir_to_idx (dir);

  gcc_checking_assert (dom_info_available_p (dir));

  if (dom_computed[dir_index] == DOM_OK)
    return;

  FOR_ALL_BB_FN (bb, cfun)
    {
      if (!bb->dom[dir_index]->father)
	assign_dfs_numbers (bb->dom[dir_index], &num);
    }

  dom_computed[dir_index] = DOM_OK;
}

/* The main entry point into this module.  DIR is set depending on whether
   we want to compute dominators or postdominators.  */

void
calculate_dominance_info (cdi_direction dir)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);

  if (dom_computed[dir_index] == DOM_OK)
    {
      checking_verify_dominators (dir);
      return;
    }

  timevar_push (TV_DOMINANCE);
  if (!dom_info_available_p (dir))
    {
      gcc_assert (!n_bbs_in_dom_tree[dir_index]);

      basic_block b;
      FOR_ALL_BB_FN (b, cfun)
	{
	  b->dom[dir_index] = et_new_tree (b);
	}
      n_bbs_in_dom_tree[dir_index] = n_basic_blocks_for_fn (cfun);

      dom_info di (cfun, dir);
      di.calc_dfs_tree ();
      di.calc_idoms ();

      FOR_EACH_BB_FN (b, cfun)
	{
	  if (basic_block d = di.get_idom (b))
	    et_set_father (b->dom[dir_index], d->dom[dir_index]);
	}

      dom_computed[dir_index] = DOM_NO_FAST_QUERY;
    }
  else
    checking_verify_dominators (dir);

  compute_dom_fast_query (dir);

  timevar_pop (TV_DOMINANCE);
}

/* Free dominance information for direction DIR.  */
void
free_dominance_info (function *fn, enum cdi_direction dir)
{
  basic_block bb;
  unsigned int dir_index = dom_convert_dir_to_idx (dir);

  if (!dom_info_available_p (fn, dir))
    return;

  FOR_ALL_BB_FN (bb, fn)
    {
      et_free_tree_force (bb->dom[dir_index]);
      bb->dom[dir_index] = NULL;
    }
  et_free_pools ();

  fn->cfg->x_n_bbs_in_dom_tree[dir_index] = 0;

  fn->cfg->x_dom_computed[dir_index] = DOM_NONE;
}

void
free_dominance_info (enum cdi_direction dir)
{
  free_dominance_info (cfun, dir);
}

/* Return the immediate dominator of basic block BB.  */
basic_block
get_immediate_dominator (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *node = bb->dom[dir_index];

  gcc_checking_assert (dom_computed[dir_index]);

  if (!node->father)
    return NULL;

  return (basic_block) node->father->data;
}

/* Set the immediate dominator of the block possibly removing
   existing edge.  NULL can be used to remove any edge.  */
void
set_immediate_dominator (enum cdi_direction dir, basic_block bb,
			 basic_block dominated_by)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *node = bb->dom[dir_index];

  gcc_checking_assert (dom_computed[dir_index]);

  if (node->father)
    {
      if (node->father->data == dominated_by)
	return;
      et_split (node);
    }

  if (dominated_by)
    et_set_father (node, dominated_by->dom[dir_index]);

  if (dom_computed[dir_index] == DOM_OK)
    dom_computed[dir_index] = DOM_NO_FAST_QUERY;
}

/* Returns the list of basic blocks immediately dominated by BB, in the
   direction DIR.  */
vec<basic_block> 
get_dominated_by (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *node = bb->dom[dir_index], *son = node->son, *ason;
  vec<basic_block> bbs = vNULL;

  gcc_checking_assert (dom_computed[dir_index]);

  if (!son)
    return vNULL;

  bbs.safe_push ((basic_block) son->data);
  for (ason = son->right; ason != son; ason = ason->right)
    bbs.safe_push ((basic_block) ason->data);

  return bbs;
}

/* Returns the list of basic blocks that are immediately dominated (in
   direction DIR) by some block between N_REGION ones stored in REGION,
   except for blocks in the REGION itself.  */

vec<basic_block> 
get_dominated_by_region (enum cdi_direction dir, basic_block *region,
			 unsigned n_region)
{
  unsigned i;
  basic_block dom;
  vec<basic_block> doms = vNULL;

  for (i = 0; i < n_region; i++)
    region[i]->flags |= BB_DUPLICATED;
  for (i = 0; i < n_region; i++)
    for (dom = first_dom_son (dir, region[i]);
	 dom;
	 dom = next_dom_son (dir, dom))
      if (!(dom->flags & BB_DUPLICATED))
	doms.safe_push (dom);
  for (i = 0; i < n_region; i++)
    region[i]->flags &= ~BB_DUPLICATED;

  return doms;
}

/* Returns the list of basic blocks including BB dominated by BB, in the
   direction DIR up to DEPTH in the dominator tree.  The DEPTH of zero will
   produce a vector containing all dominated blocks.  The vector will be sorted
   in preorder.  */

vec<basic_block> 
get_dominated_to_depth (enum cdi_direction dir, basic_block bb, int depth)
{
  vec<basic_block> bbs = vNULL;
  unsigned i;
  unsigned next_level_start;

  i = 0;
  bbs.safe_push (bb);
  next_level_start = 1; /* = bbs.length (); */

  do
    {
      basic_block son;

      bb = bbs[i++];
      for (son = first_dom_son (dir, bb);
	   son;
	   son = next_dom_son (dir, son))
	bbs.safe_push (son);

      if (i == next_level_start && --depth)
	next_level_start = bbs.length ();
    }
  while (i < next_level_start);

  return bbs;
}

/* Returns the list of basic blocks including BB dominated by BB, in the
   direction DIR.  The vector will be sorted in preorder.  */

vec<basic_block> 
get_all_dominated_blocks (enum cdi_direction dir, basic_block bb)
{
  return get_dominated_to_depth (dir, bb, 0);
}

/* Redirect all edges pointing to BB to TO.  */
void
redirect_immediate_dominators (enum cdi_direction dir, basic_block bb,
			       basic_block to)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *bb_node, *to_node, *son;

  bb_node = bb->dom[dir_index];
  to_node = to->dom[dir_index];

  gcc_checking_assert (dom_computed[dir_index]);

  if (!bb_node->son)
    return;

  while (bb_node->son)
    {
      son = bb_node->son;

      et_split (son);
      et_set_father (son, to_node);
    }

  if (dom_computed[dir_index] == DOM_OK)
    dom_computed[dir_index] = DOM_NO_FAST_QUERY;
}

/* Find first basic block in the tree dominating both BB1 and BB2.  */
basic_block
nearest_common_dominator (enum cdi_direction dir, basic_block bb1, basic_block bb2)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);

  gcc_checking_assert (dom_computed[dir_index]);

  if (!bb1)
    return bb2;
  if (!bb2)
    return bb1;

  return (basic_block) et_nca (bb1->dom[dir_index], bb2->dom[dir_index])->data;
}


/* Find the nearest common dominator for the basic blocks in BLOCKS,
   using dominance direction DIR.  */

basic_block
nearest_common_dominator_for_set (enum cdi_direction dir, bitmap blocks)
{
  unsigned i, first;
  bitmap_iterator bi;
  basic_block dom;

  first = bitmap_first_set_bit (blocks);
  dom = BASIC_BLOCK_FOR_FN (cfun, first);
  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, i, bi)
    if (dom != BASIC_BLOCK_FOR_FN (cfun, i))
      dom = nearest_common_dominator (dir, dom, BASIC_BLOCK_FOR_FN (cfun, i));

  return dom;
}

/*  Given a dominator tree, we can determine whether one thing
    dominates another in constant time by using two DFS numbers:

    1. The number for when we visit a node on the way down the tree
    2. The number for when we visit a node on the way back up the tree

    You can view these as bounds for the range of dfs numbers the
    nodes in the subtree of the dominator tree rooted at that node
    will contain.

    The dominator tree is always a simple acyclic tree, so there are
    only three possible relations two nodes in the dominator tree have
    to each other:

    1. Node A is above Node B (and thus, Node A dominates node B)

     A
     |
     C
    / \
   B   D


   In the above case, DFS_Number_In of A will be <= DFS_Number_In of
   B, and DFS_Number_Out of A will be >= DFS_Number_Out of B.  This is
   because we must hit A in the dominator tree *before* B on the walk
   down, and we will hit A *after* B on the walk back up

   2. Node A is below node B (and thus, node B dominates node A)


     B
     |
     A
    / \
   C   D

   In the above case, DFS_Number_In of A will be >= DFS_Number_In of
   B, and DFS_Number_Out of A will be <= DFS_Number_Out of B.

   This is because we must hit A in the dominator tree *after* B on
   the walk down, and we will hit A *before* B on the walk back up

   3. Node A and B are siblings (and thus, neither dominates the other)

     C
     |
     D
    / \
   A   B

   In the above case, DFS_Number_In of A will *always* be <=
   DFS_Number_In of B, and DFS_Number_Out of A will *always* be <=
   DFS_Number_Out of B.  This is because we will always finish the dfs
   walk of one of the subtrees before the other, and thus, the dfs
   numbers for one subtree can't intersect with the range of dfs
   numbers for the other subtree.  If you swap A and B's position in
   the dominator tree, the comparison changes direction, but the point
   is that both comparisons will always go the same way if there is no
   dominance relationship.

   Thus, it is sufficient to write

   A_Dominates_B (node A, node B)
   {
     return DFS_Number_In(A) <= DFS_Number_In(B)
            && DFS_Number_Out (A) >= DFS_Number_Out(B);
   }

   A_Dominated_by_B (node A, node B)
   {
     return DFS_Number_In(A) >= DFS_Number_In(B)
            && DFS_Number_Out (A) <= DFS_Number_Out(B);
   }  */

/* Return TRUE in case BB1 is dominated by BB2.  */
bool
dominated_by_p (enum cdi_direction dir, const_basic_block bb1, const_basic_block bb2)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *n1 = bb1->dom[dir_index], *n2 = bb2->dom[dir_index];

  gcc_checking_assert (dom_computed[dir_index]);

  if (dom_computed[dir_index] == DOM_OK)
    return (n1->dfs_num_in >= n2->dfs_num_in
  	    && n1->dfs_num_out <= n2->dfs_num_out);

  return et_below (n1, n2);
}

/* Returns the entry dfs number for basic block BB, in the direction DIR.  */

unsigned
bb_dom_dfs_in (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *n = bb->dom[dir_index];

  gcc_checking_assert (dom_computed[dir_index] == DOM_OK);
  return n->dfs_num_in;
}

/* Returns the exit dfs number for basic block BB, in the direction DIR.  */

unsigned
bb_dom_dfs_out (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *n = bb->dom[dir_index];

  gcc_checking_assert (dom_computed[dir_index] == DOM_OK);
  return n->dfs_num_out;
}

/* Verify invariants of dominator structure.  */
DEBUG_FUNCTION void
verify_dominators (cdi_direction dir)
{
  gcc_assert (dom_info_available_p (dir));

  dom_info di (cfun, dir);
  di.calc_dfs_tree ();
  di.calc_idoms ();

  bool err = false;
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      basic_block imm_bb = get_immediate_dominator (dir, bb);
      if (!imm_bb)
	{
	  error ("dominator of %d status unknown", bb->index);
	  err = true;
	}

      basic_block imm_bb_correct = di.get_idom (bb);
      if (imm_bb != imm_bb_correct)
	{
	  error ("dominator of %d should be %d, not %d",
		 bb->index, imm_bb_correct->index, imm_bb->index);
	  err = true;
	}
    }

  gcc_assert (!err);
}

/* Determine immediate dominator (or postdominator, according to DIR) of BB,
   assuming that dominators of other blocks are correct.  We also use it to
   recompute the dominators in a restricted area, by iterating it until it
   reaches a fixed point.  */

basic_block
recompute_dominator (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  basic_block dom_bb = NULL;
  edge e;
  edge_iterator ei;

  gcc_checking_assert (dom_computed[dir_index]);

  if (dir == CDI_DOMINATORS)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (!dominated_by_p (dir, e->src, bb))
	    dom_bb = nearest_common_dominator (dir, dom_bb, e->src);
	}
    }
  else
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (!dominated_by_p (dir, e->dest, bb))
	    dom_bb = nearest_common_dominator (dir, dom_bb, e->dest);
	}
    }

  return dom_bb;
}

/* Use simple heuristics (see iterate_fix_dominators) to determine dominators
   of BBS.  We assume that all the immediate dominators except for those of the
   blocks in BBS are correct.  If CONSERVATIVE is true, we also assume that the
   currently recorded immediate dominators of blocks in BBS really dominate the
   blocks.  The basic blocks for that we determine the dominator are removed
   from BBS.  */

static void
prune_bbs_to_update_dominators (vec<basic_block> bbs,
				bool conservative)
{
  unsigned i;
  bool single;
  basic_block bb, dom = NULL;
  edge_iterator ei;
  edge e;

  for (i = 0; bbs.iterate (i, &bb);)
    {
      if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	goto succeed;

      if (single_pred_p (bb))
	{
	  set_immediate_dominator (CDI_DOMINATORS, bb, single_pred (bb));
	  goto succeed;
	}

      if (!conservative)
	goto fail;

      single = true;
      dom = NULL;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (dominated_by_p (CDI_DOMINATORS, e->src, bb))
	    continue;

	  if (!dom)
	    dom = e->src;
	  else
	    {
	      single = false;
	      dom = nearest_common_dominator (CDI_DOMINATORS, dom, e->src);
	    }
	}

      gcc_assert (dom != NULL);
      if (single
	  || find_edge (dom, bb))
	{
	  set_immediate_dominator (CDI_DOMINATORS, bb, dom);
	  goto succeed;
	}

fail:
      i++;
      continue;

succeed:
      bbs.unordered_remove (i);
    }
}

/* Returns root of the dominance tree in the direction DIR that contains
   BB.  */

static basic_block
root_of_dom_tree (enum cdi_direction dir, basic_block bb)
{
  return (basic_block) et_root (bb->dom[dom_convert_dir_to_idx (dir)])->data;
}

/* See the comment in iterate_fix_dominators.  Finds the immediate dominators
   for the sons of Y, found using the SON and BROTHER arrays representing
   the dominance tree of graph G.  BBS maps the vertices of G to the basic
   blocks.  */

static void
determine_dominators_for_sons (struct graph *g, vec<basic_block> bbs,
			       int y, int *son, int *brother)
{
  bitmap gprime;
  int i, a, nc;
  vec<int> *sccs;
  basic_block bb, dom, ybb;
  unsigned si;
  edge e;
  edge_iterator ei;

  if (son[y] == -1)
    return;
  if (y == (int) bbs.length ())
    ybb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  else
    ybb = bbs[y];

  if (brother[son[y]] == -1)
    {
      /* Handle the common case Y has just one son specially.  */
      bb = bbs[son[y]];
      set_immediate_dominator (CDI_DOMINATORS, bb,
			       recompute_dominator (CDI_DOMINATORS, bb));
      identify_vertices (g, y, son[y]);
      return;
    }

  gprime = BITMAP_ALLOC (NULL);
  for (a = son[y]; a != -1; a = brother[a])
    bitmap_set_bit (gprime, a);

  nc = graphds_scc (g, gprime);
  BITMAP_FREE (gprime);

  /* ???  Needed to work around the pre-processor confusion with
     using a multi-argument template type as macro argument.  */
  typedef vec<int> vec_int_heap;
  sccs = XCNEWVEC (vec_int_heap, nc);
  for (a = son[y]; a != -1; a = brother[a])
    sccs[g->vertices[a].component].safe_push (a);

  for (i = nc - 1; i >= 0; i--)
    {
      dom = NULL;
      FOR_EACH_VEC_ELT (sccs[i], si, a)
	{
	  bb = bbs[a];
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      if (root_of_dom_tree (CDI_DOMINATORS, e->src) != ybb)
		continue;

	      dom = nearest_common_dominator (CDI_DOMINATORS, dom, e->src);
	    }
	}

      gcc_assert (dom != NULL);
      FOR_EACH_VEC_ELT (sccs[i], si, a)
	{
	  bb = bbs[a];
	  set_immediate_dominator (CDI_DOMINATORS, bb, dom);
	}
    }

  for (i = 0; i < nc; i++)
    sccs[i].release ();
  free (sccs);

  for (a = son[y]; a != -1; a = brother[a])
    identify_vertices (g, y, a);
}

/* Recompute dominance information for basic blocks in the set BBS.  The
   function assumes that the immediate dominators of all the other blocks
   in CFG are correct, and that there are no unreachable blocks.

   If CONSERVATIVE is true, we additionally assume that all the ancestors of
   a block of BBS in the current dominance tree dominate it.  */

void
iterate_fix_dominators (enum cdi_direction dir, vec<basic_block> bbs,
			bool conservative)
{
  unsigned i;
  basic_block bb, dom;
  struct graph *g;
  int n, y;
  size_t dom_i;
  edge e;
  edge_iterator ei;
  int *parent, *son, *brother;
  unsigned int dir_index = dom_convert_dir_to_idx (dir);

  /* We only support updating dominators.  There are some problems with
     updating postdominators (need to add fake edges from infinite loops
     and noreturn functions), and since we do not currently use
     iterate_fix_dominators for postdominators, any attempt to handle these
     problems would be unused, untested, and almost surely buggy.  We keep
     the DIR argument for consistency with the rest of the dominator analysis
     interface.  */
  gcc_checking_assert (dir == CDI_DOMINATORS && dom_computed[dir_index]);

  /* The algorithm we use takes inspiration from the following papers, although
     the details are quite different from any of them:

     [1] G. Ramalingam, T. Reps, An Incremental Algorithm for Maintaining the
	 Dominator Tree of a Reducible Flowgraph
     [2]  V. C. Sreedhar, G. R. Gao, Y.-F. Lee: Incremental computation of
	  dominator trees
     [3]  K. D. Cooper, T. J. Harvey and K. Kennedy: A Simple, Fast Dominance
	  Algorithm

     First, we use the following heuristics to decrease the size of the BBS
     set:
       a) if BB has a single predecessor, then its immediate dominator is this
	  predecessor
       additionally, if CONSERVATIVE is true:
       b) if all the predecessors of BB except for one (X) are dominated by BB,
	  then X is the immediate dominator of BB
       c) if the nearest common ancestor of the predecessors of BB is X and
	  X -> BB is an edge in CFG, then X is the immediate dominator of BB

     Then, we need to establish the dominance relation among the basic blocks
     in BBS.  We split the dominance tree by removing the immediate dominator
     edges from BBS, creating a forest F.  We form a graph G whose vertices
     are BBS and ENTRY and X -> Y is an edge of G if there exists an edge
     X' -> Y in CFG such that X' belongs to the tree of the dominance forest
     whose root is X.  We then determine dominance tree of G.  Note that
     for X, Y in BBS, X dominates Y in CFG if and only if X dominates Y in G.
     In this step, we can use arbitrary algorithm to determine dominators.
     We decided to prefer the algorithm [3] to the algorithm of
     Lengauer and Tarjan, since the set BBS is usually small (rarely exceeding
     10 during gcc bootstrap), and [3] should perform better in this case.

     Finally, we need to determine the immediate dominators for the basic
     blocks of BBS.  If the immediate dominator of X in G is Y, then
     the immediate dominator of X in CFG belongs to the tree of F rooted in
     Y.  We process the dominator tree T of G recursively, starting from leaves.
     Suppose that X_1, X_2, ..., X_k are the sons of Y in T, and that the
     subtrees of the dominance tree of CFG rooted in X_i are already correct.
     Let G' be the subgraph of G induced by {X_1, X_2, ..., X_k}.  We make
     the following observations:
       (i) the immediate dominator of all blocks in a strongly connected
	   component of G' is the same
       (ii) if X has no predecessors in G', then the immediate dominator of X
	    is the nearest common ancestor of the predecessors of X in the
	    subtree of F rooted in Y
     Therefore, it suffices to find the topological ordering of G', and
     process the nodes X_i in this order using the rules (i) and (ii).
     Then, we contract all the nodes X_i with Y in G, so that the further
     steps work correctly.  */

  if (!conservative)
    {
      /* Split the tree now.  If the idoms of blocks in BBS are not
	 conservatively correct, setting the dominators using the
	 heuristics in prune_bbs_to_update_dominators could
	 create cycles in the dominance "tree", and cause ICE.  */
      FOR_EACH_VEC_ELT (bbs, i, bb)
	set_immediate_dominator (CDI_DOMINATORS, bb, NULL);
    }

  prune_bbs_to_update_dominators (bbs, conservative);
  n = bbs.length ();

  if (n == 0)
    return;

  if (n == 1)
    {
      bb = bbs[0];
      set_immediate_dominator (CDI_DOMINATORS, bb,
			       recompute_dominator (CDI_DOMINATORS, bb));
      return;
    }

  /* Construct the graph G.  */
  hash_map<basic_block, int> map (251);
  FOR_EACH_VEC_ELT (bbs, i, bb)
    {
      /* If the dominance tree is conservatively correct, split it now.  */
      if (conservative)
	set_immediate_dominator (CDI_DOMINATORS, bb, NULL);
      map.put (bb, i);
    }
  map.put (ENTRY_BLOCK_PTR_FOR_FN (cfun), n);

  g = new_graph (n + 1);
  for (y = 0; y < g->n_vertices; y++)
    g->vertices[y].data = BITMAP_ALLOC (NULL);
  FOR_EACH_VEC_ELT (bbs, i, bb)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  dom = root_of_dom_tree (CDI_DOMINATORS, e->src);
	  if (dom == bb)
	    continue;

	  dom_i = *map.get (dom);

	  /* Do not include parallel edges to G.  */
	  if (!bitmap_set_bit ((bitmap) g->vertices[dom_i].data, i))
	    continue;

	  add_edge (g, dom_i, i);
	}
    }
  for (y = 0; y < g->n_vertices; y++)
    BITMAP_FREE (g->vertices[y].data);

  /* Find the dominator tree of G.  */
  son = XNEWVEC (int, n + 1);
  brother = XNEWVEC (int, n + 1);
  parent = XNEWVEC (int, n + 1);
  graphds_domtree (g, n, parent, son, brother);

  /* Finally, traverse the tree and find the immediate dominators.  */
  for (y = n; son[y] != -1; y = son[y])
    continue;
  while (y != -1)
    {
      determine_dominators_for_sons (g, bbs, y, son, brother);

      if (brother[y] != -1)
	{
	  y = brother[y];
	  while (son[y] != -1)
	    y = son[y];
	}
      else
	y = parent[y];
    }

  free (son);
  free (brother);
  free (parent);

  free_graph (g);
}

void
add_to_dominance_info (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);

  gcc_checking_assert (dom_computed[dir_index] && !bb->dom[dir_index]);

  n_bbs_in_dom_tree[dir_index]++;

  bb->dom[dir_index] = et_new_tree (bb);

  if (dom_computed[dir_index] == DOM_OK)
    dom_computed[dir_index] = DOM_NO_FAST_QUERY;
}

void
delete_from_dominance_info (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);

  gcc_checking_assert (dom_computed[dir_index]);

  et_free_tree (bb->dom[dir_index]);
  bb->dom[dir_index] = NULL;
  n_bbs_in_dom_tree[dir_index]--;

  if (dom_computed[dir_index] == DOM_OK)
    dom_computed[dir_index] = DOM_NO_FAST_QUERY;
}

/* Returns the first son of BB in the dominator or postdominator tree
   as determined by DIR.  */

basic_block
first_dom_son (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *son = bb->dom[dir_index]->son;

  return (basic_block) (son ? son->data : NULL);
}

/* Returns the next dominance son after BB in the dominator or postdominator
   tree as determined by DIR, or NULL if it was the last one.  */

basic_block
next_dom_son (enum cdi_direction dir, basic_block bb)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  struct et_node *next = bb->dom[dir_index]->right;

  return (basic_block) (next->father->son == next ? NULL : next->data);
}

/* Return dominance availability for dominance info DIR.  */

enum dom_state
dom_info_state (function *fn, enum cdi_direction dir)
{
  if (!fn->cfg)
    return DOM_NONE;

  unsigned int dir_index = dom_convert_dir_to_idx (dir);
  return fn->cfg->x_dom_computed[dir_index];
}

enum dom_state
dom_info_state (enum cdi_direction dir)
{
  return dom_info_state (cfun, dir);
}

/* Set the dominance availability for dominance info DIR to NEW_STATE.  */

void
set_dom_info_availability (enum cdi_direction dir, enum dom_state new_state)
{
  unsigned int dir_index = dom_convert_dir_to_idx (dir);

  dom_computed[dir_index] = new_state;
}

/* Returns true if dominance information for direction DIR is available.  */

bool
dom_info_available_p (function *fn, enum cdi_direction dir)
{
  return dom_info_state (fn, dir) != DOM_NONE;
}

bool
dom_info_available_p (enum cdi_direction dir)
{
  return dom_info_available_p (cfun, dir);
}

DEBUG_FUNCTION void
debug_dominance_info (enum cdi_direction dir)
{
  basic_block bb, bb2;
  FOR_EACH_BB_FN (bb, cfun)
    if ((bb2 = get_immediate_dominator (dir, bb)))
      fprintf (stderr, "%i %i\n", bb->index, bb2->index);
}

/* Prints to stderr representation of the dominance tree (for direction DIR)
   rooted in ROOT, indented by INDENT tabulators.  If INDENT_FIRST is false,
   the first line of the output is not indented.  */

static void
debug_dominance_tree_1 (enum cdi_direction dir, basic_block root,
			unsigned indent, bool indent_first)
{
  basic_block son;
  unsigned i;
  bool first = true;

  if (indent_first)
    for (i = 0; i < indent; i++)
      fprintf (stderr, "\t");
  fprintf (stderr, "%d\t", root->index);

  for (son = first_dom_son (dir, root);
       son;
       son = next_dom_son (dir, son))
    {
      debug_dominance_tree_1 (dir, son, indent + 1, !first);
      first = false;
    }

  if (first)
    fprintf (stderr, "\n");
}

/* Prints to stderr representation of the dominance tree (for direction DIR)
   rooted in ROOT.  */

DEBUG_FUNCTION void
debug_dominance_tree (enum cdi_direction dir, basic_block root)
{
  debug_dominance_tree_1 (dir, root, 0, false);
}
