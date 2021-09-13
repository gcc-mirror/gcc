/* Digraph reachability.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_ANALYZER_REACHABILITY_H
#define GCC_ANALYZER_REACHABILITY_H

namespace ana {

/* The set of nodes from which a target node in a digraph can be reached.  */
// TODO(stage1): move to gcc

template <typename GraphTraits>
class reachability
{
public:
  typedef typename GraphTraits::graph_t graph_t;
  typedef typename GraphTraits::node_t node_t;
  typedef typename GraphTraits::edge_t edge_t;

  reachability (const graph_t &graph,
		const node_t *target_node)
  : m_indices (graph.m_nodes.length ())
  {
    bitmap_clear (m_indices);
    auto_vec<const node_t *> worklist;
    worklist.safe_push (target_node);
    bitmap_set_bit (m_indices, target_node->m_index);

    while (worklist.length () > 0)
      {
	const node_t *next = worklist.pop ();
	unsigned i;
	edge_t *pred;
	FOR_EACH_VEC_ELT (next->m_preds, i, pred)
	  {
	    if (!reachable_from_p (pred->m_src))
	      {
		worklist.safe_push (pred->m_src);
		bitmap_set_bit (m_indices, pred->m_src->m_index);
	      }
	  }
      }
  }

  bool reachable_from_p (const node_t *src_node) const
  {
    return bitmap_bit_p (m_indices, src_node->m_index);
  }

private:
  /* The nodes that can reach the target.  */
  auto_sbitmap m_indices;
};

//TODO: selftests for reachability

} // namespace ana

#endif /* GCC_ANALYZER_REACHABILITY_H */
