/* Sorting the nodes in the supergraph.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#define INCLUDE_SET
#include "analyzer/common.h"

#include "cgraph.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"

#include "analyzer/supergraph.h"
#include "analyzer/analyzer-logging.h"

#if ENABLE_ANALYZER

namespace ana {

/* Update m_nodes to be ORDERING.
   Update the m_id of all nodes to reflect the new orderding.
   This assumes that all nodes are in ORDERING; does not delete
   any underlying nodes.  */

void
supergraph::reorder_nodes_and_ids (const std::vector<supernode *> &ordering,
				   logger *logger)
{
  LOG_SCOPE (logger);

  m_nodes.truncate (0);

  for (size_t new_id = 0; new_id < ordering.size (); ++new_id)
    {
      supernode *snode = ordering[new_id];
      if (logger)
	{
	  if ((size_t)snode->m_id == new_id)
	    logger->log ("SN %i is unchanged", snode->m_id);
	  else
	    logger->log ("old SN %i is now SN %li", snode->m_id, new_id);
	}
      m_nodes.safe_push (snode);
      snode->m_id = new_id;
    }

  m_next_snode_id = m_nodes.length ();
}

/* std::set::contains is C++20 onwards.  */
template <typename T>
static bool
set_contains_p (const std::set<T> &s, T v)
{
  return s.find (v) != s.end ();
}

namespace {

class sorting_worklist
{
public:
  sorting_worklist ()
  : m_queue (key_t (nullptr))
  {
  }

  void add_node (supernode *n);
  supernode *take_next (logger *logger);
  bool contains_p (supernode *n) const;

private:
  class key_t
  {
  public:
    key_t (supernode *snode)
    : m_snode (snode)
    {}

    bool operator< (const key_t &other) const
    {
      return cmp (*this, other) < 0;
    }

    bool operator== (const key_t &other) const
    {
      return cmp (*this, other) == 0;
    }

    bool operator> (const key_t &other) const
    {
      return !(*this == other || *this < other);
    }

  private:
    static int cmp (const key_t &ka, const key_t &kb);
    supernode *m_snode;
  };

  bool
  already_seen_all_predecessors_p (const supernode *n,
				   logger *logger) const;

  std::set<supernode *> m_set_of_ordered_nodes;
  std::set<supernode *> m_set_of_queued_nodes;
  typedef fibonacci_heap<key_t, supernode> queue_t;
  queue_t m_queue;
};

void
sorting_worklist::add_node (supernode *n)
{
  m_queue.insert ({n}, n);
  m_set_of_queued_nodes.insert (n);
}

supernode *
sorting_worklist::take_next (logger *logger)
{
  if (m_queue.empty ())
    return nullptr;

  std::vector<supernode *> rejected;

  /* First, try to find a node for which all predecessors
     have been ordered.  */
  while (!m_queue.empty ())
    {
      supernode *candidate = m_queue.extract_min ();

      // n shouldn't be already within the ordering
      gcc_assert (!set_contains_p (m_set_of_ordered_nodes, candidate));

      if (logger)
	logger->log ("consider SN %i from worklist", candidate->m_id);

      if (already_seen_all_predecessors_p (candidate, logger))
	{
	  if (logger)
	    logger->log ("all predecessors of SN %i seen; using it",
			 candidate->m_id);
	  for (auto r : rejected)
	    add_node (r);
	  m_set_of_ordered_nodes.insert (candidate);
	  m_set_of_queued_nodes.erase (candidate);
	  return candidate;
	}
      else
	rejected.push_back (candidate);
    }

  /* Otherwise, simply use the first node.  */
  for (auto r : rejected)
    add_node (r);
  supernode *n = m_queue.extract_min ();
  if (logger)
    logger->log ("using first in queue: SN %i", n->m_id);
  m_set_of_ordered_nodes.insert (n);
  m_set_of_queued_nodes.erase (n);
  return n;
}

bool
sorting_worklist::contains_p (supernode *n) const
{
  return (m_set_of_queued_nodes.find (n) != m_set_of_queued_nodes.end ()
	  || m_set_of_ordered_nodes.find (n) != m_set_of_ordered_nodes.end ());
}

int
sorting_worklist::key_t::cmp (const key_t &ka, const key_t &kb)
{
  const supernode *snode_a = ka.m_snode;
  const supernode *snode_b = kb.m_snode;

  /* Sort by BB.  */
  if (int bb_cmp = snode_a->m_bb->index - snode_b->m_bb->index)
    return bb_cmp;

  /* Sort by existing id.  */
  return snode_a->m_id - snode_b->m_id;
}

bool
sorting_worklist::already_seen_all_predecessors_p (const supernode *n,
						   logger *logger) const
{
  for (auto e : n->m_preds)
    if (!set_contains_p (m_set_of_ordered_nodes, e->m_src))
      {
	if (logger)
	  logger->log ("not yet ordered predecessor SN %i", e->m_src->m_id);
	return false;
      }
  return true;
}

static std::vector<supernode *>
get_node_ordering (const supergraph &sg,
		   logger *logger)
{
  LOG_SCOPE (logger);

  std::vector<supernode *> ordering_vec;

  cgraph_node *cgnode;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (cgnode)
    {
      function *fun = cgnode->get_fun ();

      sorting_worklist worklist;

      supernode *start_node = sg.get_node_for_function_entry (*fun);
      worklist.add_node (start_node);

      // Find the best node to add next in the ordering
      while (supernode *next = worklist.take_next (logger))
	{
	  gcc_assert (next);
	  if (logger)
	    logger->log ("next: SN: %i", next->m_id);

	  ordering_vec.push_back (next);

	  for (auto out_edge : next->m_succs)
	    {
	      supernode *dest_node = out_edge->m_dest;
	      if (!worklist.contains_p (dest_node))
		worklist.add_node (dest_node);
	    }
	}
    }

  return ordering_vec;
}

} // anonymous namespace

void
supergraph::sort_nodes (logger *logger)
{
  LOG_SCOPE (logger);

  const std::vector<supernode *> ordering = get_node_ordering (*this, logger);
  reorder_nodes_and_ids (ordering, logger);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
