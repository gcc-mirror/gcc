/* Classes for manipulating the supergraph.
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

#ifndef GCC_ANALYZER_SUPERGRAPH_MANIPULATION_H
#define GCC_ANALYZER_SUPERGRAPH_MANIPULATION_H

#if ENABLE_ANALYZER

namespace ana {
namespace supergraph_manipulation {

class worklist
{
public:
  worklist ()
  : m_indices ()
  {
    bitmap_clear (m_indices);
  }

  void ensure_node_queued (supernode *node, ana::logger *logger)
  {
    if (bitmap_bit_p (m_indices, node->m_id))
      return; // already in queue
    if (logger)
      logger->log ("queued SN: %i", node->m_id);
    m_queue.push_back (node);
    bitmap_set_bit (m_indices, node->m_id);
  }

  supernode *pop ()
  {
    if (m_queue.empty ())
      return nullptr;
    supernode *node = m_queue.front ();
    m_queue.pop_front ();
    bitmap_clear_bit (m_indices, node->m_id);
    return node;
  }

private:
  // The queue
  std::deque<supernode *> m_queue;

  /* Indices of all nodes in the queue, so
     we can lazily add them in constant time.  */
  auto_bitmap m_indices;
};


} // namespace ana::supergraph_manipulation
} // namespace ana

#endif /* #if ENABLE_ANALYZER */

#endif /* GCC_ANALYZER_SUPERGRAPH_MANIPULATION_H */
