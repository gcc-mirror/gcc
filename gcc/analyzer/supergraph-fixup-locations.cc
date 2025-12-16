/* Fixing up location_t values of supernodes.
   Copyright (C) 2025 Free Software Foundation, Inc.
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

#define INCLUDE_DEQUE
#include "analyzer/common.h"

#include "analyzer/supergraph.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/supergraph-manipulation.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

class location_fixer
{
public:
  location_fixer (supergraph &sg,
		  ana::logger *logger)
  : m_logger (logger),
    m_stats ()
  {
    for (auto node : sg.m_nodes)
      if (node->m_loc == UNKNOWN_LOCATION)
	m_worklist.ensure_node_queued (node, logger);
  }

  /* High level ops.  */

  supernode *
  pop_next_node_in_queue ()
  {
    return m_worklist.pop ();
  }

  void
  consider_node (supernode *node)
  {
    m_stats.m_num_iterations++;

    log_nesting_level sentinel (m_logger, "considering SN: %i", node->m_id);

    /* Already have a location for this node.  */
    if (useful_location_p (node->m_loc))
      return;

    /* For snodes with UNKNOWN_LOCATION with a single in-edge, try to
       propagate the location from it.  */
    if (node->m_preds.length () == 1)
      {
	auto in_edge = node->m_preds[0];
	if (useful_location_p (in_edge->m_src->m_loc))
	  {
	    node->m_loc = in_edge->m_src->m_loc;
	    m_stats.m_num_copies++;
	    if (m_logger)
	      m_logger->log ("copying location 0x%lx from SN %i to SN %i",
			     node->m_loc,
			     in_edge->m_src->m_id, node->m_id);
	    for (auto out_edge : node->m_succs)
	      m_worklist.ensure_node_queued (out_edge->m_dest, m_logger);
	  }
      }
  }

private:
  supergraph_manipulation::worklist m_worklist;
  ana::logger *m_logger;
  struct stats
  {
    stats () = default;

    void log (ana::logger &logger)
    {
      logger.log ("# iterations taken: " HOST_SIZE_T_PRINT_UNSIGNED,
		  (fmt_size_t)m_num_iterations);
      logger.log ("# locations copied: " HOST_SIZE_T_PRINT_UNSIGNED,
		  (fmt_size_t)m_num_copies);
    }

    size_t m_num_iterations;
    size_t m_num_copies;

  } m_stats;
};

} // anonymous namespace

void
supergraph::fixup_locations (logger *logger)
{
  LOG_SCOPE (logger);

  location_fixer opt (*this, logger);
  while (supernode *node = opt.pop_next_node_in_queue ())
    opt.consider_node (node);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
