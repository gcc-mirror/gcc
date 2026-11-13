/* Base class for svalues and regions.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "analyzer/symbolic-nodes.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/region-model.h"
#include "analyzer/region-model-manager.h"

#if ENABLE_ANALYZER

namespace ana {

/* Compare N1 and N2 by id.  */

int
symbolic::node::cmp_ids (const symbolic::node *n1, const symbolic::node *n2)
{
  return (long)n1->get_id () - (long)n2->get_id ();
}

// class node_manager

symbolic::node_manager::node_manager ()
: m_next_node_id (0),
  m_rmm (std::make_unique<region_model_manager> (*this))
{
}

symbolic::node_manager::~node_manager () = default;

/* Dump the various managers to LOGGER.
   If SHOW_OBJS is true, also dump the objects themselves.  */

void
symbolic::node_manager::log_stats (logger *logger, bool show_objs) const
{
  LOG_SCOPE (logger);

  m_rmm->log_stats (logger, show_objs);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
