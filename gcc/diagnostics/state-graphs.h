/* Extensions to diagnostics::digraphs to support state graphs.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DIAGNOSTICS_STATE_GRAPHS_H
#define GCC_DIAGNOSTICS_STATE_GRAPHS_H

#include "diagnostics/digraphs.h"

/* diagnostics::digraphs provides support for directed graphs.

   diagnostics::state_graphs provides a way to extend these graphs
   for representing "state graphs" i.e. a representation of the state
   of memory inside a program, for use e.g. by -fanalyzer.

   Specifically, nodes represent memory regions, and we use property bags
   in these nodes to stash extra properties (e.g. what kind of memory region
   a node is e.g. stack vs heap).  */

namespace dot { class graph; }

namespace diagnostics {
namespace state_graphs {

extern std::unique_ptr<dot::graph>
make_dot_graph (const diagnostics::digraphs::digraph &state_graph,
		const logical_locations::manager &logical_loc_mgr);

} // namespace state_graphs
} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_STATE_GRAPHS_H */
