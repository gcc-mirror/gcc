/* Private API entrypoints to libgdiagnostics purely for use by sarif-replay.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

#ifndef LIBGDIAGNOSTICS_PRIVATE_H
#define LIBGDIAGNOSTICS_PRIVATE_H

#include "libgdiagnostics.h"

namespace json { class object; }

extern "C" {

/* Private entrypoints, for use only by sarif-replay.
   These are subject to removal without notice.  */

/* Entrypoints added in LIBGDIAGNOSTICS_ABI_3.  */

extern void
private_diagnostic_graph_set_property_bag (diagnostic_graph &graph,
					   std::unique_ptr<json::object> properties);

extern void
private_diagnostic_node_set_property_bag (diagnostic_node &node,
					  std::unique_ptr<json::object> properties);

extern void
private_diagnostic_edge_set_property_bag (diagnostic_edge &edge,
					  std::unique_ptr<json::object> properties);

/* Entrypoint added in LIBGDIAGNOSTICS_ABI_4.  */

extern diagnostic_event_id
private_diagnostic_execution_path_add_event_3 (diagnostic_execution_path *path,
					       const diagnostic_physical_location *physical_loc,
					       const diagnostic_logical_location *logical_loc,
					       unsigned stack_depth,
					       diagnostic_graph *state_graph,
					       diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (5)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (6);

/* Entrypoint added in LIBGDIAGNOSTICS_ABI_5.  */

extern void
private_diagnostic_set_nesting_level (diagnostic *diag,
				      int nesting_level)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

} // extern "C"

#endif  /* LIBGDIAGNOSTICS_PRIVATE_H  */
