/* Capturing changing state in diagnostic paths.
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

#ifndef GCC_DIAGNOSTIC_STATE_H
#define GCC_DIAGNOSTIC_STATE_H

/* We want to be able to express changing program states in diagnostic paths,
   so that we can emit this in HTML and SARIF output, and to keep this
   separate from implementation details of -fanalyzer.

   For now, we use xml::document as the type in the diagnostic subsystem
   for (optionally) tracking the state at a diagnostic_event.  */

namespace xml { class document; }
namespace dot { class graph; }

extern std::unique_ptr<dot::graph>
make_dot_graph_from_xml_state (const xml::document &xml_state);

#endif /* GCC_DIAGNOSTIC_STATE_H */
