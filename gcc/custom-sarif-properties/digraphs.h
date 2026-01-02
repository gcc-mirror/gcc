/* Extra properties for digraphs in SARIF property bags.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_CUSTOM_SARIF_PROPERTIES_DIGRAPHS_H
#define GCC_CUSTOM_SARIF_PROPERTIES_DIGRAPHS_H

/* SARIF property names relating to digraphs.  */

namespace custom_sarif_properties {
  namespace digraphs {
    namespace digraph {
      /* A hint about the kind of graph we have,
	 and thus what kinds of nodes and edges to expect.  */
      extern const json::string_property kind;
      // string; values: "cfg"
    }
  }
}

#endif /* ! GCC_CUSTOM_SARIF_PROPERTIES_DIGRAPHS_H */
