/* Extra properties for CFGs in SARIF property bags.
   Copyright (C) 2025 Free Software Foundation, Inc.
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

#ifndef GCC_CUSTOM_SARIF_PROPERTIES_CFG_H
#define GCC_CUSTOM_SARIF_PROPERTIES_CFG_H

/* SARIF property names relating to GCC's CFGs.  */

namespace custom_sarif_properties {
  namespace cfg {
    namespace graph {
      extern const json::string_property pass_name;
      extern const json::integer_property pass_number;
    }

    // node kinds: "function", "loop", or "basic_block"

    // For node_kind: "loop":
    namespace loop {
      extern const json::integer_property num;
      extern const json::integer_property depth;
    }
    // For node_kind: "basic_block":
    namespace basic_block {
      extern const json::string_property kind;
      extern const json::integer_property index;
      extern const json::string_property count; // profile info
      namespace gimple {
	extern const json::array_of_string_property phis;
	extern const json::array_of_string_property stmts;
      }
      namespace rtl {
	extern const json::array_of_string_property insns;
      }
    }

    namespace node {
      extern const json::string_property kind;
    }
    namespace edge {
      extern const json::array_of_string_property flags;
      extern const json::integer_property probability_pc;
    }
  }
}

#endif /* ! GCC_CUSTOM_SARIF_PROPERTIES_CFG_H */
