/* Properties for capturing state graphs in SARIF property bags.
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

#include "json.h"

#ifndef GCC_DIAGNOSTICS_SARIF_PROPERTIES_STATE_GRAPHS_H
#define GCC_DIAGNOSTICS_SARIF_PROPERTIES_STATE_GRAPHS_H

/* SARIF property names relating to GCC's CFGs.  */

namespace custom_sarif_properties {
  namespace state_graphs {
    namespace graph {
      extern const char *const prefix;
    }
    namespace node {

      enum class kind_t
      {
	// Memory regions
	globals,
	code,
	function, // code within a particular function
	stack,
	stack_frame,
	heap_,
	thread_local_,

	/* Dynamically-allocated buffer,
	   on heap or stack (depending on parent).  */
	dynalloc_buffer,

	variable,

	field, // field within a struct or union
	padding, // padding bits in a struct or union
	element, // element within an array

	other // anything else
      };

      enum class dynalloc_state_t
      {
	unknown,
	nonnull,
	unchecked,
	freed
      };

      extern const json::enum_property<enum kind_t> kind_prop;

      extern const json::string_property function;
      extern const json::string_property dynamic_extents;
      extern const json::string_property name;
      extern const json::string_property type;
      /* The value of a memory region, expressed as a json::value.  */
      extern const json::json_property value;
      /* The value of a memory region, expressed as a string.  */
      extern const json::string_property value_str;

      /* For element nodes, the index within the array.  */
      extern const json::string_property index;

      /* The range of bits or bytes within the base region.  */
      extern const json::string_property bits;

      /* The size of a padding region.  */
      extern const json::string_property num_bits;

      extern const json::string_property deallocator;
      extern const json::string_property expected_deallocators;
      extern const json::enum_property<enum dynalloc_state_t>
	dynalloc_state_prop;
    }
    namespace edge {
      extern const char *const prefix;
    }
  }
}

#endif /* ! GCC_DIAGNOSTICS_SARIF_PROPERTIES_STATE_GRAPHS_H */
