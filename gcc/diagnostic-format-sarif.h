/* SARIF output for diagnostics.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTIC_FORMAT_SARIF_H
#define GCC_DIAGNOSTIC_FORMAT_SARIF_H

#include "json.h"
#include "diagnostic-format.h"
#include "diagnostic-output-file.h"

class logical_location;

enum class sarif_version
{
  v2_1_0,
  v2_2_prerelease_2024_08_08,

  num_versions
};

extern diagnostic_output_file
diagnostic_output_format_open_sarif_file (diagnostic_context &context,
					  line_maps *line_maps,
					  const char *base_file_name);

extern void
diagnostic_output_format_init_sarif_stderr (diagnostic_context &context,
					    const line_maps *line_maps,
					    const char *main_input_filename_,
					    bool formatted,
					    enum sarif_version version);
extern void
diagnostic_output_format_init_sarif_file (diagnostic_context &context,
					  line_maps *line_maps,
					  const char *main_input_filename_,
					  bool formatted,
					  enum sarif_version version,
					  const char *base_file_name);
extern void
diagnostic_output_format_init_sarif_stream (diagnostic_context &context,
					    const line_maps *line_maps,
					    const char *main_input_filename_,
					    bool formatted,
					    enum sarif_version version,
					    FILE *stream);
extern std::unique_ptr<diagnostic_output_format>
make_sarif_sink (diagnostic_context &context,
		 const line_maps &line_maps,
		 const char *main_input_filename_,
		 enum sarif_version version,
		 diagnostic_output_file output_file);

/* Concrete subclass of json::object for SARIF property bags
   (SARIF v2.1.0 section 3.8).  */

class sarif_property_bag : public json::object
{
};

/* Concrete subclass of json::object for SARIF objects that can
   contain property bags (as per SARIF v2.1.0 section 3.8.1, which has:
   "In addition to those properties that are explicitly documented, every
   object defined in this document MAY contain a property named properties
   whose value is a property bag.")  */

class sarif_object : public json::object
{
public:
  sarif_property_bag &get_or_create_properties ();
};

/* Subclass of sarif_object for SARIF "logicalLocation" objects
   (SARIF v2.1.0 section 3.33).  */

class sarif_logical_location : public sarif_object
{
};

extern std::unique_ptr<sarif_logical_location>
make_sarif_logical_location_object (const logical_location &logical_loc);

#endif /* ! GCC_DIAGNOSTIC_FORMAT_SARIF_H */
