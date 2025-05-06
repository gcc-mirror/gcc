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
#include "logical-location.h"

/* Enum for choosing what format to serializing the generated SARIF into.  */

enum class sarif_serialization_kind
{
   json,

   num_values
};

extern diagnostic_output_file
diagnostic_output_format_open_sarif_file (diagnostic_context &context,
					  line_maps *line_maps,
					  const char *base_file_name,
					  enum sarif_serialization_kind serialization_kind);

extern void
diagnostic_output_format_init_sarif_stderr (diagnostic_context &context,
					    const line_maps *line_maps,
					    const char *main_input_filename_,
					    bool formatted);
extern void
diagnostic_output_format_init_sarif_file (diagnostic_context &context,
					  line_maps *line_maps,
					  const char *main_input_filename_,
					  bool formatted,
					  const char *base_file_name);
extern void
diagnostic_output_format_init_sarif_stream (diagnostic_context &context,
					    const line_maps *line_maps,
					    const char *main_input_filename_,
					    bool formatted,
					    FILE *stream);

/* Abstract base class for handling JSON output vs other kinds of
   serialization of the json tree.  */

class sarif_serialization_format
{
public:
  virtual ~sarif_serialization_format () {}
  virtual void write_to_file (FILE *outf,
			      const json::value &top) = 0;
};

/* Concrete subclass for serializing SARIF as JSON.  */

class sarif_serialization_format_json : public sarif_serialization_format
{
public:
  sarif_serialization_format_json (bool formatted)
  : m_formatted (formatted)
  {
  }
  void write_to_file (FILE *outf, const json::value &top) final override;

private:
  bool m_formatted;
};

/* Control of SARIF generation.  */

enum class sarif_version
{
  v2_1_0,
  v2_2_prerelease_2024_08_08,

  num_versions
};

/* A bundle of state for controlling what to put in SARIF output,
   such as which version of SARIF to generate
   (as opposed to SARIF *serialization* options, such as formatting).  */

struct sarif_generation_options
{
  sarif_generation_options ();

  enum sarif_version m_version;
};

extern std::unique_ptr<diagnostic_output_format>
make_sarif_sink (diagnostic_context &context,
		 const line_maps &line_maps,
		 const char *main_input_filename_,
		 std::unique_ptr<sarif_serialization_format> serialization_format,
		 const sarif_generation_options &sarif_gen_opts,
		 diagnostic_output_file output_file);

class sarif_builder;

/* Concrete subclass of json::object for SARIF property bags
   (SARIF v2.1.0 section 3.8).  */

class sarif_property_bag : public json::object
{
public:
  void set_logical_location (const char *property_name,
			     sarif_builder &,
			     logical_location logical_loc);
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

#endif /* ! GCC_DIAGNOSTIC_FORMAT_SARIF_H */
