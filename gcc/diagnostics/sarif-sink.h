/* SARIF output for diagnostics.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTICS_SARIF_SINK_H
#define GCC_DIAGNOSTICS_SARIF_SINK_H

#include "json.h"
#include "diagnostics/sink.h"
#include "diagnostics/output-file.h"
#include "diagnostics/logical-locations.h"

namespace diagnostics {

namespace digraphs {
  class digraph;
  class node;
  class edge;
}

/* Enum for choosing what format to serializing the generated SARIF into.  */

enum class sarif_serialization_kind
{
   json,

   num_values
};

extern output_file
open_sarif_output_file (context &dc,
			line_maps *line_maps,
			const char *base_file_name,
			enum sarif_serialization_kind serialization_kind);

extern sink &
init_sarif_stderr (context &dc,
		   const line_maps *line_maps,
		   bool formatted);
extern sink &
init_sarif_file (context &dc,
		 line_maps *line_maps,
		 bool formatted,
		 const char *base_file_name);
extern sink &
init_sarif_stream (context &dc,
		   const line_maps *line_maps,
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
  virtual void dump (FILE *out, int indent) const = 0;
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
  void dump (FILE *out, int indent) const final override;

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

  void dump (FILE *out, int indent) const;

  enum sarif_version m_version;
  bool m_state_graph;
};

extern std::unique_ptr<sink>
make_sarif_sink (context &dc,
		 const line_maps &line_maps,
		 std::unique_ptr<sarif_serialization_format> serialization_format,
		 const sarif_generation_options &sarif_gen_opts,
		 output_file output_file_);

class sarif_builder;
class sarif_location_manager;

/* Concrete subclass of json::object for SARIF property bags
   (SARIF v2.1.0 section 3.8).  */

class sarif_property_bag : public json::object
{
public:
  void set_logical_location (const char *property_name,
			     sarif_builder &,
			     logical_locations::key logical_loc);
  void set_graph (const char *property_name,
		  sarif_builder &,
		  sarif_location_manager *sarif_location_mgr,
		  const digraphs::digraph &g);
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

/* Subclass of sarif_object for SARIF "graph" objects
   (SARIF v2.1.0 section 3.39).  */

class sarif_graph : public sarif_object
{
};

/* Subclass of sarif_object for SARIF "node" objects
   (SARIF v2.1.0 section 3.40).  */

class sarif_node : public sarif_object
{
};

/* Subclass of sarif_object for SARIF "edge" objects
   (SARIF v2.1.0 section 3.41).  */

class sarif_edge : public sarif_object
{
};

extern std::unique_ptr<sarif_graph>
make_sarif_graph (const digraphs::digraph &g,
		  sarif_builder *builder,
		  sarif_location_manager *sarif_location_mgr);

extern std::unique_ptr<sarif_node>
make_sarif_node (const digraphs::node &n,
		 sarif_builder *builder,
		 sarif_location_manager *sarif_location_mgr);

extern std::unique_ptr<sarif_edge>
make_sarif_edge (const digraphs::edge &e,
		 sarif_builder *builder);

extern void
maybe_open_sarif_sink_for_socket (context &ctxt);

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_SARIF_SINK_H */
