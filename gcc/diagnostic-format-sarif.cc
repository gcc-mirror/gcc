/* SARIF output for diagnostics
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
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


#include "config.h"
#define INCLUDE_LIST
#define INCLUDE_MAP
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "diagnostic-metadata.h"
#include "diagnostic-path.h"
#include "diagnostic-format.h"
#include "diagnostic-buffer.h"
#include "json.h"
#include "cpplib.h"
#include "logical-location.h"
#include "diagnostic-client-data-hooks.h"
#include "diagnostic-diagram.h"
#include "text-art/canvas.h"
#include "diagnostic-format-sarif.h"
#include "ordered-hash-map.h"
#include "sbitmap.h"
#include "make-unique.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "selftest-diagnostic-show-locus.h"
#include "selftest-json.h"
#include "text-range-label.h"
#include "pretty-print-format-impl.h"
#include "pretty-print-urlifier.h"
#include "demangle.h"
#include "backtrace.h"

/* Forward decls.  */
class sarif_builder;
class content_renderer;
  class escape_nonascii_renderer;

/* Subclasses of sarif_object.
   Keep these in order of their descriptions in the specification.  */
class sarif_artifact_content; // 3.3
class sarif_artifact_location; // 3.4
class sarif_message; // 3.11
class sarif_multiformat_message_string; // 3.12
class sarif_log; // 3.13
class sarif_run; // 3.14
class sarif_tool; // 3.18
class sarif_tool_component; // 3.19
class sarif_invocation; // 3.20
class sarif_artifact; // 3.24
class sarif_location_manager; // not in the spec
class sarif_result; // 3.27
class sarif_location; // 3.28
class sarif_physical_location; // 3.29
class sarif_region; // 3.30
class sarif_logical_location; // 3.33
class sarif_location_relationship; // 3.34
class sarif_code_flow; // 3.36
class sarif_thread_flow; // 3.37
class sarif_thread_flow_location; // 3.38
class sarif_reporting_descriptor; // 3.49
class sarif_reporting_descriptor_reference; // 3.53
class sarif_tool_component_reference; // 3.54
class sarif_fix; // 3.55
class sarif_artifact_change; // 3.56
class sarif_replacement; // 3.57
class sarif_ice_notification; // 3.58

// Valid values for locationRelationship's "kinds" property (3.34.3)

enum class location_relationship_kind
{
  includes,
  is_included_by,
  relevant,

  NUM_KINDS
};

/* Declarations of subclasses of sarif_object.
   Keep these in order of their descriptions in the specification.  */

/* Subclass of sarif_object for SARIF "artifactContent" objects
   (SARIF v2.1.0 section 3.3).  */

class sarif_artifact_content : public sarif_object {};

/* Subclass of sarif_object for SARIF "artifactLocation" objects
   (SARIF v2.1.0 section 3.4).  */

class sarif_artifact_location : public sarif_object {};

/* Subclass of sarif_object for SARIF "message" objects
   (SARIF v2.1.0 section 3.11).  */

class sarif_message : public sarif_object {};

/* Subclass of sarif_object for SARIF "multiformatMessageString" objects
   (SARIF v2.1.0 section 3.12).  */

class sarif_multiformat_message_string : public sarif_object {};

/* Subclass of sarif_object for SARIF "log" objects
   (SARIF v2.1.0 section 3.13).  */

class sarif_log : public sarif_object {};

/* Subclass of sarif_object for SARIF "run" objects
   (SARIF v2.1.0 section 3.14).  */

class sarif_run : public sarif_object {};

/* Subclass of sarif_object for SARIF "tool" objects
   (SARIF v2.1.0 section 3.18).  */

class sarif_tool : public sarif_object {};

/* Subclass of sarif_object for SARIF "toolComponent" objects
   (SARIF v2.1.0 section 3.19).  */

class sarif_tool_component : public sarif_object {};

/* Make a JSON string for the current date and time.
   See SARIF v2.1.0 section 3.9 "Date/time properties".
   Given that we don't run at the very beginning/end of the
   process, it doesn't make sense to be more accurate than
   the current second.  */

static std::unique_ptr<json::string>
make_date_time_string_for_current_time ()
{
  time_t t = time (nullptr);
  struct tm *tm = gmtime (&t);
  char buf[256];
  snprintf (buf, sizeof (buf) - 1,
	    ("%04i-%02i-%02iT"
	     "%02i:%02i:%02iZ"),
	    tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday,
	    tm->tm_hour, tm->tm_min, tm->tm_sec);
  return ::make_unique<json::string> (buf);
}

/* Subclass of sarif_object for SARIF "invocation" objects
   (SARIF v2.1.0 section 3.20).  */

class sarif_invocation : public sarif_object
{
public:
  sarif_invocation (sarif_builder &builder,
		    const char * const *original_argv);

  void add_notification_for_ice (const diagnostic_info &diagnostic,
				 sarif_builder &builder,
				 std::unique_ptr<json::object> backtrace);
  void prepare_to_flush (sarif_builder &builder);

private:
  std::unique_ptr<json::array> m_notifications_arr;
  bool m_success;
};

/* Corresponds to values for the SARIF artifact objects "roles" property.
   (SARIF v2.1.0 section 3.24.6).  */

enum class diagnostic_artifact_role
{
  analysis_target, /* "analysisTarget".  */
  debug_output_file, /* "debugOutputFile".  */
  result_file, /* "resultFile".  */

  /* "scannedFile" added in 2.2;
     see https://github.com/oasis-tcs/sarif-spec/issues/459 */
  scanned_file,

  traced_file, /* "tracedFile".  */

  NUM_ROLES
};

/* Subclass of sarif_object for SARIF artifact objects
   (SARIF v2.1.0 section 3.24).  */

class sarif_artifact : public sarif_object
{
public:
  sarif_artifact (const char *filename)
  : m_filename (filename),
    m_roles ((unsigned)diagnostic_artifact_role::NUM_ROLES),
    m_embed_contents (false)
  {
    bitmap_clear (m_roles);
  }

  void add_role (enum diagnostic_artifact_role role,
		 bool embed_contents);

  bool embed_contents_p () const { return m_embed_contents; }
  void populate_contents (sarif_builder &builder);
  void populate_roles ();

private:
  const char *m_filename;
  auto_sbitmap m_roles;

  /* Flag to track whether this artifact should have a "contents" property
     (SARIF v2.1.0 section 3.24.8).
     We only add the contents for those artifacts that have a location
     referencing them (so that a consumer might want to quote the source).   */
  bool m_embed_contents;
};

/* A class for sarif_objects that own a "namespace" of numeric IDs for
   managing location objects within them.  Currently (SARIF v2.1.0)
   this is just for sarif_result (section 3.28.2), but it will likely
   eventually also be for notification objects; see
   https://github.com/oasis-tcs/sarif-spec/issues/540

   Consider locations with chains of include information e.g.

   > include-chain-1.c:
   >   #include "include-chain-1.h"

   include-chain-1.h:
     | // First set of decls, which will be referenced in notes
     | #include "include-chain-1-1.h"
     |
     | // Second set of decls, which will trigger the errors
     | #include "include-chain-1-2.h"

   include-chain-1-1.h:
     | int p;
     | int q;

   include-chain-1-1.h:
     | char p;
     | char q;

   GCC's textual output emits:
     |   In file included from PATH/include-chain-1.h:5,
     |                    from PATH/include-chain-1.c:30:
     |   PATH/include-chain-1-2.h:1:6: error: conflicting types for 'p'; have 'char'
     |       1 | char p;
     |         |      ^
     |   In file included from PATH/include-chain-1.h:2:
     |   PATH/include-chain-1-1.h:1:5: note: previous declaration of 'p' with type 'int'
     |       1 | int p;
     |         |     ^
     |   PATH/include-chain-1-2.h:2:6: error: conflicting types for 'q'; have 'char'
     |       2 | char q;
     |         |      ^
     |   PATH/include-chain-1-1.h:2:5: note: previous declaration of 'q' with type 'int'
     |       2 | int q;
     |         |     ^

   Whenever a SARIF location is added for a location_t that
   was #included from somewhere, we queue up the creation of a SARIF
   location for the location of the #include.  The worklist of queued
   locations is flushed when the result is finished, which lazily creates
   any additional related locations for the include chain, and the
   relationships between the locations.  Doing so can lead to further
   include locations being processed.  The worklist approach allows us
   to lazily explore the relevant part of the directed graph of location_t
   values implicit in our line_maps structure, replicating it as a directed
   graph of SARIF locations within the SARIF result object, like this:

   [0]: error in include-chain-1-2.h ("conflicting types for 'p'; have 'char'")
   [1]: #include "include-chain-1-2.h" in include-chain-1.h
   [2]: note in include-chain-1-2.h ("previous declaration of 'p' with type 'int'")
   [3]: #include "include-chain-1-1.h" in include-chain-1.h
   [4]: #include "include-chain-1.h" in include-chain-1.c

   where we want to capture this "includes" graph in SARIF form:
   . +-----------------------------------+ +----------------------------------+
   . |"id": 0                            | |"id": 2                           |
   . | error: "conflicting types for 'p';| | note: previous declaration of 'p'|
   . |  have 'char'"|                    | | with type 'int'")                |
   . | in include-chain-1-2.h            | | in include-chain-1-1.h           |
   . +-----------------------------------+ +----------------------------------+
   .            ^         |                            ^         |
   .   includes |         | included-by       includes |         | included-by
   .            |         V                            |         V
   .  +--------------------------------+    +--------------------------------+
   .  |"id": 1                         |    |"id": 3                         |
   .  | #include "include-chain-1-2.h" |    | #include "include-chain-1-1.h" |
   .  | in include-chain-1.h           |    | in include-chain-1.h           |
   .  +--------------------------------+    +--------------------------------+
   .                   ^     |                       ^    |
   .          includes |     | included-by  includes |    | included-by
   .                   |     V                       |    V
   .                  +------------------------------------+
   .                  |"id": 4                             |
   .                  | The  #include "include-chain-1.h"  |
   .                  | in include-chain-1.c               |
   .                  +------------------------------------+
 */

class sarif_location_manager : public sarif_object
{
public:
  /* A worklist of pending actions needed to fully process this object.

     This lets us lazily walk our data structures to build the
     directed graph of locations, whilst keeping "notes" at the top
     of the "relatedLocations" array, and avoiding the need for
     recursion.  */
  struct worklist_item
  {
    enum class kind
    {
     /* Process a #include relationship where m_location_obj
	was #included-d at m_where.  */
     included_from,

     /* Process a location_t that was added as a secondary location
	to a rich_location without a label.  */
     unlabelled_secondary_location
    };

    worklist_item (sarif_location &location_obj,
		   enum kind kind,
		   location_t where)
      : m_location_obj (location_obj),
	m_kind (kind),
	m_where (where)
    {
    }

    sarif_location &m_location_obj;
    enum kind m_kind;
    location_t m_where;
  };

  sarif_location_manager ()
  : m_related_locations_arr (nullptr),
    m_next_location_id (0)
  {
  }

  unsigned allocate_location_id ()
  {
    return m_next_location_id++;
  }

  virtual void
  add_related_location (std::unique_ptr<sarif_location> location_obj,
			sarif_builder &builder);

  void
  add_relationship_to_worklist (sarif_location &location_obj,
				enum worklist_item::kind kind,
				location_t where);

  void
  process_worklist (sarif_builder &builder);

  void
  process_worklist_item (sarif_builder &builder,
			 const worklist_item &item);
private:
  json::array *m_related_locations_arr; // borrowed
  unsigned m_next_location_id;

  std::list<worklist_item> m_worklist;
  std::map<location_t, sarif_location *> m_included_from_locations;
  std::map<location_t, sarif_location *> m_unlabelled_secondary_locations;
};

/* Subclass of sarif_object for SARIF "result" objects
   (SARIF v2.1.0 section 3.27).
   Each SARIF result object has its own "namespace" of numeric IDs for
   managing location objects (SARIF v2.1.0 section 3.28.2). */

class sarif_result : public sarif_location_manager
{
public:
  sarif_result (unsigned idx_within_parent)
  : m_idx_within_parent (idx_within_parent)
  {}

  unsigned get_index_within_parent () const { return m_idx_within_parent; }

  void
  on_nested_diagnostic (const diagnostic_info &diagnostic,
			diagnostic_t orig_diag_kind,
			sarif_builder &builder);
  void on_diagram (const diagnostic_diagram &diagram,
		   sarif_builder &builder);

private:
  const unsigned m_idx_within_parent;
};

/* Subclass of sarif_object for SARIF "location" objects
   (SARIF v2.1.0 section 3.28).
   A location object can have an "id" which must be unique within
   the enclosing result, if any (see SARIF v2.1.0 section 3.28.2).  */

class sarif_location : public sarif_object
{
public:
  long lazily_add_id (sarif_location_manager &loc_mgr);
  long get_id () const;

  void lazily_add_relationship (sarif_location &target,
				enum location_relationship_kind kind,
				sarif_location_manager &loc_mgr);

private:
  sarif_location_relationship &
  lazily_add_relationship_object (sarif_location &target,
				  sarif_location_manager &loc_mgr);

  json::array &lazily_add_relationships_array ();

  std::map<sarif_location *,
	   sarif_location_relationship *> m_relationships_map;
};

/* Subclass of sarif_object for SARIF "physicalLocation" objects
   (SARIF v2.1.0 section 3.29).  */

class sarif_physical_location : public sarif_object {};

/* Subclass of sarif_object for SARIF "region" objects
   (SARIF v2.1.0 section 3.30).  */

class sarif_region : public sarif_object {};

/* Subclass of sarif_object for SARIF "locationRelationship" objects
   (SARIF v2.1.0 section 3.34).  */

class sarif_location_relationship : public sarif_object
{
public:
  sarif_location_relationship (sarif_location &target,
			       sarif_location_manager &loc_mgr);

  long get_target_id () const;

  void lazily_add_kind (enum location_relationship_kind kind);

private:
  auto_sbitmap m_kinds;
};

/* Subclass of sarif_object for SARIF "codeFlow" objects
   (SARIF v2.1.0 section 3.36).  */

class sarif_code_flow : public sarif_object
{
public:
  sarif_code_flow (sarif_result &parent,
		   unsigned idx_within_parent);

  sarif_result &get_parent () const { return m_parent; }
  unsigned get_index_within_parent () const { return m_idx_within_parent; }

  sarif_thread_flow &
  get_or_append_thread_flow (const diagnostic_thread &thread,
			     diagnostic_thread_id_t thread_id);

  sarif_thread_flow &
  get_thread_flow (diagnostic_thread_id_t thread_id);

  void add_location (sarif_thread_flow_location &);

  sarif_thread_flow_location &
  get_thread_flow_loc_obj (diagnostic_event_id_t event_id) const;

private:
  sarif_result &m_parent;
  const unsigned m_idx_within_parent;

  hash_map<int_hash<diagnostic_thread_id_t, -1, -2>,
	   sarif_thread_flow *> m_thread_id_map; // borrowed ptr
  json::array *m_thread_flows_arr; // borrowed

  /* Vec of borrowed ptr, allowing for going easily from
     an event_id to the corresponding threadFlowLocation object.  */
  std::vector<sarif_thread_flow_location *> m_all_tfl_objs;
};

/* Subclass of sarif_object for SARIF "threadFlow" objects
   (SARIF v2.1.0 section 3.37).  */

class sarif_thread_flow : public sarif_object
{
public:
  sarif_thread_flow (sarif_code_flow &parent,
		     const diagnostic_thread &thread,
		     unsigned idx_within_parent);

  sarif_code_flow &get_parent () const { return m_parent; }
  unsigned get_index_within_parent () const { return m_idx_within_parent; }

  sarif_thread_flow_location &add_location ();

private:
  sarif_code_flow &m_parent;
  json::array *m_locations_arr; // borrowed
  const unsigned m_idx_within_parent;
};

/* Subclass of sarif_object for SARIF "threadFlowLocation" objects
   (SARIF v2.1.0 section 3.38).  */

class sarif_thread_flow_location : public sarif_object
{
public:
  sarif_thread_flow_location (sarif_thread_flow &parent,
			      unsigned idx_within_parent)
  : m_parent (parent),
    m_idx_within_parent (idx_within_parent)
  {
  }

  sarif_thread_flow &get_parent () const { return m_parent; }
  unsigned get_index_within_parent () const { return m_idx_within_parent; }

private:
  sarif_thread_flow &m_parent;
  const unsigned m_idx_within_parent;
};

/* Subclass of sarif_object for SARIF "reportingDescriptor" objects
   (SARIF v2.1.0 section 3.49).  */

class sarif_reporting_descriptor : public sarif_object {};

/* Subclass of sarif_object for SARIF "reportingDescriptorReference" objects
   (SARIF v2.1.0 section 3.53).  */

class sarif_reporting_descriptor_reference : public sarif_object {};

/* Subclass of sarif_object for SARIF "toolComponentReference" objects
   (SARIF v2.1.0 section 3.54).  */

class sarif_tool_component_reference : public sarif_object {};

/* Subclass of sarif_object for SARIF "fix" objects
   (SARIF v2.1.0 section 3.55).  */

class sarif_fix : public sarif_object {};

/* Subclass of sarif_object for SARIF "artifactChange" objects
   (SARIF v2.1.0 section 3.56).  */

class sarif_artifact_change : public sarif_object {};

/* Subclass of sarif_object for SARIF "replacement" objects
   (SARIF v2.1.0 section 3.57).  */

class sarif_replacement : public sarif_object {};

/* Subclass of sarif_object for SARIF "notification" objects
   (SARIF v2.1.0 section 3.58).

   This subclass is specifically for notifying when an
   internal compiler error occurs.  */

class sarif_ice_notification : public sarif_location_manager
{
public:
  sarif_ice_notification (const diagnostic_info &diagnostic,
			  sarif_builder &builder,
			  std::unique_ptr<json::object> backtrace);

  void
  add_related_location (std::unique_ptr<sarif_location> location_obj,
			sarif_builder &builder) final override;
};

/* Abstract base class for use when making an  "artifactContent"
   object (SARIF v2.1.0 section 3.3): generate a value for the
   3.3.4 "rendered" property.
   Can return nullptr, for "no property".  */

class content_renderer
{
public:
  virtual ~content_renderer () {}

  virtual std::unique_ptr<sarif_multiformat_message_string>
  render (const sarif_builder &builder) const = 0;
};

/* Concrete buffering implementation subclass for JSON output.  */

class diagnostic_sarif_format_buffer : public diagnostic_per_format_buffer
{
public:
  friend class sarif_output_format;

  diagnostic_sarif_format_buffer (sarif_builder &builder)
  : m_builder (builder)
  {}

  void dump (FILE *out, int indent) const final override;
  bool empty_p () const final override;
  void move_to (diagnostic_per_format_buffer &dest) final override;
  void clear () final override;
  void flush () final override;

  void add_result (std::unique_ptr<sarif_result> result)
  {
    m_results.push_back (std::move (result));
  }

  size_t num_results () const { return m_results.size (); }
  sarif_result &get_result (size_t idx) { return *m_results[idx]; }

private:
  sarif_builder &m_builder;
  std::vector<std::unique_ptr<sarif_result>> m_results;
};

/* A class for managing SARIF output (for -fdiagnostics-format=sarif-stderr
   and -fdiagnostics-format=sarif-file).

   As diagnostics occur, we build "result" JSON objects, and
   accumulate state:
   - which source files are referenced
   - which warnings are emitted
   - which CWEs are used

   At the end of the compile, we use the above to build the full SARIF
   object tree, adding the result objects to the correct place, and
   creating objects for the various source files, warnings and CWEs
   referenced.

   Implemented:
   - fix-it hints
   - CWE metadata
   - diagnostic groups (see limitations below)
   - logical locations (e.g. cfun)
   - labelled ranges (as annotations)
   - secondary ranges without labels (as related locations)

   Known limitations:
   - GCC supports one-deep nesting of diagnostics (via auto_diagnostic_group),
     but we only capture location and message information from such nested
     diagnostics (e.g. we ignore fix-it hints on them)
   - although we capture command-line arguments (section 3.20.2), we don't
     yet capture response files.
   - doesn't capture "artifact.encoding" property
     (SARIF v2.1.0 section 3.24.9).
   - doesn't capture hashes of the source files
     ("artifact.hashes" property (SARIF v2.1.0 section 3.24.11).
   - doesn't capture the "analysisTarget" property
     (SARIF v2.1.0 section 3.27.13).
   - doesn't capture -Werror cleanly
   - doesn't capture inlining information (can SARIF handle this?)
   - doesn't capture macro expansion information (can SARIF handle this?).
   - doesn't capture any diagnostic_metadata::rules associated with
     a diagnostic.  */

class sarif_builder
{
public:
  friend class diagnostic_sarif_format_buffer;

  sarif_builder (diagnostic_context &context,
		 const line_maps *line_maps,
		 const char *main_input_filename_,
		 bool formatted,
		 enum sarif_version version);
  ~sarif_builder ();

  void on_report_diagnostic (const diagnostic_info &diagnostic,
			     diagnostic_t orig_diag_kind,
			     diagnostic_sarif_format_buffer *buffer);
  void emit_diagram (const diagnostic_diagram &diagram);
  void end_group ();

  std::unique_ptr<sarif_result> take_current_result ()
  {
    return std::move (m_cur_group_result);
  }

  std::unique_ptr<sarif_log> flush_to_object ();
  void flush_to_file (FILE *outf);

  std::unique_ptr<json::array>
  make_locations_arr (sarif_location_manager &loc_mgr,
		      const diagnostic_info &diagnostic,
		      enum diagnostic_artifact_role role);
  std::unique_ptr<sarif_location>
  make_location_object (sarif_location_manager &loc_mgr,
			const rich_location &rich_loc,
			const logical_location *logical_loc,
			enum diagnostic_artifact_role role);
  std::unique_ptr<sarif_location>
  make_location_object (sarif_location_manager &loc_mgr,
			location_t where,
			enum diagnostic_artifact_role role);
  std::unique_ptr<sarif_message>
  make_message_object (const char *msg) const;
  std::unique_ptr<sarif_message>
  make_message_object_for_diagram (const diagnostic_diagram &diagram);
  std::unique_ptr<sarif_artifact_content>
  maybe_make_artifact_content_object (const char *filename) const;

  std::unique_ptr<sarif_artifact_location>
  make_artifact_location_object (const char *filename);

  const sarif_code_flow *
  get_code_flow_for_event_ids () const
  {
    return m_current_code_flow;
  }

  diagnostic_context &get_context () const { return m_context; }
  pretty_printer *get_printer () const { return m_printer; }
  token_printer &get_token_printer () { return m_token_printer; }
  enum sarif_version get_version () const { return m_version; }

  size_t num_results () const { return m_results_array->size (); }
  sarif_result &get_result (size_t idx)
  {
    auto element = (*m_results_array)[idx];
    gcc_assert (element);
    return *static_cast<sarif_result *> (element);
  }

private:
  class sarif_token_printer : public token_printer
  {
  public:
    sarif_token_printer (sarif_builder &builder)
      : m_builder (builder)
    {
    }
    void print_tokens (pretty_printer *pp,
		       const pp_token_list &tokens) final override;
  private:
    sarif_builder &m_builder;
  };

  std::unique_ptr<sarif_result>
  make_result_object (const diagnostic_info &diagnostic,
		      diagnostic_t orig_diag_kind,
		      unsigned idx_within_parent);
  void
  add_any_include_chain (sarif_location_manager &loc_mgr,
			 sarif_location &location_obj,
			 location_t where);
  void
  set_any_logical_locs_arr (sarif_location &location_obj,
			    const logical_location *logical_loc);
  std::unique_ptr<sarif_location>
  make_location_object (sarif_location_manager &loc_mgr,
			const diagnostic_event &event,
			enum diagnostic_artifact_role role);
  std::unique_ptr<sarif_code_flow>
  make_code_flow_object (sarif_result &result,
			 unsigned idx_within_parent,
			 const diagnostic_path &path);
  void
  populate_thread_flow_location_object (sarif_result &result,
					sarif_thread_flow_location &thread_flow_loc_obj,
					const diagnostic_event &event,
					int event_execution_idx);
  std::unique_ptr<json::array>
  maybe_make_kinds_array (diagnostic_event::meaning m) const;
  std::unique_ptr<sarif_physical_location>
  maybe_make_physical_location_object (location_t loc,
				       enum diagnostic_artifact_role role,
				       int column_override,
				       const content_renderer *snippet_renderer);
  std::unique_ptr<sarif_artifact_location>
  make_artifact_location_object (location_t loc);
  std::unique_ptr<sarif_artifact_location>
  make_artifact_location_object_for_pwd () const;
  std::unique_ptr<sarif_region>
  maybe_make_region_object (location_t loc,
			    int column_override) const;
  std::unique_ptr<sarif_region>
  maybe_make_region_object_for_context (location_t loc,
					const content_renderer *snippet_renderer) const;
  std::unique_ptr<sarif_region>
  make_region_object_for_hint (const fixit_hint &hint) const;
  std::unique_ptr<sarif_multiformat_message_string>
  make_multiformat_message_string (const char *msg) const;
  std::unique_ptr<sarif_log>
  make_top_level_object (std::unique_ptr<sarif_invocation> invocation_obj,
			 std::unique_ptr<json::array> results);
  std::unique_ptr<sarif_run>
  make_run_object (std::unique_ptr<sarif_invocation> invocation_obj,
		   std::unique_ptr<json::array> results);
  std::unique_ptr<sarif_tool>
  make_tool_object ();
  std::unique_ptr<sarif_tool_component>
  make_driver_tool_component_object ();
  std::unique_ptr<json::array> maybe_make_taxonomies_array () const;
  std::unique_ptr<sarif_tool_component>
  maybe_make_cwe_taxonomy_object () const;
  std::unique_ptr<sarif_tool_component_reference>
  make_tool_component_reference_object_for_cwe () const;
  std::unique_ptr<sarif_reporting_descriptor>
  make_reporting_descriptor_object_for_warning (const diagnostic_info &diagnostic,
						diagnostic_t orig_diag_kind,
						const char *option_text);
  std::unique_ptr<sarif_reporting_descriptor>
  make_reporting_descriptor_object_for_cwe_id (int cwe_id) const;
  std::unique_ptr<sarif_reporting_descriptor_reference>
  make_reporting_descriptor_reference_object_for_cwe_id (int cwe_id);
  sarif_artifact &
  get_or_create_artifact (const char *filename,
			  enum diagnostic_artifact_role role,
			  bool embed_contents);
  char *
  get_source_lines (const char *filename,
		    int start_line,
		    int end_line) const;
  std::unique_ptr<sarif_artifact_content>
  maybe_make_artifact_content_object (const char *filename,
				      int start_line,
				      int end_line,
				      const content_renderer *r) const;
  std::unique_ptr<sarif_fix>
  make_fix_object (const rich_location &rich_loc);
  std::unique_ptr<sarif_artifact_change>
  make_artifact_change_object (const rich_location &richloc);
  std::unique_ptr<sarif_replacement>
  make_replacement_object (const fixit_hint &hint) const;
  std::unique_ptr<sarif_artifact_content>
  make_artifact_content_object (const char *text) const;
  int get_sarif_column (expanded_location exploc) const;

  std::unique_ptr<json::object>
  make_stack_from_backtrace ();

  diagnostic_context &m_context;
  pretty_printer *m_printer;
  const line_maps *m_line_maps;
  sarif_token_printer m_token_printer;
  enum sarif_version m_version;

  /* The JSON object for the invocation object.  */
  std::unique_ptr<sarif_invocation> m_invocation_obj;

  /* The JSON array of pending diagnostics.  */
  std::unique_ptr<json::array> m_results_array;

  /* The JSON object for the result object (if any) in the current
     diagnostic group.  */
  std::unique_ptr<sarif_result> m_cur_group_result;

  /* Ideally we'd use std::unique_ptr<sarif_artifact> here, but I had
     trouble getting this to work when building with GCC 4.8.  */
  ordered_hash_map <nofree_string_hash,
		    sarif_artifact *> m_filename_to_artifact_map;

  bool m_seen_any_relative_paths;
  hash_set <free_string_hash> m_rule_id_set;
  std::unique_ptr<json::array> m_rules_arr;

  /* The set of all CWE IDs we've seen, if any.  */
  hash_set <int_hash <int, 0, 1> > m_cwe_id_set;

  int m_tabstop;

  bool m_formatted;

  unsigned m_next_result_idx;
  sarif_code_flow *m_current_code_flow;
};

/* class sarif_object : public json::object.  */

sarif_property_bag &
sarif_object::get_or_create_properties ()
{
  json::value *properties_val = get ("properties");
  if (properties_val)
    {
      if (properties_val->get_kind () == json::JSON_OBJECT)
	return *static_cast <sarif_property_bag *> (properties_val);
    }

  sarif_property_bag *bag = new sarif_property_bag ();
  set ("properties", bag);
  return *bag;
}

/* class sarif_invocation : public sarif_object.  */

sarif_invocation::sarif_invocation (sarif_builder &builder,
				    const char * const *original_argv)
: m_notifications_arr (::make_unique<json::array> ()),
  m_success (true)
{
  // "arguments" property (SARIF v2.1.0 section 3.20.2)
  if (original_argv)
    {
      auto arguments_arr = ::make_unique<json::array> ();
      for (size_t i = 0; original_argv[i]; ++i)
	arguments_arr->append_string (original_argv[i]);
      set<json::array> ("arguments", std::move (arguments_arr));
    }

  // "workingDirectory" property (SARIF v2.1.0 section 3.20.19)
  if (const char *pwd = getpwd ())
    set<sarif_artifact_location> ("workingDirectory",
				  builder.make_artifact_location_object (pwd));

  // "startTimeUtc" property (SARIF v2.1.0 section 3.20.7)
  set<json::string> ("startTimeUtc",
		     make_date_time_string_for_current_time ());
}

/* Handle an internal compiler error DIAGNOSTIC.
   Add an object representing the ICE to the notifications array.  */

void
sarif_invocation::add_notification_for_ice (const diagnostic_info &diagnostic,
					    sarif_builder &builder,
					    std::unique_ptr<json::object> backtrace)
{
  m_success = false;

  auto notification
    = ::make_unique<sarif_ice_notification> (diagnostic,
					     builder,
					     std::move (backtrace));

  /* Support for related locations within a notification was added
     in SARIF 2.2; see https://github.com/oasis-tcs/sarif-spec/issues/540  */
  if (builder.get_version () >= sarif_version::v2_2_prerelease_2024_08_08)
    notification->process_worklist (builder);

  m_notifications_arr->append<sarif_ice_notification>
    (std::move (notification));
}

void
sarif_invocation::prepare_to_flush (sarif_builder &builder)
{
  const diagnostic_context &context = builder.get_context ();

  /* "executionSuccessful" property (SARIF v2.1.0 section 3.20.14).  */
  if (context.execution_failed_p ())
    m_success = false;
  set_bool ("executionSuccessful", m_success);

  /* "toolExecutionNotifications" property (SARIF v2.1.0 section 3.20.21).  */
  set ("toolExecutionNotifications", std::move (m_notifications_arr));

  /* Call client hook, allowing it to create a custom property bag for
     this object (SARIF v2.1.0 section 3.8) e.g. for recording time vars.  */
  if (auto client_data_hooks = context.get_client_data_hooks ())
    client_data_hooks->add_sarif_invocation_properties (*this);

  // "endTimeUtc" property (SARIF v2.1.0 section 3.20.8);
  set<json::string> ("endTimeUtc",
		     make_date_time_string_for_current_time ());
}

/* class sarif_artifact : public sarif_object.  */

/* Add ROLE to this artifact's roles.
   If EMBED_CONTENTS is true, then flag that we will attempt to embed the
   contents of this artifact when writing it out.  */

void
sarif_artifact::add_role (enum diagnostic_artifact_role role,
			  bool embed_contents)
{
  /* TODO(SARIF 2.2): "scannedFile" is to be added as a role in SARIF 2.2;
     see https://github.com/oasis-tcs/sarif-spec/issues/459

     For now, skip them.
     Ultimately, we probably shouldn't bother embedding the contents
     of such artifacts, just the snippets.  */
  if (role == diagnostic_artifact_role::scanned_file)
    return;

  if (embed_contents)
    m_embed_contents = true;

  /* In SARIF v2.1.0 section 3.24.6 "roles" property:
     "resultFile" is for an artifact
     "which the analysis tool was not explicitly instructed to scan",
     whereas "analysisTarget" is for one where the
     "analysis tool was instructed to scan this artifact".
     Hence the latter excludes the former.  */
  if (role == diagnostic_artifact_role::result_file)
    if (bitmap_bit_p (m_roles, (int)diagnostic_artifact_role::analysis_target))
	return;

  bitmap_set_bit (m_roles, (int)role);
}

/* Populate the "contents" property (SARIF v2.1.0 section 3.24.8).
   We do this after initialization to
   (a) ensure that any charset options have been set
   (b) only populate it for artifacts that are referenced by a location.  */

void
sarif_artifact::populate_contents (sarif_builder &builder)
{
  if (auto artifact_content_obj
	= builder.maybe_make_artifact_content_object (m_filename))
    set<sarif_artifact_content> ("contents", std::move (artifact_content_obj));
}

/* Get a string for ROLE corresponding to the
   SARIF v2.1.0 section 3.24.6 "roles" property.  */

static const char *
get_artifact_role_string (enum diagnostic_artifact_role role)
{
  switch (role)
    {
    default:
      gcc_unreachable ();
    case diagnostic_artifact_role::analysis_target:
      return "analysisTarget";
    case diagnostic_artifact_role::debug_output_file:
      return "debugOutputFile";
    case diagnostic_artifact_role::result_file:
      return "resultFile";
    case diagnostic_artifact_role::scanned_file:
      return "scannedFile";
    case diagnostic_artifact_role::traced_file:
      return "tracedFile";
    }
}

/* Populate the "roles" property of this sarif_artifact with a new
   json::array for the artifact.roles property (SARIF v2.1.0 section 3.24.6)
   containing strings such as "analysisTarget", "resultFile"
   and/or "tracedFile".  */

void
sarif_artifact::populate_roles ()
{
  if (bitmap_empty_p (m_roles))
    return;
  auto roles_arr (::make_unique<json::array> ());
  for (int i = 0; i < (int)diagnostic_artifact_role::NUM_ROLES; i++)
    if (bitmap_bit_p (m_roles, i))
      {
	enum diagnostic_artifact_role role = (enum diagnostic_artifact_role)i;
	roles_arr->append_string (get_artifact_role_string (role));
      }
  set<json::array> ("roles", std::move (roles_arr));
}

/* class sarif_location_manager : public sarif_object.  */

/* Base implementation of sarif_location_manager::add_related_location vfunc.

   Add LOCATION_OBJ to this object's "relatedLocations" array,
   creating it if it doesn't yet exist.  */

void
sarif_location_manager::
add_related_location (std::unique_ptr<sarif_location> location_obj,
		      sarif_builder &)
{
  if (!m_related_locations_arr)
    {
      m_related_locations_arr = new json::array ();
      /* Give ownership of m_related_locations_arr to json::object;
	 keep a borrowed ptr.  */
      set ("relatedLocations", m_related_locations_arr);
    }
  m_related_locations_arr->append (std::move (location_obj));
}

void
sarif_location_manager::
add_relationship_to_worklist (sarif_location &location_obj,
			      enum worklist_item::kind kind,
			      location_t where)
{
  m_worklist.push_back (worklist_item (location_obj,
				       kind,
				       where));
}

/* Process all items in this result's worklist.
   Doing so may temporarily add new items to the end
   of the worklist.
   Handling any item should be "lazy", and thus we should
   eventually drain the queue and terminate.  */

void
sarif_location_manager::process_worklist (sarif_builder &builder)
{
  while (!m_worklist.empty ())
    {
      const worklist_item &item = m_worklist.front ();
      process_worklist_item (builder, item);
      m_worklist.pop_front ();
    }
}

/* Process one item in this result's worklist, potentially
   adding new items to the end of the worklist.  */

void
sarif_location_manager::process_worklist_item (sarif_builder &builder,
					       const worklist_item &item)
{
  switch (item.m_kind)
    {
    default:
      gcc_unreachable ();
    case worklist_item::kind::included_from:
      {
	sarif_location &included_loc_obj = item.m_location_obj;
	sarif_location *includer_loc_obj = nullptr;
	auto iter = m_included_from_locations.find (item.m_where);
	if (iter != m_included_from_locations.end ())
	  includer_loc_obj = iter->second;
	else
	  {
	    std::unique_ptr<sarif_location> new_loc_obj
	      = builder.make_location_object
		  (*this,
		   item.m_where,
		   diagnostic_artifact_role::scanned_file);
	    includer_loc_obj = new_loc_obj.get ();
	    add_related_location (std::move (new_loc_obj), builder);
	    auto kv
	      = std::pair<location_t, sarif_location *> (item.m_where,
							 includer_loc_obj);
	    m_included_from_locations.insert (kv);
	  }

	includer_loc_obj->lazily_add_relationship
	  (included_loc_obj,
	   location_relationship_kind::includes,
	   *this);
	included_loc_obj.lazily_add_relationship
	  (*includer_loc_obj,
	   location_relationship_kind::is_included_by,
	   *this);
      }
      break;
    case worklist_item::kind::unlabelled_secondary_location:
      {
	sarif_location &primary_loc_obj = item.m_location_obj;
	sarif_location *secondary_loc_obj = nullptr;
	auto iter = m_unlabelled_secondary_locations.find (item.m_where);
	if (iter != m_unlabelled_secondary_locations.end ())
	  secondary_loc_obj = iter->second;
	else
	  {
	    std::unique_ptr<sarif_location> new_loc_obj
	      = builder.make_location_object
		  (*this,
		   item.m_where,
		   diagnostic_artifact_role::scanned_file);
	    secondary_loc_obj = new_loc_obj.get ();
	    add_related_location (std::move (new_loc_obj), builder);
	    auto kv
	      = std::pair<location_t, sarif_location *> (item.m_where,
							 secondary_loc_obj);
	    m_unlabelled_secondary_locations.insert (kv);
	  }
	gcc_assert (secondary_loc_obj);
	primary_loc_obj.lazily_add_relationship
	  (*secondary_loc_obj,
	   location_relationship_kind::relevant,
	   *this);
      }
      break;
    }
}

/* class sarif_result : public sarif_location_manager.  */

/* Handle secondary diagnostics that occur within a diagnostic group.
   The closest SARIF seems to have to nested diagnostics is the
   "relatedLocations" property of result objects (SARIF v2.1.0 section 3.27.22),
   so we lazily set this property and populate the array if and when
   secondary diagnostics occur (such as notes to a warning).  */

void
sarif_result::on_nested_diagnostic (const diagnostic_info &diagnostic,
				    diagnostic_t /*orig_diag_kind*/,
				    sarif_builder &builder)
{
  /* We don't yet generate meaningful logical locations for notes;
     sometimes these will related to current_function_decl, but
     often they won't.  */
  auto location_obj
    = builder.make_location_object (*this, *diagnostic.richloc, nullptr,
				    diagnostic_artifact_role::result_file);
  auto message_obj
    = builder.make_message_object (pp_formatted_text (builder.get_printer ()));
  pp_clear_output_area (builder.get_printer ());
  location_obj->set<sarif_message> ("message", std::move (message_obj));

  add_related_location (std::move (location_obj), builder);
}

/* Handle diagrams that occur within a diagnostic group.
   The closest thing in SARIF seems to be to add a location to the
   "releatedLocations" property  (SARIF v2.1.0 section 3.27.22),
   and to put the diagram into the "message" property of that location
   (SARIF v2.1.0 section 3.28.5).  */

void
sarif_result::on_diagram (const diagnostic_diagram &diagram,
			  sarif_builder &builder)
{
  auto location_obj = ::make_unique<sarif_location> ();
  auto message_obj = builder.make_message_object_for_diagram (diagram);
  location_obj->set<sarif_message> ("message", std::move (message_obj));

  add_related_location (std::move (location_obj), builder);
}

/* class sarif_location : public sarif_object.  */

/* Ensure this location has an "id" and return it.
   Use LOC_MGR if an id needs to be allocated.

   See the "id" property (3.28.2).

   We use this to only assign ids to locations that are
   referenced by another sarif object; others have no "id".   */

long
sarif_location::lazily_add_id (sarif_location_manager &loc_mgr)
{
  long id = get_id ();
  if (id != -1)
    return id;
  id = loc_mgr.allocate_location_id ();
  set_integer ("id", id);
  gcc_assert (id != -1);
  return id;
}

/* Get the id of this location, or -1 if it doesn't have one.  */

long
sarif_location::get_id () const
{
  json::value *id = get ("id");
  if (!id)
    return -1;
  gcc_assert (id->get_kind () == json::JSON_INTEGER);
  return static_cast <json::integer_number *> (id)->get ();
}

// 3.34.3 kinds property
static const char *
get_string_for_location_relationship_kind (enum location_relationship_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case location_relationship_kind::includes:
      return "includes";
    case location_relationship_kind::is_included_by:
      return "isIncludedBy";
    case location_relationship_kind::relevant:
      return "relevant";
    }
}

/* Lazily populate this location's "relationships" property (3.28.7)
   with the relationship of KIND to TARGET, creating objects
   as necessary.
   Use LOC_MGR for any locations that need "id" values.  */

void
sarif_location::lazily_add_relationship (sarif_location &target,
					 enum location_relationship_kind kind,
					 sarif_location_manager &loc_mgr)
{
  sarif_location_relationship &relationship_obj
    = lazily_add_relationship_object (target, loc_mgr);

  relationship_obj.lazily_add_kind (kind);
}

/* Lazily populate this location's "relationships" property (3.28.7)
   with a location_relationship to TARGET, creating objects
   as necessary.
   Use LOC_MGR for any locations that need "id" values.  */

sarif_location_relationship &
sarif_location::lazily_add_relationship_object (sarif_location &target,
						sarif_location_manager &loc_mgr)
{
  /* See if THIS already has a locationRelationship referencing TARGET.  */
  auto iter = m_relationships_map.find (&target);
  if (iter != m_relationships_map.end ())
    {
      /* We already have a locationRelationship from THIS to TARGET.  */
      sarif_location_relationship *relationship = iter->second;
      gcc_assert (relationship->get_target_id() == target.get_id ());
      return *relationship;
    }

  // Ensure that THIS has a "relationships" property (3.28.7).
  json::array &relationships_arr = lazily_add_relationships_array ();

  /* No existing locationRelationship from THIS to TARGET; make one,
     record it, and add it to the "relationships" array.  */
  auto relationship_obj
    = ::make_unique<sarif_location_relationship> (target, loc_mgr);
  sarif_location_relationship *relationship = relationship_obj.get ();
  auto kv
    = std::pair<sarif_location *,
		sarif_location_relationship *> (&target, relationship);
  m_relationships_map.insert (kv);

  relationships_arr.append (std::move (relationship_obj));

  return *relationship;
}

/* Ensure this location has a "relationships" array (3.28.7).  */

json::array &
sarif_location::lazily_add_relationships_array ()
{
  const char *const property_name = "relationships";
  if (json::value *relationships = get (property_name))
    {
      gcc_assert (relationships->get_kind () == json::JSON_ARRAY);
      return *static_cast <json::array *> (relationships);
    }
  json::array *relationships_arr = new json::array ();
  set (property_name, relationships_arr);
  return *relationships_arr;
}

/* class sarif_ice_notification : public sarif_location_manager.  */

/* sarif_ice_notification's ctor.
   DIAGNOSTIC is an internal compiler error.  */

sarif_ice_notification::
sarif_ice_notification (const diagnostic_info &diagnostic,
			sarif_builder &builder,
			std::unique_ptr<json::object> backtrace)
{
  /* "locations" property (SARIF v2.1.0 section 3.58.4).  */
  auto locations_arr
    = builder.make_locations_arr (*this,
				  diagnostic,
				  diagnostic_artifact_role::result_file);
  set<json::array> ("locations", std::move (locations_arr));

  /* "message" property (SARIF v2.1.0 section 3.85.5).  */
  auto message_obj
    = builder.make_message_object (pp_formatted_text (builder.get_printer ()));
  pp_clear_output_area (builder.get_printer ());
  set<sarif_message> ("message", std::move (message_obj));

  /* "level" property (SARIF v2.1.0 section 3.58.6).  */
  set_string ("level", "error");

  /* If we have backtrace information, add it as part of a property bag.  */
  if (backtrace)
    {
      sarif_property_bag &bag = get_or_create_properties ();
      bag.set ("gcc/backtrace", std::move (backtrace));
    }
}

/* Implementation of sarif_location_manager::add_related_location vfunc
   for notifications.  */

void
sarif_ice_notification::
add_related_location (std::unique_ptr<sarif_location> location_obj,
		      sarif_builder &builder)
{
  /* Support for related locations within a notification was added
     in SARIF 2.2; see https://github.com/oasis-tcs/sarif-spec/issues/540  */
  if (builder.get_version () >= sarif_version::v2_2_prerelease_2024_08_08)
    sarif_location_manager::add_related_location (std::move (location_obj),
						  builder);
  /* Otherwise implicitly discard LOCATION_OBJ.  */
}

/* class sarif_location_relationship : public sarif_object.  */

sarif_location_relationship::
sarif_location_relationship (sarif_location &target,
			     sarif_location_manager &loc_mgr)
: m_kinds ((unsigned)location_relationship_kind::NUM_KINDS)
{
  bitmap_clear (m_kinds);
  set_integer ("target", target.lazily_add_id (loc_mgr));
}

long
sarif_location_relationship::get_target_id () const
{
  json::value *id = get ("id");
  gcc_assert (id);
  return static_cast <json::integer_number *> (id)->get ();
}

void
sarif_location_relationship::
lazily_add_kind (enum location_relationship_kind kind)
{
  if (bitmap_bit_p (m_kinds, (int)kind))
    return; // already have this kind
  bitmap_set_bit (m_kinds, (int)kind);

  // 3.34.3 kinds property
  json::array *kinds_arr = nullptr;
  if (json::value *kinds_val = get ("kinds"))
    {
      gcc_assert (kinds_val->get_kind () == json::JSON_ARRAY);
    }
  else
    {
      kinds_arr = new json::array ();
      set ("kinds", kinds_arr);
    }
  const char *kind_str = get_string_for_location_relationship_kind (kind);
  kinds_arr->append_string (kind_str);
}

/* class sarif_code_flow : public sarif_object.  */

sarif_code_flow::sarif_code_flow (sarif_result &parent,
				  unsigned idx_within_parent)
: m_parent (parent),
  m_idx_within_parent (idx_within_parent)
{
  /* "threadFlows" property (SARIF v2.1.0 section 3.36.3).  */
  auto thread_flows_arr = ::make_unique<json::array> ();
  m_thread_flows_arr = thread_flows_arr.get (); // borrowed
  set<json::array> ("threadFlows", std::move (thread_flows_arr));
}

sarif_thread_flow &
sarif_code_flow::get_or_append_thread_flow (const diagnostic_thread &thread,
					    diagnostic_thread_id_t thread_id)
{
  sarif_thread_flow **slot = m_thread_id_map.get (thread_id);
  if (slot)
    return **slot;

  unsigned next_thread_flow_idx = m_thread_flows_arr->size ();
  auto thread_flow_obj
    = ::make_unique<sarif_thread_flow> (*this, thread, next_thread_flow_idx);
  m_thread_id_map.put (thread_id, thread_flow_obj.get ()); // borrowed
  sarif_thread_flow *result = thread_flow_obj.get ();
  m_thread_flows_arr->append<sarif_thread_flow> (std::move (thread_flow_obj));
  return *result;
}

sarif_thread_flow &
sarif_code_flow::get_thread_flow (diagnostic_thread_id_t thread_id)
{
  sarif_thread_flow **slot = m_thread_id_map.get (thread_id);
  gcc_assert (slot); // it must already have one
  return **slot;
}

void
sarif_code_flow::add_location (sarif_thread_flow_location &tfl_obj)
{
  m_all_tfl_objs.push_back (&tfl_obj);
}

sarif_thread_flow_location &
sarif_code_flow::get_thread_flow_loc_obj (diagnostic_event_id_t event_id) const
{
  gcc_assert (event_id.known_p ());
  gcc_assert ((size_t)event_id.zero_based () < m_all_tfl_objs.size ());
  sarif_thread_flow_location *tfl_obj = m_all_tfl_objs[event_id.zero_based ()];
  gcc_assert (tfl_obj);
  return *tfl_obj;
}

/* class sarif_thread_flow : public sarif_object.  */

sarif_thread_flow::sarif_thread_flow (sarif_code_flow &parent,
				      const diagnostic_thread &thread,
				      unsigned idx_within_parent)
: m_parent (parent),
  m_idx_within_parent (idx_within_parent)
{
  /* "id" property (SARIF v2.1.0 section 3.37.2).  */
  label_text name (thread.get_name (false));
  set_string ("id", name.get ());

  /* "locations" property (SARIF v2.1.0 section 3.37.6).  */
  m_locations_arr = new json::array ();

  /* Give ownership of m_locations_arr to json::object;
     keep a borrowed ptr.  */
  set ("locations", m_locations_arr);
}

/* Add a sarif_thread_flow_location to this threadFlow object, but
   don't populate it yet.  */

sarif_thread_flow_location &
sarif_thread_flow::add_location ()
{
  const unsigned thread_flow_location_idx = m_locations_arr->size ();
  sarif_thread_flow_location *thread_flow_loc_obj
    = new sarif_thread_flow_location (*this, thread_flow_location_idx);
  m_locations_arr->append (thread_flow_loc_obj);
  m_parent.add_location (*thread_flow_loc_obj);
  return *thread_flow_loc_obj;
}

/* class sarif_builder.  */

/* sarif_builder's ctor.  */

sarif_builder::sarif_builder (diagnostic_context &context,
			      const line_maps *line_maps,
			      const char *main_input_filename_,
			      bool formatted,
			      enum sarif_version version)
: m_context (context),
  m_printer (context.m_printer),
  m_line_maps (line_maps),
  m_token_printer (*this),
  m_version (version),
  m_invocation_obj
    (::make_unique<sarif_invocation> (*this,
				      context.get_original_argv ())),
  m_results_array (new json::array ()),
  m_cur_group_result (nullptr),
  m_seen_any_relative_paths (false),
  m_rule_id_set (),
  m_rules_arr (new json::array ()),
  m_tabstop (context.m_tabstop),
  m_formatted (formatted),
  m_next_result_idx (0),
  m_current_code_flow (nullptr)
{
  gcc_assert (m_line_maps);

  /* Mark MAIN_INPUT_FILENAME_ as the artifact that the tool was
     instructed to scan.
     Only quote the contents if it gets referenced by physical locations,
     since otherwise the "no diagnostics" case would quote the main input
     file, and doing so noticeably bloated the output seen in analyzer
     integration testing (build directory went from 20G -> 21G).  */
  if (main_input_filename_)
    get_or_create_artifact (main_input_filename_,
			    diagnostic_artifact_role::analysis_target,
			    false);
}

sarif_builder::~sarif_builder ()
{
  /* Normally m_filename_to_artifact_map will have been emptied as part
     of make_run_object, but this isn't run by all the selftests.
     Ensure the artifact objects are cleaned up for such cases.  */
  for (auto iter : m_filename_to_artifact_map)
    {
      sarif_artifact *artifact_obj = iter.second;
      delete artifact_obj;
    }
}

/* Functions at which to stop the backtrace print.  It's not
   particularly helpful to print the callers of these functions.  */

static const char * const bt_stop[] =
{
  "main",
  "toplev::main",
  "execute_one_pass",
  "compile_file",
};

struct bt_closure
{
  bt_closure (sarif_builder &builder,
	      json::array *frames_arr)
  : m_builder (builder),
    m_frames_arr (frames_arr)
  {
  }

  sarif_builder &m_builder;
  json::array *m_frames_arr;
};

/* A callback function passed to the backtrace_full function.  */

static int
bt_callback (void *data, uintptr_t pc, const char *filename, int lineno,
	     const char *function)
{
  bt_closure *closure = (bt_closure *)data;

  /* If we don't have any useful information, don't print
     anything.  */
  if (filename == NULL && function == NULL)
    return 0;

  /* Skip functions in diagnostic.cc or diagnostic-global-context.cc.  */
  if (closure->m_frames_arr->size () == 0
      && filename != NULL
      && (strcmp (lbasename (filename), "diagnostic.cc") == 0
	  || strcmp (lbasename (filename),
		     "diagnostic-global-context.cc") == 0))
    return 0;

  /* Print up to 20 functions.  We could make this a --param, but
     since this is only for debugging just use a constant for now.  */
  if (closure->m_frames_arr->size () >= 20)
    {
      /* Returning a non-zero value stops the backtrace.  */
      return 1;
    }

  char *alc = NULL;
  if (function != NULL)
    {
      char *str = cplus_demangle_v3 (function,
				     (DMGL_VERBOSE | DMGL_ANSI
				      | DMGL_GNU_V3 | DMGL_PARAMS));
      if (str != NULL)
	{
	  alc = str;
	  function = str;
	}

      for (size_t i = 0; i < ARRAY_SIZE (bt_stop); ++i)
	{
	  size_t len = strlen (bt_stop[i]);
	  if (strncmp (function, bt_stop[i], len) == 0
	      && (function[len] == '\0' || function[len] == '('))
	    {
	      if (alc != NULL)
		free (alc);
	      /* Returning a non-zero value stops the backtrace.  */
	      return 1;
	    }
	}
    }

  auto frame_obj = ::make_unique<json::object> ();

  /* I tried using sarifStack and sarifStackFrame for this
     but it's not a good fit e.g. PC information.  */
  char buf[128];
  snprintf (buf, sizeof (buf) - 1, "0x%lx", (unsigned long)pc);
  frame_obj->set_string ("pc", buf);
  if (function)
    frame_obj->set_string ("function", function);
  if (filename)
    frame_obj->set_string ("filename", filename);
  frame_obj->set_integer ("lineno", lineno);
  closure->m_frames_arr->append (std::move (frame_obj));

  if (alc != NULL)
    free (alc);

  return 0;
}

/* Attempt to generate a JSON object representing a backtrace,
   for adding to ICE notifications.  */

std::unique_ptr<json::object>
sarif_builder::make_stack_from_backtrace ()
{
  auto frames_arr = ::make_unique<json::array> ();

  backtrace_state *state = nullptr;
  state = backtrace_create_state (nullptr, 0, nullptr, nullptr);
  bt_closure closure (*this, frames_arr.get ());
  const int frames_to_skip = 5;
  if (state != nullptr)
    backtrace_full (state, frames_to_skip, bt_callback, nullptr,
		    (void *) &closure);

  if (frames_arr->size () == 0)
    return nullptr;

  auto stack = ::make_unique<json::object> ();
  stack->set ("frames", std::move (frames_arr));
  return stack;
}

/* Implementation of "on_report_diagnostic" for SARIF output.  */

void
sarif_builder::on_report_diagnostic (const diagnostic_info &diagnostic,
				     diagnostic_t orig_diag_kind,
				     diagnostic_sarif_format_buffer *buffer)
{
  pp_output_formatted_text (m_printer, m_context.get_urlifier ());

  if (diagnostic.kind == DK_ICE || diagnostic.kind == DK_ICE_NOBT)
    {
      std::unique_ptr<json::object> stack = make_stack_from_backtrace ();
      m_invocation_obj->add_notification_for_ice (diagnostic, *this,
						  std::move (stack));

      /* Print a header for the remaining output to stderr, and
	 return, attempting to print the usual ICE messages to
	 stderr.  Hopefully this will be helpful to the user in
	 indicating what's gone wrong (also for DejaGnu, for pruning
	 those messages).   */
      fnotice (stderr, "Internal compiler error:\n");

      return;
    }

  if (buffer)
    {
      /* When buffering, we can only handle top-level results.  */
      gcc_assert (!m_cur_group_result);
      buffer->add_result (make_result_object (diagnostic, orig_diag_kind,
					      m_next_result_idx++));
      return;
    }

  if (m_cur_group_result)
    /* Nested diagnostic.  */
    m_cur_group_result->on_nested_diagnostic (diagnostic,
					      orig_diag_kind,
					      *this);
  else
    {
      /* Top-level diagnostic.  */
      m_cur_group_result = make_result_object (diagnostic, orig_diag_kind,
					       m_next_result_idx++);
    }
}

/* Implementation of diagnostic_context::m_diagrams.m_emission_cb
   for SARIF output.  */

void
sarif_builder::emit_diagram (const diagnostic_diagram &diagram)
{
  /* We must be within the emission of a top-level diagnostic.  */
  gcc_assert (m_cur_group_result);
  m_cur_group_result->on_diagram (diagram, *this);
}

/* Implementation of "end_group_cb" for SARIF output.  */

void
sarif_builder::end_group ()
{
  if (m_cur_group_result)
    {
      m_cur_group_result->process_worklist (*this);
      m_results_array->append<sarif_result> (std::move (m_cur_group_result));
    }
}

/* Create a top-level object, and add it to all the results
   (and other entities) we've seen so far, moving ownership
   to the object.  */

std::unique_ptr<sarif_log>
sarif_builder::flush_to_object ()
{
  m_invocation_obj->prepare_to_flush (*this);
  std::unique_ptr<sarif_log> top
    = make_top_level_object (std::move (m_invocation_obj),
			     std::move (m_results_array));
  return top;
}

/* Create a top-level object, and add it to all the results
   (and other entities) we've seen so far.

   Flush it all to OUTF.  */

void
sarif_builder::flush_to_file (FILE *outf)
{
  std::unique_ptr<sarif_log> top = flush_to_object ();
  top->dump (outf, m_formatted);
  fprintf (outf, "\n");
}

/* Attempt to convert DIAG_KIND to a suitable value for the "level"
   property (SARIF v2.1.0 section 3.27.10).

   Return nullptr if there isn't one.  */

static const char *
maybe_get_sarif_level (diagnostic_t diag_kind)
{
  switch (diag_kind)
    {
    case DK_WARNING:
      return "warning";
    case DK_ERROR:
      return "error";
    case DK_NOTE:
    case DK_ANACHRONISM:
      return "note";
    default:
      return nullptr;
    }
}

/* Make a string for DIAG_KIND suitable for use a ruleId
   (SARIF v2.1.0 section 3.27.5) as a fallback for when we don't
   have anything better to use.  */

static char *
make_rule_id_for_diagnostic_kind (diagnostic_t diag_kind)
{
  /* Lose the trailing ": ".  */
  const char *kind_text = get_diagnostic_kind_text (diag_kind);
  size_t len = strlen (kind_text);
  gcc_assert (len > 2);
  gcc_assert (kind_text[len - 2] == ':');
  gcc_assert (kind_text[len - 1] == ' ');
  char *rstrip = xstrdup (kind_text);
  rstrip[len - 2] = '\0';
  return rstrip;
}

/* Make a "result" object (SARIF v2.1.0 section 3.27) for DIAGNOSTIC.  */

std::unique_ptr<sarif_result>
sarif_builder::make_result_object (const diagnostic_info &diagnostic,
				   diagnostic_t orig_diag_kind,
				   unsigned idx_within_parent)
{
  auto result_obj = ::make_unique<sarif_result> (idx_within_parent);

  /* "ruleId" property (SARIF v2.1.0 section 3.27.5).  */
  /* Ideally we'd have an option_name for these.  */
  if (char *option_text
	= m_context.make_option_name (diagnostic.option_id,
				      orig_diag_kind, diagnostic.kind))
    {
      /* Lazily create reportingDescriptor objects for and add to m_rules_arr.
	 Set ruleId referencing them.  */
      result_obj->set_string ("ruleId", option_text);
      if (m_rule_id_set.contains (option_text))
	free (option_text);
      else
	{
	  /* This is the first time we've seen this ruleId.  */
	  /* Add to set, taking ownership.  */
	  m_rule_id_set.add (option_text);

	  m_rules_arr->append<sarif_reporting_descriptor>
	    (make_reporting_descriptor_object_for_warning (diagnostic,
							   orig_diag_kind,
							   option_text));
	}
    }
  else
    {
      /* Otherwise, we have an "error" or a stray "note"; use the
	 diagnostic kind as the ruleId, so that the result object at least
	 has a ruleId.
	 We don't bother creating reportingDescriptor objects for these.  */
      char *rule_id = make_rule_id_for_diagnostic_kind (orig_diag_kind);
      result_obj->set_string ("ruleId", rule_id);
      free (rule_id);
    }

  if (diagnostic.metadata)
    {
      /* "taxa" property (SARIF v2.1.0 section 3.27.8).  */
      if (int cwe_id = diagnostic.metadata->get_cwe ())
	{
	  auto taxa_arr = ::make_unique<json::array> ();
	  taxa_arr->append<sarif_reporting_descriptor_reference>
	    (make_reporting_descriptor_reference_object_for_cwe_id (cwe_id));
	  result_obj->set<json::array> ("taxa", std::move (taxa_arr));
	}

      diagnostic.metadata->maybe_add_sarif_properties (*result_obj);

      /* We don't yet support diagnostic_metadata::rule.  */
    }

  /* "level" property (SARIF v2.1.0 section 3.27.10).  */
  if (const char *sarif_level = maybe_get_sarif_level (diagnostic.kind))
    result_obj->set_string ("level", sarif_level);

  /* "message" property (SARIF v2.1.0 section 3.27.11).  */
  auto message_obj
    = make_message_object (pp_formatted_text (m_printer));
  pp_clear_output_area (m_printer);
  result_obj->set<sarif_message> ("message", std::move (message_obj));

  /* "locations" property (SARIF v2.1.0 section 3.27.12).  */
  result_obj->set<json::array>
    ("locations",
     make_locations_arr (*result_obj.get (),
			 diagnostic,
			 diagnostic_artifact_role::result_file));

  /* "codeFlows" property (SARIF v2.1.0 section 3.27.18).  */
  if (const diagnostic_path *path = diagnostic.richloc->get_path ())
    {
      auto code_flows_arr = ::make_unique<json::array> ();
      const unsigned code_flow_index = 0;
      code_flows_arr->append<sarif_code_flow>
	(make_code_flow_object (*result_obj.get (),
				code_flow_index,
				*path));
      result_obj->set<json::array> ("codeFlows", std::move (code_flows_arr));
    }

  /* The "relatedLocations" property (SARIF v2.1.0 section 3.27.22) is
     set up later, if any nested diagnostics occur within this diagnostic
     group.  */

  /* "fixes" property (SARIF v2.1.0 section 3.27.30).  */
  const rich_location *richloc = diagnostic.richloc;
  if (richloc->get_num_fixit_hints ())
    {
      auto fix_arr = ::make_unique<json::array> ();
      fix_arr->append<sarif_fix> (make_fix_object (*richloc));
      result_obj->set<json::array> ("fixes", std::move (fix_arr));
    }

  return result_obj;
}

/* Make a "reportingDescriptor" object (SARIF v2.1.0 section 3.49)
   for a GCC warning.  */

std::unique_ptr<sarif_reporting_descriptor>
sarif_builder::
make_reporting_descriptor_object_for_warning (const diagnostic_info &diagnostic,
					      diagnostic_t /*orig_diag_kind*/,
					      const char *option_text)
{
  auto reporting_desc = ::make_unique<sarif_reporting_descriptor> ();

  /* "id" property (SARIF v2.1.0 section 3.49.3).  */
  reporting_desc->set_string ("id", option_text);

  /* We don't implement "name" property (SARIF v2.1.0 section 3.49.7), since
     it seems redundant compared to "id".  */

  /* "helpUri" property (SARIF v2.1.0 section 3.49.12).  */
  if (char *option_url = m_context.make_option_url (diagnostic.option_id))
    {
      reporting_desc->set_string ("helpUri", option_url);
      free (option_url);
    }

  return reporting_desc;
}

/* Make a "reportingDescriptor" object (SARIF v2.1.0 section 3.49)
   for CWE_ID, for use within the CWE taxa array.  */

std::unique_ptr<sarif_reporting_descriptor>
sarif_builder::make_reporting_descriptor_object_for_cwe_id (int cwe_id) const
{
  auto reporting_desc = ::make_unique<sarif_reporting_descriptor> ();

  /* "id" property (SARIF v2.1.0 section 3.49.3).  */
  {
    pretty_printer pp;
    pp_printf (&pp, "%i", cwe_id);
    reporting_desc->set_string ("id", pp_formatted_text (&pp));
  }

  /* "helpUri" property (SARIF v2.1.0 section 3.49.12).  */
  {
    char *url = get_cwe_url (cwe_id);
    reporting_desc->set_string ("helpUri", url);
    free (url);
  }

  return reporting_desc;
}

/* Make a "reportingDescriptorReference" object (SARIF v2.1.0 section 3.52)
   referencing CWE_ID, for use within a result object.
   Also, add CWE_ID to m_cwe_id_set.  */

std::unique_ptr<sarif_reporting_descriptor_reference>
sarif_builder::
make_reporting_descriptor_reference_object_for_cwe_id (int cwe_id)
{
  auto desc_ref_obj = ::make_unique<sarif_reporting_descriptor_reference> ();

  /* "id" property (SARIF v2.1.0 section 3.52.4).  */
  {
    pretty_printer pp;
    pp_printf (&pp, "%i", cwe_id);
    desc_ref_obj->set_string ("id", pp_formatted_text (&pp));
  }

  /* "toolComponent" property (SARIF v2.1.0 section 3.52.7).  */
  desc_ref_obj->set<sarif_tool_component_reference>
    ("toolComponent", make_tool_component_reference_object_for_cwe ());

  /* Add CWE_ID to our set.  */
  gcc_assert (cwe_id > 0);
  m_cwe_id_set.add (cwe_id);

  return desc_ref_obj;
}

/* Make a "toolComponentReference" object (SARIF v2.1.0 section 3.54) that
   references the CWE taxonomy.  */

std::unique_ptr<sarif_tool_component_reference>
sarif_builder::
make_tool_component_reference_object_for_cwe () const
{
  auto comp_ref_obj = ::make_unique<sarif_tool_component_reference> ();

  /* "name" property  (SARIF v2.1.0 section 3.54.3).  */
  comp_ref_obj->set_string ("name", "cwe");

  return comp_ref_obj;
}

/* Make an array suitable for use as the "locations" property of:
   - a "result" object (SARIF v2.1.0 section 3.27.12), or
   - a "notification" object (SARIF v2.1.0 section 3.58.4).
   Use LOC_MGR for any locations that need "id" values.  */

std::unique_ptr<json::array>
sarif_builder::make_locations_arr (sarif_location_manager &loc_mgr,
				   const diagnostic_info &diagnostic,
				   enum diagnostic_artifact_role role)
{
  auto locations_arr = ::make_unique<json::array> ();
  const logical_location *logical_loc = nullptr;
  if (auto client_data_hooks = m_context.get_client_data_hooks ())
    logical_loc = client_data_hooks->get_current_logical_location ();

  auto location_obj
    = make_location_object (loc_mgr, *diagnostic.richloc, logical_loc, role);
  /* Don't add entirely empty location objects to the array.  */
  if (!location_obj->is_empty ())
    locations_arr->append<sarif_location> (std::move (location_obj));

  return locations_arr;
}

/* If LOGICAL_LOC is non-null, use it to create a "logicalLocations" property
   within LOCATION_OBJ (SARIF v2.1.0 section 3.28.4).  */

void
sarif_builder::
set_any_logical_locs_arr (sarif_location &location_obj,
			  const logical_location *logical_loc)
{
  if (!logical_loc)
    return;
  auto location_locs_arr = ::make_unique<json::array> ();
  location_locs_arr->append<sarif_logical_location>
    (make_sarif_logical_location_object (*logical_loc));
  location_obj.set<json::array> ("logicalLocations",
				 std::move (location_locs_arr));
}

/* Make a "location" object (SARIF v2.1.0 section 3.28) for RICH_LOC
   and LOGICAL_LOC.
   Use LOC_MGR for any locations that need "id" values, and for
   any worklist items.  */

std::unique_ptr<sarif_location>
sarif_builder::make_location_object (sarif_location_manager &loc_mgr,
				     const rich_location &rich_loc,
				     const logical_location *logical_loc,
				     enum diagnostic_artifact_role role)
{
  class escape_nonascii_renderer : public content_renderer
  {
  public:
    escape_nonascii_renderer (const rich_location &richloc,
			      enum diagnostics_escape_format escape_format)
    : m_richloc (richloc),
      m_escape_format (escape_format)
    {}

    std::unique_ptr<sarif_multiformat_message_string>
    render (const sarif_builder &builder) const final override
    {
      diagnostic_context dc;
      diagnostic_initialize (&dc, 0);
      dc.m_source_printing.enabled = true;
      dc.m_source_printing.colorize_source_p = false;
      dc.m_source_printing.show_labels_p = true;
      dc.m_source_printing.show_line_numbers_p = true;

      rich_location my_rich_loc (m_richloc);
      my_rich_loc.set_escape_on_output (true);

      diagnostic_source_print_policy source_policy (dc);
      dc.set_escape_format (m_escape_format);
      source_policy.print (*dc.m_printer, my_rich_loc, DK_ERROR, nullptr);

      std::unique_ptr<sarif_multiformat_message_string> result
	= builder.make_multiformat_message_string
	    (pp_formatted_text (dc.m_printer));

      diagnostic_finish (&dc);

      return result;
    }
  private:
    const rich_location &m_richloc;
    enum diagnostics_escape_format m_escape_format;
  } the_renderer (rich_loc,
		  m_context.get_escape_format ());

  auto location_obj = ::make_unique<sarif_location> ();

  /* Get primary loc from RICH_LOC.  */
  location_t loc = rich_loc.get_loc ();

  /* "physicalLocation" property (SARIF v2.1.0 section 3.28.3).  */
  const content_renderer *snippet_renderer
    = rich_loc.escape_on_output_p () ? &the_renderer : nullptr;
  if (auto phs_loc_obj
	= maybe_make_physical_location_object (loc, role,
					       rich_loc.get_column_override (),
					       snippet_renderer))
    location_obj->set<sarif_physical_location> ("physicalLocation",
						std::move (phs_loc_obj));

  /* "logicalLocations" property (SARIF v2.1.0 section 3.28.4).  */
  set_any_logical_locs_arr (*location_obj, logical_loc);

  /* Handle labelled ranges and/or secondary locations.  */
  {
    std::unique_ptr<json::array> annotations_arr = nullptr;
    for (unsigned int i = 0; i < rich_loc.get_num_locations (); i++)
      {
	const location_range *range = rich_loc.get_range (i);
	bool handled = false;
	if (const range_label *label = range->m_label)
	  {
	    label_text text = label->get_text (i);
	    if (text.get ())
	      {
		/* Create annotations for any labelled ranges.  */
		location_t range_loc = rich_loc.get_loc (i);
		auto region
		  = maybe_make_region_object (range_loc,
					      rich_loc.get_column_override ());
		if (region)
		  {
		    if (!annotations_arr)
		      annotations_arr = ::make_unique<json::array> ();
		    region->set<sarif_message>
		      ("message", make_message_object (text.get ()));
		    annotations_arr->append<sarif_region> (std::move (region));
		    handled = true;
		  }
	      }
	  }

	/* Add related locations for any secondary locations in RICH_LOC
	   that don't have labels (and thus aren't added to "annotations"). */
	if (i > 0 && !handled)
	  loc_mgr.add_relationship_to_worklist
	    (*location_obj.get (),
	     sarif_location_manager::worklist_item::kind::unlabelled_secondary_location,
	     range->m_loc);
      }
    if (annotations_arr)
      /* "annotations" property (SARIF v2.1.0 section 3.28.6).  */
      location_obj->set<json::array> ("annotations",
				      std::move (annotations_arr));
  }

  add_any_include_chain (loc_mgr, *location_obj.get (), loc);

  /* A flag for hinting that the diagnostic involves issues at the
     level of character encodings (such as homoglyphs, or misleading
     bidirectional control codes), and thus that it will be helpful
     to the user if we show some representation of
     how the characters in the pertinent source lines are encoded.  */
  if (rich_loc.escape_on_output_p ())
    {
      sarif_property_bag &bag = location_obj->get_or_create_properties ();
      bag.set_bool ("gcc/escapeNonAscii", rich_loc.escape_on_output_p ());
    }

  return location_obj;
}

/* If WHERE was #included from somewhere, add a worklist item
   to LOC_MGR to lazily add a location for the #include location,
   and relationships between it and the LOCATION_OBJ.
   Compare with diagnostic_context::report_current_module, but rather
   than iterating the current chain, we add the next edge and iterate
   in the worklist, so that edges are only added once.  */

void
sarif_builder::add_any_include_chain (sarif_location_manager &loc_mgr,
				      sarif_location &location_obj,
				      location_t where)
{
  if (where <= BUILTINS_LOCATION)
    return;

  const line_map_ordinary *map = nullptr;
  linemap_resolve_location (m_line_maps, where,
			    LRK_MACRO_DEFINITION_LOCATION,
			    &map);

  if (!map)
    return;

  location_t include_loc = linemap_included_from (map);
  map = linemap_included_from_linemap (m_line_maps, map);
  if (!map)
    return;
  loc_mgr.add_relationship_to_worklist
    (location_obj,
     sarif_result::worklist_item::kind::included_from,
     include_loc);
}

/* Make a "location" object (SARIF v2.1.0 section 3.28) for WHERE
   within an include chain.  */

std::unique_ptr<sarif_location>
sarif_builder::make_location_object (sarif_location_manager &loc_mgr,
				     location_t loc,
				     enum diagnostic_artifact_role role)
{
  auto location_obj = ::make_unique<sarif_location> ();

  /* "physicalLocation" property (SARIF v2.1.0 section 3.28.3).  */
  if (auto phs_loc_obj
      = maybe_make_physical_location_object (loc, role, 0, nullptr))
    location_obj->set<sarif_physical_location> ("physicalLocation",
						std::move (phs_loc_obj));

  add_any_include_chain (loc_mgr, *location_obj.get (), loc);

  return location_obj;
}

/* Make a "location" object (SARIF v2.1.0 section 3.28) for EVENT
   within a diagnostic_path.  */

std::unique_ptr<sarif_location>
sarif_builder::make_location_object (sarif_location_manager &loc_mgr,
				     const diagnostic_event &event,
				     enum diagnostic_artifact_role role)
{
  auto location_obj = ::make_unique<sarif_location> ();

  /* "physicalLocation" property (SARIF v2.1.0 section 3.28.3).  */
  location_t loc = event.get_location ();
  if (auto phs_loc_obj
	= maybe_make_physical_location_object (loc, role, 0, nullptr))
    location_obj->set<sarif_physical_location> ("physicalLocation",
						std::move (phs_loc_obj));

  /* "logicalLocations" property (SARIF v2.1.0 section 3.28.4).  */
  const logical_location *logical_loc = event.get_logical_location ();
  set_any_logical_locs_arr (*location_obj, logical_loc);

  /* "message" property (SARIF v2.1.0 section 3.28.5).  */
  std::unique_ptr<pretty_printer> pp = get_printer ()->clone ();
  event.print_desc (*pp);
  location_obj->set<sarif_message>
    ("message",
     make_message_object (pp_formatted_text (pp.get ())));

  add_any_include_chain (loc_mgr, *location_obj.get (), loc);

  return location_obj;
}

/* Make a "physicalLocation" object (SARIF v2.1.0 section 3.29) for LOC.

   If COLUMN_OVERRIDE is non-zero, then use it as the column number
   if LOC has no column information.

   Ensure that we have an artifact object for the file, adding ROLE to it,
   and flagging that we will attempt to embed the contents of the artifact
   when writing it out.  */

std::unique_ptr<sarif_physical_location>
sarif_builder::
maybe_make_physical_location_object (location_t loc,
				     enum diagnostic_artifact_role role,
				     int column_override,
				     const content_renderer *snippet_renderer)
{
  if (loc <= BUILTINS_LOCATION || LOCATION_FILE (loc) == nullptr)
    return nullptr;

  auto phys_loc_obj = ::make_unique<sarif_physical_location> ();

  /* "artifactLocation" property (SARIF v2.1.0 section 3.29.3).  */
  phys_loc_obj->set<sarif_artifact_location>
    ("artifactLocation", make_artifact_location_object (loc));
  get_or_create_artifact (LOCATION_FILE (loc), role, true);

  /* "region" property (SARIF v2.1.0 section 3.29.4).  */
  if (auto region_obj = maybe_make_region_object (loc, column_override))
    phys_loc_obj->set<sarif_region> ("region", std::move (region_obj));

  /* "contextRegion" property (SARIF v2.1.0 section 3.29.5).  */
  if (auto context_region_obj
	= maybe_make_region_object_for_context (loc, snippet_renderer))
    phys_loc_obj->set<sarif_region> ("contextRegion",
				     std::move (context_region_obj));

  /* Instead, we add artifacts to the run as a whole,
     with artifact.contents.
     Could do both, though.  */

  return phys_loc_obj;
}

/* Make an "artifactLocation" object (SARIF v2.1.0 section 3.4) for LOC,
   or return nullptr.  */

std::unique_ptr<sarif_artifact_location>
sarif_builder::make_artifact_location_object (location_t loc)
{
  return make_artifact_location_object (LOCATION_FILE (loc));
}

/* The ID value for use in "uriBaseId" properties (SARIF v2.1.0 section 3.4.4)
   for when we need to express paths relative to PWD.  */

#define PWD_PROPERTY_NAME ("PWD")

/* Make an "artifactLocation" object (SARIF v2.1.0 section 3.4) for FILENAME,
   or return nullptr.  */

std::unique_ptr<sarif_artifact_location>
sarif_builder::make_artifact_location_object (const char *filename)
{
  auto artifact_loc_obj = ::make_unique<sarif_artifact_location> ();

  /* "uri" property (SARIF v2.1.0 section 3.4.3).  */
  artifact_loc_obj->set_string ("uri", filename);

  if (filename[0] != '/')
    {
      /* If we have a relative path, set the "uriBaseId" property
	 (SARIF v2.1.0 section 3.4.4).  */
      artifact_loc_obj->set_string ("uriBaseId", PWD_PROPERTY_NAME);
      m_seen_any_relative_paths = true;
    }

  return artifact_loc_obj;
}

/* Get the PWD, or nullptr, as an absolute file-based URI,
   adding a trailing forward slash (as required by SARIF v2.1.0
   section 3.14.14).  */

static char *
make_pwd_uri_str ()
{
  /* The prefix of a file-based URI, up to, but not including the path. */
#define FILE_PREFIX ("file://")

  const char *pwd = getpwd ();
  if (!pwd)
    return nullptr;
  size_t len = strlen (pwd);
  if (len == 0 || pwd[len - 1] != '/')
    return concat (FILE_PREFIX, pwd, "/", nullptr);
  else
    {
      gcc_assert (pwd[len - 1] == '/');
      return concat (FILE_PREFIX, pwd, nullptr);
    }
}

/* Make an "artifactLocation" object (SARIF v2.1.0 section 3.4) for the pwd,
   for use in the "run.originalUriBaseIds" property (SARIF v2.1.0
   section 3.14.14) when we have any relative paths.  */

std::unique_ptr<sarif_artifact_location>
sarif_builder::make_artifact_location_object_for_pwd () const
{
  auto artifact_loc_obj = ::make_unique<sarif_artifact_location> ();

  /* "uri" property (SARIF v2.1.0 section 3.4.3).  */
  if (char *pwd = make_pwd_uri_str ())
    {
      gcc_assert (strlen (pwd) > 0);
      gcc_assert (pwd[strlen (pwd) - 1] == '/');
      artifact_loc_obj->set_string ("uri", pwd);
      free (pwd);
    }

  return artifact_loc_obj;
}

/* Get the column number within EXPLOC.  */

int
sarif_builder::get_sarif_column (expanded_location exploc) const
{
  cpp_char_column_policy policy (m_tabstop, cpp_wcwidth);
  return location_compute_display_column (m_context.get_file_cache (),
					  exploc, policy);
}

/* Make a "region" object (SARIF v2.1.0 section 3.30) for LOC,
   or return nullptr.

   If COLUMN_OVERRIDE is non-zero, then use it as the column number
   if LOC has no column information.

   We only support text properties of regions ("text regions"),
   not binary properties ("binary regions"); see 3.30.1.  */

std::unique_ptr<sarif_region>
sarif_builder::maybe_make_region_object (location_t loc,
					 int column_override) const
{
  location_t caret_loc = get_pure_location (loc);

  if (caret_loc <= BUILTINS_LOCATION)
    return nullptr;

  location_t start_loc = get_start (loc);
  location_t finish_loc = get_finish (loc);

  expanded_location exploc_caret = expand_location (caret_loc);
  expanded_location exploc_start = expand_location (start_loc);
  expanded_location exploc_finish = expand_location (finish_loc);

  if (exploc_start.file !=exploc_caret.file)
    return nullptr;
  if (exploc_finish.file !=exploc_caret.file)
    return nullptr;

  /* We can have line == 0 in the presence of "#" lines.
     SARIF requires lines > 0, so if we hit this case we don't have a
     way of validly representing the region as SARIF; bail out.  */
  if (exploc_start.line <= 0)
    return nullptr;

  auto region_obj = ::make_unique<sarif_region> ();

  /* "startLine" property (SARIF v2.1.0 section 3.30.5) */
  region_obj->set_integer ("startLine", exploc_start.line);

  /* "startColumn" property (SARIF v2.1.0 section 3.30.6).

     We use column == 0 to mean the whole line, so omit the column
     information for this case, unless COLUMN_OVERRIDE is non-zero,
     (for handling certain awkward lexer diagnostics)  */

  if (exploc_start.column == 0 && column_override)
    /* Use the provided column number.  */
    exploc_start.column = column_override;

  if (exploc_start.column > 0)
    {
      int start_column = get_sarif_column (exploc_start);
      region_obj->set_integer ("startColumn", start_column);
    }

  /* "endLine" property (SARIF v2.1.0 section 3.30.7) */
  if (exploc_finish.line != exploc_start.line
      && exploc_finish.line > 0)
    region_obj->set_integer ("endLine", exploc_finish.line);

  /* "endColumn" property (SARIF v2.1.0 section 3.30.8).
     This expresses the column immediately beyond the range.

     We use column == 0 to mean the whole line, so omit the column
     information for this case.  */
  if (exploc_finish.column > 0)
    {
      int next_column = get_sarif_column (exploc_finish) + 1;
      region_obj->set_integer ("endColumn", next_column);
    }

  return region_obj;
}

/* Make a "region" object (SARIF v2.1.0 section 3.30) for the "contextRegion"
   property (SARIF v2.1.0 section 3.29.5) of a "physicalLocation".

   This is similar to maybe_make_region_object, but ignores column numbers,
   covering the line(s) as a whole, and including a "snippet" property
   embedding those source lines, making it easier for consumers to show
   the pertinent source.  */

std::unique_ptr<sarif_region>
sarif_builder::
maybe_make_region_object_for_context (location_t loc,
				      const content_renderer *snippet_renderer)
  const
{
  location_t caret_loc = get_pure_location (loc);

  if (caret_loc <= BUILTINS_LOCATION)
    return nullptr;

  location_t start_loc = get_start (loc);
  location_t finish_loc = get_finish (loc);

  expanded_location exploc_caret = expand_location (caret_loc);
  expanded_location exploc_start = expand_location (start_loc);
  expanded_location exploc_finish = expand_location (finish_loc);

  if (exploc_start.file !=exploc_caret.file)
    return nullptr;
  if (exploc_finish.file !=exploc_caret.file)
    return nullptr;

  /* We can have line == 0 in the presence of "#" lines.
     SARIF requires lines > 0, so if we hit this case we don't have a
     way of validly representing the region as SARIF; bail out.  */
  if (exploc_start.line <= 0)
    return nullptr;

  auto region_obj = ::make_unique<sarif_region> ();

  /* "startLine" property (SARIF v2.1.0 section 3.30.5) */
  region_obj->set_integer ("startLine", exploc_start.line);

  /* "endLine" property (SARIF v2.1.0 section 3.30.7) */
  if (exploc_finish.line != exploc_start.line
      && exploc_finish.line > 0)
    region_obj->set_integer ("endLine", exploc_finish.line);

  /* "snippet" property (SARIF v2.1.0 section 3.30.13).  */
  if (auto artifact_content_obj
	= maybe_make_artifact_content_object (exploc_start.file,
					      exploc_start.line,
					      exploc_finish.line,
					      snippet_renderer))
    region_obj->set<sarif_artifact_content> ("snippet",
					     std::move (artifact_content_obj));

  return region_obj;
}

/* Make a "region" object (SARIF v2.1.0 section 3.30) for the deletion region
   of HINT (as per SARIF v2.1.0 section 3.57.3).  */

std::unique_ptr<sarif_region>
sarif_builder::make_region_object_for_hint (const fixit_hint &hint) const
{
  location_t start_loc = hint.get_start_loc ();
  location_t next_loc = hint.get_next_loc ();

  expanded_location exploc_start = expand_location (start_loc);
  expanded_location exploc_next = expand_location (next_loc);

  auto region_obj = ::make_unique<sarif_region> ();

  /* "startLine" property (SARIF v2.1.0 section 3.30.5) */
  region_obj->set_integer ("startLine", exploc_start.line);

  /* "startColumn" property (SARIF v2.1.0 section 3.30.6) */
  int start_col = get_sarif_column (exploc_start);
  region_obj->set_integer ("startColumn", start_col);

  /* "endLine" property (SARIF v2.1.0 section 3.30.7) */
  if (exploc_next.line != exploc_start.line)
    region_obj->set_integer ("endLine", exploc_next.line);

  /* "endColumn" property (SARIF v2.1.0 section 3.30.8).
     This expresses the column immediately beyond the range.  */
  int next_col =  get_sarif_column (exploc_next);
  region_obj->set_integer ("endColumn", next_col);

  return region_obj;
}

/* Attempt to get a string for a logicalLocation's "kind" property
   (SARIF v2.1.0 section 3.33.7).
   Return nullptr if unknown.  */

static const char *
maybe_get_sarif_kind (enum logical_location_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case LOGICAL_LOCATION_KIND_UNKNOWN:
      return nullptr;

    case LOGICAL_LOCATION_KIND_FUNCTION:
      return "function";
    case LOGICAL_LOCATION_KIND_MEMBER:
      return "member";
    case LOGICAL_LOCATION_KIND_MODULE:
      return "module";
    case LOGICAL_LOCATION_KIND_NAMESPACE:
      return "namespace";
    case LOGICAL_LOCATION_KIND_TYPE:
      return "type";
    case LOGICAL_LOCATION_KIND_RETURN_TYPE:
      return "returnType";
    case LOGICAL_LOCATION_KIND_PARAMETER:
      return "parameter";
    case LOGICAL_LOCATION_KIND_VARIABLE:
      return "variable";
    }
}

/* Make a "logicalLocation" object (SARIF v2.1.0 section 3.33) for LOGICAL_LOC,
   or return nullptr.  */

std::unique_ptr<sarif_logical_location>
make_sarif_logical_location_object (const logical_location &logical_loc)
{
  auto logical_loc_obj = ::make_unique<sarif_logical_location> ();

  /* "name" property (SARIF v2.1.0 section 3.33.4).  */
  if (const char *short_name = logical_loc.get_short_name ())
    logical_loc_obj->set_string ("name", short_name);

  /* "fullyQualifiedName" property (SARIF v2.1.0 section 3.33.5).  */
  if (const char *name_with_scope = logical_loc.get_name_with_scope ())
    logical_loc_obj->set_string ("fullyQualifiedName", name_with_scope);

  /* "decoratedName" property (SARIF v2.1.0 section 3.33.6).  */
  if (const char *internal_name = logical_loc.get_internal_name ())
    logical_loc_obj->set_string ("decoratedName", internal_name);

  /* "kind" property (SARIF v2.1.0 section 3.33.7).  */
  enum logical_location_kind kind = logical_loc.get_kind ();
  if (const char *sarif_kind_str = maybe_get_sarif_kind (kind))
    logical_loc_obj->set_string ("kind", sarif_kind_str);

  return logical_loc_obj;
}

label_text
make_sarif_url_for_event (const sarif_code_flow *code_flow,
			  diagnostic_event_id_t event_id)
{
  gcc_assert (event_id.known_p ());

  if (!code_flow)
    return label_text ();

  const sarif_thread_flow_location &tfl_obj
    = code_flow->get_thread_flow_loc_obj (event_id);
  const int location_idx = tfl_obj.get_index_within_parent ();

  const sarif_thread_flow &thread_flow_obj = tfl_obj.get_parent ();
  const int thread_flow_idx = thread_flow_obj.get_index_within_parent ();

  const sarif_code_flow &code_flow_obj = thread_flow_obj.get_parent ();
  const int code_flow_idx = code_flow_obj.get_index_within_parent ();

  const sarif_result &result_obj = code_flow_obj.get_parent ();
  const int result_idx = result_obj.get_index_within_parent ();

  /* We only support a single run object in the log.  */
  const int run_idx = 0;

  char *buf = xasprintf
    ("sarif:/runs/%i/results/%i/codeFlows/%i/threadFlows/%i/locations/%i",
     run_idx, result_idx, code_flow_idx, thread_flow_idx, location_idx);
  return label_text::take (buf);
}

/* Make a "codeFlow" object (SARIF v2.1.0 section 3.36) for PATH.  */

std::unique_ptr<sarif_code_flow>
sarif_builder::make_code_flow_object (sarif_result &result,
				      unsigned idx_within_parent,
				      const diagnostic_path &path)
{
  auto code_flow_obj
    = ::make_unique <sarif_code_flow> (result, idx_within_parent);

  /* First pass:
     Create threadFlows and threadFlowLocation objects within them,
     effectively recording a mapping from event_id to threadFlowLocation
     so that we can later go from an event_id to a URI within the
     SARIF file.  */
  for (unsigned i = 0; i < path.num_events (); i++)
    {
      const diagnostic_event &event = path.get_event (i);
      const diagnostic_thread_id_t thread_id = event.get_thread_id ();

      sarif_thread_flow &thread_flow_obj
	= code_flow_obj->get_or_append_thread_flow (path.get_thread (thread_id),
						    thread_id);
      thread_flow_obj.add_location ();
    }

  /* Second pass: walk the events, populating the tfl objs.  */
  m_current_code_flow = code_flow_obj.get ();
  for (unsigned i = 0; i < path.num_events (); i++)
    {
      const diagnostic_event &event = path.get_event (i);
      sarif_thread_flow_location &thread_flow_loc_obj
	= code_flow_obj->get_thread_flow_loc_obj (i);
      populate_thread_flow_location_object (result,
					    thread_flow_loc_obj,
					    event,
					    i);
    }
  m_current_code_flow = nullptr;

  return code_flow_obj;
}

/* Populate TFL_OBJ, a "threadFlowLocation" object (SARIF v2.1.0 section 3.38)
   based on EVENT.  */

void
sarif_builder::
populate_thread_flow_location_object (sarif_result &result,
				      sarif_thread_flow_location &tfl_obj,
				      const diagnostic_event &ev,
				      int event_execution_idx)
{
  /* Give diagnostic_event subclasses a chance to add custom properties
     via a property bag.  */
  ev.maybe_add_sarif_properties (tfl_obj);

  /* "location" property (SARIF v2.1.0 section 3.38.3).  */
  tfl_obj.set<sarif_location>
    ("location",
     make_location_object (result, ev, diagnostic_artifact_role::traced_file));

  /* "kinds" property (SARIF v2.1.0 section 3.38.8).  */
  diagnostic_event::meaning m = ev.get_meaning ();
  if (auto kinds_arr = maybe_make_kinds_array (m))
    tfl_obj.set<json::array> ("kinds", std::move (kinds_arr));

  /* "nestingLevel" property (SARIF v2.1.0 section 3.38.10).  */
  tfl_obj.set_integer ("nestingLevel", ev.get_stack_depth ());

  /* "executionOrder" property (SARIF v2.1.0 3.38.11).
     Offset by 1 to match the human-readable values emitted by %@.  */
  tfl_obj.set_integer ("executionOrder", event_execution_idx + 1);

  /* It might be nice to eventually implement the following for -fanalyzer:
     - the "stack" property (SARIF v2.1.0 section 3.38.5)
     - the "state" property (SARIF v2.1.0 section 3.38.9)
     - the "importance" property (SARIF v2.1.0 section 3.38.13).  */
}

/* If M has any known meaning, make a json array suitable for the "kinds"
   property of a "threadFlowLocation" object (SARIF v2.1.0 section 3.38.8).

   Otherwise, return nullptr.  */

std::unique_ptr<json::array>
sarif_builder::maybe_make_kinds_array (diagnostic_event::meaning m) const
{
  if (m.m_verb == diagnostic_event::VERB_unknown
      && m.m_noun == diagnostic_event::NOUN_unknown
      && m.m_property == diagnostic_event::PROPERTY_unknown)
    return nullptr;

  auto kinds_arr = ::make_unique<json::array> ();
  if (const char *verb_str
	= diagnostic_event::meaning::maybe_get_verb_str (m.m_verb))
    kinds_arr->append_string (verb_str);
  if (const char *noun_str
	= diagnostic_event::meaning::maybe_get_noun_str (m.m_noun))
    kinds_arr->append_string (noun_str);
  if (const char *property_str
	= diagnostic_event::meaning::maybe_get_property_str (m.m_property))
    kinds_arr->append_string (property_str);
  return kinds_arr;
}

/* Make a "message" object (SARIF v2.1.0 section 3.11) for MSG.  */

std::unique_ptr<sarif_message>
sarif_builder::make_message_object (const char *msg) const
{
  auto message_obj = ::make_unique<sarif_message> ();

  /* "text" property (SARIF v2.1.0 section 3.11.8).  */
  message_obj->set_string ("text", msg);

  return message_obj;
}

/* Make a "message" object (SARIF v2.1.0 section 3.11) for DIAGRAM.
   We emit the diagram as a code block within the Markdown part
   of the message.  */

std::unique_ptr<sarif_message>
sarif_builder::make_message_object_for_diagram (const diagnostic_diagram &diagram)
{
  auto message_obj = ::make_unique<sarif_message> ();

  /* "text" property (SARIF v2.1.0 section 3.11.8).  */
  message_obj->set_string ("text", diagram.get_alt_text ());

  pretty_printer *const pp = m_printer;
  char *saved_prefix = pp_take_prefix (pp);
  pp_set_prefix (pp, nullptr);

  /* "To produce a code block in Markdown, simply indent every line of
     the block by at least 4 spaces or 1 tab."
     Here we use 4 spaces.  */
  diagram.get_canvas ().print_to_pp (pp, "    ");
  pp_set_prefix (pp, saved_prefix);

  /* "markdown" property (SARIF v2.1.0 section 3.11.9).  */
  message_obj->set_string ("markdown", pp_formatted_text (pp));

  pp_clear_output_area (pp);

  return message_obj;
}

/* Make a "multiformatMessageString object" (SARIF v2.1.0 section 3.12)
   for MSG.  */

std::unique_ptr<sarif_multiformat_message_string>
sarif_builder::make_multiformat_message_string (const char *msg) const
{
  auto message_obj = ::make_unique<sarif_multiformat_message_string> ();

  /* "text" property (SARIF v2.1.0 section 3.12.3).  */
  message_obj->set_string ("text", msg);

  return message_obj;
}

/* Convert VERSION to a value for the "$schema" property
   of a "sarifLog" object (SARIF v2.1.0 section 3.13.3).  */

static const char *
sarif_version_to_url (enum sarif_version version)
{
  switch (version)
    {
    default:
      gcc_unreachable ();
    case sarif_version::v2_1_0:
      return "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json";
    case sarif_version::v2_2_prerelease_2024_08_08:
      return "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/refs/tags/2.2-prerelease-2024-08-08/sarif-2.2/schema/sarif-2-2.schema.json";
    }
}

/* Convert VERSION to a value for the "version" property
   of a "sarifLog" object (SARIF v2.1.0 section 3.13.2).  */

static const char *
sarif_version_to_property (enum sarif_version version)
{
  switch (version)
    {
    default:
      gcc_unreachable ();
    case sarif_version::v2_1_0:
      return "2.1.0";
    case sarif_version::v2_2_prerelease_2024_08_08:
      /* I would have used "2.2-prerelease-2024-08-08",
	 but the schema only accepts "2.2".  */
      return "2.2";
    }
}

/* Make a top-level "sarifLog" object (SARIF v2.1.0 section 3.13).  */

std::unique_ptr<sarif_log>
sarif_builder::
make_top_level_object (std::unique_ptr<sarif_invocation> invocation_obj,
		       std::unique_ptr<json::array> results)
{
  auto log_obj = ::make_unique<sarif_log> ();

  /* "$schema" property (SARIF v2.1.0 section 3.13.3) .  */
  log_obj->set_string ("$schema", sarif_version_to_url (m_version));

  /* "version" property (SARIF v2.1.0 section 3.13.2).  */
  log_obj->set_string ("version", sarif_version_to_property (m_version));

  /* "runs" property (SARIF v2.1.0 section 3.13.4).  */
  auto run_arr = ::make_unique<json::array> ();
  auto run_obj = make_run_object (std::move (invocation_obj),
				  std::move (results));
  run_arr->append<sarif_run> (std::move (run_obj));
  log_obj->set<json::array> ("runs", std::move (run_arr));

  return log_obj;
}

/* Make a "run" object (SARIF v2.1.0 section 3.14).  */

std::unique_ptr<sarif_run>
sarif_builder::
make_run_object (std::unique_ptr<sarif_invocation> invocation_obj,
		 std::unique_ptr<json::array> results)
{
  auto run_obj = ::make_unique<sarif_run> ();

  /* "tool" property (SARIF v2.1.0 section 3.14.6).  */
  run_obj->set<sarif_tool> ("tool", make_tool_object ());

  /* "taxonomies" property (SARIF v2.1.0 section 3.14.8).  */
  if (auto taxonomies_arr = maybe_make_taxonomies_array ())
    run_obj->set<json::array> ("taxonomies", std::move (taxonomies_arr));

  /* "invocations" property (SARIF v2.1.0 section 3.14.11).  */
  {
    auto invocations_arr = ::make_unique<json::array> ();
    invocations_arr->append (std::move (invocation_obj));
    run_obj->set<json::array> ("invocations", std::move (invocations_arr));
  }

  /* "originalUriBaseIds (SARIF v2.1.0 section 3.14.14).  */
  if (m_seen_any_relative_paths)
    {
      auto orig_uri_base_ids = ::make_unique<json::object> ();
      orig_uri_base_ids->set<sarif_artifact_location>
	(PWD_PROPERTY_NAME, make_artifact_location_object_for_pwd ());
      run_obj->set<json::object> ("originalUriBaseIds",
				  std::move (orig_uri_base_ids));
    }

  /* "artifacts" property (SARIF v2.1.0 section 3.14.15).  */
  auto artifacts_arr = ::make_unique<json::array> ();
  for (auto iter : m_filename_to_artifact_map)
    {
      sarif_artifact *artifact_obj = iter.second;
      if (artifact_obj->embed_contents_p ())
	artifact_obj->populate_contents (*this);
      artifact_obj->populate_roles ();
      artifacts_arr->append (artifact_obj);
    }
  run_obj->set<json::array> ("artifacts", std::move (artifacts_arr));
  m_filename_to_artifact_map.empty ();

  /* "results" property (SARIF v2.1.0 section 3.14.23).  */
  run_obj->set<json::array> ("results", std::move (results));

  return run_obj;
}

/* Make a "tool" object (SARIF v2.1.0 section 3.18).  */

std::unique_ptr<sarif_tool>
sarif_builder::make_tool_object ()
{
  auto tool_obj = ::make_unique<sarif_tool> ();

  /* "driver" property (SARIF v2.1.0 section 3.18.2).  */
  tool_obj->set<sarif_tool_component> ("driver",
				       make_driver_tool_component_object ());

  /* Report plugins via the "extensions" property
     (SARIF v2.1.0 section 3.18.3).  */
  if (auto client_data_hooks = m_context.get_client_data_hooks ())
    if (const client_version_info *vinfo
	  = client_data_hooks->get_any_version_info ())
      {
	class my_plugin_visitor : public client_version_info :: plugin_visitor
	{
	public:
	  void on_plugin (const diagnostic_client_plugin_info &p) final override
	  {
	    /* Create a "toolComponent" object (SARIF v2.1.0 section 3.19)
	       for the plugin.  */
	    auto plugin_obj = ::make_unique<sarif_tool_component> ();

	    /* "name" property (SARIF v2.1.0 section 3.19.8).  */
	    if (const char *short_name = p.get_short_name ())
	      plugin_obj->set_string ("name", short_name);

	    /* "fullName" property (SARIF v2.1.0 section 3.19.9).  */
	    if (const char *full_name = p.get_full_name ())
	      plugin_obj->set_string ("fullName", full_name);

	    /* "version" property (SARIF v2.1.0 section 3.19.13).  */
	    if (const char *version = p.get_version ())
	      plugin_obj->set_string ("version", version);

	    m_plugin_objs.push_back (std::move (plugin_obj));
	  }
	  std::vector<std::unique_ptr<sarif_tool_component>> m_plugin_objs;
	};
	my_plugin_visitor v;
	vinfo->for_each_plugin (v);
	if (v.m_plugin_objs.size () > 0)
	  {
	    auto extensions_arr = ::make_unique<json::array> ();
	    for (auto &iter : v.m_plugin_objs)
	      extensions_arr->append<sarif_tool_component> (std::move (iter));
	    tool_obj->set<json::array> ("extensions",
					std::move (extensions_arr));
	  }
      }

  /* Perhaps we could also show GMP, MPFR, MPC, isl versions as other
     "extensions" (see toplev.cc: print_version).  */

  return tool_obj;
}

/* Make a "toolComponent" object (SARIF v2.1.0 section 3.19) for what SARIF
   calls the "driver" (see SARIF v2.1.0 section 3.18.1).  */

std::unique_ptr<sarif_tool_component>
sarif_builder::make_driver_tool_component_object ()
{
  auto driver_obj = ::make_unique<sarif_tool_component> ();

  if (auto client_data_hooks = m_context.get_client_data_hooks ())
    if (const client_version_info *vinfo
	  = client_data_hooks->get_any_version_info ())
      {
	/* "name" property (SARIF v2.1.0 section 3.19.8).  */
	if (const char *name = vinfo->get_tool_name ())
	  driver_obj->set_string ("name", name);

	/* "fullName" property (SARIF v2.1.0 section 3.19.9).  */
	if (char *full_name = vinfo->maybe_make_full_name ())
	  {
	    driver_obj->set_string ("fullName", full_name);
	    free (full_name);
	  }

	/* "version" property (SARIF v2.1.0 section 3.19.13).  */
	if (const char *version = vinfo->get_version_string ())
	  driver_obj->set_string ("version", version);

	/* "informationUri" property (SARIF v2.1.0 section 3.19.17).  */
	if (char *version_url =  vinfo->maybe_make_version_url ())
	  {
	    driver_obj->set_string ("informationUri", version_url);
	    free (version_url);
	  }
      }

  /* "rules" property (SARIF v2.1.0 section 3.19.23).  */
  driver_obj->set<json::array> ("rules", std::move (m_rules_arr));

  return driver_obj;
}

/* If we've seen any CWE IDs, make an array for the "taxonomies" property
   (SARIF v2.1.0 section 3.14.8) of a run object, containing a single
   "toolComponent" (3.19) as per 3.19.3, representing the CWE.

   Otherwise return nullptr.  */

std::unique_ptr<json::array>
sarif_builder::maybe_make_taxonomies_array () const
{
  auto cwe_obj = maybe_make_cwe_taxonomy_object ();
  if (!cwe_obj)
    return nullptr;

  /* "taxonomies" property (SARIF v2.1.0 section 3.14.8).  */
  auto taxonomies_arr = ::make_unique<json::array> ();
  taxonomies_arr->append<sarif_tool_component> (std::move (cwe_obj));
  return taxonomies_arr;
}

/* If we've seen any CWE IDs, make a "toolComponent" object
   (SARIF v2.1.0 section 3.19) representing the CWE taxonomy, as per 3.19.3.
   Populate the "taxa" property with all of the CWE IDs in m_cwe_id_set.

   Otherwise return nullptr.  */

std::unique_ptr<sarif_tool_component>
sarif_builder::maybe_make_cwe_taxonomy_object () const
{
  if (m_cwe_id_set.is_empty ())
    return nullptr;

  auto taxonomy_obj = ::make_unique<sarif_tool_component> ();

  /* "name" property (SARIF v2.1.0 section 3.19.8).  */
  taxonomy_obj->set_string ("name", "CWE");

  /* "version" property (SARIF v2.1.0 section 3.19.13).  */
  taxonomy_obj->set_string ("version", "4.7");

  /* "organization" property (SARIF v2.1.0 section 3.19.18).  */
  taxonomy_obj->set_string ("organization", "MITRE");

  /* "shortDescription" property (SARIF v2.1.0 section 3.19.19).  */
  taxonomy_obj->set<sarif_multiformat_message_string>
    ("shortDescription",
     make_multiformat_message_string ("The MITRE"
				      " Common Weakness Enumeration"));

  /* "taxa" property (SARIF v2.1.0 3.section 3.19.25).  */
  auto taxa_arr = ::make_unique<json::array> ();
  for (auto cwe_id : m_cwe_id_set)
    taxa_arr->append<sarif_reporting_descriptor>
      (make_reporting_descriptor_object_for_cwe_id (cwe_id));
  taxonomy_obj->set<json::array> ("taxa", std::move (taxa_arr));

  return taxonomy_obj;
}

/* Ensure that we have an "artifact" object (SARIF v2.1.0 section 3.24)
   for FILENAME, adding it to m_filename_to_artifact_map if not already
   found, and adding ROLE to it.
   If EMBED_CONTENTS is true, then flag that we will attempt to embed the
   contents of this artifact when writing it out.  */

sarif_artifact &
sarif_builder::get_or_create_artifact (const char *filename,
				       enum diagnostic_artifact_role role,
				       bool embed_contents)
{
  if (auto *slot = m_filename_to_artifact_map.get (filename))
    {
      (*slot)->add_role (role, embed_contents);
      return **slot;
    }

  sarif_artifact *artifact_obj = new sarif_artifact (filename);
  artifact_obj->add_role (role, embed_contents);
  m_filename_to_artifact_map.put (filename, artifact_obj);

  /* "location" property (SARIF v2.1.0 section 3.24.2).  */
  artifact_obj->set<sarif_artifact_location>
    ("location", make_artifact_location_object (filename));

  /* "sourceLanguage" property (SARIF v2.1.0 section 3.24.10).  */
  switch (role)
    {
    default:
      gcc_unreachable ();
    case diagnostic_artifact_role::analysis_target:
    case diagnostic_artifact_role::result_file:
    case diagnostic_artifact_role::scanned_file:
    case diagnostic_artifact_role::traced_file:
      /* Assume that these are in the source language.  */
      if (auto client_data_hooks = m_context.get_client_data_hooks ())
	if (const char *source_lang
	    = client_data_hooks->maybe_get_sarif_source_language (filename))
	  artifact_obj->set_string ("sourceLanguage", source_lang);
      break;

    case diagnostic_artifact_role::debug_output_file:
      /* Assume that these are not in the source language.  */
      break;
    }

  return *artifact_obj;
}

/* Make an "artifactContent" object (SARIF v2.1.0 section 3.3) for the
   full contents of FILENAME.  */

std::unique_ptr<sarif_artifact_content>
sarif_builder::maybe_make_artifact_content_object (const char *filename) const
{
  /* Let input.cc handle any charset conversion.  */
  char_span utf8_content
    = m_context.get_file_cache ().get_source_file_content (filename);
  if (!utf8_content)
    return nullptr;

  /* Don't add it if it's not valid UTF-8.  */
  if (!cpp_valid_utf8_p(utf8_content.get_buffer (), utf8_content.length ()))
    return nullptr;

  auto artifact_content_obj = ::make_unique<sarif_artifact_content> ();
  artifact_content_obj->set<json::string>
    ("text",
     ::make_unique <json::string> (utf8_content.get_buffer (),
				   utf8_content.length ()));
  return artifact_content_obj;
}

/* Attempt to read the given range of lines from FILENAME; return
   a freshly-allocated 0-terminated buffer containing them, or nullptr.  */

char *
sarif_builder::get_source_lines (const char *filename,
				 int start_line,
				 int end_line) const
{
  auto_vec<char> result;

  for (int line = start_line; line <= end_line; line++)
    {
      char_span line_content
	= m_context.get_file_cache ().get_source_line (filename, line);
      if (!line_content.get_buffer ())
	return nullptr;
      result.reserve (line_content.length () + 1);
      for (size_t i = 0; i < line_content.length (); i++)
	result.quick_push (line_content[i]);
      result.quick_push ('\n');
    }
  result.safe_push ('\0');

  return xstrdup (result.address ());
}

/* Make an "artifactContent" object (SARIF v2.1.0 section 3.3) for the given
   run of lines within FILENAME (including the endpoints).
   If R is non-NULL, use it to potentially set the "rendered"
   property (3.3.4).  */

std::unique_ptr<sarif_artifact_content>
sarif_builder::
maybe_make_artifact_content_object (const char *filename,
				    int start_line,
				    int end_line,
				    const content_renderer *r) const
{
  char *text_utf8 = get_source_lines (filename, start_line, end_line);

  if (!text_utf8)
    return nullptr;

  /* Don't add it if it's not valid UTF-8.  */
  if (!cpp_valid_utf8_p(text_utf8, strlen(text_utf8)))
    {
      free (text_utf8);
      return nullptr;
    }

  auto artifact_content_obj = ::make_unique<sarif_artifact_content> ();
  artifact_content_obj->set_string ("text", text_utf8);
  free (text_utf8);

  /* 3.3.4 "rendered" property.  */
  if (r)
    if (std::unique_ptr<sarif_multiformat_message_string> rendered
	  = r->render (*this))
      artifact_content_obj->set ("rendered", std::move (rendered));

  return artifact_content_obj;
}

/* Make a "fix" object (SARIF v2.1.0 section 3.55) for RICHLOC.  */

std::unique_ptr<sarif_fix>
sarif_builder::make_fix_object (const rich_location &richloc)
{
  auto fix_obj = ::make_unique<sarif_fix> ();

  /* "artifactChanges" property (SARIF v2.1.0 section 3.55.3).  */
  /* We assume that all fix-it hints in RICHLOC affect the same file.  */
  auto artifact_change_arr = ::make_unique<json::array> ();
  artifact_change_arr->append<sarif_artifact_change>
    (make_artifact_change_object (richloc));
  fix_obj->set<json::array> ("artifactChanges",
			     std::move (artifact_change_arr));

  return fix_obj;
}

/* Make an "artifactChange" object (SARIF v2.1.0 section 3.56) for RICHLOC.  */

std::unique_ptr<sarif_artifact_change>
sarif_builder::make_artifact_change_object (const rich_location &richloc)
{
  auto artifact_change_obj = ::make_unique<sarif_artifact_change> ();

  /* "artifactLocation" property (SARIF v2.1.0 section 3.56.2).  */
  artifact_change_obj->set<sarif_artifact_location>
    ("artifactLocation",
     make_artifact_location_object (richloc.get_loc ()));

  /* "replacements" property (SARIF v2.1.0 section 3.56.3).  */
  auto replacement_arr = ::make_unique<json::array> ();
  for (unsigned int i = 0; i < richloc.get_num_fixit_hints (); i++)
    {
      const fixit_hint *hint = richloc.get_fixit_hint (i);
      replacement_arr->append<sarif_replacement>
	(make_replacement_object (*hint));
    }
  artifact_change_obj->set<json::array> ("replacements",
					 std::move (replacement_arr));

  return artifact_change_obj;
}

/* Make a "replacement" object (SARIF v2.1.0 section 3.57) for HINT.  */

std::unique_ptr<sarif_replacement>
sarif_builder::make_replacement_object (const fixit_hint &hint) const
{
  auto replacement_obj = ::make_unique<sarif_replacement> ();

  /* "deletedRegion" property (SARIF v2.1.0 section 3.57.3).  */
  replacement_obj->set<sarif_region> ("deletedRegion",
				      make_region_object_for_hint (hint));

  /* "insertedContent" property (SARIF v2.1.0 section 3.57.4).  */
  replacement_obj->set<sarif_artifact_content>
    ("insertedContent",
     make_artifact_content_object (hint.get_string ()));

  return replacement_obj;
}

/* Make an "artifactContent" object (SARIF v2.1.0 section 3.3) for TEXT.  */

std::unique_ptr<sarif_artifact_content>
sarif_builder::make_artifact_content_object (const char *text) const
{
  auto content_obj = ::make_unique<sarif_artifact_content> ();

  /* "text" property (SARIF v2.1.0 section 3.3.2).  */
  content_obj->set_string ("text", text);

  return content_obj;
}

/* class diagnostic_sarif_format_buffer : public diagnostic_per_format_buffer.  */

void
diagnostic_sarif_format_buffer::dump (FILE *out, int indent) const
{
  fprintf (out, "%*sdiagnostic_sarif_format_buffer:\n", indent, "");
  int idx = 0;
  for (auto &result : m_results)
    {
      fprintf (out, "%*sresult[%i]:\n", indent + 2, "", idx);
      result->dump (out, true);
      fprintf (out, "\n");
      ++idx;
    }
}

bool
diagnostic_sarif_format_buffer::empty_p () const
{
  return m_results.empty ();
}

void
diagnostic_sarif_format_buffer::move_to (diagnostic_per_format_buffer &base)
{
  diagnostic_sarif_format_buffer &dest
    = static_cast<diagnostic_sarif_format_buffer &> (base);
  for (auto &&result : m_results)
    dest.m_results.push_back (std::move (result));
  m_results.clear ();
}

void
diagnostic_sarif_format_buffer::clear ()
{
  m_results.clear ();
}

void
diagnostic_sarif_format_buffer::flush ()
{
  for (auto &&result : m_results)
    {
      result->process_worklist (m_builder);
      m_builder.m_results_array->append<sarif_result> (std::move (result));
    }
  m_results.clear ();
}

class sarif_output_format : public diagnostic_output_format
{
public:
  ~sarif_output_format ()
  {
    /* Any sarifResult objects should have been handled by now.
       If not, then something's gone wrong with diagnostic
       groupings.  */
    std::unique_ptr<sarif_result> pending_result
      = m_builder.take_current_result ();
    gcc_assert (!pending_result);
  }

  void dump (FILE *out, int indent) const override
  {
    fprintf (out, "%*ssarif_output_format\n", indent, "");
    diagnostic_output_format::dump (out, indent);
  }

  std::unique_ptr<diagnostic_per_format_buffer>
  make_per_format_buffer () final override
  {
    return ::make_unique<diagnostic_sarif_format_buffer> (m_builder);
  }
  void set_buffer (diagnostic_per_format_buffer *base_buffer) final override
  {
    diagnostic_sarif_format_buffer *buffer
      = static_cast<diagnostic_sarif_format_buffer *> (base_buffer);
    m_buffer = buffer;
  }

  void on_begin_group () final override
  {
    /* No-op,  */
  }
  void on_end_group () final override
  {
    m_builder.end_group ();
  }
  void
  on_report_diagnostic (const diagnostic_info &diagnostic,
			diagnostic_t orig_diag_kind) final override
  {
    m_builder.on_report_diagnostic (diagnostic, orig_diag_kind, m_buffer);
  }
  void on_diagram (const diagnostic_diagram &diagram) final override
  {
    m_builder.emit_diagram (diagram);
  }
  void after_diagnostic (const diagnostic_info &) final override
  {
    /* No-op.  */
  }

  sarif_builder &get_builder () { return m_builder; }

  size_t num_results () const { return m_builder.num_results (); }
  sarif_result &get_result (size_t idx) { return m_builder.get_result (idx); }

protected:
  sarif_output_format (diagnostic_context &context,
		       const line_maps *line_maps,
		       const char *main_input_filename_,
		       bool formatted,
		       enum sarif_version version)
  : diagnostic_output_format (context),
    m_builder (context, line_maps, main_input_filename_, formatted, version),
    m_buffer (nullptr)
  {}

  sarif_builder m_builder;
  diagnostic_sarif_format_buffer *m_buffer;
};

class sarif_stream_output_format : public sarif_output_format
{
public:
  sarif_stream_output_format (diagnostic_context &context,
			      const line_maps *line_maps,
			      const char *main_input_filename_,
			      bool formatted,
			      enum sarif_version version,
			      FILE *stream)
  : sarif_output_format (context, line_maps, main_input_filename_,
			 formatted, version),
    m_stream (stream)
  {
  }
  ~sarif_stream_output_format ()
  {
    m_builder.flush_to_file (m_stream);
  }
  bool machine_readable_stderr_p () const final override
  {
    return m_stream == stderr;
  }
private:
  FILE *m_stream;
};

class sarif_file_output_format : public sarif_output_format
{
public:
  sarif_file_output_format (diagnostic_context &context,
			    const line_maps *line_maps,
			    const char *main_input_filename_,
			    bool formatted,
			    enum sarif_version version,
			    diagnostic_output_file output_file)
  : sarif_output_format (context, line_maps, main_input_filename_,
			 formatted, version),
    m_output_file (std::move (output_file))
  {
    gcc_assert (m_output_file.get_open_file ());
    gcc_assert (m_output_file.get_filename ());
  }
  ~sarif_file_output_format ()
  {
    m_builder.flush_to_file (m_output_file.get_open_file ());
  }
  void dump (FILE *out, int indent) const override
  {
    fprintf (out, "%*ssarif_file_output_format: %s\n",
	     indent, "",
	     m_output_file.get_filename ());
    diagnostic_output_format::dump (out, indent);
  }
  bool machine_readable_stderr_p () const final override
  {
    return false;
  }

private:
  diagnostic_output_file m_output_file;
};

/* Print the start of an embedded link to PP, as per 3.11.6.  */

static void
sarif_begin_embedded_link (pretty_printer *pp)
{
  pp_character (pp, '[');
}

/* Print the end of an embedded link to PP, as per 3.11.6.  */

static void
sarif_end_embedded_link (pretty_printer *pp,
			 const char *url)
{
  pp_string (pp, "](");
  /* TODO: does the URI need escaping?
     See https://github.com/oasis-tcs/sarif-spec/issues/657 */
  pp_string (pp, url);
  pp_character (pp, ')');
}

/* class sarif_token_printer : public token_printer.  */

/* Implementation of pretty_printer::token_printer for SARIF output.
   Emit URLs as per 3.11.6 ("Messages with embedded links").  */

void
sarif_builder::sarif_token_printer::print_tokens (pretty_printer *pp,
						  const pp_token_list &tokens)
{
  /* Convert to text, possibly with colorization, URLs, etc.  */
  label_text current_url;
  for (auto iter = tokens.m_first; iter; iter = iter->m_next)
    switch (iter->m_kind)
      {
      default:
	gcc_unreachable ();

      case pp_token::kind::text:
	{
	  const pp_token_text *sub = as_a <const pp_token_text *> (iter);
	  const char * const str = sub->m_value.get ();
	  if (current_url.get ())
	    {
	      /* Write iter->m_value, but escaping any
		 escaped link characters as per 3.11.6.  */
	      for (const char *ptr = str; *ptr; ptr++)
		{
		  const char ch = *ptr;
		  switch (ch)
		    {
		    default:
		      pp_character (pp, ch);
		      break;
		    case '\\':
		    case '[':
		    case ']':
		      pp_character (pp, '\\');
		      pp_character (pp, ch);
		      break;
		    }
		}
	    }
	  else
	    /* TODO: is other escaping needed? (e.g. of '[')
	       See https://github.com/oasis-tcs/sarif-spec/issues/658 */
	    pp_string (pp, str);
	}
	break;

      case pp_token::kind::begin_color:
      case pp_token::kind::end_color:
	/* These are no-ops.  */
	break;

      case pp_token::kind::begin_quote:
	pp_begin_quote (pp, pp_show_color (pp));
	break;
      case pp_token::kind::end_quote:
	pp_end_quote (pp, pp_show_color (pp));
	break;

      /* Emit URLs as per 3.11.6 ("Messages with embedded links").  */
      case pp_token::kind::begin_url:
	{
	  pp_token_begin_url *sub = as_a <pp_token_begin_url *> (iter);
	  sarif_begin_embedded_link (pp);
	  current_url = std::move (sub->m_value);
	}
	break;
      case pp_token::kind::end_url:
	gcc_assert (current_url.get ());
	sarif_end_embedded_link (pp, current_url.get ());
	current_url = label_text::borrow (nullptr);
	break;

      case pp_token::kind::event_id:
	{
	  pp_token_event_id *sub = as_a <pp_token_event_id *> (iter);
	  gcc_assert (sub->m_event_id.known_p ());
	  const sarif_code_flow *code_flow
	    = m_builder.get_code_flow_for_event_ids ();
	  label_text url = make_sarif_url_for_event (code_flow,
						     sub->m_event_id);
	  if (url.get ())
	    sarif_begin_embedded_link (pp);
	  pp_character (pp, '(');
	  pp_decimal_int (pp, sub->m_event_id.one_based ());
	  pp_character (pp, ')');
	  if (url.get ())
	    sarif_end_embedded_link (pp, url.get ());
	}
	break;
      }
}

/* Populate CONTEXT in preparation for SARIF output (either to stderr, or
   to a file).  */

static void
diagnostic_output_format_init_sarif (diagnostic_context &context,
				     std::unique_ptr<sarif_output_format> fmt)
{
  /* Suppress normal textual path output.  */
  context.set_path_format (DPF_NONE);

  /* Don't colorize the text.  */
  pp_show_color (fmt->get_printer ()) = false;
  context.set_show_highlight_colors (false);

  context.m_printer->set_token_printer
    (&fmt->get_builder ().get_token_printer ());
  context.set_output_format (std::move (fmt));
}

/* Populate CONTEXT in preparation for SARIF output to stderr.  */

void
diagnostic_output_format_init_sarif_stderr (diagnostic_context &context,
					    const line_maps *line_maps,
					    const char *main_input_filename_,
					    bool formatted,
					    enum sarif_version version)
{
  gcc_assert (line_maps);
  diagnostic_output_format_init_sarif
    (context,
     ::make_unique<sarif_stream_output_format> (context,
						line_maps,
						main_input_filename_,
						formatted,
						version,
						stderr));
}

/* Populate CONTEXT in preparation for SARIF output to a file named
   BASE_FILE_NAME.sarif.  */

void
diagnostic_output_format_init_sarif_file (diagnostic_context &context,
					  line_maps *line_maps,
					  const char *main_input_filename_,
					  bool formatted,
					  enum sarif_version version,
					  const char *base_file_name)
{
  gcc_assert (line_maps);

  if (!base_file_name)
    {
      rich_location richloc (line_maps, UNKNOWN_LOCATION);
      context.emit_diagnostic_with_group
	(DK_ERROR, richloc, nullptr, 0,
	 "unable to determine filename for SARIF output");
      return;
    }

  label_text filename = label_text::take (concat (base_file_name,
						  ".sarif",
						  nullptr));
  FILE *outf = fopen (filename.get (), "w");
  if (!outf)
    {
      rich_location richloc (line_maps, UNKNOWN_LOCATION);
      context.emit_diagnostic_with_group
	(DK_ERROR, richloc, nullptr, 0,
	 "unable to open %qs for SARIF output: %m",
	 filename.get ());
      return;
    }
  diagnostic_output_file output_file (outf, true, std::move (filename));
  diagnostic_output_format_init_sarif
    (context,
     ::make_unique<sarif_file_output_format> (context,
					      line_maps,
					      main_input_filename_,
					      formatted,
					      version,
					      std::move (output_file)));
}

/* Populate CONTEXT in preparation for SARIF output to STREAM.  */

void
diagnostic_output_format_init_sarif_stream (diagnostic_context &context,
					    const line_maps *line_maps,
					    const char *main_input_filename_,
					    bool formatted,
					    enum sarif_version version,
					    FILE *stream)
{
  gcc_assert (line_maps);
  diagnostic_output_format_init_sarif
    (context,
     ::make_unique<sarif_stream_output_format> (context,
						line_maps,
						main_input_filename_,
						formatted,
						version,
						stream));
}

#if CHECKING_P

namespace selftest {

/* A subclass of sarif_output_format for writing selftests.
   The JSON output is cached internally, rather than written
   out to a file.  */

class test_sarif_diagnostic_context : public test_diagnostic_context
{
public:
  test_sarif_diagnostic_context (const char *main_input_filename,
				 enum sarif_version version)
  {
    auto format = ::make_unique<buffered_output_format> (*this,
							 line_table,
							 main_input_filename,
							 true,
							 version);
    m_format = format.get (); // borrowed
    diagnostic_output_format_init_sarif (*this, std::move (format));
  }

  std::unique_ptr<sarif_log> flush_to_object ()
  {
    return m_format->flush_to_object ();
  }

  size_t num_results () const { return m_format->num_results (); }
  sarif_result &get_result (size_t idx) { return m_format->get_result (idx); }

private:
  class buffered_output_format : public sarif_output_format
  {
  public:
    buffered_output_format (diagnostic_context &context,
			    const line_maps *line_maps,
			    const char *main_input_filename_,
			    bool formatted,
			    enum sarif_version version)
    : sarif_output_format (context, line_maps, main_input_filename_,
			   formatted, version)
    {
    }
    bool machine_readable_stderr_p () const final override
    {
      return false;
    }
    std::unique_ptr<sarif_log> flush_to_object ()
    {
      return m_builder.flush_to_object ();
    }
  };

  buffered_output_format *m_format; // borrowed
};

/* Test making a sarif_location for a complex rich_location
   with labels and escape-on-output.  */

static void
test_make_location_object (const line_table_case &case_,
			   enum sarif_version version)
{
  diagnostic_show_locus_fixture_one_liner_utf8 f (case_);
  location_t line_end = linemap_position_for_column (line_table, 31);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  test_diagnostic_context dc;

  sarif_builder builder (dc, line_table, "MAIN_INPUT_FILENAME", true, version);

  /* These "columns" are byte offsets, whereas later on the columns
     in the generated SARIF use sarif_builder::get_sarif_column and
     thus respect tabs, encoding.  */
  const location_t foo
    = make_location (linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 8));
  const location_t bar
    = make_location (linemap_position_for_column (line_table, 12),
		     linemap_position_for_column (line_table, 12),
		     linemap_position_for_column (line_table, 17));
  const location_t field
    = make_location (linemap_position_for_column (line_table, 19),
		     linemap_position_for_column (line_table, 19),
		     linemap_position_for_column (line_table, 30));

  text_range_label label0 ("label0");
  text_range_label label1 ("label1");
  text_range_label label2 ("label2");

  rich_location richloc (line_table, foo, &label0, nullptr);
  richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
  richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label2);
  richloc.set_escape_on_output (true);

  sarif_result result (0);

  std::unique_ptr<sarif_location> location_obj
    = builder.make_location_object
    (result, richloc, nullptr, diagnostic_artifact_role::analysis_target);
  ASSERT_NE (location_obj, nullptr);

  auto physical_location
    = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (location_obj.get (),
					       "physicalLocation");
  {
    auto region
      = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (physical_location, "region");
    ASSERT_JSON_INT_PROPERTY_EQ (region, "startLine", 1);
    ASSERT_JSON_INT_PROPERTY_EQ (region, "startColumn", 1);
    ASSERT_JSON_INT_PROPERTY_EQ (region, "endColumn", 7);
  }
  {
    auto context_region
      = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (physical_location,
						 "contextRegion");
    ASSERT_JSON_INT_PROPERTY_EQ (context_region, "startLine", 1);

    {
      auto snippet
	= EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (context_region, "snippet");

      /* We expect the snippet's "text" to be a copy of the content.  */
      ASSERT_JSON_STRING_PROPERTY_EQ (snippet, "text",  f.m_content);

      /* We expect the snippet to have a "rendered" whose "text" has a
	 pure ASCII escaped copy of the line (with labels, etc).  */
      {
	auto rendered
	  = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (snippet, "rendered");
	ASSERT_JSON_STRING_PROPERTY_EQ
	  (rendered, "text",
	   "1 | <U+1F602>_foo = <U+03C0>_bar.<U+1F602>_field<U+03C0>;\n"
	   "  | ^~~~~~~~~~~~~   ~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~\n"
	   "  | |               |            |\n"
	   "  | label0          label1       label2\n");
      }
    }
  }
  auto annotations
    = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (location_obj.get (),
					      "annotations");
  ASSERT_EQ (annotations->size (), 3);
  {
    {
      auto a0 = (*annotations)[0];
      ASSERT_JSON_INT_PROPERTY_EQ (a0, "startLine", 1);
      ASSERT_JSON_INT_PROPERTY_EQ (a0, "startColumn", 1);
      ASSERT_JSON_INT_PROPERTY_EQ (a0, "endColumn", 7);
      auto message
	= EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (a0, "message");
      ASSERT_JSON_STRING_PROPERTY_EQ (message, "text", "label0");
    }
    {
      auto a1 = (*annotations)[1];
      ASSERT_JSON_INT_PROPERTY_EQ (a1, "startLine", 1);
      ASSERT_JSON_INT_PROPERTY_EQ (a1, "startColumn", 10);
      ASSERT_JSON_INT_PROPERTY_EQ (a1, "endColumn", 15);
      auto message
	= EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (a1, "message");
      ASSERT_JSON_STRING_PROPERTY_EQ (message, "text", "label1");
    }
    {
      auto a2 = (*annotations)[2];
      ASSERT_JSON_INT_PROPERTY_EQ (a2, "startLine", 1);
      ASSERT_JSON_INT_PROPERTY_EQ (a2, "startColumn", 16);
      ASSERT_JSON_INT_PROPERTY_EQ (a2, "endColumn", 25);
      auto message
	= EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (a2, "message");
      ASSERT_JSON_STRING_PROPERTY_EQ (message, "text", "label2");
    }
  }
}

/* Test of reporting a diagnostic at UNKNOWN_LOCATION to a
   diagnostic_context and examining the generated sarif_log.
   Verify various basic properties. */

static void
test_simple_log (enum sarif_version version)
{
  test_sarif_diagnostic_context dc ("MAIN_INPUT_FILENAME", version);

  rich_location richloc (line_table, UNKNOWN_LOCATION);
  dc.report (DK_ERROR, richloc, nullptr, 0, "this is a test: %i", 42);

  auto log_ptr = dc.flush_to_object ();

  // 3.13 sarifLog:
  auto log = log_ptr.get ();
  ASSERT_JSON_STRING_PROPERTY_EQ (log, "$schema",
				  sarif_version_to_url (version));
  ASSERT_JSON_STRING_PROPERTY_EQ (log, "version",
				  sarif_version_to_property (version));

  auto runs = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (log, "runs"); // 3.13.4
  ASSERT_EQ (runs->size (), 1);

  // 3.14 "run" object:
  auto run = (*runs)[0];

  {
    // 3.14.6:
    auto tool = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (run, "tool");

    EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (tool, "driver"); // 3.18.2
  }

  {
    // 3.14.11
    auto invocations
      = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (run, "invocations");
    ASSERT_EQ (invocations->size (), 1);

    {
      // 3.20 "invocation" object:
      auto invocation = (*invocations)[0];

      // 3.20.3 arguments property

      // 3.20.7 startTimeUtc property
      EXPECT_JSON_OBJECT_WITH_STRING_PROPERTY (invocation, "startTimeUtc");

      // 3.20.8 endTimeUtc property
      EXPECT_JSON_OBJECT_WITH_STRING_PROPERTY (invocation, "endTimeUtc");

      // 3.20.19 workingDirectory property
      {
	auto wd_obj
	  = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (invocation,
						     "workingDirectory");
	EXPECT_JSON_OBJECT_WITH_STRING_PROPERTY (wd_obj, "uri");
      }

      // 3.20.21 toolExecutionNotifications property
      auto notifications
	= EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY
	    (invocation, "toolExecutionNotifications");
      ASSERT_EQ (notifications->size (), 0);
    }
  }

  {
    // 3.14.15:
    auto artifacts = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (run, "artifacts");
    ASSERT_EQ (artifacts->size (), 1);

    {
      // 3.24 "artifact" object:
      auto artifact = (*artifacts)[0];

      // 3.24.2:
      auto location
	= EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (artifact, "location");
      ASSERT_JSON_STRING_PROPERTY_EQ (location, "uri", "MAIN_INPUT_FILENAME");

      // 3.24.6:
      auto roles = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (artifact, "roles");
      ASSERT_EQ (roles->size (), 1);
      {
	auto role = (*roles)[0];
	ASSERT_JSON_STRING_EQ (role, "analysisTarget");
      }
    }
  }

  {
    // 3.14.23:
    auto results = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (run, "results");
    ASSERT_EQ (results->size (), 1);

    {
      // 3.27 "result" object:
      auto result = (*results)[0];
      ASSERT_JSON_STRING_PROPERTY_EQ (result, "ruleId", "error");
      ASSERT_JSON_STRING_PROPERTY_EQ (result, "level", "error"); // 3.27.10

      {
	// 3.27.11:
	auto message
	  = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (result, "message");
	ASSERT_JSON_STRING_PROPERTY_EQ (message, "text",
					"this is a test: 42");
      }

      // 3.27.12:
      auto locations
	= EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (result, "locations");
      ASSERT_EQ (locations->size (), 0);
    }
  }
}

/* As above, but with a "real" location_t.  */

static void
test_simple_log_2 (const line_table_case &case_,
		   enum sarif_version version)
{
  auto_fix_quotes fix_quotes;

  const char *const content
    /* 000000000111111
       123456789012345.  */
    = "unsinged int i;\n";
  diagnostic_show_locus_fixture f (case_, content);
  location_t line_end = linemap_position_for_column (line_table, 31);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  test_sarif_diagnostic_context dc (f.get_filename (), version);

  const location_t typo_loc
    = make_location (linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 8));

  rich_location richloc (line_table, typo_loc);
  dc.report (DK_ERROR, richloc, nullptr, 0,
	     "did you misspell %qs again?",
	     "unsigned");

  auto log_ptr = dc.flush_to_object ();

  // 3.13 sarifLog:
  auto log = log_ptr.get ();

  auto runs = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (log, "runs"); // 3.13.4
  ASSERT_EQ (runs->size (), 1);

  // 3.14 "run" object:
  auto run = (*runs)[0];

  {
    // 3.14.23:
    auto results = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (run, "results");
    ASSERT_EQ (results->size (), 1);

    {
      // 3.27 "result" object:
      auto result = (*results)[0];
      ASSERT_JSON_STRING_PROPERTY_EQ (result, "ruleId", "error");
      ASSERT_JSON_STRING_PROPERTY_EQ (result, "level", "error"); // 3.27.10

      {
	// 3.27.11:
	auto message
	  = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (result, "message");
	ASSERT_JSON_STRING_PROPERTY_EQ (message, "text",
					"did you misspell `unsigned' again?");
      }

      // 3.27.12:
      auto locations
	= EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (result, "locations");
      ASSERT_EQ (locations->size (), 1);

      {
	// 3.28 "location" object:
	auto location = (*locations)[0];

	auto physical_location
	  = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (location,
						     "physicalLocation");
	{
	  auto region
	    = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (physical_location,
						       "region");
	  ASSERT_JSON_INT_PROPERTY_EQ (region, "startLine", 1);
	  ASSERT_JSON_INT_PROPERTY_EQ (region, "startColumn", 1);
	  ASSERT_JSON_INT_PROPERTY_EQ (region, "endColumn", 9);
	}
	{
	  auto context_region
	    = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (physical_location,
						       "contextRegion");
	  ASSERT_JSON_INT_PROPERTY_EQ (context_region, "startLine", 1);

	  {
	    auto snippet
	      = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (context_region,
							 "snippet");

	    /* We expect the snippet's "text" to be a copy of the content.  */
	    ASSERT_JSON_STRING_PROPERTY_EQ (snippet, "text",  f.m_content);
	  }
	}
      }
    }
  }
}

/* Assuming that a single diagnostic has been emitted within
   LOG, get a json::object for the result object.  */

static const json::object *
get_result_from_log (const sarif_log *log)
{
  auto runs = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (log, "runs"); // 3.13.4
  ASSERT_EQ (runs->size (), 1);

  // 3.14 "run" object:
  auto run = (*runs)[0];

  // 3.14.23:
  auto results = EXPECT_JSON_OBJECT_WITH_ARRAY_PROPERTY (run, "results");
  ASSERT_EQ (results->size (), 1);

  // 3.27 "result" object:
  auto result = (*results)[0];
  return expect_json_object (SELFTEST_LOCATION, result);
}

static const json::object *
get_message_from_result (const sarif_result &result)
{
  // 3.27.11:
  auto message_obj
    = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (&result, "message");
  return message_obj;
}

/* Assuming that a single diagnostic has been emitted to
   DC, get a json::object for the messsage object within
   the result.  */

static const json::object *
get_message_from_log (const sarif_log *log)
{
  auto result_obj = get_result_from_log (log);

  // 3.27.11:
  auto message_obj
    = EXPECT_JSON_OBJECT_WITH_OBJECT_PROPERTY (result_obj, "message");
  return message_obj;
}

/* Tests of messages with embedded links; see SARIF v2.1.0 3.11.6.  */

static void
test_message_with_embedded_link (enum sarif_version version)
{
  auto_fix_quotes fix_quotes;
  {
    test_sarif_diagnostic_context dc ("test.c", version);
    rich_location richloc (line_table, UNKNOWN_LOCATION);
    dc.report (DK_ERROR, richloc, nullptr, 0,
	       "before %{text%} after",
	       "http://example.com");
    std::unique_ptr<sarif_log> log = dc.flush_to_object ();

    auto message_obj = get_message_from_log (log.get ());
    ASSERT_JSON_STRING_PROPERTY_EQ
      (message_obj, "text",
       "before [text](http://example.com) after");
  }

  /* Escaping in message text.
     This is "EXAMPLE 1" from 3.11.6.  */
  {
    test_sarif_diagnostic_context dc ("test.c", version);
    rich_location richloc (line_table, UNKNOWN_LOCATION);

    /* Disable "unquoted sequence of 2 consecutive punctuation
       characters `]\' in format" warning.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif
    dc.report (DK_ERROR, richloc, nullptr, 0,
	       "Prohibited term used in %{para[0]\\spans[2]%}.",
	       "1");
#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

    std::unique_ptr<sarif_log> log = dc.flush_to_object ();

    auto message_obj = get_message_from_log (log.get ());
    ASSERT_JSON_STRING_PROPERTY_EQ
      (message_obj, "text",
       "Prohibited term used in [para\\[0\\]\\\\spans\\[2\\]](1).");
    /* This isn't exactly what EXAMPLE 1 of the spec has; reported as
       https://github.com/oasis-tcs/sarif-spec/issues/656  */
  }

  /* Urlifier.  */
  {
    class test_urlifier : public urlifier
    {
    public:
      char *
      get_url_for_quoted_text (const char *p, size_t sz) const final override
      {
	if (!strncmp (p, "-foption", sz))
	  return xstrdup ("http://example.com");
	return nullptr;
      }
    };

    test_sarif_diagnostic_context dc ("test.c", version);
    dc.set_urlifier (::make_unique<test_urlifier> ());
    rich_location richloc (line_table, UNKNOWN_LOCATION);
    dc.report (DK_ERROR, richloc, nullptr, 0,
	       "foo %<-foption%> %<unrecognized%> bar");
    std::unique_ptr<sarif_log> log = dc.flush_to_object ();

    auto message_obj = get_message_from_log (log.get ());
    ASSERT_JSON_STRING_PROPERTY_EQ
      (message_obj, "text",
       "foo `[-foption](http://example.com)' `unrecognized' bar");
  }
}

static void
test_buffering (enum sarif_version version)
{
  test_sarif_diagnostic_context dc ("test.c", version);

  diagnostic_buffer buf_a (dc);
  diagnostic_buffer buf_b (dc);

  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  ASSERT_EQ (dc.diagnostic_count (DK_ERROR), 0);
  ASSERT_EQ (buf_a.diagnostic_count (DK_ERROR), 0);
  ASSERT_EQ (buf_b.diagnostic_count (DK_ERROR), 0);
  ASSERT_EQ (dc.num_results (), 0);
  ASSERT_TRUE (buf_a.empty_p ());
  ASSERT_TRUE (buf_b.empty_p ());

  /* Unbuffered diagnostic.  */
  {
    dc.report (DK_ERROR, rich_loc, nullptr, 0,
	       "message 1");

    ASSERT_EQ (dc.diagnostic_count (DK_ERROR), 1);
    ASSERT_EQ (buf_a.diagnostic_count (DK_ERROR), 0);
    ASSERT_EQ (buf_b.diagnostic_count (DK_ERROR), 0);
    ASSERT_EQ (dc.num_results (), 1);
    sarif_result &result_obj = dc.get_result (0);
    auto message_obj = get_message_from_result (result_obj);
    ASSERT_JSON_STRING_PROPERTY_EQ (message_obj, "text",
				    "message 1");
    ASSERT_TRUE (buf_a.empty_p ());
    ASSERT_TRUE (buf_b.empty_p ());
  }

  /* Buffer diagnostic into buffer A.  */
  {
    dc.set_diagnostic_buffer (&buf_a);
    dc.report (DK_ERROR, rich_loc, nullptr, 0,
	       "message in buffer a");
    ASSERT_EQ (dc.diagnostic_count (DK_ERROR), 1);
    ASSERT_EQ (buf_a.diagnostic_count (DK_ERROR), 1);
    ASSERT_EQ (buf_b.diagnostic_count (DK_ERROR), 0);
    ASSERT_EQ (dc.num_results (), 1);
    ASSERT_FALSE (buf_a.empty_p ());
    ASSERT_TRUE (buf_b.empty_p ());
  }

  /* Buffer diagnostic into buffer B.  */
  {
    dc.set_diagnostic_buffer (&buf_b);
    dc.report (DK_ERROR, rich_loc, nullptr, 0,
	       "message in buffer b");
    ASSERT_EQ (dc.diagnostic_count (DK_ERROR), 1);
    ASSERT_EQ (buf_a.diagnostic_count (DK_ERROR), 1);
    ASSERT_EQ (buf_b.diagnostic_count (DK_ERROR), 1);
    ASSERT_EQ (dc.num_results (), 1);
    ASSERT_FALSE (buf_a.empty_p ());
    ASSERT_FALSE (buf_b.empty_p ());
  }

  /* Flush buffer B to dc.  */
  {
    dc.flush_diagnostic_buffer (buf_b);
    ASSERT_EQ (dc.diagnostic_count (DK_ERROR), 2);
    ASSERT_EQ (buf_a.diagnostic_count (DK_ERROR), 1);
    ASSERT_EQ (buf_b.diagnostic_count (DK_ERROR), 0);
    ASSERT_EQ (dc.num_results (), 2);
    sarif_result &result_1_obj = dc.get_result (1);
    auto message_1_obj = get_message_from_result (result_1_obj);
    ASSERT_JSON_STRING_PROPERTY_EQ (message_1_obj, "text",
				    "message in buffer b");
    ASSERT_FALSE (buf_a.empty_p ());
    ASSERT_TRUE (buf_b.empty_p ());
  }

  /* Clear buffer A.  */
  {
    dc.clear_diagnostic_buffer (buf_a);
    ASSERT_EQ (dc.diagnostic_count (DK_ERROR), 2);
    ASSERT_EQ (buf_a.diagnostic_count (DK_ERROR), 0);
    ASSERT_EQ (buf_b.diagnostic_count (DK_ERROR), 0);
    ASSERT_EQ (dc.num_results (), 2);
    ASSERT_TRUE (buf_a.empty_p ());
    ASSERT_TRUE (buf_b.empty_p ());
  }
}

static void
run_tests_per_version (const line_table_case &case_)
{
  for (int version_idx = 0;
       version_idx < (int)sarif_version::num_versions;
       ++version_idx)
    {
      enum sarif_version version
	= static_cast<enum sarif_version> (version_idx);

      test_make_location_object (case_, version);
      test_simple_log_2 (case_, version);
    }
}

/* Run all of the selftests within this file.  */

void
diagnostic_format_sarif_cc_tests ()
{
  for (int version_idx = 0;
       version_idx < (int)sarif_version::num_versions;
       ++version_idx)
    {
      enum sarif_version version
	= static_cast<enum sarif_version> (version_idx);

      test_simple_log (version);
      test_message_with_embedded_link (version);
      test_buffering (version);
    }

  /* Run tests per (line-table-case, SARIF version) pair.  */
  for_each_line_table_case (run_tests_per_version);
}

} // namespace selftest

#endif /* CHECKING_P */
