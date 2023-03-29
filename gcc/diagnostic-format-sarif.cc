/* SARIF output for diagnostics
   Copyright (C) 2018-2023 Free Software Foundation, Inc.
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
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "diagnostic-metadata.h"
#include "diagnostic-path.h"
#include "json.h"
#include "cpplib.h"
#include "logical-location.h"
#include "diagnostic-client-data-hooks.h"

class sarif_builder;

/* Subclass of json::object for SARIF invocation objects
   (SARIF v2.1.0 section 3.20).  */

class sarif_invocation : public json::object
{
public:
  sarif_invocation ()
  : m_notifications_arr (new json::array ()),
    m_success (true)
  {}

  void add_notification_for_ice (diagnostic_context *context,
				 diagnostic_info *diagnostic,
				 sarif_builder *builder);
  void prepare_to_flush ();

private:
  json::array *m_notifications_arr;
  bool m_success;
};

/* Subclass of json::object for SARIF result objects
   (SARIF v2.1.0 section 3.27).  */

class sarif_result : public json::object
{
public:
  sarif_result () : m_related_locations_arr (NULL) {}

  void
  on_nested_diagnostic (diagnostic_context *context,
			diagnostic_info *diagnostic,
			diagnostic_t orig_diag_kind,
			sarif_builder *builder);

private:
  json::array *m_related_locations_arr;
};

/* Subclass of json::object for SARIF notification objects
   (SARIF v2.1.0 section 3.58).

   This subclass is specifically for notifying when an
   internal compiler error occurs.  */

class sarif_ice_notification : public json::object
{
public:
  sarif_ice_notification (diagnostic_context *context,
			  diagnostic_info *diagnostic,
			  sarif_builder *builder);
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

   Known limitations:
   - GCC supports one-deep nesting of diagnostics (via auto_diagnostic_group),
     but we only capture location and message information from such nested
     diagnostics (e.g. we ignore fix-it hints on them)
   - doesn't yet capture command-line arguments: would be run.invocations
     property (SARIF v2.1.0 section 3.14.11), as invocation objects
     (SARIF v2.1.0 section 3.20), but we'd want to capture the arguments to
     toplev::main, and the response files.
   - doesn't capture escape_on_output_p
   - doesn't capture secondary locations within a rich_location
     (perhaps we should use the "relatedLocations" property: SARIF v2.1.0
     section 3.27.22)
   - doesn't capture "artifact.encoding" property
     (SARIF v2.1.0 section 3.24.9).
   - doesn't capture hashes of the source files
     ("artifact.hashes" property (SARIF v2.1.0 section 3.24.11).
   - doesn't capture the "analysisTarget" property
     (SARIF v2.1.0 section 3.27.13).
   - doesn't capture labelled ranges
   - doesn't capture -Werror cleanly
   - doesn't capture inlining information (can SARIF handle this?)
   - doesn't capture macro expansion information (can SARIF handle this?).  */

class sarif_builder
{
public:
  sarif_builder (diagnostic_context *context);

  void end_diagnostic (diagnostic_context *context, diagnostic_info *diagnostic,
		       diagnostic_t orig_diag_kind);

  void end_group ();

  void flush_to_file (FILE *outf);

  json::array *make_locations_arr (diagnostic_info *diagnostic);
  json::object *make_location_object (const rich_location &rich_loc,
				      const logical_location *logical_loc);
  json::object *make_message_object (const char *msg) const;

private:
  sarif_result *make_result_object (diagnostic_context *context,
				    diagnostic_info *diagnostic,
				    diagnostic_t orig_diag_kind);
  void set_any_logical_locs_arr (json::object *location_obj,
				 const logical_location *logical_loc);
  json::object *make_location_object (const diagnostic_event &event);
  json::object *
  make_logical_location_object (const logical_location &logical_loc) const;
  json::object *make_code_flow_object (const diagnostic_path &path);
  json::object *make_thread_flow_object (const diagnostic_path &path);
  json::object *
  make_thread_flow_location_object (const diagnostic_event &event);
  json::array *maybe_make_kinds_array (diagnostic_event::meaning m) const;
  json::object *maybe_make_physical_location_object (location_t loc);
  json::object *make_artifact_location_object (location_t loc);
  json::object *make_artifact_location_object (const char *filename);
  json::object *make_artifact_location_object_for_pwd () const;
  json::object *maybe_make_region_object (location_t loc) const;
  json::object *maybe_make_region_object_for_context (location_t loc) const;
  json::object *make_region_object_for_hint (const fixit_hint &hint) const;
  json::object *make_multiformat_message_string (const char *msg) const;
  json::object *make_top_level_object (sarif_invocation *invocation_obj,
				       json::array *results);
  json::object *make_run_object (sarif_invocation *invocation_obj,
				 json::array *results);
  json::object *make_tool_object () const;
  json::object *make_driver_tool_component_object () const;
  json::array *maybe_make_taxonomies_array () const;
  json::object *maybe_make_cwe_taxonomy_object () const;
  json::object *make_tool_component_reference_object_for_cwe () const;
  json::object *
  make_reporting_descriptor_object_for_warning (diagnostic_context *context,
						diagnostic_info *diagnostic,
						diagnostic_t orig_diag_kind,
						const char *option_text);
  json::object *make_reporting_descriptor_object_for_cwe_id (int cwe_id) const;
  json::object *
  make_reporting_descriptor_reference_object_for_cwe_id (int cwe_id);
  json::object *make_artifact_object (const char *filename);
  json::object *maybe_make_artifact_content_object (const char *filename) const;
  json::object *maybe_make_artifact_content_object (const char *filename,
						    int start_line,
						    int end_line) const;
  json::object *make_fix_object (const rich_location &rich_loc);
  json::object *make_artifact_change_object (const rich_location &richloc);
  json::object *make_replacement_object (const fixit_hint &hint) const;
  json::object *make_artifact_content_object (const char *text) const;
  int get_sarif_column (expanded_location exploc) const;

  diagnostic_context *m_context;

  /* The JSON object for the invocation object.  */
  sarif_invocation *m_invocation_obj;

  /* The JSON array of pending diagnostics.  */
  json::array *m_results_array;

  /* The JSON object for the result object (if any) in the current
     diagnostic group.  */
  sarif_result *m_cur_group_result;

  hash_set <const char *> m_filenames;
  bool m_seen_any_relative_paths;
  hash_set <free_string_hash> m_rule_id_set;
  json::array *m_rules_arr;

  /* The set of all CWE IDs we've seen, if any.  */
  hash_set <int_hash <int, 0, 1> > m_cwe_id_set;

  int m_tabstop;
};

static sarif_builder *the_builder;

/* class sarif_invocation : public json::object.  */

/* Handle an internal compiler error DIAGNOSTIC occurring on CONTEXT.
   Add an object representing the ICE to the notifications array.  */

void
sarif_invocation::add_notification_for_ice (diagnostic_context *context,
					    diagnostic_info *diagnostic,
					    sarif_builder *builder)
{
  m_success = false;

  sarif_ice_notification *notification_obj
    = new sarif_ice_notification (context, diagnostic, builder);
  m_notifications_arr->append (notification_obj);
}

void
sarif_invocation::prepare_to_flush ()
{
  /* "executionSuccessful" property (SARIF v2.1.0 section 3.20.14).  */
  set ("executionSuccessful", new json::literal (m_success));

  /* "toolExecutionNotifications" property (SARIF v2.1.0 section 3.20.21).  */
  set ("toolExecutionNotifications", m_notifications_arr);
}

/* class sarif_result : public json::object.  */

/* Handle secondary diagnostics that occur within a diagnostic group.
   The closest SARIF seems to have to nested diagnostics is the
   "relatedLocations" property of result objects (SARIF v2.1.0 section 3.27.22),
   so we lazily set this property and populate the array if and when
   secondary diagnostics occur (such as notes to a warning).  */

void
sarif_result::on_nested_diagnostic (diagnostic_context *context,
				    diagnostic_info *diagnostic,
				    diagnostic_t /*orig_diag_kind*/,
				    sarif_builder *builder)
{
  if (!m_related_locations_arr)
    {
      m_related_locations_arr = new json::array ();
      set ("relatedLocations", m_related_locations_arr);
    }

  /* We don't yet generate meaningful logical locations for notes;
     sometimes these will related to current_function_decl, but
     often they won't.  */
  json::object *location_obj
    = builder->make_location_object (*diagnostic->richloc, NULL);
  json::object *message_obj
    = builder->make_message_object (pp_formatted_text (context->printer));
  pp_clear_output_area (context->printer);
  location_obj->set ("message", message_obj);

  m_related_locations_arr->append (location_obj);
}

/* class sarif_ice_notification : public json::object.  */

/* sarif_ice_notification's ctor.
   DIAGNOSTIC is an internal compiler error.  */

sarif_ice_notification::sarif_ice_notification (diagnostic_context *context,
						diagnostic_info *diagnostic,
						sarif_builder *builder)
{
  /* "locations" property (SARIF v2.1.0 section 3.58.4).  */
  json::array *locations_arr = builder->make_locations_arr (diagnostic);
  set ("locations", locations_arr);

  /* "message" property (SARIF v2.1.0 section 3.85.5).  */
  json::object *message_obj
    = builder->make_message_object (pp_formatted_text (context->printer));
  pp_clear_output_area (context->printer);
  set ("message", message_obj);

  /* "level" property (SARIF v2.1.0 section 3.58.6).  */
  set ("level", new json::string ("error"));
}

/* class sarif_builder.  */

/* sarif_builder's ctor.  */

sarif_builder::sarif_builder (diagnostic_context *context)
: m_context (context),
  m_invocation_obj (new sarif_invocation ()),
  m_results_array (new json::array ()),
  m_cur_group_result (NULL),
  m_seen_any_relative_paths (false),
  m_rule_id_set (),
  m_rules_arr (new json::array ()),
  m_tabstop (context->tabstop)
{
}

/* Implementation of "end_diagnostic" for SARIF output.  */

void
sarif_builder::end_diagnostic (diagnostic_context *context,
			       diagnostic_info *diagnostic,
			       diagnostic_t orig_diag_kind)
{
  if (diagnostic->kind == DK_ICE || diagnostic->kind == DK_ICE_NOBT)
    {
      m_invocation_obj->add_notification_for_ice (context, diagnostic, this);
      return;
    }

  if (m_cur_group_result)
    /* Nested diagnostic.  */
    m_cur_group_result->on_nested_diagnostic (context,
					      diagnostic,
					      orig_diag_kind,
					      this);
  else
    {
      /* Top-level diagnostic.  */
      sarif_result *result_obj
	= make_result_object (context, diagnostic, orig_diag_kind);
      m_results_array->append (result_obj);
      m_cur_group_result = result_obj;
    }
}

/* Implementation of "end_group_cb" for SARIF output.  */

void
sarif_builder::end_group ()
{
  m_cur_group_result = NULL;
}

/* Create a top-level object, and add it to all the results
   (and other entities) we've seen so far.

   Flush it all to OUTF.  */

void
sarif_builder::flush_to_file (FILE *outf)
{
  m_invocation_obj->prepare_to_flush ();
  json::object *top = make_top_level_object (m_invocation_obj, m_results_array);
  top->dump (outf);
  m_invocation_obj = NULL;
  m_results_array = NULL;
  fprintf (outf, "\n");
  delete top;
}

/* Attempt to convert DIAG_KIND to a suitable value for the "level"
   property (SARIF v2.1.0 section 3.27.10).

   Return NULL if there isn't one.  */

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
      return NULL;
    }
}

/* Make a string for DIAG_KIND suitable for use a ruleId
   (SARIF v2.1.0 section 3.27.5) as a fallback for when we don't
   have anything better to use.  */

static char *
make_rule_id_for_diagnostic_kind (diagnostic_t diag_kind)
{
  static const char *const diagnostic_kind_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (T),
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
    "must-not-happen"
  };
  /* Lose the trailing ": ".  */
  const char *kind_text = diagnostic_kind_text[diag_kind];
  size_t len = strlen (kind_text);
  gcc_assert (len > 2);
  gcc_assert (kind_text[len - 2] == ':');
  gcc_assert (kind_text[len - 1] == ' ');
  char *rstrip = xstrdup (kind_text);
  rstrip[len - 2] = '\0';
  return rstrip;
}

/* Make a result object (SARIF v2.1.0 section 3.27) for DIAGNOSTIC.  */

sarif_result *
sarif_builder::make_result_object (diagnostic_context *context,
				   diagnostic_info *diagnostic,
				   diagnostic_t orig_diag_kind)
{
  sarif_result *result_obj = new sarif_result ();

  /* "ruleId" property (SARIF v2.1.0 section 3.27.5).  */
  /* Ideally we'd have an option_name for these.  */
  if (char *option_text
	= context->option_name (context, diagnostic->option_index,
				orig_diag_kind, diagnostic->kind))
    {
      /* Lazily create reportingDescriptor objects for and add to m_rules_arr.
	 Set ruleId referencing them.  */
      result_obj->set ("ruleId", new json::string (option_text));
      if (m_rule_id_set.contains (option_text))
	free (option_text);
      else
	{
	  /* This is the first time we've seen this ruleId.  */
	  /* Add to set, taking ownership.  */
	  m_rule_id_set.add (option_text);

	  json::object *reporting_desc_obj
	    = make_reporting_descriptor_object_for_warning (context,
							    diagnostic,
							    orig_diag_kind,
							    option_text);
	  m_rules_arr->append (reporting_desc_obj);
	}
    }
  else
    {
      /* Otherwise, we have an "error" or a stray "note"; use the
	 diagnostic kind as the ruleId, so that the result object at least
	 has a ruleId.
	 We don't bother creating reportingDescriptor objects for these.  */
      char *rule_id = make_rule_id_for_diagnostic_kind (orig_diag_kind);
      result_obj->set ("ruleId", new json::string (rule_id));
      free (rule_id);
    }

  /* "taxa" property (SARIF v2.1.0 section 3.27.8).  */
  if (diagnostic->metadata)
    if (int cwe_id = diagnostic->metadata->get_cwe ())
      {
	json::array *taxa_arr = new json::array ();
	json::object *cwe_id_obj
	  = make_reporting_descriptor_reference_object_for_cwe_id (cwe_id);
	taxa_arr->append (cwe_id_obj);
	result_obj->set ("taxa", taxa_arr);
      }

  /* "level" property (SARIF v2.1.0 section 3.27.10).  */
  if (const char *sarif_level = maybe_get_sarif_level (diagnostic->kind))
    result_obj->set ("level", new json::string (sarif_level));

  /* "message" property (SARIF v2.1.0 section 3.27.11).  */
  json::object *message_obj
    = make_message_object (pp_formatted_text (context->printer));
  pp_clear_output_area (context->printer);
  result_obj->set ("message", message_obj);

  /* "locations" property (SARIF v2.1.0 section 3.27.12).  */
  json::array *locations_arr = make_locations_arr (diagnostic);
  result_obj->set ("locations", locations_arr);

  /* "codeFlows" property (SARIF v2.1.0 section 3.27.18).  */
  if (const diagnostic_path *path = diagnostic->richloc->get_path ())
    {
      json::array *code_flows_arr = new json::array ();
      json::object *code_flow_obj = make_code_flow_object (*path);
      code_flows_arr->append (code_flow_obj);
      result_obj->set ("codeFlows", code_flows_arr);
    }

  /* The "relatedLocations" property (SARIF v2.1.0 section 3.27.22) is
     set up later, if any nested diagnostics occur within this diagnostic
     group.  */

  /* "fixes" property (SARIF v2.1.0 section 3.27.30).  */
  const rich_location *richloc = diagnostic->richloc;
  if (richloc->get_num_fixit_hints ())
    {
      json::array *fix_arr = new json::array ();
      json::object *fix_obj = make_fix_object (*richloc);
      fix_arr->append (fix_obj);
      result_obj->set ("fixes", fix_arr);
    }

  return result_obj;
}

/* Make a reportingDescriptor object (SARIF v2.1.0 section 3.49)
   for a GCC warning.  */

json::object *
sarif_builder::
make_reporting_descriptor_object_for_warning (diagnostic_context *context,
					      diagnostic_info *diagnostic,
					      diagnostic_t /*orig_diag_kind*/,
					      const char *option_text)
{
  json::object *reporting_desc = new json::object ();

  /* "id" property (SARIF v2.1.0 section 3.49.3).  */
  reporting_desc->set ("id", new json::string (option_text));

  /* We don't implement "name" property (SARIF v2.1.0 section 3.49.7), since
     it seems redundant compared to "id".  */

  /* "helpUri" property (SARIF v2.1.0 section 3.49.12).  */
  if (context->get_option_url)
    {
      char *option_url
	= context->get_option_url (context, diagnostic->option_index);
      if (option_url)
	{
	  reporting_desc->set ("helpUri", new json::string (option_url));
	  free (option_url);
	}
    }

  return reporting_desc;
}

/* Make a reportingDescriptor object (SARIF v2.1.0 section 3.49)
   for CWE_ID, for use within the CWE taxa array.  */

json::object *
sarif_builder::make_reporting_descriptor_object_for_cwe_id (int cwe_id) const
{
  json::object *reporting_desc = new json::object ();

  /* "id" property (SARIF v2.1.0 section 3.49.3).  */
  {
    pretty_printer pp;
    pp_printf (&pp, "%i", cwe_id);
    reporting_desc->set ("id", new json::string (pp_formatted_text (&pp)));
  }

  /* "helpUri" property (SARIF v2.1.0 section 3.49.12).  */
  {
    char *url = get_cwe_url (cwe_id);
    reporting_desc->set ("helpUri", new json::string (url));
    free (url);
  }

  return reporting_desc;
}

/* Make a reportingDescriptorReference object (SARIF v2.1.0 section 3.52)
   referencing CWE_ID, for use within a result object.
   Also, add CWE_ID to m_cwe_id_set.  */

json::object *
sarif_builder::
make_reporting_descriptor_reference_object_for_cwe_id (int cwe_id)
{
  json::object *desc_ref_obj = new json::object ();

  /* "id" property (SARIF v2.1.0 section 3.52.4).  */
  {
    pretty_printer pp;
    pp_printf (&pp, "%i", cwe_id);
    desc_ref_obj->set ("id", new json::string (pp_formatted_text (&pp)));
  }

  /* "toolComponent" property (SARIF v2.1.0 section 3.52.7).  */
  json::object *comp_ref_obj = make_tool_component_reference_object_for_cwe ();
  desc_ref_obj->set ("toolComponent", comp_ref_obj);

  /* Add CWE_ID to our set.  */
  gcc_assert (cwe_id > 0);
  m_cwe_id_set.add (cwe_id);

  return desc_ref_obj;
}

/* Make a toolComponentReference object (SARIF v2.1.0 section 3.54) that
   references the CWE taxonomy.  */

json::object *
sarif_builder::
make_tool_component_reference_object_for_cwe () const
{
  json::object *comp_ref_obj = new json::object ();

  /* "name" property  (SARIF v2.1.0 section 3.54.3).  */
  comp_ref_obj->set ("name", new json::string ("cwe"));

  return comp_ref_obj;
}

/* Make an array suitable for use as the "locations" property of:
   - a "result" object (SARIF v2.1.0 section 3.27.12), or
   - a "notification" object (SARIF v2.1.0 section 3.58.4).  */

json::array *
sarif_builder::make_locations_arr (diagnostic_info *diagnostic)
{
  json::array *locations_arr = new json::array ();
  const logical_location *logical_loc = NULL;
  if (m_context->m_client_data_hooks)
    logical_loc
      = m_context->m_client_data_hooks->get_current_logical_location ();

  json::object *location_obj
    = make_location_object (*diagnostic->richloc, logical_loc);
  locations_arr->append (location_obj);
  return locations_arr;
}

/* If LOGICAL_LOC is non-NULL, use it to create a "logicalLocations" property
   within LOCATION_OBJ (SARIF v2.1.0 section 3.28.4).  */

void
sarif_builder::
set_any_logical_locs_arr (json::object *location_obj,
			  const logical_location *logical_loc)
{
  if (!logical_loc)
    return;
  json::object *logical_loc_obj = make_logical_location_object (*logical_loc);
  json::array *location_locs_arr = new json::array ();
  location_locs_arr->append (logical_loc_obj);
  location_obj->set ("logicalLocations", location_locs_arr);
}

/* Make a location object (SARIF v2.1.0 section 3.28) for RICH_LOC
   and LOGICAL_LOC.  */

json::object *
sarif_builder::make_location_object (const rich_location &rich_loc,
				     const logical_location *logical_loc)
{
  json::object *location_obj = new json::object ();

  /* Get primary loc from RICH_LOC.  */
  location_t loc = rich_loc.get_loc ();

  /* "physicalLocation" property (SARIF v2.1.0 section 3.28.3).  */
  if (json::object *phs_loc_obj = maybe_make_physical_location_object (loc))
    location_obj->set ("physicalLocation", phs_loc_obj);

  /* "logicalLocations" property (SARIF v2.1.0 section 3.28.4).  */
  set_any_logical_locs_arr (location_obj, logical_loc);

  return location_obj;
}

/* Make a location object (SARIF v2.1.0 section 3.28) for EVENT
   within a diagnostic_path.  */

json::object *
sarif_builder::make_location_object (const diagnostic_event &event)
{
  json::object *location_obj = new json::object ();

  /* "physicalLocation" property (SARIF v2.1.0 section 3.28.3).  */
  location_t loc = event.get_location ();
  if (json::object *phs_loc_obj = maybe_make_physical_location_object (loc))
    location_obj->set ("physicalLocation", phs_loc_obj);

  /* "logicalLocations" property (SARIF v2.1.0 section 3.28.4).  */
  const logical_location *logical_loc = event.get_logical_location ();
  set_any_logical_locs_arr (location_obj, logical_loc);

  /* "message" property (SARIF v2.1.0 section 3.28.5).  */
  label_text ev_desc = event.get_desc (false);
  json::object *message_obj = make_message_object (ev_desc.get ());
  location_obj->set ("message", message_obj);

  return location_obj;
}

/* Make a physicalLocation object (SARIF v2.1.0 section 3.29) for LOC,
   or return NULL;
   Add any filename to the m_artifacts.  */

json::object *
sarif_builder::maybe_make_physical_location_object (location_t loc)
{
  if (loc <= BUILTINS_LOCATION || LOCATION_FILE (loc) == NULL)
    return NULL;

  json::object *phys_loc_obj = new json::object ();

  /* "artifactLocation" property (SARIF v2.1.0 section 3.29.3).  */
  json::object *artifact_loc_obj = make_artifact_location_object (loc);
  phys_loc_obj->set ("artifactLocation", artifact_loc_obj);
  m_filenames.add (LOCATION_FILE (loc));

  /* "region" property (SARIF v2.1.0 section 3.29.4).  */
  if (json::object *region_obj = maybe_make_region_object (loc))
    phys_loc_obj->set ("region", region_obj);

  /* "contextRegion" property (SARIF v2.1.0 section 3.29.5).  */
  if (json::object *context_region_obj
	= maybe_make_region_object_for_context (loc))
    phys_loc_obj->set ("contextRegion", context_region_obj);

  /* Instead, we add artifacts to the run as a whole,
     with artifact.contents.
     Could do both, though.  */

  return phys_loc_obj;
}

/* Make an artifactLocation object (SARIF v2.1.0 section 3.4) for LOC,
   or return NULL.  */

json::object *
sarif_builder::make_artifact_location_object (location_t loc)
{
  return make_artifact_location_object (LOCATION_FILE (loc));
}

/* The ID value for use in "uriBaseId" properties (SARIF v2.1.0 section 3.4.4)
   for when we need to express paths relative to PWD.  */

#define PWD_PROPERTY_NAME ("PWD")

/* Make an artifactLocation object (SARIF v2.1.0 section 3.4) for FILENAME,
   or return NULL.  */

json::object *
sarif_builder::make_artifact_location_object (const char *filename)
{
  json::object *artifact_loc_obj = new json::object ();

  /* "uri" property (SARIF v2.1.0 section 3.4.3).  */
  artifact_loc_obj->set ("uri", new json::string (filename));

  if (filename[0] != '/')
    {
      /* If we have a relative path, set the "uriBaseId" property
	 (SARIF v2.1.0 section 3.4.4).  */
      artifact_loc_obj->set ("uriBaseId", new json::string (PWD_PROPERTY_NAME));
      m_seen_any_relative_paths = true;
    }

  return artifact_loc_obj;
}

/* Get the PWD, or NULL, as an absolute file-based URI,
   adding a trailing forward slash (as required by SARIF v2.1.0
   section 3.14.14).  */

static char *
make_pwd_uri_str ()
{
  /* The prefix of a file-based URI, up to, but not including the path. */
#define FILE_PREFIX ("file://")

  const char *pwd = getpwd ();
  if (!pwd)
    return NULL;
  size_t len = strlen (pwd);
  if (len == 0 || pwd[len - 1] != '/')
    return concat (FILE_PREFIX, pwd, "/", NULL);
  else
    {
      gcc_assert (pwd[len - 1] == '/');
      return concat (FILE_PREFIX, pwd, NULL);
    }
}

/* Make an artifactLocation object (SARIF v2.1.0 section 3.4) for the pwd,
   for use in the "run.originalUriBaseIds" property (SARIF v2.1.0
   section 3.14.14) when we have any relative paths.  */

json::object *
sarif_builder::make_artifact_location_object_for_pwd () const
{
  json::object *artifact_loc_obj = new json::object ();

  /* "uri" property (SARIF v2.1.0 section 3.4.3).  */
  if (char *pwd = make_pwd_uri_str ())
    {
      gcc_assert (strlen (pwd) > 0);
      gcc_assert (pwd[strlen (pwd) - 1] == '/');
      artifact_loc_obj->set ("uri", new json::string (pwd));
      free (pwd);
    }

  return artifact_loc_obj;
}

/* Get the column number within EXPLOC.  */

int
sarif_builder::get_sarif_column (expanded_location exploc) const
{
  cpp_char_column_policy policy (m_tabstop, cpp_wcwidth);
  return location_compute_display_column (exploc, policy);
}

/* Make a region object (SARIF v2.1.0 section 3.30) for LOC,
   or return NULL.  */

json::object *
sarif_builder::maybe_make_region_object (location_t loc) const
{
  location_t caret_loc = get_pure_location (loc);

  if (caret_loc <= BUILTINS_LOCATION)
    return NULL;

  location_t start_loc = get_start (loc);
  location_t finish_loc = get_finish (loc);

  expanded_location exploc_caret = expand_location (caret_loc);
  expanded_location exploc_start = expand_location (start_loc);
  expanded_location exploc_finish = expand_location (finish_loc);

  if (exploc_start.file !=exploc_caret.file)
    return NULL;
  if (exploc_finish.file !=exploc_caret.file)
    return NULL;

  json::object *region_obj = new json::object ();

  /* "startLine" property (SARIF v2.1.0 section 3.30.5) */
  region_obj->set ("startLine", new json::integer_number (exploc_start.line));

  /* "startColumn" property (SARIF v2.1.0 section 3.30.6) */
  region_obj->set ("startColumn",
		   new json::integer_number (get_sarif_column (exploc_start)));

  /* "endLine" property (SARIF v2.1.0 section 3.30.7) */
  if (exploc_finish.line != exploc_start.line)
    region_obj->set ("endLine", new json::integer_number (exploc_finish.line));

  /* "endColumn" property (SARIF v2.1.0 section 3.30.8).
     This expresses the column immediately beyond the range.  */
  {
    int next_column = sarif_builder::get_sarif_column (exploc_finish) + 1;
    region_obj->set ("endColumn", new json::integer_number (next_column));
  }

  return region_obj;
}

/* Make a region object (SARIF v2.1.0 section 3.30) for the "contextRegion"
   property (SARIF v2.1.0 section 3.29.5) of a physicalLocation.

   This is similar to maybe_make_region_object, but ignores column numbers,
   covering the line(s) as a whole, and including a "snippet" property
   embedding those source lines, making it easier for consumers to show
   the pertinent source.  */

json::object *
sarif_builder::maybe_make_region_object_for_context (location_t loc) const
{
  location_t caret_loc = get_pure_location (loc);

  if (caret_loc <= BUILTINS_LOCATION)
    return NULL;

  location_t start_loc = get_start (loc);
  location_t finish_loc = get_finish (loc);

  expanded_location exploc_caret = expand_location (caret_loc);
  expanded_location exploc_start = expand_location (start_loc);
  expanded_location exploc_finish = expand_location (finish_loc);

  if (exploc_start.file !=exploc_caret.file)
    return NULL;
  if (exploc_finish.file !=exploc_caret.file)
    return NULL;

  json::object *region_obj = new json::object ();

  /* "startLine" property (SARIF v2.1.0 section 3.30.5) */
  region_obj->set ("startLine", new json::integer_number (exploc_start.line));

  /* "endLine" property (SARIF v2.1.0 section 3.30.7) */
  if (exploc_finish.line != exploc_start.line)
    region_obj->set ("endLine", new json::integer_number (exploc_finish.line));

  /* "snippet" property (SARIF v2.1.0 section 3.30.13).  */
  if (json::object *artifact_content_obj
	 = maybe_make_artifact_content_object (exploc_start.file,
					       exploc_start.line,
					       exploc_finish.line))
    region_obj->set ("snippet", artifact_content_obj);

  return region_obj;
}

/* Make a region object (SARIF v2.1.0 section 3.30) for the deletion region
   of HINT (as per SARIF v2.1.0 section 3.57.3).  */

json::object *
sarif_builder::make_region_object_for_hint (const fixit_hint &hint) const
{
  location_t start_loc = hint.get_start_loc ();
  location_t next_loc = hint.get_next_loc ();

  expanded_location exploc_start = expand_location (start_loc);
  expanded_location exploc_next = expand_location (next_loc);

  json::object *region_obj = new json::object ();

  /* "startLine" property (SARIF v2.1.0 section 3.30.5) */
  region_obj->set ("startLine", new json::integer_number (exploc_start.line));

  /* "startColumn" property (SARIF v2.1.0 section 3.30.6) */
  int start_col = get_sarif_column (exploc_start);
  region_obj->set ("startColumn",
		   new json::integer_number (start_col));

  /* "endLine" property (SARIF v2.1.0 section 3.30.7) */
  if (exploc_next.line != exploc_start.line)
    region_obj->set ("endLine", new json::integer_number (exploc_next.line));

  /* "endColumn" property (SARIF v2.1.0 section 3.30.8).
     This expresses the column immediately beyond the range.  */
  int next_col =  get_sarif_column (exploc_next);
  region_obj->set ("endColumn", new json::integer_number (next_col));

  return region_obj;
}

/* Attempt to get a string for a logicalLocation's "kind" property
   (SARIF v2.1.0 section 3.33.7).
   Return NULL if unknown.  */

static const char *
maybe_get_sarif_kind (enum logical_location_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case LOGICAL_LOCATION_KIND_UNKNOWN:
      return NULL;

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

/* Make a logicalLocation object (SARIF v2.1.0 section 3.33) for LOGICAL_LOC,
   or return NULL.  */

json::object *
sarif_builder::
make_logical_location_object (const logical_location &logical_loc) const
{
  json::object *logical_loc_obj = new json::object ();

  /* "name" property (SARIF v2.1.0 section 3.33.4).  */
  if (const char *short_name = logical_loc.get_short_name ())
    logical_loc_obj->set ("name", new json::string (short_name));

  /* "fullyQualifiedName" property (SARIF v2.1.0 section 3.33.5).  */
  if (const char *name_with_scope = logical_loc.get_name_with_scope ())
    logical_loc_obj->set ("fullyQualifiedName",
			  new json::string (name_with_scope));

  /* "decoratedName" property (SARIF v2.1.0 section 3.33.6).  */
  if (const char *internal_name = logical_loc.get_internal_name ())
    logical_loc_obj->set ("decoratedName", new json::string (internal_name));

  /* "kind" property (SARIF v2.1.0 section 3.33.7).  */
  enum logical_location_kind kind = logical_loc.get_kind ();
  if (const char *sarif_kind_str = maybe_get_sarif_kind (kind))
    logical_loc_obj->set ("kind", new json::string (sarif_kind_str));

  return logical_loc_obj;
}

/* Make a codeFlow object (SARIF v2.1.0 section 3.36) for PATH.  */

json::object *
sarif_builder::make_code_flow_object (const diagnostic_path &path)
{
  json::object *code_flow_obj = new json::object ();

  /* "threadFlows" property (SARIF v2.1.0 section 3.36.3).
     Currently we only support one thread per result.  */
  json::array *thread_flows_arr = new json::array ();
  json::object *thread_flow_obj = make_thread_flow_object (path);
  thread_flows_arr->append (thread_flow_obj);
  code_flow_obj->set ("threadFlows", thread_flows_arr);

  return code_flow_obj;
}

/* Make a threadFlow object (SARIF v2.1.0 section 3.37) for PATH.  */

json::object *
sarif_builder::make_thread_flow_object (const diagnostic_path &path)
{
  json::object *thread_flow_obj = new json::object ();

  /* "locations" property (SARIF v2.1.0 section 3.37.6).  */
  json::array *locations_arr = new json::array ();
  for (unsigned i = 0; i < path.num_events (); i++)
    {
      const diagnostic_event &event = path.get_event (i);
      json::object *thread_flow_loc_obj
	= make_thread_flow_location_object (event);
      locations_arr->append (thread_flow_loc_obj);
    }
  thread_flow_obj->set ("locations", locations_arr);

  return thread_flow_obj;
}

/* Make a threadFlowLocation object (SARIF v2.1.0 section 3.38) for EVENT.  */

json::object *
sarif_builder::make_thread_flow_location_object (const diagnostic_event &ev)
{
  json::object *thread_flow_loc_obj = new json::object ();

  /* "location" property (SARIF v2.1.0 section 3.38.3).  */
  json::object *location_obj = make_location_object (ev);
  thread_flow_loc_obj->set ("location", location_obj);

  /* "kinds" property (SARIF v2.1.0 section 3.38.8).  */
  diagnostic_event::meaning m = ev.get_meaning ();
  if (json::array *kinds_arr = maybe_make_kinds_array (m))
    thread_flow_loc_obj->set ("kinds", kinds_arr);

  /* "nestingLevel" property (SARIF v2.1.0 section 3.38.10).  */
  thread_flow_loc_obj->set ("nestingLevel",
			    new json::integer_number (ev.get_stack_depth ()));

  /* It might be nice to eventually implement the following for -fanalyzer:
     - the "stack" property (SARIF v2.1.0 section 3.38.5)
     - the "state" property (SARIF v2.1.0 section 3.38.9)
     - the "importance" property (SARIF v2.1.0 section 3.38.13).  */

  return thread_flow_loc_obj;
}

/* If M has any known meaning, make a json array suitable for the "kinds"
   property of a threadFlowLocation object (SARIF v2.1.0 section 3.38.8).

   Otherwise, return NULL.  */

json::array *
sarif_builder::maybe_make_kinds_array (diagnostic_event::meaning m) const
{
  if (m.m_verb == diagnostic_event::VERB_unknown
      && m.m_noun == diagnostic_event::NOUN_unknown
      && m.m_property == diagnostic_event::PROPERTY_unknown)
    return NULL;

  json::array *kinds_arr = new json::array ();
  if (const char *verb_str
	= diagnostic_event::meaning::maybe_get_verb_str (m.m_verb))
    kinds_arr->append (new json::string (verb_str));
  if (const char *noun_str
	= diagnostic_event::meaning::maybe_get_noun_str (m.m_noun))
    kinds_arr->append (new json::string (noun_str));
  if (const char *property_str
	= diagnostic_event::meaning::maybe_get_property_str (m.m_property))
    kinds_arr->append (new json::string (property_str));
  return kinds_arr;
}

/* Make a message object (SARIF v2.1.0 section 3.11) for MSG.  */

json::object *
sarif_builder::make_message_object (const char *msg) const
{
  json::object *message_obj = new json::object ();

  /* "text" property (SARIF v2.1.0 section 3.11.8).  */
  message_obj->set ("text", new json::string (msg));

  return message_obj;
}

/* Make a multiformatMessageString object (SARIF v2.1.0 section 3.12)
   for MSG.  */

json::object *
sarif_builder::make_multiformat_message_string (const char *msg) const
{
  json::object *message_obj = new json::object ();

  /* "text" property (SARIF v2.1.0 section 3.12.3).  */
  message_obj->set ("text", new json::string (msg));

  return message_obj;
}

#define SARIF_SCHEMA "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"
#define SARIF_VERSION "2.1.0"

/* Make a top-level sarifLog object (SARIF v2.1.0 section 3.13).
   Take ownership of INVOCATION_OBJ and RESULTS.  */

json::object *
sarif_builder::make_top_level_object (sarif_invocation *invocation_obj,
				      json::array *results)
{
  json::object *log_obj = new json::object ();

  /* "$schema" property (SARIF v2.1.0 section 3.13.3) .  */
  log_obj->set ("$schema", new json::string (SARIF_SCHEMA));

  /* "version" property (SARIF v2.1.0 section 3.13.2).  */
  log_obj->set ("version", new json::string (SARIF_VERSION));

  /* "runs" property (SARIF v2.1.0 section 3.13.4).  */
  json::array *run_arr = new json::array ();
  json::object *run_obj = make_run_object (invocation_obj, results);
  run_arr->append (run_obj);
  log_obj->set ("runs", run_arr);

  return log_obj;
}

/* Make a run object (SARIF v2.1.0 section 3.14).
   Take ownership of INVOCATION_OBJ and RESULTS.  */

json::object *
sarif_builder::make_run_object (sarif_invocation *invocation_obj,
				json::array *results)
{
  json::object *run_obj = new json::object ();

  /* "tool" property (SARIF v2.1.0 section 3.14.6).  */
  json::object *tool_obj = make_tool_object ();
  run_obj->set ("tool", tool_obj);

  /* "taxonomies" property (SARIF v2.1.0 section 3.14.8).  */
  if (json::array *taxonomies_arr = maybe_make_taxonomies_array ())
    run_obj->set ("taxonomies", taxonomies_arr);

  /* "invocations" property (SARIF v2.1.0 section 3.14.11).  */
  {
    json::array *invocations_arr = new json::array ();
    invocations_arr->append (invocation_obj);
    run_obj->set ("invocations", invocations_arr);
  }

  /* "originalUriBaseIds (SARIF v2.1.0 section 3.14.14).  */
  if (m_seen_any_relative_paths)
    {
      json::object *orig_uri_base_ids = new json::object ();
      run_obj->set ("originalUriBaseIds", orig_uri_base_ids);
      json::object *pwd_art_loc_obj = make_artifact_location_object_for_pwd ();
      orig_uri_base_ids->set (PWD_PROPERTY_NAME, pwd_art_loc_obj);
    }

  /* "artifacts" property (SARIF v2.1.0 section 3.14.15).  */
  json::array *artifacts_arr = new json::array ();
  for (auto iter : m_filenames)
    {
      json::object *artifact_obj = make_artifact_object (iter);
      artifacts_arr->append (artifact_obj);
    }
  run_obj->set ("artifacts", artifacts_arr);

  /* "results" property (SARIF v2.1.0 section 3.14.23).  */
  run_obj->set ("results", results);

  return run_obj;
}

/* Make a tool object (SARIF v2.1.0 section 3.18).  */

json::object *
sarif_builder::make_tool_object () const
{
  json::object *tool_obj = new json::object ();

  /* "driver" property (SARIF v2.1.0 section 3.18.2).  */
  json::object *driver_obj = make_driver_tool_component_object ();
  tool_obj->set ("driver", driver_obj);

  /* Report plugins via the "extensions" property
     (SARIF v2.1.0 section 3.18.3).  */
  if (m_context->m_client_data_hooks)
    if (const client_version_info *vinfo
	  = m_context->m_client_data_hooks->get_any_version_info ())
      {
	class my_plugin_visitor : public client_version_info :: plugin_visitor
	{
	public:
	  void on_plugin (const diagnostic_client_plugin_info &p) final override
	  {
	    /* Create a toolComponent object (SARIF v2.1.0 section 3.19)
	       for the plugin.  */
	    json::object *plugin_obj = new json::object ();
	    m_plugin_objs.safe_push (plugin_obj);

	    /* "name" property (SARIF v2.1.0 section 3.19.8).  */
	    if (const char *short_name = p.get_short_name ())
	      plugin_obj->set ("name", new json::string (short_name));

	    /* "fullName" property (SARIF v2.1.0 section 3.19.9).  */
	    if (const char *full_name = p.get_full_name ())
	      plugin_obj->set ("fullName", new json::string (full_name));

	    /* "version" property (SARIF v2.1.0 section 3.19.13).  */
	    if (const char *version = p.get_version ())
	      plugin_obj->set ("version", new json::string (version));
	  }
	  auto_vec <json::object *> m_plugin_objs;
	};
	my_plugin_visitor v;
	vinfo->for_each_plugin (v);
	if (v.m_plugin_objs.length () > 0)
	  {
	    json::array *extensions_arr = new json::array ();
	    tool_obj->set ("extensions", extensions_arr);
	    for (auto iter : v.m_plugin_objs)
	      extensions_arr->append (iter);
	  }
      }

  /* Perhaps we could also show GMP, MPFR, MPC, isl versions as other
     "extensions" (see toplev.cc: print_version).  */

  return tool_obj;
}

/* Make a toolComponent object (SARIF v2.1.0 section 3.19) for what SARIF
   calls the "driver" (see SARIF v2.1.0 section 3.18.1).  */

json::object *
sarif_builder::make_driver_tool_component_object () const
{
  json::object *driver_obj = new json::object ();

  if (m_context->m_client_data_hooks)
    if (const client_version_info *vinfo
	  = m_context->m_client_data_hooks->get_any_version_info ())
      {
	/* "name" property (SARIF v2.1.0 section 3.19.8).  */
	if (const char *name = vinfo->get_tool_name ())
	  driver_obj->set ("name", new json::string (name));

	/* "fullName" property (SARIF v2.1.0 section 3.19.9).  */
	if (char *full_name = vinfo->maybe_make_full_name ())
	  {
	    driver_obj->set ("fullName", new json::string (full_name));
	    free (full_name);
	  }

	/* "version" property (SARIF v2.1.0 section 3.19.13).  */
	if (const char *version = vinfo->get_version_string ())
	  driver_obj->set ("version", new json::string (version));

	/* "informationUri" property (SARIF v2.1.0 section 3.19.17).  */
	if (char *version_url =  vinfo->maybe_make_version_url ())
	  {
	    driver_obj->set ("informationUri", new json::string (version_url));
	    free (version_url);
	  }
      }

  /* "rules" property (SARIF v2.1.0 section 3.19.23).  */
  driver_obj->set ("rules", m_rules_arr);

  return driver_obj;
}

/* If we've seen any CWE IDs, make an array for the "taxonomies" property
   (SARIF v2.1.0 section 3.14.8) of a run object, containting a singl
   toolComponent (3.19) as per 3.19.3, representing the CWE.

   Otherwise return NULL.  */

json::array *
sarif_builder::maybe_make_taxonomies_array () const
{
  json::object *cwe_obj = maybe_make_cwe_taxonomy_object ();
  if (!cwe_obj)
    return NULL;

  /* "taxonomies" property (SARIF v2.1.0 section 3.14.8).  */
  json::array *taxonomies_arr = new json::array ();
  taxonomies_arr->append (cwe_obj);
  return taxonomies_arr;
}

/* If we've seen any CWE IDs, make a toolComponent object
   (SARIF v2.1.0 section 3.19) representing the CWE taxonomy, as per 3.19.3.
   Populate the "taxa" property with all of the CWE IDs in m_cwe_id_set.

   Otherwise return NULL.  */

json::object *
sarif_builder::maybe_make_cwe_taxonomy_object () const
{
  if (m_cwe_id_set.is_empty ())
    return NULL;

  json::object *taxonomy_obj = new json::object ();

  /* "name" property (SARIF v2.1.0 section 3.19.8).  */
  taxonomy_obj->set ("name", new json::string ("CWE"));

  /* "version" property (SARIF v2.1.0 section 3.19.13).  */
  taxonomy_obj->set ("version", new json::string ("4.7"));

  /* "organization" property (SARIF v2.1.0 section 3.19.18).  */
  taxonomy_obj->set ("organization", new json::string ("MITRE"));

  /* "shortDescription" property (SARIF v2.1.0 section 3.19.19).  */
  json::object *short_desc
    = make_multiformat_message_string ("The MITRE"
				       " Common Weakness Enumeration");
  taxonomy_obj->set ("shortDescription", short_desc);

  /* "taxa" property (SARIF v2.1.0 3.section 3.19.25).  */
  json::array *taxa_arr = new json::array ();
  for (auto cwe_id : m_cwe_id_set)
    {
      json::object *cwe_taxon
	= make_reporting_descriptor_object_for_cwe_id (cwe_id);
      taxa_arr->append (cwe_taxon);
    }
  taxonomy_obj->set ("taxa", taxa_arr);

  return taxonomy_obj;
}

/* Make an artifact object (SARIF v2.1.0 section 3.24).  */

json::object *
sarif_builder::make_artifact_object (const char *filename)
{
  json::object *artifact_obj = new json::object ();

  /* "location" property (SARIF v2.1.0 section 3.24.2).  */
  json::object *artifact_loc_obj = make_artifact_location_object (filename);
  artifact_obj->set ("location", artifact_loc_obj);

  /* "contents" property (SARIF v2.1.0 section 3.24.8).  */
  if (json::object *artifact_content_obj
	= maybe_make_artifact_content_object (filename))
    artifact_obj->set ("contents", artifact_content_obj);

  /* "sourceLanguage" property (SARIF v2.1.0 section 3.24.10).  */
  if (m_context->m_client_data_hooks)
    if (const char *source_lang
	= m_context->m_client_data_hooks->maybe_get_sarif_source_language
	    (filename))
      artifact_obj->set ("sourceLanguage", new json::string (source_lang));

  return artifact_obj;
}

/* Make an artifactContent object (SARIF v2.1.0 section 3.3) for the
   full contents of FILENAME.  */

json::object *
sarif_builder::maybe_make_artifact_content_object (const char *filename) const
{
  /* Let input.cc handle any charset conversion.  */
  char_span utf8_content = get_source_file_content (filename);
  if (!utf8_content)
    return NULL;

  /* Don't add it if it's not valid UTF-8.  */
  if (!cpp_valid_utf8_p(utf8_content.get_buffer (), utf8_content.length ()))
    return NULL;

  json::object *artifact_content_obj = new json::object ();
  artifact_content_obj->set ("text",
			     new json::string (utf8_content.get_buffer (),
					       utf8_content.length ()));
  return artifact_content_obj;
}

/* Attempt to read the given range of lines from FILENAME; return
   a freshly-allocated 0-terminated buffer containing them, or NULL.  */

static char *
get_source_lines (const char *filename,
		  int start_line,
		  int end_line)
{
  auto_vec<char> result;

  for (int line = start_line; line <= end_line; line++)
    {
      char_span line_content = location_get_source_line (filename, line);
      if (!line_content.get_buffer ())
	return NULL;
      result.reserve (line_content.length () + 1);
      for (size_t i = 0; i < line_content.length (); i++)
	result.quick_push (line_content[i]);
      result.quick_push ('\n');
    }
  result.safe_push ('\0');

  return xstrdup (result.address ());
}

/* Make an artifactContent object (SARIF v2.1.0 section 3.3) for the given
   run of lines within FILENAME (including the endpoints).  */

json::object *
sarif_builder::maybe_make_artifact_content_object (const char *filename,
						   int start_line,
						   int end_line) const
{
  char *text_utf8 = get_source_lines (filename, start_line, end_line);

  if (!text_utf8)
    return NULL;

  /* Don't add it if it's not valid UTF-8.  */
  if (!cpp_valid_utf8_p(text_utf8, strlen(text_utf8)))
    {
      free (text_utf8);
      return NULL;
    }

  json::object *artifact_content_obj = new json::object ();
  artifact_content_obj->set ("text", new json::string (text_utf8));
  free (text_utf8);

  return artifact_content_obj;
}

/* Make a fix object (SARIF v2.1.0 section 3.55) for RICHLOC.  */

json::object *
sarif_builder::make_fix_object (const rich_location &richloc)
{
  json::object *fix_obj = new json::object ();

  /* "artifactChanges" property (SARIF v2.1.0 section 3.55.3).  */
  /* We assume that all fix-it hints in RICHLOC affect the same file.  */
  json::array *artifact_change_arr = new json::array ();
  json::object *artifact_change_obj = make_artifact_change_object (richloc);
  artifact_change_arr->append (artifact_change_obj);
  fix_obj->set ("artifactChanges", artifact_change_arr);

  return fix_obj;
}

/* Make an artifactChange object (SARIF v2.1.0 section 3.56) for RICHLOC.  */

json::object *
sarif_builder::make_artifact_change_object (const rich_location &richloc)
{
  json::object *artifact_change_obj = new json::object ();

  /* "artifactLocation" property (SARIF v2.1.0 section 3.56.2).  */
  json::object *artifact_location_obj
    = make_artifact_location_object (richloc.get_loc ());
  artifact_change_obj->set ("artifactLocation", artifact_location_obj);

  /* "replacements" property (SARIF v2.1.0 section 3.56.3).  */
  json::array *replacement_arr = new json::array ();
  for (unsigned int i = 0; i < richloc.get_num_fixit_hints (); i++)
    {
      const fixit_hint *hint = richloc.get_fixit_hint (i);
      json::object *replacement_obj = make_replacement_object (*hint);
      replacement_arr->append (replacement_obj);
    }
  artifact_change_obj->set ("replacements", replacement_arr);

  return artifact_change_obj;
}

/* Make a replacement object (SARIF v2.1.0 section 3.57) for HINT.  */

json::object *
sarif_builder::make_replacement_object (const fixit_hint &hint) const
{
  json::object *replacement_obj = new json::object ();

  /* "deletedRegion" property (SARIF v2.1.0 section 3.57.3).  */
  json::object *region_obj = make_region_object_for_hint (hint);
  replacement_obj->set ("deletedRegion", region_obj);

  /* "insertedContent" property (SARIF v2.1.0 section 3.57.4).  */
  json::object *content_obj = make_artifact_content_object (hint.get_string ());
  replacement_obj->set ("insertedContent", content_obj);

  return replacement_obj;
}

/* Make an artifactContent object (SARIF v2.1.0 section 3.3) for TEXT.  */

json::object *
sarif_builder::make_artifact_content_object (const char *text) const
{
  json::object *content_obj = new json::object ();

  /* "text" property (SARIF v2.1.0 section 3.3.2).  */
  content_obj->set ("text", new json::string (text));

  return content_obj;
}

/* No-op implementation of "begin_diagnostic" for SARIF output.  */

static void
sarif_begin_diagnostic (diagnostic_context *, diagnostic_info *)
{
}

/* Implementation of "end_diagnostic" for SARIF output.  */

static void
sarif_end_diagnostic (diagnostic_context *context, diagnostic_info *diagnostic,
		      diagnostic_t orig_diag_kind)
{
  gcc_assert (the_builder);
  the_builder->end_diagnostic (context, diagnostic, orig_diag_kind);
}

/* No-op implementation of "begin_group_cb" for SARIF output.  */

static void
sarif_begin_group (diagnostic_context *)
{
}

/* Implementation of "end_group_cb" for SARIF output.  */

static void
sarif_end_group (diagnostic_context *)
{
  gcc_assert (the_builder);
  the_builder->end_group ();
}

/* Flush the top-level array to OUTF.  */

static void
sarif_flush_to_file (FILE *outf)
{
  gcc_assert (the_builder);
  the_builder->flush_to_file (outf);
  delete the_builder;
  the_builder = NULL;
}

/* Callback for final cleanup for SARIF output to stderr.  */

static void
sarif_stderr_final_cb (diagnostic_context *)
{
  gcc_assert (the_builder);
  sarif_flush_to_file (stderr);
}

static char *sarif_output_base_file_name;

/* Callback for final cleanup for SARIF output to a file.  */

static void
sarif_file_final_cb (diagnostic_context *)
{
  char *filename = concat (sarif_output_base_file_name, ".sarif", NULL);
  FILE *outf = fopen (filename, "w");
  if (!outf)
    {
      const char *errstr = xstrerror (errno);
      fnotice (stderr, "error: unable to open '%s' for writing: %s\n",
	       filename, errstr);
      free (filename);
      return;
    }
  gcc_assert (the_builder);
  sarif_flush_to_file (outf);
  fclose (outf);
  free (filename);
}

/* Callback for diagnostic_context::ice_handler_cb for when an ICE
   occurs.  */

static void
sarif_ice_handler (diagnostic_context *context)
{
  /* Attempt to ensure that a .sarif file is written out.  */
  diagnostic_finish (context);

  /* Print a header for the remaining output to stderr, and
     return, attempting to print the usual ICE messages to
     stderr.  Hopefully this will be helpful to the user in
     indicating what's gone wrong (also for DejaGnu, for pruning
     those messages).   */
  fnotice (stderr, "Internal compiler error:\n");
}

/* Populate CONTEXT in preparation for SARIF output (either to stderr, or
   to a file).  */

static void
diagnostic_output_format_init_sarif (diagnostic_context *context)
{
  the_builder = new sarif_builder (context);

  /* Override callbacks.  */
  context->begin_diagnostic = sarif_begin_diagnostic;
  context->end_diagnostic = sarif_end_diagnostic;
  context->begin_group_cb = sarif_begin_group;
  context->end_group_cb =  sarif_end_group;
  context->print_path = NULL; /* handled in sarif_end_diagnostic.  */
  context->ice_handler_cb = sarif_ice_handler;

  /* The metadata is handled in SARIF format, rather than as text.  */
  context->show_cwe = false;
  context->show_rules = false;

  /* The option is handled in SARIF format, rather than as text.  */
  context->show_option_requested = false;

  /* Don't colorize the text.  */
  pp_show_color (context->printer) = false;
}

/* Populate CONTEXT in preparation for SARIF output to stderr.  */

void
diagnostic_output_format_init_sarif_stderr (diagnostic_context *context)
{
  diagnostic_output_format_init_sarif (context);
  context->final_cb = sarif_stderr_final_cb;
}

/* Populate CONTEXT in preparation for SARIF output to a file named
   BASE_FILE_NAME.sarif.  */

void
diagnostic_output_format_init_sarif_file (diagnostic_context *context,
					 const char *base_file_name)
{
  diagnostic_output_format_init_sarif (context);
  context->final_cb = sarif_file_final_cb;
  sarif_output_base_file_name = xstrdup (base_file_name);
}
