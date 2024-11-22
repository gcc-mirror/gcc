/* A library for re-emitting diagnostics saved in SARIF form
   via libdiagnostics.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
#define INCLUDE_VECTOR
#define INCLUDE_MAP
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "libdiagnostics++.h"
#include "json-parsing.h"
#include "intl.h"
#include "sarif-spec-urls.def"
#include "libsarifreplay.h"
#include "label-text.h"

namespace {

/* Read the contents of PATH into memory.
   Issue an error to MGR and return nullptr if there are any problems.  */

static std::unique_ptr<std::vector<char>>
read_file (const char *path, libdiagnostics::manager &mgr)
{
  FILE *f_in = fopen (path, "r");
  if (!f_in)
    {
      char *errmsg = xstrerror (errno);
      auto err (mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR));
      err.finish ("cannot open %qs: %s", path, errmsg);
      return nullptr;
    }

  /* Read content, allocating a buffer for it.  */
  auto result = ::make_unique<std::vector<char>> ();
  char buf[4096];
  size_t iter_sz_in;

  while ( (iter_sz_in = fread (buf, 1, sizeof (buf), f_in)) )
    {
      size_t old_total_sz = result->size ();
      size_t new_total_sz = old_total_sz + iter_sz_in;
      size_t old_alloc_sz = result->capacity ();
      if (new_total_sz > old_alloc_sz)
	{
	  size_t new_alloc_sz = std::max (old_alloc_sz * 2, new_total_sz);
	  result->reserve (new_alloc_sz);
	}
      gcc_assert (result->capacity () >= new_total_sz);
      result->resize (new_total_sz);
      memcpy (result->data () + old_total_sz, buf, iter_sz_in);
    }

  if (!feof (f_in))
    {
      char *errmsg = xstrerror (errno);
      auto err (mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR));
      err.finish ("error reading from %qs: %s", path, errmsg);
      return nullptr;
    }

  fclose (f_in);

  return result;
}

static libdiagnostics::physical_location
make_physical_location (libdiagnostics::manager &mgr,
			libdiagnostics::file f,
			const json::location_map::point &point)
{
  /* json::location_map::point uses 0-based columns,
     whereas libdiagnostics uses 1-based columns.  */
  return mgr.new_location_from_file_line_column (f,
						 point.m_line,
						 point.m_column + 1);
}

static libdiagnostics::physical_location
make_physical_location (libdiagnostics::manager &mgr,
			libdiagnostics::file f,
			const json::location_map::range &range)
{
  libdiagnostics::physical_location start
    = make_physical_location (mgr, f, range.m_start);
  libdiagnostics::physical_location end
    = make_physical_location (mgr, f, range.m_end);
  return mgr.new_location_from_range (start, start, end);
}

enum class status
{
  ok,
  err_reading_file,
  err_malformed_json,
  err_invalid_sarif,
  err_unhandled_sarif
};

/* A reference to the SARIF specification.  */

class spec_ref
{
public:
  spec_ref (const char *section)
    : m_section (section)
  {}

  virtual char *make_description () const
  {
    /* 'SECTION SIGN' (U+00A7).  */
#define SECTION_SIGN_UTF8 "\xC2\xA7"
    return xasprintf ("SARIF v2.1.0 " SECTION_SIGN_UTF8 "%s", m_section);
  }

  char *make_url () const
  {
    const char *anchor = get_anchor_for_section (m_section);
    if (!anchor)
      return nullptr;
    return xasprintf ("%s#%s", sarif_spec_base_url, anchor);
  }

private:
  static const char *
  get_anchor_for_section (const char *section)
  {
    /* Linear search, but the array is only a few hundred entries .  */
    for (size_t i = 0; i < ARRAY_SIZE (sarif_spec_anchor_arr); i++)
      {
	if (strcmp (sarif_spec_anchor_arr[i].m_ref, section) == 0)
	  return sarif_spec_anchor_arr[i].m_anchor;
      }
    return nullptr;
  }

  /* e.g. "3.1" for section 3.1 of the spec.  */
  const char *m_section;
};

/* A reference to the SARIF specification for a particular kind of object.  */

class object_spec_ref : public spec_ref
{
public:
  object_spec_ref (const char *obj_name, const char *section)
  : spec_ref (section),
    m_obj_name (obj_name)
  {}

  const char *get_obj_name () const { return m_obj_name; }

private:
  const char *m_obj_name;
};

/* A reference to the SARIF specification for a particular property
   of a particular kind of object.  */

class property_spec_ref : public object_spec_ref
{
public:
  property_spec_ref (const char *obj_name,
		     const char *property_name,
		     const char *section)
  : object_spec_ref (obj_name, section),
    m_property_name (property_name)
  {}

  const char *get_property_name () const { return m_property_name; }

private:
  const char *m_property_name;
};

template <typename ValueType>
struct string_property_value
{
  const char *m_string;
  ValueType m_value;
};

class sarif_replayer
{
public:
  sarif_replayer (libdiagnostics::manager &&output_manager,
		  libdiagnostics::manager &&control_manager)
  : m_output_mgr (std::move (output_manager)),
    m_control_mgr (std::move (control_manager)),
    m_driver_obj (nullptr),
    m_artifacts_arr (nullptr)
  {
  }

  enum status replay_file (const char *filename,
			   const replay_options &replay_opts);

private:
  class replayer_location_map : public json::location_map
  {
  public:
    void record_range_for_value (json::value *jv,
				 const range &r) final override
    {
      m_map_jv_to_range[jv] = r;
    }

    const json::location_map::range &
    get_range_for_value (const json::value &jv) const
    {
      auto iter = m_map_jv_to_range.find (&jv);
      gcc_assert (iter != m_map_jv_to_range.end ());
      return iter->second;
    }

  private:
    std::map<const json::value *, range> m_map_jv_to_range;
  };

  enum status emit_sarif_as_diagnostics (const json::value &jv);

  label_text
  make_plain_text_within_result_message (const json::object *tool_component_obj,
					 const json::object &message_obj,
					 const json::object *rule_obj);

  /* Handlers for specific parts of the SARIF spec.
     Keep this in the same order as the spec.  */

  // "artifactLocation" object (§3.4)
  enum status
  handle_artifact_location_object (const json::object &artifact_loc,
				   libdiagnostics::file &out);

  // Message string lookup algorithm (§3.11.7)
  const char *
  lookup_plain_text_within_result_message (const json::object *tool_component_obj,
					   const json::object &message_obj,
					   const json::object *rule_obj);

  // "multiformatMessageString" object (§3.12).
  const char *
  get_plain_text_from_mfms (json::value &mfms_val,
			    const property_spec_ref &prop);

  // "run" object (§3.14)
  enum status
  handle_run_obj (const json::object &run_obj);

  // "tool" object (§3.18)
  enum status
  handle_tool_obj (const json::object &tool_obj);

  // "result" object (§3.27)
  enum status
  handle_result_obj (const json::object &result_obj,
		     const json::object &tool_obj);
  json::result<enum diagnostic_level, enum status>
  get_level_from_level_str (const json::string &level_str);

  // "location" object (§3.28)
  enum status
  handle_location_object (const json::object &location_obj,
			  libdiagnostics::physical_location &out_physical_loc,
			  libdiagnostics::logical_location &out_logical_loc);

  // "physicalLocation" object (§3.29)
  enum status
  handle_physical_location_object (const json::object &phys_loc_obj,
				   libdiagnostics::physical_location &out);

  // "region" object (§3.30)
  enum status
  handle_region_object (const json::object &region_obj,
			libdiagnostics::file file,
			libdiagnostics::physical_location &out);

  // "logicalLocation" object (§3.33)
  enum status
  handle_logical_location_object (const json::object &logical_loc_obj,
				  libdiagnostics::logical_location &out);

  // "threadFlow" object (§3.37)
  enum status
  handle_thread_flow_object (const json::object &thread_flow_obj,
			     libdiagnostics::execution_path &out);

  // "threadFlowLocation" object (§3.38)
  enum status
  handle_thread_flow_location_object (const json::object &tflow_loc_obj,
				      libdiagnostics::execution_path &out);

  // reportingDescriptor lookup (§3.52.3)
  const json::object *
  lookup_rule_by_id_in_tool (const char *rule_id,
			     const json::object &tool_obj,
			     const json::object *&tool_component_obj);

  const json::object *
  lookup_rule_by_id_in_component (const char *rule_id,
				  const json::object &tool_component_obj);

  /* Support functions.  */

  /* Report an error to m_control_mgr about JV violating REF,
     and return status::err_invalid_sarif.  */

  enum status
  report_invalid_sarif (const json::value &jv,
			const spec_ref &ref,
			const char *gmsgid, ...)
    LIBDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (4, 5)
  {
    va_list ap;
    va_start (ap, gmsgid);
    report_problem (jv, ref, gmsgid, &ap, DIAGNOSTIC_LEVEL_ERROR);
    va_end (ap);
    return status::err_invalid_sarif;
  }

  /* Report a "sorry" to m_control_mgr inability to handle JV and REF,
     and return status::err_unhandled_sarif.  */

  enum status
  report_unhandled_sarif (const json::value &jv,
			  const spec_ref &ref,
			  const char *gmsgid, ...)
    LIBDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (4, 5)
  {
    va_list ap;
    va_start (ap, gmsgid);
    report_problem (jv, ref, gmsgid, &ap, DIAGNOSTIC_LEVEL_SORRY);
    va_end (ap);
    return status::err_unhandled_sarif;
  }

  void
  report_problem (const json::value &jv,
		  const spec_ref &ref,
		  const char *gmsgid,
		  va_list *args,
		  enum diagnostic_level level)
    LIBDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (4, 0)
  {
    auto diag (m_control_mgr.begin_diagnostic (level));

    /* Add rule specifying the pertinent section of the specification.
       There doesn't seem to be a systematic mapping from spec sections to
       HTML anchors, so we can't provide URLs
       (filed as https://github.com/oasis-tcs/sarif-spec/issues/533 ).  */
    char *ref_desc = ref.make_description ();
    char *ref_url = ref.make_url ();
    diag.add_rule (ref_desc, ref_url);
    free (ref_desc);
    free (ref_url);

    auto loc_range
      = make_physical_location (m_control_mgr,
				m_loaded_file,
				m_json_location_map.get_range_for_value (jv));
    diag.set_location (loc_range);

    diag.finish_va (gmsgid, args);
  }

  /* Require OBJ to have at least one of OBJ_PROP1 or OBJ_PROP2.
     If successful, result status::ok.
     Otherwise, complain about OBJ_CONSTRAINTS and return
     status::invalid_sarif.  */
  enum status
  report_invalid_sarif_at_least_one_of (const json::object &obj,
					const object_spec_ref &obj_constraints,
					const property_spec_ref &obj_prop_1,
					const property_spec_ref &obj_prop_2)
  {
      return report_invalid_sarif
	(obj, obj_constraints,
	 "expected SARIF %qs object to contain at least one of %qs or %qs",
	 obj_constraints.get_obj_name (),
	 obj_prop_1.get_property_name (),
	 obj_prop_2.get_property_name ());
  }

  /* Require VAL to be a json::object.
     If successful, return it as an object.
     Otherwise, complain using REF and return nullptr.  */
  const json::object *
  require_object (const json::value &val, const property_spec_ref &ref)
  {
    const json::object *obj = dyn_cast <const json::object *> (&val);
    if (!obj)
      {
	report_invalid_sarif (val, ref, "expected %s.%s to be an object",
			      ref.get_obj_name (), ref.get_property_name ());
	return nullptr;
      }
    return obj;
  }

  /* Require VAL to be a json::string
     If successful, return it as an string.
     Otherwise, complain using REF and return nullptr.  */
  const json::string *
  require_string (const json::value &val, const property_spec_ref &ref)
  {
    const json::string *str = dyn_cast <const json::string *> (&val);
    if (!str)
      {
	report_invalid_sarif (val, ref, "expected %s.%s to be an string",
			      ref.get_obj_name (), ref.get_property_name ());
	return nullptr;
      }
    return str;
  }
  /* Look for an optional property within OBJ based on REF.  */
  const json::value *
  get_optional_property (const json::object &obj,
			 const property_spec_ref &ref)
  {
    return obj.get (ref.get_property_name ());
  }

  /* Look for a property of VAL based on REF.
     If present, it must be of kind JsonType.
     If found and valid, return the property's value.
     If not found, silently return nullptr.
     Otherwise, complain and return nullptr.  */
  template <typename JsonType>
  const JsonType *
  get_optional_property (const json::object &obj,
			 const property_spec_ref &ref)
  {
    const json::value *property_val = get_optional_property (obj, ref);
    if (!property_val)
      return nullptr;
    const JsonType *sub = dyn_cast<const JsonType *> (property_val);
    if (!sub)
      {
	/* Property is wrong kind of value.  */
	report_bad_property_kind<JsonType> (obj, ref, *property_val);
	return nullptr;
      }
    return sub;
  }

  /* Require VAL to be a json::object.
     Look for a property of VAL based on REF, which must be of
     kind JsonType.
     If successful, return the property's value.
     Otherwise, complain and return nullptr.  */
  template <typename JsonType>
  const JsonType *
  get_required_property (const json::value &val,
			 const property_spec_ref &ref)
  {
    const json::object *obj = require_object (val, ref);
    if (!obj)
      return nullptr;
    return get_required_property<JsonType> (*obj, ref);
  }

  /* Look for a property of VAL based on REF, which must be of
     kind JsonType.
     If successful, return the property's value.
     Otherwise, complain and return nullptr.  */
  template <typename JsonType>
  const JsonType *
  get_required_property (const json::object &obj,
			 const property_spec_ref &ref)
  {
    const json::value *property_val = get_optional_property (obj, ref);
    if (!property_val)
      {
	/* Property not present.  */
	report_invalid_sarif (obj, ref,
			      "expected %s object to have a %qs property",
			      ref.get_obj_name (), ref.get_property_name ());
	return nullptr;
      }
    const JsonType *sub = dyn_cast<const JsonType *> (property_val);
    if (!sub)
      {
	/* Property is wrong kind of value.  */
	report_bad_property_kind<JsonType> (obj, ref, *property_val);
	return nullptr;
      }
    return sub;
  }

  template <typename JsonType>
  void
  report_bad_property_kind (const json::object &obj,
			    const property_spec_ref &ref,
			    const json::value &property_val);

  const json::object *
  require_object_for_element (const json::value &jv,
			      const property_spec_ref &ref)
  {
    const json::object *obj = dyn_cast <const json::object *> (&jv);
    if (!obj)
      {
	report_invalid_sarif (jv, ref,
			      "expected element of %s.%s array to be an object",
			      ref.get_obj_name (), ref.get_property_name ());
	return nullptr;
      }
    return obj;
  }

  template <typename ValueType>
  json::result<ValueType, enum status>
  get_value_from_json_string (const json::string &json_str,
			      const property_spec_ref &prop,
			      const string_property_value<ValueType> *value_arr,
			      size_t num_values);

  /* The manager to replay the SARIF files to.  */
  libdiagnostics::manager m_output_mgr;

  /* The manager for reporting issues loading SARIF files.  */
  libdiagnostics::manager m_control_mgr;

  /* The file within m_control_mgr representing the .sarif file.  */
  libdiagnostics::file m_loaded_file;

  replayer_location_map m_json_location_map;

  const json::object *m_driver_obj;
  const json::value *m_artifacts_arr;
};

static const char *
describe_kind (const json::value &val)
{
  switch (val.get_kind ())
    {
    default:
      gcc_unreachable ();
    case json::JSON_OBJECT:
      return _("JSON object");

    case json::JSON_ARRAY:
      return _("JSON array");

    case json::JSON_INTEGER:
    case json::JSON_FLOAT:
      return _("JSON number");

    case json::JSON_STRING:
      return _("JSON string");

    case json::JSON_TRUE:
    case json::JSON_FALSE:
    case json::JSON_NULL:
      return _("JSON literal");
    }
}

/* class sarif_replayer.  */

template <>
void
sarif_replayer::
report_bad_property_kind<json::value> (const json::object &,
				       const property_spec_ref &,
				       const json::value &)
{
  gcc_unreachable ();
}

template <>
void
sarif_replayer::
report_bad_property_kind<json::integer_number> (const json::object &,
						const property_spec_ref &ref,
						const json::value &propval)
{
  report_invalid_sarif (propval, ref, "expected %s.%s to be a JSON integer; got %s",
			ref.get_obj_name (), ref.get_property_name (),
			describe_kind (propval));
}

template <>
void
sarif_replayer::
report_bad_property_kind<json::string> (const json::object &,
					const property_spec_ref &ref,
					const json::value &propval)
{
  report_invalid_sarif (propval, ref, "expected %s.%s to be a JSON string; got %s",
			ref.get_obj_name (), ref.get_property_name (),
			describe_kind (propval));
}

template <>
void
sarif_replayer::
report_bad_property_kind<json::array> (const json::object &,
				       const property_spec_ref &ref,
				       const json::value &propval)
{
  report_invalid_sarif (propval, ref, "expected %s.%s to be a JSON array; got %s",
			ref.get_obj_name (), ref.get_property_name (),
			describe_kind (propval));
}

template <>
void
sarif_replayer::
report_bad_property_kind<json::object> (const json::object &,
					const property_spec_ref &ref,
					const json::value &propval)
{
  report_invalid_sarif (propval, ref, "expected %s.%s to be a JSON object; got %s",
			ref.get_obj_name (), ref.get_property_name (),
			describe_kind (propval));
}

enum status
sarif_replayer::replay_file (const char *filename,
			     const replay_options &replay_opts)
{
  std::unique_ptr<std::vector<char>> buf = read_file (filename, m_control_mgr);
  if (!buf)
    return status::err_reading_file;

  /* Use "sarif" as the sourceLanguage for SARIF itself; see
     https://github.com/oasis-tcs/sarif-spec/issues/654 */
  const char * const source_language = "sarif";
  m_loaded_file = m_control_mgr.new_file (filename, source_language);

  if (replay_opts.m_echo_file)
    {
      fprintf (stderr, "%s: (%li bytes)\n",
	       filename, (long)buf->size ());
      for (size_t i = 0; i < buf->size(); i++)
	fputc ((*buf)[i], stderr);
    }

  json::parser_result_t result
    (json::parse_utf8_string (buf->size (),
			      (const char *)buf->data (),
			      replay_opts.m_json_comments,
			      &m_json_location_map));

  if (auto json_err = result.m_err.get ())
    {
      gcc_assert (!result.m_val.get ());
      auto file = m_control_mgr.new_file (filename, source_language);
      auto loc_range = make_physical_location (m_control_mgr,
					       file,
					       json_err->get_range ());
      auto err (m_control_mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR));
      err.set_location (loc_range);
      err.finish ("%s", json_err->get_msg ());

      return status::err_malformed_json;
    }

  gcc_assert (result.m_val.get ());
  return emit_sarif_as_diagnostics (*result.m_val.get ());
}

#define PROP_sarifLog_version \
  property_spec_ref ("sarifLog", "version", "3.13.2")

#define PROP_sarifLog_runs \
  property_spec_ref ("sarifLog", "runs", "3.13.4")

enum status
sarif_replayer::emit_sarif_as_diagnostics (const json::value &jv)
{
  /* We expect a sarifLog object as the top-level value
     (SARIF v2.1.0 section 3.13).  */
  const json::object *toplev_obj = dyn_cast <const json::object *> (&jv);
  if (!toplev_obj)
    return report_invalid_sarif
      (jv, spec_ref ("3.1"),
       "expected a sarifLog object as the top-level value");

  /* sarifLog objects SHALL have a property named "version"
     (SARIF v2.1.0 section 3.13.2) with a string value.  */
  if (!get_required_property<json::string> (*toplev_obj,
					    PROP_sarifLog_version))
    return status::err_invalid_sarif;

  /* sarifLog.runs must be null or be an array.  */
  const property_spec_ref prop_runs (PROP_sarifLog_runs);
  const json::value *runs
    = get_required_property<json::value> (*toplev_obj, prop_runs);
  if (!runs)
    return status::err_invalid_sarif;

  switch (runs->get_kind ())
    {
    default:
      return report_invalid_sarif (*runs, prop_runs,
				   "expected sarifLog.runs to be"
				   " %<null%> or an array");

    case json::JSON_NULL:
      /* Nothing to do.  */
      break;

    case json::JSON_ARRAY:
      {
	const json::array &runs_arr = *as_a <const json::array *> (runs);
	for (auto element : runs_arr)
	  {
	    const json::object *run_obj
	      = require_object_for_element (*element, prop_runs);
	    if (!run_obj)
	      return status::err_invalid_sarif;
	    enum status s = handle_run_obj (*run_obj);
	    if (s != status::ok)
	      return s;
	  }
      }
      break;
    }

  return status::ok;
}

/* Process a run object (SARIF v2.1.0 section 3.14).  */

enum status
sarif_replayer::handle_run_obj (const json::object &run_obj)
{
  const json::object *tool_obj
    = get_required_property<json::object> (run_obj,
					   property_spec_ref ("run", "tool",
							      "3.14.6"));
  if (!tool_obj)
    return status::err_invalid_sarif;
  {
    enum status err = handle_tool_obj (*tool_obj);
    if (err != status::ok)
      return err;
  }

  m_driver_obj
    = get_required_property<json::object> (*tool_obj,
					   property_spec_ref ("tool", "driver",
							      "3.18.2"));
  if (!m_driver_obj)
    return status::err_invalid_sarif;

#if 0
  m_artifacts_arr = get_optional_property<json::array>
    (run_obj, property_spec_ref ("run", "artifacts","3.14.15"));
#endif

  /* If present, run.results must be null or be an array.  */
  const property_spec_ref prop_results ("run", "results", "3.14.23");
  if (const json::value *results = get_optional_property (run_obj,
							  prop_results))
    switch (results->get_kind ())
      {
      default:
	return report_invalid_sarif (*results, prop_results,
				     "expected run.results to be"
				     " %<null%> or an array");

      case json::JSON_NULL:
	/* Nothing to do.  */
	break;
      case json::JSON_ARRAY:
	{
	  const json::array *results_arr = as_a <const json::array *> (results);
	  for (auto element : *results_arr)
	    {
	      const json::object *result_obj
		= require_object_for_element (*element, prop_results);
	      if (!result_obj)
		return status::err_invalid_sarif;
	      enum status s = handle_result_obj (*result_obj, *tool_obj);
	      if (s != status::ok)
		return s;
	    }
	}
	break;
      }

  return status::ok;
}

/* Process a tool object (SARIF v2.1.0 section 3.18).  */

enum status
sarif_replayer::handle_tool_obj (const json::object &tool_obj)
{
  auto driver_obj
    = get_required_property<json::object> (tool_obj,
					   property_spec_ref ("tool", "driver",
							      "3.18.2"));
  if (!driver_obj)
    return status::err_invalid_sarif;

  const property_spec_ref name_prop ("toolComponent", "name", "3.19.8");
  if (auto name_jstr = get_optional_property<json::string> (*driver_obj,
							    name_prop))
    m_output_mgr.set_tool_name (name_jstr->get_string ());

  const property_spec_ref full_name_prop
    ("toolComponent", "fullName", "3.19.9");
  if (auto name_jstr = get_optional_property<json::string> (*driver_obj,
							    full_name_prop))
    m_output_mgr.set_full_name (name_jstr->get_string ());

  const property_spec_ref version_prop ("toolComponent", "version", "3.19.13");
  if (auto name_jstr = get_optional_property<json::string> (*driver_obj,
							    version_prop))
    m_output_mgr.set_version_string (name_jstr->get_string ());

  const property_spec_ref
    info_uri_prop ("toolComponent", "informationUri", "3.19.17");
  if (auto name_jstr = get_optional_property<json::string> (*driver_obj,
							    info_uri_prop))
    m_output_mgr.set_version_url (name_jstr->get_string ());

  return status::ok;
}

/* Compare the value of JSON_STR to the values in VALUE_ARR.
   If found, return it in the result's m_val.
   Otherwise, complain using PROP and return status::invalid_sarif
   in results's m_err.  */

template <typename ValueType>
json::result<ValueType, enum status>
sarif_replayer::
get_value_from_json_string (const json::string &json_str,
			    const property_spec_ref &prop,
			    const string_property_value<ValueType> *value_arr,
			    size_t num_values)
{
  const char *str = json_str.get_string ();
  for (size_t i = 0; i < num_values; i++)
    if (strcmp (str, value_arr[i].m_string) == 0)
      return value_arr[i].m_value;
  return report_invalid_sarif (json_str, prop,
			       "unrecognized value for %qs: %qs",
			       prop.get_property_name (),
			       str);
}

const property_spec_ref prop_result_level ("result", "level", "3.27.10");

/* Handle a value for result's "level" property (§3.27.10).
   Limitation: doesn't yet support "none".  */
json::result<enum diagnostic_level, enum status>
sarif_replayer::get_level_from_level_str (const json::string &level_str)
{
  if (strcmp (level_str.get_string (), "none") == 0)
    return report_unhandled_sarif (level_str, prop_result_level,
				   "unable to handle value for %qs: %qs",
				   prop_result_level.get_property_name (),
				   level_str.get_string ());

  const string_property_value<enum diagnostic_level> level_values[]
    = { {"warning",
	 DIAGNOSTIC_LEVEL_WARNING},
	{"error",
	 DIAGNOSTIC_LEVEL_ERROR},
	{"note",
	 DIAGNOSTIC_LEVEL_NOTE} };
  return get_value_from_json_string<enum diagnostic_level>
    (level_str,
     prop_result_level,
     level_values, ARRAY_SIZE (level_values));
}

/* Process a result object (SARIF v2.1.0 section 3.27).
   Known limitations:
   - doesn't yet handle "ruleIndex" property (§3.27.6)
   - doesn't yet handle "taxa" property (§3.27.8)
   - handling of "level" property (§3.27.10) doesn't yet support the
     full logic for when "level" is absent.
   - doesn't yet handle "relatedLocations" property (§3.27.22)
   - doesn't yet handle "fixes" property (§3.27.30)
   - doesn't yet support multithreaded flows (§3.36.3)
*/

#define PROP_result_ruleId \
  property_spec_ref ("result", "ruleId", "3.27.5")

#define PROP_result_message \
  property_spec_ref ("result", "message", "3.27.11")

enum status
sarif_replayer::handle_result_obj (const json::object &result_obj,
				   const json::object &tool_obj)
{
  const json::object *rule_obj = nullptr;
  const json::object *tool_component_obj = nullptr;
  const json::string *rule_id
    = get_optional_property<json::string> (result_obj, PROP_result_ruleId);
  if (rule_id)
    {
      rule_obj = lookup_rule_by_id_in_tool (rule_id->get_string (),
					    tool_obj,
					    tool_component_obj);
      // TODO: error handling
    }

  enum diagnostic_level level = DIAGNOSTIC_LEVEL_WARNING;
  if (auto level_str
	= get_optional_property<json::string> (result_obj,
					       prop_result_level))
    {
      auto result = get_level_from_level_str (*level_str);
      if (result.m_err != status::ok)
	return result.m_err;
      level = result.m_val;
    }

  // §3.27.11 "message" property
  label_text text;
  if (auto message_obj
	= get_optional_property<json::object> (result_obj, PROP_result_message))
    text = make_plain_text_within_result_message (nullptr, // TODO: tool_component_obj,
						  *message_obj,
						  rule_obj);
  if (!text.get ())
    return status::err_invalid_sarif;

  // §3.27.12 "locations" property
  libdiagnostics::physical_location physical_loc;
  libdiagnostics::logical_location logical_loc;
  const property_spec_ref locations_prop ("result", "locations", "3.27.12");
  const json::array *locations_arr
    = get_required_property<json::array> (result_obj, locations_prop);
  if (!locations_arr)
    return status::err_invalid_sarif;
  if (locations_arr->length () > 0)
    {
      /* Only look at the first, if there's more than one.  */
      // location objects (§3.28)
      const json::object *location_obj
	= require_object_for_element (*locations_arr->get (0), locations_prop);
      if (!location_obj)
	return status::err_invalid_sarif;
      enum status s = handle_location_object (*location_obj,
					      physical_loc,
					      logical_loc);
      if (s != status::ok)
	return s;
    }

  // §3.27.18 "codeFlows" property
  libdiagnostics::execution_path path;
  const property_spec_ref code_flows ("result", "codeFlows", "3.27.18");
  if (auto code_flows_arr = get_optional_property<json::array> (result_obj,
								code_flows))
    {
      // TODO: what if more than one?
      if (code_flows_arr->length () == 1)
	{
	  const json::object *code_flow_obj
	    = require_object_for_element (*code_flows_arr->get (0), code_flows);
	  if (!code_flow_obj)
	    return status::err_invalid_sarif;

	  const property_spec_ref prop_thread_flows
	    ("result", "threadFlows", "3.36.3");
	  if (auto thread_flows_arr
	      = get_optional_property<json::array> (*code_flow_obj,
						    prop_thread_flows))
	    {
	      if (thread_flows_arr->length () == 1)
		{
		  const json::object *thread_flow_obj
		    = require_object_for_element (*thread_flows_arr->get (0),
						  prop_thread_flows);
		  if (!thread_flow_obj)
		    return status::err_invalid_sarif;
		  handle_thread_flow_object (*thread_flow_obj, path);
		}
	    }
	}
    }

  libdiagnostics::group g (m_output_mgr);
  auto err (m_output_mgr.begin_diagnostic (level));
  if (rule_id)
    err.add_rule (rule_id->get_string (), nullptr);
  err.set_location (physical_loc);
  err.set_logical_location (logical_loc);
  if (path.m_inner)
    err.take_execution_path (std::move (path));
  err.finish ("%s", text.get ());

  // §3.27.22 relatedLocations property
  const property_spec_ref prop_related_locations
    ("result", "relatedLocations", "3.27.22");
  if (auto related_locations_arr
      = get_optional_property<json::array> (result_obj,
					    prop_related_locations))
    {
      for (auto rel_loc : *related_locations_arr)
	{
	  libdiagnostics::physical_location physical_loc;
	  libdiagnostics::logical_location logical_loc;
	  const json::object *location_obj
	    = require_object_for_element (*rel_loc,
					  prop_related_locations);
	  if (!location_obj)
	    return status::err_invalid_sarif;
	  enum status s = handle_location_object (*location_obj,
						  physical_loc,
						  logical_loc);
	  if (s != status::ok)
	    return s;

	  // §3.28.5 message property
	  const property_spec_ref prop_message
	    ("location", "message", "3.28.5");
	  if (auto message_obj
	      = get_optional_property<json::object> (*location_obj,
						     prop_message))
	    {
	      /* Treat related locations with a message as a "note".  */
	      label_text text
		(make_plain_text_within_result_message
		 (tool_component_obj,
		  *message_obj,
		  rule_obj));
	      if (!text.get ())
		return status::err_invalid_sarif;
	      auto note (m_output_mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_NOTE));
	      note.set_location (physical_loc);
	      note.set_logical_location (logical_loc);
	      note.finish ("%s", text.get ());
	    }
	}
    }

  return status::ok;

}

/*  If ITER_SRC starts with a placeholder as per §3.11.5, advance ITER_SRC
    to immediately beyond the placeholder, write to *OUT_ARG_IDX, and
    return true.

    Otherwise, leave ITER_SRC untouched and return false.  */

static bool
maybe_consume_placeholder (const char *&iter_src, unsigned *out_arg_idx)
{
  if (*iter_src != '{')
    return false;
  const char *first_digit = iter_src + 1;
  const char *iter_digit = first_digit;
  while (char ch = *iter_digit)
    switch (ch)
      {
      default:
	return false;

      case '}':
	if (iter_digit == first_digit)
	  {
	    /* No digits, we simply have "{}" which is not a placeholder
	       (and malformed: the braces should have been escaped).  */
	    return false;
	  }
	*out_arg_idx = atoi (first_digit);
	iter_src = iter_digit + 1;
	return true;

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
	/* TODO: what about multiple leading zeroes?  */
	iter_digit++;
	continue;
    }
  return false;
}

/* Lookup the plain text string within a result.message (§3.27.11),
   and substitute for any placeholders (§3.11.5).

   Limitations:
   - we don't yet support embedded links

   MESSAGE_OBJ is "theMessage"
   RULE_OBJ is "theRule".  */

label_text
sarif_replayer::
make_plain_text_within_result_message (const json::object *tool_component_obj,
				       const json::object &message_obj,
				       const json::object *rule_obj)
{
  const char *original_text
    = lookup_plain_text_within_result_message (tool_component_obj,
					       message_obj,
					       rule_obj);
  if (!original_text)
    return label_text::borrow (nullptr);

  /* Look up any arguments for substituting into placeholders.  */
  const property_spec_ref arguments_prop ("message", "arguments", "3.11.11");
  const json::array *arguments
    = get_optional_property<json::array> (message_obj, arguments_prop);

  /* Duplicate original_text, substituting any placeholders.  */
  std::string accum;

  const char *iter_src = original_text;
  while (char ch = *iter_src)
    {
      unsigned arg_idx;
      if (maybe_consume_placeholder (iter_src, &arg_idx))
	{
	  if (!arguments)
	    {
	      report_invalid_sarif
		(message_obj, arguments_prop,
		 "message string contains placeholder %<{%i}%>"
		 " but message object has no %qs property",
		 (int)arg_idx,
		 arguments_prop.get_property_name ());
	      return label_text::borrow (nullptr);
	    }
	  if (arg_idx >= arguments->length ())
	    {
	      report_invalid_sarif
		(message_obj, arguments_prop,
		 "not enough strings in %qs array for"
		 " placeholder %<{%i}%>",
		 arguments_prop.get_property_name (),
		 (int)arg_idx);
	      // TODO: might be nice to add a note showing the args
	      return label_text::borrow (nullptr);
	    }
	  auto replacement_jstr
	    = require_string (*arguments->get (arg_idx), arguments_prop);
	  if (!replacement_jstr)
	    return label_text::borrow (nullptr);
	  accum += replacement_jstr->get_string ();
	}
      else if (ch == '{' || ch == '}')
	{
	  /* '{' and '}' are escaped by repeating them.  */
	  if (iter_src[1] == ch)
	    {
	      accum += ch;
	      iter_src += 2;
	    }
	  else
	    {
	      report_invalid_sarif (message_obj, arguments_prop,
				    "unescaped '%c' within message string",
				    ch);
	      return label_text::borrow (nullptr);
	    }
	}
      else
	{
	  accum += ch;
	  iter_src++;
	}
    }

  return label_text::take (xstrdup (accum.c_str ()));
}

/* Handle a value that should be a multiformatMessageString object (§3.12).
   Complain using prop if MFMS_VAL is not an object.
   Return get the "text" value (or nullptr, and complain).  */

const char *
sarif_replayer::get_plain_text_from_mfms (json::value &mfms_val,
					  const property_spec_ref &prop)
{
  auto mfms_obj = require_object (mfms_val, prop);
  if (!mfms_obj)
    return nullptr;

  const property_spec_ref text_prop
    ("multiformatMessageString", "text", "3.12.3");
  auto text_jstr = get_required_property<json::string> (*mfms_obj, text_prop);
  if (!text_jstr)
    return nullptr;
  return text_jstr->get_string ();
}

#define PROP_message_text \
  property_spec_ref ("message", "text", "3.11.8")

#define PROP_message_id \
  property_spec_ref ("message", "id", "3.11.10")

/* Implement the message string lookup algorithm from
   SARIF v2.1.0 section 3.11.7, for the case where theMessage
   is the value of result.message (§3.27.11).

   MESSAGE_OBJ is "theMessage"
   RULE_OBJ is "theRule".  */

const char *
sarif_replayer::
lookup_plain_text_within_result_message (const json::object *tool_component_obj,
					 const json::object &message_obj,
					 const json::object *rule_obj)
{
  // rule_obj can be NULL

  /* IF theMessage.text is present and the desired language is theRun.language THEN
       Use the text or markdown property of theMessage as appropriate.  */
  if (const json::string *str
      = get_optional_property<json::string> (message_obj, PROP_message_text))
    // TODO: check language
    return str->get_string ();

  if (rule_obj)
    if (auto message_id_jstr
	= get_optional_property<json::string> (message_obj, PROP_message_id))
      {
	const char *message_id = message_id_jstr->get_string ();
	const property_spec_ref message_strings
	  ("reportingDescriptor", "messageStrings", "3.49.11");
	if (auto message_strings_obj
	    = get_optional_property<json::object> (*rule_obj,
						   message_strings))
	  if (json::value *mfms = message_strings_obj->get (message_id))
	    return get_plain_text_from_mfms (*mfms, message_strings);

	/* Look up by theMessage.id within theComponent.globalMessageStrings
	   (§3.19.22).  */
	if (tool_component_obj)
	  {
	    const property_spec_ref prop_gms
	      ("toolComponent", "globalMessageStrings", "3.19.22");
	    if (auto global_message_strings
		= get_optional_property<json::object> (*tool_component_obj,
						       prop_gms))
	      if (auto mfms = global_message_strings->get (message_id))
		return get_plain_text_from_mfms (*mfms, prop_gms);
	  }
      }

  /* Failure.  */
  report_invalid_sarif (message_obj, spec_ref ("3.11.7"),
			"could not find string for %<message%> object");
  return nullptr;
}

/* Populate OUT for THREAD_FLOW_OBJ, a
   SARIF threadFlow object (section 3.37).  */

enum status
sarif_replayer::handle_thread_flow_object (const json::object &thread_flow_obj,
					   libdiagnostics::execution_path &out)
{
  const property_spec_ref locations ("threadFlow", "locations", "3.37.6");
  const json::array *locations_arr
    = get_required_property<json::array> (thread_flow_obj, locations);
  if (!locations_arr)
    return status::err_invalid_sarif;

  out = m_output_mgr.new_execution_path ();
  for (auto location : *locations_arr)
    {
      /* threadFlowLocation object (§3.38).  */
      const json::object *tflow_loc_obj
	= require_object_for_element (*location, locations);
      if (!tflow_loc_obj)
	return status::err_invalid_sarif;
      handle_thread_flow_location_object (*tflow_loc_obj, out);
    }

  return status::ok;
}

/* "threadFlowLocation" object (§3.38).
   Attempt to add an event for TFLOW_LOC_OBJ to PATH.  */

enum status
sarif_replayer::
handle_thread_flow_location_object (const json::object &tflow_loc_obj,
				    libdiagnostics::execution_path &path)
{
  libdiagnostics::physical_location physical_loc;
  libdiagnostics::logical_location logical_loc;
  label_text message;
  int stack_depth = 0;

  const property_spec_ref location_prop
    ("threadFlowLocation", "location", "3.38.3");
  if (auto location_obj = get_optional_property<json::object> (tflow_loc_obj,
							       location_prop))
    {
      /* location object (§3.28).  */
      enum status s
	= handle_location_object (*location_obj, physical_loc, logical_loc);
      if (s != status::ok)
	return s;

      /* Get any message from here.  */
      const property_spec_ref location_message
	("location", "message", "3.28.5");
      if (auto message_obj
	  = get_optional_property<json::object> (*location_obj,
						 location_message))
	{
	  message = make_plain_text_within_result_message
	    (nullptr,
	     *message_obj,
	     nullptr/* TODO.  */);
	}
    }

  // §3.38.8 "kinds" property
  const property_spec_ref kinds ("threadFlowLocation", "kinds", "3.38.8");
  if (auto kinds_arr
	= get_optional_property<json::array> (tflow_loc_obj, kinds))
    {
      std::vector<const char *> kind_strs;
      for (auto iter : *kinds_arr)
	{
	  const json::string *kind_str = dyn_cast <const json::string *> (iter);
	  if (!kind_str)
	    {
	    }
	  kind_strs.push_back (kind_str->get_string ());
	  // TODO: handle meaning?
	  /*  TOOD: probably just want to add sarif kinds to
	      the libdiagnostics event, and have libdiagnostics
	      turn that back into a "meaning".  */
	}
    }

  /* nestingLevel property (§3.38.10).  */
  const property_spec_ref nesting_level
    ("threadFlowLocation", "nestingLevel", "3.38.10");
  if (auto nesting_level_jv
      = get_optional_property<json::integer_number> (tflow_loc_obj,
						     nesting_level))
    {
      stack_depth = nesting_level_jv->get ();
      if (stack_depth < 0)
	{
	  return report_invalid_sarif (tflow_loc_obj, nesting_level,
				       "expected a non-negative integer");
	}
    }

  if (message.get ())
    path.add_event (physical_loc,
		    logical_loc,
		    stack_depth,
		    "%s", message.get ());
  else
    path.add_event (physical_loc,
		    logical_loc,
		    stack_depth,
		    "");

  return status::ok;
}

/* Handle LOCATION_OBJ, a "location" (§3.28).  */

enum status
sarif_replayer::
handle_location_object (const json::object &location_obj,
			libdiagnostics::physical_location &out_physical_loc,
			libdiagnostics::logical_location &out_logical_loc)
{
  // §3.28.3 "physicalLocation" property
  {
    const property_spec_ref physical_location_prop
      ("location", "physicalLocation", "3.28.3");
    if (const json::object *phys_loc_obj
	= get_optional_property<json::object> (location_obj,
					       physical_location_prop))
      {
	enum status s = handle_physical_location_object (*phys_loc_obj,
							 out_physical_loc);
	if (s!= status::ok)
	  return s;
      }
  }

  // §3.28.4 "logicalLocations" property
  {
    const property_spec_ref logical_locations_prop
      ("location", "logicalLocations", "3.28.4");
    if (const json::array *logical_loc_arr
	= get_optional_property<json::array> (location_obj,
					      logical_locations_prop))
      if (logical_loc_arr->length () > 0)
	{
	  /* Only look at the first, if there's more than one.  */
	  const json::object *logical_loc_obj
	    = require_object_for_element (*logical_loc_arr->get (0),
					  logical_locations_prop);
	  if (!logical_loc_obj)
	    return status::err_invalid_sarif;
	  enum status s = handle_logical_location_object (*logical_loc_obj,
							  out_logical_loc);
	  if (s != status::ok)
	    return s;
	}
  }

  return status::ok;
}

/* Handle PHYS_LOC_OBJ, a "physicalLocation" object (§3.29).
   Limitations:
   - we don't yet support the "contextRegion" property (§3.29.5)  */

enum status
sarif_replayer::
handle_physical_location_object (const json::object &phys_loc_obj,
				 libdiagnostics::physical_location &out)
{
  libdiagnostics::file artifact_file;

  // §3.29.3 "artifactLocation" property
  const property_spec_ref artifact_location_prop
    ("physicalLocation", "artifactLocation", "3.29.3");
  if (const json::object *artifact_loc_obj
	= get_optional_property<json::object> (phys_loc_obj,
					       artifact_location_prop))
    {
      enum status s = handle_artifact_location_object (*artifact_loc_obj,
						       artifact_file);
      if (s != status::ok)
	return s;
    }

  // §3.29.6 "address" property
  const property_spec_ref artifact_address_prop
    ("physicalLocation", "address", "3.29.6");

  if (!artifact_file.m_inner)
    {
      const object_spec_ref constraints ("physicalLocation", "3.29.2");
      return report_invalid_sarif_at_least_one_of (phys_loc_obj,
						   constraints,
						   artifact_address_prop,
						   artifact_location_prop);
    }

  //3.29.4 region property
  const property_spec_ref region_prop ("physicalLocation", "region", "3.29.4");
  if (const json::object *region_obj
      = get_optional_property<json::object> (phys_loc_obj, region_prop))
    {
      enum status s
	= handle_region_object (*region_obj, artifact_file, out);
      if (s != status::ok)
	return s;
      // TODO:
    }

  return status::ok;
}

/* Handle ARTIFACT_LOC, an "artifactLocation" object (§3.4).  */

enum status
sarif_replayer::handle_artifact_location_object (const json::object &artifact_loc,
						 libdiagnostics::file &out)
{
  // §3.4.3 "uri" property
  const property_spec_ref uri_prop ("artifactLocation", "uri", "3.4.3");
  auto uri = get_optional_property<json::string> (artifact_loc, uri_prop);

  // §3.4.5 "index" property
  const property_spec_ref index_prop ("artifactLocation", "index", "3.4.5");
  auto index = get_optional_property<json::integer_number> (artifact_loc,
							    index_prop);
  if (uri == nullptr && index == nullptr)
    {
      object_spec_ref constraints ("artifactLocation", "3.4.2");
      return report_invalid_sarif_at_least_one_of (artifact_loc,
						   constraints,
						   uri_prop,
						   index_prop);
    }

  if (uri)
    {
      // TODO: source language
      out = m_output_mgr.new_file (uri->get_string (), nullptr);
      return status::ok;
    }

  return status::ok;
}

/* Handle a "region" object (§3.30) within FILE, writing to OUT.  */

enum status
sarif_replayer::
handle_region_object (const json::object &region_obj,
		      libdiagnostics::file file,
		      libdiagnostics::physical_location &out)
{
  gcc_assert (file.m_inner);

  // §3.30.5 "startLine" property
  const property_spec_ref start_line_prop ("region", "startLine", "3.30.5");
  libdiagnostics::physical_location start;
  libdiagnostics::physical_location end;
  if (auto start_line_jnum
      = get_optional_property<json::integer_number> (region_obj,
						     start_line_prop))
    {
      /* Text region defined by line/column properties.  */
      const property_spec_ref start_column_prop
	("region", "startColumn", "3.30.6");
      if (auto start_column_jnum
	  = get_optional_property<json::integer_number> (region_obj,
							 start_column_prop))
	{
	start = m_output_mgr.new_location_from_file_line_column
	  (file, start_line_jnum->get (), start_column_jnum->get ());
	}
      else
	start = m_output_mgr.new_location_from_file_and_line
	  (file, start_line_jnum->get ());

      int end_line = start_line_jnum->get ();
      const property_spec_ref end_line_prop ("region", "endLine", "3.30.7");
      if (auto end_line_jnum
	  = get_optional_property<json::integer_number> (region_obj,
							 end_line_prop))
	end_line = end_line_jnum->get ();

      const property_spec_ref end_column_prop ("region", "endColumn", "3.30.8");
      if (auto end_column_jnum
	  = get_optional_property<json::integer_number> (region_obj,
							 end_column_prop))
	{
	  /* SARIF's endColumn is 1 beyond the final column in the region,
	     whereas GCC's end columns are inclusive.  */
	  end = m_output_mgr.new_location_from_file_line_column
	    (file, end_line, end_column_jnum->get ());
	}
      else
	{
	  // missing "endColumn" means the whole of the rest of the row
	  end = m_output_mgr.new_location_from_file_and_line
	    (file, end_line);
	}

      out = m_output_mgr.new_location_from_range (start, start, end);
    }

  return status::ok;
}

/* Handle a "logicalLocation" object (§3.33), using it to populate OUT.
   Known limitations:
   - doesn't yet handle "parentIndex" property (§3.33.8)
*/

enum status
sarif_replayer::
handle_logical_location_object (const json::object &logical_loc_obj,
				libdiagnostics::logical_location &out)
{
  const property_spec_ref name_prop ("logicalLocation", "name", "3.33.4");
  const char *short_name = nullptr;
  if (auto name_jstr = get_optional_property<json::string> (logical_loc_obj,
							    name_prop))
    short_name = name_jstr->get_string ();

  const property_spec_ref fqname_prop
    ("logicalLocation", "fullyQualifiedName", "3.33.5");
  const char *fully_qualified_name = nullptr;
  if (auto fully_qualified_name_jstr
      = get_optional_property<json::string> (logical_loc_obj, fqname_prop))
    fully_qualified_name = fully_qualified_name_jstr->get_string ();

  const property_spec_ref decorated_name_prop
    ("logicalLocation", "decoratedName", "3.33.6");
  const char *decorated_name = nullptr;
  if (auto decorated_name_jstr
	= get_optional_property<json::string> (logical_loc_obj,
					       decorated_name_prop))
    decorated_name = decorated_name_jstr->get_string ();

  // §3.33.7 "kind" property
  const property_spec_ref kind_prop ("logicalLocation", "kind", "3.33.7");
  enum diagnostic_logical_location_kind_t kind
    = DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION;
  if (auto kind_str = get_optional_property<json::string> (logical_loc_obj,
							   kind_prop))
    {
      const string_property_value<enum diagnostic_logical_location_kind_t>
	kind_values[]
	= {
	    { "function",
	      DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION },
	    { "member",
	      DIAGNOSTIC_LOGICAL_LOCATION_KIND_MEMBER },
	    { "module",
	     DIAGNOSTIC_LOGICAL_LOCATION_KIND_MODULE },
	    { "namespace",
	      DIAGNOSTIC_LOGICAL_LOCATION_KIND_NAMESPACE },
	    { "type",
	      DIAGNOSTIC_LOGICAL_LOCATION_KIND_TYPE },
	    { "returnType",
	      DIAGNOSTIC_LOGICAL_LOCATION_KIND_RETURN_TYPE },
	    { "parameter",
	      DIAGNOSTIC_LOGICAL_LOCATION_KIND_PARAMETER },
	    { "variable",
	      DIAGNOSTIC_LOGICAL_LOCATION_KIND_VARIABLE } };
      auto result
	= get_value_from_json_string<enum diagnostic_logical_location_kind_t>
	    (*kind_str, kind_prop, kind_values, ARRAY_SIZE (kind_values));
      if (result.m_err != status::ok)
	return result.m_err;
      kind = result.m_val;
    }

  libdiagnostics::logical_location parent;
  out = m_output_mgr.new_logical_location (kind,
					   parent,
					   short_name,
					   fully_qualified_name,
					   decorated_name);

  return status::ok;
}

// 3.52.3 reportingDescriptor lookup
// "For an example of the interaction between ruleId and rule.id, see §3.52.4."

const json::object *
sarif_replayer::
lookup_rule_by_id_in_tool (const char *rule_id,
			   const json::object &tool_obj,
			   const json::object *&tool_component_obj)
{
  auto driver_obj
    = get_required_property<json::object> (tool_obj,
					   property_spec_ref ("tool", "driver",
							      "3.18.2"));
  if (!driver_obj)
    return nullptr;

  if (auto rule_obj = lookup_rule_by_id_in_component (rule_id, *driver_obj))
    {
      tool_component_obj = driver_obj;
      return rule_obj;
    }

  // TODO: also handle extensions

  return NULL;
}

const json::object *
sarif_replayer::
lookup_rule_by_id_in_component (const char *rule_id,
				const json::object &tool_component_obj)
{
  const property_spec_ref rules ("toolComponent", "rules", "3.18.2");

  auto rules_arr
    = get_optional_property<json::array> (tool_component_obj, rules);
  if (!rules_arr)
    return nullptr;

  for (auto element : *rules_arr)
    {
      const json::object *reporting_desc_obj
	= require_object_for_element (*element, rules);

      /* reportingDescriptor objects (§3.49).  */
      const property_spec_ref id ("reportingDescriptor", "id", "3.49.3");
      auto desc_id_jstr
	= get_required_property<json::string> (*reporting_desc_obj, id);
      if (!desc_id_jstr)
	return nullptr;

      if (!strcmp (rule_id, desc_id_jstr->get_string ()))
	return reporting_desc_obj;
    }

  /* Not found.  */
  return nullptr;
}

} // anonymous namespace

/* Error-checking at the API boundary.  */

#define FAIL_IF_NULL(PTR_ARG) \
  do {						    \
    GCC_DIAGNOSTIC_PUSH_IGNORED(-Wnonnull-compare)  \
    if (!(PTR_ARG)) {				    \
      fprintf (stderr, "%s: %s must be non-NULL\n", \
	       __func__, #PTR_ARG);		    \
      abort ();					    \
    }						    \
    GCC_DIAGNOSTIC_POP				    \
  } while (0)

/* Public entrypoint.  */

int
sarif_replay_path (const char *sarif_file,
		   diagnostic_manager *output_manager,
		   diagnostic_manager *control_manager,
		   const replay_options *options)
{
  FAIL_IF_NULL (sarif_file);
  FAIL_IF_NULL (output_manager);
  FAIL_IF_NULL (control_manager);
  FAIL_IF_NULL (options);

  sarif_replayer r (libdiagnostics::manager (output_manager, false),
		    libdiagnostics::manager (control_manager, false));
  return (int)r.replay_file (sarif_file, *options);
}
