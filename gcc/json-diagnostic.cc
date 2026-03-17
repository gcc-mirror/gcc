/* Diagnostics relating to JSON values.
   Copyright (C) 2026 Free Software Foundation, Inc.
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

#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "diagnostic.h"
#include "json-diagnostic.h"
#include "diagnostics/dumping.h"
#include "diagnostics/logging.h"
#include "diagnostics/logical-locations.h"
#include "diagnostics/client-data-hooks.h"
#include "diagnostics/text-sink.h"
#include "diagnostics/physical-location-maker.h"
#include "pretty-print-markup.h"

static bool
emit_json_diagnostic (gcc_json_context &ctxt,
		      enum diagnostics::kind kind,
		      const json::value &js_val,
		      diagnostics::option_id option_id,
		      const char *gmsgid, va_list *ap)
  ATTRIBUTE_GCC_DIAG(5,0);

using log_function_params = diagnostics::logging::log_function_params;
using auto_inc_log_depth = diagnostics::logging::auto_inc_depth;

class json_logical_location_manager
  : public diagnostics::logical_locations::manager
{
public:
  using key = diagnostics::logical_locations::key;
  using kind = diagnostics::logical_locations::kind;

  void
  dump (FILE *outfile, int indent) const final override
  {
    diagnostics::dumping::emit_heading (outfile, indent,
					"json_logical_location_manager");
  }

  label_text
  get_short_name (key k) const final override
  {
    auto *js_val = js_from_key (k);
    const json::pointer::token &pointer_token = js_val->get_pointer_token ();
    pretty_printer pp;
    pointer_token.print (&pp);
    return label_text::take (xstrdup (pp_formatted_text (&pp)));
  }

  label_text
  get_name_with_scope (key k) const final override
  {
    auto *js_val = js_from_key (k);
    pretty_printer pp;
    js_val->print_pointer (&pp);
    return label_text::take (xstrdup (pp_formatted_text (&pp)));
  }

  label_text
  get_internal_name (key) const final override
  {
    return label_text ();
  }

  kind
  get_kind (key k) const final override
  {
    auto *js_val = js_from_key (k);

    switch (js_val->get_kind ())
      {
      default:
	gcc_unreachable ();

      case json::JSON_OBJECT:
	return kind::object;

      case json::JSON_ARRAY:
	return kind::array;

      case json::JSON_INTEGER:
      case json::JSON_FLOAT:
      case json::JSON_STRING:
      case json::JSON_TRUE:
      case json::JSON_FALSE:
      case json::JSON_NULL:
	return kind::property;
      }
  }

  label_text
  get_name_for_path_output (key k) const final override
  {
    return get_name_with_scope (k);
  }

  key
  get_parent (key k) const final override
  {
    auto *js_val = js_from_key (k);
    auto &pointer_token = js_val->get_pointer_token ();
    return key_from_js (pointer_token.m_parent);
  }

  static const json::value *
  js_from_key (key k)
  {
    return k.cast_to<const json::value *> ();
  }

  static key
  key_from_js (const json::value *js_val)
  {
    return key::from_ptr (js_val);
  }

private:
  static void
    print_json_pointer_token (pretty_printer *);
};

/* Implementation of diagnostics::client_data_hooks
   for reporting a diagnostic at a particular json::value
   in a JSON input file.

   It wraps another hooks instance, but uses a
   json_logical_location_manager, has a specific
   json::value for the current logical location,
   and treats the SARIF source lang as "json".  */

class json_client_data_hooks : public diagnostics::client_data_hooks_decorator
{
public:
  json_client_data_hooks (const json::value &js_val,
			  const client_data_hooks *inner)
  : diagnostics::client_data_hooks_decorator (inner),
    m_js_val (js_val)
  {}

  const diagnostics::logical_locations::manager *
  get_logical_location_manager () const override
  {
    return &m_logical_loc_mgr;
  }

  diagnostics::logical_locations::key
  get_current_logical_location () const override
  {
    /* Use the json value's pointer as the key.  */
    return json_logical_location_manager::key_from_js (&m_js_val);
  }

  const char *
  maybe_get_sarif_source_language (const char *) const override
  {
    return "json";
  }

private:
  json_logical_location_manager m_logical_loc_mgr;
  const json::value &m_js_val;
};

namespace pp_markup {

/* Print the JSON Pointer of a given json::value in quotes.  */

class quoted_json_pointer : public pp_element
{
public:
  quoted_json_pointer (const json::value &js_val)
  : m_js_val (js_val)
  {
  }

  void
  add_to_phase_2 (context &ctxt) final override
  {
    ctxt.begin_quote ();
    m_js_val.print_pointer (&ctxt.m_pp);
    ctxt.end_quote ();
  }

private:
  const json::value &m_js_val;
};

} // namespace pp_markup

/* text_sink starter for diagnostics relating to JSON.  */

static void
json_text_starter (diagnostics::text_sink &sink,
		   const diagnostics::diagnostic_info *diagnostic)
{
  pretty_printer *pp = sink.get_printer ();

  /* If this isn't the root value, report its json pointer.  */
  diagnostics::logical_locations::key k;
  if (auto data_hooks = sink.get_context ().get_client_data_hooks ())
    k = data_hooks->get_current_logical_location ();
  const json::value *js_val = json_logical_location_manager::js_from_key (k);
  if (js_val && js_val->get_pointer_token ().m_parent)
    {
      const char *file = LOCATION_FILE (diagnostic_location (diagnostic));
      char *new_prefix = file ?  sink.file_name_as_prefix (file) : nullptr;
      pp_set_prefix (pp, new_prefix);

      pp_markup::quoted_json_pointer e (*js_val);
      switch (js_val->get_kind ())
	{
	default:
	  pp_printf (pp, _("In JSON value %e"), &e);
	  break;
	case json::JSON_OBJECT:
	  pp_printf (pp, _("In JSON object %e"), &e);
	  break;
	case json::JSON_ARRAY:
	  pp_printf (pp, _("In JSON array %e"), &e);
	  break;
	}
      pp_newline (pp);
    }

  pp_set_prefix (pp, sink.build_prefix (*diagnostic));
}


/* class gcc_json_context : public json::simple_location_map.  */

location_t
gcc_json_context::make_location_for_point (const json::location_map::point &p)
{
  diagnostics::physical_location_maker m (line_table);
  return m.new_location_from_file_line_column (m_filename,
					       p.m_line,
					       p.m_column + 1);
}

location_t
gcc_json_context::make_location_for_range (const json::location_map::range &r)
{
  location_t start_loc = make_location_for_point (r.m_start);
  location_t end_loc = make_location_for_point (r.m_end);
  return make_location (start_loc, start_loc, end_loc);
}

/* Emit a diagnostic on global_dc of the relevant KIND relating to JS_VAL,
   using CTXT to get at file/line/column info.

   Temporarily override global_dc's logical location to refer to JS_VAL
   and the text_sink starter and finalizer to be suitable for handling
   reporting within a JSON file.  */

static bool
emit_json_diagnostic (gcc_json_context &ctxt,
		      enum diagnostics::kind kind,
		      const json::value &js_val,
		      diagnostics::option_id option_id,
		      const char *gmsgid, va_list *ap)
{
  auto logger = global_dc->get_logger ();
  log_function_params (logger, __func__)
    .log_param_option_id ("option_id", option_id)
    .log_param_string ("gmsgid", gmsgid);
  auto_inc_log_depth depth_sentinel (logger);

  auto_diagnostic_group d;

  // Get physical location for JS_VAL.
  location_t phys_loc
    = ctxt.make_location_for_range (ctxt.get_range_for_value (js_val));
  rich_location richloc (line_table, phys_loc);

  // Set logical location by overriding client data hooks
  auto tmp_client_data_hooks
    = std::make_unique<json_client_data_hooks>
	(js_val, global_dc->get_client_data_hooks ());
  auto old_client_data_hooks
    = global_dc->set_client_data_hooks (std::move (tmp_client_data_hooks));

  // Override text hooks
  auto old_text_starter = text_starter (global_dc);
  auto old_text_finalizer = text_finalizer (global_dc);
  text_starter (global_dc) = json_text_starter;
  text_finalizer (global_dc) = diagnostics::default_text_finalizer;

  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_id,
					 gmsgid, ap, kind);

  // Restore old text and client data hooks:
  text_starter (global_dc) = old_text_starter;
  text_finalizer (global_dc) = old_text_finalizer;
  global_dc->set_client_data_hooks (std::move (old_client_data_hooks));

  if (logger)
    logger->log_bool_return ("emit_diagnostic", ret);

  return ret;
}


/* Emit an error on gcc's global_dc relating to JS_VAL.  */

void
json_error (gcc_json_context &ctxt,
	    const json::value &js_val,
	    const char *gmsgid, ...)
{
  va_list ap;
  va_start (ap, gmsgid);
  emit_json_diagnostic (ctxt,
			diagnostics::kind::error,
			js_val, -1,
			gmsgid, &ap);
  va_end (ap);
}

/* Emit a warning on gcc's global_dc relating to JS_VAL.  */

bool
json_warning (gcc_json_context &ctxt,
	      const json::value &js_val,
	      diagnostics::option_id option_id,
	      const char *gmsgid, ...)
{
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = emit_json_diagnostic (ctxt,
				   diagnostics::kind::warning,
				   js_val, option_id,
				   gmsgid, &ap);
  va_end (ap);
  return ret;
}

/* Emit a note on gcc's global_dc relating to JS_VAL.  */

void
json_note (gcc_json_context &ctxt,
	   const json::value &js_val,
	   const char *gmsgid, ...)
{
  va_list ap;
  va_start (ap, gmsgid);
  emit_json_diagnostic (ctxt,
			diagnostics::kind::note,
			js_val, -1,
			gmsgid, &ap);
  va_end (ap);
}
