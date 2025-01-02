/* JSON output for diagnostics
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
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
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "selftest-diagnostic.h"
#include "diagnostic-metadata.h"
#include "diagnostic-path.h"
#include "diagnostic-format.h"
#include "diagnostic-buffer.h"
#include "json.h"
#include "selftest.h"
#include "logical-location.h"
#include "make-unique.h"

class json_output_format;

/* Concrete buffering implementation subclass for JSON output.  */

class diagnostic_json_format_buffer : public diagnostic_per_format_buffer
{
public:
  friend class json_output_format;

  diagnostic_json_format_buffer (json_output_format &format)
  : m_format (format)
  {}

  void dump (FILE *out, int indent) const final override;
  bool empty_p () const final override;
  void move_to (diagnostic_per_format_buffer &dest) final override;
  void clear () final override;
  void flush () final override;

private:
  json_output_format &m_format;
  std::vector<std::unique_ptr<json::object>> m_results;
};

/* Subclass of diagnostic_output_format for JSON output.  */

class json_output_format : public diagnostic_output_format
{
public:
  friend class diagnostic_json_format_buffer;

  void dump (FILE *out, int indent) const override
  {
    fprintf (out, "%*sjson_output_format\n", indent, "");
    diagnostic_output_format::dump (out, indent);
  }

  std::unique_ptr<diagnostic_per_format_buffer>
  make_per_format_buffer () final override
  {
    return ::make_unique<diagnostic_json_format_buffer> (*this);
  }
  void set_buffer (diagnostic_per_format_buffer *base_buffer) final override
  {
    diagnostic_json_format_buffer *buffer
      = static_cast<diagnostic_json_format_buffer *> (base_buffer);
    m_buffer = buffer;
  }

  void on_begin_group () final override
  {
    /* No-op.  */
  }
  void on_end_group () final override
  {
    m_cur_group = nullptr;
    m_cur_children_array = nullptr;
  }
  void
  on_report_diagnostic (const diagnostic_info &diagnostic,
			diagnostic_t orig_diag_kind) final override;
  void on_diagram (const diagnostic_diagram &) final override
  {
    /* No-op.  */
  }
  void after_diagnostic (const diagnostic_info &) final override
  {
    /* No-op.  */
  }
  void update_printer () final override
  {
    m_printer = m_context.clone_printer ();
    pp_show_color (m_printer.get ()) = false;
  }
  bool follows_reference_printer_p () const final override
  {
    return false;
  }

protected:
  json_output_format (diagnostic_context &context,
		      bool formatted)
  : diagnostic_output_format (context),
    m_buffer (nullptr),
    m_toplevel_array (::make_unique<json::array> ()),
    m_cur_group (nullptr),
    m_cur_children_array (nullptr),
    m_formatted (formatted)
  {
  }

  /* Flush the top-level array to OUTF.  */
  void
  flush_to_file (FILE *outf)
  {
    m_toplevel_array->dump (outf, m_formatted);
    fprintf (outf, "\n");
    m_toplevel_array = nullptr;
  }

private:
  diagnostic_json_format_buffer *m_buffer;

  /* The top-level JSON array of pending diagnostics.  */
  std::unique_ptr<json::array> m_toplevel_array;

  /* The JSON object for the current diagnostic group.  */
  json::object *m_cur_group; // borrowed

  /* The JSON array for the "children" array within the current diagnostic
     group.  */
  json::array *m_cur_children_array; // borrowed

  bool m_formatted;
};

/* Generate a JSON object for LOC.  */

static std::unique_ptr<json::object>
json_from_expanded_location (diagnostic_context &context, location_t loc)
{
  expanded_location exploc = expand_location (loc);
  std::unique_ptr<json::object> result = ::make_unique <json::object> ();
  if (exploc.file)
    result->set_string ("file", exploc.file);
  result->set_integer ("line", exploc.line);

  const enum diagnostics_column_unit orig_unit = context.m_column_unit;
  struct
  {
    const char *name;
    enum diagnostics_column_unit unit;
  } column_fields[] = {
    {"display-column", DIAGNOSTICS_COLUMN_UNIT_DISPLAY},
    {"byte-column", DIAGNOSTICS_COLUMN_UNIT_BYTE}
  };
  int the_column = INT_MIN;
  for (int i = 0; i != ARRAY_SIZE (column_fields); ++i)
    {
      context.m_column_unit = column_fields[i].unit;
      diagnostic_column_policy col_policy (context);
      const int col = col_policy.converted_column (exploc);
      result->set_integer (column_fields[i].name, col);
      if (column_fields[i].unit == orig_unit)
	the_column = col;
    }
  gcc_assert (the_column != INT_MIN);
  result->set_integer ("column", the_column);
  context.m_column_unit = orig_unit;
  return result;
}

/* Generate a JSON object for LOC_RANGE.  */

static std::unique_ptr<json::object>
json_from_location_range (diagnostic_context &context,
			  const location_range *loc_range, unsigned range_idx)
{
  location_t caret_loc = get_pure_location (loc_range->m_loc);

  if (caret_loc == UNKNOWN_LOCATION)
    return nullptr;

  location_t start_loc = get_start (loc_range->m_loc);
  location_t finish_loc = get_finish (loc_range->m_loc);

  std::unique_ptr<json::object> result = ::make_unique <json::object> ();
  result->set ("caret",
	       json_from_expanded_location (context, caret_loc));
  if (start_loc != caret_loc
      && start_loc != UNKNOWN_LOCATION)
    result->set ("start",
		 json_from_expanded_location (context, start_loc));
  if (finish_loc != caret_loc
      && finish_loc != UNKNOWN_LOCATION)
    result->set ("finish",
		 json_from_expanded_location (context, finish_loc));

  if (loc_range->m_label)
    {
      label_text text (loc_range->m_label->get_text (range_idx));
      if (text.get ())
	result->set_string ("label", text.get ());
    }

  return result;
}

/* Generate a JSON object for HINT.  */

static std::unique_ptr<json::object>
json_from_fixit_hint (diagnostic_context &context, const fixit_hint *hint)
{
  std::unique_ptr<json::object> fixit_obj = ::make_unique <json::object> ();

  location_t start_loc = hint->get_start_loc ();
  fixit_obj->set ("start",
		  json_from_expanded_location (context, start_loc));
  location_t next_loc = hint->get_next_loc ();
  fixit_obj->set ("next",
		  json_from_expanded_location (context, next_loc). release ());
  fixit_obj->set_string ("string", hint->get_string ());

  return fixit_obj;
}

/* Generate a JSON object for METADATA.  */

static std::unique_ptr<json::object>
json_from_metadata (const diagnostic_metadata *metadata)
{
  std::unique_ptr<json::object> metadata_obj = ::make_unique <json::object> ();

  if (metadata->get_cwe ())
    metadata_obj->set_integer ("cwe", metadata->get_cwe ());

  return metadata_obj;
}

/* Make a JSON value for PATH.  */

static std::unique_ptr<json::array>
make_json_for_path (diagnostic_context &context,
		    pretty_printer *ref_pp,
		    const diagnostic_path *path)
{
  std::unique_ptr<json::array> path_array = ::make_unique<json::array> ();
  for (unsigned i = 0; i < path->num_events (); i++)
    {
      const diagnostic_event &event = path->get_event (i);

      std::unique_ptr<json::object> event_obj = ::make_unique <json::object> ();
      if (event.get_location ())
	event_obj->set ("location",
			json_from_expanded_location (context,
						     event.get_location ()));
      auto pp = ref_pp->clone ();
      event.print_desc (*pp.get ());
      event_obj->set_string ("description", pp_formatted_text (pp.get ()));
      if (const logical_location *logical_loc = event.get_logical_location ())
	{
	  label_text name (logical_loc->get_name_for_path_output ());
	  event_obj->set_string ("function", name.get ());
	}
      event_obj->set_integer ("depth", event.get_stack_depth ());
      path_array->append (std::move (event_obj));
    }
  return path_array;
}

/* class diagnostic_json_format_buffer : public diagnostic_per_format_buffer.  */

void
diagnostic_json_format_buffer::dump (FILE *out, int indent) const
{
  fprintf (out, "%*sdiagnostic_json_format_buffer:\n", indent, "");
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
diagnostic_json_format_buffer::empty_p () const
{
  return m_results.empty ();
}

void
diagnostic_json_format_buffer::move_to (diagnostic_per_format_buffer &base)
{
  diagnostic_json_format_buffer &dest
    = static_cast<diagnostic_json_format_buffer &> (base);
  for (auto &&result : m_results)
    dest.m_results.push_back (std::move (result));
  m_results.clear ();
}

void
diagnostic_json_format_buffer::clear ()
{
  m_results.clear ();
}

void
diagnostic_json_format_buffer::flush ()
{
  for (auto &&result : m_results)
    m_format.m_toplevel_array->append (std::move (result));
  m_results.clear ();
}

/* Implementation of "on_report_diagnostic" vfunc for JSON output.
   Generate a JSON object for DIAGNOSTIC, and store for output
   within current diagnostic group.  */

void
json_output_format::on_report_diagnostic (const diagnostic_info &diagnostic,
					  diagnostic_t orig_diag_kind)
{
  pretty_printer *const pp = get_printer ();
  pp_output_formatted_text (pp, m_context.get_urlifier ());

  json::object *diag_obj = new json::object ();

  /* Get "kind" of diagnostic.  */
  {
    /* Lose the trailing ": ".  */
    const char *kind_text = get_diagnostic_kind_text (diagnostic.kind);
    size_t len = strlen (kind_text);
    gcc_assert (len > 2);
    gcc_assert (kind_text[len - 2] == ':');
    gcc_assert (kind_text[len - 1] == ' ');
    char *rstrip = xstrdup (kind_text);
    rstrip[len - 2] = '\0';
    diag_obj->set_string ("kind", rstrip);
    free (rstrip);
  }

  // FIXME: encoding of the message (json::string requires UTF-8)
  diag_obj->set_string ("message", pp_formatted_text (pp));
  pp_clear_output_area (pp);

  if (char *option_text = m_context.make_option_name (diagnostic.option_id,
						      orig_diag_kind,
						      diagnostic.kind))
    {
      diag_obj->set_string ("option", option_text);
      free (option_text);
    }

  if (char *option_url = m_context.make_option_url (diagnostic.option_id))
    {
      diag_obj->set_string ("option_url", option_url);
      free (option_url);
    }

  if (m_buffer)
    {
      gcc_assert (!m_cur_group);
      m_buffer->m_results.push_back (std::unique_ptr<json::object> (diag_obj));
    }
  else
    {
      /* If we've already emitted a diagnostic within this auto_diagnostic_group,
	 then add diag_obj to its "children" array.  */
      if (m_cur_group)
	{
	  gcc_assert (m_cur_children_array);
	  m_cur_children_array->append (diag_obj);
	}
      else
	{
	  /* Otherwise, make diag_obj be the top-level object within the group;
	     add a "children" array and record the column origin.  */
	  m_cur_group = diag_obj;
	  std::unique_ptr<json::array> children_array
	    = ::make_unique<json::array> ();
	  m_cur_children_array = children_array.get (); // borrowed
	  diag_obj->set ("children", std::move (children_array));
	  diag_obj->set_integer ("column-origin", m_context.m_column_origin);
	  m_toplevel_array->append (diag_obj);
	}
    }

  /* diag_obj is now owned by either m_cur_children_array or
     m_toplevel_array; further uses of diag_obj are borrowing it.  */

  const rich_location *richloc = diagnostic.richloc;

  {
    std::unique_ptr<json::array> loc_array = ::make_unique<json::array> ();
    for (unsigned int i = 0; i < richloc->get_num_locations (); i++)
      {
	const location_range *loc_range = richloc->get_range (i);
	if (std::unique_ptr<json::object> loc_obj
	      = json_from_location_range (m_context, loc_range, i))
	  loc_array->append (std::move (loc_obj));
      }
    diag_obj->set ("locations", std::move (loc_array));
  }

  if (richloc->get_num_fixit_hints ())
    {
      std::unique_ptr<json::array> fixit_array = ::make_unique<json::array> ();
      for (unsigned int i = 0; i < richloc->get_num_fixit_hints (); i++)
	{
	  const fixit_hint *hint = richloc->get_fixit_hint (i);
	  fixit_array->append (json_from_fixit_hint (m_context, hint));
	}
      diag_obj->set ("fixits", std::move (fixit_array));
    }

  /* TODO: tree-ish things:
     TODO: functions
     TODO: inlining information
     TODO: macro expansion information.  */

  if (diagnostic.metadata)
    diag_obj->set ("metadata", json_from_metadata (diagnostic.metadata));

  const diagnostic_path *path = richloc->get_path ();
  if (path)
    diag_obj->set ("path", make_json_for_path (m_context, get_printer (), path));

  diag_obj->set_bool ("escape-source", richloc->escape_on_output_p ());
}

class json_stderr_output_format : public json_output_format
{
public:
  json_stderr_output_format (diagnostic_context &context,
			     bool formatted)
    : json_output_format (context, formatted)
  {
  }
  ~json_stderr_output_format ()
  {
    flush_to_file (stderr);
  }
  bool machine_readable_stderr_p () const final override
  {
    return true;
  }
};

class json_file_output_format : public json_output_format
{
public:
  json_file_output_format (diagnostic_context &context,
			   bool formatted,
			   const char *base_file_name)
  : json_output_format (context, formatted),
    m_base_file_name (xstrdup (base_file_name))
  {
  }

  ~json_file_output_format ()
  {
    char *filename = concat (m_base_file_name, ".gcc.json", nullptr);
    free (m_base_file_name);
    m_base_file_name = nullptr;
    FILE *outf = fopen (filename, "w");
    if (!outf)
      {
	const char *errstr = xstrerror (errno);
	fnotice (stderr, "error: unable to open '%s' for writing: %s\n",
		 filename, errstr);
	free (filename);
	return;
      }
    flush_to_file (outf);
    fclose (outf);
    free (filename);
  }
  bool machine_readable_stderr_p () const final override
  {
    return false;
  }

private:
  char *m_base_file_name;
};

/* Populate CONTEXT in preparation for JSON output (either to stderr, or
   to a file).  */

static void
diagnostic_output_format_init_json (diagnostic_context &context,
				    std::unique_ptr<json_output_format> fmt)
{
  /* Don't colorize the text.  */
  pp_show_color (fmt->get_printer ()) = false;
  context.set_show_highlight_colors (false);

  context.set_output_format (std::move (fmt));
}

/* Populate CONTEXT in preparation for JSON output to stderr.  */

void
diagnostic_output_format_init_json_stderr (diagnostic_context &context,
					   bool formatted)
{
  diagnostic_output_format_init_json
    (context,
     ::make_unique<json_stderr_output_format> (context,
					       formatted));
}

/* Populate CONTEXT in preparation for JSON output to a file named
   BASE_FILE_NAME.gcc.json.  */

void
diagnostic_output_format_init_json_file (diagnostic_context &context,
					 bool formatted,
					 const char *base_file_name)
{
  diagnostic_output_format_init_json
    (context,
     ::make_unique<json_file_output_format> (context,
					     formatted,
					     base_file_name));
}

#if CHECKING_P

namespace selftest {

/* We shouldn't call json_from_expanded_location on UNKNOWN_LOCATION,
   but verify that we handle this gracefully.  */

static void
test_unknown_location ()
{
  test_diagnostic_context dc;
  json_from_expanded_location (dc, UNKNOWN_LOCATION);
}

/* Verify that we gracefully handle attempts to serialize bad
   compound locations.  */

static void
test_bad_endpoints ()
{
  location_t bad_endpoints
    = make_location (BUILTINS_LOCATION,
		     UNKNOWN_LOCATION, UNKNOWN_LOCATION);

  location_range loc_range;
  loc_range.m_loc = bad_endpoints;
  loc_range.m_range_display_kind = SHOW_RANGE_WITH_CARET;
  loc_range.m_label = nullptr;

  test_diagnostic_context dc;
  std::unique_ptr<json::object> obj
    = json_from_location_range (dc, &loc_range, 0);
  /* We should have a "caret" value, but no "start" or "finish" values.  */
  ASSERT_TRUE (obj != nullptr);
  ASSERT_TRUE (obj->get ("caret") != nullptr);
  ASSERT_TRUE (obj->get ("start") == nullptr);
  ASSERT_TRUE (obj->get ("finish") == nullptr);
}

/* Run all of the selftests within this file.  */

void
diagnostic_format_json_cc_tests ()
{
  test_unknown_location ();
  test_bad_endpoints ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
