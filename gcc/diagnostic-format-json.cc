/* JSON output for diagnostics
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
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "selftest-diagnostic.h"
#include "diagnostic-metadata.h"
#include "json.h"
#include "selftest.h"

/* Subclass of diagnostic_output_format for JSON output.  */

class json_output_format : public diagnostic_output_format
{
public:
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
  on_begin_diagnostic (const diagnostic_info &) final override
  {
    /* No-op.  */
  }
  void
  on_end_diagnostic (const diagnostic_info &diagnostic,
		     diagnostic_t orig_diag_kind) final override;
  void on_diagram (const diagnostic_diagram &) final override
  {
    /* No-op.  */
  }

protected:
  json_output_format (diagnostic_context &context,
		      bool formatted)
  : diagnostic_output_format (context),
    m_toplevel_array (new json::array ()),
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
    delete m_toplevel_array;
    m_toplevel_array = nullptr;
  }

private:
  /* The top-level JSON array of pending diagnostics.  */
  json::array *m_toplevel_array;

  /* The JSON object for the current diagnostic group.  */
  json::object *m_cur_group;

  /* The JSON array for the "children" array within the current diagnostic
     group.  */
  json::array *m_cur_children_array;

  bool m_formatted;
};

/* Generate a JSON object for LOC.  */

json::value *
json_from_expanded_location (diagnostic_context *context, location_t loc)
{
  expanded_location exploc = expand_location (loc);
  json::object *result = new json::object ();
  if (exploc.file)
    result->set_string ("file", exploc.file);
  result->set_integer ("line", exploc.line);

  const enum diagnostics_column_unit orig_unit = context->m_column_unit;
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
      context->m_column_unit = column_fields[i].unit;
      const int col = context->converted_column (exploc);
      result->set_integer (column_fields[i].name, col);
      if (column_fields[i].unit == orig_unit)
	the_column = col;
    }
  gcc_assert (the_column != INT_MIN);
  result->set_integer ("column", the_column);
  context->m_column_unit = orig_unit;
  return result;
}

/* Generate a JSON object for LOC_RANGE.  */

static json::object *
json_from_location_range (diagnostic_context *context,
			  const location_range *loc_range, unsigned range_idx)
{
  location_t caret_loc = get_pure_location (loc_range->m_loc);

  if (caret_loc == UNKNOWN_LOCATION)
    return NULL;

  location_t start_loc = get_start (loc_range->m_loc);
  location_t finish_loc = get_finish (loc_range->m_loc);

  json::object *result = new json::object ();
  result->set ("caret", json_from_expanded_location (context, caret_loc));
  if (start_loc != caret_loc
      && start_loc != UNKNOWN_LOCATION)
    result->set ("start", json_from_expanded_location (context, start_loc));
  if (finish_loc != caret_loc
      && finish_loc != UNKNOWN_LOCATION)
    result->set ("finish", json_from_expanded_location (context, finish_loc));

  if (loc_range->m_label)
    {
      label_text text (loc_range->m_label->get_text (range_idx));
      if (text.get ())
	result->set_string ("label", text.get ());
    }

  return result;
}

/* Generate a JSON object for HINT.  */

static json::object *
json_from_fixit_hint (diagnostic_context *context, const fixit_hint *hint)
{
  json::object *fixit_obj = new json::object ();

  location_t start_loc = hint->get_start_loc ();
  fixit_obj->set ("start", json_from_expanded_location (context, start_loc));
  location_t next_loc = hint->get_next_loc ();
  fixit_obj->set ("next", json_from_expanded_location (context, next_loc));
  fixit_obj->set_string ("string", hint->get_string ());

  return fixit_obj;
}

/* Generate a JSON object for METADATA.  */

static json::object *
json_from_metadata (const diagnostic_metadata *metadata)
{
  json::object *metadata_obj = new json::object ();

  if (metadata->get_cwe ())
    metadata_obj->set_integer ("cwe", metadata->get_cwe ());

  return metadata_obj;
}

/* Implementation of "on_end_diagnostic" vfunc for JSON output.
   Generate a JSON object for DIAGNOSTIC, and store for output
   within current diagnostic group.  */

void
json_output_format::on_end_diagnostic (const diagnostic_info &diagnostic,
				       diagnostic_t orig_diag_kind)
{
  json::object *diag_obj = new json::object ();

  /* Get "kind" of diagnostic.  */
  {
    static const char *const diagnostic_kind_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (T),
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
      "must-not-happen"
    };
    /* Lose the trailing ": ".  */
    const char *kind_text = diagnostic_kind_text[diagnostic.kind];
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
  diag_obj->set_string ("message", pp_formatted_text (m_context.printer));
  pp_clear_output_area (m_context.printer);

  if (char *option_text = m_context.make_option_name (diagnostic.option_index,
						      orig_diag_kind,
						      diagnostic.kind))
    {
      diag_obj->set_string ("option", option_text);
      free (option_text);
    }

  if (char *option_url = m_context.make_option_url (diagnostic.option_index))
    {
      diag_obj->set_string ("option_url", option_url);
      free (option_url);
    }

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
      m_toplevel_array->append (diag_obj);
      m_cur_group = diag_obj;
      m_cur_children_array = new json::array ();
      diag_obj->set ("children", m_cur_children_array);
      diag_obj->set_integer ("column-origin", m_context.m_column_origin);
    }

  const rich_location *richloc = diagnostic.richloc;

  json::array *loc_array = new json::array ();
  diag_obj->set ("locations", loc_array);

  for (unsigned int i = 0; i < richloc->get_num_locations (); i++)
    {
      const location_range *loc_range = richloc->get_range (i);
      json::object *loc_obj
	= json_from_location_range (&m_context, loc_range, i);
      if (loc_obj)
	loc_array->append (loc_obj);
    }

  if (richloc->get_num_fixit_hints ())
    {
      json::array *fixit_array = new json::array ();
      diag_obj->set ("fixits", fixit_array);
      for (unsigned int i = 0; i < richloc->get_num_fixit_hints (); i++)
	{
	  const fixit_hint *hint = richloc->get_fixit_hint (i);
	  json::object *fixit_obj = json_from_fixit_hint (&m_context, hint);
	  fixit_array->append (fixit_obj);
	}
    }

  /* TODO: tree-ish things:
     TODO: functions
     TODO: inlining information
     TODO: macro expansion information.  */

  if (diagnostic.metadata)
    {
      json::object *metadata_obj = json_from_metadata (diagnostic.metadata);
      diag_obj->set ("metadata", metadata_obj);
    }

  const diagnostic_path *path = richloc->get_path ();
  if (path && m_context.m_make_json_for_path)
    {
      json::value *path_value
	= m_context.m_make_json_for_path (&m_context, path);
      diag_obj->set ("path", path_value);
    }

  diag_obj->set ("escape-source",
		 new json::literal (richloc->escape_on_output_p ()));
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
    char *filename = concat (m_base_file_name, ".gcc.json", NULL);
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
diagnostic_output_format_init_json (diagnostic_context *context)
{
  /* Override callbacks.  */
  context->m_print_path = nullptr; /* handled in json_end_diagnostic.  */

  /* The metadata is handled in JSON format, rather than as text.  */
  context->set_show_cwe (false);
  context->set_show_rules (false);

  /* The option is handled in JSON format, rather than as text.  */
  context->set_show_option_requested (false);

  /* Don't colorize the text.  */
  pp_show_color (context->printer) = false;
}

/* Populate CONTEXT in preparation for JSON output to stderr.  */

void
diagnostic_output_format_init_json_stderr (diagnostic_context *context,
					   bool formatted)
{
  diagnostic_output_format_init_json (context);
  context->set_output_format (new json_stderr_output_format (*context,
							     formatted));
}

/* Populate CONTEXT in preparation for JSON output to a file named
   BASE_FILE_NAME.gcc.json.  */

void
diagnostic_output_format_init_json_file (diagnostic_context *context,
					 bool formatted,
					 const char *base_file_name)
{
  diagnostic_output_format_init_json (context);
  context->set_output_format (new json_file_output_format (*context,
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
  delete json_from_expanded_location (&dc, UNKNOWN_LOCATION);
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
  loc_range.m_label = NULL;

  test_diagnostic_context dc;
  json::object *obj = json_from_location_range (&dc, &loc_range, 0);
  /* We should have a "caret" value, but no "start" or "finish" values.  */
  ASSERT_TRUE (obj != NULL);
  ASSERT_TRUE (obj->get ("caret") != NULL);
  ASSERT_TRUE (obj->get ("start") == NULL);
  ASSERT_TRUE (obj->get ("finish") == NULL);
  delete obj;
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
