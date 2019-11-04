/* JSON output for diagnostics
   Copyright (C) 2018-2019 Free Software Foundation, Inc.
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
#include "json.h"
#include "selftest.h"

/* The top-level JSON array of pending diagnostics.  */

static json::array *toplevel_array;

/* The JSON object for the current diagnostic group.  */

static json::object *cur_group;

/* The JSON array for the "children" array within the current diagnostic
   group.  */

static json::array *cur_children_array;

/* Generate a JSON object for LOC.  */

static json::object *
json_from_expanded_location (location_t loc)
{
  expanded_location exploc = expand_location (loc);
  json::object *result = new json::object ();
  if (exploc.file)
    result->set ("file", new json::string (exploc.file));
  result->set ("line", new json::integer_number (exploc.line));
  result->set ("column", new json::integer_number (exploc.column));
  return result;
}

/* Generate a JSON object for LOC_RANGE.  */

static json::object *
json_from_location_range (const location_range *loc_range, unsigned range_idx)
{
  location_t caret_loc = get_pure_location (loc_range->m_loc);

  if (caret_loc == UNKNOWN_LOCATION)
    return NULL;

  location_t start_loc = get_start (loc_range->m_loc);
  location_t finish_loc = get_finish (loc_range->m_loc);

  json::object *result = new json::object ();
  result->set ("caret", json_from_expanded_location (caret_loc));
  if (start_loc != caret_loc
      && start_loc != UNKNOWN_LOCATION)
    result->set ("start", json_from_expanded_location (start_loc));
  if (finish_loc != caret_loc
      && finish_loc != UNKNOWN_LOCATION)
    result->set ("finish", json_from_expanded_location (finish_loc));

  if (loc_range->m_label)
    {
      label_text text;
      text = loc_range->m_label->get_text (range_idx);
      if (text.m_buffer)
	result->set ("label", new json::string (text.m_buffer));
      text.maybe_free ();
    }

  return result;
}

/* Generate a JSON object for HINT.  */

static json::object *
json_from_fixit_hint (const fixit_hint *hint)
{
  json::object *fixit_obj = new json::object ();

  location_t start_loc = hint->get_start_loc ();
  fixit_obj->set ("start", json_from_expanded_location (start_loc));
  location_t next_loc = hint->get_next_loc ();
  fixit_obj->set ("next", json_from_expanded_location (next_loc));
  fixit_obj->set ("string", new json::string (hint->get_string ()));

  return fixit_obj;
}

/* No-op implementation of "begin_diagnostic" for JSON output.  */

static void
json_begin_diagnostic (diagnostic_context *, diagnostic_info *)
{
}

/* Implementation of "end_diagnostic" for JSON output.
   Generate a JSON object for DIAGNOSTIC, and store for output
   within current diagnostic group.  */

static void
json_end_diagnostic (diagnostic_context *context, diagnostic_info *diagnostic,
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
    const char *kind_text = diagnostic_kind_text[diagnostic->kind];
    size_t len = strlen (kind_text);
    gcc_assert (len > 2);
    gcc_assert (kind_text[len - 2] == ':');
    gcc_assert (kind_text[len - 1] == ' ');
    char *rstrip = xstrdup (kind_text);
    rstrip[len - 2] = '\0';
    diag_obj->set ("kind", new json::string (rstrip));
    free (rstrip);
  }

  // FIXME: encoding of the message (json::string requires UTF-8)
  diag_obj->set ("message",
		 new json::string (pp_formatted_text (context->printer)));
  pp_clear_output_area (context->printer);

  char *option_text;
  option_text = context->option_name (context, diagnostic->option_index,
				      orig_diag_kind, diagnostic->kind);
  if (option_text)
    {
      diag_obj->set ("option", new json::string (option_text));
      free (option_text);
    }

  if (context->get_option_url)
    {
      char *option_url = context->get_option_url (context,
						  diagnostic->option_index);
      if (option_url)
	{
	  diag_obj->set ("option_url", new json::string (option_url));
	  free (option_url);
	}
    }

  /* If we've already emitted a diagnostic within this auto_diagnostic_group,
     then add diag_obj to its "children" array.  */
  if (cur_group)
    {
      gcc_assert (cur_children_array);
      cur_children_array->append (diag_obj);
    }
  else
    {
      /* Otherwise, make diag_obj be the top-level object within the group;
	 add a "children" array.  */
      toplevel_array->append (diag_obj);
      cur_group = diag_obj;
      cur_children_array = new json::array ();
      diag_obj->set ("children", cur_children_array);
    }

  const rich_location *richloc = diagnostic->richloc;

  json::array *loc_array = new json::array ();
  diag_obj->set ("locations", loc_array);

  for (unsigned int i = 0; i < richloc->get_num_locations (); i++)
    {
      const location_range *loc_range = richloc->get_range (i);
      json::object *loc_obj = json_from_location_range (loc_range, i);
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
	  json::object *fixit_obj = json_from_fixit_hint (hint);
	  fixit_array->append (fixit_obj);
	}
    }

  /* TODO: tree-ish things:
     TODO: functions
     TODO: inlining information
     TODO: macro expansion information.  */
}

/* No-op implementation of "begin_group_cb" for JSON output.  */

static void
json_begin_group (diagnostic_context *)
{
}

/* Implementation of "end_group_cb" for JSON output.  */

static void
json_end_group (diagnostic_context *)
{
  cur_group = NULL;
  cur_children_array = NULL;
}

/* Callback for final cleanup for JSON output.  */

static void
json_final_cb (diagnostic_context *)
{
  /* Flush the top-level array.  */
  toplevel_array->dump (stderr);
  fprintf (stderr, "\n");
  delete toplevel_array;
  toplevel_array = NULL;
}

/* Set the output format for CONTEXT to FORMAT.  */

void
diagnostic_output_format_init (diagnostic_context *context,
			       enum diagnostics_output_format format)
{
  switch (format)
    {
    default:
      gcc_unreachable ();
    case DIAGNOSTICS_OUTPUT_FORMAT_TEXT:
      /* The default; do nothing.  */
      break;

    case DIAGNOSTICS_OUTPUT_FORMAT_JSON:
      {
	/* Set up top-level JSON array.  */
	if (toplevel_array == NULL)
	  toplevel_array = new json::array ();

	/* Override callbacks.  */
	context->begin_diagnostic = json_begin_diagnostic;
	context->end_diagnostic = json_end_diagnostic;
	context->begin_group_cb = json_begin_group;
	context->end_group_cb =  json_end_group;
	context->final_cb =  json_final_cb;

	/* The option is handled in JSON format, rather than as text.  */
	context->show_option_requested = false;

	/* Don't colorize the text.  */
	pp_show_color (context->printer) = false;
      }
      break;
    }
}

#if CHECKING_P

namespace selftest {

/* We shouldn't call json_from_expanded_location on UNKNOWN_LOCATION,
   but verify that we handle this gracefully.  */

static void
test_unknown_location ()
{
  delete json_from_expanded_location (UNKNOWN_LOCATION);
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

  json::object *obj = json_from_location_range (&loc_range, 0);
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
