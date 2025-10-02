/* Classic text-based output of diagnostics.
   Copyright (C) 1999-2025 Free Software Foundation, Inc.

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
#include "version.h"
#include "intl.h"
#include "diagnostics/color.h"
#include "diagnostics/url.h"
#include "diagnostics/metadata.h"
#include "diagnostics/paths.h"
#include "diagnostics/client-data-hooks.h"
#include "diagnostics/diagram.h"
#include "diagnostics/text-sink.h"
#include "diagnostics/buffering.h"
#include "diagnostics/dumping.h"
#include "diagnostics/logging.h"
#include "text-art/theme.h"

/* Disable warnings about quoting issues in the pp_xxx calls below
   that (intentionally) don't follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

namespace diagnostics {

/* Concrete buffering implementation subclass for text output.  */

class text_sink_buffer : public per_sink_buffer
{
public:
  friend class text_sink;

  text_sink_buffer (sink &sink_);

  void dump (FILE *out, int indent) const final override;

  bool empty_p () const final override;
  void move_to (per_sink_buffer &dest) final override;
  void clear () final override;
  void flush () final override;

private:
  sink &m_sink;
  output_buffer m_output_buffer;
};

/* class text_sink_buffer : public per_sink_buffer.  */

text_sink_buffer::text_sink_buffer (sink &sink_)
: m_sink (sink_)
{
  m_output_buffer.m_flush_p = false;
}

void
text_sink_buffer::dump (FILE *out, int indent) const
{
  dumping::emit_heading (out, indent, "text_sink_buffer");
  m_output_buffer.dump (out, indent + 2);
}

bool
text_sink_buffer::empty_p () const
{
  return output_buffer_last_position_in_text (&m_output_buffer) == nullptr;
}

void
text_sink_buffer::move_to (per_sink_buffer &base_dest)
{
  text_sink_buffer &dest
    = static_cast<text_sink_buffer &> (base_dest);
  const char *str = output_buffer_formatted_text (&m_output_buffer);
  output_buffer_append_r (&dest.m_output_buffer, str, strlen (str));

  obstack_free (m_output_buffer.m_obstack,
		obstack_base (m_output_buffer.m_obstack));
  m_output_buffer.m_line_length = 0;
}

void
text_sink_buffer::clear ()
{
  pretty_printer *const pp = m_sink.get_printer ();
  output_buffer *const old_output_buffer = pp_buffer (pp);

  pp_buffer (pp) = &m_output_buffer;

  pp_clear_output_area (pp);
  gcc_assert (empty_p ());

  pp_buffer (pp) = old_output_buffer;
}

void
text_sink_buffer::flush ()
{
  pretty_printer *const pp = m_sink.get_printer ();
  output_buffer *const old_output_buffer = pp_buffer (pp);

  pp_buffer (pp) = &m_output_buffer;

  pp_really_flush (pp);
  gcc_assert (empty_p ());

  pp_buffer (pp) = old_output_buffer;
}

/* class diagnostics::text_sink : public diagnostics::sink.  */

text_sink::~text_sink ()
{
  /* Some of the errors may actually have been warnings.  */
  if (m_context.diagnostic_count (kind::werror))
    {
      pretty_printer *pp = get_printer ();
      /* -Werror was given.  */
      if (m_context.warning_as_error_requested_p ())
	pp_verbatim (pp,
		     _("%s: all warnings being treated as errors"),
		     progname);
      /* At least one -Werror= was given.  */
      else
	pp_verbatim (pp,
		     _("%s: some warnings being treated as errors"),
		     progname);
      pp_newline_and_flush (pp);
    }

  if (m_includes_seen)
    {
      delete m_includes_seen;
      m_includes_seen = nullptr;
    }
}

void
text_sink::dump (FILE *outfile, int indent) const
{
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_follows_reference_printer);
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_show_nesting);
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_show_locations_in_nesting);
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_show_nesting_levels);

  sink::dump (outfile, indent);
  dumping::emit_heading (outfile, indent, "saved_output_buffer");
  if (m_saved_output_buffer)
    m_saved_output_buffer->dump (outfile, indent + 2);
  else
    dumping::emit_none (outfile, indent + 2);
}

void
text_sink::set_buffer (per_sink_buffer *base)
{
  text_sink_buffer * const buffer
    = static_cast<text_sink_buffer *> (base);

  pretty_printer *const pp = get_printer ();

  if (!m_saved_output_buffer)
    m_saved_output_buffer = pp_buffer (pp);

  if (buffer)
    pp_buffer (pp) = &buffer->m_output_buffer;
  else
    {
      gcc_assert (m_saved_output_buffer);
      pp_buffer (pp) = m_saved_output_buffer;
    }
}

std::unique_ptr<per_sink_buffer>
text_sink::make_per_sink_buffer ()
{
  return std::make_unique<text_sink_buffer> (*this);
}

/* Implementation of diagnostics::sink::on_report_diagnostic vfunc
   for GCC's standard textual output.  */

void
text_sink::on_report_diagnostic (const diagnostic_info &diagnostic,
				 enum kind orig_diag_kind)
{
  auto logger = get_logger ();
  DIAGNOSTICS_LOG_SCOPE_PRINTF0
    (logger,
     "diagnostics::text_sink::on_report_diagnostic");

  pretty_printer *pp = get_printer ();

  (*text_starter (&m_context)) (*this, &diagnostic);

  pp_output_formatted_text (pp, m_context.get_urlifier ());

  if (m_context.m_show_cwe)
    print_any_cwe (diagnostic);

  if (m_context.m_show_rules)
    print_any_rules (diagnostic);

  if (m_context.m_show_option_requested)
    print_option_information (diagnostic, orig_diag_kind);

  /* If we're showing nested diagnostics, then print the location
     on a new line, indented.  */
  if (m_show_nesting && m_show_locations_in_nesting)
    {
      const int nesting_level = get_context ().get_diagnostic_nesting_level ();
      if (nesting_level > 0)
	{
	  location_t loc = diagnostic_location (&diagnostic);
	  pp_set_prefix (pp, nullptr);
	  char *indent_prefix = build_indent_prefix (false);
	  /* Only print changes of location.  */
	  if (loc != get_context ().m_last_location
	      && loc > BUILTINS_LOCATION)
	    {
	      const expanded_location s
		= diagnostic_expand_location (&diagnostic);
	      label_text location_text = get_location_text (s);
	      pp_newline (pp);
	      pp_printf (pp, "%s%s", indent_prefix, location_text.get ());
	    }
	  pp_set_prefix (pp, indent_prefix);
	}
    }

  (*text_finalizer (&m_context)) (*this,
				  &diagnostic,
				  orig_diag_kind);

  if (m_show_nesting && m_show_locations_in_nesting)
    get_context ().m_last_location = diagnostic_location (&diagnostic);
}

void
text_sink::on_report_verbatim (text_info &text)
{
  pp_format_verbatim (get_printer (), &text);
  pp_newline_and_flush (get_printer ());
}

void
text_sink::on_diagram (const diagnostics::diagram &d)
{
  pretty_printer *const pp = get_printer ();

  char *saved_prefix = pp_take_prefix (pp);
  pp_set_prefix (pp, nullptr);
  /* Use a newline before and after and a two-space indent
     to make the diagram stand out a little from the wall of text.  */
  pp_newline (pp);
  d.get_canvas ().print_to_pp (pp, "  ");
  pp_newline (pp);
  pp_set_prefix (pp, saved_prefix);
  pp_flush (pp);
}

void
text_sink::
after_diagnostic (const diagnostic_info &diagnostic)
{
  if (const paths::path *path = diagnostic.m_richloc->get_path ())
    print_path (*path);
}

/* Return a malloc'd string describing a location and the severity of the
   diagnostic, e.g. "foo.c:42:10: error: ".

   If m_show_nesting, then the above will be preceded by indentation to show
   the level, and a bullet point.

   The caller is responsible for freeing the memory.  */
char *
text_sink::build_prefix (const diagnostic_info &diagnostic) const
{
  gcc_assert (diagnostic.m_kind < kind::last_diagnostic_kind);

  const char *text = _(get_text_for_kind (diagnostic.m_kind));
  const char *text_cs = "", *text_ce = "";
  pretty_printer *pp = get_printer ();

  if (const char *color_name = get_color_for_kind (diagnostic.m_kind))
    {
      text_cs = colorize_start (pp_show_color (pp), color_name);
      text_ce = colorize_stop (pp_show_color (pp));
    }

  const int nesting_level = get_context ().get_diagnostic_nesting_level ();
  if (m_show_nesting && nesting_level > 0)
    {
      char *indent_prefix = build_indent_prefix (true);

      /* Reduce verbosity of nested diagnostics by not printing "note: "
	 all the time.  */
      if (diagnostic.m_kind == kind::note)
	return indent_prefix;

      char *result = build_message_string ("%s%s%s%s", indent_prefix,
					   text_cs, text, text_ce);
      free (indent_prefix);
      return result;
    }
  else
    {
      const expanded_location s = diagnostic_expand_location (&diagnostic);
      label_text location_text = get_location_text (s);
      return build_message_string ("%s %s%s%s", location_text.get (),
				   text_cs, text, text_ce);
    }
}

/* Same as build_prefix, but only the source FILE is given.  */
char *
text_sink::file_name_as_prefix (const char *f) const
{
  pretty_printer *const pp = get_printer ();
  const char *locus_cs
    = colorize_start (pp_show_color (pp), "locus");
  const char *locus_ce = colorize_stop (pp_show_color (pp));
  return build_message_string ("%s%s:%s ", locus_cs, f, locus_ce);
}

/* Get the unicode code point for bullet points when showing
   nested diagnostics.  */

static unsigned
get_bullet_point_unichar (bool unicode)
{
  if (unicode)
    return 0x2022; /* U+2022: Bullet */
  else
    return '*';
}

/* Return true if DC's theme supports unicode characters.  */

static bool
use_unicode_p (const context &dc)
{
  if (text_art::theme *theme = dc.get_diagram_theme ())
    return theme->unicode_p ();
  else
    return false;
}

/* Get the unicode code point for bullet points when showing
   nested diagnostics.  */

static unsigned
get_bullet_point_unichar (context &dc)
{
  return get_bullet_point_unichar (use_unicode_p (dc));
}

/* Return a malloc'd string for use as a prefix to show indentation.
   If m_show_nesting is false, or we're at the top-level, then the
   result will be the empty string.

   If m_show_nesting, then the result will contain indentation to show
   the nesting level, then either a bullet point (if WITH_BULLET is true),
   or a space.

   The caller is responsible for freeing the memory.  */

char *
text_sink::build_indent_prefix (bool with_bullet) const
{
  if (!m_show_nesting)
    return xstrdup ("");

  const int nesting_level = get_context ().get_diagnostic_nesting_level ();
  if (nesting_level == 0)
    return xstrdup ("");

  pretty_printer pp;
  for (int i = 0; i < nesting_level; i++)
    pp_string (&pp, "  ");
  if (with_bullet)
    pp_unicode_character (&pp, get_bullet_point_unichar (get_context ()));
  else
    pp_space (&pp);
  pp_space (&pp);
  if (m_show_nesting_levels)
    pp_printf (&pp, "(level %i):", nesting_level);
  return xstrdup (pp_formatted_text (&pp));
}

/* Add a purely textual note with text GMSGID and with LOCATION.  */

void
text_sink::append_note (location_t location,
			const char * gmsgid, ...)
{
  context *dc = &get_context ();

  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc, kind::note);
  if (dc->m_inhibit_notes_p)
    {
      va_end (ap);
      return;
    }
  pretty_printer *pp = get_printer ();
  char *saved_prefix = pp_take_prefix (pp);
  pp_set_prefix (pp, build_prefix (diagnostic));
  pp_format (pp, &diagnostic.m_message);
  pp_output_formatted_text (pp);
  pp_destroy_prefix (pp);
  pp_set_prefix (pp, saved_prefix);
  pp_newline (pp);
  diagnostic_show_locus (dc, get_source_printing_options (),
			 &richloc, kind::note, pp);
  va_end (ap);
}

bool
text_sink::follows_reference_printer_p () const
{
  return m_follows_reference_printer;
}

void
text_sink::update_printer ()
{
  pretty_printer *copy_from_pp
    = (m_follows_reference_printer
       ? get_context ().get_reference_printer ()
       : m_printer.get ());
  const bool show_color = pp_show_color (copy_from_pp);
  const diagnostic_url_format url_format = copy_from_pp->get_url_format ();

  m_printer = get_context ().clone_printer ();

  pp_show_color (m_printer.get ()) = show_color;
  m_printer->set_url_format (url_format);
  // ...etc

  m_source_printing = get_context ().m_source_printing;
}

/* If DIAGNOSTIC has a CWE identifier, print it.

   For example, if the diagnostic metadata associates it with CWE-119,
   " [CWE-119]" will be printed, suitably colorized, and with a URL of a
   description of the security issue.  */

void
text_sink::print_any_cwe (const diagnostic_info &diagnostic)
{
  if (!diagnostic.m_metadata)
    return;

  int cwe = diagnostic.m_metadata->get_cwe ();
  if (cwe)
    {
      pretty_printer * const pp = get_printer ();
      char *saved_prefix = pp_take_prefix (pp);
      pp_string (pp, " [");
      const char *kind_color = get_color_for_kind (diagnostic.m_kind);
      pp_string (pp, colorize_start (pp_show_color (pp), kind_color));
      if (pp->supports_urls_p ())
	{
	  char *cwe_url = get_cwe_url (cwe);
	  pp_begin_url (pp, cwe_url);
	  free (cwe_url);
	}
      pp_printf (pp, "CWE-%i", cwe);
      pp_set_prefix (pp, saved_prefix);
      if (pp->supports_urls_p ())
	pp_end_url (pp);
      pp_string (pp, colorize_stop (pp_show_color (pp)));
      pp_character (pp, ']');
    }
}

/* If DIAGNOSTIC has any rules associated with it, print them.

   For example, if the diagnostic metadata associates it with a rule
   named "STR34-C", then " [STR34-C]" will be printed, suitably colorized,
   with any URL provided by the rule.  */

void
text_sink::print_any_rules (const diagnostic_info &diagnostic)
{
  if (!diagnostic.m_metadata)
    return;

  for (unsigned idx = 0; idx < diagnostic.m_metadata->get_num_rules (); idx++)
    {
      const diagnostics::metadata::rule &rule
	= diagnostic.m_metadata->get_rule (idx);
      if (char *desc = rule.make_description ())
	{
	  pretty_printer * const pp = get_printer ();
	  char *saved_prefix = pp_take_prefix (pp);
	  pp_string (pp, " [");
	  const char *kind_color = get_color_for_kind (diagnostic.m_kind);
	  pp_string (pp, colorize_start (pp_show_color (pp), kind_color));
	  char *url = nullptr;
	  if (pp->supports_urls_p ())
	    {
	      url = rule.make_url ();
	      if (url)
		pp_begin_url (pp, url);
	    }
	  pp_string (pp, desc);
	  pp_set_prefix (pp, saved_prefix);
	  if (pp->supports_urls_p ())
	    if (url)
	      pp_end_url (pp);
	  free (url);
	  pp_string (pp, colorize_stop (pp_show_color (pp)));
	  pp_character (pp, ']');
	  free (desc);
	}
    }
}

/* Print any metadata about the option used to control DIAGNOSTIC to
   the context's printer, e.g. " [-Werror=uninitialized]".  */

void
text_sink::print_option_information (const diagnostic_info &diagnostic,
				     enum kind orig_diag_kind)
{
  if (char *option_text
      = m_context.make_option_name (diagnostic.m_option_id,
				    orig_diag_kind, diagnostic.m_kind))
    {
      char *option_url = nullptr;
      pretty_printer * const pp = get_printer ();
      if (pp->supports_urls_p ())
	option_url = m_context.make_option_url (diagnostic.m_option_id);
      pp_string (pp, " [");
      const char *kind_color = get_color_for_kind (diagnostic.m_kind);
      pp_string (pp, colorize_start (pp_show_color (pp), kind_color));
      if (option_url)
	pp_begin_url (pp, option_url);
      pp_string (pp, option_text);
      if (option_url)
	{
	  pp_end_url (pp);
	  free (option_url);
	}
      pp_string (pp, colorize_stop (pp_show_color (pp)));
      pp_character (pp, ']');
      free (option_text);
    }
}

/* Only dump the "In file included from..." stack once for each file.  */

bool
text_sink::includes_seen_p (const line_map_ordinary *map)
{
  /* No include path for main.  */
  if (MAIN_FILE_P (map))
    return true;

  /* Always identify C++ modules, at least for now.  */
  auto probe = map;
  if (linemap_check_ordinary (map)->reason == LC_RENAME)
    /* The module source file shows up as LC_RENAME inside LC_MODULE.  */
    probe = linemap_included_from_linemap (line_table, map);
  if (MAP_MODULE_P (probe))
    return false;

  if (!m_includes_seen)
    m_includes_seen = new hash_set<location_t, false, location_hash>;

  /* Hash the location of the #include directive to better handle files
     that are included multiple times with different macros defined.  */
  return m_includes_seen->add (linemap_included_from (map));
}

label_text
text_sink::get_location_text (const expanded_location &s) const
{
  column_policy column_policy_ (get_context ());
  return column_policy_.get_location_text (s,
					   show_column_p (),
					   pp_show_color (get_printer ()));
}

/* Helpers for writing lang-specific starters/finalizers for text output.  */

/* Return a formatted line and column ':%line:%column'.  Elided if
   line == 0 or col < 0.  (A column of 0 may be valid due to the
   -fdiagnostics-column-origin option.)
   The result is a statically allocated buffer.  */

const char *
text_sink::maybe_line_and_column (int line, int col)
{
  static char result[32];

  if (line)
    {
      size_t l
	= snprintf (result, sizeof (result),
		    col >= 0 ? ":%d:%d" : ":%d", line, col);
      gcc_checking_assert (l < sizeof (result));
    }
  else
    result[0] = 0;
  return result;
}

void
text_sink::report_current_module (location_t where)
{
  pretty_printer *pp = get_printer ();
  const line_map_ordinary *map = nullptr;

  if (pp_needs_newline (pp))
    {
      pp_newline (pp);
      pp_needs_newline (pp) = false;
    }

  if (where <= BUILTINS_LOCATION)
    return;

  linemap_resolve_location (line_table, where,
			    LRK_MACRO_DEFINITION_LOCATION,
			    &map);

  if (map && m_last_module != map)
    {
      m_last_module = map;
      if (!includes_seen_p (map))
	{
	  bool first = true, need_inc = true, was_module = MAP_MODULE_P (map);
	  expanded_location s = {};
	  do
	    {
	      where = linemap_included_from (map);
	      map = linemap_included_from_linemap (line_table, map);
	      bool is_module = MAP_MODULE_P (map);
	      s.file = LINEMAP_FILE (map);
	      s.line = SOURCE_LINE (map, where);
	      int col = -1;
	      if (first && show_column_p ())
		{
		  s.column = SOURCE_COLUMN (map, where);
		  col = get_column_policy ().converted_column (s);
		}
	      const char *line_col = maybe_line_and_column (s.line, col);
	      static const char *const msgs[] =
		{
		 nullptr,
		 N_("                 from"),
		 N_("In file included from"),	/* 2 */
		 N_("        included from"),
		 N_("In module"),		/* 4 */
		 N_("of module"),
		 N_("In module imported at"),	/* 6 */
		 N_("imported at"),
		};

	      unsigned index = (was_module ? 6 : is_module ? 4
				: need_inc ? 2 : 0) + !first;

	      pp_verbatim (pp, "%s%s %r%s%s%R",
			   first ? "" : was_module ? ", " : ",\n",
			   _(msgs[index]),
			   "locus", s.file, line_col);
	      first = false, need_inc = was_module, was_module = is_module;
	    }
	  while (!includes_seen_p (map));
	  pp_verbatim (pp, ":");
	  pp_newline (pp);
	}
    }
}

void
default_text_starter (text_sink &text_output,
		      const diagnostic_info *diagnostic)
{
  text_output.report_current_module (diagnostic_location (diagnostic));
  pretty_printer *const pp = text_output.get_printer ();
  pp_set_prefix (pp, text_output.build_prefix (*diagnostic));
}

void
default_text_finalizer (text_sink &text_output,
			const diagnostic_info *diagnostic,
			enum kind)
{
  pretty_printer *const pp = text_output.get_printer ();
  char *saved_prefix = pp_take_prefix (pp);
  pp_set_prefix (pp, nullptr);
  pp_newline (pp);
  diagnostic_show_locus (&text_output.get_context (),
			 text_output.get_source_printing_options (),
			 diagnostic->m_richloc, diagnostic->m_kind, pp);
  pp_set_prefix (pp, saved_prefix);
  pp_flush (pp);
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

} // namespace diagnostics
