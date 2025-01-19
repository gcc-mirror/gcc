/* Classic text-based output of diagnostics.
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

#ifndef GCC_DIAGNOSTIC_FORMAT_TEXT_H
#define GCC_DIAGNOSTIC_FORMAT_TEXT_H

#include "diagnostic-format.h"

/* Subclass of diagnostic_output_format for classic text-based output
   to stderr.

   Uses diagnostic_context.m_text_callbacks to provide client-specific
   textual output (e.g. include paths, macro expansions, etc).  */

class diagnostic_text_output_format : public diagnostic_output_format
{
public:
  diagnostic_text_output_format (diagnostic_context &context,
				 diagnostic_source_printing_options *source_printing = nullptr,
				 bool follows_reference_printer = false)
  : diagnostic_output_format (context),
    m_saved_output_buffer (nullptr),
    m_column_policy (context),
    m_last_module (nullptr),
    m_includes_seen (nullptr),
    m_source_printing (source_printing
		       ? *source_printing
		       : context.m_source_printing),
    m_follows_reference_printer (follows_reference_printer),
    m_show_nesting (false),
    m_show_nesting_levels (false)
  {}
  ~diagnostic_text_output_format ();

  void dump (FILE *out, int indent) const override;

  std::unique_ptr<diagnostic_per_format_buffer>
  make_per_format_buffer () final override;
  void set_buffer (diagnostic_per_format_buffer *) final override;

  void on_begin_group () override {}
  void on_end_group () override {}
  void on_report_diagnostic (const diagnostic_info &,
			     diagnostic_t orig_diag_kind) override;
  void on_report_verbatim (text_info &) final override;
  void on_diagram (const diagnostic_diagram &diagram) override;
  void after_diagnostic (const diagnostic_info &) override;
  bool machine_readable_stderr_p () const final override
  {
    return false;
  }
  bool follows_reference_printer_p () const final override;

  void update_printer () override;

  /* Helpers for writing lang-specific starters/finalizers for text output.  */
  char *build_prefix (const diagnostic_info &) const;
  void report_current_module (location_t where);
  void append_note (location_t location,
		    const char * gmsgid, ...) ATTRIBUTE_GCC_DIAG(3,4);


  char *file_name_as_prefix (const char *) const;

  char *build_indent_prefix (bool with_bullet) const;

  void print_path (const diagnostic_path &path);

  bool show_column_p () const { return get_context ().m_show_column; }

  const diagnostic_column_policy &get_column_policy () const
  {
    return m_column_policy;
  }
  diagnostic_location_print_policy get_location_print_policy () const;

  bool show_nesting_p () const { return m_show_nesting; }
  bool show_locations_in_nesting_p () const
  {
    return m_show_locations_in_nesting;
  }

  void set_show_nesting (bool show_nesting) { m_show_nesting = show_nesting; }
  void set_show_locations_in_nesting (bool val)
  {
    m_show_locations_in_nesting = val;
  }
  void set_show_nesting_levels (bool show_nesting_levels)
  {
    m_show_nesting_levels = show_nesting_levels;
  }

  label_text get_location_text (const expanded_location &s) const;

  diagnostic_source_printing_options &get_source_printing_options ()
  {
    return m_source_printing;
  }
  const diagnostic_source_printing_options &get_source_printing_options () const
  {
    return m_source_printing;
  }

protected:
  void print_any_cwe (const diagnostic_info &diagnostic);
  void print_any_rules (const diagnostic_info &diagnostic);
  void print_option_information (const diagnostic_info &diagnostic,
				 diagnostic_t orig_diag_kind);

  bool includes_seen_p (const line_map_ordinary *map);

  /* For handling diagnostic_buffer.  */
  output_buffer *m_saved_output_buffer;

  diagnostic_column_policy m_column_policy;

  /* Used to detect when the input file stack has changed since last
     described.  */
  const line_map_ordinary *m_last_module;

  /* Include files that report_current_module has already listed the
     include path for.  */
  hash_set<location_t, false, location_hash> *m_includes_seen;

  diagnostic_source_printing_options &m_source_printing;

  /* If true, this is the initial default text output format created
     when the diagnostic_context was created, and, in particular, before
     initializations of color and m_url_format.  Hence this should follow
     the dc's reference printer for these.
     If false, this text output was created after the dc was created, and
     thus tracks its own values for color and m_url_format.  */
  bool m_follows_reference_printer;

  /* If true, then use indentation to show the nesting structure of
     nested diagnostics, and print locations on separate lines after the
     diagnostic message, rather than as a prefix to the message.  */
  bool m_show_nesting;

  /* Set to false to suppress location-printing when showing nested
     diagnostics, for use in DejaGnu tests.  */
  bool m_show_locations_in_nesting;

  /* If true, then add "(level N):" when printing nested diagnostics.  */
  bool m_show_nesting_levels;
};

#endif /* ! GCC_DIAGNOSTIC_FORMAT_TEXT_H */
