/* Selftest support for diagnostics.
   Copyright (C) 2016-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_SELFTEST_CONTEXT_H
#define GCC_DIAGNOSTICS_SELFTEST_CONTEXT_H

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace diagnostics {
namespace selftest {

/* Convenience subclass of diagnostics::context for testing
   the diagnostic subsystem.  */

class test_context : public context
{
 public:
  test_context ();
  ~test_context ();

  /* Implementation of diagnostics::start_span_fn, hiding the
     real filename (to avoid printing the names of tempfiles).  */
  static void
  start_span_cb (const location_print_policy &,
		 to_text &sink,
		 expanded_location exploc);

  /* Report a diagnostic to this context.  For a selftest, this
     should only be called on a context that uses a non-standard formatter
     that e.g. gathers the results in memory, rather than emits to stderr.  */
  bool
  report (enum kind kind,
	  rich_location &richloc,
	  const metadata *,
	  option_id opt_id,
	  const char * fmt, ...) ATTRIBUTE_GCC_DIAG(6,7);

  const char *test_show_locus (rich_location &richloc);

  /* Setters for the context's source_printing_options
     for use in selftests.  */
  void colorize_source (bool val)
  {
    get_source_printing_options ().colorize_source_p = val;
  }
  void show_labels (bool val)
  {
    get_source_printing_options ().show_labels_p = val;
  }
  void show_line_numbers (bool val)
  {
    get_source_printing_options ().show_line_numbers_p = val;
  }
  void show_ruler (bool val)
  {
    get_source_printing_options ().show_ruler_p = val;
  }
  void show_event_links (bool val)
  {
    get_source_printing_options ().show_event_links_p = val;
  }
  void set_caret_char (unsigned idx, char ch)
  {
    gcc_assert (idx < rich_location::STATICALLY_ALLOCATED_RANGES);
    get_source_printing_options ().caret_chars[idx] = ch;
  }
};

} // namespace selftest
} // namespace diagnostics

#endif /* #if CHECKING_P */

#endif /* GCC_DIAGNOSTICS_SELFTEST_CONTEXT_H */
