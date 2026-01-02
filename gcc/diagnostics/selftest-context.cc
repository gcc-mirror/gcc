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

#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostics/sink.h"
#include "selftest.h"
#include "diagnostics/selftest-context.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace diagnostics {
namespace selftest {

/* Implementation of class diagnostics::selftest::test_context.  */

test_context::test_context ()
{
  diagnostic_initialize (this, 0);
  pp_show_color (get_reference_printer ()) = false;

  auto &source_printing_opts = get_source_printing_options ();
  source_printing_opts.enabled = true;
  source_printing_opts.show_labels_p = true;
  m_show_column = true;
  start_span (this) = start_span_cb;
  source_printing_opts.min_margin_width = 6;
  source_printing_opts.max_width = 80;
  pp_buffer (get_sink (0).get_printer ())->m_flush_p = false;
}

test_context::~test_context ()
{
  diagnostic_finish (this);
}

/* Implementation of diagnostics::start_span_fn, hiding the
   real filename (to avoid printing the names of tempfiles).  */

void
test_context::
start_span_cb (const location_print_policy &loc_policy,
	       to_text &html_or_text,
	       expanded_location exploc)
{
  exploc.file = "FILENAME";
  default_start_span_fn<to_text>
    (loc_policy, html_or_text, exploc);
}

bool
test_context::report (enum kind kind,
		      rich_location &richloc,
		      const metadata *metadata_,
		      option_id opt_id,
		      const char * fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  begin_group ();
  bool result = diagnostic_impl (&richloc, metadata_, opt_id, fmt, &ap, kind);
  end_group ();
  va_end (ap);
  return result;
}

/* Print RICHLOC's source and annotations to this context's m_printer.
   Return the text buffer from the printer.  */

const char *
test_context::test_show_locus (rich_location &richloc)
{
  pretty_printer *pp = get_reference_printer ();
  gcc_assert (pp);
  source_print_policy source_policy (*this);
  source_policy.print (*pp, richloc, kind::error, nullptr);
  return pp_formatted_text (pp);
}

} // namespace selftest
} // namespace diagnostics

#endif /* #if CHECKING_P */
