/* Selftest support for diagnostics.
   Copyright (C) 2016-2025 Free Software Foundation, Inc.

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
#include "diagnostic-format.h"
#include "selftest.h"
#include "selftest-diagnostic.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace selftest {

/* Implementation of class selftest::test_diagnostic_context.  */

test_diagnostic_context::test_diagnostic_context ()
{
  diagnostic_initialize (this, 0);
  pp_show_color (get_reference_printer ()) = false;
  m_source_printing.enabled = true;
  m_source_printing.show_labels_p = true;
  m_show_column = true;
  diagnostic_start_span (this) = start_span_cb;
  m_source_printing.min_margin_width = 6;
  m_source_printing.max_width = 80;
  pp_buffer (get_output_format (0).get_printer ())->m_flush_p = false;
}

test_diagnostic_context::~test_diagnostic_context ()
{
  diagnostic_finish (this);
}

/* Implementation of diagnostic_start_span_fn, hiding the
   real filename (to avoid printing the names of tempfiles).  */

void
test_diagnostic_context::
start_span_cb (const diagnostic_location_print_policy &loc_policy,
	       pretty_printer *pp,
	       expanded_location exploc)
{
  exploc.file = "FILENAME";
  default_diagnostic_start_span_fn (loc_policy, pp, exploc);
}

bool
test_diagnostic_context::report (diagnostic_t kind,
				 rich_location &richloc,
				 const diagnostic_metadata *metadata,
				 int option,
				 const char * fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  begin_group ();
  bool result = diagnostic_impl (&richloc, metadata, option, fmt, &ap, kind);
  end_group ();
  va_end (ap);
  return result;
}

/* Print RICHLOC's source and annotations to this context's m_printer.
   Return the text buffer from the printer.  */

const char *
test_diagnostic_context::test_show_locus (rich_location &richloc)
{
  pretty_printer *pp = get_reference_printer ();
  gcc_assert (pp);
  diagnostic_source_print_policy source_policy (*this);
  source_policy.print (*pp, richloc, DK_ERROR, nullptr);
  return pp_formatted_text (pp);
}

} // namespace selftest

#endif /* #if CHECKING_P */
