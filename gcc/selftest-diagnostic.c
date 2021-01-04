/* Selftest support for diagnostics.
   Copyright (C) 2016-2021 Free Software Foundation, Inc.

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
  show_caret = true;
  show_labels_p = true;
  show_column = true;
  start_span = start_span_cb;
  min_margin_width = 6;
}

test_diagnostic_context::~test_diagnostic_context ()
{
  diagnostic_finish (this);
}

/* Implementation of diagnostic_start_span_fn, hiding the
   real filename (to avoid printing the names of tempfiles).  */

void
test_diagnostic_context::start_span_cb (diagnostic_context *context,
					expanded_location exploc)
{
  exploc.file = "FILENAME";
  default_diagnostic_start_span_fn (context, exploc);
}

} // namespace selftest

#endif /* #if CHECKING_P */
