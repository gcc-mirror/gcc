/* Selftest support for diagnostics.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "selftest.h"
#include "diagnostics/diagnostics-selftests.h"

#if CHECKING_P

namespace diagnostics {

namespace selftest {

/* Run all diagnostics-specific selftests,
   apart from context_cc_tests, which according to
   https://gcc.gnu.org/pipermail/gcc/2021-November/237703.html
   has some language-specific assumptions, and thus is run from
   c_family_tests instead.  */

void
run_diagnostics_selftests ()
{
  color_cc_tests ();
  file_cache_cc_tests ();
  source_printing_cc_tests ();
  html_sink_cc_tests ();
  sarif_sink_cc_tests ();
  digraphs_cc_tests ();
  output_spec_cc_tests ();
  lazy_paths_cc_tests ();
  paths_output_cc_tests ();
  changes_cc_tests ();
}

} /* end of namespace diagnostics::selftest.  */

} // namespace diagnostics

#endif /* #if CHECKING_P */
