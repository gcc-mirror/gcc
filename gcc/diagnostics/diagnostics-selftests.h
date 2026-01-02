/* Selftests for diagnostics.
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

#ifndef GCC_DIAGNOSTICS_SELFTESTS_H
#define GCC_DIAGNOSTICS_SELFTESTS_H

#if CHECKING_P

namespace diagnostics {

namespace selftest {

extern void run_diagnostics_selftests ();

/* Declarations for specific families of tests (by source file within
   "diagnostics/"), in alphabetical order.  */

extern void changes_cc_tests ();
extern void color_cc_tests ();
extern void context_cc_tests ();
extern void digraphs_cc_tests ();
extern void file_cache_cc_tests ();
extern void html_sink_cc_tests ();
extern void lazy_paths_cc_tests ();
extern void output_spec_cc_tests ();
extern void paths_output_cc_tests ();
extern void sarif_sink_cc_tests ();
extern void selftest_logical_locations_cc_tests ();
extern void source_printing_cc_tests ();

} /* end of namespace diagnostics::selftest.  */

} // namespace diagnostics

#endif /* #if CHECKING_P */

#endif /* GCC_DIAGNOSTICS_SELFTESTS_H */
