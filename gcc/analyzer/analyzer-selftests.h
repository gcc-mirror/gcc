/* Selftests for the analyzer.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_SELFTESTS_H
#define GCC_ANALYZER_SELFTESTS_H

#if CHECKING_P

namespace ana {

namespace selftest {

extern tree build_global_decl (const char *name, tree type);

extern void run_analyzer_selftests ();

/* Declarations for specific families of tests (by source file), in
   alphabetical order.  */
extern void analyzer_checker_script_cc_tests ();
extern void analyzer_constraint_manager_cc_tests ();
extern void analyzer_function_set_cc_tests ();
extern void analyzer_program_point_cc_tests ();
extern void analyzer_program_state_cc_tests ();
extern void analyzer_ranges_cc_tests ();
extern void analyzer_region_model_cc_tests ();
extern void analyzer_sm_file_cc_tests ();
extern void analyzer_sm_signal_cc_tests ();
extern void analyzer_store_cc_tests ();

} /* end of namespace ana::selftest.  */

} // namespace ana

#endif /* #if CHECKING_P */

#endif /* GCC_ANALYZER_SELFTESTS_H */
