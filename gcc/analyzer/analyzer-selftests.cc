/* Selftest support for the analyzer.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
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
#include "tree.h"
#include "stringpool.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-selftests.h"

#if CHECKING_P

namespace ana {

namespace selftest {

/* Build a VAR_DECL named NAME of type TYPE, simulating a file-level
   static variable.  */

tree
build_global_decl (const char *name, tree type)
{
  tree decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			  get_identifier (name), type);
  TREE_STATIC (decl) = 1;
  return decl;
}

/* Run all analyzer-specific selftests.  */

void
run_analyzer_selftests ()
{
#if ENABLE_ANALYZER
  analyzer_access_diagram_cc_tests ();
  analyzer_constraint_manager_cc_tests ();
  analyzer_function_set_cc_tests ();
  analyzer_program_point_cc_tests ();
  analyzer_program_state_cc_tests ();
  analyzer_ranges_cc_tests ();
  analyzer_region_model_cc_tests ();
  analyzer_sm_file_cc_tests ();
  analyzer_sm_signal_cc_tests ();
  analyzer_store_cc_tests ();
#endif /* #if ENABLE_ANALYZER */
}

} /* end of namespace selftest.  */

} // namespace ana

#endif /* #if CHECKING_P */
