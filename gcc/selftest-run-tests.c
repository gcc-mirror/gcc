/* Implementation of selftests.
   Copyright (C) 2015-2020 Free Software Foundation, Inc.

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
#include "selftest.h"
#include "tree.h"
#include "target.h"
#include "langhooks.h"
#include "options.h"
#include "stringpool.h"
#include "attribs.h"
#include "analyzer/analyzer-selftests.h"

/* This function needed to be split out from selftest.c as it references
   tests from the whole source tree, and so is within
   OBJS in Makefile.in, whereas selftest.o is within OBJS-libcommon.
   This allows us to embed tests within files in OBJS-libcommon without
   introducing a dependency on objects within OBJS.  */

#if CHECKING_P

/* Run all tests, aborting if any fail.  */

void
selftest::run_tests ()
{
  /* Makefile.in has -fself-test=$(srcdir)/testsuite/selftests, so that
     flag_self_test contains the path to the selftest subdirectory of the
     source tree (without a trailing slash).  Copy it up to
     path_to_selftest_files, to avoid selftest.c depending on
     option-handling.  */
  path_to_selftest_files = flag_self_test;

  test_runner r ("-fself-test");

  /* Run all the tests, in hand-coded order of (approximate) dependencies:
     run the tests for lowest-level code first.  */

  /* Sanity-check for selftests themselves.  */
  selftest_c_tests ();

  /* Low-level data structures.  */
  bitmap_c_tests ();
  sbitmap_c_tests ();
  dumpfile_c_tests ();
  et_forest_c_tests ();
  hash_map_tests_c_tests ();
  hash_set_tests_c_tests ();
  vec_c_tests ();
  pretty_print_c_tests ();
  wide_int_cc_tests ();
  ggc_tests_c_tests ();
  sreal_c_tests ();
  fibonacci_heap_c_tests ();
  typed_splay_tree_c_tests ();
  unique_ptr_tests_cc_tests ();
  opt_proposer_c_tests ();
  json_cc_tests ();
  cgraph_c_tests ();
  optinfo_emit_json_cc_tests ();
  opt_problem_cc_tests ();
  ordered_hash_map_tests_cc_tests ();

  /* Mid-level data structures.  */
  input_c_tests ();
  vec_perm_indices_c_tests ();
  tree_c_tests ();
  convert_c_tests ();
  gimple_c_tests ();
  rtl_tests_c_tests ();
  read_rtl_function_c_tests ();
  digraph_cc_tests ();
  tristate_cc_tests ();

  /* Higher-level tests, or for components that other selftests don't
     rely on.  */
  diagnostic_show_locus_c_tests ();
  diagnostic_c_tests ();
  diagnostic_format_json_cc_tests ();
  edit_context_c_tests ();
  fold_const_c_tests ();
  spellcheck_c_tests ();
  spellcheck_tree_c_tests ();
  tree_cfg_c_tests ();
  tree_diagnostic_path_cc_tests ();
  attribute_c_tests ();

  /* This one relies on most of the above.  */
  function_tests_c_tests ();

  /* Run any target-specific selftests.  */
  if (targetm.run_target_selftests)
    targetm.run_target_selftests ();

  store_merging_c_tests ();
  predict_c_tests ();
  simplify_rtx_c_tests ();
  dbgcnt_c_tests ();

  /* Run any lang-specific selftests.  */
  lang_hooks.run_lang_selftests ();

  /* Run the analyzer selftests (if enabled).  */
  ana::selftest::run_analyzer_selftests ();

  /* Force a GC at the end of the selftests, to shake out GC-related
     issues.  For example, if any GC-managed items have buggy (or missing)
     finalizers, this last collection will ensure that things that were
     failed to be finalized can be detected by valgrind.  */
  forcibly_ggc_collect ();

  /* Finished running tests; the test_runner dtor will print a summary.  */
}

#endif /* #if CHECKING_P */
