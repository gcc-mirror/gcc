/* Implementation of selftests.
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

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
  long start_time = get_run_time ();

  /* Run all the tests, in hand-coded order of (approximate) dependencies:
     run the tests for lowest-level code first.  */

  /* Low-level data structures.  */
  bitmap_c_tests ();
  et_forest_c_tests ();
  hash_map_tests_c_tests ();
  hash_set_tests_c_tests ();
  vec_c_tests ();
  pretty_print_c_tests ();
  wide_int_cc_tests ();
  ggc_tests_c_tests ();

  /* Mid-level data structures.  */
  input_c_tests ();
  tree_c_tests ();
  gimple_c_tests ();
  rtl_tests_c_tests ();

  /* Higher-level tests, or for components that other selftests don't
     rely on.  */
  diagnostic_show_locus_c_tests ();
  diagnostic_c_tests ();
  fold_const_c_tests ();
  spellcheck_c_tests ();
  spellcheck_tree_c_tests ();
  tree_cfg_c_tests ();

  /* This one relies on most of the above.  */
  function_tests_c_tests ();

  /* Finished running tests.  */
  long finish_time = get_run_time ();
  long elapsed_time = finish_time - start_time;

  fprintf (stderr,
	   "-fself-test: %i pass(es) in %ld.%06ld seconds\n",
	   num_passes,
	   elapsed_time / 1000000, elapsed_time % 1000000);
}

#endif /* #if CHECKING_P */
