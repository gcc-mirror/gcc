/* Rich optional information on why an optimization wasn't possible.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "pretty-print.h"
#include "opt-problem.h"
#include "dump-context.h"
#include "tree-pass.h"
#include "selftest.h"

/* opt_problem's ctor.

   Use FMT and AP to emit a message to the "immediate" dump destinations
   as if via:
     dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc, ...)

   The optinfo_item instances are not emitted yet.  Instead, they
   are retained internally so that the message can be replayed and
   emitted when this problem is handled, higher up the call stack.  */

opt_problem::opt_problem (const dump_location_t &loc,
			  const char *fmt, va_list *ap)
: m_optinfo (loc, OPTINFO_KIND_FAILURE, current_pass)
{
  /* We shouldn't be bothering to construct these objects if
     dumping isn't enabled.  */
  gcc_assert (dump_enabled_p ());

  /* Update the singleton.  */
  delete s_the_problem;
  s_the_problem = this;

  /* Print the location to the "immediate" dump destinations.  */
  dump_context &dc = dump_context::get ();
  dc.dump_loc (MSG_MISSED_OPTIMIZATION, loc.get_user_location ());

  /* Print the formatted string to this opt_problem's optinfo, dumping
     the items to the "immediate" dump destinations, and storing items
     for later retrieval.  */
  {
    dump_pretty_printer pp (&dump_context::get (), MSG_MISSED_OPTIMIZATION);

    text_info text (fmt, /* No i18n is performed.  */
		    ap, errno);

    /* Phases 1 and 2, using pp_format.  */
    pp_format (&pp, &text);

    /* Phase 3: dump the items to the "immediate" dump destinations,
       and storing them into m_optinfo for later retrieval.  */
    pp.set_optinfo (&m_optinfo);
    pp_output_formatted_text (&pp, nullptr);
  }
}

/* Emit this problem and delete it, clearing the current opt_problem.  */

void
opt_problem::emit_and_clear ()
{
  gcc_assert (this == s_the_problem);

  m_optinfo.emit_for_opt_problem ();

  delete this;
  s_the_problem = NULL;
}

/* The singleton opt_problem *.  */

opt_problem *opt_problem::s_the_problem;

#if CHECKING_P

namespace selftest {

static opt_result
function_that_succeeds ()
{
  return opt_result::success ();
}

/* Verify that opt_result::success works.  */

static void
test_opt_result_success ()
{
  /* Run all tests twice, with and then without dumping enabled.  */
  for (int i = 0 ; i < 2; i++)
    {
      bool with_dumping = (i == 0);

      temp_dump_context tmp (with_dumping, with_dumping,
			     MSG_ALL_KINDS | MSG_ALL_PRIORITIES);

      if (with_dumping)
	gcc_assert (dump_enabled_p ());
      else
	gcc_assert (!dump_enabled_p ());

      opt_result res = function_that_succeeds ();

      /* Verify that "success" can be used as a "true" boolean.  */
      ASSERT_TRUE (res);

      /* Verify the underlying opt_wrapper<bool>.  */
      ASSERT_TRUE (res.get_result ());
      ASSERT_EQ (res.get_problem (), NULL);

      /* Nothing should have been dumped.  */
      ASSERT_DUMPED_TEXT_EQ (tmp, "");
      optinfo *info = tmp.get_pending_optinfo ();
      ASSERT_EQ (info, NULL);
    }
}

/* Example of a function that fails, with a non-trivial
   pre-canned error message.  */

static opt_result
function_that_fails (const greturn *stmt)
{
  gcc_assert (stmt);
  gcc_assert (gimple_return_retval (stmt));

  AUTO_DUMP_SCOPE ("function_that_fails", stmt);

  return opt_result::failure_at (stmt,
				 "can't handle return type: %T for stmt: %G",
				 TREE_TYPE (gimple_return_retval (stmt)),
				 static_cast <const gimple *> (stmt));
}

/* Example of a function that indirectly fails.  */

static opt_result
function_that_indirectly_fails (const greturn *stmt)
{
  AUTO_DUMP_SCOPE ("function_that_indirectly_fails", stmt);

  opt_result res = function_that_fails (stmt);
  if (!res)
    return res;
  return opt_result::success ();
}

/* Verify that opt_result::failure_at works.
   Simulate a failure handling a stmt at one location whilst considering
   an optimization that's notionally at another location (as a microcosm
   of e.g. a problematic statement within a loop that prevents loop
   vectorization).  */

static void
test_opt_result_failure_at (const line_table_case &case_)
{
  /* Generate a location_t for testing.  */
  line_table_test ltt (case_);
  const line_map_ordinary *ord_map
    = linemap_check_ordinary (linemap_add (line_table, LC_ENTER, false,
					   "test.c", 0));
  linemap_line_start (line_table, 5, 100);

  /* A test location: "test.c:5:10".  */
  const location_t line_5 = linemap_position_for_column (line_table, 10);

  /* Another test location: "test.c:6:12".  */
  const location_t line_6
    = linemap_position_for_line_and_column (line_table, ord_map, 6, 12);

  if (line_6 > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Generate statements using "line_5" and "line_6" for testing.  */
  greturn *stmt_at_5 = gimple_build_return (integer_one_node);
  gimple_set_location (stmt_at_5, line_5);

  greturn *stmt_at_6 = gimple_build_return (integer_zero_node);
  gimple_set_location (stmt_at_6, line_6);

  /* Run with and then without dumping enabled.  */
  for (int i = 0; i < 2; i++)
    {
      bool with_dumping = (i == 0);

      /* Run with all 4 combinations of
	 with and without MSG_PRIORITY_INTERNALS and
	 with and without MSG_PRIORITY_REEMITTED.  */
      for (int j = 0; j < 4; j++)
	{
	  dump_flags_t filter = MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING;
	  if (j / 2)
	    filter |= MSG_PRIORITY_INTERNALS;
	  if (j % 2)
	    filter |= MSG_PRIORITY_REEMITTED;

	  temp_dump_context tmp (with_dumping, with_dumping, filter);

	  if (with_dumping)
	    gcc_assert (dump_enabled_p ());
	  else
	    gcc_assert (!dump_enabled_p ());

	  /* Simulate attempting to optimize "stmt_at_6".  */
	  opt_result res = function_that_indirectly_fails (stmt_at_6);

	  /* Verify that "failure" can be used as a "false" boolean.  */
	  ASSERT_FALSE (res);

	  /* Verify the underlying opt_wrapper<bool>.  */
	  ASSERT_FALSE (res.get_result ());
	  opt_problem *problem = res.get_problem ();

	  if (with_dumping)
	    {
	      ASSERT_NE (problem, NULL);
	      ASSERT_EQ (problem->get_dump_location ().get_location_t (),
			 line_6);
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8)
	      /* Verify that the problem captures the implementation location
		 it was emitted from.  */
	      const dump_impl_location_t &impl_location
		= problem->get_dump_location ().get_impl_location ();
	      ASSERT_STR_CONTAINS (impl_location.m_function,
				   "function_that_fails");
#endif

	      /* Verify that the underlying dump items are retained in the
		 opt_problem.  */
	      const optinfo &info = problem->get_optinfo ();
	      ASSERT_EQ (info.get_dump_location ().get_location_t (), line_6);
	      ASSERT_EQ (info.num_items (), 4);
	      ASSERT_IS_TEXT (info.get_item (0), "can't handle return type: ");
	      ASSERT_IS_TREE (info.get_item (1), UNKNOWN_LOCATION, "int");
	      ASSERT_IS_TEXT (info.get_item (2), " for stmt: ");
	      ASSERT_IS_GIMPLE (info.get_item (3), line_6, "return 0;\n");

	      /* ...but not in the dump_context's pending_optinfo.  */
	      ASSERT_EQ (tmp.get_pending_optinfo (), NULL);

	      /* Simulate emitting a high-level summary message, followed
		 by the problem.  */
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, stmt_at_5,
			       "can't optimize loop\n");
	      problem->emit_and_clear ();
	      ASSERT_EQ (res.get_problem (), NULL);

	      /* Verify that the error message was dumped (when the failure
		 occurred).  We can't use a switch here as not all of the
		 values are const expressions (using C++98).  */
	      dump_flags_t effective_filter
		= filter & (MSG_PRIORITY_INTERNALS | MSG_PRIORITY_REEMITTED);
	      if (effective_filter
		  == (MSG_PRIORITY_INTERNALS | MSG_PRIORITY_REEMITTED))
		/* The -fopt-info-internals case.  */
		ASSERT_DUMPED_TEXT_EQ
		  (tmp,
		   "test.c:6:12: note:  === function_that_indirectly_fails"
		   " ===\n"
		   "test.c:6:12: note:   === function_that_fails ===\n"
		   "test.c:6:12: missed:   can't handle return type: int"
		   " for stmt: return 0;\n"
		   "test.c:5:10: missed: can't optimize loop\n"
		   "test.c:6:12: missed: can't handle return type: int"
		   " for stmt: return 0;\n");
	      else if (effective_filter == MSG_PRIORITY_INTERNALS)
		/* The default for dump files.  */
		ASSERT_DUMPED_TEXT_EQ
		  (tmp,
		   "test.c:6:12: note:  === function_that_indirectly_fails"
		   " ===\n"
		   "test.c:6:12: note:   === function_that_fails ===\n"
		   "test.c:6:12: missed:   can't handle return type: int"
		     " for stmt: return 0;\n"
		   "test.c:5:10: missed: can't optimize loop\n");
	      else if (effective_filter == MSG_PRIORITY_REEMITTED)
		/* The default when -fopt-info is enabled.  */
		ASSERT_DUMPED_TEXT_EQ
		  (tmp,
		   "test.c:5:10: missed: can't optimize loop\n"
		   "test.c:6:12: missed: can't handle return type: int"
		   " for stmt: return 0;\n");
	      else
		{
		  gcc_assert (effective_filter == 0);
		  ASSERT_DUMPED_TEXT_EQ
		    (tmp,
		     "test.c:5:10: missed: can't optimize loop\n");
		}
	    }
	  else
	    {
	      /* If dumping was disabled, then no problem should have been
		 created, and nothing should have been dumped.  */
	      ASSERT_EQ (problem, NULL);
	      ASSERT_DUMPED_TEXT_EQ (tmp, "");
	    }
	}
    }
}

/* Run all of the selftests within this file.  */

void
c_opt_problem_cc_tests ()
{
  test_opt_result_success ();
  for_each_line_table_case (test_opt_result_failure_at);
}

} // namespace selftest

#endif /* CHECKING_P */
