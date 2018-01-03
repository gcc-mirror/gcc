/* Run expensive selftests.  */
/* { dg-options "-O" } */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "edit-context.h"
#include "selftest.h"
#include "selftest-diagnostic.h"

int plugin_is_GPL_compatible;

#if CHECKING_P

namespace selftest {

/* Subroutine of test_fixit_on_very_long_line.
   Verify that LOC has the EXPECTED_COLUMN, apart from the various
   cases where it can't.  */

static void
verify_column (location_t loc,
	       const line_map_ordinary *ord_map,
	       int line_width,
	       int expected_column)
{
  ASSERT_TRUE (/* Normal case.  */
	       LOCATION_COLUMN (loc) == expected_column
	       /* ord_map can't store columns e.g. due to
		  max_column_hint being too high.  */
	       || ord_map->m_column_and_range_bits == 0
	       /* Running out of location_t values.  */
	       || loc > LINE_MAP_MAX_LOCATION_WITH_COLS
	       /* column exceeds LINE_MAP_MAX_COLUMN_NUMBER.  */
	       || expected_column > (int)LINE_MAP_MAX_COLUMN_NUMBER
	       /* column exceeds max_column_hint for ord_map.  */
	       || expected_column > line_width);
}

/* Subroutine of test_fixit_on_very_long_line.
   Run various things for RICHLOC, but don't check; we just want them
   to survive.  */

static void
test_richloc (rich_location *richloc)
{
  /* Run the diagnostic and fix-it printing code.  */
  test_diagnostic_context dc;
  diagnostic_show_locus (&dc, richloc, DK_ERROR);

  /* Generate a diff.  */
  edit_context ec;
  ec.add_fixits (richloc);
  char *diff = ec.generate_diff (true);
  free (diff);
}

/* Verify that the fix-it-printing code can cope with very long lines
   (PR c/82050).  */

static void
test_fixit_on_very_long_line (const line_table_case &case_)
{
  /* Various interesting column/line-width values, to try to tickle
     out bugs.  */
  const int VERY_LONG_LINE = 8192;
  const int columns[] = {0,
			 1,
			 80,
			 LINE_MAP_MAX_COLUMN_NUMBER - 2,
			 LINE_MAP_MAX_COLUMN_NUMBER - 1,
			 LINE_MAP_MAX_COLUMN_NUMBER,
			 LINE_MAP_MAX_COLUMN_NUMBER + 1,
			 LINE_MAP_MAX_COLUMN_NUMBER + 2,
			 VERY_LONG_LINE,
			 VERY_LONG_LINE + 5};
  for (unsigned int width_idx = 0; width_idx < ARRAY_SIZE (columns);
       width_idx++)
    {
      int line_width = columns[width_idx];

      /* Create a source file with a very long line.  */
      named_temp_file tmp (".c");
      FILE *f = fopen (tmp.get_filename (), "w");
      for (int i = 0; i < line_width; i++)
	fputc (' ', f);
      fputc ('\n', f);
      fclose (f);

      line_table_test ltt (case_);
      const line_map_ordinary *ord_map = linemap_check_ordinary
	(linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 0));
      linemap_line_start (line_table, 1, line_width);

      for (unsigned int start_idx = 0; start_idx < ARRAY_SIZE (columns);
	   start_idx++)
	{
	  int start_col = columns[start_idx];
	  location_t start_loc
	    = linemap_position_for_line_and_column (line_table, ord_map, 1,
						    start_col);
	  verify_column (start_loc, ord_map, line_width, start_col);
	  for (unsigned int finish_idx = 0; finish_idx < ARRAY_SIZE (columns);
	       finish_idx++)
	    {
	      int finish_col = columns[finish_idx];
	      location_t finish_loc
		= linemap_position_for_line_and_column (line_table, ord_map, 1,
							finish_col);
	      verify_column (finish_loc, ord_map, line_width, finish_col);

	      /* Now use start-finish to exercise the fix-it code.
		 In each case, run the printing code, but don't check;
		 we just want it to survive.  */

	      /* Insertion.  */
	      {
		rich_location richloc (line_table, start_loc);
		richloc.add_fixit_insert_after (start_loc, "insertion");
		test_richloc (&richloc);
	      }

	      /* Replacement.  */
	      {
		rich_location richloc (line_table, start_loc);
		source_range range
		  = source_range::from_locations (start_loc, finish_loc);
		richloc.add_fixit_replace (range, "replacement");
		test_richloc (&richloc);
	      }

	      /* Deletion.  */
	      {
		rich_location richloc (line_table, start_loc);
		source_range range
		  = source_range::from_locations (start_loc, finish_loc);
		richloc.add_fixit_remove (range);
		test_richloc (&richloc);
	      }
	    }
	}
    }
}

/* Callback handler for the PLUGIN_FINISH event.
   At this point, all GCC subsystems should be initialized and
   "warmed up"; this is where we run our unit tests.  */

static void
expensive_tests (void */*gcc_data*/, void */*user_data*/)
{
  test_runner r ("expensive_selftests_plugin");

  for_each_line_table_case (test_fixit_on_very_long_line);
}

} // namespace selftest

#endif /* #if CHECKING_P */

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
#if CHECKING_P
  const char *plugin_name = plugin_info->base_name;
  register_callback (plugin_info->base_name,
		     PLUGIN_FINISH,
		     selftest::expensive_tests,
		     NULL); /* void *user_data */
#else
  inform (UNKNOWN_LOCATION, "self-tests are not enabled in this build");
#endif /* #if CHECKING_P */
  return 0;
}
