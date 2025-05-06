/* C++ example of using a logical location.

   Intended output is similar to:

In function 'test_qualified_name':
PATH/test-error-with-note.cc:17:8: error: can't find 'foo'
   17 | PRINT "hello world!";
      |        ^~~~~~~~~~~~

   along with the equivalent in SARIF.  */

#include "libgdiagnostics++.h"

/* Placeholder source:
_________111111111122
123456789012345678901
PRINT "hello world!";
*/
const int line_num = __LINE__ - 2;

#include <assert.h>
#include <string.h>

int
main ()
{
  libgdiagnostics::manager mgr;

  auto file = mgr.new_file (__FILE__, "c");

  mgr.add_text_sink (stderr, DIAGNOSTIC_COLORIZE_IF_TTY);

  auto loc_start = mgr.new_location_from_file_line_column (file, line_num, 8);
  auto loc_end = mgr.new_location_from_file_line_column (file, line_num, 19);
  auto loc_range = mgr.new_location_from_range (loc_start, loc_start, loc_end);

  auto err (mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR));
  err.set_location (loc_range);

  libgdiagnostics::logical_location logical_loc
    = mgr.new_logical_location (DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION,
				NULL, /* parent */
				"test_short_name",
				"test_qualified_name",
				"test_decorated_name");
  err.set_logical_location (logical_loc);

  err.finish ("can't find %qs", "foo");

  /* Verify that the accessors work.  */
  assert (logical_loc.get_kind ()
	  == DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION);
  assert (logical_loc.get_parent ().m_inner == nullptr);
  assert (!strcmp (logical_loc.get_short_name (),
		   "test_short_name"));
  assert (!strcmp (logical_loc.get_fully_qualified_name (),
		   "test_qualified_name"));
  assert (!strcmp (logical_loc.get_decorated_name (),
		   "test_decorated_name"));

  /* Verify that libgdiagnostic::logical_location instances created with
     equal values compare as equal.  */
  libgdiagnostics::logical_location dup
    = mgr.new_logical_location (DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION,
				NULL, /* parent */
				"test_short_name",
				"test_qualified_name",
				"test_decorated_name");
  assert (dup == logical_loc); 

  /* Verify that libgdiagnostic::logical_location instances created with
     differing values compare as non-equal.  */
  libgdiagnostics::logical_location other
    = mgr.new_logical_location (DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION,
				NULL, /* parent */
				"something_else",
				NULL, NULL);
  assert (other != logical_loc); 

  return 0;
}

/* Check the output from the text sink.  */
/* { dg-begin-multiline-output "" }
In function 'test_qualified_name':
   { dg-end-multiline-output "" } */
/* { dg-regexp "\[^\n\r\]+test-logical-location.cc:17:8: error: can't find 'foo'" } */
/* { dg-begin-multiline-output "" }
   17 | PRINT "hello world!";
      |        ^~~~~~~~~~~~
   { dg-end-multiline-output "" } */
