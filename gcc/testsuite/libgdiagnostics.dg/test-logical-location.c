/* Example of using a logical location.

   Intended output is similar to:

In function 'test_qualified_name':
PATH/test-error-with-note.c:18:8: error: can't find 'foo'
   18 | PRINT "hello world!";
      |        ^~~~~~~~~~~~

   along with the equivalent in SARIF.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

/* Placeholder source:
_________111111111122
123456789012345678901
PRINT "hello world!";
*/
const int line_num = __LINE__ - 2;

#include <assert.h>

int
main ()
{
  begin_test ("test-logical-location.c.exe",
	      "test-logical-location.c.sarif",
	      __FILE__, "c");

  const diagnostic_physical_location *loc_start
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							     main_file,
							     line_num,
							     8);
  const diagnostic_physical_location *loc_end
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							     main_file,
							     line_num,
							     19);
  const diagnostic_physical_location *loc_range
    = diagnostic_manager_new_location_from_range (diag_mgr,
					  loc_start,
					  loc_start,
					  loc_end);

  /* begin quoted source */
  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_set_location (d, loc_range);

  const diagnostic_logical_location *logical_loc
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION,
					       NULL, /* parent */
					       "test_short_name",
					       "test_qualified_name",
					       "test_decorated_name");

  diagnostic_set_logical_location (d, logical_loc);

  diagnostic_finish (d, "can't find %qs", "foo");
  /* end quoted source */

  /* Verify that creating a diagnostic_logical_location with equal values 
     yields the same instance.  */
  const diagnostic_logical_location *dup
    = diagnostic_manager_new_logical_location (diag_mgr,
					       DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION,
					       NULL, /* parent */
					       "test_short_name",
					       "test_qualified_name",
					       "test_decorated_name");
  assert (dup == logical_loc); 

  return end_test ();
}

/* Check the output from the text sink.  */
/* { dg-begin-multiline-output "" }
In function 'test_qualified_name':
   { dg-end-multiline-output "" } */
/* { dg-regexp "\[^\n\r\]+test-logical-location.c:18:8: error: can't find 'foo'" } */
/* { dg-begin-multiline-output "" }
   18 | PRINT "hello world!";
      |        ^~~~~~~~~~~~
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-logical-location.c "test-logical-location-c.py" } } */
