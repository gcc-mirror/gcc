/* Example of emitting an error.

   Intended output is similar to:

PATH/test-error-with-note.c:6:11: error: can't find 'foo.h'
    6 | #include <foo.h>
      |           ^~~~~

   along with the equivalent in SARIF.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

/*
_________1111111
1234567890123456
#include <foo.h>
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  begin_test ("test-error.c.exe",
	      "test-error.c.sarif",
	      __FILE__, "c");

  /* begin quoted source */
  const diagnostic_physical_location *loc_start
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr, main_file, line_num, 11);
  const diagnostic_physical_location *loc_end
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr, main_file, line_num, 15);
  const diagnostic_physical_location *loc_range
    = diagnostic_manager_new_location_from_range (diag_mgr,
						  loc_start,
						  loc_start,
						  loc_end);

  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_set_location (d, loc_range);
  
  diagnostic_finish (d, "can't find %qs", "foo.h");
  /* end quoted source */

  return end_test ();
};

/* Verify the output from the text sink.
   { dg-regexp "\[^\n\r\]+test-error.c:17:11: error: can't find 'foo.h'" }
   { dg-begin-multiline-output "" }
   17 | #include <foo.h>
      |           ^~~~~
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-error.c "test-error-c.py" } } */
