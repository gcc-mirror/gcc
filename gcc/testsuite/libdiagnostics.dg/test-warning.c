/* Example of emitting a warning.

   Intended output is similar to:

/PATH/test-warning.c:17:8: warning: this is a warning
   17 | PRINT "hello world!";
      |        ^~~~~~~~~~~~

   along with the equivalent in SARIF.  */

#include "libdiagnostics.h"
#include "test-helpers.h"

/*
_________111111111122
123456789012345678901
PRINT "hello world!";
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  begin_test ("test-warning.c.exe",
	      "test-warning.c.sarif",
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
				    DIAGNOSTIC_LEVEL_WARNING);
  diagnostic_set_location (d, loc_range);

  diagnostic_finish (d, "this is a warning");
  /* end quoted source */

  return end_test ();
}

/* Verify the output from the text sink.
   { dg-regexp "\[^\n\r\]+test-warning.c:17:8: warning: this is a warning" }
   { dg-begin-multiline-output "" }
   17 | PRINT "hello world!";
      |        ^~~~~~~~~~~~
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-warning.c "test-warning-c.py" } } */
