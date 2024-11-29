/* Example of emitting an error without a column number.

   Intended output is similar to:

PATH/test-error-with-note.c:6: error: can't find 'foo'
    6 | #include <foo.h>

   along with the equivalent in SARIF.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

/*
_________111111111122
123456789012345678901
#include <foo.h>
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  begin_test ("test-no-column.c.exe",
	      "test-no-column.c.sarif",
	      __FILE__, "c");

  /* begin quoted source */
  const diagnostic_physical_location *loc
    = diagnostic_manager_new_location_from_file_and_line (diag_mgr,
							  main_file,
							  line_num);

  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_set_location (d, loc);

  diagnostic_finish (d, "can't find %qs", "foo.h");
  /* end quoted source */

  return end_test ();
}

/* Verify the output from the text sink.
   { dg-regexp "\[^\n\r\]+test-no-column.c:16: error: can't find 'foo.h'" }
   { dg-begin-multiline-output "" }
   16 | #include <foo.h>
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-no-column.c "test-no-column-c.py" } } */
