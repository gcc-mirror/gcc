/* Example of emitting an error with an associated note.

   Intended output is similar to:

PATH/test-error-with-note.c:18:11: error: can't find 'foo.h'
    6 | #include <foo.h>
      |           ^~~~~
PATH/test-error-with-note.c:18:11: note: have you looked behind the couch?

   along with the equivalent in SARIF.  */

#include "libdiagnostics.h"
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
  begin_test ("test-error-with-note.c.exe",
	      "test-error-with-note.c.sarif",
	      __FILE__, "c");

  const diagnostic_physical_location *loc_start
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							     main_file,
							     line_num,
							     11);
  const diagnostic_physical_location *loc_end
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							     main_file,
							     line_num,
							     15);
  const diagnostic_physical_location *loc_range
    = diagnostic_manager_new_location_from_range (diag_mgr,
						  loc_start,
						  loc_start,
						  loc_end);

  /* begin quoted source */
  diagnostic_manager_begin_group (diag_mgr);
  
  diagnostic *err = diagnostic_begin (diag_mgr,
				      DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_set_location (err, loc_range);
  diagnostic_finish (err, "can't find %qs", "foo.h");

  diagnostic *note = diagnostic_begin (diag_mgr, DIAGNOSTIC_LEVEL_NOTE);
  diagnostic_set_location (note, loc_range);
  diagnostic_finish (note, "have you looked behind the couch?");

  diagnostic_manager_end_group (diag_mgr);
  /* end quoted source */

  return end_test ();
};

/* Verify the output from the text sink.
   { dg-regexp "\[^\n\r\]+test-error-with-note.c:18:11: error: can't find 'foo.h'" }
   { dg-begin-multiline-output "" }
   18 | #include <foo.h>
      |           ^~~~~
   { dg-end-multiline-output "" }
   { dg-regexp "\[^\n\r\]+test-error-with-note.c:18:11: note: have you looked behind the couch." } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-error-with-note.c "test-error-with-note-c.py" } } */
