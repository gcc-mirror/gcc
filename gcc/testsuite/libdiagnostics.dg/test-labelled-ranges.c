/* Example of multiple locations, with labelling of ranges.

   Intended output is similar to:

PATH/test-labelled-ranges.c:9:6: error: mismatching types: 'int' and 'const char *'
   19 |   42 + "foo"
      |   ~~ ^ ~~~~~
      |   |    |
      |   int  const char *

   along with the equivalent in SARIF.  */

#include "libdiagnostics.h"
#include "test-helpers.h"

/*
_________11111111112
12345678901234567890
  42 + "foo"
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  begin_test ("test-labelled-ranges.c.exe",
	      "test-labelled-ranges.c.sarif",
	      __FILE__, "c");

  const diagnostic_physical_location *loc_operator
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							     main_file,
							     line_num,
							     6);

  /* begin quoted source */
  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_set_location (d, loc_operator);
  diagnostic_add_location_with_label (d,
				      make_range (diag_mgr,
						  main_file,
						  line_num, 3, 4),
				      "int");
  diagnostic_add_location_with_label (d,
				      make_range (diag_mgr,
						  main_file,
						  line_num, 8, 12),
				      "const char *");
  
  diagnostic_finish (d, "mismatching types: %qs and %qs", "int", "const char *");
  /* end quoted source */
  
  return end_test ();
}

/* Check the output from the text sink.  */
/* { dg-regexp "\[^\n\r\]+test-labelled-ranges.c:19:6: error: mismatching types: 'int' and 'const char \\*'" } */
/* { dg-begin-multiline-output "" }
   19 |   42 + "foo"
      |   ~~ ^ ~~~~~
      |   |    |
      |   int  const char *
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-labelled-ranges.c "test-labelled-ranges.py" } } */
