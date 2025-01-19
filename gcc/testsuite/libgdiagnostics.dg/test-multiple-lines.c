/* Example of a warning with multiple locations in various source lines,
   with an insertion fix-it hint.

   Intended output is similar to:
   
/PATH/test-multiple-lines.c:23:29: warning: missing comma
   22 | const char *strs[3] = {"foo",
      |                        ~~~~~ 
   23 |                        "bar"
      |                        ~~~~~^
   24 |                        "baz"};
      |                        ~~~~~ 

   along with the equivalent in SARIF.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

/* Placeholder source (missing comma after "bar"):
_________11111111112222222222
12345678901234567890123456789
const char *strs[3] = {"foo",
                       "bar"
                       "baz"};
*/
const int foo_line_num = __LINE__ - 4;

int
main ()
{
  begin_test ("test-multiple-lines.c.exe",
	      "test-multiple-lines.c.sarif",
	      __FILE__, "c");
  
  /* begin quoted source */
  const diagnostic_physical_location *loc_comma
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr,
							     main_file,
							     foo_line_num + 1,
							     29);
  const diagnostic_physical_location *loc_foo
    = make_range (diag_mgr, main_file, foo_line_num, 24, 28);
  const diagnostic_physical_location *loc_bar
    = make_range (diag_mgr, main_file, foo_line_num + 1, 24, 28);
  const diagnostic_physical_location *loc_baz
    = make_range (diag_mgr, main_file, foo_line_num + 2, 24, 28);

  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_WARNING);
  diagnostic_set_location (d, loc_comma);
  diagnostic_add_location (d, loc_foo);
  diagnostic_add_location (d, loc_bar);
  diagnostic_add_location (d, loc_baz);

  diagnostic_add_fix_it_hint_insert_after (d, loc_bar, ",");

  diagnostic_finish (d, "missing comma");
  /* end quoted source */

  return end_test ();
};

/* { dg-regexp "\[^\n\r\]+test-multiple-lines.c:23:29: warning: missing comma" } */
/* { dg-begin-multiline-output "" }
   22 | const char *strs[3] = {"foo",
      |                        ~~~~~ 
   23 |                        "bar"
      |                        ~~~~~^
   24 |                        "baz"};
      |                        ~~~~~ 
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-multiple-lines.c "test-multiple-lines-c.py" } } */
