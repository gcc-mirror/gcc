/* Example of a grouped error and note, with a fix-it hint on the note.

   Intended output is similar to:
   
/PATH/test-note-with-fix-it-hint.c:21:13: error: unknown field 'colour'
   21 |   return p->colour;
      |             ^~~~~~
/PATH/test-note-with-fix-it-hint.c:21:13: note: did you mean 'color'
   21 |   return p->colour;
      |             ^~~~~~
      |             color

   along with the equivalent in SARIF.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

/* Placeholder source:
_________11111111112
12345678901234567890
  return p->colour;
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  begin_test ("test-note-with-fix-it-hint.c.exe",
	      "test-note-with-fix-it-hint.c.sarif",
	      __FILE__, "c");

  const diagnostic_physical_location *loc_token
    = make_range (diag_mgr, main_file, line_num, 13, 18);

  diagnostic_manager_begin_group (diag_mgr);

  diagnostic *err = diagnostic_begin (diag_mgr, DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_set_location (err, loc_token);
  diagnostic_finish (err, "unknown field %qs", "colour");

  diagnostic *n = diagnostic_begin (diag_mgr, DIAGNOSTIC_LEVEL_NOTE);
  diagnostic_set_location (n, loc_token);
  diagnostic_add_fix_it_hint_replace (n, loc_token, "color");
  diagnostic_finish (n, "did you mean %qs", "color");

  diagnostic_manager_end_group (diag_mgr);

  return end_test ();
}

/* Verify the output from the text sink.
   { dg-regexp "\[^\n\r\]+test-note-with-fix-it-hint.c:21:13: error: unknown field 'colour'" }
   { dg-begin-multiline-output "" }
   21 |   return p->colour;
      |             ^~~~~~
   { dg-end-multiline-output "" }
   { dg-regexp "\[^\n\r\]+test-note-with-fix-it-hint.c:21:13: note: did you mean 'color'" }
   { dg-begin-multiline-output "" }
   21 |   return p->colour;
      |             ^~~~~~
      |             color
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-note-with-fix-it-hint.c "test-note-with-fix-it-hint-c.py" } } */
