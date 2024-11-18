/* Example of a fix-it hint, including patch generation.

   Intended output is similar to:

PATH/test-fix-it-hint.c:19:13: error: unknown field 'colour'; did you mean 'color'
   19 |   return p->colour;
      |             ^~~~~~
      |             color

   along with the equivalent in SARIF, and a generated patch (on stderr) to
   make the change.  */

#include "libdiagnostics.h"
#include "test-helpers.h"

/*
_________11111111112
12345678901234567890
  return p->colour;
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  begin_test ("test-fix-it-hint.c.exe",
	      "test-fix-it-hint.c.sarif",
	      __FILE__, "c");

  const diagnostic_physical_location *loc_token
    = make_range (diag_mgr, main_file, line_num, 13, 18);

  /* begin quoted source */
  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_set_location (d, loc_token);

  diagnostic_add_fix_it_hint_replace (d, loc_token, "color");
  
  diagnostic_finish (d, "unknown field %qs; did you mean %qs", "colour", "color");
  /* end quoted source */

  diagnostic_manager_write_patch (diag_mgr, stderr);

  return end_test ();
}

/* Verify the output from the text sink.
   { dg-regexp "\[^\n\r\]+test-fix-it-hint.c:19:13: error: unknown field 'colour'; did you mean 'color'" }
   { dg-begin-multiline-output "" }
   19 |   return p->colour;
      |             ^~~~~~
      |             color
   { dg-end-multiline-output "" } */

/* Verify the output from diagnostic_manager_write_patch.
   We expect the patch to begin with a header, containing this
   source filename, via an absolute path.
   Given the path, we can only capture it via regexps.  */
/* { dg-regexp "\\-\\-\\- .*" } */
/* { dg-regexp "\\+\\+\\+ .*" } */
/* Use #if 0/#endif rather than comments, to allow the text to contain
   a comment.  */
#if 0
{ dg-begin-multiline-output "" }
@@ -16,7 +16,7 @@
 /*
 _________11111111112
 12345678901234567890
-  return p->colour;
+  return p->color;
 */
 const int line_num = __LINE__ - 2;
 
{ dg-end-multiline-output "" }
#endif

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-fix-it-hint.c "test-fix-it-hint-c.py" } } */
