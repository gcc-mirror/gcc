/* Example of setting a CWE and adding extra metadata.

   Intended output is similar to:

PATH/test-metadata.c:21:3: warning: never use 'gets' [CWE-242] [STR34-C]
   21 |   gets (buf);
      |   ^~~~~~~~~~

   where the metadata tags are linkified in a sufficiently capable terminal,
   along with the equivalent in SARIF.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

/* Placeholder source:
_________11111111112
12345678901234567890
void test_cwe (void)
{
  char buf[1024];
  gets (buf);
}
*/
const int line_num = __LINE__ - 3;

int
main ()
{
  begin_test ("FooChecker",
	      "test-metadata.c.sarif",
	      __FILE__, "c");

  diagnostic_manager_set_full_name (diag_mgr, "FooChecker 0.1 (en_US)");
  diagnostic_manager_set_version_string (diag_mgr, "0.1");
  diagnostic_manager_set_version_url (diag_mgr, "https://www.example.com/0.1/");

  const diagnostic_physical_location *loc_token
    = make_range (diag_mgr, main_file, line_num, 3, 12);
  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_WARNING);
  diagnostic_set_location (d, loc_token);
  diagnostic_set_cwe (d, 242); /* CWE-242: Use of Inherently Dangerous Function.  */
  diagnostic_add_rule (d, "STR34-C", "https://example.com/");
  
  diagnostic_finish (d, "never use %qs", "gets");

  return end_test ();
}

/* { dg-regexp "\[^\n\r\]+test-metadata.c:21:3: warning: never use 'gets' \\\[CWE-242\\\] \\\[STR34-C\\\]" } */
/* { dg-begin-multiline-output "" }
   21 |   gets (buf);
      |   ^~~~~~~~~~
   { dg-end-multiline-output "" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-metadata.c "test-metadata-c.py" } } */
