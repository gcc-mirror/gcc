/* Example of writing diagnostics as SARIF to a file.  */

#include "libdiagnostics.h"

/*
_________111111111122
123456789012345678901
PRINT "hello world!";
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  FILE *sarif_outfile = fopen ("test-write-sarif-to-file.c.sarif", "w");
  if (!sarif_outfile)
    return -1;

  diagnostic_manager *diag_mgr = diagnostic_manager_new ();
  diagnostic_manager_set_tool_name (diag_mgr, "test-write-sarif-to-file.c.exe");

  const diagnostic_file *file = diagnostic_manager_new_file (diag_mgr, __FILE__, "c");

  diagnostic_manager_add_sarif_sink (diag_mgr, sarif_outfile, file,
				     DIAGNOSTIC_SARIF_VERSION_2_1_0);

  const diagnostic_physical_location *loc_start
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr, file, line_num, 8);
  const diagnostic_physical_location *loc_end
    = diagnostic_manager_new_location_from_file_line_column (diag_mgr, file, line_num, 19);
  const diagnostic_physical_location *loc_range
    = diagnostic_manager_new_location_from_range (diag_mgr,
						  loc_start,
						  loc_start,
						  loc_end);

  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_set_location (d, loc_range);

  diagnostic_finish (d, "can't find %qs", "foo");

  diagnostic_manager_release (diag_mgr);

  fclose (sarif_outfile);

  return 0;
};

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-write-sarif-to-file.c "test-write-sarif-to-file-c.py" } } */
