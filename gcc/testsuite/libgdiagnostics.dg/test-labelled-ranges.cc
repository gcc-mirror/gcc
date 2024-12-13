/* C++ example of multiple locations, with labelling of ranges.

   Intended output is similar to:

PATH/test-labelled-ranges.cc:19:6: error: mismatching types: 'int' and 'const char *'
   19 |   42 + "foo"
      |   ~~ ^ ~~~~~
      |   |    |
      |   int  const char *

   along with the equivalent in SARIF.  */

#include "libgdiagnostics++.h"
#include "test-helpers++.h"

/*
_________11111111112
12345678901234567890
  42 + "foo"
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  FILE *sarif_outfile;
  libgdiagnostics::manager mgr;
  mgr.set_tool_name ("test-labelled-ranges.cc.exe");

  libgdiagnostics::file file = mgr.new_file (__FILE__, "c");
  
  mgr.add_text_sink (stderr, DIAGNOSTIC_COLORIZE_IF_TTY);
  sarif_outfile = fopen ("test-labelled-ranges.cc.sarif", "w");
  if (sarif_outfile)
    mgr.add_sarif_sink (sarif_outfile, file, DIAGNOSTIC_SARIF_VERSION_2_1_0);

  auto loc_operator = mgr.new_location_from_file_line_column (file, line_num, 6);

  auto d (mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR));
  d.set_location (loc_operator);
  d.add_location_with_label (make_range (mgr, file, line_num, 3, 4),
			     "int");
  d.add_location_with_label (make_range (mgr, file, line_num, 8, 12),
			     "const char *");
  d.finish ("mismatching types: %qs and %qs", "int", "const char *");
  
  return 0;
}

/* Check the output from the text sink.  */
/* { dg-regexp "\[^\n\r\]+test-labelled-ranges.cc:19:6: error: mismatching types: 'int' and 'const char \\*'" } */
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
   { dg-final { run-sarif-pytest test-labelled-ranges.cc "test-labelled-ranges.py" } } */
