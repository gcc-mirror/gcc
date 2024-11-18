/* C++ example of emitting an error.

   Intended output is similar to:

PATH/test-error.cc:16:8: error: can't find 'foo'
   16 | PRINT "hello world!";
      |        ^~~~~~~~~~~~

   along with the equivalent in SARIF.  */

#include "libdiagnostics++.h"

/*
_________111111111122
123456789012345678901
PRINT "hello world!";
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  libdiagnostics::manager mgr;

  auto file = mgr.new_file (__FILE__, "c");

  mgr.add_text_sink (stderr, DIAGNOSTIC_COLORIZE_IF_TTY);

  auto loc_start = mgr.new_location_from_file_line_column (file, line_num, 8);
  auto loc_end = mgr.new_location_from_file_line_column (file, line_num, 19);
  auto loc_range = mgr.new_location_from_range (loc_start,
						loc_start,
						loc_end);

  libdiagnostics::diagnostic d (mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR));
  d.set_location (loc_range);
  d.finish ("can't find %qs", "foo");

  return 0;
};

/* Verify the output from the text sink.
   { dg-regexp "\[^\n\r\]+test-error.cc:16:8: error: can't find 'foo'" }
   { dg-begin-multiline-output "" }
   16 | PRINT "hello world!";
      |        ^~~~~~~~~~~~
   { dg-end-multiline-output "" } */
