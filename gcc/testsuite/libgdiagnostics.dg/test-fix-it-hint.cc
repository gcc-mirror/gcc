/* C++ example of a fix-it hint, including patch generation.

   Intended output is similar to:

PATH/test-fix-it-hint.cc:19:13: error: unknown field 'colour'; did you mean 'color'
   19 |   return p->colour;
      |             ^~~~~~
      |             color

   along with the equivalent in SARIF, and a generated patch (on stderr) to
   make the change.  */

#include "libgdiagnostics++.h"
#include "test-helpers++.h"

/*
_________11111111112
12345678901234567890
  return p->colour;
*/
const int line_num = __LINE__ - 2;

int
main ()
{
  libgdiagnostics::manager mgr;

  auto file = mgr.new_file (__FILE__, "c");

  mgr.add_text_sink (stderr, DIAGNOSTIC_COLORIZE_IF_TTY);

  auto loc_token = make_range (mgr, file, line_num, 13, 18);

  auto d = mgr.begin_diagnostic (DIAGNOSTIC_LEVEL_ERROR);
  d.set_location (loc_token);

  d.add_fix_it_hint_replace (loc_token, "color");
  
  d.finish ("unknown field %qs; did you mean %qs", "colour", "color");

  mgr.write_patch (stderr);

  return 0;
}

/* Verify the output from the text sink.
   { dg-regexp "\[^\n\r\]+test-fix-it-hint.cc:19:13: error: unknown field 'colour'; did you mean 'color'" }
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
