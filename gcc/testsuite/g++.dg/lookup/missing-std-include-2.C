/* Example of fix-it hints that add #include directives,
   adding them after a pre-existing #include.  */

/* { dg-options "-fdiagnostics-generate-patch" } */

/* This is padding (to avoid the generated patch containing DejaGnu
   directives).  */

#include "empty.h"

namespace std
{
  extern int sprintf (char *dst, const char *format, ...);
};

void test (void)
{
  std::string s ("hello world"); // { dg-error ".string. is not a member of .std." }
  // { dg-message ".std::string. is defined in header .<string>.; did you forget to .#include <string>.?" "" { target *-*-* } .-1 }

  std::cout << 10; // { dg-error ".cout. is not a member of .std." }
  // { dg-message ".std::cout. is defined in header .<iostream>.; did you forget to .#include <iostream>.?" "" { target *-*-* } .-1 }
}

/* Same again, to test idempotency of the added "#include" fix-it.  */

void test_2 (void)
{
  std::string s ("hello again"); // { dg-error ".string. is not a member of .std." }
  // { dg-message ".std::string. is defined in header .<string>.; did you forget to .#include <string>.?" "" { target *-*-* } .-1 }

  std::cout << 10; // { dg-error ".cout. is not a member of .std." }
  // { dg-message ".std::cout. is defined in header .<iostream>.; did you forget to .#include <iostream>.?" "" { target *-*-* } .-1 }
}

/* Verify the output from -fdiagnostics-generate-patch.
   We expect the patch to begin with a header, containing this
   source filename, via an absolute path.
   Given the path, we can only capture it via regexps.  */
/* { dg-regexp "\\-\\-\\- .*" } */
/* { dg-regexp "\\+\\+\\+ .*" } */

/* Verify the hunks within the patch.
   Use #if 0/#endif rather than comments, to allow the text to contain
   a comment.
   We expect a "#include <string>" and "#include <iostream>" to each have been
   added once, immediately below the last #include.  */
#if 0
{ dg-begin-multiline-output "" }
@@ -7,6 +7,8 @@
    directives).  */
 
 #include "empty.h"
+#include <string>
+#include <iostream>
 
 namespace std
 {
{ dg-end-multiline-output "" }
#endif
