/* Contributed by Nicholas Ormrod */
/* Origin: PR preprocessor/60723 */

/* This tests that multi-line macro callsites, which are defined
   in system headers and whose expansion contains a builtin followed
   by a non-builtin token, do not generate a line directive that
   mark the current file as being a system file, when performing
   non-integrated preprocessing. */
/* System files suppress div-by-zero warnings, so the presence of
   such indicates the lack of the bug.

   { dg-do compile }
   { dg-options -no-integrated-cpp }  */

#include "syshdr4.h"
FOO(
)

int
foo()
{
  return 1 / 0; /* { dg-warning "div-by-zero" } */
  return 0;
}
