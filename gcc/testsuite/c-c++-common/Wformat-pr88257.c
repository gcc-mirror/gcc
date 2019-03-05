/* { dg-options "-Wformat -fdiagnostics-show-caret" } */

#include "Wformat-pr88257.h"

void test (const char *msg)
{
  __builtin_printf ("size: %" PRIu32 "\n", msg); /* { dg-warning "expects argument of type" } */
  /* { dg-begin-multiline-output "" }
   __builtin_printf ("size: %" PRIu32 "\n", msg);
                     ^~~~~~~~~              ~~~
                                            |
                                            const char *
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output "" }
 # define PRIu32  "u"
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output "" }
   __builtin_printf ("size: %" PRIu32 "\n", msg);
                     ^~~~~~~~~~~~~~~~~~~~~  ~~~
                                            |
                                            const char*
     { dg-end-multiline-output "" { target c++ } } */
}
