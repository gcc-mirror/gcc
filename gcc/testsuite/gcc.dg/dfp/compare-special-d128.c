/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.5.8 Relational operators.
   C99 6.5.9 Equality operators.
   Compare decimal float special values at runtime.  */

#define WIDTH 128
#include "compare-special.h"

int
main ()
{
  test_compares ();
  return 0;
}
