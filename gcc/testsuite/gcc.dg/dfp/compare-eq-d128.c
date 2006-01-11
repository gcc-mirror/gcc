/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.5.9 Equality operators.
   Compare decimal float values against each other at runtime.  */

#define WIDTH 128
#include "compare-eq.h"

int main ()
{
  test_compares ();
  return 0;
}
