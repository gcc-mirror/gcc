/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.5.8 Relational operators.
   Compare decimal float values against each other at runtime.  */

#define WIDTH 32
#include "compare-rel.h"

int
main ()
{
  test_compares ();
  return 0;
}
