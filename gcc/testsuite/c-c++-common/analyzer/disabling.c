/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

/* Verify that we can override -fanalyzer with -fno-analyzer.  */
/* { dg-additional-options "-fno-analyzer" } */

#include <stdlib.h>

void test (void *ptr)
{
  free (ptr);
  free (ptr); /* { dg-bogus "free" } */
}
