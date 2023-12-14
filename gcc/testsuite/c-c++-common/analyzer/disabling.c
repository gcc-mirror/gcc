/* Verify that we can override -fanalyzer with -fno-analyzer.  */
/* { dg-additional-options "-fno-analyzer" } */

#include <stdlib.h>

void test (void *ptr)
{
  free (ptr);
  free (ptr); /* { dg-bogus "free" } */
}
