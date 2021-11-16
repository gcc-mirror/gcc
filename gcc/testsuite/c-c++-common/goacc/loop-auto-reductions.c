/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-graphite-details" } */

#include <stdlib.h>

#define n 10000

unsigned int a[n];

void  __attribute__((noinline,noclone))
foo (void)
{
  int i;
  unsigned int sum = 1;

#pragma acc parallel copyin (a[0:n])
  {
#pragma acc loop auto reduction(+:sum) /* { dg-message "optimized: assigned OpenACC gang vector loop parallelism"} */
    for (i = 0; i < n; ++i)
      sum += a[i];
  }
}
