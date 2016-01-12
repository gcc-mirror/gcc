/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int ca[100];

__attribute__ ((noinline))
void foo (int n)
{
  unsigned int i;

  for (i = 0; i < n; i++)
    ca[i] = 2;
}

int main (void)
{
  int i;

  check_vect ();

  foo(100);

  for (i = 0; i < 100; ++i) {
    if (ca[i] != 2)
      abort();
  }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
