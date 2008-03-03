/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

__attribute__ ((noinline))
void with_restrict(int * __restrict p)
{
  int i;
  int *q = p - 2;

  for (i = 0; i < 1000; ++i) {
    p[i] = q[i];
  }
}

__attribute__ ((noinline))
void without_restrict(int * p)
{
  int i;
  int *q = p - 2;

  for (i = 0; i < 1000; ++i) {
    p[i] = q[i];
  }
}

int main(void)
{
  int i;
  int a[1002];
  int b[1002];

  check_vect ();

  for (i = 0; i < 1002; ++i) {
    a[i] = b[i] = i;
  }

  with_restrict(a + 2);
  without_restrict(b + 2);

  for (i = 0; i < 1002; ++i) {
    if (a[i] != b[i])
      abort();
  }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 2 "vect"  } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
