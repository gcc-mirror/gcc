/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int a[1020];

void __attribute__((noinline))
foo ()
{
  for (int i = 0; i < 1020; i += 5)
    {
      a[i] = i;
      a[i+1] = i;
      a[i+2] = i;
      a[i+3] = i;
      a[i+4] = i;
    }
}

int main ()
{
  check_vect ();

  foo ();

  /* check results */
#pragma GCC novector
  for (int i = 0; i < 1020; ++i)
    if (a[i] != ((i + 4) / 5) * 5)
      abort ();

  return 0;
}

/* Make sure we are not triggering hybrid SLP due to the IV update.  */
/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
