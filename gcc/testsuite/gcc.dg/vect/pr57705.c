/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int a[1024];
unsigned char b[1024];

extern void abort (void);

__attribute__((noinline, noclone)) void
foo (int k, int m)
{
  int i, k2 = k;
  for (i = 0; i < 1024; i++)
    {
      a[i] = k2;
      k2 += m + 1;
    }
}

__attribute__((noinline, noclone)) void
bar (int k, int m)
{
  int i, k2 = k;
  for (i = 0; i < 1024; i++)
    {
      k2 += m + 1;
      a[i] = k2;
    }
}

__attribute__((noinline, noclone)) void
baz (int k, int m)
{
  int i, k2 = k;
  for (i = 0; i < 1024; i++)
    {
      a[i] = k2;
      b[i] = i;
      k2 += m + 1;
    }
}

int
main ()
{
  int i;
  check_vect ();
  foo (5, 3);
  for (i = 0; i < 1024; i++)
    if (a[i] != 5 + 4 * i)
      abort ();
  bar (5, 3);
  for (i = 0; i < 1024; i++)
    if (a[i] != 9 + 4 * i)
      abort ();
  baz (5, 3);
  for (i = 0; i < 1024; i++)
    if (a[i] != 5 + 4 * i || b[i] != (unsigned char) i)
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 3 "vect" { target vect_pack_trunc } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 2 "vect" { target { ! vect_pack_trunc } } } } */
