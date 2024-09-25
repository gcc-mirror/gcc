/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 50

#ifndef SIGNEDNESS_1
#define SIGNEDNESS_1 signed
#define SIGNEDNESS_2 unsigned
#define SIGNEDNESS_3 signed
#endif

SIGNEDNESS_1 int __attribute__ ((noipa))
f (SIGNEDNESS_1 int res,
   SIGNEDNESS_2 char *restrict a,
   SIGNEDNESS_2 char *restrict b,
   SIGNEDNESS_3 short *restrict c,
   SIGNEDNESS_3 short *restrict d,
   SIGNEDNESS_1 int *restrict e)
{
  for (int i = 0; i < N; ++i)
    {
      short diff = a[i] - b[i];
      SIGNEDNESS_2 short abs = diff < 0 ? -diff : diff;
      res += abs;
      res += c[i] * d[i];
      res += e[i];
    }
  return res;
}

#define BASE2 ((SIGNEDNESS_2 int) -1 < 0 ? -126 : 4)
#define BASE3 ((SIGNEDNESS_3 int) -1 < 0 ? -1236 : 373)
#define OFFSET 20

int
main (void)
{
  check_vect ();

  SIGNEDNESS_2 char a[N], b[N];
  SIGNEDNESS_3 short c[N], d[N];
  SIGNEDNESS_1 int e[N];
  int expected = 0x12345;

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      a[i] = BASE2 + i * 5;
      b[i] = BASE2 - i * 4;
      c[i] = BASE3 + i * 2;
      d[i] = BASE3 + OFFSET + i * 3;
      e[i] = i;
      short diff = a[i] - b[i];
      SIGNEDNESS_2 short abs = diff < 0 ? -diff : diff;
      expected += abs;
      expected += c[i] * d[i];
      expected += e[i];
    }

  if (f (0x12345, a, b, c, d, e) != expected)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vectorizing statement: \\S+ = SAD_EXPR" "vect" { target vect_udot_qi } } } */
/* { dg-final { scan-tree-dump "vectorizing statement: \\S+ = DOT_PROD_EXPR" "vect" { target vect_sdot_hi } } } */
