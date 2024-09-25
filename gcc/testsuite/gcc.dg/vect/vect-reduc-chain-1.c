/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw { target { aarch64*-*-* || arm*-*-* } } } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#include "tree-vect.h"

#define N 50

#ifndef SIGNEDNESS_1
#define SIGNEDNESS_1 signed
#define SIGNEDNESS_2 signed
#endif

SIGNEDNESS_1 int __attribute__ ((noipa))
f (SIGNEDNESS_1 int res,
   SIGNEDNESS_2 char *restrict a,
   SIGNEDNESS_2 char *restrict b,
   SIGNEDNESS_2 char *restrict c,
   SIGNEDNESS_2 char *restrict d,
   SIGNEDNESS_1 int *restrict e)
{
  for (int i = 0; i < N; ++i)
    {
      res += a[i] * b[i];
      res += c[i] * d[i];
      res += e[i];
    }
  return res;
}

#define BASE ((SIGNEDNESS_2 int) -1 < 0 ? -126 : 4)
#define OFFSET 20

int
main (void)
{
  check_vect ();

  SIGNEDNESS_2 char a[N], b[N];
  SIGNEDNESS_2 char c[N], d[N];
  SIGNEDNESS_1 int e[N];
  int expected = 0x12345;

  #pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      a[i] = BASE + i * 5;
      b[i] = BASE + OFFSET + i * 4;
      c[i] = BASE + i * 2;
      d[i] = BASE + OFFSET + i * 3;
      e[i] = i;
      expected += a[i] * b[i];
      expected += c[i] * d[i];
      expected += e[i];
    }

  if (f (0x12345, a, b, c, d, e) != expected)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vect_recog_dot_prod_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing statement: \\S+ = DOT_PROD_EXPR" 2 "vect" { target vect_sdot_qi } } } */
