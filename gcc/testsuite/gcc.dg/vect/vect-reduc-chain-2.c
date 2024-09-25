/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw { target { aarch64*-*-* || arm*-*-* } } } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#include "tree-vect.h"

#define N 50

#ifndef SIGNEDNESS_1
#define SIGNEDNESS_1 signed
#define SIGNEDNESS_2 unsigned
#define SIGNEDNESS_3 signed
#define SIGNEDNESS_4 signed
#endif

SIGNEDNESS_1 int __attribute__ ((noipa))
fn (SIGNEDNESS_1 int res,
   SIGNEDNESS_2 char *restrict a,
   SIGNEDNESS_2 char *restrict b,
   SIGNEDNESS_3 char *restrict c,
   SIGNEDNESS_3 char *restrict d,
   SIGNEDNESS_4 short *restrict e,
   SIGNEDNESS_4 short *restrict f,
   SIGNEDNESS_1 int *restrict g)
{
  for (int i = 0; i < N; ++i)
    {
      res += a[i] * b[i];
      res += i + 1;
      res += c[i] * d[i];
      res += e[i] * f[i];
      res += g[i];
    }
  return res;
}

#define BASE2 ((SIGNEDNESS_2 int) -1 < 0 ? -126 : 4)
#define BASE3 ((SIGNEDNESS_3 int) -1 < 0 ? -126 : 4)
#define BASE4 ((SIGNEDNESS_4 int) -1 < 0 ? -1026 : 373)
#define OFFSET 20

int
main (void)
{
  check_vect ();

  SIGNEDNESS_2 char a[N], b[N];
  SIGNEDNESS_3 char c[N], d[N];
  SIGNEDNESS_4 short e[N], f[N];
  SIGNEDNESS_1 int g[N];
  int expected = 0x12345;

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      a[i] = BASE2 + i * 5;
      b[i] = BASE2 + OFFSET + i * 4;
      c[i] = BASE3 + i * 2;
      d[i] = BASE3 + OFFSET + i * 3;
      e[i] = BASE4 + i * 6;
      f[i] = BASE4 + OFFSET + i * 5;
      g[i] = i;
      expected += a[i] * b[i];
      expected += i + 1;
      expected += c[i] * d[i];
      expected += e[i] * f[i];
      expected += g[i];
    }

  if (fn (0x12345, a, b, c, d, e, f, g) != expected)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vect_recog_dot_prod_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump "vectorizing statement: \\S+ = DOT_PROD_EXPR" "vect" { target { vect_sdot_qi } } } } */
/* { dg-final { scan-tree-dump "vectorizing statement: \\S+ = DOT_PROD_EXPR" "vect" { target { vect_udot_qi } } } } */
/* { dg-final { scan-tree-dump "vectorizing statement: \\S+ = DOT_PROD_EXPR" "vect" { target { vect_sdot_hi } } } } */
