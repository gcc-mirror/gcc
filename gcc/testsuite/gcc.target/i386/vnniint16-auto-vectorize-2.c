/* { dg-do run } */
/* { dg-options "-O2 -mavxvnniint16" } */
/* { dg-require-effective-target avxvnniint16 } */

#define AVXVNNIINT16
#ifndef CHECK
#define CHECK "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include CHECK
#include "vnniint16-auto-vectorize-1.c"

#define N 256

short a_i16[N];
unsigned short b_u16[N], c_u16[N], d_u16[N];
int i16_exp, i16_ref;

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
udot_prod_hi_scalar (unsigned short * restrict a, unsigned short * restrict b,
		     int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
usdot_prod_hi_scalar (unsigned short * restrict a, short *restrict b,
		      int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

void init ()
{
  int i;

  i16_exp = i16_ref = 65535;

  for (i = 0; i < N; i++)
    {
      a_i16[i] = -i + 2;
      b_u16[i] = i * 2;
      c_u16[i] = i * 3;
      d_u16[i] = i * 4;
    }
}

void
TEST (void)
{
  init ();
  i16_exp = usdot_prod_hi (a_i16, b_u16, i16_exp, N);
  i16_ref = usdot_prod_hi_scalar (a_i16, b_u16, i16_ref, N);
  if (i16_exp != i16_ref)
    abort ();

  init ();
  i16_exp = udot_prod_hi (c_u16, d_u16, i16_exp, N);
  i16_ref = udot_prod_hi_scalar (c_u16, d_u16, i16_ref, N);
  if (i16_exp != i16_ref)
    abort ();
}
