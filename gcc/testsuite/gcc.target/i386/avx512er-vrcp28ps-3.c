/* { dg-do run } */
/* { dg-require-effective-target avx512er } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx512er" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include "avx512er-check.h"

#define MAX 1000
#define EPS 0.00001

__attribute__ ((noinline, optimize (0)))
void static
compute_rcp_ref (float *a, float *b, float *r)
{
  for (int i = 0; i < MAX; i++)
    r[i] = a[i] / b[i];
}

__attribute__ ((noinline))
void static
compute_rcp_exp (float *a, float *b, float *r)
{
  for (int i = 0; i < MAX; i++)
    r[i] = a[i] / b[i];
}

void static
avx512er_test (void)
{
  float a[MAX];
  float b[MAX];
  float ref[MAX];
  float exp[MAX];

  for (int i = 0; i < MAX; i++)
    {
      a[i] = 179.345 - 6.5645 * i;
      b[i] = 8765.987 - 8.6756 * i;
    }

  compute_rcp_ref (a, b, ref);
  compute_rcp_exp (a, b, exp);

#pragma GCC novector
  for (int i = 0; i < MAX; i++)
    {
      float rel_err = (ref[i] - exp[i]) / ref[i];
      rel_err = rel_err > 0.0 ? rel_err : -rel_err;
      if (rel_err > EPS)
	abort ();
    }
}
