/* { dg-do run } */
/* { dg-require-effective-target avx512er } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx512er" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include <math.h>
#include "avx512er-check.h"

#define MAX 1000
#define EPS 0.00001

__attribute__ ((noinline, optimize (1, "-fno-fast-math")))
void static
compute_rsqrt_ref (float *a, float *r)
{
  for (int i = 0; i < MAX; i++)
    r[i] = 1.0 / sqrtf (a[i]);
}

__attribute__ ((noinline))
void static
compute_rsqrt_exp (float *a, float *r)
{
  for (int i = 0; i < MAX; i++)
    r[i] = 1.0 / sqrtf (a[i]);
}

void static
avx512er_test (void)
{
  float in[MAX];
  float ref[MAX];
  float exp[MAX];

  for (int i = 0; i < MAX; i++)
    in[i] = 8765.987 - 8.6756 * i;

  compute_rsqrt_ref (in, ref);
  compute_rsqrt_exp (in, exp);

#pragma GCC novector
  for (int i = 0; i < MAX; i++)
    {
      float rel_err = (ref[i] - exp[i]) / ref[i];
      rel_err = rel_err > 0.0 ? rel_err : -rel_err;
      if (rel_err > EPS)
	abort ();
    }
}
