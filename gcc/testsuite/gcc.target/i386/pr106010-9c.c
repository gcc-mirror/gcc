/* { dg-do run } */
/* { dg-options "-O3 -mavx2 -fvect-cost-model=unlimited" } */
/* { dg-require-effective-target avx2 } */

#include <string.h>
#include "avx2-check.h"
#include "pr106010-9a.c"

static void
avx2_test (void)
{
  _Complex double* pd_src = (_Complex double*) malloc (N * sizeof (_Complex double));
  _Complex double* pd_dst = (_Complex double*) malloc (N * sizeof (_Complex double));
  _Complex double* pd_src2 = (_Complex double*) malloc (N * sizeof (_Complex double));
  _Complex double* pd_dst2 = (_Complex double*) malloc (N * sizeof (_Complex double));
  cdf* cdf_src = (cdf*) malloc (N * sizeof (cdf));
  cdf* cdf_dst = (cdf*) malloc (N * sizeof (cdf));
  cdf2* cdf2_src = (cdf2*) malloc (N * sizeof (cdf2));
  cdf2* cdf2_dst = (cdf2*) malloc (N * sizeof (cdf2));
  cdf3* cdf3_src = (cdf3*) malloc (N * sizeof (cdf3));
  cdf3* cdf3_dst = (cdf3*) malloc (N * sizeof (cdf3));
  cdf4* cdf4_src = (cdf4*) malloc (N * sizeof (cdf4));
  cdf4* cdf4_dst = (cdf4*) malloc (N * sizeof (cdf4));
  
  char* p_init = (char*) malloc (N * sizeof (cdf3));

  __builtin_memset (cdf_dst, 0, N * sizeof (cdf));
  __builtin_memset (cdf2_dst, 0, N * sizeof (cdf2));
  __builtin_memset (cdf3_dst, 0, N * sizeof (cdf3));
  __builtin_memset (cdf4_dst, 0, N * sizeof (cdf4));
  __builtin_memset (pd_dst, 0, N * sizeof (_Complex double));
  __builtin_memset (pd_dst2, 0, N * sizeof (_Complex double));

  for (int i = 0; i != N * sizeof (cdf3); i++)
    p_init[i] = i;

  memcpy (cdf_src, p_init, N * sizeof (cdf));
  memcpy (cdf2_src, p_init, N * sizeof (cdf2));
  memcpy (cdf3_src, p_init, N * sizeof (cdf3));
  memcpy (cdf4_src, p_init, N * sizeof (cdf4));
  memcpy (pd_src, p_init, N * sizeof (_Complex double));
  for (int i = 0; i != 2 * N * sizeof (double); i++)
    p_init[i] = i % 16;
  memcpy (pd_src2, p_init, N * sizeof (_Complex double));

  foo (cdf_dst, cdf_src);
  foo1 (cdf2_dst, cdf2_src);
  foo2 (cdf3_dst, cdf3_src);
  foo3 (cdf4_dst, cdf4_src);
  foo4 (pd_dst, pd_src);
  foo5 (pd_dst2, pd_src2);
  for (int i = 0; i != N; i++)
    {
      p_init[(N - i - 1) * 16] = i * 16;
      p_init[(N - i - 1) * 16 + 1] = i * 16 + 1;
      p_init[(N - i - 1) * 16 + 2] = i * 16 + 2;
      p_init[(N - i - 1) * 16 + 3] = i * 16 + 3;
      p_init[(N - i - 1) * 16 + 4] = i * 16 + 4;
      p_init[(N - i - 1) * 16 + 5] = i * 16 + 5;
      p_init[(N - i - 1) * 16 + 6] = i * 16 + 6;
      p_init[(N - i - 1) * 16 + 7] = i * 16 + 7;
      p_init[(N - i - 1) * 16 + 8] = i * 16 + 8;
      p_init[(N - i - 1) * 16 + 9] = i * 16 + 9;
      p_init[(N - i - 1) * 16 + 10] = i * 16 + 10;
      p_init[(N - i - 1) * 16 + 11] = i * 16 + 11;
      p_init[(N - i - 1) * 16 + 12] = i * 16 + 12;
      p_init[(N - i - 1) * 16 + 13] = i * 16 + 13;
      p_init[(N - i - 1) * 16 + 14] = i * 16 + 14;
      p_init[(N - i - 1) * 16 + 15] = i * 16 + 15;
    }
  memcpy (pd_src, p_init, N * 16);
 
  if (__builtin_memcmp (pd_dst, pd_src, N * 2 * sizeof (double)) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (pd_dst2, pd_src2, N * 2 * sizeof (double)) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (cdf_dst, cdf_src, N * sizeof (cdf)) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (cdf2_dst, cdf2_src, N * sizeof (cdf2)) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (cdf3_dst, cdf3_src, N * sizeof (cdf3)) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (cdf4_dst, cdf4_src, N * sizeof (cdf4)) != 0)
    __builtin_abort ();
}
