/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O3 -mavx512fp16 -mavx512vl -ffast-math" } */

static void recip_op_test (void);
#define DO_TEST recip_op_test
#define AVX512FP16
#define AVX512VL
#include "avx512f-check.h"
#include "avx512fp16-recip-1.c"

_Float16 a[32], b[32], vexp[32], vref[32], sa, sb, sexp, sref;

#define NO_FAST_ATTR  \
  __attribute__((noinline, noclone, \
		 optimize("fast-math,trapping-math")))

_Float16 NO_FAST_ATTR
scalar_hf_rcp_no_fast (_Float16 a, _Float16 b)
{
  return a / b;
}

_Float16 NO_FAST_ATTR
scalar_hf_rsqrt_no_fast (_Float16 a, _Float16 b)
{
  return a / __builtin_sqrtf16 (b);
}

void NO_FAST_ATTR
vector_hf_rcp_no_fast (_Float16 * restrict a, _Float16 * restrict b,
		    _Float16 * restrict c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    c[i] = a[i] / b[i];
}

void NO_FAST_ATTR
vector_hf_rsqrt_no_fast (_Float16 * restrict a, _Float16 * restrict b,
		    _Float16 * restrict c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    c[i] = a[i] / __builtin_sqrtf16 (b[i]);
}

void init()
{
  int i;
  sa = 3.75;
  sb = 6.25;
  sexp = sref = 2.75;
  for (i = 0; i < 32; i++)
    {
      a[i] = i + 0.5; 
      b[i] = i * 1.5;
      vexp[i] = vref[i] = 2.75 * i;
    }
}

int check_cond(void *a, void *b, int size)
{
  int i;
  unsigned short *pa = (unsigned short *)a,
		 *pb = (unsigned short *)b;
  for (i = 0; i < size; i++)
    if (pa[i] != pb[i])
      return 0;
  return 1;
}

static void recip_op_test()
{
  init ();
  sexp = scalar_hf_rcp_fast (sa, sb);
  sref = scalar_hf_rcp_no_fast (sa, sb);
  if (!check_cond (&sexp, &sref, 1))
    abort ();

  init ();
  sexp = scalar_hf_rsqrt_fast (sa, sb);
  sref = scalar_hf_rsqrt_no_fast (sa, sb);
  if (!check_cond (&sexp, &sref, 1))
    abort ();

  init ();
  vector_hf_rcp_fast (a, b, vexp, 32);
  vector_hf_rcp_no_fast (a, b, vref, 32);
  if (!check_cond (vexp, vref, 1))
    abort ();

  init ();
  vector_hf_rsqrt_fast (a, b, vexp, 32);
  vector_hf_rsqrt_no_fast (a, b, vref, 32);
  if (!check_cond (vexp, vref, 1))
    abort ();
}
