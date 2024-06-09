/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O1 -mavx512fp16 -mavx512vl -fdump-tree-slp-details -fdump-tree-optimized" } */

extern void abort ();

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

#define N 16
_Float16 a[N] = {-0.1f, -3.2f, -6.3f, -9.4f,
		 -12.5f, -15.6f, -18.7f, -21.8f,
		 24.9f, 27.1f, 30.2f, 33.3f,
		 36.4f, 39.5f, 42.6f, 45.7f};
_Float16 b[N] = {-1.2f, 3.4f, -5.6f, 7.8f,
		 -9.0f, 1.0f, -2.0f, 3.0f,
		 -4.0f, -5.0f, 6.0f, 7.0f,
		 -8.0f, -9.0f, 10.0f, 11.0f};
_Float16 r[N];

void
__attribute__((noipa,noinline,optimize("O2")))
copysign_32 (void)
{
  r[0] = __builtin_copysignf16 (1.0f, b[0]);
  r[1] = __builtin_copysignf16 (1.0f, b[1]);
}

void
__attribute__((noipa,noinline,optimize("O2")))
copysign_64 (void)
{
  r[0] =  __builtin_copysignf16 (1.0f, b[0]);
  r[1] =  __builtin_copysignf16 (1.0f, b[1]);
  r[2] =  __builtin_copysignf16 (1.0f, b[2]);
  r[3] =  __builtin_copysignf16 (1.0f, b[3]);
}

static void
__attribute__ ((noinline, noclone))
do_test (void)
{
  copysign_32 ();
  /* check results:  */
  for (int i = 0; i != 2; i++)
    if (r[i] != __builtin_copysignf16 (1.0f, b[i]))
      abort ();

  copysign_64 ();
  /* check results:  */
  for (int i = 0; i != 4; i++)
    if (r[i] != __builtin_copysignf16 (1.0f, b[i]))
      abort ();
}

/* { dg-final { scan-tree-dump-times "vectorized using 8 byte vectors" 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times "vectorized using 4 byte vectors" 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times ".COPYSIGN" 2 "optimized" { target { ! ia32 } } } } */
