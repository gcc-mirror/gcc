/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O1 -mavx512fp16 -mavx512vl -fdump-tree-slp-details -fdump-tree-optimized" } */

extern void abort ();

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

#define N 16
_Float16 b[N] = {-1.2f, 3.4f, -5.6f, 7.8f,
		 -9.0f, 1.0f, -2.0f, 3.0f,
		 -4.0f, -5.0f, 6.0f, 7.0f,
		 -8.0f, -9.0f, 10.0f, 11.0f};
_Float16 r[N];

void
__attribute__((noipa,noinline,optimize("Ofast")))
round_32 (void)
{
  r[0] = __builtin_roundf16 (b[0]);
  r[1] = __builtin_roundf16 (b[1]);
}

void
__attribute__((noipa,noinline,optimize("Ofast")))
round_64 (void)
{
  r[0] =  __builtin_roundf16 (b[0]);
  r[1] =  __builtin_roundf16 (b[1]);
  r[2] =  __builtin_roundf16 (b[2]);
  r[3] =  __builtin_roundf16 (b[3]);
}

void
__attribute__((noipa,noinline,optimize("O2")))
rint_32 (void)
{
  r[0] = __builtin_rintf16 (b[0]);
  r[1] = __builtin_rintf16 (b[1]);
}

void
__attribute__((noipa,noinline,optimize("O2")))
rint_64 (void)
{
  r[0] =  __builtin_rintf16 (b[0]);
  r[1] =  __builtin_rintf16 (b[1]);
  r[2] =  __builtin_rintf16 (b[2]);
  r[3] =  __builtin_rintf16 (b[3]);
}

void
__attribute__((noipa,noinline,optimize("O2")))
nearbyint_32 (void)
{
  r[0] = __builtin_nearbyintf16 (b[0]);
  r[1] = __builtin_nearbyintf16 (b[1]);
}

void
__attribute__((noipa,noinline,optimize("O2")))
nearbyint_64 (void)
{
  r[0] =  __builtin_nearbyintf16 (b[0]);
  r[1] =  __builtin_nearbyintf16 (b[1]);
  r[2] =  __builtin_nearbyintf16 (b[2]);
  r[3] =  __builtin_nearbyintf16 (b[3]);
}

void
__attribute__((noipa,noinline,optimize("Ofast")))
trunc_32 (void)
{
  r[0] = __builtin_truncf16 (b[0]);
  r[1] = __builtin_truncf16 (b[1]);
}

void
__attribute__((noipa,noinline,optimize("Ofast")))
trunc_64 (void)
{
  r[0] =  __builtin_truncf16 (b[0]);
  r[1] =  __builtin_truncf16 (b[1]);
  r[2] =  __builtin_truncf16 (b[2]);
  r[3] =  __builtin_truncf16 (b[3]);
}

void
__attribute__((noipa,noinline,optimize("Ofast")))
floor_32 (void)
{
  r[0] =  __builtin_floorf16 (b[0]);
  r[1] =  __builtin_floorf16 (b[1]);
}

void
__attribute__((noipa,noinline,optimize("Ofast")))
floor_64 (void)
{
  r[0] =  __builtin_floorf16 (b[0]);
  r[1] =  __builtin_floorf16 (b[1]);
  r[2] =  __builtin_floorf16 (b[2]);
  r[3] =  __builtin_floorf16 (b[3]);
}

void
__attribute__((noipa,noinline,optimize("Ofast")))
ceil_32 (void)
{
  r[0] =  __builtin_ceilf16 (b[0]);
  r[1] =  __builtin_ceilf16 (b[1]);
}

void
__attribute__((noipa,noinline,optimize("Ofast")))
ceil_64 (void)
{
  r[0] =  __builtin_ceilf16 (b[0]);
  r[1] =  __builtin_ceilf16 (b[1]);
  r[2] =  __builtin_ceilf16 (b[2]);
  r[3] =  __builtin_ceilf16 (b[3]);
}

_Float16
__attribute__((noipa,noinline,optimize("Ofast")))
dummy_roundf16 (_Float16 a)
{
  return __builtin_roundf16 (a);
}
static void
__attribute__ ((noinline, noclone))
do_test (void)
{
  round_32 ();
  /* check results:  */
  for (int i = 0; i != 2; i++)
    if (r[i] != dummy_roundf16 (b[i]))
      abort ();

  round_64 ();
  /* check results:  */
  for (int i = 0; i != 4; i++)
    if (r[i] != dummy_roundf16 (b[i]))
      abort ();

  rint_32 ();
  /* check results:  */
  for (int i = 0; i != 2; i++)
    if (r[i] != __builtin_rintf16 (b[i]))
      abort ();

  rint_64 ();
  /* check results:  */
  for (int i = 0; i != 4; i++)
    if (r[i] != __builtin_rintf16 (b[i]))
      abort ();

  nearbyint_32 ();
  /* check results:  */
  for (int i = 0; i != 2; i++)
    if (r[i] != __builtin_nearbyintf16 (b[i]))
      abort ();

  nearbyint_64 ();
  /* check results:  */
  for (int i = 0; i != 4; i++)
    if (r[i] != __builtin_nearbyintf16 (b[i]))
      abort ();

  trunc_32 ();
  /* check results:  */
  for (int i = 0; i != 2; i++)
    if (r[i] != __builtin_truncf16 (b[i]))
      abort ();

  trunc_64 ();
  /* check results:  */
  for (int i = 0; i != 4; i++)
    if (r[i] != __builtin_truncf16 (b[i]))
      abort ();

  floor_32 ();
  /* check results:  */
  for (int i = 0; i != 2; i++)
    if (r[i] != __builtin_floorf16 (b[i]))
      abort ();

  floor_64 ();
  /* check results:  */
  for (int i = 0; i != 4; i++)
    if (r[i] != __builtin_floorf16 (b[i]))
      abort ();

  ceil_32 ();
  /* check results:  */
  for (int i = 0; i != 2; i++)
    if (r[i] != __builtin_ceilf16 (b[i]))
      abort ();

  ceil_64 ();
  /* check results:  */
  for (int i = 0; i != 4; i++)
    if (r[i] != __builtin_ceilf16 (b[i]))
      abort ();
}

/* { dg-final { scan-tree-dump-times "vectorized using 8 byte vectors" 6 "slp2" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times "vectorized using 4 byte vectors" 6 "slp2" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times {(?n).CEIL \(vect} 2 "optimized" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times {(?n).FLOOR \(vect} 2 "optimized" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times {(?n).ROUND \(vect} 2 "optimized" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times {(?n).RINT \(vect} 2 "optimized" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times {(?n).NEARBYINT \(vect} 2 "optimized" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times {(?n).TRUNC \(vect} 2 "optimized" { target { ! ia32 } } } } */
