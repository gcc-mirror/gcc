/* { dg-do compile { target int128 } } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O -fcaller-saves -mavx512vl -mno-avx512bw" } */

typedef int __attribute__((__vector_size__ (8))) T;
typedef signed char __attribute__((__vector_size__ (64))) U;
typedef int __attribute__((__vector_size__ (16))) V;
typedef long long __attribute__((__vector_size__ (8))) W;
typedef int __attribute__((__vector_size__ (64))) X;
typedef _Decimal128 __attribute__((__vector_size__ (64))) D;

D d;
T t;
U u;
V v;
W w;

void
foo (void)
{
  T t0 = t;
  T t1 = (T) __builtin_ia32_palignr (w, (W) { }, 0);
  U u1 = __builtin_shufflevector (u, u, 7, 6, 2, 3, 6, 4, 5, 2, 3, 8, 3, 2, 0,
				  4, 0, 6, 2, 2, 5, 3, 1, 0, 7, 5, 3, 3, 7, 6,
				  2, 0, 4, 5, 4, 1, 7, 7, 0, 6, 1, 9, 3, 0, 3,
				  5, 5, 0, 0, 2, 1, 5, 4, 8, 7,
				  2, 1, 1, 6, 4, 9, 9, 1, 5, 0, 2);
  V v1 = v;
  d += 0.;
  U u0 = u + u + u1 + (U) d;
  V v0 = ((X)u0)[0] + v + v;
  t = (T) (long) (__int128) v0 + t + t + t1;
}
