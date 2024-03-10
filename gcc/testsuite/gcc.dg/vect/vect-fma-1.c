/* { dg-require-effective-target scalar_all_fma } */

#include "tree-vect.h"

#define N (VECTOR_BITS * 11 / 64 + 3)

#define DEF(INV)					\
  void __attribute__ ((noipa))				\
  f_##INV (double *restrict a, double *restrict b,	\
	   double *restrict c, double *restrict d)	\
  {							\
    for (int i = 0; i < N; ++i)				\
      {							\
	double mb = (INV & 1 ? -b[i] : b[i]);		\
	double mc = c[i];				\
	double md = (INV & 2 ? -d[i] : d[i]);		\
	double fma = __builtin_fma (mb, mc, md);	\
	a[i] = (INV & 4 ? -fma : fma);			\
      }							\
  }

#define TEST(INV)					\
  {							\
    f_##INV (a, b, c, d);				\
    _Pragma("GCC novector")				\
    for (int i = 0; i < N; ++i)				\
      {							\
	double mb = (INV & 1 ? -b[i] : b[i]);		\
	double mc = c[i];				\
	double md = (INV & 2 ? -d[i] : d[i]);		\
	double fma = __builtin_fma (mb, mc, md);	\
	double expected = (INV & 4 ? -fma : fma);	\
	if (a[i] != expected)				\
	  __builtin_abort ();				\
	asm volatile ("" ::: "memory");			\
      }							\
  }

#define FOR_EACH_INV(T)	\
  T (0) T (1) T (2) T (3) T (4) T (5) T (6) T (7)

FOR_EACH_INV (DEF)

int
main (void)
{
  double a[N], b[N], c[N], d[N];
  for (int i = 0; i < N; ++i)
    {
      b[i] = i % 17;
      c[i] = i % 9 + 11;
      d[i] = i % 13 + 14;
      asm volatile ("" ::: "memory");
    }
  FOR_EACH_INV (TEST)
  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 8 "vect" { target vect_double } } } */
