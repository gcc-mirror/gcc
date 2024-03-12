/* { dg-require-effective-target scalar_all_fma } */
/* { dg-additional-options "-fdump-tree-optimized -ffp-contract=fast" } */

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
	a[i] = b[i] < 10 ? mb * mc + md : 10.0;		\
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
	if (a[i] != (i % 17 < 10 ? fma : 10.0))		\
	  __builtin_abort ();				\
	asm volatile ("" ::: "memory");			\
      }							\
  }

#define FOR_EACH_INV(T) \
  T (0) T (1) T (2) T (3)

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

/* { dg-final { scan-tree-dump-times { = \.COND_FMA } 1 "optimized" { target vect_double_cond_arith } } } */
/* { dg-final { scan-tree-dump-times { = \.COND_FMS } 1 "optimized" { target vect_double_cond_arith } } } */
/* { dg-final { scan-tree-dump-times { = \.COND_FNMA } 1 "optimized" { target vect_double_cond_arith } } } */
/* { dg-final { scan-tree-dump-times { = \.COND_FNMS } 1 "optimized" { target vect_double_cond_arith } } } */
