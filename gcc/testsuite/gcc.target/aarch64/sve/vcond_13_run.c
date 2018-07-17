/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "vcond_13.c"

#define TEST_LOOP(INV, TYPE, CMPTYPE, SUFFIX)		\
  {							\
    TYPE a[N], b[N], c[N], d[N];			\
    CMPTYPE cond[N];					\
    for (int i = 0; i < N; ++i)				\
      {							\
	b[i] = i % 15;					\
	c[i] = i % 9 + 11;				\
	d[i] = i % 13 + 14;				\
	cond[i] = i % 17;				\
	asm volatile ("" ::: "memory");			\
      }							\
    f_##INV##_##SUFFIX (a, b, c, d, cond);		\
    for (int i = 0; i < N; ++i)				\
      {							\
	double mb = (INV & 1 ? -b[i] : b[i]);		\
	double mc = c[i];				\
	double md = (INV & 2 ? -d[i] : d[i]);		\
	double fma = __builtin_fma (mb, mc, md);	\
	double truev = (INV & 4 ? -fma : fma);		\
	if (a[i] != (i % 17 < 10 ? truev : b[i]))	\
	  __builtin_abort ();				\
	asm volatile ("" ::: "memory");			\
      }							\
  }

int
main (void)
{
  FOR_EACH_INV (TEST_LOOP)
  return 0;
}
