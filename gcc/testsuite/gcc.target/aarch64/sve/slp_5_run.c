/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include "slp_5.c"

#define N (141 * 2)

#define HARNESS(TYPE)					\
  {							\
    TYPE a[N], b[2] = { 40, 22 };			\
    for (unsigned int i = 0; i < N; ++i)		\
      {							\
	a[i] = i * 2 + i % 5;				\
	asm volatile ("" ::: "memory");			\
      }							\
    vec_slp_##TYPE (a, b, N / 2);			\
    TYPE x0 = 40;					\
    TYPE x1 = 22;					\
    for (unsigned int i = 0; i < N; i += 2)		\
      {							\
	x0 += a[i];					\
	x1 += a[i + 1];					\
	asm volatile ("" ::: "memory");			\
      }							\
    /* _Float16 isn't precise enough for this.  */	\
    if ((TYPE) 0x1000 + 1 != (TYPE) 0x1000		\
	&& (x0 != b[0] || x1 != b[1]))			\
      __builtin_abort ();				\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (HARNESS)
}
