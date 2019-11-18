/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#ifndef FILENAME
#define FILENAME "slp_7.c"
#endif

#include FILENAME

#define N (54 * 4)

#define HARNESS(TYPE)							\
  {									\
    TYPE a[N], b[4] = { 40, 22, 75, 19 };				\
    for (unsigned int i = 0; i < N; ++i)				\
      {									\
	a[i] = i * 2 + i % 5;						\
	asm volatile ("" ::: "memory");					\
      }									\
    vec_slp_##TYPE (a, b, N / 4);					\
    TYPE x0 = 40;							\
    TYPE x1 = 22;							\
    TYPE x2 = 75;							\
    TYPE x3 = 19;							\
    for (unsigned int i = 0; i < N; i += 4)				\
      {									\
	x0 += a[i];							\
	x1 += a[i + 1];							\
	x2 += a[i + 2];							\
	x3 += a[i + 3];							\
	asm volatile ("" ::: "memory");					\
      }									\
    /* _Float16 isn't precise enough for this.  */			\
    if ((TYPE) 0x1000 + 1 != (TYPE) 0x1000				\
	&& (x0 != b[0] || x1 != b[1] || x2 != b[2] || x3 != b[3]))	\
      __builtin_abort ();						\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (HARNESS)
}
