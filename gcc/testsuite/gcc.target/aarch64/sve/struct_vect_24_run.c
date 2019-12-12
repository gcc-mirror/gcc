/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-options "-O3 -fopenmp-simd -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */

#include "struct_vect_24.c"

#undef TEST_LOOP
#define TEST_LOOP(NAME, TYPE)				\
  {							\
    TYPE out[N];					\
    TYPE in[N * 4];					\
    for (int i = 0; i < N; ++i)				\
      {							\
	out[i] = i * 7 / 2;				\
	asm volatile ("" ::: "memory");			\
      }							\
    for (int i = 0; i < N * 4; ++i)			\
      {							\
	in[i] = i * 9 / 2;				\
	asm volatile ("" ::: "memory");			\
      }							\
    NAME (out, in);					\
    for (int i = 0; i < N; ++i)				\
      {							\
	TYPE expected = i * 7 / 2;			\
	if (out[i] != out[0] + expected)		\
	  __builtin_abort ();				\
	asm volatile ("" ::: "memory");			\
      }							\
  }

int __attribute__ ((optimize (0)))
main (void)
{
  TEST (test);
  return 0;
}
