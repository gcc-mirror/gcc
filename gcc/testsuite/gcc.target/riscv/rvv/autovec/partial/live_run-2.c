/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable" } */

#include "live-2.c"

#define TEST_LOOP(TYPE, N)                                                     \
  {                                                                            \
    TYPE a##N[N];                                                              \
    TYPE b##N[N];                                                              \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	a##N[i] = i & 1;                                                       \
	b##N[i] = 0;                                                           \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
    TYPE expected##N = !(a##N[N - 1]);                                         \
    TYPE res##N = test_##TYPE (a##N, b##N, N);                                 \
    if (res##N != expected##N)                                                 \
      __builtin_abort ();                                                      \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	if (b##N[i] != !a##N[i])                                               \
	  __builtin_abort ();                                                  \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
  }

#define TEST_ALL_N(T, N)                                                       \
  T (int8_t, N)                                                                \
  T (int16_t, N)                                                               \
  T (int32_t, N)                                                               \
  T (int64_t, N)                                                               \
  T (uint8_t, N)                                                               \
  T (uint16_t, N)                                                              \
  T (uint32_t, N)                                                              \
  T (uint64_t, N)

int __attribute__ ((optimize (1))) main (void)
{
  TEST_ALL_N (TEST_LOOP, 2);
  TEST_ALL_N (TEST_LOOP, 3);
  TEST_ALL_N (TEST_LOOP, 4);
  TEST_ALL_N (TEST_LOOP, 5);
  TEST_ALL_N (TEST_LOOP, 6);
  TEST_ALL_N (TEST_LOOP, 7);
  TEST_ALL_N (TEST_LOOP, 8);
  TEST_ALL_N (TEST_LOOP, 17);
  TEST_ALL_N (TEST_LOOP, 64);
  TEST_ALL_N (TEST_LOOP, 107);
  TEST_ALL_N (TEST_LOOP, 255);
  TEST_ALL_N (TEST_LOOP, 256);
  TEST_ALL_N (TEST_LOOP, 4389);
  return 0;
}
