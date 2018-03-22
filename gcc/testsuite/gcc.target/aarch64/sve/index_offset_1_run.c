/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256" { target aarch64_sve256_hw } } */

#include "index_offset_1.c"

#define TEST_INDEX_OFFSET(SIGNED, TYPE, ITERTYPE)		\
{								\
  SIGNED TYPE out[SIZE + 1];					\
  SIGNED TYPE in1[SIZE + 1];					\
  SIGNED TYPE in2[SIZE + 1];					\
  for (int i = 0; i < SIZE + 1; ++i)				\
    {								\
      in1[i] = (i * 4) ^ i;					\
      in2[i] = (i * 2) ^ i;					\
      asm volatile ("" ::: "memory");				\
    }								\
  out[SIZE] = 42;						\
  set_##SIGNED##_##TYPE##_##ITERTYPE (out, in1);		\
  if (0 != __builtin_memcmp (out, in1, SIZE * sizeof (TYPE)))	\
    __builtin_abort ();						\
  set_##SIGNED##_##TYPE##_##ITERTYPE##_var (out, in2, SIZE);	\
  if (0 != __builtin_memcmp (out, in2, SIZE * sizeof (TYPE)))	\
    __builtin_abort ();						\
  if (out[SIZE] != 42)						\
    __builtin_abort ();						\
}

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (TEST_INDEX_OFFSET);
  return 0;
}
