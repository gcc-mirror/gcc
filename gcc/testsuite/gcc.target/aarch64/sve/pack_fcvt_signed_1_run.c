/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "pack_fcvt_signed_1.c"

#define ARRAY_SIZE 107

#define VAL1 ((i * 345.434) - (19 * 345.434))

int __attribute__ ((optimize (1)))
main (void)
{
  static int32_t array_dest[ARRAY_SIZE];
  double array_source[ARRAY_SIZE];

  for (int i = 0; i < ARRAY_SIZE; i++)
    {
      array_source[i] = VAL1;
      asm volatile ("" ::: "memory");
    }

  pack_int_double_plus_3 (array_dest, array_source, ARRAY_SIZE);
  for (int i = 0; i < ARRAY_SIZE; i++)
    if (array_dest[i] != (int32_t) VAL1 + 3)
      __builtin_abort ();

  return 0;
}
