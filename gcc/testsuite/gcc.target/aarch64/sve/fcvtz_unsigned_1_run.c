/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "fcvtz_unsigned_1.c"

#define ARRAY_SIZE 75

#define VAL1 (i * 19)
#define VAL2 (i * 2574.33)
#define VAL3 ((double) (i * 0xff23efef))

int __attribute__ ((optimize (1)))
main (void)
{
  static uint16_t array_dest16[ARRAY_SIZE];
  static uint32_t array_dest32[ARRAY_SIZE];
  static uint64_t array_dest64[ARRAY_SIZE];

  _Float16 array_source16[ARRAY_SIZE];
  float array_source32[ARRAY_SIZE];
  double array_source64[ARRAY_SIZE];

  for (int i = 0; i < ARRAY_SIZE; i++)
    {
      array_source16[i] = VAL1;
      array_source32[i] = VAL2;
      array_source64[i] = VAL3;
      asm volatile ("" ::: "memory");
    }

  vfcvtz_16 (array_dest16, array_source16, ARRAY_SIZE);
  for (int i = 0; i < ARRAY_SIZE; i++)
    if (array_dest16[i] != (uint16_t) VAL1)
      __builtin_abort ();

  vfcvtz_32 (array_dest32, array_source32, ARRAY_SIZE);
  for (int i = 0; i < ARRAY_SIZE; i++)
    if (array_dest32[i] != (uint32_t) VAL2)
      __builtin_abort ();

  vfcvtz_64 (array_dest64, array_source64, ARRAY_SIZE);
  for (int i = 0; i < ARRAY_SIZE; i++)
    if (array_dest64[i] != (uint64_t) VAL3)
      __builtin_abort ();

  return 0;
}
