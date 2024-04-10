/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O1" } */
/* { dg-additional-sources abi-call-args-1.c } */

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "riscv_vector.h"

#define FOO_(TYPE) void foo_##TYPE (TYPE val, TYPE *out);

FOO_ (vbool1_t)
FOO_ (vbool2_t)
FOO_ (vbool4_t)
FOO_ (vbool8_t)
FOO_ (vbool16_t)
FOO_ (vbool32_t)
FOO_ (vbool64_t)
FOO_ (vint8mf8_t)
FOO_ (vint8mf4_t)
FOO_ (vint8mf2_t)
FOO_ (vint8m1_t)
FOO_ (vint8m2_t)
FOO_ (vint8m4_t)
FOO_ (vint8m8_t)
FOO_ (vint8m1x5_t)
FOO_ (vint8m1x8_t)
FOO_ (vint8m2x3_t)
FOO_ (vint8m2x4_t)
FOO_ (vint8m4x2_t)

bool
check_mask (int8_t *test_data, int8_t *golden_data, size_t vl)
{
  size_t i = 0;
  for (; i + 8 <= vl; i += 8)
    {
      if (test_data[i / 8] != golden_data[i / 8])
	{
	  printf ("mask diff %lu: %d, %d\n", i / 8, test_data[i / 8],
		  golden_data[i / 8]);
	  return false;
	}
    }
  if (vl % 8 != 0)
    {
      if ((test_data[i / 8] << (8 - (vl % 8)))
	  != (golden_data[i / 8] << (8 - (vl % 8))))
	{
	  printf ("mask tail diff %lu, tail %d: %d, %d\n", i / 8, vl % 8,
		  test_data[i / 8], golden_data[i / 8]);
	  return false;
	}
    }
  return true;
}

bool
check_data (int8_t *test_data, int8_t *golden_data, size_t vl)
{
  for (size_t i = 0; i < vl; i += 1)
    {
      if (test_data[i] != golden_data[i])
	{
	  printf ("data diff %lu: %d, %d\n", i, test_data[i], golden_data[i]);
	  return false;
	}
    }
  return true;
}

#define INIT_DATA                                                              \
  size_t vlmax_e8m8 = __riscv_vsetvlmax_e8m8 ();                               \
  int8_t golden_data[vlmax_e8m8];                                              \
  memset (golden_data, 0, vlmax_e8m8 * sizeof (int8_t));                       \
  int8_t test_data[vlmax_e8m8];                                                \
  memset (test_data, 0, vlmax_e8m8 * sizeof (int8_t));                         \
  for (size_t i = 0; i < vlmax_e8m8; i += 1)                                   \
    golden_data[i] = vlmax_e8m8 - 1;

#define FOO_MASK(TYPE, VL)                                                     \
  {                                                                            \
    INIT_DATA                                                                  \
    for (size_t i = 0; i < vlmax_e8m8; i += 1)                                 \
      test_data[i] = 0;                                                        \
    TYPE val = *(TYPE *) golden_data;                                          \
    foo_##TYPE (val, (TYPE *) test_data);                                      \
    if (!check_mask (test_data, golden_data, VL))                              \
      abort ();                                                                \
  }

#define FOO_DATA(TYPE, VL)                                                     \
  {                                                                            \
    INIT_DATA                                                                  \
    for (size_t i = 0; i < vlmax_e8m8; i += 1)                                 \
      test_data[i] = 0;                                                        \
    TYPE val = *(TYPE *) golden_data;                                          \
    foo_##TYPE (val, (TYPE *) test_data);                                      \
    if (!check_data (test_data, golden_data, VL))                              \
      abort ();                                                                \
  }

int
main ()
{
  size_t vlmax = __riscv_vsetvlmax_e8mf8 ();
  FOO_MASK (vbool1_t, vlmax * 64)
  FOO_MASK (vbool2_t, vlmax * 32)
  FOO_MASK (vbool4_t, vlmax * 16)
  FOO_MASK (vbool8_t, vlmax * 8)
  FOO_MASK (vbool16_t, vlmax * 4)
  FOO_MASK (vbool32_t, vlmax * 2)
  FOO_MASK (vbool64_t, vlmax)
  FOO_DATA (vint8mf8_t, vlmax)
  FOO_DATA (vint8mf4_t, vlmax * 2)
  FOO_DATA (vint8mf2_t, vlmax * 4)
  FOO_DATA (vint8m1_t, vlmax * 8)
  FOO_DATA (vint8m2_t, vlmax * 16)
  FOO_DATA (vint8m4_t, vlmax * 32)
  FOO_DATA (vint8m8_t, vlmax * 64)
  FOO_DATA (vint8m1x5_t, vlmax * 8 * 5)
  FOO_DATA (vint8m1x8_t, vlmax * 8 * 8)
  FOO_DATA (vint8m2x3_t, vlmax * 16 * 3)
  FOO_DATA (vint8m2x4_t, vlmax * 16 * 4)
  FOO_DATA (vint8m4x2_t, vlmax * 32 * 2)
}
