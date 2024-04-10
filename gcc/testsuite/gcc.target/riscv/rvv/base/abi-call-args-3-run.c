/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O1" } */
/* { dg-additional-sources abi-call-args-3.c } */

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "riscv_vector.h"

#define INIT_DATA                                                              \
  size_t vlmax_e8m8 = __riscv_vsetvlmax_e8m8 ();                               \
  int8_t golden_data[vlmax_e8m8];                                              \
  memset (golden_data, 0, vlmax_e8m8 * sizeof (int8_t));                       \
  int8_t test_data[vlmax_e8m8];                                                \
  memset (test_data, 0, vlmax_e8m8 * sizeof (int8_t));                         \
  for (size_t i = 0; i < vlmax_e8m8; i += 1)                                   \
    golden_data[i] = vlmax_e8m8 - 1;                                           \
  int8_t dummy_data[vlmax_e8m8];                                               \
  for (size_t i = 0; i < vlmax_e8m8; i += 1)                                   \
    dummy_data[i] = -1;

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

void
foo1 (vbool1_t a, vbool2_t b, vbool4_t c, vbool2_t *out_b);
void
check_foo1 ()
{
  INIT_DATA

  size_t vlmax_e8mf8 = __riscv_vsetvlmax_e8mf8 ();
  vbool1_t a = *(vbool1_t *) dummy_data;
  vbool2_t b = *(vbool2_t *) golden_data;
  vbool4_t c = *(vbool4_t *) dummy_data;
  foo1 (a, b, c, (vbool2_t *) test_data);
  if (!check_mask (test_data, golden_data, vlmax_e8mf8 * 32))
    abort ();
}

void
foo2 (vbool1_t a, vbool2_t b, vbool4_t c, vbool8_t d, vbool16_t e, vbool32_t f,
      vbool64_t g, vbool64_t *out_g);
void
check_foo2 ()
{
  INIT_DATA

  size_t vlmax_e8mf8 = __riscv_vsetvlmax_e8mf8 ();
  vbool1_t a = *(vbool1_t *) dummy_data;
  vbool2_t b = *(vbool2_t *) dummy_data;
  vbool4_t c = *(vbool4_t *) dummy_data;
  vbool8_t d = *(vbool8_t *) dummy_data;
  vbool16_t e = *(vbool16_t *) dummy_data;
  vbool32_t f = *(vbool32_t *) dummy_data;
  vbool64_t g = *(vbool64_t *) golden_data;
  foo2 (a, b, c, d, e, f, g, (vbool64_t *) test_data);
  if (!check_mask (test_data, golden_data, vlmax_e8mf8))
    abort ();
}

void
foo3 (vbool1_t a, vint8m4_t b, vbool2_t c, vbool2_t *out_c);
void
check_foo3 ()
{
  INIT_DATA

  size_t vlmax_e8mf8 = __riscv_vsetvlmax_e8mf8 ();
  vbool1_t a = *(vbool1_t *) dummy_data;
  vint8m4_t b = *(vint8m4_t *) dummy_data;
  vbool2_t c = *(vbool2_t *) golden_data;
  foo3 (a, b, c, (vbool2_t *) test_data);
  if (!check_mask (test_data, golden_data, vlmax_e8mf8 * 32))
    abort ();
}

void
foo4 (vbool1_t a, vint8m4_t b, vbool2_t c, vint8m8_t d, vint8m8_t *out_d);
void
check_foo4 ()
{
  INIT_DATA

  size_t vlmax_e8mf8 = __riscv_vsetvlmax_e8mf8 ();
  vbool1_t a = *(vbool1_t *) dummy_data;
  vint8m4_t b = *(vint8m4_t *) dummy_data;
  vbool2_t c = *(vbool2_t *) dummy_data;
  vint8m8_t d = *(vint8m8_t *) golden_data;
  foo4 (a, b, c, d, (vint8m8_t *) test_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 64))
    abort ();
}

void
foo5 (vbool1_t a, vint8m8_t b, vint8m8_t c, vint8m4_t d, vint8m4_t *out_d);
void
check_foo5 ()
{
  INIT_DATA

  size_t vlmax_e8mf8 = __riscv_vsetvlmax_e8mf8 ();
  vbool1_t a = *(vbool1_t *) dummy_data;
  vint8m8_t b = *(vint8m8_t *) dummy_data;
  vint8m8_t c = *(vint8m8_t *) dummy_data;
  vint8m4_t d = *(vint8m4_t *) golden_data;
  foo5 (a, b, c, d, (vint8m4_t *) test_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 32))
    abort ();
}

void
foo6 (vint8m1_t a, vint8m8_t b, vint8m4_t c, vint8m2_t d, vint8m1_t e,
      vint8m1_t *out_a, vint8m8_t *out_b, vint8m4_t *out_c, vint8m2_t *out_d,
      vint8m1_t *out_e);
void
check_foo6 ()
{
  INIT_DATA

  size_t vlmax_e8mf8 = __riscv_vsetvlmax_e8mf8 ();
  vint8m1_t a = *(vint8m1_t *) golden_data;
  vint8m8_t b = *(vint8m8_t *) golden_data;
  vint8m4_t c = *(vint8m4_t *) golden_data;
  vint8m2_t d = *(vint8m2_t *) golden_data;
  vint8m1_t e = *(vint8m1_t *) golden_data;
  foo6 (a, b, c, d, e, (vint8m1_t *) test_data, (vint8m8_t *) dummy_data,
	(vint8m4_t *) dummy_data, (vint8m2_t *) dummy_data,
	(vint8m1_t *) dummy_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 8))
    abort ();

  foo6 (a, b, c, d, e, (vint8m1_t *) dummy_data, (vint8m8_t *) test_data,
	(vint8m4_t *) dummy_data, (vint8m2_t *) dummy_data,
	(vint8m1_t *) dummy_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 64))
    abort ();

  foo6 (a, b, c, d, e, (vint8m1_t *) dummy_data, (vint8m8_t *) dummy_data,
	(vint8m4_t *) test_data, (vint8m2_t *) dummy_data,
	(vint8m1_t *) dummy_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 32))
    abort ();

  foo6 (a, b, c, d, e, (vint8m1_t *) dummy_data, (vint8m8_t *) dummy_data,
	(vint8m4_t *) dummy_data, (vint8m2_t *) test_data,
	(vint8m1_t *) dummy_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 16))
    abort ();

  foo6 (a, b, c, d, e, (vint8m1_t *) dummy_data, (vint8m8_t *) dummy_data,
	(vint8m4_t *) dummy_data, (vint8m2_t *) dummy_data,
	(vint8m1_t *) test_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 8))
    abort ();
}

void
foo7 (vint8m1_t a1, vint8m1_t a2, vint8m1_t a3, vint8m1_t a4, vint8m1_t a5,
      vint8m1_t a6, vint8m1_t a7, vint8m1_t a8, vint8m1_t a9, vint8m1_t a10,
      vint8m1_t a11, vint8m1_t a12, vint8m1_t a13, vint8m1_t a14, vint8m1_t a15,
      vint8m1_t a16, vint8m1_t a17, vint8m1_t *out_a17);
void
check_foo7 ()
{
  INIT_DATA

  size_t vlmax_e8mf8 = __riscv_vsetvlmax_e8mf8 ();
  vint8m1_t a1 = *(vint8m1_t *) dummy_data;
  vint8m1_t a2 = *(vint8m1_t *) dummy_data;
  vint8m1_t a3 = *(vint8m1_t *) dummy_data;
  vint8m1_t a4 = *(vint8m1_t *) dummy_data;
  vint8m1_t a5 = *(vint8m1_t *) dummy_data;
  vint8m1_t a6 = *(vint8m1_t *) dummy_data;
  vint8m1_t a7 = *(vint8m1_t *) dummy_data;
  vint8m1_t a8 = *(vint8m1_t *) dummy_data;
  vint8m1_t a9 = *(vint8m1_t *) dummy_data;
  vint8m1_t a10 = *(vint8m1_t *) dummy_data;
  vint8m1_t a11 = *(vint8m1_t *) dummy_data;
  vint8m1_t a12 = *(vint8m1_t *) dummy_data;
  vint8m1_t a13 = *(vint8m1_t *) dummy_data;
  vint8m1_t a14 = *(vint8m1_t *) dummy_data;
  vint8m1_t a15 = *(vint8m1_t *) dummy_data;
  vint8m1_t a16 = *(vint8m1_t *) dummy_data;
  vint8m1_t a17 = *(vint8m1_t *) golden_data;
  foo7 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16,
	a17, (vint8m1_t *) test_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 8))
    abort ();
}

void
foo8 (vint8m8_t a1, vint8m8_t a2, vint8m8_t a3,
      vint8m8_t *out_a3);
void
check_foo8 ()
{
  INIT_DATA

  size_t vlmax_e8mf8 = __riscv_vsetvlmax_e8mf8 ();
  vint8m8_t a1 = *(vint8m8_t *) dummy_data;
  vint8m8_t a2 = *(vint8m8_t *) dummy_data;
  vint8m8_t a3 = *(vint8m8_t *) golden_data;

  foo8 (a1, a2, a3, (vint8m8_t *) test_data);
  if (!check_data (test_data, golden_data, vlmax_e8mf8 * 64))
    abort ();
}

int
main ()
{
  check_foo1 ();
  check_foo2 ();
  check_foo3 ();
  check_foo4 ();
  check_foo5 ();
  check_foo6 ();
  check_foo7 ();
  check_foo8 ();
  return 0;
}
