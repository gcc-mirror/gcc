/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O1" } */
/* { dg-additional-sources abi-call-args-4.c } */

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint-gcc.h>
#include "riscv_vector.h"

vint64m8_t
foo1 (vint8m1_t a1, vint8m1_t a2, vint16m2_t b1, vint32m4_t c1, vint64m8_t d1,
      size_t vl);
vint64m8_t
foo2 (vint8m1_t a1, vint16m2_t b1, vint8m1_t a2, vint32m4_t c1, vint64m8_t d1,
      size_t vl);
vint64m8_t
foo3 (vint8m1_t a1, vint16m2_t b1, vint32m4_t c1, vint8m1_t a2, vint64m8_t d1,
      size_t vl);
vint64m8_t
foo4 (vint8m1_t a1, vint16m2_t b1, vint32m4_t c1, vint64m8_t d1, vint8m1_t a2,
      size_t vl);

vint8m1x8_t
foo5 (vint8m8_t a1, vint8m1x8_t a2);

int
main ()
{
  size_t vlmax_e8m1 = __riscv_vsetvlmax_e8m1 ();
  int8_t a1[vlmax_e8m1], a2[vlmax_e8m1];
  int16_t b1[vlmax_e8m1], b2[vlmax_e8m1];
  int32_t c1[vlmax_e8m1], c2[vlmax_e8m1];
  int64_t d1[vlmax_e8m1], d2[vlmax_e8m1];
  memset (a1, 0, vlmax_e8m1 * sizeof (int8_t));
  memset (a2, 0, vlmax_e8m1 * sizeof (int8_t));
  memset (b1, 0, vlmax_e8m1 * sizeof (int16_t));
  memset (b2, 0, vlmax_e8m1 * sizeof (int16_t));
  memset (c1, 0, vlmax_e8m1 * sizeof (int32_t));
  memset (c2, 0, vlmax_e8m1 * sizeof (int32_t));
  memset (d1, 0, vlmax_e8m1 * sizeof (int64_t));
  memset (d2, 0, vlmax_e8m1 * sizeof (int64_t));

  for (size_t i = 0; i < vlmax_e8m1; i++)
    {
      a1[i] = 67 * i;
      a2[i] = 83 * i;
      b1[i] = 132 * i;
      c1[i] = 1928 * i;
      d1[i] = 23495 * i;
    }

  for (size_t i = 0; i < vlmax_e8m1; i++)
    {
      b2[i] = a1[i] + a2[i];
    }
  for (size_t i = 0; i < vlmax_e8m1; i++)
    {
      c2[i] = b1[i] - b2[i];
    }

  for (size_t i = 0; i < vlmax_e8m1; i++)
    {
      d2[i] = c1[i] * c2[i];
      d2[i] = d2[i] & d1[i];
    }
  int64_t golden = 0;
  for (size_t i = 0; i < vlmax_e8m1; i++)
    {
      golden += d2[i];
    }

  int64_t test;

  vint64m8_t res1
    = foo1 (*(vint8m1_t *) a1, *(vint8m1_t *) a2, *(vint16m2_t *) b1,
	    *(vint32m4_t *) c1, *(vint64m8_t *) d1, vlmax_e8m1);
  test = __riscv_vmv_x_s_i64m1_i64 (
    __riscv_vredsum_vs_i64m8_i64m1 (res1, __riscv_vmv_v_x_i64m1 (0, vlmax_e8m1),
				    vlmax_e8m1));

  if (test != golden)
    {
      printf ("foo1: %ld, %ld\n", test, golden);
      abort ();
    }

  vint64m8_t res2
    = foo2 (*(vint8m1_t *) a1, *(vint16m2_t *) b1, *(vint8m1_t *) a2,
	    *(vint32m4_t *) c1, *(vint64m8_t *) d1, vlmax_e8m1);
  test = __riscv_vmv_x_s_i64m1_i64 (
    __riscv_vredsum_vs_i64m8_i64m1 (res2, __riscv_vmv_v_x_i64m1 (0, vlmax_e8m1),
				    vlmax_e8m1));

  if (test != golden)
    {
      printf ("foo2: %ld, %ld\n", test, golden);
      abort ();
    }

  vint64m8_t res3
    = foo3 (*(vint8m1_t *) a1, *(vint16m2_t *) b1, *(vint32m4_t *) c1,
	    *(vint8m1_t *) a2, *(vint64m8_t *) d1, vlmax_e8m1);
  test = __riscv_vmv_x_s_i64m1_i64 (
    __riscv_vredsum_vs_i64m8_i64m1 (res3, __riscv_vmv_v_x_i64m1 (0, vlmax_e8m1),
				    vlmax_e8m1));
  if (test != golden)
    {
      printf ("foo3: %ld, %ld\n", test, golden);
      abort ();
    }

  vint64m8_t res4
    = foo4 (*(vint8m1_t *) a1, *(vint16m2_t *) b1, *(vint32m4_t *) c1,
	    *(vint64m8_t *) d1, *(vint8m1_t *) a2, vlmax_e8m1);
  test = __riscv_vmv_x_s_i64m1_i64 (
    __riscv_vredsum_vs_i64m8_i64m1 (res4, __riscv_vmv_v_x_i64m1 (0, vlmax_e8m1),
				    vlmax_e8m1));
  if (test != golden)
    {
      printf ("foo4: %ld, %ld\n", test, golden);
      abort ();
    }

  int8_t t1[vlmax_e8m1 * 8];
  int8_t t2[vlmax_e8m1 * 8];
  for (size_t i = 0; i < vlmax_e8m1 * 8; i++)
    {
      t1[i] = 67 * i;
      t2[i] = 83 * i;
    }
  vint8m1x8_t res5 = foo5 (*(vint8m8_t *) t1, *(vint8m1x8_t *) t2);
  int8_t test_arr[vlmax_e8m1 * 8];
  memset (test_arr, 0, vlmax_e8m1 * 8 * sizeof (int8_t));
  *(vint8m1x8_t *) test_arr = res5;
  for (size_t i = 0; i < vlmax_e8m1 * 8; i += 1)
    if (t2[i] != test_arr[i])
      {
	printf ("foo5 %d: %ld, %ld\n", i, test_arr[i], t2[i]);
	abort ();
      }

  return 0;
}
