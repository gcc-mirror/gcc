/* { dg-do run { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */

#include "dlstp-int64x2.c"

int main ()
{
  int i;
  int64_t temp1[N];
  int64_t temp3[N];
  reset_data64  (temp1, temp3, N);
  test (temp1, temp3, 0);
  check_memcpy64 (temp1, temp3, 0);

  reset_data64  (temp1, temp3, N);
  test (temp1, temp3, 1);
  check_memcpy64 (temp1, temp3, 1);

  reset_data64  (temp1, temp3, N);
  test (temp1, temp3, 2);
  check_memcpy64 (temp1, temp3, 2);

  reset_data64  (temp1, temp3, N);
  test (temp1, temp3, 3);
  check_memcpy64 (temp1, temp3, 3);

  reset_data64  (temp1, temp3, N);
  test (temp1, temp3, 4);
  check_memcpy64 (temp1, temp3, 4);

  reset_data64  (temp1, temp3, N);
  test (temp1, temp3, 5);
  check_memcpy64 (temp1, temp3, 5);

  reset_data64  (temp1, temp3, N);
  test (temp1, temp3, 6);
  check_memcpy64 (temp1, temp3, 6);

  reset_data64  (temp1, temp3, N);
  test (temp1, temp3, 7);
  check_memcpy64 (temp1, temp3, 7);

  reset_data64  (temp1, temp3, N);
}

