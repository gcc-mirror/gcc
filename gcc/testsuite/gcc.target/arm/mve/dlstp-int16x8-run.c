/* { dg-do run { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */
#include "dlstp-int16x8.c"

int main ()
{
  int i;
  int16_t temp1[N];
  int16_t temp2[N];
  int16_t temp3[N];
  reset_data16 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 0);
  check_plus16 (temp1, temp2, temp3, 0);

  reset_data16 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 1);
  check_plus16 (temp1, temp2, temp3, 1);

  reset_data16 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 7);
  check_plus16 (temp1, temp2, temp3, 7);

  reset_data16 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 8);
  check_plus16 (temp1, temp2, temp3, 8);

  reset_data16 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 9);
  check_plus16 (temp1, temp2, temp3, 9);

  reset_data16 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 16);
  check_plus16 (temp1, temp2, temp3, 16);

  reset_data16 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 17);
  check_plus16 (temp1, temp2, temp3, 17);

  reset_data16 (temp1, temp2, temp3, N);
}

