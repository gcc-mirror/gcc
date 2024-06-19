/* { dg-do run { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */

#include "dlstp-int32x4.c"

int main ()
{
  int i;
  int32_t temp1[N];
  int32_t temp2[N];
  int32_t temp3[N];
  reset_data32 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 0);
  check_plus32 (temp1, temp2, temp3, 0);

  reset_data32 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 1);
  check_plus32 (temp1, temp2, temp3, 1);

  reset_data32 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 3);
  check_plus32 (temp1, temp2, temp3, 3);

  reset_data32 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 4);
  check_plus32 (temp1, temp2, temp3, 4);

  reset_data32 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 5);
  check_plus32 (temp1, temp2, temp3, 5);

  reset_data32 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 8);
  check_plus32 (temp1, temp2, temp3, 8);

  reset_data32 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 9);
  check_plus32 (temp1, temp2, temp3, 9);

  reset_data32 (temp1, temp2, temp3, N);
}

