/* { dg-do run { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_v8_1m_mve } */

#include "dlstp-int8x16.c"

int main ()
{
  int i;
  int8_t temp1[N];
  int8_t temp2[N];
  int8_t temp3[N];
  reset_data8 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 0);
  check_plus8 (temp1, temp2, temp3, 0);

  reset_data8 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 1);
  check_plus8 (temp1, temp2, temp3, 1);

  reset_data8 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 15);
  check_plus8 (temp1, temp2, temp3, 15);

  reset_data8 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 16);
  check_plus8 (temp1, temp2, temp3, 16);

  reset_data8 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 17);
  check_plus8 (temp1, temp2, temp3, 17);

  reset_data8 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 32);
  check_plus8 (temp1, temp2, temp3, 32);

  reset_data8 (temp1, temp2, temp3, N);
  test (temp1, temp2, temp3, 33);
  check_plus8 (temp1, temp2, temp3, 33);

  reset_data8 (temp1, temp2, temp3, N);
}
