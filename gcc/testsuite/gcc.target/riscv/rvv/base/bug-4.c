/* { dg-do run { target { riscv_vector } } } */
/* { dg-options "-O2" } */

#include "riscv_vector.h"
#include <stdio.h>

int main()
{
  float a = 1.0;
  float b[1] = {3.0};
  float c[1] = {10.0};
  float d[1] = {0.0};
  vfloat32m1_t vb = __riscv_vle32_v_f32m1 (b, 1);
  vfloat32m1_t vc = __riscv_vle32_v_f32m1 (c, 1);
  vfloat32m1_t vd = __riscv_vfnmsub_vf_f32m1 (vb, a, vc, 1);
  __riscv_vse32_v_f32m1 (d, vd, 1);
  if (d[0] != 7.0){
      printf("d[0] should be 7.0, but got %f\n", d[0]);
      __builtin_abort ();
  }
  return 0;
}
