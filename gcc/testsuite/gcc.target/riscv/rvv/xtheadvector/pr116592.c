/* { dg-do assemble } */
/* { dg-options "-march=rv32gc_zfh_xtheadvector -mabi=ilp32d -O2 -save-temps" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_zfh_xtheadvector -mabi=lp64d -O2 -save-temps" { target { rv64 } } } */

#include <math.h>
#include "riscv_vector.h"

static vfloat32m8_t atan2_ps(vfloat32m8_t a, vfloat32m8_t b, size_t vl)
{
  float tmpx[vl];
  float tmpy[vl];
  __riscv_vse32_v_f32m8(tmpx, a, vl);
  __riscv_vse32_v_f32m8(tmpy, b, vl);
  for (size_t i = 0; i < vl; i++)
  {
    tmpx[i] = atan2(tmpx[i], tmpy[i]);
  }
  return __riscv_vle32_v_f32m8(tmpx, vl);
}

void my_atan2(const float *x, const float *y, float *out, int size)
{
  int n = size;
  while (n > 0)
  {
    size_t vl = __riscv_vsetvl_e32m8(n);
    vfloat32m8_t _x = __riscv_vle32_v_f32m8(x, vl);
    vfloat32m8_t _y = __riscv_vle32_v_f32m8(y, vl);
    vfloat32m8_t _out = atan2_ps(_x, _y, vl);
    __riscv_vse32_v_f32m8(out, _out, vl);
    n -= vl;
    x += vl;
    y += vl;
    out += vl;
  }
}

/* { dg-final { scan-assembler-not {th\.vsetvli\s+zero,0} } } */
