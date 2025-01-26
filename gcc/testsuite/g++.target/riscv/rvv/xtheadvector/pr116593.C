/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zfh_xtheadvector -mabi=ilp32d -O2" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_zfh_xtheadvector -mabi=lp64d -O2" { target { rv64 } } } */

#include <math.h>
#include <riscv_vector.h>
#include <vector>

static
vfloat32m8_t atan2_ps (vfloat32m8_t a, vfloat32m8_t b, size_t vl)
{
  std::vector<float> tmpx (vl);
  std::vector<float> tmpy (vl);
  __riscv_vse32_v_f32m8 (tmpx.data (), a, vl);
  __riscv_vse32_v_f32m8 (tmpy.data (), b, vl);
  for (size_t i = 0; i < vl; i++)
  {
    tmpx[i] = atan2 (tmpx[i], tmpy[i]);
  }
  return __riscv_vle32_v_f32m8 (tmpx.data (), vl);
}

void
atan2 (const float *x, const float *y, float *out, int size, int ch)
{
  for (int i = 0; i < ch; i++)
  {
    const float *xx = x + size * i;
    const float *yy = y + size * i;
    float *zz = out + size * i;

    int n = size;
    while (n > 0)
    {
      size_t vl = __riscv_vsetvl_e32m8 (n);
      vfloat32m8_t _xx = __riscv_vle32_v_f32m8 (xx, vl);
      vfloat32m8_t _yy = __riscv_vle32_v_f32m8 (yy, vl);
      vfloat32m8_t _zz = atan2_ps (_xx, _yy, vl);
      __riscv_vse32_v_f32m8 (zz, _zz, vl);
      n -= vl;
      xx += vl;
      yy += vl;
      zz += vl;
    }
  }
}
