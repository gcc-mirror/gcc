/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

#include "riscv_vector.h"

typedef float float32_t;
void test (uint32_t blockSize1, float32_t *pDst, uint32_t blkCnt)
{
    float32_t sum = 0.0f;
    uint32_t blockSize2 = blockSize1;
    uint32_t count = 1;
    vfloat32m1_t temp00m1;

    while (blockSize1 > 0)
    {
      uint32_t vblkCnt = count;
      size_t l =  __riscv_vsetvl_e32m1 (1);
      temp00m1 = __riscv_vfmv_v_f_f32m1 (1, l);
      for (; (l = __riscv_vsetvl_e32m8 (vblkCnt)) > 0; vblkCnt -= l);
      sum +=  __riscv_vfmv_f_s_f32m1_f32 (temp00m1);
      count++;
      blockSize1--;
    }

    while (blkCnt > 0)
    {
      size_t l = __riscv_vsetvl_e32m1 (blockSize1);
      temp00m1 = __riscv_vfmv_v_f_f32m1 (0, l);
      blkCnt--;
    }

    while (blockSize2-- > 0)
    {
      size_t l =  __riscv_vsetvl_e32m1 (1);
      temp00m1 =  __riscv_vfmv_v_f_f32m1 (0, l);
      sum += __riscv_vfmv_f_s_f32m1_f32 (temp00m1);
      *pDst++ = sum;
    }
}

/* { dg-final { scan-assembler-times {vsetivli\s+[a-x0-9]+,\s*1,\s*e32,\s*m1,\s*ta,\s*ma\s*vfmv\.v\.f\s+v[0-9]+,\s*[a-x0-9]+} 1 } } */
