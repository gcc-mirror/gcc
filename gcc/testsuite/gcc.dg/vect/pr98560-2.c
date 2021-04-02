/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-tree-vrp -fno-tree-fre -fno-tree-pre -fno-code-hoisting -fvect-cost-model=dynamic" } */
/* { dg-additional-options "-msve-vector-bits=128" { target aarch64_sve } } */

#include <stdint.h>

void
f (uint16_t *restrict dst, uint32_t *restrict src1, float *restrict src2)
{
  int i = 0;
  for (int j = 0; j < 4; ++j)
    {
      uint16_t tmp = src1[i] >> 1;
      dst[i] = (uint16_t) (src2[i] == 0 && i < 4 ? tmp : 1);
      i += 1;
    }
}
