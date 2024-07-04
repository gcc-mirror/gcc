
/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

void
foo (int8_t *__restrict a, int8_t *__restrict b,    int8_t *__restrict c,
      int8_t *__restrict a2, int8_t *__restrict b2, int8_t *__restrict c2,
      int8_t *__restrict a3, int8_t *__restrict b3, int8_t *__restrict c3,
      int8_t *__restrict a4, int8_t *__restrict b4, int8_t *__restrict c4,
      int8_t *__restrict a5, int8_t *__restrict b5, int8_t *__restrict c5,
      int8_t *__restrict d,
      int8_t *__restrict d2,
      int8_t *__restrict d3,
      int8_t *__restrict d4,
      int8_t *__restrict d5,
      int8_t *__restrict e,
      int8_t *__restrict e2,
      int8_t *__restrict e3,
      int8_t *__restrict e4,
      int8_t *__restrict e5,
      int8_t *__restrict f,
      int8_t *__restrict f2,
      int8_t *__restrict f3,
      int8_t *__restrict f4,
      int8_t *__restrict f5,
      int8_t *__restrict g,
      int8_t *__restrict g2,
      int8_t *__restrict g3,
      int8_t *__restrict g4,
      int8_t *__restrict g5,
  
      int8_t *__restrict gg,
      int8_t *__restrict gg2,
      int8_t *__restrict gg3,
      int8_t *__restrict gg4,
      int8_t *__restrict gg5,
  
      int8_t *__restrict ggg,
      int8_t *__restrict ggg2,
      int8_t *__restrict ggg3,
      int8_t *__restrict ggg4,
      int8_t *__restrict ggg5,

      int8_t *__restrict gggg,
      int8_t *__restrict gggg2,
      int8_t *__restrict gggg3,
      int8_t *__restrict gggg4,
      int8_t *__restrict gggg5,

      int8_t *__restrict ggggg,
      int8_t *__restrict ggggg2,
      int8_t *__restrict ggggg3,
      int8_t *__restrict ggggg4,
      int8_t *__restrict ggggg5,
      int n)
{
  for (int i = 0; i < n; i++)
    {
      a[i] = b[i] + c[i];
      b5[i] = b[i] + c[i];
      a2[i] = b2[i] + c2[i];
      a3[i] = b3[i] + c3[i];
      a4[i] = b4[i] + c4[i];
      a5[i] = a[i] + a4[i];
      
      d2[i] = a2[i] + c2[i];
      d3[i] = a3[i] + c3[i];
      d4[i] = a4[i] + c4[i];
      d5[i] = a[i] + a4[i];
      a[i] = a5[i] + b5[i] + a[i];

      c2[i] = a[i] + c[i];
      c3[i] = b5[i] * a5[i];
      c4[i] = a2[i] * a3[i];
      c5[i] = b5[i] * a2[i];
      c[i] = a[i] + c3[i];
      c2[i] = a[i] + c4[i];

      e[i] = c2[i] + c2[i];
      e2[i] = c2[i] + d2[i];
      e3[i] = d3[i] + d3[i];
      e4[i] = c4[i] + a4[i];
      e5[i] = a[i] + a4[i];
      a5[i] = a[i] + a4[i];

      f[i] = e2[i] + c2[i];
      f2[i] = e2[i] + d2[i];
      f3[i] = e3[i] + d3[i];
      f4[i] = e4[i] + a4[i];
      f5[i] = e[i] + a4[i];
      f5[i] = e5[i] + a4[i];

      g[i] = f2[i] + c2[i];
      g2[i] = f2[i] + d2[i];
      g3[i] = f3[i] + d3[i];
      g4[i] = f4[i] + a4[i];
      g5[i] = f[i] + a4[i];
      g5[i] = f5[i] + a4[i];


      gg[i] = f2[i] + c2[i];
      gg2[i] = f2[i] + d2[i];
      gg3[i] = f3[i] + d3[i];
      gg4[i] = f4[i] + a4[i];
      gg5[i] = f[i] + a4[i];
      gg5[i] = f5[i] + a4[i];


      ggg[i] = f2[i] + c2[i];
      ggg2[i] = f2[i] + d2[i];
      ggg3[i] = f3[i] + d3[i];
      ggg4[i] = f4[i] + a4[i];
      ggg5[i] = f[i] + a4[i];
      ggg5[i] = f5[i] + a4[i];

      gggg[i] = f2[i] + c2[i];
      gggg2[i] = f2[i] + d2[i];
      gggg3[i] = f3[i] + d3[i];
      gggg4[i] = f4[i] + a4[i];
      gggg5[i] = f[i] + a4[i];
      gggg5[i] = f5[i] + a4[i];

      ggggg[i] = f2[i] + c2[i];
      ggggg2[i] = f2[i] + d2[i];
      ggggg3[i] = f3[i] + d3[i];
      ggggg4[i] = f4[i] + a4[i];
      ggggg5[i] = f[i] + a4[i];
      ggggg5[i] = f5[i] + a4[i];

      a[i] = a[i] + b5[i] + a[i] * a2[i] * a3[i] * a4[i] 
      * a5[i] * c[i] * c2[i] * c3[i] * c4[i] * c5[i]
      * d[i] * d2[i] * d3[i] * d4[i] * d5[i]
      * e[i] * e2[i] * e3[i] * e4[i] * e5[i]
      * f[i] * f2[i] * f3[i] * f4[i] * f5[i]
      * g[i] * g2[i] * g3[i] * g4[i] * g5[i]
      * gg[i] * gg2[i] * gg3[i] * gg4[i] * gg5[i]
      * ggg[i] * ggg2[i] * ggg3[i] * ggg4[i] * ggg5[i]
      * gggg[i] * gggg2[i] * gggg3[i] * gggg4[i] * gggg5[i]
      * ggggg[i] * ggggg2[i] * ggggg3[i] * ggggg4[i] * ggggg5[i];
    }
}

/* { dg-final { scan-assembler {e8,m1} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-tree-dump-times "Preferring smaller LMUL loop because it has unexpected spills" 3 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 8" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 2" 1 "vect" } } */
