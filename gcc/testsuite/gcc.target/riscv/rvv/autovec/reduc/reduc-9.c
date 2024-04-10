/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -ffast-math -fno-vect-cost-model -fdump-tree-optimized-details" } */

float __attribute__((noipa))
add_loop (float *x, int n, float res)
{
  for (int i = 0; i < n; ++i)
    {
      res += x[i * 2];
      res += x[i * 2 + 1];
    }
  return res;
}

/* { dg-final { scan-tree-dump-times "VEC_SHL_INSERT" 1 "optimized" } } */
/* { dg-final { scan-assembler-times {vfslide1up\.vf\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
