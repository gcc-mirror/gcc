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

/* { dg-final { scan-assembler-times {fadd\.s\s+f[a-x0-9]+,\s*f[a-x0-9]+,fa0} 1 } } */
