/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -mrvv-vector-bits=zvl" } */

int f[12][100];

void bad1(int v1, int v2)
{
  for (int r = 0; r < 100; r += 4)
    {
      int i = r + 1;
      f[0][r] = f[1][r] * (f[2][r]) - f[1][i] * (f[2][i]);
      f[0][i] = f[1][r] * (f[2][i]) + f[1][i] * (f[2][r]);
      f[0][r+2] = f[1][r+2] * (f[2][r+2]) - f[1][i+2] * (f[2][i+2]);
      f[0][i+2] = f[1][r+2] * (f[2][i+2]) + f[1][i+2] * (f[2][r+2]);
    }
}

/* { dg-final { scan-assembler {e32,m2} } } */
/* { dg-final { scan-assembler-not {jr} } } */
/* { dg-final { scan-assembler-times {ret} 1 } } */
