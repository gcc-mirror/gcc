/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

float f[12][100];

void bad1(float v1, float v2)
{
  for (int r = 0; r < 100; r += 4)
    {
      int i = r + 1;
      f[0][r] = f[1][r] * (f[2][r] + v2) - f[1][i] * (f[2][i] + v1);
      f[0][i] = f[1][r] * (f[2][i] + v1) + f[1][i] * (f[2][r] + v2);
      f[0][r+2] = f[1][r+2] * (f[2][r+2] + v2) - f[1][i+2] * (f[2][i+2] + v1);
      f[0][i+2] = f[1][r+2] * (f[2][i+2] + v1) + f[1][i+2] * (f[2][r+2] + v2);
    }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*1,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli} 2 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
