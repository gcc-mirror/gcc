/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "csel\t" 0 } } */

void f(const int *restrict in,
   int *restrict out,
   int n, int threshold)
{
  for (int i = 0; i < n; ++i) {
    int v = in[i];
    if (v > threshold) {
      int t = v * 3;
      t += 7;
      t ^= 0x55;
      t *= 0x55;
      t -= 0x5;
      t &= 0xFE;
      t ^= 0x55;
      out[i] = t;
    } else {
      out[i] = v;
    }
  }
}

