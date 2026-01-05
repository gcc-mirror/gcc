/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "csel\t" 1 } } */

void f(const int *restrict in,
   int *restrict out,
   int n, int threshold)
{
  for (int i = 0; i < n; ++i) {
    int v = in[i];
    if (v > threshold) {
      int t = v * 3;
      t += 7;
      out[i] = t;
    } else {
      out[i] = v;
    }
  }
}

