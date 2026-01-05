/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "csel\t" 0 } } */

void f(const int *restrict in,
   int *restrict out,
   int n, int threshold)
{
  for (int i = 0; i < n; i+=4) {
    out[i+0] = in[i+0] > threshold ? in[i+0] : in[i+0] + i;
    out[i+1] = in[i+1] > threshold ? in[i+1] : in[i+1] + i;
    out[i+2] = in[i+2] > threshold ? in[i+2] : in[i+2] + i;
    out[i+3] = in[i+3] > threshold ? in[i+3] : in[i+3] + i;
  }
}

