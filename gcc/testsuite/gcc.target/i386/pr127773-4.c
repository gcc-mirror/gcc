/* PR target/127773 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */
/* f doesn't write any SSE register, so there is no need to issue
   vzeroupper before calling it in the loop.  */
/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */
/* { dg-final { scan-assembler "call\[ \t\]+_?f\\M.*vzeroupper" } } */

typedef long long v4di __attribute__ ((vector_size (32)));

static __attribute__ ((noinline)) int
f (int x)
{
  return x * 3 + 1;
}

__attribute__ ((target ("avx2"))) int
g (v4di *p, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    {
      p[i] += p[i];
      s += f (i);
    }
  return s;
}
