/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

void loop (int * __restrict__ a, int * __restrict__ b, int * __restrict__ c,
	   int * __restrict__ d, int * __restrict__ e, int * __restrict__ f,
	   int * __restrict__ g, int * __restrict__ h)
{
  int i = 0;
  for (i = 0; i < 3; i++)
    {
      a[i] += i;
      b[i] += i;
      c[i] += i;
      d[i] += i;
      e[i] += i;
      f[i] += a[i] + 7;
      g[i] += b[i] - 3;
      h[i] += c[i] + 3;
    }
}

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, } 8 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, } 8 } } */
