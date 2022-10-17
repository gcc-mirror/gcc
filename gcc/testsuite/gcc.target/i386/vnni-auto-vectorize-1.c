/* { dg-do compile } */                                     
/* { dg-options "-mavx512f -mavx512vnni -mavx512vl -O2" } */

/* { dg-final { scan-assembler "vpdpwssd\t" } } */
/* { dg-final { scan-assembler "vpdpbusd\t" } } */
/* { dg-final { scan-assembler-not "vpmaddwd\t" } } */

int __attribute__((noinline, noclone, optimize("tree-vectorize")))
sdot_prod_hi (short * restrict a, short * restrict b,
	      int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noinline, noclone, optimize("tree-vectorize")))
usdot_prod_qi (unsigned char * restrict a, char *restrict b,
	       int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}
