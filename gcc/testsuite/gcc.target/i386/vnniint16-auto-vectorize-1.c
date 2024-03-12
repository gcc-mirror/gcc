/* { dg-do compile } */                                     
/* { dg-options "-mavxvnniint16 -O2" } */
/* { dg-final { scan-assembler "vpdpwusd\t" } } */
/* { dg-final { scan-assembler "vpdpwuud\t" } } */

int __attribute__((noinline, noclone, optimize("tree-vectorize")))
usdot_prod_hi (unsigned short * restrict a, short * restrict b,
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
udot_prod_hi (unsigned short * restrict a, unsigned short *restrict b,
	      int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}
