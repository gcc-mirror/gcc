/* { dg-do compile } */                                     
/* { dg-options "-mavxvnniint8 -O2" } */
/* { dg-final { scan-assembler "vpdpbssd\t" } } */
/* { dg-final { scan-assembler "vpdpbuud\t" } } */

int __attribute__((noinline, noclone, optimize("tree-vectorize")))
sdot_prod_qi (char * restrict a, char * restrict b,
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
udot_prod_qi (unsigned char * restrict a, unsigned char *restrict b,
	      int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}
