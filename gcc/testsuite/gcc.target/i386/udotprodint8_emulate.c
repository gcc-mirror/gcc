/* { dg-do compile } */
/* { dg-options "-mavxvnni -O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "DOT_PROD_EXPR" 1 "optimized" } } */
/* { dg-final { scan-assembler-times "vpdpwssd" 2 } } */

int
foo (unsigned char* a, unsigned char* b)
{
  int sum = 0;
  for (int i = 0; i != 16; i++)
    {
      sum += a[i] * b[i];
    }
  return sum;
}
