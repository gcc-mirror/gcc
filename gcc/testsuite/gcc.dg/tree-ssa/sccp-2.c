/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int
test(unsigned int quant)
{
  unsigned int sum = 0;
  for (unsigned int i = 0; i < quant; ++i)
    sum += quant;
  return sum;
}

/* A single basic-block should remain (computing and
   returning quant * quant).  */
/* { dg-final { scan-tree-dump-times "bb" 1 "optimized" } } */
