/* { dg-do compile } */
/* { dg-options "-O2 --param tree-reassoc-width=2 -fdump-tree-reassoc2-details" } */

/* Comparison leaves can have uses outside the chain.  A chain stays linear
   when another comparison can be folded into the flags.  */

volatile int result;

int
one_multi_use (int a, int b, int c, int d, int e, int f, int g, int h)
{
  int x = a < b;
  result = x;
  return x & (c < d) & (e < f) & (g < h);
}
/* { dg-final { scan-assembler-times {\tccmp\t} 3 } } */
/* { dg-final { scan-tree-dump-not "was chosen for reassociation" "reassoc2" } } */
