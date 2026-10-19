/* { dg-do compile } */
/* { dg-options "-O2 --param tree-reassoc-width=2 -fdump-tree-reassoc2-details" } */

/* If no comparison can be folded, the chain keeps the parallel form.  */

volatile int results[4];

int
all_multi_use (int a, int b, int c, int d, int e, int f, int g, int h)
{
  int x0 = a < b;
  int x1 = c < d;
  int x2 = e < f;
  int x3 = g < h;
  results[0] = x0;
  results[1] = x1;
  results[2] = x2;
  results[3] = x3;
  return x0 & x1 & x2 & x3;
}

/* { dg-final { scan-tree-dump-times "was chosen for reassociation" 1 "reassoc2" } } */
/* { dg-final { scan-assembler-not {\tccmp\t} } } */
