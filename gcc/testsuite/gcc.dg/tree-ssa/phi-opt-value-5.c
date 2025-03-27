/* { dg-do compile } */
/* PR tree-optimization/114894 */
/* Phi-OPT should be able to optimize these without sinking being invoked. */
/* { dg-options "-O -fdump-tree-phiopt2 -fdump-tree-phiopt3 -fdump-tree-optimized -fno-tree-sink" } */

int fmul1(int a, int b)
{
  int c = a * b;
  if (a != 0)
    return c;
  return 0;
}


int fand1(int a, int b)
{
  int c = a & b;
  if (a != 0)
    return c;
  return 0;
}


void g(int);

int fdiv1(int a, int b)
{
  int d = b|1;
  g(d);
  int c = a / d;
  return a != 0 ? c : 0;
}

/* fdiv1 requires until later than phiopt2 to be able to detect that
   d is non-zero. to be able to remove the conditional.  */
/* { dg-final { scan-tree-dump-times "goto" 2 "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "goto" "phiopt3" } } */
/* { dg-final { scan-tree-dump-not "goto" "optimized" } } */

