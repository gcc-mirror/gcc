/* PR tree-optimization/126627 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32 } */

int test1 (int a) { return (a & -a) < 0; }
int test2 (int a) { return (a & -a) >= 0; }
int test3 (unsigned a) { return (int)(a & -a) < 0; }
int test4 (unsigned a) { return (int)(a & -a) >= 0; }

/* Negation done in unsigned to avoid UB.  */
int test_si (signed a)
{
  unsigned b = a;
  b = -b;
  signed t = b;
  return (a & t) >= 0;
}

/* { dg-final { scan-tree-dump-not " & " "optimized" } } */
/* { dg-final { scan-tree-dump-times " == -2147483648" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " != -2147483648" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " == 2147483648" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " != 2147483648" 1 "optimized" } } */
