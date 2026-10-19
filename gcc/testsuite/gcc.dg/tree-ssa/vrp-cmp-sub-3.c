/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-evrp" } */
/* PR tree-optimization/127485 */

void dead (int);

void f (int a, int b)
{
  int d = a - b;
  if (d != 0) // `a - b != 0` -> `a != b`
    if (a == b)
      dead (d);
  return;
}

/* { dg-final { scan-tree-dump-not " == " "optimized" } } */
/* { dg-final { scan-tree-dump-not " if " "optimized" } } */
/* { dg-final { scan-tree-dump-not " dead " "optimized" } } */
/* { dg-final { scan-tree-dump-not " != " "optimized" } } */

/* { dg-final { scan-tree-dump-not " dead " "evrp" } } */
