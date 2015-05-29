/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized -frounding-math" } */

void bar(float x);
void foo(double x)
{
  bar(-x);
}

/* { dg-final { scan-tree-dump-not "-\\(float\\)" "optimized" } } */
