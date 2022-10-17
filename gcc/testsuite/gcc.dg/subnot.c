/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

float g(float a, float b)
{
  return ~(int)a - ~(int)b;
}

/* { dg-final { scan-tree-dump-not "~" "optimized" } } */
