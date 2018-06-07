/* PR tree-optimization/69615 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " >= 0" "optimized" } } */
/* { dg-final { scan-tree-dump-not " < 0" "optimized" } } */

extern void foo (void);

void
bar (int z, unsigned int y)
{
  long long x = z;
  y &= 0xf;
  if (x >= 0 && x < (int) y)
    foo ();
}
