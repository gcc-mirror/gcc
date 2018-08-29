/* PR tree-optimization/69615 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " >= 0" "optimized" } } */
/* { dg-final { scan-tree-dump-not " < 0" "optimized" } } */
/* { dg-final { scan-tree-dump-not " <= 23" "optimized" } } */
/* { dg-final { scan-tree-dump-not " > 23" "optimized" } } */

extern void foo (void);

void
f1 (int x)
{
  if (x >= 0 && x <= __INT_MAX__ - 1)
    foo ();
}

void
f2 (int x, int y)
{
  if (x >= 0 && y && x <= __INT_MAX__ - 1)
    foo ();
}

void
f3 (int x)
{
  if (x > -__INT_MAX__ - 1 && x <= 23)
    foo ();
}

void
f4 (int x, int y)
{
  if (x > -__INT_MAX__ - 1 && y && x <= 23)
    foo ();
}
