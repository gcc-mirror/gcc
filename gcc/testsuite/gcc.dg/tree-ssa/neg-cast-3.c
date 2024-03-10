/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1 -fdump-tree-optimized" } */
/* PR tree-optimization/107137 */

unsigned f(_Bool a)
{
  int t = a;
  t = -t;
  return t;
}

/* There should be no cast to int at all. */
/* { dg-final { scan-tree-dump-not "\\\(int\\\)" "forwprop1" } } */
