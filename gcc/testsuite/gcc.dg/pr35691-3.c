/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1-details" } */

int foo(int z0, unsigned z1)
{
  int t0 = (z0 == -1);
  int t1 = (z1 == -1U);
  int t2 = (t0 & t1);
  return t2;
}

/* { dg-final { scan-tree-dump "gimple_simplified to _\[0-9\]* = \\(int\\) z1_\[0-9\]*\\(D\\);" "forwprop1" } } */
