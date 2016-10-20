/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop-details" } */

unsigned f(unsigned a, unsigned b)
{
  unsigned t1 = a ^ b;
  unsigned t2 = t1 | a;
  return t2;
}

/* { dg-final { scan-tree-dump "gimple_simplified to t2_\[0-9\] = a_\[0-9\]*\\(D\\) | b_\[0-9\]*\\(D\\)" "forwprop1" } } */
