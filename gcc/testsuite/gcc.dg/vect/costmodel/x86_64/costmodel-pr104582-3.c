/* { dg-do compile } */
/* { dg-additional-options "-msse -fdump-tree-slp2-details" } */

struct S { double a, b; } s;

void
foo (double a, double b)
{
  s.a = a;
  s.b = b;
}

/* { dg-final { scan-tree-dump "basic block part vectorized" "slp2" } } */
