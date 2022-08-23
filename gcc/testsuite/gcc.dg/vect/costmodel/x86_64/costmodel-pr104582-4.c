/* { dg-do compile } */
/* { dg-additional-options "-msse -fdump-tree-slp2-details" } */

struct S { unsigned long a, b; } s;

void
foo (signed long *a, unsigned long *b)
{
  unsigned long a_ = *a;
  unsigned long b_ = *b;
  s.a = a_;
  s.b = b_;
}

/* { dg-final { scan-tree-dump "basic block part vectorized" "slp2" } } */
