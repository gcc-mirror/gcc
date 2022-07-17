/* { dg-do compile } */
/* { dg-additional-options "-msse -mtune=generic -fdump-tree-slp2-details" } */

struct S { unsigned long a, b; } s;

void
foo (unsigned long a, unsigned long b)
{
  s.a = a;
  s.b = b;
}

/* { dg-final { scan-tree-dump-not "basic block part vectorized" "slp2" } } */
