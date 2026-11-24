// PR tree-optimization/112628
// { dg-do compile }
// { dg-options "-O2 -ftrapv -fdump-tree-optimized" }

int
f (int a)
{
  return (~a) < -a;
}

// The negation must survive so that it can trap
// { dg-final { scan-tree-dump "= -a_" "optimized" } }
