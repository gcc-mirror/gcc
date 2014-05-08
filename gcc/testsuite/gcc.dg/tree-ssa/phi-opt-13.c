/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

// Division is expensive
long f(long a, long b) {
  if (__builtin_expect(b == 1, 1)) return a;
  return a / b;
}

/* { dg-final { scan-tree-dump-times "goto " 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
