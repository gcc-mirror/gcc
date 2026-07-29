/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions -fdump-tree-optimized" } */

/* A possibly zero divisor can trap when non-call exceptions are enabled,
   so the comparison must keep the division or remainder.  */

int f1 (unsigned int x, unsigned int y) { return x / y == 0; }
int f2 (unsigned int x, unsigned int y) { return x / y != 0; }
int f3 (unsigned int x, unsigned int y) { return x % y == x; }
int f4 (unsigned int x, unsigned int y) { return x % y != x; }

/* A known nonzero divisor can still fold.  */
int f5 (unsigned int x) { return x / 3 == 0; }
int f6 (unsigned int x) { return x % 3 == x; }

/* An explicit zero must retain the front-end diagnostic and trap.  */
int f7 (unsigned int x)
{
  return x / 0 == 0; /* { dg-warning "division by zero" } */
}
int f8 (unsigned int x)
{
  return x % 0 == x; /* { dg-warning "division by zero" } */
}

/* { dg-final { scan-tree-dump-times " / " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " % " 3 "optimized" } } */
