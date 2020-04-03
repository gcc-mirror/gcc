/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */

union U {
  struct S { int a : 1, b : 4, c : 27; } s;
  struct T { int d : 2; int e : 2; int f : 28; } t;
};

int
foo (void)
{
  union U u;
  u.s.b = 10;
  return u.t.e;
}
