/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return 593;" "fre1" } } */

union U {
  struct S { int a : 1, b : 14, c : 17; } s;
  struct T { int d : 2; int e : 12; int f : 18; } t;
};

int
foo (void)
{
  union U u;
  u.s.b = -7005;
  return u.t.e;
}
