/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return 1;" "fre1" { target be } } } */
/* { dg-final { scan-tree-dump "return 2;" "fre1" { target le } } } */

union U {
  struct S { int a : 1, b : 14, c : 17; } s;
  struct T { int d : 10; int e : 4; int f : 18; } t;
};

int
foo (void)
{
  union U u;
  u.s.b = -7005;
  return u.t.e;
}
