/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return -413012;" "fre1" { target le } } } */
/* { dg-final { scan-tree-dump "return -611112;" "fre1" { target be } } } */

union U {
  struct S { int a : 12, b : 5, c : 10, d : 5; } s;
  struct T { int a : 7, b : 21, c : 4; } t;
};
struct A { char a[48]; union U u; };
void bar (struct A *);

int
foo (void)
{
  struct A a;
  bar (&a);
  a.u.s.a = 1590;
  a.u.s.b = -11;
  a.u.s.c = -404;
  a.u.s.d = 7;
  return a.u.t.b;
}
