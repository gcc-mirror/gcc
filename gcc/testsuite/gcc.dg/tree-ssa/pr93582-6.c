/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return 890118;" "fre1" { target le } } } */
/* { dg-final { scan-tree-dump "return 447899;" "fre1" { target be } } } */

union U {
  struct S { int a : 16, b : 5, c : 10, d : 1; } s;
  struct T { int a : 8, b : 21, c : 3; } t;
};
struct A { char a[4]; union U u; };
void bar (struct A *);

int
foo (void)
{
  struct A a;
  bar (&a);
  a.u.s.a = 1590;
  a.u.s.b = -11;
  a.u.s.c = 620;
  a.u.s.d = -1;
  return a.u.t.b;
}
