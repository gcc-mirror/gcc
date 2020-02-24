/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return -1462729318;" "fre1" { target le } } } */
/* { dg-final { scan-tree-dump "return 1300568597;" "fre1" { target be } } } */

union U {
  struct S { int a : 1, b : 7, c : 8, d : 11, e : 5; } s;
  unsigned int i;
};
struct A { char a[8]; union U u; };
void bar (struct A *);

int
foo (void)
{
  struct A a;
  bar (&a);
  a.u.s.a = 0;
  a.u.s.b = -51;
  a.u.s.c = -123;
  a.u.s.d = 208;
  a.u.s.e = -11;
  return a.u.i;
}
