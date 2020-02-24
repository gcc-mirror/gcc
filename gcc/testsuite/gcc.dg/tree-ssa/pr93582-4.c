/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return -1991560811;" "fre1" { target le } } } */
/* { dg-final { scan-tree-dump "return -733324916;" "fre1" { target be } } } */

union U {
  struct S { int a : 1, b : 4, c : 27; } s;
  unsigned int i;
};
struct A { char a[24]; union U u; };
void bar (struct A *);

int
foo (void)
{
  struct A a;
  bar (&a);
  a.u.s.a = -1;
  a.u.s.b = -6;
  a.u.s.c = -62236276;
  return a.u.i;
}
