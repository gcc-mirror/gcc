/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return 72876566;" "fre1" { target le } } } */
/* { dg-final { scan-tree-dump "return 559957376;" "fre1" { target be } } } */

union U {
  struct S { int a : 12, b : 5, c : 10, d : 5; } s;
  unsigned int i;
};
struct A { char a[12]; union U u; };
void bar (struct A *);

unsigned
foo (void)
{
  struct A a;
  bar (&a);
  a.u.s.a = 1590;
  a.u.s.c = -404;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define M 0x67e0a5f
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define M 0xa5f067e0
#else
#define M 0
#endif
  return a.u.i & M;
}
