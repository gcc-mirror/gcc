/* PR tree-optimization/36504 */
/* { dg-do compile } */
/* { dg-options "-O3 -fprefetch-loop-arrays -w" } */
/* { dg-additional-options "-march=i686 -msse" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

struct A { struct { int a; } b[8]; };
struct B { int c; int d; };
struct C { struct B d; };

void bar (struct C *, int);

struct B
foo (struct C *p, struct A *e, int b)
{
  struct B q;
  bar (p, e->b[b].a);
  return q;
}

void
baz (int b, struct A *e)
{
  struct C p;
  for (; b; ++b)
    p.d = foo (&p, e, b);
}
