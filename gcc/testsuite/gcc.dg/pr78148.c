/* PR target/78148 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

struct A { int a, b; };
struct B { char c, d; };
extern void bar (struct A, struct B);
struct C { char e, f; } a;
struct D
{
  int g;
  struct C h[4];
};
struct D *e;

struct D
foo (void)
{
  int b;
  struct B c;
  struct A d;
  d.b = c.c = c.d = 0;
  bar (d, c);
}

void
baz ()
{
  e->h[0].e = e->h[0].f = 0;
  foo ();
}
