/* { dg-do compile } */
/* { dg-options "-O -fgimple" } */

struct Y { int b[2]; };
struct X { int a; struct Y y; };
struct X x;

int __GIMPLE ()
foo (struct X *p, _Complex int q)
{
  int b;
  b = __real q;
  p->a = b;
  x.y.b[b] = b;
  b = p->y.b[1];
  b = x.a;
  return b;
}
