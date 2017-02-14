/* { dg-do compile } */
/* { dg-options "-O -fgimple" } */

struct Y { int b[2]; };
struct X { int a; struct Y y; };
struct X x;

int __GIMPLE ()
foo (struct X *p, _Complex int q)
{
  int b;
  b_1 = __real q;
  p_4(D)->a = b_1;
  x.y.b[b_1] = b_1;
  b_2 = p->y.b[1];
  b_3 = x.a;
  return b_3;
}
