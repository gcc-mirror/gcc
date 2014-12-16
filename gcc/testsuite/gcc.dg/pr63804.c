/* PR rtl-optimization/63804 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

struct A { int gen; } e;
int a, d;
long b;
enum B { C };
struct D
{
  enum B type : 1;
  int nr : 1;
  struct { unsigned ud; } dw1;
};
enum B c;

void
fn1 (int p1)
{
  b = p1 & a;
}

int fn2 ();
void fn3 ();

void
fn4 (struct D p1, unsigned p2, int p3)
{
  struct D f, g, h, j = p1, l, m = l;
  struct A i = e;
  if (i.gen)
    p2 = 0;
  j.type = c;
  g = j;
  p1 = g;
  fn3 ();
  int k = p2, v = p1.nr, p = v;
  m.dw1.ud = k;
  f = m;
  h = f;
  struct D n = h;
  fn3 (n);
  {
    d = fn2 ();
    int o = d;
    fn1 (o);
  }
  if (i.gen)
    fn3 (p1);
  b = p & a;
  fn3 (p3);
}
