// PR sanitizer/82353

#include "pr82353-2.h"

B a;
E b;
B C::c0;
unsigned D::d0;

void
foo ()
{
  a.b1 = p.f2.e2.b1 = 5;
}

void
bar ()
{
  int c = p.f2.e4.d1.a0 - -~p.f4 * 89;
  q.c0.b0 = i > g * a.b0 * h - k % a.b1;
  if ((~(m * j) && -~p.f4 * 90284000534361) % ~m * j)
    b.e2.b0 << l << f;
  o = -~p.f4 * 89;
  int d = p.f4;
  if (b.e2.b0)
    b.e2.b1 = c;
  bool e = ~-~p.f4;
  a.b1 % e;
  if (k / p.f2.e2.b1)
    b.e4.d0 = g * a.b0 * h;
  n = j;
}
