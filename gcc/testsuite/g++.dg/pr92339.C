/* PR c++/92339  */
/* { dg-options "-std=c++11" } */

class a {
  template <typename b, b a::*> struct c { c(a *); };
  int m_fn1();
  unsigned long d;
  using e = c<unsigned long, &a::d>;
};
int a::m_fn1() { e(this); return 0; }
