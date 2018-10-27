/* { dg-do compile } */
/* { dg-options "-O2" } */

class a {
public:
  enum b { c, g, d, e } f;
  a(b h) : f(h) {}
  a i() {
    switch (f) {
    case d:
      return c;
    case e:
      return g;
    }
  } /* { dg-warning "control reaches end of non-void function" } */
};
struct k {
  a j;
  k l() { j.i(); } /*  { dg-warning "no return statement in function returning non-void" } */
};
void m(k h) { h.l(); }
