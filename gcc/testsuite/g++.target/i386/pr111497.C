// { dg-do compile { target ia32 } }
// { dg-options "-march=i686 -mtune=generic -fPIC -O2 -g" }
// { dg-require-effective-target fpic }

class A;
struct B { const char *b1; int b2; };
struct C : B { C (const char *x, int y) { b1 = x; b2 = y; } };
struct D : C { D (B x) : C (x.b1, x.b2) {} };
struct E { E (A *); };
struct F : E { D f1, f2, f3, f4, f5, f6; F (A *, const B &, const B &, const B &); };
struct G : F { G (A *, const B &, const B &, const B &); };
struct H { int h; };
struct I { H i; };
struct J { I *j; };
struct A : J {};
inline F::F (A *x, const B &y, const B &z, const B &w)
  : E(x), f1(y), f2(z), f3(w), f4(y), f5(z), f6(w) {}
G::G (A *x, const B &y, const B &z, const B &w) : F(x, y, z, w)
{
  H *h = &x->j->i;
  if (h)
    h->h++;
}
