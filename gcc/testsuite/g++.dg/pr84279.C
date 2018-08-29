/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-O3 -mcpu=power8 -g -fPIC -fvisibility=hidden -fstack-protector-strong" } */

template <typename, typename T> struct E { T e; };
struct J {
  unsigned k, l;
  J (unsigned x, unsigned y) : k(x), l(y) {}
};
typedef struct A {
  J n, p;
  A ();
  A (J x, J y) : n(x), p(y) {}
} *S;
S t;
struct B {
  struct C {
    S q, r;
    int u, v;
    bool m1 (S, A &);
    J m2 () const;
    J m3 () const;
    A m4 () const;
  };
  typedef E<unsigned, S> D;
  void m5 (D *);
  void m6 (unsigned, A);
};
bool B::C::m1 (S, A &x) { bool o; x = m4 (); return o; }
J B::C::m2 () const { unsigned g (u == 0); unsigned h (v); return J (g, h); }
J B::C::m3 () const { unsigned g (q != t); unsigned h (r != t); return J (g, h); }
A B::C::m4 () const { return A (m2 (), m3 ()); }
void B::m5 (D *c) { unsigned x; C ar; A am; if (ar.m1 (c->e, am)) m6 (x, am); }
