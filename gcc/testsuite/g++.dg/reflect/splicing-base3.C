// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using namespace std::meta;

struct A { int a; };
struct B { int b; };
struct C { int c; };
struct D { int d; };
struct E { int e; };
struct F { int f; };
struct G { int g; };
struct H { int h; };
struct I : public A, protected B, private C { int i; };
struct J : protected D, private E, public F { int j; };
struct K : public G, protected I, private H, public J { int k; };
struct L : virtual A { int l; };
struct M : virtual A { int m; };
struct N : public L, protected M, public virtual A { int n; };

constexpr access_context gctx = access_context::current ();
constexpr access_context uctx = access_context::unchecked ();

void
g ()
{
  I i;
  A &a1 = i.[: bases_of(^^I, gctx)[0] :];
  a1.a++;
  A &a2 = i.[: bases_of(^^I, uctx)[0] :];
  a2.a++;
  B &b = i.[: bases_of(^^I, uctx)[1] :];
  b.b++;
  C &c = i.[: bases_of(^^I, uctx)[2] :];
  c.c++;
  A *ap1 = &i.[: bases_of(^^I, gctx)[0] :];
  ap1->a++;
  A *ap2 = &i.[: bases_of(^^I, uctx)[0] :];
  ap2->a++;
  B *bp = &i.[: bases_of(^^I, uctx)[1] :];
  bp->b++;
  C *cp = &i.[: bases_of(^^I, uctx)[2] :];
  cp->c++;

  J j;
  F &f1 = j.[: bases_of(^^J, gctx)[0] :];
  f1.f++;
  D &d = j.[: bases_of(^^J, uctx)[0] :];
  d.d++;
  E &e = j.[: bases_of(^^J, uctx)[1] :];
  e.e++;
  F &f2 = j.[: bases_of(^^J, uctx)[2] :];
  f2.f++;
  F *fp1 = &j.[: bases_of(^^J, gctx)[0] :];
  fp1->f++;
  D *dp = &j.[: bases_of(^^J, uctx)[0] :];
  dp->d++;
  E *ep = &j.[: bases_of(^^J, uctx)[1] :];
  ep->e++;
  F *fp2 = &j.[: bases_of(^^J, uctx)[2] :];
  fp2->f++;

  K k;
  G &g1 = k.[: bases_of(^^K, gctx)[0] :];
  g1.g++;
  J &j1 = k.[: bases_of(^^K, gctx)[1] :];
  j1.j++;
  G *gp = &k.[: bases_of(^^K, gctx)[0] :];
  gp->g++;
  J *jp1 = &k.[: bases_of(^^K, gctx)[1] :];
  jp1->j++;
  G &g2 = k.[: bases_of(^^K, uctx)[0] :];
  g2.g++;
  I &i2 = k.[: bases_of(^^K, uctx)[1] :];
  i2.i++;
  H &h = k.[: bases_of(^^K, uctx)[2] :];
  h.h++;
  J &j3 = k.[: bases_of(^^K, uctx)[3] :];
  j3.j++;
  G *gp2 = &k.[: bases_of(^^K, uctx)[0] :];
  gp2->g++;
  I *ip2 = &k.[: bases_of(^^K, uctx)[1] :];
  ip2->i++;
  H *hp = &k.[: bases_of(^^K, uctx)[2] :];
  hp->h++;
  J *jp3 = &k.[: bases_of(^^K, uctx)[3] :];
  jp3->j++;

  N n;
  L &l = n.[: bases_of(^^N, gctx)[0] :];
  l.l++;
  A &a3 = n.[: bases_of(^^N, gctx)[1] :];
  a3.a++;
  L *lp = &n.[: bases_of(^^N, gctx)[0] :];
  lp->l++;
  A *ap3 = &n.[: bases_of(^^N, gctx)[1] :];
  ap3->a++;
  L &l2 = n.[: bases_of(^^N, uctx)[0] :];
  l2.l++;
  M &m = n.[: bases_of(^^N, uctx)[1] :];
  m.m++;
  A &a4 = n.[: bases_of(^^N, uctx)[2] :];
  a4.a++;
  L *lp2 = &n.[: bases_of(^^N, uctx)[0] :];
  lp2->l++;
  M *mp = &n.[: bases_of(^^N, uctx)[1] :];
  mp->m++;
  A *ap4 = &n.[: bases_of(^^N, uctx)[2] :];
  ap4->a++;
}
