// PR c++/122634
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using namespace std::meta;

struct A { int a; };
namespace N { struct B { int b; }; }
struct C : typename [: ^^A :] {};
struct D : [: ^^A :] {};
struct E : [: ^^N :] :: B {};
struct F : [: ^^:: :] :: A {};
struct G : public typename [: ^^A :] {};
struct H : public [: ^^A :] {};
struct I : public [: ^^N :] :: B {};
struct J : public [: ^^:: :] :: A {};
template <auto I>
struct K : typename [: I :] {};
template <auto I>
struct L : [: I :] {};
template <auto I>
struct M : [: I :] :: B {};
template <auto I>
struct O : [: I :] :: A {};
template <auto I>
struct P : public typename [: I :] {};
template <auto I>
struct Q : public [: I :] {};
template <auto I>
struct R : public [: I :] :: B {};
template <auto I>
struct S : public [: I :] :: A {};

constexpr access_context ctx = access_context::unprivileged ();
static_assert (type_of (bases_of (^^C, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^D, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^E, ctx)[0]) == ^^N::B);
static_assert (type_of (bases_of (^^F, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^G, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^H, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^I, ctx)[0]) == ^^N::B);
static_assert (type_of (bases_of (^^J, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^K <^^A>, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^L <^^A>, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^M <^^N>, ctx)[0]) == ^^N::B);
static_assert (type_of (bases_of (^^O <^^::>, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^P <^^A>, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^Q <^^A>, ctx)[0]) == ^^A);
static_assert (type_of (bases_of (^^R <^^N>, ctx)[0]) == ^^N::B);
static_assert (type_of (bases_of (^^S <^^::>, ctx)[0]) == ^^A);

int
foo (C &c, D &d, E &e, F &f, G &g, H &h, I &i, J &j)
{
  return c.a + d.a + e.b + f.a + g.a + h.a + i.b + j.a;
}

int
bar (K <^^A> &k, L <^^A> &l, M <^^N> &m, O <^^::> &o,
     P <^^A> &p, Q <^^A> &q, R <^^N> &r, S <^^::> &s)
{
  return k.a + l.a + m.b + o.a + p.a + q.a + r.b + s.a;
}
