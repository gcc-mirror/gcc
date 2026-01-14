// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_accessible.

#include <meta>

using namespace std::meta;

struct A { int a; };
struct B { int b; };
struct C { int c; };

struct D : A, protected B, private C {
  static consteval access_context foo () { return access_context::current (); }
  int d;
public:
  struct E {};
  enum F { F1 };
  int bar () { return 0; }
  template <typename T>
  void baz () { }
  using g = int;
  static constexpr int j = 42;
protected:
  int e;
  struct G {};
  enum H { H1 };
  int qux () { return 0; }
  template <typename T>
  void freddy () { }
  using h = int;
  static constexpr int k = 42;
private:
  int f;
  struct I {};
  enum J { J1 };
  int corge () { return 0; }
  template <typename T>
  void garply () {}
  using i = int;
  static constexpr int l = 42;
  friend struct K;
  friend void plugh ();
public:
  void waldo ()
  {
    constexpr auto ctx = access_context::current ();
    static_assert (is_accessible (^^d, ctx));
    static_assert (is_accessible (^^E, ctx));
    static_assert (is_accessible (^^F, ctx));
    static_assert (is_accessible (^^F1, ctx));
    static_assert (is_accessible (^^bar, ctx));
    static_assert (is_accessible (^^baz, ctx));
    static_assert (is_accessible (^^g, ctx));
    static_assert (is_accessible (^^j, ctx));
    static_assert (is_accessible (^^e, ctx));
    static_assert (is_accessible (^^G, ctx));
    static_assert (is_accessible (^^H, ctx));
    static_assert (is_accessible (^^H1, ctx));
    static_assert (is_accessible (^^qux, ctx));
    static_assert (is_accessible (^^freddy, ctx));
    static_assert (is_accessible (^^h, ctx));
    static_assert (is_accessible (^^k, ctx));
    static_assert (is_accessible (^^f, ctx));
    static_assert (is_accessible (^^I, ctx));
    static_assert (is_accessible (^^J, ctx));
    static_assert (is_accessible (^^J1, ctx));
    static_assert (is_accessible (^^corge, ctx));
    static_assert (is_accessible (^^garply, ctx));
    static_assert (is_accessible (^^i, ctx));
    static_assert (is_accessible (^^l, ctx));
    static_assert (is_accessible (^^waldo, ctx));
  }
  static constexpr info rd = ^^d;
  static constexpr info rE = ^^E;
  static constexpr info rF = ^^F;
  static constexpr info rF1 = ^^F1;
  static constexpr info rbar = ^^bar;
  static constexpr info rbaz = ^^baz;
  static constexpr info rg = ^^g;
  static constexpr info rj = ^^j;
  static constexpr info re = ^^e;
  static constexpr info rG = ^^G;
  static constexpr info rH = ^^H;
  static constexpr info rH1 = ^^H1;
  static constexpr info rqux = ^^qux;
  static constexpr info rfreddy = ^^freddy;
  static constexpr info rh = ^^h;
  static constexpr info rk = ^^k;
  static constexpr info rf = ^^f;
  static constexpr info rI = ^^I;
  static constexpr info rJ = ^^J;
  static constexpr info rJ1 = ^^J1;
  static constexpr info rcorge = ^^corge;
  static constexpr info rgarply = ^^garply;
  static constexpr info ri = ^^i;
  static constexpr info rl = ^^l;
  static constexpr info rwaldo = ^^waldo;
};

constexpr auto gctx = access_context::current ();
static_assert (is_accessible (D::rd, gctx));
static_assert (is_accessible (D::rE, gctx));
static_assert (is_accessible (D::rF, gctx));
static_assert (is_accessible (D::rF1, gctx));
static_assert (is_accessible (D::rbar, gctx));
static_assert (is_accessible (D::rbaz, gctx));
static_assert (is_accessible (D::rg, gctx));
static_assert (is_accessible (D::rj, gctx));
static_assert (!is_accessible (D::re, gctx));
static_assert (!is_accessible (D::rG, gctx));
static_assert (!is_accessible (D::rH, gctx));
static_assert (!is_accessible (D::rH1, gctx));
static_assert (!is_accessible (D::rqux, gctx));
static_assert (!is_accessible (D::rfreddy, gctx));
static_assert (!is_accessible (D::rh, gctx));
static_assert (!is_accessible (D::rk, gctx));
static_assert (!is_accessible (D::rf, gctx));
static_assert (!is_accessible (D::rI, gctx));
static_assert (!is_accessible (D::rJ, gctx));
static_assert (!is_accessible (D::rJ1, gctx));
static_assert (!is_accessible (D::rcorge, gctx));
static_assert (!is_accessible (D::rgarply, gctx));
static_assert (!is_accessible (D::ri, gctx));
static_assert (!is_accessible (D::rl, gctx));
static_assert (is_accessible (D::rwaldo, gctx));
static_assert (is_accessible (D::rd, D::foo ()));
static_assert (is_accessible (D::rE, D::foo ()));
static_assert (is_accessible (D::rF, D::foo ()));
static_assert (is_accessible (D::rF1, D::foo ()));
static_assert (is_accessible (D::rbar, D::foo ()));
static_assert (is_accessible (D::rbaz, D::foo ()));
static_assert (is_accessible (D::rg, D::foo ()));
static_assert (is_accessible (D::rj, D::foo ()));
static_assert (is_accessible (D::re, D::foo ()));
static_assert (is_accessible (D::rG, D::foo ()));
static_assert (is_accessible (D::rH, D::foo ()));
static_assert (is_accessible (D::rH1, D::foo ()));
static_assert (is_accessible (D::rqux, D::foo ()));
static_assert (is_accessible (D::rfreddy, D::foo ()));
static_assert (is_accessible (D::rh, D::foo ()));
static_assert (is_accessible (D::rk, D::foo ()));
static_assert (is_accessible (D::rf, D::foo ()));
static_assert (is_accessible (D::rI, D::foo ()));
static_assert (is_accessible (D::rJ, D::foo ()));
static_assert (is_accessible (D::rJ1, D::foo ()));
static_assert (is_accessible (D::rcorge, D::foo ()));
static_assert (is_accessible (D::rgarply, D::foo ()));
static_assert (is_accessible (D::ri, D::foo ()));
static_assert (is_accessible (D::rl, D::foo ()));
static_assert (is_accessible (D::rwaldo, D::foo ()));
constexpr auto uctx = access_context::unchecked ();
static_assert (is_accessible (D::rd, uctx));
static_assert (is_accessible (D::rE, uctx));
static_assert (is_accessible (D::rF, uctx));
static_assert (is_accessible (D::rF1, uctx));
static_assert (is_accessible (D::rbar, uctx));
static_assert (is_accessible (D::rbaz, uctx));
static_assert (is_accessible (D::rg, uctx));
static_assert (is_accessible (D::rj, uctx));
static_assert (is_accessible (D::re, uctx));
static_assert (is_accessible (D::rG, uctx));
static_assert (is_accessible (D::rH, uctx));
static_assert (is_accessible (D::rH1, uctx));
static_assert (is_accessible (D::rqux, uctx));
static_assert (is_accessible (D::rfreddy, uctx));
static_assert (is_accessible (D::rh, uctx));
static_assert (is_accessible (D::rk, uctx));
static_assert (is_accessible (D::rf, uctx));
static_assert (is_accessible (D::rI, uctx));
static_assert (is_accessible (D::rJ, uctx));
static_assert (is_accessible (D::rJ1, uctx));
static_assert (is_accessible (D::rcorge, uctx));
static_assert (is_accessible (D::rgarply, uctx));
static_assert (is_accessible (D::ri, uctx));
static_assert (is_accessible (D::rl, uctx));
static_assert (is_accessible (D::rwaldo, uctx));

struct K {
  static constexpr auto ctx = access_context::current ();
  static_assert (is_accessible (D::rd, ctx));
  static_assert (is_accessible (D::rE, ctx));
  static_assert (is_accessible (D::rF, ctx));
  static_assert (is_accessible (D::rF1, ctx));
  static_assert (is_accessible (D::rbar, ctx));
  static_assert (is_accessible (D::rbaz, ctx));
  static_assert (is_accessible (D::rg, ctx));
  static_assert (is_accessible (D::rj, ctx));
  static_assert (is_accessible (D::re, ctx));
  static_assert (is_accessible (D::rG, ctx));
  static_assert (is_accessible (D::rH, ctx));
  static_assert (is_accessible (D::rH1, ctx));
  static_assert (is_accessible (D::rqux, ctx));
  static_assert (is_accessible (D::rfreddy, ctx));
  static_assert (is_accessible (D::rh, ctx));
  static_assert (is_accessible (D::rk, ctx));
  static_assert (is_accessible (D::rf, ctx));
  static_assert (is_accessible (D::rI, ctx));
  static_assert (is_accessible (D::rJ, ctx));
  static_assert (is_accessible (D::rJ1, ctx));
  static_assert (is_accessible (D::rcorge, ctx));
  static_assert (is_accessible (D::rgarply, ctx));
  static_assert (is_accessible (D::ri, ctx));
  static_assert (is_accessible (D::rl, ctx));
  static_assert (is_accessible (D::rwaldo, ctx));
  static constexpr auto uctx = access_context::unprivileged ();
  static_assert (is_accessible (D::rd, uctx));
  static_assert (is_accessible (D::rE, uctx));
  static_assert (is_accessible (D::rF, uctx));
  static_assert (is_accessible (D::rF1, uctx));
  static_assert (is_accessible (D::rbar, uctx));
  static_assert (is_accessible (D::rbaz, uctx));
  static_assert (is_accessible (D::rg, uctx));
  static_assert (is_accessible (D::rj, uctx));
  static_assert (!is_accessible (D::re, uctx));
  static_assert (!is_accessible (D::rG, uctx));
  static_assert (!is_accessible (D::rH, uctx));
  static_assert (!is_accessible (D::rH1, uctx));
  static_assert (!is_accessible (D::rqux, uctx));
  static_assert (!is_accessible (D::rfreddy, uctx));
  static_assert (!is_accessible (D::rh, uctx));
  static_assert (!is_accessible (D::rk, uctx));
  static_assert (!is_accessible (D::rf, uctx));
  static_assert (!is_accessible (D::rI, uctx));
  static_assert (!is_accessible (D::rJ, uctx));
  static_assert (!is_accessible (D::rJ1, uctx));
  static_assert (!is_accessible (D::rcorge, uctx));
  static_assert (!is_accessible (D::rgarply, uctx));
  static_assert (!is_accessible (D::ri, uctx));
  static_assert (!is_accessible (D::rl, uctx));
  static_assert (is_accessible (D::rwaldo, uctx));
};

void
plugh ()
{
  static constexpr auto ctx = access_context::current ();
  static_assert (is_accessible (D::rd, ctx));
  static_assert (is_accessible (D::rE, ctx));
  static_assert (is_accessible (D::rF, ctx));
  static_assert (is_accessible (D::rF1, ctx));
  static_assert (is_accessible (D::rbar, ctx));
  static_assert (is_accessible (D::rbaz, ctx));
  static_assert (is_accessible (D::rg, ctx));
  static_assert (is_accessible (D::rj, ctx));
  static_assert (is_accessible (D::re, ctx));
  static_assert (is_accessible (D::rG, ctx));
  static_assert (is_accessible (D::rH, ctx));
  static_assert (is_accessible (D::rH1, ctx));
  static_assert (is_accessible (D::rqux, ctx));
  static_assert (is_accessible (D::rfreddy, ctx));
  static_assert (is_accessible (D::rh, ctx));
  static_assert (is_accessible (D::rk, ctx));
  static_assert (is_accessible (D::rf, ctx));
  static_assert (is_accessible (D::rI, ctx));
  static_assert (is_accessible (D::rJ, ctx));
  static_assert (is_accessible (D::rJ1, ctx));
  static_assert (is_accessible (D::rcorge, ctx));
  static_assert (is_accessible (D::rgarply, ctx));
  static_assert (is_accessible (D::ri, ctx));
  static_assert (is_accessible (D::rl, ctx));
  static_assert (is_accessible (D::rwaldo, ctx));
}

void
xyzzy ()
{
  static constexpr auto ctx = access_context::current ();
  static_assert (is_accessible (D::rd, ctx));
  static_assert (is_accessible (D::rE, ctx));
  static_assert (is_accessible (D::rF, ctx));
  static_assert (is_accessible (D::rF1, ctx));
  static_assert (is_accessible (D::rbar, ctx));
  static_assert (is_accessible (D::rbaz, ctx));
  static_assert (is_accessible (D::rg, ctx));
  static_assert (is_accessible (D::rj, ctx));
  static_assert (!is_accessible (D::re, ctx));
  static_assert (!is_accessible (D::rG, ctx));
  static_assert (!is_accessible (D::rH, ctx));
  static_assert (!is_accessible (D::rH1, ctx));
  static_assert (!is_accessible (D::rqux, ctx));
  static_assert (!is_accessible (D::rfreddy, ctx));
  static_assert (!is_accessible (D::rh, ctx));
  static_assert (!is_accessible (D::rk, ctx));
  static_assert (!is_accessible (D::rf, ctx));
  static_assert (!is_accessible (D::rI, ctx));
  static_assert (!is_accessible (D::rJ, ctx));
  static_assert (!is_accessible (D::rJ1, ctx));
  static_assert (!is_accessible (D::rcorge, ctx));
  static_assert (!is_accessible (D::rgarply, ctx));
  static_assert (!is_accessible (D::ri, ctx));
  static_assert (!is_accessible (D::rl, ctx));
  static_assert (is_accessible (D::rwaldo, ctx));
}

struct L : public D
{
  static constexpr auto ctx = access_context::current ();
  static_assert (is_accessible (D::rd, ctx));
  static_assert (is_accessible (D::rE, ctx));
  static_assert (is_accessible (D::rF, ctx));
  static_assert (is_accessible (D::rF1, ctx));
  static_assert (is_accessible (D::rbar, ctx));
  static_assert (is_accessible (D::rbaz, ctx));
  static_assert (is_accessible (D::rg, ctx));
  static_assert (is_accessible (D::rj, ctx));
  static_assert (!is_accessible (D::re, ctx));
  static_assert (is_accessible (D::rG, ctx));
  static_assert (is_accessible (D::rH, ctx));
  static_assert (is_accessible (D::rH1, ctx));
  static_assert (!is_accessible (D::rqux, ctx));
  static_assert (!is_accessible (D::rfreddy, ctx));
  static_assert (is_accessible (D::rh, ctx));
  static_assert (is_accessible (D::rk, ctx));
  static_assert (!is_accessible (D::rf, ctx));
  static_assert (!is_accessible (D::rI, ctx));
  static_assert (!is_accessible (D::rJ, ctx));
  static_assert (!is_accessible (D::rJ1, ctx));
  static_assert (!is_accessible (D::rcorge, ctx));
  static_assert (!is_accessible (D::rgarply, ctx));
  static_assert (!is_accessible (D::ri, ctx));
  static_assert (!is_accessible (D::rl, ctx));
  static_assert (is_accessible (D::rwaldo, ctx));
  friend void thud ();
};

consteval {
  constexpr auto ctx = L::ctx.via (^^L);
  static_assert (is_accessible (D::rd, ctx));
  static_assert (is_accessible (D::rE, ctx));
  static_assert (is_accessible (D::rF, ctx));
  static_assert (is_accessible (D::rF1, ctx));
  static_assert (is_accessible (D::rbar, ctx));
  static_assert (is_accessible (D::rbaz, ctx));
  static_assert (is_accessible (D::rg, ctx));
  static_assert (is_accessible (D::rj, ctx));
  static_assert (is_accessible (D::re, ctx));
  static_assert (is_accessible (D::rG, ctx));
  static_assert (is_accessible (D::rH, ctx));
  static_assert (is_accessible (D::rH1, ctx));
  static_assert (is_accessible (D::rqux, ctx));
  static_assert (is_accessible (D::rfreddy, ctx));
  static_assert (is_accessible (D::rh, ctx));
  static_assert (is_accessible (D::rk, ctx));
  static_assert (!is_accessible (D::rf, ctx));
  static_assert (!is_accessible (D::rI, ctx));
  static_assert (!is_accessible (D::rJ, ctx));
  static_assert (!is_accessible (D::rJ1, ctx));
  static_assert (!is_accessible (D::rcorge, ctx));
  static_assert (!is_accessible (D::rgarply, ctx));
  static_assert (!is_accessible (D::ri, ctx));
  static_assert (!is_accessible (D::rl, ctx));
  static_assert (is_accessible (D::rwaldo, ctx));
}

void
thud ()
{
  static constexpr auto ctx = access_context::current ();
  static_assert (is_accessible (D::rd, ctx));
  static_assert (is_accessible (D::rE, ctx));
  static_assert (is_accessible (D::rF, ctx));
  static_assert (is_accessible (D::rF1, ctx));
  static_assert (is_accessible (D::rbar, ctx));
  static_assert (is_accessible (D::rbaz, ctx));
  static_assert (is_accessible (D::rg, ctx));
  static_assert (is_accessible (D::rj, ctx));
  static_assert (!is_accessible (D::re, ctx));
  // TODO: clang++ disagrees with GCC on the D::r{G,H,H12,h,k} cases.
  static_assert (is_accessible (D::rG, ctx));
  static_assert (is_accessible (D::rH, ctx));
  static_assert (is_accessible (D::rH1, ctx));
  static_assert (!is_accessible (D::rqux, ctx));
  static_assert (!is_accessible (D::rfreddy, ctx));
  static_assert (is_accessible (D::rh, ctx));
  static_assert (is_accessible (D::rk, ctx));
  static_assert (!is_accessible (D::rf, ctx));
  static_assert (!is_accessible (D::rI, ctx));
  static_assert (!is_accessible (D::rJ, ctx));
  static_assert (!is_accessible (D::rJ1, ctx));
  static_assert (!is_accessible (D::rcorge, ctx));
  static_assert (!is_accessible (D::rgarply, ctx));
  static_assert (!is_accessible (D::ri, ctx));
  static_assert (!is_accessible (D::rl, ctx));
  static_assert (is_accessible (D::rwaldo, ctx));
  static constexpr auto ctx2 = access_context::current ().via (^^L);
  static_assert (is_accessible (D::rd, ctx2));
  static_assert (is_accessible (D::rE, ctx2));
  static_assert (is_accessible (D::rF, ctx2));
  static_assert (is_accessible (D::rF1, ctx2));
  static_assert (is_accessible (D::rbar, ctx2));
  static_assert (is_accessible (D::rbaz, ctx2));
  static_assert (is_accessible (D::rg, ctx2));
  static_assert (is_accessible (D::rj, ctx2));
  static_assert (is_accessible (D::re, ctx2));
  static_assert (is_accessible (D::rG, ctx2));
  static_assert (is_accessible (D::rH, ctx2));
  static_assert (is_accessible (D::rH1, ctx2));
  static_assert (is_accessible (D::rqux, ctx2));
  static_assert (is_accessible (D::rfreddy, ctx2));
  static_assert (is_accessible (D::rh, ctx2));
  static_assert (is_accessible (D::rk, ctx2));
  static_assert (!is_accessible (D::rf, ctx2));
  static_assert (!is_accessible (D::rI, ctx2));
  static_assert (!is_accessible (D::rJ, ctx2));
  static_assert (!is_accessible (D::rJ1, ctx2));
  static_assert (!is_accessible (D::rcorge, ctx2));
  static_assert (!is_accessible (D::rgarply, ctx2));
  static_assert (!is_accessible (D::ri, ctx2));
  static_assert (!is_accessible (D::rl, ctx2));
  static_assert (is_accessible (D::rwaldo, ctx2));
}

struct M : D
{
  static constexpr auto ctx = access_context::current ();
  static_assert (is_accessible (^^D::A::a, ctx));
  static_assert (is_accessible (^^D::B::b, ctx));
  static_assert (is_accessible (^^::C::c, ctx));
};

consteval {
  constexpr auto ctx = M::ctx.via (^^M);
  static_assert (!is_accessible (^^::C::c, ctx));
}

static union { int a; };

static_assert (is_accessible (^^a, gctx));
static_assert (is_accessible (^^a, D::foo ()));
static_assert (is_accessible (^^a, uctx));

struct N
{
  union { int a; };
protected:
  union { int b; };
private:
  union { int c; };
public:
  static constexpr auto ctx = access_context::current ();
  static constexpr info ra = ^^a;
  static constexpr info rb = ^^b;
  static constexpr info rc = ^^c;
};

struct O : public N
{
  static constexpr auto ctx = access_context::current ();
};

static_assert (is_accessible (N::ra, gctx));
// TODO: clang++ disagrees with g++ on the N::r{b,c} cases with gctx and O::ctx.
static_assert (!is_accessible (N::rb, gctx));
static_assert (!is_accessible (N::rc, gctx));
static_assert (is_accessible (N::ra, N::ctx));
// TODO: This case ICEs.
//static_assert (is_accessible (N::rb, N::ctx));
static_assert (is_accessible (N::rc, N::ctx));
static_assert (is_accessible (N::ra, N::ctx.via (^^N)));
static_assert (is_accessible (N::rb, N::ctx.via (^^N)));
static_assert (is_accessible (N::rc, N::ctx.via (^^N)));
static_assert (is_accessible (N::ra, O::ctx));
static_assert (!is_accessible (N::rb, O::ctx));
static_assert (!is_accessible (N::rc, O::ctx));
static_assert (is_accessible (N::ra, O::ctx.via (^^O)));
static_assert (is_accessible (N::rb, O::ctx.via (^^O)));
static_assert (!is_accessible (N::rc, O::ctx.via (^^O)));
static_assert (is_accessible (^^A::a, O::ctx));
static_assert (!is_accessible (^^A::a, O::ctx.via (^^O)));
