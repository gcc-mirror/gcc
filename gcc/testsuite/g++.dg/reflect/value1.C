// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -g" }

using info = decltype(^^::);
using size_t = decltype (sizeof 0);
void *operator new (size_t, void *);

consteval int imm_fn() { return 42; }
constexpr int not_imm_fn() { return 42; }
int fn();
auto id (info i) { return i; }

struct S { info r; };
struct DS : S { int i; };
constexpr DS cds{ { ^^int }, 0 };
DS dsn{ { info{} }, 0 };
DS ds{ { ^^int }, 0 };	// { dg-error "initialized with a consteval-only value" }
auto basep1 = (S *) &cds; // { dg-error "initialized with a consteval-only value" }
constexpr auto basep2 = (S *) &cds;

struct O { consteval int f() const { return 1; } };
auto pm1 = &O::f;	      // { dg-error "address of an immediate function" }
constexpr auto pm2 = &O::f;   // { dg-bogus "returns address" "" { xfail *-*-* } }

struct C {
  info r;
  constexpr C(info x) : r(x) {}
};
constexpr C c1(^^int);
C c2(info{});
C c3(^^int);		// { dg-error "initialized with a consteval-only value" }

struct CB : C {
  constexpr CB(info x) : C(x) {}
};
constexpr CB cb1(^^int);
CB cb2(info{});
CB cb3(^^int);		// { dg-error "initialized with a consteval-only value" }

struct Deleg {
  info r;
  constexpr Deleg() : Deleg(info{}) {}
  constexpr Deleg(info x) : r(x) {}
};
constexpr Deleg cdlg{};
Deleg dlg{};

struct N {
  info i = ^^int;
};

struct N2 {
  info i = ^^int;	// { dg-error "consteval-only value" }
  constexpr N2() {}     // not immediate-escalating
  constexpr N2(info x) : i(x) {}
};
constexpr N2 cn2a;

struct N3 {
  info i = ^^int;
  consteval N3() {}
  constexpr N3(info x) : i(x) {}
};
constexpr N3 cn3a;

struct N4 {
  info i = ^^int;
  constexpr N4() = default;
  constexpr N4(info x) : i(x) {}
};
constexpr N4 cn4a;
N4 n4a;	    // { dg-error "initialized with a consteval-only value" }

struct N5 {
  static constexpr info si = ^^int;
  static info si2;
};
info N5::si2 = ^^int;	// { dg-error "initialized with a consteval-only value" }

struct L {
  static constexpr info si = info{};
  static info si2;
};
info L::si2 = info{};

template<typename T, typename U>
struct V {
  T t;
  U u;
};
auto v1 = V{info{}, 42};
constexpr auto v2 = V{^^int, 42};
auto v3 = V{^^int, 42};	  // { dg-error "initialized with a consteval-only value" }

struct Wrapper { const info &r; };
Wrapper w1{ ^^int };	    // { dg-error "initialized with a consteval-only value" }
constexpr Wrapper w2{ ^^int };

info a;
info *pa;
info **ppa;
S b;
S b2{};

template<typename> struct E;
void foo (E<int> *);

auto normal() -> void {
  info c;
  S d;
}

constexpr auto r = ^^int;
auto e1 = ^^int; // { dg-error "initialized with a consteval-only value" }
auto en = info{};

const info &ref1 = r;	    // { dg-error "initialized with a consteval-only value" }
constexpr const info &ref2 = r;

constexpr info arr[] = { ^^int, ^^char };
constexpr info e5 = arr[0];
info e6 = arr[0];    // { dg-error "initialized with a consteval-only value" }
auto epast1 = arr + 2;	// { dg-error "initialized with a consteval-only value" }
constexpr auto epast2 = arr + 2;
info narr[] = { info{}, info{} };
info narr2[3];
info narr3[3]{};

// See PR124249.  This is OK because the initializer is constant:
// namespace scope, (r, 42) is a constant expression -> i2 has constant
// initialization -> the initializer is manifestly constant-evaluated
// -> r is in an immediate function context -> we accept.
int i1 = (^^int, 42);
int i2 = (r, 42);
int i3 = (info{}, 42);
// No constant initialization, so not MCE.
int i4 = (r, fn());    // { dg-error "initialized with a consteval-only value" }

const int &gref = (r, 42);

constexpr N cng{};
N ng;	// { dg-error "initialized with a consteval-only value" }
static N nsg;	// { dg-error "initialized with a consteval-only value" }

constexpr const info *p = &r;
const info *q = &r;   // { dg-error "initialized with a consteval-only value" }

struct Ptr {
  const info *p;
};
auto ptr1 = Ptr{ .p = nullptr };
constexpr auto ptr2 = Ptr{ .p = &r };
auto ptr3 = Ptr{ .p = &r };    // { dg-error "initialized with a consteval-only value" }

struct Pair { info r; int i; };
constexpr Pair p1 = { ^^int, 0 };
Pair p2 = { info{}, 0 };
Pair p3 = { ^^int, 0 };	  // { dg-error "initialized with a consteval-only value" }
auto [sb1, sb2] = Pair{^^int, 0}; // { dg-error "initialized with a consteval-only value" }
constexpr auto [sb3, sb4] = Pair{^^int, 0};
auto [sb5, sb6] = Pair{info{}, 0};

struct Nested { Pair inner; int c; };
auto ne = Nested{{^^int, 0}, 1};  // { dg-error "initialized with a consteval-only value" }
constexpr auto cne = Nested{{^^int, 0}, 1};
auto ne2 = Nested{{info{}, 0}, 1};
constexpr auto cne2 = Nested{{info{}, 0}, 1};

consteval auto getr () { return ^^int; }
consteval auto getnr () { return info{}; }
constexpr auto r1 = getr ();
auto r2 = getnr ();
// r3 is an object with a constituent consteval-only value
// but is not associated with a constexpr variable.
auto r3 = getr ();	  // { dg-error "initialized with a consteval-only value" }

union U { info r; int i; };
// Only the active member counts here.
constexpr U u1 = { .i = 0 };
constexpr U u2 = { .r = ^^int };
auto u3 = U{ .i = 0 };
auto u4 = U{ .r = ^^int };	// { dg-error "initialized with a consteval-only value" }
auto u5 = U{ .r = info{} };

template<bool B>
void
tfn ()
{
  info rn = B ? info{} : ^^int;	  // { dg-error "initialized with a consteval-only value" }
  info ri = B ? ^^int : info{};	  // { dg-error "initialized with a consteval-only value" }
}

template<bool B>
constexpr void
ctfn ()
{
  info rn = B ? info{} : ^^int;
  info ri = B ? ^^int : info{};
}

template<bool B>
void
uninst ()
{
  info rn = B ? info{} : ^^int;
  info ri = B ? ^^int : info{};
}

template<typename>
constexpr void
esc ()
{
  N n;
}

void
dflt (info x = ^^int) // { dg-error "consteval-only value outside an immediate function context" }
{
}

void
dflt_null (info x = info{})
{
}

consteval void
dflt_ok (info x = ^^int)
{
}

template<auto V> struct TC { };
auto e7 = TC<^^int>();
auto e7n = TC<info{}>();

template<Pair> struct D { };
auto e8 = D<Pair{^^int, 42}>();
auto e8n = D<Pair{info{}, 42}>();

void
g (bool b)
{
  info null;
  null = info();

  constexpr auto lr = ^^int;
  auto e2 = ^^int;	  // { dg-error "initialized with a consteval-only value" }
  auto en = info{};
  static constexpr info sg = ^^int;
  static info sg2 = ^^int;  // { dg-error "initialized with a consteval-only value" }
  const info &lref1 = r;    // { dg-error "initialized with a consteval-only value" }
  constexpr const info &lref2 = r;

  // Don't accept these: the "has constant initialization" rule applies to
  // variables at namespace scope only.
  int e3 = (^^int, 42);	    // { dg-error "initialized with a consteval-only value" }
  int e4 = (r, 42);	    // { dg-error "initialized with a consteval-only value" }
  int e5 = (r, fn());	    // { dg-error "initialized with a consteval-only value" }
  // But these are static.
  static int e6 = (^^int, 42);
  static int e7 = (r, 42);
  static int e8 = (r, fn());       // { dg-error "initialized with a consteval-only value" }

  // grok_reference_init turns the init into
  // (const int &) &D.2686 and so store_init_value won't see
  // any consteval-only value.
  const int &ref = (r, 42);	  // { dg-error "initialized with a consteval-only value" "" { xfail *-*-* } }

  info rn = b ? info{} : ^^int;	  // { dg-error "initialized with a consteval-only value" }
  info ri = b ? ^^int : info{};	  // { dg-error "initialized with a consteval-only value" }

  constexpr info larr[] = { ^^int, ^^char };
  constexpr info le5 = arr[0];
  info le6 = arr[0];    // { dg-error "initialized with a consteval-only value" }
  info lnarr[] = { info{}, info{} };

  constexpr const info *lp = &r;
  const info *lq = &r;   // { dg-error "initialized with a consteval-only value" }

  constexpr C lc1(^^int);
  C lc2(info{});
  C lc3(^^int);		// { dg-error "initialized with a consteval-only value" }

  constexpr CB lcb1(^^int);
  CB lcb2(info{});
  CB lcb3(^^int);		// { dg-error "initialized with a consteval-only value" }

  Wrapper lw1{ ^^int };	    // { dg-error "initialized with a consteval-only value" }
  // w2 at block scope has this initializer:
  // {.r=(<<< Unknown tree: lang_type >>> &) &_ZGRL2w2_}
  // which is reduced_constant_expression_p but here we have
  // {.r=(<<< Unknown tree: lang_type >>> &) &D.2718}
  // which is not reduced_constant_expression_p.  The error
  // also happens without reflection:
  // struct W { const int &r; }
  // void h () { constexpr W lw{42}; }
  constexpr Wrapper lw2{ ^^int }; // { dg-error "not a constant expression" }

  constexpr N lcng{};
  N lng;	// { dg-error "initialized with a consteval-only value" }

  constexpr Pair p4 = { ^^int, 0 };
  Pair p5 = { info{}, 0 };
  Pair p6 = { ^^int, 0 };	  // { dg-error "initialized with a consteval-only value" }
  auto [lsb1, lsb2] = Pair{^^int, 0}; // { dg-error "initialized with a consteval-only value" }
  constexpr auto [lsb3, lsb4] = Pair{^^int, 0};
  auto [lsb5, lsb6] = Pair{info{}, 0};

  auto lptr1 = Ptr{ .p = nullptr };
  constexpr auto lptr2 = Ptr{ .p = &r };
  auto lptr3 = Ptr{ .p = &r };    // { dg-error "initialized with a consteval-only value" }

  auto lne = Nested{{^^int, 0}, 1};  // { dg-error "initialized with a consteval-only value" }
  constexpr auto lcne = Nested{{^^int, 0}, 1};
  auto lne2 = Nested{{info{}, 0}, 1};
  constexpr auto lcne2 = Nested{{info{}, 0}, 1};

  constexpr DS lcds{ { ^^int }, 0 };
  DS ldsn{ { info{} }, 0 };
  DS lds{ { ^^int }, 0 };	// { dg-error "initialized with a consteval-only value" }
  auto lbasep1 = (S *) &cds; // { dg-error "initialized with a consteval-only value" }
  constexpr auto lbasep2 = (S *) &cds;

  constexpr U lu1 = { .i = 0 };
  constexpr U lu2 = { .r = ^^int };
  auto lu3 = U{ .i = 0 };
  auto lu4 = U{ .r = ^^int };	// { dg-error "initialized with a consteval-only value" }
  auto lu5 = U{ .r = info{} };

  constexpr auto lr1 = getr ();
  auto lr2 = getnr ();
  auto lr3 = getr ();	  // { dg-error "initialized with a consteval-only value" }

  // We think that [expr.const.imm]/2 gives us the rationale for
  // rejecting this code, even though r isn't ODR-used here.
  ^^int;  // { dg-error "consteval-only value outside an immediate function context" }
  r;	  // { dg-error "consteval-only value outside an immediate function context" }
  (^^int, true);  // { dg-error "consteval-only value outside an immediate function context" }
  (r, true);	  // { dg-error "consteval-only value outside an immediate function context" }
  lr;	  // { dg-error "consteval-only value outside an immediate function context" }

  a = info();
  a = ^^int; // { dg-error "consteval-only value outside an immediate function context" }
  a = r; // { dg-error "consteval-only value outside an immediate function context" }

  a == a;
  a == ^^int; // { dg-error "consteval-only value outside an immediate function context" }
  ^^int != info{};  // { dg-error "consteval-only value outside an immediate function context" }
  ^^int == ^^int;   // { dg-error "consteval-only value outside an immediate function context" }
  constexpr bool b1 = ^^int != info{};
  constexpr bool b2 = ^^int == ^^int;

  a == r; // { dg-error "consteval-only value outside an immediate function context" }

  constexpr N cln;
  N ln;	// { dg-error "initialized with a consteval-only value" }

  new (&a) info();
  new (&a) info(^^int); // { dg-error "consteval-only value outside an immediate function context" }
  new info(info{});
  new info(^^int);	// { dg-error "consteval-only value outside an immediate function context" }

  S{};
  S{.r={}};
  S{.r=^^int}; // { dg-error "consteval-only value outside an immediate function context" }

  auto lam = [] { info l = ^^int; };
  lam ();

  auto lam2 = [] { return ^^int; };
  auto e9 = lam2 ();	// { dg-error "initialized with a consteval-only value" }
  constexpr auto e10 = lam2 ();
  constexpr auto lam3 = [i = ^^int]{ return i; };
  auto lam4 = [i = ^^int]{ return i; };	// { dg-error "initialized with a consteval-only value" }
  auto lam5 = [lr]{ return lr; };	// { dg-error "initialized with a consteval-only value" }
  constexpr auto lam6 = [lr]{ return lr; };

  if consteval {
    auto ifc = ^^int;
  }

  auto x1 = imm_fn ();
  auto x2 = []() consteval { return ^^int; }();  // { dg-error "initialized with a consteval-only value" }

  id (null);
  id (^^int);	// { dg-error "consteval-only value outside an immediate function context" }
  dflt ();
  dflt_null ();
  dflt_ok ();
  tfn<true>();
  ctfn<true>();
  esc<int>();
}

consteval void
ceg (bool b)
{
  info null;
  null = info();

  constexpr auto lr = ^^int;
  auto e2 = ^^int;
  static constexpr info sg = ^^int;
  static info sg2 = ^^int;
  const info &lref1 = r;
  constexpr const info &lref2 = r;

  int e3 = (^^int, 42);
  int e4 = (r, 42);
  int e5 = (r, fn());
  static int e6 = (^^int, 42);
  static int e7 = (r, 42);
  static int e8 = (r, fn());

  const int &ref = (r, 42);

  info rn = b ? info{} : ^^int;
  info ri = b ? ^^int : info{};

  constexpr info larr[] = { ^^int, ^^char };
  constexpr info le5 = arr[0];
  info le6 = arr[0];
  info lnarr[] = { info{}, info{} };

  constexpr const info *lp = &r;
  const info *lq = &r;

  constexpr C lc1(^^int);
  C lc2(info{});
  C lc3(^^int);

  constexpr CB lcb1(^^int);
  CB lcb2(info{});
  CB lcb3(^^int);

  Wrapper lw1{ ^^int };

  constexpr N lcng{};
  N lng;

  constexpr Pair p4 = { ^^int, 0 };
  Pair p5 = { info{}, 0 };
  Pair p6 = { ^^int, 0 };
  auto [lsb1, lsb2] = Pair{^^int, 0};
  constexpr auto [lsb3, lsb4] = Pair{^^int, 0};
  auto [lsb5, lsb6] = Pair{info{}, 0};

  auto lptr1 = Ptr{ .p = nullptr };
  constexpr auto lptr2 = Ptr{ .p = &r };
  auto lptr3 = Ptr{ .p = &r };

  auto lne = Nested{{^^int, 0}, 1};
  constexpr auto lcne = Nested{{^^int, 0}, 1};
  auto lne2 = Nested{{info{}, 0}, 1};
  constexpr auto lcne2 = Nested{{info{}, 0}, 1};

  constexpr DS lcds{ { ^^int }, 0 };
  DS ldsn{ { info{} }, 0 };
  DS lds{ { ^^int }, 0 };
  auto lbasep1 = (S *) &cds;
  constexpr auto lbasep2 = (S *) &cds;

  constexpr U lu1 = { .i = 0 };
  constexpr U lu2 = { .r = ^^int };
  auto lu3 = U{ .i = 0 };
  auto lu4 = U{ .r = ^^int };
  auto lu5 = U{ .r = info{} };

  constexpr auto lr1 = getr ();
  auto lr2 = getnr ();
  auto lr3 = getr ();

  ^^int;
  r;
  lr;
  (^^int, true);
  (r, true);

  a = info();
  a = ^^int;
  a = r;

  a == a;
  a == ^^int;
  ^^int != info{};
  ^^int == ^^int;
  constexpr bool b1 = ^^int != info{};
  constexpr bool b2 = ^^int == ^^int;

  a == r;

  constexpr N cln;
  N ln;

  new (&a) info();
  new (&a) info(^^int);
  new info(info{});
  new info(^^int);

  S{};
  S{.r={}};
  S{.r=^^int};

  auto lam = [] { info l = ^^int; };
  lam ();

  auto lam2 = [] { return ^^int; };
  auto e9 = lam2 ();
  constexpr auto e10 = lam2 ();
  constexpr auto lam3 = [i = ^^int]{ return i; };
  auto lam4 = [i = ^^int]{ return i; };
  auto lam5 = [lr]{ return lr; };
  constexpr auto lam6 = [lr]{ return lr; };

  if consteval {
    auto ifc = ^^int;
  }

  auto x1 = imm_fn ();
  auto x2 = []() consteval { return ^^int; }();

  id (null);
  id (^^int);
}

constexpr void
cg (bool b)
{
  int e3 = (^^int, 42);	    // { dg-error "initialized with a consteval-only value" }
  int e4 = (r, 42);	    // { dg-error "initialized with a consteval-only value" }
  int e5 = (r, fn());	    // { dg-error "initialized with a consteval-only value" }
  static int e6 = (^^int, 42);
  static int e7 = (r, 42);
  static int e8 = (r, fn());       // { dg-error "initialized with a consteval-only value" }

  info rn = b ? info{} : ^^int;	// { dg-error "initialized with a consteval-only value" }
  info ri = b ? ^^int : info{};	// { dg-error "initialized with a consteval-only value" }

  constexpr auto lr1 = getr ();
  auto lr2 = getnr ();
  auto lr3 = getr ();	  // { dg-error "consteval-only value" }
}
