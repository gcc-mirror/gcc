// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

info g = info ();
info g2;

struct S { info a, b; };
constexpr S s = { ^^::S };
static_assert (s.b == info ());

constexpr S s2[2] = { { ^^::S, ^^::S } };
static_assert (s2[1].a == info ());

S gs = {};
info garr[3] = { };

constexpr info arr[3] = { ^^::S };
static_assert (arr[1] == arr[2]);
static_assert (arr[2] == info ());

struct T { info r; T () = default; };
constexpr T t {};
static_assert (t.r == info ());

bool h (S s) { return s.b == info (); }

constexpr info r = info ();
static_assert (r == info ());

info *k () { static info lg = info (); return &lg; }
