// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on types.

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

using size_t = decltype(sizeof(int));
using info = decltype(^^void);
using T = int;

struct S { int i; };

constexpr auto g1 = ^^unsigned;
constexpr auto g2 = ^^S;
constexpr static auto g3 = ^^unsigned;
constexpr static auto g4 = ^^S;
constexpr info g5 = ^^void;
constexpr info g6 = ^^decltype(42);
constexpr info g7 = ^^T;
constexpr info g8 = ^^decltype(^^int);
constexpr info g9 = ^^void() const & noexcept;

[: g1 :] u1;  // { dg-error "expected unqualified-id" }
typename [: g1 :] u2;

namespace N {
  [: g1 :] nu1;  // { dg-error "expected unqualified-id" }
  typename [: g1 :] nu2;
}

void
f1 ()
{
  constexpr auto r1 = ^^int;
  [: r1 :] v1 = 42;  // { dg-error "expected a reflection of an expression" }
  typename [: r1 :] v1t = 42;
  same_type<decltype(v1t), int>();

  const typename [: r1 :] v2 = 42;
  same_type<decltype(v2), const int>();

  const volatile typename [: r1 :] v3 = 42;
  same_type<decltype(v3), const volatile int>();
  const typename [: r1 :] *v4 = &v1t;
  same_type<decltype(v4), const int *>();

  constexpr auto r2 = ^^double;
  [: r2 :] v5 = 42.2;  // { dg-error "expected a reflection of an expression" }
  typename [: r2 :] v5t = 42.2;
  same_type<decltype(v5t), double>();

  [: r2 :] &v6 = v5t;  // { dg-error "expected a reflection of an expression|.v6. was not declared" }
  typename [: r2 :] &v6t = v5t;
  same_type<decltype(v6t), double &>();

  constexpr auto r3 = ^^S;
  [: r3 :] v7 = { 42 };  // { dg-error "expected a reflection of an expression" }
  typename [: r3 :] v7t = { 42 };
  same_type<decltype(v7t), S>();
  const typename [: r3 :] v8 = { 42 };
  same_type<decltype(v8), const S>();

  constexpr auto r4 = ^^long long int;
  [: r4 :] v9 = 0ll;  // { dg-error "expected a reflection of an expression" }
  typename [: r4 :] v9t = 0ll;
  same_type<decltype(v9t), long long int>();

  constexpr auto r5 = ^^const int;
  [: r5 :] v10 = 0;  // { dg-error "expected a reflection of an expression" }
  typename [: r5 :] v10t = 0;
  same_type<decltype(v10t), const int>();

  constexpr auto r6 = ^^volatile short;
  [: r6 :] v11 = 0;  // { dg-error "expected a reflection of an expression" }
  typename [: r6 :] v11t = 0;
  same_type<decltype(v11t), volatile short>();

  constexpr auto r7 = ^^bool;
  [: r7 :] v12 = 0;  // { dg-error "expected a reflection of an expression" }
  typename [: r7 :] v12t = 0;
  same_type<decltype(v12t), bool>();

  constexpr auto r8 = ^^wchar_t;
  [: r8 :] v13 = 0;  // { dg-error "expected a reflection of an expression" }
  typename [: r8 :] v13t = 0;
  same_type<decltype(v13t), wchar_t>();

  constexpr auto r9 = ^^decltype(sizeof 0);
  [: r9 :] v14 = 0;  // { dg-error "expected a reflection of an expression" }
  typename [: r9 :] v14t = 0;
  same_type<decltype(v14t), size_t>();

  constexpr auto r10 = ^^signed;
  [: r10 :] v15 = 0;  // { dg-error "expected a reflection of an expression" }
  typename [: r10 :] v15t = 0;
  same_type<decltype(v15t), int>();

}

void
f2 ()
{
  typename [:^^char:] c1 = '*';
  same_type<decltype(c1), char>();

  const typename [:^^char:] c2 = '*';
  same_type<decltype(c2), const char>();

  typename [:^^int:]* c3 = nullptr;
  same_type<decltype(c3), int *>();

  typename [:^^int:] c4 = 42;
  same_type<decltype(c4), int>();

  typename [:^^int:] &c5 = c4;
  same_type<decltype(c5), int &>();

  typename [:^^int:] arr1[10];
  same_type<decltype(arr1), int[10]>();
}

void
f3 ()
{
  typename [: g1 :] v1 = 42;
  same_type<decltype(v1), unsigned>();
  typename [: g3 :] v2 = 42;
  same_type<decltype(v2), unsigned>();
  typename [: g2 :] v3 = { 42 };
  same_type<decltype(v3), S>();
  typename [: g4 :] v4 = { 42 };
  same_type<decltype(v4), S>();
}

void
f4 ()
{
  static constexpr auto r = ^^unsigned;
  constexpr auto p = &r;
  [: *p :] i1 = 0u;  // { dg-error "expected a reflection of an expression" }
  typename [: *p :] i2 = 0u;
}

constexpr void
f5 ()
{
  static constexpr auto r = ^^unsigned;
  constexpr auto p = &r;
  [: *p :] i1 = 0u;  // { dg-error "expected a reflection of an expression" }
  typename [: *p :] i2 = 0u;
}

consteval void
f6 ()
{
  static constexpr auto r = ^^unsigned;
  constexpr auto p = &r;
  [: *p :] i1 = 0u;  // { dg-error "expected a reflection of an expression" }
  typename [: *p :] i2 = 0u;
  auto t = r;
  ^^int;
}

void
f7 ()
{
  {
    {
      constexpr auto r = ^^int;
      typename [: r :] v = 42;
      same_type<decltype(v), int>();
    }
  }
}

enum E { X, Y };
enum class SE { yay, nay };

void
f8 ()
{
  constexpr auto r = ^^E;
  [: r :] e = Y;  // { dg-error "expected a reflection of an expression" }
  typename [: r :] et;

  constexpr auto r2 = ^^SE;
  [: r2 :] e2 = SE::yay;  // { dg-error "expected a reflection of an expression" }
  typename [: r2 :] et2;
}
