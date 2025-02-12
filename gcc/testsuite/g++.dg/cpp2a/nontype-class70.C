// PR c++/113800
// P2308R1 - Template parameter initialization
// { dg-do compile { target c++20 } }

struct S {
  int a = 0;
  int b = 42;
};

template <S t>
struct A {
  static constexpr auto a = t.a;
  static constexpr auto b = t.b;
};

static_assert(A<{}>::a == 0);
static_assert(A<{}>::b == 42);
static_assert(A<{.a = 3}>::a == 3);
static_assert(A<{.b = 4}>::b == 4);

template<S = {}>
struct D1 {};

template<S = {1, 2}>
struct D2 {};

template <S = {.b = 5}>
struct D3 {};

struct E {};

struct I {
  constexpr I(E) {};
};

template<typename T, T>
struct W {};

void
g ()
{
  D1<> d1;
  D2<> d2;
  D3<> d3;

  W<I, {E{}}> w;
}
