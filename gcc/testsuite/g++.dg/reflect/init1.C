// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test various forms of initialization with reflections.

#include <initializer_list>

using info = decltype(^^int);

struct S { int i; };
struct X {
  consteval operator info() { return ^^int; }
};

struct Y {
  info i = ^^int;
};

constexpr Y y;

struct W {
  consteval W() { }
  decltype(^^::) i = ^^W::i;
};

constinit info r = ^^int;
constinit info *p = &r;

consteval void
f ()
{
  constexpr auto refl = ^^S;
  auto a1 = refl;
  auto a2(refl);
  auto a3 = { refl };
  auto a4{refl};

  constexpr decltype(refl) arr[] = { refl };
  constexpr auto ra = arr[0];
  typename [: ra :] s = { .i = 42 };

  constexpr static auto srefl = ^^S;
  constexpr static auto *p = &srefl;
  constexpr auto *const *q = &p;
  typename [: **q :] s2 = { .i = 42 };
}

void
g ()
{
  f ();
}

void
h ()
{
  X x;
  typename [: x :] i = 42;
  typename [: X{} :] j = i;
}
