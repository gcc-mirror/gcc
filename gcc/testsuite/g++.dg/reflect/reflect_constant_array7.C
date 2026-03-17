// PR c++/124425
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct A { };

template <typename T>
struct B
{
  using value_type = T;
  using difference_type = int;
  struct C
  {
    T i;
    constexpr operator T () const { return i; }
  };
  constexpr B &operator++ () { ++i; return *this; }
  constexpr B operator++ (int) { return B { i++ }; }
  constexpr C operator * () const { return C { i }; }
  constexpr bool operator== (A) const { return i == 10; }
  constexpr B () : p (new char (42)) {}
  constexpr B (const B &x) : i (x.i), p (new char (42)) {}
  constexpr ~B () { delete p; }
  int i = 0;
  char *p;
};

template <typename T>
struct D
{
  constexpr B <T> begin () const { return {}; };
  constexpr A end () const { return {}; };
};

struct E
{
  constexpr E (int x) : a (x), b (x + 1), c (x + 2) {}
  constexpr E (const E &x) : a (x.a), b (x.b), c (x.c) {}
  constexpr ~E () {}
  int a, b, c;
};

constexpr auto &d = [: std::meta::reflect_constant_array (D <int> {}) :];
constexpr auto &e = [: std::meta::reflect_constant_array (D <E> {}) :];
consteval {
  for (int i = 0; i < 10; ++i)
    if (d[i] != i || e[i].a != i || e[i].b != i + 1 || e[i].c != i + 2)
      throw 1;
}
