// CWG3153
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <compare>

struct A {
  decltype (^^::) a = ^^::;
  consteval A () {}
  bool operator== (const A &) const = default;	// { dg-bogus "function of consteval-only type must be declared 'consteval'" }
};

template <typename T, T V>
struct B
{
  T b = V;
  consteval B () {}
  bool operator== (const B &) const = default;
};

struct C {
  decltype (^^::) c= ^^::;
  int d = 0;
  consteval C () {}
  consteval bool operator== (const C &x) const { return d == x.d; }
  consteval auto operator<=> (const C &x) const { return d <=> x.d; }
};

struct D : public C {
  int e = 0;
  consteval D () {}
  auto operator<=> (const D &) const = default;
};

consteval
{
  A a;
  A b;
  if (a != b) throw 1;
  B <decltype (^^::), ^^::> c;
  B <decltype (^^::), ^^::> d;
  if (c != d) throw 2;
  D e;
  D f;
  if (e != f) throw 3;
  f.d = 2;
  if (e >= f) throw 4;
  f.d = 0;
  f.e = -1;
  if (e <= f) throw 5;
}
