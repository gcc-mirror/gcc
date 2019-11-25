// PR c++/92443
// { dg-do compile { target c++11 } }

struct a {
  constexpr a(long) : b() {}
  operator long() const;
  operator bool();
  constexpr friend a operator|(a, a c) { return c; }
  long b;
};
using d = a;
constexpr d e = 6, f = f | e;
