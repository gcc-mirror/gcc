// CWG1996
// { dg-do compile { target c++11 } }

struct D { constexpr D() {} } d;
struct S {
  template <class T>
  constexpr operator T& () const { return d; }
};
constexpr S s;
constexpr const D &dr1(s);
static_assert (&dr1 == &d, "");
constexpr const D &dr2{s};
static_assert (&dr2 == &d, "");
