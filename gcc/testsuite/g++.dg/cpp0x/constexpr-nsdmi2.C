// PR c++/107079
// { dg-do compile { target c++11 } }

struct X {
  const X* x = this;
};
constexpr const X& x = X{};
// TODO: The assert should pass once we implement DR2126 (c++/101588).
//static_assert(x.x == &x);
