// PR c++/107938
// { dg-do compile { target c++14 } }

struct Q {
  int n;
  constexpr const Q* operator()(int) const { return this; }
};

constexpr Q q{};

template<int>
constexpr const Q* p = q(0);
