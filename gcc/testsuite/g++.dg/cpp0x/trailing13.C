// PR c++/69139
// { dg-do compile { target c++11 } }

struct X {
  auto get(int) const & noexcept -> int { return {}; }
  auto get(int) && throw () -> long { return {}; }
};

template <class R> auto f(auto (X::*)(int) const & -> R) -> R {}

using I = decltype(f(&X::get));
using I = int;
