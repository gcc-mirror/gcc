// PR c++/84689
// { dg-do compile { target c++11 } }

struct base {
  void operator()();
};

struct a : base { };
struct b : base { };

struct f : a, b {
  using a::operator();
  using b::operator();
};

template <class T> auto g(int) -> decltype(T()());
template <class T> auto g(...) -> int;

using type = decltype(g<f>(0));
using type = int;
