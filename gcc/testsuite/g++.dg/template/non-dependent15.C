// PR c++/95468
// { dg-do compile { target c++11 } }

struct A {
  template <int N>
  static constexpr int condition() { return N; }
};

template <int> struct B {};

template <class>
using T = B<A::condition<int(1)>()>;
