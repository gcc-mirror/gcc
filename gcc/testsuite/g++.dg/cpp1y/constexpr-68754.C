// PR c++/68754
// { dg-do compile { target c++14 } }

struct base { };
struct derived : base {
  constexpr derived& operator=(derived const&) = default;
};
