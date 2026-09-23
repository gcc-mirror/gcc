// PR c++/126406
// { dg-do compile { target c++14 } }

template<class T> auto g(T) { }
static_assert(g<int>, "");

template<class T>
struct B {
  static auto g(T) { }
};
static_assert(B<int>::g, "");
