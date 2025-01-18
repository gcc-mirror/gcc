// PR c++/118513
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int a, b;
  template <int I> int &get () { if (I == 0) return a; else return b; }
};

template <> struct std::tuple_size <A> { static const int value = 2; };
template <int I> struct std::tuple_element <I, A> { using type = int; };

namespace {
auto [x, y] = A { 42, 43 };
}
