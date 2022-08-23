// PR c++/105623
// { dg-do compile { target c++14 } }

template <class T>
auto g(T fn) { }

template<typename>
struct base {
  static auto value() { }
};

struct S : base<void> {
  static void f() { g(value); }
};
