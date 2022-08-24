// PR c++/104079
// { dg-do compile { target c++11 } }
// A variant of noexcept74.C where f is a function template.

template<bool B>
struct AT {
  template<class...> static void f() noexcept(B);

  void g() noexcept(noexcept(f())) {
    static_assert(noexcept(f()), "");
  }
};
