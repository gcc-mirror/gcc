// PR c++/104079
// { dg-do compile { target c++11 } }

template<bool B>
struct AT {
  static void f() noexcept(B);

  void g() noexcept(noexcept(f())) {
    static_assert(noexcept(f()), "");
  }
};
