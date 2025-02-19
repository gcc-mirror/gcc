// PR c++/118190
// { dg-do compile { target c++11 } }

struct S {
  template<typename T>
  struct S5 {
    void f1() noexcept(noexcept(i)) { }
    int i;
  };
  S5<int> s5;
  static_assert (noexcept(s5.f1()), ""); // { dg-error "not available until end of class|static assertion failed" }
};
