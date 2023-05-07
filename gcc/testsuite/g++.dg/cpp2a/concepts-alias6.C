// Verify we can check satisfaction of a non-dependent member alias
// template-id whose constraints don't depend on outer template
// parameters ahead of time.
// { dg-do compile { target c++20 } }

template<class T>
struct A {
  template<int N> requires (N > 0)
  using at = T;

  void f() {
    using ty1 = at<0>; // { dg-error "constraint" }
    using ty2 = at<1>;
  }
};
