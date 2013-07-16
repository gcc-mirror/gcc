// PR c++/56358
// { dg-do compile { target c++11 } }

struct foo {
  explicit foo(int) {}
};

template<typename T>
struct bar: T {
  using T::T;

  // Bad
  explicit bar(): T(0) {}

  void baz()
  {
    // Also bad
    using qux = T;
  }
};

bar<foo> b, b2(42);
