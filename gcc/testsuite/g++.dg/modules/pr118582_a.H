// PR c++/118582
// { dg-additional-options "-fmodule-header -fno-module-lazy -std=c++20" }
// { dg-module-cmi {} }

template <typename> struct I;

template <typename T>
  requires (!requires { T::x; })
struct I<T> {
  using type = int;
};

template <typename>
struct R {
  friend void f();
};
