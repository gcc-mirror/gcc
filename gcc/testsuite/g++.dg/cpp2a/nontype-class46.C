// PR c++/93383
// { dg-do compile { target c++20 } }

template <int> struct A {};

template <A a> struct B {
  void foo(B<+a>);
  void bar(B<a.x>);
  template <class T> using type = B<T{}>;
  template <class> static inline auto y = A{0}; // { dg-error "deduction|no match" }
};
