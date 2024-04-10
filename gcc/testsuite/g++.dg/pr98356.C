// PR c++/98356
// { dg-do compile { target c++11 } }

template <template <typename> class T> struct S {
  using A = T<int>;
  using A::foo;
  void foo ();
  void bar () {foo.}  // { dg-error "invalid use of member function" }
};
