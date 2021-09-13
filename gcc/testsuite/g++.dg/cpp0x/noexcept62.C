// PR c++/98333
// { dg-do compile { target c++11 } }

struct T {
  template <bool N>
  struct S {
    S () noexcept (N) {}
  };
  int a = __has_nothrow_constructor (S<true>);
};
