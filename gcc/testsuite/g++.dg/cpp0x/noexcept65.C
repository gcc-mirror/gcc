// PR c++/98899
// { dg-do compile { target c++11 } }

template <int __v> struct integral_constant {
  static constexpr int value = __v;
};

struct S {
  template<class> struct B {
    B() noexcept(noexcept(x));
    int x;
  };
  struct A : B<int> {
    A() : B() {}
  };
};

struct S2 {
  template<class> struct B {
    B() noexcept(integral_constant<false>::value);
  };
  struct A : B<int> {
    A() : B() {}
  };
};

struct S3 {
  template<class> struct B {
    B() noexcept(b);
  };
  struct A : B<int> {
    A() : B() {}
  };
  static constexpr bool b = false;
};
