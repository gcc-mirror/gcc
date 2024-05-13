// PR c++/114903
// { dg-do compile { target c++20 } }

template <typename T>
struct Key {
  Key(int);
};

class Forward {};

template <typename T>
constexpr bool C = __is_same(T, Forward);

template <typename Z>
struct Outer {
  template <typename U>
  struct Foo {
    Foo(U);
    U u;
  };

  template <typename V>
    requires(C<Z>)
  Foo(V) -> Foo<int>;
};

template <typename Y>
struct T {
  template <typename Y2>
  struct T2 {
    template <typename K>
    using AFoo = Outer<Y2>::template Foo<K>;
  };
};

T<Forward>::T2<Forward>::AFoo a(1.0);
using type = decltype(a);
using type = Outer<Forward>::Foo<int>;
