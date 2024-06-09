// PR c++/114901
// { dg-do compile { target c++20 } }

template <class D>
constexpr bool C = sizeof(D);

template <typename U>
struct T {
  template<typename V, typename N>
  struct Foo {
    Foo(V, N);
  };

  template<typename X, typename N>
  requires (C<N>) // removes the require-clause will make the crash disappear
  Foo(X, N) -> Foo<X, N>;

  template <typename Y, typename Y2, int N = sizeof(Y2)>
  using AFoo = Foo<decltype(N), Y2>;
};

T<double>::AFoo s{1, 2}; // { dg-error "deduction|no match" }
