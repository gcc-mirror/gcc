// PR c++/60167

template <int& F>
struct Foo {
  typedef int Bar;

  static Bar cache;
};

template <int& F> typename Foo<F>::Bar Foo<F>::cache;
