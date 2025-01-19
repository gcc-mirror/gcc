// { dg-do compile { target c++20 } }

// PR c++/106221
using T = decltype([](){});

template<typename Opts>
using foo = T;

using bar = foo<int>;

// PR c++/110680
template <auto X = []{}>
struct S {
  auto f() { return X; }
};

template <class T>
using C = decltype(S().f());

using D = C<int>;
