// PR c++/84813
// { dg-do compile }
// { dg-options "-g -std=c++14" }

template <typename>
struct P {};

template <int>
struct F {
  using type = int;
};

template <typename T>
void
apply ()
{
  constexpr int N = T::N;
  [] (typename F<N>::type)
  {
    auto f = [] () {};
    P<decltype (f)>{};
  };
}

struct A {
  static constexpr int N = 1;
};

void
instantiate ()
{
  apply<A> ();
}
