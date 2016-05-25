// PR c++/55992
// { dg-do compile { target c++11 } }

template <int> struct A { };

template <int I>
struct B
{
  static constexpr int f (int i) { return i; }

  template <int J>
  using C = A<f (J)>;

  C<I> c;
};
