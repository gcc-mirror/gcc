// { dg-options -std=c++11 }

struct B
{
  constexpr operator int() { return 4; }
};

template <int I>
struct C;

template<>
struct C<4> { typedef int TP; };

template <class T>
struct A
{
  constexpr static B t = B();
  C<t>::TP tp;
};

A<B> a;
