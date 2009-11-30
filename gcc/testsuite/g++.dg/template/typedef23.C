// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/42069
// { dg-do compile }

struct A
{
  static const int N = 0;
};

template<int> struct B {};

template<typename T, int>
struct C
{
  typedef T U;
  B<U::N> b;
};

template<typename T>
struct C<T*, 0>
{
  B<T::N> b;
};

C<A*, 0> c;
