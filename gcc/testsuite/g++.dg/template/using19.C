// PR c++/23211
// { dg-do compile }

struct A { int x; };

template <class T>
struct B: A
{
  using T::x;
};

B<A> b;
