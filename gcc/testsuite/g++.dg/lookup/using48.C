// PR c++/51190
// { dg-do compile }

struct A
{
  int i;
};

template<typename> struct B
{
  A* p;
};

template<typename T> struct C : B<T>
{
  using B<T>::p;

  C() { p->i; }

  int i1, i2, i3, i4, i5;
};

C<A> c;
