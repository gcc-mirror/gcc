// PR c++/44909
// { dg-options -std=c++11 }
// Declaring A<D<E>>'s copy ctor means choosing a ctor to initialize D<E>,
// which means choosing a ctor for C<B<E>>, which meant considering
// C(const B<E>&) which means choosing a ctor for B<E>, which means choosing
// a ctor for A<D<E>>.  Cycle.

template<typename T>
struct A
{
  T t;
};

template <typename T>
struct B
{
  typename T::U u;
};

template <typename T>
struct C
{
  C(const T&);
};

template <typename T>
struct D
{
  C<B<T> > v;
};

struct E {
  typedef A<D<E> > U;
};

extern A<D<E> > a;
A<D<E> > a2(a);
