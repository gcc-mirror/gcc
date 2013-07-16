// PR c++/56395

struct A
{
  template <class T> struct B { };
};

template <class T> struct D { };

template <class T, class U> struct C
{
  typedef T _Type;
  typedef typename T::template B<_Type> _BType;
  D<_BType> d;
};

template <class T> struct C<T,T>
{
  typedef T _Type;
  typedef typename T::template B<_Type> _BType;
  D<_BType> d;
};

C<A,A> c;
