// { dg-do assemble  }

template <class T = int> struct A;

template <class T> struct B
{
  friend class A<T>;
};

template class B<int>;
