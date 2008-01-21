// PR c++/34486

template<typename> struct A
{
  typedef A* X;
};

template<typename T> struct B
{
  using A<T>::X::Y; // { dg-error "not a base type" }
};

B<int> b;
