// PR c++/53856

template<typename T>
struct A
{
  struct B;
};

template<typename T = int>
struct A<T>::B  // { dg-error "default argument" }
{
  int i;
};

A<int>::B b = { };
