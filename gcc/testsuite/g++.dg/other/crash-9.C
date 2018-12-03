// Origin: PR c++/42915
// { dg-do compile }

template <typename T>
class A
{
  template <typename U>
  class B
  {
    B foo();
  };
};
template <typename T> template <typename U>
A<T>::template B<U> A<T>::B<U>::foo() {}

