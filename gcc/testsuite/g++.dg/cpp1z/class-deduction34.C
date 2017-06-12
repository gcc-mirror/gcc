// { dg-options -std=c++1z }

template <class T>
struct A
{
  template <class U>
  static constexpr bool B = U();

  template <class U, bool V = B<U>>
  A(T, U);
};

A a (1,2);
