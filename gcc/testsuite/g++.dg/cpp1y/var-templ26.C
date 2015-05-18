// { dg-do compile { target c++14 } }

template <class T> const int V = 0;
template <> const int V<char> = 42;

template <class T>
struct A
{
  using N = T;
};

#define SA(X) static_assert((X),#X)
template <class T>
struct B
{
  SA(V<typename A<T>::N> == 42);
};

B<char> b;
