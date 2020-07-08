// { dg-do compile { target c++20 } }

template <class T, int N>
struct A
{
  T ar[N];
};

A a = { "foo" };

template<class, class> struct same;
template<class T> struct same<T,T> {};
same<decltype (a.ar), char[4]> s;

