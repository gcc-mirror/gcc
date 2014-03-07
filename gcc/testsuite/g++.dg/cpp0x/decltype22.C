// PR c++/42761
// { dg-do compile { target c++11 } }

template<typename _Tp> _Tp* fn();

template <class T> struct A
{
  template <class U,
	    class S = decltype(fn<T>())>
  struct B { };
};

A<int> a;
