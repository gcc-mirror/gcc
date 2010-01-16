// PR c++/42761
// { dg-options "-std=c++0x" }

template<typename _Tp> _Tp* fn();

template <class T> struct A
{
  template <class U,
	    class S = decltype(fn<T>())>
  struct B { };
};

A<int> a;
