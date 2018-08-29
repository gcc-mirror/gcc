// PR c++/79056

template<class> struct A {};

template<class T> void foo(A<T>=A<T>()) {} // { dg-error "" }

void bar()
{
  foo(A<int>());		// { dg-error "" }
}
