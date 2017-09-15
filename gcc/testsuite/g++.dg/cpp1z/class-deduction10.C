// { dg-options -std=c++17 }

template <class T>
struct A
{
  int i;
  A(...);
};

template <class T>
A(T) -> A<T> { }		// { dg-error "1:function body" }
