// { dg-options -std=c++1z }

template <class T>
struct A
{
  int i;
  A(...);			// { dg-message "candidate" }
};

A a(42);			// { dg-error "" }

template <class T>
A(T) -> A<T>;

A a2(42);
