// { dg-do assemble  }
template <class T> struct B { static const int i = 3; };
template <class T> struct A { static const int i = B<T>::i; };
enum { i = A<int>::i };
