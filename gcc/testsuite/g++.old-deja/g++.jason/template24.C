// { dg-do run  }
// Bug: g++ doesn't find the conversion from ostream_withassign to ostream.

#include <iostream>

template <class T>
struct A {
  T t;
};

template <class T>
std::ostream & operator<< (std::ostream & os, A<T> & a)
{
  os << a.t;
  return os;
}

int main ()
{
  A<int> a = { 1 };
  std::cout << a << std::endl;
}

