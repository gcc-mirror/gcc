// Bug: g++ doesn't find the conversion from ostream_withassign to ostream.

#include <iostream.h>

template <class T>
struct A {
  T t;
};

template <class T>
ostream & operator<< (ostream & os, A<T> & a)
{
  os << a.t;
  return os;
}

int main ()
{
  A<int> a = { 1 };
  cout << a << endl;
}
