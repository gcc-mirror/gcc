// { dg-do run  }
// PRMS Id: 5286
// Bug: g++ forgets side-effects of object in call to nonexistent destructor.

#include <new>

int r;

template <class T> struct A {
  T *p;
  int i;
  A() { i = 0; p = (T*) new char[sizeof (T)]; new (p + i++) T; }
  ~A() { p[--i].~T(); r = i; }
};

int main()
{
  { A<int> a; }

  int* p = (int*) new char[sizeof (int)];
  new (p + r++) int;
  typedef int I;
  p[--r].~I();
  
  return r;
}
