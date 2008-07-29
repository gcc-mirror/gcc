// PR c++/36852

#include "array-1.H"

template <class T>
T
A::foo (T t[3][3])
{
  return t[0][1];
}

int
main ()
{
}
