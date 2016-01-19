// { dg-do run }
#include <cassert>

template<typename T, int I> int foo (T [I][I]) { return 0; }

template<typename T>
int foo (T [3][2])
{
  return 1;
}

template <>
int foo (bool [3][2])
{
  return 2;
}

bool z[3][2];

int a = foo (z);

int
main ()
{
  assert (a == 2);
}
