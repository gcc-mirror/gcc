// { dg-do run }
#include <cassert>

template<typename T, int I> int foo (T [I][I]) { return 0; }

template int foo (char [][6]);

template <typename T>
int foo (T *)
{
  return -1;
}

template <typename T>
int foo (T [3][3])
{
  return 1;
}

template <int I>
int foo (bool [I][I])
{
  return 2;
}

template <>
int foo (bool [3][2])
{
  return 3;
}

char x[3];
bool y[4];
bool z[3][2];

int a = foo (&x);
int b = foo (&y);
int c = foo (z);

int
main ()
{
  assert (a == 1);
  assert (b == 2);
  assert (c == 3);
}
