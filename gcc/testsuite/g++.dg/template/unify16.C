// { dg-do run }
#include <cassert>

template <typename T>
struct Foo
{
  static int foo (T) { return 0; }
};

template <typename T, int I>
struct Foo<T[I]>
{
  static int foo (T[I]) { return 1; }
};

template <int I>
struct Foo<char[I]>
{
  static int foo (char[I]) { return 2; }
};

template <typename T>
struct Foo<T[5]>
{
  static int foo (T[5]) { return 3; }
};

template <>
struct Foo<char[5]>
{
  static int foo (char[5]) { return 4; }
};

template <>
struct Foo<const char[5]>
{
  static int foo (const char[5]) { return 5; }
};

int a = Foo<const char[5]>::foo (0);
int b = Foo<char[5]>::foo (0);
int c = Foo<bool[5]>::foo (0);
int d = Foo<char[4]>::foo (0);
int e = Foo<bool[4]>::foo (0);
int f = Foo<char[]>::foo (0);

int
main (void)
{
  assert (a == 5);
  assert (b == 4);
  assert (c == 3);
  assert (d == 2);
  assert (e == 1);
  assert (f == 0);
}
