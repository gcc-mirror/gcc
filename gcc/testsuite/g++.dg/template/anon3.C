// PR c++/28370
// { dg-do run }

namespace
{
  template<typename T> struct A { static int *a; };
  template<typename T> int *A<T>::a = 0;
}

int *
foo ()
{
  return A<int>::a;
}

int
main ()
{
  return foo() != 0;
}
