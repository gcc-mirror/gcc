// PR c++/60994
// { dg-do compile { target c++11 } }

enum struct A
{
  n = 3
};

A
foo()
{
  int A;
  return A::n;
}
