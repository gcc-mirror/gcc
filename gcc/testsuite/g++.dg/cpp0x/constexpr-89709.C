// PR c++/89709
// { dg-do compile { target c++11 } }
// { dg-options "-O" }

struct A { int i; };
A a;

constexpr int *
foo ()
{
  return &a.i;
}

bool
bar ()
{
  return foo () == &a.i;
}
