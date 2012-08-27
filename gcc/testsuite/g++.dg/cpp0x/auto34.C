// PR c++/51421
// { dg-do compile { target c++11 } }

int foo1(int);

void bar1()
{
  auto i = foo1(i);   // { dg-error "before deduction" }
}

struct A {};

A foo2(A);

void bar2()
{
  auto a = foo2(a);   // { dg-error "before deduction" }
}
