// PR c++/27427
// { dg-do compile }

struct A;

template<A&> void foo();

A a;  // { dg-error "incomplete type|storage size" }

void bar()
{
  foo<a>();  // { dg-error "no matching function" }
}
