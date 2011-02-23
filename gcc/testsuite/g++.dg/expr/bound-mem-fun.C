// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/38228
// { dg-do compile }

struct A
{
  A ();
  template<typename T> A(T);
};

struct B
{
  int foo();
};

A a = B().*(&B::foo); // { dg-error "invalid use of non-static member function" }


