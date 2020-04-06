// PR c++/93805
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wnoexcept" }

struct B
{
  B() {}
};

struct C
{
  B b = B();
};

C c; // { dg-bogus "noexcept-expression" }
