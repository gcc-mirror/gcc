// PR c++/60052
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

struct A
{
  void foo(auto);
};

void A::foo(auto) {}

struct B
{
  void bar(auto);
};
