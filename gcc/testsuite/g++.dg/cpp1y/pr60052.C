// PR c++/60052
// { dg-do compile }
// { dg-options "-std=c++1y" }

struct A
{
  void foo(auto);
};

void A::foo(auto) {}

struct B
{
  void bar(auto);
};
