// PR c++/60052
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct A
{
  void foo(auto);
};

void A::foo(auto) {}

struct B
{
  void bar(auto);
};
