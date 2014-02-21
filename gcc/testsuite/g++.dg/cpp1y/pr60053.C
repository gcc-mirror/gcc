// PR c++/60053
// { dg-do compile }
// { dg-options "-std=c++1y" }

struct A
{
  void foo(auto);
};

void A::foo(auto) {}

template<typename> struct B
{
  template<typename T> void bar(auto);
};
