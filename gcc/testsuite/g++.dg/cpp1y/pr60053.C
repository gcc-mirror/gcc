// PR c++/60053
// { dg-do compile { target c++1y } }
// { dg-options "" }

struct A
{
  void foo(auto);
};

void A::foo(auto) {}

template<typename> struct B
{
  template<typename T> void bar(auto);
};
