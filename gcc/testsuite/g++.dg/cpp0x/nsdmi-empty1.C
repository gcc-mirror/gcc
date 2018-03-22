// PR c++/82764
// { dg-do compile { target c++11 } }

struct Empty {};
struct Empty2 : Empty {};

struct A : Empty2
{
  int x {1};
  int y {2};
};

struct B
{
  A a {};
};

B b;
