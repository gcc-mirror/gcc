// PR c++/46289
// { dg-options -std=c++0x }

struct A
{
  int i[2];
};

struct B
{
  A a;
  B(): a({{1,2}}) { }
};
