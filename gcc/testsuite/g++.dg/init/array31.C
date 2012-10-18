// PR c++/54501
// { dg-options "" }

struct A
{
  int i[0];
  int j;
};

struct A a = { 1 };
