// PR c++/51009

struct A
{
  ~A();
};

struct B
{
  A a;
  B(const A& = A());
};

struct C
{
  B b1, b2;
};

C c = {};
