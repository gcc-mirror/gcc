// { dg-options -std=c++0x }

struct A
{
  int i;
  A(int);
};

struct B
{
  virtual void f();
  A ar[3];
};

extern B b;
B b2(b);
