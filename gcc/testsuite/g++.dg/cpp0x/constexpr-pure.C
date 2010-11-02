// { dg-options -std=c++0x }

struct A
{
  virtual void f() = 0;
};

struct B: A
{
  void f() { }
};

B b;
