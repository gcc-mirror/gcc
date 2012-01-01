// PR c++/19406
// { dg-do compile }

struct A
{
  virtual int foo();
  double d;
};

struct B : public A
{
  A::d; // { dg-warning "deprecated" }
};

B b;
