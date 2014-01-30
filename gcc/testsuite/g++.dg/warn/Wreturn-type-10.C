// PR c++/59916
// { dg-options "-Os -Wreturn-type" }

class A {};

struct B : virtual public A
{
  B();
  virtual ~B();
};

B::B() {}
B::~B() {}
