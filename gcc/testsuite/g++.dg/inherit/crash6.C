// PR c++/70202

class A
{
  virtual void foo () { }
};
class B : public A, A { };  // { dg-error "duplicate base type" }

B b1, &b2 = b1;
A a = b2;
