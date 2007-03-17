// { dg-do compile }
// { dg-options "-g" }

struct A{
  typedef int T;
  virtual ~A();
};
struct B:public A{
  using A::T;
};
