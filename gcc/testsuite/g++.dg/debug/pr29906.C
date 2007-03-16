// { dg-do compile }
// { dg-options "-g -fno-emit-class-debug-always" }

struct A{
  typedef int T;
  virtual ~A();
};
struct B:public A{
  using A::T;
};
