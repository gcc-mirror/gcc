// PR c++/51474
// { dg-do compile { target c++11 } }

struct A
{
  virtual int foo() = 0;
  int i = foo();  // { dg-warning "pure virtual" }
};
