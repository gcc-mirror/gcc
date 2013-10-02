// PR c++/58535
// { dg-options "-std=gnu++1y" }

struct A
{
  virtual void foo(auto); // { dg-error "templates" }
};
