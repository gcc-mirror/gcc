// PR c++/58535
// { dg-do compile { target c++14 } }

struct A
{
  virtual void foo(auto); // { dg-error "auto|templates" }
};
