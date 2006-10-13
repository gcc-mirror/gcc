// PR c++/28506

struct A
{
  virtual void* foo() = 1; // { dg-error "pure" }
};

struct B
{
  void operator()()() = 1; // { dg-error "pure|function|initializer" }
};
