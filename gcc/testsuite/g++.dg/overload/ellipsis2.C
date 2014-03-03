// PR c++/60253

struct A
{
  ~A();
};

struct B
{
  B(...);
};

B b(0, A());  // { dg-error "cannot pass" }
