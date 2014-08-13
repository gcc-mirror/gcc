// PR c++/60253
// { dg-options "-Wconditionally-supported" }

struct A
{
  ~A();
};

struct B
{
  B(...);
};

B b(0, A());  // { dg-message "pass" }
