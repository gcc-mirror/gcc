// PR c++/51431

struct A                  // { dg-message "note" }
{
  virtual void foo() = 0; // { dg-message "note" }
};

struct B
{
  A a;           // { dg-error "abstract" }
  B() : a() {}   // { dg-error "abstract" }
};
