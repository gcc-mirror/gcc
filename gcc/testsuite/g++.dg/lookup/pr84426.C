// PR c++/84426 ICE after error

struct A
{
  int foo; // { dg-message "previous" }
  virtual void foo(); // { dg-error "conflict" }
};

struct B : A {}; // ICED here
