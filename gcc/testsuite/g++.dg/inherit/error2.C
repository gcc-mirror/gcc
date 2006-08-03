// PR c++/28259
// { dg-do compile }

struct A
{
  virtual A* foo();
};

struct B : virtual A;  // { dg-error "before" }

struct C : A
{
  virtual B* foo();
};

B* C::foo() { return 0; }
