// PR c++/28259
// { dg-do compile }

struct A
{
  virtual A* foo();    // { dg-message "overridden" }
};

struct B : virtual A;  // { dg-error "before" }

struct C : A
{
  virtual B* foo();    // { dg-error "invalid covariant" }
};

B* C::foo() { return 0; }
