// PR c++/60574
// { dg-options "-flto" }
// { dg-do compile { target c++1y } }

struct A
{
  virtual auto foo() {}		// { dg-error "virtual.*deduced" }
};

struct B : A
{
  auto foo();
};

B b;
