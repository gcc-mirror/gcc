// PR c++/60574
// { dg-do compile { target c++14 } }
// { dg-require-effective-target lto }
// { dg-options "-flto" }

struct A
{
  virtual auto foo() {}		// { dg-error "11:virtual.*deduced" }
};

struct B : A
{
  auto foo();
};

B b;
