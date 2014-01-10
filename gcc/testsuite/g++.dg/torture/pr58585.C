// { dg-do compile }
// { dg-options "-fpic" { target fpic } }
struct A
{
  virtual void foo() {}
  void bar();
};
void A::bar() { foo(); }

struct B : virtual A
{
  virtual void foo() {}
  char c;
};

struct C : virtual B
{
  C();
};
C::C() { bar(); }
