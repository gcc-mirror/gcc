// { dg-do assemble  }

class A {
public:
  virtual int foo() = 0; // { dg-error "" } original definition
};

class B {
public:
    virtual double foo() = 0;
};

class C
  : public A, public B
{
public:
  virtual double foo() { return 2; } // { dg-error "" } conflicting return type
};

class D
  : public B, public A
{
public:
  virtual double foo() { return 2; } // { dg-error "" } conflicting return type
};
