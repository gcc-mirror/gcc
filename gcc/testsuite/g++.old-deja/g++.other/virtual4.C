// Build don't link:

class A {
public:
  virtual int foo() = 0; // ERROR - original definition
};

class B {
public:
    virtual double foo() = 0;
};

class C
  : public A, public B
{
public:
  virtual double foo() { return 2; } // ERROR - conflicting return type
};

class D
  : public B, public A
{
public:
  virtual double foo() { return 2; } // ERROR - conflicting return type
};
