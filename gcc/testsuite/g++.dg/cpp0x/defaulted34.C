// { dg-options -std=c++0x }
// { dg-final { scan-assembler "__cxa_deleted_virtual" } }

struct A
{
  virtual void f();
  virtual ~A() = delete;
};

void A::f() {}
