// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "__cxa_deleted_virtual" } }

struct A
{
  virtual void f();
  virtual ~A() = delete;
};

void A::f() {}
