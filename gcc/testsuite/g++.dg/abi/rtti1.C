// Test that we don't emit the type_info for a polymorphic class other than
// with the vtable.

struct A {
  virtual ~A();
};

void f ()
{
  throw A();
}

// { dg-final { scan-assembler-dem-not {\ntypeinfo for A[: \t\n]} } }
