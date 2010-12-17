// Test for constant initialization of class with vtable
// { dg-options "-std=c++0x -save-temps" }
// { dg-final { scan-assembler-not "static_initialization" } }
// { dg-final cleanup-saved-temps }
// { dg-do run }

int r = 1;
// implicit default constructor for A and B is constexpr
struct A { virtual void f() {} };
struct B: A { virtual void f() { r = 0; } };

B b;

int main()
{
  b.f();
  return r;
}
