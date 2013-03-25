// Test that the anonymous namespace isn't mangled with random characters,
// but also doesn't get mixed up with an anonymous namespace in another
// translation unit.
// { dg-final { scan-assembler "\\*N12_GLOBAL__N_11AE" } }

namespace {
  struct A
  {
    virtual void f();
  };

  void A::f() { }
}

extern void g();

int main()
{
  try {
    try {
      g();
    } catch (A) { __builtin_abort(); }
  } catch (...) { }
}
