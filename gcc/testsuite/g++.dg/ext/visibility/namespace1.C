// PR c++/21764
// Test for namespace visibility attribute semantics.

// { dg-do compile { target i?86-*-linux* x86_64-*-linux* powerpc*-*-linux* } }
// { dg-final { scan-assembler "hidden\[ \t\]*_ZN3foo1fEv" } }
// { dg-final { scan-assembler "hidden\[ \t\]*_ZN3foo1gEv" } }
// { dg-final { scan-assembler "hidden\[ \t\]*_ZN3foo1A1mEv" } }
// { dg-final { scan-assembler "hidden\[ \t\]*_ZN3foo1tIiEEvv" } }
// { dg-final { scan-assembler-not "hidden\[ \t\]*_ZN3foo1hEv" } }

namespace foo __attribute ((visibility ("hidden")))
{
  int f() { }
  void g();
  template <typename T> void t() { }
  class A
  {
    void m ();
  };
}

namespace foo
{
  void h() {}
}

void foo::g() { t<int> (); }

void foo::A::m() { }

