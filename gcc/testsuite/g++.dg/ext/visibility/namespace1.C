// PR c++/21764
// Test for namespace visibility attribute semantics.

// { dg-require-visibility "" }
// { dg-final { scan-hidden "_ZN3foo1fEv" } }
// { dg-final { scan-hidden "_ZN3foo1gEv" } }
// { dg-final { scan-hidden "_ZN3foo1A1mEv" } }
// { dg-final { scan-hidden "_ZN3foo1tIiEEvv" } }
// { dg-final { scan-not-hidden "_ZN3foo1hEv" } }

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

