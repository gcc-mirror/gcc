//PR c++/11507
// Origin: kai-gcc-bugs@khms.westfalen.de and bangerth@dealii.org
//The new parser used to fail on this.

// { dg-do compile }

namespace NS
{
  void foo(bool arg1);
}

namespace M {
  namespace K {
    bool Bc(bool x);
  }

  void bar() {
    NS::foo (K::Bc(true)); // GCC could not find K or Bc.
  }
}
