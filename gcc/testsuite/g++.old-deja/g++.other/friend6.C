// { dg-do assemble  }
// Origin: Martin v. Löwis  <loewis@informatik.hu-berlin.de>
// Test for resolution of core issue 125.

struct A{
  struct B{};
};

A::B C();

namespace B{
  A C();
}

class Test{
  friend A (::B::C)();  // Ok
  friend A::B (::C)();  // Ok
  friend A::B::C();     // { dg-error "" } no A::B::C
};
