// PR c++/121702
// { dg-additional-options "-fmodules" }

import b;
namespace gmf::blah {
  constexpr int g() { return 123; }
}
namespace gmf::other {
  constexpr int h() { return 99; }
}
namespace x {
  constexpr int i() { return 5; }
}
static_assert(b::f() == 42);
static_assert(b::g() == 123);
static_assert(c::other::h() == 99);
static_assert(y::i() == 5);
