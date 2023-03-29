// PR c++/65327
// { dg-do compile { target c++11 } }
// DR1688 says that constexpr can be used together with volatile.

constexpr volatile int i = 10;

void
foo ()
{
  constexpr volatile int j = 5;
  static constexpr volatile int k = 5;
}

constexpr volatile int // { dg-warning "deprecated" "" { target c++2a } }
bar ()
{
  return i;  // { dg-error "lvalue-to-rvalue conversion of a volatile lvalue" "" { target c++20_down } }
}
