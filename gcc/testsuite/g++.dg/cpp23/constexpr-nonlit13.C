// PR c++/106649
// P2448 - Relaxing some constexpr restrictions
// { dg-do compile { target c++23 } }
// { dg-options "-Winvalid-constexpr" }

constexpr volatile int i = 10;

constexpr int
bar ()
{
  return i;  // { dg-warning "lvalue-to-rvalue conversion of a volatile lvalue" }
}

constexpr int x = bar (); // { dg-error "called in a constant expression" }
