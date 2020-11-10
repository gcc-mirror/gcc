// { dg-do compile { target c++20 } }
// Test [depr.arith.conv.enum] for <=>.

#include <compare>

enum E1 { e } e1;
enum E2 { f } e2;
static double d;

void
g ()
{
  void(e1 <=> e);
  e1 <=> d; // { dg-error "invalid operands of types .E1. and .double." }
  d <=> e1; // { dg-error "invalid operands of types .double. and .E1." }
  e <=> d; // { dg-error "invalid operands of types .E1. and .double." }
  d <=> e; // { dg-error "invalid operands of types .double. and .E1." }

  e <=> f; // { dg-error "invalid operands of types .E1. and .E2." }
  f <=> e; // { dg-error "invalid operands of types .E2. and .E1." }
  e1 <=> e2; // { dg-error "invalid operands of types .E1. and .E2." }
  e2 <=> e1; // { dg-error "invalid operands of types .E2. and .E1." }
}
