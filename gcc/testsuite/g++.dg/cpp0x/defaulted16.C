// Test that non-inline default causes the function to be defined even if
// it isn't used.

// { dg-options -std=c++0x }
// { dg-final { scan-assembler "_ZN1AC1Ev" } }

struct A
{
  A();
};

A::A() = default;

