// Test that non-inline default causes the function to be defined even if
// it isn't used.

// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZN1AC1Ev" } }

struct A
{
  A();
};

A::A() = default;

