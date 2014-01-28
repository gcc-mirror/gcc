// { dg-options "-std=c++1y" }

struct A
{
  operator auto();
};

// { dg-final { scan-assembler "_ZN1AcvDaEv" } }
A::operator auto() { return 42; }
