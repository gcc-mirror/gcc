// { dg-do compile { target c++14 } }

struct A
{
  operator auto();
};

// { dg-final { scan-assembler "_ZN1AcvDaEv" } }
A::operator auto() { return 42; }
