// Verify mangling of class literals of types with ctors.
// { dg-do compile { target c++2a } }

struct A
{
  char i;
  constexpr A (): i (1) { }
  constexpr A (int i): i (i) { }
};

struct B { A a[3]; };

template <B> struct X { };

void f___ (X<B{{ }}>) { }
// { dg-final { scan-assembler "_Z4f0001XIXtl1BEEE" } }

void f0__ (X<B{{ 0 }}>) { }
// { dg-final { scan-assembler "_Z4f0__1XIXtl1BtlA3_1AtlS1_EtlS1_Lc1EEtlS1_Lc1EEEEEE" } }

void f00_ (X<B{{ 0, 0 }}>) { }
// { dg-final { scan-assembler "_Z4f00_1XIXtl1BtlA3_1AtlS1_EtlS1_EtlS1_Lc1EEEEEE" } }

void f000 (X<B{{ 0, 0, 0 }}>) { }
// { dg-final { scan-assembler "_Z4f0001XIXtl1BEEE" } }

void f1__ (X<B{{ 1 }}>) { }
// { dg-final { scan-assembler "_Z4f1__1XIXtl1BtlA3_1AtlS1_Lc1EEtlS1_Lc1EEtlS1_Lc1EEEEEE" } }
