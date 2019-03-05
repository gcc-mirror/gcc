// { dg-do compile { target c++17 } }

struct S { int m; static int s; } s;

struct T: S { };

template<int* p> class X { };
template<const int S::*> class M {};
template<const S&> class SP {};

constexpr int *p = &S::s;
constexpr int S::*sp = &T::m;
constexpr S& sr = s;
constexpr S& sf() { return s; }

// { dg-final { scan-assembler "_Z1g1XIXadL_ZN1S1sEEEE" } }
void g (X<&s.s>) { }
// { dg-final { scan-assembler "_Z1f1XIXadL_ZN1S1sEEEE" } }
void f (X<p>) { }
// { dg-final { scan-assembler "_Z1f1MIXadL_ZN1S1mEEEE" } }
void f (M<sp>) {}
// { dg-final { scan-assembler "_Z1f2SPIL_Z1sEE" } }
void f (SP<sr>) {}
// { dg-final { scan-assembler "_Z1g2SPIL_Z1sEE" } }
void g (SP<sf()>) {}
