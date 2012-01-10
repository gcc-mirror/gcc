// { dg-options "-std=c++0x" }

struct A { int i; };
// { dg-final { scan-assembler "_Z2f1Ii1AEDTdsfp_fp0_ET0_MS2_T_" } }
template <class T, class U> auto f1 (U u, T U::* p) -> decltype(u.*p) { return u.*p; }
// { dg-final { scan-assembler "_Z2f2Ii1AEDTpmfp_fp0_EPT0_MS2_T_" } }
template <class T, class U> auto f2 (U* u, T U::* p) -> decltype(u->*p) { return u->*p; }

int main()
{
  A a = {};
  f1(a, &A::i);
  f2(&a, &A::i);
}
