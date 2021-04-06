// PR c++/91241
// { dg-do compile { target c++11 } }

struct A {
  int *b(const int & = []() -> int { return 0; }(),
	 const int & = []() -> int { return 0; }());
};
int *A::b(const int &, const int &) { b(); return 0; }
// { dg-final { scan-assembler "_ZN1A1bERKiS1_" } }
// { dg-final { scan-assembler "_ZZN1A1bERKiS1_Ed_NKUlvE_clEv" } }
// { dg-final { scan-assembler "_ZZN1A1bERKiS1_Ed0_NKUlvE_clEv" } }
