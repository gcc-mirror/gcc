// { dg-do compile }

// Mangling of classes from std::decimal are special-cased.
// Derived from g++.dg/abi/mangle20-1.C.

namespace std {
  namespace decimal {
    class decimal64 {
      public:
	typedef float __decfloat64 __attribute__ ((mode (DD)));
	explicit decimal64 (int __r):__val (__r) {}
      private:
	__decfloat64 __val;
    };
  }
}

template <int I> void f(std::decimal::decimal64 (*)[2]) {}
template <int I> void g(std::decimal::decimal64 (*)[I+2]) {}

static const std::decimal::decimal64 I(1);
static const std::decimal::decimal64 J(2);

template void f<1>(std::decimal::decimal64 (*)[2]);
template void g<1>(std::decimal::decimal64 (*)[3]);

//  { dg-final { scan-assembler "\n_?_Z1fILi1EEvPA2_Dd\[: \t\n\]" } }
//  { dg-final { scan-assembler "\n_?_Z1gILi1EEvPAplT_Li2E_Dd\[: \t\n\]" } }
