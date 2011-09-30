// { dg-do compile }

// Mangling of classes from std::decimal are special-cased.
// Derived from g++.dg/abi/mangle15.C.

namespace std {
  namespace decimal {
    class decimal64 {
      public:
	typedef float __decfloat64 __attribute__ ((mode (DD)));
	explicit decimal64 (float __r):__val (__r) {}
      private:
	__decfloat64 __val;
    };
  }
}

struct A {
  template <typename T> std::decimal::decimal64 f ();
};

typedef std::decimal::decimal64 (A::*P)();

template <P> struct S {};

void g (S<&A::f<std::decimal::decimal64> >) {}

// { dg-final { scan-assembler "\n?_Z1g1SIXadL_ZN1A1fIDdEEDdvEEE\[: \t\n\]" } }
