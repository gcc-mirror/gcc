// { dg-do compile }

// Mangling of classes from std::decimal are special-cased.
// Derived from g++.dg/abi/mangle13.C.

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
  std::decimal::decimal64 operator+();
  operator std::decimal::decimal64 ();
  template <typename T>
  std::decimal::decimal64 operator-();
};

typedef std::decimal::decimal64 (A::*P)();

template <P> struct S {};

template <typename T> void g (S<&T::template f<std::decimal::decimal64> >) {}
template <typename T> void g (S<&T::operator+ >) {}
template <typename T> void g (S<&T::operator std::decimal::decimal64>) {}
template <typename T> void g (S<&T::template operator- <std::decimal::decimal64> >) {}

template void g<A> (S<&A::f<std::decimal::decimal64> >);
template void g<A> (S<&A::operator+>);
template void g<A> (S<&A::operator std::decimal::decimal64>);

// { dg-final { scan-assembler "\n?_Z1gI1AEv1SIXadsrT_1fIDdEEE\[: \t\n\]" } }
// { dg-final { scan-assembler "\n?_Z1gI1AEv1SIXadsrT_plEE\[: \t\n\]" } }
