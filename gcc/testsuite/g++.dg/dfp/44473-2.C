// { dg-do compile }

// Mangling of classes from std::decimal are special-cased.

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

int bar (const std::decimal::decimal64 & x) { }

int foo ()
{
  std::decimal::decimal64 x(0);
  bar (x);
}

// { dg-final { scan-assembler "_Z3barRKDd:" } }
