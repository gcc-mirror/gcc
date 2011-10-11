// { dg-do compile }

// Mangling of classes from std::decimal are special-cased.
// Derived from g++.dg/abi/mangle30.C.

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

struct A
{
  template <class T>
  struct B
  {
    typedef T myT;
  };
};

template <class T>
void f (T t, typename T::template B<std::decimal::decimal64>::myT u, typename T::template B<int>::myT v);

void foo ()
{
  f (A(), std::decimal::decimal64(0), 1);
}

// { dg-final { scan-assembler "_Z1fI1AEvT_NS1_1BIDdE3myTENS2_IiE3myTE" } }
