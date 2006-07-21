/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility-inlines-hidden" } */
/* { dg-final { scan-not-hidden "_ZN1IIiE3fooEv" } } */
/* { dg-final { scan-not-hidden "_ZN1OIiE3fooEv" } } */
/* { dg-final { scan-hidden "_ZN1S3fooEv" } } */

template <class T>
struct O {
  static inline void foo() { }
};

template void O<int>::foo();

template <class T>
struct I {
  static inline void foo() { }
};

extern template void I<int>::foo();

struct S {
  static inline void foo() { }
};

void bar() {
  I<int>::foo();
  O<int>::foo();
  S::foo();
}
