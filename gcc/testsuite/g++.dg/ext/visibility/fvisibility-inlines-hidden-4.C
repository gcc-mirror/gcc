/* PR c++/30066: Test that -fvisibility-inlines-hidden affects functions. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility-inlines-hidden" } */
/* { dg-final { scan-hidden "_Z3barv" } } */
/* { dg-final { scan-not-hidden "_ZZ3barvE1n" } } */
/* { dg-final { scan-not-hidden "_Z3fooIiEvv" } } */
/* { dg-final { scan-hidden "_Z3fooIvEvv" } } */
/* { dg-final { scan-hidden "_ZZN1A5innerEvE1n" } } */

inline int * bar()
{
  static int n;
  return &n;
}

template <class T>
inline void foo() { }

template void foo<int>();

namespace A __attribute__ ((visibility ("hidden")))
{
  inline int * inner()
  {
    static int n;
    return &n;
  }
}

int main(void)
{
  bar();
  foo<void>();
  A::inner();
  return 0;
}
