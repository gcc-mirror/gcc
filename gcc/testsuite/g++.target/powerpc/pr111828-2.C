/* { dg-do compile } */
/* { dg-skip-if "HTM inline asm supported" { powerpc_as_p10_htm } } */
/* Use -Wno-attributes to suppress the possible warning on always_inline.  */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -Wno-attributes" } */

/* Verify it emits error messages on non-empty inline asm.  */

#include <stddef.h>
#define HWY_PRAGMA(tokens) _Pragma (#tokens)
#define HWY_PUSH_ATTRIBUTES(targets_str) HWY_PRAGMA (GCC target targets_str)
__attribute__ ((always_inline)) void
PreventElision (int output) /* { dg-error "inlining failed in call to .* target specific option mismatch" } */
{
  asm("nop" : "+r"(output) : : "memory");
}
#define HWY_BEFORE_NAMESPACE() HWY_PUSH_ATTRIBUTES (",cpu=power10")
HWY_BEFORE_NAMESPACE () namespace detail
{
  template <typename, size_t, int> struct CappedTagChecker
  {
  };
}
template <typename T, size_t kLimit, int kPow2 = 0>
using CappedTag = detail::CappedTagChecker<T, kLimit, kPow2>;
template <typename, size_t, size_t kMinArg, class Test> struct ForeachCappedR
{
  static void Do (size_t, size_t)
  {
    CappedTag<int, kMinArg> d;
    Test () (int(), d);
  }
};
template <class Test> struct ForPartialVectors
{
  template <typename T> void operator() (T)
  {
    ForeachCappedR<T, 1, 1, Test>::Do (1, 1);
  }
};
struct TestFloorLog2
{
  template <class T, class DF> void operator() (T, DF)
  {
    PreventElision (0x10); /* { dg-message "called from here" } */
  }
};
void
TestAllFloorLog2 ()
{
  ForPartialVectors<TestFloorLog2> () (float());
}

