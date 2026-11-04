/* PR rtl-optimization/128007 */
/* { dg-do run } */
/* { dg-require-effective-target fenv } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -frounding-math -ffinite-math-only" } */

#include <fenv.h>

__attribute__((noipa)) double f(double x) { return x - x; }

int main(void) {
#ifdef FE_DOWNWARD
  fesetround(FE_DOWNWARD);
  if (!__builtin_signbit(f(3.0)))
    __builtin_abort();
#endif
  return 0;
}
