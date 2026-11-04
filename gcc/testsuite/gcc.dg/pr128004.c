/* PR middle-end/128004 */
/* { dg-do run } */
/* { dg-require-effective-target fenv } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -frounding-math" } */

#include <fenv.h>

int main(void) {
#ifdef FE_DOWNWARD
  double a = 1.0, b = -1.0;
  fesetround(FE_DOWNWARD);
  double r = a + b;
  if (!__builtin_signbit(r))
    __builtin_abort();
#endif
  return 0;
}
