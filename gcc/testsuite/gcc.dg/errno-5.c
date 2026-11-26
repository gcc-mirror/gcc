/* PR tree-optimization/128135 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-xfail-run-if "HP-UX 11.11 libm pow returns 0 instead of HUGE_VAL on subnormal overflow" { hppa64-hp-hpux11.[01]* } } */

#ifdef __NO_MATH_ERRNO__
int main() { return 0; }
#else
#include <errno.h>
#include <math.h>

int main(void)
{
#ifdef math_errhandling
#ifdef MATH_ERRNO
  if ((math_errhandling & MATH_ERRNO) == 0)
    return 0;
#else
  if ((math_errhandling & 1) == 0)
    return 0;
#endif
#endif

  errno = 0;
  double r = pow(1e-310, -1.0);
  if (r != HUGE_VAL)
    __builtin_abort();
#ifdef math_errhandling
#ifdef MATH_ERRNO
  if ((math_errhandling & MATH_ERRNO) && errno != ERANGE)
    __builtin_abort();
#else
  if ((math_errhandling & 1) && errno != ERANGE)
    __builtin_abort();
#endif
#else
  if (errno != ERANGE)
    __builtin_abort();
#endif
  return 0;
}
#endif
