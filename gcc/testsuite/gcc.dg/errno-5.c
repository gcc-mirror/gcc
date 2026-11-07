/* PR tree-optimization/128135 */
/* { dg-do run } */
/* { dg-options "-O2" } */

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
  if ((math_errhandling & MATH_ERRNO) && errno != ERANGE)
    __builtin_abort();
  return 0;
}
#endif
