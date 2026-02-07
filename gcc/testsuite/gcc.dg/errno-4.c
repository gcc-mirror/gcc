/* PR middle-end/123826 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#ifdef __NO_MATH_ERRNO__
int main() { return 0; }
#else
#include <errno.h>
#include <float.h>
#include <math.h>

double foo(double x)
{
  return x + pow(x, 2.0);
}

int main()
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
  double x = foo(DBL_MAX);
  if (errno != ERANGE)
    __builtin_abort ();
  return 0;
}
#endif
