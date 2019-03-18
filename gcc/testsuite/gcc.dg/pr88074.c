/* { dg-do compile { target c99_runtime } } */
/* { dg-options "-O" } */

#include <complex.h>

int main()
{
  _Complex double x;
  __real x = 3.091e+8;
  __imag x = -4.045e+8;
  /* This used to spend huge amounts of compile-time inside mpc.  */
  volatile _Complex double y = ctan (x);
  return 0;
}
