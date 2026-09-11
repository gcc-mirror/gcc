/* { dg-additional-options "-march=armv8-a+sve" { target aarch64_sve_hw } } */

#include "tree-vect.h"

__attribute__ ((noipa)) unsigned int
mixed_dot_induction (unsigned int n)
{
  unsigned int sum = 0;
  for (unsigned int i = 0; i < n; ++i)
    sum += (signed char) i * (unsigned char) (i * 17 + 3);
  return sum;
}

int
main ()
{
  unsigned int expected = 0;
  check_vect ();
#pragma GCC novector
  for (unsigned int n = 0; n <= 1025; ++n)
    {
      if (mixed_dot_induction (n) != expected)
       __builtin_abort ();
      int x = (int) (n & 127) - (int) (n & 128);
      int y = (n * 17 + 3) & 255;
      expected += x * y;
    }
  return 0;
}
