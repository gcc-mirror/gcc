/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "var_stride_2.c"
#include "var_stride_1.h"

int
main (void)
{
  for (int n = 0; n < 10; ++n)
    for (int m = 0; m < 10; ++m)
      for (int offset = -17; offset <= 17; ++offset)
	{
	  test (n, m, offset);
	  test (n, m, offset + n * (SIZE - 1));
	}
  return 0;
}
