/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

int
main (void)
{
  long double res, x = 40., y = 2.;
  asm("lxr\t%0,%1\n"
      "\taxbr\t%0,%2"
      : "=&f"(res)
      : "%f"(x), "f"(y));
  assert (res == 42.);
}
