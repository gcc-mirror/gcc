/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-O2 -maltivec" } */

/* Verify it run successfully.  */

#include <altivec.h>

__attribute__ ((noipa))
vector signed int
test ()
{
  vector signed int v = {-16, -16, -16, -16};
  vector signed int res = vec_sld (v, v, 3);
  return res;
}

int
main ()
{
  vector signed int res = test ();
  if (res[0] != 0xf0ffffff)
    __builtin_abort ();
  return 0;
}
