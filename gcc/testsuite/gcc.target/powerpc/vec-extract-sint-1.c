/* Test to verify that the vec_extract from a vector of
   signed ints remains signed.  */
/* { dg-do run } */
/* { dg-options "-ansi -mdejagnu-cpu=power8 " } */
/* { dg-require-effective-target p8vector_hw } */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

int test1(signed int si) {
  long long int sie;

  vector signed int v = vec_splats(si);
  sie = vec_extract(v,0);

  if (sie != si)
    abort();
  return 0;
}

int main()
{
  test1 (0xf6000000);
  test1 (0x76000000);
  test1 (0x06000000);
  return 0;
}
