/* Test to verify that the vec_extract from a vector of
   signed longs remains signed.  */
/* { dg-do run } */
/* { dg-options "-ansi -mdejagnu-cpu=power8 " } */
/* { dg-require-effective-target p8vector_hw } */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

int test1(signed long long int sl) {

  vector signed long long int v = vec_splats(sl);

  if (vec_extract (v, 0) > sl)
    abort();
  return 0;
}

int main()
{
  test1 (0xf600000000000000LL);
  test1 (0x7600000000000000LL);
  test1 (0x0600000000000000LL);
  return 0;
}
