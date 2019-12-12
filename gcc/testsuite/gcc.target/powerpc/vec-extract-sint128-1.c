/* Test to verify that the vec_extract from a vector of
   signed __int128s remains signed.  */
/* { dg-do run { target int128 } } */
/* { dg-options "-ansi -mdejagnu-cpu=power8 " } */
/* { dg-require-effective-target p8vector_hw } */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

int test1(signed __int128 st) {

  vector signed __int128 v = vec_splats(st);

  if (vec_extract (v, 0) > st)
    abort();
  return 0;
}

int main()
{
  test1 (((__int128) 0xf600000000000000LL) << 64);
  test1 (((__int128) 0x7600000000000000LL) << 64);
  test1 (((__int128) 0x0600000000000000LL) << 64);
  return 0;
}
