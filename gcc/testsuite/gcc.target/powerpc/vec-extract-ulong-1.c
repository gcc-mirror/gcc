/* Test to verify that the vec_extract from a vector of
   unsigned longs remains unsigned.  */
/* { dg-do run } */
/* { dg-options "-ansi -mdejagnu-cpu=power8 " } */
/* { dg-require-effective-target p8vector_hw } */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

int test1(unsigned long long int ul) {

  vector unsigned long long int v = vec_splats(ul);

  if (vec_extract (v, 0) < ul)
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
