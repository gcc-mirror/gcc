/* Test to verify that the vec_extract from a vector of
   unsigned ints remains unsigned.  */
/* { dg-do run } */
/* { dg-options "-ansi -mcpu=power8 " } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

int test1(unsigned int ui) {
  long long int uie;

  vector unsigned int v = vec_splats(ui);
  uie = vec_extract(v,0);

  if (uie != ui)
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
