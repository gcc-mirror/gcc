/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector unsigned long long int
get_significands (__vector double *p)
{
  __vector double source = *p;

  return vec_extract_sig (source);
}

int
main ()
{
  __vector double argument;
  __vector unsigned long long int result;

  argument[0] = (double) (0xbabeLL << 22);
  argument[1] = (double) (0xcafeLL << 23);

  result = get_significands (&argument);
  if ((result[0] != (0xbabeULL << 37)) || (result[1] != (0xcafeULL << 37)))
    abort();
  return 0;
}

