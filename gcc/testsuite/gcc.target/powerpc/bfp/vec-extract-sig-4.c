/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector unsigned int
get_significands (__vector float *p)
{
  __vector float source = *p;

  return vec_extract_sig (source);
}

int
main ()
{
  __vector float argument;
  __vector unsigned int result;

  argument[0] = (float) (0x1234 << 10);
  argument[1] = (float) (0x4321 << 9);
  argument[2] = (float) (0xbabe << 8);
  argument[3] = (float) (0xcafe << 7);

  result = get_significands (&argument);
  if ((result[0] != 0x91a000) || (result[1] != 0x864200) ||
      (result[2] != 0xbabe00) || (result[3] != 0xcafe00))
    abort();
  return 0;
}
