/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector unsigned int
get_exponents (__vector float *p)
{
  __vector float source = *p;

  return vec_extract_exp (source);
}

unsigned int bias_float_exp (int unbiased_exp)
{
  return (unsigned int) (unbiased_exp + 127);
}

int
main ()
{
  __vector float argument;
  __vector unsigned int result;

  argument[0] = (float) (0x1 << 10);
  argument[1] = (float) (0x1 << 9);
  argument[2] = (float) (0x1 << 8);
  argument[3] = (float) (0x1 << 7);

  result = get_exponents (&argument);
  if ((result[0] != bias_float_exp (10)) ||
      (result[1] != bias_float_exp (9)) ||
      (result[2] != bias_float_exp (8)) || (result[3] != bias_float_exp (7)))
    abort();
  return 0;
}
