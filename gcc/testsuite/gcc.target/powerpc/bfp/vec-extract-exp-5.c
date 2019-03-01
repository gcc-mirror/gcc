/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector unsigned long long int
get_exponents (__vector double *p)
{
  __vector double source = *p;

  return vec_extract_exp (source);
}

unsigned long long int
bias_double_exp (long long int unbiased_exp)
{
  return (unsigned long long int) (unbiased_exp + 1023);
}

int
main ()
{
  __vector double argument;
  __vector unsigned long long int result;

  argument[0] = (double) (0x1 << 22);
  argument[1] = (double) (0x1 << 23);

  result = get_exponents (&argument);
  if ((result[0] != bias_double_exp (22)) ||
      (result[1] != bias_double_exp (23)))
    abort();
  return 0;
}
