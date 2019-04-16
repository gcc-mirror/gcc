/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_data_class (double *p, const int condition_flag)
{
  double source = *p;

  return scalar_test_data_class (source, condition_flag); /* { dg-error "argument 2 must be a 7-bit unsigned literal" } */
}

