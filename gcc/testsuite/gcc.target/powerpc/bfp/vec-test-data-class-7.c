/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

__vector bool long long int
get_data_class_flags (__vector double *p, int condition_flag)
{
  __vector double source = *p;

  return vec_test_data_class (source, condition_flag); /* { dg-error "argument 2 must be a literal between 0 and 127, inclusive" } */
}
