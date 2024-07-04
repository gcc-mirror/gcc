/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector bool long long int
get_data_class_flags (__vector double *p, int condition_flag)
{
  __vector double source = *p;

  return vec_test_data_class (source, condition_flag); /* { dg-error "argument 2 must be a literal between 0 and 127, inclusive" } */
}
