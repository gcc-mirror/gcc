/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

__vector int
get_data_class_flags (__vector float *p)
{
  __vector float source = *p;

  return __builtin_vec_test_data_class (source, 0x37); /* { dg-error "Builtin function __builtin_vsx_test_data_class_sp requires" } */
}
