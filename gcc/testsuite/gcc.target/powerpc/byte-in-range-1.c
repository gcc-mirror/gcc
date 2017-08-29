/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

int
test_byte_in_range (unsigned char b,
		    unsigned char low_range, unsigned char high_range)
{
  unsigned int range_encoding = (high_range << 8) | low_range;
  return __builtin_byte_in_range (b, range_encoding); /* { dg-error "builtin function '__builtin_scalar_byte_in_range' requires" } */
}

