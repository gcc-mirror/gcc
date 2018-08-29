/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>

/* This test should succeed only on 32-bit configurations.  */

int
test_byte_in_set (unsigned char b, unsigned long long set_members)
{
  return __builtin_byte_in_set (b, set_members); /* { dg-error "builtin function '__builtin_byte_in_set' not supported in this compiler configuration" } */
}
