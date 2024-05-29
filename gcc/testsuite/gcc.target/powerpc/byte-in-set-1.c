/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int
test_byte_in_set (unsigned char b, unsigned long long set_members)
{
  return __builtin_byte_in_set (b, set_members); /* { dg-error "'__builtin_scalar_byte_in_set' requires" } */
}
