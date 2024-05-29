/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

/* The vec_xl_len() function is not available on power8 configurations.  */

#include <stddef.h>
#include <altivec.h>

__vector float
fetch_data (float *address, size_t length)
{
  return __builtin_vec_lxvl (address, length); /* { dg-error "'__builtin_vsx_lxvl' requires" } */
}
