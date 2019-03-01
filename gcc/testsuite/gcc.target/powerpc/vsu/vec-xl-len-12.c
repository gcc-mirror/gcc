/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

/* The vec_xl_len() function is not available on power8 configurations.  */

#include <stddef.h>
#include <altivec.h>

__vector float
fetch_data (float *address, size_t length)
{
  return __builtin_vec_lxvl (address, length); /* { dg-error "builtin function '__builtin_vsx_lxvl' requires" } */
}
