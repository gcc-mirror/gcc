/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

unsigned long long int
get_unbiased_exponent (__ieee128 *p)
{
  __ieee128 source = *p;

  return scalar_extract_exp (source) - 16383;
}

int
main ()
{
  __ieee128 x = (__ieee128) (((__int128) 0x1100LL) << 114);
  __ieee128 z = (__ieee128) (((__int128) 0x1101LL) << 112);

  if (get_unbiased_exponent (&x) != 126)
    abort ();
  if (get_unbiased_exponent (&z) != 124)
    abort ();
  return 0;
}
