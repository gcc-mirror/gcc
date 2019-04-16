/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

unsigned __int128
get_significand (__ieee128 *p)
{
  __ieee128 source = *p;

  return scalar_extract_sig (source);
}

int
main ()
{
  __ieee128 x = (__ieee128) (((__int128) 0x1100LL) << 114);
  __ieee128 z = (__ieee128) (((__int128) 0x1101LL) << 112);

  /* 113 bits in the significand */
  /* our constant mantissas have 13 bits */

  unsigned __int128 first_anticipated_result = ((__int128) 0x1100LL) << 100;
  unsigned __int128 second_anticipated_result = ((__int128) 0x1101LL) << 100;

  if (get_significand (&x) != first_anticipated_result)
    abort ();
  if (get_significand (&z) != second_anticipated_result)
    abort ();
  return 0;
}
