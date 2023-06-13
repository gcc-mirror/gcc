/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mno-vsx" } */

/* Verify there is no ICE but one expected error message instead.  */

#include <altivec.h>

extern vector signed __int128 res_vslll;
extern unsigned long long aull[2];

void
testVectorInt128Pack ()
{
  res_vslll = __builtin_pack_vector_int128 (aull[0], aull[1]); /* { dg-error "'__builtin_pack_vector_int128' requires the '-mvsx' option" } */
}

