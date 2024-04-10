/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

vector bool int *vecubi;
vector bool long long *vecublli;
vector signed int *vecsi;
vector signed long long int *vecslli;

int main ()
{

  /*  use of ‘long long’ in AltiVec types requires -mvsx */
  /* __builtin_altivec_vupkhsw and __builtin_altivec_vupklsw
     requires the -mcpu=power8 and -mvsx option */
  *vecublli++ = vec_unpackh(vecubi[0]);
  *vecublli++ = vec_unpackl(vecubi[0]);
  *vecslli++ = vec_unpackh(vecsi[0]);
  *vecslli++ = vec_unpackl(vecsi[0]);
  
  return 0;
}

/* Expected results:
     vec_unpackh                    vupklsw
     vec_unpackl                    vupkhsw
*/

/* { dg-final { scan-assembler-times "vupklsw" 2 } } */
/* { dg-final { scan-assembler-times "vupkhsw" 2 } } */
