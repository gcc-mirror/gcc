/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -mvsx" } */

#include <altivec.h>

vector bool int *vecubi;
vector bool long long *vecublli;
vector signed int *vecsi;
vector signed long long int *vecslli;

int main ()
{

  /*  use of ‘long long’ in AltiVec types requires -mvsx */
  /* __builtin_altivec_vupkhsw and __builtin_altivec_vupklsw
     requires the -mpower8-vector option */
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
