/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx" } */

#include <altivec.h>

vector float *vecfloat;
vector double *vecdouble;

int main2 ()
{

  *vecdouble++ = vec_unpackl(vecfloat[0]);
  *vecdouble++ = vec_unpackh(vecfloat[0]);

  return 0;
}

/* Expected results:
     vec_unpackl                    xvcvspdp
     vec_unpackh                    xvcvspdp
*/

/* { dg-final { scan-assembler-times "xvcvspdp" 2 } } */

