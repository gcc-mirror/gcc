
/* This test code is included into vsx-7-be.c.
 * this is meant to supplement code in altivec-7.h.  */

#include <altivec.h>


vector float *vecfloat;
vector double *vecdouble;

int main2 ()
{

  *vecdouble++ = vec_unpackl(vecfloat[0]);
  *vecdouble++ = vec_unpackh(vecfloat[0]);

  return 0;
}
