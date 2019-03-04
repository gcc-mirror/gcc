/* { dg-do compile { target powerpc_altivec_ok } } */
/* { dg-options "-maltivec -mdejagnu-cpu=G5 -O2 -Wno-deprecated" } */

#include <altivec.h>

void foo( float scalar)
{
    unsigned long width;
    unsigned long x;
    vector float vColor;
    vector unsigned int selectMask;
    vColor = vec_perm( vec_ld( 0, &scalar), vec_ld( 3, &scalar), vec_lvsl( 0, &scalar) );

    float *destRow;
    vector float store, load0;

    for( ; x < width; x++)
    {
            load0 = vec_sel( vColor, load0, selectMask );
            vec_st( store, 0, destRow );
            store = load0;
    }
}
