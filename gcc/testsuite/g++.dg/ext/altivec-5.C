/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec" } */

/* PR c++/14426 */

#include <altivec.h>

vector unsigned int splat0u()
{
    return vec_splat_u32(0);
}
vector int splat0s()
{
    return vec_splat_s32(0);
}
