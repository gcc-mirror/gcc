/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-mcpu=7400 -maltivec" } */

/* PR c++/14425 */

#include <altivec.h>

vector unsigned int splat0(vector unsigned int x)
{
    return vec_splat(x, 0);
}
