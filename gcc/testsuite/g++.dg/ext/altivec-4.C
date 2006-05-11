/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* PR c++/14425 */

#include <altivec.h>

vector unsigned int splat0(vector unsigned int x)
{
    return vec_splat(x, 0);
}
