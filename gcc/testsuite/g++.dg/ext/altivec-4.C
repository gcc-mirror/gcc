/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec" } */

/* PR c++/14425 */

#include <altivec.h>

vector unsigned int splat0(vector unsigned int x)
{
    return vec_splat(x, 0);
}
