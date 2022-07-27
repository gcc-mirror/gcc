/* PR target/83660 */
/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power7" } */

#include <altivec.h>

typedef __vector unsigned int  uvec32_t  __attribute__((__aligned__(16)));

unsigned get_word(uvec32_t v)
{
    return ({const unsigned _B1 = 32;
            vec_extract((uvec32_t)v, 2);});
}
