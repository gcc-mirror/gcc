/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

/* Test whether the C front-end is not excessively picky about
   the integral types and literals that AltiVec instrinsics will
   accept.  */

vector int vi = { 1, 2, 3, 4 };

int
main (void)
{
    unsigned long ul = 2;
    signed long sl = 2;
    unsigned int ui = 2;
    signed int si = 2;
    float fl = 2.0;

    vec_dst (&vi, ul, '\0');
    vec_dst (&vi, sl, 0);
    vec_dst (&vi, ui, '\0');
    vec_dst (&vi, si, 0);
    vec_dstst (&vi, (short)fl, '\0');

    return 0;
}

