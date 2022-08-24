/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

#include <altivec.h>
vector signed int foo1 (vector signed int a) {
    vector signed int b = {0};
    return vec_sum2s(a, b);
}

vector signed int foo2 (vector signed int a) {
    vector signed int b = {0};
    return vec_sld(b, b, 4);
}

/* { dg-final { scan-assembler-times {\mvsldoi\M} 1 {target le} } } */
