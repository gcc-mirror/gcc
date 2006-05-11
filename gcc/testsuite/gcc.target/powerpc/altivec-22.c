/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O3 -maltivec" } */
/* { dg-final { scan-assembler-not "mfcr" } } */

#include <altivec.h>

int foo(vector float x, vector float y) {
        if (vec_all_eq(x,y)) return 3245;
        else return 12;
}
