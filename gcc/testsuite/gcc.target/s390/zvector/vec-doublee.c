/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch -mzvector --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */

/*
 * The vector intrinsic vec_doublee(a) converts the even-indexed
 * single-precision numbers in a vector to double precision.
 */
#include <assert.h>
#include <vecintrin.h>

int
main (void)
{
    vector float in = { 1.0, 2.0, 3.0, 4.0 };

    vector double result = vec_doublee(in);
    /* { dg-final { scan-assembler-times {\n\tvldeb} 1 } } */

    assert(result[0] == (double)in[0]);
    assert(result[1] == (double)in[2]);
}
