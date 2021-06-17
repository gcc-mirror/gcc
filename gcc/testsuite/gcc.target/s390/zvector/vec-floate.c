/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch -mzvector --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */

/*
 * The vector intrinsic vec_floate(a) rounds a vector of double-precision
 * numbers to single-precision. The results are stored in the even-numbered
 * target elements.
 */
#include <assert.h>
#include <vecintrin.h>

int
main (void)
{
    vector double in = { 1.0, 2.0 };

    vector float result = vec_floate(in);
    /* { dg-final { scan-assembler-times {\n\tvledb} 1 } } */

    assert(result[0] == (float)in[0]);
    assert(result[2] == (float)in[1]);
}
