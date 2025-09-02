/* { dg-do compile } */
/* { dg-options "-S -O3 -march=armv9-a+sme2" } */

#include <arm_sme.h>

svfloat16x2_t test (svfloat16x2_t zd, svfloat16x2_t zm) __arm_streaming
{
    return svamin_f16_x2 (zd, zm); // { dg-error "ACLE function .svamin_f16_x2. requires ISA extension .faminmax." }
}
