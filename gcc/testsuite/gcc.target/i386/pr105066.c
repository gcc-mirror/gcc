/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse4.1" } */
/* { dg-final { scan-assembler-not "movzwl" } } */
/* { dg-final { scan-assembler {(?n)pinsrw[ \t]+\$0.*\(%} } } */

#include <immintrin.h>

__m128i load16(void *p){
    return _mm_loadu_si16(p);
}
