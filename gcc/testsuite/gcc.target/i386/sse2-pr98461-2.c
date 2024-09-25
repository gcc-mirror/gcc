/* PR target/98461 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -masm=att" } */
/* { dg-final { scan-assembler-times "\tpmovmskb\t" 3 } } */
/* { dg-final { scan-assembler-not "\tmovzwl" } } */
/* { dg-final { scan-assembler-times "\tnotl" 1 } } */
/* { dg-final { scan-assembler-times "\txorl" 1 } } */

#include <immintrin.h>

unsigned int movemask_not1(__m128i logical) {
  unsigned short res = (unsigned short)(_mm_movemask_epi8(logical));
  return ~res;
}

unsigned int movemask_not2(__m128i logical) {
  unsigned short res = (unsigned short)(_mm_movemask_epi8(logical));
  res = ~res;
  return res;
}

unsigned int movemask_zero_extend(__m128i logical) {
  unsigned int res = _mm_movemask_epi8(logical);
  return res & 0xffff;
}
