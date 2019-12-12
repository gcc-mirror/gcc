/* PR target/91704 */
/* { dg-do compile } */
/* { dg-options "-O2 -funsigned-char -mavx2 -mavx512f -masm=att" } */
/* { dg-final { scan-assembler-times "\tvpcmpgtb\t%ymm" 1 } } */
/* { dg-final { scan-assembler-not "\tvpsubusb\t" } } */
/* { dg-final { scan-assembler-not "\tvpcmpeqb\t" } } */

#include <x86intrin.h>

__m256i
foo (__m256i x, __m256i y)
{
  return _mm256_cmpgt_epi8 (x, y);
}
