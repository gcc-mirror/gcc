/* PR target/45336 */
/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -msse4 -mtune=generic" } */
/* { dg-final { scan-assembler "movsbq" } } */
/* { dg-final { scan-assembler "movswq" } } */
/* { dg-final { scan-assembler "(cltq|movslq)" } } */
/* { dg-final { scan-assembler "pextrb" } } */
/* { dg-final { scan-assembler "pextrw" } } */
/* { dg-final { scan-assembler "pextrd" } } */

#include <smmintrin.h>
long long int foo8(__m128i x) { return (char) _mm_extract_epi8(x, 4); }
long long int foo16(__m128i x) { return (short) _mm_extract_epi16(x, 3); }
long long int foo32(__m128i x) { return (int) _mm_extract_epi32(x, 2); }
