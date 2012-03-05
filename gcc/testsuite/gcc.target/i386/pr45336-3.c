/* PR target/45336 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4 -mtune=generic" } */
/* { dg-final { scan-assembler "movsbl" } } */
/* { dg-final { scan-assembler "(movswl|cwtl)" } } */
/* { dg-final { scan-assembler "pextrb" } } */
/* { dg-final { scan-assembler "pextrw" } } */
/* { dg-final { scan-assembler "pextrd" { target { ! x86_64-*-mingw* } } } } */

#include <smmintrin.h>
int foo8(__m128i x) { return (char) _mm_extract_epi8(x, 4); }
int foo16(__m128i x) { return (short) _mm_extract_epi16(x, 3); }
int foo32(__m128i x) { return _mm_extract_epi32(x, 2); }
