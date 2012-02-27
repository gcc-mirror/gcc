/* PR target/45336 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4 -mtune=generic" } */
/* { dg-final { scan-assembler-not "movsbl" } } */
/* { dg-final { scan-assembler-not "movswl" } } */
/* { dg-final { scan-assembler-not "movzbl" } } */
/* { dg-final { scan-assembler-not "movzwl" } } */
/* { dg-final { scan-assembler-not "cwtl" } } */
/* { dg-final { scan-assembler "pextrb" } } */
/* { dg-final { scan-assembler "pextrw" } } */
/* { dg-final { scan-assembler "pextrd" { target { ! x86_64-*-mingw* } } } } */

#include <smmintrin.h>
unsigned int foo8(__m128i x) { return _mm_extract_epi8(x, 4); }
unsigned int foo16(__m128i x) { return _mm_extract_epi16(x, 3); }
unsigned int foo32(__m128i x) { return _mm_extract_epi32(x, 2); }
